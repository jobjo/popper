open Ppxlib
module A = Ast_builder.Default

type bindings =
  { type_name : string
  ; sized : value_binding
  ; alias : value_binding
  ; num_poly : int
  }

let loc = Location.none

let sized_generator_name n =
  if String.equal n "t" then
    Printf.sprintf "generate_sized"
  else
    Printf.sprintf "generate_sized_%s" n

let generator_name n =
  if String.equal n "t" then
    "generate"
  else
    Printf.sprintf "generate_%s" n

let pp_name = function
  | "t" -> "pp"
  | name -> Printf.sprintf "pp_%s" name

let eq_name = function
  | "t" -> "equal"
  | name -> Printf.sprintf "equal_%s" name

let comparator_name = function
  | "t" -> "comparator"
  | name -> Printf.sprintf "%s_comparator" name

let poly_fun_name n = Printf.sprintf "generate_poly_%s" n

let generator_of_type ~size ~loc = function
  | "int" -> [%expr Popper.Generator.int]
  | "string" -> [%expr Popper.Generator.string]
  | "bool" -> [%expr Popper.Generator.bool]
  | "float" -> [%expr Popper.Generator.float]
  | "int32" -> [%expr Popper.Generator.int32]
  | t ->
    let e = A.evar ~loc (sized_generator_name t) in
    [%expr Popper.Generator.delayed (fun () -> [%e e] [%e size])]

let one_of_exp exps =
  let exps =
    (* Select the non-recursive fields first *)
    List.sort (fun x y -> compare (snd x) (snd y)) exps |> List.map fst
  in
  [%expr
    if size <= 0 then
      [%e List.hd exps]
    else
      Popper.Generator.one_of [%e A.elist ~loc exps]]

let rec of_label_declarations ~loc fields f =
  let size = [%expr size / [%e A.eint ~loc @@ List.length fields]] in
  let accum (var, value) body =
    let name = A.pvar ~loc var in
    [%expr
      Popper.Generator.Syntax.(
        let* [%p name] = [%e value] in
        [%e body])]
  in
  let field_exprs = List.map (of_label_declaration ~size) fields in
  let ident_exps =
    List.map
      (fun (name, _) ->
        let exp = A.evar ~loc name in
        ({ txt = lident name; loc }, exp))
      field_exprs
  in
  let record =
    [%expr Popper.Generator.return [%e f @@ A.pexp_record ~loc ident_exps None]]
  in
  List.fold_right accum field_exprs record

and of_tuple ~loc ~size types f =
  let size = [%expr [%e size] / [%e A.eint ~loc @@ List.length types]] in
  let accum (name, value) body =
    [%expr
      Popper.Generator.Syntax.(
        let* [%p A.pvar ~loc name] = [%e value] in
        [%e body])]
  in
  let exprs = List.map (of_core_type ~size) types in
  let name_exp_list =
    List.mapi
      (fun i x ->
        let name = Printf.sprintf "x%d" i in
        (name, x))
      exprs
  in
  let evars = List.map (fun (n, _) -> A.evar ~loc n) name_exp_list in
  let tuple =
    [%expr Popper.Generator.return [%e f @@ A.pexp_tuple ~loc evars]]
  in
  List.fold_right accum name_exp_list tuple

and of_applied_type ~size ~name ts =
  match (name, ts) with
  | "option", [ t ] -> [%expr Popper.Generator.option [%e of_core_type ~size t]]
  | "list", [ t ] -> [%expr Popper.Generator.list [%e of_core_type ~size t]]
  | "result", [ t1; t2 ] ->
    [%expr
      Popper.Generator.result
        ~ok:[%e of_core_type ~size t1]
        ~error:[%e of_core_type ~size t2]]
  | name, ts ->
    let accum exp t = [%expr [%e exp] [%e of_core_type ~size t]] in
    let exp =
      List.fold_left accum (A.evar ~loc (sized_generator_name name))
      @@ List.rev ts
    in
    [%expr [%e exp] [%e size]]

and of_row_field_desc = function
  | Rtag (name, _, []) ->
    [%expr Popper.Generator.return [%e A.pexp_variant ~loc name.txt None]]
  | Rtag (name, _, cts) ->
    of_tuple ~loc ~size:[%expr size] cts
    @@ fun expr -> [%expr [%e A.pexp_variant ~loc name.txt (Some expr)]]
  | Rinherit _ -> failwith "Rinherit"

and of_row_field { prf_desc; _ } = of_row_field_desc prf_desc
and of_row_fields rfs = List.map of_row_field rfs

and of_core_type_desc ~size = function
  | Ptyp_constr ({ txt = Lident name; loc }, []) ->
    generator_of_type ~size ~loc name
  | Ptyp_constr ({ txt = Lident name; _ }, ts) -> of_applied_type ~size ~name ts
  | Ptyp_arrow (_, _, t) ->
    let gen_exp = of_core_type ~size:[%expr size] t in
    [%expr Popper.Generator.arrow [%e gen_exp]]
  | Ptyp_tuple ts -> of_tuple ~loc ~size:[%expr size] ts Fun.id
  | Ptyp_alias (t, _) -> of_core_type ~size t
  | Ptyp_variant (row_fields, _, _) ->
    one_of_exp @@ List.map (fun x -> (x, false)) (of_row_fields row_fields)
  | Ptyp_poly _ -> failwith "Unsupported core-type `Ptype_poly'"
  | Ptyp_any -> failwith "Unsupported core-type `Any'"
  | Ptyp_var v -> A.evar ~loc (poly_fun_name v)
  | _ -> failwith "Unsupported core-type"

and of_core_type ~size { ptyp_desc; ptyp_attributes = _; _ } =
  of_core_type_desc ~size ptyp_desc

and of_core_types ~size = function
  | [] -> failwith "Unexpected"
  | [ ct ] -> of_core_type ~size ct
  | ct :: cts ->
    let e = of_core_type ~size ct in
    let es = of_core_types ~size cts in
    [%expr [%e e] [%e es]]

and of_label_declaration
  ~size
  { pld_name = { txt = name; loc = _ }
  ; pld_mutable = _
  ; pld_type = { ptyp_desc; ptyp_attributes = _; _ }
  ; pld_loc = _
  ; pld_attributes = _
  }
  =
  let type_name = of_core_type_desc ~size ptyp_desc in
  (name, [%expr [%e type_name]])

let sized_fun ~loc ~fun_name ~param_types body =
  let pat = A.pvar ~loc fun_name in
  let poly_funs =
    List.filter_map
      (function
        | { ptyp_desc = Ptyp_var n; _ } -> Some (poly_fun_name n)
        | _ -> None)
      param_types
  in
  let expr =
    let size_fun =
      [%expr
        fun size ->
          ignore size;
          [%e body]]
    in
    let accum body ct =
      match ct with
      | { ptyp_desc = Ptyp_var n; _ } ->
        let pfn = poly_fun_name n in
        let poly_gen = A.pvar ~loc pfn in
        [%expr fun [%p poly_gen] -> [%e body]]
      | _ -> failwith "More than one param"
    in
    List.fold_left accum size_fun param_types
  in
  (A.value_binding ~loc ~pat ~expr, poly_funs)

let of_record ~loc ~fun_name ~param_types label_decls =
  of_label_declarations ~loc label_decls Fun.id
  |> sized_fun ~loc ~fun_name ~param_types

let with_loc ~loc txt = { txt; loc }

let has_rec_types ~is_rec_type cargs =
  let rec aux { ptyp_desc; _ } =
    match ptyp_desc with
    | Ptyp_constr ({ txt = Lident "option"; _ }, _) -> false
    | Ptyp_constr ({ txt = Lident "list"; _ }, _) -> false
    | Ptyp_constr ({ txt = Lident name; _ }, cts) ->
      is_rec_type name || List.exists aux cts
    | Ptyp_arrow (_, _, t) -> aux t
    | Ptyp_tuple ts -> List.exists aux ts
    | Ptyp_alias (t, _) -> aux t
    | Ptyp_var v -> is_rec_type v
    | _ -> false
  in
  match cargs with
  | Pcstr_tuple [] -> false
  | Pcstr_tuple cs -> List.exists aux cs
  | Pcstr_record cs -> List.exists (fun { pld_type; _ } -> aux pld_type) cs

let of_constructor_declaration
  ~size
  ~is_rec_type
  { pcd_name = { txt = name; _ }; pcd_args; pcd_res = _; _ }
  =
  let constr_decl =
    let name = with_loc ~loc name in
    A.constructor_declaration ~loc ~name ~args:pcd_args ~res:None
  in
  let construct expr = [%expr [%e A.econstruct constr_decl (Some expr)]] in
  let exp =
    match pcd_args with
    | Pcstr_tuple [] ->
      [%expr Popper.Generator.return [%e A.econstruct constr_decl None]]
    | Pcstr_tuple ts -> of_tuple ~loc ~size ts construct
    | Pcstr_record ldl -> of_label_declarations ~loc ldl construct
  in
  (exp, has_rec_types ~is_rec_type pcd_args)

let of_variant ~is_rec_type ~loc ~fun_name ~param_types constrs =
  let size = [%expr size] in
  constrs
  |> List.map (of_constructor_declaration ~is_rec_type ~size)
  |> one_of_exp
  |> sized_fun ~loc ~fun_name ~param_types

let make_abstract ~fun_name ptype_manifest =
  match ptype_manifest with
  | Some t ->
    sized_fun ~loc ~fun_name ~param_types:[] (of_core_type ~size:[%expr size] t)
  | None -> failwith "Unsupprted type kind"

let of_type_declaration
  ~is_rec_type
  { ptype_name = { txt = type_name; _ }
  ; ptype_params
  ; ptype_cstrs = _
  ; ptype_kind
  ; ptype_loc = loc
  ; ptype_private = _
  ; ptype_manifest
  ; ptype_attributes = _
  }
  =
  let fun_name_sized = sized_generator_name type_name in
  let param_types = List.map fst ptype_params in
  let sized, poly_gens =
    match ptype_kind with
    | Ptype_record fields ->
      of_record ~loc ~fun_name:fun_name_sized ~param_types fields
    | Ptype_variant constrs ->
      of_variant ~is_rec_type ~loc ~fun_name:fun_name_sized ~param_types constrs
    | Ptype_abstract -> make_abstract ~fun_name:fun_name_sized ptype_manifest
    | _ -> failwith "Unsupported type-kind"
  in
  let alias =
    let fun_name = generator_name type_name in
    let pat = A.pvar ~loc fun_name in
    let body =
      let type_constraint =
        let li = Ldot (Ldot (Lident "Popper", "Generator"), "t") in
        let type_vars =
          poly_gens
          |> List.mapi (fun ix _ -> A.ptyp_var ~loc @@ Printf.sprintf "a%d" ix)
        in
        A.ptyp_constr
          ~loc
          (with_loc ~loc li)
          [ A.ptyp_constr ~loc (with_loc ~loc (Lident type_name)) type_vars ]
      in
      let expr =
        let accum exp p = [%expr [%e exp] [%e A.evar ~loc p]] in
        List.fold_left accum [%expr [%e A.evar ~loc fun_name_sized]] poly_gens
      in
      A.pexp_constraint
        ~loc
        [%expr Popper.Generator.sized [%e expr]]
        type_constraint
    in
    let expr =
      let accum exp p = [%expr fun [%p A.pvar ~loc p] -> [%e exp]] in
      List.fold_left accum body poly_gens
    in
    A.value_binding ~loc ~pat ~expr
  in
  { type_name; sized; alias; num_poly = List.length poly_gens }

let of_type_declarations ~is_rec_type =
  List.map (of_type_declaration ~is_rec_type)

let comparator { type_name; num_poly; _ } =
  let expr =
    let accum (e1, e2) ix =
      let eq_poly = Printf.sprintf "eq_poly_%d" ix in
      let pp_poly = Printf.sprintf "pp_poly_%d" ix in
      ( [%expr [%e e1] [%e A.evar ~loc eq_poly]]
      , [%expr [%e e2] [%e A.evar ~loc pp_poly]] )
    in
    let zero =
      (A.evar ~loc @@ eq_name type_name, A.evar ~loc @@ pp_name type_name)
    in
    let ixs = List.rev @@ List.init num_poly Fun.id in
    let peq, ppp = List.fold_left accum zero ixs in
    let body = [%expr Popper.Comparator.make [%e peq] [%e ppp]] in
    let accum exp ix =
      let eq_poly = Printf.sprintf "eq_poly_%d" ix in
      let pp_poly = Printf.sprintf "pp_poly_%d" ix in
      [%expr fun [%p A.pvar ~loc eq_poly] [%p A.pvar ~loc pp_poly] -> [%e exp]]
    in
    List.fold_left accum body ixs
  in
  A.value_binding ~loc ~pat:(A.pvar ~loc @@ comparator_name type_name) ~expr

let sized bindings = bindings |> List.map (fun { sized; _ } -> sized)
let aliases bindings = bindings |> List.map (fun { alias; _ } -> alias)

let comparators bindings =
  List.map comparator bindings |> A.pstr_value_list ~loc Nonrecursive

let generate_impl ~ctxt:_ (rec_flag, type_declarations) =
  let rec_flag = really_recursive rec_flag type_declarations in
  let is_rec_type =
    let ts =
      type_declarations |> List.map (fun { ptype_name = { txt; _ }; _ } -> txt)
    in
    fun t -> rec_flag = Recursive && List.exists (String.equal t) ts
  in
  let bindings = of_type_declarations ~is_rec_type type_declarations in
  let generators =
    A.pstr_value_list ~loc rec_flag (sized bindings)
    @ A.pstr_value_list ~loc Nonrecursive (aliases bindings)
  in
  let comparators = comparators bindings in
  generators @ comparators

let impl_generator = Deriving.Generator.V2.make_noarg generate_impl
let my_deriver = Deriving.add "popper" ~str_type_decl:impl_generator
