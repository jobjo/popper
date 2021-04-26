open Ppxlib

type bindings =
  { type_name : string
  ; sized : value_binding
  ; alias : value_binding
  ; num_poly : int
  }

let sample_name n =
  if String.equal n "t" then
    "sample"
  else
    Printf.sprintf "sample_%s" n

let pp_name = function
  | "t" -> "pp"
  | name -> Printf.sprintf "pp_%s" name

let compare_name = function
  | "t" -> "compare"
  | name -> Printf.sprintf "compare_%s" name

let comparator_name = function
  | "t" -> "comparator"
  | name -> Printf.sprintf "%s_comparator" name

let poly_fun_name n = Printf.sprintf "sample_poly_%s" n

let sample_of_type ~is_rec_type ~size ~loc = function
  | "int" -> [%expr Popper.Sample.int]
  | "string" -> [%expr Popper.Sample.string]
  | "bool" -> [%expr Popper.Sample.bool]
  | "float" -> [%expr Popper.Sample.float]
  | "int32" -> [%expr Popper.Sample.int32]
  | t ->
    let (module A) = Ast_builder.make loc in
    let body =
      let sample = A.evar (sample_name t) in
      if is_rec_type t then
        [%expr [%e sample] [%e size]]
      else
        sample
    in
    [%expr Popper.Sample.delayed (fun () -> [%e body])]

let one_of_exp ~loc exps =
  let exps =
    (* Select the non-recursive fields first *)
    List.sort (fun x y -> compare (snd x) (snd y)) exps |> List.map fst
  in
  let (module A) = Ast_builder.make loc in
  [%expr
    if size <= 0 then
      [%e List.hd exps]
    else
      Popper.Sample.one_of [%e A.elist exps]]

let rec of_label_declarations ~is_rec_type ~loc fields f =
  let (module A) = Ast_builder.make loc in
  let size = [%expr size / [%e A.eint @@ List.length fields]] in
  let accum (var, value) body =
    let name = A.pvar var in
    [%expr
      Popper.Sample.Syntax.(
        let* [%p name] =
          Popper.Sample.resize
            [%e size]
            (Popper.Sample.tag_name [%e A.estring var] [%e value])
        in
        [%e body])]
  in
  let field_exprs = List.map (of_label_declaration ~is_rec_type ~size) fields in
  let ident_exps =
    List.map
      (fun (name, _) ->
        let exp = A.evar name in
        ({ txt = lident name; loc }, exp))
      field_exprs
  in
  let record =
    [%expr Popper.Sample.return [%e f @@ A.pexp_record ident_exps None]]
  in
  List.fold_right accum field_exprs record

and of_tuple ~is_rec_type ~loc ~size types f =
  let (module A) = Ast_builder.make loc in
  let size = [%expr [%e size] / [%e A.eint @@ List.length types]] in
  let accum (name, value) body =
    [%expr
      Popper.Sample.Syntax.(
        let* [%p A.pvar name] =
          Popper.Sample.tag_name [%e A.estring name] [%e value]
        in
        [%e body])]
  in
  let exprs = List.map (of_core_type ~is_rec_type ~size) types in
  let name_exp_list =
    List.mapi
      (fun i x ->
        let name = Printf.sprintf "x%d" i in
        (name, x))
      exprs
  in
  let evars = List.map (fun (n, _) -> A.evar n) name_exp_list in
  let tuple = [%expr Popper.Sample.return [%e f @@ A.pexp_tuple evars]] in
  List.fold_right accum name_exp_list tuple

and of_applied_type ~loc ~is_rec_type ~size ~name ts =
  let (module A) = Ast_builder.make loc in
  match (name, ts) with
  | "option", [ t ] ->
    [%expr Popper.Sample.option [%e of_core_type ~is_rec_type ~size t]]
  | "list", [ t ] ->
    [%expr Popper.Sample.list [%e of_core_type ~is_rec_type ~size t]]
  | "result", [ t1; t2 ] ->
    [%expr
      Popper.Sample.result
        ~ok:[%e of_core_type ~is_rec_type ~size t1]
        ~error:[%e of_core_type ~is_rec_type ~size t2]]
  | name, ts ->
    let accum exp t = [%expr [%e exp] [%e of_core_type ~is_rec_type ~size t]] in
    let is_rec_type = is_rec_type name in
    let name = A.evar (sample_name name) in
    let exp = List.fold_left accum name ts in
    if is_rec_type then
      [%expr [%e exp] [%e size]]
    else
      exp

and of_row_field_desc ~loc ~is_rec_type desc =
  let (module A) = Ast_builder.make loc in
  match desc with
  | Rtag (name, _, []) ->
    [%expr Popper.Sample.return [%e A.pexp_variant name.txt None]]
  | Rtag (name, _, cts) ->
    of_tuple ~is_rec_type ~loc ~size:[%expr size] cts @@ fun expr ->
    [%expr [%e A.pexp_variant name.txt (Some expr)]]
  | Rinherit _ -> failwith "Rinherit"

and of_row_field ~is_rec_type { prf_desc; prf_loc = loc; _ } =
  of_row_field_desc ~loc ~is_rec_type prf_desc

and of_row_fields ~is_rec_type rfs = List.map (of_row_field ~is_rec_type) rfs

and of_core_type_desc ~loc ~is_rec_type ~size exp =
  let (module A) = Ast_builder.make loc in
  match exp with
  | Ptyp_constr ({ txt = Lident name; loc }, []) ->
    sample_of_type ~is_rec_type ~size ~loc name
  | Ptyp_constr ({ txt = Lident name; _ }, ts) ->
    of_applied_type ~loc ~is_rec_type ~size ~name ts
  | Ptyp_arrow (_, _, t) ->
    let gen_exp = of_core_type ~is_rec_type ~size:[%expr size] t in
    [%expr Popper.Sample.arrow [%e gen_exp]]
  | Ptyp_tuple ts -> of_tuple ~is_rec_type ~loc ~size:[%expr size] ts Fun.id
  | Ptyp_alias (t, _) -> of_core_type ~is_rec_type ~size t
  | Ptyp_variant (row_fields, _, _) ->
    of_row_fields ~is_rec_type row_fields
    |> List.map (fun x -> (x, false))
    |> one_of_exp ~loc
  | Ptyp_poly _ -> failwith "Unsupported core-type `Ptype_poly'"
  | Ptyp_any -> failwith "Unsupported core-type `Any'"
  | Ptyp_var v -> A.evar (poly_fun_name v)
  | _ -> failwith "Unsupported core-type"

and of_core_type ~is_rec_type ~size { ptyp_desc; ptyp_loc = loc; _ } =
  of_core_type_desc ~loc ~is_rec_type ~size ptyp_desc

and of_label_declaration
  ~is_rec_type
  ~size
  { pld_name = { txt = name; loc = _ }
  ; pld_mutable = _
  ; pld_type = { ptyp_desc; ptyp_attributes = _; _ }
  ; pld_loc = loc
  ; pld_attributes = _
  }
  =
  let type_name = of_core_type_desc ~loc ~is_rec_type ~size ptyp_desc in
  (name, [%expr [%e type_name]])

let sized_fun ~loc ~fun_name ~param_types body =
  let (module A) = Ast_builder.make loc in
  let pat = A.pvar fun_name in
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
        let poly_gen = A.pvar pfn in
        [%expr fun [%p poly_gen] -> [%e body]]
      | _ -> failwith "More than one param"
    in
    List.fold_left accum size_fun param_types
  in
  (A.value_binding ~pat ~expr, poly_funs)

let of_record ~is_rec_type ~loc ~fun_name ~param_types label_decls =
  of_label_declarations ~is_rec_type ~loc label_decls Fun.id
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
  ~is_rec_type
  ~size
  { pcd_name = { txt = name; _ }; pcd_args; pcd_loc = loc; _ }
  =
  let (module A) = Ast_builder.make loc in
  let constr_decl =
    let name = with_loc ~loc name in
    A.constructor_declaration ~name ~args:pcd_args ~res:None
  in
  let construct expr = [%expr [%e A.econstruct constr_decl (Some expr)]] in
  let exp =
    match pcd_args with
    | Pcstr_tuple [] ->
      [%expr Popper.Sample.return [%e A.econstruct constr_decl None]]
    | Pcstr_tuple ts -> of_tuple ~is_rec_type ~loc ~size ts construct
    | Pcstr_record ldl -> of_label_declarations ~is_rec_type ~loc ldl construct
  in
  (exp, has_rec_types ~is_rec_type pcd_args)

let of_variant ~is_rec_type ~loc ~fun_name ~param_types constrs =
  let size = [%expr size] in
  constrs
  |> List.map (of_constructor_declaration ~is_rec_type ~size)
  |> one_of_exp ~loc
  |> sized_fun ~loc ~fun_name ~param_types

let make_abstract ~loc ~is_rec_type ~fun_name ptype_manifest =
  match ptype_manifest with
  | Some t ->
    sized_fun
      ~loc
      ~fun_name
      ~param_types:[]
      (of_core_type ~is_rec_type ~size:[%expr size] t)
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
  let (module A) = Ast_builder.make loc in
  let fun_name = sample_name type_name in
  let param_types = List.map fst ptype_params in
  let sized, poly_gens =
    match ptype_kind with
    | Ptype_record fields ->
      of_record ~is_rec_type ~loc ~fun_name ~param_types fields
    | Ptype_variant constrs ->
      of_variant ~is_rec_type ~loc ~fun_name ~param_types constrs
    | Ptype_abstract -> make_abstract ~loc ~is_rec_type ~fun_name ptype_manifest
    | _ -> failwith "Unsupported type-kind"
  in
  let alias =
    let pat = A.pvar fun_name in
    let body =
      let type_constraint =
        let li = Ldot (Ldot (Lident "Popper", "Sample"), "t") in
        let type_vars =
          poly_gens
          |> List.mapi (fun ix _ -> A.ptyp_var @@ Printf.sprintf "a%d" ix)
        in
        A.ptyp_constr
          (with_loc ~loc li)
          [ A.ptyp_constr (with_loc ~loc (Lident type_name)) type_vars ]
      in
      let expr =
        let accum exp p = [%expr [%e exp] [%e A.evar p]] in
        List.fold_left accum [%expr [%e A.evar fun_name]] poly_gens
      in
      A.pexp_constraint [%expr Popper.Sample.sized [%e expr]] type_constraint
    in
    let expr =
      let accum exp p = [%expr fun [%p A.pvar p] -> [%e exp]] in
      List.fold_left accum body poly_gens
    in
    A.value_binding ~pat ~expr
  in
  { type_name; sized; alias; num_poly = List.length poly_gens }

let of_type_declarations ~is_rec_type =
  List.map (of_type_declaration ~is_rec_type)

let comparator ~loc { type_name; num_poly; _ } =
  let (module A) = Ast_builder.make loc in
  let expr =
    let accum (e1, e2) ix =
      let compare_poly = Printf.sprintf "compare_poly_%d" ix in
      let pp_poly = Printf.sprintf "pp_poly_%d" ix in
      ( [%expr [%e e1] [%e A.evar compare_poly]]
      , [%expr [%e e2] [%e A.evar pp_poly]] )
    in
    let zero =
      (A.evar @@ compare_name type_name, A.evar @@ pp_name type_name)
    in
    let ixs = List.rev @@ List.init num_poly Fun.id in
    let pcompare, ppp = List.fold_left accum zero ixs in
    let body = [%expr Popper.Comparator.make [%e pcompare] [%e ppp]] in
    let accum exp ix =
      let compare_poly = Printf.sprintf "compare_poly_%d" ix in
      let pp_poly = Printf.sprintf "pp_poly_%d" ix in
      [%expr fun [%p A.pvar compare_poly] [%p A.pvar pp_poly] -> [%e exp]]
    in
    List.fold_left accum body ixs
  in
  A.value_binding ~pat:(A.pvar @@ comparator_name type_name) ~expr

let sized bindings = bindings |> List.map (fun { sized; _ } -> sized)
let aliases bindings = bindings |> List.map (fun { alias; _ } -> alias)

let comparators ~loc bindings =
  let (module A) = Ast_builder.make loc in
  List.map (comparator ~loc) bindings |> A.pstr_value_list ~loc Nonrecursive

let samples_and_comparators ~loc (rec_flag, type_declarations) =
  let (module A) = Ast_builder.make loc in
  let rec_flag = really_recursive rec_flag type_declarations in
  let is_rec_type =
    let ts =
      type_declarations |> List.map (fun { ptype_name = { txt; _ }; _ } -> txt)
    in
    fun t -> rec_flag = Recursive && List.exists (String.equal t) ts
  in
  let bindings = of_type_declarations ~is_rec_type type_declarations in
  let samples =
    A.pstr_value_list ~loc rec_flag (sized bindings)
    @ A.pstr_value_list ~loc Nonrecursive (aliases bindings)
  in
  let comparators = comparators ~loc bindings in
  (samples, comparators)

let sample_popper ~ctxt:_ decls =
  let loc = Location.none in
  let gs, cs = samples_and_comparators ~loc decls in
  gs @ cs

let sample_samples ~ctxt:_ decls =
  let loc = Location.none in
  fst @@ samples_and_comparators ~loc decls

let sample_comparators ~ctxt:_ decls =
  let loc = Location.none in
  snd @@ samples_and_comparators ~loc decls

let popper = Deriving.Generator.V2.make_noarg sample_popper
let sample = Deriving.Generator.V2.make_noarg sample_samples
let comparator = Deriving.Generator.V2.make_noarg sample_comparators
let _ = Deriving.add "sample" ~str_type_decl:sample
let _ = Deriving.add "comparator" ~str_type_decl:comparator
let _ = Deriving.add "popper" ~str_type_decl:popper
