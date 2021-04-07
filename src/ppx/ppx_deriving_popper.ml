open Ppxlib
module A = Ast_builder.Default

let loc = Location.none

let fun_name n =
  if String.equal n "t" then
    "generate"
  else
    Printf.sprintf "generate_%s" n

let poly_fun_name n = Printf.sprintf "generate_poly_%s" n

let generator_of_type ~size ~loc = function
  | "int" -> [%expr Popper.Generator.int]
  | "string" -> [%expr Popper.Generator.string]
  | "bool" -> [%expr Popper.Generator.bool]
  | "float" -> [%expr Popper.Generator.float]
  | "int32" -> [%expr Popper.Generator.int32]
  | t ->
    let e = A.evar ~loc @@ fun_name t in
    [%expr Popper.Generator.delayed (fun () -> [%e e] [%e size])]

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

and of_tuple ~loc types f =
  let size = [%expr size / [%e A.eint ~loc @@ List.length types]] in
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
  | s, ts ->
    let accum exp t = [%expr [%e exp] [%e of_core_type ~size t]] in
    List.fold_left accum (A.evar ~loc @@ fun_name s) ts

and of_row_field_desc = function
  | Rtag (name, _, []) ->
    [%expr Popper.Generator.return [%e A.pexp_variant ~loc name.txt None]]
  | Rtag (name, _, cts) ->
    of_tuple ~loc cts
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
  | Ptyp_tuple ts -> of_tuple ~loc ts Fun.id
  | Ptyp_alias (t, _) -> of_core_type ~size t
  | Ptyp_variant (row_fields, _, _) ->
    let exps = of_row_fields row_fields in
    [%expr
      if size <= 0 then
        [%e List.hd exps]
      else
        Popper.Generator.one_of [%e A.elist ~loc exps]]
  | Ptyp_poly _ -> failwith "Unsupported Ptype_poly"
  | Ptyp_any -> failwith "Any"
  | Ptyp_var v -> A.evar ~loc @@ poly_fun_name v
  | _ -> failwith "Deriving popper failed for type"

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

let of_record ~loc ~fun_name param_types label_decls =
  let body = of_label_declarations ~loc label_decls Fun.id in
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
    (* fun poly_gen1 poly_gen2 size -> ...*)
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

let with_loc ~loc txt = { txt; loc }

let of_constructor_declaration
  ~size:_
  { pcd_name = { txt = name; _ }; pcd_args; pcd_res = _; _ }
  =
  let constr_decl =
    let name = with_loc ~loc name in
    A.constructor_declaration ~loc ~name ~args:pcd_args ~res:None
  in
  let construct expr = [%expr [%e A.econstruct constr_decl (Some expr)]] in
  match pcd_args with
  | Pcstr_tuple [] ->
    [%expr Popper.Generator.return [%e A.econstruct constr_decl None]]
  | Pcstr_tuple ts -> of_tuple ~loc ts construct
  | Pcstr_record ldl -> of_label_declarations ~loc ldl construct

let of_variant ~loc ~fun_name constrs =
  let size = [%expr (size - 1) / [%e A.eint ~loc (List.length constrs)]] in
  let exps = List.map (of_constructor_declaration ~size) constrs in
  let pat = A.pvar ~loc fun_name in
  let body = A.elist ~loc exps in
  let expr =
    [%expr
      fun size ->
        if size <= 0 then
          [%e List.hd exps]
        else
          Popper.Generator.one_of [%e body]]
  in
  A.value_binding ~loc ~pat ~expr

type bindings =
  { type_name : string
  ; sized : value_binding
  ; alias : value_binding
  ; num_poly : int
  }

let of_type_declaration
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
  let fun_name = fun_name type_name in
  let sized, poly_gens =
    match ptype_kind with
    | Ptype_record fields ->
      of_record ~loc ~fun_name (List.map fst ptype_params) fields
    | Ptype_variant constrs -> (of_variant ~loc ~fun_name constrs, [])
    | Ptype_abstract ->
      (match ptype_manifest with
      | Some t ->
        let expr =
          [%expr
            fun size ->
              ignore size;
              [%e of_core_type ~size:[%expr size] t]]
        in
        let pat = A.pvar ~loc fun_name in
        (A.value_binding ~loc ~pat ~expr, [])
      | None -> failwith "Unsupprted type kind")
    | _ -> failwith "Unsupported type kind"
  in
  let alias =
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
        List.fold_left accum [%expr [%e A.evar ~loc fun_name]] poly_gens
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

let of_type_declarations = List.map of_type_declaration

let pp_name = function
  | "t" -> "pp"
  | name -> Printf.sprintf "pp_%s" name

let eq_name = function
  | "t" -> "equal"
  | name -> Printf.sprintf "equal_%s" name

let comparator_name = function
  | "t" -> "comparator"
  | name -> Printf.sprintf "%s_comparator" name

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
    let body = [%expr fun a b -> Popper.Comparator.make [%e peq] [%e ppp]] in
    let accum exp ix =
      let eq_poly = Printf.sprintf "eq_poly_%d" ix in
      let pp_poly = Printf.sprintf "pp_poly_%d" ix in
      [%expr fun [%p A.pvar ~loc eq_poly] [%p A.pvar ~loc pp_poly] -> [%e exp]]
    in
    List.fold_left accum body ixs
  in
  A.value_binding ~loc ~pat:(A.pvar ~loc @@ comparator_name type_name) ~expr

let sized ~rec_flag bindings =
  bindings
  |> List.map (fun { sized; _ } -> sized)
  |> A.pstr_value_list ~loc rec_flag

let aliases bindings =
  bindings
  |> List.map (fun { alias; _ } -> alias)
  |> A.pstr_value_list ~loc Nonrecursive

let comparators bindings =
  List.map comparator bindings |> A.pstr_value_list ~loc Nonrecursive

let generate_impl ~ctxt:_ (rec_flag, type_declarations) =
  let rec_flag = really_recursive rec_flag type_declarations in
  let bindings = of_type_declarations type_declarations in
  let sized = sized ~rec_flag bindings in
  let aliases = aliases bindings in
  let comparators = comparators bindings in
  sized @ aliases @ comparators

let impl_generator = Deriving.Generator.V2.make_noarg generate_impl
let my_deriver = Deriving.add "popper" ~str_type_decl:impl_generator
