open Ppxlib
module A = Ast_builder.Default

let loc = Location.none

let fun_name n =
  if String.equal n "t" then
    "generate"
  else
    Printf.sprintf "generate_%s" n

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
  | s, _ -> failwith ("Unsupported type operator: " ^ s)

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

let of_record ~loc ~fun_name fields =
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
    [%expr Popper.Generator.return [%e A.pexp_record ~loc ident_exps None]]
  in
  let body = List.fold_right accum field_exprs record in
  let pat = A.pvar ~loc fun_name in
  let expr =
    [%expr
      fun size ->
        ignore size;
        [%e body]]
  in
  A.value_binding ~loc ~pat ~expr

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

let of_type_declaration
  { ptype_name = { txt = type_name; _ }
  ; ptype_params = _
  ; ptype_cstrs = _
  ; ptype_kind
  ; ptype_loc = loc
  ; ptype_private = _
  ; ptype_manifest
  ; ptype_attributes = _
  }
  =
  let fun_name = fun_name type_name in
  let fn =
    match ptype_kind with
    | Ptype_record fields -> of_record ~loc ~fun_name fields
    | Ptype_variant constrs -> of_variant ~loc ~fun_name constrs
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
        A.value_binding ~loc ~pat ~expr
      | None -> failwith "Unsupprted type kind")
    | _ -> failwith "Unsupported type kind"
  in
  let pat = A.pvar ~loc fun_name in
  let expr =
    let type_constraint =
      let li = Ldot (Ldot (Lident "Popper", "Generator"), "t") in
      A.ptyp_constr
        ~loc
        (with_loc ~loc li)
        [ A.ptyp_constr ~loc (with_loc ~loc (Lident type_name)) [] ]
    in
    A.pexp_constraint
      ~loc
      [%expr Popper.Generator.sized [%e A.evar ~loc fun_name]]
      type_constraint
  in
  (fn, A.value_binding ~loc ~pat ~expr)

let of_type_declarations = List.map of_type_declaration

let pp_name = function
  | "t" -> "pp"
  | name -> Printf.sprintf "pp_%s" name

let eq_name = function
  | "t" -> "equal"
  | name -> Printf.sprintf "equal_%s" name

let type_names =
  List.map (fun { ptype_name = { txt = type_name; _ }; _ } -> type_name)

let comparator_name = function
  | "t" -> "comparator"
  | name -> Printf.sprintf "%s_comparator" name

let comparator name =
  A.value_binding
    ~loc
    ~pat:(A.pvar ~loc @@ comparator_name name)
    ~expr:
      [%expr
        Popper.Comparator.make
          [%e A.evar ~loc @@ eq_name name]
          [%e A.evar ~loc @@ pp_name name]]

let sized ~rec_flag bindings = bindings |> A.pstr_value_list ~loc rec_flag
let aliases bindings = bindings |> A.pstr_value_list ~loc Nonrecursive

let comparators type_names =
  List.map comparator type_names |> A.pstr_value_list ~loc Nonrecursive

let generate_impl ~ctxt:_ (rec_flag, type_declarations) =
  let bindings = of_type_declarations type_declarations in
  let rec_flag = really_recursive rec_flag type_declarations in
  let sized = sized ~rec_flag @@ List.map fst bindings in
  let aliases = aliases @@ List.map snd bindings in
  let type_names = type_names type_declarations in
  let comparators = comparators type_names in
  sized @ aliases @ comparators

let impl_generator = Deriving.Generator.V2.make_noarg generate_impl
let my_deriver = Deriving.add "popper" ~str_type_decl:impl_generator
