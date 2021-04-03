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

let rec of_tuple ~loc types f =
  let size = [%expr size / [%e A.eint ~loc @@ List.length types]] in
  let accum (name, value) body =
    [%expr
      let* [%p A.pvar ~loc name] = [%e value] in
      [%e body]]
  in
  let exprs = List.map (of_core_type ~size) types in
  let name_exp_list =
    List.mapi
      (fun i x ->
        let name = Printf.sprintf "x%d" i in
        name, x)
      exprs
  in
  let evars = List.map (fun (n, _) -> A.evar ~loc n) name_exp_list in
  let tuple =
    [%expr Popper.Generator.return [%e f @@ A.pexp_tuple ~loc evars]]
  in
  List.fold_right accum name_exp_list tuple

and of_applied_type ~size ~name ts =
  match name, ts with
  | "option", [ t ] -> [%expr Popper.Generator.option [%e of_core_type ~size t]]
  | "list", [ t ] -> [%expr Popper.Generator.list [%e of_core_type ~size t]]
  | "result", [ t1; t2 ] ->
    [%expr
      Popper.Generator.result
        ~ok:[%e of_core_type ~size t1]
        ~error:[%e of_core_type ~size t2]]
  | s, _ -> failwith ("Unsupported type operator: " ^ s)

and of_core_type_desc ~size = function
  | Ptyp_constr ({ txt = Lident name; loc }, []) ->
    generator_of_type ~size ~loc name
  | Ptyp_constr ({ txt = Lident name; _ }, ts) -> of_applied_type ~size ~name ts
  | Ptyp_arrow (_, _, t) ->
    let gen_exp = of_core_type ~size:[%expr size] t in
    [%expr Popper.Generator.arrow [%e gen_exp]]
  | Ptyp_tuple ts -> of_tuple ~loc ts Fun.id
  | Ptyp_alias (t, _) -> of_core_type ~size t
  (* 
  | Ptyp_variant of row_field list * closed_flag * label list option
  | Ptyp_poly of string loc list * core_type
  | Ptyp_package of package_type
  | Ptyp_extension of extension 
  *)
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
  name, [%expr [%e type_name]]

let of_record ~loc ~fun_name fields =
  let size = [%expr size / [%e A.eint ~loc @@ List.length fields]] in
  let accum (var, value) body =
    let name = A.pvar ~loc var in
    [%expr
      let* [%p name] = [%e value] in
      [%e body]]
  in
  let field_exprs = List.map (of_label_declaration ~size) fields in
  let ident_exps =
    List.map
      (fun (name, _) ->
        let exp = A.evar ~loc name in
        { txt = lident name; loc }, exp)
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
        let open Popper.Generator.Syntax in
        [%e body]]
  in
  A.value_binding ~loc ~pat ~expr

let with_loc ~loc txt = { txt; loc }

let of_constructor_declaration
  ~size:_
  { pcd_name = { txt = name; _ }; pcd_args; pcd_res = _; _ }
  =
  match pcd_args with
  | Pcstr_tuple ts ->
    let constr_decl =
      let name = with_loc ~loc name in
      A.constructor_declaration ~loc ~name ~args:pcd_args ~res:None
    in
    of_tuple ~loc ts
    @@ fun expr -> [%expr [%e A.econstruct constr_decl (Some expr)]]
  | _ -> failwith "Constructor not supported"

let of_variant ~loc ~fun_name constrs =
  let size = [%expr (size - 1) / [%e A.eint ~loc (List.length constrs)]] in
  let exps = List.map (of_constructor_declaration ~size) constrs in
  let pat = A.pvar ~loc fun_name in
  let body = A.elist ~loc exps in
  let expr =
    [%expr
      fun size ->
        let open Popper.Generator.Syntax in
        if size <= 0 then
          [%e List.hd exps]
        else
          Popper.Generator.one_of [%e body]]
  in
  A.value_binding ~loc ~pat ~expr

let of_type_declaration
  ({ ptype_name = { txt = type_name; _ }
   ; ptype_params = _
   ; ptype_cstrs = _
   ; ptype_kind
   ; ptype_loc = loc
   ; ptype_private = _
   ; ptype_manifest
   ; ptype_attributes = _
   } :
    type_declaration)
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
            fun _ ->
              let open Popper.Generator.Syntax in
              [%e of_core_type ~size:[%expr size] t]]
        in
        let pat = A.pvar ~loc fun_name in
        A.value_binding ~loc ~pat ~expr
      | None -> failwith "Unsupprted type kind")
    | _ -> failwith "Unsupported type kind"
  in
  ( fn
  , A.value_binding
      ~loc
      ~pat:(A.pvar ~loc fun_name)
      ~expr:[%expr Popper.Generator.sized [%e A.evar ~loc fun_name]] )

let of_type_declarations = List.map of_type_declaration

let generate_impl ~ctxt:_ (rec_flag, type_declarations) =
  let xs = of_type_declarations type_declarations in
  let rec_flag = really_recursive rec_flag type_declarations in
  let impl = xs |> List.map fst |> A.pstr_value_list ~loc rec_flag in
  let aliases = xs |> List.map snd |> A.pstr_value_list ~loc Nonrecursive in
  impl @ aliases

let impl_generator = Deriving.Generator.V2.make_noarg generate_impl
let my_deriver = Deriving.add "popper" ~str_type_decl:impl_generator
