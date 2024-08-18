open Ppxlib
open Ast_builder.Default

let rec random_expr_of_type ~loc ctyp =
  match ctyp.ptyp_desc with
  | Ptyp_constr ({ txt = Lident "int"; _ }, _) -> [%expr Random.int 100]
  | Ptyp_constr ({ txt = Lident "float"; _ }, _) -> [%expr Random.float 100.]
  | Ptyp_constr ({ txt = Lident "bool"; _ }, _) -> [%expr Random.bool ()]
  | Ptyp_constr ({ txt = Lident "string"; _ }, _) -> 
      [%expr String.init (Random.int 10 + 1) (fun _ -> char_of_int (Random.int 26 + 97))]
  | Ptyp_constr ({ txt = Lident "option"; _ }, [ty]) ->
      let gen_expr = random_expr_of_type ~loc ty in
      [%expr (Some [%e gen_expr])]
  | Ptyp_constr ({ txt = Lident "list"; _ }, [ty]) ->
      let gen_expr = random_expr_of_type ~loc ty in
      [%expr List.init (Random.int 3 + 2) (fun _ -> [%e gen_expr])]
  | Ptyp_tuple typs ->
      let gens = List.map (random_expr_of_type ~loc) typs in
      pexp_tuple ~loc gens
  | Ptyp_constr ({ txt = Lident type_name; _ }, _) -> 
      let fn_name = "random_" ^ type_name in
      [%expr [%e evar ~loc fn_name] ()]
  | _ -> Location.raise_errorf ~loc "Type not supported"

let generate_impl ~ctxt (_rec_flag, type_declarations) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  let type_decl = List.nth type_declarations 0 in
      let type_name = type_decl.ptype_name.txt in
      let body =
        match type_decl.ptype_kind with
        | Ptype_record labels ->
            let gens =
              labels
              |> List.map (fun ld ->
                  let field_name = ld.pld_name.txt in
                  let gen = random_expr_of_type ~loc ld.pld_type in
                  (Located.lident ~loc field_name, gen)
                )
            in
            pexp_record ~loc gens None
        | _ -> Location.raise_errorf ~loc "Only record types are supported"
      in
      let func_name = "random_" ^ type_name in
      let body = [%expr fun () -> [%e body]] in
      [pstr_value ~loc Nonrecursive 
         [value_binding ~loc ~pat:(pvar ~loc func_name) ~expr:body]]

let impl_generator = Deriving.Generator.V2.make_noarg generate_impl

let random_data =
  Deriving.add "random_data" ~str_type_decl:impl_generator

