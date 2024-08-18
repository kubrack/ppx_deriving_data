open Ppxlib
open Ast_builder.Default

let random_string_expr ~loc () =
  [%expr String.init (Random.int 10 + 1) (fun _ -> char_of_int (Random.int 26 + 97))]

let random_int_expr ~loc () =
  [%expr Random.int 100]

let rec random_expr_of_core_type ~loc ctyp =
  match ctyp.ptyp_desc with
  | Ptyp_constr ({ txt = Lident "int"; _ }, _) -> random_int_expr ~loc ()
  | Ptyp_constr ({ txt = Lident "string"; _ }, _) -> random_string_expr ~loc ()
  | Ptyp_constr ({ txt = Lident "list"; _ }, [ty]) ->
      let gen_expr = random_expr_of_core_type ~loc ty in
      [%expr List.init (Random.int 5 + 1) (fun _ -> [%e gen_expr])]
  | Ptyp_tuple typs ->
      let gens = List.map (random_expr_of_core_type ~loc) typs in
      pexp_tuple ~loc gens
  | Ptyp_constr ({ txt = Lident type_name; _ }, _) ->
      let fn_name = "random_" ^ type_name in
      [%expr [%e evar ~loc fn_name] ()]
  | _ -> Location.raise_errorf ~loc "Type not supported"

let generate_impl ~ctxt (_rec_flag, type_declarations) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  List.concat_map (fun (type_decl : type_declaration) ->
      let type_name = type_decl.ptype_name.txt in
      let body =
        match type_decl.ptype_kind with
        | Ptype_record labels ->
            let gens =
              labels
              |> List.map (fun ld ->
                  let field_name = ld.pld_name.txt in
                  let gen = random_expr_of_core_type ~loc ld.pld_type in
                  (Located.lident ~loc field_name, gen)
                )
            in
            pexp_record ~loc gens None
        | _ -> Location.raise_errorf ~loc "Only record types are supported"
      in
      let func_name = "random_" ^ type_name in
      let body = [%expr fun () -> [%e body]] in
      [pstr_value ~loc Nonrecursive 
         [value_binding ~loc ~pat:(pvar ~loc func_name) ~expr:body]])
    type_declarations

let impl_generator = Deriving.Generator.V2.make_noarg generate_impl

let random_data =
  Deriving.add "random_data" ~str_type_decl:impl_generator

