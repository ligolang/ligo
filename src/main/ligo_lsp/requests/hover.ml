let hover_string : Syntax_types.t -> Scopes.def -> string =
 fun syntax ->
  let opening_comment, closing_comment = Utils.get_comment syntax in
  let type_to_string t = Utils.pp_type_expression ~syntax (`Core t) in
  function
  | Variable vdef ->
    Type_definition.get_type vdef
    |> Option.map ~f:type_to_string
    |> Option.value ~default:(opening_comment ^ " Unresolved " ^ closing_comment)
  | Type tdef -> type_to_string tdef.content
  | Module mdef -> Utils.print_module syntax mdef
