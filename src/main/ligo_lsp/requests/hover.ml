open Handler
open Lsp.Types
open Utils

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


let on_req_hover : Position.t -> DocumentUri.t -> Hover.t option Handler.t =
 fun pos uri ->
  with_cached_doc uri None
  @@ fun { get_scope_info; _ } ->
  when_some' (Go_to_definition.get_definition pos uri get_scope_info.definitions)
  @@ fun definition ->
  when_some' (get_syntax uri)
  @@ fun syntax ->
  let syntax_highlight = Syntax.to_string syntax in
  let hover_string = hover_string syntax definition in
  let marked_string : MarkedString.t =
    { value = Format.sprintf "```%s\n" syntax_highlight ^ hover_string ^ "\n```"
    ; language = None
    }
  in
  let contents = `MarkedString marked_string in
  let hover = Hover.create ~contents () in
  return (Some hover)
