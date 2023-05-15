open Lsp_helpers
open Handler
module Loc = Simple_utils.Location

let get_type (vdef : Scopes.Types.vdef) : Ast_core.type_expression option =
  match vdef.t with
  | Core ty -> Some ty
  | Resolved ty -> Option.some @@ Checking.untype_type_expression ty
  | Unresolved -> None


let on_req_type_definition : Position.t -> DocumentUri.t -> Locations.t option Handler.t =
 fun pos uri ->
  with_cached_doc uri None
  @@ fun { get_scope_info; _ } ->
  when_some' (Go_to_definition.get_definition pos uri get_scope_info.definitions)
  @@ fun definition ->
  when_some'
    (match definition with
    | Variable vdef ->
      let open Option.Monad_infix in
      get_type vdef
      >>= fun type_expression ->
      Option.some
      @@
      let location = type_expression.location in
      Option.value
        ~default:location
        (Position.from_location location
        >>= fun region ->
        Go_to_definition.get_definition region uri get_scope_info.definitions
        >>= function
        | Variable _vdef -> None
        | Module _mdef -> None
        | Type tdef -> Some tdef.range)
    | Type tdef -> Some tdef.range
    | Module _mdef -> None)
  @@ function
  | File region ->
    return
    (* stdlib ranges have an empty file name. They have no type definition location. *)
    @@ Option.some_if
         (not (Helpers_file.is_stdlib region#file))
         (`Location [ Location.of_region region ])
  | Virtual _ -> return None
