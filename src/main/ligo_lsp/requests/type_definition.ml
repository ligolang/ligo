open Handler
open Utils
module Loc = Simple_utils.Location

(* TODO: use Set from Core *)
module LSet = Caml.Set.Make (Loc)

let get_type (vdef : Scopes.Types.vdef) : Ast_core.type_expression option =
  match vdef.t with
  | Core ty -> Some ty
  | Resolved ty -> Option.some @@ Checking.untype_type_expression ty
  | Unresolved -> None


let on_req_type_definition
    :  Lsp.Types.Position.t -> Lsp.Types.DocumentUri.t
    -> Lsp.Types.Locations.t option Handler.t
  =
 fun pos uri ->
  with_cached_doc uri None
  @@ fun { get_scope_info; _ } ->
  when_some' (Definition.get_definition pos uri get_scope_info.definitions)
  @@ fun definition ->
  when_some'
    (match definition with
    | Variable vdef ->
      bind_option (get_type vdef)
      @@ fun type_expression ->
      Option.some
      @@
      let location = type_expression.location in
      Option.value ~default:location
      @@ bind_option (Utils.position_of_location location)
      @@ fun region ->
      bind_option
        (Definition.get_definition region uri get_scope_info.definitions)
        (function
          | Variable _vdef -> None
          | Module _mdef -> None
          | Type tdef -> Some tdef.range)
    | Type tdef -> Some tdef.range
    | Module _mdef -> None)
  @@ function
  | File region -> return @@ Some (`Location [ region_to_location region ])
  | Virtual _ -> return None
