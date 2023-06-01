open Lsp_helpers
open Handler
module Loc = Simple_utils.Location

let get_type (vdef : Scopes.Types.vdef) : Ast_core.type_expression option =
  match vdef.t with
  | Core ty -> Some ty
  | Resolved ty -> Option.some @@ Checking.untype_type_expression ty
  | Unresolved -> None


let on_req_type_definition : Position.t -> Path.t -> Locations.t option Handler.t =
 fun pos file ->
  with_cached_doc file None
  @@ fun { get_scope_info; _ } ->
  when_some' (Go_to_definition.get_definition pos file get_scope_info.definitions)
  @@ fun def ->
  when_some'
    (let from_def_location : Def.t -> Def.Def_location.loc_in_file option =
      fun def ->
       match Def.get_location def with
       | StdLib _ | Virtual _ -> None
       | File { range; path } -> Some { range; path }
     in
     match def with
     | Type _ -> from_def_location def
     | Variable vdef ->
       let open Option.Monad_infix in
       get_type vdef
       >>= fun type_expression ->
       let location = Def.Def_location.of_loc type_expression.location in
       (match location with
       | StdLib _ | Virtual _ ->
         None (* We can't return any position to user: type of this vdef is inferred *)
       | File { range; path } ->
         Option.some
         @@ Option.value ~default:Def.Def_location.{ range; path }
         @@ (Go_to_definition.get_definition range.start file get_scope_info.definitions
            >>= from_def_location))
     | Module _mdef -> None)
  @@ fun { range; path } ->
  return @@ Some (`Location [ Location.create ~range ~uri:(DocumentUri.of_path path) ])
