open Lsp_helpers
open Handler
module Trace = Simple_utils.Trace

let get_type (vdef : Scopes.Types.vdef) : Ast_core.type_expression option =
  match vdef.t with
  | Core ty -> Some ty
  | Resolved ty ->
    Trace.try_with
      ~fast_fail:false
      (fun ~raise ~catch:_ ->
        try Some (Checking.untype_type_expression ~raise ty) with
        | _exn -> None)
      (fun ~catch:_ _ -> None)
  | Unresolved -> None


let on_req_type_definition : Position.t -> Path.t -> Locations.t option Handler.t =
 fun pos file ->
  with_cached_doc file ~default:None
  @@ fun { definitions; _ } ->
  let@ normalize = ask_normalize in
  when_some' (Def.get_definition ~normalize pos file definitions)
  @@ fun def ->
  when_some'
    (let from_def_location : Def.t -> Def.Loc_in_file.t option =
      fun def ->
       match Def.get_location ~normalize def with
       | StdLib _ | Virtual _ -> None
       | File { range; path } -> Some { range; path }
     in
     match def with
     | Type _ -> from_def_location def
     | Variable vdef ->
       let open Option.Monad_infix in
       Def.get_type ~use_module_accessor:false vdef
       >>= fun type_expression ->
       let location =
         Def.Def_location.of_loc
           ~normalize
           (Def.use_var_name_if_available type_expression).location
       in
       (match location with
       | StdLib _ | Virtual _ ->
         None (* We can't return any position to user: type of this vdef is inferred *)
       | File { range; path } ->
         Option.some
         @@ Option.value ~default:Def.Loc_in_file.{ range; path }
         @@ (Def.get_definition ~normalize range.start file definitions
            >>= from_def_location))
     | Module _mdef -> None
     | Label ldef ->
       (match Def.Def_location.of_loc ~normalize ldef.orig_type_loc with
       | StdLib _ | Virtual _ -> None
       | File file_loc -> Some file_loc))
  @@ fun { range; path } ->
  return @@ Some (`Location [ Location.create ~range ~uri:(DocumentUri.of_path path) ])
