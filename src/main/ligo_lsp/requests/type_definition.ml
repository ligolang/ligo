open Lsp_helpers
open Handler

let on_req_type_definition : Position.t -> Path.t -> Locations.t option Handler.t =
 fun pos file ->
  with_cached_doc file ~default:None
  @@ fun { definitions; _ } ->
  when_some' (Def.get_definition pos file definitions)
  @@ fun def ->
  when_some'
    (let from_def_location : Def.t -> Def.Loc_in_file.t option =
      fun def ->
       match Def.get_location def with
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
         Def.Def_location.of_loc (Def.use_var_name_if_available type_expression).location
       in
       (match location with
       | StdLib _ | Virtual _ ->
         None (* We can't return any position to user: type of this vdef is inferred *)
       | File { range; path } ->
         Option.some
         @@ Option.value ~default:Def.Loc_in_file.{ range; path }
         @@ (Def.get_definition range.start file definitions >>= from_def_location))
     | Module _mdef -> None)
  @@ fun { range; path } ->
  return @@ Some (`Location [ Location.create ~range ~uri:(DocumentUri.of_path path) ])
