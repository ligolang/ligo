open Lsp_helpers
open Handler

(* E.g. when [type t = A | B], the type info for A would have
   var_name [Some <tvar for t>]
   and contents A | B (which is a TSum)  *)
type type_info =
  { var_name : Ast_core.type_expression option
  ; contents : Ast_core.type_expression
  }

(* Use most compact type expression available *)
let use_var_name_if_availiable : type_info -> Ast_core.type_expression =
 fun { var_name; contents } -> Option.value ~default:contents var_name


let get_type (vdef : Scopes.Types.vdef) : type_info option =
  match vdef.t with
  | Core ty -> Some { var_name = None; contents = ty }
  | Resolved ({ location; _ } as ty) ->
    let orig_var =
      Option.map
        ~f:(fun x -> Ast_core.{ type_content = T_variable x; location })
        ty.orig_var (* This is non-empty in case there is a name for our type *)
    in
    Some
      { var_name = orig_var
      ; contents =
          (* We want to preserve both the type var and type expression here, so we
            set [use_orig_var = True] so this expression will be pretty,
            and we also set [orig_var = None] before untyping so we're getting
            full expression and not just `T_variable` *)
          Checking.untype_type_expression ~use_orig_var:true { ty with orig_var = None }
      }
  | Unresolved -> None


let on_req_type_definition : Position.t -> Path.t -> Locations.t option Handler.t =
 fun pos file ->
  with_cached_doc file None
  @@ fun { definitions; _ } ->
  when_some' (Go_to_definition.get_definition pos file definitions)
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
       get_type vdef
       >>= fun type_expression ->
       let location =
         Def.Def_location.of_loc (use_var_name_if_availiable type_expression).location
       in
       (match location with
       | StdLib _ | Virtual _ ->
         None (* We can't return any position to user: type of this vdef is inferred *)
       | File { range; path } ->
         Option.some
         @@ Option.value ~default:Def.Loc_in_file.{ range; path }
         @@ (Go_to_definition.get_definition range.start file definitions
            >>= from_def_location))
     | Module _mdef -> None)
  @@ fun { range; path } ->
  return @@ Some (`Location [ Location.create ~range ~uri:(DocumentUri.of_path path) ])
