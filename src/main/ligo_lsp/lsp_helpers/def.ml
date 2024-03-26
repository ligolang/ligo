open Imports

type t = Scopes.def
type definitions = Scopes.definitions
type 'a fold_control = 'a Cst_shared.Fold.fold_control

(* TODO use this in Scopes instead of `Loc` and `LSet` *)

module Loc_in_file = struct
  type t =
    { path : Path.t
    ; range : Range.t
    }
  [@@deriving eq, ord, sexp]

  let pp : t Fmt.t = fun ppf -> Format.fprintf ppf "%a" Sexp.pp <@ sexp_of_t
end

module Def_location = struct
  type t =
    | File of Loc_in_file.t
    | StdLib of { range : Range.t }
    | Virtual of string
  [@@deriving eq, ord, sexp]

  let of_loc : normalize:(string -> Path.t) -> Loc.t -> t =
   fun ~normalize -> function
    | File region when Helpers_file.is_stdlib region#file ->
      StdLib { range = Range.of_region region }
    | File region -> File { range = Range.of_region region; path = normalize region#file }
    | Virtual s -> Virtual s


  let pp : t Fmt.t = fun ppf -> Format.fprintf ppf "%a" Sexp.pp <@ sexp_of_t
end

module Def_locations = Set.Make (Def_location)

let to_string (def : t) = Format.asprintf "%a" Scopes.PP.definitions [ def ]

let get_location : normalize:(string -> Path.t) -> Scopes.def -> Def_location.t =
 fun ~normalize -> Def_location.of_loc ~normalize <@ Scopes.Types.get_range


let get_path : normalize:(string -> Path.t) -> Scopes.def -> Path.t option =
 fun ~normalize ->
  Def_location.(
    function
    | File { path; _ } -> Some path
    | StdLib _ | Virtual _ -> None)
  <@ get_location ~normalize


let references_getter : normalize:(string -> Path.t) -> Scopes.def -> Def_locations.t =
 fun ~normalize def ->
  let module LSet = Scopes.Types.LSet in
  let lset =
    match def with
    | Variable vdef -> LSet.add vdef.range vdef.references
    | Type tdef -> LSet.add tdef.range tdef.references
    | Module mdef -> LSet.add mdef.range mdef.references
    | Label ldef -> LSet.add ldef.range ldef.references
  in
  Def_locations.of_sequence
  @@ Sequence.map ~f:(Def_location.of_loc ~normalize)
  @@ Sequence.of_seq (LSet.to_seq lset)


let is_reference
    : normalize:(string -> Path.t) -> Position.t -> Path.t -> Scopes.def -> bool
  =
 fun ~normalize pos file definition ->
  let check_pos : Def_location.t -> bool = function
    | File { path; range } -> Range.contains_position pos range && Path.equal path file
    | StdLib _ | Virtual _ -> false
  in
  Def_locations.exists ~f:check_pos @@ references_getter ~normalize definition


(** Fold over definitions, flattening them along the fold. The [fold_control] allows you
    to choose whether to continue or stop (with or without accumulating the new value) the
    fold into that definition's children, in case it is a module. *)
let fold_definitions : init:'a -> f:('a -> t -> 'a fold_control) -> definitions -> 'a =
 fun ~init ~f { definitions } ->
  let rec go init =
    List.fold ~init ~f:(fun acc (def : t) ->
        let[@inline] fold_inner acc =
          match def with
          | Module { mod_case = Def definitions; _ } -> go acc definitions
          | Module { mod_case = Alias _; _ } | Variable _ | Type _ | Label _ -> acc
        in
        match f acc def with
        | Stop -> acc
        | Skip -> fold_inner acc
        | Continue acc -> fold_inner acc
        | Last acc -> acc)
  in
  go init definitions


let find_map : f:(t -> 'a option) -> definitions -> 'a option =
 fun ~f ->
  fold_definitions ~init:None ~f:(fun acc def ->
      match acc with
      | Some _ -> Last acc
      | None -> Continue (f def))


let find : f:(t -> bool) -> definitions -> t option =
 fun ~f -> find_map ~f:(fun def -> Option.some_if (f def) def)


let filter_map : f:(t -> 'a option) -> definitions -> 'a list =
 fun ~f ->
  List.rev
  <@ fold_definitions ~init:[] ~f:(fun acc def ->
         Continue (Option.value_map ~default:acc ~f:(fun x -> x :: acc) (f def)))


let filter : f:(t -> bool) -> definitions -> t list =
 fun ~f -> filter_map ~f:(fun def -> Option.some_if (f def) def)


let filter_file : file:Path.t -> definitions -> t list =
 fun ~file ->
  fold_definitions ~init:[] ~f:(fun acc def ->
      match Scopes.Types.get_decl_range def with
      | File region ->
        if Path.(equal (from_absolute region#file) file)
        then Continue (def :: acc)
        else Stop
      | Virtual _ -> Stop)


let get_definition
    : normalize:(string -> Path.t) -> Position.t -> Path.t -> definitions -> t option
  =
 fun ~normalize pos path definitions ->
  find ~f:(is_reference ~normalize pos path) definitions


(* E.g. when [type t = A | B], the type info for A would have
   var_name [Some <tvar for t>]
   and contents A | B (which is a TSum)  *)
type type_info =
  { var_name : Ast_core.type_expression option
  ; contents : Ast_core.type_expression
  }

(* Use most compact type expression available *)
let use_var_name_if_available : type_info -> Ast_core.type_expression =
 fun { var_name; contents } -> Option.value ~default:contents var_name


(** Get the [type_info] from [vdef.t]. If the type is [Resolved] and [use_module_accessor]
    is [true], then this function will try to create a [T_module_accessor] using the
    module path in which the [orig_var] was defined. For example, if we have
    [module M = struct type t = int end], then the user will see [M.t] when hovering over
    something of this type. Returns [None] if the type is [Unresolved]. *)
let get_type ~(use_module_accessor : bool) (vdef : Scopes.Types.vdef) : type_info option =
  match vdef.t with
  | Core contents -> Some { var_name = None; contents }
  | Resolved { type_content; abbrev; location } ->
    let%bind.Option contents =
      (* We want to preserve both the type var and type expression here, so we set
         [use_orig_var = True] so this expression will be pretty, and we also set
         [orig_var = None] before untyping so we're getting full expression and not just
         [T_variable]. *)
      try
        Simple_utils.Trace.to_option ~fast_fail:false
        @@ Checking.untype_type_expression
             ~use_orig_var:true
             { type_content; abbrev = None; location }
      with
      | _exn -> None
    in
    Some
      { var_name =
          (* This is non-empty in case there is a name for our type. *)
          Option.map abbrev ~f:(fun { orig_var = module_path, element; applied_types } ->
              let type_content : Ast_core.type_content =
                let module_path = if use_module_accessor then module_path else [] in
                match applied_types, module_path with
                | _ :: _, _ ->
                  let type_operator = Ligo_prim.Module_access.{ module_path; element } in
                  let arguments =
                    List.filter_map applied_types ~f:(fun t ->
                        try
                          Simple_utils.Trace.to_option ~fast_fail:false
                          @@ Checking.untype_type_expression ~use_orig_var:true t
                        with
                        | _exn -> None)
                  in
                  T_app { type_operator; arguments }
                | [], _ :: _ -> T_module_accessor { module_path; element }
                | [], [] -> T_variable element
              in
              Ast_core.{ type_content; location })
      ; contents
      }
  | Unresolved -> None


let get_comments : t -> string list = function
  | Variable vdef ->
    (match vdef.attributes with
    | Value_attr attr -> attr.leading_comments
    | Sig_item attr -> attr.leading_comments
    | No_attributes -> [])
  | Type tdef ->
    (match tdef.attributes with
    | Type_attr attr -> attr.leading_comments
    | Sig_type attr -> attr.leading_comments
    | No_attributes -> [])
  | Module mdef ->
    (match mdef.attributes with
    | Module_attr attr -> attr.leading_comments
    | Signature_attr attr -> attr.leading_comments
    | No_attributes -> [])
  | Label _ -> []


module Hierarchy = struct
  type t = Scopes.def Rose.forest

  let create : normalize:(string -> Path.t) -> definitions -> t =
   fun ~normalize defs ->
    Rose.map_forest ~f:Tuple3.get3
    @@ Rose.forest_of_list
       (* See the expect test in [Rose] on why we compare and check for intersection
              like this. *)
         ~compare:(fun ((range1 : Range.t), file1, _def1) (range2, file2, _def2) ->
           let path_ord = Path.compare file1 file2 in
           if path_ord = 0
           then (
             let pos_ord = Position.compare range1.start range2.start in
             if pos_ord = 0 then Position.compare range2.end_ range1.end_ else pos_ord)
           else path_ord)
         ~intersects:(fun (range1, file1, _def1) (range2, file2, _def2) ->
           Path.equal file1 file2 && Position.compare range1.end_ range2.start > 0)
    @@ filter_map defs ~f:(fun def ->
           match Scopes.Types.get_decl_range def with
           | File region ->
             let range = Range.of_region region in
             let path = normalize region#file in
             Some (range, path, def)
           | Virtual _ -> None)


  let scope_at_point (file : Path.t) (point : Position.t) (mod_path : Scopes.Uid.t list)
      : t -> Scopes.def list
    =
    let shadow_defs : Scopes.def list -> Scopes.def list =
      List.filter_map ~f:List.hd
      <@ List.sort_and_group ~compare:Scopes.Types.compare_def_by_name
    in
    (* A def is interesting to us if either it's from another file, or it's from the same
       file but it's declaration happened before the current point. Also, it must be
       visible from our current module.
         There is a caveat: constructors are declared in the global scope, so we need to
       consider that they are of interest even if they aren't in the scope "spine".
         To handle this, we consider that types and constructors are always definitions of
       interest (so we can visit them), and that constructors are always parents of
       interest (so we always add them to the scope). Moreover, constructors declared in
       local type definitions are not added globally, so we want to add them only if we're
       completing a scope within a child's body, and hence we have to also ask whether we
       have children of interest. *)
    let is_def_of_interest (def : Scopes.def) : bool =
      match Scopes.Types.get_decl_range def with
      | File region ->
        (Position.(of_pos region#start <= point)
        || not Path.(equal file (from_absolute region#file)))
        &&
        (match def with
        | Label { label_case = Ctor; _ } | Type _ -> true
        | Label { label_case = Field; _ } | Variable _ | Module _ ->
          List.is_prefix
            mod_path
            ~equal:Scopes.Uid.equal
            ~prefix:(Scopes.Types.get_mod_path def))
      | Virtual _ -> false
    in
    (* We consider that a parent is of interest if it doesn't contain the point. Such
       parents are the ones in the "spine" of the scope, such as our current variable or
       module, which we don't want to show.
         The same caveat about constructors above apply here.
         TODO: Strictly speaking, we should check whether the definition is recursive or
       not, and show it in case it's recursive. *)
    let is_parent_of_interest (def : Scopes.def) : bool =
      match Scopes.Types.get_decl_range def with
      | File region ->
        let excludes_position = not Range.(contains_position point (of_region region)) in
        (match def with
        | Label { label_case = Ctor; _ } -> true
        | Label { label_case = Field; _ } | Variable _ | Type _ | Module _ ->
          excludes_position)
      | Virtual _ -> false
    in
    (* We only have this function because of global constructors. Only add constructors
       declared within a variable's body if our scope point is inside the variable in the
       first place. Otherwise, children are always valid. *)
    let are_children_of_interest (def : Scopes.def) : bool =
      match def with
      | Variable _ ->
        (match Scopes.Types.get_decl_range def with
        | File region -> Range.(contains_position point (of_region region))
        | Virtual _ -> false)
      | Type _ | Module _ | Label _ -> true
    in
    let rec go : t -> Scopes.def list = function
      | [] -> []
      | Rose.Tree ((parent, parents), children) :: ts ->
        if is_def_of_interest parent
        then
          if is_parent_of_interest parent
          then
            parent
            :: List.concat
                 [ parents
                 ; (if are_children_of_interest parent then go children else [])
                 ; go ts
                 ]
          else if are_children_of_interest parent
          then go children @ go ts
          else go ts
        else go ts
    in
    shadow_defs <@ go
end
