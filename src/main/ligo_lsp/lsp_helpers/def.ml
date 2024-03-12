open Imports

type t = Scopes.def
type definitions = Scopes.definitions

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

  let of_loc : Loc.t -> t = function
    | File region when Helpers_file.is_stdlib region#file ->
      StdLib { range = Range.of_region region }
    | File region ->
      File { range = Range.of_region region; path = Path.from_absolute region#file }
    | Virtual s -> Virtual s


  let pp : t Fmt.t = fun ppf -> Format.fprintf ppf "%a" Sexp.pp <@ sexp_of_t
end

module Def_locations = Set.Make (Def_location)

let to_string (def : t) = Format.asprintf "%a" Scopes.PP.definitions [ def ]

let get_location : Scopes.def -> Def_location.t =
  Def_location.of_loc <@ Scopes.Types.get_range


let get_path : Scopes.def -> Path.t option =
  Def_location.(
    function
    | File { path; _ } -> Some path
    | StdLib _ | Virtual _ -> None)
  <@ get_location


let references_getter : Scopes.def -> Def_locations.t =
 fun def ->
  let module LSet = Scopes.Types.LSet in
  let lset =
    match def with
    | Variable vdef -> LSet.add vdef.range vdef.references
    | Type tdef -> LSet.add tdef.range tdef.references
    | Module mdef -> LSet.add mdef.range mdef.references
    | Label ldef -> LSet.add ldef.range ldef.references
  in
  Def_locations.of_sequence
  @@ Sequence.map ~f:Def_location.of_loc
  @@ Sequence.of_seq (LSet.to_seq lset)


let is_reference : Position.t -> Path.t -> Scopes.def -> bool =
 fun pos file definition ->
  let check_pos : Def_location.t -> bool = function
    | File { path; range } -> Range.contains_position pos range && Path.equal path file
    | StdLib _ | Virtual _ -> false
  in
  Def_locations.exists ~f:check_pos @@ references_getter definition


let rec fold_definitions : init:'a -> f:('a -> t -> 'a) -> definitions -> 'a =
 fun ~init ~f { definitions } ->
  List.fold_left
    ~init
    ~f:(fun acc def ->
      let acc = f acc def in
      match def with
      | Module { mod_case = Def definitions; _ } ->
        fold_definitions ~init:acc ~f { definitions }
      | Module { mod_case = Alias _; _ } | Variable _ | Type _ | Label _ -> acc)
    definitions


let find_map : f:(t -> 'a option) -> definitions -> 'a option =
 fun ~f ->
  fold_definitions ~init:None ~f:(fun acc def ->
      match acc with
      | Some _ -> acc
      | None -> f def)


let find : f:(t -> bool) -> definitions -> t option =
 fun ~f -> find_map ~f:(fun def -> Option.some_if (f def) def)


let filter_map : f:(t -> 'a option) -> definitions -> 'a list =
 fun ~f ->
  List.rev
  <@ fold_definitions ~init:[] ~f:(fun acc def ->
         Option.value_map ~default:acc ~f:(fun x -> x :: acc) (f def))


let filter : f:(t -> bool) -> definitions -> t list =
 fun ~f -> filter_map ~f:(fun def -> Option.some_if (f def) def)


let get_definition : Position.t -> Path.t -> definitions -> t option =
 fun pos uri definitions -> find ~f:(is_reference pos uri) definitions


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
      Simple_utils.Trace.to_option ~fast_fail:false
      @@ Checking.untype_type_expression
           ~use_orig_var:true
           { type_content; abbrev = None; location }
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
                        Simple_utils.Trace.to_option ~fast_fail:false
                        @@ Checking.untype_type_expression ~use_orig_var:true t)
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
