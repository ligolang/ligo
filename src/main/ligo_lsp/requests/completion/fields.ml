(** Completions for things that come after dot, like [List.le] or [person.na]
    This module finds if the cursor is located like this, finds corresponding record
    or module, and calls [Records] or [Modules]. *)

(* TODO: we should handle field completion using ast_typed rather than scopes *)
open Common
open Lsp_helpers
module Utils = Simple_utils.Utils
module Fold = Cst_shared.Fold

(** A module selection might be a module type at term level, type level, or module level. *)
type ('module_expr, 'module_type_expr, 'module_name) expr_kind =
  | Module_path_expr of 'module_expr
  | Module_path_type_expr of 'module_type_expr
  | Module_path_selection of 'module_name

(** We want to prove to the compiler that a module of type [Compatible_CST] is containing
    either a CameLIGO or JsLIGO CST. *)
type (_, _, _, _, _) cst_witness =
  | Witness_CameLIGO
      : ( Cst_cameligo.CST.cst
        , Cst_cameligo.CST.projection
        , Cst_cameligo.CST.expr Cst_cameligo.CST.module_path
        , Cst_cameligo.CST.type_expr Cst_cameligo.CST.module_path
        , Cst_cameligo.CST.module_name Cst_cameligo.CST.module_path )
        cst_witness
  | Witness_JsLIGO
      : ( Cst_jsligo.CST.cst
        , Cst_jsligo.CST.projection
        , Cst_jsligo.CST.expr Cst_jsligo.CST.namespace_path
        , Cst_jsligo.CST.type_expr Cst_jsligo.CST.namespace_path
        , Cst_jsligo.CST.namespace_name Cst_jsligo.CST.namespace_path )
        cst_witness

(** Module type to abstract similar nodes from the CameLIGO and JsLIGO CSTs. *)
module type Compatible_CST = sig
  (** Top-level CST node. *)
  type cst

  (** Expression node. *)
  type expr

  (** Type expression node. *)
  type type_expr

  (** Record projection node. *)
  type projection

  (** Record projection selection node. Note: this selection must include the dot (as in
      JsLIGO). *)
  type selection

  (* OCaml compiler wants concrete types to make the witness thing working *)

  (** Module/namespace path projection specialized to expressions. *)
  type expr_module_path

  (** Module/namespace path projection specialized to type expressions. *)
  type type_expr_module_path

  (** Module/namespace path projection specialized to modules. *)
  type module_name_path

  (** Extracts the region encompassing an expression. *)
  val expr_to_region : expr -> Region.t

  (** Extracts the projected expression (the LHS of the dot). *)
  val expr_of_projection : projection -> expr

  (** Attempts to extract the projection node out an expression. *)
  val try_get_projection : expr -> projection option

  (** Attempts to extract the dot lexeme out a selection. *)
  val dot_of_selection : selection -> dot option

  (* TODO: In the future, we should try to work with the expression before the dot rather
     than just the lexeme. This currently allows us to just complete from a simple record
     name, not arbitrary expressions. *)

  (** Attempts to extract the lexeme out a selection. This is the record name just before
      the dot lexeme. *)
  val lexeme_of_selection : selection -> lexeme option

  (** Returns a non-empty list containing the dot lexemes and selections of the given
      projection. *)
  val selections_of_projection : projection -> selection Utils.nseq

  (** Returns the field name lexemes from the provided [expr_kind]. *)
  val lexemes_of_module_path
    :  (expr_module_path, type_expr_module_path, module_name_path) expr_kind
    -> lexeme wrap list

  (** Extracts the field/property node that is accessed by the module path. *)
  val field_of_module_path : expr_module_path -> expr

  (** A runtime witness of the [cst_witness] type. *)
  val cst_witness
    : ( cst
      , projection
      , expr_module_path
      , type_expr_module_path
      , module_name_path )
      cst_witness
end

module C_CameLIGO : Compatible_CST with type cst = Cst_cameligo.CST.t = struct
  include Cst_cameligo.CST

  type nonrec selection = dot * selection
  type expr_module_path = expr module_path
  type type_expr_module_path = type_expr module_path
  type module_name_path = module_name module_path

  let expr_of_projection node = node.record_or_tuple

  let try_get_projection = function
    | E_Proj proj -> Some proj.value
    | _ -> None


  let dot_of_selection (dot, _expr) = Some dot

  let lexeme_of_selection (_dot, expr) =
    match expr with
    | FieldName v ->
      (match v with
      | Var name -> Some name#payload
      | Esc name -> Some ("@" ^ name#payload))
    | Component _ -> None


  let selections_of_projection node =
    let hd, tl = node.field_path in
    (node.selector, hd), tl


  let lexemes_of_module_path = function
    | Module_path_expr node -> Utils.nsepseq_to_list node.module_path
    | Module_path_type_expr node -> Utils.nsepseq_to_list node.module_path
    | Module_path_selection node -> Utils.nsepseq_to_list node.module_path


  let field_of_module_path node = node.field
  let cst_witness = Witness_CameLIGO
end

module C_JsLIGO : Compatible_CST with type cst = Cst_jsligo.CST.t = struct
  include Cst_jsligo.CST

  type expr_module_path = expr namespace_path
  type type_expr_module_path = type_expr namespace_path
  type module_name_path = namespace_name namespace_path

  let expr_of_projection node = node.object_or_array

  let try_get_projection = function
    | E_Proj proj -> Some proj.value
    | _ -> None


  let dot_of_selection = function
    | PropertyName (dot, _) -> Some dot
    | _ -> None


  let lexeme_of_selection = function
    | PropertyName (_dot, v) ->
      (match v with
      | Var name -> Some name#payload
      | Esc name -> Some ("@" ^ name#payload))
    | PropertyStr _ -> None
    | Component _ -> None


  let selections_of_projection node = node.property_path

  let lexemes_of_module_path = function
    | Module_path_expr (node : _ namespace_path) ->
      Utils.nsepseq_to_list node.namespace_path
    | Module_path_type_expr (node : _ namespace_path) ->
      Utils.nsepseq_to_list node.namespace_path
    | Module_path_selection (node : _ namespace_path) ->
      Utils.nsepseq_to_list node.namespace_path


  let field_of_module_path node = node.property
  let cst_witness = Witness_JsLIGO
end

(** Collects the absolute range of some CST node as well as the distance of that
    node to a given cursor. *)
type distance =
  { range : Range.t
  ; dist : Position.t
  }

(** Simultaneously calculate the [distance]s of the cursor to a dot lexeme as
    well as of the cursor to the last seen lexeme. *)
type completion_distance =
  { dot : distance option
  ; lexeme : distance option
  }

(** This monoid is used to choose between two [distance]s, picking the one that has the
    smallest distance (or the one with the smallest range in case the distances are the
    same). By negative distance, we mean a distance that is to the left of the reference. *)
let smallest_negative_distance_monoid : distance option Cst_shared.Fold.monoid =
  { empty = None
  ; append =
      (fun lhs rhs ->
        match lhs, rhs with
        | None, None -> None
        | x, None | None, x -> x
        | ( (Some { range = lhs_range; dist = lhs_dist } as lhs)
          , (Some { range = rhs_range; dist = rhs_dist } as rhs) ) ->
          (* Return the least distance between the two. *)
          (match Position.compare_ord lhs_dist rhs_dist with
          | Less -> rhs
          (* The two have equal distances, pick the smallest of them, i.e., the
             one whose start is closest to the position. *)
          | Equal ->
            if Position.is_to_the_left lhs_range.start rhs_range.start then rhs else lhs
          | Greater -> lhs))
  }


(** Just like [smallest_negative_distance_monoid], but works on a [completion_distance],
    calculating it for both the [dot] and [lexeme]. *)
let completion_distance_monoid : completion_distance Cst_shared.Fold.monoid =
  let dist_monoid = smallest_negative_distance_monoid in
  { empty = { dot = dist_monoid.empty; lexeme = dist_monoid.empty }
  ; append =
      (fun { dot = dot_lhs; lexeme = lexeme_lhs } { dot = dot_rhs; lexeme = lexeme_rhs } ->
        { dot = dist_monoid.append dot_lhs dot_rhs
        ; lexeme = dist_monoid.append lexeme_lhs lexeme_rhs
        })
  }


(** The CST for a record projection or module path might be in a tree structure, which is
    difficult to work with. This data type represents that the structure was flattened
    into lists for ease of development. *)
type linearized_path =
  | Projection of (Position.t * lexeme option list)
      (** A flattened record projection. The [Position.t] indicates the position of the
          record being projected. The [lexeme option list] represents each lexeme after
          the dots, which may as well be module names. The [option] is due to non-record
          fields (e.g., list/tuple access). For example, if we have ["A.B.C.d.e.f"], then
          the list will contain [[ "A"; "B"; "C"; "e"; "f"]], and the position will have
          that of the missing ["d"]. *)
  | Module of (Modules.def_scope * lexeme wrap list)
      (** A flattened module path. The [Modules.def_scope] indicates the level of the
          module path, and the [lexeme wrap list] represent each module name being
          accessed. *)

(** Tries to get the record projection or module path of the given input. *)
let get_linearized_path
    (type a)
    (module C : Compatible_CST with type cst = a)
    ({ cst; pos; _ } : a Common.input)
    : linearized_path option
  =
  (* Find the greatest dot position that is less than or equal to the position
     of the cursor. *)
  let farthest_dot_position_before_cursor, farthest_lexeme_position_before_cursor =
    let empty = completion_distance_monoid.empty in
    let mk_dist region =
      (* Calculate the least distance between [range] and [pos]. Returns [None] if
         [pos] is to the right of [range]. *)
      let distance_to_pos (pos : Position.t) (range : Range.t) : Position.t option =
        match Position.compare_ord range.end_ pos with
        | Less | Equal ->
          Some
            (Position.create
               ~line:(range.end_.line - pos.line)
               ~character:(range.end_.character - pos.character))
        | Greater -> None
      in
      let range = Range.of_region region in
      Option.map (distance_to_pos pos range) ~f:(fun dist -> { range; dist })
    in
    let open Fold in
    let { dot; lexeme } =
      match C.cst_witness with
      | Witness_CameLIGO ->
        let open Cst_cameligo.Fold in
        let collect (Some_node (node, sing)) =
          match sing with
          | S_dot -> Last { empty with dot = mk_dist node#region }
          (* If we are writing at the end of the file, we don't want to consider
             the [eof] [lexeme wrap]. *)
          | S_eof -> Stop
          | S_wrap _ -> Last { empty with lexeme = mk_dist node#region }
          | S_reg _ -> Continue { empty with lexeme = mk_dist node.region }
          | _ -> Skip
        in
        fold_map_cst completion_distance_monoid collect cst
      | Witness_JsLIGO ->
        let open Cst_jsligo.Fold in
        let collect (Some_node (node, sing)) =
          match sing with
          | S_dot -> Last { empty with dot = mk_dist node#region }
          | S_eof -> Stop
          | S_wrap _ -> Last { empty with lexeme = mk_dist node#region }
          | S_reg _ -> Continue { empty with lexeme = mk_dist node.region }
          | _ -> Skip
        in
        fold_map_cst completion_distance_monoid collect cst
    in
    Option.value_map (Option.both dot lexeme) ~default:(pos, pos) ~f:(fun (dot, lexeme) ->
        dot.range.start, lexeme.range.start)
  in
  let expr_start (expr : C.expr) : Position.t =
    Position.of_pos (C.expr_to_region expr)#start
  in
  (* Transform an expression such as ["A.B.C.d.e.f"] into a list containing
     names such as [[ "A" ; "B" ; "C" ; "e" ; "f" ]] such that all names are
     to the left of the cursor so we may handle the completion based on the
     last name that appears (module or field). The returned position is that
     of the start of the record or tuple being completed, in this case, ["d"],
     which will be missing from the list. *)
  let linearize_projection (node : C.projection) : Position.t * lexeme option list =
    ( expr_start @@ C.expr_of_projection node
    , List.take_while
        (Utils.nseq_to_list @@ C.selections_of_projection node)
        ~f:(fun s ->
          match C.dot_of_selection s with
          | None -> false
          | Some dot ->
            Position.(is_to_the_left (of_pos dot#region#stop))
              farthest_dot_position_before_cursor)
      |> List.map ~f:C.lexeme_of_selection )
  in
  (* Returns list of module names before the cursor *)
  let linearize_module_path
      (node : (C.expr_module_path, C.type_expr_module_path, C.module_name_path) expr_kind)
      : lexeme wrap list
    =
    List.take_while (C.lexemes_of_module_path node) ~f:(fun name ->
        Position.(is_to_the_left (of_pos name#region#stop))
          farthest_dot_position_before_cursor)
  in
  let linearize_expr_path
      (node : (C.expr_module_path, C.type_expr_module_path, C.module_name_path) expr_kind)
      : Modules.def_scope * lexeme wrap list
    =
    let linearized = linearize_module_path node in
    match node with
    | Module_path_expr _ -> Term_scope, linearized
    | Module_path_type_expr _ -> Type_scope, linearized
    | Module_path_selection _ -> Module_scope, linearized
  in
  let is_region_of_interest (region : Region.t) : bool =
    let open Range in
    let range = of_region region in
    (* We only care if the region contains the last dot (the one we are trying
       to complete) as well as the last lexeme. This last part is surprising,
       but important: it's possible that there is some dot before the cursor,
       but it's not a projection we're trying to complete. Having the last
       lexeme ensures that we're not trying to compare something that is way
       back in the file. See "Complete from scope after a dot" in the
       completion tests for an example of why this is needed. *)
    contains_position farthest_dot_position_before_cursor range
    && contains_position farthest_lexeme_position_before_cursor range
  in
  if Position.equal pos farthest_dot_position_before_cursor
  then None
  else (
    match C.cst_witness with
    | Witness_CameLIGO ->
      let open Cst_cameligo.Fold in
      let instruction (Some_node (node, sing)) =
        match sing with
        | S_reg S_projection
        (* It's a projection, but is it the one we're trying to complete? If the
           cursor is immediately after the dot ([r.]), it may either have a
           [ghost_ident] or something the user has typed (but backtraced). To
           solve this, we check whether the token that immediately precedes the
           cursor is part of this projection. Then, we take every field whose
           selector (the dot) is before such token. *)
          when is_region_of_interest node.region ->
          Fold.Continue (Projection (linearize_projection node.value))
        | S_reg (S_module_path S_expr) when is_region_of_interest node.region ->
          Continue (Module (linearize_expr_path (Module_path_expr node.value)))
        | S_reg (S_module_path S_type_expr) when is_region_of_interest node.region ->
          Continue (Module (linearize_expr_path (Module_path_type_expr node.value)))
        | S_reg (S_module_path S_module_name) when is_region_of_interest node.region ->
          Continue (Module (linearize_expr_path (Module_path_selection node.value)))
        | _ -> Skip
      in
      fold_cst None (Fn.const Option.some) instruction cst
    | Witness_JsLIGO ->
      let open Cst_jsligo.Fold in
      let instruction (Some_node (node, sing)) =
        match sing with
        | S_reg S_projection when is_region_of_interest node.region ->
          Fold.Continue (Projection (linearize_projection node.value))
        | S_reg (S_namespace_path S_expr) when is_region_of_interest node.region ->
          Continue (Module (linearize_expr_path (Module_path_expr node.value)))
        | S_reg (S_namespace_path S_type_expr) when is_region_of_interest node.region ->
          Continue (Module (linearize_expr_path (Module_path_type_expr node.value)))
        | S_reg (S_namespace_path S_namespace_name) when is_region_of_interest node.region
          -> Continue (Module (linearize_expr_path (Module_path_selection node.value)))
        | _ -> Skip
      in
      fold_cst None (Fn.const Option.some) instruction cst)


(** Attempts to find the linearized record projection or module path for the provided
    input and return completion items from the resolved module or record. Currently just
    works for completing from module names or struct names. *)
let complete_fields
    (type a)
    ~(normalize : Path.normalization)
    (module C : Compatible_CST with type cst = a)
    (input : a Common.input)
    : CompletionItem.t list
  =
  Option.value ~default:[]
  @@ Option.bind ~f:(function
         | Projection (struct_pos, proj_fields_before_cursor) ->
           Records.projection_impl ~normalize input struct_pos proj_fields_before_cursor
         | Module (def_scope, module_names_before_cursor) ->
           Modules.module_path_impl ~normalize module_names_before_cursor input def_scope)
  @@ get_linearized_path (module C) input


(** Gets completions for record fields and module names for the provided CST and position.
    See [complete_fields] for more information. *)
let get_fields_completions ~(normalize : Path.normalization) (input : Common.input_d)
    : CompletionItem.t list
  =
  match input.cst with
  | CameLIGO cst -> complete_fields ~normalize (module C_CameLIGO) { input with cst }
  | JsLIGO cst -> complete_fields ~normalize (module C_JsLIGO) { input with cst }
