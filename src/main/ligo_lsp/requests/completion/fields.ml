(* Completions for things that come after dot, like [List.le] or [person.na]
   This module finds if the cursor is located like this, finds corresponding record
   or module, and calls [Records] or [Modules]  *)
(* TODO: we should handle field completion using ast_typed rather than scopes *)
open Common
open Lsp_helpers
module Utils = Simple_utils.Utils
module Fold = Cst_shared.Fold

type ('module_expr, 'module_type_expr, 'module_name) expr_kind =
  | Module_path_expr of 'module_expr
  | Module_path_type_expr of 'module_type_expr
  | Module_path_selection of 'module_name

(** We want to prove to the compiler that a module of type [Compatible_CST]
    is containing either CameLIGO or JsLIGO cst *)
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

module type Compatible_CST = sig
  type cst
  type expr
  type type_expr
  type projection
  type selection (* Note: this selection must include the dot (as in JsLIGO) *)

  (* OCaml compiler wants concrete types to make the witness thing working *)
  type expr_module_path
  type type_expr_module_path
  type module_name_path

  val expr_to_region : expr -> Region.t
  val expr_of_projection : projection -> expr
  val try_get_projection : expr -> projection option
  val dot_of_selection : selection -> dot option
  val lexeme_of_selection : selection -> lexeme option
  val selections_of_projection : projection -> selection Utils.nseq

  val lexemes_of_module_path
    :  (expr_module_path, type_expr_module_path, module_name_path) expr_kind
    -> lexeme wrap list

  val field_of_module_path : expr_module_path -> expr

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


let completion_distance_monoid : completion_distance Cst_shared.Fold.monoid =
  let dist_monoid = smallest_negative_distance_monoid in
  { empty = { dot = dist_monoid.empty; lexeme = dist_monoid.empty }
  ; append =
      (fun { dot = dot_lhs; lexeme = lexeme_lhs } { dot = dot_rhs; lexeme = lexeme_rhs } ->
        { dot = dist_monoid.append dot_lhs dot_rhs
        ; lexeme = dist_monoid.append lexeme_lhs lexeme_rhs
        })
  }


let complete_fields
    (type a)
    (module C : Compatible_CST with type cst = a)
    ({ cst; pos; _ } as input : a Common.input)
    : CompletionItem.t list
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
  let expr_path_impl
      (node : (C.expr_module_path, C.type_expr_module_path, C.module_name_path) expr_kind)
      : CompletionItem.t list option
    =
    match node with
    | Module_path_expr expr ->
      let module_names_before_cursor = linearize_module_path (Module_path_expr expr) in
      Modules.module_path_impl module_names_before_cursor input Term_scope
    | Module_path_type_expr type_expr ->
      let module_names_before_cursor =
        linearize_module_path (Module_path_type_expr type_expr)
      in
      Modules.module_path_impl module_names_before_cursor input Type_scope
    | Module_path_selection module_name ->
      let module_names_before_cursor =
        linearize_module_path (Module_path_selection module_name)
      in
      Modules.module_path_impl module_names_before_cursor input Module_scope
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
  let field_completion () =
    Option.value ~default:[]
    @@
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
          let struct_pos, proj_fields_before_cursor = linearize_projection node.value in
          Fold.Continue
            (Records.projection_impl input struct_pos proj_fields_before_cursor)
        | S_reg (S_module_path S_expr) when is_region_of_interest node.region ->
          Continue (expr_path_impl (Module_path_expr node.value))
        | S_reg (S_module_path S_type_expr) when is_region_of_interest node.region ->
          Continue (expr_path_impl (Module_path_type_expr node.value))
        | S_reg (S_module_path S_module_name) when is_region_of_interest node.region ->
          Continue (expr_path_impl (Module_path_selection node.value))
        | _ -> Skip
      in
      fold_map_cst Fold.last_monoid instruction cst
    | Witness_JsLIGO ->
      let open Cst_jsligo.Fold in
      let instruction (Some_node (node, sing)) =
        match sing with
        | S_reg S_projection when is_region_of_interest node.region ->
          let struct_pos, proj_fields_before_cursor = linearize_projection node.value in
          Fold.Continue
            (Records.projection_impl input struct_pos proj_fields_before_cursor)
        | S_reg (S_namespace_path S_expr) when is_region_of_interest node.region ->
          Continue (expr_path_impl (Module_path_expr node.value))
        | S_reg (S_namespace_path S_type_expr) when is_region_of_interest node.region ->
          Continue (expr_path_impl (Module_path_type_expr node.value))
        | S_reg (S_namespace_path S_namespace_name) when is_region_of_interest node.region
          -> Continue (expr_path_impl (Module_path_selection node.value))
        | _ -> Skip
      in
      fold_map_cst Fold.last_monoid instruction cst
  in
  if Position.equal pos farthest_dot_position_before_cursor
  then []
  else field_completion ()


let get_fields_completions (input : Common.input_d) : CompletionItem.t list =
  match input.cst with
  | CameLIGO cst -> complete_fields (module C_CameLIGO) { input with cst }
  | JsLIGO cst -> complete_fields (module C_JsLIGO) { input with cst }
