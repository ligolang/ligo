open Handler
module Req_hover = Hover
open Lsp_helpers
module SMap = Map.Make (String)

type module_path = string list

(** The context of some completion. The constructors of this data type should be
    ordered so that the first one has the highest priority and the last one has
    the lowest. *)
type completion_context =
  | File
  | Record_field
  | Module_field
  | Scope of module_path
  | Keyword

(** Obtain the [CompletionItem.t.sortText] for some completion item given its
    context. *)
let completion_context_priority
    ?(type_aware : bool = false)
    ?(same_file : bool = false)
    (ctx : completion_context)
    : string
  =
  let base =
    match ctx with
    | File -> 0
    | Record_field -> 1
    | Module_field -> 2
    | Scope _ -> 3
    | Keyword -> 4
  in
  let scores = [| type_aware; same_file |] in
  let score = Array.sum (module Int) ~f:Bool.to_int scores in
  let max_score = Array.length scores + 1 in
  (* The LSP specification accepts [sortText] as a string that will be used to
     sort completion items, which is sorted lexicographically. If two items have
     the same [sortText]s, then their [label]s are used to compare next.
       The idea is to allocate strings [\x00], [\x01], [\x02], ... to sort these
     items according to their context. However, we may also want to have other
     factors, such as type-aware completion (even if it's currently not
     implemented). or priority to items defined in the same file, meaning that
     some items may come first.
       First we allocate the numbers 0, [max_score]*1, [max_score]*2, ... to
     represent some priority. Now, we subtract the score for this completion in
     order for it to appear higher in the completion list.
       Invariant: 0 <= [score] && [score] <= 255. *)
  String.of_char (Char.of_int_exn ((base * max_score) - score))


module type Built_in = sig
  type t

  val keywords : (Simple_utils.Region.t -> t) SMap.t
  val symbols : (Simple_utils.Region.t -> t) SMap.t
end

let dialect_keyword_completions (module Built_in : Built_in) : CompletionItem.t list =
  List.map
    Built_in.(Map.keys keywords @ Map.keys symbols)
    ~f:(fun keyword ->
      CompletionItem.create
        ~label:keyword
        ~kind:CompletionItemKind.Keyword
        ~sortText:(completion_context_priority Keyword)
        ())


let cameligo_keyword_completions : CompletionItem.t list =
  dialect_keyword_completions (module Lx_ml_self_tokens.Token)


let jsligo_keyword_completions : CompletionItem.t list =
  dialect_keyword_completions (module Lx_js_self_tokens.Token)


let get_keyword_completions : Syntax_types.t -> CompletionItem.t list = function
  | CameLIGO -> cameligo_keyword_completions
  | JsLIGO -> jsligo_keyword_completions


let defs_to_completion_items
    (context : completion_context)
    (path : Path.t)
    (syntax : Syntax_types.t)
    : Scopes.Types.def list -> CompletionItem.t list
  =
  let rec drop_common_prefix scope_path cxt_path =
    match scope_path, cxt_path with
    | mod_scope :: mods_scope, mod_cxt :: mods_cxt when String.(mod_scope = mod_cxt) ->
      drop_common_prefix mods_scope mods_cxt
    | _, _ -> scope_path
  in
  let format_label mod_path name =
    match context with
    | Scope cxt_path ->
      let path = drop_common_prefix mod_path cxt_path in
      Option.some_if (List.is_empty path) name
    | _ -> Some name
  in
  List.filter_map ~f:(fun def ->
      let label = format_label (Def.get_mod_path def) (Def.get_name def) in
      Option.map label ~f:(fun label ->
          let same_file = Option.map (Def.get_path def) ~f:(Path.equal path) in
          let sortText = completion_context_priority ?same_file context in
          let kind, detail =
            match def with
            | Scopes.Types.Variable vdef ->
              let show_type : Ast_core.type_expression -> string =
                (* VSCode is ignoring any newlines in completion detail *)
                let pp_mode = Pretty.{ width = 60; indent = 2 } in
                fun te ->
                  match Pretty.pretty_print_type_expression pp_mode ~syntax te with
                  | `Ok str -> str
                  | `Nonpretty (_exn, str) -> str
                (* Sending log messages from here or adding exn to return type will make the code
                less straightforward, so we're just silently ignoring it
                since one can use hover on this term to see the exn anyway. *)
              in
              ( CompletionItemKind.Variable
              , Option.some
                @@ Option.value_map
                     ~default:(Helpers_pretty.unresolved_type_as_comment syntax)
                     ~f:(show_type <@ Type_definition.use_var_name_if_availiable)
                @@ Type_definition.get_type vdef )
            | Scopes.Types.Type _ -> CompletionItemKind.TypeParameter, None
            | Scopes.Types.Module _ -> CompletionItemKind.Module, None
          in
          CompletionItem.create ~label ~kind ~sortText ?detail ()))


let get_defs_completions
    (path : Path.t)
    (syntax : Syntax_types.t)
    (cst : Dialect_cst.t)
    (pos : Position.t)
    (scopes : Ligo_interface.scopes)
    (definitions : Def.t list)
    : CompletionItem.t list
  =
  let scope =
    List.find scopes ~f:(fun (loc, _defs) ->
        match Range.of_loc loc with
        | None -> false
        | Some loc -> Range.contains_position pos loc)
  in
  let module_path =
    let open Cst_shared.Fold in
    match cst with
    | CameLIGO cst ->
      let open Cst_cameligo.Fold in
      let collect (Some_node (node, sing)) =
        match sing with
        | S_reg S_module_decl when Range.(contains_position pos (of_region node.region))
          -> Continue node.value.name
        | _ -> Skip
      in
      fold_cst [] (Fn.flip List.cons) collect cst
    | JsLIGO cst ->
      let open Cst_jsligo.Fold in
      let collect (Some_node (node, sing)) =
        match sing with
        | S_reg S_namespace_decl
          when Range.(contains_position pos (of_region node.region)) ->
          Continue node.value.namespace_name
        | _ -> Skip
      in
      fold_cst [] (Fn.flip List.cons) collect cst
  in
  defs_to_completion_items
    (Scope (List.rev_map ~f:(fun name -> name#payload) module_path))
    path
    syntax
    (* TODO: In case we found [None], let's at least show the entire scope to
       the user so the completions aren't empty. This happens because scopes
       aren't accurate and may be missing on some ranges. As soon as scopes are
       improved, we should remove this workaround. *)
    (Option.value_map scope ~default:definitions ~f:snd)


let complete_files (pos : Position.t) (code : string) (files : string list)
    : CompletionItem.t list
  =
  let regex = Str.regexp {|^#[ \t]*\(include\|import\)[ \t]*\"|} in
  (* n.b.: We may not use [List.nth_exn] because if the user happens to trigger
     a completion at the last line while "Editor: Render Final Newline" is
     enabled, it will crash the language server. *)
  let current_line =
    Option.value ~default:"" @@ List.nth (String.split_lines code) pos.line
  in
  if Str.string_match regex current_line 0
  then
    List.map files ~f:(fun file ->
        CompletionItem.create
          ~label:file
          ~kind:CompletionItemKind.File
          ~sortText:(completion_context_priority File)
          ())
  else []


let with_code (path : Path.t) (f : string -> 'a Handler.t) : 'a Handler.t =
  let@ docs = ask_docs_cache in
  match Docs_cache.find docs path with
  | None -> return None
  | Some file_data -> f file_data.code


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


type ('module_expr, 'module_type_expr) expr_kind =
  | Module_path_expr of 'module_expr
  | Module_path_type_expr of 'module_type_expr

(* TODO: we should handle field completion using ast_typed rather than scopes *)
let complete_fields
    (path : Path.t)
    (syntax : Syntax_types.t)
    (cst : Dialect_cst.t)
    (pos : Position.t)
    (definitions : Def.t list)
    : CompletionItem.t list
  =
  (* Recursively resolve a projection path. This is done with
     [find_record_from_path], which takes the current field's path and its type.
     We look into the record's known fields and check for the presence of the
     current field. If resolved, the algorithm recursively looks up this field's
     type and proceeds to try to resolve it as a record with the remainder of
     the field path. Once there is no more fields, we return the current record. *)
  let core_record_to_completion_items (row : Ast_core.row) : CompletionItem.t list =
    List.map (Map.to_alist row.fields) ~f:(fun (Label label, texp) ->
        let () = Ast_core.PP.type_expression Format.str_formatter texp in
        let detail = Format.flush_str_formatter () in
        let sortText = completion_context_priority Record_field in
        CompletionItem.create ~label ~kind:CompletionItemKind.Field ~detail ~sortText ())
  in
  let rec find_record_in_core : Ast_core.type_content -> Ast_core.row option = function
    | T_record row -> Some row
    | T_variable var ->
      Option.bind
        (List.find_map definitions ~f:(function
            | Type tdef ->
              if Ligo_prim.Type_var.is_name var tdef.name then Some tdef else None
            | Variable _ | Module _ -> None))
        ~f:(fun tdef ->
          match tdef.content with
          | None -> None
          | Some content -> find_record_in_core content.type_content)
    | _ -> None
  in
  let rec find_record_from_path
      (struct_type : Ast_core.type_expression)
      (field_path : string option list)
      : Ast_core.row option
    =
    let open Option.Monad_infix in
    find_record_in_core struct_type.type_content
    >>= fun struct_type ->
    match field_path with
    | [] -> Some struct_type
    | selection :: field_path ->
      selection
      >>= fun name ->
      Ligo_prim.(Record.find_opt struct_type.fields (Label.of_string name))
      >>= Fn.flip find_record_from_path field_path
  in
  (* Calculate the least distance between [range] and [pos]. Returns [None] if
     [pos] is to the right of [range]. *)
  let distance_to_pos (range : Range.t) : Position.t option =
    match Position.compare_ord range.end_ pos with
    | Less | Equal ->
      Some
        (Position.create
           ~line:(range.end_.line - pos.line)
           ~character:(range.end_.character - pos.character))
    | Greater -> None
  in
  let get_module_from_pos (filter_def : Def.t -> bool) (module_pos : Position.t)
      : CompletionItem.t list option
    =
    let open Option.Monad_infix in
    let module_defs_to_completion_items (defs : Scopes.def list) : CompletionItem.t list =
      defs_to_completion_items Module_field path syntax
      @@ List.filter
           ~f:(fun def ->
             match Def.get_def_type def with
             | Module_field -> filter_def def
             | Local | Parameter | Global -> false)
           defs
    in
    let rec get_module_defs : Scopes.Types.mod_case -> CompletionItem.t list option
      = function
      | Def defs -> Some (module_defs_to_completion_items defs)
      | Alias { module_path = _; resolved_module; file_name = _ } ->
        Option.bind resolved_module ~f:(fun resolved ->
            List.find_map definitions ~f:(function
                | Variable _ | Type _ -> None
                | Module m ->
                  if Scopes.Types.Uid.(m.uid = resolved)
                  then get_module_defs m.mod_case
                  else None))
    in
    Go_to_definition.get_definition module_pos path definitions
    >>= function
    | Module { mod_case; _ } -> get_module_defs mod_case
    | Variable _ | Type _ -> None
  in
  let projection_impl
      (struct_pos : Position.t)
      (proj_fields_before_cursor : string option list)
      : CompletionItem.t list option
    =
    match Go_to_definition.get_definition struct_pos path definitions with
    | Some (Variable { t; _ }) ->
      let mk_completions t =
        Option.map ~f:core_record_to_completion_items
        @@ find_record_from_path t proj_fields_before_cursor
      in
      (match t with
      | Core t -> mk_completions t
      | Resolved t -> mk_completions (Checking.untype_type_expression t)
      | Unresolved -> None)
    | None | Some (Type _ | Module _) -> None
  in
  let module_path_impl
      (module_names_before_cursor : Cst_shared.Tree.lexeme Lexing_shared.Wrap.t list)
      (filter_def : Scopes.def -> bool)
      : CompletionItem.t list option
    =
    Option.bind (List.last module_names_before_cursor) ~f:(fun module_name ->
        let module_pos = Position.of_pos module_name#region#start in
        get_module_from_pos filter_def module_pos)
  in
  let module_path_impl_expr
      (module_names_before_cursor : Cst_shared.Tree.lexeme Lexing_shared.Wrap.t list)
      : CompletionItem.t list option
    =
    module_path_impl module_names_before_cursor (function
        | Type _ -> false
        | Variable _ | Module _ -> true)
  in
  let module_path_impl_type_expr
      (module_names_before_cursor : Cst_shared.Tree.lexeme Lexing_shared.Wrap.t list)
      : CompletionItem.t list option
    =
    module_path_impl module_names_before_cursor (function
        | Variable _ -> false
        | Type _ | Module _ -> true)
  in
  let mk_dist region =
    let range = Range.of_region region in
    Option.map (distance_to_pos range) ~f:(fun dist -> { range; dist })
  in
  let open Cst_shared.Fold in
  (* JsLIGO's completion code is just a copy and paste of CameLIGO's
     completion code because their CSTs are very similar, with some minor changes where
     needed. The comments in CameLIGO's branch also generalize to the other two branches. *)
  match cst with
  | CameLIGO cst ->
    let open Cst_cameligo.CST in
    let open Cst_cameligo.Fold in
    (* Find the greatest dot position that is less than or equal to the position
       of the cursor. Returns a negative number if to the left, null if inside,
       or positive otherwise. *)
    let farthest_dot_position_before_cursor, farthest_lexeme_position_before_cursor =
      let empty = completion_distance_monoid.empty in
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
      let { dot; lexeme } = fold_map_cst completion_distance_monoid collect cst in
      Option.value_map
        (Option.both dot lexeme)
        ~default:(pos, pos)
        ~f:(fun (dot, lexeme) -> dot.range.start, lexeme.range.start)
    in
    let expr_start (expr : expr) : Position.t =
      Position.of_pos (expr_to_region expr)#start
    in
    (* Transform an expression such as ["A.B.C.d.e.f"] into a list containing
       names such as [[ "A" ; "B" ; "C" ; "e" ; "f" ]] such that all names are
       to the left of the cursor so we may handle the completion based on the
       last name that appears (module or field). The returned position is that
       of the start of the record or tuple being completed, in this case, ["d"],
       which will be missing from the list. *)
    let linearize_projection (node : projection) : Position.t * lexeme option list =
      let hd, tl = node.field_path in
      ( expr_start node.record_or_tuple
      , List.take_while ((node.selector, hd) :: tl) ~f:(fun (dot, _field) ->
            Position.(is_to_the_left (of_pos dot#region#stop))
              farthest_dot_position_before_cursor)
        |> List.map ~f:(fun (_dot, (field : selection)) ->
               match field with
               | FieldName v ->
                 (match v with
                 | Var name -> Some name#payload
                 | Esc name -> Some ("@" ^ name#payload))
               | Component _ -> None) )
    in
    let linearize_module_path (type expr) (node : expr module_path) : lexeme wrap list =
      List.take_while
        (Simple_utils.Utils.nsepseq_to_list node.module_path)
        ~f:(fun name ->
          Position.(is_to_the_left (of_pos name#region#stop))
            farthest_dot_position_before_cursor)
    in
    (* Returns: position of struct (if there is one), list of module names
       before the cursor, and the list of field names before the cursor ([None]
       means it's a [Component] rather than [FieldName]). *)
    let linearize_module_path_expr (node : expr module_path)
        : Position.t option * lexeme wrap list * lexeme option list
      =
      let module_names_before_cursor = linearize_module_path node in
      let struct', proj_fields_before_cursor =
        match node.field with
        | E_Proj proj
          when Position.(
                 is_to_the_left
                   (of_pos proj.value.selector#region#start)
                   farthest_dot_position_before_cursor) ->
          Tuple.T2.map_fst ~f:Option.some (linearize_projection proj.value)
        | _ -> None, []
      in
      struct', module_names_before_cursor, proj_fields_before_cursor
    in
    let expr_path_impl
        (node : (expr module_path reg, type_expr module_path reg) expr_kind)
        : CompletionItem.t list option
      =
      let struct', module_names_before_cursor, proj_fields_before_cursor =
        match node with
        | Module_path_expr expr -> linearize_module_path_expr expr.value
        | Module_path_type_expr type_expr ->
          None, linearize_module_path type_expr.value, []
      in
      (* Are we completing a module or a projection? *)
      match struct' with
      | None ->
        (match node with
        | Module_path_expr _ -> module_path_impl_expr module_names_before_cursor
        | Module_path_type_expr _ -> module_path_impl_type_expr module_names_before_cursor)
      | Some struct' -> projection_impl struct' proj_fields_before_cursor
    in
    let is_reg_node_of_interest (type a) (node : a reg) : bool =
      let open Range in
      let range = of_region node.region in
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
    let field_completion (Some_node (node, sing)) =
      match sing with
      | S_reg S_projection
      (* It's a projection, but is it the one we're trying to complete? If the
         cursor is immediately after the dot ([r.]), it may either have a
         [ghost_ident] or something the user has typed (but backtraced). To
         solve this, we check whether the token that immediately precedes the
         cursor is part of this projection. Then, we take every field whose
         selector (the dot) is before such token. *)
        when is_reg_node_of_interest node ->
        let struct_pos, proj_fields_before_cursor = linearize_projection node.value in
        Continue (projection_impl struct_pos proj_fields_before_cursor)
      | S_reg (S_module_path S_expr) when is_reg_node_of_interest node ->
        Continue (expr_path_impl (Module_path_expr node))
      | S_reg (S_module_path S_type_expr) when is_reg_node_of_interest node ->
        Continue (expr_path_impl (Module_path_type_expr node))
      | _ -> Skip
    in
    if Position.equal pos farthest_dot_position_before_cursor
    then []
    else Option.value ~default:[] (fold_map_cst first_monoid field_completion cst)
  | JsLIGO cst ->
    let open Cst_jsligo.CST in
    let open Cst_jsligo.Fold in
    let farthest_dot_position_before_cursor, farthest_lexeme_position_before_cursor =
      let empty = completion_distance_monoid.empty in
      let collect (Some_node (node, sing)) =
        match sing with
        | S_dot -> Last { empty with dot = mk_dist node#region }
        | S_eof -> Stop
        | S_wrap _ -> Last { empty with lexeme = mk_dist node#region }
        | S_reg _ -> Continue { empty with lexeme = mk_dist node.region }
        | _ -> Skip
      in
      let { dot; lexeme } = fold_map_cst completion_distance_monoid collect cst in
      Option.value_map
        (Option.both dot lexeme)
        ~default:(pos, pos)
        ~f:(fun (dot, lexeme) -> dot.range.start, lexeme.range.start)
    in
    let expr_start (expr : expr) : Position.t =
      Position.of_pos (expr_to_region expr)#start
    in
    let linearize_projection (node : projection) : Position.t * lexeme option list =
      let hd, tl = node.property_path in
      ( expr_start node.object_or_array
      , List.take_while (hd :: tl) ~f:(function
            | PropertyName (dot, _name) ->
              Position.(is_to_the_left (of_pos dot#region#stop))
                farthest_dot_position_before_cursor
            | PropertyStr _ -> false
            | Component _ -> false)
        |> List.map ~f:(function
               | PropertyName (_dot, v) ->
                 (match v with
                 | Var name -> Some name#payload
                 | Esc name -> Some ("@" ^ name#payload))
               | PropertyStr _ -> None
               | Component _ -> None) )
    in
    let linearize_namespace_path (type expr) (node : expr namespace_path)
        : lexeme wrap list
      =
      List.take_while
        (Simple_utils.Utils.nsepseq_to_list node.namespace_path)
        ~f:(fun name ->
          Position.(is_to_the_left (of_pos name#region#stop))
            farthest_dot_position_before_cursor)
    in
    let linearize_namespace_path_expr (node : expr namespace_path)
        : Position.t option * lexeme wrap list * lexeme option list
      =
      let module_names_before_cursor = linearize_namespace_path node in
      let struct', proj_fields_before_cursor =
        match node.property with
        | E_Proj proj ->
          let hd, _tl = proj.value.property_path in
          (match hd with
          | PropertyName (dot, _name) ->
            if Position.(
                 is_to_the_left
                   (of_pos dot#region#start)
                   farthest_dot_position_before_cursor)
            then Tuple.T2.map_fst ~f:Option.some (linearize_projection proj.value)
            else None, []
          | PropertyStr _ -> None, []
          | Component _ -> None, [])
        | _ -> None, []
      in
      struct', module_names_before_cursor, proj_fields_before_cursor
    in
    let expr_path_impl
        (node : (expr namespace_path reg, type_expr namespace_path reg) expr_kind)
        : CompletionItem.t list option
      =
      let struct', module_names_before_cursor, proj_fields_before_cursor =
        match node with
        | Module_path_expr expr -> linearize_namespace_path_expr expr.value
        | Module_path_type_expr type_expr ->
          None, linearize_namespace_path type_expr.value, []
      in
      match struct' with
      | None ->
        (match node with
        | Module_path_expr _ -> module_path_impl_expr module_names_before_cursor
        | Module_path_type_expr _ -> module_path_impl_type_expr module_names_before_cursor)
      | Some struct' -> projection_impl struct' proj_fields_before_cursor
    in
    let is_reg_node_of_interest (type a) (node : a reg) : bool =
      let open Range in
      let range = of_region node.region in
      contains_position farthest_dot_position_before_cursor range
      && contains_position farthest_lexeme_position_before_cursor range
    in
    let field_completion (Some_node (node, sing)) =
      match sing with
      | S_reg S_projection when is_reg_node_of_interest node ->
        let struct_pos, proj_fields_before_cursor = linearize_projection node.value in
        Continue (projection_impl struct_pos proj_fields_before_cursor)
      | S_reg (S_namespace_path S_expr) when is_reg_node_of_interest node ->
        Continue (expr_path_impl (Module_path_expr node))
      | S_reg (S_namespace_path S_type_expr) when is_reg_node_of_interest node ->
        Continue (expr_path_impl (Module_path_type_expr node))
      | _ -> Skip
    in
    if Position.equal pos farthest_dot_position_before_cursor
    then []
    else Option.value ~default:[] (fold_map_cst first_monoid field_completion cst)


let mk_completion_list (items : CompletionItem.t list)
    : [ `CompletionList of CompletionList.t | `List of CompletionItem.t list ] option
  =
  Option.some @@ `CompletionList (CompletionList.create ~isIncomplete:false ~items ())


let on_req_completion (pos : Position.t) (path : Path.t)
    : [ `CompletionList of CompletionList.t | `List of CompletionItem.t list ] option
    Handler.t
  =
  when_some' (Path.get_syntax path)
  @@ fun syntax ->
  let keyword_completions = get_keyword_completions syntax in
  (* TODO (#1657): After a project system is implemented, we should support
     completing from files here. Meanwhile, we leave it with []. *)
  (*with_code path
  @@ fun _code ->
  let file_completions = complete_files pos code files in *)
  let file_completions = [] in
  let completions_without_scopes = file_completions @ keyword_completions in
  (* Even if scopes fail for whatever reason, we can at least show files and
     keywords to the user. *)
  let completions_so_far = mk_completion_list completions_without_scopes in
  with_cached_doc path completions_so_far
  @@ fun { definitions; code; _ } ->
  with_cst path completions_so_far
  @@ fun cst ->
  let field_completions = complete_fields path syntax cst pos definitions in
  let all_completions =
    (* If we are completing a record or module field, there is no need to also
       suggest scopes or keywords. *)
    if List.is_empty field_completions
    then (
      let scopes =
        Ligo_interface.get_scopes
          ~project_root:(Project_root.get_project_root path)
          ~definitions
          ~code
          path
      in
      let scope_completions =
        get_defs_completions path syntax cst pos scopes definitions
      in
      let field_and_scope_completions =
        (* Keep the first item to deal with shadowing. *)
        List.remove_consecutive_duplicates
          ~which_to_keep:`First
          ~equal:(fun x y -> String.equal x.label y.label)
          (List.sort (field_completions @ scope_completions) ~compare:(fun x y ->
               String.compare x.label y.label))
      in
      field_and_scope_completions @ completions_without_scopes)
    else field_completions
  in
  return @@ mk_completion_list all_completions
