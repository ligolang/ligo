open Lsp_helpers

type lexeme = Cst_shared.Types.lexeme
type 'a wrap = 'a Cst_shared.Types.wrap
type dot = lexeme wrap

(** The context of some completion. The constructors of this data type should be
    ordered so that the first one has the highest priority and the last one has
    the lowest. *)
type completion_context =
  | File
  | Record_field
  | Module_field
  | Scope
  | Keyword

(** Obtain the [CompletionItem.t.sortText] for some completion item given its context. *)
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
    | Scope -> 3
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


let show_type : syntax:Syntax_types.t -> Ast_core.type_expression -> string =
  (* VSCode is ignoring any newlines in completion detail *)
  let pp_mode = Pretty.{ width = 60; indent = 2 } in
  fun ~syntax te ->
    match Pretty.pretty_print_type_expression pp_mode ~syntax te with
    | `Ok str -> str
    (* Sending log messages from here or adding exn to return type will make the code
            less straightforward, so we're just silently ignoring it
            since one can use hover on this term to see the exn anyway. *)
    | `Nonpretty (_exn, str) -> str


let defs_to_completion_items
    (context : completion_context)
    (path : Path.t)
    (syntax : Syntax_types.t)
    (defs : Scopes.Types.def list)
    : CompletionItem.t list
  =
  List.map defs ~f:(fun def ->
      let name = Def.get_name def in
      let same_file = Option.map (Def.get_path def) ~f:(Path.equal path) in
      let sortText = completion_context_priority ?same_file context in
      let kind, detail =
        match def with
        | Scopes.Types.Variable vdef ->
          ( CompletionItemKind.Variable
          , Option.some
            @@ Option.value_map
                 ~default:(Helpers_pretty.unresolved_type_as_comment syntax)
                 ~f:(show_type ~syntax <@ Def.use_var_name_if_available)
            @@ Def.get_type vdef )
        | Scopes.Types.Type _ -> CompletionItemKind.TypeParameter, None
        | Scopes.Types.Module _ -> CompletionItemKind.Module, None
      in
      CompletionItem.create ~label:name ~kind ~sortText ?detail ())


(* Details of a (successfully parsed) file that user plus the cursor position.
   This type is introduced to avoid functions that have e.g. two [Position.t] or
   [Def.t list] in arguments and to pass all arguments to functions
   like [Fields.get_fields_completions] at once *)
type 'cst input =
  { cst : 'cst
  ; syntax : Syntax_types.t
  ; path : Path.t
  ; definitions : Def.t list
  ; pos : Position.t
  }

type input_d = Dialect_cst.t input

let mk_input_d ~(cst : Dialect_cst.t) ~syntax ~path ~definitions ~pos : input_d =
  { cst; syntax; path; definitions; pos }


(* Scopes are not perfect so sometimes they can show 2 things with same identifiers
   "belonging to our scope". To make the output more compact,
   we're keeping only one for them *)
let nub_sort_items : CompletionItem.t list -> CompletionItem.t list =
 fun with_possible_duplicates ->
  List.remove_consecutive_duplicates
    ~which_to_keep:`First
    ~equal:CompletionItem.(fun x y -> String.equal x.label y.label)
  @@ List.sort with_possible_duplicates ~compare:(fun x y ->
         String.compare x.label y.label)
