open Simple_utils
open Simple_utils.Function
open PPrint

let with_raise ?(cleanup = fun () -> ()) f =
  let result = Trace.to_stdlib_result f in
  cleanup ();
  result


let format_errors = function
  | Ok ((), _) -> Ok ("", "")
  | Error (e, _) ->
    Error
      ( Format.asprintf
          "%a"
          (Main_errors.Formatter.error_ppformat
             ~display_format:Human_readable
             ~no_colour:true)
          e
      , "" )


(* FIXME support situations like
   [ #import "x.jsligo" "X"; namespace Y = X.Y ] *)
let unmangle (str : string) : string option =
  let open Option.Let_syntax in
  let%bind str = String.chop_prefix str ~prefix:"Mangled_module_" in
  let open Str in
  str
  |> global_replace (regexp_string "_p_") "."
  |> global_replace (regexp_string "_c_") ":"
  |> global_replace (regexp_string "_b_") "\\"
  |> global_replace (regexp_string "_s_") "/"
  |> global_replace (regexp_string "_a_") "@"
  |> global_replace (regexp_string "_d_") "-"
  |> global_replace (regexp_string "_l_") "("
  |> global_replace (regexp_string "_r_") ")"
  |> global_replace (regexp_string "_u_") "_"
  |> return


(** reach_path_from_dir
    ~path:"/home/a/projects/b/c.mligo"
    ~dir:"/home/a/projects/c/"
    = "../b/c.mligo" *)
let reach_path_from_dir ~path ~dir =
  let rec helper path dir =
    match path, dir with
    | path, [] -> path
    | x :: xs, y :: ys when String.equal x y -> helper xs ys
    | xs, _y :: ys -> helper (".." :: xs) ys
  in
  Filename.of_parts @@ helper (Filename.parts path) (Filename.parts dir)


let doc_to_string_compact (doc : PPrint.document) : string =
  let buffer = Buffer.create 131 in
  PPrint.ToBuffer.compact buffer doc;
  Buffer.contents buffer


(** Used for [<SyntaxTitle>] since we don't create a ```-style codeblock here *)
let escape_string_to_html : string -> string =
  String.substr_replace_all ~pattern:"<" ~with_:"&lt;"
  <@ String.substr_replace_all ~pattern:">" ~with_:"&gt;"
  <@ String.substr_replace_all ~pattern:"\"" ~with_:"&quot;"
  <@ String.substr_replace_all ~pattern:"'" ~with_:"&#39;"
  <@ String.substr_replace_all ~pattern:"{" ~with_:"&#123;"
  <@ String.substr_replace_all ~pattern:"}" ~with_:"&#125;"
  <@ String.substr_replace_all ~pattern:"_" ~with_:"&#95;"
  <@ String.substr_replace_all ~pattern:"&" ~with_:"&amp;"


(* Module declarations come first *)
let compare_declarations (a : Ast_typed.declaration) (b : Ast_typed.declaration) =
  let aux =
    Ast_typed.(
      function
      | D_signature _ -> 0
      | D_module _ -> 1
      | _ -> 2)
  in
  compare (aux a.wrap_content) (aux b.wrap_content)


let decompile_core_type
    ?(escape_html_characters = false)
    ?prefix
    ~(syntax : Syntax_types.t)
    (core_type : Ast_core.ty_expr)
    : document
  =
  let pp_mode : Lsp_helpers.Pretty.pp_mode = { indent = 2; width = 120 } in
  let d =
    match
      Lsp_helpers.Pretty.pretty_print_type_expression ?prefix ~syntax pp_mode core_type
    with
    | `Ok s -> s
    | `Nonpretty (_exn, s) -> s
  in
  string @@ if escape_html_characters then escape_string_to_html d else d


let decompile_type
    ?escape_html_characters
    ?prefix
    ~(syntax : Syntax_types.t)
    (ty_expr : Ast_typed.ty_expr)
    : document
  =
  let core_type = Checking.untype_type_expression ~use_orig_var:true ty_expr in
  decompile_core_type ?escape_html_characters ?prefix ~syntax core_type


let decompile_type_case
    ?escape_html_characters
    ?prefix
    ~(syntax : Syntax_types.t)
    (t : Scopes.Types.type_case)
    : document
  =
  match t with
  | Core t -> decompile_core_type ?escape_html_characters ?prefix ~syntax t
  | Resolved t -> decompile_type ?escape_html_characters ?prefix ~syntax t
  | Unresolved -> Option.value ~default:empty prefix ^/^ !^"unresolved"
