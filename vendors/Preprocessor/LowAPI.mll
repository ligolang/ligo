(* Simple preprocessor based on cpp and C#, to be processed by [ocamllex]. *)

{
(* START OF HEADER *)

module Array  = Caml.Array (* Generated by ocamllex *)

(* Vendor dependencies *)

module Region = Simple_utils.Region
module Pos    = Simple_utils.Pos
module Lexbuf = Simple_utils.Lexbuf

(* Utilities *)

let (<@) f g x = f (g x)

let sprintf = Printf.sprintf

(* Functor *)

type file_path   = string
type module_name = string
type module_deps = (file_path * module_name) list

type text        = Buffer.t
type success     = text * module_deps
type message     = string Region.reg
type error       = text option * message
type result      = (success, error) Core.result

type 'src preprocessor = 'src -> result

module type S =
  sig
    (* Preprocessing from various sources *)

    val from_lexbuf  : Lexing.lexbuf preprocessor
    val from_channel : in_channel    preprocessor
    val from_string  : string        preprocessor
    val from_file    : file_path     preprocessor
    val from_buffer  : Buffer.t      preprocessor
  end

module Make (Config : Config.S) (Options : Options.S) =
  struct
    (* FINDING FILES *)

    (* Finding a file to #include & #import *)

    (* The function [find_in_cli_paths file_path dirs] tries to find a
       valid path by prepending a directory from [dirs] to
       [file_path], The list [dirs] is a list of directories passed
       via the CLI option "-I". *)

    let rec find_in_cli_paths file_path = function
      [] -> None
    | dir::dirs ->
        let path =
          if String.(dir = "." || dir = "") then file_path
          else dir ^ Filename.dir_sep ^ file_path in
        match Sys.file_exists path with
          `Yes           -> Some path
        | `No | `Unknown -> find_in_cli_paths file_path dirs

    (* The call [find dir file inclusion_paths] looks for [file] in
       [dir]. If the file is not found, it is sought in the
       directories [Options.dirs] (from the CLI flag "-I"). If the
       file is still not found, we try to search for the file using
       the [inclusion_paths] with the help of module [ModRes]. *)

    let find dir file inclusion_paths =
      let path =
        if String.(dir = "." || dir = "") then file
        else dir ^ Filename.dir_sep ^ file in
      match Sys.file_exists path with
        `Yes -> Some path
      | `No | `Unknown ->
          match find_in_cli_paths file Options.dirs with
            Some _ as some -> some
          | None ->
              let file_opt =
                ModRes.find_external_file ~file ~inclusion_paths
              in match file_opt with
                   None -> None
                 | Some file ->
                     match Sys.file_exists file with
                       `Yes -> file_opt
                     | `No | `Unknown -> None

    (* STRING PROCESSING *)

    (* The value of [mk_string p] ("make string") is a string
       containing the characters in the list [p], in reverse
       order. For instance, [mk_string ['c';'b';'a'] = "abc"]. *)

    let mk_string (p : char list) : string =
      let len   = List.length p in
      let bytes = Bytes.make len ' ' in
      let rec fill i = function
             [] -> bytes
      | char::l -> Bytes.set bytes i char; fill (i-1) l
      in fill (len-1) p |> Bytes.to_string

    (* ERRORS *)

    let fail = E_Lexer.fail (* Raise exception [Error.Error] *)
    and stop = E_Lexer.stop (* Raise exception [Error.Error] *)

    (* Conditionals *)

    let reduce_cond state region =
      match state#reduce_cond with
        Ok state  -> state
      | Error err -> fail state region err

    let extend state cond mode region =
      match state#extend cond mode with
        Ok state  -> state
      | Error err -> fail state region err

    (* Scanning #include at top-level and inside verbatim strings *)

    let include_action ~callback hash_pos state lexbuf =
      (* We extract information about the current file so we can
         restore it after the processing of #include is complete. *)

      let line = state#pos#line
      and base = Filename.basename state#pos#file in

      (* By calling [scan_include], we read the string containing the
         name of the file to include. The result is quadruple:

           (1) The first component is the updated state;

           (2) the second component [incl_region] is the region in the
               file corresponding to the string, in case we will need
               to format an error message when the file is not found
               on the filesystem;

           (3) the third component [incl_file] is the name of the
               file to include;

           (4) the fourth and last component [_] is the directive
               "#include" as a value of type [Directive.t], which we
               discard here because it is used solely by the lexer
               library [LexerLib] to tokenise the directives.

         Note that [scan_include] does not need to return a new state,
         as the only update to it is by performing a side-effect on
         the field [out]. *)

      match Directive.scan_include hash_pos state lexbuf with
        Stdlib.Error (region, error) ->
          fail state region error

      | Ok (state, incl_directive, _, _) -> (* Directive dropped. *)
          let ()          = state#copy_nl lexbuf in
          let state       = fst (state#newline lexbuf) in
          let incl_region = incl_directive#file_path.region
          and incl_file   = incl_directive#file_path.value in

          if state#is_copy then
            (* If in copy mode, we establish the directory where the
               file to include is expected to reside. This directory
               may be relative to the current directory or not. See
               [incl_path] below. *)

            let path = state#incl in

            (* We resolve the file names to be included. *)

            let external_dirs =
              let file =
                match state#parent with
                  Some parent -> parent
                | None        -> state#pos#file in
              ModRes.get_dependencies ~file state#mod_res in

            (* We try to find the file to include. If missing, the
               exception [Error] is raised, with the value
               [Error.File_not_found]. Otherwise, we obtain a triple
               [incl_path, incl_chan, state].

                 * The first component, [incl_path], may be different
                   from [incl] if the preprocessor is standalone and
                   was given a list of directories with the
                   command-line option [-I] ([external_dirs]) and the
                   file to include was not found relatively to the
                   current directory, but from one of those given with
                   [-I]. This is consistent with the behaviour of
                   [cpp], insofar as we proofed it.

                 * The second component of the triple, [incl_chan], is
                   an input channel of type [in_channel], which has
                   been registered with the [state], so we can close
                   it when we are done.

                 * The third component of the triple is an updated
                   value of the [state] (see [incl_chan] above). *)

            let incl_path, incl_chan, state =
              match find path incl_file external_dirs with
                None ->
                  fail state incl_region (Error.File_not_found incl_file)
              | Some incl_path ->
                  try
                    let in_chan = open_in incl_path in
                    let state   = state#push_chan in_chan
                    in incl_path, in_chan, state
                  with Sys_error msg ->
                    Error.Failed_opening (incl_path, msg)
                    |> fail state incl_region in

            (* We are ready now to output the linemarker before
               including the file (as the rightmost flag [1]
               states). Of course we start at line 1 in the included
               file (as the leftmost flag [1] states). *)

            let () = state#print (sprintf "# 1 %S 1\n" incl_path) in

            (* We prepare a lexing buffer from the input channel bound to
               the file to include. *)

            let incl_buf = Lexing.from_channel incl_chan in

            (* We instruct the lexing buffer just created that the
               corresponding file name is [incl_file] (recall that
               this may not be the fully qualified name, but the one
               given after #include) at the current position (_not_
               the first position matched in the lexing buffer, called
               _start position_ in module [Lexing], precisely because
               we change file with #include). *)

            let () = Lexbuf.reset_file incl_file lexbuf in

            (* We make a variant copy of the current state meant to
               scan the file to include: we force the copy mode from
               the start, and we set an empty trace for conditional
               directives, so a conditional directive opened in the
               current file cannot be closed in the included file. *)

            let state' = (state#set_mode State.Copy)#set_trace [] in

            (* We set the first position in the file to include to
               line [1]. *)

            let pos'   = (state#pos#set_file incl_file)#set_line 1 in
            let state' = state'#set_pos pos' in

            (* We perform a recursive call which will preprocess the
               file to include, because we thread the new state
               [state'] we just created, after saving the include
               directory in it with a call to [push_dir]. *)

            let state' = state'#set_incl (Filename.dirname incl_path) in
            let state' = callback state' incl_buf in

            (* After returning from the recursive call, we restore the
               state before the call, but we retain and commit some
               information from the state returned by the call: the
               symbol environment, the opened channels and the imports
               (#import of modules). The first because we want to
               enable an included file to contain #define and #undef
               directives, typically following the traditional design
               pattern to avoid double inclusions. The second because
               the included file may contain its own #include
               directives and therefore open new input channels, which
               will need closing when we are done. The third because
               the included file may contain dependencies from #import
               directives. *)

            (* let state = state#set_pos XXX ? in *)
            let state = state#set_env     state'#env in
            let state = state#set_chans   state'#chans in
            let state = state#set_imports state'#imports in

            (* We now have to prepare the linemarker that indicates
               that we returned from a file inclusion. First, we need
               the filesystem path to the current file [path], which
               we used earlier to locate the file to include. We
               format it to conform to the convention of [cpp]. *)

            let path = if String.(path = "" || path = ".") then base
                       else path ^ Filename.dir_sep ^ base in

            (* Finally we can output the linemarker. The rightmost
               flag is 2, to specify a return from an #include. The
               leftmost flag is the line number [line+1], which we
               extracted and kept safe earlier, before doing
               anything. *)

            let () = state#print (sprintf "\n# %i %S 2\n" (line+1) path)

            (* We can now resume preprocessing the current file. *)

            in callback state lexbuf

          (* If in skip mode, we resume scanning. The #include and its
             argument will be missing in the output. *)

          else callback state lexbuf

(* Scanning #import directives *)

let import_action ~callback hash_pos state lexbuf =
  match Directive.scan_import hash_pos state lexbuf with
    Stdlib.Error (region, error) -> fail state region error
  | Ok (state, import, _, _) ->
      let ()            = state#copy_nl lexbuf in
      let state         = fst (state#newline lexbuf) in
      let import_region = import#region
      and import_file   = import#file_path.Region.value
      and module_name   = import#module_name.Region.value in
      let state =
        if state#is_copy then
          let file =
            match state#parent with
              Some parent -> parent
            | None        -> state#pos#file in
          (* We resolve the file names to be included. *)
          let external_dirs =
            ModRes.get_dependencies ~file state#mod_res in
          let import_path =
            match find state#incl import_file external_dirs with
              None ->
                Error.File_not_found import_file
                |> fail state import_region
            | Some import_path -> import_path
          in state#push_import import_path module_name
        else state
      in callback state lexbuf

(* Scanning #if directives *)

let if_action ~callback dir_region state lexbuf =
  match Directive.scan_if dir_region#start state lexbuf with
    Ok (state, bool_expr, _, _) -> (* The directive and ending dropped. *)
      let open State in
      let ()    = state#copy_nl lexbuf in
      let state = fst (state#newline lexbuf) in
      let ast   = bool_expr#expression in
      let mode  = if E_AST.eval state#env ast then Copy else Skip in
      let mode  = if state#is_copy then mode else Skip in
      let state = extend state (If state#mode) mode dir_region
      in callback state lexbuf
  | Error (region, error) -> fail state region error

(* Scanning #elif directives *)

let elif_action ~callback (dir_region: Region.t) state lexbuf =
  match Directive.scan_if dir_region#start state lexbuf with
    Ok (state, bool_expr, _, _) -> (* The directive and ending dropped. *)
      let open State in
      let ()    = state#copy_nl lexbuf in
      let state = fst (state#newline lexbuf) in
      let ast   = bool_expr#expression in
      let mode  = if E_AST.eval state#env ast then Copy else Skip in
      let state =
        match state#mode with
          Copy ->
            extend state (Elif Skip) Skip dir_region
        | Skip ->
            let old_mode = state#last_mode in
            let new_mode = if Caml.(old_mode = Copy) then mode else Skip
            in extend state (Elif old_mode) new_mode dir_region
      in callback state lexbuf
  | Error (region, error) -> fail state region error

(* Scanning #else directives *)

let else_action ~callback dir_region state lexbuf =
  let state, _, _ =
    Directive.scan_else dir_region#start state lexbuf in
  let ()    = state#copy_nl lexbuf in
  let state = fst (state#newline lexbuf) in
  let mode  = match state#mode with
                State.Copy -> State.Skip
              | State.Skip -> state#last_mode in
  let state = extend state State.Else mode dir_region
  in callback state lexbuf

(* Scanning #endif directives *)

let endif_action ~callback dir_region state lexbuf =
  let state, _, _ =
    Directive.scan_endif dir_region#start state lexbuf in
  let ()    = state#copy_nl lexbuf in
  let state = fst (state#newline lexbuf) in
  callback (reduce_cond state dir_region) lexbuf

(* Scanning #define directives *)

let define_action ~callback dir_region state lexbuf =
  match Directive.scan_define dir_region state lexbuf with
    Ok (state, sym, _, _) -> (* The directive is dropped. *)
      let sym   = sym#symbol.Region.value in
      let ()    = state#copy_nl lexbuf in
      let state = fst (state#newline lexbuf) in
      let state = if state#is_copy then state#add_symbol sym else state
      in callback state lexbuf
  | Error (region, error) -> fail state region error

(* Scanning #undef directives *)

let undef_action ~callback dir_region state lexbuf =
  match Directive.scan_undef dir_region state lexbuf with
    Ok (state, sym, _, _) -> (* The directive is dropped. *)
      let sym   = sym#symbol.Region.value in
      let ()    = state#copy_nl lexbuf in
      let state = fst (state#newline lexbuf) in
      let state =
        if state#is_copy then state#remove_symbol sym else state
      in callback state lexbuf
  | Error (region, error) -> fail state region error

(* Scanning #error directives *)

let error_action ~callback dir_region state lexbuf =
  match Directive.scan_error dir_region#start state lexbuf with
    Ok (state, msg, _, _) -> (* The directive is dropped. *)
      let Region.{region; value} = msg in
      let ()    = state#copy_nl lexbuf in
      let state = fst (state#newline lexbuf) in
      let state =
        if state#is_copy then
          if String.(value = "") then
            fail state dir_region (Error.Error_directive value)
          else
            fail state region (Error.Error_directive value)
        else state
      in callback state lexbuf
  | Error (region, error) -> fail state region error

(* Scanning linemarkers *)

(* IMPORTANT: Linemarkers are copied to the output. If an error occurs
   during their preprocessing, it will be reported within the input
   file, regardless of any previous linemarker. The lexer, in
   contrast, interprets the linemarkers for tokenisation and error
   reporting. *)

let linemarker_action ~callback region linenum state lexbuf =
  let hash_pos = region#start
  and length   = String.length linenum in
  let start    = region#stop#shift_bytes (-length) in
  let line_reg = Region.make ~start ~stop:region#stop in
  let linenum  = Region.{region=line_reg; value=linenum} in
  match Directive.scan_linemarker hash_pos linenum state lexbuf with
    Ok (state, line_directive, dir, _) ->
      let dir   = Directive.to_lexeme dir in
      let ()    = state#print dir.Region.value in
      let ()    = state#copy_nl lexbuf in
      let state = fst (state#newline lexbuf)
      in callback state lexbuf
  | Error (region, error) -> fail state region error

  (* END OF HEADER *)
}

(* REGULAR EXPRESSIONS *)

let utf8_bom  = "\xEF\xBB\xBF" (* Byte Order Mark for UTF-8 *)
let nl        = '\n' | '\r' | "\r\n"
let blank     = ' ' | '\t'
let digit     = ['0'-'9']
let natural   = digit | digit (digit | '_')* digit
let small     = ['a'-'z']
let capital   = ['A'-'Z']
let letter    = small | capital
let ident     = letter (letter | '_' | digit)*
let directive = '#' blank* (small+ as id)

(* Comment delimiters *)

let pascaligo_block_comment_opening  = "(*"
let pascaligo_block_comment_closing  = "*)"
let pascaligo_line_comment_opening   = "//"

let cameligo_block_comment_opening   = "(*"
let cameligo_block_comment_closing   = "*)"
let cameligo_line_comment_opening    = "//"

let reasonligo_block_comment_opening = "/*"
let reasonligo_block_comment_closing = "*/"
let reasonligo_line_comment_opening  = "//"

let jsligo_block_comment_opening     = "/*"
let jsligo_block_comment_closing     = "*/"
let jsligo_line_comment_opening      = "//"

let block_comment_opening =
   pascaligo_block_comment_opening
|   cameligo_block_comment_opening
| reasonligo_block_comment_opening
|     jsligo_block_comment_opening

let block_comment_closing =
   pascaligo_block_comment_closing
|   cameligo_block_comment_closing
| reasonligo_block_comment_closing
|     jsligo_block_comment_closing

let line_comment_opening =
   pascaligo_line_comment_opening
|   cameligo_line_comment_opening
| reasonligo_line_comment_opening
|     jsligo_line_comment_opening

(* String delimiters *)

let  pascaligo_string_delimiter = "\""
let   cameligo_string_delimiter = "\""
let reasonligo_string_delimiter = "\""
let     jsligo_string_delimiter = "\""

let string_delimiter =
   pascaligo_string_delimiter
|   cameligo_string_delimiter
| reasonligo_string_delimiter
|     jsligo_string_delimiter

(* RULES *)

(* The rule [scan] scans the input buffer for directives, strings,
   comments, blanks, new lines and end of file characters. As a
   result, either the matched input is copied to the buffer or not,
   depending on the compilation directives. Even if not in copy mode,
   new line characters are output.

   Important note: Comments and strings are recognised both in
   copy and skip mode, as GNU GCC does. *)

rule scan state = parse
  (* Markup *)

  nl? eof { if List.is_empty state#trace then state
            else
              let start  = state#pos in
              let region = Region.make ~start ~stop:start in
              fail state region Error.Missing_endif }
| nl      { (* New-line characters are always preserved. *)
            state#copy_nl lexbuf;
            scan (fst @@ state#newline lexbuf) lexbuf }
| ' '+    { state#copy lexbuf;
            scan (fst @@ state#sync lexbuf) lexbuf }
| '\t'+   { state#copy lexbuf;
            scan (fst @@ state#sync lexbuf) lexbuf }

  (* Strings *)

| string_delimiter {
    state#copy lexbuf;
    let state, quote = state#sync lexbuf in
    match Config.string with
      Some delimiter when String.(delimiter = quote.Region.value) ->
        let state = in_string quote state lexbuf
        in scan state lexbuf
    | Some _ | None -> scan state lexbuf }

  (* Comments *)

| block_comment_opening {
    state#copy lexbuf;
    let state, Region.{region; value} = state#sync lexbuf in
    let lexeme = value in
    match Config.block with
      Some block when String.(block#opening = lexeme) ->
        let region = Region.from_lexbuf lexbuf in
        let state  = in_block block region state lexbuf
        in scan state lexbuf
    | Some _ | None -> scan state lexbuf }

| line_comment_opening {
    state#copy lexbuf;
    let state, Region.{region; value} = state#sync lexbuf in
    let lexeme = value in
    match Config.line with
      Some line when String.(line = lexeme) ->
        scan (in_line state lexbuf) lexbuf
    | Some _ | None -> scan state lexbuf }

  (* Directives *)

| '#' blank* (small+ as id) {
    let state, Region.{region; _} = state#sync lexbuf in
    match id with
      "include" ->
        include_action ~callback:scan region#start state lexbuf
    | "import" ->
        import_action  ~callback:scan region#start state lexbuf
    | "define" ->
        define_action  ~callback:scan region#start state lexbuf
    | "undef" ->
        undef_action   ~callback:scan region#start state lexbuf
    | "error" ->
        error_action   ~callback:scan region       state lexbuf
    | "if" ->
        if_action      ~callback:scan region       state lexbuf
    | "elif" ->
        elif_action    ~callback:scan region       state lexbuf
    | "else" ->
        else_action    ~callback:scan region       state lexbuf
    | "endif" ->
        endif_action   ~callback:scan region       state lexbuf
    | _ -> (* IMPORTANT: Unknown directives are silently ignored. *)
           state#copy lexbuf; scan state lexbuf }

  (* Linemarkers *)

| '#' blank* (natural as linenum) {
    let state, Region.{region; _} = state#sync lexbuf in
    linemarker_action ~callback:scan region linenum state lexbuf }

  (* Others *)

| _ { let () = state#copy lexbuf in
      scan (fst @@ state#sync lexbuf) lexbuf }

(* Comments *)

and in_block block opening state = parse
  string_delimiter {
    state#copy lexbuf;
    let lexeme = Lexing.lexeme lexbuf in
    match Config.string with
      Some delimiter when String.(delimiter = lexeme) ->
        let state, opening = state#sync lexbuf in
        let state  = in_string opening state lexbuf
        in in_block block opening.Region.region state lexbuf
    | Some _ | None -> in_block block opening state lexbuf }

| block_comment_opening {
    state#copy lexbuf;
    let lexeme = Lexing.lexeme lexbuf in
    if String.(block#opening = lexeme) then
      let state  = fst (state#sync lexbuf) in
      let region = Region.from_lexbuf lexbuf in
      let state  = in_block block region state lexbuf
      in in_block block opening state lexbuf
    else in_block block opening state lexbuf }

| block_comment_closing {
    state#copy lexbuf;
    let state, Region.{value; _} = state#sync lexbuf in
    if String.(block#closing = value) then state
    else in_block block opening state lexbuf }

| nl  { state#copy lexbuf;
        let state = fst @@ state#newline lexbuf
        in in_block block opening state lexbuf }
| eof { Error.Unterminated_comment block#closing
        |> fail state opening }
| _   { state#copy lexbuf;
        let state, _ = state#sync lexbuf
        in in_block block opening state lexbuf }

(* Line comments *)

and in_line state = parse
  nl  { state#copy lexbuf; fst @@ state#newline lexbuf }
| eof { state                                     }
| _   { state#copy lexbuf;
        in_line (fst @@ state#sync lexbuf) lexbuf }
(* Strings *)

and in_string opening state = parse
  string_delimiter {
        state#copy lexbuf;
        let state, Region.{value; _} = state#sync lexbuf in
        match Config.string with
          Some delimiter when String.(delimiter = value) -> state
        | Some _ | None -> in_string opening state lexbuf }
| nl  { fail state opening.Region.region Error.Newline_in_string }
| eof { Error.Unterminated_string opening.Region.value
        |> fail state opening.Region.region }
| ['\000' - '\031'] | ['\128' - '\255'] as c
        (* Control characters and 8-bit ASCII *)
      { stop state lexbuf (Error.Invalid_character_in_string c) }
| "\\\"" | "\\\\" | _
      { state#copy lexbuf;
        in_string opening (fst @@ state#sync lexbuf) lexbuf }
(* Entry point *)

and preproc state = parse
  utf8_bom { linemarker (fst @@ state#sync lexbuf) lexbuf }
| eof      { linemarker state lexbuf }
| _        { Lexbuf.rollback lexbuf; linemarker state lexbuf }

(* Printing the first linemarker *)

and linemarker state = parse
  eof { let name = Lexbuf.current_filename lexbuf in
        let ()   = if String.(name <> "") then
                     state#print (sprintf "# 1 %S\n" name)
        in state }
| _   { let ()   = Lexbuf.rollback lexbuf in
        let name = Lexbuf.current_filename lexbuf in
        let ()   = if String.(name <> "") then
                     state#print (sprintf "# 1 %S\n" name)
        in scan state lexbuf }

{
(* START OF TRAILER *)

    (* Initial environment, i.e. predefined symbols *)

    let init_env =
      let f env symbol = Env.add symbol env
      in List.fold_left ~f ~init:Env.empty Options.define

    (* Preprocessing from various sources *)

    let from_lexbuf state lexbuf =
      match preproc state lexbuf with
        state ->
          List.iter ~f:close_in state#chans;
          Ok (state#out, state#imports)
      | exception Error.Error (buffer, msg) ->
          (* See functions [fail] and [stop]. *)
          Error (Some buffer, msg)

    let from_file file =
      try
        let channel = open_in file in
        let lexbuf  = Lexing.from_channel channel in
        let ()      = Lexbuf.reset_file file lexbuf in
        let root    = Options.project_root in
        let state   = new State.t
                        ?project_root:root ~init_env (Pos.min ~file) in
        let state   = state#push_chan channel
        in from_lexbuf state lexbuf
      with Sys_error msg ->
        let error  = Error.Failed_opening (file, msg) in
        let msg    = Error.to_string error in
        let region = Region.min ~file in
        Error (None, Region.{value=msg; region})

    let from_lexbuf lexbuf =
      let file  = Lexbuf.current_filename lexbuf
      and root  = Options.project_root in
      let state = new State.t
                    ?project_root:root ~init_env (Pos.min ~file)
      in from_lexbuf state lexbuf

    let from_channel = from_lexbuf <@ Lexing.from_channel

    let from_string = from_lexbuf <@ Lexing.from_string

    let from_buffer = from_string <@ Buffer.contents

  end (* of functor [Make] *)

(* END OF TRAILER *)
}
