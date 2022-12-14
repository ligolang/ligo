(* A library for writing UTF8-aware lexers *)

{
(* START HEADER *)

(* Vendor dependencies *)

module Region    = Simple_utils.Region
module Pos       = Simple_utils.Pos
module Lexbuf    = Simple_utils.Lexbuf
module Directive = Preprocessor.Directive
module Config    = Preprocessor.Config

(* Third-party libraries *)

module Array  = Caml.Array  (* Used in the generated code only *)

(* UTILITIES *)

let (let*) : ('a, 'e) result -> ('a -> ('b, 'e) result) -> ('b, 'e) result =
  fun r f ->
    match r with
      Ok x         -> f x
    | Error _ as e -> e

(* The functor return signature *)

module type S =
  sig
    (* Lexical units *)

    type lex_unit

    (* Utility types *)

    type file_path = string
    type message   = string Region.reg

    type units = lex_unit list

    (* LEXER INSTANCE (see README.md) *)

    (* Errors *)

    type error = {
      used_units : units;
      message    : message
    }

    (* Instances *)

    type instance = {
      input      : Lexbuf.input;
      read_units : Lexing.lexbuf -> (units, error) result;
      lexbuf     : Lexing.lexbuf;
      close      : Lexbuf.close
    }

    val open_stream : Lexbuf.input -> (instance, message) result
  end

(* THE FUNCTOR *)

module Make (Config : Config.S) (Client : Client.S) =
  struct
    type lex_unit = Client.token Unit.t

    type units = lex_unit list

    (* Errors *)

    type message = string Region.reg

    type error = {
      used_units : units;
      message    : message
    }

    (* Failure *)

    let fail state region error =
      let value      = Error.to_string error in
      let message    = Region.{value; region} in
      let used_units = List.rev state#lexical_units
      in Stdlib.Error {used_units; message}

    (* Auxiliary functions for preprocessing directives *)

    let handle_ending state lexbuf = function
      `EOL (state', ending) ->
         (state#push_newline (Some ending) lexbuf)#set_pos state'#pos
    | `EOF (state', region) ->
         (state#push_token (Client.mk_eof region))#set_pos state'#pos

    let scan_dir ~callback scan (state: Client.token State.t) lexbuf =
      let state, Region.{region; _} = state#sync lexbuf in
      let state' = new Preprocessor.State.t state#pos in
      match scan region#start state' lexbuf with
        Stdlib.Error (region, err) ->
          fail state region (Error.Invalid_directive err)
      | Ok (state', _, dir, ending) ->
          let state = state#set_pos state'#pos in
          let state = state#push_directive dir in
          let state = handle_ending state lexbuf ending
          in callback state lexbuf

    let scan_if   = scan_dir
    let scan_elif = scan_dir

    let scan_else ~callback scan state lexbuf =
      let state, Region.{region; _} = state#sync lexbuf in
      let state' = new Preprocessor.State.t state#pos in
      let _, dir, ending = scan region#start state' lexbuf in
      let state = state#push_directive dir in
      let state = handle_ending state lexbuf ending
      in callback state lexbuf

    let scan_endif = scan_else

    let scan_linemarker ~callback linenum state lexbuf =
      (* We save the state and position before the lexing buffer was
         matched. *)

      let hash_state = state in

      (* We syncronise the logical state with the matched string *)

      let state, Region.{region; _} = state#sync lexbuf in

      (* We determine the regon of the line number *)

      let length   = String.length linenum in
      let start    = region#stop#shift_bytes (-length) in
      let line_reg = Region.make ~start ~stop:region#stop in
      let linenum  = Region.{region=line_reg; value=linenum} in


      (* We make a preprocessing state and scan the expected
         linemarker. *)

      let preproc_state = new Preprocessor.State.t state#pos in

      match Directive.scan_linemarker
              hash_state#pos linenum preproc_state lexbuf
      with
        Stdlib.Error (region, error) ->
          fail hash_state region (Error.Invalid_directive error)

      | Ok (preproc_state, args, directive, ending) ->
          (* We use the current position (after reading the linemarker)
             of the preprocessing state [preproc_state] to reset the
             position of saved lexing state [hash_state]. (Remember that
             positions contain the file name.) We push the linemarker in
             that updated lexing state. *)

          let state = hash_state#set_pos preproc_state#pos in
          let state = state#push_directive directive in
          let state = state#push_newline None lexbuf in

          let arg_file = args#file_path.Region.value in

          let push state =
            let pos   = state#pos in
            let pos   = pos#set_file arg_file in
            let pos   = pos#set_line args#linenum.Region.value in
            let pos   = pos#reset_cnum in
            let state = state#set_pos pos in
            let ()    = Lexbuf.reset_file arg_file lexbuf
            in state in

          match args#flag with
            Some Region.{value=Directive.Pop; _} ->
              (* The linemarker has been produced by the end of the
                 preprocessing of an #include directive. We assume that
                 the user never writes one, otherwise preprocessing
                 may fail. More precisely, we assume that each [Push]
                 (below) is associate to one [Pop] (this case). *)
              Lexbuf.reset_file arg_file lexbuf;
              Ok state

          | None ->
              (* The linemarker is the one generated at the start of
                 the file or one written by the user. *)
              callback (push state) lexbuf

          | Some Region.{value=Directive.Push; _} ->
              (* The linemarker has been produced by the start of the
                 preprocessing of an #include directive. See case above
                 ([Pop]).

                   We call recursively [callback] to scan until a
                 linemarker with a flag "2" is found, that is, the case
                 [Pop] above. Between a [Push] linemarker and a [Pop],
                 we assume that we scan the contents of the included
                 file. *)
              let* state = callback (push state) lexbuf in

              (* The contents of the included file was scanned
                 successfully, that is, the case [Pop] above was
                 hit. We restore the position saved at the start of
                 this semantic action, that is, just before the "#" of
                 the [Push] linemarker. With that position committed
                 to the state, we call recursively [callback] to
                 resume scanning the rest of the file corresponding to
                 what was just after the original #include. *)

            callback (state#set_pos hash_state#pos) lexbuf

    (* Internal client *)

    module Client =
      struct
        let mk_string = Client.mk_string
        let mk_eof    = Client.mk_eof

        let callback state lexbuf =
          match Client.callback state lexbuf with
            Ok (token, state) ->
              Ok (state#push_token token)
          | Error message ->
              let used_units = List.rev state#lexical_units
              in Error {used_units; message}
      end

    (* Client callback with local continuation [scan] *)

    let callback_with_cont scan state lexbuf =
      let ()     = Lexbuf.rollback lexbuf in
      let* state = Client.callback state lexbuf
      in scan state lexbuf

    (* The lexer instance: the main exported data type *)

    type file_path = string

    type instance = {
      input      : Lexbuf.input;
      read_units : Lexing.lexbuf -> (units, error) result;
      lexbuf     : Lexing.lexbuf;
      close      : Lexbuf.close
    }

    (* The main function *)

    let open_stream scan input : (instance, message) result =
      let file = Lexbuf.file_from_input input in
      let read_units lexbuf =
        let state  = State.empty ~file in
        let* state = scan state lexbuf in
        Ok (List.rev state#lexical_units) in
      let* lexbuf, close = Lexbuf.from_input input
      in Ok {read_units; input; lexbuf; close}

    (* Reading UTF-8 encoded characters *)

    let scan_utf8_wrap scan_utf8 callback thread state lexbuf =
      let ()  = Lexbuf.rollback lexbuf in
      let len = thread#length in
      match scan_utf8 thread state lexbuf with
        Stdlib.Ok (thread, state) ->
          let delta = thread#length - len in
          let stop  = state#pos#shift_one_uchar delta
          in callback thread (state#set_pos stop) lexbuf
      | Error (thread, state, error) ->
          let delta  = thread#length - len in
          let stop   = state#pos#shift_one_uchar delta in
          let region = Region.make ~start:state#pos ~stop
          in fail state region error

    let open_block thread state =
      Stdlib.Error (thread, state, Error.Unterminated_comment)

(* END HEADER *)
}

(* START LEXER DEFINITION *)

(* NAMED REGULAR EXPRESSIONS *)

let utf8_bom = "\xEF\xBB\xBF" (* Byte Order Mark for UTF-8 *)
let nl       = ['\n' '\r'] | "\r\n"
let blank    = ' ' | '\t'
let digit   = ['0'-'9']
let natural = digit | digit+ digit (* Linemarkers *)

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

let pyligo_block_comment_opening     = "/*"
let pyligo_block_comment_closing     = "*/"
let pyligo_line_comment_opening      = "##"

let block_comment_opening =
   pascaligo_block_comment_opening
|   cameligo_block_comment_opening
| reasonligo_block_comment_opening
|     jsligo_block_comment_opening
|     pyligo_block_comment_opening

let block_comment_closing =
   pascaligo_block_comment_closing
|   cameligo_block_comment_closing
| reasonligo_block_comment_closing
|     jsligo_block_comment_closing
|     pyligo_block_comment_closing

let line_comment_opening =
   pascaligo_line_comment_opening
|   cameligo_line_comment_opening
| reasonligo_line_comment_opening
|     jsligo_line_comment_opening
|     pyligo_line_comment_opening

(* String delimiters *)

let  pascaligo_string_delimiter = "\""
let   cameligo_string_delimiter = "\""
let reasonligo_string_delimiter = "\""
let     jsligo_string_delimiter = "\""
let     pyligo_string_delimiter = "\""

let string_delimiter =
   pascaligo_string_delimiter
|   cameligo_string_delimiter
| reasonligo_string_delimiter
|     jsligo_string_delimiter
|     pyligo_string_delimiter

(* Preprocessing directives *)

let directive =
  "include"
| "import"
| "if"
| "elif"
| "else"
| "endif"
| "define"
| "undef"
| "error"

(* RULES (SCANNERS) *)

rule scan state = parse
  (* Markup *)

  nl    { scan (state#push_newline None lexbuf) lexbuf }
| ' '+  { scan (state#push_space        lexbuf) lexbuf }
| '\t'+ { scan (state#push_tabs         lexbuf) lexbuf }

  (* Strings *)

| string_delimiter {
    let lexeme = Lexing.lexeme lexbuf in
    match Config.string with
      Some delimiter when String.(delimiter = lexeme) ->
        let state, Region.{region; _} = state#sync lexbuf in
        let thread = Thread.make ~opening:region in
        let* thread, state = in_string thread state lexbuf in
        let token = Client.mk_string thread
        in scan (state#push_token token) lexbuf
    | Some _ | None -> callback_with_cont scan state lexbuf }

  (* Comments *)

| block_comment_opening {
    let lexeme = Lexing.lexeme lexbuf in
    match Config.block with
      Some block when String.(block#opening = lexeme) ->
        let state, Region.{region; _} = state#sync lexbuf in
        let thread = Thread.make ~opening:region in
        let thread = thread#push_string lexeme in
        let* thread, state = in_block block thread state lexbuf
        in scan (state#push_block thread) lexbuf
    | Some _ | None -> callback_with_cont scan state lexbuf }

| line_comment_opening {
    let lexeme = Lexing.lexeme lexbuf in
    match Config.line with
      Some opening when String.(opening = lexeme) ->
        let state, Region.{region; _} = state#sync lexbuf in
        let thread = Thread.make ~opening:region in
        let thread = thread#push_string lexeme in
        let* state = in_line thread state lexbuf
        in scan state lexbuf
    | Some _ | None -> callback_with_cont scan state lexbuf }

| '#' blank* (directive as id) {
    match id with
      "include" ->
        scan_dir   ~callback:scan Directive.scan_include state lexbuf
    | "import" ->
        scan_dir   ~callback:scan Directive.scan_import  state lexbuf
    | "if" ->
        scan_if    ~callback:scan Directive.scan_if      state lexbuf
    | "elif" ->
        scan_elif  ~callback:scan Directive.scan_elif    state lexbuf
    | "else" ->
        scan_else  ~callback:scan Directive.scan_else    state lexbuf
    | "endif" ->
        scan_endif ~callback:scan Directive.scan_endif   state lexbuf
    | "define" ->
        scan_dir   ~callback:scan Directive.scan_define  state lexbuf
    | "undef" ->
        scan_dir   ~callback:scan Directive.scan_undef   state lexbuf
    | "error" ->
        scan_dir   ~callback:scan Directive.scan_error   state lexbuf
    | _ ->
        callback_with_cont scan state lexbuf }

  (* Linemarkers preprocessing directives (from #include) *)

| '#' blank* (natural as linenum) {
    scan_linemarker ~callback:scan linenum state lexbuf }

  (* End-of-File: we return the final state *)

| eof { Client.callback state lexbuf }

  (* Other tokens *)

| _ { callback_with_cont scan state lexbuf }

(* Block comments *)

and in_block block thread state = parse
  string_delimiter {
    let lexeme = Lexing.lexeme lexbuf in
    match Config.string with
      Some delimiter when String.(delimiter = lexeme) ->
        let opening = thread#opening in
        let state, Region.{region; _} = state#sync lexbuf in
        let thread = thread#push_string lexeme in
        let thread = thread#set_opening region in
        let* thread, state = in_string thread state lexbuf in
        let thread = thread#push_string lexeme in
        let thread = thread#set_opening opening
        in in_block block thread state lexbuf
    | Some _ | None ->
        scan_utf8_wrap (scan_utf8 open_block) (in_block block)
                       thread state lexbuf }

| block_comment_opening {
    let lexeme = Lexing.lexeme lexbuf in
    if   String.(block#opening = lexeme)
    then let opening = thread#opening in
         let state, Region.{region; _} = state#sync lexbuf in
         let thread = thread#push_string lexeme in
         let thread = thread#set_opening region in
         let* thread, state = in_block block thread state lexbuf in
         let thread = thread#set_opening opening
         in in_block block thread state lexbuf
    else scan_utf8_wrap (scan_utf8 open_block) (in_block block)
                        thread state lexbuf }

| block_comment_closing {
    let state, Region.{value=lexeme; _} = state#sync lexbuf in
    if   String.(block#closing = lexeme)
    then Ok (thread#push_string lexeme, state)
    else scan_utf8_wrap (scan_utf8 open_block)
                        (in_block block)
                        thread state lexbuf }
| nl as nl {
    let thread = thread#push_string nl
    and state  = state#newline lexbuf
    in in_block block thread state lexbuf }

| eof { fail state thread#opening Error.Unterminated_comment }

| _ { scan_utf8_wrap (scan_utf8 open_block)
                     (in_block block)
                     thread state lexbuf }

(* Line comments *)

and in_line thread state = parse
  nl as nl { let state = state#push_line thread in
             Ok (state#push_newline None lexbuf) }
| eof      { Ok (state#push_line thread) }
| _        { let scan_utf8 =
               scan_utf8 (fun thread state -> Ok (thread, state))
             in scan_utf8_wrap scan_utf8 in_line thread state lexbuf }

(* Scanning UTF-8 encoded characters *)

and scan_utf8 if_eof thread state = parse
  eof { if_eof thread state }
| _   { let lexeme = Lexing.lexeme lexbuf in
        let thread = thread#push_string lexeme in
        let     () = state#supply (Bytes.of_string lexeme) 0 1 in
        match Uutf.decode state#decoder with
          `Uchar _     -> Ok (thread, state)
        | `Malformed _
        | `End         -> Error (thread, state,
                                 Error.Invalid_utf8_sequence)
        | `Await       -> scan_utf8 if_eof thread state lexbuf }

(* Scanning strings *)

and in_string thread state = parse
  string_delimiter {
         let state, Region.{value; region} = state#sync lexbuf in
         let lexeme = value in
         match Config.string with
           Some delimiter when String.(delimiter = lexeme) ->
             (* Closing the string *)
             Ok (thread#set_closing region, state)
         | Some _ | None -> (* Still inside the string *)
             let thread = thread#push_string lexeme
             in in_string thread state lexbuf }
| '\\' { let state, Region.{region; _} = state#sync lexbuf
         in unescape region thread state lexbuf }
| nl   { fail state thread#opening Error.Newline_in_string }
| eof  { fail state thread#opening Error.Unterminated_string }
| ['\000' - '\031'] | ['\128' - '\255'] as c
           (* Control characters and 8-bit ASCII *)
       { let _, Region.{region; _} = state#sync lexbuf in
         fail state region (Error.Invalid_character_in_string c) }
| _    { let state, Region.{value; _} = state#sync lexbuf in
         in_string (thread#push_string value) state lexbuf }

and unescape backslash thread state = parse
  string_delimiter {
         let state, Region.{value=lexeme; _} = state#sync lexbuf in
         let interpretation =
           match Config.string with
             Some delimiter when String.(delimiter = lexeme) ->
               lexeme (* E.g. unescaped \" into " *)
           | Some _ | None -> "\\" ^ lexeme (* verbatim *) in
         let thread = thread#push_string interpretation
         in in_string thread state lexbuf }
| 'n'  { let state, _ = state#sync lexbuf
         (* Unescaped "\n" into '\010': *)
         and thread = thread#push_char '\n'
         in in_string thread state lexbuf }
| '\\' { let state, Region.{value=lexeme; _} = state#sync lexbuf in
         (* Unescaped "\\" into '\\': *)
         let thread = thread#push_string lexeme
         in in_string thread state lexbuf }
| _    { let _, Region.{region; _} = state#sync lexbuf in
         let region = Region.cover backslash region in
         fail state region Undefined_escape_sequence }

(* Scanner called first *)

and init state = parse
  utf8_bom { scan (state#push_bom lexbuf) lexbuf       }
| eof      { Client.callback state lexbuf              }
| _        { Lexbuf.rollback lexbuf; scan state lexbuf }

(* END LEXER DEFINITION *)

{
(* START TRAILER *)

    let open_stream : Lexbuf.input -> (instance, message) result =
      let first_call = ref true in
      let scan state =
        (if !first_call then (first_call := false; init) else scan)
        state
      in open_stream scan

  end (* of functor [Make] *)
(* END TRAILER *)
}
