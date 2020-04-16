(* Simple preprocessor based on C#, to be processed by [ocamllex]. *)

{
(* START OF HEADER *)

module Region = Simple_utils.Region
module Pos = Simple_utils.Pos

let sprintf = Printf.sprintf

(* Rolling back one lexeme _within the current semantic action_ *)

let rollback buffer =
  let open Lexing in
  let len = String.length (lexeme buffer) in
  let pos_cnum = buffer.lex_curr_p.pos_cnum - len in
  buffer.lex_curr_pos <- buffer.lex_curr_pos - len;
  buffer.lex_curr_p <- {buffer.lex_curr_p with pos_cnum}

(* STRING PROCESSING *)

(* The value of [mk_str len p] ("make string") is a string of length
   [len] containing the [len] characters in the list [p], in reverse
   order. For instance, [mk_str 3 ['c';'b';'a'] = "abc"]. *)

 let mk_str (len: int) (p: char list) : string =
  let () = assert (len = List.length p) in
  let bytes = Bytes.make len ' ' in
  let rec fill i = function
         [] -> bytes
  | char::l -> Bytes.set bytes i char; fill (i-1) l
  in fill (len-1) p |> Bytes.to_string

(* The type [mode] defines the two scanning modes of the preprocessor:
   either we copy the current characters or we skip them. *)

type mode = Copy | Skip

(* Trace of directives

   We keep track of directives #if, #elif, #else, #region and #endregion.
*)

type cond  = If of mode | Elif of mode | Else | Region
type trace = cond list

(* Line offsets

   The value [Inline] of type [offset] means that the current location
   cannot be reached from the start of the line with only white
   space. The same holds for the special value [Prefix 0]. Values of
   the form [Prefix n] mean that the current location can be reached
   from the start of the line with [n] white spaces (padding). These
   distinctions are needed because preprocessor directives cannot
   occur inside lines.
*)

type offset = Prefix of int | Inline

(* Environments *)

module Env = Set.Make (String)

let rec eval env =
  let open E_AST
in function
   Or (e1,e2) -> eval env e1 || eval env e2
| And (e1,e2) -> eval env e1 && eval env e2
|  Eq (e1,e2) -> eval env e1 = eval env e2
| Neq (e1,e2) -> eval env e1 != eval env e2
|       Not e -> not (eval env e)
|        True -> true
|       False -> false
|    Ident id -> Env.mem id env

(* The type [state] groups the information that needs to be threaded
   along the scanning functions:
     * the field [env] records the symbols defined;
     * the field [mode] informs whether the preprocessor is in copying or
       skipping mode;
     * the field [offset] tells whether the current location can be
       reached from the start of the line with only white space;
     * the field [trace] is a stack of previous, still active conditional
       directives;
     * the field [out] keeps the output buffer;
     * the field [incl] is a list of opened input channels (#include);
     * the field [opt] holds the CLI options;
     * the field [dir] is the file system's path to the the current input
       file.
*)

type state = {
  env     : Env.t;
  mode    : mode;
  offset  : offset;
  trace   : trace;
  out     : Buffer.t;
  incl    : in_channel list;
  opt     : EvalOpt.options;
  dir     : string list
}

(* Directories *)

let push_dir dir state =
  if dir = "." then state else {state with dir = dir :: state.dir}

let mk_path state =
  String.concat Filename.dir_sep (List.rev state.dir)

(* ERRORS *)

type error =
  Directive_inside_line
| Missing_endif
| Invalid_line_indicator of string
| No_line_indicator
| End_line_indicator
| Newline_in_string
| Open_string
| Dangling_endif
| Open_region_in_conditional
| Dangling_endregion
| Conditional_in_region
| If_follows_elif
| Else_follows_else
| Dangling_else
| Elif_follows_else
| Dangling_elif
| Reserved_symbol of string
| Multiply_defined_symbol of string
| Error_directive of string
| Parse_error
| No_line_comment_or_blank
| Invalid_symbol
| File_not_found of string
| Invalid_character of char

let error_to_string = function
  Directive_inside_line ->
    sprintf "Directive inside a line."
| Missing_endif ->
    sprintf "Missing #endif directive."
| Invalid_line_indicator id ->
    sprintf "Invalid line indicator \"%s\".\n\
             Hint: Try \"default\" or \"hidden\"." id
| No_line_indicator ->
    sprintf "Missing line indicator."
| End_line_indicator ->
    sprintf "Invalid ending of numerical line indicator.\n\
             Hint: Try a string, end of line, or a line comment."
| Newline_in_string ->
    sprintf "Invalid newline character in string."
| Open_string ->
    sprintf "Unterminated string.\n\
             Hint: Close with double quotes."
| Dangling_endif ->
    sprintf "Dangling #endif directive.\n\
             Hint: Remove it or add a #if before."
| Open_region_in_conditional ->
    sprintf "Unterminated of #region in conditional.\n\
             Hint: Close with #endregion before #endif."
| Dangling_endregion ->
    sprintf "Dangling #endregion directive.\n\
             Hint: Remove it or use #region before."
| Conditional_in_region ->
    sprintf "Conditional in region.\n\
             Hint: Remove the conditional or the region."
| If_follows_elif ->
    sprintf "Directive #if found in a clause #elif."
| Else_follows_else ->
    sprintf "Directive #else found in a clause #else."
| Dangling_else ->
    sprintf "Directive #else without #if."
| Elif_follows_else ->
    sprintf "Directive #elif found in a clause #else."
| Dangling_elif ->
    sprintf "Dangling #elif directive.\n\
             Hint: Remove it or add a #if before."
| Reserved_symbol sym ->
    sprintf "Reserved symbol \"%s\".\n\
             Hint: Use another symbol." sym
| Multiply_defined_symbol sym ->
    sprintf "Multiply-defined symbol \"%s\".\n\
             Hint: Change the name or remove one definition." sym
| Error_directive msg ->
    msg
| Parse_error ->
    "Parse error in expression."
| No_line_comment_or_blank ->
    "Line comment or whitespace expected."
| Invalid_symbol ->
    "Expected a symbol (identifier)."
| File_not_found name ->
    sprintf "File \"%s\" to include not found." name
| Invalid_character c ->
    E_Lexer.error_to_string (E_Lexer.Invalid_character c)

let format ?(offsets=true) Region.{region; value} ~file =
  let msg   = error_to_string value
  and reg   = region#to_string ~file ~offsets `Byte in
  let value = sprintf "Preprocessing error %s:\n%s" reg msg
  in Region.{value; region}

exception Error of (Buffer.t * error Region.reg)

let mk_reg buffer =
  let start  = Lexing.lexeme_start_p buffer |> Pos.from_byte
  and stop   = Lexing.lexeme_end_p buffer |> Pos.from_byte
  in Region.make ~start ~stop

(* IMPORTANT : Make sure the function [stop] remains the only one
   raising [Error]. *)

let stop value state region =
  List.iter close_in state.incl;
  raise (Error (state.out, Region.{region; value}))

let fail error state buffer = stop error state (mk_reg buffer)

(* The function [reduce_cond] is called when a #endif directive is
   found, and the trace (see type [trace] above) needs updating. *)

let reduce_cond state region =
  let rec reduce = function
                [] -> stop Dangling_endif state region
  | If mode::trace -> {state with mode; trace; offset = Prefix 0}
  |      Region::_ -> stop Open_region_in_conditional state region
  |       _::trace -> reduce trace
  in reduce state.trace

(* The function [reduce_region] is called when a #endregion directive is
   read, and the trace needs updating. *)

let reduce_region state region =
  match state.trace with
               [] -> stop Dangling_endregion state region
  | Region::trace -> {state with trace; offset = Prefix 0}
  |             _ -> stop Conditional_in_region state region

(* The function [extend] is called when encountering conditional
   directives #if, #else and #elif. As its name suggests, it extends
   the current trace with the current conditional directive, whilst
   performing some validity checks. *)

let extend cond state region =
  match cond, state.trace with
    If _, Elif _::_ -> stop If_follows_elif state region
  | Else,   Else::_ -> stop Else_follows_else state region
  | Else,        [] -> stop Dangling_else state region
  | Elif _, Else::_ -> stop Elif_follows_else state region
  | Elif _,      [] -> stop Dangling_elif state region
  |     hd,     tl  -> hd::tl

(* The function [last_mode] seeks the last mode as recorded in the
   trace (see type [trace] above). *)

let rec last_mode = function
                        [] -> assert false
| (If mode | Elif mode)::_ -> mode
|                 _::trace -> last_mode trace

(* Finding a file to #include *)

let rec find base = function
         [] -> None
| dir::dirs ->
    let path =
      if dir = "." || dir = "" then base
      else dir ^ Filename.dir_sep ^ base in
    try Some (path, open_in path) with
      Sys_error _ -> find base dirs

let find dir file libs =
  let path =
    if dir = "." || dir = "" then file
    else dir ^ Filename.dir_sep ^ file in
  try Some (path, open_in path) with
    Sys_error _ ->
      let base = Filename.basename file in
      if base = file then find file libs else None

(* PRINTING *)

(* Copying the current lexeme to [stdout] *)

let copy state buffer = Buffer.add_string state.out (Lexing.lexeme buffer)

(* End of lines *)

let proc_nl state buffer = Lexing.new_line buffer; copy state buffer

(* Copying a string *)

let print state string = Buffer.add_string state.out string

(* Expanding the offset into whitespace *)

let expand_offset state =
  match state.offset with
    Prefix 0 | Inline -> ()
  | Prefix n -> print state (String.make n ' ')

(* Evaluating a preprocessor expression

   The evaluation of conditional directives may involve symbols whose
   value may be defined using #define directives, or undefined by
   means of #undef. Therefore, we need to evaluate conditional
   expressions in an environment made of a set of defined symbols.

   Note that we rely on an external lexer and parser for the
   conditional expressions. See modules [E_Lexer] and [E_Parser].
*)

let expr state buffer : mode =
  let ast =
    try E_Parser.expr E_Lexer.scan buffer with
      E_Lexer.Error Region.{value; region} ->
        (match value with
           E_Lexer.Invalid_character c ->
             stop (Invalid_character c) state region)
    | E_Parser.Error ->
        fail Parse_error state buffer in
  let () = print state "\n" in
  if eval state.env ast then Copy else Skip

(* DIRECTIVES *)

let directives = [
  "define"; "elif"; "else"; "endif"; "endregion"; "error";
  "if"; "include"; (*"line";*) "region"; "undef" (* "; warning" *)
]

(* END OF HEADER *)
}

(* REGULAR EXPRESSIONS *)

let nl        = '\n' | '\r' | "\r\n"
let blank     = ' ' | '\t'
let digit     = ['0'-'9']
let natural   = digit | digit (digit | '_')* digit
let small     = ['a'-'z']
let capital   = ['A'-'Z']
let letter    = small | capital
let ident     = letter (letter | '_' | digit)*
let directive = '#' (blank* as space) (small+ as id)

(* Rules *)

(* The rule [scan] scans the input buffer for directives, strings,
   comments, blanks, new lines and end of file characters. As a
   result, either the matched input is copied to [stdout] or not,
   depending on the compilation directives. If not copied, new line
   characters are output.

   Scanning is triggered by the function call [scan env mode offset
   trace lexbuf], where [env] is the set of defined symbols
   (introduced by `#define'), [mode] specifies whether we are copying
   or skipping the input, [offset] informs about the location in the
   line (either there is a prefix of blanks, or at least a non-blank
   character has been read), and [trace] is the stack of conditional
   directives read so far.

   The first call is [scan {env=Env.empty; mode=Copy; offset = Prefix
   0; trace=[]; incl=[]; opt}], meaning that we start with an empty
   environment, that copying the input is enabled by default, and that
   we are at the start of a line and no previous conditional
   directives have been read yet. The field [opt] is the CLI options.

   When an "#if" is matched, the trace is extended by the call [extend
   lexbuf (If mode) trace], during the evaluation of which the
   syntactic validity of having encountered an "#if" is checked (for
   example, it would be invalid had an "#elif" been last read). Note
   that the current mode is stored in the trace with the current
   directive -- that mode may be later restored (see below for some
   examples). Moreover, the directive would be deemed invalid if its
   current position in the line (that is, its offset) were not
   preceeded by blanks or nothing, otherwise the rule [expr] is called
   to scan the boolean expression associated with the "#if": if it
   evaluates to [true], the result is [Copy], meaning that we may copy
   what follows, otherwise skip it -- the actual decision depending on
   the current mode. That new mode is used if we were in copy mode,
   and the offset is reset to the start of a new line (as we read a
   new line in [expr]); otherwise we were in skipping mode and the
   value of the conditional expression must be ignored (but not its
   syntax), and we continue skipping the input.

   When an "#else" is matched, the trace is extended with [Else],
   then, if the directive is not at a wrong offset, the rest of the
   line is scanned with [skip_line]. If we were in copy mode, the new
   mode toggles to skipping mode; otherwise, the trace is searched for
   the last encountered "#if" of "#elif" and the associated mode is
   restored.

   The case "#elif" is the result of the fusion (in the technical
   sense) of the code for dealing with an "#else" followed by an
   "#if".

   When an "#endif" is matched, the trace is reduced, that is, all
   conditional directives are popped until an [If mode'] is found and
   [mode'] is restored as the current mode.

   Consider the following four cases, where the modes (Copy/Skip) are
   located between the lines:

                    Copy ----+                          Copy ----+
   #if true                  |       #if true                    |
                    Copy     |                          Copy     |
   #else                     |       #else                       |
                +-- Skip --+ |                      +-- Skip --+ |
     #if true   |          | |         #if false    |          | |
                |   Skip   | |                      |   Skip   | |
     #else      |          | |         #else        |          | |
                +-> Skip   | |                      +-> Skip   | |
     #endif                | |         #endif                  | |
                    Skip <-+ |                          Skip <-+ |
   #endif                    |       #endif                      |
                    Copy <---+                          Copy <---+


                +-- Copy ----+                          Copy --+-+
   #if false    |            |       #if false                 | |
                |   Skip     |                          Skip   | |
   #else        |            |       #else                     | |
                +-> Copy --+ |                    +-+-- Copy <-+ |
     #if true              | |         #if false  | |            |
                    Copy   | |                    | |   Skip     |
     #else                 | |         #else      | |            |
                    Skip   | |                    | +-> Copy     |
     #endif                | |         #endif     |              |
                    Copy <-+ |                    +---> Copy     |
   #endif                    |       #endif                      |
                    Copy <---+                          Copy <---+

   The following four cases feature #elif. Note that we put between
   brackets the mode saved for the #elif, which is sometimes restored
   later.

                    Copy --+                            Copy --+
   #if true                |         #if true                  |
                    Copy   |                            Copy   |
   #elif true   +--[Skip]  |         #elif false    +--[Skip]  |
                |   Skip   |                        |   Skip   |
   #else        |          |         #else          |          |
                +-> Skip   |                        +-> Skip   |
   #endif                  |         #endif                    |
                    Copy <-+                            Copy <-+


                +-- Copy --+-+                      +-- Copy ----+
   #if false    |          | |       #if false      |            |
                |   Skip   | |                      |   Skip     |
   #elif true   +->[Copy]  | |       #elif false    +->[Copy]--+ |
                    Copy <-+ |                          Skip   | |
   #else                     |       #else                     | |
                    Skip     |                          Copy <-+ |
   #endif                    |       #endif                      |
                    Copy <---+                          Copy <---+

   Note how "#elif" indeed behaves like an "#else" followed by an
   "#if", and the mode stored with the data constructor [Elif]
   corresponds to the mode before the virtual "#if".

   Important note: Comments and strings are recognised as such only in
   copy mode, which is a different behaviour from the preprocessor of
   GNU GCC, which always does.
*)

rule scan state = parse
  nl    { expand_offset state; proc_nl state lexbuf;
          scan {state with offset = Prefix 0} lexbuf }
| blank { match state.offset with
            Prefix n ->
              scan {state with offset = Prefix (n+1)} lexbuf
          | Inline ->
              if state.mode = Copy then copy state lexbuf;
              scan state lexbuf }
| directive {
    if   not (List.mem id directives)
    then begin
           if state.mode = Copy then copy state lexbuf;
           scan state lexbuf
         end
    else
    if   state.offset = Inline
    then fail Directive_inside_line state lexbuf
    else
    let region = mk_reg lexbuf in
    match id with
      "include" ->
        let line = Lexing.(lexbuf.lex_curr_p.pos_lnum)
        and file = Lexing.(lexbuf.lex_curr_p.pos_fname) in
        let base = Filename.basename file
        and reg, incl_file = scan_inclusion state lexbuf in
        let incl_dir = Filename.dirname incl_file in
        let path = mk_path state in
        let incl_path, incl_chan =
          match find path incl_file state.opt#libs with
            Some p -> p
          | None -> stop (File_not_found incl_file) state reg in
        let () = print state (sprintf "\n# 1 \"%s\" 1\n" incl_path) in
        let incl_buf = Lexing.from_channel incl_chan in
        let () =
          let open Lexing in
          incl_buf.lex_curr_p <-
            {incl_buf.lex_curr_p with pos_fname = incl_file} in
        let state  = {state with incl = incl_chan::state.incl} in
        let state' = {state with mode=Copy; trace=[]} in
        let state' = scan (push_dir incl_dir state') incl_buf in
        let state  = {state with env=state'.env; incl=state'.incl} in
        let path   = if path = "" then base
                     else path ^ Filename.dir_sep ^ base in
        print state (sprintf "\n# %i \"%s\" 2" (line+1) path);
        scan state lexbuf
    | "if" ->
        let mode  = expr state lexbuf in
        let mode  = if state.mode = Copy then mode else Skip in
        let trace = extend (If state.mode) state region in
        let state = {state with mode; offset = Prefix 0; trace}
        in scan state lexbuf
    | "else" ->
        let () = skip_line state lexbuf in
        let mode = match state.mode with
                     Copy -> Skip
                   | Skip -> last_mode state.trace in
        let trace = extend Else state region
        in scan {state with mode; offset = Prefix 0; trace} lexbuf
    | "elif" ->
        let mode = expr state lexbuf in
        let trace, mode =
          match state.mode with
            Copy -> extend (Elif Skip) state region, Skip
          | Skip -> let old_mode = last_mode state.trace
                    in extend (Elif old_mode) state region,
                       if old_mode = Copy then mode else Skip
        in scan {state with mode; offset = Prefix 0; trace} lexbuf
    | "endif" ->
        skip_line state lexbuf;
        scan (reduce_cond state region) lexbuf
    | "define" ->
        let id, region = variable state lexbuf in
        if id="true" || id="false"
        then stop (Reserved_symbol id) state region;
        if Env.mem id state.env
        then stop (Multiply_defined_symbol id) state region;
        let state = {state with env = Env.add id state.env;
                                offset = Prefix 0}
        in scan state lexbuf
    | "undef" ->
        let id, _ = variable state lexbuf in
        let state = {state with env = Env.remove id state.env;
                                offset = Prefix 0}
        in scan state lexbuf
    | "error" ->
        stop (Error_directive (message [] lexbuf)) state region
    | "region" ->
        let msg = message [] lexbuf
        in expand_offset state;
           print state ("#" ^ space ^ "region" ^ msg ^ "\n");
           let state =
             {state with offset = Prefix 0; trace=Region::state.trace}
           in scan state lexbuf
    | "endregion" ->
        let msg = message [] lexbuf
        in expand_offset state;
           print state ("#" ^ space ^ "endregion" ^ msg ^ "\n");
           scan (reduce_region state region) lexbuf
(*
    | "line" ->
        expand_offset state;
        print state ("#" ^ space ^ "line");
        line_ind state lexbuf;
        scan {state with offset = Prefix 0} lexbuf

    | "warning" ->
        let start_p, end_p = region in
        let msg = message [] lexbuf in
        let open Lexing
        in prerr_endline
             ("Warning at line " ^ string_of_int start_p.pos_lnum
             ^ ", char "
             ^ string_of_int (start_p.pos_cnum - start_p.pos_bol)
             ^ "--" ^ string_of_int (end_p.pos_cnum - end_p.pos_bol)
             ^ ":\n" ^ msg);
           scan env mode (Prefix 0) trace lexbuf
 *)
    | _ -> assert false
  }
| eof   { match state.trace with
            [] -> expand_offset state; state
          |  _ -> fail Missing_endif state lexbuf }
| '"'   { if state.mode = Copy then
            begin
              expand_offset state;
              copy state lexbuf;
              in_string (mk_reg lexbuf) state lexbuf
            end;
          scan {state with offset=Inline} lexbuf }
| "//"  { if state.mode = Copy then
            begin
              expand_offset state;
              copy state lexbuf;
              in_line_com state lexbuf
            end;
          scan {state with offset=Inline} lexbuf }
| "/*"  { if state.mode = Copy then
            begin
              expand_offset state;
              copy state lexbuf;
              if state.opt#lang = `ReasonLIGO then
                reasonLIGO_com (mk_reg lexbuf) state lexbuf
            end;
          scan {state with offset=Inline} lexbuf }
| "(*"  { if state.mode = Copy then
            begin
              expand_offset state;
              copy state lexbuf;
              if state.opt#lang = `CameLIGO
               || state.opt#lang = `PascaLIGO then
                cameLIGO_com (mk_reg lexbuf) state lexbuf
            end;
          scan {state with offset=Inline} lexbuf }
| _     { if state.mode = Copy then
            begin
              expand_offset state;
              copy state lexbuf
            end;
          scan {state with offset=Inline} lexbuf }

(* Support for #define and #undef *)

and variable state = parse
  blank+ { let id = symbol state lexbuf
           in skip_line state lexbuf; id }

and symbol state = parse
  ident as id { id, mk_reg lexbuf                }
| _           { fail Invalid_symbol state lexbuf }

(*
(* Line indicator (#line) *)

and line_ind state = parse
  blank* { copy state lexbuf; line_indicator state lexbuf }

and line_indicator state = parse
  natural  { copy state lexbuf; end_indicator state lexbuf }
| ident as id {
    match id with
      "default" | "hidden" ->
        print state (id ^ message [] lexbuf)
    | _ -> fail (Invalid_line_indicator id) state lexbuf   }
| _ { fail No_line_indicator state lexbuf                  }

and end_indicator state = parse
  blank+ { copy state lexbuf; end_indicator state lexbuf }
| nl     { proc_nl state lexbuf                          }
| eof    { copy state lexbuf                             }
| "//"   { copy state lexbuf;
           print state (message [] lexbuf ^ "\n")        }
| '"'    { copy state lexbuf;
           in_string (mk_reg lexbuf) state lexbuf;
           opt_line_com state lexbuf                     }
| _      { fail End_line_indicator state lexbuf          }

and opt_line_com state = parse
  nl     { proc_nl state lexbuf                         }
| eof    { copy state lexbuf                            }
| blank+ { copy state lexbuf; opt_line_com state lexbuf }
| "//"   { print state ("//" ^ message [] lexbuf)       }
 *)

(* New lines and verbatim sequence of characters *)

and skip_line state = parse
  nl     { proc_nl state lexbuf                       }
| blank+ { skip_line state lexbuf                     }
| "//"   { in_line_com {state with mode=Skip} lexbuf  }
| _      { fail No_line_comment_or_blank state lexbuf }
| eof    { ()                                         }

and message acc = parse
  nl     { Lexing.new_line lexbuf;
           mk_str (List.length acc) acc }
| eof    { mk_str (List.length acc) acc }
| _ as c { message (c::acc) lexbuf      }

(* Comments *)

and in_line_com state = parse
  nl  { proc_nl state lexbuf                         }
| eof { ()                                           }
| _   { if state.mode = Copy then copy state lexbuf;
        in_line_com state lexbuf                     }

and reasonLIGO_com opening state = parse
  nl   { proc_nl state lexbuf; reasonLIGO_com opening state lexbuf }
| "*/" { copy state lexbuf                                         }
| eof  { ()                                                        }
| _    { copy state lexbuf; reasonLIGO_com opening state lexbuf    }

and cameLIGO_com opening state = parse
  nl   { proc_nl state lexbuf; cameLIGO_com opening state lexbuf }
| "*)" { copy state lexbuf                                       }
| eof  { ()                                                      }
| _    { copy state lexbuf; cameLIGO_com opening state lexbuf    }

(* Included filename *)

and scan_inclusion state = parse
  blank+ { scan_inclusion state lexbuf                    }
| '"'    { in_inclusion (mk_reg lexbuf) [] 0 state lexbuf }

and in_inclusion opening acc len state = parse
  '"'    { let closing = mk_reg lexbuf
           in Region.cover opening closing,
              mk_str len acc                                  }
| nl     { fail Newline_in_string state lexbuf                }
| eof    { stop Open_string state opening                     }
| _ as c { in_inclusion opening (c::acc) (len+1) state lexbuf }

(* Strings *)

and in_string opening state = parse
  "\\\"" { copy state lexbuf; in_string opening state lexbuf }
| '"'    { copy state lexbuf                                 }
| eof    { ()                                                }
| _      { copy state lexbuf; in_string opening state lexbuf }

and preproc state = parse
  eof { state }
| _   { let open Lexing in
        let ()   = rollback lexbuf in
        let name = lexbuf.lex_start_p.pos_fname in
        let ()   = if name <> "" then
                     print state (sprintf "# 1 \"%s\"\n" name)
        in scan state lexbuf }

{
  (* START OF TRAILER *)

(* The function [lex] is a wrapper of [scan], which also checks that
   the trace is empty at the end.  Note that we discard the state at
   the end. *)

let lex opt buffer =
  let path = buffer.Lexing.lex_curr_p.Lexing.pos_fname in
  let dir  = [Filename.dirname path] in
  let state = {
    env    = Env.empty;
    mode   = Copy;
    offset = Prefix 0;
    trace  = [];
    out    = Buffer.create 80;
    incl   = [];
    opt;
    dir
  } in
  match preproc state buffer with
    state -> List.iter close_in state.incl;
             Stdlib.Ok state.out
  | exception Error e -> Stdlib.Error e

(* END OF TRAILER *)
}
