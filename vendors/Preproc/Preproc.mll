(* Simple preprocessor based on C#, to be processed by [ocamllex]. *)

{
(* START HEADER *)

module Region = Simple_utils.Region
(*module Pos = Simple_utils.Pos*)

let sprintf = Printf.sprintf

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

(* The call [explode s a] is the list made by pushing the characters
   in the string [s] on top of [a], in reverse order. For example,
   [explode "ba" ['c';'d'] = ['a'; 'b'; 'c'; 'd']]. *)

let explode s acc =
  let rec push = function
    0 -> acc
  | i -> s.[i-1] :: push (i-1)
in push (String.length s)

(* ERRORS *)

module Error =
  struct
    type t =
      Invalid_directive of string
    | Directive_inside_line
    | Missing_endif
    | Invalid_line_indicator of string
    | No_line_indicator
    | End_line_indicator
    | Newline_in_string
    | Unterminated_comment
    | Unterminated_string
    | Dangling_endif
    | Unterminated_region_in_conditional
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

    let to_string = function
      Invalid_directive name ->
        sprintf "Invalid directive \"%s\".\n" name
    | Directive_inside_line ->
        sprintf "Directive inside a line.\n"
    | Missing_endif ->
        sprintf "Missing #endif directive.\n"
    | Invalid_line_indicator id ->
        sprintf "Invalid line indicator \"%s\".\n\
                 Hint: Try \"default\" or \"hidden\".\n" id
    | No_line_indicator ->
        sprintf "Missing line indicator.\n"
    | End_line_indicator ->
        sprintf "Invalid ending of numerical line indicator.\n\
                 Hint: Try a string, end of line, or a line comment.\n"
    | Newline_in_string ->
        sprintf "Invalid newline character in string.\n"
    | Unterminated_comment ->
        sprintf "Unterminated comment.\n"
    | Unterminated_string ->
        sprintf "Unterminated string.\n\
                 Hint: Close with double quotes.\n"
    | Dangling_endif ->
        sprintf "Dangling #endif directive.\n\
                 Hint: Remove it or add a #if before.\n"
    | Unterminated_region_in_conditional ->
        sprintf "Unterminated of #region in conditional.\n\
                 Hint: Close with #endregion before #endif.\n"
    | Dangling_endregion ->
        sprintf "Dangling #endregion directive.\n\
                 Hint: Remove it or use #region before.\n"
    | Conditional_in_region ->
        sprintf "Conditional in region.\n\
                 Hint: Remove the conditional or the region.\n"
    | If_follows_elif ->
        sprintf "Directive #if found in a clause #elif.\n"
    | Else_follows_else ->
        sprintf "Directive #else found in a clause #else.\n"
    | Dangling_else ->
        sprintf "Directive #else without #if.\n"
    | Elif_follows_else ->
        sprintf "Directive #elif found in a clause #else.\n"
    | Dangling_elif ->
        sprintf "Dangling #elif directive.\n\
                 Hint: Remove it or add a #if before.\n"
    | Reserved_symbol sym ->
        sprintf "Reserved symbol \"%s\".\n\
                 Hint: Use another symbol.\n" sym
    | Multiply_defined_symbol sym ->
        sprintf "Multiply-defined symbol \"%s\".\n\
                 Hint: Change the name or remove one definition.\n" sym
    | Error_directive msg ->
        msg ^ "\n"
    | Parse_error ->
        "Parse error in expression.\n"
    | No_line_comment_or_blank ->
        "Line comment or whitespace expected.\n"

    let format ?(offsets=true) Region.{region; value} ~file =
      let msg   = to_string value
      and reg   = region#to_string ~file ~offsets `Byte in
      let value = sprintf "Preprocessing error %s:\n%s" reg msg
      in Region.{value; region}
end

exception Error of Error.t Region.reg

let mk_reg buffer =
  let start  = Lexing.lexeme_start_p buffer |> Pos.from_byte
  and stop   = Lexing.lexeme_end_p buffer |> Pos.from_byte
  in Region.make ~start ~stop

let stop value region = raise (Error Region.{region; value})
let fail error buffer = stop error (mk_reg buffer)

(* LEXING ENGINE *)

(* C# PREPROCESSOR DIRECTIVES *)

(* The type [mode] defines the two scanning modes of the preprocessor:
   either we copy the current characters or we skip them. *)

type mode = Copy | Skip

(* Trace of directives

   We keep track of directives #if, #elif, #else, #region and #endregion.
*)

type cond  = If of mode | Elif of mode | Else | Region
type trace = cond list

(* The function [reduce_cond] is called when a #endif directive is
   found, and the trace (see type [trace] above) needs updating. *)

let rec reduce_cond reg = function
              [] -> stop Error.Dangling_endif reg
| If mode::trace -> trace, mode
|      Region::_ -> stop Error.Unterminated_region_in_conditional reg
|       _::trace -> reduce_cond reg trace

(* The function [reduce_reg] is called when a #endregion directive is
   read, and the trace needs updating. *)

let reduce_reg reg = function
             [] -> stop Error.Dangling_endregion reg
| Region::trace -> trace
|             _ -> stop Error.Conditional_in_region reg

(* The function [extend] is called when encountering conditional
   directives #if, #else and #elif. As its name suggests, it extends
   the current trace with the current conditional directive, whilst
   performing some validity checks. *)

let extend reg cond trace =
  match cond, trace with
    If _, Elif _::_ -> stop Error.If_follows_elif reg
  | Else,   Else::_ -> stop Error.Else_follows_else reg
  | Else,        [] -> stop Error.Dangling_else reg
  | Elif _, Else::_ -> stop Error.Elif_follows_else reg
  | Elif _,      [] -> stop Error.Dangling_elif reg
  |               _ -> cond::trace

(* The function [last_mode] seeks the last mode as recorded in the
   trace (see type [trace] above). *)

let rec last_mode = function
                        [] -> assert false
| (If mode | Elif mode)::_ -> mode
|                 _::trace -> last_mode trace

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

(* Directives *)

let directives = [
  "if"; "else"; "elif"; "endif"; "define"; "undef";
  "error"; (*"warning";*) "line"; "region"; "endregion";
  "include"]

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
   along the scanning functions. *)

type state = {
  env    : Env.t;
  mode   : mode;
  offset : offset;
  trace  : trace;
  out    : Buffer.t;
  incl   : in_channel list
}

(* Evaluating a preprocessor expression

   The evaluation of conditional directives may involve symbols whose
   value may be defined using #define directives, or undefined by
   means of #undef. Therefore, we need to evaluate conditional
   expressions in an environment made of a set of defined symbols.

   Note that we rely on an external lexer and parser for the
   conditional expressions. See modules [E_Lexer] and [E_Parser].
*)

let expr state buffer =
  let ast =
    try E_Parser.expr E_Lexer.scan buffer with
      E_Parser.Error ->
        let region = mk_reg buffer in
        let value  = Error.Parse_error
        in raise (Error Region.{value; region})
  in if eval state.env ast then Copy else Skip

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

(* END OF HEADER *)
}

(* REGULAR EXPRESSIONS *)

let nl        = '\n' | '\r' | "\r\n"
let blank     = ' ' | '\t'
let digit     = ['0'-'9']
let natural   = digit | digit (digit | '_')* digit
let decimal   = digit+ '.' digit+
let small     = ['a'-'z']
let capital   = ['A'-'Z']
let letter    = small | capital
let ident     = small (letter | '_' | digit)*
let directive = '#' (blank* as space) (ident as id)

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
   0; trace=[]}], meaning that we start with an empty environment,
   that copying the input is enabled by default, and that we are at
   the start of a line and no previous conditional directives have
   been read yet.

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
  nl    { proc_nl state lexbuf;
          scan {state with offset = Prefix 0} lexbuf }
| blank { match state.offset with
            Prefix n -> scan {state with offset = Prefix (n+1)} lexbuf
          |   Inline -> copy state lexbuf; scan state lexbuf }
| directive {
    if   not (List.mem id directives)
    then fail (Error.Invalid_directive id) lexbuf;
    if   state.offset = Inline
    then fail Error.Directive_inside_line lexbuf;
    let reg = mk_reg lexbuf in
    match id with
      "include" ->
        let line = Lexing.(lexbuf.lex_curr_p.pos_lnum)
        and file = Lexing.(lexbuf.lex_curr_p.pos_fname)
                   |> Filename.basename
        and incl_file = scan_inclusion lexbuf in
        print state (sprintf "# 1 \"%s\" 1\n" incl_file);
        let incl_chan = open_in incl_file in
        let state = {state with incl = incl_chan::state.incl} in
        cat state (Lexing.from_channel incl_chan);
        print state (sprintf "# %i \"%s\" 2\n" (line+1) file);
        scan state lexbuf
    | "if" ->
        let mode  = expr state lexbuf in
        let mode  = if state.mode = Copy then mode else Skip in
        let trace = extend reg (If state.mode) state.trace in
        let state = {state with mode; offset = Prefix 0; trace}
        in scan state lexbuf
    | "else" ->
        let () = skip_line state lexbuf in
        let mode = match state.mode with
                     Copy -> Skip
                   | Skip -> last_mode state.trace in
        let trace = extend reg Else state.trace
        in scan {state with mode; offset = Prefix 0; trace} lexbuf
    | "elif" ->
        let mode = expr state lexbuf in
        let trace, mode =
          match state.mode with
            Copy -> extend reg (Elif Skip) state.trace, Skip
          | Skip -> let old_mode = last_mode state.trace
                    in extend reg (Elif old_mode) state.trace,
                       if old_mode = Copy then mode else Skip
        in scan {state with mode; offset = Prefix 0; trace} lexbuf
    | "endif" ->
        let () = skip_line state lexbuf in
        let trace, mode = reduce_cond reg state.trace
        in scan {state with mode; offset = Prefix 0; trace} lexbuf
    | "define" ->
        let id, reg = variable state lexbuf in
        if id="true" || id="false"
        then stop (Error.Reserved_symbol id) reg;
        if Env.mem id state.env
        then stop (Error.Multiply_defined_symbol id) reg;
        let state = {state with env = Env.add id state.env;
                                offset = Prefix 0}
        in scan state lexbuf
    | "undef" ->
        let id, _ = variable state lexbuf in
        let state = {state with env = Env.remove id state.env;
                                offset = Prefix 0}
        in scan state lexbuf
    | "error" ->
        stop (Error.Error_directive (message [] lexbuf)) reg
(*
    | "warning" ->
        let start_p, end_p = reg in
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
           let state =
             {state with offset = Prefix 0;
                         trace  = reduce_reg reg state.trace}
           in scan state lexbuf
    | "line" ->
        expand_offset state;
        print state ("#" ^ space ^ "line");
        line_ind state lexbuf;
        scan {state with offset = Prefix 0} lexbuf
    | _ -> assert false
  }
| eof   { match state.trace with
            [] -> expand_offset state; state
          |  _ -> fail Error.Missing_endif lexbuf }
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
              in_block_com (mk_reg lexbuf) state lexbuf
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
  blank* { let id = __ident lexbuf
           in skip_line state lexbuf; id }

and __ident = parse
  ident as id { id, mk_reg lexbuf }

(* Line indicator (#line) *)

and line_ind state = parse
  blank* { copy state lexbuf; line_indicator state lexbuf }

and line_indicator state = parse
  decimal  { copy state lexbuf; end_indicator state lexbuf }
| nl | eof { fail Error.No_line_indicator lexbuf     }
| ident as id {
    match id with
      "default" | "hidden" ->
        print state (id ^ message [] lexbuf)
    | _ -> fail (Error.Invalid_line_indicator id) lexbuf }

and end_indicator state = parse
  blank+ { copy state lexbuf; end_indicator state lexbuf  }
| nl     { copy state lexbuf; proc_nl state lexbuf        }
| eof    { copy state lexbuf                              }
| "//"   { copy state lexbuf;
           print state (message [] lexbuf ^ "\n")         }
| '"'    { copy state lexbuf;
           in_string (mk_reg lexbuf) state lexbuf;
           opt_line_com state lexbuf                      }
| _      { fail Error.End_line_indicator lexbuf           }

and opt_line_com state = parse
  nl     { proc_nl state lexbuf                         }
| eof    { copy state lexbuf                            }
| blank+ { copy state lexbuf; opt_line_com state lexbuf }
| "//"   { print state ("//" ^ message [] lexbuf)       }

(* New lines and verbatim sequence of characters *)

and skip_line state = parse
  nl     { proc_nl state lexbuf                       }
| blank+ { skip_line state lexbuf                     }
| "//"   { in_line_com {state with mode=Skip} lexbuf  }
| _      { fail Error.No_line_comment_or_blank lexbuf }

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

and in_block_com opening state = parse
  nl   { proc_nl state lexbuf; in_block_com opening state lexbuf }
| "*/" { copy state lexbuf                                       }
| eof  { stop Error.Unterminated_comment opening                 }
| _    { copy state lexbuf; in_block_com opening state lexbuf    }

(* Include a file *)

and cat state = parse
  eof { ()                                  }
| _   { copy state lexbuf; cat state lexbuf }

(* Included filename *)

and scan_inclusion = parse
  blank+ { scan_inclusion lexbuf                    }
| '"'    { in_inclusion (mk_reg lexbuf) [] 0 lexbuf }

and in_inclusion opening acc len = parse
  '"'    { mk_str len acc                               }
| nl     { fail Error.Newline_in_string lexbuf          }
| eof    { stop Error.Unterminated_string opening       }
| _ as c { in_inclusion opening (c::acc) (len+1) lexbuf }

(* Strings *)

and in_string opening state = parse
  "\\\"" { copy state lexbuf; in_string opening state lexbuf  }
| '"'    { copy state lexbuf                                  }
| nl     { fail Error.Newline_in_string lexbuf                }
| eof    { stop Error.Unterminated_string opening             }
| _      { copy state lexbuf; in_string opening state lexbuf  }


{
(* The function [lex] is a wrapper of [scan], which also checks that
   the trace is empty at the end.  Note that we discard the
   environment at the end. *)

let lex buffer =
  let state = {
    env    = Env.empty;
    mode   = Copy;
    offset = Prefix 0;
    trace  = [];
    out    = Buffer.create 80;
    incl   = []
  } in
  let state = scan state buffer
in ()

(* Exported definitions *)
(*
let trace options : unit =
  match open_in options#input with
    cin ->
      let open Lexing in
      let buffer = from_channel cin in
      let pos_fname = Filename.basename options#input in
      let () = buffer.lex_curr_p <- {buffer.lex_curr_p with pos_fname}
      in (try lex buffer with
            Error err ->
              let msg =
                Error.format ~offsets:options#offsets err ~file:options#input
              in prerr_endline msg
          | E_Lexer.Error err ->
              let msg =
                E_Lexer.Error.format ~offsets:options#offsets
                                     err ~file:options#input
              in prerr_endline msg
          | Sys_error msg -> prerr_endline msg);
         close_in cin
  | exception Sys_error msg -> prerr_endline msg
 *)
}
