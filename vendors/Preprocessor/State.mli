(* State threaded along the scanning functions of [LowAPI] *)

(* Vendor dependencies *)

module Region = Simple_utils.Region
module Pos    = Simple_utils.Pos

(* The type [mode] defines the two scanning modes of the preprocessor:
   either we copy the current characters or we skip them. *)

type mode = Copy | Skip

(* Trace of directives. We keep track of directives #if, #elif, and #else *)

type cond  = If of mode | Elif of mode | Else
type trace = cond list

(* The type [state] groups the information that needs to be
   threaded along the scanning functions:

     * the field [env] records the symbols defined by #define and not
       undefined by #undef;

     * the field [mode] informs whether the preprocessor is in
       copying or skipping mode;

     * the field [trace] is a stack of previous, still active
       conditional directives in order to support the parsing of
       conditionals without resorting to a parser generator like
       [menhir];

     * the field [out] keeps the output buffer;

     * the field [chans] is a list of opened input channels (this is
       to keep track of embedded file inclusions by #include and close
       them when we are done and avoid a memory leak (channels are not
       collected by the OCaml collector));

     * the field [incl] is isomorphic to the file system path to
       the current input file, and it is changed to that of any
       included file;

     * the field [imports] is a list of (filename, module) imports
       (#import);

     * the field [parent] points to the parent file when processing
       nested #include;

     * the field [pos] records the current position in the input;

     * the field [set_pos] sets a new current position in the input,
       in order to support the processing of #include and the
       linemarkers it generates;

     * the field [sync] reads the lexing buffer after it has been
       matched by a regular expression in a scannning rule, and
       extracts the new state and a region packed with the scanned
       lexeme;

     * the field [newline] must be called on each newline character of
       the input. *)

type file_path = string
type module_name = string
type lexeme = string

class t :
  ?project_root:file_path -> ?init_env:Env.t -> Pos.t ->
  object ('state)
    method pos     : Pos.t
    method set_pos : Pos.t -> 'state
    method sync    : Lexing.lexbuf -> 'state * lexeme Region.reg
    method newline : Lexing.lexbuf -> 'state * lexeme Region.reg

    method env     : Env.t
    method mode    : mode
    method trace   : trace
    method out     : Buffer.t
    method chans   : in_channel list
    method incl    : file_path
    method imports : (file_path * module_name) list
    method parent  : file_path option

  (* DIRECTORIES *)

    method set_incl   : file_path -> 'state
    method set_parent : file_path -> 'state
    method orphan     : 'state

  (* CONDITIONAL DIRECTIVES *)

  (* Predicate *)

    method is_copy : bool

  (* The method [reduce_cond] is called when a #endif directive is
     found, and the trace (see type [trace] above) needs updating. *)

    method reduce_cond : ('state, Error.t) result

  (* The method [extend] is called upon encountering conditional
     directives #if, #else and #elif. As its name suggests, it extends
     the current trace with the current conditional directive, whilst
     performing some validity checks (what would otherwise be
     interpreted as syntax for conditionals). *)

    method extend : cond -> mode -> ('state, Error.t) result

  (* Setting the trace *)

    method set_trace : trace -> 'state

  (* MODE *)

  (* Setting the mode *)

    method set_mode : mode -> 'state

  (* The function [last_mode] seeks the last mode as recorded in the
     trace (see type [trace] above). *)

    method last_mode : mode

  (* PRINTING *)

  (* Copying the current lexeme to the buffer if the mode is [Copy],
     otherwise a no-operation. *)

    method copy : Lexing.lexbuf -> unit

  (* End of lines are always copied. ALWAYS AND ONLY USE AFTER
     SCANNING newline characters (nl). *)

    method copy_nl : Lexing.lexbuf -> unit

  (* Copying a string *)

    method print : string -> unit

  (* SYMBOL ENVIRONMENT *)

    method set_env       : Env.t -> 'state
    method add_symbol    : string -> 'state
    method remove_symbol : string -> 'state

  (* INPUT CHANNELS *)

    method set_chans : in_channel list -> 'state
    method push_chan : in_channel -> 'state

  (* MODULES IMPORTS AND PATH RESOLUTION *)

    method set_imports : (file_path * module_name) list -> 'state
    method push_import : file_path -> string -> 'state
    method mod_res     : ModRes.t option
  end
