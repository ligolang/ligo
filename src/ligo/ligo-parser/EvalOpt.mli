(* Parsing the command-line option for testing the LIGO lexer and
   parser *)

(* If the value [offsets] is [true], then the user requested that
   messages about source positions and regions be expressed in terms
   of horizontal offsets. *)

val offsets : bool

(* If the value [mode] is [`Byte], then the unit in which source
   positions and regions are expressed in messages is the byte. If
   [`Point], the unit is unicode points. *)

val mode : [`Byte | `Point]

(* If the option [verbose] is set to a list of predefined stages of
   the compiler chain, then more information may be displayed about
   those stages. *)

val verbose : Utils.String.Set.t

(* If the value [input] is [None] or [Some "-"], the input is standard
   input. If [Some f], then the input is the file whose name (file
   path) is [f]. *)

val input : string option

(* Paths where to find LIGO files for inclusion *)

val libs : string list

(* If the value [cmd] is
    * [Quiet], then no output from the lexer and parser should be
      expected, safe error messages: this is the default value;
    * [Copy], then lexemes of tokens and markup will be printed to
      standard output, with the expectation of a perfect match with
      the input file;
    * [Units], then the tokens and markup will be printed to standard
      output, that is, the abstract representation of the concrete
      lexical syntax;
    * [Tokens], then the tokens only will be printed.
*)

type command = Quiet | Copy | Units | Tokens

val cmd : command
