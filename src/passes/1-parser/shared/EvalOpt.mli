(** Parsing the command-line options of LIGO *)

(** The type [command] denotes some possible behaviours of the
    compiler. The constructors are
    {ul

      {li [Quiet], then no output from the lexer and parser should be
          expected, safe error messages: this is the default value;}

      {li [Copy], then lexemes of tokens and markup will be printed to
          standard output, with the expectation of a perfect match
          with the input file;}

      {li [Units], then the tokens and markup will be printed to
          standard output, that is, the abstract representation of the
          concrete lexical syntax;}

      {li [Tokens], then the tokens only will be printed.}
    }
 *)
type command = Quiet | Copy | Units | Tokens

(** The type [options] gathers the command-line options.
    {ul

      {li If the field [input] is [Some src], the name of the LIGO
          source file is [src]. If [input] is [Some "-"] or [None],
          the source file is read from standard input.}

      {li The field [libs] is the paths where to find LIGO files
          for inclusion (#include).}

      {li The field [verbose] is a set of stages of the compiler
          chain, about which more information may be displayed.}

      {li If the field [offsets] is [true], then the user requested
          that messages about source positions and regions be
          expressed in terms of horizontal offsets.}

      {li If the value [mode] is [`Byte], then the unit in which
          source positions and regions are expressed in messages is
          the byte. If [`Point], the unit is unicode points.}

      {li If the field [mono] is [true], then the monolithic API of
          Menhir is called, otherwise the incremental API is.}

      {li If the field [expr] is [true], then the parser for
          expressions is used, otherwise a full-fledged contract is
          expected.}
} *)
type options = <
  input   : string option;
  libs    : string list;
  verbose : Utils.String.Set.t;
  offsets : bool;
  mode    : [`Byte | `Point];
  cmd     : command;
  mono    : bool;
  expr    : bool
>

val make :
  input:string option ->
  libs:string list ->
  verbose:Utils.String.Set.t ->
  offsets:bool ->
  mode:[`Byte | `Point] ->
  cmd:command ->
  mono:bool ->
  expr:bool ->
  options

(** Parsing the command-line options on stdin.  The first parameter is
   the name of the concrete syntax, e.g., "pascaligo", and the second
   is the file extension, e.g., ".ligo".
 *)
val read : string -> string -> options
