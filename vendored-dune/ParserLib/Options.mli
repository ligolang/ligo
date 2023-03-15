(* CLI options of ParserLib only *)

module type S =
  sig
    (* We cumulate the lexer options: *)

    include LexerLib.Options.S

    val mono        : bool          (* --mono        *)
    val pretty      : bool          (* --pretty      *)
    val cst         : bool          (* --cst         *)
    val recovery    : bool          (* --recovery    *)
    val used_tokens : bool          (* --used-tokens *)

    (* File path where tracing will be printed:
         * [None] means no option;
         * [Some None] means [stdout];
         * [Some (Some file)] means [file] as output. *)

    val trace_recovery : string option option
  end

(* Default values for plugging the parser into the compiler *)

module MakeDefault (Options : LexerLib.Options.S) : S
