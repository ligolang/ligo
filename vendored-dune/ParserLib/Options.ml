(* CLI options of ParserLib only *)

module type S =
  sig
    include LexerLib.Options.S

    val mono           : bool
    val pretty         : bool
    val cst            : bool
    val recovery       : bool
    val used_tokens    : bool
    val trace_recovery : string option option
  end

(* Default values for plugging the parser into the compiler *)

module MakeDefault (Options : LexerLib.Options.S) =
  struct
    include Options

    let mono = false
    let pretty = false
    let cst = false
    let recovery = true
    let trace_recovery = None
    let used_tokens = false
  end
