(* Gathering CLI options *)

(* Signature *)

module type S =
  sig
    (* We cumulate the preprocessor options *)

    include Preprocessor.Options.S

    (* If the value [mode] is [`Byte], then the unit in which source
       positions and regions are expressed in messages is the byte. If
       [`Point], the unit is unicode points (UFT-8).

       The value [command] denotes some possible behaviours of the
       compiler. The constructors are

         * [`Copy]: the lexemes of tokens and markup will be printed
           to standard output, with the expectation of a perfect match
           with the input file;

         * [`Units]: the tokens and markup will be printed to standard
           output, that is, the abstract representation of the
           concrete lexical syntax;

         * [`Tokens]: the tokens only will be printed. *)

    val postprocess  : int option         (* --post=0/1/2/etc.       *)
    val preprocess   : bool               (* --pre                   *)
    val string       : string option      (* --string                *)
    val print_passes : bool               (* --print-passes          *)
    val jsligo       : string option option (* --jsligo              *)
    val mode         : [`Byte | `Point]   (* --bytes                 *)
    val command      : [`Copy | `Units | `Tokens] option
                                         (* --copy/--units/--tokens *)
  end

(* Default values for plugging the lexer into the compiler *)

module MakeDefault (Options : Preprocessor.Options.S) : S
