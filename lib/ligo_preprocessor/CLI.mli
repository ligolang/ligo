(* Command-Line Interface (CLI) *)

(* Configuration, options and the parsing status of the latter *)

module type PARAMETERS =
  sig
    module Config  : Config.S
    module Options : Options.S
    module Status  : module type of Status
  end

(* The instantiation of functor [Make] reads the command line
   interface. *)

module Make (Config : Config.S) : PARAMETERS

(* Default parameters (without actually reading the CLI). Used to
   interface the compiler. *)

module MakeDefault (Config : Config.S) : PARAMETERS
