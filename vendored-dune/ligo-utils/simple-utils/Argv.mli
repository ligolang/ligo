(* Patching [Sys.argv] *)

(* The effect of the call [filter ~opt_with_arg ~opt_wo_arg] is a side
   effect on the contents of the array [Sys.argv] so only options with
   arguments in [opt_with_arg] and options without [opt_wo_arg]
   remain. Consequently, the client is expected to make a copy of
   [Sys.argv] before calling [filter] and restore the original version
   if they want to parse the command line several times.

   Indeed, this simple function enables different clients of this
   module to parse their own options in [Sys.argv]. In other words,
   what looks like the client's options have to be retained and
   because unknown options will be silently ignored, the client should
   include an option, e.g. "--cli", to debug the parsing of the
   command line by showing the options actually recognised here.

   WARNING: We assume that there are no concatenated short
   options. Anonymous arguments (that is, arguments without an option,
   MUST be given after "--". The reason for the later constraint is
   that if the anonymous argument follows immediately an unknown
   option, there is no way to know whether the argument is that of the
   option or not. *)

open Core

val filter : opt_with_arg:String.Set.t -> opt_wo_arg:String.Set.t -> unit
