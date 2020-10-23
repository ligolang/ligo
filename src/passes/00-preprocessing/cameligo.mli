(* This file provides an interface to the ReasonLIGO parser. *)

(* Vendor dependencies *)

module Trace = Simple_utils.Trace

(* Some parameters' types *)

type file_path = string
type dirs      = file_path list (* For #include directives *)

(* Results and errors *)

type error  = Errors.preproc_error
type c_unit = (Buffer.t * (string * string) list, error) Trace.result

(* Preprocessing *)

val preprocess        : dirs -> file_path -> c_unit (* from a file *)
val preprocess_string : dirs -> string -> c_unit (* from a string *)
