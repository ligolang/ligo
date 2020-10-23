(* This file provides an interface to the CameLIGO preprocessor. *)

(* Vendor dependencies *)

module Trace = Simple_utils.Trace

(* Internal dependencies on CameLIGO *)

module File      = Lexer_pascaligo.File
module Comments  = Lexer_pascaligo.Comments

(* Preprocessing *)

module MkPreproc = Common.MakePreproc

(* Some parameters' types *)

type file_path = string
type dirs      = file_path list (* For #include directives *)

(* Results and errors *)

type error  = Errors.preproc_error
type c_unit = (Buffer.t * (string * string) list, error) Trace.result

let fail msg = Trace.fail @@ Errors.generic msg

let preprocess dirs file_path =
  let module Preproc = MkPreproc (File) (Comments)
  in match Preproc.preprocess dirs file_path with
       Stdlib.Error msg -> fail msg
     | Ok (buffer,deps) -> Trace.ok (buffer,deps)

let preprocess_string dirs string =
  let module Preproc = MkPreproc (File) (Comments)
  in match Preproc.preprocess_string dirs string with
       Stdlib.Error msg -> fail msg
     | Ok (buffer,deps) -> Trace.ok (buffer,deps)
