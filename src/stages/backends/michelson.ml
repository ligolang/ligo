module Ligo_string = Simple_utils.Ligo_string

(* available languages (used in code insertion, types..) *)
let name = "Michelson"

(* some constant string used in code generation for a given backend *)
let fw_partial_match = Ligo_string.verbatim "PARTIAL MATCH"
