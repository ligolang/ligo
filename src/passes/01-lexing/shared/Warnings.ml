(* Lexing warnings for the compiler *)

let stage = "lexing"

(* Vendor dependencies *)

module Region   = Simple_utils.Region
module Location = Simple_utils.Location
module Snippet  = Simple_utils.Snippet
module Display  = Simple_utils.Display

(* Warnings *)

type t = [
]
