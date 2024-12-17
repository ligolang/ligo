(* This module defines all the possible errors for stripping the AST down *)

(* Vendor dependencies *)

module Region = Simple_utils.Region

(* LIGO dependencies *)

module Wrap = Lexing_shared.Wrap

let of_region ?(hint : string option) (region : Region.t) (msg : string) =
  let hint =
    match hint with
    | None | Some "" -> ""
    | Some msg -> "\nHint: " ^ msg
  in
  Error (Printf.sprintf "%s:\n%s%s" (region#to_string `Byte) msg hint)

let error ?hint wrap msg = of_region ?hint wrap#region msg
let make = error
