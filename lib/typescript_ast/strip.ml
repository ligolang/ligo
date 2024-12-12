(* Decoding the tree-sitter CST for TypeScript and stripping it *)

(* Disabling warnings *)

[@@@warning "-30"] (* multiply-defined record labels *)

(* Vendor dependencies *)

module Utils = Simple_utils.Utils
module Region = Simple_utils.Region

(* Local dependencies *)

module Wrap = Lexing_shared.Wrap
module Attr = Lexing_shared.Attr

(* Utilities *)

type 'a reg = 'a Region.reg
type 'a wrap = 'a Wrap.wrap
