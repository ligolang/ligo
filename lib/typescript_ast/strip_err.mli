(* This module defines all the possible errors for stripping the AST down *)

(* Vendor dependencies *)

module Region = Simple_utils.Region

(* LIGO dependencies *)

module Wrap = Lexing_shared.Wrap

val make : ?hint:string -> _ Wrap.t -> string -> (_, string) Result.t

val error : ?hint:string -> _ Wrap.t -> string -> (_, string) Result.t

val of_region : ?hint:string -> Region.t -> string ->  (_, string) Result.t
