[@@@warning "-42"]

module List = Core.List

module CST = Cst_pyligo.CST
(*open CST*)
module Region = Simple_utils.Region
open! Region
open! PPrint
module Option = Simple_utils.Option
module Token  = Lexing_pyligo.Token

type state = unit

let default_state : state = ()

(*
let pp_par printer {value; _} =
  string "(" ^^ nest 1 (printer value.inside ^^ string ")")
                   *)
(* The CST *)

type cst       = CST.t
type expr      = CST.expr
type type_expr = CST.type_expr
type pattern   = CST.pattern

let print _ _ = empty
let print_expr _ _ = empty
let print_type_expr _ _ = empty
let print_pattern _ _ = empty
