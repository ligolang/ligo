open Stage_common.Types

module From_mini_c : sig
  type type_content = Mini_c.Types.type_content
  type type_expression = Mini_c.Types.type_expression
  type type_base = Mini_c.Types.type_base
  type value = Mini_c.Types.value
end = struct
  include Mini_c.Types
end
include From_mini_c

type environment = type_expression list

open Util
type ('a, 'b) split = splitting * 'a * 'b

type 'a bind = type_expression * usage * 'a

(* generalization to arbitrary number of bound variables -- useful for
   conditionals which are all essentially same, except for the number
   of bound variables in the branches, ranging from 0 to 2 *)
type 'a binds =
  | Bind_zero of 'a
  | Bind_suc of 'a binds bind

let binds0 (x : 'a) = Bind_zero x
let binds1 (ty, used, x) = Bind_suc (ty, used, binds0 x)
let binds2 (ty, used, x) = Bind_suc (ty, used, binds1 x)

let rec num_binds : 'a binds -> int = function
  | Bind_zero _ -> 0
  | Bind_suc (_, _, b) -> 1 + num_binds b

type expression_content =
  | E_literal of literal
  | E_closure of expression bind
  | E_constant of constant
  | E_application of (expression, expression) split
  | E_variable
  | E_iterator of constant' * (expression bind, expression) split
  (* init, (coll, body) *)
  | E_fold of (expression, (expression, expression bind) split) split
  | E_if_bool of (expression, (expression, expression) split) split
  | E_if_none of (expression, (expression, expression bind) split) split
  | E_if_cons of (expression, (expression bind bind, expression) split) split
  | E_if_left of (expression, (expression bind, expression bind) split) split
  | E_let_in of bool * (expression, expression bind) split
  | E_record_update of [`Left | `Right] list * (expression, expression) split
  | E_raw_michelson of string

and expression = {
  content : expression_content ;
  type_expression : type_expression ;
  location : Simple_utils.Location.t;
}

(* list-like n-ary split for constant args *)
and 'a args =
  | Arg_nil
  | Arg_cons of ('a, 'a args) split

and constant = {
  cons_name : constant' ;
  arguments : expression args ;
}

let rec args_count : 'a args -> int = function
  | Arg_nil -> 0
  | Arg_cons (_, _, args) -> 1 + args_count args
