open Types
open Combinators

let make_a_e_empty expression type_annotation = make_a_e expression type_annotation Environment.full_empty

let e_a_empty_unit = e_a_unit Environment.full_empty
let e_a_empty_int n = e_a_int n Environment.full_empty
let e_a_empty_nat n = e_a_nat n Environment.full_empty
let e_a_empty_tez n = e_a_tez n Environment.full_empty
let e_a_empty_bool b = e_a_bool b Environment.full_empty
let e_a_empty_string s = e_a_string s Environment.full_empty
let e_a_empty_address s = e_a_address s Environment.full_empty
let e_a_empty_pair a b = e_a_pair a b Environment.full_empty
let e_a_empty_some s = e_a_some s Environment.full_empty
let e_a_empty_none t = e_a_none t Environment.full_empty
let e_a_empty_tuple lst = e_a_tuple lst Environment.full_empty
let e_a_empty_record r = e_a_record r Environment.full_empty
let e_a_empty_map lst k v = e_a_map lst k v Environment.full_empty
let e_a_empty_list lst t = e_a_list lst t Environment.full_empty
let ez_e_a_empty_record r = ez_e_a_record r Environment.full_empty
let e_a_empty_lambda l i o = e_a_lambda l i o Environment.full_empty

open Environment

let env_sum_type ?(env = full_empty)
    ?(name = "a_sum_type")
    (lst : (string * type_value) list) =
  add_type name (make_t_ez_sum lst) env
