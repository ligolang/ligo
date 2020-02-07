open Cli_expect

let contract basename =
  "../../test/contracts/" ^ basename

let%expect_test _ =
  run_ligo_good [ "ligo-interpret" ; contract "interpret_test.mligo" ] ;
  [%expect {|
    val lambda_call = 16 : int
    val higher_order1 = 5 : int
    val higher_order2 = 5 : int
    val concats = 0x7070 : bytes
    val record_concat = "ab" : string
    val record_patch = {  ; a = ("a" : string) ; b = ("c" : string) }
    val record_lambda = 5 : int
    val variant_exp = {  ; 0 = (Foo(unit)) ; 1 = (Bar(1 : int)) ; 2 = (Baz("b" : string)) }
    val variant_match = 2 : int
    val bool_match = 1 : int
    val list_match = [ ; 1 : int ; 1 : int ; 2 : int ; 3 : int ; 4 : int]
    val tuple_proj = true
    val list_const = [ ; 0 : int ; 1 : int ; 2 : int ; 3 : int ; 4 : int]
    val options_match_some = 0 : int
    val options_match_none = 0 : int
    val is_nat_nat = {  ; 0 = (Some(1 : nat)) ; 1 = (None(unit)) }
    val abs_int = 5 : int
    val nat_int = 5 : int
    val map_list = [ ; 2 : int ; 3 : int ; 4 : int ; 5 : int]
    val fail_alone = "you failed" : failure
    val iter_list_fail = "you failed" : failure
    val fold_list = 10 : int
    val comparison_int = {  ; 0 = (false) ; 1 = (true) ; 2 = (false) ; 3 = (true) }
    val comparison_string = {  ; 0 = (false) ; 1 = (true) }
    val divs = {  ; 0 = (0 : int) ; 1 = (0 : nat) ; 2 = (500000 : mutez) ; 3 = (0 : nat) }
    val var_neg = -2 : int
    val sizes = {  ; 0 = (5 : nat) ; 1 = (5 : nat) ; 2 = (5 : nat) ; 3 = (3 : nat) ; 4 = (2 : nat) }
    val modi = 1 : nat
    val fold_while = {  ; 0 = (20 : int) ; 1 = (10 : int) }
    val assertion_pass = unit
    val assertion_fail = "failed assertion" : failure
    val lit_address = "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" : address
    val map_finds = Some(2 : int)
    val map_finds_fail = "failed map find" : failure
    val map_empty = {  ; 0 = ([]) ; 1 = ([]) }
    val m = [ ; "one" : string -> 1 : int ; "two" : string -> 2 : int ; "three" : string -> 3 : int]
    val map_fold = 4 : int
    val map_iter = unit
    val map_map = [ ; "one" : string -> 4 : int ; "two" : string -> 5 : int ; "three" : string -> 8 : int]
    val map_mem = {  ; 0 = (true) ; 1 = (false) }
    val map_remove = {  ; 0 = ([ ; "two" : string -> 2 : int ; "three" : string -> 3 : int]) ; 1 = ([ ; "one" : string -> 1 : int ; "two" : string -> 2 : int ; "three" : string -> 3 : int]) }
    val map_update = {  ; 0 = ([ ; "one" : string -> 1 : int]) ; 1 = ([]) ; 2 = ([]) ; 3 = ([ ; "one" : string -> 1 : int]) }
    val s = { ; 1 : int ; 2 : int ; 3 : int}
    val set_add = {  ; 0 = ({ ; 1 : int ; 2 : int ; 3 : int}) ; 1 = ({ ; 1 : int ; 2 : int ; 3 : int ; 4 : int}) ; 2 = ({ ; 1 : int}) }
    val set_iter_fail = "set_iter_fail" : failure
    val set_mem = {  ; 0 = (true) ; 1 = (false) ; 2 = (false) } |}] ;