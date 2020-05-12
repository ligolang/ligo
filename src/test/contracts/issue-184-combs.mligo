(* Test case from https://gitlab.com/ligolang/ligo/-/issues/184 *)

type foo = {
  bar : string;
  baz : nat;
}

type foo_michelson = foo michelson_pair_right_comb

type union1 =
| Choice1 of foo
| Choice2 of foo

type union1_aux =
| Option1 of foo_michelson
| Option2 of foo_michelson

type union1_michelson = union1_aux michelson_or_right_comb

let union1_from_michelson (m : union1_michelson) : union1 =
 let aux : union1_aux = Layout.convert_from_right_comb m in
 match aux with
 | Option1 fm ->
  let f : foo = Layout.convert_from_right_comb fm in
  Choice1 f
| Option2 fm ->
  let f : foo = Layout.convert_from_right_comb fm in
  Choice2 f

let main2 (ums, us : (union1_michelson list) * (union1 list)) =
  let new_us = List.map union1_from_michelson ums in
  ([] : operation list), new_us


