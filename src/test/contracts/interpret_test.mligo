let lambda_call =
  let a = 3 in
  let foo = fun (i : int) -> i * i
  in foo (a + 1)

let higher_order1 =
  let a = 2 in
  let foo = fun (i : int) (j : int) (k : int) -> a + i + j + 0 in
  let bar = (foo 1 2)
  in bar 3

let higher_order2 =
  let a = 2 in
  let foo = fun (i : int) ->
              let b = 2 in
              let bar = fun (i : int) -> i + a + b
              in bar i
  in foo 1

let higher_order3 =
  let foo = fun (i : int) -> i + 1 in
  let bar = fun (f : int -> int) (i : int) -> f i + 1 in
  let baz : int -> int = bar foo
  in baz 3

let higher_order4 =
  let a = 3 in
  let foo = fun (i : int) -> a + i in
  let bar : int -> int = fun (i : int) -> foo i
  in bar 2

let concats = 0x70 ^ 0x70

type foo_record = {
  a : string;
  b : string
}

let record_concat =
  let ab : foo_record = {a="a"; b="b"}
  in ab.a ^ ab.b

let record_patch =
  let ab : foo_record = {a="a"; b="b"}
  in {ab with b = "c"}

type bar_record = {
  f   : int -> int;
  arg : int
}

let record_lambda =
  let a = 1 in
  let foo : int -> int = fun (i : int) -> a + i*2 in
  let farg : bar_record = {f = foo; arg = 2}
  in farg.f farg.arg

type foo_variant =
| Foo
| Bar of int
| Baz of string

let variant_exp = Foo, Bar 1, Baz "b"

let variant_match =
  let a = Bar 1 in
  match a with
  | Foo   -> 1
  | Bar i -> 2
  | Baz s -> 3

(* UNSUPPORTED: No deep patterns yet.
type bar_variant =
| Baz
| Buz of int * int
| Biz of int * int * string

let long_variant_match =
  let a = Biz (1,2,"Biz") in
  match a with
  | Baz -> "Baz"
  | Buz (a,b) -> "Buz"
  | Biz (a,b,c) -> c
 *)

let bool_match =
  let b = true in
  match b with
  | true -> 1
  | false -> 2

let list_match =
  let a = [1; 2; 3; 4] in
  match a with
  | hd::tl -> hd::a
  | [] -> a

let tuple_proj =
  let a, b = true, false
  in a or b

let list_const =
  let a = [1; 2; 3; 4]
  in 0::a

type foobar = int option

let options_match_some =
  let a = Some 0 in
  match a with
  | Some i -> i
  | None -> 1

let options_match_none =
  let a : foobar = None in
  match a with
  | Some i -> i
  | None -> 0

let is_nat_nat =
  let i : int = 1 in
  let j : int = -1
  in is_nat i, is_nat j

let abs_int = abs (-5)

let nat_int = int 5n

let map_list =
  let a = [1; 2; 3; 4] in
  let add_one : (int -> int) = fun (i : int) -> i + 1
  in List.map add_one a

let fail_alone = failwith "you failed"

let iter_list_fail =
  let a = [1; 2; 3; 4] in
  let check_something : int -> unit =
    fun (i : int) ->
      if i = 2 then failwith "you failed"
  in List.iter check_something a

let fold_list =
  let a = [1; 2; 3; 4] in
  let acc : int * int -> int =
    fun (prev, el : int * int) -> prev + el
  in List.fold acc a 0

let comparison_int = 1 > 2, 2 > 1, 1 >=2, 2 >= 1

let comparison_string = "foo"="bar", "baz"="baz"

let divs : int * nat * tez * nat = 1/2, 1n/2n, 1tez/2n, 1tez/2tez

let var_neg =
  let a = 2 in -a

let sizes =
  let a = [1; 2; 3; 4; 5] in
  let b = "12345" in
  let c = Set.literal [1; 2; 3; 4; 5] in
  let d = Map.literal [(1,1); (2,2); (3,3)] in
  let e = 0xFFFF in
  List.length a,
  String.length b,
  Set.cardinal c,
  Map.size d,
  Bytes.length e

let modi = 3 mod 2

let fold_while =
  let aux : int -> bool * int =
    fun (i : int) ->
    if i < 10 then Loop.resume (i + 1) else Loop.stop i
  in (Loop.fold_while aux 20, Loop.fold_while aux 0)

let assertion_pass = assert (1=1)

let assertion_fail = assert (1=2)

let lit_address = ("KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" : address)

let map_finds =
  let m = Map.literal [("one", 1); ("two", 2); ("three", 3)]
  in Map.find_opt "two" m

let map_finds_fail =
  let m = Map.literal [("one", 1); ("two", 2); ("three", 3)]
  in Map.find "four" m

let map_empty =
  ((Map.empty : (int, int) map), (Map.literal [] : (int, int) map))

let m = Map.literal [("one", 1); ("two", 2); ("three", 3)]

let map_fold =
  let aux = fun (i : int * (string * int)) -> i.0 + i.1.1
  in Map.fold aux m (-2)

let map_iter =
  let aux =
    fun (i : string * int) -> if i.1 = 12 then failwith "never"
  in Map.iter aux m

let map_map =
  let aux = fun (i : string * int) -> i.1 + String.length i.0
  in Map.map aux m

let map_mem = Map.mem "one" m, Map.mem "four" m

let map_remove = Map.remove "one" m, Map.remove "four" m

let map_update =
  Map.update "one" (Some 1) (Map.literal ["one", 2]),
  Map.update "one" (None : int option) (Map.literal ["one", 1]),
  Map.update "one" (None : int option) (Map.empty : (string, int) map),
  Map.update "one" (Some 1) (Map.literal [] : (string, int) map)

let s = Set.literal [1; 2; 3]

let set_add =
  Set.add 1 s,
  Set.add 4 s,
  Set.add 1 (Set.empty : int set)

let set_iter_fail =
  let aux = fun (i : int) -> if i = 1 then failwith "set_iter_fail"
  in Set.iter aux (Set.literal [1; 2; 3])

let set_mem =
  Set.mem 1 s,
  Set.mem 4 s,
  Set.mem 1 (Set.empty : int set)
