let lambda_call =
  let a = 3 in
  let foo = fun (i : int) -> i * i in
  (foo (a + 1)) = 16

let higher_order1 =
  let a = 2 in
  let foo = fun (i : int) (j : int) (k : int) -> a + i + j + 0 in
  let bar = (foo 1 2) in
  (bar 3 = 5)

let higher_order2 =
  let a = 2 in
  let foo = fun (i : int) ->
              let b = 2 in
              let bar = fun (i : int) -> i + a + b
              in bar i
  in
  (foo 1 = 5)

let higher_order3 =
  let foo = fun (i : int) -> i + 1 in
  let bar = fun (f : int -> int) (i : int) -> f i + 1 in
  let baz : int -> int = bar foo
  in
  (baz 3 = 5)

let higher_order4 =
  let a = 3 in
  let foo = fun (i : int) -> a + i in
  let bar : int -> int = fun (i : int) -> foo i
  in
  (bar 2 = 5)

let concats = (0x70 ^ 0x70  = 0x7070)

type foo_record = {
  a : string;
  b : string
}

let record_concat =
  let ab : foo_record = {a="a"; b="b"}
  in (ab.a ^ ab.b = "ab")

let record_patch =
  let ab : foo_record = {a="a"; b="b"} in
  let res = {ab with b = "c"} in 
  (res.b = "c")

type bar_record = {
  f   : int -> int;
  arg : int
}

let record_lambda =
  let a = 1 in
  let foo : int -> int = fun (i : int) -> a + i*2 in
  let farg : bar_record = {f = foo; arg = 2}
  in (farg.f farg.arg = 5)

type foo_variant =
| Foo
| Bar of int
| Baz of string

let variant_match =
  let a = Bar 1 in
  match a with
  | Foo   -> false
  | Bar i -> true
  | Baz s -> false

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
  | true -> true
  | false -> false

let list_match =
  let a = [1; 2; 3; 4] in
  match a with
  | hd::tl -> true
  | [] -> false

let tuple_proj =
  let a, b = true, false
  in a or b

let list_const =
  let a = [1; 2; 3; 4] in
  (List.length (0::a)) = 5n

type foobar = int option

let options_match_some =
  let a = Some 0 in
  match a with
  | Some i -> true
  | None -> false

let options_match_none =
  let a : foobar = None in
  match a with
  | Some i -> false
  | None -> true

let is_nat_yes =
  let i : int = 1 in
  match (is_nat i) with
  | Some i -> true
  | None -> false
  
let is_nat_no =
  let j : int = -1 in
  match (is_nat j) with
  | Some i -> false
  | None -> true

let abs_int =
  let a : nat = abs (-5) in
  a = 5n

let nat_int = ((int 5n) = 5)

let map_list =
  let a = [1; 1; 1; 1] in
  let add_one : (int -> int) = fun (i : int) -> i + 1 in
  match (List.map add_one a) with
  | hd::tl -> (hd = 2)
  | [] -> false

let iter_list_fail =
  let a = [1; 2; 3; 4] in
  let check_something : int -> unit =
    fun (i : int) ->
      if i = 2 then failwith "you failed"
  in
  Test.assert_failure (fun (u:unit) -> List.iter check_something a)

let fold_list =
  let a = [1; 2; 3; 4] in
  let acc : int * int -> int =
    fun (prev, el : int * int) -> prev + el
  in
  (List.fold acc a 0) = 10

let comparison_int =
  let a = 1 > 2 in
  let b = 2 > 1 in
  let c = 1 >= 2 in
  let d = 2 >= 1 in
  ( not(a) && b && (not c) && d )

let comparison_string = not("foo"="bar") && ("baz"="baz")

let divs_int =
  let a = 1/2 in
  (* let b = 1/2n in *)
  (* let c = 1n/2 in *)
  (a = 0)

let divs_nat =
  let a = 1n/2n in
  let b = 1tz/2tz in
  (a = 0n) && (a = b)

let divs_tez =
  let a = 1tz/2n in
  (a = 0.5tz)

let var_neg =
  let a = 2 in
  (-a = -2)

let sizes =
  let a = [1; 2; 3; 4; 5] in
  let b = "12345" in
  let c = Set.literal [1; 2; 3; 4; 5] in
  let d = Map.literal [(1,1); (2,2); (3,3) ; (4,4) ; (5,5)] in
  let e = 0xFFFF in
  (List.length a = 5n) &&
  (String.length b = 5n) &&
  (Set.cardinal c = 5n) &&
  (Map.size d = 5n) &&
  (Bytes.length e = 2n)

let modi = (3 mod 2 = 1n)

let fold_while =
  let aux : int -> bool * int =
    fun (i : int) ->
    if i < 10 then Loop.resume (i + 1) else Loop.stop i
  in
  (Loop.fold_while aux 20 = 20) &&  (Loop.fold_while aux 0 = 10)

let assertion_pass =
  let unitt = assert (1=1) in
  true

let assertion_fail = Test.assert_failure (fun (u:unit) -> assert (1=2))

let map_finds =
  let m = Map.literal [("one", 1); ("two", 2); ("three", 3)]
  in
  match (Map.find_opt "two" m) with
  | Some v -> true
  | None -> false

let map_finds_fail =
  let m = Map.literal [("one", 1); ("two", 2); ("three", 3)]
  in Test.assert_failure (fun (u:unit) -> Map.find "four" m)

let m = Map.literal [("one", 1); ("two", 2); ("three", 3)]

let map_fold =
  let aux = fun (i : int * (string * int)) -> i.0 + i.1.1
  in (Map.fold aux m 0 = 6)

let map_iter =
  let aux =
    fun (i : string * int) -> if i.1 = 12 then failwith "never"
  in
  let u = Map.iter aux m in true

let map_map =
  let aux = fun (i : string * int) -> i.1 + String.length i.0 in
  (Map.find "one" (Map.map aux m) = 4)

let map_mem = (Map.mem "one" m) && (Map.mem "two" m) && (Map.mem "three" m)

let map_remove =
  let m = Map.remove "one" m in
  let m = Map.remove "two" m in
  let m = Map.remove "three" m in
  not (Map.mem "one" m) &&  not (Map.mem "two" m) && not (Map.mem "three" m)

let map_update =
  let m1 = Map.update "four" (Some 4) m in
  let m2 = Map.update "one" (None : int option) m in
  (Map.mem "four" m1) && not (Map.mem "one" m2)

let s = Set.literal [1; 2; 3]

let set_add =
  let s = Set.add 1 (Set.empty : int set) in
  Set.mem 1 s

let set_iter_fail =
  let aux = fun (i : int) -> if i = 1 then failwith "set_iter_fail"
  in Test.assert_failure (fun (u:unit) -> Set.iter aux s)

let set_mem =
  (Set.mem 1 s) && (Set.mem 2 s) && (Set.mem 3 s)

let recursion_let_rec_in =
  let rec sum : int*int -> int = fun ((n,res):int*int) ->
    if (n<1) then res else sum (n-1,res+n)
  in
  sum (10,0) = 55

let rec sum_rec ((n,acc):int * int) : int =
    if (n < 1) then acc else sum_rec (n-1, acc+n)

let top_level_recursion = (sum_rec (10,0) = 55)

let pack_unpack =
  let packed1 : bytes = Bytes.pack 1 in
  let packed2 : bytes = Bytes.pack "hey" in
  let v1 : int option = Bytes.unpack packed1 in
  let v2 : string option = Bytes.unpack packed2 in
  let v1 = match v1 with
  | Some s -> s
  | None -> -1
  in
  let v2 = match v2 with
  | Some s -> s
  | None -> ""
  in
  (v1 = 1) && (v2 = "hey")