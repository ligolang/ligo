let test_lambda_call =
  let a = 3 in
  let foo = fun (i : int) -> i * i in
  assert ((foo (a + 1)) = 16)

let test_higher_order1 =
  let a = 2 in
  let foo = fun (i : int) (j : int) (_k : int) -> a + i + j + 0 in
  let bar = (foo 1 2) in
  assert (bar 3 = 5)

let test_higher_order2 =
  let a = 2 in
  let foo = fun (i : int) ->
              let b = 2 in
              let bar = fun (i : int) -> i + a + b
              in bar i
  in
  assert (foo 1 = 5)

let test_higher_order3 =
  let foo = fun (i : int) -> i + 1 in
  let bar = fun (f : int -> int) (i : int) -> f i + 1 in
  let baz : int -> int = bar foo
  in
  assert (baz 3 = 5)

let test_higher_order4 =
  let a = 3 in
  let foo = fun (i : int) -> a + i in
  let bar : int -> int = fun (i : int) -> foo i
  in
  assert (bar 2 = 5)

let test_concats = assert (0x70 ^ 0x70  = 0x7070)

type foo_record = {
  a : string;
  b : string
}

let test_record_concat =
  let ab : foo_record = {a="a"; b="b"}
  in assert (ab.a ^ ab.b = "ab")

let test_record_patch =
  let ab : foo_record = {a="a"; b="b"} in
  let res = {ab with b = "c"} in
  assert (res.b = "c")

type bar_record = {
  f   : int -> int;
  arg : int
}

let test_record_lambda =
  let a = 1 in
  let foo : int -> int = fun (i : int) -> a + i*2 in
  let farg : bar_record = {f = foo; arg = 2}
  in assert (farg.f farg.arg = 5)

type foo_variant =
| Foo
| Bar of int
| Baz of string

let test_variant_match =
  let a = Bar 1 in
  assert (match a with
  | Foo   -> false
  | Bar _ -> true
  | Baz _ -> false)

let test_bool_match =
  let b = true in
  assert (match b with
  | True -> true
  | False -> false)

let test_list_match =
  let a = [1; 2; 3; 4] in
  assert (match a with
  | _::_ -> true
  | [] -> false)

let test_tuple_proj =
  let a, b = true, false
  in assert (a or b)

let test_list_const =
  let a = [1; 2; 3; 4] in
  assert ((List.length (0::a)) = 5n)

type foobar = int option

let test_options_match_some =
  let a = Some 0 in
  assert (match a with
  | Some _ -> true
  | None -> false)

let test_options_match_none =
  let a : foobar = None in
  assert (match a with
  | Some _ -> false
  | None -> true)

let test_is_nat_yes =
  let i : int = 1 in
  assert (match (is_nat i) with
  | Some _ -> true
  | None -> false)

let test_is_nat_no =
  let j : int = -1 in
  assert (match (is_nat j) with
  | Some _ -> false
  | None -> true)

let test_abs_int =
  let a : nat = abs (-5) in
  assert (a = 5n)

let test_nat_int = assert ((int 5n) = 5)

let test_map_list =
  let a = [1; 1; 1; 1] in
  let add_one : (int -> int) = fun (i : int) -> i + 1 in
  assert (match (List.map add_one a) with
  | hd::_tl -> (hd = 2)
  | [] -> false)

let test_fold_list =
  let a = [1; 2; 3; 4] in
  let acc : int * int -> int =
    fun (prev, el : int * int) -> prev + el
  in
  assert ((List.fold acc a 0) = 10)

let test_comparison_int =
  let a = 1 > 2 in
  let b = 2 > 1 in
  let c = 1 >= 2 in
  let d = 2 >= 1 in
  assert ( not(a) && b && (not c) && d )

let test_comparison_string = assert (not("foo"="bar") && ("baz"="baz"))

let test_divs_int =
  let a = 1/2 in
  assert (a = 0)

let test_divs_nat =
  let a = 1n/2n in
  assert (a = 0n)

let test_var_neg =
  let a = 2 in
  assert (-a = -2)

let test_sizes =
  let a = [1; 2; 3; 4; 5] in
  let b = "12345" in
  let c = Set.literal [1; 2; 3; 4; 5] in
  let d = Map.literal [(1,1); (2,2); (3,3) ; (4,4) ; (5,5)] in
  let e = 0xFFFF in
  assert ((List.length a = 5n) &&
          (String.length b = 5n) &&
          (Set.cardinal c = 5n) &&
          (Map.size d = 5n) &&
          (Bytes.length e = 2n))

let test_modi = assert (3 mod 2 = 1n)

let test_assertion_pass =
  let () = assert (1=1) in
  assert true

let test_map_finds =
  let m = Map.literal [("one", 1); ("two", 2); ("three", 3)]
  in
  assert (match (Map.find_opt "two" m) with
  | Some _ -> true
  | None -> false)

let m = Map.literal [("one", 1); ("two", 2); ("three", 3)]

let test_map_fold =
  let aux = fun (i : int * (string * int)) -> i.0 + i.1.1
  in assert (Map.fold aux m 0 = 6)

let test_map_map =
  let aux = fun (i : string * int) -> i.1 + String.length i.0 in
  assert (Map.find "one" (Map.map aux m) = 4)

let test_map_mem = assert ((Map.mem "one" m) && (Map.mem "two" m) && (Map.mem "three" m))

let test_map_remove =
  let m = Map.remove "one" m in
  let m = Map.remove "two" m in
  let m = Map.remove "three" m in
  assert (not (Map.mem "one" m) &&  not (Map.mem "two" m) && not (Map.mem "three" m))

let test_map_update =
  let m1 = Map.update "four" (Some 4) m in
  let m2 = Map.update "one" (None : int option) m in
  assert ((Map.mem "four" m1) && not (Map.mem "one" m2))

let s = Set.literal [1; 2; 3]

let test_set_add =
  let s = Set.add 1 (Set.empty : int set) in
  assert (Set.mem 1 s)

let test_set_mem =
  assert ((Set.mem 1 s) && (Set.mem 2 s) && (Set.mem 3 s))

let test_set_remove =
  let s = Set.literal [0n; 1n; 2n] in
  let () = assert ((Set.mem 1n s) && (Set.mem 2n s) && (Set.mem 0n s)) in
  let s = Set.remove 0n s in
  assert ((Set.mem 1n s) && (Set.mem 2n s) && not (Set.mem 0n s))


let test_recursion_let_rec_in =
  let rec sum : int*int -> int = fun ((n,res):int*int) ->
    if (n<1) then res else sum (n-1,res+n)
  in
  assert (sum (10,0) = 55)

let rec sum_rec ((n,acc):int * int) : int =
    if (n < 1) then acc else sum_rec (n-1, acc+n)

let test_top_level_recursion = assert (sum_rec (10,0) = 55)

let test_bitwise_ops  =
    let b_and_int     = 7   land 4n in
    let b_and_nat     = 4n  land 4n in
    let b_or          = 7n  lor  4n in
    let b_xor         = 7n  lxor 4n in
    let b_shift_left  = 7n  lsl  2n in
    let b_shift_right = 14n lsr  2n in

    assert (b_and_int     = 4n  &&
            b_and_nat     = 4n  &&
            b_or          = 7n  &&
            b_xor         = 3n  &&
            b_shift_left  = 28n &&
            b_shift_right = 3n   )

let test_bitwise_module =
    let b_and_int       = Bitwise.and         7   4n in
    let b_and_nat       = Bitwise.and         4n  4n in
    let b_or            = Bitwise.or          7n  4n in
    let b_xor           = Bitwise.xor         7n  4n in
    let b_shift_left    = Bitwise.shift_left  7n  2n in
    let b_shift_right   = Bitwise.shift_right 14n 2n in

    assert (b_and_int     = 4n  &&
            b_and_nat     = 4n  &&
            b_or          = 7n  &&
            b_xor         = 3n  &&
            b_shift_left  = 28n &&
            b_shift_right = 3n   )

let concat (xs : nat list) (ys : nat list) =
  List.fold_right (fun (x,ys : (nat * nat list)) -> x :: ys) xs ys

let test_list_concat =
  let xs = [1n;2n;3n] in
  let ys = [4n;5n;6n] in
  let zs = concat xs ys in
  assert (zs = [1n;2n;3n;4n;5n;6n])

let test_list_head_opt =
  assert (List.head_opt ([1n;2n;3n] : nat list) = (Some 1n : nat option) &&
          List.head_opt ([2n;3n]    : nat list) = (Some 2n : nat option) &&
          List.head_opt ([3n]       : nat list) = (Some 3n : nat option) &&
          List.head_opt ([]         : nat list) = (None    : nat option))

let test_list_tail_opt =
  assert (List.tail_opt ([1n;2n;3n] : nat list) = (Some [2n;3n]         : nat list option) &&
          List.tail_opt ([2n;3n]    : nat list) = (Some [3n]            : nat list option) &&
          List.tail_opt ([3n]       : nat list) = (Some ([] : nat list) : nat list option) &&
          List.tail_opt ([]         : nat list) = (None                 : nat list option))

let reverse (xs : nat list) =
  List.fold_left (fun (ys,x : (nat list * nat)) -> x :: ys) ([] : nat list) xs

let test_list_reverse =
  let xs = [1n;2n;3n] in
  assert (reverse xs = [3n;2n;1n])

let test_set_fold_desc =
  let xs = Set.literal [1n;2n;3n] in
  let sum = Set.fold_desc (fun (x,acc : nat * nat) -> acc + x) xs 0n in
  assert (sum = 6n)

let test_set_update =
  let xs = Set.literal [1n;2n;3n] in
  let xs = Set.update 4n true xs in
  let xs = Set.update 3n false xs in
  let xs = Set.update 5n false xs in
  assert (xs = Set.literal [1n;2n;4n])

let test_map_get_and_update =
  let xs = Map.literal [(1n,"Hello");(2n,"World")] in
  let (old,xs) = Map.get_and_update 2n (Some "Foo") xs in
  let ys = Map.literal [(1n,"Hello");(2n,"Foo")] in
  assert (xs = ys && old = Some "World")

let test_big_map_get_and_update =
  let xs = Big_map.literal [(1n,"Hello");(2n,"World")] in
  let (old,xs) = Big_map.get_and_update 2n (Some "Foo") xs in
  let ys = Big_map.literal [(1n,"Hello");(2n,"Foo")] in
  assert (xs = ys && old = Some "World")

let test_add_mutez =
  let m = 10tez in
  let n = 1tez in
  assert (m + n = 11tez)

let test_sub_mutez =
  let m = 10tez in
  let n = 1tez in
  assert (m - n = Some 9tez)

let test_div_mutez =
  let a = 1tez/2tez in
  assert (a = 0n)

let test_sub_timestamp =
  let today : timestamp = ("2001-01-01t10:10:10Z" : timestamp) in
  let some_date : timestamp = ("2000-01-01t10:10:10Z" : timestamp) in
  let diff : int = today - some_date in
  assert (diff = 31622400)

let test_list_fold_left_sum =
  let xs = [1;2;3] in
  let sum = List.fold_left (fun (x,acc : (int * int)) -> x + acc) 0 xs in
  assert (sum = 6)

let test_bytes_sub =
  let () = assert (Bytes.sub 0n 3n (Bytes.pack 5n) = (Bytes.pack 5)) in
  let () = assert (Bytes.sub 1n 2n (Bytes.pack 5n) = 0x0005) in
  let () = assert (Bytes.sub 0n 0n (Bytes.pack 5n) = Bytes.sub 2n 0n (Bytes.pack 5)) in
  let () = assert (Bytes.sub 2n 1n (Bytes.pack 5n) = 0x05) in
  assert (Bytes.sub 0n 1n (Bytes.pack 5n) = 0x05)

let test_with_error =
  assert_with_error true "foo"

let test_some =
  assert_some (Some 1 : int option)

let test_some_with_error =
  assert_some_with_error (Some 2 : int option) "bar"

let test_none =
  assert_none (None : int option)

let test_none_with_error =
  assert_none_with_error (None : int option) "bar"

let test_unopt =
  assert (Option.unopt (Some 1 : int option) = 1)

let test_unopt_with_error =
  assert (Option.unopt_with_error (Some 2 : int option) "bar" = 2)

let test_sha256 =
  let hash5n = (0xf6c5c0ad2216920e105be5e940c4a71ead0741f9dbdb32bfab9570df57cc983a : bytes) in
  let hashempty = (0xe3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855 : bytes) in
  let () = assert (Crypto.sha256 (Bytes.pack 5n) = hash5n) in
  let () = assert (Test.eval (Crypto.sha256 (Bytes.pack 5n)) = Test.run (fun (n : nat) -> Crypto.sha256 (Bytes.pack n)) 5n) in
  assert (Crypto.sha256 (Bytes.sub 0n 0n (Bytes.pack 5n)) = hashempty)

let test_sha512 =
  let hash5n =
    (0x31ca75e6613c8e8db4f59d9b69f51b0944a4f6ccb8ced6501122569d2890c519c8ce3a2ccc2a09428ad37a7af6b1903ece93ed863b65b94f072e45a8fdae4e5c : bytes) in
  let hashempty =
    (0xcf83e1357eefb8bdf1542850d66d8007d620e4050b5715dc83f4a921d36ce9ce47d0d13c5d85f2b0ff8318d2877eec2f63b931bd47417a81a538327af927da3e : bytes) in
  let () = assert (Crypto.sha512 (Bytes.pack 5n) = hash5n) in
  let () = assert (Test.eval (Crypto.sha512 (Bytes.pack 5n)) = Test.run (fun (n : nat) -> Crypto.sha512 (Bytes.pack n)) 5n) in
  assert (Crypto.sha512 (Bytes.sub 0n 0n (Bytes.pack 5n)) = hashempty)

let test_blake2b =
  let hash5n = (0x2af6f7eb61511de4fa3a667a63e2f26f9c506042b335e62fed916335d04a08ed : bytes) in
  let hashempty = (0x0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8 : bytes) in
  let () = assert (Crypto.blake2b (Bytes.pack 5n) = hash5n) in
  let () = assert (Test.eval (Crypto.blake2b (Bytes.pack 5n)) = Test.run (fun (n : nat) -> Crypto.blake2b (Bytes.pack n)) 5n) in
  assert (Crypto.blake2b (Bytes.sub 0n 0n (Bytes.pack 5n)) = hashempty)

let test_keccak =
  let hash5n = (0xb40da68da4779bf68d31e6ab2eb21c26d950cc23e13efb4da0ec424f138000c3 : bytes) in
  let hashempty = (0xc5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470 : bytes) in
  let () = assert (Crypto.keccak (Bytes.pack 5n) = hash5n) in
  let () = assert (Test.eval (Crypto.keccak (Bytes.pack 5n)) = Test.run (fun (n : nat) -> Crypto.keccak (Bytes.pack n)) 5n) in
  assert (Crypto.keccak (Bytes.sub 0n 0n (Bytes.pack 5n)) = hashempty)

let test_sha3 =
  let hash5n = (0xfe2ecff30c0281f99ad639b9cfa50970ee98b382fa1688cef0bd33c7f5b0be16 : bytes) in
  let hashempty = (0xa7ffc6f8bf1ed76651c14756a061d662f580ff4de43b49fa82d80a4b80f8434a : bytes) in
  let () = assert (Crypto.sha3 (Bytes.pack 5n) = hash5n) in
  let () = assert (Test.eval (Crypto.sha3 (Bytes.pack 5n)) = Test.run (fun (n : nat) -> Crypto.sha3 (Bytes.pack n)) 5n) in
  assert (Crypto.sha3 (Bytes.sub 0n 0n (Bytes.pack 5n)) = hashempty)

let test_key_hash =
  let key = ("edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav" : key) in
  let key_hash = ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : key_hash) in
  let () = assert (Test.eval (Crypto.hash_key key) = Test.run (fun (k : key) -> Crypto.hash_key k) key) in
  assert (Crypto.hash_key key = key_hash)

let test_check =
  let key = ("edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav" : key) in
  let signature = ("edsigthTzJ8X7MPmNeEwybRAvdxS1pupqcM5Mk4uCuyZAe7uEk68YpuGDeViW8wSXMrCi5CwoNgqs8V2w8ayB5dMJzrYCHhD8C7" : signature) in
  let message = Crypto.blake2b (Bytes.pack "hello") in
  let () = assert (Crypto.check key signature message) in
  let message = Crypto.blake2b (Bytes.pack "hola") in
  assert (not (Crypto.check key signature message))

let test_int_bls =
  let alpha = (0xe406000000000000000000000000000000000000000000000000000000000000 : bls12_381_fr) in
  let alpha_int = int alpha in
  let mich_int = Test.run (fun (_ : unit) -> int (0xe406000000000000000000000000000000000000000000000000000000000000 : bls12_381_fr)) () in
  assert (Test.eval alpha_int = mich_int)

let test_not =
  let f ((x, y) : nat * int) : int = x * not y in
  assert (Test.eval (f (313n , 2938818607801353443)) = Test.run f (313n , 2938818607801353443))

let test_chain_id = 
  let chain_id = Test.eval ("NetXH12Aer3be93" : chain_id) in
  assert (chain_id = Test.eval (Tezos.get_chain_id ()))

let test_concats =
  let ss = ["aa"; "bb"; ""; "cc"] in
  let () = assert (String.concats ss = "aabbcc") in
  let bs = [(0x00 : bytes); (0x0102 : bytes); (0x03 : bytes)] in
  let () = assert (Bytes.concats bs = (0x00010203 : bytes)) in
  let () = assert (String.concats [] = "") in
  let () = assert (Bytes.concats [] = Bytes.sub 0n 0n (0x00 : bytes)) in
  let () = assert (Test.run (fun () -> String.concats ss) () = Test.eval (String.concats ss)) in
  let () = assert (Test.run (fun () -> Bytes.concats bs) () = Test.eval (Bytes.concats bs)) in
  ()