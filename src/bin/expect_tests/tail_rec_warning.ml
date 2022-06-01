open Cli_expect

let contract = test

let%expect_test _ =
  run_ligo_good ["compile"; "contract" ; contract "unused_recursion.mligo" ] ;
  [%expect {|
    { parameter unit ;
      storage (pair (int %bar) (int %foo)) ;
      code { CDR ;
             PUSH int 0 ;
             PUSH int 1 ;
             PUSH int 2 ;
             ADD ;
             ADD ;
             SWAP ;
             CDR ;
             SWAP ;
             PAIR ;
             NIL operation ;
             PAIR } } |} ]

let%expect_test _ =
  run_ligo_good ["compile"; "contract" ; contract "unused_recursion.mligo" ; "--warn-unused-rec" ] ;
  [%expect {|
    File "../../test/contracts/unused_recursion.mligo", line 15, characters 10-13:
     14 |   (* fun_name shadowed in body *)
     15 |   let rec bar : int -> t = fun (x : int) ->
     16 |     let bar = x in
    :
    Warning: unused recursion .
    Hint: remove recursion from the function "bar" to prevent this warning.

    File "../../test/contracts/unused_recursion.mligo", line 12, characters 10-13:
     11 |   (* parameter shadows fun_name: complex *)
     12 |   let rec foo : (int -> int) -> int = fun (foo : (int -> int)) -> let foo = foo 0 in foo in
     13 |
    :
    Warning: unused recursion .
    Hint: remove recursion from the function "foo" to prevent this warning.

    File "../../test/contracts/unused_recursion.mligo", line 9, characters 10-14:
      8 |   (* parameter shadows fun_name: simple *)
      9 |   let rec toto : int -> int = fun (toto:int) : int -> let number = toto in number + 1 in
     10 |
    :
    Warning: unused recursion .
    Hint: remove recursion from the function "toto" to prevent this warning.

    { parameter unit ;
      storage (pair (int %bar) (int %foo)) ;
      code { CDR ;
             PUSH int 0 ;
             PUSH int 1 ;
             PUSH int 2 ;
             ADD ;
             ADD ;
             SWAP ;
             CDR ;
             SWAP ;
             PAIR ;
             NIL operation ;
             PAIR } } |} ]

let%expect_test _ =
  run_ligo_good ["compile"; "contract" ; contract "unused_recursion.jsligo" ] ;
  [%expect{|
    File "../../test/contracts/unused_recursion.jsligo", line 3, characters 4-10:
      2 |
      3 | let coucou = (storage : t) : t => {
      4 |   let number = 2;

    Toplevel let declaration are silently change to const declaration.

    File "../../test/contracts/unused_recursion.jsligo", line 31, characters 4-8:
     30 |
     31 | let main = ([_, storage] : [unit, t]) : [list<operation>, t] => {
     32 |   return [

    Toplevel let declaration are silently change to const declaration.

    { parameter unit ;
      storage (pair (int %bar) (int %foo)) ;
      code { CDR ;
             PUSH int 0 ;
             PUSH int 1 ;
             PUSH int 2 ;
             ADD ;
             ADD ;
             SWAP ;
             CDR ;
             SWAP ;
             PAIR ;
             NIL operation ;
             PAIR } } |} ]

let%expect_test _ =
  run_ligo_good ["compile"; "contract" ; contract "unused_recursion.jsligo" ; "--warn-unused-rec" ] ;
  [%expect{|
    File "../../test/contracts/unused_recursion.jsligo", line 3, characters 4-10:
      2 |
      3 | let coucou = (storage : t) : t => {
      4 |   let number = 2;

    Toplevel let declaration are silently change to const declaration.

    File "../../test/contracts/unused_recursion.jsligo", line 31, characters 4-8:
     30 |
     31 | let main = ([_, storage] : [unit, t]) : [list<operation>, t] => {
     32 |   return [

    Toplevel let declaration are silently change to const declaration.

    { parameter unit ;
      storage (pair (int %bar) (int %foo)) ;
      code { CDR ;
             PUSH int 0 ;
             PUSH int 1 ;
             PUSH int 2 ;
             ADD ;
             ADD ;
             SWAP ;
             CDR ;
             SWAP ;
             PAIR ;
             NIL operation ;
             PAIR } } |} ]