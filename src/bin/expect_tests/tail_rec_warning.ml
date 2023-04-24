open Cli_expect

let contract = test

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "unused_recursion.mligo" ];
  [%expect
    {|
    { parameter unit ;
      storage (pair (int %bar) (int %foo)) ;
      code { CDR ;
             PUSH int 0 ;
             PUSH int 1 ;
             PUSH int 2 ;
             ADD ;
             ADD ;
             UPDATE 1 ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"; "contract"; contract "unused_recursion.mligo"; "--warn-unused-rec" ];
  [%expect
    {|
    File "../../test/contracts/unused_recursion.mligo", line 9, characters 10-14:
      8 |   (* parameter shadows fun_name: simple *)
      9 |   let rec toto : int -> int = fun (toto:int) : int -> let number = toto in number + 1 in
                    ^^^^
     10 |
    :
    Warning: unused recursion .
    Hint: remove recursion from the function "toto" to prevent this warning.

    File "../../test/contracts/unused_recursion.mligo", line 12, characters 10-13:
     11 |   (* parameter shadows fun_name: complex *)
     12 |   let rec foo : (int -> int) -> int = fun (foo : (int -> int)) -> let foo = foo 0 in foo in
                    ^^^
     13 |
    :
    Warning: unused recursion .
    Hint: remove recursion from the function "foo" to prevent this warning.

    File "../../test/contracts/unused_recursion.mligo", line 15, characters 10-13:
     14 |   (* fun_name shadowed in body *)
     15 |   let rec bar : int -> t = fun (x : int) ->
                    ^^^
     16 |     let bar = x in
    :
    Warning: unused recursion .
    Hint: remove recursion from the function "bar" to prevent this warning.

    { parameter unit ;
      storage (pair (int %bar) (int %foo)) ;
      code { CDR ;
             PUSH int 0 ;
             PUSH int 1 ;
             PUSH int 2 ;
             ADD ;
             ADD ;
             UPDATE 1 ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "unused_recursion.jsligo" ];
  [%expect
    {|
    File "../../test/contracts/unused_recursion.jsligo", line 3, character 0 to line 29, character 1:
      2 |
      3 | let coucou = (storage : t) : t => {
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      4 |   let number = 2;
          ^^^^^^^^^^^^^^^^^
      5 |
          ^^
      6 |   let id = (x : int) : int => x;
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      7 |
          ^^
      8 |   /* parameter shadows fun_name: simple */
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      9 |   let toto = (toto:int) : int => {
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     10 |     let number = toto;
          ^^^^^^^^^^^^^^^^^^^^^^
     11 |     return number + 1;
          ^^^^^^^^^^^^^^^^^^^^^^
     12 |   };
          ^^^^
     13 |

     14 |   /* parameter shadows fun_name: complex */
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     15 |   let foo = (foo : ((p:int) => int)) : int => {
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     16 |     let foox = foo(0);
          ^^^^^^^^^^^^^^^^^^^^^^
     17 |     return foox;
          ^^^^^^^^^^^^^^^^
     18 |   };
          ^^^^
     19 |
          ^^
     20 |   /* fun_name shadowed in body */
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     21 |   let bar = (x : int) : t => {
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     22 |     let bar = x;
          ^^^^^^^^^^^^^^^^
     23 |     return { ...storage, bar : bar };
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     24 |   };
          ^^^^
     25 |
          ^^
     26 |   let n = toto(number)  + foo(id);
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     27 |
          ^^
     28 |   return bar(n);
          ^^^^^^^^^^^^^^^^^
     29 | }
          ^
     30 |

    Toplevel let declaration are silently change to const declaration.

    File "../../test/contracts/unused_recursion.jsligo", line 31, character 0 to line 36, character 1:
     30 |
     31 | let main = (_ : unit, storage : t) : [list<operation>, t] => {
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     32 |   return [
          ^^^^^^^^^^
     33 |     (list([]) as list<operation>),
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     34 |     coucou(storage)
          ^^^^^^^^^^^^^^^^^^^
     35 |   ];
          ^^^^
     36 | }
          ^

    Toplevel let declaration are silently change to const declaration.

    { parameter unit ;
      storage (pair (int %bar) (int %foo)) ;
      code { CDR ;
             PUSH int 0 ;
             PUSH int 1 ;
             PUSH int 2 ;
             ADD ;
             ADD ;
             UPDATE 1 ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"; "contract"; contract "unused_recursion.jsligo"; "--warn-unused-rec" ];
  [%expect
    {|
    File "../../test/contracts/unused_recursion.jsligo", line 3, character 0 to line 29, character 1:
      2 |
      3 | let coucou = (storage : t) : t => {
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      4 |   let number = 2;
          ^^^^^^^^^^^^^^^^^
      5 |
          ^^
      6 |   let id = (x : int) : int => x;
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      7 |
          ^^
      8 |   /* parameter shadows fun_name: simple */
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      9 |   let toto = (toto:int) : int => {
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     10 |     let number = toto;
          ^^^^^^^^^^^^^^^^^^^^^^
     11 |     return number + 1;
          ^^^^^^^^^^^^^^^^^^^^^^
     12 |   };
          ^^^^
     13 |

     14 |   /* parameter shadows fun_name: complex */
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     15 |   let foo = (foo : ((p:int) => int)) : int => {
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     16 |     let foox = foo(0);
          ^^^^^^^^^^^^^^^^^^^^^^
     17 |     return foox;
          ^^^^^^^^^^^^^^^^
     18 |   };
          ^^^^
     19 |
          ^^
     20 |   /* fun_name shadowed in body */
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     21 |   let bar = (x : int) : t => {
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     22 |     let bar = x;
          ^^^^^^^^^^^^^^^^
     23 |     return { ...storage, bar : bar };
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     24 |   };
          ^^^^
     25 |
          ^^
     26 |   let n = toto(number)  + foo(id);
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     27 |
          ^^
     28 |   return bar(n);
          ^^^^^^^^^^^^^^^^^
     29 | }
          ^
     30 |

    Toplevel let declaration are silently change to const declaration.

    File "../../test/contracts/unused_recursion.jsligo", line 31, character 0 to line 36, character 1:
     30 |
     31 | let main = (_ : unit, storage : t) : [list<operation>, t] => {
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     32 |   return [
          ^^^^^^^^^^
     33 |     (list([]) as list<operation>),
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     34 |     coucou(storage)
          ^^^^^^^^^^^^^^^^^^^
     35 |   ];
          ^^^^
     36 | }
          ^

    Toplevel let declaration are silently change to const declaration.

    { parameter unit ;
      storage (pair (int %bar) (int %foo)) ;
      code { CDR ;
             PUSH int 0 ;
             PUSH int 1 ;
             PUSH int 2 ;
             ADD ;
             ADD ;
             UPDATE 1 ;
             NIL operation ;
             PAIR } } |}]
