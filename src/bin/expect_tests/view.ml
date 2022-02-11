open Cli_expect

let contract = test

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "view.mligo" ; "--protocol" ; "hangzhou" ] ;
  [%expect {|
    Translat expression : let (gen#64, gen#65) = gen#66 in
                          PAIR(LIST_EMPTY() , gen#65)
     with env: Env[gen#66 : (unit * int)]
    Translat expression : gen#66
     with env: Env[gen#66 : (unit * int)]
    Translat expression : PAIR(LIST_EMPTY() , gen#65)
     with env: Env[gen#64 : unit , gen#65 : int ,
    gen#66 : (unit * int)]
    Translat expression : gen#65
     with env: Env[gen#64 : unit , gen#65 : int ,
    gen#66 : (unit * int)]
    Translat expression : LIST_EMPTY()
     with env: Env[gen#64 : unit , gen#65 : int ,
    gen#66 : (unit * int)]
    Translat expression : let (gen#130, gen#131) = gen#106 in
                          ADD(ADD(gen#131 , gen#130) , L(1))
     with env: Env[gen#106 : (int * int)]
    Translat expression : gen#106
     with env: Env[gen#106 : (int * int)]
    Translat expression : ADD(ADD(gen#131 , gen#130) , L(1))
     with env: Env[gen#130 : int , gen#131 : int ,
    gen#106 : (int * int)]
    Translat expression : L(1)
     with env: Env[gen#130 : int , gen#131 : int ,
    gen#106 : (int * int)]
    Translat expression : ADD(gen#131 , gen#130)
     with env: Env[gen#130 : int , gen#131 : int ,
    gen#106 : (int * int)]
    Translat expression : gen#130
     with env: Env[gen#130 : int , gen#131 : int ,
    gen#106 : (int * int)]
    Translat expression : gen#131
     with env: Env[gen#130 : int , gen#131 : int ,
    gen#106 : (int * int)]
    { parameter unit ;
      storage int ;
      code { CDR ; NIL operation ; PAIR } ;
      view "v1" int int { UNPAIR ; PUSH int 1 ; SWAP ; DIG 2 ; ADD ; ADD } } |}]

(* not warning is expected because the annotated view is still being included in the contract *)
let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "view.mligo" ; "--protocol" ; "hangzhou" ; "--views" ; "v1,v2" ] ;
  [%expect {|
    Translat expression : let (gen#64, gen#65) = gen#66 in
                          PAIR(LIST_EMPTY() , gen#65)
     with env: Env[gen#66 : (unit * int)]
    Translat expression : gen#66
     with env: Env[gen#66 : (unit * int)]
    Translat expression : PAIR(LIST_EMPTY() , gen#65)
     with env: Env[gen#64 : unit , gen#65 : int ,
    gen#66 : (unit * int)]
    Translat expression : gen#65
     with env: Env[gen#64 : unit , gen#65 : int ,
    gen#66 : (unit * int)]
    Translat expression : LIST_EMPTY()
     with env: Env[gen#64 : unit , gen#65 : int ,
    gen#66 : (unit * int)]
    Translat expression : let (gen#130, gen#131) = gen#106 in
                          ADD(ADD(gen#131 , gen#130) , L(1))
     with env: Env[gen#106 : (int * int)]
    Translat expression : gen#106
     with env: Env[gen#106 : (int * int)]
    Translat expression : ADD(ADD(gen#131 , gen#130) , L(1))
     with env: Env[gen#130 : int , gen#131 : int ,
    gen#106 : (int * int)]
    Translat expression : L(1)
     with env: Env[gen#130 : int , gen#131 : int ,
    gen#106 : (int * int)]
    Translat expression : ADD(gen#131 , gen#130)
     with env: Env[gen#130 : int , gen#131 : int ,
    gen#106 : (int * int)]
    Translat expression : gen#130
     with env: Env[gen#130 : int , gen#131 : int ,
    gen#106 : (int * int)]
    Translat expression : gen#131
     with env: Env[gen#130 : int , gen#131 : int ,
    gen#106 : (int * int)]
    Translat expression : let (gen#132, gen#133) = gen#107 in ADD(gen#133 , L(2))
     with env: Env[gen#107 : (int * int)]
    Translat expression : gen#107
     with env: Env[gen#107 : (int * int)]
    Translat expression : ADD(gen#133 , L(2))
     with env: Env[gen#132 : int , gen#133 : int ,
    gen#107 : (int * int)]
    Translat expression : L(2)
     with env: Env[gen#132 : int , gen#133 : int ,
    gen#107 : (int * int)]
    Translat expression : gen#133
     with env: Env[gen#132 : int , gen#133 : int ,
    gen#107 : (int * int)]
    { parameter unit ;
      storage int ;
      code { CDR ; NIL operation ; PAIR } ;
      view "v1" int int { UNPAIR ; PUSH int 1 ; SWAP ; DIG 2 ; ADD ; ADD } ;
      view "v2" int int { CDR ; PUSH int 2 ; ADD } } |}]

(* the following should trigger a warning because an annotated view is being ignored *)
let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "view.mligo" ; "--protocol" ; "hangzhou" ; "--views" ; "v2" ] ;
  [%expect {|
    Translat expression : let (gen#64, gen#65) = gen#66 in
                          PAIR(LIST_EMPTY() , gen#65)
     with env: Env[gen#66 : (unit * int)]
    Translat expression : gen#66
     with env: Env[gen#66 : (unit * int)]
    Translat expression : PAIR(LIST_EMPTY() , gen#65)
     with env: Env[gen#64 : unit , gen#65 : int ,
    gen#66 : (unit * int)]
    Translat expression : gen#65
     with env: Env[gen#64 : unit , gen#65 : int ,
    gen#66 : (unit * int)]
    Translat expression : LIST_EMPTY()
     with env: Env[gen#64 : unit , gen#65 : int ,
    gen#66 : (unit * int)]
    Translat expression : let (gen#132, gen#133) = gen#107 in ADD(gen#133 , L(2))
     with env: Env[gen#107 : (int * int)]
    Translat expression : gen#107
     with env: Env[gen#107 : (int * int)]
    Translat expression : ADD(gen#133 , L(2))
     with env: Env[gen#132 : int , gen#133 : int ,
    gen#107 : (int * int)]
    Translat expression : L(2)
     with env: Env[gen#132 : int , gen#133 : int ,
    gen#107 : (int * int)]
    Translat expression : gen#133
     with env: Env[gen#132 : int , gen#133 : int ,
    gen#107 : (int * int)]
    File "../../test/contracts/view.mligo", line 3, characters 12-14:
      2 |
      3 | [@view] let v1 (n,s: int * int) : int = s + n + 1
      4 | let v2 (_,s: int * int) : int = s + 2

    This view will be ignored, command line option override [
    view] annotation

    { parameter unit ;
      storage int ;
      code { CDR ; NIL operation ; PAIR } ;
      view "v2" int int { CDR ; PUSH int 2 ; ADD } } |}]

(* bad view type *)
let%expect_test _ =
  run_ligo_bad [ "compile" ; "contract" ; contract "view.mligo" ; "--protocol" ; "hangzhou" ; "--views" ; "v1,bad_view" ] ;
  [%expect {|
    Translat expression : let (gen#64, gen#65) = gen#66 in
                          PAIR(LIST_EMPTY() , gen#65)
     with env: Env[gen#66 : (unit * int)]
    Translat expression : gen#66
     with env: Env[gen#66 : (unit * int)]
    Translat expression : PAIR(LIST_EMPTY() , gen#65)
     with env: Env[gen#64 : unit , gen#65 : int ,
    gen#66 : (unit * int)]
    Translat expression : gen#65
     with env: Env[gen#64 : unit , gen#65 : int ,
    gen#66 : (unit * int)]
    Translat expression : LIST_EMPTY()
     with env: Env[gen#64 : unit , gen#65 : int ,
    gen#66 : (unit * int)]
    File "../../test/contracts/view.mligo", line 5, characters 14-17:
      4 | let v2 (_,s: int * int) : int = s + 2
      5 | let bad_view (_,_: int * nat ) : nat = 1n
      6 |

    Invalid view argument.
    View 'bad_view' has storage type 'nat' and contract 'main' has storage type 'int'. |}]