open Trace

module Simplify = struct

  let type_constants = [
    ("unit" , "unit") ;
    ("string" , "string") ;
    ("bytes" , "bytes") ;
    ("nat" , "nat") ;
    ("int" , "int") ;
    ("tez" , "tez") ;
    ("bool" , "bool") ;
    ("operation" , "operation") ;
    ("address" , "address") ;
    ("contract" , "contract") ;
    ("list" , "list") ;
    ("option" , "option") ;
    ("set" , "set") ;
    ("map" , "map") ;
    ("big_map" , "big_map") ;
  ]

  module Pascaligo = struct

    let constants = [
      ("get_force" , "MAP_GET_FORCE") ;
      ("transaction" , "CALL") ;
      ("get_contract" , "CONTRACT") ;
      ("size" , "SIZE") ;
      ("int" , "INT") ;
      ("abs" , "ABS") ;
      ("amount" , "AMOUNT") ;
      ("unit" , "UNIT") ;
      ("source" , "SOURCE") ;
    ]

    let type_constants = type_constants
  end

  module Camligo = struct
    let constants = [
      ("Bytes.pack" , "PACK") ;
      ("Crypto.hash" , "HASH") ;
      ("Operation.transaction" , "CALL") ;
      ("Operation.get_contract" , "GET_CONTRACT") ;
      ("sender" , "SENDER") ;
      ("unit" , "UNIT") ;
      ("source" , "SOURCE") ;
    ]

    let type_constants = type_constants
  end

  module Ligodity = struct
    include Pascaligo
  end

end

module Typer = struct

  open Ast_typed

  module Errors = struct
    let wrong_param_number = fun name expected got ->
      let title () = "wrong number of params" in
      let full () = Format.asprintf "constant name: %s\nexpected: %d\ngot: %d\n"
          name expected (List.length got) in
      error title full
  end


  type type_result = string * type_value
  type typer' = type_value list -> type_value option -> type_result result
  type typer = string * typer'

  let typer'_0 : name -> (type_value option -> type_value result) -> typer' = fun s f lst tv_opt ->
    match lst with
    | [] -> (
      let%bind tv' = f tv_opt in
      ok (s , tv')
    )
    | _ -> fail @@ Errors.wrong_param_number s 0 lst
  let typer_0 name f : typer = (name , typer'_0 name f)

  let typer'_1 : name -> (type_value -> type_value result) -> typer' = fun s f lst _ ->
    match lst with
    | [ a ] -> (
        let%bind tv' = f a in
        ok (s , tv')
      )
    | _ -> fail @@ Errors.wrong_param_number s 1 lst
  let typer_1 name f : typer = (name , typer'_1 name f)

  let typer'_1_opt : name -> (type_value -> type_value option -> type_value result) -> typer' = fun s f lst tv_opt ->
    match lst with
    | [ a ] -> (
        let%bind tv' = f a tv_opt in
        ok (s , tv')
      )
    | _ -> fail @@ Errors.wrong_param_number s 1 lst
  let typer_1_opt name f : typer = (name , typer'_1_opt name f)

  let typer'_2 : name -> (type_value -> type_value -> type_value result) -> typer' = fun s f lst _ ->
    match lst with
    | [ a ; b ] -> (
        let%bind tv' = f a b in
        ok (s , tv')
      )
    | _ -> fail @@ Errors.wrong_param_number s 2 lst
  let typer_2 name f : typer = (name , typer'_2 name f)

  let typer'_3 : name -> (type_value -> type_value -> type_value -> type_value result) -> typer' = fun s f lst _ ->
    match lst with
    | [ a ; b ; c ] -> (
        let%bind tv' = f a b c in
        ok (s , tv')
      )
    | _ -> fail @@ Errors.wrong_param_number s 3 lst
  let typer_3 name f : typer = (name , typer'_3 name f)

  let constant name cst = typer_0 name (fun _ -> ok cst)

  open Combinators

  let eq_1 a cst = type_value_eq (a , cst)
  let eq_2 (a , b) cst = type_value_eq (a , cst) && type_value_eq (b , cst)

  let comparator : string -> typer = fun s -> typer_2 s @@ fun a b ->
    let%bind () =
      trace_strong (simple_error "Types a and b aren't comparable") @@
      Assert.assert_true @@
      List.exists (eq_2 (a , b)) [
        t_int () ;
        t_nat () ;
        t_tez () ;
        t_string () ;
        t_bytes () ;
        t_address () ;
      ] in
    ok @@ t_bool ()

  let boolean_operator_2 : string -> typer = fun s -> typer_2 s @@ fun a b ->
    let%bind () =
      trace_strong (simple_error "A isn't of type bool") @@
      Assert.assert_true @@
      type_value_eq (t_bool () , a) in
    let%bind () =
      trace_strong (simple_error "B isn't of type bool") @@
      Assert.assert_true @@
      type_value_eq (t_bool () , b) in
    ok @@ t_bool ()

  let none = typer_0 "NONE" @@ fun tv_opt ->
    match tv_opt with
    | None -> simple_fail "untyped NONE"
    | Some t -> ok t

  let sub = typer_2 "SUB" @@ fun a b ->
    let%bind () =
      trace_strong (simple_error "Types a and b aren't numbers") @@
      Assert.assert_true @@
      List.exists (eq_2 (a , b)) [
        t_int () ;
        t_nat () ;
      ] in
    ok @@ t_int ()

  let some = typer_1 "SOME" @@ fun a -> ok @@ t_option a ()

  let map_remove : typer = typer_2 "MAP_REMOVE" @@ fun k m ->
    let%bind (src , _) = get_t_map m in
    let%bind () = assert_type_value_eq (src , k) in
    ok m

  let map_update : typer = typer_3 "MAP_UPDATE" @@ fun k v m ->
    let%bind (src, dst) = get_t_map m in
    let%bind () = assert_type_value_eq (src, k) in
    let%bind () = assert_type_value_eq (dst, v) in
    ok m

  let size = typer_1 "SIZE" @@ fun t ->
    let%bind () =
      Assert.assert_true @@
      (is_t_map t || is_t_list t) in
    ok @@ t_nat ()

  let get_force = typer_2 "MAP_GET_FORCE" @@ fun i m ->
    let%bind (src, dst) = get_t_map m in
    let%bind _ = assert_type_value_eq (src, i) in
    ok dst

  let int : typer = typer_1 "INT" @@ fun t ->
    let%bind () = assert_t_nat t in
    ok @@ t_int ()

  let bytes_pack : typer = typer_1 "PACK" @@ fun _t ->
    ok @@ t_bytes ()

  let bytes_unpack = typer_1_opt "UNPACK" @@ fun input output_opt ->
    let%bind () = assert_t_bytes input in
    trace_option (simple_error "untyped UNPACK") @@
    output_opt

  let crypto_hash = typer_1 "HASH" @@ fun t ->
    let%bind () = assert_t_bytes t in
    ok @@ t_bytes ()

  let sender = constant "SENDER" @@ t_address ()

  let source = constant "SOURCE" @@ t_address ()

  let unit = constant "UNIT" @@ t_unit ()

  let amount = constant "AMOUNT" @@ t_tez ()

  let transaction = typer_3 "CALL" @@ fun param amount contract ->
    let%bind () = assert_t_tez amount in
    let%bind contract_param = get_t_contract contract in
    let%bind () = assert_type_value_eq (param , contract_param) in
    ok @@ t_operation ()

  let get_contract = typer_1_opt "CONTRACT" @@ fun _ tv_opt ->
    let%bind tv =
      trace_option (simple_error "get_contract needs a type annotation") tv_opt in
    let%bind tv' =
      trace_strong (simple_error "get_contract has a not-contract annotation") @@
      get_t_contract tv in
    ok @@ t_contract tv' ()

  let abs = typer_1 "ABS" @@ fun t ->
    let%bind () = assert_t_int t in
    ok @@ t_nat ()

  let times = typer_2 "TIMES" @@ fun a b ->
    if eq_2 (a , b) (t_nat ())
    then ok @@ t_nat () else
    if eq_2 (a , b) (t_int ())
    then ok @@ t_int () else
    if (eq_1 a (t_nat ()) && eq_1 b (t_tez ())) || (eq_1 b (t_nat ()) && eq_1 a (t_tez ()))
    then ok @@ t_tez () else
      simple_fail "Multiplying with wrong types"

  let div = typer_2 "DIV" @@ fun a b ->
    if eq_2 (a , b) (t_nat ())
    then ok @@ t_nat () else
    if eq_2 (a , b) (t_int ())
    then ok @@ t_int () else
      simple_fail "Dividing with wrong types"

  let mod_ = typer_2 "MOD" @@ fun a b ->
    if (eq_1 a (t_nat ()) || eq_1 a (t_int ())) && (eq_1 b (t_nat ()) || eq_1 b (t_int ()))
    then ok @@ t_nat () else
      simple_fail "Computing modulo with wrong types"

  let add = typer_2 "ADD" @@ fun a b ->
    if eq_2 (a , b) (t_nat ())
    then ok @@ t_nat () else
    if eq_2 (a , b) (t_int ())
    then ok @@ t_int () else
    if (eq_1 a (t_nat ()) && eq_1 b (t_int ())) || (eq_1 b (t_nat ()) && eq_1 a (t_int ()))
    then ok @@ t_int () else
      simple_fail "Adding with wrong types"



  let constant_typers =
    let typer_to_kv : typer -> (string * _) = fun x -> x in
    Map.String.of_list
    @@ List.map typer_to_kv [
      add ;
      times ;
      div ;
      mod_ ;
      sub ;
      none ;
      some ;
      comparator "EQ" ;
      comparator "NEQ" ;
      comparator "LT" ;
      comparator "GT" ;
      comparator "LE" ;
      comparator "GE" ;
      boolean_operator_2 "OR" ;
      boolean_operator_2 "AND" ;
      map_remove ;
      map_update ;
      int ;
      size ;
      get_force ;
      bytes_pack ;
      bytes_unpack ;
      crypto_hash ;
      sender ;
      source ;
      unit ;
      amount ;
      transaction ;
      get_contract ;
      abs ;
    ]

end

module Compiler = struct

  module Michelson = Tezos_utils.Michelson
  open Michelson

  type predicate =
    | Constant of michelson
    | Unary of michelson
    | Binary of michelson
    | Ternary of michelson

  let simple_constant c = Constant c

  let simple_unary c = Unary c

  let simple_binary c = Binary c

  let simple_ternary c = Ternary c

  let predicates = Map.String.of_list [
    ("ADD" , simple_binary @@ prim I_ADD) ;
    ("SUB" , simple_binary @@ prim I_SUB) ;
    ("TIMES" , simple_binary @@ prim I_MUL) ;
    ("DIV" , simple_binary @@ seq [prim I_EDIV ; i_assert_some_msg (i_push_string "DIV by 0") ; i_car]) ;
    ("MOD" , simple_binary @@ seq [prim I_EDIV ; i_assert_some_msg (i_push_string "MOD by 0") ; i_cdr]) ;
    ("NEG" , simple_unary @@ prim I_NEG) ;
    ("OR" , simple_binary @@ prim I_OR) ;
    ("AND" , simple_binary @@ prim I_AND) ;
    ("PAIR" , simple_binary @@ prim I_PAIR) ;
    ("CAR" , simple_unary @@ prim I_CAR) ;
    ("CDR" , simple_unary @@ prim I_CDR) ;
    ("EQ" , simple_binary @@ seq [prim I_COMPARE ; prim I_EQ]) ;
    ("NEQ" , simple_binary @@ seq [prim I_COMPARE ; prim I_NEQ]) ;
    ("LT" , simple_binary @@ seq [prim I_COMPARE ; prim I_LT]) ;
    ("LE" , simple_binary @@ seq [prim I_COMPARE ; prim I_LE]) ;
    ("GT" , simple_binary @@ seq [prim I_COMPARE ; prim I_GT]) ;
    ("GE" , simple_binary @@ seq [prim I_COMPARE ; prim I_GE]) ;
    ("UPDATE" , simple_ternary @@ prim I_UPDATE) ;
    ("SOME" , simple_unary @@ prim I_SOME) ;
    ("MAP_GET_FORCE" , simple_binary @@ seq [prim I_GET ; i_assert_some_msg (i_push_string "GET_FORCE")]) ;
    ("MAP_GET" , simple_binary @@ prim I_GET) ;
    ("SIZE" , simple_unary @@ prim I_SIZE) ;
    ("FAILWITH" , simple_unary @@ prim I_FAILWITH) ;
    ("ASSERT" , simple_binary @@ i_if (seq [i_failwith]) (seq [i_drop ; i_push_unit])) ;
    ("INT" , simple_unary @@ prim I_INT) ;
    ("ABS" , simple_unary @@ prim I_ABS) ;
    ("CONS" , simple_binary @@ prim I_CONS) ;
    ("UNIT" , simple_constant @@ prim I_UNIT) ;
    ("AMOUNT" , simple_constant @@ prim I_AMOUNT) ;
    ("CALL" , simple_ternary @@ prim I_TRANSFER_TOKENS) ;
    ("SOURCE" , simple_constant @@ prim I_SOURCE) ;
    ("SENDER" , simple_constant @@ prim I_SENDER) ;
    ( "MAP_UPDATE" , simple_ternary @@ seq [dip (i_some) ; prim I_UPDATE ]) ;
  ]

end
