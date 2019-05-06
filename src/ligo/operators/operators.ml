open Trace

module Simplify = struct

  let type_constants = [
    ("unit" , 0) ;
    ("string" , 0) ;
    ("bytes" , 0) ;
    ("nat" , 0) ;
    ("int" , 0) ;
    ("tez" , 0) ;
    ("bool" , 0) ;
    ("operation" , 0) ;
    ("address" , 0) ;
    ("contract" , 1) ;
    ("list" , 1) ;
    ("option" , 1) ;
    ("set" , 1) ;
    ("map" , 2) ;
    ("big_map" , 2) ;
  ]

  let constants = [
    ("get_force" , 2) ;
    ("transaction" , 3) ;
    ("get_contract" , 1) ;
    ("size" , 1) ;
    ("int" , 1) ;
    ("abs" , 1) ;
    ("amount" , 0) ;
    ("unit" , 0) ;
    ("source" , 0) ;
  ]

  module Camligo = struct
    let constants = [
      ("Bytes.pack" , 1) ;
      ("Crypto.hash" , 1) ;
      ("Operation.transaction" , 3) ;
      ("Operation.get_contract" , 1) ;
      ("sender" , 0) ;
      ("unit" , 0) ;
      ("source" , 0) ;
    ]
  end

end

module Typer = struct
  module Errors = struct
    let wrong_param_number = fun name ->
      let title () = "wrong number of params" in
      let full () = name in
      error title full
  end

  open Ast_typed

  type typer_predicate = type_value list -> bool
  type type_result = string * type_value
  type typer' = type_value list -> type_value option -> type_result result
  type typer = string * int * (typer_predicate * typer') list

  let predicate_0 : typer_predicate = fun lst ->
    match lst with
    | [] -> true
    | _ -> false

  let predicate_1 : (type_value -> bool) -> typer_predicate = fun f lst ->
    match lst with
    | [ a ] -> f a
    | _ -> false

  let predicate_2 : (type_value -> type_value -> bool) -> typer_predicate = fun f lst ->
    match lst with
    | [ a ; b ] -> f a b
    | _ -> false

  let predicate_3 : (type_value -> type_value -> type_value -> bool) -> typer_predicate = fun f lst ->
    match lst with
    | [ a ; b ; c ] -> f a b c
    | _ -> false

  let true_1 = predicate_1 (fun _ -> true)
  let true_2 = predicate_2 (fun _ _ -> true)
  let true_3 = predicate_3 (fun _ _ _ -> true)

  let eq_1 : type_value -> typer_predicate = fun v ->
    let aux = fun a -> type_value_eq (a, v) in
    predicate_1 aux

  let eq_2 : type_value -> typer_predicate = fun v ->
    let aux = fun a b -> type_value_eq (a, v) && type_value_eq (b, v) in
    predicate_2 aux

  let typer'_0 : (type_value option -> type_result result) -> typer' = fun f lst tv ->
    match lst with
    | [] -> f tv
    | _ -> simple_fail "!!!"

  let typer'_1 : (type_value -> type_result result) -> typer' = fun f lst _ ->
    match lst with
    | [ a ] -> f a
    | _ -> simple_fail "!!!"

  let typer'_1_opt : (type_value -> type_value option -> type_result result) -> typer' = fun f lst tv_opt ->
    match lst with
    | [ a ] -> f a tv_opt
    | _ -> simple_fail "!!!"

  let typer'_2 : (type_value -> type_value -> type_result result) -> typer' = fun f lst _ ->
    match lst with
    | [ a ; b ] -> f a b
    | _ -> simple_fail "!!!"

  let typer'_3 : (type_value -> type_value -> type_value -> type_result result) -> typer' = fun f lst _ ->
    match lst with
    | [ a ; b ; c ] -> f a b c
    | _ -> simple_fail "!!!"

  let typer_constant cst : typer' = fun _ _ -> ok cst

  let constant_2 : string -> type_value -> typer' = fun s tv ->
    let aux = fun _ _ -> ok (s, tv) in
    typer'_2 aux

  let make_2 : string -> _ list -> typer = fun name pfs ->
    (name , 2 , List.map (Tuple.map_h_2 predicate_2 typer'_2) pfs)

  let same_2 : string -> (string * type_value) list -> typer = fun s lst ->
    let aux (s, tv) = eq_2 tv, constant_2 s tv in
    (s , 2 , List.map aux lst)

  let very_same_2 : string -> type_value -> typer = fun s tv -> same_2 s [s , tv]

  open Combinators

  let comparator : string -> typer = fun s -> s , 2 , [
      (eq_2 (t_int ()), constant_2 s (t_bool ())) ;
      (eq_2 (t_nat ()), constant_2 s (t_bool ())) ;
      (eq_2 (t_tez ()), constant_2 s (t_bool ())) ;
      (eq_2 (t_bytes ()), constant_2 s (t_bool ())) ;
      (eq_2 (t_string ()), constant_2 s (t_bool ())) ;
      (eq_2 (t_address ()), constant_2 s (t_bool ())) ;
    ]

  let boolean_operator_2 : string -> typer = fun s -> very_same_2 s (t_bool ())

  let none = "NONE" , 0 , [
      predicate_0 , typer'_0 (fun tv_opt -> match tv_opt with
          | None -> simple_fail "untyped NONE"
          | Some t -> ok ("NONE", t))
    ]

  let sub = "SUB" , 2 , [
      eq_2 (t_int ()) , constant_2 "SUB_INT" (t_int ()) ;
      eq_2 (t_nat ()) , constant_2 "SUB_NAT" (t_int ()) ;
    ]

  let some = "SOME" , 1 , [
      true_1 , typer'_1 (fun s -> ok ("SOME", t_option s ())) ;
    ]

  let map_remove : typer = "MAP_REMOVE" , 2 , [
      (true_2 , typer'_2 (fun k m ->
          let%bind (src, _) = get_t_map m in
          let%bind () = assert_type_value_eq (src, k) in
          ok ("MAP_REMOVE", m)
        ))
    ]

  let map_update : typer = "MAP_UPDATE" , 3 , [
      (true_3 , typer'_3 (fun k v m ->
           let%bind (src, dst) = get_t_map m in
           let%bind () = assert_type_value_eq (src, k) in
           let%bind () = assert_type_value_eq (dst, v) in
           ok ("MAP_UPDATE", m)))
    ]

  let size : typer = "size" , 1 , [
      (true_1, typer'_1 (fun t ->
           let%bind () = bind_or (assert_t_map t, assert_t_list t) in
           ok ("SIZE", t_nat ())))
    ]

  let get_force : typer = "get_force" , 2 , [
      (true_2, typer'_2 (fun i_ty m_ty ->
           let%bind (src, dst) = get_t_map m_ty in
           let%bind _ = assert_type_value_eq (src, i_ty) in
           ok ("GET_FORCE", dst)))
    ]

  let int : typer = "int" , 1 , [
      (eq_1 (t_nat ()), typer_constant ("INT" , t_int ()))
    ]

  let bytes_pack : typer = "Bytes.pack" , 1 , [
      (true_1 , typer'_1 (fun _ -> ok ("PACK" , t_bytes ())))
    ]

  let bytes_unpack = "Bytes.unpack" , 1 , [
      eq_1 (t_bytes ()) , typer'_1_opt (fun _ tv_opt -> match tv_opt with
          | None -> simple_fail "untyped UNPACK"
          | Some t -> ok ("UNPACK", t))
    ]

  let crypto_hash = "Crypto.hash" , 1 , [
      eq_1 (t_bytes ()) , typer_constant ("HASH" , t_bytes ()) ;
    ]

  let sender = "sender" , 0 , [
      predicate_0 , typer_constant ("SENDER", t_address ())
    ]

  let source = "source" , 0 , [
      predicate_0 , typer_constant ("SOURCE", t_address ())
    ]

  let unit = "unit" , 0 , [
      predicate_0 , typer_constant ("UNIT", t_unit ())
    ]

  let amount = "amount" , 0 , [
      predicate_0 , typer_constant ("AMOUNT", t_tez ())
    ]

  let transaction = "Operation.transaction" , 3 , [
      true_3 , typer'_3 (
        fun param amount contract ->
          let%bind () =
            assert_t_tez amount in
          let%bind contract_param =
            get_t_contract contract in
          let%bind () =
            assert_type_value_eq (param , contract_param) in
          ok ("TRANSFER_TOKENS" , t_operation ())
      )
    ]
  let transaction' = "transaction" , 3 , [
      true_3 , typer'_3 (
        fun param amount contract ->
          let%bind () =
            assert_t_tez amount in
          let%bind contract_param =
            get_t_contract contract in
          let%bind () =
            assert_type_value_eq (param , contract_param) in
          ok ("TRANSFER_TOKENS" , t_operation ())
      )
    ]

  let get_contract = "Operation.get_contract" , 1 , [
      eq_1 (t_address ()) , typer'_1_opt (
        fun _ tv_opt ->
          let%bind tv =
            trace_option (simple_error "get_contract needs a type annotation") tv_opt in
          let%bind tv' =
            trace_strong (simple_error "get_contract has a not-contract annotation") @@
            get_t_contract tv in
          ok ("CONTRACT" , t_contract tv' ())
      )
    ]
  let get_contract' = "get_contract" , 1 , [
      eq_1 (t_address ()) , typer'_1_opt (
        fun _ tv_opt ->
          let%bind tv =
            trace_option (simple_error "get_contract needs a type annotation") tv_opt in
          let%bind tv' =
            trace_strong (simple_error "get_contract has a not-contract annotation") @@
            get_t_contract tv in
          ok ("CONTRACT" , t_contract tv' ())
      )
    ]

  let num_2 : typer_predicate =
    let aux = fun a b ->
      (type_value_eq (a , t_int ()) || type_value_eq (a , t_nat ())) &&
      (type_value_eq (b , t_int ()) || type_value_eq (b , t_nat ())) in
    predicate_2 aux

  let mod_ = "MOD" , 2 , [
      num_2 , constant_2 "MOD" (t_nat ()) ;
    ]

  let abs = "abs" , 1 , [
      eq_1 (t_int ()) , typer_constant ("ABS" , (t_nat ())) ;
    ]

  let times = "TIMES" , 2 , [
      (eq_2 (t_nat ()) , constant_2 "TIMES_NAT" (t_nat ())) ;
      (num_2 , constant_2 "TIMES_INT" (t_int ())) ;
      (
        let aux a b =
          (type_value_eq (a , t_nat ()) && type_value_eq (b , t_tez ())) ||
          (type_value_eq (b , t_nat ()) && type_value_eq (a , t_tez ())) in
       predicate_2 aux , constant_2 "TIMES_TEZ" (t_tez ())
     ) ;
    ]

  let constant_typers =
    let typer_to_kv : typer -> (string * _) = fun (a, b, c) -> (a, (b, c)) in
    Map.String.of_list
    @@ List.map typer_to_kv [
      same_2 "ADD" [
        ("ADD_INT" , t_int ()) ;
        ("ADD_NAT" , t_nat ()) ;
        ("CONCAT" , t_string ()) ;
      ] ;
      times ;
      same_2 "DIV" [
        ("DIV_INT" , t_int ()) ;
        ("DIV_NAT" , t_nat ()) ;
      ] ;
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
      transaction' ;
      get_contract ;
      get_contract' ;
      abs ;
    ]

end

module Compiler = struct

  module Michelson = Micheline.Michelson
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
    ("ADD_INT" , simple_binary @@ prim I_ADD) ;
    ("ADD_NAT" , simple_binary @@ prim I_ADD) ;
    ("SUB_INT" , simple_binary @@ prim I_SUB) ;
    ("SUB_NAT" , simple_binary @@ prim I_SUB) ;
    ("TIMES_INT" , simple_binary @@ prim I_MUL) ;
    ("TIMES_NAT" , simple_binary @@ prim I_MUL) ;
    ("TIMES_TEZ" , simple_binary @@ prim I_MUL) ;
    ("DIV_INT" , simple_binary @@ seq [prim I_EDIV ; i_assert_some_msg (i_push_string "DIV by 0") ; i_car]) ;
    ("DIV_NAT" , simple_binary @@ seq [prim I_EDIV ; i_assert_some_msg (i_push_string "DIV by 0") ; i_car]) ;
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
    ("GET_FORCE" , simple_binary @@ seq [prim I_GET ; i_assert_some_msg (i_push_string "GET_FORCE")]) ;
    ("GET" , simple_binary @@ prim I_GET) ;
    ("SIZE" , simple_unary @@ prim I_SIZE) ;
    ("FAILWITH" , simple_unary @@ prim I_FAILWITH) ;
    ("ASSERT" , simple_binary @@ i_if (seq [i_failwith]) (seq [i_drop ; i_push_unit])) ;
    ("INT" , simple_unary @@ prim I_INT) ;
    ("ABS" , simple_unary @@ prim I_ABS) ;
    ("CONS" , simple_binary @@ prim I_CONS) ;
    ("UNIT" , simple_constant @@ prim I_UNIT) ;
    ("AMOUNT" , simple_constant @@ prim I_AMOUNT) ;
    ("TRANSFER_TOKENS" , simple_ternary @@ prim I_TRANSFER_TOKENS) ;
    ("SOURCE" , simple_constant @@ prim I_SOURCE) ;
    ("SENDER" , simple_constant @@ prim I_SENDER) ;
    ( "MAP_UPDATE" , simple_ternary @@ seq [dip (i_some) ; prim I_UPDATE ]) ;
  ]

end
