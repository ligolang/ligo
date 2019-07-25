open Trace

(*
  This file is used throughout the pipeline. Its idea is to add a unique place
  that you have to modify when you add a new operator/constant to the language.

  This file mirrors the LIGO pipeline, starting with Simplify, then Typer and
  ending with Compiler. Usually, when adding a new operator, you'll have to add
  a new constructor at all those places.
*)

module Simplify = struct

  (*
    Each front-end has its owns constants.

    Constants are special names that have their own case in the AST. E_constant
    for regular constants, and T_constant for type constants. Both types are
    defined in `Ast_simplified/types.ml`.
    For instance, "2 + 2" in Pascaligo is translated to `E_constant ("ADD" , [
      E_literal (Literal_int 2) ;
      E_literal (Literal_int 2) ;
    ])`.

    They are used to represent what can't expressed in the languages:
    - Primitives. Like "int", "string", "unit" for types. Or "+" for values.
    - Tezos specific stuff. Like "operation" for types. Or "source" for values.
    - What can't be represented in the language yet. Like "list" or "List.fold".

    Each constant is expressed as a pair:
    - The left-hand-side is the reserved name in the given front-end.
    - The right-hand-side is the name that will be used in the AST.
  *)

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
    ("key" , "key") ;
    ("key_hash" , "key_hash") ;
    ("signature" , "signature") ;
    ("timestamp" , "timestamp") ;
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
      ("now" , "NOW") ;
      ("unit" , "UNIT") ;
      ("source" , "SOURCE") ;
      ("sender" , "SENDER") ;
      ("failwith" , "FAILWITH") ;
      ("bitwise_or" , "OR") ;
      ("bitwise_and" , "AND") ;
      ("bitwise_xor" , "XOR") ;
      ("string_concat" , "CONCAT") ;
      ("string_slice" , "SLICE") ;
      ("set_empty" , "SET_EMPTY") ;
      ("set_mem" , "SET_MEM") ;
      ("set_add" , "SET_ADD") ;
      ("set_remove" , "SET_REMOVE") ;
      ("set_iter" , "SET_ITER") ;
      ("list_iter" , "LIST_ITER") ;
      ("list_map" , "LIST_MAP") ;
      ("map_iter" , "MAP_ITER") ;
      ("map_map" , "MAP_MAP") ;
    ]

    let type_constants = type_constants
  end

  module Camligo = struct
    let constants = [
      ("Bytes.pack" , "PACK") ;
      ("Crypto.hash" , "HASH") ;
      ("Operation.transaction" , "CALL") ;
      ("Operation.get_contract" , "CONTRACT") ;
      ("sender" , "SENDER") ;
      ("unit" , "UNIT") ;
      ("source" , "SOURCE") ;
    ]

    let type_constants = type_constants
  end

  module Ligodity = struct
    let constants = [
      ("assert" , "ASSERT") ;

      ("Current.balance", "BALANCE") ;
      ("balance", "BALANCE") ;
      ("Current.time", "NOW") ;
      ("time", "NOW") ;
      ("Current.amount" , "AMOUNT") ;
      ("amount", "AMOUNT") ;
      ("Current.gas", "STEPS_TO_QUOTA") ;
      ("gas", "STEPS_TO_QUOTA") ;
      ("Current.sender" , "SENDER") ;
      ("sender", "SENDER") ;
      ("Current.source" , "SOURCE") ;
      ("source", "SOURCE") ;
      ("Current.failwith", "FAILWITH") ;
      ("failwith" , "FAILWITH") ;

      ("Crypto.hash" , "HASH") ;
      ("Crypto.black2b", "BLAKE2B") ;
      ("Crypto.sha256", "SHA256") ;
      ("Crypto.sha512", "SHA512") ;
      ("Crypto.hash_key", "HASH_KEY") ;
      ("Crypto.check", "CHECK_SIGNATURE") ;

      ("Bytes.pack" , "PACK") ;
      ("Bytes.unpack", "UNPACK") ;
      ("Bytes.length", "SIZE") ;
      ("Bytes.size" , "SIZE") ;
      ("Bytes.concat", "CONCAT") ;
      ("Bytes.slice", "SLICE") ;
      ("Bytes.sub", "SLICE") ;

      ("Set.mem" , "SET_MEM") ;
      ("Set.empty" , "SET_EMPTY") ;
      ("Set.add" , "SET_ADD") ;
      ("Set.remove" , "SET_REMOVE") ;

      ("Map.find_opt" , "MAP_FIND_OPT") ;
      ("Map.find" , "MAP_FIND") ;
      ("Map.update" , "MAP_UPDATE") ;
      ("Map.add" , "MAP_ADD") ;
      ("Map.remove" , "MAP_REMOVE") ;

      ("String.length", "SIZE") ;
      ("String.size", "SIZE") ;
      ("String.slice", "SLICE") ;
      ("String.sub", "SLICE") ;
      ("String.concat", "CONCAT") ;

      ("List.length", "SIZE") ;
      ("List.size", "SIZE") ;
      ("List.iter", "ITER") ;

      ("Operation.transaction" , "CALL") ;
      ("Operation.get_contract" , "CONTRACT") ;
      ("int" , "INT") ;
      ("abs" , "ABS") ;
      ("unit" , "UNIT") ;
      ("source" , "SOURCE") ;
    ]

    let type_constants = type_constants
  end

end

module Typer = struct
  (*
    Each constant has its own type.

    LIGO's type-system is currently too
    weak to express the constant's type. For instance:
    - "ADD" has a special kind of type of polymorphism. If "ADD" gets two `int`s,
      it will return an `int`. If it gets two `nat`s, it will return a `nat`.
      Regular polymorphism wouldn't work because "ADD" only accepts `int`s or
      `nat`s.
    - "NONE" (from Some/None) requires an annotation.

    Instead of a LIGO type, constant types are representend as functions. These
    functions take as parameters:
    - The list of types of the arguments of the constants. When typing `2 + 2`,
      the types might be `[ int ; int ]`.
    - The expected type of the whole expression. It is optional. When typing
      `[] : list(operation)`, it will be `Some ( list (operation) )`. When
      typing `2 + 2` (with no additional context), it will be `None`.
    The output is the type of the whole expression. An error is returned through
    the Trace monad if it doesn't type-check (`"toto" + 42`).

    Various helpers are defined and explaines in `Helpers.Typer`.
  *)

  open Helpers.Typer
  open Ast_typed

  let none = typer_0 "NONE" @@ fun tv_opt ->
    match tv_opt with
    | None -> simple_fail "untyped NONE"
    | Some t -> ok t

  let set_empty = typer_0 "SET_EMPTY" @@ fun tv_opt ->
    match tv_opt with
    | None -> simple_fail "untyped SET_EMPTY"
    | Some t -> ok t

  let sub = typer_2 "SUB" @@ fun a b ->
    if (eq_2 (a , b) (t_int ()))
    then ok @@ t_int () else
    if (eq_2 (a , b) (t_nat ()))
    then ok @@ t_int () else
    if (eq_2 (a , b) (t_timestamp ()))
    then ok @@ t_int () else
    if (eq_1 a (t_timestamp ()) && eq_1 b (t_int ()))
    then ok @@ t_timestamp () else
    if (eq_2 (a , b) (t_tez ()))
    then ok @@ t_tez () else
      fail (simple_error "Typing substraction, bad parameters.")

  let some = typer_1 "SOME" @@ fun a -> ok @@ t_option a ()

  let map_remove : typer = typer_2 "MAP_REMOVE" @@ fun k m ->
    let%bind (src , _) = get_t_map m in
    let%bind () = assert_type_value_eq (src , k) in
    ok m

  let map_add : typer = typer_3 "MAP_ADD" @@ fun k v m ->
    let%bind (src, dst) = get_t_map m in
    let%bind () = assert_type_value_eq (src, k) in
    let%bind () = assert_type_value_eq (dst, v) in
    ok m

  let map_update : typer = typer_3 "MAP_UPDATE" @@ fun k v m ->
    let%bind (src, dst) = get_t_map m in
    let%bind () = assert_type_value_eq (src, k) in
    let%bind v' = get_t_option v in
    let%bind () = assert_type_value_eq (dst, v') in
    ok m

  let map_mem : typer = typer_2 "MAP_MEM" @@ fun k m ->
    let%bind (src, _dst) = get_t_map m in
    let%bind () = assert_type_value_eq (src, k) in
    ok @@ t_bool ()

  let map_find : typer = typer_2 "MAP_FIND" @@ fun k m ->
    let%bind (src, dst) = get_t_map m in
    let%bind () = assert_type_value_eq (src, k) in
    ok @@ dst

  let map_find_opt : typer = typer_2 "MAP_FIND_OPT" @@ fun k m ->
    let%bind (src, dst) = get_t_map m in
    let%bind () = assert_type_value_eq (src, k) in
    ok @@ t_option dst ()

  let map_iter : typer = typer_2 "MAP_ITER" @@ fun m f ->
    let%bind (k, v) = get_t_map m in
    let%bind (arg , res) = get_t_function f in
    let%bind () = assert_eq_1 arg (t_pair k v ()) in
    let%bind () = assert_eq_1 res (t_unit ()) in
    ok @@ t_unit ()

  let map_map : typer = typer_2 "MAP_MAP" @@ fun m f ->
    let%bind (k, v) = get_t_map m in
    let%bind (arg , res) = get_t_function f in
    let%bind () = assert_eq_1 arg (t_pair k v ()) in
    ok @@ t_map k res ()

  let map_fold : typer = typer_2 "MAP_FOLD" @@ fun f m ->
    let%bind (k, v) = get_t_map m in
    let%bind (arg_1 , res) = get_t_function f in
    let%bind (arg_2 , res') = get_t_function res in
    let%bind (arg_3 , res'') = get_t_function res' in
    let%bind () = assert_eq_1 arg_1 k in
    let%bind () = assert_eq_1 arg_2 v in
    let%bind () = assert_eq_1 arg_3 res'' in
    ok @@ res'

  let big_map_remove : typer = typer_2 "BIG_MAP_REMOVE" @@ fun k m ->
    let%bind (src , _) = get_t_big_map m in
    let%bind () = assert_type_value_eq (src , k) in
    ok m

  let big_map_add : typer = typer_3 "BIG_MAP_ADD" @@ fun k v m ->
    let%bind (src, dst) = get_t_big_map m in
    let%bind () = assert_type_value_eq (src, k) in
    let%bind () = assert_type_value_eq (dst, v) in
    ok m

  let big_map_update : typer = typer_3 "BIG_MAP_UPDATE" @@ fun k v m ->
    let%bind (src, dst) = get_t_big_map m in
    let%bind () = assert_type_value_eq (src, k) in
    let%bind v' = get_t_option v in
    let%bind () = assert_type_value_eq (dst, v') in
    ok m

  let big_map_mem : typer = typer_2 "BIG_MAP_MEM" @@ fun k m ->
    let%bind (src, _dst) = get_t_big_map m in
    let%bind () = assert_type_value_eq (src, k) in
    ok @@ t_bool ()

  let big_map_find : typer = typer_2 "BIG_MAP_FIND" @@ fun k m ->
    let%bind (src, dst) = get_t_big_map m in
    let%bind () = assert_type_value_eq (src, k) in
    ok @@ dst


  let size = typer_1 "SIZE" @@ fun t ->
    let%bind () =
      Assert.assert_true @@
      (is_t_map t || is_t_list t || is_t_string t || is_t_bytes t || is_t_set t || is_t_big_map t) in
    ok @@ t_nat ()

  let slice = typer_3 "SLICE" @@ fun i j s ->
    let%bind () = assert_eq_1 i (t_nat ()) in
    let%bind () = assert_eq_1 j (t_nat ()) in
    if eq_1 s (t_string ())
    then ok @@ t_string ()
    else if eq_1 s (t_bytes ())
    then ok @@ t_bytes ()
    else simple_fail "bad slice"

  let failwith_ = typer_1 "FAILWITH" @@ fun t ->
    let%bind () =
      Assert.assert_true @@
      (is_t_string t) in
    ok @@ t_unit ()

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

  let hash256 = typer_1 "SHA256" @@ fun t ->
    let%bind () = assert_t_bytes t in
    ok @@ t_bytes ()

  let hash512 = typer_1 "SHA512" @@ fun t ->
    let%bind () = assert_t_bytes t in
    ok @@ t_bytes ()

  let blake2b = typer_1 "BLAKE2b" @@ fun t ->
    let%bind () = assert_t_bytes t in
    ok @@ t_bytes ()

  let hash_key = typer_1 "HASH_KEY" @@ fun t ->
    let%bind () = assert_t_key t in
    ok @@ t_key_hash ()

  let check_signature = typer_3 "CHECK_SIGNATURE" @@ fun k s b ->
    let%bind () = assert_t_key k in
    let%bind () = assert_t_signature s in
    let%bind () = assert_t_bytes b in
    ok @@ t_bool ()

  let sender = constant "SENDER" @@ t_address ()

  let source = constant "SOURCE" @@ t_address ()

  let unit = constant "UNIT" @@ t_unit ()

  let amount = constant "AMOUNT" @@ t_tez ()

  let balance = constant "BALANCE" @@ t_tez ()

  let address = constant "ADDRESS" @@ t_address ()

  let now = constant "NOW" @@ t_timestamp ()

  let transaction = typer_3 "CALL" @@ fun param amount contract ->
    let%bind () = assert_t_tez amount in
    let%bind contract_param = get_t_contract contract in
    let%bind () = assert_type_value_eq (param , contract_param) in
    ok @@ t_operation ()

  let originate = typer_6 "ORIGINATE" @@ fun manager delegate_opt spendable delegatable init_balance code ->
    let%bind () = assert_eq_1 manager (t_key_hash ()) in
    let%bind () = assert_eq_1 delegate_opt (t_option (t_key_hash ()) ()) in
    let%bind () = assert_eq_1 spendable (t_bool ()) in
    let%bind () = assert_eq_1 delegatable (t_bool ()) in
    let%bind () = assert_t_tez init_balance in
    let%bind (arg , res) = get_t_function code in
    let%bind (_param , storage) = get_t_pair arg in
    let%bind (storage' , op_lst) = get_t_pair res in
    let%bind () = assert_eq_1 storage storage' in
    let%bind () = assert_eq_1 op_lst (t_list (t_operation ()) ()) in
    ok @@ (t_pair (t_operation ()) (t_address ()) ())

  let get_contract = typer_1_opt "CONTRACT" @@ fun _ tv_opt ->
    let%bind tv =
      trace_option (simple_error "get_contract needs a type annotation") tv_opt in
    let%bind tv' =
      trace_strong (simple_error "get_contract has a not-contract annotation") @@
      get_t_contract tv in
    ok @@ t_contract tv' ()

  let set_delegate = typer_1 "SET_DELEGATE" @@ fun delegate_opt ->
    let%bind () = assert_eq_1 delegate_opt (t_option (t_key_hash ()) ()) in
    ok @@ t_operation ()

  let abs = typer_1 "ABS" @@ fun t ->
    let%bind () = assert_t_int t in
    ok @@ t_nat ()

  let neg = typer_1 "NEG" @@ fun t ->
    let%bind () = Assert.assert_true (eq_1 t (t_nat ()) || eq_1 t (t_int ())) in
    ok @@ t_int ()

  let assertion = typer_1 "ASSERT" @@ fun a ->
    if eq_1 a (t_bool ())
    then ok @@ t_unit ()
    else simple_fail "Asserting a non-bool"

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
    if eq_1 a (t_tez ()) && eq_1 b (t_nat ())
    then ok @@ t_tez () else
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
    if eq_2 (a , b) (t_tez ())
    then ok @@ t_tez () else
    if (eq_1 a (t_nat ()) && eq_1 b (t_int ())) || (eq_1 b (t_nat ()) && eq_1 a (t_int ()))
    then ok @@ t_int () else
    if (eq_1 a (t_timestamp ()) && eq_1 b (t_int ())) || (eq_1 b (t_timestamp ()) && eq_1 a (t_int ()))
    then ok @@ t_timestamp () else
      simple_fail "Adding with wrong types. Expected nat, int or tez."

  let set_mem = typer_2 "SET_MEM" @@ fun elt set ->
    let%bind key = get_t_set set in
    if eq_1 elt key
    then ok @@ t_bool ()
    else simple_fail "Set_mem: elt and set don't match"

  let set_add = typer_2 "SET_ADD" @@ fun elt set ->
    let%bind key = get_t_set set in
    if eq_1 elt key
    then ok set
    else simple_fail "Set_add: elt and set don't match"

  let set_remove = typer_2 "SET_REMOVE" @@ fun elt set ->
    let%bind key = get_t_set set in
    if eq_1 elt key
    then ok set
    else simple_fail "Set_remove: elt and set don't match"

  let set_iter = typer_2 "SET_ITER" @@ fun set body ->
    let%bind (arg , res) = get_t_function body in
    let%bind () = Assert.assert_true (eq_1 res (t_unit ())) in
    let%bind key = get_t_set set in
    if eq_1 key arg
    then ok (t_unit ())
    else simple_fail "bad set iter"

  let list_iter = typer_2 "LIST_ITER" @@ fun lst body ->
    let%bind (arg , res) = get_t_function body in
    let%bind () = Assert.assert_true (eq_1 res (t_unit ())) in
    let%bind key = get_t_list lst in
    if eq_1 key arg
    then ok (t_unit ())
    else simple_fail "bad list iter"

  let list_map = typer_2 "LIST_MAP" @@ fun lst body ->
    let%bind (arg , res) = get_t_function body in
    let%bind key = get_t_list lst in
    if eq_1 key arg
    then ok (t_list res ())
    else simple_fail "bad list iter"

  let not_ = typer_1 "NOT" @@ fun elt ->
    if eq_1 elt (t_bool ())
    then ok @@ t_bool ()
    else if eq_1 elt (t_nat ()) || eq_1 elt (t_int ())
    then ok @@ t_int ()
    else simple_fail "bad parameter to not"

  let or_ = typer_2 "OR" @@ fun a b ->
    if eq_2 (a , b) (t_bool ())
    then ok @@ t_bool ()
    else if eq_2 (a , b) (t_nat ())
    then ok @@ t_nat ()
    else simple_fail "bad or"

  let xor = typer_2 "XOR" @@ fun a b ->
    if eq_2 (a , b) (t_bool ())
    then ok @@ t_bool ()
    else if eq_2 (a , b) (t_nat ())
    then ok @@ t_nat ()
    else simple_fail "bad xor"

  let and_ = typer_2 "AND" @@ fun a b ->
    if eq_2 (a , b) (t_bool ())
    then ok @@ t_bool ()
    else if eq_2 (a , b) (t_nat ()) || (eq_1 b (t_nat ()) && eq_1 a (t_int ()))
    then ok @@ t_nat ()
    else simple_fail "bad end"

  let lsl_ = typer_2 "LSL" @@ fun a b ->
    if eq_2 (a , b) (t_nat ())
    then ok @@ t_nat ()
    else simple_fail "bad lsl"

  let lsr_ = typer_2 "LSR" @@ fun a b ->
    if eq_2 (a , b) (t_nat ())
    then ok @@ t_nat ()
    else simple_fail "bad lsr"

  let concat = typer_2 "CONCAT" @@ fun a b ->
    if eq_2 (a , b) (t_string ())
    then ok @@ t_string ()
    else if eq_2 (a , b) (t_bytes ())
    then ok @@ t_bytes ()
    else simple_fail "bad concat"

  let cons = typer_2 "CONS" @@ fun hd tl ->
    let%bind elt = get_t_list tl in
    let%bind () = assert_eq_1 hd elt in
    ok tl

  let constant_typers = Map.String.of_list [
      add ;
      times ;
      div ;
      mod_ ;
      sub ;
      none ;
      some ;
      concat ;
      slice ;
      comparator "EQ" ;
      comparator "NEQ" ;
      comparator "LT" ;
      comparator "GT" ;
      comparator "LE" ;
      comparator "GE" ;
      or_ ;
      and_ ;
      xor ;
      not_ ;
      map_remove ;
      map_add ;
      map_update ;
      map_mem ;
      map_find ;
      map_map ;
      map_fold ;
      map_iter ;
      map_map ;
      set_empty ;
      set_mem ;
      set_add ;
      set_remove ;
      set_iter ;
      list_iter ;
      list_map ;
      int ;
      size ;
      failwith_ ;
      get_force ;
      bytes_pack ;
      bytes_unpack ;
      hash256 ;
      hash512 ;
      blake2b ;
      hash_key ;
      check_signature ;
      sender ;
      source ;
      unit ;
      amount ;
      transaction ;
      get_contract ;
      neg ;
      abs ;
      now ;
      slice ;
      address ;
      assertion ;
    ]

end

module Compiler = struct
  (*
    Most constants pass through the Transpiler unchanged. So they need to be
    compiled down to Michelson. This is the last step.

    When compiling the constant, we need to provide its arity (through the type
    predicate, defined in `Helpers.Compiler`, and its michelson code.
    In the case of an n-ary constant, we assume that the stack has the form:
    `x1 :: x2 :: x3 ... :: xn :: _`.

    This step requires knowledge of Michelson. Knowledge of
    `Tezos_utils.Michelson` will help too, so that no Michelson has to actually
    be written by hand.
  *)

  include Helpers.Compiler
  open Tezos_utils.Michelson

  let predicates = Map.String.of_list [
    ("ADD" , simple_binary @@ prim I_ADD) ;
    ("SUB" , simple_binary @@ prim I_SUB) ;
    ("TIMES" , simple_binary @@ prim I_MUL) ;
    ("DIV" , simple_binary @@ seq [prim I_EDIV ; i_assert_some_msg (i_push_string "DIV by 0") ; i_car]) ;
    ("MOD" , simple_binary @@ seq [prim I_EDIV ; i_assert_some_msg (i_push_string "MOD by 0") ; i_cdr]) ;
    ("NEG" , simple_unary @@ prim I_NEG) ;
    ("OR" , simple_binary @@ prim I_OR) ;
    ("AND" , simple_binary @@ prim I_AND) ;
    ("XOR" , simple_binary @@ prim I_XOR) ;
    ("NOT" , simple_unary @@ prim I_NOT) ;
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
    ("MAP_FIND" , simple_binary @@ seq [prim I_GET ; i_assert_some_msg (i_push_string "MAP FIND")]) ;
    ("MAP_GET" , simple_binary @@ prim I_GET) ;
    ("SIZE" , simple_unary @@ prim I_SIZE) ;
    ("FAILWITH" , simple_unary @@ prim I_FAILWITH) ;
    ("ASSERT_INFERRED" , simple_binary @@ i_if (seq [i_failwith]) (seq [i_drop ; i_push_unit])) ;
    ("ASSERT" , simple_unary @@ i_if (seq [i_push_unit ; i_failwith]) (seq [i_push_unit])) ;
    ("INT" , simple_unary @@ prim I_INT) ;
    ("ABS" , simple_unary @@ prim I_ABS) ;
    ("CONS" , simple_binary @@ prim I_CONS) ;
    ("UNIT" , simple_constant @@ prim I_UNIT) ;
    ("AMOUNT" , simple_constant @@ prim I_AMOUNT) ;
    ("ADDRESS" , simple_constant @@ prim I_ADDRESS) ;
    ("NOW" , simple_constant @@ prim I_NOW) ;
    ("CALL" , simple_ternary @@ prim I_TRANSFER_TOKENS) ;
    ("SOURCE" , simple_constant @@ prim I_SOURCE) ;
    ("SENDER" , simple_constant @@ prim I_SENDER) ;
    ("MAP_ADD" , simple_ternary @@ seq [dip (i_some) ; prim I_UPDATE]) ;
    ("MAP_UPDATE" , simple_ternary @@ prim I_UPDATE) ;
    ("SET_MEM" , simple_binary @@ prim I_MEM) ;
    ("SET_ADD" , simple_binary @@ seq [dip (i_push (prim T_bool) (prim D_True)) ; prim I_UPDATE]) ;
    ("SET_REMOVE" , simple_binary @@ seq [dip (i_push (prim T_bool) (prim D_False)) ; prim I_UPDATE]) ;
    ("SLICE" , simple_ternary @@ seq [prim I_SLICE ; i_assert_some_msg (i_push_string "SLICE")]) ;
    ("SHA256" , simple_unary @@ prim I_SHA256) ;
    ("SHA512" , simple_unary @@ prim I_SHA512) ;
    ("BLAKE2B" , simple_unary @@ prim I_BLAKE2B) ;
    ("CHECK_SIGNATURE" , simple_ternary @@ prim I_CHECK_SIGNATURE) ;
    ("HASH_KEY" , simple_unary @@ prim I_HASH_KEY) ;
    ("PACK" , simple_unary @@ prim I_PACK) ;
    ("CONCAT" , simple_binary @@ prim I_CONCAT) ;
  ]

  (* Some complex predicates will need to be added in compiler/compiler_program *)

end
