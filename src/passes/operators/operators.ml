open Trace

(*
  This file is used throughout the pipeline. Its idea is to add a unique place
  that you have to modify when you add a new operator/constant to the language.

  This file mirrors the LIGO pipeline, starting with Simplify, then Typer and
  ending with Compiler. Usually, when adding a new operator, you'll have to add
  a new constructor at all those places.
*)

module Simplify = struct

  open Ast_simplified
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
  let unit_expr = make_t @@ T_constant TC_unit 

  let type_constants s =
  match s with
    | "chain_id"  -> ok TC_chain_id
    | "unit"      -> ok TC_unit
    | "string"    -> ok TC_string
    | "bytes"     -> ok TC_bytes
    | "nat"       -> ok TC_nat
    | "int"       -> ok TC_int
    | "tez"       -> ok TC_mutez
    | "bool"      -> ok TC_bool
    | "operation" -> ok TC_operation
    | "address"   -> ok TC_address
    | "key"       -> ok TC_key
    | "key_hash"  -> ok TC_key_hash
    | "signature" -> ok TC_signature
    | "timestamp" -> ok TC_timestamp
    | _           -> simple_fail @@ "Not a type_constant " ^ s

  let type_operators s =
  match s with
    | "list"      -> ok @@ TC_list unit_expr
    | "option"    -> ok @@ TC_option unit_expr
    | "set"       -> ok @@ TC_set unit_expr
    | "map"       -> ok @@ TC_map (unit_expr,unit_expr)
    | "big_map"   -> ok @@ TC_big_map (unit_expr,unit_expr)
    | "contract"  -> ok @@ TC_contract unit_expr
    | _           -> simple_fail @@ "Not a typ_operator " ^ s


  module Pascaligo = struct

    let constants = function
      | "get_force"       -> ok C_MAP_GET_FORCE
      | "get_chain_id"    -> ok C_CHAIN_ID
      | "transaction"     -> ok C_CALL
      | "get_contract"    -> ok C_CONTRACT
      | "get_entrypoint"  -> ok C_CONTRACT_ENTRYPOINT
      | "size"            -> ok C_SIZE
      | "int"             -> ok C_INT
      | "abs"             -> ok C_ABS
      | "is_nat"          -> ok C_IS_NAT
      | "amount"          -> ok C_AMOUNT
      | "balance"         -> ok C_BALANCE
      | "now"             -> ok C_NOW
      | "unit"            -> ok C_UNIT
      | "source"          -> ok C_SOURCE
      | "sender"          -> ok C_SENDER
      | "failwith"        -> ok C_FAILWITH
      | "bitwise_or"      -> ok C_OR
      | "bitwise_and"     -> ok C_AND
      | "bitwise_xor"     -> ok C_XOR
      | "string_concat"   -> ok C_CONCAT
      | "string_slice"    -> ok C_SLICE
      | "crypto_check"    -> ok C_CHECK_SIGNATURE
      | "crypto_hash_key" -> ok C_HASH_KEY
      | "bytes_concat"    -> ok C_CONCAT
      | "bytes_slice"     -> ok C_SLICE
      | "bytes_pack"      -> ok C_BYTES_PACK
      | "bytes_unpack"    -> ok C_BYTES_UNPACK
      | "set_empty"       -> ok C_SET_EMPTY
      | "set_mem"         -> ok C_SET_MEM
      | "set_add"         -> ok C_SET_ADD
      | "set_remove"      -> ok C_SET_REMOVE
      | "set_iter"        -> ok C_SET_ITER
      | "set_fold"        -> ok C_SET_FOLD
      | "list_iter"       -> ok C_LIST_ITER
      | "list_fold"       -> ok C_LIST_FOLD
      | "list_map"        -> ok C_LIST_MAP
      | "map_iter"        -> ok C_MAP_ITER
      | "map_map"         -> ok C_MAP_MAP
      | "map_fold"        -> ok C_MAP_FOLD
      | "map_remove"      -> ok C_MAP_REMOVE
      | "map_update"      -> ok C_MAP_UPDATE
      | "map_get"         -> ok C_MAP_GET
      | "map_mem"         -> ok C_MAP_MEM
      | "sha_256"         -> ok C_SHA256
      | "sha_512"         -> ok C_SHA512
      | "blake2b"         -> ok C_BLAKE2b
      | "cons"            -> ok C_CONS
      | "EQ"              -> ok C_EQ
      | "NEQ"             -> ok C_NEQ
      | "NEG"             -> ok C_NEG
      | "ADD"             -> ok C_ADD
      | "SUB"             -> ok C_SUB
      | "TIMES"           -> ok C_MUL
      | "DIV"             -> ok C_DIV
      | "MOD"             -> ok C_MOD
      | "NOT"             -> ok C_NOT
      | "AND"             -> ok C_AND
      | "OR"              -> ok C_OR
      | "GT"              -> ok C_GT
      | "GE"              -> ok C_GE
      | "LT"              -> ok C_LT
      | "LE"              -> ok C_LE
      | "CONS"            -> ok C_CONS
      | "address"         -> ok C_ADDRESS
      | "self_address"    -> ok C_SELF_ADDRESS
      | "implicit_account"-> ok C_IMPLICIT_ACCOUNT
      | "set_delegate"    -> ok C_SET_DELEGATE
      | _                 -> simple_fail "Not a PascaLIGO constant"

    let type_constants = type_constants
    let type_operators = type_operators
  end


  module Cameligo = struct
    let constants = function
      | "assert"                   -> ok C_ASSERTION
      | "Current.balance"          -> ok C_BALANCE
      | "balance"                  -> ok C_BALANCE
      | "Current.time"             -> ok C_NOW
      | "time"                     -> ok C_NOW
      | "Current.amount"           -> ok C_AMOUNT
      | "amount"                   -> ok C_AMOUNT
      | "Current.gas"              -> ok C_STEPS_TO_QUOTA
      | "gas"                      -> ok C_STEPS_TO_QUOTA
      | "Current.sender"           -> ok C_SENDER
      | "Current.address"          -> ok C_ADDRESS
      | "Current.self_address"     -> ok C_SELF_ADDRESS
      | "Current.implicit_account" -> ok C_IMPLICIT_ACCOUNT
      | "sender"                   -> ok C_SENDER
      | "Current.source"           -> ok C_SOURCE
      | "source"                   -> ok C_SOURCE
      | "Current.failwith"         -> ok C_FAILWITH
      | "failwith"                 -> ok C_FAILWITH

      | "Crypto.hash"              -> ok C_HASH
      | "Crypto.black2b"           -> ok C_BLAKE2b
      | "Crypto.sha256"            -> ok C_SHA256
      | "Crypto.sha512"            -> ok C_SHA512
      | "Crypto.hash_key"          -> ok C_HASH_KEY
      | "Crypto.check"             -> ok C_CHECK_SIGNATURE

      | "Bytes.pack"               -> ok C_BYTES_PACK
      | "Bytes.unpack"             -> ok C_BYTES_UNPACK
      | "Bytes.length"             -> ok C_SIZE
      | "Bytes.size"               -> ok C_SIZE
      | "Bytes.concat"             -> ok C_CONCAT
      | "Bytes.slice"              -> ok C_SLICE
      | "Bytes.sub"                -> ok C_SLICE

      | "Set.mem"                  -> ok C_SET_MEM
      | "Set.empty"                -> ok C_SET_EMPTY
      | "Set.literal"              -> ok C_SET_LITERAL
      | "Set.add"                  -> ok C_SET_ADD
      | "Set.remove"               -> ok C_SET_REMOVE
      | "Set.fold"                 -> ok C_SET_FOLD
      | "Set.size"                 -> ok C_SIZE

      | "Map.find_opt"             -> ok C_MAP_FIND_OPT
      | "Map.find"                 -> ok C_MAP_FIND
      | "Map.update"               -> ok C_MAP_UPDATE
      | "Map.add"                  -> ok C_MAP_ADD
      | "Map.remove"               -> ok C_MAP_REMOVE
      | "Map.iter"                 -> ok C_MAP_ITER
      | "Map.map"                  -> ok C_MAP_MAP
      | "Map.fold"                 -> ok C_MAP_FOLD
      | "Map.mem"                  -> ok C_MAP_MEM
      | "Map.empty"                -> ok C_MAP_EMPTY
      | "Map.literal"              -> ok C_MAP_LITERAL
      | "Map.size"                 -> ok C_SIZE

      | "Big_map.find_opt"         -> ok C_MAP_FIND_OPT
      | "Big_map.find"             -> ok C_MAP_FIND
      | "Big_map.update"           -> ok C_MAP_UPDATE
      | "Big_map.add"              -> ok C_MAP_ADD
      | "Big_map.remove"           -> ok C_MAP_REMOVE
      | "Big_map.literal"          -> ok C_BIG_MAP_LITERAL
      | "Big_map.empty"            -> ok C_BIG_MAP_EMPTY

      | "Bitwise.lor"              -> ok C_OR
      | "Bitwise.land"             -> ok C_AND
      | "Bitwise.lxor"             -> ok C_XOR

      | "String.length"            -> ok C_SIZE
      | "String.size"              -> ok C_SIZE
      | "String.slice"             -> ok C_SLICE
      | "String.sub"               -> ok C_SLICE
      | "String.concat"            -> ok C_CONCAT

      | "List.length"              -> ok C_SIZE
      | "List.size"                -> ok C_SIZE
      | "List.iter"                -> ok C_LIST_ITER
      | "List.map"                 -> ok C_LIST_MAP
      | "List.fold"                -> ok C_LIST_FOLD

      | "Loop.fold_while"          -> ok C_FOLD_WHILE
      | "continue"                 -> ok C_CONTINUE
      | "stop"                     -> ok C_STOP

      | "Operation.transaction"    -> ok C_CALL
      | "Operation.set_delegate"   -> ok C_SET_DELEGATE
      | "Operation.get_contract"   -> ok C_CONTRACT
      | "Operation.get_entrypoint" -> ok C_CONTRACT_ENTRYPOINT
      | "int"                      -> ok C_INT
      | "abs"                      -> ok C_ABS
      | "unit"                     -> ok C_UNIT

      | "NEG"                      -> ok C_NEG
      | "ADD"                      -> ok C_ADD
      | "SUB"                      -> ok C_SUB
      | "TIMES"                    -> ok C_MUL
      | "DIV"                      -> ok C_DIV
      | "MOD"                      -> ok C_MOD
      | "EQ"                       -> ok C_EQ
      | "NOT"                      -> ok C_NOT
      | "AND"                      -> ok C_AND
      | "OR"                       -> ok C_OR
      | "GT"                       -> ok C_GT
      | "GE"                       -> ok C_GE
      | "LT"                       -> ok C_LT
      | "LE"                       -> ok C_LE
      | "CONS"                     -> ok C_CONS
      | "NEQ"                      -> ok C_NEQ

      | "Michelson.is_nat"         -> ok C_IS_NAT
      | _                          -> simple_fail "Not a constant"

    let type_constants = type_constants
    let type_operators = type_operators
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

  module Operators_types = struct
    open Typesystem.Shorthands

    let tc_subarg   a b c = tc [a;b;c] [ (*TODO…*) ]
    let tc_sizearg  a     = tc [a]     [ [int] ]
    let tc_packable a     = tc [a]     [ [int] ; [string] ; [bool] (*TODO…*) ]
    let tc_timargs  a b c = tc [a;b;c] [ [nat;nat;nat] ; [int;int;int] (*TODO…*) ]
    let tc_divargs  a b c = tc [a;b;c] [ (*TODO…*) ]
    let tc_modargs  a b c = tc [a;b;c] [ (*TODO…*) ]
    let tc_addargs  a b c = tc [a;b;c] [ (*TODO…*) ]

    let t_none         = forall "a" @@ fun a -> option a
    let t_sub          = forall3_tc "a" "b" "c" @@ fun a b c -> [tc_subarg a b c] => a --> b --> c (* TYPECLASS *)
    let t_some         = forall "a" @@ fun a -> a --> option a
    let t_map_remove   = forall2 "src" "dst" @@ fun src dst -> src --> map src dst --> map src dst
    let t_map_add      = forall2 "src" "dst" @@ fun src dst -> src --> dst --> map src dst --> map src dst
    let t_map_update   = forall2 "src" "dst" @@ fun src dst -> src --> option dst --> map src dst --> map src dst
    let t_map_mem      = forall2 "src" "dst" @@ fun src dst -> src --> map src dst --> bool
    let t_map_find     = forall2 "src" "dst" @@ fun src dst -> src --> map src dst --> dst
    let t_map_find_opt = forall2 "src" "dst" @@ fun src dst -> src --> map src dst --> option dst
    let t_map_fold     = forall3 "src" "dst" "acc" @@ fun src dst acc -> ( ( (src * dst) * acc ) --> acc ) --> map src dst --> acc --> acc
    let t_map_map      = forall3 "k" "v" "result" @@ fun k v result -> ((k * v) --> result) --> map k v --> map k result

    (* TODO: the type of map_map_fold might be wrong, check it. *)
    let t_map_map_fold = forall4 "k" "v" "acc" "dst" @@ fun k v acc dst -> ( ((k * v) * acc) --> acc * dst ) --> map k v --> (k * v) --> (map k dst * acc)
    let t_map_iter     = forall2 "k" "v" @@ fun k v -> ( (k * v) --> unit ) --> map k v --> unit
    let t_size         = forall_tc "c" @@ fun c -> [tc_sizearg c] => c --> nat (* TYPECLASS *)
    let t_slice        = nat --> nat --> string --> string
    let t_failwith     = string --> unit
    let t_get_force    = forall2 "src" "dst" @@ fun src dst -> src --> map src dst --> dst
    let t_int          = nat --> int
    let t_bytes_pack   = forall_tc "a" @@ fun a -> [tc_packable a] => a --> bytes (* TYPECLASS *)
    let t_bytes_unpack = forall_tc "a" @@ fun a -> [tc_packable a] => bytes --> a (* TYPECLASS *)
    let t_hash256      = bytes --> bytes
    let t_hash512      = bytes --> bytes
    let t_blake2b      = bytes --> bytes
    let t_hash_key     = key --> key_hash
    let t_check_signature = key --> signature --> bytes --> bool
    let t_sender       = address
    let t_source       = address
    let t_unit         = unit
    let t_amount       = mutez
    let t_address      = address
    let t_now          = timestamp
    let t_transaction  = forall "a" @@ fun a -> a --> mutez --> contract a --> operation
    let t_get_contract = forall "a" @@ fun a -> contract a
    let t_abs          = int --> nat
    let t_cons         = forall "a" @@ fun a -> a --> list a --> list a
    let t_assertion    = bool --> unit
    let t_times        = forall3_tc "a" "b" "c" @@ fun a b c -> [tc_timargs a b c] => a --> b --> c (* TYPECLASS *)
    let t_div          = forall3_tc "a" "b" "c" @@ fun a b c -> [tc_divargs a b c] => a --> b --> c (* TYPECLASS *)
    let t_mod          = forall3_tc "a" "b" "c" @@ fun a b c -> [tc_modargs a b c] => a --> b --> c (* TYPECLASS *)
    let t_add          = forall3_tc "a" "b" "c" @@ fun a b c -> [tc_addargs a b c] => a --> b --> c (* TYPECLASS *)
    let t_set_mem      = forall "a" @@ fun a -> a --> set a --> bool
    let t_set_add      = forall "a" @@ fun a -> a --> set a --> set a
    let t_set_remove   = forall "a" @@ fun a -> a --> set a --> set a
    let t_not          = bool --> bool
  end

  let none = typer_0 "NONE" @@ fun tv_opt ->
    match tv_opt with
    | None -> simple_fail "untyped NONE"
    | Some t -> ok t

  let set_empty = typer_0 "SET_EMPTY" @@ fun tv_opt ->
    match tv_opt with
    | None -> simple_fail "untyped SET_EMPTY"
    | Some t -> ok t

  let sub = typer_2 "SUB" @@ fun a b ->
    if (eq_1 a (t_int ()) || eq_1 a (t_nat ()))
    && (eq_1 b (t_int ()) || eq_1 b (t_nat ()))
    then ok @@ t_int () else
    if (eq_2 (a , b) (t_timestamp ()))
    then ok @@ t_int () else
    if (eq_1 a (t_timestamp ()) && eq_1 b (t_int ()))
    then ok @@ t_timestamp () else
    if (eq_2 (a , b) (t_mutez ()))
    then ok @@ t_mutez () else
      fail (simple_error "Typing substraction, bad parameters.")

  let some = typer_1 "SOME" @@ fun a -> ok @@ t_option a ()

  let list_cons : typer = typer_2 "CONS" @@ fun hd tl ->
    let%bind tl' = get_t_list tl in
    let%bind () = assert_type_value_eq (hd , tl') in
    ok tl

  let map_remove : typer = typer_2 "MAP_REMOVE" @@ fun k m ->
    let%bind (src , _) = bind_map_or (get_t_map , get_t_big_map) m in
    let%bind () = assert_type_value_eq (src , k) in
    ok m

  let map_add : typer = typer_3 "MAP_ADD" @@ fun k v m ->
    let%bind (src, dst) = bind_map_or (get_t_map , get_t_big_map) m in
    let%bind () = assert_type_value_eq (src, k) in
    let%bind () = assert_type_value_eq (dst, v) in
    ok m

  let map_update : typer = typer_3 "MAP_UPDATE" @@ fun k v m ->
    let%bind (src, dst) = bind_map_or (get_t_map , get_t_big_map) m in
    let%bind () = assert_type_value_eq (src, k) in
    let%bind v' = get_t_option v in
    let%bind () = assert_type_value_eq (dst, v') in
    ok m

  let map_mem : typer = typer_2 "MAP_MEM" @@ fun k m ->
    let%bind (src, _dst) = bind_map_or (get_t_map , get_t_big_map) m in
    let%bind () = assert_type_value_eq (src, k) in
    ok @@ t_bool ()

  let map_find : typer = typer_2 "MAP_FIND" @@ fun k m ->
    let%bind (src, dst) =
      trace_strong (simple_error "MAP_FIND: not map or bigmap") @@
      bind_map_or (get_t_map , get_t_big_map) m in
    let%bind () = assert_type_value_eq (src, k) in
    ok @@ dst

  let map_find_opt : typer = typer_2 "MAP_FIND_OPT" @@ fun k m ->
    let%bind (src, dst) = bind_map_or (get_t_map , get_t_big_map) m in
    let%bind () = assert_type_value_eq (src, k) in
    ok @@ t_option dst ()

  let map_iter : typer = typer_2 "MAP_ITER" @@ fun f m ->
    let%bind (k, v) = get_t_map m in
    let%bind (arg , res) = get_t_function f in
    let%bind () = assert_eq_1 arg (t_pair k v ()) in
    let%bind () = assert_eq_1 res (t_unit ()) in
    ok @@ t_unit ()

  let map_map : typer = typer_2 "MAP_MAP" @@ fun f m ->
    let%bind (k, v) = get_t_map m in
    let%bind (arg , res) = get_t_function f in
    let%bind () = assert_eq_1 arg (t_pair k v ()) in
    ok @@ t_map k res ()

  let size = typer_1 "SIZE" @@ fun t ->
    let%bind () =
      Assert.assert_true @@
      (is_t_map t || is_t_list t || is_t_string t || is_t_bytes t || is_t_set t ) in
    ok @@ t_nat ()

  let slice = typer_3 "SLICE" @@ fun i j s ->
    let%bind () = assert_eq_1 i (t_nat ()) in
    let%bind () = assert_eq_1 j (t_nat ()) in
    if eq_1 s (t_string ())
    then ok @@ t_string ()
    else if eq_1 s (t_bytes ())
    then ok @@ t_bytes ()
    else simple_fail "bad slice"

  let failwith_ = typer_1_opt "FAILWITH" @@ fun t opt ->
    let%bind () =
      Assert.assert_true @@
      (is_t_string t) in
    let default = t_unit () in
    ok @@ Simple_utils.Option.unopt ~default opt

  let map_get_force = typer_2 "MAP_GET_FORCE" @@ fun i m ->
    let%bind (src, dst) = bind_map_or (get_t_map , get_t_big_map) m in
    let%bind _ = assert_type_value_eq (src, i) in
    ok dst

  let map_get = typer_2 "MAP_GET" @@ fun i m ->
    let%bind (src, dst) = bind_map_or (get_t_map , get_t_big_map) m in
    let%bind _ = assert_type_value_eq (src, i) in
    ok @@ t_option dst ()

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

  let amount = constant "AMOUNT" @@ t_mutez ()

  let balance = constant "BALANCE" @@ t_mutez ()

  let chain_id = constant "CHAIN_ID" @@ t_chain_id ()

  let address = typer_1 "ADDRESS" @@ fun contract ->
    let%bind () = assert_t_contract contract in
    ok @@ t_address ()

  let self_address = typer_0 "SELF_ADDRESS" @@ fun _ ->
    ok @@ t_address ()

  let implicit_account = typer_1 "IMPLICIT_ACCOUNT" @@ fun key_hash ->
    let%bind () = assert_t_key_hash key_hash in
    ok @@ t_contract (t_unit () ) ()

  let now = constant "NOW" @@ t_timestamp ()

  let transaction = typer_3 "CALL" @@ fun param amount contract ->
    let%bind () = assert_t_mutez amount in
    let%bind contract_param = get_t_contract contract in
    let%bind () = assert_type_value_eq (param , contract_param) in
    ok @@ t_operation ()

  let originate = typer_6 "ORIGINATE" @@ fun manager delegate_opt spendable delegatable init_balance code ->
    let%bind () = assert_eq_1 manager (t_key_hash ()) in
    let%bind () = assert_eq_1 delegate_opt (t_option (t_key_hash ()) ()) in
    let%bind () = assert_eq_1 spendable (t_bool ()) in
    let%bind () = assert_eq_1 delegatable (t_bool ()) in
    let%bind () = assert_t_mutez init_balance in
    let%bind (arg , res) = get_t_function code in
    let%bind (_param , storage) = get_t_pair arg in
    let%bind (storage' , op_lst) = get_t_pair res in
    let%bind () = assert_eq_1 storage storage' in
    let%bind () = assert_eq_1 op_lst (t_list (t_operation ()) ()) in
    ok @@ (t_pair (t_operation ()) (t_address ()) ())

  let get_contract = typer_1_opt "CONTRACT" @@ fun addr_tv tv_opt ->
    if not (type_value_eq (addr_tv, t_address ()))
    then fail @@ simple_error (Format.asprintf "get_contract expects an address, got %a" PP.type_value addr_tv)
    else
    let%bind tv =
      trace_option (simple_error "get_contract needs a type annotation") tv_opt in
    let%bind tv' =
      trace_strong (simple_error "get_contract has a not-contract annotation") @@
      get_t_contract tv in
    ok @@ t_contract tv' ()

  let get_entrypoint = typer_2_opt "CONTRACT_ENTRYPOINT" @@ fun entry_tv addr_tv tv_opt ->
    if not (type_value_eq (entry_tv, t_string ()))
    then fail @@ simple_error (Format.asprintf "get_entrypoint expects a string entrypoint label for first argument, got %a" PP.type_value entry_tv)
    else
    if not (type_value_eq (addr_tv, t_address ()))
    then fail @@ simple_error (Format.asprintf "get_entrypoint expects an address for second argument, got %a" PP.type_value addr_tv)
    else
    let%bind tv =
      trace_option (simple_error "get_entrypoint needs a type annotation") tv_opt in
    let%bind tv' =
      trace_strong (simple_error "get_entrypoint has a not-contract annotation") @@
      get_t_contract tv in
    ok @@ t_contract tv' ()

  let set_delegate = typer_1 "SET_DELEGATE" @@ fun delegate_opt ->
    let%bind () = assert_eq_1 delegate_opt (t_option (t_key_hash ()) ()) in
    ok @@ t_operation ()

  let abs = typer_1 "ABS" @@ fun t ->
    let%bind () = assert_t_int t in
    ok @@ t_nat ()

  let is_nat = typer_1 "ISNAT" @@ fun t ->
    let%bind () = assert_t_int t in
    ok @@ t_option (t_nat ()) ()

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
    if (eq_1 a (t_nat ()) && eq_1 b (t_mutez ())) || (eq_1 b (t_nat ()) && eq_1 a (t_mutez ()))
    then ok @@ t_mutez () else
      simple_fail "Multiplying with wrong types"

  let div = typer_2 "DIV" @@ fun a b ->
    if eq_2 (a , b) (t_nat ())
    then ok @@ t_nat () else
    if eq_2 (a , b) (t_int ())
    then ok @@ t_int () else
    if eq_1 a (t_mutez ()) && eq_1 b (t_nat ())
    then ok @@ t_mutez () else
    if eq_1 a (t_mutez ()) && eq_1 b (t_mutez ())
    then ok @@ t_nat () else
      simple_fail "Dividing with wrong types"

  let mod_ = typer_2 "MOD" @@ fun a b ->
    if (eq_1 a (t_nat ()) || eq_1 a (t_int ())) && (eq_1 b (t_nat ()) || eq_1 b (t_int ()))
    then ok @@ t_nat () else
    if eq_1 a (t_mutez ()) && eq_1 b (t_mutez ())
    then ok @@ t_mutez () else
      simple_fail "Computing modulo with wrong types"

  let add = typer_2 "ADD" @@ fun a b ->
    if eq_2 (a , b) (t_nat ())
    then ok @@ t_nat () else
    if eq_2 (a , b) (t_int ())
    then ok @@ t_int () else
    if eq_2 (a , b) (t_mutez ())
    then ok @@ t_mutez () else
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

  let set_iter = typer_2 "SET_ITER" @@ fun body set ->
    let%bind (arg , res) = get_t_function body in
    let%bind () = Assert.assert_true (eq_1 res (t_unit ())) in
    let%bind key = get_t_set set in
    if eq_1 key arg
    then ok (t_unit ())
    else simple_fail "bad set iter"

  let list_iter = typer_2 "LIST_ITER" @@ fun body lst ->
    let%bind (arg , res) = get_t_function body in
    let%bind () = Assert.assert_true (eq_1 res (t_unit ())) in
    let%bind key = get_t_list lst in
    if eq_1 key arg
    then ok (t_unit ())
    else simple_fail "bad list iter"

  let list_map = typer_2 "LIST_MAP" @@ fun body lst ->
    let%bind (arg , res) = get_t_function body in
    let%bind key = get_t_list lst in
    if eq_1 key arg
    then ok (t_list res ())
    else simple_fail "bad list map"

  let list_fold = typer_3 "LIST_FOLD" @@ fun body lst init ->
    let%bind (arg , res) = get_t_function body in
    let%bind (prec , cur) = get_t_pair arg in
    let%bind key = get_t_list lst in
    let msg = Format.asprintf "%a vs %a"
        Ast_typed.PP.type_value key
        Ast_typed.PP.type_value arg
    in
    trace (simple_error ("bad list fold:" ^ msg)) @@
    let%bind () = assert_eq_1 ~msg:"key cur" key cur in
    let%bind () = assert_eq_1 ~msg:"prec res" prec res in
    let%bind () = assert_eq_1 ~msg:"res init" res init in
    ok res

  let set_fold = typer_3 "SET_FOLD" @@ fun body lst init ->
    let%bind (arg , res) = get_t_function body in
    let%bind (prec , cur) = get_t_pair arg in
    let%bind key = get_t_set lst in
    let msg = Format.asprintf "%a vs %a"
        Ast_typed.PP.type_value key
        Ast_typed.PP.type_value arg
    in
    trace (simple_error ("bad set fold:" ^ msg)) @@
    let%bind () = assert_eq_1 ~msg:"key cur" key cur in
    let%bind () = assert_eq_1 ~msg:"prec res" prec res in
    let%bind () = assert_eq_1 ~msg:"res init" res init in
    ok res

  let map_fold = typer_3 "MAP_FOLD" @@ fun body map init ->
    let%bind (arg , res) = get_t_function body in
    let%bind (prec , cur) = get_t_pair arg in
    let%bind (key , value) = get_t_map map in
    let msg = Format.asprintf "%a vs %a"
        Ast_typed.PP.type_value key
        Ast_typed.PP.type_value arg
    in
    trace (simple_error ("bad list fold:" ^ msg)) @@
    let%bind () = assert_eq_1 ~msg:"key cur" (t_pair key value ()) cur in
    let%bind () = assert_eq_1 ~msg:"prec res" prec res in
    let%bind () = assert_eq_1 ~msg:"res init" res init in
    ok res

  (** FOLD_WHILE is a fold operation that takes an initial value of a certain type
      and then iterates on it until a condition is reached. The auxillary function
      that does the fold returns either boolean true or boolean false to indicate
      whether the fold should continue or not. Necessarily then the initial value
      must match the input parameter of the auxillary function, and the auxillary
      should return type (bool * input) *)
  let fold_while = typer_2 "FOLD_WHILE" @@ fun body init ->
    let%bind (arg, result) = get_t_function body in
    let%bind () = assert_eq_1 arg init in
    let%bind () = assert_eq_1 (t_pair (t_bool ()) init ()) result
    in ok init

  (* Continue and Stop are just syntactic sugar for building a pair (bool * a') *)
  let continue = typer_1 "CONTINUE" @@ fun arg ->
    ok @@ t_pair (t_bool ()) arg ()

  let stop = typer_1 "STOP" @@ fun arg ->
    ok (t_pair (t_bool ()) arg ())

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

  let constant_typers c : typer result = match c with
    | C_INT                 -> ok @@ int ;
    | C_UNIT                -> ok @@ unit ;
    | C_NOW                 -> ok @@ now ;
    | C_IS_NAT              -> ok @@ is_nat ;
    | C_SOME                -> ok @@ some ;
    | C_NONE                -> ok @@ none ;
    | C_ASSERTION           -> ok @@ assertion ;
    | C_FAILWITH            -> ok @@ failwith_ ;
    (* LOOPS *)
    | C_FOLD_WHILE          -> ok @@ fold_while ;
    | C_CONTINUE            -> ok @@ continue ;
    | C_STOP                -> ok @@ stop ;
     (* MATH *)
    | C_NEG                 -> ok @@ neg ;
    | C_ABS                 -> ok @@ abs ;
    | C_ADD                 -> ok @@ add ;
    | C_SUB                 -> ok @@ sub ;
    | C_MUL                 -> ok @@ times;
    | C_DIV                 -> ok @@ div ;
    | C_MOD                 -> ok @@ mod_ ;
    (* LOGIC *)
    | C_NOT                 -> ok @@ not_ ;
    | C_AND                 -> ok @@ and_ ;
    | C_OR                  -> ok @@ or_ ;
    | C_XOR                 -> ok @@ xor ;
    (* COMPARATOR *)
    | C_EQ                  -> ok @@ comparator "EQ" ;
    | C_NEQ                 -> ok @@ comparator "NEQ" ;
    | C_LT                  -> ok @@ comparator "LT" ;
    | C_GT                  -> ok @@ comparator "GT" ;
    | C_LE                  -> ok @@ comparator "LE" ;
    | C_GE                  -> ok @@ comparator "GE" ;
    (* BYTES / STRING *)
    | C_SIZE                -> ok @@ size ;
    | C_CONCAT              -> ok @@ concat ;
    | C_SLICE               -> ok @@ slice ;
    | C_BYTES_PACK          -> ok @@ bytes_pack ;
    | C_BYTES_UNPACK        -> ok @@ bytes_unpack ;
    | C_CONS                -> ok @@ cons ;
    (* SET  *)
    | C_SET_EMPTY           -> ok @@ set_empty ;
    | C_SET_ADD             -> ok @@ set_add ;
    | C_SET_REMOVE          -> ok @@ set_remove ;
    | C_SET_ITER            -> ok @@ set_iter ;
    | C_SET_FOLD            -> ok @@ set_fold ;
    | C_SET_MEM             -> ok @@ set_mem ;

    (* LIST *)
    | C_LIST_ITER           -> ok @@ list_iter ;
    | C_LIST_MAP            -> ok @@ list_map ;
    | C_LIST_FOLD           -> ok @@ list_fold ;
    | C_LIST_CONS           -> ok @@ list_cons ;
    (* MAP *)
    | C_MAP_GET             -> ok @@ map_get ;
    | C_MAP_GET_FORCE       -> ok @@ map_get_force ;
    | C_MAP_ADD             -> ok @@ map_add ;
    | C_MAP_REMOVE          -> ok @@ map_remove ;
    | C_MAP_UPDATE          -> ok @@ map_update ;
    | C_MAP_ITER            -> ok @@ map_iter ;
    | C_MAP_MAP             -> ok @@ map_map ;
    | C_MAP_FOLD            -> ok @@ map_fold ;
    | C_MAP_MEM             -> ok @@ map_mem ;
    | C_MAP_FIND            -> ok @@ map_find ;
    | C_MAP_FIND_OPT        -> ok @@ map_find_opt ;
    (* BIG MAP *)
    (* CRYPTO *)
    | C_SHA256              -> ok @@ hash256 ;
    | C_SHA512              -> ok @@ hash512 ;
    | C_BLAKE2b             -> ok @@ blake2b ;
    | C_HASH_KEY            -> ok @@ hash_key ;
    | C_CHECK_SIGNATURE     -> ok @@ check_signature ;
    | C_CHAIN_ID            -> ok @@ chain_id ;
    (*BLOCKCHAIN *)
    | C_CONTRACT            -> ok @@ get_contract ;
    | C_CONTRACT_ENTRYPOINT -> ok @@ get_entrypoint ;
    | C_AMOUNT              -> ok @@ amount ;
    | C_BALANCE             -> ok @@ balance ;
    | C_CALL                -> ok @@ transaction ;
    | C_SENDER              -> ok @@ sender ;
    | C_SOURCE              -> ok @@ source ;
    | C_ADDRESS             -> ok @@ address ;
    | C_SELF_ADDRESS        -> ok @@ self_address;
    | C_IMPLICIT_ACCOUNT    -> ok @@ implicit_account;
    | C_SET_DELEGATE        -> ok @@ set_delegate ;
    | _                     -> simple_fail @@ Format.asprintf "Typer not implemented for consant %a" Stage_common.PP.constant c



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
  open Mini_c

  let get_operators c : predicate result =
  match c with
    | C_ADD             -> ok @@ simple_binary @@ prim I_ADD
    | C_SUB             -> ok @@ simple_binary @@ prim I_SUB
    | C_MUL             -> ok @@ simple_binary @@ prim I_MUL
    | C_DIV             -> ok @@ simple_binary @@ seq [prim I_EDIV ; i_assert_some_msg (i_push_string "DIV by 0") ; i_car]
    | C_MOD             -> ok @@ simple_binary @@ seq [prim I_EDIV ; i_assert_some_msg (i_push_string "MOD by 0") ; i_cdr]
    | C_NEG             -> ok @@ simple_unary @@ prim I_NEG
    | C_OR              -> ok @@ simple_binary @@ prim I_OR
    | C_AND             -> ok @@ simple_binary @@ prim I_AND
    | C_XOR             -> ok @@ simple_binary @@ prim I_XOR
    | C_NOT             -> ok @@ simple_unary @@ prim I_NOT
    | C_PAIR            -> ok @@ simple_binary @@ prim I_PAIR
    | C_CAR             -> ok @@ simple_unary @@ prim I_CAR
    | C_CDR             -> ok @@ simple_unary @@ prim I_CDR
    | C_EQ              -> ok @@ simple_binary @@ seq [prim I_COMPARE ; prim I_EQ]
    | C_NEQ             -> ok @@ simple_binary @@ seq [prim I_COMPARE ; prim I_NEQ]
    | C_LT              -> ok @@ simple_binary @@ seq [prim I_COMPARE ; prim I_LT]
    | C_LE              -> ok @@ simple_binary @@ seq [prim I_COMPARE ; prim I_LE]
    | C_GT              -> ok @@ simple_binary @@ seq [prim I_COMPARE ; prim I_GT]
    | C_GE              -> ok @@ simple_binary @@ seq [prim I_COMPARE ; prim I_GE]
    | C_UPDATE          -> ok @@ simple_ternary @@ prim I_UPDATE
    | C_SOME            -> ok @@ simple_unary  @@ prim I_SOME
    | C_MAP_GET_FORCE   -> ok @@ simple_binary @@ seq [prim I_GET ; i_assert_some_msg (i_push_string "GET_FORCE")]
    | C_MAP_FIND        -> ok @@ simple_binary @@ seq [prim I_GET ; i_assert_some_msg (i_push_string "MAP FIND")]
    | C_MAP_GET         -> ok @@ simple_binary @@ prim I_GET
    | C_MAP_MEM         -> ok @@ simple_binary @@ prim I_MEM
    | C_MAP_FIND_OPT    -> ok @@ simple_binary @@ prim I_GET
    | C_MAP_ADD         -> ok @@ simple_ternary @@ seq [dip (i_some) ; prim I_UPDATE]
    | C_MAP_UPDATE      -> ok @@ simple_ternary @@ prim I_UPDATE
    | C_FOLD_WHILE      -> ok @@ simple_binary @@ seq [i_swap ; (i_push (prim T_bool) (prim D_True));prim ~children:[seq [dip i_dup; i_exec; i_unpair]] I_LOOP ;i_swap ; i_drop]
    | C_CONTINUE        -> ok @@ simple_unary @@ seq [(i_push (prim T_bool) (prim D_True)); i_pair]
    | C_STOP            -> ok @@ simple_unary @@ seq [(i_push (prim T_bool) (prim D_False)); i_pair]
    | C_SIZE            -> ok @@ simple_unary @@ prim I_SIZE
    | C_FAILWITH        -> ok @@ simple_unary @@ prim I_FAILWITH
    | C_ASSERT_INFERRED -> ok @@ simple_binary @@ i_if (seq [i_failwith]) (seq [i_drop ; i_push_unit])
    | C_ASSERTION       -> ok @@ simple_unary @@ i_if (seq [i_push_unit]) (seq [i_push_unit ; i_failwith])
    | C_INT             -> ok @@ simple_unary @@ prim I_INT
    | C_ABS             -> ok @@ simple_unary @@ prim I_ABS
    | C_IS_NAT          -> ok @@ simple_unary @@ prim I_ISNAT
    | C_CONS            -> ok @@ simple_binary @@ prim I_CONS
    | C_UNIT            -> ok @@ simple_constant @@ prim I_UNIT
    | C_BALANCE         -> ok @@ simple_constant @@ prim I_BALANCE
    | C_AMOUNT          -> ok @@ simple_constant @@ prim I_AMOUNT
    | C_ADDRESS         -> ok @@ simple_unary @@ prim I_ADDRESS
    | C_SELF_ADDRESS    -> ok @@ simple_constant @@ seq [prim I_SELF; prim I_ADDRESS]
    | C_IMPLICIT_ACCOUNT -> ok @@ simple_unary @@ prim I_IMPLICIT_ACCOUNT
    | C_SET_DELEGATE    -> ok @@ simple_unary @@ prim I_SET_DELEGATE
    | C_NOW             -> ok @@ simple_constant @@ prim I_NOW
    | C_CALL            -> ok @@ simple_ternary @@ prim I_TRANSFER_TOKENS
    | C_SOURCE          -> ok @@ simple_constant @@ prim I_SOURCE
    | C_SENDER          -> ok @@ simple_constant @@ prim I_SENDER
    | C_SET_MEM         -> ok @@ simple_binary @@ prim I_MEM
    | C_SET_ADD         -> ok @@ simple_binary @@ seq [dip (i_push (prim T_bool) (prim D_True)) ; prim I_UPDATE]
    | C_SET_REMOVE      -> ok @@ simple_binary @@ seq [dip (i_push (prim T_bool) (prim D_False)) ; prim I_UPDATE]
    | C_SLICE           -> ok @@ simple_ternary @@ seq [prim I_SLICE ; i_assert_some_msg (i_push_string "SLICE")]
    | C_SHA256          -> ok @@ simple_unary @@ prim I_SHA256
    | C_SHA512          -> ok @@ simple_unary @@ prim I_SHA512
    | C_BLAKE2b         -> ok @@ simple_unary @@ prim I_BLAKE2B
    | C_CHECK_SIGNATURE -> ok @@ simple_ternary @@ prim I_CHECK_SIGNATURE
    | C_HASH_KEY        -> ok @@ simple_unary @@ prim I_HASH_KEY
    | C_BYTES_PACK      -> ok @@ simple_unary @@ prim I_PACK
    | C_CONCAT          -> ok @@ simple_binary @@ prim I_CONCAT
    | C_CHAIN_ID        -> ok @@ simple_constant @@ prim I_CHAIN_ID
    | _                 -> simple_fail @@ Format.asprintf "operator not implemented for %a" Stage_common.PP.constant c
  

  (*
    Some complex operators will need to be added in compiler/compiler_program.
    All operators whose compilations involve a type are found there.
  *)

end
