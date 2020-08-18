open Trace
open Helpers
open Errors
open Ast_typed

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

  Various helpers are defined bellow.
*)

let need_real_type tv loc = trace_option (not_annotated loc) @@ 
match tv.type_content with T_wildcard -> None | _ -> Some tv


let none = typer_0 "NONE" @@ fun tv l ->
  need_real_type tv l

let set_empty = typer_0 "SET_EMPTY" @@ fun tv l ->
  need_real_type tv l

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
    fail bad_substraction

let some = typer_1 "SOME" @@ fun a -> ok @@ t_option a

let map_remove : typer = typer_2 "MAP_REMOVE" @@ fun k m ->
  let%bind (src , _) = bind_map_or (
      (fun m -> trace_option (expected_map m) @@ get_t_map m) ,
      (fun m -> trace_option (expected_big_map m) @@ get_t_big_map m)
    ) m in
  let%bind () = assert_eq src k in
  ok m

let map_empty = typer_0 "MAP_EMPTY" @@ fun tv l ->
  let%bind tv = need_real_type tv l in
  let%bind (src, dst) = trace_option (expected_map tv) @@ get_t_map tv in
  ok @@ t_map src dst

let big_map_empty = typer_0 "BIG_MAP_EMPTY" @@ fun tv l ->
  let%bind tv = need_real_type tv l in
  let%bind (src, dst) = trace_option (expected_big_map tv) @@ get_t_big_map tv in
  ok @@ t_big_map src dst

let map_add : typer = typer_3 "MAP_ADD" @@ fun k v m ->
  let%bind (src , dst) = bind_map_or (
      (fun m -> trace_option (expected_map m) @@ get_t_map m) ,
      (fun m -> trace_option (expected_big_map m) @@ get_t_big_map m)
    ) m in
  let%bind () = assert_eq src k in
  let%bind () = assert_eq dst v in
  ok m

let map_update : typer = typer_3 "MAP_UPDATE" @@ fun k v m ->
  let%bind (src , dst) = bind_map_or (
      (fun m -> trace_option (expected_map m) @@ get_t_map m) ,
      (fun m -> trace_option (expected_big_map m) @@ get_t_big_map m)
    ) m in
  let%bind () = assert_eq src k in
  let%bind v' = trace_option (expected_option v) @@ get_t_option v in
  let%bind () = assert_eq dst v' in
  ok m

let map_mem : typer = typer_2 "MAP_MEM" @@ fun k m ->
  let%bind (src , _dst) = bind_map_or (
      (fun m -> trace_option (expected_map m) @@ get_t_map m) ,
      (fun m -> trace_option (expected_big_map m) @@ get_t_big_map m)
    ) m in
  let%bind () = assert_eq src k in
  ok @@ t_bool ()

let map_find : typer = typer_2 "MAP_FIND" @@ fun k m ->
  let%bind (src , dst) = bind_map_or (
      (fun m -> trace_option (expected_map m) @@ get_t_map m) ,
      (fun m -> trace_option (expected_big_map m) @@ get_t_big_map m)
    ) m in
  let%bind () = assert_eq src k in
  ok @@ dst

let map_find_opt : typer = typer_2 "MAP_FIND_OPT" @@ fun k m ->
  let%bind (src , dst) = bind_map_or (
      (fun m -> trace_option (expected_map m) @@ get_t_map m) ,
      (fun m -> trace_option (expected_big_map m) @@ get_t_big_map m)
    ) m in
  let%bind () = assert_eq src k in
  ok @@ t_option dst

let map_iter : typer = typer_2 "MAP_ITER" @@ fun f m ->
  let%bind (k, v) = trace_option (expected_map m) @@ get_t_map m in
  let%bind (arg , res) = trace_option (expected_function f) @@ get_t_function f in
  let kv = t_pair k v in
  let unit = t_unit () in
  let%bind () = assert_eq arg kv in
  let%bind () = assert_eq res unit in
  ok @@ t_unit ()

let map_map : typer = typer_2 "MAP_MAP" @@ fun f m ->
  let%bind (k, v) = trace_option (expected_map m) @@ get_t_map m in
  let%bind (arg , res) = trace_option (expected_function f) @@ get_t_function f in
  let kv = t_pair k v in
  let%bind () = assert_eq arg kv in
  ok @@ t_map k res

let size = typer_1 "SIZE" @@ fun t ->
  let%bind () =
    Assert.assert_true (wrong_size t) @@
    (is_t_map t || is_t_list t || is_t_string t || is_t_bytes t || is_t_set t ) in
  ok @@ t_nat ()

let slice = typer_3 "SLICE" @@ fun i j s ->
  let t_nat = t_nat () in
  let%bind () = assert_eq i t_nat in
  let%bind () = assert_eq j t_nat in
  if eq_1 s (t_string ())
  then ok @@ t_string ()
  else if eq_1 s (t_bytes ())
  then ok @@ t_bytes ()
  else fail @@ typeclass_error
                 [
                   [t_nat;t_nat;t_string()] ;
                   [t_nat;t_nat;t_bytes()] ;
                 ]
                 [i ; j ; s]

let failwith_ = typer_1_opt "FAILWITH" @@ fun t opt ->
  let%bind _ =
    if eq_1 t (t_string ())
    then ok ()
    else if eq_1 t (t_nat ())
    then ok ()
    else if eq_1 t (t_int ())
    then ok ()
    else
      fail @@ typeclass_error
        [
          [t_string()] ;
          [t_nat()] ;
          [t_int()] ;
        ]
        [t] in
  ok @@ match opt.type_content with
    T_wildcard -> t_unit () | _ -> opt

let int : typer = typer_1 "INT" @@ fun t ->
  let%bind () = trace_option (expected_nat t) @@ assert_t_nat t in
  ok @@ t_int ()

let bytes_pack : typer = typer_1 "PACK" @@ fun _t ->
  ok @@ t_bytes ()

let bytes_unpack = typer_1_opt "UNPACK" @@ fun input output ->
  let%bind () = trace_option (expected_bytes input) @@ assert_t_bytes input in
  need_real_type output input.location

let hash256 = typer_1 "SHA256" @@ fun t ->
  let%bind () = trace_option (expected_bytes t) @@ assert_t_bytes t in
  ok @@ t_bytes ()

let hash512 = typer_1 "SHA512" @@ fun t ->
  let%bind () = trace_option (expected_bytes t) @@ assert_t_bytes t in
  ok @@ t_bytes ()

let blake2b = typer_1 "BLAKE2b" @@ fun t ->
  let%bind () = trace_option (expected_bytes t) @@ assert_t_bytes t in
  ok @@ t_bytes ()

let hash_key = typer_1 "HASH_KEY" @@ fun t ->
  let%bind () = trace_option (expected_key t) @@ assert_t_key t in
  ok @@ t_key_hash ()

let check_signature = typer_3 "CHECK_SIGNATURE" @@ fun k s b ->
  let%bind () = trace_option (expected_key k) @@ assert_t_key k in
  let%bind () = trace_option (expected_signature s) @@ assert_t_signature s in
  let%bind () = trace_option (expected_bytes b) @@ assert_t_bytes b in
  ok @@ t_bool ()

let sender = constant' "SENDER" @@ t_address ()

let source = constant' "SOURCE" @@ t_address ()

let unit = constant' "UNIT" @@ t_unit ()

let amount = constant' "AMOUNT" @@ t_mutez ()

let balance = constant' "BALANCE" @@ t_mutez ()

let chain_id = constant' "CHAIN_ID" @@ t_chain_id ()

let address = typer_1 "ADDRESS" @@ fun c ->
  let%bind () = trace_option (expected_contract c) @@ assert_t_contract c in
  ok @@ t_address ()

let self_address = typer_0 "SELF_ADDRESS" @@ fun _ _ ->
  ok @@ t_address ()

let self = typer_1_opt "SELF" @@ fun entrypoint_as_string tv ->
  let%bind () = trace_option (expected_string entrypoint_as_string) @@ assert_t_string entrypoint_as_string in
  need_real_type tv entrypoint_as_string.location

let implicit_account = typer_1 "IMPLICIT_ACCOUNT" @@ fun key_hash ->
  let%bind () = trace_option (expected_key_hash key_hash) @@ assert_t_key_hash key_hash in
  ok @@ t_contract (t_unit () )

let now = constant' "NOW" @@ t_timestamp ()

let transaction = typer_3 "CALL" @@ fun param amount contract ->
  let%bind () = trace_option (expected_mutez amount) @@ assert_t_mutez amount in
  let%bind contract_param = trace_option (expected_contract contract) @@ get_t_contract contract in
  let%bind () = assert_eq param contract_param in
  ok @@ t_operation ()

let create_contract = typer_4 "CREATE_CONTRACT" @@ fun f kh_opt amount init_storage  ->
  let%bind (args , ret) = trace_option (expected_function f) @@ get_t_function f in
  let%bind (_,s) = trace_option (expected_pair args) @@ get_t_pair args in
  let%bind (oplist,s') = trace_option (expected_pair ret) @@ get_t_pair ret in
  let%bind () = trace_option (expected_mutez amount) @@ assert_t_mutez amount in
  let%bind (delegate) = trace_option (expected_option kh_opt) @@ get_t_option kh_opt in
  let%bind () = assert_eq s s' in
  let%bind () = assert_eq s init_storage in
  let%bind () = trace_option (expected_op_list oplist) @@ assert_t_list_operation oplist in
  let%bind () = trace_option (expected_key_hash delegate) @@ assert_t_key_hash delegate in
  ok @@ t_pair (t_operation ()) (t_address ())

let get_contract = typer_1_opt "CONTRACT" @@ fun addr_tv tv ->
  let t_addr = t_address () in
  let%bind () = assert_eq addr_tv t_addr in
  let%bind tv = need_real_type tv addr_tv.location in
  let%bind tv' = trace_option (expected_contract tv) @@ get_t_contract tv in
  ok @@ t_contract tv'

let get_contract_opt = typer_1_opt "CONTRACT OPT" @@ fun addr_tv tv ->
  let t_addr = t_address () in
  let%bind () = assert_eq addr_tv t_addr in
  let%bind tv = need_real_type tv addr_tv.location in
  let%bind tv = trace_option (expected_option tv) @@ get_t_option tv in
  let%bind tv' = trace_option (expected_contract tv) @@ get_t_contract tv in
  ok @@ t_option (t_contract tv')

let get_entrypoint = typer_2_opt "CONTRACT_ENTRYPOINT" @@ fun entry_tv addr_tv tv ->
  let t_string = t_string () in
  let t_addr = t_address () in
  let%bind () = assert_eq entry_tv t_string in
  let%bind () = assert_eq addr_tv t_addr in
  let%bind tv = need_real_type tv entry_tv.location in
  let%bind tv' = trace_option (expected_contract tv) @@ get_t_contract tv in
  ok @@ t_contract tv'

let get_entrypoint_opt = typer_2_opt "CONTRACT_ENTRYPOINT_OPT" @@ fun entry_tv addr_tv tv ->
  let t_string = t_string () in
  let t_addr = t_address () in
  let%bind () = assert_eq entry_tv t_string in
  let%bind () = assert_eq addr_tv t_addr in
  let%bind tv = need_real_type tv entry_tv.location in
  let%bind tv = trace_option (expected_option tv) @@ get_t_option tv in
  let%bind tv' = trace_option (expected_contract tv) @@ get_t_contract tv in
  ok @@ t_option (t_contract tv' )

let set_delegate = typer_1 "SET_DELEGATE" @@ fun delegate_opt ->
  let kh_opt = (t_option (t_key_hash ()) ) in
  let%bind () = assert_eq delegate_opt kh_opt in
  ok @@ t_operation ()

let abs = typer_1 "ABS" @@ fun t ->
  let%bind () = trace_option (expected_int t) @@ assert_t_int t in
  ok @@ t_nat ()

let is_nat = typer_1 "ISNAT" @@ fun t ->
  let%bind () = trace_option (expected_int t) @@ assert_t_int t in
  ok @@ t_option (t_nat ())

let neg = typer_1 "NEG" @@ fun t ->
  let%bind () = Assert.assert_true (wrong_neg t) @@ (eq_1 t (t_nat ()) || eq_1 t (t_int ())) in
  ok @@ t_int ()

let assertion = typer_1 "ASSERT" @@ fun a ->
  let%bind () = trace_option (expected_bool a) @@ assert_t_bool a in
  ok @@ t_unit ()

let times = typer_2 "TIMES" @@ fun a b ->
  if eq_2 (a , b) (t_nat ())
  then ok @@ t_nat () else
  if eq_2 (a , b) (t_int ())
  then ok @@ t_int () else
  if (eq_1 a (t_nat ()) && eq_1 b (t_mutez ())) || (eq_1 b (t_nat ()) && eq_1 a (t_mutez ()))
  then ok @@ t_mutez () else
    fail @@ typeclass_error
              [
                [t_nat();t_nat()] ;
                [t_int();t_int()] ;
                [t_nat();t_mutez()] ;
                [t_mutez();t_nat()] ;
              ]
              [a; b]

let ediv = typer_2 "EDIV" @@ fun a b ->
  if eq_2 (a , b) (t_nat ())
  then ok @@ t_option (t_pair (t_nat ()) (t_nat ()) ) else
  if eq_2 (a , b) (t_int ())
  then ok @@ t_option (t_pair (t_int ()) (t_nat ()) ) else
  if eq_1 a (t_mutez ()) && eq_1 b (t_mutez ())
  then ok @@ t_option (t_pair (t_nat ()) (t_mutez ()) ) else
  if eq_1 a (t_mutez ()) && eq_1 b (t_nat ())
  then ok @@ t_option (t_pair (t_mutez ()) (t_mutez ()) ) else
    fail @@ typeclass_error
              [
                [t_nat();t_nat()] ;
                [t_int();t_int()] ;
                [t_mutez();t_nat()] ;
                [t_mutez();t_mutez()] ;
              ]
              [a; b]

let div = typer_2 "DIV" @@ fun a b ->
  if eq_2 (a , b) (t_nat ())
  then ok @@ t_nat () else
  if eq_2 (a , b) (t_int ())
  then ok @@ t_int () else
  if eq_1 a (t_mutez ()) && eq_1 b (t_nat ())
  then ok @@ t_mutez () else
  if eq_1 a (t_mutez ()) && eq_1 b (t_mutez ())
  then ok @@ t_nat () else
    fail @@ typeclass_error
              [
                [t_nat();t_nat()] ;
                [t_int();t_int()] ;
                [t_mutez();t_nat()] ;
                [t_mutez();t_mutez()] ;
              ]
              [a; b]

let mod_ = typer_2 "MOD" @@ fun a b ->
  if (eq_1 a (t_nat ()) || eq_1 a (t_int ())) && (eq_1 b (t_nat ()) || eq_1 b (t_int ()))
  then ok @@ t_nat () else
  if eq_1 a (t_mutez ()) && eq_1 b (t_mutez ())
  then ok @@ t_mutez () else
    fail @@ typeclass_error
              [
                [t_nat();t_nat()] ;
                [t_nat();t_int()] ;
                [t_int();t_nat()] ;
                [t_int();t_int()] ;
                [t_mutez();t_mutez()] ;
              ]
              [a; b]

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
    fail @@ typeclass_error
              [
                [t_nat();t_nat()] ;
                [t_int();t_int()] ;
                [t_mutez();t_mutez()] ;
                [t_nat();t_int()] ;
                [t_int();t_nat()] ;
                [t_timestamp();t_int()] ;
                [t_int();t_timestamp()] ;
              ]
              [a; b]

let set_mem = typer_2 "SET_MEM" @@ fun elt set ->
  let%bind key = trace_option (expected_set set) @@ get_t_set set in
  let%bind () = assert_eq elt key in
  ok @@ t_bool ()

let set_add = typer_2 "SET_ADD" @@ fun elt set ->
  let%bind key = trace_option (expected_set set) @@ get_t_set set in
  let%bind () = assert_eq elt key in
  ok set

let set_remove = typer_2 "SET_REMOVE" @@ fun elt set ->
  let%bind key = trace_option (expected_set set) @@ get_t_set set in
  let%bind () = assert_eq elt key in
  ok set

let set_iter = typer_2 "SET_ITER" @@ fun body set ->
  let%bind (arg , res) = trace_option (expected_function body) @@ get_t_function body in
  let%bind () = assert_eq res (t_unit ()) in
  let%bind key = trace_option (expected_set set) @@ get_t_set set in
  let%bind () = assert_eq key arg in
  ok (t_unit ())

let list_empty = typer_0 "LIST_EMPTY" @@ fun tv l ->
  need_real_type tv l

let list_iter = typer_2 "LIST_ITER" @@ fun body lst ->
  let%bind (arg , res) = trace_option (expected_function body) @@ get_t_function body in
  let%bind () = assert_eq res (t_unit ()) in
  let%bind key = trace_option (expected_list lst) @@ get_t_list lst in
  let%bind () = assert_eq key arg in
  ok (t_unit ())

let list_map = typer_2 "LIST_MAP" @@ fun body lst ->
  let%bind (arg , res) = trace_option (expected_function body) @@ get_t_function body in
  let%bind key = trace_option (expected_list lst) @@ get_t_list lst in
  let%bind () = assert_eq key arg in
  ok (t_list res )

let list_fold = typer_3 "LIST_FOLD" @@ fun body lst init ->
  let%bind (arg , res) = trace_option (expected_function body) @@ get_t_function body in
  let%bind (prec , cur) = trace_option (expected_pair arg) @@ get_t_pair arg in
  let%bind key = trace_option (expected_list lst) @@ get_t_list lst in
  let%bind () = assert_eq key cur in
  let%bind () = assert_eq prec res in
  let%bind () = assert_eq res init in
  ok res

let set_fold = typer_3 "SET_FOLD" @@ fun body lst init ->
  let%bind (arg , res) = trace_option (expected_function body) @@ get_t_function body in
  let%bind (prec , cur) = trace_option (expected_pair arg) @@ get_t_pair arg in
  let%bind key = trace_option (expected_set lst) @@ get_t_set lst in
  let%bind () = assert_eq key cur in
  let%bind () = assert_eq prec res in
  let%bind () = assert_eq res init in
  ok res

let map_fold = typer_3 "MAP_FOLD" @@ fun body map init ->
  let%bind (arg , res) = trace_option (expected_function body) @@ get_t_function body in
  let%bind (prec , cur) = trace_option (expected_pair arg) @@ get_t_pair arg in
  let%bind (key , value) = trace_option (expected_map map) @@ get_t_map map in
  let kv = t_pair key value in
  let%bind () = assert_eq kv cur in
  let%bind () = assert_eq prec res in
  let%bind () = assert_eq res init in
  ok res

(** FOLD_WHILE is a fold operation that takes an initial value of a certain type
    and then iterates on it until a condition is reached. The auxillary function
    that does the fold returns either boolean true or boolean false to indicate
    whether the fold should continue or not. Necessarily then the initial value
    must match the input parameter of the auxillary function, and the auxillary
    should return type (bool * input) *)
let fold_while = typer_2 "FOLD_WHILE" @@ fun body init ->
  let%bind (arg, result) = trace_option (expected_function body) @@ get_t_function body in
  let%bind () = assert_eq arg init in
  let%bind () = assert_eq (t_pair (t_bool ()) init) result
  in ok init

(* Continue and Stop are just syntactic sugar for building a pair (bool * a') *)
let continue = typer_1 "CONTINUE" @@ fun arg ->
  ok @@ t_pair (t_bool ()) arg

let stop = typer_1 "STOP" @@ fun arg ->
  ok (t_pair (t_bool ()) arg)

let not_ = typer_1 "NOT" @@ fun elt ->
  if eq_1 elt (t_bool ())
  then ok @@ t_bool ()
  else if eq_1 elt (t_nat ()) || eq_1 elt (t_int ())
  then ok @@ t_int ()
  else fail @@ wrong_not elt

let or_ = typer_2 "OR" @@ fun a b ->
  if eq_2 (a , b) (t_bool ())
  then ok @@ t_bool ()
  else if eq_2 (a , b) (t_nat ())
  then ok @@ t_nat ()
  else fail @@ typeclass_error
                 [
                   [t_bool();t_bool()] ;
                   [t_nat();t_nat()] ;
                 ]
                 [a; b]

let xor = typer_2 "XOR" @@ fun a b ->
  if eq_2 (a , b) (t_bool ())
  then ok @@ t_bool ()
  else if eq_2 (a , b) (t_nat ())
  then ok @@ t_nat ()
  else fail @@ typeclass_error
                 [
                   [t_bool();t_bool()] ;
                   [t_nat();t_nat()] ;
                 ]
                 [a; b]

let and_ = typer_2 "AND" @@ fun a b ->
  if eq_2 (a , b) (t_bool ())
  then ok @@ t_bool ()
  else if eq_2 (a , b) (t_nat ()) || (eq_1 b (t_nat ()) && eq_1 a (t_int ()))
  then ok @@ t_nat ()
  else fail @@ typeclass_error
                 [
                   [t_bool();t_bool()] ;
                   [t_nat();t_nat()] ;
                   [t_int();t_nat()] ;
                 ]
                 [a; b]

let lsl_ = typer_2 "LSL" @@ fun a b ->
  if eq_2 (a , b) (t_nat ())
  then ok @@ t_nat ()
  else fail @@ typeclass_error
                 [
                   [t_nat();t_nat()] ;
                 ]
                 [a; b]

let lsr_ = typer_2 "LSR" @@ fun a b ->
  if eq_2 (a , b) (t_nat ())
  then ok @@ t_nat ()
  else fail @@ typeclass_error
                 [
                   [t_nat();t_nat()] ;
                 ]
                 [a; b]

let concat = typer_2 "CONCAT" @@ fun a b ->
  if eq_2 (a , b) (t_string ())
  then ok @@ t_string ()
  else if eq_2 (a , b) (t_bytes ())
  then ok @@ t_bytes ()
  else fail @@ typeclass_error
                 [
                   [t_string();t_string()] ;
                   [t_bytes();t_bytes()] ;
                 ]
                 [a; b]

let cons = typer_2 "CONS" @@ fun hd tl ->
  let%bind elt = trace_option (expected_list tl) @@ get_t_list tl in
  let%bind () = assert_eq hd elt in
  ok tl

let convert_to_right_comb = typer_1 "CONVERT_TO_RIGHT_COMB" @@ fun t ->
  match t.type_content with
    | T_record lmap ->
      let kvl = LMap.to_kv_list lmap in
      let%bind () = Michelson_type_converter.field_checks kvl t.location in
      let pair = Michelson_type_converter.convert_pair_to_right_comb kvl in
      ok {t with type_content = pair}
    | T_sum cmap ->
      let kvl = LMap.to_kv_list cmap in
      let%bind () = Michelson_type_converter.field_checks kvl t.location in
      let michelson_or = Michelson_type_converter.convert_variant_to_right_comb kvl in
      ok {t with type_content = michelson_or}
    | _ -> fail @@ wrong_converter t

let convert_to_left_comb = typer_1 "CONVERT_TO_LEFT_COMB" @@ fun t ->
  match t.type_content with
    | T_record lmap ->
      let kvl =  LMap.to_kv_list lmap in
      let%bind () = Michelson_type_converter.field_checks kvl t.location in
      let pair = Michelson_type_converter.convert_pair_to_left_comb kvl in
      ok {t with type_content = pair}
    | T_sum cmap ->
      let kvl = LMap.to_kv_list cmap in
      let%bind () = Michelson_type_converter.field_checks kvl t.location in
      let michelson_or = Michelson_type_converter.convert_variant_to_left_comb kvl in
      ok {t with type_content = michelson_or}
    | _ -> fail @@ wrong_converter t

let convert_from_right_comb = typer_1_opt "CONVERT_FROM_RIGHT_COMB" @@ fun t tv ->
  let%bind dst_t = need_real_type tv t.location in
  match t.type_content with
    | T_record src_lmap ->
      let%bind dst_lmap = trace_option (expected_record dst_t) @@ get_t_record dst_t in
      let%bind record = Michelson_type_converter.convert_pair_from_right_comb src_lmap dst_lmap in
      ok {t with type_content = record}
    | T_sum src_cmap ->
      let%bind dst_cmap = trace_option (expected_variant dst_t) @@ get_t_sum dst_t in
      let%bind variant = Michelson_type_converter.convert_variant_from_right_comb src_cmap dst_cmap in
      ok {t with type_content = variant}
    | _ -> fail @@ wrong_converter t

let convert_from_left_comb = typer_1_opt "CONVERT_FROM_LEFT_COMB" @@ fun t tv ->
  let%bind dst_t = need_real_type tv t.location in
  match t.type_content with
    | T_record src_lmap ->
      let%bind dst_lmap = trace_option (expected_record dst_t) @@ get_t_record dst_t in
      let%bind record = Michelson_type_converter.convert_pair_from_left_comb src_lmap dst_lmap in
      ok {t with type_content = record}
    | T_sum src_cmap ->
      let%bind dst_cmap = trace_option (expected_variant dst_t) @@ get_t_sum dst_t in
      let%bind variant = Michelson_type_converter.convert_variant_from_left_comb src_cmap dst_cmap in
      ok {t with type_content = variant}
    | _ -> fail @@ wrong_converter t

let simple_comparator : string -> typer = fun s -> typer_2 s @@ fun a b ->
  let%bind () =
    Assert.assert_true (uncomparable_types a b) @@
    List.exists (eq_2 (a , b)) [
      t_int () ;
      t_nat () ;
      t_bool () ;
      t_mutez () ;
      t_string () ;
      t_bytes () ;
      t_address () ;
      t_timestamp () ;
      t_key_hash () ;
    ] in
  ok @@ t_bool ()

let rec pair_comparator : string -> typer = fun s -> typer_2 s @@ fun a b ->
  let%bind () =
    Assert.assert_true (uncomparable_types a b) @@ eq_1 a b
  in
  let%bind (a_k, a_v) = 
    trace_option (comparator_composed a) @@
    get_t_pair a in
  let%bind (b_k, b_v) = trace_option (expected_pair b) @@ get_t_pair b in
  let%bind _ = simple_comparator s [a_k;b_k] @@ t_wildcard ()
  in
  comparator s [a_v;b_v] @@ t_wildcard ()
    
and comparator : string -> typer = fun s -> typer_2 s @@ fun a b ->
  bind_or (pair_comparator s [a;b] @@ t_wildcard (), simple_comparator s [a;b] @@ t_wildcard ())

let constant_typers c loc : (typer , typer_error) result = match c with
  | C_INT                 -> ok @@ int ;
  | C_UNIT                -> ok @@ unit loc;
  | C_NOW                 -> ok @@ now loc;
  | C_IS_NAT              -> ok @@ is_nat ;
  | C_SOME                -> ok @@ some ;
  | C_NONE                -> ok @@ none loc ;
  | C_ASSERTION           -> ok @@ assertion ;
  | C_FAILWITH            -> ok @@ failwith_ ;
  (* LOOPS *)
  | C_FOLD_WHILE          -> ok @@ fold_while ;
  | C_FOLD_CONTINUE       -> ok @@ continue ;
  | C_FOLD_STOP           -> ok @@ stop ;
   (* MATH *)
  | C_NEG                 -> ok @@ neg ;
  | C_ABS                 -> ok @@ abs ;
  | C_ADD                 -> ok @@ add ;
  | C_SUB                 -> ok @@ sub ;
  | C_MUL                 -> ok @@ times ;
  | C_EDIV                -> ok @@ ediv ;
  | C_DIV                 -> ok @@ div ;
  | C_MOD                 -> ok @@ mod_ ;
  (* LOGIC *)
  | C_NOT                 -> ok @@ not_ ;
  | C_AND                 -> ok @@ and_ ;
  | C_OR                  -> ok @@ or_ ;
  | C_XOR                 -> ok @@ xor ;
  | C_LSL                 -> ok @@ lsl_;
  | C_LSR                 -> ok @@ lsr_;
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
  (* SET  *)
  | C_SET_EMPTY           -> ok @@ set_empty loc;
  | C_SET_ADD             -> ok @@ set_add ;
  | C_SET_REMOVE          -> ok @@ set_remove ;
  | C_SET_ITER            -> ok @@ set_iter ;
  | C_SET_FOLD            -> ok @@ set_fold ;
  | C_SET_MEM             -> ok @@ set_mem ;

  (* LIST *)
  | C_CONS                -> ok @@ cons ;
  | C_LIST_EMPTY          -> ok @@ list_empty loc;
  | C_LIST_ITER           -> ok @@ list_iter ;
  | C_LIST_MAP            -> ok @@ list_map ;
  | C_LIST_FOLD           -> ok @@ list_fold ;
  (* MAP *)
  | C_MAP_EMPTY           -> ok @@ map_empty loc;
  | C_BIG_MAP_EMPTY       -> ok @@ big_map_empty loc;
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
  | C_CHAIN_ID            -> ok @@ chain_id loc;
  (*BLOCKCHAIN *)
  | C_CONTRACT            -> ok @@ get_contract ;
  | C_CONTRACT_OPT        -> ok @@ get_contract_opt ;
  | C_CONTRACT_ENTRYPOINT -> ok @@ get_entrypoint ;
  | C_CONTRACT_ENTRYPOINT_OPT -> ok @@ get_entrypoint_opt ;
  | C_AMOUNT              -> ok @@ amount loc;
  | C_BALANCE             -> ok @@ balance loc;
  | C_CALL                -> ok @@ transaction ;
  | C_SENDER              -> ok @@ sender loc;
  | C_SOURCE              -> ok @@ source loc;
  | C_ADDRESS             -> ok @@ address;
  | C_SELF                -> ok @@ self;
  | C_SELF_ADDRESS        -> ok @@ self_address loc;
  | C_IMPLICIT_ACCOUNT    -> ok @@ implicit_account;
  | C_SET_DELEGATE        -> ok @@ set_delegate ;
  | C_CREATE_CONTRACT     -> ok @@ create_contract ;
  | C_CONVERT_TO_RIGHT_COMB -> ok @@ convert_to_right_comb ;
  | C_CONVERT_TO_LEFT_COMB  -> ok @@ convert_to_left_comb ;
  | C_CONVERT_FROM_RIGHT_COMB -> ok @@ convert_from_right_comb ;
  | C_CONVERT_FROM_LEFT_COMB  -> ok @@ convert_from_left_comb ;
  | _                     -> fail (corner_case "typer not implemented for constant")
