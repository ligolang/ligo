module H=Helpers
open Trace
open Errors
open Ast_typed
open H

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


let none loc = typer_0 loc "NONE" @@ fun tv_opt ->
  match tv_opt with
  | None -> fail (not_annotated loc)
  | Some t ->
    let* () = trace_option (expected_option loc t) @@ assert_t_option t in
    ok t

let set_empty loc = typer_0 loc "SET_EMPTY" @@ fun tv_opt ->
  match tv_opt with
  | None -> fail (not_annotated loc)
  | Some t ->
    let* () = trace_option (expected_set loc t) @@ assert_t_set t in
    ok t

let set_update loc = typer_3 loc "SET_UPDATE" @@ fun elt flag set ->
  let* elt' = trace_option (expected_set loc set) @@ get_t_set set in
  let* () = trace_option (expected_bool loc flag) @@ assert_t_bool flag in
  let* () = assert_eq loc elt elt' in
  ok set

let sub loc = typer_2 loc "SUB" @@ fun a b ->
  if eq_2 (a , b) (t_bls12_381_g1 ())
  then ok (t_bls12_381_g1 ()) else
  if eq_2 (a , b) (t_bls12_381_g2 ())
  then ok (t_bls12_381_g2 ()) else
  if eq_2 (a , b) (t_bls12_381_fr ())
  then ok (t_bls12_381_fr ()) else
  if (eq_1 a (t_int ()) || eq_1 a (t_nat ()))
  && (eq_1 b (t_int ()) || eq_1 b (t_nat ()))
  then ok @@ t_int () else
  if (eq_2 (a , b) (t_timestamp ()))
  then ok @@ t_int () else
  if (eq_1 a (t_timestamp ()) && eq_1 b (t_int ()))
  then ok @@ t_timestamp () else
  if (eq_2 (a , b) (t_mutez ()))
  then ok @@ t_mutez () else
    fail (bad_subtraction loc)

let some loc = typer_1 loc "SOME" @@ fun a -> ok @@ t_option a

let map_remove loc : typer = typer_2 loc "MAP_REMOVE" @@ fun k m ->
  let* (src , _) = bind_map_or (
      (fun m -> trace_option (expected_map loc m) @@ get_t_map m) ,
      (fun m -> trace_option (expected_big_map loc m) @@ get_t_big_map m)
    ) m in
  let* () = assert_eq loc src k in
  ok m

let map_empty loc = typer_0 loc "MAP_EMPTY" @@ fun tv_opt ->
  match tv_opt with
  | None -> fail (not_annotated loc)
  | Some t ->
    let* (src, dst) = trace_option (expected_map loc t) @@ get_t_map t in
    ok @@ t_map src dst

let big_map_empty loc = typer_0 loc "BIG_MAP_EMPTY" @@ fun tv_opt ->
  match tv_opt with
  | None -> fail (not_annotated loc)
  | Some t ->
    let* (src, dst) = trace_option (expected_big_map loc t) @@ get_t_big_map t in
    ok @@ t_big_map src dst

let big_map_identifier loc = typer_1_opt loc "BIG_MAP_IDENTIFIER" @@ fun id tv_opt  ->
  match tv_opt with
  | None -> fail (not_annotated loc)
  | Some t ->
    let* () = trace_option (expected_nat loc id) @@ get_t_nat id in
    let* (src, dst) = trace_option (expected_big_map loc t) @@ get_t_big_map t in
    ok @@ t_big_map src dst

let map_add loc : typer = typer_3 loc "MAP_ADD" @@ fun k v m ->
  let* (src , dst) = bind_map_or (
      (fun m -> trace_option (expected_map loc m) @@ get_t_map m) ,
      (fun m -> trace_option (expected_big_map loc m) @@ get_t_big_map m)
    ) m in
  let* () = assert_eq loc src k in
  let* () = assert_eq loc dst v in
  ok m

let map_update loc : typer = typer_3 loc "MAP_UPDATE" @@ fun k v m ->
  let* (src , dst) = bind_map_or (
      (fun m -> trace_option (expected_map loc m) @@ get_t_map m) ,
      (fun m -> trace_option (expected_big_map loc m) @@ get_t_big_map m)
    ) m in
  let* () = assert_eq loc src k in
  let* v' = trace_option (expected_option loc v) @@ get_t_option v in
  let* () = assert_eq loc dst v' in
  ok m

let map_mem loc : typer = typer_2 loc "MAP_MEM" @@ fun k m ->
  let* (src , _dst) = bind_map_or (
      (fun m -> trace_option (expected_map loc m) @@ get_t_map m) ,
      (fun m -> trace_option (expected_big_map loc m) @@ get_t_big_map m)
    ) m in
  let* () = assert_eq loc src k in
  ok @@ t_bool ()

let map_find loc : typer = typer_2 loc "MAP_FIND" @@ fun k m ->
  let* (src , dst) = bind_map_or (
      (fun m -> trace_option (expected_map loc m) @@ get_t_map m) ,
      (fun m -> trace_option (expected_big_map loc m) @@ get_t_big_map m)
    ) m in
  let* () = assert_eq loc src k in
  ok @@ dst

let map_find_opt loc : typer = typer_2 loc "MAP_FIND_OPT" @@ fun k m ->
  let* (src , dst) = bind_map_or (
      (fun m -> trace_option (expected_map loc m) @@ get_t_map m) ,
      (fun m -> trace_option (expected_big_map loc m) @@ get_t_big_map m)
    ) m in
  let* () = assert_eq loc src k in
  ok @@ t_option dst

let map_iter loc : typer = typer_2 loc "MAP_ITER" @@ fun f m ->
  let* (k, v) = trace_option (expected_map loc m) @@ get_t_map m in
  let* (arg , res) = trace_option (expected_function loc f) @@ get_t_function f in
  let kv = t_pair k v in
  let unit = t_unit () in
  let* () = assert_eq loc arg kv in
  let* () = assert_eq loc res unit in
  ok @@ t_unit ()

let map_map loc : typer = typer_2 loc "MAP_MAP" @@ fun f m ->
  let* (k, v) = trace_option (expected_map loc m) @@ get_t_map m in
  let* (arg , res) = trace_option (expected_function loc f) @@ get_t_function f in
  let kv = t_pair k v in
  let* () = assert_eq loc arg kv in
  ok @@ t_map k res

let map_get_and_update loc : typer = typer_3 loc "MAP_GET_AND_UPDATE" @@ fun k opt_v m ->
  let* v = trace_option (expected_option loc opt_v) @@ get_t_option opt_v in
  let* (src , dst) = trace_option (expected_map loc m) @@ get_t_map m in
  let* () = assert_eq loc src k in
  let* () = assert_eq loc dst v in
  ok @@ t_pair opt_v m

let big_map_get_and_update loc : typer = typer_3 loc "BIG_MAP_GET_AND_UPDATE" @@ fun k opt_v m ->
  let* v = trace_option (expected_option loc opt_v) @@ get_t_option opt_v in
  let* (src , dst) = trace_option (expected_map loc m) @@ get_t_big_map m in
  let* () = assert_eq loc src k in
  let* () = assert_eq loc dst v in
  ok @@ t_pair opt_v m

let size loc = typer_1 loc "SIZE" @@ fun t ->
  let* () =
    Assert.assert_true (wrong_size loc t) @@
    (is_t_map t || is_t_list t || is_t_string t || is_t_bytes t || is_t_set t ) in
  ok @@ t_nat ()

let slice loc = typer_3 loc "SLICE" @@ fun i j s ->
  let t_nat = t_nat () in
  let* () = assert_eq loc i t_nat in
  let* () = assert_eq loc j t_nat in
  if eq_1 s (t_string ())
  then ok @@ t_string ()
  else if eq_1 s (t_bytes ())
  then ok @@ t_bytes ()
  else fail @@ typeclass_error loc
      [
        [t_nat;t_nat;t_string()] ;
        [t_nat;t_nat;t_bytes()] ;
      ]
      [i ; j ; s]

let failwith_ loc = typer_1_opt loc "FAILWITH" @@ fun t opt ->
  let* _ =
    if eq_1 t (t_string ())
    then ok ()
    else if eq_1 t (t_nat ())
    then ok ()
    else if eq_1 t (t_int ())
    then ok ()
    else
      fail @@ typeclass_error loc
        [
          [t_string()] ;
          [t_nat()] ;
          [t_int()] ;
        ]
        [t] in
  let default = t_unit () in
  ok @@ Simple_utils.Option.value ~default opt

let int loc : typer = typer_1 loc "INT" @@ fun t ->
  if (eq_1 t (t_nat ()) || eq_1 t (t_bls12_381_fr ()))
  then ok (t_int ()) else
    fail @@ typeclass_error loc
              [
                [t_bls12_381_fr()] ;
                [t_nat ()] ;
              ]
              [t]

let bytes_pack loc : typer = typer_1 loc "PACK" @@ fun _t ->
  ok @@ t_bytes ()

let bytes_unpack loc = typer_1_opt loc "UNPACK" @@ fun input output_opt ->
  let* () = trace_option (expected_bytes loc input) @@ assert_t_bytes input in
  trace_option (not_annotated loc) @@ output_opt

let hash256 loc = typer_1 loc "SHA256" @@ fun t ->
  let* () = trace_option (expected_bytes loc t) @@ assert_t_bytes t in
  ok @@ t_bytes ()

let hash512 loc = typer_1 loc "SHA512" @@ fun t ->
  let* () = trace_option (expected_bytes loc t) @@ assert_t_bytes t in
  ok @@ t_bytes ()

let blake2b loc = typer_1 loc "BLAKE2b" @@ fun t ->
  let* () = trace_option (expected_bytes loc t) @@ assert_t_bytes t in
  ok @@ t_bytes ()

let sha3 loc = typer_1 loc "SHA3" @@ fun t ->
  let* () = trace_option (expected_bytes loc t) @@ assert_t_bytes t in
  ok @@ t_bytes ()

let keccak loc = typer_1 loc "KECCAK" @@ fun t ->
  let* () = trace_option (expected_bytes loc t) @@ assert_t_bytes t in
  ok @@ t_bytes ()

let hash_key loc = typer_1 loc "HASH_KEY" @@ fun t ->
  let* () = trace_option (expected_key loc t) @@ assert_t_key t in
  ok @@ t_key_hash ()

let check_signature loc = typer_3 loc "CHECK_SIGNATURE" @@ fun k s b ->
  let* () = trace_option (expected_key loc k) @@ assert_t_key k in
  let* () = trace_option (expected_signature loc s) @@ assert_t_signature s in
  let* () = trace_option (expected_bytes loc b) @@ assert_t_bytes b in
  ok @@ t_bool ()

let sender loc = constant' loc "SENDER" @@ t_address ()

let source loc = constant' loc "SOURCE" @@ t_address ()

let unit loc = constant' loc "UNIT" @@ t_unit ()

let never loc = typer_1_opt loc "NEVER" @@ fun nev tv_opt ->
  let* () = assert_eq loc nev (t_never ()) in
  match tv_opt with
  | None -> fail (not_annotated loc)
  | Some t -> ok @@ t

let amount loc = constant' loc "AMOUNT" @@ t_mutez ()

let balance loc = constant' loc "BALANCE" @@ t_mutez ()

let chain_id loc = constant' loc "CHAIN_ID" @@ t_chain_id ()

let level loc = constant' loc "LEVEL" @@ t_nat ()

let total_voting_power loc = constant' loc "TOTAL_VOTING_POWER" @@ t_nat ()

let voting_power loc = typer_1 loc "VOTING_POWER" @@ fun t ->
  let* () = trace_option (expected_key_hash loc t) @@ assert_t_key_hash t in
  ok @@ t_nat ()

let address loc = typer_1 loc "ADDRESS" @@ fun c ->
  let* () = trace_option (expected_contract loc c) @@ assert_t_contract c in
  ok @@ t_address ()

let self_address loc = typer_0 loc "SELF_ADDRESS" @@ fun _ ->
  ok @@ t_address ()

let self loc = typer_1_opt loc "SELF" @@ fun entrypoint_as_string tv_opt ->
  let* () = trace_option (expected_string loc entrypoint_as_string) @@ assert_t_string entrypoint_as_string in
  match tv_opt with
  | None -> fail (not_annotated loc)
  | Some t -> ok @@ t

let implicit_account loc = typer_1 loc "IMPLICIT_ACCOUNT" @@ fun key_hash ->
  let* () = trace_option (expected_key_hash loc key_hash) @@ assert_t_key_hash key_hash in
  ok @@ t_contract (t_unit () )

let now loc = constant' loc "NOW" @@ t_timestamp ()

let transaction loc = typer_3 loc "CALL" @@ fun param amount contract ->
  let* () = trace_option (expected_mutez loc amount) @@ assert_t_mutez amount in
  let* contract_param = trace_option (expected_contract loc contract) @@ get_t_contract contract in
  let* () = assert_eq loc param contract_param in
  ok @@ t_operation ()

let create_contract loc = typer_4 loc "CREATE_CONTRACT" @@ fun f kh_opt amount init_storage  ->
  let* (args , ret) = trace_option (expected_function loc f) @@ get_t_function f in
  let* (_,s) = trace_option (expected_pair loc args) @@ get_t_pair args in
  let* (oplist,s') = trace_option (expected_pair loc ret) @@ get_t_pair ret in
  let* () = trace_option (expected_mutez loc amount) @@ assert_t_mutez amount in
  let* (delegate) = trace_option (expected_option loc kh_opt) @@ get_t_option kh_opt in
  let* () = assert_eq loc s s' in
  let* () = assert_eq loc s init_storage in
  let* () = trace_option (expected_op_list loc oplist) @@ assert_t_list_operation oplist in
  let* () = trace_option (expected_key_hash loc delegate) @@ assert_t_key_hash delegate in
  ok @@ t_pair (t_operation ()) (t_address ())

let get_contract loc = typer_1_opt loc "CONTRACT" @@ fun addr_tv tv_opt ->
  let t_addr = t_address () in
  let* () = assert_eq loc addr_tv t_addr in
  let* tv = trace_option (not_annotated loc) tv_opt in
  let* tv' = trace_option (expected_contract loc tv) @@ get_t_contract tv in
  ok @@ t_contract tv'

let get_contract_opt loc = typer_1_opt loc "CONTRACT OPT" @@ fun addr_tv tv_opt ->
  let t_addr = t_address () in
  let* () = assert_eq loc addr_tv t_addr in
  let* tv = trace_option (not_annotated loc) tv_opt in
  let* tv = trace_option (expected_option loc tv) @@ get_t_option tv in
  let* tv' = trace_option (expected_contract loc tv) @@ get_t_contract tv in
  ok @@ t_option (t_contract tv')

let get_entrypoint loc = typer_2_opt loc "CONTRACT_ENTRYPOINT" @@ fun entry_tv addr_tv tv_opt ->
  let t_string = t_string () in
  let t_addr = t_address () in
  let* () = assert_eq loc entry_tv t_string in
  let* () = assert_eq loc addr_tv t_addr in
  let* tv = trace_option (not_annotated loc) tv_opt in
  let* tv' = trace_option (expected_contract loc tv) @@ get_t_contract tv in
  ok @@ t_contract tv'

let get_entrypoint_opt loc = typer_2_opt loc "CONTRACT_ENTRYPOINT_OPT" @@ fun entry_tv addr_tv tv_opt ->
  let t_string = t_string () in
  let t_addr = t_address () in
  let* () = assert_eq loc entry_tv t_string in
  let* () = assert_eq loc addr_tv t_addr in
  let* tv = trace_option (not_annotated loc) tv_opt in
  let* tv = trace_option (expected_option loc tv) @@ get_t_option tv in
  let* tv' = trace_option (expected_contract loc tv) @@ get_t_contract tv in
  ok @@ t_option (t_contract tv' )

let set_delegate loc = typer_1 loc "SET_DELEGATE" @@ fun delegate_opt ->
  let kh_opt = (t_option (t_key_hash ()) ) in
  let* () = assert_eq loc delegate_opt kh_opt in
  ok @@ t_operation ()

let abs loc = typer_1 loc "ABS" @@ fun t ->
  let* () = trace_option (expected_int loc t) @@ assert_t_int t in
  ok @@ t_nat ()

let is_nat loc = typer_1 loc "ISNAT" @@ fun t ->
  let* () = trace_option (expected_int loc t) @@ assert_t_int t in
  ok @@ t_option (t_nat ())

let neg loc = typer_1 loc "NEG" @@ fun t ->
  let* () = Assert.assert_true (wrong_neg loc t) @@ (eq_1 t (t_nat ()) || eq_1 t (t_int ())) in
  ok @@ t_int ()

let assertion loc = typer_1 loc "ASSERT" @@ fun a ->
  let* () = trace_option (expected_bool loc a) @@ assert_t_bool a in
  ok @@ t_unit ()

let assert_some loc = typer_1 loc "ASSERT_SOME" @@ fun a ->
  let* () = trace_option (expected_option loc a) @@ assert_t_option a in
  ok @@ t_unit ()

let times loc = typer_2 loc "TIMES" @@ fun a b ->
  if (eq_1 a (t_bls12_381_g1 ()) && eq_1 b (t_bls12_381_fr ()))
  then ok (t_bls12_381_g1 ()) else
  if (eq_1 a (t_bls12_381_g2 ()) && eq_1 b (t_bls12_381_fr ()))
  then ok (t_bls12_381_g2 ()) else
  if (eq_1 a (t_bls12_381_fr ()) && eq_1 b (t_bls12_381_fr ()))
  then ok (t_bls12_381_fr ()) else
  if (eq_1 a (t_nat ()) && eq_1 b (t_bls12_381_fr ()))
  then ok (t_bls12_381_fr ()) else
  if (eq_1 a (t_int ()) && eq_1 b (t_bls12_381_fr ()))
  then ok (t_bls12_381_fr ()) else
  if (eq_1 a (t_bls12_381_fr ()) && eq_1 b (t_nat ()))
  then ok (t_bls12_381_fr ()) else
  if (eq_1 a (t_bls12_381_fr ()) && eq_1 b (t_int ()))
  then ok (t_bls12_381_fr ()) else
  if eq_2 (a , b) (t_nat ())
  then ok @@ t_nat () else
  if eq_2 (a , b) (t_int ())
  then ok @@ t_int () else
  if (eq_1 a (t_nat ()) && eq_1 b (t_mutez ())) || (eq_1 b (t_nat ()) && eq_1 a (t_mutez ()))
  then ok @@ t_mutez () else
  if (eq_1 a (t_nat ()) && eq_1 b (t_int ())) || (eq_1 b (t_nat ()) && eq_1 a (t_int ()))
  then ok @@ t_int () else
    fail @@ typeclass_error loc
              [
                [t_bls12_381_g1();t_bls12_381_g1()] ;
                [t_bls12_381_g2();t_bls12_381_g2()] ;
                [t_bls12_381_fr();t_bls12_381_fr()] ;
                [t_nat();t_bls12_381_fr()] ;
                [t_int();t_bls12_381_fr()] ;
                [t_bls12_381_fr();t_nat()] ;
                [t_bls12_381_fr();t_int()] ;
                [t_nat();t_nat()] ;
                [t_int();t_int()] ;
                [t_nat();t_mutez()] ;
                [t_mutez();t_nat()] ;
                [t_nat();t_int()] ;
                [t_int();t_nat()] ;
              ]
              [a; b]

let ediv loc = typer_2 loc "EDIV" @@ fun a b ->
  if eq_2 (a , b) (t_nat ())
  then ok @@ t_option (t_pair (t_nat ()) (t_nat ()) ) else
  if eq_2 (a , b) (t_int ())
  then ok @@ t_option (t_pair (t_int ()) (t_nat ()) ) else
  if eq_1 a (t_nat ()) && eq_1 b (t_int ())
  then ok @@ t_option (t_pair (t_int ()) (t_nat ()) ) else
  if eq_1 a (t_int ()) && eq_1 b (t_nat ())
  then ok @@ t_option (t_pair (t_int ()) (t_nat ()) ) else
  if eq_1 a (t_mutez ()) && eq_1 b (t_mutez ())
  then ok @@ t_option (t_pair (t_nat ()) (t_mutez ()) ) else
  if eq_1 a (t_mutez ()) && eq_1 b (t_nat ())
  then ok @@ t_option (t_pair (t_mutez ()) (t_mutez ()) ) else
    fail @@ typeclass_error loc
      [
        [t_nat();t_nat()] ;
        [t_int();t_int()] ;
        [t_nat();t_int()] ;
        [t_int();t_nat()] ;
        [t_mutez();t_nat()] ;
        [t_mutez();t_mutez()] ;
      ]
      [a; b]

let div loc = typer_2 loc "DIV" @@ fun a b ->
  if eq_2 (a , b) (t_nat ())
  then ok @@ t_nat () else
  if eq_2 (a , b) (t_int ())
  then ok @@ t_int () else
  if eq_1 a (t_int ()) && eq_1 b (t_nat ())
  then ok @@ t_int () else
  if eq_1 a (t_nat ()) && eq_1 b (t_int ())
  then ok @@ t_int () else
  if eq_1 a (t_mutez ()) && eq_1 b (t_nat ())
  then ok @@ t_mutez () else
  if eq_1 a (t_mutez ()) && eq_1 b (t_mutez ())
  then ok @@ t_nat () else
    fail @@ typeclass_error loc
      [
        [t_nat();t_nat()] ;
        [t_int();t_int()] ;
        [t_nat();t_int()] ;
        [t_int();t_nat()] ;
        [t_mutez();t_nat()] ;
        [t_mutez();t_mutez()] ;
      ]
      [a; b]

let mod_ loc = typer_2 loc "MOD" @@ fun a b ->
  if (eq_1 a (t_nat ()) || eq_1 a (t_int ())) && (eq_1 b (t_nat ()) || eq_1 b (t_int ()))
  then ok @@ t_nat () else
  if eq_1 a (t_mutez ()) && eq_1 b (t_mutez ())
  then ok @@ t_mutez () else
  if eq_1 a (t_mutez ()) && eq_1 b (t_nat ())
  then ok @@ t_mutez () else
    fail @@ typeclass_error loc
      [
        [t_nat();t_nat()] ;
        [t_nat();t_int()] ;
        [t_int();t_nat()] ;
        [t_int();t_int()] ;
        [t_mutez();t_nat()] ;
        [t_mutez();t_mutez()] ;
      ]
      [a; b]

let add loc = typer_2 loc "ADD" @@ fun a b ->
  if eq_2 (a , b) (t_bls12_381_g1 ())
  then ok (t_bls12_381_g1 ()) else
  if eq_2 (a , b) (t_bls12_381_g2 ())
  then ok (t_bls12_381_g2 ()) else
  if eq_2 (a , b) (t_bls12_381_fr ())
  then ok (t_bls12_381_fr ()) else
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
    fail @@ typeclass_error loc
              [ 
                [t_bls12_381_g1();t_bls12_381_g1()] ;
                [t_bls12_381_g2();t_bls12_381_g2()] ;
                [t_bls12_381_fr();t_bls12_381_fr()] ;
                [t_nat();t_nat()] ;
                [t_int();t_int()] ;
                [t_mutez();t_mutez()] ;
                [t_nat();t_int()] ;
                [t_int();t_nat()] ;
                [t_timestamp();t_int()] ;
                [t_int();t_timestamp()] ;
              ]
              [a; b]

let polymorphic_add loc = typer_2 loc "POLYMORPHIC_ADD" @@ fun a b ->
  if eq_2 (a , b) (t_string ())
  then ok @@ t_string () else
  if eq_2 (a , b) (t_bls12_381_g1 ())
  then ok (t_bls12_381_g1 ()) else
  if eq_2 (a , b) (t_bls12_381_g2 ())
  then ok (t_bls12_381_g2 ()) else
  if eq_2 (a , b) (t_bls12_381_fr ())
  then ok (t_bls12_381_fr ()) else
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
    fail @@ typeclass_error loc
              [ 
                [t_string();t_string()] ;
                [t_bls12_381_g1();t_bls12_381_g1()] ;
                [t_bls12_381_g2();t_bls12_381_g2()] ;
                [t_bls12_381_fr();t_bls12_381_fr()] ;
                [t_nat();t_nat()] ;
                [t_int();t_int()] ;
                [t_mutez();t_mutez()] ;
                [t_nat();t_int()] ;
                [t_int();t_nat()] ;
                [t_timestamp();t_int()] ;
                [t_int();t_timestamp()] ;
              ]
              [a; b]

let set_mem loc = typer_2 loc "SET_MEM" @@ fun elt set ->
  let* key = trace_option (expected_set loc set) @@ get_t_set set in
  let* () = assert_eq loc elt key in
  ok @@ t_bool ()

let set_add loc = typer_2 loc "SET_ADD" @@ fun elt set ->
  let* key = trace_option (expected_set loc set) @@ get_t_set set in
  let* () = assert_eq loc elt key in
  ok set

let set_remove loc = typer_2 loc "SET_REMOVE" @@ fun elt set ->
  let* key = trace_option (expected_set loc set) @@ get_t_set set in
  let* () = assert_eq loc elt key in
  ok set

let set_iter loc = typer_2 loc "SET_ITER" @@ fun body set ->
  let* (arg , res) = trace_option (expected_function loc body) @@ get_t_function body in
  let* () = assert_eq loc res (t_unit ()) in
  let* key = trace_option (expected_set loc set) @@ get_t_set set in
  let* () = assert_eq loc key arg in
  ok (t_unit ())

let list_empty loc = typer_0 loc "LIST_EMPTY" @@ fun tv_opt ->
  match tv_opt with
  | None -> fail (not_annotated loc)
  | Some t ->
    let* () = trace_option (expected_list loc t) @@ assert_t_list t in
    ok t

let list_iter loc = typer_2 loc "LIST_ITER" @@ fun body lst ->
  let* (arg , res) = trace_option (expected_function loc body) @@ get_t_function body in
  let* () = assert_eq loc res (t_unit ()) in
  let* key = trace_option (expected_list loc lst) @@ get_t_list lst in
  let* () = assert_eq loc key arg in
  ok (t_unit ())

let list_map loc = typer_2 loc "LIST_MAP" @@ fun body lst ->
  let* (arg , res) = trace_option (expected_function loc body) @@ get_t_function body in
  let* key = trace_option (expected_list loc lst) @@ get_t_list lst in
  let* () = assert_eq loc key arg in
  ok (t_list res )

let fold loc = typer_3 loc "FOLD" @@ fun body container init ->
  let* (arg , res) = trace_option (expected_function loc body) @@ get_t_function body in
  let* (prec , cur) = trace_option (expected_pair loc arg) @@ get_t_pair arg in
  let* key = trace_option (expected_list loc container) @@ Option.map_pair_or (get_t_list,get_t_set) container in
  let* () = assert_eq loc key cur in
  let* () = assert_eq loc prec res in
  let* () = assert_eq loc res init in
  ok res
let list_fold loc = typer_3 loc "LIST_FOLD" @@ fun body lst init ->
  let* (arg , res) = trace_option (expected_function loc body) @@ get_t_function body in
  let* (prec , cur) = trace_option (expected_pair loc arg) @@ get_t_pair arg in
  let* key = trace_option (expected_list loc lst) @@ get_t_list lst in
  let* () = assert_eq loc key cur in
  let* () = assert_eq loc prec res in
  let* () = assert_eq loc res init in
  ok res

let list_fold_left loc = typer_3 loc "LIST_FOLD_LEFT" @@ fun body init lst ->
  let* (arg , res) = trace_option (expected_function loc body) @@ get_t_function body in
  let* (prec , cur) = trace_option (expected_pair loc arg) @@ get_t_pair arg in
  let* key = trace_option (expected_list loc lst) @@ get_t_list lst in
  let* () = assert_eq loc key cur in
  let* () = assert_eq loc prec res in
  let* () = assert_eq loc res init in
  ok res

let list_fold_right loc = typer_3 loc "LIST_FOLD_RIGHT" @@ fun body lst init ->
  let* (arg , res) = trace_option (expected_function loc body) @@ get_t_function body in
  let* (cur , prec) = trace_option (expected_pair loc arg) @@ get_t_pair arg in
  let* key = trace_option (expected_list loc lst) @@ get_t_list lst in
  let* () = assert_eq loc key cur in
  let* () = assert_eq loc prec res in
  let* () = assert_eq loc res init in
  ok res

let list_head_opt loc = typer_1 loc "LIST_HEAD_OPT" @@ fun lst ->
  let* key = trace_option (expected_list loc lst) @@ get_t_list lst in
  ok @@ t_option ~loc key

let list_tail_opt loc = typer_1 loc "LIST_TAIL_OPT" @@ fun lst ->
  let* key = trace_option (expected_list loc lst) @@ get_t_list lst in
  ok @@ t_option ~loc @@ t_list ~loc key

let set_fold loc = typer_3 loc "SET_FOLD" @@ fun body lst init ->
  let* (arg , res) = trace_option (expected_function loc body) @@ get_t_function body in
  let* (prec , cur) = trace_option (expected_pair loc arg) @@ get_t_pair arg in
  let* key = trace_option (expected_set loc lst) @@ get_t_set lst in
  let* () = assert_eq loc key cur in
  let* () = assert_eq loc prec res in
  let* () = assert_eq loc res init in
  ok res

let set_fold_desc loc = typer_3 loc "SET_FOLD_DESC" @@ fun body lst init ->
  let* (arg , res) = trace_option (expected_function loc body) @@ get_t_function body in
  let* (cur , prec) = trace_option (expected_pair loc arg) @@ get_t_pair arg in
  let* key = trace_option (expected_set loc lst) @@ get_t_set lst in
  let* () = assert_eq loc key cur in
  let* () = assert_eq loc prec res in
  let* () = assert_eq loc res init in
  ok res

let map_fold loc = typer_3 loc "MAP_FOLD" @@ fun body map init ->
  let* (arg , res) = trace_option (expected_function loc body) @@ get_t_function body in
  let* (prec , cur) = trace_option (expected_pair loc arg) @@ get_t_pair arg in
  let* (key , value) = trace_option (expected_map loc map) @@ get_t_map map in
  let kv = t_pair key value in
  let* () = assert_eq loc kv cur in
  let* () = assert_eq loc prec res in
  let* () = assert_eq loc res init in
  ok res

(** FOLD_WHILE is a fold operation that takes an initial value of a certain type
    and then iterates on it until a condition is reached. The auxillary function
    that does the fold returns either boolean true or boolean false to indicate
    whether the fold should continue or not. Necessarily then the initial value
    must match the input parameter of the auxillary function, and the auxillary
    should return type (bool * input) *)
let fold_while loc = typer_2 loc "FOLD_WHILE" @@ fun body init ->
  let* (arg, result) = trace_option (expected_function loc body) @@ get_t_function body in
  let* () = assert_eq loc arg init in
  let* () = assert_eq loc (t_pair (t_bool ()) init) result
  in ok init

(* Continue and Stop are just syntactic sugar for building a pair (bool * a') *)
let continue loc = typer_1 loc "CONTINUE" @@ fun arg ->
  ok @@ t_pair (t_bool ()) arg

let stop loc = typer_1 loc "STOP" @@ fun arg ->
  ok (t_pair (t_bool ()) arg)

let not_ loc = typer_1 loc "NOT" @@ fun elt ->
  if eq_1 elt (t_bool ())
  then ok @@ t_bool ()
  else if eq_1 elt (t_nat ()) || eq_1 elt (t_int ())
  then ok @@ t_int ()
  else fail @@ wrong_not loc elt

let or_ loc = typer_2 loc "OR" @@ fun a b ->
  if eq_2 (a , b) (t_bool ())
  then ok @@ t_bool ()
  else if eq_2 (a , b) (t_nat ())
  then ok @@ t_nat ()
  else fail @@ typeclass_error loc
      [
        [t_bool();t_bool()] ;
        [t_nat();t_nat()] ;
      ]
      [a; b]

let xor loc = typer_2 loc "XOR" @@ fun a b ->
  if eq_2 (a , b) (t_bool ())
  then ok @@ t_bool ()
  else if eq_2 (a , b) (t_nat ())
  then ok @@ t_nat ()
  else fail @@ typeclass_error loc
      [
        [t_bool();t_bool()] ;
        [t_nat();t_nat()] ;
      ]
      [a; b]

let and_ loc = typer_2 loc "AND" @@ fun a b ->
  if eq_2 (a , b) (t_bool ())
  then ok @@ t_bool ()
  else if eq_2 (a , b) (t_nat ()) || (eq_1 b (t_nat ()) && eq_1 a (t_int ()))
  then ok @@ t_nat ()
  else fail @@ typeclass_error loc
      [
        [t_bool();t_bool()] ;
        [t_nat();t_nat()] ;
        [t_int();t_nat()] ;
      ]
      [a; b]

let lsl_ loc = typer_2 loc "LSL" @@ fun a b ->
  if eq_2 (a , b) (t_nat ())
  then ok @@ t_nat ()
  else fail @@ typeclass_error loc
      [
        [t_nat();t_nat()] ;
      ]
      [a; b]

let lsr_ loc = typer_2 loc "LSR" @@ fun a b ->
  if eq_2 (a , b) (t_nat ())
  then ok @@ t_nat ()
  else fail @@ typeclass_error loc
      [
        [t_nat();t_nat()] ;
      ]
      [a; b]

let concat loc = typer_2 loc "CONCAT" @@ fun a b ->
  if eq_2 (a , b) (t_string ())
  then ok @@ t_string ()
  else if eq_2 (a , b) (t_bytes ())
  then ok @@ t_bytes ()
  else fail @@ typeclass_error loc
      [
        [t_string();t_string()] ;
        [t_bytes();t_bytes()] ;
      ]
      [a; b]

let cons loc = typer_2 loc "CONS" @@ fun hd tl ->
  let* elt = trace_option (expected_list loc tl) @@ get_t_list tl in
  let* () = assert_eq loc hd elt in
  ok tl

let convert_to_right_comb loc = typer_1 loc "CONVERT_TO_RIGHT_COMB" @@ fun t ->
  match t.type_content with
  | T_record lmap ->
    let kvl = LMap.to_kv_list_rev lmap.content in
    let* () = Michelson_type_converter.field_checks kvl loc in
    let pair = Michelson_type_converter.convert_pair_to_right_comb kvl in
    ok {t with type_content = pair}
  | T_sum cmap ->
    let kvl = LMap.to_kv_list_rev cmap.content in
    let* () = Michelson_type_converter.field_checks kvl loc in
    let michelson_or = Michelson_type_converter.convert_variant_to_right_comb kvl in
    ok {t with type_content = michelson_or}
  | _ -> fail @@ wrong_converter t

let convert_to_left_comb loc = typer_1 loc "CONVERT_TO_LEFT_COMB" @@ fun t ->
  match t.type_content with
  | T_record lmap ->
    let kvl =  LMap.to_kv_list_rev lmap.content in
    let* () = Michelson_type_converter.field_checks kvl loc in
    let pair = Michelson_type_converter.convert_pair_to_left_comb kvl in
    ok {t with type_content = pair}
  | T_sum cmap ->
    let kvl = LMap.to_kv_list_rev cmap.content in
    let* () = Michelson_type_converter.field_checks kvl loc in
    let michelson_or = Michelson_type_converter.convert_variant_to_left_comb kvl in
    ok {t with type_content = michelson_or}
  | _ -> fail @@ wrong_converter t

let convert_from_right_comb loc = typer_1_opt loc "CONVERT_FROM_RIGHT_COMB" @@ fun t opt ->
  let* dst_t = trace_option (not_annotated loc) opt in
  match t.type_content with
  | T_record {content=src_lmap;_} ->
    let* dst_lmap = trace_option (expected_record loc dst_t) @@ get_t_record dst_t in
    let* record = Michelson_type_converter.convert_pair_from_right_comb src_lmap dst_lmap.content in
    ok {t with type_content = record}
  | T_sum src_cmap ->
    let* dst_cmap = trace_option (expected_variant loc dst_t) @@ get_t_sum dst_t in
    let* variant = Michelson_type_converter.convert_variant_from_right_comb src_cmap.content dst_cmap.content in
    ok {t with type_content = variant}
  | _ -> fail @@ wrong_converter t

let convert_from_left_comb loc = typer_1_opt loc "CONVERT_FROM_LEFT_COMB" @@ fun t opt ->
  let* dst_t = trace_option (not_annotated loc) opt in
  match t.type_content with
  | T_record {content=src_lmap;_} ->
    let* dst_lmap = trace_option (expected_record loc dst_t) @@ get_t_record dst_t in
    let* record = Michelson_type_converter.convert_pair_from_left_comb src_lmap dst_lmap.content in
    ok {t with type_content = record}
  | T_sum src_cmap ->
    let* dst_cmap = trace_option (expected_variant loc dst_t) @@ get_t_sum dst_t in
    let* variant = Michelson_type_converter.convert_variant_from_left_comb src_cmap.content dst_cmap.content in
    ok {t with type_content = variant}
  | _ -> fail @@ wrong_converter t

let simple_comparator : Location.t -> string -> typer = fun loc s -> typer_2 loc s @@ fun a b ->
  let* () =
    Assert.assert_true (uncomparable_types loc a b) @@
    List.exists ~f:(eq_2 (a , b)) [
      t_address () ;
      t_bool () ;
      t_bytes () ;
      t_chain_id ();
      t_int () ;
      t_key () ;
      t_key_hash () ;
      t_mutez () ;
      t_nat () ;
      t_signature ();
      t_string () ;
      t_timestamp () ;
      t_unit ();
      t_never ();
    ] in
  ok @@ t_bool ()

let rec record_comparator : Location.t -> string -> typer = fun loc s -> typer_2 loc s @@ fun a b ->
  let* () =
    Assert.assert_true (uncomparable_types loc a b) @@ eq_1 a b
  in
  let* a_r =
    trace_option (comparator_composed loc a) @@
    get_t_record a in
  let* b_r = trace_option (expected_variant loc b) @@ get_t_record b in
  let aux (a,b) : (type_expression, typer_error) result =
    comparator loc s [a.associated_type;b.associated_type] None
  in
  let* _ = bind_map_list aux @@ List.zip_exn (LMap.to_list a_r.content) (LMap.to_list b_r.content) in
  ok @@ t_bool ()

and sum_comparator : Location.t -> string -> typer = fun loc s -> typer_2 loc s @@ fun a b ->
  let* () =
    Assert.assert_true (uncomparable_types loc a b) @@ eq_1 a b
  in
  let* a_r =
    trace_option (comparator_composed loc a) @@
    get_t_sum a in
  let* b_r = trace_option (expected_variant loc b) @@ get_t_sum b in
  let aux (a,b) : (type_expression, typer_error) result =
    comparator loc s [a.associated_type;b.associated_type] None
  in
  let* _ = bind_map_list aux @@ List.zip_exn (LMap.to_list a_r.content) (LMap.to_list b_r.content) in
  ok @@ t_bool ()

and option_comparator : Location.t -> string -> typer = fun loc s -> typer_2 loc s @@ fun a_opt b_opt ->
  let* () =
    Assert.assert_true (uncomparable_types loc a_opt b_opt) @@ eq_1 a_opt b_opt
  in
  let* a =
    trace_option (comparator_composed loc a_opt) @@
    get_t_option a_opt in
  let* b = trace_option (expected_option loc b_opt) @@ get_t_option b_opt in
  comparator loc s [a;b] None

and comparator : Location.t -> string -> typer = fun loc s -> typer_2 loc s @@ fun a b ->

  bind_or (
    bind_or (simple_comparator loc s [a;b] None, option_comparator loc s [a;b] None),
    bind_or (record_comparator loc s [a;b] None, sum_comparator loc s [a;b] None)
  )

let ticket loc = typer_2 loc "TICKET" @@ fun dat amt ->
  let* () = assert_eq loc amt (t_nat ()) in
  ok @@ t_ticket dat

let read_ticket loc = typer_1 loc "READ_TICKET" @@ fun ticket ->
  let* payload = trace_option (expected_ticket loc ticket) @@ get_t_ticket ticket in
  ok @@ t_pair (t_pair (t_address ()) (t_pair payload (t_nat ()))) ticket

let split_ticket loc = typer_2 loc "SPLIT_TICKET" @@ fun ticket amts ->
  let t_nat = t_nat () in
  let* (a,b) = trace_option (expected_pair loc amts) @@ get_t_pair amts in
  let* () = assert_eq loc a t_nat in
  let* () = assert_eq loc b t_nat in
  let* _ = trace_option (expected_ticket loc ticket) @@ get_t_ticket ticket in
  ok @@ t_option (t_pair ticket ticket)

let join_ticket loc = typer_1 loc "JOIN_TICKET" @@ fun ticks ->
  let* (ticka,tickb) = trace_option (expected_pair loc ticks) @@ get_t_pair ticks in
  let* data = trace_option (expected_ticket loc ticka) @@ get_t_ticket ticka in
  let* datb = trace_option (expected_ticket loc tickb) @@ get_t_ticket tickb in
  let* () = assert_eq loc data datb in
  ok @@ t_option ticka

let pairing_check loc = typer_1 loc "PAIRING_CHECK" @@ fun lst ->
  let* p = trace_option (expected_list loc lst) @@ get_t_list lst in
  let* (g1,g2) = trace_option (expected_list loc p) @@ get_t_pair p in
  let* () = assert_eq loc g1 (t_bls12_381_g1 ()) in (*TODO expected_tbls .. ? *)
  let* () = assert_eq loc g2 (t_bls12_381_g2 ()) in
  ok (t_bool ())

let sapling_verify_update loc = typer_2 loc "SAPLING_VERIFY_UPDATE" @@ fun tr state ->
  let* singleton_tr = trace_option (expected_sapling_transaction loc tr) @@ get_t_sapling_transaction tr in
  let* singleton_state = trace_option (expected_sapling_state loc state) @@ get_t_sapling_state state in
  let* () = assert_eq loc singleton_tr singleton_state in
  ok (t_option (t_pair (t_int ()) state))

let sapling_empty_state loc = typer_0 loc "SAPLING_EMPTY_STATE" @@ fun tv_opt ->
  trace_option (not_annotated loc) @@ tv_opt

let test_originate loc = typer_3 loc "TEST_ORIGINATE" @@ fun main storage balance ->
  let* in_ty,_ = trace_option (expected_function loc main) @@ get_t_function main in
  let* param_ty,storage_ty = trace_option (expected_pair loc in_ty) @@ get_t_pair in_ty in
  let* () = assert_eq loc balance (t_mutez ()) in
  let* () = assert_eq loc storage storage_ty in
  ok (t_triplet (t_typed_address param_ty storage_ty) (t_michelson_code ()) (t_int ()))

let test_compile_expression_subst loc = typer_3 loc "TEST_COMPILE_EXPRESSION_SUBST" @@ fun source_file_opt ligo_insertion subst_list ->
  let* subst_pair = trace_option (expected_list loc subst_list) @@ get_t_list subst_list in
  let* (str,code) = trace_option (expected_pair loc subst_pair) @@ get_t_pair subst_pair in
  let* source_file = trace_option (expected_option loc source_file_opt) @@ get_t_option source_file_opt in
  let* () = trace_option (expected_michelson_code loc code) @@ get_t_michelson_code code in
  let* () = trace_option (expected_string loc str) @@ get_t_string str in
  let* () = trace_option (expected_string loc source_file) @@ assert_t_string source_file in
  let* () = trace_option (expected_ligo_code loc ligo_insertion) @@ assert_t_ligo_code ligo_insertion in
  ok (t_michelson_code ())

let test_compile_expression loc = typer_2 loc "TEST_COMPILE_EXPRESSION" @@ fun source_file_opt ligo_insertion ->
  let* source_file = trace_option (expected_option loc source_file_opt) @@ get_t_option source_file_opt in
  let* () = trace_option (expected_string loc source_file) @@ assert_t_string source_file in
  let* () = trace_option (expected_ligo_code loc ligo_insertion) @@ assert_t_ligo_code ligo_insertion in
  ok (t_michelson_code ())

let test_state_reset loc = typer_2 loc "TEST_STATE_RESET" @@ fun n amts ->
  let* amt = trace_option (expected_list loc amts) @@ get_t_list amts in
  let* () = trace_option (expected_nat loc amt) @@ get_t_nat amt in
  let* () = trace_option (expected_nat loc n) @@ get_t_nat n in
  ok (t_unit ())

let test_set_now loc = typer_1 loc "TEST_SET_NOW" @@ fun time ->
  let* () = assert_eq loc time (t_timestamp ()) in
  ok (t_unit ())

let test_set_source loc = typer_1 loc "TEST_SET" @@ fun s ->
  let* () = assert_eq loc s (t_address ()) in
  ok (t_unit ())

let test_get_nth loc = typer_1 loc "TEST_GET_NTH" @@ fun n ->
  let* () = trace_option (expected_int loc n) @@ assert_t_int n in
  ok (t_address ())

let test_external_call_to_contract_exn loc = typer_3 loc "TEST_EXTERNAL_CALL_TO_CONTRACT_EXN" @@ fun addr p amt  ->
  let* contract_ty = trace_option (expected_contract loc addr) @@ get_t_contract addr in
  let* () = assert_eq loc amt (t_mutez ()) in
  let* () = assert_eq loc p contract_ty in
  ok (t_unit ())

let test_external_call_to_contract loc = typer_3 loc "TEST_EXTERNAL_CALL_TO_CONTRACT" @@ fun addr p amt  ->
  let* contract_ty = trace_option (expected_contract loc addr) @@ get_t_contract addr in
  let* () = assert_eq loc amt (t_mutez ()) in
  let* () = assert_eq loc p contract_ty in
  ok (t_test_exec_result ())

let test_external_call_to_address_exn loc = typer_3 loc "TEST_EXTERNAL_CALL_TO_ADDRESS_EXN" @@ fun addr p amt  ->
  let* () = assert_eq loc addr (t_address ()) in
  let* () = assert_eq loc amt (t_mutez ()) in
  let* () = assert_eq loc p (t_michelson_code ()) in
  ok (t_unit ())

let test_external_call_to_address loc = typer_3 loc "TEST_EXTERNAL_CALL_TO_ADDRESS" @@ fun addr p amt  ->
  let* () = assert_eq loc addr (t_address ()) in
  let* () = assert_eq loc amt (t_mutez ()) in
  let* () = assert_eq loc p (t_michelson_code ()) in
  ok (t_test_exec_result ())

let test_get_storage loc = typer_1 loc "TEST_GET_STORAGE" @@ fun c ->
  let* (_, storage_ty) = trace_option (expected_contract loc (t_unit ())) @@ get_t_typed_address c in
  ok storage_ty

let test_get_storage_of_address loc = typer_1 loc "TEST_GET_STORAGE_OF_ADDRESS" @@ fun addr ->
  let* () = assert_eq loc addr (t_address ()) in
  ok (t_michelson_code ())

let test_get_balance loc = typer_1 loc "TEST_GET_BALANCE" @@ fun addr ->
  let* () = assert_eq loc addr (t_address ()) in
  ok (t_mutez ())

let test_michelson_equal loc = typer_2 loc "TEST_ASSERT_EQUAL" @@ fun x y ->
  let* () = trace_option (expected_michelson_code loc x) @@ assert_t_michelson_code x in
  let* () = trace_option (expected_michelson_code loc y) @@ assert_t_michelson_code y in
  ok (t_bool ())

let test_log loc = typer_1 loc "TEST_LOG" @@ fun _ -> ok @@ t_unit ()

let test_last_originations loc = typer_1 loc "TEST_LAST_ORIGINATIONS" @@ fun u ->
  let* () = trace_option (expected_unit loc u) @@ assert_t_unit u in
  ok (t_map (t_address ()) (t_list (t_address ())))

let test_compile_meta_value loc = typer_1 loc "TEST_LAST_ORIGINATIONS" @@ fun _ ->
  ok (t_michelson_code ())

let test_run loc = typer_2 loc "TEST_RUN" @@ fun _ _ ->
  ok (t_michelson_code ())

let test_eval loc = typer_1 loc "TEST_EVAL" @@ fun _ ->
  ok (t_michelson_code ())

let test_to_contract loc = typer_1 loc "TEST_TO_CONTRACT" @@ fun t ->
  let* param_ty, _ = trace_option (expected_michelson_code loc t) @@
                       get_t_typed_address t in
  let param_ty = Option.value (Ast_typed.Helpers.get_entrypoint "default" param_ty) ~default:param_ty in
  ok (t_contract param_ty)

let test_to_entrypoint loc = typer_2_opt loc "TEST_TO_ENTRYPOINT" @@ fun entry_tv contract_tv tv_opt ->
  let t_string = t_string () in
  let* () = assert_eq loc entry_tv t_string in
  let* _ = trace_option (expected_contract loc contract_tv) @@
             get_t_typed_address contract_tv in
  let* tv = trace_option (not_annotated loc) tv_opt in
  let* tv' = trace_option (expected_contract loc tv) @@ get_t_contract tv in
  ok @@ t_contract tv'

let test_originate_from_file loc = typer_4 loc "TEST_ORIGINATE_FROM_FILE" @@ fun source_file entrypoint storage balance ->
  let* () = trace_option (expected_string loc source_file) @@ assert_t_string source_file in
  let* () = trace_option (expected_string loc entrypoint) @@ assert_t_string entrypoint in
  let* () = trace_option (expected_michelson_code loc storage) @@ assert_t_michelson_code storage in
  let* () = assert_eq loc balance (t_mutez ()) in
  ok (t_triplet (t_address ()) (t_michelson_code ()) (t_int ()))

let test_compile_contract loc = typer_1 loc "TEST_COMPILE_CONTRACT" @@ fun _ ->
  ok (t_michelson_code ())

let constant_typers loc c : (typer , typer_error) result = match c with
  | C_INT                 -> ok @@ int loc ;
  | C_UNIT                -> ok @@ unit loc ;
  | C_NEVER               -> ok @@ never loc ;
  | C_NOW                 -> ok @@ now loc ;
  | C_IS_NAT              -> ok @@ is_nat loc ;
  | C_SOME                -> ok @@ some loc ;
  | C_NONE                -> ok @@ none loc ;
  | C_ASSERTION           -> ok @@ assertion loc ;
  | C_ASSERT_SOME         -> ok @@ assert_some loc ;
  | C_FAILWITH            -> ok @@ failwith_ loc ;
    (* LOOPS *)
  | C_FOLD_WHILE          -> ok @@ fold_while loc ;
  | C_FOLD_CONTINUE       -> ok @@ continue loc ;
  | C_FOLD_STOP           -> ok @@ stop loc ;
  | C_FOLD                -> ok @@ fold loc ;
   (* MATH *)
  | C_NEG                 -> ok @@ neg loc ;
  | C_ABS                 -> ok @@ abs loc ;
  | C_ADD                 -> ok @@ add loc ;
  | C_SUB                 -> ok @@ sub loc ;
  | C_MUL                 -> ok @@ times loc ;
  | C_EDIV                -> ok @@ ediv loc ;
  | C_DIV                 -> ok @@ div loc ;
  | C_MOD                 -> ok @@ mod_ loc ;
    (* LOGIC *)
  | C_NOT                 -> ok @@ not_ loc ;
  | C_AND                 -> ok @@ and_ loc ;
  | C_OR                  -> ok @@ or_ loc ;
  | C_XOR                 -> ok @@ xor loc ;
  | C_LSL                 -> ok @@ lsl_ loc;
  | C_LSR                 -> ok @@ lsr_ loc;
    (* COMPARATOR *)
  | C_EQ                  -> ok @@ comparator loc "EQ" ;
  | C_NEQ                 -> ok @@ comparator loc "NEQ" ;
  | C_LT                  -> ok @@ comparator loc "LT" ;
  | C_GT                  -> ok @@ comparator loc "GT" ;
  | C_LE                  -> ok @@ comparator loc "LE" ;
  | C_GE                  -> ok @@ comparator loc "GE" ;
    (* BYTES / STRING *)
  | C_SIZE                -> ok @@ size loc ;
  | C_CONCAT              -> ok @@ concat loc ;
  | C_SLICE               -> ok @@ slice loc ;
  | C_BYTES_PACK          -> ok @@ bytes_pack loc ;
  | C_BYTES_UNPACK        -> ok @@ bytes_unpack loc ;
    (* SET  *)
  | C_SET_EMPTY           -> ok @@ set_empty loc;
  | C_SET_ADD             -> ok @@ set_add loc ;
  | C_SET_REMOVE          -> ok @@ set_remove loc ;
  | C_SET_ITER            -> ok @@ set_iter loc ;
  | C_SET_FOLD            -> ok @@ set_fold loc ;
  | C_SET_FOLD_DESC       -> ok @@ set_fold_desc loc ;
  | C_SET_MEM             -> ok @@ set_mem loc ;
  | C_SET_UPDATE          -> ok @@ set_update loc;
  (* LIST *)
  | C_CONS                -> ok @@ cons loc ;
  | C_LIST_EMPTY          -> ok @@ list_empty loc;
  | C_LIST_ITER           -> ok @@ list_iter loc ;
  | C_LIST_MAP            -> ok @@ list_map loc ;
  | C_LIST_FOLD           -> ok @@ list_fold loc ;
  | C_LIST_FOLD_LEFT      -> ok @@ list_fold_left loc ;
  | C_LIST_FOLD_RIGHT     -> ok @@ list_fold_right loc ;
  | C_LIST_HEAD_OPT       -> ok @@ list_head_opt loc;
  | C_LIST_TAIL_OPT       -> ok @@ list_tail_opt loc;
    (* MAP *)
  | C_MAP_EMPTY           -> ok @@ map_empty loc;
  | C_BIG_MAP_EMPTY       -> ok @@ big_map_empty loc;
  | C_BIG_MAP_IDENTIFIER  -> ok @@ big_map_identifier loc;
  | C_MAP_ADD             -> ok @@ map_add loc ;
  | C_MAP_REMOVE          -> ok @@ map_remove loc ;
  | C_MAP_UPDATE          -> ok @@ map_update loc ;
  | C_MAP_ITER            -> ok @@ map_iter loc ;
  | C_MAP_MAP             -> ok @@ map_map loc ;
  | C_MAP_FOLD            -> ok @@ map_fold loc ;
  | C_MAP_MEM             -> ok @@ map_mem loc ;
  | C_MAP_FIND            -> ok @@ map_find loc ;
  | C_MAP_FIND_OPT        -> ok @@ map_find_opt loc ;
  | C_MAP_GET_AND_UPDATE -> ok @@ map_get_and_update loc ;
  (* BIG MAP *)
  | C_BIG_MAP_GET_AND_UPDATE -> ok @@ big_map_get_and_update loc;
  (* CRYPTO *)
  | C_SHA256              -> ok @@ hash256 loc ;
  | C_SHA512              -> ok @@ hash512 loc ;
  | C_BLAKE2b             -> ok @@ blake2b loc ;
  | C_HASH_KEY            -> ok @@ hash_key loc ;
  | C_CHECK_SIGNATURE     -> ok @@ check_signature loc ;
  | C_CHAIN_ID            -> ok @@ chain_id loc;
    (*BLOCKCHAIN *)
  | C_CONTRACT            -> ok @@ get_contract loc ;
  | C_CONTRACT_OPT        -> ok @@ get_contract_opt loc ;
  | C_CONTRACT_ENTRYPOINT -> ok @@ get_entrypoint loc ;
  | C_CONTRACT_ENTRYPOINT_OPT -> ok @@ get_entrypoint_opt loc ;
  | C_AMOUNT              -> ok @@ amount loc ;
  | C_BALANCE             -> ok @@ balance loc ;
  | C_CALL                -> ok @@ transaction loc ;
  | C_SENDER              -> ok @@ sender loc ;
  | C_SOURCE              -> ok @@ source loc ;
  | C_ADDRESS             -> ok @@ address loc ;
  | C_SELF                -> ok @@ self loc ;
  | C_SELF_ADDRESS        -> ok @@ self_address loc ;
  | C_IMPLICIT_ACCOUNT    -> ok @@ implicit_account loc ;
  | C_SET_DELEGATE        -> ok @@ set_delegate loc ;
  | C_CREATE_CONTRACT     -> ok @@ create_contract loc ;
  | C_CONVERT_TO_RIGHT_COMB -> ok @@ convert_to_right_comb loc ;
  | C_CONVERT_TO_LEFT_COMB  -> ok @@ convert_to_left_comb loc ;
  | C_CONVERT_FROM_RIGHT_COMB -> ok @@ convert_from_right_comb loc ;
  | C_CONVERT_FROM_LEFT_COMB  -> ok @@ convert_from_left_comb loc ;
  | C_SHA3              -> ok @@ sha3 loc ;
  | C_KECCAK            -> ok @@ keccak loc ;
  | C_LEVEL             -> ok @@ level loc ;
  | C_VOTING_POWER      -> ok @@ voting_power loc ;
  | C_TOTAL_VOTING_POWER -> ok @@ total_voting_power loc ;
  | C_TICKET -> ok @@ ticket loc ;
  | C_READ_TICKET -> ok @@ read_ticket loc ;
  | C_SPLIT_TICKET -> ok @@ split_ticket loc ;
  | C_JOIN_TICKET -> ok @@ join_ticket loc ;
  | C_PAIRING_CHECK -> ok @@ pairing_check loc ;
  | C_SAPLING_VERIFY_UPDATE -> ok @@ sapling_verify_update loc ;
  | C_SAPLING_EMPTY_STATE -> ok @@ sapling_empty_state loc ;
  | C_TEST_ORIGINATE -> ok @@ test_originate loc ;
  | C_TEST_SET_NOW -> ok @@ test_set_now loc ;
  | C_TEST_SET_SOURCE -> ok @@ test_set_source loc ;
  | C_TEST_SET_BAKER -> ok @@ test_set_source loc ;
  | C_TEST_EXTERNAL_CALL_TO_CONTRACT -> ok @@ test_external_call_to_contract loc ;
  | C_TEST_EXTERNAL_CALL_TO_CONTRACT_EXN -> ok @@ test_external_call_to_contract_exn loc ;
  | C_TEST_EXTERNAL_CALL_TO_ADDRESS -> ok @@ test_external_call_to_address loc ;
  | C_TEST_EXTERNAL_CALL_TO_ADDRESS_EXN -> ok @@ test_external_call_to_address_exn loc ;
  | C_TEST_GET_STORAGE -> ok @@ test_get_storage loc ;
  | C_TEST_GET_STORAGE_OF_ADDRESS -> ok @@ test_get_storage_of_address loc ;
  | C_TEST_GET_BALANCE -> ok @@ test_get_balance loc ;
  | C_TEST_MICHELSON_EQUAL -> ok @@ test_michelson_equal loc ;
  | C_TEST_GET_NTH_BS -> ok @@ test_get_nth loc ;
  | C_TEST_LOG -> ok @@ test_log loc ;
  | C_TEST_COMPILE_EXPRESSION -> ok @@ test_compile_expression loc ;
  | C_TEST_COMPILE_EXPRESSION_SUBST -> ok @@ test_compile_expression_subst loc ;
  | C_TEST_STATE_RESET -> ok @@ test_state_reset loc ;
  | C_TEST_LAST_ORIGINATIONS -> ok @@ test_last_originations loc ;
  | C_TEST_COMPILE_META_VALUE -> ok @@ test_compile_meta_value loc ;
  | C_TEST_RUN -> ok @@ test_run loc ;
  | C_TEST_EVAL -> ok @@ test_eval loc ;
  | C_TEST_COMPILE_CONTRACT -> ok @@ test_compile_contract loc ;
  | C_TEST_TO_CONTRACT -> ok @@ test_to_contract loc ;
  | C_TEST_TO_ENTRYPOINT -> ok @@ test_to_entrypoint loc ;
  | C_TEST_ORIGINATE_FROM_FILE -> ok @@ test_originate_from_file loc ;
  (* JsLIGO *)
  | C_POLYMORPHIC_ADD  -> ok @@ polymorphic_add loc ;
  | _ as cst -> fail (corner_case @@ Format.asprintf "typer not implemented for constant %a" PP.constant' cst)
