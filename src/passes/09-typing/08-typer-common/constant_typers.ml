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


let none loc = typer_0 loc "NONE" @@ fun l tv_opt ->
  match tv_opt with
  | None -> fail (not_annotated l)
  | Some t -> ok t

let set_empty loc = typer_0 loc "SET_EMPTY" @@ fun l tv_opt ->
  match tv_opt with
  | None -> fail (not_annotated l)
  | Some t -> ok t

let sub loc = typer_2 loc "SUB" @@ fun loc a b ->
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

let some loc = typer_1 loc "SOME" @@ fun _ a -> ok @@ t_option a

let map_remove loc : typer = typer_2 loc "MAP_REMOVE" @@ fun loc k m ->
  let%bind (src , _) = bind_map_or (
      (fun m -> trace_option (expected_map loc m) @@ get_t_map m) ,
      (fun m -> trace_option (expected_big_map loc m) @@ get_t_big_map m)
    ) m in
  let%bind () = assert_eq loc src k in
  ok m

let map_empty loc = typer_0 loc "MAP_EMPTY" @@ fun loc tv_opt ->
  match tv_opt with
  | None -> fail (not_annotated loc)
  | Some t -> 
    let%bind (src, dst) = trace_option (expected_map loc t) @@ get_t_map t in
    ok @@ t_map src dst

let big_map_empty loc = typer_0 loc "BIG_MAP_EMPTY" @@ fun loc tv_opt ->
  match tv_opt with
  | None -> fail (not_annotated loc)
  | Some t -> 
    let%bind (src, dst) = trace_option (expected_big_map loc t) @@ get_t_big_map t in
    ok @@ t_big_map src dst

let map_add loc : typer = typer_3 loc "MAP_ADD" @@ fun loc k v m ->
  let%bind (src , dst) = bind_map_or (
      (fun m -> trace_option (expected_map loc m) @@ get_t_map m) ,
      (fun m -> trace_option (expected_big_map loc m) @@ get_t_big_map m)
    ) m in
  let%bind () = assert_eq loc src k in
  let%bind () = assert_eq loc dst v in
  ok m

let map_update loc : typer = typer_3 loc "MAP_UPDATE" @@ fun loc k v m ->
  let%bind (src , dst) = bind_map_or (
      (fun m -> trace_option (expected_map loc m) @@ get_t_map m) ,
      (fun m -> trace_option (expected_big_map loc m) @@ get_t_big_map m)
    ) m in
  let%bind () = assert_eq loc src k in
  let%bind v' = trace_option (expected_option loc v) @@ get_t_option v in
  let%bind () = assert_eq loc dst v' in
  ok m

let map_mem loc : typer = typer_2 loc "MAP_MEM" @@ fun loc k m ->
  let%bind (src , _dst) = bind_map_or (
      (fun m -> trace_option (expected_map loc m) @@ get_t_map m) ,
      (fun m -> trace_option (expected_big_map loc m) @@ get_t_big_map m)
    ) m in
  let%bind () = assert_eq loc src k in
  ok @@ t_bool ()

let map_find loc : typer = typer_2 loc "MAP_FIND" @@ fun loc k m ->
  let%bind (src , dst) = bind_map_or (
      (fun m -> trace_option (expected_map loc m) @@ get_t_map m) ,
      (fun m -> trace_option (expected_big_map loc m) @@ get_t_big_map m)
    ) m in
  let%bind () = assert_eq loc src k in
  ok @@ dst

let map_find_opt loc : typer = typer_2 loc "MAP_FIND_OPT" @@ fun loc k m ->
  let%bind (src , dst) = bind_map_or (
      (fun m -> trace_option (expected_map loc m) @@ get_t_map m) ,
      (fun m -> trace_option (expected_big_map loc m) @@ get_t_big_map m)
    ) m in
  let%bind () = assert_eq loc src k in
  ok @@ t_option dst

let map_iter loc : typer = typer_2 loc "MAP_ITER" @@ fun loc f m ->
  let%bind (k, v) = trace_option (expected_map loc m) @@ get_t_map m in
  let%bind (arg , res) = trace_option (expected_function loc f) @@ get_t_function f in
  let kv = t_pair k v in
  let unit = t_unit () in
  let%bind () = assert_eq loc arg kv in
  let%bind () = assert_eq loc res unit in
  ok @@ t_unit ()

let map_map loc : typer = typer_2 loc "MAP_MAP" @@ fun loc f m ->
  let%bind (k, v) = trace_option (expected_map loc m) @@ get_t_map m in
  let%bind (arg , res) = trace_option (expected_function loc f) @@ get_t_function f in
  let kv = t_pair k v in
  let%bind () = assert_eq loc arg kv in
  ok @@ t_map k res

let size loc = typer_1 loc "SIZE" @@ fun _ t ->
  let%bind () =
    Assert.assert_true (wrong_size loc t) @@
    (is_t_map t || is_t_list t || is_t_string t || is_t_bytes t || is_t_set t ) in
  ok @@ t_nat ()

let slice loc = typer_3 loc "SLICE" @@ fun loc i j s ->
  let t_nat = t_nat () in
  let%bind () = assert_eq loc i t_nat in
  let%bind () = assert_eq loc j t_nat in
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

let failwith_ loc = typer_1_opt loc "FAILWITH" @@ fun loc t opt ->
  let%bind _ =
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
  ok @@ Simple_utils.Option.unopt ~default opt

let int loc : typer = typer_1 loc "INT" @@ fun loc t ->
  let%bind () = trace_option (expected_nat loc t) @@ assert_t_nat t in
  ok @@ t_int ()

let bytes_pack loc : typer = typer_1 loc "PACK" @@ fun _loc _t ->
  ok @@ t_bytes ()

let bytes_unpack loc = typer_1_opt loc "UNPACK" @@ fun loc input output_opt ->
  let%bind () = trace_option (expected_bytes loc input) @@ assert_t_bytes input in
  trace_option (not_annotated loc) @@ output_opt

let hash256 loc = typer_1 loc "SHA256" @@ fun loc t ->
  let%bind () = trace_option (expected_bytes loc t) @@ assert_t_bytes t in
  ok @@ t_bytes ()

let hash512 loc = typer_1 loc "SHA512" @@ fun loc t ->
  let%bind () = trace_option (expected_bytes loc t) @@ assert_t_bytes t in
  ok @@ t_bytes ()

let blake2b loc = typer_1 loc "BLAKE2b" @@ fun loc t ->
  let%bind () = trace_option (expected_bytes loc t) @@ assert_t_bytes t in
  ok @@ t_bytes ()

let hash_key loc = typer_1 loc "HASH_KEY" @@ fun loc t ->
  let%bind () = trace_option (expected_key loc t) @@ assert_t_key t in
  ok @@ t_key_hash ()

let check_signature loc = typer_3 loc "CHECK_SIGNATURE" @@ fun loc k s b ->
  let%bind () = trace_option (expected_key loc k) @@ assert_t_key k in
  let%bind () = trace_option (expected_signature loc s) @@ assert_t_signature s in
  let%bind () = trace_option (expected_bytes loc b) @@ assert_t_bytes b in
  ok @@ t_bool ()

let sender loc = constant' loc "SENDER" @@ t_address ()

let source loc = constant' loc "SOURCE" @@ t_address ()

let unit loc = constant' loc "UNIT" @@ t_unit ()

let amount loc = constant' loc "AMOUNT" @@ t_mutez ()

let balance loc = constant' loc "BALANCE" @@ t_mutez ()

let chain_id loc = constant' loc "CHAIN_ID" @@ t_chain_id ()

let address loc = typer_1 loc "ADDRESS" @@ fun loc c ->
  let%bind () = trace_option (expected_contract loc c) @@ assert_t_contract c in
  ok @@ t_address ()

let self_address loc = typer_0 loc "SELF_ADDRESS" @@ fun _ _ ->
  ok @@ t_address ()

let self loc = typer_1_opt loc "SELF" @@ fun loc entrypoint_as_string tv_opt ->
  let%bind () = trace_option (expected_string loc entrypoint_as_string) @@ assert_t_string entrypoint_as_string in
  match tv_opt with
  | None -> fail (not_annotated loc)
  | Some t -> ok @@ t

let implicit_account loc = typer_1 loc "IMPLICIT_ACCOUNT" @@ fun loc key_hash ->
  let%bind () = trace_option (expected_key_hash loc key_hash) @@ assert_t_key_hash key_hash in
  ok @@ t_contract (t_unit () )

let now loc = constant' loc "NOW" @@ t_timestamp ()

let transaction loc = typer_3 loc "CALL" @@ fun loc param amount contract ->
  let%bind () = trace_option (expected_mutez loc amount) @@ assert_t_mutez amount in
  let%bind contract_param = trace_option (expected_contract loc contract) @@ get_t_contract contract in
  let%bind () = assert_eq loc param contract_param in
  ok @@ t_operation ()

let create_contract loc = typer_4 loc "CREATE_CONTRACT" @@ fun loc f kh_opt amount init_storage  ->
  let%bind (args , ret) = trace_option (expected_function loc f) @@ get_t_function f in
  let%bind (_,s) = trace_option (expected_pair loc args) @@ get_t_pair args in
  let%bind (oplist,s') = trace_option (expected_pair loc ret) @@ get_t_pair ret in
  let%bind () = trace_option (expected_mutez loc amount) @@ assert_t_mutez amount in
  let%bind (delegate) = trace_option (expected_option loc kh_opt) @@ get_t_option kh_opt in
  let%bind () = assert_eq loc s s' in
  let%bind () = assert_eq loc s init_storage in
  let%bind () = trace_option (expected_op_list loc oplist) @@ assert_t_list_operation oplist in
  let%bind () = trace_option (expected_key_hash loc delegate) @@ assert_t_key_hash delegate in
  ok @@ t_pair (t_operation ()) (t_address ())

let get_contract loc = typer_1_opt loc "CONTRACT" @@ fun loc addr_tv tv_opt ->
  let t_addr = t_address () in
  let%bind () = assert_eq loc addr_tv t_addr in
  let%bind tv = trace_option (not_annotated loc) tv_opt in
  let%bind tv' = trace_option (expected_contract loc tv) @@ get_t_contract tv in
  ok @@ t_contract tv'

let get_contract_opt loc = typer_1_opt loc "CONTRACT OPT" @@ fun loc addr_tv tv_opt ->
  let t_addr = t_address () in
  let%bind () = assert_eq loc addr_tv t_addr in
  let%bind tv = trace_option (not_annotated loc) tv_opt in
  let%bind tv = trace_option (expected_option loc tv) @@ get_t_option tv in
  let%bind tv' = trace_option (expected_contract loc tv) @@ get_t_contract tv in
  ok @@ t_option (t_contract tv')

let get_entrypoint loc = typer_2_opt loc "CONTRACT_ENTRYPOINT" @@ fun loc entry_tv addr_tv tv_opt ->
  let t_string = t_string () in
  let t_addr = t_address () in
  let%bind () = assert_eq loc entry_tv t_string in
  let%bind () = assert_eq loc addr_tv t_addr in
  let%bind tv = trace_option (not_annotated loc) tv_opt in
  let%bind tv' = trace_option (expected_contract loc tv) @@ get_t_contract tv in
  ok @@ t_contract tv'

let get_entrypoint_opt loc = typer_2_opt loc "CONTRACT_ENTRYPOINT_OPT" @@ fun loc entry_tv addr_tv tv_opt ->
  let t_string = t_string () in
  let t_addr = t_address () in
  let%bind () = assert_eq loc entry_tv t_string in
  let%bind () = assert_eq loc addr_tv t_addr in
  let%bind tv = trace_option (not_annotated loc) tv_opt in
  let%bind tv = trace_option (expected_option loc tv) @@ get_t_option tv in
  let%bind tv' = trace_option (expected_contract loc tv) @@ get_t_contract tv in
  ok @@ t_option (t_contract tv' )

let set_delegate loc = typer_1 loc "SET_DELEGATE" @@ fun loc delegate_opt ->
  let kh_opt = (t_option (t_key_hash ()) ) in
  let%bind () = assert_eq loc delegate_opt kh_opt in
  ok @@ t_operation ()

let abs loc = typer_1 loc "ABS" @@ fun loc t ->
  let%bind () = trace_option (expected_int loc t) @@ assert_t_int t in
  ok @@ t_nat ()

let is_nat loc = typer_1 loc "ISNAT" @@ fun loc t ->
  let%bind () = trace_option (expected_int loc t) @@ assert_t_int t in
  ok @@ t_option (t_nat ())

let neg loc = typer_1 loc "NEG" @@ fun loc t ->
  let%bind () = Assert.assert_true (wrong_neg loc t) @@ (eq_1 t (t_nat ()) || eq_1 t (t_int ())) in
  ok @@ t_int ()

let assertion loc = typer_1 loc "ASSERT" @@ fun loc a ->
  let%bind () = trace_option (expected_bool loc a) @@ assert_t_bool a in
  ok @@ t_unit ()

let assert_some loc = typer_1 loc "ASSERT_SOME" @@ fun loc a ->
  let%bind () = trace_option (expected_option loc a) @@ assert_t_option a in
  ok @@ t_unit ()

let times loc = typer_2 loc "TIMES" @@ fun loc a b ->
  if eq_2 (a , b) (t_nat ())
  then ok @@ t_nat () else
  if eq_2 (a , b) (t_int ())
  then ok @@ t_int () else
  if (eq_1 a (t_nat ()) && eq_1 b (t_mutez ())) || (eq_1 b (t_nat ()) && eq_1 a (t_mutez ()))
  then ok @@ t_mutez () else
    fail @@ typeclass_error loc
              [
                [t_nat();t_nat()] ;
                [t_int();t_int()] ;
                [t_nat();t_mutez()] ;
                [t_mutez();t_nat()] ;
              ]
              [a; b]

let ediv loc = typer_2 loc "EDIV" @@ fun loc a b ->
  if eq_2 (a , b) (t_nat ())
  then ok @@ t_option (t_pair (t_nat ()) (t_nat ()) ) else
  if eq_2 (a , b) (t_int ())
  then ok @@ t_option (t_pair (t_int ()) (t_nat ()) ) else
  if eq_1 a (t_mutez ()) && eq_1 b (t_mutez ())
  then ok @@ t_option (t_pair (t_nat ()) (t_mutez ()) ) else
  if eq_1 a (t_mutez ()) && eq_1 b (t_nat ())
  then ok @@ t_option (t_pair (t_mutez ()) (t_mutez ()) ) else
    fail @@ typeclass_error loc
              [
                [t_nat();t_nat()] ;
                [t_int();t_int()] ;
                [t_mutez();t_nat()] ;
                [t_mutez();t_mutez()] ;
              ]
              [a; b]

let div loc = typer_2 loc "DIV" @@ fun loc a b ->
  if eq_2 (a , b) (t_nat ())
  then ok @@ t_nat () else
  if eq_2 (a , b) (t_int ())
  then ok @@ t_int () else
  if eq_1 a (t_mutez ()) && eq_1 b (t_nat ())
  then ok @@ t_mutez () else
  if eq_1 a (t_mutez ()) && eq_1 b (t_mutez ())
  then ok @@ t_nat () else
    fail @@ typeclass_error loc
              [
                [t_nat();t_nat()] ;
                [t_int();t_int()] ;
                [t_mutez();t_nat()] ;
                [t_mutez();t_mutez()] ;
              ]
              [a; b]

let mod_ loc = typer_2 loc "MOD" @@ fun loc a b ->
  if (eq_1 a (t_nat ()) || eq_1 a (t_int ())) && (eq_1 b (t_nat ()) || eq_1 b (t_int ()))
  then ok @@ t_nat () else
  if eq_1 a (t_mutez ()) && eq_1 b (t_mutez ())
  then ok @@ t_mutez () else
    fail @@ typeclass_error loc
              [
                [t_nat();t_nat()] ;
                [t_nat();t_int()] ;
                [t_int();t_nat()] ;
                [t_int();t_int()] ;
                [t_mutez();t_mutez()] ;
              ]
              [a; b]

let add loc = typer_2 loc "ADD" @@ fun loc a b ->
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
                [t_nat();t_nat()] ;
                [t_int();t_int()] ;
                [t_mutez();t_mutez()] ;
                [t_nat();t_int()] ;
                [t_int();t_nat()] ;
                [t_timestamp();t_int()] ;
                [t_int();t_timestamp()] ;
              ]
              [a; b]

let set_mem loc = typer_2 loc "SET_MEM" @@ fun loc elt set ->
  let%bind key = trace_option (expected_set loc set) @@ get_t_set set in
  let%bind () = assert_eq loc elt key in
  ok @@ t_bool ()

let set_add loc = typer_2 loc "SET_ADD" @@ fun loc elt set ->
  let%bind key = trace_option (expected_set loc set) @@ get_t_set set in
  let%bind () = assert_eq loc elt key in
  ok set

let set_remove loc = typer_2 loc "SET_REMOVE" @@ fun loc elt set ->
  let%bind key = trace_option (expected_set loc set) @@ get_t_set set in
  let%bind () = assert_eq loc elt key in
  ok set

let set_iter loc = typer_2 loc "SET_ITER" @@ fun loc body set ->
  let%bind (arg , res) = trace_option (expected_function loc body) @@ get_t_function body in
  let%bind () = assert_eq loc res (t_unit ()) in
  let%bind key = trace_option (expected_set loc set) @@ get_t_set set in
  let%bind () = assert_eq loc key arg in
  ok (t_unit ())

let list_empty loc = typer_0 loc "LIST_EMPTY" @@ fun loc tv_opt ->
  match tv_opt with
  | None -> fail (not_annotated loc)
  | Some t -> ok t

let list_iter loc = typer_2 loc "LIST_ITER" @@ fun loc body lst ->
  let%bind (arg , res) = trace_option (expected_function loc body) @@ get_t_function body in
  let%bind () = assert_eq loc res (t_unit ()) in
  let%bind key = trace_option (expected_list loc lst) @@ get_t_list lst in
  let%bind () = assert_eq loc key arg in
  ok (t_unit ())

let list_map loc = typer_2 loc "LIST_MAP" @@ fun loc body lst ->
  let%bind (arg , res) = trace_option (expected_function loc body) @@ get_t_function body in
  let%bind key = trace_option (expected_list loc lst) @@ get_t_list lst in
  let%bind () = assert_eq loc key arg in
  ok (t_list res )

let list_fold loc = typer_3 loc "LIST_FOLD" @@ fun loc body lst init ->
  let%bind (arg , res) = trace_option (expected_function loc body) @@ get_t_function body in
  let%bind (prec , cur) = trace_option (expected_pair loc arg) @@ get_t_pair arg in
  let%bind key = trace_option (expected_list loc lst) @@ get_t_list lst in
  let%bind () = assert_eq loc key cur in
  let%bind () = assert_eq loc prec res in
  let%bind () = assert_eq loc res init in
  ok res

let set_fold loc = typer_3 loc "SET_FOLD" @@ fun loc body lst init ->
  let%bind (arg , res) = trace_option (expected_function loc body) @@ get_t_function body in
  let%bind (prec , cur) = trace_option (expected_pair loc arg) @@ get_t_pair arg in
  let%bind key = trace_option (expected_set loc lst) @@ get_t_set lst in
  let%bind () = assert_eq loc key cur in
  let%bind () = assert_eq loc prec res in
  let%bind () = assert_eq loc res init in
  ok res

let map_fold loc = typer_3 loc "MAP_FOLD" @@ fun loc body map init ->
  let%bind (arg , res) = trace_option (expected_function loc body) @@ get_t_function body in
  let%bind (prec , cur) = trace_option (expected_pair loc arg) @@ get_t_pair arg in
  let%bind (key , value) = trace_option (expected_map loc map) @@ get_t_map map in
  let kv = t_pair key value in
  let%bind () = assert_eq loc kv cur in
  let%bind () = assert_eq loc prec res in
  let%bind () = assert_eq loc res init in
  ok res

(** FOLD_WHILE is a fold operation that takes an initial value of a certain type
    and then iterates on it until a condition is reached. The auxillary function
    that does the fold returns either boolean true or boolean false to indicate
    whether the fold should continue or not. Necessarily then the initial value
    must match the input parameter of the auxillary function, and the auxillary
    should return type (bool * input) *)
let fold_while loc = typer_2 loc "FOLD_WHILE" @@ fun loc body init ->
  let%bind (arg, result) = trace_option (expected_function loc body) @@ get_t_function body in
  let%bind () = assert_eq loc arg init in
  let%bind () = assert_eq loc (t_pair (t_bool ()) init) result
  in ok init

(* Continue and Stop are just syntactic sugar for building a pair (bool * a') *)
let continue loc = typer_1 loc "CONTINUE" @@ fun _loc arg ->
  ok @@ t_pair (t_bool ()) arg

let stop loc = typer_1 loc "STOP" @@ fun _loc arg ->
  ok (t_pair (t_bool ()) arg)

let not_ loc = typer_1 loc "NOT" @@ fun loc elt ->
  if eq_1 elt (t_bool ())
  then ok @@ t_bool ()
  else if eq_1 elt (t_nat ()) || eq_1 elt (t_int ())
  then ok @@ t_int ()
  else fail @@ wrong_not loc elt

let or_ loc = typer_2 loc "OR" @@ fun loc a b ->
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

let xor loc = typer_2 loc "XOR" @@ fun loc a b ->
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

let and_ loc = typer_2 loc "AND" @@ fun loc a b ->
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

let lsl_ loc = typer_2 loc "LSL" @@ fun loc a b ->
  if eq_2 (a , b) (t_nat ())
  then ok @@ t_nat ()
  else fail @@ typeclass_error loc
                 [
                   [t_nat();t_nat()] ;
                 ]
                 [a; b]

let lsr_ loc = typer_2 loc "LSR" @@ fun loc a b ->
  if eq_2 (a , b) (t_nat ())
  then ok @@ t_nat ()
  else fail @@ typeclass_error loc
                 [
                   [t_nat();t_nat()] ;
                 ]
                 [a; b]

let concat loc = typer_2 loc "CONCAT" @@ fun loc a b ->
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

let cons loc = typer_2 loc "CONS" @@ fun loc hd tl ->
  let%bind elt = trace_option (expected_list loc tl) @@ get_t_list tl in
  let%bind () = assert_eq loc hd elt in
  ok tl

let convert_to_right_comb loc = typer_1 loc "CONVERT_TO_RIGHT_COMB" @@ fun _loc t ->
  match t.type_content with
    | T_record lmap ->
      let kvl = LMap.to_kv_list_rev lmap.content in
      let%bind () = Michelson_type_converter.field_checks kvl loc in
      let pair = Michelson_type_converter.convert_pair_to_right_comb kvl in
      ok {t with type_content = pair}
    | T_sum cmap ->
      let kvl = LMap.to_kv_list_rev cmap.content in
      let%bind () = Michelson_type_converter.field_checks kvl loc in
      let michelson_or = Michelson_type_converter.convert_variant_to_right_comb kvl in
      ok {t with type_content = michelson_or}
    | _ -> fail @@ wrong_converter t

let convert_to_left_comb loc = typer_1 loc "CONVERT_TO_LEFT_COMB" @@ fun _loc t ->
  match t.type_content with
    | T_record lmap ->
      let kvl =  LMap.to_kv_list_rev lmap.content in
      let%bind () = Michelson_type_converter.field_checks kvl loc in
      let pair = Michelson_type_converter.convert_pair_to_left_comb kvl in
      ok {t with type_content = pair}
    | T_sum cmap ->
      let kvl = LMap.to_kv_list_rev cmap.content in
      let%bind () = Michelson_type_converter.field_checks kvl loc in
      let michelson_or = Michelson_type_converter.convert_variant_to_left_comb kvl in
      ok {t with type_content = michelson_or}
    | _ -> fail @@ wrong_converter t

let convert_from_right_comb loc = typer_1_opt loc "CONVERT_FROM_RIGHT_COMB" @@ fun loc t opt ->
  let%bind dst_t = trace_option (not_annotated loc) opt in
  match t.type_content with
    | T_record {content=src_lmap;_} ->
      let%bind dst_lmap = trace_option (expected_record loc dst_t) @@ get_t_record dst_t in
      let%bind record = Michelson_type_converter.convert_pair_from_right_comb src_lmap dst_lmap.content in
      ok {t with type_content = record}
    | T_sum src_cmap ->
      let%bind dst_cmap = trace_option (expected_variant loc dst_t) @@ get_t_sum dst_t in
      let%bind variant = Michelson_type_converter.convert_variant_from_right_comb src_cmap.content dst_cmap.content in
      ok {t with type_content = variant}
    | _ -> fail @@ wrong_converter t

let convert_from_left_comb loc = typer_1_opt loc "CONVERT_FROM_LEFT_COMB" @@ fun loc t opt ->
  let%bind dst_t = trace_option (not_annotated loc) opt in
  match t.type_content with
    | T_record {content=src_lmap;_} ->
      let%bind dst_lmap = trace_option (expected_record loc dst_t) @@ get_t_record dst_t in
      let%bind record = Michelson_type_converter.convert_pair_from_left_comb src_lmap dst_lmap.content in
      ok {t with type_content = record}
    | T_sum src_cmap ->
      let%bind dst_cmap = trace_option (expected_variant loc dst_t) @@ get_t_sum dst_t in
      let%bind variant = Michelson_type_converter.convert_variant_from_left_comb src_cmap.content dst_cmap.content in
      ok {t with type_content = variant}
    | _ -> fail @@ wrong_converter t

let simple_comparator : Location.t -> string -> typer = fun loc s -> typer_2 loc s @@ fun loc a b ->
  let%bind () =
    Assert.assert_true (uncomparable_types loc a b) @@
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

let rec pair_comparator : Location.t -> string -> typer = fun loc s -> typer_2 loc s @@ fun loc a b ->
  let%bind () =
    Assert.assert_true (uncomparable_types loc a b) @@ eq_1 a b
  in
  let%bind (a_k, a_v) = 
    trace_option (comparator_composed loc a) @@
    get_t_pair a in
  let%bind (b_k, b_v) = trace_option (expected_pair loc b) @@ get_t_pair b in
  let%bind _ = simple_comparator loc s [a_k;b_k] None
  in
  comparator loc s [a_v;b_v] None

and comparator : Location.t -> string -> typer = fun loc s -> typer_2 loc s @@ fun loc a b ->
  bind_or (pair_comparator loc s [a;b] None, simple_comparator loc s [a;b] None)


let constant_typers loc c : (typer , typer_error) result = match c with
  | C_INT                 -> ok @@ int loc ;
  | C_UNIT                -> ok @@ unit loc ;
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
  | C_SET_MEM             -> ok @@ set_mem loc ;

  (* LIST *)
  | C_CONS                -> ok @@ cons loc ;
  | C_LIST_EMPTY          -> ok @@ list_empty loc;
  | C_LIST_ITER           -> ok @@ list_iter loc ;
  | C_LIST_MAP            -> ok @@ list_map loc ;
  | C_LIST_FOLD           -> ok @@ list_fold loc ;
  (* MAP *)
  | C_MAP_EMPTY           -> ok @@ map_empty loc;
  | C_BIG_MAP_EMPTY       -> ok @@ big_map_empty loc;
  | C_MAP_ADD             -> ok @@ map_add loc ;
  | C_MAP_REMOVE          -> ok @@ map_remove loc ;
  | C_MAP_UPDATE          -> ok @@ map_update loc ;
  | C_MAP_ITER            -> ok @@ map_iter loc ;
  | C_MAP_MAP             -> ok @@ map_map loc ;
  | C_MAP_FOLD            -> ok @@ map_fold loc ;
  | C_MAP_MEM             -> ok @@ map_mem loc ;
  | C_MAP_FIND            -> ok @@ map_find loc ;
  | C_MAP_FIND_OPT        -> ok @@ map_find_opt loc ;
  (* BIG MAP *)
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
  | C_AMOUNT              -> ok @@ amount loc;
  | C_BALANCE             -> ok @@ balance loc;
  | C_CALL                -> ok @@ transaction loc ;
  | C_SENDER              -> ok @@ sender loc;
  | C_SOURCE              -> ok @@ source loc;
  | C_ADDRESS             -> ok @@ address loc;
  | C_SELF                -> ok @@ self loc;
  | C_SELF_ADDRESS        -> ok @@ self_address loc;
  | C_IMPLICIT_ACCOUNT    -> ok @@ implicit_account loc;
  | C_SET_DELEGATE        -> ok @@ set_delegate loc ;
  | C_CREATE_CONTRACT     -> ok @@ create_contract loc ;
  | C_CONVERT_TO_RIGHT_COMB -> ok @@ convert_to_right_comb loc ;
  | C_CONVERT_TO_LEFT_COMB  -> ok @@ convert_to_left_comb loc ;
  | C_CONVERT_FROM_RIGHT_COMB -> ok @@ convert_from_right_comb loc ;
  | C_CONVERT_FROM_LEFT_COMB  -> ok @@ convert_from_left_comb loc ;
  | _                     -> fail (corner_case "typer not implemented for constant")
