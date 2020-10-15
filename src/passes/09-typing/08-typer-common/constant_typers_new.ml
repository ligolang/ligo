open Errors
open Ast_typed
open Trace

module Operators_types = struct
  open Typesystem.Shorthands

  let tc_subarg   a b c = tc "arguments for (-)"        [a;b;c] [ (*TODO…*) ]
  let tc_sizearg  a     = tc "arguments for size"       [a]     [ [int] ]
  let tc_packable a     = tc "packable"                 [a]     [ [int] ; [string] ; [bool] (*TODO…*) ]
  let tc_timargs  a b c = tc "arguments for ( * )"      [a;b;c] [ [nat;nat;nat] ; [int;int;int] (*TODO…*) ]
  let tc_edivargs a b c = tc "arguments for ediv"       [a;b;c] [ (*TODO…*) ]
  let tc_divargs  a b c = tc "arguments for div"        [a;b;c] [ (*TODO…*) ]
  let tc_modargs  a b c = tc "arguments for mod"        [a;b;c] [ (*TODO…*) ]
  let tc_addargs  a b c = tc "arguments for (+)"        [a;b;c] [ [nat;nat;nat] ; [int;int;int] (*TODO…*) ]
  let tc_comparable a   = tc "comparable"               [a]     [ [nat] ; [int] ; [mutez] ; [timestamp] ]
  let tc_concatable a   = tc "concatenable"             [a]     [ [string] ; [bytes] ]
  let tc_storable a     = tc "storable"                 [a]     [ [string] ; [bytes] ; (*Humm .. TODO ?*) ]

  let t_none         = forall "a" @@ fun a -> option a

  let t_sub          = forall3_tc "a" "b" "c" @@ fun a b c -> [tc_subarg a b c] => tuple2 a b --> c (* TYPECLASS *)
  let t_some         = forall "a" @@ fun a -> a --> option a
  let t_map_remove   = forall2 "src" "dst" @@ fun src dst -> tuple2 src (map src dst) --> map src dst
  let t_map_add      = forall2 "src" "dst" @@ fun src dst -> tuple3 src dst (map src dst) --> map src dst
  let t_map_update   = forall2 "src" "dst" @@ fun src dst -> tuple3 src (option dst) (map src dst) --> map src dst
  let t_map_mem      = forall2 "src" "dst" @@ fun src dst -> tuple2 src (map src dst) --> bool
  let t_map_find     = forall2 "src" "dst" @@ fun src dst -> tuple2 src (map src dst) --> dst
  let t_map_find_opt = forall2 "src" "dst" @@ fun src dst -> tuple2 src (map src dst) --> option dst
  let t_map_fold     = forall3 "src" "dst" "acc" @@ fun src dst acc -> tuple3 ( ( (src * dst) * acc ) --> acc ) (map src dst) acc --> acc
  let t_map_map      = forall3 "k" "v" "result" @@ fun k v result -> tuple2 ((k * v) --> result) (map k v) --> map k result

  (* TODO: the type of map_map_fold might be wrong, check it. *)
  let t_map_map_fold = forall4 "k" "v" "acc" "dst" @@ fun k v acc dst -> tuple3 ( ((k * v) * acc) --> acc * dst ) (map k v) (k * v) --> (map k dst * acc)
  let t_map_iter     = forall2 "k" "v" @@ fun k v -> tuple2 ( (k * v) --> unit ) (map k v) --> unit
  let t_size         = forall_tc "c" @@ fun c -> [tc_sizearg c] => tuple1 c --> nat (* TYPECLASS *)
  let t_slice        = tuple3 nat nat string --> string
  let t_failwith     = tuple1 string --> unit
  let t_get_force    = forall2 "src" "dst" @@ fun src dst -> tuple2 src (map src dst) --> dst
  let t_int          = tuple1 nat --> int
  let t_bytes_pack   = forall_tc "a" @@ fun a -> [tc_packable a] => tuple1 a --> bytes (* TYPECLASS *)
  let t_bytes_unpack = forall_tc "a" @@ fun a -> [tc_packable a] => tuple1 bytes --> a (* TYPECLASS *)
  let t_hash256      = tuple1 bytes --> bytes
  let t_hash512      = tuple1 bytes --> bytes
  let t_blake2b      = tuple1 bytes --> bytes
  let t_hash_key     = tuple1 key --> key_hash
  let t_is_nat       = tuple1 int --> bool
  let t_check_signature = tuple3 key signature bytes --> bool
  let t_chain_id     = tuple0 --> chain_id
  let t_sender       = tuple0 --> address
  let t_source       = tuple0 --> address
  let t_unit         = tuple0 --> unit
  let t_amount       = tuple0 --> mutez
  let t_balance      = tuple0 --> mutez
  let t_address      = tuple0 --> address
  let t_now          = tuple0 --> timestamp
  let t_transaction  = forall "a" @@ fun a -> tuple3 a mutez (contract a) --> operation
  let t_get_contract = forall "a" @@ fun a -> tuple0 --> contract a
  let t_abs          = tuple1 int --> nat
  let t_cons         = forall "a" @@ fun a -> tuple2 a (list a) --> list a
  let t_assertion    = tuple1 bool --> unit
  let t_assert_some  = forall "a" @@ fun a -> tuple1 (option a) --> unit
  let t_times        = forall3_tc "a" "b" "c" @@ fun a b c -> [tc_timargs a b c] => tuple2 a b --> c (* TYPECLASS *)
  let t_ediv         = forall3_tc "a" "b" "c" @@ fun a b c -> [tc_edivargs a b c] => tuple2 a b --> c (* TYPECLASS *)
  let t_div          = forall3_tc "a" "b" "c" @@ fun a b c -> [tc_divargs a b c] => tuple2 a b --> c (* TYPECLASS *)
  let t_mod          = forall3_tc "a" "b" "c" @@ fun a b c -> [tc_modargs a b c] => tuple2 a b --> c (* TYPECLASS *)
  let t_add          = forall3_tc "a" "b" "c" @@ fun a b c -> [tc_addargs a b c] => tuple2 a b --> c (* TYPECLASS *)
  let t_set_mem      = forall "a" @@ fun a -> tuple2 a (set a) --> bool
  let t_set_add      = forall "a" @@ fun a -> tuple2 a (set a) --> set a
  let t_set_remove   = forall "a" @@ fun a -> tuple2 a (set a) --> set a
  let t_not          = tuple1 bool --> bool

  let t_continuation = forall "a" @@ fun a -> tuple2 bool a --> pair bool a
  let t_fold_while   = forall "a" @@ fun a -> tuple2 (a --> pair bool a) a --> a
  let t_neg          = tuple1 int --> int
  let t_and          = tuple2 bool bool --> bool
  let t_or           = tuple2 bool bool --> bool
  let t_xor          = tuple2 bool bool --> bool
  let t_lsl          = tuple2 nat nat --> nat
  let t_lsr          = tuple2 nat nat --> nat
  let t_comp         = forall_tc "a" @@ fun a -> [tc_comparable a] => tuple2 a a --> bool
  let t_concat       = forall_tc "a" @@ fun a -> [tc_concatable a] => tuple2 a a --> a
  let t_set_empty    = forall_tc "a" @@ fun a -> [tc_comparable a] => tuple0 --> set a
  let t_set_iter     = forall_tc "a" @@ fun a -> [tc_comparable a] => tuple2 (a --> unit) (set a) --> unit
  (* TODO: check that the implementation has this type *)
  let t_set_fold     = forall2_tc "a" "b" @@ fun a b -> [tc_comparable b] => tuple3 (pair a b --> a) (set b) a --> a
  let t_list_iter    = forall "a" @@ fun a -> tuple2 (a --> unit) (list a) --> unit
  let t_list_map     = forall "a" @@ fun a -> tuple2 (a --> a) (list a) --> (list a)
  (* TODO: check that the implementation has this type *)
  let t_list_fold    = forall2 "a" "b" @@ fun a b -> tuple3 (pair a b --> a) (list b) a --> a
  let t_self_address = tuple0 --> address
  let t_implicit_account = forall_tc "a" @@ fun a -> [tc_storable a] => tuple1 key_hash --> contract a
  let t_set_delegate = tuple1 (option key_hash) --> operation

  let constant_type : constant' -> (Typesystem.Core.type_value, typer_error) result = function
    | C_INT                 -> ok @@ t_int ;
    | C_UNIT                -> ok @@ t_unit ;
    | C_NOW                 -> ok @@ t_now ;
    | C_IS_NAT              -> ok @@ t_is_nat ;
    | C_SOME                -> ok @@ t_some ;
    | C_NONE                -> ok @@ t_none ;
    | C_ASSERTION           -> ok @@ t_assertion ;
    | C_ASSERT_SOME         -> ok @@ t_assert_some ;
    | C_FAILWITH            -> ok @@ t_failwith ;
    (* LOOPS *)
    | C_FOLD_WHILE          -> ok @@ t_fold_while ;
    | C_FOLD_CONTINUE       -> ok @@ t_continuation ;
    | C_FOLD_STOP           -> ok @@ t_continuation ;
    (* MATH *)
    | C_NEG                 -> ok @@ t_neg ;
    | C_ABS                 -> ok @@ t_abs ;
    | C_ADD                 -> ok @@ t_add ;
    | C_SUB                 -> ok @@ t_sub ;
    | C_MUL                 -> ok @@ t_times ;
    | C_EDIV                -> ok @@ t_ediv ;
    | C_DIV                 -> ok @@ t_div ;
    | C_MOD                 -> ok @@ t_mod ;
    (* LOGIC *)
    | C_NOT                 -> ok @@ t_not ;
    | C_AND                 -> ok @@ t_and ;
    | C_OR                  -> ok @@ t_or ;
    | C_XOR                 -> ok @@ t_xor ;
    | C_LSL                 -> ok @@ t_lsl ;
    | C_LSR                 -> ok @@ t_lsr ;
    (* COMPARATOR *)
    | C_EQ                  -> ok @@ t_comp ;
    | C_NEQ                 -> ok @@ t_comp ;
    | C_LT                  -> ok @@ t_comp ;
    | C_GT                  -> ok @@ t_comp ;
    | C_LE                  -> ok @@ t_comp ;
    | C_GE                  -> ok @@ t_comp ;
    (* BYTES / STRING *)
    | C_SIZE                -> ok @@ t_size ;
    | C_CONCAT              -> ok @@ t_concat ;
    | C_SLICE               -> ok @@ t_slice ;
    | C_BYTES_PACK          -> ok @@ t_bytes_pack ;
    | C_BYTES_UNPACK        -> ok @@ t_bytes_unpack ;
    | C_CONS                -> ok @@ t_cons ;
    (* SET  *)
    | C_SET_EMPTY           -> ok @@ t_set_empty ;
    | C_SET_ADD             -> ok @@ t_set_add ;
    | C_SET_REMOVE          -> ok @@ t_set_remove ;
    | C_SET_ITER            -> ok @@ t_set_iter ;
    | C_SET_FOLD            -> ok @@ t_set_fold ;
    | C_SET_MEM             -> ok @@ t_set_mem ;

    (* LIST *)
    | C_LIST_ITER           -> ok @@ t_list_iter ;
    | C_LIST_MAP            -> ok @@ t_list_map ;
    | C_LIST_FOLD           -> ok @@ t_list_fold ;

    (* MAP *)
    | C_MAP_ADD             -> ok @@ t_map_add ;
    | C_MAP_REMOVE          -> ok @@ t_map_remove ;
    | C_MAP_UPDATE          -> ok @@ t_map_update ;
    | C_MAP_ITER            -> ok @@ t_map_iter ;
    | C_MAP_MAP             -> ok @@ t_map_map ;
    | C_MAP_FOLD            -> ok @@ t_map_fold ;
    | C_MAP_MEM             -> ok @@ t_map_mem ;
    | C_MAP_FIND            -> ok @@ t_map_find ;
    | C_MAP_FIND_OPT        -> ok @@ t_map_find_opt ;
    (* BIG MAP *)
    (* CRYPTO *)
    | C_SHA256              -> ok @@ t_hash256 ;
    | C_SHA512              -> ok @@ t_hash512 ;
    | C_BLAKE2b             -> ok @@ t_blake2b ;
    | C_HASH_KEY            -> ok @@ t_hash_key ;
    | C_CHECK_SIGNATURE     -> ok @@ t_check_signature ;
    | C_CHAIN_ID            -> ok @@ t_chain_id ;
    (*BLOCKCHAIN *)
    | C_CONTRACT            -> ok @@ t_get_contract ;
    | C_CONTRACT_ENTRYPOINT -> ok @@ failwith "t_get_entrypoint" ;
    | C_AMOUNT              -> ok @@ t_amount ;
    | C_BALANCE             -> ok @@ t_balance ;
    | C_CALL                -> ok @@ t_transaction ;
    | C_SENDER              -> ok @@ t_sender ;
    | C_SOURCE              -> ok @@ t_source ;
    | C_ADDRESS             -> ok @@ t_address ;
    | C_SELF_ADDRESS        -> ok @@ t_self_address;
    | C_IMPLICIT_ACCOUNT    -> ok @@ t_implicit_account;
    | C_SET_DELEGATE        -> ok @@ t_set_delegate ;
    | c                     -> fail (corner_case (Format.asprintf "Typer not implemented for constant %a" Ast_typed.PP.constant' c))
end
