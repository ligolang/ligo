open Errors
open Ast_core
open Trace

module Operators_types = struct
  open Typesystem.Shorthands
  let var = p_var

  (* Typeclass for arithmetic operations *)
  let tc_addargs  a b c = 
    tc "arguments for (+)"
      ~bound:[] ~constraints:[] ()
      [a;b;c] [ [nat;nat;nat] ; 
                [int;int;int] ; 
                [nat;int;int] ;
                [int;nat;int] ;
                [timestamp;int;timestamp] ;
                [int;timestamp;timestamp] ;
                [mutez;mutez;mutez] ;
                [bls12_381_g1;bls12_381_g1;bls12_381_g1] ;
                [bls12_381_g2;bls12_381_g2;bls12_381_g2] ;
                [bls12_381_fr;bls12_381_fr;bls12_381_fr] ;
              ]

let tc_polymorphic_addargs  a b c = 
tc "arguments for (+)"
  ~bound:[] ~constraints:[] ()
  [a;b;c] [ [string;string;string] ; 
            [nat;nat;nat] ; 
            [int;int;int] ; 
            [nat;int;int] ;
            [int;nat;int] ;
            [timestamp;int;timestamp] ;
            [int;timestamp;timestamp] ;
            [mutez;mutez;mutez] ;
            [bls12_381_g1;bls12_381_g1;bls12_381_g1] ;
            [bls12_381_g2;bls12_381_g2;bls12_381_g2] ;
            [bls12_381_fr;bls12_381_fr;bls12_381_fr] ;
          ]    

  let tc_subarg   a b c = 
    tc "arguments for (-)"
      ~bound:[] ~constraints:[] ()
      [a;b;c] [ [nat;nat;int] ;
                [nat;int;int] ;
                [int;nat;int] ;
                [int;int;int] ;
                [timestamp;int;timestamp] ;
                [timestamp;timestamp;int] ;
                [mutez;mutez;mutez] ;
              ]

  let tc_timargs  a b c = tc "arguments for ( * )"      ~bound:[] ~constraints:[] ()
                                                        [a;b;c] [ [nat;nat;nat] ; 
                                                                  [nat;int;int] ; 
                                                                  [int;nat;int] ;
                                                                  [int;int;int] ;
                                                                  [mutez;nat;mutez] ;
                                                                  [nat;mutez;mutez] ;
                                                                  [bls12_381_g1;bls12_381_fr;bls12_381_fr] ;
                                                                  [bls12_381_g2;bls12_381_fr;bls12_381_fr] ;
                                                                  [bls12_381_fr;bls12_381_fr;bls12_381_fr] ;
                                                                  [int; bls12_381_fr; bls12_381_fr] ;
                                                                  [nat; bls12_381_fr; bls12_381_fr] ;
                                                                  [bls12_381_fr; int; bls12_381_fr] ;
                                                                  [bls12_381_fr; nat; bls12_381_fr] ;
                                                                ]
  let tc_edivargs a b c d = tc "arguments and return values for ediv, div and mod"
                                                        ~bound:[] ~constraints:[] ()
                                                        [a;b;c;d] [ [nat;nat;nat;nat] ; 
                                                                    [nat;int;int;nat] ;
                                                                    [int;nat;int;nat] ;
                                                                    [int;int;int;nat] ;
                                                                    [mutez;nat;mutez;mutez] ;
                                                                    [mutez;mutez;nat;mutez] ;
                                                                  ]
  (* Typeclass for boolean operations *)
  let tc_andargs a b c  = tc "arguments and return values for and"
    ~bound:[] ~constraints:[] ()
    [a;b;c] [ [nat;nat;nat] ;
              [bool;bool;bool] ;
              [int;nat;nat] ;
            ]
  let tc_bitwise a b c  = tc "bitwise"                  ~bound:[] ~constraints:[] ()
                                                        [a;b;c] [ [nat;nat;nat] ;
                                                                  [bool;bool;bool] ;
                                                                ]
  let tc_notargs a b = tc "arguments and return values for not"
    ~bound:[] ~constraints:[] ()
    [a;b] [ [bool;bool] ;
            [nat;int] ; 
            [int;int] ]


  (* Typeclasses for data structure manipulations *)
  let tc_concatable a b = tc "concatenable"             ~bound:[] ~constraints:[] ()
                                                        [a;b]   [ [tuple2 string string  ; string ] ;
                                                                  [tuple1 @@ list string ; string ] ; 
                                                                  [tuple2 bytes  bytes   ; bytes  ] ;
                                                                  [tuple1 @@ list bytes  ; bytes  ] ;
                                                                ]

  let tc_sizearg  a     = 
    let x = Var.fresh () in
    let y = Var.fresh () in
    tc "arguments for size"
      ~bound:[x;y] ~constraints:[] ()
      [a]     [ [string] ; [bytes] ; [list (var x)] ; [set (var x)] ; [map (var x) (var y)] ]

  let tc_slicable   a   = tc "slicable"                 ~bound:[] ~constraints:[] ()
                                                        [a]     [ [string] ; [bytes] ]

  (* Typeclasses for typegroups *)
  let tc_comparable a = 
    let x = Var.fresh () in
    tc "comparable"
      ~bound:[x] ~constraints:[c_apply comparable x "tc_comparable:bound"] ()
      [a]     [ 
                [address] ;
                [bool] ;
                [bytes] ;
                [chain_id] ;
                [int] ; 
                [key] ; 
                [key_hash] ;
                [mutez] ; 
                [nat] ; 
                [never] ;
                [option (var x)] ;
                (* TODO: this is a placeholder, we need some row variables to allow constraints on all the fields of a record https://gitlab.com/ligolang/ligo/-/merge_requests/1189 *)
                [variant] ;
                [record] ;
                [signature] ;
                [string] ;
                [timestamp] ;
                [unit] ; 
                (* pair of comparable *)
              ]
  let tc_storable a =
    let c = Var.fresh () in
    let x = Var.fresh () in
    let y = Var.fresh () in
    tc "storable"
      ~bound:[x;y] ~constraints:[
                                 tc_comparable (p_var c);
                                 c_apply storable c "tc_storable:bound";
                                 c_apply storable x "tc_storable:bound"; 
                                 c_apply storable y "tc_storable:bound"] ()
      [a]     [ 
                [address] ;
                [big_map (var x) (var y)] ;
                [bls12_381_fr] ;
                [bls12_381_g1] ;
                [bls12_381_g2] ;
                [bool] ;
                [bytes] ;
                [chain_id] ;
                [int] ; 
                [key] ; 
                [key_hash] ; 
                [(var x) --> (var y)] ;
                [list (var x)] ;
                [map (var c) (var y)] ;
                [mutez] ; 
                [nat] ; 
                [never] ;
                [option (var x)] ;
                [variant] ;
                [record] ;
                (* [sapling_state (var x)] ;
                [sapling_transaction (var x)] ; *)
                [set (var c)] ;
                [signature] ;
                [string] ;
                (* [ticket (var x)] ; *)
                [timestamp] ;
                [unit] ; 
              ]
  let tc_packable a =  
    let c = Var.fresh () in
    let x = Var.fresh () in
    let y = Var.fresh () in
    tc "packable"
      ~bound:[c;x;y] ~constraints:[
                                      tc_comparable (p_var c);
                                      c_apply packable c "tc_packable:bound";
                                      c_apply packable x "tc_packable:bound"; 
                                      c_apply packable y "tc_packable:bound"] ()
      [a]     [ 
                [address] ;
                [bls12_381_fr] ;
                [bls12_381_g1] ;
                [bls12_381_g2] ;
                [bool] ;
                [bytes] ;
                [chain_id] ;
                [contract (var x)] ;
                [int] ; 
                [key] ; 
                [key_hash] ; 
                [(var x) --> (var y)] ;
                [list (var x)] ;
                [map (var c) (var y)] ;
                [mutez] ; 
                [nat] ; 
                [never] ;
                [option (var x)] ;
                [variant] ;
                [record] ;
                [set (var c)] ;
                [signature] ;
                [string] ;
                [timestamp] ;
                [unit] ; 
              ]

  (* Others typeclasses *)
  let tc_failwith a = tc "failwith" ~bound:[] ~constraints: [] () [a] [ [nat] ; [string] ; [int] ]
  let tc_map_or_big_map m src dst = tc "map or big_map" ~bound:[] ~constraints:[] () [m;src;dst] [ [map src dst;src;dst]; [big_map src dst;src;dst]]


  let t_never        = forall "a" @@ fun a -> tuple1 never --> a
  let t_none         = forall "a" @@ fun a -> tuple0 --> option a
  let t_sub          = forall3_tc "a" "b" "c" @@ fun a b c -> [tc_subarg a b c] => tuple2 a b --> c (* TYPECLASS *)
  let t_some         = forall "a" @@ fun a -> tuple1 a --> option a
  let t_map_empty    = forall2_tc "src" "dst" @@ fun src dst -> [] => tuple0 --> map src dst
  let t_big_map_empty = forall2_tc "src" "dst" @@ fun src dst -> [] => tuple0 --> big_map src dst
  let t_map_get_and_update = forall2_tc "k" "v" @@ fun k v -> [] => tuple3 k (option v) (map k v) --> tuple2 (option v) (map k v)
  let t_big_map_get_and_update = forall2_tc "k" "v" @@ fun k v -> [] => tuple3 k (option v) (big_map k v) --> tuple2 (option v) (big_map k v)

  let t_map_remove   = forall3_tc "m" "src" "dst" @@ fun m src dst -> [ tc_map_or_big_map m src dst] => tuple2 src m --> m
  let t_map_add      = forall3_tc "m" "src" "dst" @@ fun m src dst -> [ tc_map_or_big_map m src dst] => tuple3 src dst m --> m
  let t_map_update   = forall3_tc "m" "src" "dst" @@ fun m src dst -> [ tc_map_or_big_map m src dst] => tuple3 src (option dst) m --> m
  let t_map_mem      = forall3_tc "m" "src" "dst" @@ fun m src dst -> [ tc_map_or_big_map m src dst] => tuple2 src m --> bool
  let t_map_find     = forall3_tc "m" "src" "dst" @@ fun m src dst -> [ tc_map_or_big_map m src dst] => tuple2 src m --> dst
  let t_map_find_opt = forall3_tc "m" "src" "dst" @@ fun m src dst -> [ tc_map_or_big_map m src dst] => tuple2 src m --> option dst
  let t_map_fold     = forall4_tc "m" "src" "dst" "acc" @@ fun m src dst acc -> [tc_comparable src; tc_map_or_big_map m src dst] => tuple3 ( (pair acc (pair src dst )) --> acc ) m acc --> acc
  let t_map_map      = forall3_tc "k" "v" "result" @@ fun k v result -> [tc_comparable k] => tuple2 ((k * v) --> result) (map k v) --> map k result

  (* TODO: the type of map_map_fold might be wrong, check it. *)
  let t_map_map_fold = forall4_tc "k" "v" "acc" "dst" @@ fun k v acc dst -> [tc_comparable k] => tuple3 ( ((k * v) * acc) --> acc * dst ) (map k v) (k * v) --> (map k dst * acc)
  let t_map_iter     = forall2_tc "k" "v" @@ fun k v -> [tc_comparable k] => tuple2 ( (k * v) --> unit ) (map k v) --> unit
  let t_size         = forall_tc "c" @@ fun c -> [tc_sizearg c] => tuple1 c --> nat (* TYPECLASS *)
  let t_slice        = forall_tc "s" @@ fun s -> [tc_slicable s] => tuple3 nat nat s --> s
  let t_failwith     = forall2_tc "a" "b" @@ fun a b -> [tc_failwith b] => tuple1 b --> a
  let t_get_force    = forall2_tc "src" "dst" @@ fun src dst -> [tc_comparable src] => tuple2 src (map src dst) --> dst
  let t_int          = tuple1 nat --> int
  (* This bug because of the impossibility to express record in typeclass *)
  (* let t_bytes_pack   = forall_tc "a" @@ fun a -> [tc_packable a] => tuple1 a --> bytes (* TYPECLASS *) *)
  let t_bytes_pack   = forall "a" @@ fun a -> tuple1 a --> bytes (* TYPECLASS *)
  let t_bytes_unpack = forall_tc "a" @@ fun a -> [tc_packable a] => tuple1 bytes --> option a (* TYPECLASS *)
  let t_hash256      = tuple1 bytes --> bytes
  let t_hash512      = tuple1 bytes --> bytes
  let t_blake2b      = tuple1 bytes --> bytes
  let t_hash_key     = tuple1 key --> key_hash
  let t_is_nat       = tuple1 int --> option nat
  let t_check_signature = tuple3 key signature bytes --> bool
  let t_chain_id     = tuple0 --> chain_id
  let t_sender       = tuple0 --> address
  let t_source       = tuple0 --> address
  let t_unit         = tuple0 --> unit
  let t_amount       = tuple0 --> mutez
  let t_balance      = tuple0 --> mutez
  let t_address      = forall "a" @@ fun a -> tuple1 (contract a) --> address
  let t_now          = tuple0 --> timestamp
  let t_transaction  = forall "a" @@ fun a -> tuple3 a mutez (contract a) --> operation
  let t_get_contract = forall2 "a" "addr" @@ fun a addr -> tuple1 addr --> contract a
  let t_get_contract_opt = forall2 "a" "addr" @@ fun a addr -> tuple1 addr --> option (contract a)
  let t_get_entrypoint = forall3 "a" "entry" "addr" @@ fun a entry addr ->tuple2 entry addr --> contract a
  let t_get_entrypoint_opt = forall3 "a" "entry" "addr" @@ fun a entry addr ->tuple2 entry addr --> option (contract a)
  let t_abs          = tuple1 int --> nat
  let t_cons         = forall "a" @@ fun a -> tuple2 a (list a) --> list a
  let t_assertion    = tuple1 bool --> unit
  let t_assert_some  = forall "a" @@ fun a -> tuple1 (option a) --> unit
  let t_times        = forall3_tc "a" "b" "c" @@ fun a b c -> [tc_timargs a b c] => tuple2 a b --> c (* TYPECLASS *)
  let t_ediv         = forall4_tc "a" "b" "c" "d" @@ fun a b c d -> [tc_edivargs a b c d] => tuple2 a b --> (option @@ tuple2 c d) (* TYPECLASS *)
  let t_div          = forall4_tc "a" "b" "c" "d" @@ fun a b c d -> [tc_edivargs a b c d] => tuple2 a b --> c (* TYPECLASS *)
  let t_mod          = forall4_tc "a" "b" "c" "d" @@ fun a b c d -> [tc_edivargs a b c d] => tuple2 a b --> d (* TYPECLASS *)
  let t_add          = forall3_tc "a" "b" "c" @@ fun a b c -> [tc_addargs a b c] => tuple2 a b --> c (* TYPECLASS *)
  let t_polymorphic_add  = forall3_tc "a" "b" "c" @@ fun a b c -> [tc_polymorphic_addargs a b c] => tuple2 a b --> c (* TYPECLASS *)
  let t_set_mem      = forall_tc "a" @@ fun a -> [tc_comparable a] => tuple2 a (set a) --> bool
  let t_set_add      = forall_tc "a" @@ fun a -> [tc_comparable a] => tuple2 a (set a) --> set a
  let t_set_remove   = forall_tc "a" @@ fun a -> [tc_comparable a] => tuple2 a (set a) --> set a
  let t_set_update   = forall_tc "a" @@ fun a -> [tc_comparable a] => tuple3 a bool (set a) --> (set a)
  let t_not          = forall2_tc "a" "b" @@ fun a b -> [tc_notargs a b] => tuple1 a --> b
  let t_continuation  = forall "a" @@ fun a -> tuple1 a --> pair bool a
  let t_fold_while    = forall "a" @@ fun a -> tuple2 (a --> pair bool a) a --> a
  let t_neg           = tuple1 int --> int
  let t_and           = forall3_tc "a" "b" "c" @@ fun a b c -> [tc_andargs a b c] => tuple2 a b --> c
  let t_or            = forall3_tc "a" "b" "c" @@ fun a b c -> [tc_bitwise a b c] => tuple2 a b --> c
  let t_xor           = forall3_tc "a" "b" "c" @@ fun a b c -> [tc_bitwise a b c] => tuple2 a b --> c
  let t_lsl           = forall3_tc "a" "b" "c" @@ fun a b c -> [tc_bitwise a b c] => tuple2 a b --> c
  let t_lsr           = forall3_tc "a" "b" "c" @@ fun a b c -> [tc_bitwise a b c] => tuple2 a b --> c
  let t_comp          = forall_tc "a" @@ fun a -> [tc_comparable a] => tuple2 a a --> bool
  let t_concat        = forall2_tc "a" "b" @@ fun a b -> [tc_concatable a b] => a --> b

  let t_set_empty     = forall_tc "a" @@ fun a -> [tc_comparable a] => tuple0 --> set a
  let t_set_iter      = forall_tc "a" @@ fun a -> [tc_comparable a] => tuple2 (a --> unit) (set a) --> unit
  (* TODO: check that the implementation has this type *)
  let t_set_fold      = forall2_tc "a" "b" @@ fun a b -> [tc_comparable b] => tuple3 (pair a b --> a) (set b) a --> a
  let t_SET_FOLD_DESC= forall2_tc "a" "b" @@ fun a b -> [tc_comparable b] => tuple3 (pair b a --> a) (set b) a --> a
  let t_list_empty    = forall "a" @@ fun a -> tuple0 --> list a
  let t_list_iter     = forall "a" @@ fun a -> tuple2 (a --> unit) (list a) --> unit
  let t_list_map      = forall2 "a" "b" @@ fun a b -> tuple2 (a --> b) (list a) --> (list b)
  (* TODO: check that the implementation has this type *)
  let t_list_fold     = forall2 "a" "b" @@ fun a b -> tuple3 (pair a b --> a) (list b) a --> a
  let t_list_fold_left  = forall2 "a" "b" @@ fun a b -> tuple3 (pair a b --> a) a (list b) --> a
  let t_list_fold_right = forall2 "a" "b" @@ fun a b -> tuple3 (pair b a --> a) (list b) a --> a
  let t_list_head_opt = forall "a" @@ fun a -> tuple1 (list a) --> option a
  let t_list_tail_opt = forall "a" @@ fun a -> tuple1 (list a) --> option (list a)
  let t_self_address  = tuple0 --> address
  let t_implicit_account = forall_tc "a" @@ fun a -> [tc_storable a] => tuple1 key_hash --> contract a
  let t_set_delegate  = tuple1 (option key_hash) --> operation

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
    | C_NEVER               -> ok @@ t_never ;
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
    (* SET  *)
    | C_SET_EMPTY           -> ok @@ t_set_empty ;
    | C_SET_ADD             -> ok @@ t_set_add ;
    | C_SET_REMOVE          -> ok @@ t_set_remove ;
    | C_SET_ITER            -> ok @@ t_set_iter ;
    | C_SET_FOLD            -> ok @@ t_set_fold ;
    | C_SET_FOLD_DESC      -> ok @@ t_SET_FOLD_DESC ;
    | C_SET_MEM             -> ok @@ t_set_mem ;
    | C_SET_UPDATE          -> ok @@ t_set_update ;

    (* LIST *)
    | C_CONS                -> ok @@ t_cons ;
    | C_LIST_EMPTY          -> ok @@ t_list_empty ;
    | C_LIST_ITER           -> ok @@ t_list_iter ;
    | C_LIST_MAP            -> ok @@ t_list_map ;
    | C_LIST_FOLD           -> ok @@ t_list_fold ;
    | C_LIST_FOLD_LEFT      -> ok @@ t_list_fold_left ;
    | C_LIST_FOLD_RIGHT     -> ok @@ t_list_fold_right ;
    | C_LIST_HEAD_OPT       -> ok @@ t_list_head_opt ;
    | C_LIST_TAIL_OPT       -> ok @@ t_list_tail_opt ;

    (* MAP *)
    | C_MAP_EMPTY           -> ok @@ t_map_empty ;
    | C_BIG_MAP_EMPTY       -> ok @@ t_big_map_empty ;
    | C_MAP_ADD             -> ok @@ t_map_add ;
    | C_MAP_REMOVE          -> ok @@ t_map_remove ;
    | C_MAP_UPDATE          -> ok @@ t_map_update ;
    | C_MAP_ITER            -> ok @@ t_map_iter ;
    | C_MAP_MAP             -> ok @@ t_map_map ;
    | C_MAP_FOLD            -> ok @@ t_map_fold ;
    | C_MAP_MEM             -> ok @@ t_map_mem ;
    | C_MAP_FIND            -> ok @@ t_map_find ;
    | C_MAP_FIND_OPT        -> ok @@ t_map_find_opt ;
    | C_MAP_GET_AND_UPDATE  -> ok @@ t_map_get_and_update ;
    (* BIG MAP *)
    | C_BIG_MAP_GET_AND_UPDATE -> ok @@ t_big_map_get_and_update ;
    (* CRYPTO *)
    | C_SHA256              -> ok @@ t_hash256 ;
    | C_SHA512              -> ok @@ t_hash512 ;
    | C_BLAKE2b             -> ok @@ t_blake2b ;
    | C_HASH_KEY            -> ok @@ t_hash_key ;
    | C_CHECK_SIGNATURE     -> ok @@ t_check_signature ;
    | C_CHAIN_ID            -> ok @@ t_chain_id ;
    (*BLOCKCHAIN *)
    | C_CONTRACT            -> ok @@ t_get_contract ;
    | C_CONTRACT_OPT        -> ok @@ t_get_contract_opt ;
    | C_CONTRACT_ENTRYPOINT -> ok @@ t_get_entrypoint ;
    | C_CONTRACT_ENTRYPOINT_OPT -> ok @@ t_get_entrypoint_opt ;
    | C_AMOUNT              -> ok @@ t_amount ;
    | C_BALANCE             -> ok @@ t_balance ;
    | C_CALL                -> ok @@ t_transaction ;
    | C_SENDER              -> ok @@ t_sender ;
    | C_SOURCE              -> ok @@ t_source ;
    | C_ADDRESS             -> ok @@ t_address ;
    | C_SELF_ADDRESS        -> ok @@ t_self_address;
    | C_IMPLICIT_ACCOUNT    -> ok @@ t_implicit_account;
    | C_SET_DELEGATE        -> ok @@ t_set_delegate ;
    | C_POLYMORPHIC_ADD     -> ok @@ t_polymorphic_add ;
    | c                     -> fail (corner_case (Format.asprintf "Typer not implemented for constant %a" Ast_typed.PP.constant' c))
end
