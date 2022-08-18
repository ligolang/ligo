open Cli_expect

let contract basename =
  "../../test/contracts/aggregation/" ^ basename

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias.mligo" ] ;
  [%expect {xxx|
let get_balance#8unit -> tez =
  lambda (_uunit)tez return ([%Michelson {| { DROP ; BALANCE } |}])@(unit)[@inline][@hidden] in
let get_amount#9unit -> tez =
  lambda (_uunit)tez return ([%Michelson {| { DROP ; AMOUNT } |}])@(unit)[@inline][@hidden] in
let get_now#10unit -> timestamp =
  lambda (_uunit)timestamp return ([%Michelson {| { DROP ; NOW } |}])@(unit)[@inline][@hidden] in
let get_sender#11unit -> address =
  lambda (_uunit)address return ([%Michelson {| { DROP ; SENDER } |}])@(unit)[@inline][@hidden] in
let get_source#12unit -> address =
  lambda (_uunit)address return ([%Michelson {| { DROP ; SOURCE } |}])@(unit)[@inline][@hidden] in
let get_level#13unit -> nat =
  lambda (_uunit)nat return ([%Michelson {| { DROP ; LEVEL } |}])@(unit)[@inline][@hidden] in
let get_self_address#14unit -> address =
  lambda (_uunit)address return SELF_ADDRESS()[@inline][@hidden] in
let get_chain_id#15unit -> chain_id =
  lambda (_uunit)chain_id return ([%Michelson {| { DROP ; CHAIN_ID } |}])@(unit)[@inline][@hidden] in
let get_total_voting_power#16unit -> nat =
  lambda (_uunit)nat return ([%Michelson {| { DROP ; TOTAL_VOTING_POWER } |}])@(unit)[@inline][@hidden] in
let get_min_block_time#17unit -> nat =
  lambda (_uunit)nat return ([%Michelson {| { DROP; MIN_BLOCK_TIME } |}])@(unit)[@inline][@hidden] in
let voting_power#18key_hash -> nat =
  lambda (khkey_hash)nat return ([%Michelson {| { VOTING_POWER } |}])@(kh)[@inline][@hidden] in
let address#19funtype a : * . contract (a) -> address = Λ a ->
lambda (ccontract (a))address return ADDRESS(c)[@inline][@hidden] in
let implicit_account#20key_hash -> contract (unit) =
  lambda (khkey_hash)contract (unit) return IMPLICIT_ACCOUNT(kh)[@inline][@hidden] in
let join_tickets#21funtype a : * . ( ticket (a) * ticket (a) ) -> option (ticket (a)) =
  Λ a ->
lambda (t( ticket (a) * ticket (a) ))option (ticket (a)) return ([%Michelson {| { JOIN_TICKETS } |}])@(t)[@inline][@hidden] in
let read_ticket#22funtype a : * . ticket (a) -> ( ( address * ( a * nat ) ) * ticket (a) ) =
  Λ a ->
lambda (tticket (a))( ( address * ( a * nat ) ) * ticket (a) ) return
([%Michelson {| { READ_TICKET ; PAIR } |}])@(t)[@inline][@hidden] in
let never#23funtype a : * . never -> a = Λ a ->
lambda (nnever)a return ([%Michelson {| { NEVER } |}])@(n)[@inline][@hidden] in
let pairing_check#24list (( bls12_381_g1 * bls12_381_g2 )) -> bool =
  lambda (llist (( bls12_381_g1 * bls12_381_g2 )))bool return ([%Michelson {| { PAIRING_CHECK } |}])@(l)[@inline][@hidden] in
let constant#25funtype a : * . string -> a = Λ a ->
lambda (sstring)a return GLOBAL_CONSTANT(s)[@inline][@hidden] in
let set_delegate#26option (key_hash) -> operation =
  lambda (ooption (key_hash))operation return SET_DELEGATE(o)[@inline][@hidden] in
let get_contract#27funtype a : * . address -> contract (a) = Λ a ->
lambda (aaddress)contract (a) return CONTRACT(a)[@inline][@hidden] in
let get_contract_opt#28funtype a : * . address -> option (contract (a)) =
  Λ a ->
lambda (aaddress)option (contract (a)) return CONTRACT_OPT(a)[@inline][@hidden] in
let get_contract_with_error#29funtype a : * . address -> string -> contract (a) =
  Λ a ->
lambda (aaddress)string -> contract (a) return lambda (sstring)contract (a) return
CONTRACT_WITH_ERROR(a , s)[@inline][@hidden] in
let create_ticket#30funtype a : * . a -> nat -> ticket (a) = Λ a ->
lambda (va)nat -> ticket (a) return lambda (nnat)ticket (a) return ([%Michelson {| { UNPAIR ; TICKET } |}])@(
                                                                   ( v ,
                                                                    n ))[@inline][@hidden] in
let transaction#31funtype a : * . a -> tez -> contract (a) -> operation =
  Λ a ->
lambda (aa)tez -> contract (a) -> operation return lambda (mutez)contract (a) -> operation return lambda (ccontract (a))operation return
CALL(a , mu , c)[@inline][@hidden] in
let open_chest#32chest_key -> chest -> nat -> sum[Fail_decrypt -> unit , Fail_timelock -> unit , Ok_opening -> bytes] =
  lambda (ckchest_key)chest -> nat -> sum[Fail_decrypt -> unit , Fail_timelock -> unit , Ok_opening -> bytes] return lambda (cchest)nat ->
sum[Fail_decrypt -> unit , Fail_timelock -> unit , Ok_opening -> bytes] return lambda (nnat)
sum[Fail_decrypt -> unit , Fail_timelock -> unit , Ok_opening -> bytes] return
OPEN_CHEST(ck , c , n)[@inline][@hidden] in
let call_view#33funtype a : * . funtype b : * . string -> a -> address -> option (b) =
  Λ a ->
Λ b ->
lambda (sstring)a -> address -> option (b) return lambda (xa)address -> option (b) return lambda (aaddress)option (b) return
VIEW(s , x , a)[@inline][@hidden] in
let split_ticket#34funtype a : * . ticket (a) -> ( nat * nat ) -> option (
( ticket (a) * ticket (a) )) = Λ a ->
lambda (tticket (a))( nat * nat ) -> option (( ticket (a) * ticket (a) )) return lambda (p
( nat * nat ))option (( ticket (a) * ticket (a) )) return ([%Michelson {| { UNPAIR ; SPLIT_TICKET } |}])@(
                                                          ( t ,
                                                            p ))[@inline][@hidden] in
let xor#35nat -> nat -> nat =
  lambda (lnat)nat -> nat return lambda (rnat)nat return XOR(l , r)[@inline][@hidden] in
let shift_left#36nat -> nat -> nat =
  lambda (lnat)nat -> nat return lambda (rnat)nat return LSL(l , r)[@inline][@hidden] in
let shift_right#37nat -> nat -> nat =
  lambda (lnat)nat -> nat return lambda (rnat)nat return LSR(l , r)[@inline][@hidden] in
let empty#38funtype k : * . funtype v : * . big_map (k , v) = Λ k ->
Λ v ->  BIG_MAP_EMPTY()[@inline][@hidden] in
let mem#39funtype k : * . funtype v : * . k -> big_map (k , v) -> bool =
  Λ k ->
Λ v ->
lambda (kk)big_map (k , v) -> bool return lambda (mbig_map (k ,
v))bool return ([%Michelson {| { UNPAIR ; MEM } |}])@(( k , m ))[@inline][@hidden] in
let add#40funtype k : * . funtype v : * . k -> v -> big_map (k ,
v) -> big_map (k , v) = Λ k ->
Λ v ->
lambda (kk)v -> big_map (k , v) -> big_map (k ,
v) return lambda (vv)big_map (k , v) -> big_map (k ,
v) return lambda (mbig_map (k , v))big_map (k , v) return MAP_ADD(k , v , m)[@inline][@hidden] in
let remove#41funtype k : * . funtype v : * . k -> big_map (k ,
v) -> big_map (k , v) = Λ k ->
Λ v ->
lambda (kk)big_map (k , v) -> big_map (k , v) return lambda (mbig_map (k ,
v))big_map (k , v) return MAP_REMOVE(k , m)[@inline][@hidden] in
let update#42funtype k : * . funtype v : * . k -> option (v) -> big_map (k ,
v) -> big_map (k , v) = Λ k ->
Λ v ->
lambda (kk)option (v) -> big_map (k , v) -> big_map (k ,
v) return lambda (voption (v))big_map (k , v) -> big_map (k ,
v) return lambda (mbig_map (k , v))big_map (k ,
v) return MAP_UPDATE(k , v , m)[@inline][@hidden] in
let get_and_update#43funtype k : * . funtype v : * . k -> option (v) -> big_map (k ,
v) -> ( option (v) * big_map (k , v) ) = Λ k ->
Λ v ->
lambda (kk)option (v) -> big_map (k ,
v) -> ( option (v) * big_map (k , v) ) return lambda (voption (v))big_map (k ,
v) -> ( option (v) * big_map (k , v) ) return lambda (mbig_map (k ,
v))( option (v) * big_map (k , v) ) return BIG_MAP_GET_AND_UPDATE(k , v , m)[@inline][@hidden] in
let find_opt#44funtype k : * . funtype v : * . k -> big_map (k ,
v) -> option (v) = Λ k ->
Λ v ->
lambda (kk)big_map (k , v) -> option (v) return lambda (mbig_map (k ,
v))option (v) return MAP_FIND_OPT(k , m)[@inline][@hidden] in
let find#45funtype k : * . funtype v : * . k -> big_map (k , v) -> v =
  Λ k ->
Λ v ->
lambda (kk)big_map (k , v) -> v return lambda (mbig_map (k ,
v))v return MAP_FIND(k , m)[@inline][@hidden] in
let empty#46funtype k : * . funtype v : * . map (k , v) = Λ k ->
Λ v ->  MAP_EMPTY()[@inline][@hidden] in
let size#47funtype k : * . funtype v : * . map (k , v) -> nat = Λ k ->
Λ v ->  lambda (mmap (k , v))nat return ([%Michelson {| { SIZE } |}])@(m)[@inline][@hidden] in
let mem#48funtype k : * . funtype v : * . k -> map (k , v) -> bool = Λ k ->
Λ v ->
lambda (kk)map (k , v) -> bool return lambda (mmap (k ,
v))bool return ([%Michelson {| { UNPAIR ; MEM } |}])@(( k , m ))[@inline][@hidden] in
let add#49funtype k : * . funtype v : * . k -> v -> map (k , v) -> map (k ,
v) = Λ k ->
Λ v ->
lambda (kk)v -> map (k , v) -> map (k , v) return lambda (vv)map (k ,
v) -> map (k , v) return lambda (mmap (k , v))map (k ,
v) return MAP_ADD(k , v , m)[@inline][@hidden] in
let remove#50funtype k : * . funtype v : * . k -> map (k , v) -> map (k ,
v) = Λ k ->
Λ v ->
lambda (kk)map (k , v) -> map (k , v) return lambda (mmap (k , v))map (k ,
v) return MAP_REMOVE(k , m)[@inline][@hidden] in
let update#51funtype k : * . funtype v : * . k -> option (v) -> map (k ,
v) -> map (k , v) = Λ k ->
Λ v ->
lambda (kk)option (v) -> map (k , v) -> map (k ,
v) return lambda (voption (v))map (k , v) -> map (k ,
v) return lambda (mmap (k , v))map (k , v) return MAP_UPDATE(k , v , m)[@inline][@hidden] in
let get_and_update#52funtype k : * . funtype v : * . k -> option (v) -> map (k ,
v) -> ( option (v) * map (k , v) ) = Λ k ->
Λ v ->
lambda (kk)option (v) -> map (k ,
v) -> ( option (v) * map (k , v) ) return lambda (voption (v))map (k ,
v) -> ( option (v) * map (k , v) ) return lambda (mmap (k ,
v))( option (v) * map (k , v) ) return MAP_GET_AND_UPDATE(k , v , m)[@inline][@hidden] in
let find#53funtype k : * . funtype v : * . k -> map (k , v) -> v = Λ k ->
Λ v ->
lambda (kk)map (k , v) -> v return lambda (mmap (k ,
v))v return MAP_FIND(k , m)[@inline][@hidden] in
let find_opt#54funtype k : * . funtype v : * . k -> map (k ,
v) -> option (v) = Λ k ->
Λ v ->
lambda (kk)map (k , v) -> option (v) return lambda (mmap (k ,
v))option (v) return MAP_FIND_OPT(k , m)[@inline][@hidden] in
let iter#55funtype k : * . funtype v : * . ( k * v ) -> unit -> map (k ,
v) -> unit = Λ k ->
Λ v ->
lambda (f( k * v ) -> unit)map (k , v) -> unit return lambda (mmap (k ,
v))unit return MAP_ITER(f , m)[@inline][@hidden] in
let map#56funtype k : * . funtype v : * . funtype w : * . ( k * v ) -> w -> map (k ,
v) -> map (k , w) = Λ k ->
Λ v ->
Λ w ->
lambda (f( k * v ) -> w)map (k , v) -> map (k , w) return lambda (mmap (k ,
v))map (k , w) return MAP_MAP(f , m)[@inline][@hidden] in
let fold#57funtype k : * . funtype v : * . funtype c : * . ( c * ( k * v ) ) -> c -> map (k ,
v) -> c -> c = Λ k ->
Λ v ->
Λ c ->
lambda (f( c * ( k * v ) ) -> c)map (k ,
v) -> c -> c return lambda (mmap (k ,
v))c -> c return lambda (ic)c return MAP_FOLD(f , m , i)[@inline][@hidden] in
let empty#58funtype a : * . set (a) = Λ a ->
SET_EMPTY()[@inline][@hidden] in
let size#59funtype a : * . set (a) -> nat = Λ a ->
lambda (sset (a))nat return ([%Michelson {| { SIZE } |}])@(s)[@inline][@hidden] in
let cardinal#60funtype a : * . set (a) -> nat = Λ a ->
lambda (sset (a))nat return ([%Michelson {| { SIZE } |}])@(s)[@inline][@hidden] in
let mem#61funtype a : * . a -> set (a) -> bool = Λ a ->
lambda (xa)set (a) -> bool return lambda (sset (a))bool return SET_MEM(x , s)[@inline][@hidden] in
let add#62funtype a : * . a -> set (a) -> set (a) = Λ a ->
lambda (xa)set (a) -> set (a) return lambda (sset (a))set (a) return
SET_ADD(x , s)[@inline][@hidden] in
let remove#63funtype a : * . a -> set (a) -> set (a) = Λ a ->
lambda (xa)set (a) -> set (a) return lambda (sset (a))set (a) return
SET_REMOVE(x , s)[@inline][@hidden] in
let update#64funtype a : * . a -> bool -> set (a) -> set (a) = Λ a ->
lambda (xa)bool -> set (a) -> set (a) return lambda (bbool)set (a) -> set (a) return lambda (sset (a))set (a) return
SET_UPDATE(x , b , s)[@inline][@hidden] in
let iter#65funtype a : * . a -> unit -> set (a) -> unit = Λ a ->
lambda (fa -> unit)set (a) -> unit return lambda (sset (a))unit return
SET_ITER(f , s)[@inline][@hidden] in
let fold#66funtype a : * . funtype b : * . ( b * a ) -> b -> set (a) -> b -> b =
  Λ a ->
Λ b ->
lambda (f( b * a ) -> b)set (a) -> b -> b return lambda (sset (a))b -> b return lambda (ib)b return
SET_FOLD(f , s , i)[@inline][@hidden] in
let fold_desc#67funtype a : * . funtype b : * . ( a * b ) -> b -> set (a) -> b -> b =
  Λ a ->
Λ b ->
lambda (f( a * b ) -> b)set (a) -> b -> b return lambda (sset (a))b -> b return lambda (ib)b return
SET_FOLD_DESC(f , s , i)[@inline][@hidden] in
let length#68funtype a : * . list (a) -> nat = Λ a ->
lambda (xslist (a))nat return ([%Michelson {| { SIZE } |}])@(xs)[@inline][@hidden] in
let size#69funtype a : * . list (a) -> nat = Λ a ->
lambda (xslist (a))nat return ([%Michelson {| { SIZE } |}])@(xs)[@inline][@hidden] in
let head_opt#70funtype a : * . list (a) -> option (a) = Λ a ->
lambda (xslist (a))option (a) return  match xs with
                                       | Cons ctor_proj#2 ->
                                          match ctor_proj#2 with
                                           | ( x , _#2 ) ->
                                           Some(x)
                                       | Nil unit_proj#4 ->
                                         None(unit)[@inline][@hidden] in
let tail_opt#71funtype a : * . list (a) -> option (list (a)) = Λ a ->
lambda (xslist (a))option (list (a)) return  match xs with
                                              | Cons ctor_proj#5 ->
                                                 match ctor_proj#5 with
                                                  | ( _#3 , xs ) ->
                                                  Some(xs)
                                              | Nil unit_proj#7 ->
                                                None(unit)[@inline][@hidden] in
let map#72funtype a : * . funtype b : * . a -> b -> list (a) -> list (b) =
  Λ a ->
Λ b ->
lambda (fa -> b)list (a) -> list (b) return lambda (xslist (a))list (b) return
LIST_MAP(f , xs)[@inline][@hidden] in
let iter#73funtype a : * . a -> unit -> list (a) -> unit = Λ a ->
lambda (fa -> unit)list (a) -> unit return lambda (xslist (a))unit return
LIST_ITER(f , xs)[@inline][@hidden] in
let fold#74funtype a : * . funtype b : * . ( b * a ) -> b -> list (a) -> b -> b =
  Λ a ->
Λ b ->
lambda (f( b * a ) -> b)list (a) -> b -> b return lambda (xslist (a))b -> b return lambda (ib)b return
LIST_FOLD(f , xs , i)[@inline][@hidden] in
let fold_left#75funtype a : * . funtype b : * . ( b * a ) -> b -> b -> list (a) -> b =
  Λ a ->
Λ b ->
lambda (f( b * a ) -> b)b -> list (a) -> b return lambda (ib)list (a) -> b return lambda (xslist (a))b return
LIST_FOLD_LEFT(f , i , xs)[@inline][@hidden] in
let fold_right#76funtype a : * . funtype b : * . ( a * b ) -> b -> list (a) -> b -> b =
  Λ a ->
Λ b ->
lambda (f( a * b ) -> b)list (a) -> b -> b return lambda (xslist (a))b -> b return lambda (ib)b return
LIST_FOLD_RIGHT(f , xs , i)[@inline][@hidden] in
let cons#77funtype a : * . a -> list (a) -> list (a) = Λ a ->
lambda (xa)list (a) -> list (a) return lambda (xslist (a))list (a) return
CONS(x , xs)[@inline][@hidden] in
let length#78string -> nat =
  lambda (bstring)nat return ([%Michelson {| { SIZE } |}])@(b)[@inline][@hidden] in
let concat#79string -> string -> string =
  lambda (b1string)string -> string return lambda (b2string)string return
([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b1 , b2 ))[@inline][@hidden] in
let sub#80nat -> nat -> string -> string =
  lambda (snat)nat -> string -> string return lambda (lnat)string -> string return lambda (bstring)string return
([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(
( s ,
  l ,
  b ))[@inline][@hidden] in
let unopt#81funtype a : * . option (a) -> a = Λ a ->
lambda (voption (a))a return UNOPT(v)[@inline][@hidden] in
let unopt_with_error#82funtype a : * . option (a) -> string -> a = Λ a ->
lambda (voption (a))string -> a return lambda (sstring)a return UNOPT_WITH_ERROR
                                                                (v ,
                                                                 s)[@inline][@hidden] in
let pack#83funtype a : * . a -> bytes = Λ a ->
lambda (va)bytes return ([%Michelson {| { PACK } |}])@(v)[@inline][@hidden] in
let unpack#84funtype a : * . bytes -> option (a) = Λ a ->
lambda (bbytes)option (a) return BYTES_UNPACK(b)[@inline][@hidden] in
let length#85bytes -> nat =
  lambda (bbytes)nat return ([%Michelson {| { SIZE } |}])@(b)[@inline][@hidden] in
let concat#86bytes -> bytes -> bytes =
  lambda (b1bytes)bytes -> bytes return lambda (b2bytes)bytes return
([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b1 , b2 ))[@inline][@hidden] in
let sub#87nat -> nat -> bytes -> bytes =
  lambda (snat)nat -> bytes -> bytes return lambda (lnat)bytes -> bytes return lambda (bbytes)bytes return
([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(
( s ,
  l ,
  b ))[@inline][@hidden] in
let blake2b#88bytes -> bytes =
  lambda (bbytes)bytes return ([%Michelson {| { BLAKE2B } |}])@(b)[@inline][@hidden] in
let sha256#89bytes -> bytes =
  lambda (bbytes)bytes return ([%Michelson {| { SHA256 } |}])@(b)[@inline][@hidden] in
let sha512#90bytes -> bytes =
  lambda (bbytes)bytes return ([%Michelson {| { SHA512 } |}])@(b)[@inline][@hidden] in
let sha3#91bytes -> bytes =
  lambda (bbytes)bytes return ([%Michelson {| { SHA3 } |}])@(b)[@inline][@hidden] in
let keccak#92bytes -> bytes =
  lambda (bbytes)bytes return ([%Michelson {| { KECCAK } |}])@(b)[@inline][@hidden] in
let hash_key#93key -> key_hash =
  lambda (kkey)key_hash return ([%Michelson {| { HASH_KEY } |}])@(k)[@inline][@hidden] in
let check#94key -> signature -> bytes -> bool =
  lambda (kkey)signature -> bytes -> bool return lambda (ssignature)bytes -> bool return lambda (bbytes)bool return
([%Michelson {| { UNPAIR ; UNPAIR ; CHECK_SIGNATURE } |}])@(( k , s , b ))[@inline][@hidden] in
let assertbool -> unit =
  lambda (bbool)unit return ([%Michelson {| { IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } } |}])@(b)[@inline][@private][@hidden] in
let assert_somefuntype a : * . option (a) -> unit = Λ a ->
lambda (voption (a))unit return ([%Michelson {| { IF_NONE { PUSH string "failed assert some" ; FAILWITH } { DROP ; UNIT } } |}])@(v)[@inline][@private][@hidden] in
let assert_nonefuntype a : * . option (a) -> unit = Λ a ->
lambda (voption (a))unit return ([%Michelson {| { IF_NONE { UNIT } { PUSH string "failed assert none" ; FAILWITH } } |}])@(v)[@inline][@private][@hidden] in
let absint -> nat =
  lambda (iint)nat return ([%Michelson {| { ABS } |}])@(i)[@inline][@private][@hidden] in
let is_natint -> option (nat) =
  lambda (iint)option (nat) return ([%Michelson {| { ISNAT } |}])@(i)[@inline][@private][@hidden] in
let truebool = TRUE()[@inline][@private][@hidden] in
let falsebool = FALSE()[@inline][@private][@hidden] in
let unitunit = UNIT()[@inline][@private][@hidden] in
let failwithfuntype a : * . funtype b : * . a -> b = Λ a ->
Λ b ->  [%Michelson {|{ FAILWITH }|}][@inline][@private][@hidden] in
let intfuntype a : * . a -> external_int (a) = Λ a ->
lambda (va)external_int (a) return ([%Michelson {| { INT } |}])@(v)[@inline][@private][@hidden] in
let assert_with_errorbool -> string -> unit =
  lambda (bbool)string -> unit return lambda (sstring)unit return ([%Michelson {| { UNPAIR ; IF { DROP ; UNIT } { FAILWITH } } |}])@(
                                                                  ( b ,
                                                                    s ))[@inline][@private][@hidden] in
let assert_some_with_errorfuntype a : * . option (a) -> string -> unit =
  Λ a ->
lambda (voption (a))string -> unit return lambda (sstring)unit return
([%Michelson {| { UNPAIR ; IF_NONE { FAILWITH } { DROP 2 ; UNIT } } |}])@(
( v ,
  s ))[@inline][@private][@hidden] in
let assert_none_with_errorfuntype a : * . option (a) -> string -> unit =
  Λ a ->
lambda (voption (a))string -> unit return lambda (sstring)unit return
([%Michelson {| { UNPAIR ; IF_NONE { DROP ; UNIT } { DROP ; FAILWITH } } |}])@(
( v ,
  s ))[@inline][@private][@hidden] in
let edivfuntype a : * . funtype b : * . a -> b -> external_ediv (a , b) =
  Λ a ->
Λ b ->
lambda (la)b -> external_ediv (a , b) return lambda (rb)external_ediv (a ,
b) return ([%Michelson {| { UNPAIR ; EDIV } |}])@(( l , r ))[@inline][@private][@hidden] in
let stub#95funtype a : * . funtype b : * . a -> b = Λ a ->
Λ b ->  lambda (xa)b return ([%Michelson {|{ FAILWITH }|}])@(x)[@inline][@private][@hidden] in
let run#96funtype a : * . funtype b : * . a -> b -> a -> unit = Λ a ->
Λ b ->
lambda (_fa -> b)a -> unit return lambda (_va)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
let eval#97funtype a : * . a -> unit = Λ a ->
lambda (_xa)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
let compile_value#98funtype a : * . a -> unit = Λ a ->
lambda (_xa)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
let get_total_voting_power#99unit -> nat =
  lambda (_uunit)nat return (stub#95@{unit}@{nat})@(unit)[@inline][@hidden] in
let failwith#100funtype a : * . funtype b : * . a -> b = Λ a ->
Λ b ->  lambda (_va)b return (stub#95@{unit}@{b})@(unit)[@inline][@hidden] in
let to_contract#101funtype p : * . funtype s : * . unit -> contract (p) =
  Λ p ->
Λ s ->
lambda (_tunit)contract (p) return (stub#95@{unit}@{contract (p)})@(unit)[@inline][@hidden] in
let set_source#102address -> unit =
  lambda (_aaddress)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
let get_storage_of_address#103address -> unit =
  lambda (_aaddress)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
let get_balance#104address -> tez =
  lambda (_aaddress)tez return (stub#95@{unit}@{tez})@(unit)[@inline][@hidden] in
let print#105string -> unit =
  lambda (_vstring)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
let eprint#106string -> unit =
  lambda (_vstring)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
let get_voting_power#107key_hash -> nat =
  lambda (_khkey_hash)nat return (stub#95@{unit}@{nat})@(unit)[@inline][@hidden] in
let nth_bootstrap_contract#108nat -> address =
  lambda (_inat)address return (stub#95@{unit}@{address})@(unit)[@inline][@hidden] in
let nth_bootstrap_account#109int -> address =
  lambda (_iint)address return (stub#95@{unit}@{address})@(unit)[@inline][@hidden] in
let get_bootstrap_account#110nat -> ( address * key * string ) =
  lambda (_nnat)( address * key * string ) return (stub#95@{unit}@{( address * key * string )})@(unit)[@inline][@hidden] in
let nth_bootstrap_typed_address#111funtype a : * . funtype b : * . nat -> unit =
  Λ a ->
Λ b ->  lambda (_nnat)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
let last_originations#112unit -> map (address , list (address)) =
  lambda (_uunit)map (address ,
list (address)) return (stub#95@{unit}@{map (address ,
                       list (address))})@(unit)[@inline][@hidden] in
let random#113funtype a : * . unit -> a = Λ a ->
lambda (_uunit)a return (stub#95@{unit}@{a})@(unit)[@inline][@hidden] in
let new_account#114unit -> ( string * key ) =
  lambda (_uunit)( string * key ) return (stub#95@{unit}@{( string * key )})@(unit)[@inline][@hidden] in
let decompile#115funtype a : * . unit -> a = Λ a ->
lambda (_munit)a return (stub#95@{unit}@{a})@(unit)[@inline][@hidden] in
let bake_until_n_cycle_end#116nat -> unit =
  lambda (_nnat)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
let cast_address#117funtype a : * . funtype b : * . address -> unit =
  Λ a ->
Λ b ->  lambda (_aaddress)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
let register_delegate#118key_hash -> unit =
  lambda (_khkey_hash)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
let register_constant#119unit -> string =
  lambda (_munit)string return (stub#95@{unit}@{string})@(unit)[@inline][@hidden] in
let to_typed_address#120funtype a : * . funtype b : * . contract (a) -> unit =
  Λ a ->
Λ b ->  lambda (_ccontract (a))unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
let constant_to_michelson_program#121string -> unit =
  lambda (_sstring)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
let restore_context#122unit -> unit =
  lambda (_uunit)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
let save_context#123unit -> unit =
  lambda (_uunit)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
let drop_context#124unit -> unit =
  lambda (_uunit)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
let to_string#125funtype a : * . a -> string = Λ a ->
lambda (_va)string return (stub#95@{unit}@{string})@(unit)[@inline][@hidden] in
let get_storage#126funtype p : * . funtype s : * . unit -> s = Λ p ->
Λ s ->  lambda (_tunit)s return (stub#95@{unit}@{s})@(unit)[@inline][@hidden] in
let set_baker_policy#127unit -> unit =
  lambda (_bpunit)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
let set_baker#128address -> unit =
  lambda (_aaddress)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
let size#129unit -> int =
  lambda (_cunit)int return (stub#95@{unit}@{int})@(unit)[@inline][@hidden] in
let compile_contract#130funtype p : * . funtype s : * . ( p * s ) ->
( list (operation) * s ) -> unit = Λ p ->
Λ s ->
lambda (_f( p * s ) -> ( list (operation) * s ))unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
let read_contract_from_file#131string -> unit =
  lambda (_fnstring)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
let chr#132nat -> option (string) =
  lambda (_nnat)option (string) return (stub#95@{unit}@{option (string)})@(unit)[@inline][@hidden] in
let nl#133string = "NEWLINE"[@inline][@hidden] in
let println#134string -> unit =
  lambda (_vstring)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
let transfer#135address -> unit -> tez -> unit =
  lambda (_aaddress)unit -> tez -> unit return lambda (_sunit)tez -> unit return lambda (_ttez)unit return
(stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
let transfer_exn#136address -> unit -> tez -> nat =
  lambda (_aaddress)unit -> tez -> nat return lambda (_sunit)tez -> nat return lambda (_ttez)nat return
(stub#95@{unit}@{nat})@(unit)[@inline][@hidden] in
let log#137funtype a : * . a -> unit = Λ a ->
lambda (_va)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
let reset_state#138nat -> list (tez) -> unit =
  lambda (_nnat)list (tez) -> unit return lambda (_llist (tez))unit return
(stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
let reset_state_at#139timestamp -> nat -> list (tez) -> unit =
  lambda (_ttimestamp)nat -> list (tez) -> unit return lambda (_nnat)list (tez) -> unit return lambda (_llist (tez))unit return
(stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
let bootstrap_contract#140funtype p : * . funtype s : * . ( p * s ) ->
( list (operation) * s ) -> s -> tez -> unit = Λ p ->
Λ s ->
lambda (_f( p * s ) -> ( list (operation) * s ))s -> tez -> unit return lambda (_ss)tez -> unit return lambda (_ttez)unit return
(stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
let mutate_value#141funtype a : * . nat -> a -> option (( a * unit )) =
  Λ a ->
lambda (_nnat)a -> option (( a * unit )) return lambda (_va)option (( a * unit )) return
(stub#95@{unit}@{option (( a * unit ))})@(unit)[@inline][@hidden] in
let save_mutation#142string -> unit -> option (string) =
  lambda (_sstring)unit -> option (string) return lambda (_munit)option (string) return
(stub#95@{unit}@{option (string)})@(unit)[@inline][@hidden] in
let mutation_test#143funtype a : * . funtype b : * . a -> a -> b -> option (
( b * unit )) = Λ a ->
Λ b ->
lambda (_va)a -> b -> option (( b * unit )) return lambda (_fa -> b)option (
( b * unit )) return (stub#95@{unit}@{option (( b * unit ))})@(unit)[@inline][@hidden] in
let mutation_test_all#144funtype a : * . funtype b : * . a -> a -> b -> list (
( b * unit )) = Λ a ->
Λ b ->
lambda (_va)a -> b -> list (( b * unit )) return lambda (_fa -> b)list (
( b * unit )) return (stub#95@{unit}@{list (( b * unit ))})@(unit)[@inline][@hidden] in
let sign#145string -> bytes -> signature =
  lambda (_skstring)bytes -> signature return lambda (_dbytes)signature return
(stub#95@{unit}@{signature})@(unit)[@inline][@hidden] in
let add_account#146string -> key -> unit =
  lambda (_sstring)key -> unit return lambda (_kkey)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
let baker_account#147( string * key ) -> option (tez) -> unit =
  lambda (_p( string * key ))option (tez) -> unit return lambda (_ooption (tez))unit return
(stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
let set_big_map#148funtype a : * . funtype b : * . int -> big_map (a ,
b) -> unit = Λ a ->
Λ b ->
lambda (_iint)big_map (a , b) -> unit return lambda (_mbig_map (a ,
b))unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
let create_chest#149bytes -> nat -> ( chest * chest_key ) =
  lambda (_bbytes)nat -> ( chest * chest_key ) return lambda (_nnat)( chest * chest_key ) return
(stub#95@{unit}@{( chest * chest_key )})@(unit)[@inline][@hidden] in
let create_chest_key#150chest -> nat -> chest_key =
  lambda (_cchest)nat -> chest_key return lambda (_nnat)chest_key return
(stub#95@{unit}@{chest_key})@(unit)[@inline][@hidden] in
let transfer_to_contract#151funtype p : * . contract (p) -> p -> tez -> unit =
  Λ p ->
lambda (_ccontract (p))p -> tez -> unit return lambda (_sp)tez -> unit return lambda (_ttez)unit return
(stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
let transfer_to_contract_exn#152funtype p : * . contract (p) -> p -> tez -> nat =
  Λ p ->
lambda (_ccontract (p))p -> tez -> nat return lambda (_sp)tez -> nat return lambda (_ttez)nat return
(stub#95@{unit}@{nat})@(unit)[@inline][@hidden] in
let michelson_equal#153unit -> unit -> bool =
  lambda (_m1unit)unit -> bool return lambda (_m2unit)bool return (stub#95@{unit}@{bool})@(unit)[@inline][@hidden] in
let to_entrypoint#154funtype a : * . funtype b : * . funtype c : * . string -> unit -> contract (c) =
  Λ a ->
Λ b ->
Λ c ->
lambda (_sstring)unit -> contract (c) return lambda (_tunit)contract (c) return
(stub#95@{unit}@{contract (c)})@(unit)[@inline][@hidden] in
let originate_contract#155unit -> unit -> tez -> address =
  lambda (_cunit)unit -> tez -> address return lambda (_sunit)tez -> address return lambda (_ttez)address return
(stub#95@{unit}@{address})@(unit)[@inline][@hidden] in
let originate#156funtype p : * . funtype s : * . ( p * s ) -> ( list (operation) * s ) -> s -> tez ->
( unit * unit * int ) = Λ p ->
Λ s ->
lambda (_f( p * s ) -> ( list (operation) * s ))s -> tez -> ( unit * unit * int ) return lambda (_ss)tez ->
( unit * unit * int ) return lambda (_ttez)( unit * unit * int ) return
(stub#95@{unit}@{( unit * unit * int )})@(unit)[@inline][@hidden] in
let compile_contract_from_file#157string -> string -> list (string) -> unit =
  lambda (_fnstring)string -> list (string) -> unit return lambda (_estring)list (string) -> unit return lambda (_vlist (string))unit return
(stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
let originate_from_file#158string -> string -> list (string) -> unit -> tez ->
( address * unit * int ) =
  lambda (_fnstring)string -> list (string) -> unit -> tez -> ( address * unit * int ) return lambda (_estring)list (string) -> unit -> tez ->
( address * unit * int ) return lambda (_vlist (string))unit -> tez ->
( address * unit * int ) return lambda (_sunit)tez -> ( address * unit * int ) return lambda (_ttez)
( address * unit * int ) return (stub#95@{unit}@{( address * unit * int )})@(unit)[@inline][@hidden] in
let a#159int = 42 in let b#160int = 1 in let xint = a#159 in unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias2.mligo" ] ;
  [%expect {xxx|
    let get_balance#8unit -> tez =
      lambda (_uunit)tez return ([%Michelson {| { DROP ; BALANCE } |}])@(unit)[@inline][@hidden] in
    let get_amount#9unit -> tez =
      lambda (_uunit)tez return ([%Michelson {| { DROP ; AMOUNT } |}])@(unit)[@inline][@hidden] in
    let get_now#10unit -> timestamp =
      lambda (_uunit)timestamp return ([%Michelson {| { DROP ; NOW } |}])@(unit)[@inline][@hidden] in
    let get_sender#11unit -> address =
      lambda (_uunit)address return ([%Michelson {| { DROP ; SENDER } |}])@(unit)[@inline][@hidden] in
    let get_source#12unit -> address =
      lambda (_uunit)address return ([%Michelson {| { DROP ; SOURCE } |}])@(unit)[@inline][@hidden] in
    let get_level#13unit -> nat =
      lambda (_uunit)nat return ([%Michelson {| { DROP ; LEVEL } |}])@(unit)[@inline][@hidden] in
    let get_self_address#14unit -> address =
      lambda (_uunit)address return SELF_ADDRESS()[@inline][@hidden] in
    let get_chain_id#15unit -> chain_id =
      lambda (_uunit)chain_id return ([%Michelson {| { DROP ; CHAIN_ID } |}])@(unit)[@inline][@hidden] in
    let get_total_voting_power#16unit -> nat =
      lambda (_uunit)nat return ([%Michelson {| { DROP ; TOTAL_VOTING_POWER } |}])@(unit)[@inline][@hidden] in
    let get_min_block_time#17unit -> nat =
      lambda (_uunit)nat return ([%Michelson {| { DROP; MIN_BLOCK_TIME } |}])@(unit)[@inline][@hidden] in
    let voting_power#18key_hash -> nat =
      lambda (khkey_hash)nat return ([%Michelson {| { VOTING_POWER } |}])@(kh)[@inline][@hidden] in
    let address#19funtype a : * . contract (a) -> address = Λ a ->
    lambda (ccontract (a))address return ADDRESS(c)[@inline][@hidden] in
    let implicit_account#20key_hash -> contract (unit) =
      lambda (khkey_hash)contract (unit) return IMPLICIT_ACCOUNT(kh)[@inline][@hidden] in
    let join_tickets#21funtype a : * . ( ticket (a) * ticket (a) ) -> option (ticket (a)) =
      Λ a ->
    lambda (t( ticket (a) * ticket (a) ))option (ticket (a)) return ([%Michelson {| { JOIN_TICKETS } |}])@(t)[@inline][@hidden] in
    let read_ticket#22funtype a : * . ticket (a) -> ( ( address * ( a * nat ) ) * ticket (a) ) =
      Λ a ->
    lambda (tticket (a))( ( address * ( a * nat ) ) * ticket (a) ) return
    ([%Michelson {| { READ_TICKET ; PAIR } |}])@(t)[@inline][@hidden] in
    let never#23funtype a : * . never -> a = Λ a ->
    lambda (nnever)a return ([%Michelson {| { NEVER } |}])@(n)[@inline][@hidden] in
    let pairing_check#24list (( bls12_381_g1 * bls12_381_g2 )) -> bool =
      lambda (llist (( bls12_381_g1 * bls12_381_g2 )))bool return ([%Michelson {| { PAIRING_CHECK } |}])@(l)[@inline][@hidden] in
    let constant#25funtype a : * . string -> a = Λ a ->
    lambda (sstring)a return GLOBAL_CONSTANT(s)[@inline][@hidden] in
    let set_delegate#26option (key_hash) -> operation =
      lambda (ooption (key_hash))operation return SET_DELEGATE(o)[@inline][@hidden] in
    let get_contract#27funtype a : * . address -> contract (a) = Λ a ->
    lambda (aaddress)contract (a) return CONTRACT(a)[@inline][@hidden] in
    let get_contract_opt#28funtype a : * . address -> option (contract (a)) =
      Λ a ->
    lambda (aaddress)option (contract (a)) return CONTRACT_OPT(a)[@inline][@hidden] in
    let get_contract_with_error#29funtype a : * . address -> string -> contract (a) =
      Λ a ->
    lambda (aaddress)string -> contract (a) return lambda (sstring)contract (a) return
    CONTRACT_WITH_ERROR(a , s)[@inline][@hidden] in
    let create_ticket#30funtype a : * . a -> nat -> ticket (a) = Λ a ->
    lambda (va)nat -> ticket (a) return lambda (nnat)ticket (a) return ([%Michelson {| { UNPAIR ; TICKET } |}])@(
                                                                       ( v ,
                                                                        n ))[@inline][@hidden] in
    let transaction#31funtype a : * . a -> tez -> contract (a) -> operation =
      Λ a ->
    lambda (aa)tez -> contract (a) -> operation return lambda (mutez)contract (a) -> operation return lambda (ccontract (a))operation return
    CALL(a , mu , c)[@inline][@hidden] in
    let open_chest#32chest_key -> chest -> nat -> sum[Fail_decrypt -> unit , Fail_timelock -> unit , Ok_opening -> bytes] =
      lambda (ckchest_key)chest -> nat -> sum[Fail_decrypt -> unit , Fail_timelock -> unit , Ok_opening -> bytes] return lambda (cchest)nat ->
    sum[Fail_decrypt -> unit , Fail_timelock -> unit , Ok_opening -> bytes] return lambda (nnat)
    sum[Fail_decrypt -> unit , Fail_timelock -> unit , Ok_opening -> bytes] return
    OPEN_CHEST(ck , c , n)[@inline][@hidden] in
    let call_view#33funtype a : * . funtype b : * . string -> a -> address -> option (b) =
      Λ a ->
    Λ b ->
    lambda (sstring)a -> address -> option (b) return lambda (xa)address -> option (b) return lambda (aaddress)option (b) return
    VIEW(s , x , a)[@inline][@hidden] in
    let split_ticket#34funtype a : * . ticket (a) -> ( nat * nat ) -> option (
    ( ticket (a) * ticket (a) )) = Λ a ->
    lambda (tticket (a))( nat * nat ) -> option (( ticket (a) * ticket (a) )) return lambda (p
    ( nat * nat ))option (( ticket (a) * ticket (a) )) return ([%Michelson {| { UNPAIR ; SPLIT_TICKET } |}])@(
                                                              ( t ,
                                                                p ))[@inline][@hidden] in
    let xor#35nat -> nat -> nat =
      lambda (lnat)nat -> nat return lambda (rnat)nat return XOR(l , r)[@inline][@hidden] in
    let shift_left#36nat -> nat -> nat =
      lambda (lnat)nat -> nat return lambda (rnat)nat return LSL(l , r)[@inline][@hidden] in
    let shift_right#37nat -> nat -> nat =
      lambda (lnat)nat -> nat return lambda (rnat)nat return LSR(l , r)[@inline][@hidden] in
    let empty#38funtype k : * . funtype v : * . big_map (k , v) = Λ k ->
    Λ v ->  BIG_MAP_EMPTY()[@inline][@hidden] in
    let mem#39funtype k : * . funtype v : * . k -> big_map (k , v) -> bool =
      Λ k ->
    Λ v ->
    lambda (kk)big_map (k , v) -> bool return lambda (mbig_map (k ,
    v))bool return ([%Michelson {| { UNPAIR ; MEM } |}])@(( k , m ))[@inline][@hidden] in
    let add#40funtype k : * . funtype v : * . k -> v -> big_map (k ,
    v) -> big_map (k , v) = Λ k ->
    Λ v ->
    lambda (kk)v -> big_map (k , v) -> big_map (k ,
    v) return lambda (vv)big_map (k , v) -> big_map (k ,
    v) return lambda (mbig_map (k , v))big_map (k , v) return MAP_ADD(k , v , m)[@inline][@hidden] in
    let remove#41funtype k : * . funtype v : * . k -> big_map (k ,
    v) -> big_map (k , v) = Λ k ->
    Λ v ->
    lambda (kk)big_map (k , v) -> big_map (k , v) return lambda (mbig_map (k ,
    v))big_map (k , v) return MAP_REMOVE(k , m)[@inline][@hidden] in
    let update#42funtype k : * . funtype v : * . k -> option (v) -> big_map (k ,
    v) -> big_map (k , v) = Λ k ->
    Λ v ->
    lambda (kk)option (v) -> big_map (k , v) -> big_map (k ,
    v) return lambda (voption (v))big_map (k , v) -> big_map (k ,
    v) return lambda (mbig_map (k , v))big_map (k ,
    v) return MAP_UPDATE(k , v , m)[@inline][@hidden] in
    let get_and_update#43funtype k : * . funtype v : * . k -> option (v) -> big_map (k ,
    v) -> ( option (v) * big_map (k , v) ) = Λ k ->
    Λ v ->
    lambda (kk)option (v) -> big_map (k ,
    v) -> ( option (v) * big_map (k , v) ) return lambda (voption (v))big_map (k ,
    v) -> ( option (v) * big_map (k , v) ) return lambda (mbig_map (k ,
    v))( option (v) * big_map (k , v) ) return BIG_MAP_GET_AND_UPDATE(k , v , m)[@inline][@hidden] in
    let find_opt#44funtype k : * . funtype v : * . k -> big_map (k ,
    v) -> option (v) = Λ k ->
    Λ v ->
    lambda (kk)big_map (k , v) -> option (v) return lambda (mbig_map (k ,
    v))option (v) return MAP_FIND_OPT(k , m)[@inline][@hidden] in
    let find#45funtype k : * . funtype v : * . k -> big_map (k , v) -> v =
      Λ k ->
    Λ v ->
    lambda (kk)big_map (k , v) -> v return lambda (mbig_map (k ,
    v))v return MAP_FIND(k , m)[@inline][@hidden] in
    let empty#46funtype k : * . funtype v : * . map (k , v) = Λ k ->
    Λ v ->  MAP_EMPTY()[@inline][@hidden] in
    let size#47funtype k : * . funtype v : * . map (k , v) -> nat = Λ k ->
    Λ v ->  lambda (mmap (k , v))nat return ([%Michelson {| { SIZE } |}])@(m)[@inline][@hidden] in
    let mem#48funtype k : * . funtype v : * . k -> map (k , v) -> bool = Λ k ->
    Λ v ->
    lambda (kk)map (k , v) -> bool return lambda (mmap (k ,
    v))bool return ([%Michelson {| { UNPAIR ; MEM } |}])@(( k , m ))[@inline][@hidden] in
    let add#49funtype k : * . funtype v : * . k -> v -> map (k , v) -> map (k ,
    v) = Λ k ->
    Λ v ->
    lambda (kk)v -> map (k , v) -> map (k , v) return lambda (vv)map (k ,
    v) -> map (k , v) return lambda (mmap (k , v))map (k ,
    v) return MAP_ADD(k , v , m)[@inline][@hidden] in
    let remove#50funtype k : * . funtype v : * . k -> map (k , v) -> map (k ,
    v) = Λ k ->
    Λ v ->
    lambda (kk)map (k , v) -> map (k , v) return lambda (mmap (k , v))map (k ,
    v) return MAP_REMOVE(k , m)[@inline][@hidden] in
    let update#51funtype k : * . funtype v : * . k -> option (v) -> map (k ,
    v) -> map (k , v) = Λ k ->
    Λ v ->
    lambda (kk)option (v) -> map (k , v) -> map (k ,
    v) return lambda (voption (v))map (k , v) -> map (k ,
    v) return lambda (mmap (k , v))map (k , v) return MAP_UPDATE(k , v , m)[@inline][@hidden] in
    let get_and_update#52funtype k : * . funtype v : * . k -> option (v) -> map (k ,
    v) -> ( option (v) * map (k , v) ) = Λ k ->
    Λ v ->
    lambda (kk)option (v) -> map (k ,
    v) -> ( option (v) * map (k , v) ) return lambda (voption (v))map (k ,
    v) -> ( option (v) * map (k , v) ) return lambda (mmap (k ,
    v))( option (v) * map (k , v) ) return MAP_GET_AND_UPDATE(k , v , m)[@inline][@hidden] in
    let find#53funtype k : * . funtype v : * . k -> map (k , v) -> v = Λ k ->
    Λ v ->
    lambda (kk)map (k , v) -> v return lambda (mmap (k ,
    v))v return MAP_FIND(k , m)[@inline][@hidden] in
    let find_opt#54funtype k : * . funtype v : * . k -> map (k ,
    v) -> option (v) = Λ k ->
    Λ v ->
    lambda (kk)map (k , v) -> option (v) return lambda (mmap (k ,
    v))option (v) return MAP_FIND_OPT(k , m)[@inline][@hidden] in
    let iter#55funtype k : * . funtype v : * . ( k * v ) -> unit -> map (k ,
    v) -> unit = Λ k ->
    Λ v ->
    lambda (f( k * v ) -> unit)map (k , v) -> unit return lambda (mmap (k ,
    v))unit return MAP_ITER(f , m)[@inline][@hidden] in
    let map#56funtype k : * . funtype v : * . funtype w : * . ( k * v ) -> w -> map (k ,
    v) -> map (k , w) = Λ k ->
    Λ v ->
    Λ w ->
    lambda (f( k * v ) -> w)map (k , v) -> map (k , w) return lambda (mmap (k ,
    v))map (k , w) return MAP_MAP(f , m)[@inline][@hidden] in
    let fold#57funtype k : * . funtype v : * . funtype c : * . ( c * ( k * v ) ) -> c -> map (k ,
    v) -> c -> c = Λ k ->
    Λ v ->
    Λ c ->
    lambda (f( c * ( k * v ) ) -> c)map (k ,
    v) -> c -> c return lambda (mmap (k ,
    v))c -> c return lambda (ic)c return MAP_FOLD(f , m , i)[@inline][@hidden] in
    let empty#58funtype a : * . set (a) = Λ a ->
    SET_EMPTY()[@inline][@hidden] in
    let size#59funtype a : * . set (a) -> nat = Λ a ->
    lambda (sset (a))nat return ([%Michelson {| { SIZE } |}])@(s)[@inline][@hidden] in
    let cardinal#60funtype a : * . set (a) -> nat = Λ a ->
    lambda (sset (a))nat return ([%Michelson {| { SIZE } |}])@(s)[@inline][@hidden] in
    let mem#61funtype a : * . a -> set (a) -> bool = Λ a ->
    lambda (xa)set (a) -> bool return lambda (sset (a))bool return SET_MEM(x , s)[@inline][@hidden] in
    let add#62funtype a : * . a -> set (a) -> set (a) = Λ a ->
    lambda (xa)set (a) -> set (a) return lambda (sset (a))set (a) return
    SET_ADD(x , s)[@inline][@hidden] in
    let remove#63funtype a : * . a -> set (a) -> set (a) = Λ a ->
    lambda (xa)set (a) -> set (a) return lambda (sset (a))set (a) return
    SET_REMOVE(x , s)[@inline][@hidden] in
    let update#64funtype a : * . a -> bool -> set (a) -> set (a) = Λ a ->
    lambda (xa)bool -> set (a) -> set (a) return lambda (bbool)set (a) -> set (a) return lambda (sset (a))set (a) return
    SET_UPDATE(x , b , s)[@inline][@hidden] in
    let iter#65funtype a : * . a -> unit -> set (a) -> unit = Λ a ->
    lambda (fa -> unit)set (a) -> unit return lambda (sset (a))unit return
    SET_ITER(f , s)[@inline][@hidden] in
    let fold#66funtype a : * . funtype b : * . ( b * a ) -> b -> set (a) -> b -> b =
      Λ a ->
    Λ b ->
    lambda (f( b * a ) -> b)set (a) -> b -> b return lambda (sset (a))b -> b return lambda (ib)b return
    SET_FOLD(f , s , i)[@inline][@hidden] in
    let fold_desc#67funtype a : * . funtype b : * . ( a * b ) -> b -> set (a) -> b -> b =
      Λ a ->
    Λ b ->
    lambda (f( a * b ) -> b)set (a) -> b -> b return lambda (sset (a))b -> b return lambda (ib)b return
    SET_FOLD_DESC(f , s , i)[@inline][@hidden] in
    let length#68funtype a : * . list (a) -> nat = Λ a ->
    lambda (xslist (a))nat return ([%Michelson {| { SIZE } |}])@(xs)[@inline][@hidden] in
    let size#69funtype a : * . list (a) -> nat = Λ a ->
    lambda (xslist (a))nat return ([%Michelson {| { SIZE } |}])@(xs)[@inline][@hidden] in
    let head_opt#70funtype a : * . list (a) -> option (a) = Λ a ->
    lambda (xslist (a))option (a) return  match xs with
                                           | Cons ctor_proj#2 ->
                                              match ctor_proj#2 with
                                               | ( x , _#2 ) ->
                                               Some(x)
                                           | Nil unit_proj#4 ->
                                             None(unit)[@inline][@hidden] in
    let tail_opt#71funtype a : * . list (a) -> option (list (a)) = Λ a ->
    lambda (xslist (a))option (list (a)) return  match xs with
                                                  | Cons ctor_proj#5 ->
                                                     match ctor_proj#5 with
                                                      | ( _#3 , xs ) ->
                                                      Some(xs)
                                                  | Nil unit_proj#7 ->
                                                    None(unit)[@inline][@hidden] in
    let map#72funtype a : * . funtype b : * . a -> b -> list (a) -> list (b) =
      Λ a ->
    Λ b ->
    lambda (fa -> b)list (a) -> list (b) return lambda (xslist (a))list (b) return
    LIST_MAP(f , xs)[@inline][@hidden] in
    let iter#73funtype a : * . a -> unit -> list (a) -> unit = Λ a ->
    lambda (fa -> unit)list (a) -> unit return lambda (xslist (a))unit return
    LIST_ITER(f , xs)[@inline][@hidden] in
    let fold#74funtype a : * . funtype b : * . ( b * a ) -> b -> list (a) -> b -> b =
      Λ a ->
    Λ b ->
    lambda (f( b * a ) -> b)list (a) -> b -> b return lambda (xslist (a))b -> b return lambda (ib)b return
    LIST_FOLD(f , xs , i)[@inline][@hidden] in
    let fold_left#75funtype a : * . funtype b : * . ( b * a ) -> b -> b -> list (a) -> b =
      Λ a ->
    Λ b ->
    lambda (f( b * a ) -> b)b -> list (a) -> b return lambda (ib)list (a) -> b return lambda (xslist (a))b return
    LIST_FOLD_LEFT(f , i , xs)[@inline][@hidden] in
    let fold_right#76funtype a : * . funtype b : * . ( a * b ) -> b -> list (a) -> b -> b =
      Λ a ->
    Λ b ->
    lambda (f( a * b ) -> b)list (a) -> b -> b return lambda (xslist (a))b -> b return lambda (ib)b return
    LIST_FOLD_RIGHT(f , xs , i)[@inline][@hidden] in
    let cons#77funtype a : * . a -> list (a) -> list (a) = Λ a ->
    lambda (xa)list (a) -> list (a) return lambda (xslist (a))list (a) return
    CONS(x , xs)[@inline][@hidden] in
    let length#78string -> nat =
      lambda (bstring)nat return ([%Michelson {| { SIZE } |}])@(b)[@inline][@hidden] in
    let concat#79string -> string -> string =
      lambda (b1string)string -> string return lambda (b2string)string return
    ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b1 , b2 ))[@inline][@hidden] in
    let sub#80nat -> nat -> string -> string =
      lambda (snat)nat -> string -> string return lambda (lnat)string -> string return lambda (bstring)string return
    ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(
    ( s ,
      l ,
      b ))[@inline][@hidden] in
    let unopt#81funtype a : * . option (a) -> a = Λ a ->
    lambda (voption (a))a return UNOPT(v)[@inline][@hidden] in
    let unopt_with_error#82funtype a : * . option (a) -> string -> a = Λ a ->
    lambda (voption (a))string -> a return lambda (sstring)a return UNOPT_WITH_ERROR
                                                                    (v ,
                                                                     s)[@inline][@hidden] in
    let pack#83funtype a : * . a -> bytes = Λ a ->
    lambda (va)bytes return ([%Michelson {| { PACK } |}])@(v)[@inline][@hidden] in
    let unpack#84funtype a : * . bytes -> option (a) = Λ a ->
    lambda (bbytes)option (a) return BYTES_UNPACK(b)[@inline][@hidden] in
    let length#85bytes -> nat =
      lambda (bbytes)nat return ([%Michelson {| { SIZE } |}])@(b)[@inline][@hidden] in
    let concat#86bytes -> bytes -> bytes =
      lambda (b1bytes)bytes -> bytes return lambda (b2bytes)bytes return
    ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b1 , b2 ))[@inline][@hidden] in
    let sub#87nat -> nat -> bytes -> bytes =
      lambda (snat)nat -> bytes -> bytes return lambda (lnat)bytes -> bytes return lambda (bbytes)bytes return
    ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(
    ( s ,
      l ,
      b ))[@inline][@hidden] in
    let blake2b#88bytes -> bytes =
      lambda (bbytes)bytes return ([%Michelson {| { BLAKE2B } |}])@(b)[@inline][@hidden] in
    let sha256#89bytes -> bytes =
      lambda (bbytes)bytes return ([%Michelson {| { SHA256 } |}])@(b)[@inline][@hidden] in
    let sha512#90bytes -> bytes =
      lambda (bbytes)bytes return ([%Michelson {| { SHA512 } |}])@(b)[@inline][@hidden] in
    let sha3#91bytes -> bytes =
      lambda (bbytes)bytes return ([%Michelson {| { SHA3 } |}])@(b)[@inline][@hidden] in
    let keccak#92bytes -> bytes =
      lambda (bbytes)bytes return ([%Michelson {| { KECCAK } |}])@(b)[@inline][@hidden] in
    let hash_key#93key -> key_hash =
      lambda (kkey)key_hash return ([%Michelson {| { HASH_KEY } |}])@(k)[@inline][@hidden] in
    let check#94key -> signature -> bytes -> bool =
      lambda (kkey)signature -> bytes -> bool return lambda (ssignature)bytes -> bool return lambda (bbytes)bool return
    ([%Michelson {| { UNPAIR ; UNPAIR ; CHECK_SIGNATURE } |}])@(( k , s , b ))[@inline][@hidden] in
    let assertbool -> unit =
      lambda (bbool)unit return ([%Michelson {| { IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } } |}])@(b)[@inline][@private][@hidden] in
    let assert_somefuntype a : * . option (a) -> unit = Λ a ->
    lambda (voption (a))unit return ([%Michelson {| { IF_NONE { PUSH string "failed assert some" ; FAILWITH } { DROP ; UNIT } } |}])@(v)[@inline][@private][@hidden] in
    let assert_nonefuntype a : * . option (a) -> unit = Λ a ->
    lambda (voption (a))unit return ([%Michelson {| { IF_NONE { UNIT } { PUSH string "failed assert none" ; FAILWITH } } |}])@(v)[@inline][@private][@hidden] in
    let absint -> nat =
      lambda (iint)nat return ([%Michelson {| { ABS } |}])@(i)[@inline][@private][@hidden] in
    let is_natint -> option (nat) =
      lambda (iint)option (nat) return ([%Michelson {| { ISNAT } |}])@(i)[@inline][@private][@hidden] in
    let truebool = TRUE()[@inline][@private][@hidden] in
    let falsebool = FALSE()[@inline][@private][@hidden] in
    let unitunit = UNIT()[@inline][@private][@hidden] in
    let failwithfuntype a : * . funtype b : * . a -> b = Λ a ->
    Λ b ->  [%Michelson {|{ FAILWITH }|}][@inline][@private][@hidden] in
    let intfuntype a : * . a -> external_int (a) = Λ a ->
    lambda (va)external_int (a) return ([%Michelson {| { INT } |}])@(v)[@inline][@private][@hidden] in
    let assert_with_errorbool -> string -> unit =
      lambda (bbool)string -> unit return lambda (sstring)unit return ([%Michelson {| { UNPAIR ; IF { DROP ; UNIT } { FAILWITH } } |}])@(
                                                                      ( b ,
                                                                        s ))[@inline][@private][@hidden] in
    let assert_some_with_errorfuntype a : * . option (a) -> string -> unit =
      Λ a ->
    lambda (voption (a))string -> unit return lambda (sstring)unit return
    ([%Michelson {| { UNPAIR ; IF_NONE { FAILWITH } { DROP 2 ; UNIT } } |}])@(
    ( v ,
      s ))[@inline][@private][@hidden] in
    let assert_none_with_errorfuntype a : * . option (a) -> string -> unit =
      Λ a ->
    lambda (voption (a))string -> unit return lambda (sstring)unit return
    ([%Michelson {| { UNPAIR ; IF_NONE { DROP ; UNIT } { DROP ; FAILWITH } } |}])@(
    ( v ,
      s ))[@inline][@private][@hidden] in
    let edivfuntype a : * . funtype b : * . a -> b -> external_ediv (a , b) =
      Λ a ->
    Λ b ->
    lambda (la)b -> external_ediv (a , b) return lambda (rb)external_ediv (a ,
    b) return ([%Michelson {| { UNPAIR ; EDIV } |}])@(( l , r ))[@inline][@private][@hidden] in
    let stub#95funtype a : * . funtype b : * . a -> b = Λ a ->
    Λ b ->  lambda (xa)b return ([%Michelson {|{ FAILWITH }|}])@(x)[@inline][@private][@hidden] in
    let run#96funtype a : * . funtype b : * . a -> b -> a -> unit = Λ a ->
    Λ b ->
    lambda (_fa -> b)a -> unit return lambda (_va)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
    let eval#97funtype a : * . a -> unit = Λ a ->
    lambda (_xa)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
    let compile_value#98funtype a : * . a -> unit = Λ a ->
    lambda (_xa)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
    let get_total_voting_power#99unit -> nat =
      lambda (_uunit)nat return (stub#95@{unit}@{nat})@(unit)[@inline][@hidden] in
    let failwith#100funtype a : * . funtype b : * . a -> b = Λ a ->
    Λ b ->  lambda (_va)b return (stub#95@{unit}@{b})@(unit)[@inline][@hidden] in
    let to_contract#101funtype p : * . funtype s : * . unit -> contract (p) =
      Λ p ->
    Λ s ->
    lambda (_tunit)contract (p) return (stub#95@{unit}@{contract (p)})@(unit)[@inline][@hidden] in
    let set_source#102address -> unit =
      lambda (_aaddress)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
    let get_storage_of_address#103address -> unit =
      lambda (_aaddress)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
    let get_balance#104address -> tez =
      lambda (_aaddress)tez return (stub#95@{unit}@{tez})@(unit)[@inline][@hidden] in
    let print#105string -> unit =
      lambda (_vstring)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
    let eprint#106string -> unit =
      lambda (_vstring)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
    let get_voting_power#107key_hash -> nat =
      lambda (_khkey_hash)nat return (stub#95@{unit}@{nat})@(unit)[@inline][@hidden] in
    let nth_bootstrap_contract#108nat -> address =
      lambda (_inat)address return (stub#95@{unit}@{address})@(unit)[@inline][@hidden] in
    let nth_bootstrap_account#109int -> address =
      lambda (_iint)address return (stub#95@{unit}@{address})@(unit)[@inline][@hidden] in
    let get_bootstrap_account#110nat -> ( address * key * string ) =
      lambda (_nnat)( address * key * string ) return (stub#95@{unit}@{( address * key * string )})@(unit)[@inline][@hidden] in
    let nth_bootstrap_typed_address#111funtype a : * . funtype b : * . nat -> unit =
      Λ a ->
    Λ b ->  lambda (_nnat)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
    let last_originations#112unit -> map (address , list (address)) =
      lambda (_uunit)map (address ,
    list (address)) return (stub#95@{unit}@{map (address ,
                           list (address))})@(unit)[@inline][@hidden] in
    let random#113funtype a : * . unit -> a = Λ a ->
    lambda (_uunit)a return (stub#95@{unit}@{a})@(unit)[@inline][@hidden] in
    let new_account#114unit -> ( string * key ) =
      lambda (_uunit)( string * key ) return (stub#95@{unit}@{( string * key )})@(unit)[@inline][@hidden] in
    let decompile#115funtype a : * . unit -> a = Λ a ->
    lambda (_munit)a return (stub#95@{unit}@{a})@(unit)[@inline][@hidden] in
    let bake_until_n_cycle_end#116nat -> unit =
      lambda (_nnat)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
    let cast_address#117funtype a : * . funtype b : * . address -> unit =
      Λ a ->
    Λ b ->  lambda (_aaddress)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
    let register_delegate#118key_hash -> unit =
      lambda (_khkey_hash)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
    let register_constant#119unit -> string =
      lambda (_munit)string return (stub#95@{unit}@{string})@(unit)[@inline][@hidden] in
    let to_typed_address#120funtype a : * . funtype b : * . contract (a) -> unit =
      Λ a ->
    Λ b ->  lambda (_ccontract (a))unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
    let constant_to_michelson_program#121string -> unit =
      lambda (_sstring)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
    let restore_context#122unit -> unit =
      lambda (_uunit)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
    let save_context#123unit -> unit =
      lambda (_uunit)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
    let drop_context#124unit -> unit =
      lambda (_uunit)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
    let to_string#125funtype a : * . a -> string = Λ a ->
    lambda (_va)string return (stub#95@{unit}@{string})@(unit)[@inline][@hidden] in
    let get_storage#126funtype p : * . funtype s : * . unit -> s = Λ p ->
    Λ s ->  lambda (_tunit)s return (stub#95@{unit}@{s})@(unit)[@inline][@hidden] in
    let set_baker_policy#127unit -> unit =
      lambda (_bpunit)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
    let set_baker#128address -> unit =
      lambda (_aaddress)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
    let size#129unit -> int =
      lambda (_cunit)int return (stub#95@{unit}@{int})@(unit)[@inline][@hidden] in
    let compile_contract#130funtype p : * . funtype s : * . ( p * s ) ->
    ( list (operation) * s ) -> unit = Λ p ->
    Λ s ->
    lambda (_f( p * s ) -> ( list (operation) * s ))unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
    let read_contract_from_file#131string -> unit =
      lambda (_fnstring)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
    let chr#132nat -> option (string) =
      lambda (_nnat)option (string) return (stub#95@{unit}@{option (string)})@(unit)[@inline][@hidden] in
    let nl#133string = "NEWLINE"[@inline][@hidden] in
    let println#134string -> unit =
      lambda (_vstring)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
    let transfer#135address -> unit -> tez -> unit =
      lambda (_aaddress)unit -> tez -> unit return lambda (_sunit)tez -> unit return lambda (_ttez)unit return
    (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
    let transfer_exn#136address -> unit -> tez -> nat =
      lambda (_aaddress)unit -> tez -> nat return lambda (_sunit)tez -> nat return lambda (_ttez)nat return
    (stub#95@{unit}@{nat})@(unit)[@inline][@hidden] in
    let log#137funtype a : * . a -> unit = Λ a ->
    lambda (_va)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
    let reset_state#138nat -> list (tez) -> unit =
      lambda (_nnat)list (tez) -> unit return lambda (_llist (tez))unit return
    (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
    let reset_state_at#139timestamp -> nat -> list (tez) -> unit =
      lambda (_ttimestamp)nat -> list (tez) -> unit return lambda (_nnat)list (tez) -> unit return lambda (_llist (tez))unit return
    (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
    let bootstrap_contract#140funtype p : * . funtype s : * . ( p * s ) ->
    ( list (operation) * s ) -> s -> tez -> unit = Λ p ->
    Λ s ->
    lambda (_f( p * s ) -> ( list (operation) * s ))s -> tez -> unit return lambda (_ss)tez -> unit return lambda (_ttez)unit return
    (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
    let mutate_value#141funtype a : * . nat -> a -> option (( a * unit )) =
      Λ a ->
    lambda (_nnat)a -> option (( a * unit )) return lambda (_va)option (( a * unit )) return
    (stub#95@{unit}@{option (( a * unit ))})@(unit)[@inline][@hidden] in
    let save_mutation#142string -> unit -> option (string) =
      lambda (_sstring)unit -> option (string) return lambda (_munit)option (string) return
    (stub#95@{unit}@{option (string)})@(unit)[@inline][@hidden] in
    let mutation_test#143funtype a : * . funtype b : * . a -> a -> b -> option (
    ( b * unit )) = Λ a ->
    Λ b ->
    lambda (_va)a -> b -> option (( b * unit )) return lambda (_fa -> b)option (
    ( b * unit )) return (stub#95@{unit}@{option (( b * unit ))})@(unit)[@inline][@hidden] in
    let mutation_test_all#144funtype a : * . funtype b : * . a -> a -> b -> list (
    ( b * unit )) = Λ a ->
    Λ b ->
    lambda (_va)a -> b -> list (( b * unit )) return lambda (_fa -> b)list (
    ( b * unit )) return (stub#95@{unit}@{list (( b * unit ))})@(unit)[@inline][@hidden] in
    let sign#145string -> bytes -> signature =
      lambda (_skstring)bytes -> signature return lambda (_dbytes)signature return
    (stub#95@{unit}@{signature})@(unit)[@inline][@hidden] in
    let add_account#146string -> key -> unit =
      lambda (_sstring)key -> unit return lambda (_kkey)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
    let baker_account#147( string * key ) -> option (tez) -> unit =
      lambda (_p( string * key ))option (tez) -> unit return lambda (_ooption (tez))unit return
    (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
    let set_big_map#148funtype a : * . funtype b : * . int -> big_map (a ,
    b) -> unit = Λ a ->
    Λ b ->
    lambda (_iint)big_map (a , b) -> unit return lambda (_mbig_map (a ,
    b))unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
    let create_chest#149bytes -> nat -> ( chest * chest_key ) =
      lambda (_bbytes)nat -> ( chest * chest_key ) return lambda (_nnat)( chest * chest_key ) return
    (stub#95@{unit}@{( chest * chest_key )})@(unit)[@inline][@hidden] in
    let create_chest_key#150chest -> nat -> chest_key =
      lambda (_cchest)nat -> chest_key return lambda (_nnat)chest_key return
    (stub#95@{unit}@{chest_key})@(unit)[@inline][@hidden] in
    let transfer_to_contract#151funtype p : * . contract (p) -> p -> tez -> unit =
      Λ p ->
    lambda (_ccontract (p))p -> tez -> unit return lambda (_sp)tez -> unit return lambda (_ttez)unit return
    (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
    let transfer_to_contract_exn#152funtype p : * . contract (p) -> p -> tez -> nat =
      Λ p ->
    lambda (_ccontract (p))p -> tez -> nat return lambda (_sp)tez -> nat return lambda (_ttez)nat return
    (stub#95@{unit}@{nat})@(unit)[@inline][@hidden] in
    let michelson_equal#153unit -> unit -> bool =
      lambda (_m1unit)unit -> bool return lambda (_m2unit)bool return (stub#95@{unit}@{bool})@(unit)[@inline][@hidden] in
    let to_entrypoint#154funtype a : * . funtype b : * . funtype c : * . string -> unit -> contract (c) =
      Λ a ->
    Λ b ->
    Λ c ->
    lambda (_sstring)unit -> contract (c) return lambda (_tunit)contract (c) return
    (stub#95@{unit}@{contract (c)})@(unit)[@inline][@hidden] in
    let originate_contract#155unit -> unit -> tez -> address =
      lambda (_cunit)unit -> tez -> address return lambda (_sunit)tez -> address return lambda (_ttez)address return
    (stub#95@{unit}@{address})@(unit)[@inline][@hidden] in
    let originate#156funtype p : * . funtype s : * . ( p * s ) -> ( list (operation) * s ) -> s -> tez ->
    ( unit * unit * int ) = Λ p ->
    Λ s ->
    lambda (_f( p * s ) -> ( list (operation) * s ))s -> tez -> ( unit * unit * int ) return lambda (_ss)tez ->
    ( unit * unit * int ) return lambda (_ttez)( unit * unit * int ) return
    (stub#95@{unit}@{( unit * unit * int )})@(unit)[@inline][@hidden] in
    let compile_contract_from_file#157string -> string -> list (string) -> unit =
      lambda (_fnstring)string -> list (string) -> unit return lambda (_estring)list (string) -> unit return lambda (_vlist (string))unit return
    (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
    let originate_from_file#158string -> string -> list (string) -> unit -> tez ->
    ( address * unit * int ) =
      lambda (_fnstring)string -> list (string) -> unit -> tez -> ( address * unit * int ) return lambda (_estring)list (string) -> unit -> tez ->
    ( address * unit * int ) return lambda (_vlist (string))unit -> tez ->
    ( address * unit * int ) return lambda (_sunit)tez -> ( address * unit * int ) return lambda (_ttez)
    ( address * unit * int ) return (stub#95@{unit}@{( address * unit * int )})@(unit)[@inline][@hidden] in
    let a#159int = 40 in
    let b#162int =
      let ba#160int = 1 in let baa#161int = ba#160 in ADD(ba#160 , baa#161) in
    let xint = ADD(a#159 , b#162) in unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias2.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias3.mligo" ] ;
  [%expect {xxx|
    let get_balance#8unit -> tez =
      lambda (_uunit)tez return ([%Michelson {| { DROP ; BALANCE } |}])@(unit)[@inline][@hidden] in
    let get_amount#9unit -> tez =
      lambda (_uunit)tez return ([%Michelson {| { DROP ; AMOUNT } |}])@(unit)[@inline][@hidden] in
    let get_now#10unit -> timestamp =
      lambda (_uunit)timestamp return ([%Michelson {| { DROP ; NOW } |}])@(unit)[@inline][@hidden] in
    let get_sender#11unit -> address =
      lambda (_uunit)address return ([%Michelson {| { DROP ; SENDER } |}])@(unit)[@inline][@hidden] in
    let get_source#12unit -> address =
      lambda (_uunit)address return ([%Michelson {| { DROP ; SOURCE } |}])@(unit)[@inline][@hidden] in
    let get_level#13unit -> nat =
      lambda (_uunit)nat return ([%Michelson {| { DROP ; LEVEL } |}])@(unit)[@inline][@hidden] in
    let get_self_address#14unit -> address =
      lambda (_uunit)address return SELF_ADDRESS()[@inline][@hidden] in
    let get_chain_id#15unit -> chain_id =
      lambda (_uunit)chain_id return ([%Michelson {| { DROP ; CHAIN_ID } |}])@(unit)[@inline][@hidden] in
    let get_total_voting_power#16unit -> nat =
      lambda (_uunit)nat return ([%Michelson {| { DROP ; TOTAL_VOTING_POWER } |}])@(unit)[@inline][@hidden] in
    let get_min_block_time#17unit -> nat =
      lambda (_uunit)nat return ([%Michelson {| { DROP; MIN_BLOCK_TIME } |}])@(unit)[@inline][@hidden] in
    let voting_power#18key_hash -> nat =
      lambda (khkey_hash)nat return ([%Michelson {| { VOTING_POWER } |}])@(kh)[@inline][@hidden] in
    let address#19funtype a : * . contract (a) -> address = Λ a ->
    lambda (ccontract (a))address return ADDRESS(c)[@inline][@hidden] in
    let implicit_account#20key_hash -> contract (unit) =
      lambda (khkey_hash)contract (unit) return IMPLICIT_ACCOUNT(kh)[@inline][@hidden] in
    let join_tickets#21funtype a : * . ( ticket (a) * ticket (a) ) -> option (ticket (a)) =
      Λ a ->
    lambda (t( ticket (a) * ticket (a) ))option (ticket (a)) return ([%Michelson {| { JOIN_TICKETS } |}])@(t)[@inline][@hidden] in
    let read_ticket#22funtype a : * . ticket (a) -> ( ( address * ( a * nat ) ) * ticket (a) ) =
      Λ a ->
    lambda (tticket (a))( ( address * ( a * nat ) ) * ticket (a) ) return
    ([%Michelson {| { READ_TICKET ; PAIR } |}])@(t)[@inline][@hidden] in
    let never#23funtype a : * . never -> a = Λ a ->
    lambda (nnever)a return ([%Michelson {| { NEVER } |}])@(n)[@inline][@hidden] in
    let pairing_check#24list (( bls12_381_g1 * bls12_381_g2 )) -> bool =
      lambda (llist (( bls12_381_g1 * bls12_381_g2 )))bool return ([%Michelson {| { PAIRING_CHECK } |}])@(l)[@inline][@hidden] in
    let constant#25funtype a : * . string -> a = Λ a ->
    lambda (sstring)a return GLOBAL_CONSTANT(s)[@inline][@hidden] in
    let set_delegate#26option (key_hash) -> operation =
      lambda (ooption (key_hash))operation return SET_DELEGATE(o)[@inline][@hidden] in
    let get_contract#27funtype a : * . address -> contract (a) = Λ a ->
    lambda (aaddress)contract (a) return CONTRACT(a)[@inline][@hidden] in
    let get_contract_opt#28funtype a : * . address -> option (contract (a)) =
      Λ a ->
    lambda (aaddress)option (contract (a)) return CONTRACT_OPT(a)[@inline][@hidden] in
    let get_contract_with_error#29funtype a : * . address -> string -> contract (a) =
      Λ a ->
    lambda (aaddress)string -> contract (a) return lambda (sstring)contract (a) return
    CONTRACT_WITH_ERROR(a , s)[@inline][@hidden] in
    let create_ticket#30funtype a : * . a -> nat -> ticket (a) = Λ a ->
    lambda (va)nat -> ticket (a) return lambda (nnat)ticket (a) return ([%Michelson {| { UNPAIR ; TICKET } |}])@(
                                                                       ( v ,
                                                                        n ))[@inline][@hidden] in
    let transaction#31funtype a : * . a -> tez -> contract (a) -> operation =
      Λ a ->
    lambda (aa)tez -> contract (a) -> operation return lambda (mutez)contract (a) -> operation return lambda (ccontract (a))operation return
    CALL(a , mu , c)[@inline][@hidden] in
    let open_chest#32chest_key -> chest -> nat -> sum[Fail_decrypt -> unit , Fail_timelock -> unit , Ok_opening -> bytes] =
      lambda (ckchest_key)chest -> nat -> sum[Fail_decrypt -> unit , Fail_timelock -> unit , Ok_opening -> bytes] return lambda (cchest)nat ->
    sum[Fail_decrypt -> unit , Fail_timelock -> unit , Ok_opening -> bytes] return lambda (nnat)
    sum[Fail_decrypt -> unit , Fail_timelock -> unit , Ok_opening -> bytes] return
    OPEN_CHEST(ck , c , n)[@inline][@hidden] in
    let call_view#33funtype a : * . funtype b : * . string -> a -> address -> option (b) =
      Λ a ->
    Λ b ->
    lambda (sstring)a -> address -> option (b) return lambda (xa)address -> option (b) return lambda (aaddress)option (b) return
    VIEW(s , x , a)[@inline][@hidden] in
    let split_ticket#34funtype a : * . ticket (a) -> ( nat * nat ) -> option (
    ( ticket (a) * ticket (a) )) = Λ a ->
    lambda (tticket (a))( nat * nat ) -> option (( ticket (a) * ticket (a) )) return lambda (p
    ( nat * nat ))option (( ticket (a) * ticket (a) )) return ([%Michelson {| { UNPAIR ; SPLIT_TICKET } |}])@(
                                                              ( t ,
                                                                p ))[@inline][@hidden] in
    let xor#35nat -> nat -> nat =
      lambda (lnat)nat -> nat return lambda (rnat)nat return XOR(l , r)[@inline][@hidden] in
    let shift_left#36nat -> nat -> nat =
      lambda (lnat)nat -> nat return lambda (rnat)nat return LSL(l , r)[@inline][@hidden] in
    let shift_right#37nat -> nat -> nat =
      lambda (lnat)nat -> nat return lambda (rnat)nat return LSR(l , r)[@inline][@hidden] in
    let empty#38funtype k : * . funtype v : * . big_map (k , v) = Λ k ->
    Λ v ->  BIG_MAP_EMPTY()[@inline][@hidden] in
    let mem#39funtype k : * . funtype v : * . k -> big_map (k , v) -> bool =
      Λ k ->
    Λ v ->
    lambda (kk)big_map (k , v) -> bool return lambda (mbig_map (k ,
    v))bool return ([%Michelson {| { UNPAIR ; MEM } |}])@(( k , m ))[@inline][@hidden] in
    let add#40funtype k : * . funtype v : * . k -> v -> big_map (k ,
    v) -> big_map (k , v) = Λ k ->
    Λ v ->
    lambda (kk)v -> big_map (k , v) -> big_map (k ,
    v) return lambda (vv)big_map (k , v) -> big_map (k ,
    v) return lambda (mbig_map (k , v))big_map (k , v) return MAP_ADD(k , v , m)[@inline][@hidden] in
    let remove#41funtype k : * . funtype v : * . k -> big_map (k ,
    v) -> big_map (k , v) = Λ k ->
    Λ v ->
    lambda (kk)big_map (k , v) -> big_map (k , v) return lambda (mbig_map (k ,
    v))big_map (k , v) return MAP_REMOVE(k , m)[@inline][@hidden] in
    let update#42funtype k : * . funtype v : * . k -> option (v) -> big_map (k ,
    v) -> big_map (k , v) = Λ k ->
    Λ v ->
    lambda (kk)option (v) -> big_map (k , v) -> big_map (k ,
    v) return lambda (voption (v))big_map (k , v) -> big_map (k ,
    v) return lambda (mbig_map (k , v))big_map (k ,
    v) return MAP_UPDATE(k , v , m)[@inline][@hidden] in
    let get_and_update#43funtype k : * . funtype v : * . k -> option (v) -> big_map (k ,
    v) -> ( option (v) * big_map (k , v) ) = Λ k ->
    Λ v ->
    lambda (kk)option (v) -> big_map (k ,
    v) -> ( option (v) * big_map (k , v) ) return lambda (voption (v))big_map (k ,
    v) -> ( option (v) * big_map (k , v) ) return lambda (mbig_map (k ,
    v))( option (v) * big_map (k , v) ) return BIG_MAP_GET_AND_UPDATE(k , v , m)[@inline][@hidden] in
    let find_opt#44funtype k : * . funtype v : * . k -> big_map (k ,
    v) -> option (v) = Λ k ->
    Λ v ->
    lambda (kk)big_map (k , v) -> option (v) return lambda (mbig_map (k ,
    v))option (v) return MAP_FIND_OPT(k , m)[@inline][@hidden] in
    let find#45funtype k : * . funtype v : * . k -> big_map (k , v) -> v =
      Λ k ->
    Λ v ->
    lambda (kk)big_map (k , v) -> v return lambda (mbig_map (k ,
    v))v return MAP_FIND(k , m)[@inline][@hidden] in
    let empty#46funtype k : * . funtype v : * . map (k , v) = Λ k ->
    Λ v ->  MAP_EMPTY()[@inline][@hidden] in
    let size#47funtype k : * . funtype v : * . map (k , v) -> nat = Λ k ->
    Λ v ->  lambda (mmap (k , v))nat return ([%Michelson {| { SIZE } |}])@(m)[@inline][@hidden] in
    let mem#48funtype k : * . funtype v : * . k -> map (k , v) -> bool = Λ k ->
    Λ v ->
    lambda (kk)map (k , v) -> bool return lambda (mmap (k ,
    v))bool return ([%Michelson {| { UNPAIR ; MEM } |}])@(( k , m ))[@inline][@hidden] in
    let add#49funtype k : * . funtype v : * . k -> v -> map (k , v) -> map (k ,
    v) = Λ k ->
    Λ v ->
    lambda (kk)v -> map (k , v) -> map (k , v) return lambda (vv)map (k ,
    v) -> map (k , v) return lambda (mmap (k , v))map (k ,
    v) return MAP_ADD(k , v , m)[@inline][@hidden] in
    let remove#50funtype k : * . funtype v : * . k -> map (k , v) -> map (k ,
    v) = Λ k ->
    Λ v ->
    lambda (kk)map (k , v) -> map (k , v) return lambda (mmap (k , v))map (k ,
    v) return MAP_REMOVE(k , m)[@inline][@hidden] in
    let update#51funtype k : * . funtype v : * . k -> option (v) -> map (k ,
    v) -> map (k , v) = Λ k ->
    Λ v ->
    lambda (kk)option (v) -> map (k , v) -> map (k ,
    v) return lambda (voption (v))map (k , v) -> map (k ,
    v) return lambda (mmap (k , v))map (k , v) return MAP_UPDATE(k , v , m)[@inline][@hidden] in
    let get_and_update#52funtype k : * . funtype v : * . k -> option (v) -> map (k ,
    v) -> ( option (v) * map (k , v) ) = Λ k ->
    Λ v ->
    lambda (kk)option (v) -> map (k ,
    v) -> ( option (v) * map (k , v) ) return lambda (voption (v))map (k ,
    v) -> ( option (v) * map (k , v) ) return lambda (mmap (k ,
    v))( option (v) * map (k , v) ) return MAP_GET_AND_UPDATE(k , v , m)[@inline][@hidden] in
    let find#53funtype k : * . funtype v : * . k -> map (k , v) -> v = Λ k ->
    Λ v ->
    lambda (kk)map (k , v) -> v return lambda (mmap (k ,
    v))v return MAP_FIND(k , m)[@inline][@hidden] in
    let find_opt#54funtype k : * . funtype v : * . k -> map (k ,
    v) -> option (v) = Λ k ->
    Λ v ->
    lambda (kk)map (k , v) -> option (v) return lambda (mmap (k ,
    v))option (v) return MAP_FIND_OPT(k , m)[@inline][@hidden] in
    let iter#55funtype k : * . funtype v : * . ( k * v ) -> unit -> map (k ,
    v) -> unit = Λ k ->
    Λ v ->
    lambda (f( k * v ) -> unit)map (k , v) -> unit return lambda (mmap (k ,
    v))unit return MAP_ITER(f , m)[@inline][@hidden] in
    let map#56funtype k : * . funtype v : * . funtype w : * . ( k * v ) -> w -> map (k ,
    v) -> map (k , w) = Λ k ->
    Λ v ->
    Λ w ->
    lambda (f( k * v ) -> w)map (k , v) -> map (k , w) return lambda (mmap (k ,
    v))map (k , w) return MAP_MAP(f , m)[@inline][@hidden] in
    let fold#57funtype k : * . funtype v : * . funtype c : * . ( c * ( k * v ) ) -> c -> map (k ,
    v) -> c -> c = Λ k ->
    Λ v ->
    Λ c ->
    lambda (f( c * ( k * v ) ) -> c)map (k ,
    v) -> c -> c return lambda (mmap (k ,
    v))c -> c return lambda (ic)c return MAP_FOLD(f , m , i)[@inline][@hidden] in
    let empty#58funtype a : * . set (a) = Λ a ->
    SET_EMPTY()[@inline][@hidden] in
    let size#59funtype a : * . set (a) -> nat = Λ a ->
    lambda (sset (a))nat return ([%Michelson {| { SIZE } |}])@(s)[@inline][@hidden] in
    let cardinal#60funtype a : * . set (a) -> nat = Λ a ->
    lambda (sset (a))nat return ([%Michelson {| { SIZE } |}])@(s)[@inline][@hidden] in
    let mem#61funtype a : * . a -> set (a) -> bool = Λ a ->
    lambda (xa)set (a) -> bool return lambda (sset (a))bool return SET_MEM(x , s)[@inline][@hidden] in
    let add#62funtype a : * . a -> set (a) -> set (a) = Λ a ->
    lambda (xa)set (a) -> set (a) return lambda (sset (a))set (a) return
    SET_ADD(x , s)[@inline][@hidden] in
    let remove#63funtype a : * . a -> set (a) -> set (a) = Λ a ->
    lambda (xa)set (a) -> set (a) return lambda (sset (a))set (a) return
    SET_REMOVE(x , s)[@inline][@hidden] in
    let update#64funtype a : * . a -> bool -> set (a) -> set (a) = Λ a ->
    lambda (xa)bool -> set (a) -> set (a) return lambda (bbool)set (a) -> set (a) return lambda (sset (a))set (a) return
    SET_UPDATE(x , b , s)[@inline][@hidden] in
    let iter#65funtype a : * . a -> unit -> set (a) -> unit = Λ a ->
    lambda (fa -> unit)set (a) -> unit return lambda (sset (a))unit return
    SET_ITER(f , s)[@inline][@hidden] in
    let fold#66funtype a : * . funtype b : * . ( b * a ) -> b -> set (a) -> b -> b =
      Λ a ->
    Λ b ->
    lambda (f( b * a ) -> b)set (a) -> b -> b return lambda (sset (a))b -> b return lambda (ib)b return
    SET_FOLD(f , s , i)[@inline][@hidden] in
    let fold_desc#67funtype a : * . funtype b : * . ( a * b ) -> b -> set (a) -> b -> b =
      Λ a ->
    Λ b ->
    lambda (f( a * b ) -> b)set (a) -> b -> b return lambda (sset (a))b -> b return lambda (ib)b return
    SET_FOLD_DESC(f , s , i)[@inline][@hidden] in
    let length#68funtype a : * . list (a) -> nat = Λ a ->
    lambda (xslist (a))nat return ([%Michelson {| { SIZE } |}])@(xs)[@inline][@hidden] in
    let size#69funtype a : * . list (a) -> nat = Λ a ->
    lambda (xslist (a))nat return ([%Michelson {| { SIZE } |}])@(xs)[@inline][@hidden] in
    let head_opt#70funtype a : * . list (a) -> option (a) = Λ a ->
    lambda (xslist (a))option (a) return  match xs with
                                           | Cons ctor_proj#2 ->
                                              match ctor_proj#2 with
                                               | ( x , _#2 ) ->
                                               Some(x)
                                           | Nil unit_proj#4 ->
                                             None(unit)[@inline][@hidden] in
    let tail_opt#71funtype a : * . list (a) -> option (list (a)) = Λ a ->
    lambda (xslist (a))option (list (a)) return  match xs with
                                                  | Cons ctor_proj#5 ->
                                                     match ctor_proj#5 with
                                                      | ( _#3 , xs ) ->
                                                      Some(xs)
                                                  | Nil unit_proj#7 ->
                                                    None(unit)[@inline][@hidden] in
    let map#72funtype a : * . funtype b : * . a -> b -> list (a) -> list (b) =
      Λ a ->
    Λ b ->
    lambda (fa -> b)list (a) -> list (b) return lambda (xslist (a))list (b) return
    LIST_MAP(f , xs)[@inline][@hidden] in
    let iter#73funtype a : * . a -> unit -> list (a) -> unit = Λ a ->
    lambda (fa -> unit)list (a) -> unit return lambda (xslist (a))unit return
    LIST_ITER(f , xs)[@inline][@hidden] in
    let fold#74funtype a : * . funtype b : * . ( b * a ) -> b -> list (a) -> b -> b =
      Λ a ->
    Λ b ->
    lambda (f( b * a ) -> b)list (a) -> b -> b return lambda (xslist (a))b -> b return lambda (ib)b return
    LIST_FOLD(f , xs , i)[@inline][@hidden] in
    let fold_left#75funtype a : * . funtype b : * . ( b * a ) -> b -> b -> list (a) -> b =
      Λ a ->
    Λ b ->
    lambda (f( b * a ) -> b)b -> list (a) -> b return lambda (ib)list (a) -> b return lambda (xslist (a))b return
    LIST_FOLD_LEFT(f , i , xs)[@inline][@hidden] in
    let fold_right#76funtype a : * . funtype b : * . ( a * b ) -> b -> list (a) -> b -> b =
      Λ a ->
    Λ b ->
    lambda (f( a * b ) -> b)list (a) -> b -> b return lambda (xslist (a))b -> b return lambda (ib)b return
    LIST_FOLD_RIGHT(f , xs , i)[@inline][@hidden] in
    let cons#77funtype a : * . a -> list (a) -> list (a) = Λ a ->
    lambda (xa)list (a) -> list (a) return lambda (xslist (a))list (a) return
    CONS(x , xs)[@inline][@hidden] in
    let length#78string -> nat =
      lambda (bstring)nat return ([%Michelson {| { SIZE } |}])@(b)[@inline][@hidden] in
    let concat#79string -> string -> string =
      lambda (b1string)string -> string return lambda (b2string)string return
    ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b1 , b2 ))[@inline][@hidden] in
    let sub#80nat -> nat -> string -> string =
      lambda (snat)nat -> string -> string return lambda (lnat)string -> string return lambda (bstring)string return
    ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(
    ( s ,
      l ,
      b ))[@inline][@hidden] in
    let unopt#81funtype a : * . option (a) -> a = Λ a ->
    lambda (voption (a))a return UNOPT(v)[@inline][@hidden] in
    let unopt_with_error#82funtype a : * . option (a) -> string -> a = Λ a ->
    lambda (voption (a))string -> a return lambda (sstring)a return UNOPT_WITH_ERROR
                                                                    (v ,
                                                                     s)[@inline][@hidden] in
    let pack#83funtype a : * . a -> bytes = Λ a ->
    lambda (va)bytes return ([%Michelson {| { PACK } |}])@(v)[@inline][@hidden] in
    let unpack#84funtype a : * . bytes -> option (a) = Λ a ->
    lambda (bbytes)option (a) return BYTES_UNPACK(b)[@inline][@hidden] in
    let length#85bytes -> nat =
      lambda (bbytes)nat return ([%Michelson {| { SIZE } |}])@(b)[@inline][@hidden] in
    let concat#86bytes -> bytes -> bytes =
      lambda (b1bytes)bytes -> bytes return lambda (b2bytes)bytes return
    ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b1 , b2 ))[@inline][@hidden] in
    let sub#87nat -> nat -> bytes -> bytes =
      lambda (snat)nat -> bytes -> bytes return lambda (lnat)bytes -> bytes return lambda (bbytes)bytes return
    ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(
    ( s ,
      l ,
      b ))[@inline][@hidden] in
    let blake2b#88bytes -> bytes =
      lambda (bbytes)bytes return ([%Michelson {| { BLAKE2B } |}])@(b)[@inline][@hidden] in
    let sha256#89bytes -> bytes =
      lambda (bbytes)bytes return ([%Michelson {| { SHA256 } |}])@(b)[@inline][@hidden] in
    let sha512#90bytes -> bytes =
      lambda (bbytes)bytes return ([%Michelson {| { SHA512 } |}])@(b)[@inline][@hidden] in
    let sha3#91bytes -> bytes =
      lambda (bbytes)bytes return ([%Michelson {| { SHA3 } |}])@(b)[@inline][@hidden] in
    let keccak#92bytes -> bytes =
      lambda (bbytes)bytes return ([%Michelson {| { KECCAK } |}])@(b)[@inline][@hidden] in
    let hash_key#93key -> key_hash =
      lambda (kkey)key_hash return ([%Michelson {| { HASH_KEY } |}])@(k)[@inline][@hidden] in
    let check#94key -> signature -> bytes -> bool =
      lambda (kkey)signature -> bytes -> bool return lambda (ssignature)bytes -> bool return lambda (bbytes)bool return
    ([%Michelson {| { UNPAIR ; UNPAIR ; CHECK_SIGNATURE } |}])@(( k , s , b ))[@inline][@hidden] in
    let assertbool -> unit =
      lambda (bbool)unit return ([%Michelson {| { IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } } |}])@(b)[@inline][@private][@hidden] in
    let assert_somefuntype a : * . option (a) -> unit = Λ a ->
    lambda (voption (a))unit return ([%Michelson {| { IF_NONE { PUSH string "failed assert some" ; FAILWITH } { DROP ; UNIT } } |}])@(v)[@inline][@private][@hidden] in
    let assert_nonefuntype a : * . option (a) -> unit = Λ a ->
    lambda (voption (a))unit return ([%Michelson {| { IF_NONE { UNIT } { PUSH string "failed assert none" ; FAILWITH } } |}])@(v)[@inline][@private][@hidden] in
    let absint -> nat =
      lambda (iint)nat return ([%Michelson {| { ABS } |}])@(i)[@inline][@private][@hidden] in
    let is_natint -> option (nat) =
      lambda (iint)option (nat) return ([%Michelson {| { ISNAT } |}])@(i)[@inline][@private][@hidden] in
    let truebool = TRUE()[@inline][@private][@hidden] in
    let falsebool = FALSE()[@inline][@private][@hidden] in
    let unitunit = UNIT()[@inline][@private][@hidden] in
    let failwithfuntype a : * . funtype b : * . a -> b = Λ a ->
    Λ b ->  [%Michelson {|{ FAILWITH }|}][@inline][@private][@hidden] in
    let intfuntype a : * . a -> external_int (a) = Λ a ->
    lambda (va)external_int (a) return ([%Michelson {| { INT } |}])@(v)[@inline][@private][@hidden] in
    let assert_with_errorbool -> string -> unit =
      lambda (bbool)string -> unit return lambda (sstring)unit return ([%Michelson {| { UNPAIR ; IF { DROP ; UNIT } { FAILWITH } } |}])@(
                                                                      ( b ,
                                                                        s ))[@inline][@private][@hidden] in
    let assert_some_with_errorfuntype a : * . option (a) -> string -> unit =
      Λ a ->
    lambda (voption (a))string -> unit return lambda (sstring)unit return
    ([%Michelson {| { UNPAIR ; IF_NONE { FAILWITH } { DROP 2 ; UNIT } } |}])@(
    ( v ,
      s ))[@inline][@private][@hidden] in
    let assert_none_with_errorfuntype a : * . option (a) -> string -> unit =
      Λ a ->
    lambda (voption (a))string -> unit return lambda (sstring)unit return
    ([%Michelson {| { UNPAIR ; IF_NONE { DROP ; UNIT } { DROP ; FAILWITH } } |}])@(
    ( v ,
      s ))[@inline][@private][@hidden] in
    let edivfuntype a : * . funtype b : * . a -> b -> external_ediv (a , b) =
      Λ a ->
    Λ b ->
    lambda (la)b -> external_ediv (a , b) return lambda (rb)external_ediv (a ,
    b) return ([%Michelson {| { UNPAIR ; EDIV } |}])@(( l , r ))[@inline][@private][@hidden] in
    let stub#95funtype a : * . funtype b : * . a -> b = Λ a ->
    Λ b ->  lambda (xa)b return ([%Michelson {|{ FAILWITH }|}])@(x)[@inline][@private][@hidden] in
    let run#96funtype a : * . funtype b : * . a -> b -> a -> unit = Λ a ->
    Λ b ->
    lambda (_fa -> b)a -> unit return lambda (_va)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
    let eval#97funtype a : * . a -> unit = Λ a ->
    lambda (_xa)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
    let compile_value#98funtype a : * . a -> unit = Λ a ->
    lambda (_xa)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
    let get_total_voting_power#99unit -> nat =
      lambda (_uunit)nat return (stub#95@{unit}@{nat})@(unit)[@inline][@hidden] in
    let failwith#100funtype a : * . funtype b : * . a -> b = Λ a ->
    Λ b ->  lambda (_va)b return (stub#95@{unit}@{b})@(unit)[@inline][@hidden] in
    let to_contract#101funtype p : * . funtype s : * . unit -> contract (p) =
      Λ p ->
    Λ s ->
    lambda (_tunit)contract (p) return (stub#95@{unit}@{contract (p)})@(unit)[@inline][@hidden] in
    let set_source#102address -> unit =
      lambda (_aaddress)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
    let get_storage_of_address#103address -> unit =
      lambda (_aaddress)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
    let get_balance#104address -> tez =
      lambda (_aaddress)tez return (stub#95@{unit}@{tez})@(unit)[@inline][@hidden] in
    let print#105string -> unit =
      lambda (_vstring)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
    let eprint#106string -> unit =
      lambda (_vstring)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
    let get_voting_power#107key_hash -> nat =
      lambda (_khkey_hash)nat return (stub#95@{unit}@{nat})@(unit)[@inline][@hidden] in
    let nth_bootstrap_contract#108nat -> address =
      lambda (_inat)address return (stub#95@{unit}@{address})@(unit)[@inline][@hidden] in
    let nth_bootstrap_account#109int -> address =
      lambda (_iint)address return (stub#95@{unit}@{address})@(unit)[@inline][@hidden] in
    let get_bootstrap_account#110nat -> ( address * key * string ) =
      lambda (_nnat)( address * key * string ) return (stub#95@{unit}@{( address * key * string )})@(unit)[@inline][@hidden] in
    let nth_bootstrap_typed_address#111funtype a : * . funtype b : * . nat -> unit =
      Λ a ->
    Λ b ->  lambda (_nnat)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
    let last_originations#112unit -> map (address , list (address)) =
      lambda (_uunit)map (address ,
    list (address)) return (stub#95@{unit}@{map (address ,
                           list (address))})@(unit)[@inline][@hidden] in
    let random#113funtype a : * . unit -> a = Λ a ->
    lambda (_uunit)a return (stub#95@{unit}@{a})@(unit)[@inline][@hidden] in
    let new_account#114unit -> ( string * key ) =
      lambda (_uunit)( string * key ) return (stub#95@{unit}@{( string * key )})@(unit)[@inline][@hidden] in
    let decompile#115funtype a : * . unit -> a = Λ a ->
    lambda (_munit)a return (stub#95@{unit}@{a})@(unit)[@inline][@hidden] in
    let bake_until_n_cycle_end#116nat -> unit =
      lambda (_nnat)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
    let cast_address#117funtype a : * . funtype b : * . address -> unit =
      Λ a ->
    Λ b ->  lambda (_aaddress)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
    let register_delegate#118key_hash -> unit =
      lambda (_khkey_hash)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
    let register_constant#119unit -> string =
      lambda (_munit)string return (stub#95@{unit}@{string})@(unit)[@inline][@hidden] in
    let to_typed_address#120funtype a : * . funtype b : * . contract (a) -> unit =
      Λ a ->
    Λ b ->  lambda (_ccontract (a))unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
    let constant_to_michelson_program#121string -> unit =
      lambda (_sstring)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
    let restore_context#122unit -> unit =
      lambda (_uunit)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
    let save_context#123unit -> unit =
      lambda (_uunit)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
    let drop_context#124unit -> unit =
      lambda (_uunit)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
    let to_string#125funtype a : * . a -> string = Λ a ->
    lambda (_va)string return (stub#95@{unit}@{string})@(unit)[@inline][@hidden] in
    let get_storage#126funtype p : * . funtype s : * . unit -> s = Λ p ->
    Λ s ->  lambda (_tunit)s return (stub#95@{unit}@{s})@(unit)[@inline][@hidden] in
    let set_baker_policy#127unit -> unit =
      lambda (_bpunit)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
    let set_baker#128address -> unit =
      lambda (_aaddress)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
    let size#129unit -> int =
      lambda (_cunit)int return (stub#95@{unit}@{int})@(unit)[@inline][@hidden] in
    let compile_contract#130funtype p : * . funtype s : * . ( p * s ) ->
    ( list (operation) * s ) -> unit = Λ p ->
    Λ s ->
    lambda (_f( p * s ) -> ( list (operation) * s ))unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
    let read_contract_from_file#131string -> unit =
      lambda (_fnstring)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
    let chr#132nat -> option (string) =
      lambda (_nnat)option (string) return (stub#95@{unit}@{option (string)})@(unit)[@inline][@hidden] in
    let nl#133string = "NEWLINE"[@inline][@hidden] in
    let println#134string -> unit =
      lambda (_vstring)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
    let transfer#135address -> unit -> tez -> unit =
      lambda (_aaddress)unit -> tez -> unit return lambda (_sunit)tez -> unit return lambda (_ttez)unit return
    (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
    let transfer_exn#136address -> unit -> tez -> nat =
      lambda (_aaddress)unit -> tez -> nat return lambda (_sunit)tez -> nat return lambda (_ttez)nat return
    (stub#95@{unit}@{nat})@(unit)[@inline][@hidden] in
    let log#137funtype a : * . a -> unit = Λ a ->
    lambda (_va)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
    let reset_state#138nat -> list (tez) -> unit =
      lambda (_nnat)list (tez) -> unit return lambda (_llist (tez))unit return
    (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
    let reset_state_at#139timestamp -> nat -> list (tez) -> unit =
      lambda (_ttimestamp)nat -> list (tez) -> unit return lambda (_nnat)list (tez) -> unit return lambda (_llist (tez))unit return
    (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
    let bootstrap_contract#140funtype p : * . funtype s : * . ( p * s ) ->
    ( list (operation) * s ) -> s -> tez -> unit = Λ p ->
    Λ s ->
    lambda (_f( p * s ) -> ( list (operation) * s ))s -> tez -> unit return lambda (_ss)tez -> unit return lambda (_ttez)unit return
    (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
    let mutate_value#141funtype a : * . nat -> a -> option (( a * unit )) =
      Λ a ->
    lambda (_nnat)a -> option (( a * unit )) return lambda (_va)option (( a * unit )) return
    (stub#95@{unit}@{option (( a * unit ))})@(unit)[@inline][@hidden] in
    let save_mutation#142string -> unit -> option (string) =
      lambda (_sstring)unit -> option (string) return lambda (_munit)option (string) return
    (stub#95@{unit}@{option (string)})@(unit)[@inline][@hidden] in
    let mutation_test#143funtype a : * . funtype b : * . a -> a -> b -> option (
    ( b * unit )) = Λ a ->
    Λ b ->
    lambda (_va)a -> b -> option (( b * unit )) return lambda (_fa -> b)option (
    ( b * unit )) return (stub#95@{unit}@{option (( b * unit ))})@(unit)[@inline][@hidden] in
    let mutation_test_all#144funtype a : * . funtype b : * . a -> a -> b -> list (
    ( b * unit )) = Λ a ->
    Λ b ->
    lambda (_va)a -> b -> list (( b * unit )) return lambda (_fa -> b)list (
    ( b * unit )) return (stub#95@{unit}@{list (( b * unit ))})@(unit)[@inline][@hidden] in
    let sign#145string -> bytes -> signature =
      lambda (_skstring)bytes -> signature return lambda (_dbytes)signature return
    (stub#95@{unit}@{signature})@(unit)[@inline][@hidden] in
    let add_account#146string -> key -> unit =
      lambda (_sstring)key -> unit return lambda (_kkey)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
    let baker_account#147( string * key ) -> option (tez) -> unit =
      lambda (_p( string * key ))option (tez) -> unit return lambda (_ooption (tez))unit return
    (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
    let set_big_map#148funtype a : * . funtype b : * . int -> big_map (a ,
    b) -> unit = Λ a ->
    Λ b ->
    lambda (_iint)big_map (a , b) -> unit return lambda (_mbig_map (a ,
    b))unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
    let create_chest#149bytes -> nat -> ( chest * chest_key ) =
      lambda (_bbytes)nat -> ( chest * chest_key ) return lambda (_nnat)( chest * chest_key ) return
    (stub#95@{unit}@{( chest * chest_key )})@(unit)[@inline][@hidden] in
    let create_chest_key#150chest -> nat -> chest_key =
      lambda (_cchest)nat -> chest_key return lambda (_nnat)chest_key return
    (stub#95@{unit}@{chest_key})@(unit)[@inline][@hidden] in
    let transfer_to_contract#151funtype p : * . contract (p) -> p -> tez -> unit =
      Λ p ->
    lambda (_ccontract (p))p -> tez -> unit return lambda (_sp)tez -> unit return lambda (_ttez)unit return
    (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
    let transfer_to_contract_exn#152funtype p : * . contract (p) -> p -> tez -> nat =
      Λ p ->
    lambda (_ccontract (p))p -> tez -> nat return lambda (_sp)tez -> nat return lambda (_ttez)nat return
    (stub#95@{unit}@{nat})@(unit)[@inline][@hidden] in
    let michelson_equal#153unit -> unit -> bool =
      lambda (_m1unit)unit -> bool return lambda (_m2unit)bool return (stub#95@{unit}@{bool})@(unit)[@inline][@hidden] in
    let to_entrypoint#154funtype a : * . funtype b : * . funtype c : * . string -> unit -> contract (c) =
      Λ a ->
    Λ b ->
    Λ c ->
    lambda (_sstring)unit -> contract (c) return lambda (_tunit)contract (c) return
    (stub#95@{unit}@{contract (c)})@(unit)[@inline][@hidden] in
    let originate_contract#155unit -> unit -> tez -> address =
      lambda (_cunit)unit -> tez -> address return lambda (_sunit)tez -> address return lambda (_ttez)address return
    (stub#95@{unit}@{address})@(unit)[@inline][@hidden] in
    let originate#156funtype p : * . funtype s : * . ( p * s ) -> ( list (operation) * s ) -> s -> tez ->
    ( unit * unit * int ) = Λ p ->
    Λ s ->
    lambda (_f( p * s ) -> ( list (operation) * s ))s -> tez -> ( unit * unit * int ) return lambda (_ss)tez ->
    ( unit * unit * int ) return lambda (_ttez)( unit * unit * int ) return
    (stub#95@{unit}@{( unit * unit * int )})@(unit)[@inline][@hidden] in
    let compile_contract_from_file#157string -> string -> list (string) -> unit =
      lambda (_fnstring)string -> list (string) -> unit return lambda (_estring)list (string) -> unit return lambda (_vlist (string))unit return
    (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
    let originate_from_file#158string -> string -> list (string) -> unit -> tez ->
    ( address * unit * int ) =
      lambda (_fnstring)string -> list (string) -> unit -> tez -> ( address * unit * int ) return lambda (_estring)list (string) -> unit -> tez ->
    ( address * unit * int ) return lambda (_vlist (string))unit -> tez ->
    ( address * unit * int ) return lambda (_sunit)tez -> ( address * unit * int ) return lambda (_ttez)
    ( address * unit * int ) return (stub#95@{unit}@{( address * unit * int )})@(unit)[@inline][@hidden] in
    let a#159int = 1 in
    let as#160int = 42 in
    let x#161int = a#159 in let b#162int = as#160 in let xint = as#160 in unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias3.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias4.mligo" ] ;
  [%expect {xxx|
  let get_balance#8unit -> tez =
    lambda (_uunit)tez return ([%Michelson {| { DROP ; BALANCE } |}])@(unit)[@inline][@hidden] in
  let get_amount#9unit -> tez =
    lambda (_uunit)tez return ([%Michelson {| { DROP ; AMOUNT } |}])@(unit)[@inline][@hidden] in
  let get_now#10unit -> timestamp =
    lambda (_uunit)timestamp return ([%Michelson {| { DROP ; NOW } |}])@(unit)[@inline][@hidden] in
  let get_sender#11unit -> address =
    lambda (_uunit)address return ([%Michelson {| { DROP ; SENDER } |}])@(unit)[@inline][@hidden] in
  let get_source#12unit -> address =
    lambda (_uunit)address return ([%Michelson {| { DROP ; SOURCE } |}])@(unit)[@inline][@hidden] in
  let get_level#13unit -> nat =
    lambda (_uunit)nat return ([%Michelson {| { DROP ; LEVEL } |}])@(unit)[@inline][@hidden] in
  let get_self_address#14unit -> address =
    lambda (_uunit)address return SELF_ADDRESS()[@inline][@hidden] in
  let get_chain_id#15unit -> chain_id =
    lambda (_uunit)chain_id return ([%Michelson {| { DROP ; CHAIN_ID } |}])@(unit)[@inline][@hidden] in
  let get_total_voting_power#16unit -> nat =
    lambda (_uunit)nat return ([%Michelson {| { DROP ; TOTAL_VOTING_POWER } |}])@(unit)[@inline][@hidden] in
  let get_min_block_time#17unit -> nat =
    lambda (_uunit)nat return ([%Michelson {| { DROP; MIN_BLOCK_TIME } |}])@(unit)[@inline][@hidden] in
  let voting_power#18key_hash -> nat =
    lambda (khkey_hash)nat return ([%Michelson {| { VOTING_POWER } |}])@(kh)[@inline][@hidden] in
  let address#19funtype a : * . contract (a) -> address = Λ a ->
  lambda (ccontract (a))address return ADDRESS(c)[@inline][@hidden] in
  let implicit_account#20key_hash -> contract (unit) =
    lambda (khkey_hash)contract (unit) return IMPLICIT_ACCOUNT(kh)[@inline][@hidden] in
  let join_tickets#21funtype a : * . ( ticket (a) * ticket (a) ) -> option (ticket (a)) =
    Λ a ->
  lambda (t( ticket (a) * ticket (a) ))option (ticket (a)) return ([%Michelson {| { JOIN_TICKETS } |}])@(t)[@inline][@hidden] in
  let read_ticket#22funtype a : * . ticket (a) -> ( ( address * ( a * nat ) ) * ticket (a) ) =
    Λ a ->
  lambda (tticket (a))( ( address * ( a * nat ) ) * ticket (a) ) return
  ([%Michelson {| { READ_TICKET ; PAIR } |}])@(t)[@inline][@hidden] in
  let never#23funtype a : * . never -> a = Λ a ->
  lambda (nnever)a return ([%Michelson {| { NEVER } |}])@(n)[@inline][@hidden] in
  let pairing_check#24list (( bls12_381_g1 * bls12_381_g2 )) -> bool =
    lambda (llist (( bls12_381_g1 * bls12_381_g2 )))bool return ([%Michelson {| { PAIRING_CHECK } |}])@(l)[@inline][@hidden] in
  let constant#25funtype a : * . string -> a = Λ a ->
  lambda (sstring)a return GLOBAL_CONSTANT(s)[@inline][@hidden] in
  let set_delegate#26option (key_hash) -> operation =
    lambda (ooption (key_hash))operation return SET_DELEGATE(o)[@inline][@hidden] in
  let get_contract#27funtype a : * . address -> contract (a) = Λ a ->
  lambda (aaddress)contract (a) return CONTRACT(a)[@inline][@hidden] in
  let get_contract_opt#28funtype a : * . address -> option (contract (a)) =
    Λ a ->
  lambda (aaddress)option (contract (a)) return CONTRACT_OPT(a)[@inline][@hidden] in
  let get_contract_with_error#29funtype a : * . address -> string -> contract (a) =
    Λ a ->
  lambda (aaddress)string -> contract (a) return lambda (sstring)contract (a) return
  CONTRACT_WITH_ERROR(a , s)[@inline][@hidden] in
  let create_ticket#30funtype a : * . a -> nat -> ticket (a) = Λ a ->
  lambda (va)nat -> ticket (a) return lambda (nnat)ticket (a) return ([%Michelson {| { UNPAIR ; TICKET } |}])@(
                                                                     ( v ,
                                                                      n ))[@inline][@hidden] in
  let transaction#31funtype a : * . a -> tez -> contract (a) -> operation =
    Λ a ->
  lambda (aa)tez -> contract (a) -> operation return lambda (mutez)contract (a) -> operation return lambda (ccontract (a))operation return
  CALL(a , mu , c)[@inline][@hidden] in
  let open_chest#32chest_key -> chest -> nat -> sum[Fail_decrypt -> unit , Fail_timelock -> unit , Ok_opening -> bytes] =
    lambda (ckchest_key)chest -> nat -> sum[Fail_decrypt -> unit , Fail_timelock -> unit , Ok_opening -> bytes] return lambda (cchest)nat ->
  sum[Fail_decrypt -> unit , Fail_timelock -> unit , Ok_opening -> bytes] return lambda (nnat)
  sum[Fail_decrypt -> unit , Fail_timelock -> unit , Ok_opening -> bytes] return
  OPEN_CHEST(ck , c , n)[@inline][@hidden] in
  let call_view#33funtype a : * . funtype b : * . string -> a -> address -> option (b) =
    Λ a ->
  Λ b ->
  lambda (sstring)a -> address -> option (b) return lambda (xa)address -> option (b) return lambda (aaddress)option (b) return
  VIEW(s , x , a)[@inline][@hidden] in
  let split_ticket#34funtype a : * . ticket (a) -> ( nat * nat ) -> option (
  ( ticket (a) * ticket (a) )) = Λ a ->
  lambda (tticket (a))( nat * nat ) -> option (( ticket (a) * ticket (a) )) return lambda (p
  ( nat * nat ))option (( ticket (a) * ticket (a) )) return ([%Michelson {| { UNPAIR ; SPLIT_TICKET } |}])@(
                                                            ( t ,
                                                              p ))[@inline][@hidden] in
  let xor#35nat -> nat -> nat =
    lambda (lnat)nat -> nat return lambda (rnat)nat return XOR(l , r)[@inline][@hidden] in
  let shift_left#36nat -> nat -> nat =
    lambda (lnat)nat -> nat return lambda (rnat)nat return LSL(l , r)[@inline][@hidden] in
  let shift_right#37nat -> nat -> nat =
    lambda (lnat)nat -> nat return lambda (rnat)nat return LSR(l , r)[@inline][@hidden] in
  let empty#38funtype k : * . funtype v : * . big_map (k , v) = Λ k ->
  Λ v ->  BIG_MAP_EMPTY()[@inline][@hidden] in
  let mem#39funtype k : * . funtype v : * . k -> big_map (k , v) -> bool =
    Λ k ->
  Λ v ->
  lambda (kk)big_map (k , v) -> bool return lambda (mbig_map (k ,
  v))bool return ([%Michelson {| { UNPAIR ; MEM } |}])@(( k , m ))[@inline][@hidden] in
  let add#40funtype k : * . funtype v : * . k -> v -> big_map (k ,
  v) -> big_map (k , v) = Λ k ->
  Λ v ->
  lambda (kk)v -> big_map (k , v) -> big_map (k ,
  v) return lambda (vv)big_map (k , v) -> big_map (k ,
  v) return lambda (mbig_map (k , v))big_map (k , v) return MAP_ADD(k , v , m)[@inline][@hidden] in
  let remove#41funtype k : * . funtype v : * . k -> big_map (k ,
  v) -> big_map (k , v) = Λ k ->
  Λ v ->
  lambda (kk)big_map (k , v) -> big_map (k , v) return lambda (mbig_map (k ,
  v))big_map (k , v) return MAP_REMOVE(k , m)[@inline][@hidden] in
  let update#42funtype k : * . funtype v : * . k -> option (v) -> big_map (k ,
  v) -> big_map (k , v) = Λ k ->
  Λ v ->
  lambda (kk)option (v) -> big_map (k , v) -> big_map (k ,
  v) return lambda (voption (v))big_map (k , v) -> big_map (k ,
  v) return lambda (mbig_map (k , v))big_map (k ,
  v) return MAP_UPDATE(k , v , m)[@inline][@hidden] in
  let get_and_update#43funtype k : * . funtype v : * . k -> option (v) -> big_map (k ,
  v) -> ( option (v) * big_map (k , v) ) = Λ k ->
  Λ v ->
  lambda (kk)option (v) -> big_map (k ,
  v) -> ( option (v) * big_map (k , v) ) return lambda (voption (v))big_map (k ,
  v) -> ( option (v) * big_map (k , v) ) return lambda (mbig_map (k ,
  v))( option (v) * big_map (k , v) ) return BIG_MAP_GET_AND_UPDATE(k , v , m)[@inline][@hidden] in
  let find_opt#44funtype k : * . funtype v : * . k -> big_map (k ,
  v) -> option (v) = Λ k ->
  Λ v ->
  lambda (kk)big_map (k , v) -> option (v) return lambda (mbig_map (k ,
  v))option (v) return MAP_FIND_OPT(k , m)[@inline][@hidden] in
  let find#45funtype k : * . funtype v : * . k -> big_map (k , v) -> v =
    Λ k ->
  Λ v ->
  lambda (kk)big_map (k , v) -> v return lambda (mbig_map (k ,
  v))v return MAP_FIND(k , m)[@inline][@hidden] in
  let empty#46funtype k : * . funtype v : * . map (k , v) = Λ k ->
  Λ v ->  MAP_EMPTY()[@inline][@hidden] in
  let size#47funtype k : * . funtype v : * . map (k , v) -> nat = Λ k ->
  Λ v ->  lambda (mmap (k , v))nat return ([%Michelson {| { SIZE } |}])@(m)[@inline][@hidden] in
  let mem#48funtype k : * . funtype v : * . k -> map (k , v) -> bool = Λ k ->
  Λ v ->
  lambda (kk)map (k , v) -> bool return lambda (mmap (k ,
  v))bool return ([%Michelson {| { UNPAIR ; MEM } |}])@(( k , m ))[@inline][@hidden] in
  let add#49funtype k : * . funtype v : * . k -> v -> map (k , v) -> map (k ,
  v) = Λ k ->
  Λ v ->
  lambda (kk)v -> map (k , v) -> map (k , v) return lambda (vv)map (k ,
  v) -> map (k , v) return lambda (mmap (k , v))map (k ,
  v) return MAP_ADD(k , v , m)[@inline][@hidden] in
  let remove#50funtype k : * . funtype v : * . k -> map (k , v) -> map (k ,
  v) = Λ k ->
  Λ v ->
  lambda (kk)map (k , v) -> map (k , v) return lambda (mmap (k , v))map (k ,
  v) return MAP_REMOVE(k , m)[@inline][@hidden] in
  let update#51funtype k : * . funtype v : * . k -> option (v) -> map (k ,
  v) -> map (k , v) = Λ k ->
  Λ v ->
  lambda (kk)option (v) -> map (k , v) -> map (k ,
  v) return lambda (voption (v))map (k , v) -> map (k ,
  v) return lambda (mmap (k , v))map (k , v) return MAP_UPDATE(k , v , m)[@inline][@hidden] in
  let get_and_update#52funtype k : * . funtype v : * . k -> option (v) -> map (k ,
  v) -> ( option (v) * map (k , v) ) = Λ k ->
  Λ v ->
  lambda (kk)option (v) -> map (k ,
  v) -> ( option (v) * map (k , v) ) return lambda (voption (v))map (k ,
  v) -> ( option (v) * map (k , v) ) return lambda (mmap (k ,
  v))( option (v) * map (k , v) ) return MAP_GET_AND_UPDATE(k , v , m)[@inline][@hidden] in
  let find#53funtype k : * . funtype v : * . k -> map (k , v) -> v = Λ k ->
  Λ v ->
  lambda (kk)map (k , v) -> v return lambda (mmap (k ,
  v))v return MAP_FIND(k , m)[@inline][@hidden] in
  let find_opt#54funtype k : * . funtype v : * . k -> map (k ,
  v) -> option (v) = Λ k ->
  Λ v ->
  lambda (kk)map (k , v) -> option (v) return lambda (mmap (k ,
  v))option (v) return MAP_FIND_OPT(k , m)[@inline][@hidden] in
  let iter#55funtype k : * . funtype v : * . ( k * v ) -> unit -> map (k ,
  v) -> unit = Λ k ->
  Λ v ->
  lambda (f( k * v ) -> unit)map (k , v) -> unit return lambda (mmap (k ,
  v))unit return MAP_ITER(f , m)[@inline][@hidden] in
  let map#56funtype k : * . funtype v : * . funtype w : * . ( k * v ) -> w -> map (k ,
  v) -> map (k , w) = Λ k ->
  Λ v ->
  Λ w ->
  lambda (f( k * v ) -> w)map (k , v) -> map (k , w) return lambda (mmap (k ,
  v))map (k , w) return MAP_MAP(f , m)[@inline][@hidden] in
  let fold#57funtype k : * . funtype v : * . funtype c : * . ( c * ( k * v ) ) -> c -> map (k ,
  v) -> c -> c = Λ k ->
  Λ v ->
  Λ c ->
  lambda (f( c * ( k * v ) ) -> c)map (k ,
  v) -> c -> c return lambda (mmap (k ,
  v))c -> c return lambda (ic)c return MAP_FOLD(f , m , i)[@inline][@hidden] in
  let empty#58funtype a : * . set (a) = Λ a ->
  SET_EMPTY()[@inline][@hidden] in
  let size#59funtype a : * . set (a) -> nat = Λ a ->
  lambda (sset (a))nat return ([%Michelson {| { SIZE } |}])@(s)[@inline][@hidden] in
  let cardinal#60funtype a : * . set (a) -> nat = Λ a ->
  lambda (sset (a))nat return ([%Michelson {| { SIZE } |}])@(s)[@inline][@hidden] in
  let mem#61funtype a : * . a -> set (a) -> bool = Λ a ->
  lambda (xa)set (a) -> bool return lambda (sset (a))bool return SET_MEM(x , s)[@inline][@hidden] in
  let add#62funtype a : * . a -> set (a) -> set (a) = Λ a ->
  lambda (xa)set (a) -> set (a) return lambda (sset (a))set (a) return
  SET_ADD(x , s)[@inline][@hidden] in
  let remove#63funtype a : * . a -> set (a) -> set (a) = Λ a ->
  lambda (xa)set (a) -> set (a) return lambda (sset (a))set (a) return
  SET_REMOVE(x , s)[@inline][@hidden] in
  let update#64funtype a : * . a -> bool -> set (a) -> set (a) = Λ a ->
  lambda (xa)bool -> set (a) -> set (a) return lambda (bbool)set (a) -> set (a) return lambda (sset (a))set (a) return
  SET_UPDATE(x , b , s)[@inline][@hidden] in
  let iter#65funtype a : * . a -> unit -> set (a) -> unit = Λ a ->
  lambda (fa -> unit)set (a) -> unit return lambda (sset (a))unit return
  SET_ITER(f , s)[@inline][@hidden] in
  let fold#66funtype a : * . funtype b : * . ( b * a ) -> b -> set (a) -> b -> b =
    Λ a ->
  Λ b ->
  lambda (f( b * a ) -> b)set (a) -> b -> b return lambda (sset (a))b -> b return lambda (ib)b return
  SET_FOLD(f , s , i)[@inline][@hidden] in
  let fold_desc#67funtype a : * . funtype b : * . ( a * b ) -> b -> set (a) -> b -> b =
    Λ a ->
  Λ b ->
  lambda (f( a * b ) -> b)set (a) -> b -> b return lambda (sset (a))b -> b return lambda (ib)b return
  SET_FOLD_DESC(f , s , i)[@inline][@hidden] in
  let length#68funtype a : * . list (a) -> nat = Λ a ->
  lambda (xslist (a))nat return ([%Michelson {| { SIZE } |}])@(xs)[@inline][@hidden] in
  let size#69funtype a : * . list (a) -> nat = Λ a ->
  lambda (xslist (a))nat return ([%Michelson {| { SIZE } |}])@(xs)[@inline][@hidden] in
  let head_opt#70funtype a : * . list (a) -> option (a) = Λ a ->
  lambda (xslist (a))option (a) return  match xs with
                                         | Cons ctor_proj#2 ->
                                            match ctor_proj#2 with
                                             | ( x , _#2 ) ->
                                             Some(x)
                                         | Nil unit_proj#4 ->
                                           None(unit)[@inline][@hidden] in
  let tail_opt#71funtype a : * . list (a) -> option (list (a)) = Λ a ->
  lambda (xslist (a))option (list (a)) return  match xs with
                                                | Cons ctor_proj#5 ->
                                                   match ctor_proj#5 with
                                                    | ( _#3 , xs ) ->
                                                    Some(xs)
                                                | Nil unit_proj#7 ->
                                                  None(unit)[@inline][@hidden] in
  let map#72funtype a : * . funtype b : * . a -> b -> list (a) -> list (b) =
    Λ a ->
  Λ b ->
  lambda (fa -> b)list (a) -> list (b) return lambda (xslist (a))list (b) return
  LIST_MAP(f , xs)[@inline][@hidden] in
  let iter#73funtype a : * . a -> unit -> list (a) -> unit = Λ a ->
  lambda (fa -> unit)list (a) -> unit return lambda (xslist (a))unit return
  LIST_ITER(f , xs)[@inline][@hidden] in
  let fold#74funtype a : * . funtype b : * . ( b * a ) -> b -> list (a) -> b -> b =
    Λ a ->
  Λ b ->
  lambda (f( b * a ) -> b)list (a) -> b -> b return lambda (xslist (a))b -> b return lambda (ib)b return
  LIST_FOLD(f , xs , i)[@inline][@hidden] in
  let fold_left#75funtype a : * . funtype b : * . ( b * a ) -> b -> b -> list (a) -> b =
    Λ a ->
  Λ b ->
  lambda (f( b * a ) -> b)b -> list (a) -> b return lambda (ib)list (a) -> b return lambda (xslist (a))b return
  LIST_FOLD_LEFT(f , i , xs)[@inline][@hidden] in
  let fold_right#76funtype a : * . funtype b : * . ( a * b ) -> b -> list (a) -> b -> b =
    Λ a ->
  Λ b ->
  lambda (f( a * b ) -> b)list (a) -> b -> b return lambda (xslist (a))b -> b return lambda (ib)b return
  LIST_FOLD_RIGHT(f , xs , i)[@inline][@hidden] in
  let cons#77funtype a : * . a -> list (a) -> list (a) = Λ a ->
  lambda (xa)list (a) -> list (a) return lambda (xslist (a))list (a) return
  CONS(x , xs)[@inline][@hidden] in
  let length#78string -> nat =
    lambda (bstring)nat return ([%Michelson {| { SIZE } |}])@(b)[@inline][@hidden] in
  let concat#79string -> string -> string =
    lambda (b1string)string -> string return lambda (b2string)string return
  ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b1 , b2 ))[@inline][@hidden] in
  let sub#80nat -> nat -> string -> string =
    lambda (snat)nat -> string -> string return lambda (lnat)string -> string return lambda (bstring)string return
  ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(
  ( s ,
    l ,
    b ))[@inline][@hidden] in
  let unopt#81funtype a : * . option (a) -> a = Λ a ->
  lambda (voption (a))a return UNOPT(v)[@inline][@hidden] in
  let unopt_with_error#82funtype a : * . option (a) -> string -> a = Λ a ->
  lambda (voption (a))string -> a return lambda (sstring)a return UNOPT_WITH_ERROR
                                                                  (v ,
                                                                   s)[@inline][@hidden] in
  let pack#83funtype a : * . a -> bytes = Λ a ->
  lambda (va)bytes return ([%Michelson {| { PACK } |}])@(v)[@inline][@hidden] in
  let unpack#84funtype a : * . bytes -> option (a) = Λ a ->
  lambda (bbytes)option (a) return BYTES_UNPACK(b)[@inline][@hidden] in
  let length#85bytes -> nat =
    lambda (bbytes)nat return ([%Michelson {| { SIZE } |}])@(b)[@inline][@hidden] in
  let concat#86bytes -> bytes -> bytes =
    lambda (b1bytes)bytes -> bytes return lambda (b2bytes)bytes return
  ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b1 , b2 ))[@inline][@hidden] in
  let sub#87nat -> nat -> bytes -> bytes =
    lambda (snat)nat -> bytes -> bytes return lambda (lnat)bytes -> bytes return lambda (bbytes)bytes return
  ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(
  ( s ,
    l ,
    b ))[@inline][@hidden] in
  let blake2b#88bytes -> bytes =
    lambda (bbytes)bytes return ([%Michelson {| { BLAKE2B } |}])@(b)[@inline][@hidden] in
  let sha256#89bytes -> bytes =
    lambda (bbytes)bytes return ([%Michelson {| { SHA256 } |}])@(b)[@inline][@hidden] in
  let sha512#90bytes -> bytes =
    lambda (bbytes)bytes return ([%Michelson {| { SHA512 } |}])@(b)[@inline][@hidden] in
  let sha3#91bytes -> bytes =
    lambda (bbytes)bytes return ([%Michelson {| { SHA3 } |}])@(b)[@inline][@hidden] in
  let keccak#92bytes -> bytes =
    lambda (bbytes)bytes return ([%Michelson {| { KECCAK } |}])@(b)[@inline][@hidden] in
  let hash_key#93key -> key_hash =
    lambda (kkey)key_hash return ([%Michelson {| { HASH_KEY } |}])@(k)[@inline][@hidden] in
  let check#94key -> signature -> bytes -> bool =
    lambda (kkey)signature -> bytes -> bool return lambda (ssignature)bytes -> bool return lambda (bbytes)bool return
  ([%Michelson {| { UNPAIR ; UNPAIR ; CHECK_SIGNATURE } |}])@(( k , s , b ))[@inline][@hidden] in
  let assertbool -> unit =
    lambda (bbool)unit return ([%Michelson {| { IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } } |}])@(b)[@inline][@private][@hidden] in
  let assert_somefuntype a : * . option (a) -> unit = Λ a ->
  lambda (voption (a))unit return ([%Michelson {| { IF_NONE { PUSH string "failed assert some" ; FAILWITH } { DROP ; UNIT } } |}])@(v)[@inline][@private][@hidden] in
  let assert_nonefuntype a : * . option (a) -> unit = Λ a ->
  lambda (voption (a))unit return ([%Michelson {| { IF_NONE { UNIT } { PUSH string "failed assert none" ; FAILWITH } } |}])@(v)[@inline][@private][@hidden] in
  let absint -> nat =
    lambda (iint)nat return ([%Michelson {| { ABS } |}])@(i)[@inline][@private][@hidden] in
  let is_natint -> option (nat) =
    lambda (iint)option (nat) return ([%Michelson {| { ISNAT } |}])@(i)[@inline][@private][@hidden] in
  let truebool = TRUE()[@inline][@private][@hidden] in
  let falsebool = FALSE()[@inline][@private][@hidden] in
  let unitunit = UNIT()[@inline][@private][@hidden] in
  let failwithfuntype a : * . funtype b : * . a -> b = Λ a ->
  Λ b ->  [%Michelson {|{ FAILWITH }|}][@inline][@private][@hidden] in
  let intfuntype a : * . a -> external_int (a) = Λ a ->
  lambda (va)external_int (a) return ([%Michelson {| { INT } |}])@(v)[@inline][@private][@hidden] in
  let assert_with_errorbool -> string -> unit =
    lambda (bbool)string -> unit return lambda (sstring)unit return ([%Michelson {| { UNPAIR ; IF { DROP ; UNIT } { FAILWITH } } |}])@(
                                                                    ( b ,
                                                                      s ))[@inline][@private][@hidden] in
  let assert_some_with_errorfuntype a : * . option (a) -> string -> unit =
    Λ a ->
  lambda (voption (a))string -> unit return lambda (sstring)unit return
  ([%Michelson {| { UNPAIR ; IF_NONE { FAILWITH } { DROP 2 ; UNIT } } |}])@(
  ( v ,
    s ))[@inline][@private][@hidden] in
  let assert_none_with_errorfuntype a : * . option (a) -> string -> unit =
    Λ a ->
  lambda (voption (a))string -> unit return lambda (sstring)unit return
  ([%Michelson {| { UNPAIR ; IF_NONE { DROP ; UNIT } { DROP ; FAILWITH } } |}])@(
  ( v ,
    s ))[@inline][@private][@hidden] in
  let edivfuntype a : * . funtype b : * . a -> b -> external_ediv (a , b) =
    Λ a ->
  Λ b ->
  lambda (la)b -> external_ediv (a , b) return lambda (rb)external_ediv (a ,
  b) return ([%Michelson {| { UNPAIR ; EDIV } |}])@(( l , r ))[@inline][@private][@hidden] in
  let stub#95funtype a : * . funtype b : * . a -> b = Λ a ->
  Λ b ->  lambda (xa)b return ([%Michelson {|{ FAILWITH }|}])@(x)[@inline][@private][@hidden] in
  let run#96funtype a : * . funtype b : * . a -> b -> a -> unit = Λ a ->
  Λ b ->
  lambda (_fa -> b)a -> unit return lambda (_va)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let eval#97funtype a : * . a -> unit = Λ a ->
  lambda (_xa)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let compile_value#98funtype a : * . a -> unit = Λ a ->
  lambda (_xa)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let get_total_voting_power#99unit -> nat =
    lambda (_uunit)nat return (stub#95@{unit}@{nat})@(unit)[@inline][@hidden] in
  let failwith#100funtype a : * . funtype b : * . a -> b = Λ a ->
  Λ b ->  lambda (_va)b return (stub#95@{unit}@{b})@(unit)[@inline][@hidden] in
  let to_contract#101funtype p : * . funtype s : * . unit -> contract (p) =
    Λ p ->
  Λ s ->
  lambda (_tunit)contract (p) return (stub#95@{unit}@{contract (p)})@(unit)[@inline][@hidden] in
  let set_source#102address -> unit =
    lambda (_aaddress)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let get_storage_of_address#103address -> unit =
    lambda (_aaddress)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let get_balance#104address -> tez =
    lambda (_aaddress)tez return (stub#95@{unit}@{tez})@(unit)[@inline][@hidden] in
  let print#105string -> unit =
    lambda (_vstring)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let eprint#106string -> unit =
    lambda (_vstring)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let get_voting_power#107key_hash -> nat =
    lambda (_khkey_hash)nat return (stub#95@{unit}@{nat})@(unit)[@inline][@hidden] in
  let nth_bootstrap_contract#108nat -> address =
    lambda (_inat)address return (stub#95@{unit}@{address})@(unit)[@inline][@hidden] in
  let nth_bootstrap_account#109int -> address =
    lambda (_iint)address return (stub#95@{unit}@{address})@(unit)[@inline][@hidden] in
  let get_bootstrap_account#110nat -> ( address * key * string ) =
    lambda (_nnat)( address * key * string ) return (stub#95@{unit}@{( address * key * string )})@(unit)[@inline][@hidden] in
  let nth_bootstrap_typed_address#111funtype a : * . funtype b : * . nat -> unit =
    Λ a ->
  Λ b ->  lambda (_nnat)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let last_originations#112unit -> map (address , list (address)) =
    lambda (_uunit)map (address ,
  list (address)) return (stub#95@{unit}@{map (address ,
                         list (address))})@(unit)[@inline][@hidden] in
  let random#113funtype a : * . unit -> a = Λ a ->
  lambda (_uunit)a return (stub#95@{unit}@{a})@(unit)[@inline][@hidden] in
  let new_account#114unit -> ( string * key ) =
    lambda (_uunit)( string * key ) return (stub#95@{unit}@{( string * key )})@(unit)[@inline][@hidden] in
  let decompile#115funtype a : * . unit -> a = Λ a ->
  lambda (_munit)a return (stub#95@{unit}@{a})@(unit)[@inline][@hidden] in
  let bake_until_n_cycle_end#116nat -> unit =
    lambda (_nnat)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let cast_address#117funtype a : * . funtype b : * . address -> unit =
    Λ a ->
  Λ b ->  lambda (_aaddress)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let register_delegate#118key_hash -> unit =
    lambda (_khkey_hash)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let register_constant#119unit -> string =
    lambda (_munit)string return (stub#95@{unit}@{string})@(unit)[@inline][@hidden] in
  let to_typed_address#120funtype a : * . funtype b : * . contract (a) -> unit =
    Λ a ->
  Λ b ->  lambda (_ccontract (a))unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let constant_to_michelson_program#121string -> unit =
    lambda (_sstring)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let restore_context#122unit -> unit =
    lambda (_uunit)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let save_context#123unit -> unit =
    lambda (_uunit)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let drop_context#124unit -> unit =
    lambda (_uunit)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let to_string#125funtype a : * . a -> string = Λ a ->
  lambda (_va)string return (stub#95@{unit}@{string})@(unit)[@inline][@hidden] in
  let get_storage#126funtype p : * . funtype s : * . unit -> s = Λ p ->
  Λ s ->  lambda (_tunit)s return (stub#95@{unit}@{s})@(unit)[@inline][@hidden] in
  let set_baker_policy#127unit -> unit =
    lambda (_bpunit)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let set_baker#128address -> unit =
    lambda (_aaddress)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let size#129unit -> int =
    lambda (_cunit)int return (stub#95@{unit}@{int})@(unit)[@inline][@hidden] in
  let compile_contract#130funtype p : * . funtype s : * . ( p * s ) ->
  ( list (operation) * s ) -> unit = Λ p ->
  Λ s ->
  lambda (_f( p * s ) -> ( list (operation) * s ))unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let read_contract_from_file#131string -> unit =
    lambda (_fnstring)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let chr#132nat -> option (string) =
    lambda (_nnat)option (string) return (stub#95@{unit}@{option (string)})@(unit)[@inline][@hidden] in
  let nl#133string = "NEWLINE"[@inline][@hidden] in
  let println#134string -> unit =
    lambda (_vstring)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let transfer#135address -> unit -> tez -> unit =
    lambda (_aaddress)unit -> tez -> unit return lambda (_sunit)tez -> unit return lambda (_ttez)unit return
  (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let transfer_exn#136address -> unit -> tez -> nat =
    lambda (_aaddress)unit -> tez -> nat return lambda (_sunit)tez -> nat return lambda (_ttez)nat return
  (stub#95@{unit}@{nat})@(unit)[@inline][@hidden] in
  let log#137funtype a : * . a -> unit = Λ a ->
  lambda (_va)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let reset_state#138nat -> list (tez) -> unit =
    lambda (_nnat)list (tez) -> unit return lambda (_llist (tez))unit return
  (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let reset_state_at#139timestamp -> nat -> list (tez) -> unit =
    lambda (_ttimestamp)nat -> list (tez) -> unit return lambda (_nnat)list (tez) -> unit return lambda (_llist (tez))unit return
  (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let bootstrap_contract#140funtype p : * . funtype s : * . ( p * s ) ->
  ( list (operation) * s ) -> s -> tez -> unit = Λ p ->
  Λ s ->
  lambda (_f( p * s ) -> ( list (operation) * s ))s -> tez -> unit return lambda (_ss)tez -> unit return lambda (_ttez)unit return
  (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let mutate_value#141funtype a : * . nat -> a -> option (( a * unit )) =
    Λ a ->
  lambda (_nnat)a -> option (( a * unit )) return lambda (_va)option (( a * unit )) return
  (stub#95@{unit}@{option (( a * unit ))})@(unit)[@inline][@hidden] in
  let save_mutation#142string -> unit -> option (string) =
    lambda (_sstring)unit -> option (string) return lambda (_munit)option (string) return
  (stub#95@{unit}@{option (string)})@(unit)[@inline][@hidden] in
  let mutation_test#143funtype a : * . funtype b : * . a -> a -> b -> option (
  ( b * unit )) = Λ a ->
  Λ b ->
  lambda (_va)a -> b -> option (( b * unit )) return lambda (_fa -> b)option (
  ( b * unit )) return (stub#95@{unit}@{option (( b * unit ))})@(unit)[@inline][@hidden] in
  let mutation_test_all#144funtype a : * . funtype b : * . a -> a -> b -> list (
  ( b * unit )) = Λ a ->
  Λ b ->
  lambda (_va)a -> b -> list (( b * unit )) return lambda (_fa -> b)list (
  ( b * unit )) return (stub#95@{unit}@{list (( b * unit ))})@(unit)[@inline][@hidden] in
  let sign#145string -> bytes -> signature =
    lambda (_skstring)bytes -> signature return lambda (_dbytes)signature return
  (stub#95@{unit}@{signature})@(unit)[@inline][@hidden] in
  let add_account#146string -> key -> unit =
    lambda (_sstring)key -> unit return lambda (_kkey)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let baker_account#147( string * key ) -> option (tez) -> unit =
    lambda (_p( string * key ))option (tez) -> unit return lambda (_ooption (tez))unit return
  (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let set_big_map#148funtype a : * . funtype b : * . int -> big_map (a ,
  b) -> unit = Λ a ->
  Λ b ->
  lambda (_iint)big_map (a , b) -> unit return lambda (_mbig_map (a ,
  b))unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let create_chest#149bytes -> nat -> ( chest * chest_key ) =
    lambda (_bbytes)nat -> ( chest * chest_key ) return lambda (_nnat)( chest * chest_key ) return
  (stub#95@{unit}@{( chest * chest_key )})@(unit)[@inline][@hidden] in
  let create_chest_key#150chest -> nat -> chest_key =
    lambda (_cchest)nat -> chest_key return lambda (_nnat)chest_key return
  (stub#95@{unit}@{chest_key})@(unit)[@inline][@hidden] in
  let transfer_to_contract#151funtype p : * . contract (p) -> p -> tez -> unit =
    Λ p ->
  lambda (_ccontract (p))p -> tez -> unit return lambda (_sp)tez -> unit return lambda (_ttez)unit return
  (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let transfer_to_contract_exn#152funtype p : * . contract (p) -> p -> tez -> nat =
    Λ p ->
  lambda (_ccontract (p))p -> tez -> nat return lambda (_sp)tez -> nat return lambda (_ttez)nat return
  (stub#95@{unit}@{nat})@(unit)[@inline][@hidden] in
  let michelson_equal#153unit -> unit -> bool =
    lambda (_m1unit)unit -> bool return lambda (_m2unit)bool return (stub#95@{unit}@{bool})@(unit)[@inline][@hidden] in
  let to_entrypoint#154funtype a : * . funtype b : * . funtype c : * . string -> unit -> contract (c) =
    Λ a ->
  Λ b ->
  Λ c ->
  lambda (_sstring)unit -> contract (c) return lambda (_tunit)contract (c) return
  (stub#95@{unit}@{contract (c)})@(unit)[@inline][@hidden] in
  let originate_contract#155unit -> unit -> tez -> address =
    lambda (_cunit)unit -> tez -> address return lambda (_sunit)tez -> address return lambda (_ttez)address return
  (stub#95@{unit}@{address})@(unit)[@inline][@hidden] in
  let originate#156funtype p : * . funtype s : * . ( p * s ) -> ( list (operation) * s ) -> s -> tez ->
  ( unit * unit * int ) = Λ p ->
  Λ s ->
  lambda (_f( p * s ) -> ( list (operation) * s ))s -> tez -> ( unit * unit * int ) return lambda (_ss)tez ->
  ( unit * unit * int ) return lambda (_ttez)( unit * unit * int ) return
  (stub#95@{unit}@{( unit * unit * int )})@(unit)[@inline][@hidden] in
  let compile_contract_from_file#157string -> string -> list (string) -> unit =
    lambda (_fnstring)string -> list (string) -> unit return lambda (_estring)list (string) -> unit return lambda (_vlist (string))unit return
  (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let originate_from_file#158string -> string -> list (string) -> unit -> tez ->
  ( address * unit * int ) =
    lambda (_fnstring)string -> list (string) -> unit -> tez -> ( address * unit * int ) return lambda (_estring)list (string) -> unit -> tez ->
  ( address * unit * int ) return lambda (_vlist (string))unit -> tez ->
  ( address * unit * int ) return lambda (_sunit)tez -> ( address * unit * int ) return lambda (_ttez)
  ( address * unit * int ) return (stub#95@{unit}@{( address * unit * int )})@(unit)[@inline][@hidden] in
  let as#159int = 20 in
  let s_as#160int = 22 in let xint = ADD(as#159 , s_as#160) in unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias4.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias5.mligo" ] ;
  [%expect{xxx|
    let get_balance#8unit -> tez =
      lambda (_uunit)tez return ([%Michelson {| { DROP ; BALANCE } |}])@(unit)[@inline][@hidden] in
    let get_amount#9unit -> tez =
      lambda (_uunit)tez return ([%Michelson {| { DROP ; AMOUNT } |}])@(unit)[@inline][@hidden] in
    let get_now#10unit -> timestamp =
      lambda (_uunit)timestamp return ([%Michelson {| { DROP ; NOW } |}])@(unit)[@inline][@hidden] in
    let get_sender#11unit -> address =
      lambda (_uunit)address return ([%Michelson {| { DROP ; SENDER } |}])@(unit)[@inline][@hidden] in
    let get_source#12unit -> address =
      lambda (_uunit)address return ([%Michelson {| { DROP ; SOURCE } |}])@(unit)[@inline][@hidden] in
    let get_level#13unit -> nat =
      lambda (_uunit)nat return ([%Michelson {| { DROP ; LEVEL } |}])@(unit)[@inline][@hidden] in
    let get_self_address#14unit -> address =
      lambda (_uunit)address return SELF_ADDRESS()[@inline][@hidden] in
    let get_chain_id#15unit -> chain_id =
      lambda (_uunit)chain_id return ([%Michelson {| { DROP ; CHAIN_ID } |}])@(unit)[@inline][@hidden] in
    let get_total_voting_power#16unit -> nat =
      lambda (_uunit)nat return ([%Michelson {| { DROP ; TOTAL_VOTING_POWER } |}])@(unit)[@inline][@hidden] in
    let get_min_block_time#17unit -> nat =
      lambda (_uunit)nat return ([%Michelson {| { DROP; MIN_BLOCK_TIME } |}])@(unit)[@inline][@hidden] in
    let voting_power#18key_hash -> nat =
      lambda (khkey_hash)nat return ([%Michelson {| { VOTING_POWER } |}])@(kh)[@inline][@hidden] in
    let address#19funtype a : * . contract (a) -> address = Λ a ->
    lambda (ccontract (a))address return ADDRESS(c)[@inline][@hidden] in
    let implicit_account#20key_hash -> contract (unit) =
      lambda (khkey_hash)contract (unit) return IMPLICIT_ACCOUNT(kh)[@inline][@hidden] in
    let join_tickets#21funtype a : * . ( ticket (a) * ticket (a) ) -> option (ticket (a)) =
      Λ a ->
    lambda (t( ticket (a) * ticket (a) ))option (ticket (a)) return ([%Michelson {| { JOIN_TICKETS } |}])@(t)[@inline][@hidden] in
    let read_ticket#22funtype a : * . ticket (a) -> ( ( address * ( a * nat ) ) * ticket (a) ) =
      Λ a ->
    lambda (tticket (a))( ( address * ( a * nat ) ) * ticket (a) ) return
    ([%Michelson {| { READ_TICKET ; PAIR } |}])@(t)[@inline][@hidden] in
    let never#23funtype a : * . never -> a = Λ a ->
    lambda (nnever)a return ([%Michelson {| { NEVER } |}])@(n)[@inline][@hidden] in
    let pairing_check#24list (( bls12_381_g1 * bls12_381_g2 )) -> bool =
      lambda (llist (( bls12_381_g1 * bls12_381_g2 )))bool return ([%Michelson {| { PAIRING_CHECK } |}])@(l)[@inline][@hidden] in
    let constant#25funtype a : * . string -> a = Λ a ->
    lambda (sstring)a return GLOBAL_CONSTANT(s)[@inline][@hidden] in
    let set_delegate#26option (key_hash) -> operation =
      lambda (ooption (key_hash))operation return SET_DELEGATE(o)[@inline][@hidden] in
    let get_contract#27funtype a : * . address -> contract (a) = Λ a ->
    lambda (aaddress)contract (a) return CONTRACT(a)[@inline][@hidden] in
    let get_contract_opt#28funtype a : * . address -> option (contract (a)) =
      Λ a ->
    lambda (aaddress)option (contract (a)) return CONTRACT_OPT(a)[@inline][@hidden] in
    let get_contract_with_error#29funtype a : * . address -> string -> contract (a) =
      Λ a ->
    lambda (aaddress)string -> contract (a) return lambda (sstring)contract (a) return
    CONTRACT_WITH_ERROR(a , s)[@inline][@hidden] in
    let create_ticket#30funtype a : * . a -> nat -> ticket (a) = Λ a ->
    lambda (va)nat -> ticket (a) return lambda (nnat)ticket (a) return ([%Michelson {| { UNPAIR ; TICKET } |}])@(
                                                                       ( v ,
                                                                        n ))[@inline][@hidden] in
    let transaction#31funtype a : * . a -> tez -> contract (a) -> operation =
      Λ a ->
    lambda (aa)tez -> contract (a) -> operation return lambda (mutez)contract (a) -> operation return lambda (ccontract (a))operation return
    CALL(a , mu , c)[@inline][@hidden] in
    let open_chest#32chest_key -> chest -> nat -> sum[Fail_decrypt -> unit , Fail_timelock -> unit , Ok_opening -> bytes] =
      lambda (ckchest_key)chest -> nat -> sum[Fail_decrypt -> unit , Fail_timelock -> unit , Ok_opening -> bytes] return lambda (cchest)nat ->
    sum[Fail_decrypt -> unit , Fail_timelock -> unit , Ok_opening -> bytes] return lambda (nnat)
    sum[Fail_decrypt -> unit , Fail_timelock -> unit , Ok_opening -> bytes] return
    OPEN_CHEST(ck , c , n)[@inline][@hidden] in
    let call_view#33funtype a : * . funtype b : * . string -> a -> address -> option (b) =
      Λ a ->
    Λ b ->
    lambda (sstring)a -> address -> option (b) return lambda (xa)address -> option (b) return lambda (aaddress)option (b) return
    VIEW(s , x , a)[@inline][@hidden] in
    let split_ticket#34funtype a : * . ticket (a) -> ( nat * nat ) -> option (
    ( ticket (a) * ticket (a) )) = Λ a ->
    lambda (tticket (a))( nat * nat ) -> option (( ticket (a) * ticket (a) )) return lambda (p
    ( nat * nat ))option (( ticket (a) * ticket (a) )) return ([%Michelson {| { UNPAIR ; SPLIT_TICKET } |}])@(
                                                              ( t ,
                                                                p ))[@inline][@hidden] in
    let xor#35nat -> nat -> nat =
      lambda (lnat)nat -> nat return lambda (rnat)nat return XOR(l , r)[@inline][@hidden] in
    let shift_left#36nat -> nat -> nat =
      lambda (lnat)nat -> nat return lambda (rnat)nat return LSL(l , r)[@inline][@hidden] in
    let shift_right#37nat -> nat -> nat =
      lambda (lnat)nat -> nat return lambda (rnat)nat return LSR(l , r)[@inline][@hidden] in
    let empty#38funtype k : * . funtype v : * . big_map (k , v) = Λ k ->
    Λ v ->  BIG_MAP_EMPTY()[@inline][@hidden] in
    let mem#39funtype k : * . funtype v : * . k -> big_map (k , v) -> bool =
      Λ k ->
    Λ v ->
    lambda (kk)big_map (k , v) -> bool return lambda (mbig_map (k ,
    v))bool return ([%Michelson {| { UNPAIR ; MEM } |}])@(( k , m ))[@inline][@hidden] in
    let add#40funtype k : * . funtype v : * . k -> v -> big_map (k ,
    v) -> big_map (k , v) = Λ k ->
    Λ v ->
    lambda (kk)v -> big_map (k , v) -> big_map (k ,
    v) return lambda (vv)big_map (k , v) -> big_map (k ,
    v) return lambda (mbig_map (k , v))big_map (k , v) return MAP_ADD(k , v , m)[@inline][@hidden] in
    let remove#41funtype k : * . funtype v : * . k -> big_map (k ,
    v) -> big_map (k , v) = Λ k ->
    Λ v ->
    lambda (kk)big_map (k , v) -> big_map (k , v) return lambda (mbig_map (k ,
    v))big_map (k , v) return MAP_REMOVE(k , m)[@inline][@hidden] in
    let update#42funtype k : * . funtype v : * . k -> option (v) -> big_map (k ,
    v) -> big_map (k , v) = Λ k ->
    Λ v ->
    lambda (kk)option (v) -> big_map (k , v) -> big_map (k ,
    v) return lambda (voption (v))big_map (k , v) -> big_map (k ,
    v) return lambda (mbig_map (k , v))big_map (k ,
    v) return MAP_UPDATE(k , v , m)[@inline][@hidden] in
    let get_and_update#43funtype k : * . funtype v : * . k -> option (v) -> big_map (k ,
    v) -> ( option (v) * big_map (k , v) ) = Λ k ->
    Λ v ->
    lambda (kk)option (v) -> big_map (k ,
    v) -> ( option (v) * big_map (k , v) ) return lambda (voption (v))big_map (k ,
    v) -> ( option (v) * big_map (k , v) ) return lambda (mbig_map (k ,
    v))( option (v) * big_map (k , v) ) return BIG_MAP_GET_AND_UPDATE(k , v , m)[@inline][@hidden] in
    let find_opt#44funtype k : * . funtype v : * . k -> big_map (k ,
    v) -> option (v) = Λ k ->
    Λ v ->
    lambda (kk)big_map (k , v) -> option (v) return lambda (mbig_map (k ,
    v))option (v) return MAP_FIND_OPT(k , m)[@inline][@hidden] in
    let find#45funtype k : * . funtype v : * . k -> big_map (k , v) -> v =
      Λ k ->
    Λ v ->
    lambda (kk)big_map (k , v) -> v return lambda (mbig_map (k ,
    v))v return MAP_FIND(k , m)[@inline][@hidden] in
    let empty#46funtype k : * . funtype v : * . map (k , v) = Λ k ->
    Λ v ->  MAP_EMPTY()[@inline][@hidden] in
    let size#47funtype k : * . funtype v : * . map (k , v) -> nat = Λ k ->
    Λ v ->  lambda (mmap (k , v))nat return ([%Michelson {| { SIZE } |}])@(m)[@inline][@hidden] in
    let mem#48funtype k : * . funtype v : * . k -> map (k , v) -> bool = Λ k ->
    Λ v ->
    lambda (kk)map (k , v) -> bool return lambda (mmap (k ,
    v))bool return ([%Michelson {| { UNPAIR ; MEM } |}])@(( k , m ))[@inline][@hidden] in
    let add#49funtype k : * . funtype v : * . k -> v -> map (k , v) -> map (k ,
    v) = Λ k ->
    Λ v ->
    lambda (kk)v -> map (k , v) -> map (k , v) return lambda (vv)map (k ,
    v) -> map (k , v) return lambda (mmap (k , v))map (k ,
    v) return MAP_ADD(k , v , m)[@inline][@hidden] in
    let remove#50funtype k : * . funtype v : * . k -> map (k , v) -> map (k ,
    v) = Λ k ->
    Λ v ->
    lambda (kk)map (k , v) -> map (k , v) return lambda (mmap (k , v))map (k ,
    v) return MAP_REMOVE(k , m)[@inline][@hidden] in
    let update#51funtype k : * . funtype v : * . k -> option (v) -> map (k ,
    v) -> map (k , v) = Λ k ->
    Λ v ->
    lambda (kk)option (v) -> map (k , v) -> map (k ,
    v) return lambda (voption (v))map (k , v) -> map (k ,
    v) return lambda (mmap (k , v))map (k , v) return MAP_UPDATE(k , v , m)[@inline][@hidden] in
    let get_and_update#52funtype k : * . funtype v : * . k -> option (v) -> map (k ,
    v) -> ( option (v) * map (k , v) ) = Λ k ->
    Λ v ->
    lambda (kk)option (v) -> map (k ,
    v) -> ( option (v) * map (k , v) ) return lambda (voption (v))map (k ,
    v) -> ( option (v) * map (k , v) ) return lambda (mmap (k ,
    v))( option (v) * map (k , v) ) return MAP_GET_AND_UPDATE(k , v , m)[@inline][@hidden] in
    let find#53funtype k : * . funtype v : * . k -> map (k , v) -> v = Λ k ->
    Λ v ->
    lambda (kk)map (k , v) -> v return lambda (mmap (k ,
    v))v return MAP_FIND(k , m)[@inline][@hidden] in
    let find_opt#54funtype k : * . funtype v : * . k -> map (k ,
    v) -> option (v) = Λ k ->
    Λ v ->
    lambda (kk)map (k , v) -> option (v) return lambda (mmap (k ,
    v))option (v) return MAP_FIND_OPT(k , m)[@inline][@hidden] in
    let iter#55funtype k : * . funtype v : * . ( k * v ) -> unit -> map (k ,
    v) -> unit = Λ k ->
    Λ v ->
    lambda (f( k * v ) -> unit)map (k , v) -> unit return lambda (mmap (k ,
    v))unit return MAP_ITER(f , m)[@inline][@hidden] in
    let map#56funtype k : * . funtype v : * . funtype w : * . ( k * v ) -> w -> map (k ,
    v) -> map (k , w) = Λ k ->
    Λ v ->
    Λ w ->
    lambda (f( k * v ) -> w)map (k , v) -> map (k , w) return lambda (mmap (k ,
    v))map (k , w) return MAP_MAP(f , m)[@inline][@hidden] in
    let fold#57funtype k : * . funtype v : * . funtype c : * . ( c * ( k * v ) ) -> c -> map (k ,
    v) -> c -> c = Λ k ->
    Λ v ->
    Λ c ->
    lambda (f( c * ( k * v ) ) -> c)map (k ,
    v) -> c -> c return lambda (mmap (k ,
    v))c -> c return lambda (ic)c return MAP_FOLD(f , m , i)[@inline][@hidden] in
    let empty#58funtype a : * . set (a) = Λ a ->
    SET_EMPTY()[@inline][@hidden] in
    let size#59funtype a : * . set (a) -> nat = Λ a ->
    lambda (sset (a))nat return ([%Michelson {| { SIZE } |}])@(s)[@inline][@hidden] in
    let cardinal#60funtype a : * . set (a) -> nat = Λ a ->
    lambda (sset (a))nat return ([%Michelson {| { SIZE } |}])@(s)[@inline][@hidden] in
    let mem#61funtype a : * . a -> set (a) -> bool = Λ a ->
    lambda (xa)set (a) -> bool return lambda (sset (a))bool return SET_MEM(x , s)[@inline][@hidden] in
    let add#62funtype a : * . a -> set (a) -> set (a) = Λ a ->
    lambda (xa)set (a) -> set (a) return lambda (sset (a))set (a) return
    SET_ADD(x , s)[@inline][@hidden] in
    let remove#63funtype a : * . a -> set (a) -> set (a) = Λ a ->
    lambda (xa)set (a) -> set (a) return lambda (sset (a))set (a) return
    SET_REMOVE(x , s)[@inline][@hidden] in
    let update#64funtype a : * . a -> bool -> set (a) -> set (a) = Λ a ->
    lambda (xa)bool -> set (a) -> set (a) return lambda (bbool)set (a) -> set (a) return lambda (sset (a))set (a) return
    SET_UPDATE(x , b , s)[@inline][@hidden] in
    let iter#65funtype a : * . a -> unit -> set (a) -> unit = Λ a ->
    lambda (fa -> unit)set (a) -> unit return lambda (sset (a))unit return
    SET_ITER(f , s)[@inline][@hidden] in
    let fold#66funtype a : * . funtype b : * . ( b * a ) -> b -> set (a) -> b -> b =
      Λ a ->
    Λ b ->
    lambda (f( b * a ) -> b)set (a) -> b -> b return lambda (sset (a))b -> b return lambda (ib)b return
    SET_FOLD(f , s , i)[@inline][@hidden] in
    let fold_desc#67funtype a : * . funtype b : * . ( a * b ) -> b -> set (a) -> b -> b =
      Λ a ->
    Λ b ->
    lambda (f( a * b ) -> b)set (a) -> b -> b return lambda (sset (a))b -> b return lambda (ib)b return
    SET_FOLD_DESC(f , s , i)[@inline][@hidden] in
    let length#68funtype a : * . list (a) -> nat = Λ a ->
    lambda (xslist (a))nat return ([%Michelson {| { SIZE } |}])@(xs)[@inline][@hidden] in
    let size#69funtype a : * . list (a) -> nat = Λ a ->
    lambda (xslist (a))nat return ([%Michelson {| { SIZE } |}])@(xs)[@inline][@hidden] in
    let head_opt#70funtype a : * . list (a) -> option (a) = Λ a ->
    lambda (xslist (a))option (a) return  match xs with
                                           | Cons ctor_proj#2 ->
                                              match ctor_proj#2 with
                                               | ( x , _#2 ) ->
                                               Some(x)
                                           | Nil unit_proj#4 ->
                                             None(unit)[@inline][@hidden] in
    let tail_opt#71funtype a : * . list (a) -> option (list (a)) = Λ a ->
    lambda (xslist (a))option (list (a)) return  match xs with
                                                  | Cons ctor_proj#5 ->
                                                     match ctor_proj#5 with
                                                      | ( _#3 , xs ) ->
                                                      Some(xs)
                                                  | Nil unit_proj#7 ->
                                                    None(unit)[@inline][@hidden] in
    let map#72funtype a : * . funtype b : * . a -> b -> list (a) -> list (b) =
      Λ a ->
    Λ b ->
    lambda (fa -> b)list (a) -> list (b) return lambda (xslist (a))list (b) return
    LIST_MAP(f , xs)[@inline][@hidden] in
    let iter#73funtype a : * . a -> unit -> list (a) -> unit = Λ a ->
    lambda (fa -> unit)list (a) -> unit return lambda (xslist (a))unit return
    LIST_ITER(f , xs)[@inline][@hidden] in
    let fold#74funtype a : * . funtype b : * . ( b * a ) -> b -> list (a) -> b -> b =
      Λ a ->
    Λ b ->
    lambda (f( b * a ) -> b)list (a) -> b -> b return lambda (xslist (a))b -> b return lambda (ib)b return
    LIST_FOLD(f , xs , i)[@inline][@hidden] in
    let fold_left#75funtype a : * . funtype b : * . ( b * a ) -> b -> b -> list (a) -> b =
      Λ a ->
    Λ b ->
    lambda (f( b * a ) -> b)b -> list (a) -> b return lambda (ib)list (a) -> b return lambda (xslist (a))b return
    LIST_FOLD_LEFT(f , i , xs)[@inline][@hidden] in
    let fold_right#76funtype a : * . funtype b : * . ( a * b ) -> b -> list (a) -> b -> b =
      Λ a ->
    Λ b ->
    lambda (f( a * b ) -> b)list (a) -> b -> b return lambda (xslist (a))b -> b return lambda (ib)b return
    LIST_FOLD_RIGHT(f , xs , i)[@inline][@hidden] in
    let cons#77funtype a : * . a -> list (a) -> list (a) = Λ a ->
    lambda (xa)list (a) -> list (a) return lambda (xslist (a))list (a) return
    CONS(x , xs)[@inline][@hidden] in
    let length#78string -> nat =
      lambda (bstring)nat return ([%Michelson {| { SIZE } |}])@(b)[@inline][@hidden] in
    let concat#79string -> string -> string =
      lambda (b1string)string -> string return lambda (b2string)string return
    ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b1 , b2 ))[@inline][@hidden] in
    let sub#80nat -> nat -> string -> string =
      lambda (snat)nat -> string -> string return lambda (lnat)string -> string return lambda (bstring)string return
    ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(
    ( s ,
      l ,
      b ))[@inline][@hidden] in
    let unopt#81funtype a : * . option (a) -> a = Λ a ->
    lambda (voption (a))a return UNOPT(v)[@inline][@hidden] in
    let unopt_with_error#82funtype a : * . option (a) -> string -> a = Λ a ->
    lambda (voption (a))string -> a return lambda (sstring)a return UNOPT_WITH_ERROR
                                                                    (v ,
                                                                     s)[@inline][@hidden] in
    let pack#83funtype a : * . a -> bytes = Λ a ->
    lambda (va)bytes return ([%Michelson {| { PACK } |}])@(v)[@inline][@hidden] in
    let unpack#84funtype a : * . bytes -> option (a) = Λ a ->
    lambda (bbytes)option (a) return BYTES_UNPACK(b)[@inline][@hidden] in
    let length#85bytes -> nat =
      lambda (bbytes)nat return ([%Michelson {| { SIZE } |}])@(b)[@inline][@hidden] in
    let concat#86bytes -> bytes -> bytes =
      lambda (b1bytes)bytes -> bytes return lambda (b2bytes)bytes return
    ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b1 , b2 ))[@inline][@hidden] in
    let sub#87nat -> nat -> bytes -> bytes =
      lambda (snat)nat -> bytes -> bytes return lambda (lnat)bytes -> bytes return lambda (bbytes)bytes return
    ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(
    ( s ,
      l ,
      b ))[@inline][@hidden] in
    let blake2b#88bytes -> bytes =
      lambda (bbytes)bytes return ([%Michelson {| { BLAKE2B } |}])@(b)[@inline][@hidden] in
    let sha256#89bytes -> bytes =
      lambda (bbytes)bytes return ([%Michelson {| { SHA256 } |}])@(b)[@inline][@hidden] in
    let sha512#90bytes -> bytes =
      lambda (bbytes)bytes return ([%Michelson {| { SHA512 } |}])@(b)[@inline][@hidden] in
    let sha3#91bytes -> bytes =
      lambda (bbytes)bytes return ([%Michelson {| { SHA3 } |}])@(b)[@inline][@hidden] in
    let keccak#92bytes -> bytes =
      lambda (bbytes)bytes return ([%Michelson {| { KECCAK } |}])@(b)[@inline][@hidden] in
    let hash_key#93key -> key_hash =
      lambda (kkey)key_hash return ([%Michelson {| { HASH_KEY } |}])@(k)[@inline][@hidden] in
    let check#94key -> signature -> bytes -> bool =
      lambda (kkey)signature -> bytes -> bool return lambda (ssignature)bytes -> bool return lambda (bbytes)bool return
    ([%Michelson {| { UNPAIR ; UNPAIR ; CHECK_SIGNATURE } |}])@(( k , s , b ))[@inline][@hidden] in
    let assertbool -> unit =
      lambda (bbool)unit return ([%Michelson {| { IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } } |}])@(b)[@inline][@private][@hidden] in
    let assert_somefuntype a : * . option (a) -> unit = Λ a ->
    lambda (voption (a))unit return ([%Michelson {| { IF_NONE { PUSH string "failed assert some" ; FAILWITH } { DROP ; UNIT } } |}])@(v)[@inline][@private][@hidden] in
    let assert_nonefuntype a : * . option (a) -> unit = Λ a ->
    lambda (voption (a))unit return ([%Michelson {| { IF_NONE { UNIT } { PUSH string "failed assert none" ; FAILWITH } } |}])@(v)[@inline][@private][@hidden] in
    let absint -> nat =
      lambda (iint)nat return ([%Michelson {| { ABS } |}])@(i)[@inline][@private][@hidden] in
    let is_natint -> option (nat) =
      lambda (iint)option (nat) return ([%Michelson {| { ISNAT } |}])@(i)[@inline][@private][@hidden] in
    let truebool = TRUE()[@inline][@private][@hidden] in
    let falsebool = FALSE()[@inline][@private][@hidden] in
    let unitunit = UNIT()[@inline][@private][@hidden] in
    let failwithfuntype a : * . funtype b : * . a -> b = Λ a ->
    Λ b ->  [%Michelson {|{ FAILWITH }|}][@inline][@private][@hidden] in
    let intfuntype a : * . a -> external_int (a) = Λ a ->
    lambda (va)external_int (a) return ([%Michelson {| { INT } |}])@(v)[@inline][@private][@hidden] in
    let assert_with_errorbool -> string -> unit =
      lambda (bbool)string -> unit return lambda (sstring)unit return ([%Michelson {| { UNPAIR ; IF { DROP ; UNIT } { FAILWITH } } |}])@(
                                                                      ( b ,
                                                                        s ))[@inline][@private][@hidden] in
    let assert_some_with_errorfuntype a : * . option (a) -> string -> unit =
      Λ a ->
    lambda (voption (a))string -> unit return lambda (sstring)unit return
    ([%Michelson {| { UNPAIR ; IF_NONE { FAILWITH } { DROP 2 ; UNIT } } |}])@(
    ( v ,
      s ))[@inline][@private][@hidden] in
    let assert_none_with_errorfuntype a : * . option (a) -> string -> unit =
      Λ a ->
    lambda (voption (a))string -> unit return lambda (sstring)unit return
    ([%Michelson {| { UNPAIR ; IF_NONE { DROP ; UNIT } { DROP ; FAILWITH } } |}])@(
    ( v ,
      s ))[@inline][@private][@hidden] in
    let edivfuntype a : * . funtype b : * . a -> b -> external_ediv (a , b) =
      Λ a ->
    Λ b ->
    lambda (la)b -> external_ediv (a , b) return lambda (rb)external_ediv (a ,
    b) return ([%Michelson {| { UNPAIR ; EDIV } |}])@(( l , r ))[@inline][@private][@hidden] in
    let stub#95funtype a : * . funtype b : * . a -> b = Λ a ->
    Λ b ->  lambda (xa)b return ([%Michelson {|{ FAILWITH }|}])@(x)[@inline][@private][@hidden] in
    let run#96funtype a : * . funtype b : * . a -> b -> a -> unit = Λ a ->
    Λ b ->
    lambda (_fa -> b)a -> unit return lambda (_va)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
    let eval#97funtype a : * . a -> unit = Λ a ->
    lambda (_xa)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
    let compile_value#98funtype a : * . a -> unit = Λ a ->
    lambda (_xa)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
    let get_total_voting_power#99unit -> nat =
      lambda (_uunit)nat return (stub#95@{unit}@{nat})@(unit)[@inline][@hidden] in
    let failwith#100funtype a : * . funtype b : * . a -> b = Λ a ->
    Λ b ->  lambda (_va)b return (stub#95@{unit}@{b})@(unit)[@inline][@hidden] in
    let to_contract#101funtype p : * . funtype s : * . unit -> contract (p) =
      Λ p ->
    Λ s ->
    lambda (_tunit)contract (p) return (stub#95@{unit}@{contract (p)})@(unit)[@inline][@hidden] in
    let set_source#102address -> unit =
      lambda (_aaddress)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
    let get_storage_of_address#103address -> unit =
      lambda (_aaddress)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
    let get_balance#104address -> tez =
      lambda (_aaddress)tez return (stub#95@{unit}@{tez})@(unit)[@inline][@hidden] in
    let print#105string -> unit =
      lambda (_vstring)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
    let eprint#106string -> unit =
      lambda (_vstring)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
    let get_voting_power#107key_hash -> nat =
      lambda (_khkey_hash)nat return (stub#95@{unit}@{nat})@(unit)[@inline][@hidden] in
    let nth_bootstrap_contract#108nat -> address =
      lambda (_inat)address return (stub#95@{unit}@{address})@(unit)[@inline][@hidden] in
    let nth_bootstrap_account#109int -> address =
      lambda (_iint)address return (stub#95@{unit}@{address})@(unit)[@inline][@hidden] in
    let get_bootstrap_account#110nat -> ( address * key * string ) =
      lambda (_nnat)( address * key * string ) return (stub#95@{unit}@{( address * key * string )})@(unit)[@inline][@hidden] in
    let nth_bootstrap_typed_address#111funtype a : * . funtype b : * . nat -> unit =
      Λ a ->
    Λ b ->  lambda (_nnat)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
    let last_originations#112unit -> map (address , list (address)) =
      lambda (_uunit)map (address ,
    list (address)) return (stub#95@{unit}@{map (address ,
                           list (address))})@(unit)[@inline][@hidden] in
    let random#113funtype a : * . unit -> a = Λ a ->
    lambda (_uunit)a return (stub#95@{unit}@{a})@(unit)[@inline][@hidden] in
    let new_account#114unit -> ( string * key ) =
      lambda (_uunit)( string * key ) return (stub#95@{unit}@{( string * key )})@(unit)[@inline][@hidden] in
    let decompile#115funtype a : * . unit -> a = Λ a ->
    lambda (_munit)a return (stub#95@{unit}@{a})@(unit)[@inline][@hidden] in
    let bake_until_n_cycle_end#116nat -> unit =
      lambda (_nnat)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
    let cast_address#117funtype a : * . funtype b : * . address -> unit =
      Λ a ->
    Λ b ->  lambda (_aaddress)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
    let register_delegate#118key_hash -> unit =
      lambda (_khkey_hash)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
    let register_constant#119unit -> string =
      lambda (_munit)string return (stub#95@{unit}@{string})@(unit)[@inline][@hidden] in
    let to_typed_address#120funtype a : * . funtype b : * . contract (a) -> unit =
      Λ a ->
    Λ b ->  lambda (_ccontract (a))unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
    let constant_to_michelson_program#121string -> unit =
      lambda (_sstring)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
    let restore_context#122unit -> unit =
      lambda (_uunit)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
    let save_context#123unit -> unit =
      lambda (_uunit)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
    let drop_context#124unit -> unit =
      lambda (_uunit)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
    let to_string#125funtype a : * . a -> string = Λ a ->
    lambda (_va)string return (stub#95@{unit}@{string})@(unit)[@inline][@hidden] in
    let get_storage#126funtype p : * . funtype s : * . unit -> s = Λ p ->
    Λ s ->  lambda (_tunit)s return (stub#95@{unit}@{s})@(unit)[@inline][@hidden] in
    let set_baker_policy#127unit -> unit =
      lambda (_bpunit)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
    let set_baker#128address -> unit =
      lambda (_aaddress)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
    let size#129unit -> int =
      lambda (_cunit)int return (stub#95@{unit}@{int})@(unit)[@inline][@hidden] in
    let compile_contract#130funtype p : * . funtype s : * . ( p * s ) ->
    ( list (operation) * s ) -> unit = Λ p ->
    Λ s ->
    lambda (_f( p * s ) -> ( list (operation) * s ))unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
    let read_contract_from_file#131string -> unit =
      lambda (_fnstring)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
    let chr#132nat -> option (string) =
      lambda (_nnat)option (string) return (stub#95@{unit}@{option (string)})@(unit)[@inline][@hidden] in
    let nl#133string = "NEWLINE"[@inline][@hidden] in
    let println#134string -> unit =
      lambda (_vstring)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
    let transfer#135address -> unit -> tez -> unit =
      lambda (_aaddress)unit -> tez -> unit return lambda (_sunit)tez -> unit return lambda (_ttez)unit return
    (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
    let transfer_exn#136address -> unit -> tez -> nat =
      lambda (_aaddress)unit -> tez -> nat return lambda (_sunit)tez -> nat return lambda (_ttez)nat return
    (stub#95@{unit}@{nat})@(unit)[@inline][@hidden] in
    let log#137funtype a : * . a -> unit = Λ a ->
    lambda (_va)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
    let reset_state#138nat -> list (tez) -> unit =
      lambda (_nnat)list (tez) -> unit return lambda (_llist (tez))unit return
    (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
    let reset_state_at#139timestamp -> nat -> list (tez) -> unit =
      lambda (_ttimestamp)nat -> list (tez) -> unit return lambda (_nnat)list (tez) -> unit return lambda (_llist (tez))unit return
    (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
    let bootstrap_contract#140funtype p : * . funtype s : * . ( p * s ) ->
    ( list (operation) * s ) -> s -> tez -> unit = Λ p ->
    Λ s ->
    lambda (_f( p * s ) -> ( list (operation) * s ))s -> tez -> unit return lambda (_ss)tez -> unit return lambda (_ttez)unit return
    (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
    let mutate_value#141funtype a : * . nat -> a -> option (( a * unit )) =
      Λ a ->
    lambda (_nnat)a -> option (( a * unit )) return lambda (_va)option (( a * unit )) return
    (stub#95@{unit}@{option (( a * unit ))})@(unit)[@inline][@hidden] in
    let save_mutation#142string -> unit -> option (string) =
      lambda (_sstring)unit -> option (string) return lambda (_munit)option (string) return
    (stub#95@{unit}@{option (string)})@(unit)[@inline][@hidden] in
    let mutation_test#143funtype a : * . funtype b : * . a -> a -> b -> option (
    ( b * unit )) = Λ a ->
    Λ b ->
    lambda (_va)a -> b -> option (( b * unit )) return lambda (_fa -> b)option (
    ( b * unit )) return (stub#95@{unit}@{option (( b * unit ))})@(unit)[@inline][@hidden] in
    let mutation_test_all#144funtype a : * . funtype b : * . a -> a -> b -> list (
    ( b * unit )) = Λ a ->
    Λ b ->
    lambda (_va)a -> b -> list (( b * unit )) return lambda (_fa -> b)list (
    ( b * unit )) return (stub#95@{unit}@{list (( b * unit ))})@(unit)[@inline][@hidden] in
    let sign#145string -> bytes -> signature =
      lambda (_skstring)bytes -> signature return lambda (_dbytes)signature return
    (stub#95@{unit}@{signature})@(unit)[@inline][@hidden] in
    let add_account#146string -> key -> unit =
      lambda (_sstring)key -> unit return lambda (_kkey)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
    let baker_account#147( string * key ) -> option (tez) -> unit =
      lambda (_p( string * key ))option (tez) -> unit return lambda (_ooption (tez))unit return
    (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
    let set_big_map#148funtype a : * . funtype b : * . int -> big_map (a ,
    b) -> unit = Λ a ->
    Λ b ->
    lambda (_iint)big_map (a , b) -> unit return lambda (_mbig_map (a ,
    b))unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
    let create_chest#149bytes -> nat -> ( chest * chest_key ) =
      lambda (_bbytes)nat -> ( chest * chest_key ) return lambda (_nnat)( chest * chest_key ) return
    (stub#95@{unit}@{( chest * chest_key )})@(unit)[@inline][@hidden] in
    let create_chest_key#150chest -> nat -> chest_key =
      lambda (_cchest)nat -> chest_key return lambda (_nnat)chest_key return
    (stub#95@{unit}@{chest_key})@(unit)[@inline][@hidden] in
    let transfer_to_contract#151funtype p : * . contract (p) -> p -> tez -> unit =
      Λ p ->
    lambda (_ccontract (p))p -> tez -> unit return lambda (_sp)tez -> unit return lambda (_ttez)unit return
    (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
    let transfer_to_contract_exn#152funtype p : * . contract (p) -> p -> tez -> nat =
      Λ p ->
    lambda (_ccontract (p))p -> tez -> nat return lambda (_sp)tez -> nat return lambda (_ttez)nat return
    (stub#95@{unit}@{nat})@(unit)[@inline][@hidden] in
    let michelson_equal#153unit -> unit -> bool =
      lambda (_m1unit)unit -> bool return lambda (_m2unit)bool return (stub#95@{unit}@{bool})@(unit)[@inline][@hidden] in
    let to_entrypoint#154funtype a : * . funtype b : * . funtype c : * . string -> unit -> contract (c) =
      Λ a ->
    Λ b ->
    Λ c ->
    lambda (_sstring)unit -> contract (c) return lambda (_tunit)contract (c) return
    (stub#95@{unit}@{contract (c)})@(unit)[@inline][@hidden] in
    let originate_contract#155unit -> unit -> tez -> address =
      lambda (_cunit)unit -> tez -> address return lambda (_sunit)tez -> address return lambda (_ttez)address return
    (stub#95@{unit}@{address})@(unit)[@inline][@hidden] in
    let originate#156funtype p : * . funtype s : * . ( p * s ) -> ( list (operation) * s ) -> s -> tez ->
    ( unit * unit * int ) = Λ p ->
    Λ s ->
    lambda (_f( p * s ) -> ( list (operation) * s ))s -> tez -> ( unit * unit * int ) return lambda (_ss)tez ->
    ( unit * unit * int ) return lambda (_ttez)( unit * unit * int ) return
    (stub#95@{unit}@{( unit * unit * int )})@(unit)[@inline][@hidden] in
    let compile_contract_from_file#157string -> string -> list (string) -> unit =
      lambda (_fnstring)string -> list (string) -> unit return lambda (_estring)list (string) -> unit return lambda (_vlist (string))unit return
    (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
    let originate_from_file#158string -> string -> list (string) -> unit -> tez ->
    ( address * unit * int ) =
      lambda (_fnstring)string -> list (string) -> unit -> tez -> ( address * unit * int ) return lambda (_estring)list (string) -> unit -> tez ->
    ( address * unit * int ) return lambda (_vlist (string))unit -> tez ->
    ( address * unit * int ) return lambda (_sunit)tez -> ( address * unit * int ) return lambda (_ttez)
    ( address * unit * int ) return (stub#95@{unit}@{( address * unit * int )})@(unit)[@inline][@hidden] in
    let a#159int = 1 in
    let as#160int = 42 in let as#161int = 3 in let xint = as#160 in unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias5.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias6.mligo" ] ;
  [%expect {xxx|
  let get_balance#8unit -> tez =
    lambda (_uunit)tez return ([%Michelson {| { DROP ; BALANCE } |}])@(unit)[@inline][@hidden] in
  let get_amount#9unit -> tez =
    lambda (_uunit)tez return ([%Michelson {| { DROP ; AMOUNT } |}])@(unit)[@inline][@hidden] in
  let get_now#10unit -> timestamp =
    lambda (_uunit)timestamp return ([%Michelson {| { DROP ; NOW } |}])@(unit)[@inline][@hidden] in
  let get_sender#11unit -> address =
    lambda (_uunit)address return ([%Michelson {| { DROP ; SENDER } |}])@(unit)[@inline][@hidden] in
  let get_source#12unit -> address =
    lambda (_uunit)address return ([%Michelson {| { DROP ; SOURCE } |}])@(unit)[@inline][@hidden] in
  let get_level#13unit -> nat =
    lambda (_uunit)nat return ([%Michelson {| { DROP ; LEVEL } |}])@(unit)[@inline][@hidden] in
  let get_self_address#14unit -> address =
    lambda (_uunit)address return SELF_ADDRESS()[@inline][@hidden] in
  let get_chain_id#15unit -> chain_id =
    lambda (_uunit)chain_id return ([%Michelson {| { DROP ; CHAIN_ID } |}])@(unit)[@inline][@hidden] in
  let get_total_voting_power#16unit -> nat =
    lambda (_uunit)nat return ([%Michelson {| { DROP ; TOTAL_VOTING_POWER } |}])@(unit)[@inline][@hidden] in
  let get_min_block_time#17unit -> nat =
    lambda (_uunit)nat return ([%Michelson {| { DROP; MIN_BLOCK_TIME } |}])@(unit)[@inline][@hidden] in
  let voting_power#18key_hash -> nat =
    lambda (khkey_hash)nat return ([%Michelson {| { VOTING_POWER } |}])@(kh)[@inline][@hidden] in
  let address#19funtype a : * . contract (a) -> address = Λ a ->
  lambda (ccontract (a))address return ADDRESS(c)[@inline][@hidden] in
  let implicit_account#20key_hash -> contract (unit) =
    lambda (khkey_hash)contract (unit) return IMPLICIT_ACCOUNT(kh)[@inline][@hidden] in
  let join_tickets#21funtype a : * . ( ticket (a) * ticket (a) ) -> option (ticket (a)) =
    Λ a ->
  lambda (t( ticket (a) * ticket (a) ))option (ticket (a)) return ([%Michelson {| { JOIN_TICKETS } |}])@(t)[@inline][@hidden] in
  let read_ticket#22funtype a : * . ticket (a) -> ( ( address * ( a * nat ) ) * ticket (a) ) =
    Λ a ->
  lambda (tticket (a))( ( address * ( a * nat ) ) * ticket (a) ) return
  ([%Michelson {| { READ_TICKET ; PAIR } |}])@(t)[@inline][@hidden] in
  let never#23funtype a : * . never -> a = Λ a ->
  lambda (nnever)a return ([%Michelson {| { NEVER } |}])@(n)[@inline][@hidden] in
  let pairing_check#24list (( bls12_381_g1 * bls12_381_g2 )) -> bool =
    lambda (llist (( bls12_381_g1 * bls12_381_g2 )))bool return ([%Michelson {| { PAIRING_CHECK } |}])@(l)[@inline][@hidden] in
  let constant#25funtype a : * . string -> a = Λ a ->
  lambda (sstring)a return GLOBAL_CONSTANT(s)[@inline][@hidden] in
  let set_delegate#26option (key_hash) -> operation =
    lambda (ooption (key_hash))operation return SET_DELEGATE(o)[@inline][@hidden] in
  let get_contract#27funtype a : * . address -> contract (a) = Λ a ->
  lambda (aaddress)contract (a) return CONTRACT(a)[@inline][@hidden] in
  let get_contract_opt#28funtype a : * . address -> option (contract (a)) =
    Λ a ->
  lambda (aaddress)option (contract (a)) return CONTRACT_OPT(a)[@inline][@hidden] in
  let get_contract_with_error#29funtype a : * . address -> string -> contract (a) =
    Λ a ->
  lambda (aaddress)string -> contract (a) return lambda (sstring)contract (a) return
  CONTRACT_WITH_ERROR(a , s)[@inline][@hidden] in
  let create_ticket#30funtype a : * . a -> nat -> ticket (a) = Λ a ->
  lambda (va)nat -> ticket (a) return lambda (nnat)ticket (a) return ([%Michelson {| { UNPAIR ; TICKET } |}])@(
                                                                     ( v ,
                                                                      n ))[@inline][@hidden] in
  let transaction#31funtype a : * . a -> tez -> contract (a) -> operation =
    Λ a ->
  lambda (aa)tez -> contract (a) -> operation return lambda (mutez)contract (a) -> operation return lambda (ccontract (a))operation return
  CALL(a , mu , c)[@inline][@hidden] in
  let open_chest#32chest_key -> chest -> nat -> sum[Fail_decrypt -> unit , Fail_timelock -> unit , Ok_opening -> bytes] =
    lambda (ckchest_key)chest -> nat -> sum[Fail_decrypt -> unit , Fail_timelock -> unit , Ok_opening -> bytes] return lambda (cchest)nat ->
  sum[Fail_decrypt -> unit , Fail_timelock -> unit , Ok_opening -> bytes] return lambda (nnat)
  sum[Fail_decrypt -> unit , Fail_timelock -> unit , Ok_opening -> bytes] return
  OPEN_CHEST(ck , c , n)[@inline][@hidden] in
  let call_view#33funtype a : * . funtype b : * . string -> a -> address -> option (b) =
    Λ a ->
  Λ b ->
  lambda (sstring)a -> address -> option (b) return lambda (xa)address -> option (b) return lambda (aaddress)option (b) return
  VIEW(s , x , a)[@inline][@hidden] in
  let split_ticket#34funtype a : * . ticket (a) -> ( nat * nat ) -> option (
  ( ticket (a) * ticket (a) )) = Λ a ->
  lambda (tticket (a))( nat * nat ) -> option (( ticket (a) * ticket (a) )) return lambda (p
  ( nat * nat ))option (( ticket (a) * ticket (a) )) return ([%Michelson {| { UNPAIR ; SPLIT_TICKET } |}])@(
                                                            ( t ,
                                                              p ))[@inline][@hidden] in
  let xor#35nat -> nat -> nat =
    lambda (lnat)nat -> nat return lambda (rnat)nat return XOR(l , r)[@inline][@hidden] in
  let shift_left#36nat -> nat -> nat =
    lambda (lnat)nat -> nat return lambda (rnat)nat return LSL(l , r)[@inline][@hidden] in
  let shift_right#37nat -> nat -> nat =
    lambda (lnat)nat -> nat return lambda (rnat)nat return LSR(l , r)[@inline][@hidden] in
  let empty#38funtype k : * . funtype v : * . big_map (k , v) = Λ k ->
  Λ v ->  BIG_MAP_EMPTY()[@inline][@hidden] in
  let mem#39funtype k : * . funtype v : * . k -> big_map (k , v) -> bool =
    Λ k ->
  Λ v ->
  lambda (kk)big_map (k , v) -> bool return lambda (mbig_map (k ,
  v))bool return ([%Michelson {| { UNPAIR ; MEM } |}])@(( k , m ))[@inline][@hidden] in
  let add#40funtype k : * . funtype v : * . k -> v -> big_map (k ,
  v) -> big_map (k , v) = Λ k ->
  Λ v ->
  lambda (kk)v -> big_map (k , v) -> big_map (k ,
  v) return lambda (vv)big_map (k , v) -> big_map (k ,
  v) return lambda (mbig_map (k , v))big_map (k , v) return MAP_ADD(k , v , m)[@inline][@hidden] in
  let remove#41funtype k : * . funtype v : * . k -> big_map (k ,
  v) -> big_map (k , v) = Λ k ->
  Λ v ->
  lambda (kk)big_map (k , v) -> big_map (k , v) return lambda (mbig_map (k ,
  v))big_map (k , v) return MAP_REMOVE(k , m)[@inline][@hidden] in
  let update#42funtype k : * . funtype v : * . k -> option (v) -> big_map (k ,
  v) -> big_map (k , v) = Λ k ->
  Λ v ->
  lambda (kk)option (v) -> big_map (k , v) -> big_map (k ,
  v) return lambda (voption (v))big_map (k , v) -> big_map (k ,
  v) return lambda (mbig_map (k , v))big_map (k ,
  v) return MAP_UPDATE(k , v , m)[@inline][@hidden] in
  let get_and_update#43funtype k : * . funtype v : * . k -> option (v) -> big_map (k ,
  v) -> ( option (v) * big_map (k , v) ) = Λ k ->
  Λ v ->
  lambda (kk)option (v) -> big_map (k ,
  v) -> ( option (v) * big_map (k , v) ) return lambda (voption (v))big_map (k ,
  v) -> ( option (v) * big_map (k , v) ) return lambda (mbig_map (k ,
  v))( option (v) * big_map (k , v) ) return BIG_MAP_GET_AND_UPDATE(k , v , m)[@inline][@hidden] in
  let find_opt#44funtype k : * . funtype v : * . k -> big_map (k ,
  v) -> option (v) = Λ k ->
  Λ v ->
  lambda (kk)big_map (k , v) -> option (v) return lambda (mbig_map (k ,
  v))option (v) return MAP_FIND_OPT(k , m)[@inline][@hidden] in
  let find#45funtype k : * . funtype v : * . k -> big_map (k , v) -> v =
    Λ k ->
  Λ v ->
  lambda (kk)big_map (k , v) -> v return lambda (mbig_map (k ,
  v))v return MAP_FIND(k , m)[@inline][@hidden] in
  let empty#46funtype k : * . funtype v : * . map (k , v) = Λ k ->
  Λ v ->  MAP_EMPTY()[@inline][@hidden] in
  let size#47funtype k : * . funtype v : * . map (k , v) -> nat = Λ k ->
  Λ v ->  lambda (mmap (k , v))nat return ([%Michelson {| { SIZE } |}])@(m)[@inline][@hidden] in
  let mem#48funtype k : * . funtype v : * . k -> map (k , v) -> bool = Λ k ->
  Λ v ->
  lambda (kk)map (k , v) -> bool return lambda (mmap (k ,
  v))bool return ([%Michelson {| { UNPAIR ; MEM } |}])@(( k , m ))[@inline][@hidden] in
  let add#49funtype k : * . funtype v : * . k -> v -> map (k , v) -> map (k ,
  v) = Λ k ->
  Λ v ->
  lambda (kk)v -> map (k , v) -> map (k , v) return lambda (vv)map (k ,
  v) -> map (k , v) return lambda (mmap (k , v))map (k ,
  v) return MAP_ADD(k , v , m)[@inline][@hidden] in
  let remove#50funtype k : * . funtype v : * . k -> map (k , v) -> map (k ,
  v) = Λ k ->
  Λ v ->
  lambda (kk)map (k , v) -> map (k , v) return lambda (mmap (k , v))map (k ,
  v) return MAP_REMOVE(k , m)[@inline][@hidden] in
  let update#51funtype k : * . funtype v : * . k -> option (v) -> map (k ,
  v) -> map (k , v) = Λ k ->
  Λ v ->
  lambda (kk)option (v) -> map (k , v) -> map (k ,
  v) return lambda (voption (v))map (k , v) -> map (k ,
  v) return lambda (mmap (k , v))map (k , v) return MAP_UPDATE(k , v , m)[@inline][@hidden] in
  let get_and_update#52funtype k : * . funtype v : * . k -> option (v) -> map (k ,
  v) -> ( option (v) * map (k , v) ) = Λ k ->
  Λ v ->
  lambda (kk)option (v) -> map (k ,
  v) -> ( option (v) * map (k , v) ) return lambda (voption (v))map (k ,
  v) -> ( option (v) * map (k , v) ) return lambda (mmap (k ,
  v))( option (v) * map (k , v) ) return MAP_GET_AND_UPDATE(k , v , m)[@inline][@hidden] in
  let find#53funtype k : * . funtype v : * . k -> map (k , v) -> v = Λ k ->
  Λ v ->
  lambda (kk)map (k , v) -> v return lambda (mmap (k ,
  v))v return MAP_FIND(k , m)[@inline][@hidden] in
  let find_opt#54funtype k : * . funtype v : * . k -> map (k ,
  v) -> option (v) = Λ k ->
  Λ v ->
  lambda (kk)map (k , v) -> option (v) return lambda (mmap (k ,
  v))option (v) return MAP_FIND_OPT(k , m)[@inline][@hidden] in
  let iter#55funtype k : * . funtype v : * . ( k * v ) -> unit -> map (k ,
  v) -> unit = Λ k ->
  Λ v ->
  lambda (f( k * v ) -> unit)map (k , v) -> unit return lambda (mmap (k ,
  v))unit return MAP_ITER(f , m)[@inline][@hidden] in
  let map#56funtype k : * . funtype v : * . funtype w : * . ( k * v ) -> w -> map (k ,
  v) -> map (k , w) = Λ k ->
  Λ v ->
  Λ w ->
  lambda (f( k * v ) -> w)map (k , v) -> map (k , w) return lambda (mmap (k ,
  v))map (k , w) return MAP_MAP(f , m)[@inline][@hidden] in
  let fold#57funtype k : * . funtype v : * . funtype c : * . ( c * ( k * v ) ) -> c -> map (k ,
  v) -> c -> c = Λ k ->
  Λ v ->
  Λ c ->
  lambda (f( c * ( k * v ) ) -> c)map (k ,
  v) -> c -> c return lambda (mmap (k ,
  v))c -> c return lambda (ic)c return MAP_FOLD(f , m , i)[@inline][@hidden] in
  let empty#58funtype a : * . set (a) = Λ a ->
  SET_EMPTY()[@inline][@hidden] in
  let size#59funtype a : * . set (a) -> nat = Λ a ->
  lambda (sset (a))nat return ([%Michelson {| { SIZE } |}])@(s)[@inline][@hidden] in
  let cardinal#60funtype a : * . set (a) -> nat = Λ a ->
  lambda (sset (a))nat return ([%Michelson {| { SIZE } |}])@(s)[@inline][@hidden] in
  let mem#61funtype a : * . a -> set (a) -> bool = Λ a ->
  lambda (xa)set (a) -> bool return lambda (sset (a))bool return SET_MEM(x , s)[@inline][@hidden] in
  let add#62funtype a : * . a -> set (a) -> set (a) = Λ a ->
  lambda (xa)set (a) -> set (a) return lambda (sset (a))set (a) return
  SET_ADD(x , s)[@inline][@hidden] in
  let remove#63funtype a : * . a -> set (a) -> set (a) = Λ a ->
  lambda (xa)set (a) -> set (a) return lambda (sset (a))set (a) return
  SET_REMOVE(x , s)[@inline][@hidden] in
  let update#64funtype a : * . a -> bool -> set (a) -> set (a) = Λ a ->
  lambda (xa)bool -> set (a) -> set (a) return lambda (bbool)set (a) -> set (a) return lambda (sset (a))set (a) return
  SET_UPDATE(x , b , s)[@inline][@hidden] in
  let iter#65funtype a : * . a -> unit -> set (a) -> unit = Λ a ->
  lambda (fa -> unit)set (a) -> unit return lambda (sset (a))unit return
  SET_ITER(f , s)[@inline][@hidden] in
  let fold#66funtype a : * . funtype b : * . ( b * a ) -> b -> set (a) -> b -> b =
    Λ a ->
  Λ b ->
  lambda (f( b * a ) -> b)set (a) -> b -> b return lambda (sset (a))b -> b return lambda (ib)b return
  SET_FOLD(f , s , i)[@inline][@hidden] in
  let fold_desc#67funtype a : * . funtype b : * . ( a * b ) -> b -> set (a) -> b -> b =
    Λ a ->
  Λ b ->
  lambda (f( a * b ) -> b)set (a) -> b -> b return lambda (sset (a))b -> b return lambda (ib)b return
  SET_FOLD_DESC(f , s , i)[@inline][@hidden] in
  let length#68funtype a : * . list (a) -> nat = Λ a ->
  lambda (xslist (a))nat return ([%Michelson {| { SIZE } |}])@(xs)[@inline][@hidden] in
  let size#69funtype a : * . list (a) -> nat = Λ a ->
  lambda (xslist (a))nat return ([%Michelson {| { SIZE } |}])@(xs)[@inline][@hidden] in
  let head_opt#70funtype a : * . list (a) -> option (a) = Λ a ->
  lambda (xslist (a))option (a) return  match xs with
                                         | Cons ctor_proj#2 ->
                                            match ctor_proj#2 with
                                             | ( x , _#2 ) ->
                                             Some(x)
                                         | Nil unit_proj#4 ->
                                           None(unit)[@inline][@hidden] in
  let tail_opt#71funtype a : * . list (a) -> option (list (a)) = Λ a ->
  lambda (xslist (a))option (list (a)) return  match xs with
                                                | Cons ctor_proj#5 ->
                                                   match ctor_proj#5 with
                                                    | ( _#3 , xs ) ->
                                                    Some(xs)
                                                | Nil unit_proj#7 ->
                                                  None(unit)[@inline][@hidden] in
  let map#72funtype a : * . funtype b : * . a -> b -> list (a) -> list (b) =
    Λ a ->
  Λ b ->
  lambda (fa -> b)list (a) -> list (b) return lambda (xslist (a))list (b) return
  LIST_MAP(f , xs)[@inline][@hidden] in
  let iter#73funtype a : * . a -> unit -> list (a) -> unit = Λ a ->
  lambda (fa -> unit)list (a) -> unit return lambda (xslist (a))unit return
  LIST_ITER(f , xs)[@inline][@hidden] in
  let fold#74funtype a : * . funtype b : * . ( b * a ) -> b -> list (a) -> b -> b =
    Λ a ->
  Λ b ->
  lambda (f( b * a ) -> b)list (a) -> b -> b return lambda (xslist (a))b -> b return lambda (ib)b return
  LIST_FOLD(f , xs , i)[@inline][@hidden] in
  let fold_left#75funtype a : * . funtype b : * . ( b * a ) -> b -> b -> list (a) -> b =
    Λ a ->
  Λ b ->
  lambda (f( b * a ) -> b)b -> list (a) -> b return lambda (ib)list (a) -> b return lambda (xslist (a))b return
  LIST_FOLD_LEFT(f , i , xs)[@inline][@hidden] in
  let fold_right#76funtype a : * . funtype b : * . ( a * b ) -> b -> list (a) -> b -> b =
    Λ a ->
  Λ b ->
  lambda (f( a * b ) -> b)list (a) -> b -> b return lambda (xslist (a))b -> b return lambda (ib)b return
  LIST_FOLD_RIGHT(f , xs , i)[@inline][@hidden] in
  let cons#77funtype a : * . a -> list (a) -> list (a) = Λ a ->
  lambda (xa)list (a) -> list (a) return lambda (xslist (a))list (a) return
  CONS(x , xs)[@inline][@hidden] in
  let length#78string -> nat =
    lambda (bstring)nat return ([%Michelson {| { SIZE } |}])@(b)[@inline][@hidden] in
  let concat#79string -> string -> string =
    lambda (b1string)string -> string return lambda (b2string)string return
  ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b1 , b2 ))[@inline][@hidden] in
  let sub#80nat -> nat -> string -> string =
    lambda (snat)nat -> string -> string return lambda (lnat)string -> string return lambda (bstring)string return
  ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(
  ( s ,
    l ,
    b ))[@inline][@hidden] in
  let unopt#81funtype a : * . option (a) -> a = Λ a ->
  lambda (voption (a))a return UNOPT(v)[@inline][@hidden] in
  let unopt_with_error#82funtype a : * . option (a) -> string -> a = Λ a ->
  lambda (voption (a))string -> a return lambda (sstring)a return UNOPT_WITH_ERROR
                                                                  (v ,
                                                                   s)[@inline][@hidden] in
  let pack#83funtype a : * . a -> bytes = Λ a ->
  lambda (va)bytes return ([%Michelson {| { PACK } |}])@(v)[@inline][@hidden] in
  let unpack#84funtype a : * . bytes -> option (a) = Λ a ->
  lambda (bbytes)option (a) return BYTES_UNPACK(b)[@inline][@hidden] in
  let length#85bytes -> nat =
    lambda (bbytes)nat return ([%Michelson {| { SIZE } |}])@(b)[@inline][@hidden] in
  let concat#86bytes -> bytes -> bytes =
    lambda (b1bytes)bytes -> bytes return lambda (b2bytes)bytes return
  ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b1 , b2 ))[@inline][@hidden] in
  let sub#87nat -> nat -> bytes -> bytes =
    lambda (snat)nat -> bytes -> bytes return lambda (lnat)bytes -> bytes return lambda (bbytes)bytes return
  ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(
  ( s ,
    l ,
    b ))[@inline][@hidden] in
  let blake2b#88bytes -> bytes =
    lambda (bbytes)bytes return ([%Michelson {| { BLAKE2B } |}])@(b)[@inline][@hidden] in
  let sha256#89bytes -> bytes =
    lambda (bbytes)bytes return ([%Michelson {| { SHA256 } |}])@(b)[@inline][@hidden] in
  let sha512#90bytes -> bytes =
    lambda (bbytes)bytes return ([%Michelson {| { SHA512 } |}])@(b)[@inline][@hidden] in
  let sha3#91bytes -> bytes =
    lambda (bbytes)bytes return ([%Michelson {| { SHA3 } |}])@(b)[@inline][@hidden] in
  let keccak#92bytes -> bytes =
    lambda (bbytes)bytes return ([%Michelson {| { KECCAK } |}])@(b)[@inline][@hidden] in
  let hash_key#93key -> key_hash =
    lambda (kkey)key_hash return ([%Michelson {| { HASH_KEY } |}])@(k)[@inline][@hidden] in
  let check#94key -> signature -> bytes -> bool =
    lambda (kkey)signature -> bytes -> bool return lambda (ssignature)bytes -> bool return lambda (bbytes)bool return
  ([%Michelson {| { UNPAIR ; UNPAIR ; CHECK_SIGNATURE } |}])@(( k , s , b ))[@inline][@hidden] in
  let assertbool -> unit =
    lambda (bbool)unit return ([%Michelson {| { IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } } |}])@(b)[@inline][@private][@hidden] in
  let assert_somefuntype a : * . option (a) -> unit = Λ a ->
  lambda (voption (a))unit return ([%Michelson {| { IF_NONE { PUSH string "failed assert some" ; FAILWITH } { DROP ; UNIT } } |}])@(v)[@inline][@private][@hidden] in
  let assert_nonefuntype a : * . option (a) -> unit = Λ a ->
  lambda (voption (a))unit return ([%Michelson {| { IF_NONE { UNIT } { PUSH string "failed assert none" ; FAILWITH } } |}])@(v)[@inline][@private][@hidden] in
  let absint -> nat =
    lambda (iint)nat return ([%Michelson {| { ABS } |}])@(i)[@inline][@private][@hidden] in
  let is_natint -> option (nat) =
    lambda (iint)option (nat) return ([%Michelson {| { ISNAT } |}])@(i)[@inline][@private][@hidden] in
  let truebool = TRUE()[@inline][@private][@hidden] in
  let falsebool = FALSE()[@inline][@private][@hidden] in
  let unitunit = UNIT()[@inline][@private][@hidden] in
  let failwithfuntype a : * . funtype b : * . a -> b = Λ a ->
  Λ b ->  [%Michelson {|{ FAILWITH }|}][@inline][@private][@hidden] in
  let intfuntype a : * . a -> external_int (a) = Λ a ->
  lambda (va)external_int (a) return ([%Michelson {| { INT } |}])@(v)[@inline][@private][@hidden] in
  let assert_with_errorbool -> string -> unit =
    lambda (bbool)string -> unit return lambda (sstring)unit return ([%Michelson {| { UNPAIR ; IF { DROP ; UNIT } { FAILWITH } } |}])@(
                                                                    ( b ,
                                                                      s ))[@inline][@private][@hidden] in
  let assert_some_with_errorfuntype a : * . option (a) -> string -> unit =
    Λ a ->
  lambda (voption (a))string -> unit return lambda (sstring)unit return
  ([%Michelson {| { UNPAIR ; IF_NONE { FAILWITH } { DROP 2 ; UNIT } } |}])@(
  ( v ,
    s ))[@inline][@private][@hidden] in
  let assert_none_with_errorfuntype a : * . option (a) -> string -> unit =
    Λ a ->
  lambda (voption (a))string -> unit return lambda (sstring)unit return
  ([%Michelson {| { UNPAIR ; IF_NONE { DROP ; UNIT } { DROP ; FAILWITH } } |}])@(
  ( v ,
    s ))[@inline][@private][@hidden] in
  let edivfuntype a : * . funtype b : * . a -> b -> external_ediv (a , b) =
    Λ a ->
  Λ b ->
  lambda (la)b -> external_ediv (a , b) return lambda (rb)external_ediv (a ,
  b) return ([%Michelson {| { UNPAIR ; EDIV } |}])@(( l , r ))[@inline][@private][@hidden] in
  let stub#95funtype a : * . funtype b : * . a -> b = Λ a ->
  Λ b ->  lambda (xa)b return ([%Michelson {|{ FAILWITH }|}])@(x)[@inline][@private][@hidden] in
  let run#96funtype a : * . funtype b : * . a -> b -> a -> unit = Λ a ->
  Λ b ->
  lambda (_fa -> b)a -> unit return lambda (_va)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let eval#97funtype a : * . a -> unit = Λ a ->
  lambda (_xa)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let compile_value#98funtype a : * . a -> unit = Λ a ->
  lambda (_xa)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let get_total_voting_power#99unit -> nat =
    lambda (_uunit)nat return (stub#95@{unit}@{nat})@(unit)[@inline][@hidden] in
  let failwith#100funtype a : * . funtype b : * . a -> b = Λ a ->
  Λ b ->  lambda (_va)b return (stub#95@{unit}@{b})@(unit)[@inline][@hidden] in
  let to_contract#101funtype p : * . funtype s : * . unit -> contract (p) =
    Λ p ->
  Λ s ->
  lambda (_tunit)contract (p) return (stub#95@{unit}@{contract (p)})@(unit)[@inline][@hidden] in
  let set_source#102address -> unit =
    lambda (_aaddress)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let get_storage_of_address#103address -> unit =
    lambda (_aaddress)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let get_balance#104address -> tez =
    lambda (_aaddress)tez return (stub#95@{unit}@{tez})@(unit)[@inline][@hidden] in
  let print#105string -> unit =
    lambda (_vstring)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let eprint#106string -> unit =
    lambda (_vstring)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let get_voting_power#107key_hash -> nat =
    lambda (_khkey_hash)nat return (stub#95@{unit}@{nat})@(unit)[@inline][@hidden] in
  let nth_bootstrap_contract#108nat -> address =
    lambda (_inat)address return (stub#95@{unit}@{address})@(unit)[@inline][@hidden] in
  let nth_bootstrap_account#109int -> address =
    lambda (_iint)address return (stub#95@{unit}@{address})@(unit)[@inline][@hidden] in
  let get_bootstrap_account#110nat -> ( address * key * string ) =
    lambda (_nnat)( address * key * string ) return (stub#95@{unit}@{( address * key * string )})@(unit)[@inline][@hidden] in
  let nth_bootstrap_typed_address#111funtype a : * . funtype b : * . nat -> unit =
    Λ a ->
  Λ b ->  lambda (_nnat)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let last_originations#112unit -> map (address , list (address)) =
    lambda (_uunit)map (address ,
  list (address)) return (stub#95@{unit}@{map (address ,
                         list (address))})@(unit)[@inline][@hidden] in
  let random#113funtype a : * . unit -> a = Λ a ->
  lambda (_uunit)a return (stub#95@{unit}@{a})@(unit)[@inline][@hidden] in
  let new_account#114unit -> ( string * key ) =
    lambda (_uunit)( string * key ) return (stub#95@{unit}@{( string * key )})@(unit)[@inline][@hidden] in
  let decompile#115funtype a : * . unit -> a = Λ a ->
  lambda (_munit)a return (stub#95@{unit}@{a})@(unit)[@inline][@hidden] in
  let bake_until_n_cycle_end#116nat -> unit =
    lambda (_nnat)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let cast_address#117funtype a : * . funtype b : * . address -> unit =
    Λ a ->
  Λ b ->  lambda (_aaddress)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let register_delegate#118key_hash -> unit =
    lambda (_khkey_hash)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let register_constant#119unit -> string =
    lambda (_munit)string return (stub#95@{unit}@{string})@(unit)[@inline][@hidden] in
  let to_typed_address#120funtype a : * . funtype b : * . contract (a) -> unit =
    Λ a ->
  Λ b ->  lambda (_ccontract (a))unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let constant_to_michelson_program#121string -> unit =
    lambda (_sstring)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let restore_context#122unit -> unit =
    lambda (_uunit)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let save_context#123unit -> unit =
    lambda (_uunit)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let drop_context#124unit -> unit =
    lambda (_uunit)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let to_string#125funtype a : * . a -> string = Λ a ->
  lambda (_va)string return (stub#95@{unit}@{string})@(unit)[@inline][@hidden] in
  let get_storage#126funtype p : * . funtype s : * . unit -> s = Λ p ->
  Λ s ->  lambda (_tunit)s return (stub#95@{unit}@{s})@(unit)[@inline][@hidden] in
  let set_baker_policy#127unit -> unit =
    lambda (_bpunit)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let set_baker#128address -> unit =
    lambda (_aaddress)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let size#129unit -> int =
    lambda (_cunit)int return (stub#95@{unit}@{int})@(unit)[@inline][@hidden] in
  let compile_contract#130funtype p : * . funtype s : * . ( p * s ) ->
  ( list (operation) * s ) -> unit = Λ p ->
  Λ s ->
  lambda (_f( p * s ) -> ( list (operation) * s ))unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let read_contract_from_file#131string -> unit =
    lambda (_fnstring)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let chr#132nat -> option (string) =
    lambda (_nnat)option (string) return (stub#95@{unit}@{option (string)})@(unit)[@inline][@hidden] in
  let nl#133string = "NEWLINE"[@inline][@hidden] in
  let println#134string -> unit =
    lambda (_vstring)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let transfer#135address -> unit -> tez -> unit =
    lambda (_aaddress)unit -> tez -> unit return lambda (_sunit)tez -> unit return lambda (_ttez)unit return
  (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let transfer_exn#136address -> unit -> tez -> nat =
    lambda (_aaddress)unit -> tez -> nat return lambda (_sunit)tez -> nat return lambda (_ttez)nat return
  (stub#95@{unit}@{nat})@(unit)[@inline][@hidden] in
  let log#137funtype a : * . a -> unit = Λ a ->
  lambda (_va)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let reset_state#138nat -> list (tez) -> unit =
    lambda (_nnat)list (tez) -> unit return lambda (_llist (tez))unit return
  (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let reset_state_at#139timestamp -> nat -> list (tez) -> unit =
    lambda (_ttimestamp)nat -> list (tez) -> unit return lambda (_nnat)list (tez) -> unit return lambda (_llist (tez))unit return
  (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let bootstrap_contract#140funtype p : * . funtype s : * . ( p * s ) ->
  ( list (operation) * s ) -> s -> tez -> unit = Λ p ->
  Λ s ->
  lambda (_f( p * s ) -> ( list (operation) * s ))s -> tez -> unit return lambda (_ss)tez -> unit return lambda (_ttez)unit return
  (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let mutate_value#141funtype a : * . nat -> a -> option (( a * unit )) =
    Λ a ->
  lambda (_nnat)a -> option (( a * unit )) return lambda (_va)option (( a * unit )) return
  (stub#95@{unit}@{option (( a * unit ))})@(unit)[@inline][@hidden] in
  let save_mutation#142string -> unit -> option (string) =
    lambda (_sstring)unit -> option (string) return lambda (_munit)option (string) return
  (stub#95@{unit}@{option (string)})@(unit)[@inline][@hidden] in
  let mutation_test#143funtype a : * . funtype b : * . a -> a -> b -> option (
  ( b * unit )) = Λ a ->
  Λ b ->
  lambda (_va)a -> b -> option (( b * unit )) return lambda (_fa -> b)option (
  ( b * unit )) return (stub#95@{unit}@{option (( b * unit ))})@(unit)[@inline][@hidden] in
  let mutation_test_all#144funtype a : * . funtype b : * . a -> a -> b -> list (
  ( b * unit )) = Λ a ->
  Λ b ->
  lambda (_va)a -> b -> list (( b * unit )) return lambda (_fa -> b)list (
  ( b * unit )) return (stub#95@{unit}@{list (( b * unit ))})@(unit)[@inline][@hidden] in
  let sign#145string -> bytes -> signature =
    lambda (_skstring)bytes -> signature return lambda (_dbytes)signature return
  (stub#95@{unit}@{signature})@(unit)[@inline][@hidden] in
  let add_account#146string -> key -> unit =
    lambda (_sstring)key -> unit return lambda (_kkey)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let baker_account#147( string * key ) -> option (tez) -> unit =
    lambda (_p( string * key ))option (tez) -> unit return lambda (_ooption (tez))unit return
  (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let set_big_map#148funtype a : * . funtype b : * . int -> big_map (a ,
  b) -> unit = Λ a ->
  Λ b ->
  lambda (_iint)big_map (a , b) -> unit return lambda (_mbig_map (a ,
  b))unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let create_chest#149bytes -> nat -> ( chest * chest_key ) =
    lambda (_bbytes)nat -> ( chest * chest_key ) return lambda (_nnat)( chest * chest_key ) return
  (stub#95@{unit}@{( chest * chest_key )})@(unit)[@inline][@hidden] in
  let create_chest_key#150chest -> nat -> chest_key =
    lambda (_cchest)nat -> chest_key return lambda (_nnat)chest_key return
  (stub#95@{unit}@{chest_key})@(unit)[@inline][@hidden] in
  let transfer_to_contract#151funtype p : * . contract (p) -> p -> tez -> unit =
    Λ p ->
  lambda (_ccontract (p))p -> tez -> unit return lambda (_sp)tez -> unit return lambda (_ttez)unit return
  (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let transfer_to_contract_exn#152funtype p : * . contract (p) -> p -> tez -> nat =
    Λ p ->
  lambda (_ccontract (p))p -> tez -> nat return lambda (_sp)tez -> nat return lambda (_ttez)nat return
  (stub#95@{unit}@{nat})@(unit)[@inline][@hidden] in
  let michelson_equal#153unit -> unit -> bool =
    lambda (_m1unit)unit -> bool return lambda (_m2unit)bool return (stub#95@{unit}@{bool})@(unit)[@inline][@hidden] in
  let to_entrypoint#154funtype a : * . funtype b : * . funtype c : * . string -> unit -> contract (c) =
    Λ a ->
  Λ b ->
  Λ c ->
  lambda (_sstring)unit -> contract (c) return lambda (_tunit)contract (c) return
  (stub#95@{unit}@{contract (c)})@(unit)[@inline][@hidden] in
  let originate_contract#155unit -> unit -> tez -> address =
    lambda (_cunit)unit -> tez -> address return lambda (_sunit)tez -> address return lambda (_ttez)address return
  (stub#95@{unit}@{address})@(unit)[@inline][@hidden] in
  let originate#156funtype p : * . funtype s : * . ( p * s ) -> ( list (operation) * s ) -> s -> tez ->
  ( unit * unit * int ) = Λ p ->
  Λ s ->
  lambda (_f( p * s ) -> ( list (operation) * s ))s -> tez -> ( unit * unit * int ) return lambda (_ss)tez ->
  ( unit * unit * int ) return lambda (_ttez)( unit * unit * int ) return
  (stub#95@{unit}@{( unit * unit * int )})@(unit)[@inline][@hidden] in
  let compile_contract_from_file#157string -> string -> list (string) -> unit =
    lambda (_fnstring)string -> list (string) -> unit return lambda (_estring)list (string) -> unit return lambda (_vlist (string))unit return
  (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let originate_from_file#158string -> string -> list (string) -> unit -> tez ->
  ( address * unit * int ) =
    lambda (_fnstring)string -> list (string) -> unit -> tez -> ( address * unit * int ) return lambda (_estring)list (string) -> unit -> tez ->
  ( address * unit * int ) return lambda (_vlist (string))unit -> tez ->
  ( address * unit * int ) return lambda (_sunit)tez -> ( address * unit * int ) return lambda (_ttez)
  ( address * unit * int ) return (stub#95@{unit}@{( address * unit * int )})@(unit)[@inline][@hidden] in
  let x#159int = 1 in
  let fooint =
    let xint = 20 in
    let x#160int = x in
    let y#161int = x#159 in
    let z#162int = y#161 in ADD(ADD(ADD(x#160 , y#161) , x) , z#162) in
  let xint = foo in unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias6.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias7.mligo" ] ;
  [%expect {xxx|
  let get_balance#8unit -> tez =
    lambda (_uunit)tez return ([%Michelson {| { DROP ; BALANCE } |}])@(unit)[@inline][@hidden] in
  let get_amount#9unit -> tez =
    lambda (_uunit)tez return ([%Michelson {| { DROP ; AMOUNT } |}])@(unit)[@inline][@hidden] in
  let get_now#10unit -> timestamp =
    lambda (_uunit)timestamp return ([%Michelson {| { DROP ; NOW } |}])@(unit)[@inline][@hidden] in
  let get_sender#11unit -> address =
    lambda (_uunit)address return ([%Michelson {| { DROP ; SENDER } |}])@(unit)[@inline][@hidden] in
  let get_source#12unit -> address =
    lambda (_uunit)address return ([%Michelson {| { DROP ; SOURCE } |}])@(unit)[@inline][@hidden] in
  let get_level#13unit -> nat =
    lambda (_uunit)nat return ([%Michelson {| { DROP ; LEVEL } |}])@(unit)[@inline][@hidden] in
  let get_self_address#14unit -> address =
    lambda (_uunit)address return SELF_ADDRESS()[@inline][@hidden] in
  let get_chain_id#15unit -> chain_id =
    lambda (_uunit)chain_id return ([%Michelson {| { DROP ; CHAIN_ID } |}])@(unit)[@inline][@hidden] in
  let get_total_voting_power#16unit -> nat =
    lambda (_uunit)nat return ([%Michelson {| { DROP ; TOTAL_VOTING_POWER } |}])@(unit)[@inline][@hidden] in
  let get_min_block_time#17unit -> nat =
    lambda (_uunit)nat return ([%Michelson {| { DROP; MIN_BLOCK_TIME } |}])@(unit)[@inline][@hidden] in
  let voting_power#18key_hash -> nat =
    lambda (khkey_hash)nat return ([%Michelson {| { VOTING_POWER } |}])@(kh)[@inline][@hidden] in
  let address#19funtype a : * . contract (a) -> address = Λ a ->
  lambda (ccontract (a))address return ADDRESS(c)[@inline][@hidden] in
  let implicit_account#20key_hash -> contract (unit) =
    lambda (khkey_hash)contract (unit) return IMPLICIT_ACCOUNT(kh)[@inline][@hidden] in
  let join_tickets#21funtype a : * . ( ticket (a) * ticket (a) ) -> option (ticket (a)) =
    Λ a ->
  lambda (t( ticket (a) * ticket (a) ))option (ticket (a)) return ([%Michelson {| { JOIN_TICKETS } |}])@(t)[@inline][@hidden] in
  let read_ticket#22funtype a : * . ticket (a) -> ( ( address * ( a * nat ) ) * ticket (a) ) =
    Λ a ->
  lambda (tticket (a))( ( address * ( a * nat ) ) * ticket (a) ) return
  ([%Michelson {| { READ_TICKET ; PAIR } |}])@(t)[@inline][@hidden] in
  let never#23funtype a : * . never -> a = Λ a ->
  lambda (nnever)a return ([%Michelson {| { NEVER } |}])@(n)[@inline][@hidden] in
  let pairing_check#24list (( bls12_381_g1 * bls12_381_g2 )) -> bool =
    lambda (llist (( bls12_381_g1 * bls12_381_g2 )))bool return ([%Michelson {| { PAIRING_CHECK } |}])@(l)[@inline][@hidden] in
  let constant#25funtype a : * . string -> a = Λ a ->
  lambda (sstring)a return GLOBAL_CONSTANT(s)[@inline][@hidden] in
  let set_delegate#26option (key_hash) -> operation =
    lambda (ooption (key_hash))operation return SET_DELEGATE(o)[@inline][@hidden] in
  let get_contract#27funtype a : * . address -> contract (a) = Λ a ->
  lambda (aaddress)contract (a) return CONTRACT(a)[@inline][@hidden] in
  let get_contract_opt#28funtype a : * . address -> option (contract (a)) =
    Λ a ->
  lambda (aaddress)option (contract (a)) return CONTRACT_OPT(a)[@inline][@hidden] in
  let get_contract_with_error#29funtype a : * . address -> string -> contract (a) =
    Λ a ->
  lambda (aaddress)string -> contract (a) return lambda (sstring)contract (a) return
  CONTRACT_WITH_ERROR(a , s)[@inline][@hidden] in
  let create_ticket#30funtype a : * . a -> nat -> ticket (a) = Λ a ->
  lambda (va)nat -> ticket (a) return lambda (nnat)ticket (a) return ([%Michelson {| { UNPAIR ; TICKET } |}])@(
                                                                     ( v ,
                                                                      n ))[@inline][@hidden] in
  let transaction#31funtype a : * . a -> tez -> contract (a) -> operation =
    Λ a ->
  lambda (aa)tez -> contract (a) -> operation return lambda (mutez)contract (a) -> operation return lambda (ccontract (a))operation return
  CALL(a , mu , c)[@inline][@hidden] in
  let open_chest#32chest_key -> chest -> nat -> sum[Fail_decrypt -> unit , Fail_timelock -> unit , Ok_opening -> bytes] =
    lambda (ckchest_key)chest -> nat -> sum[Fail_decrypt -> unit , Fail_timelock -> unit , Ok_opening -> bytes] return lambda (cchest)nat ->
  sum[Fail_decrypt -> unit , Fail_timelock -> unit , Ok_opening -> bytes] return lambda (nnat)
  sum[Fail_decrypt -> unit , Fail_timelock -> unit , Ok_opening -> bytes] return
  OPEN_CHEST(ck , c , n)[@inline][@hidden] in
  let call_view#33funtype a : * . funtype b : * . string -> a -> address -> option (b) =
    Λ a ->
  Λ b ->
  lambda (sstring)a -> address -> option (b) return lambda (xa)address -> option (b) return lambda (aaddress)option (b) return
  VIEW(s , x , a)[@inline][@hidden] in
  let split_ticket#34funtype a : * . ticket (a) -> ( nat * nat ) -> option (
  ( ticket (a) * ticket (a) )) = Λ a ->
  lambda (tticket (a))( nat * nat ) -> option (( ticket (a) * ticket (a) )) return lambda (p
  ( nat * nat ))option (( ticket (a) * ticket (a) )) return ([%Michelson {| { UNPAIR ; SPLIT_TICKET } |}])@(
                                                            ( t ,
                                                              p ))[@inline][@hidden] in
  let xor#35nat -> nat -> nat =
    lambda (lnat)nat -> nat return lambda (rnat)nat return XOR(l , r)[@inline][@hidden] in
  let shift_left#36nat -> nat -> nat =
    lambda (lnat)nat -> nat return lambda (rnat)nat return LSL(l , r)[@inline][@hidden] in
  let shift_right#37nat -> nat -> nat =
    lambda (lnat)nat -> nat return lambda (rnat)nat return LSR(l , r)[@inline][@hidden] in
  let empty#38funtype k : * . funtype v : * . big_map (k , v) = Λ k ->
  Λ v ->  BIG_MAP_EMPTY()[@inline][@hidden] in
  let mem#39funtype k : * . funtype v : * . k -> big_map (k , v) -> bool =
    Λ k ->
  Λ v ->
  lambda (kk)big_map (k , v) -> bool return lambda (mbig_map (k ,
  v))bool return ([%Michelson {| { UNPAIR ; MEM } |}])@(( k , m ))[@inline][@hidden] in
  let add#40funtype k : * . funtype v : * . k -> v -> big_map (k ,
  v) -> big_map (k , v) = Λ k ->
  Λ v ->
  lambda (kk)v -> big_map (k , v) -> big_map (k ,
  v) return lambda (vv)big_map (k , v) -> big_map (k ,
  v) return lambda (mbig_map (k , v))big_map (k , v) return MAP_ADD(k , v , m)[@inline][@hidden] in
  let remove#41funtype k : * . funtype v : * . k -> big_map (k ,
  v) -> big_map (k , v) = Λ k ->
  Λ v ->
  lambda (kk)big_map (k , v) -> big_map (k , v) return lambda (mbig_map (k ,
  v))big_map (k , v) return MAP_REMOVE(k , m)[@inline][@hidden] in
  let update#42funtype k : * . funtype v : * . k -> option (v) -> big_map (k ,
  v) -> big_map (k , v) = Λ k ->
  Λ v ->
  lambda (kk)option (v) -> big_map (k , v) -> big_map (k ,
  v) return lambda (voption (v))big_map (k , v) -> big_map (k ,
  v) return lambda (mbig_map (k , v))big_map (k ,
  v) return MAP_UPDATE(k , v , m)[@inline][@hidden] in
  let get_and_update#43funtype k : * . funtype v : * . k -> option (v) -> big_map (k ,
  v) -> ( option (v) * big_map (k , v) ) = Λ k ->
  Λ v ->
  lambda (kk)option (v) -> big_map (k ,
  v) -> ( option (v) * big_map (k , v) ) return lambda (voption (v))big_map (k ,
  v) -> ( option (v) * big_map (k , v) ) return lambda (mbig_map (k ,
  v))( option (v) * big_map (k , v) ) return BIG_MAP_GET_AND_UPDATE(k , v , m)[@inline][@hidden] in
  let find_opt#44funtype k : * . funtype v : * . k -> big_map (k ,
  v) -> option (v) = Λ k ->
  Λ v ->
  lambda (kk)big_map (k , v) -> option (v) return lambda (mbig_map (k ,
  v))option (v) return MAP_FIND_OPT(k , m)[@inline][@hidden] in
  let find#45funtype k : * . funtype v : * . k -> big_map (k , v) -> v =
    Λ k ->
  Λ v ->
  lambda (kk)big_map (k , v) -> v return lambda (mbig_map (k ,
  v))v return MAP_FIND(k , m)[@inline][@hidden] in
  let empty#46funtype k : * . funtype v : * . map (k , v) = Λ k ->
  Λ v ->  MAP_EMPTY()[@inline][@hidden] in
  let size#47funtype k : * . funtype v : * . map (k , v) -> nat = Λ k ->
  Λ v ->  lambda (mmap (k , v))nat return ([%Michelson {| { SIZE } |}])@(m)[@inline][@hidden] in
  let mem#48funtype k : * . funtype v : * . k -> map (k , v) -> bool = Λ k ->
  Λ v ->
  lambda (kk)map (k , v) -> bool return lambda (mmap (k ,
  v))bool return ([%Michelson {| { UNPAIR ; MEM } |}])@(( k , m ))[@inline][@hidden] in
  let add#49funtype k : * . funtype v : * . k -> v -> map (k , v) -> map (k ,
  v) = Λ k ->
  Λ v ->
  lambda (kk)v -> map (k , v) -> map (k , v) return lambda (vv)map (k ,
  v) -> map (k , v) return lambda (mmap (k , v))map (k ,
  v) return MAP_ADD(k , v , m)[@inline][@hidden] in
  let remove#50funtype k : * . funtype v : * . k -> map (k , v) -> map (k ,
  v) = Λ k ->
  Λ v ->
  lambda (kk)map (k , v) -> map (k , v) return lambda (mmap (k , v))map (k ,
  v) return MAP_REMOVE(k , m)[@inline][@hidden] in
  let update#51funtype k : * . funtype v : * . k -> option (v) -> map (k ,
  v) -> map (k , v) = Λ k ->
  Λ v ->
  lambda (kk)option (v) -> map (k , v) -> map (k ,
  v) return lambda (voption (v))map (k , v) -> map (k ,
  v) return lambda (mmap (k , v))map (k , v) return MAP_UPDATE(k , v , m)[@inline][@hidden] in
  let get_and_update#52funtype k : * . funtype v : * . k -> option (v) -> map (k ,
  v) -> ( option (v) * map (k , v) ) = Λ k ->
  Λ v ->
  lambda (kk)option (v) -> map (k ,
  v) -> ( option (v) * map (k , v) ) return lambda (voption (v))map (k ,
  v) -> ( option (v) * map (k , v) ) return lambda (mmap (k ,
  v))( option (v) * map (k , v) ) return MAP_GET_AND_UPDATE(k , v , m)[@inline][@hidden] in
  let find#53funtype k : * . funtype v : * . k -> map (k , v) -> v = Λ k ->
  Λ v ->
  lambda (kk)map (k , v) -> v return lambda (mmap (k ,
  v))v return MAP_FIND(k , m)[@inline][@hidden] in
  let find_opt#54funtype k : * . funtype v : * . k -> map (k ,
  v) -> option (v) = Λ k ->
  Λ v ->
  lambda (kk)map (k , v) -> option (v) return lambda (mmap (k ,
  v))option (v) return MAP_FIND_OPT(k , m)[@inline][@hidden] in
  let iter#55funtype k : * . funtype v : * . ( k * v ) -> unit -> map (k ,
  v) -> unit = Λ k ->
  Λ v ->
  lambda (f( k * v ) -> unit)map (k , v) -> unit return lambda (mmap (k ,
  v))unit return MAP_ITER(f , m)[@inline][@hidden] in
  let map#56funtype k : * . funtype v : * . funtype w : * . ( k * v ) -> w -> map (k ,
  v) -> map (k , w) = Λ k ->
  Λ v ->
  Λ w ->
  lambda (f( k * v ) -> w)map (k , v) -> map (k , w) return lambda (mmap (k ,
  v))map (k , w) return MAP_MAP(f , m)[@inline][@hidden] in
  let fold#57funtype k : * . funtype v : * . funtype c : * . ( c * ( k * v ) ) -> c -> map (k ,
  v) -> c -> c = Λ k ->
  Λ v ->
  Λ c ->
  lambda (f( c * ( k * v ) ) -> c)map (k ,
  v) -> c -> c return lambda (mmap (k ,
  v))c -> c return lambda (ic)c return MAP_FOLD(f , m , i)[@inline][@hidden] in
  let empty#58funtype a : * . set (a) = Λ a ->
  SET_EMPTY()[@inline][@hidden] in
  let size#59funtype a : * . set (a) -> nat = Λ a ->
  lambda (sset (a))nat return ([%Michelson {| { SIZE } |}])@(s)[@inline][@hidden] in
  let cardinal#60funtype a : * . set (a) -> nat = Λ a ->
  lambda (sset (a))nat return ([%Michelson {| { SIZE } |}])@(s)[@inline][@hidden] in
  let mem#61funtype a : * . a -> set (a) -> bool = Λ a ->
  lambda (xa)set (a) -> bool return lambda (sset (a))bool return SET_MEM(x , s)[@inline][@hidden] in
  let add#62funtype a : * . a -> set (a) -> set (a) = Λ a ->
  lambda (xa)set (a) -> set (a) return lambda (sset (a))set (a) return
  SET_ADD(x , s)[@inline][@hidden] in
  let remove#63funtype a : * . a -> set (a) -> set (a) = Λ a ->
  lambda (xa)set (a) -> set (a) return lambda (sset (a))set (a) return
  SET_REMOVE(x , s)[@inline][@hidden] in
  let update#64funtype a : * . a -> bool -> set (a) -> set (a) = Λ a ->
  lambda (xa)bool -> set (a) -> set (a) return lambda (bbool)set (a) -> set (a) return lambda (sset (a))set (a) return
  SET_UPDATE(x , b , s)[@inline][@hidden] in
  let iter#65funtype a : * . a -> unit -> set (a) -> unit = Λ a ->
  lambda (fa -> unit)set (a) -> unit return lambda (sset (a))unit return
  SET_ITER(f , s)[@inline][@hidden] in
  let fold#66funtype a : * . funtype b : * . ( b * a ) -> b -> set (a) -> b -> b =
    Λ a ->
  Λ b ->
  lambda (f( b * a ) -> b)set (a) -> b -> b return lambda (sset (a))b -> b return lambda (ib)b return
  SET_FOLD(f , s , i)[@inline][@hidden] in
  let fold_desc#67funtype a : * . funtype b : * . ( a * b ) -> b -> set (a) -> b -> b =
    Λ a ->
  Λ b ->
  lambda (f( a * b ) -> b)set (a) -> b -> b return lambda (sset (a))b -> b return lambda (ib)b return
  SET_FOLD_DESC(f , s , i)[@inline][@hidden] in
  let length#68funtype a : * . list (a) -> nat = Λ a ->
  lambda (xslist (a))nat return ([%Michelson {| { SIZE } |}])@(xs)[@inline][@hidden] in
  let size#69funtype a : * . list (a) -> nat = Λ a ->
  lambda (xslist (a))nat return ([%Michelson {| { SIZE } |}])@(xs)[@inline][@hidden] in
  let head_opt#70funtype a : * . list (a) -> option (a) = Λ a ->
  lambda (xslist (a))option (a) return  match xs with
                                         | Cons ctor_proj#2 ->
                                            match ctor_proj#2 with
                                             | ( x , _#2 ) ->
                                             Some(x)
                                         | Nil unit_proj#4 ->
                                           None(unit)[@inline][@hidden] in
  let tail_opt#71funtype a : * . list (a) -> option (list (a)) = Λ a ->
  lambda (xslist (a))option (list (a)) return  match xs with
                                                | Cons ctor_proj#5 ->
                                                   match ctor_proj#5 with
                                                    | ( _#3 , xs ) ->
                                                    Some(xs)
                                                | Nil unit_proj#7 ->
                                                  None(unit)[@inline][@hidden] in
  let map#72funtype a : * . funtype b : * . a -> b -> list (a) -> list (b) =
    Λ a ->
  Λ b ->
  lambda (fa -> b)list (a) -> list (b) return lambda (xslist (a))list (b) return
  LIST_MAP(f , xs)[@inline][@hidden] in
  let iter#73funtype a : * . a -> unit -> list (a) -> unit = Λ a ->
  lambda (fa -> unit)list (a) -> unit return lambda (xslist (a))unit return
  LIST_ITER(f , xs)[@inline][@hidden] in
  let fold#74funtype a : * . funtype b : * . ( b * a ) -> b -> list (a) -> b -> b =
    Λ a ->
  Λ b ->
  lambda (f( b * a ) -> b)list (a) -> b -> b return lambda (xslist (a))b -> b return lambda (ib)b return
  LIST_FOLD(f , xs , i)[@inline][@hidden] in
  let fold_left#75funtype a : * . funtype b : * . ( b * a ) -> b -> b -> list (a) -> b =
    Λ a ->
  Λ b ->
  lambda (f( b * a ) -> b)b -> list (a) -> b return lambda (ib)list (a) -> b return lambda (xslist (a))b return
  LIST_FOLD_LEFT(f , i , xs)[@inline][@hidden] in
  let fold_right#76funtype a : * . funtype b : * . ( a * b ) -> b -> list (a) -> b -> b =
    Λ a ->
  Λ b ->
  lambda (f( a * b ) -> b)list (a) -> b -> b return lambda (xslist (a))b -> b return lambda (ib)b return
  LIST_FOLD_RIGHT(f , xs , i)[@inline][@hidden] in
  let cons#77funtype a : * . a -> list (a) -> list (a) = Λ a ->
  lambda (xa)list (a) -> list (a) return lambda (xslist (a))list (a) return
  CONS(x , xs)[@inline][@hidden] in
  let length#78string -> nat =
    lambda (bstring)nat return ([%Michelson {| { SIZE } |}])@(b)[@inline][@hidden] in
  let concat#79string -> string -> string =
    lambda (b1string)string -> string return lambda (b2string)string return
  ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b1 , b2 ))[@inline][@hidden] in
  let sub#80nat -> nat -> string -> string =
    lambda (snat)nat -> string -> string return lambda (lnat)string -> string return lambda (bstring)string return
  ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(
  ( s ,
    l ,
    b ))[@inline][@hidden] in
  let unopt#81funtype a : * . option (a) -> a = Λ a ->
  lambda (voption (a))a return UNOPT(v)[@inline][@hidden] in
  let unopt_with_error#82funtype a : * . option (a) -> string -> a = Λ a ->
  lambda (voption (a))string -> a return lambda (sstring)a return UNOPT_WITH_ERROR
                                                                  (v ,
                                                                   s)[@inline][@hidden] in
  let pack#83funtype a : * . a -> bytes = Λ a ->
  lambda (va)bytes return ([%Michelson {| { PACK } |}])@(v)[@inline][@hidden] in
  let unpack#84funtype a : * . bytes -> option (a) = Λ a ->
  lambda (bbytes)option (a) return BYTES_UNPACK(b)[@inline][@hidden] in
  let length#85bytes -> nat =
    lambda (bbytes)nat return ([%Michelson {| { SIZE } |}])@(b)[@inline][@hidden] in
  let concat#86bytes -> bytes -> bytes =
    lambda (b1bytes)bytes -> bytes return lambda (b2bytes)bytes return
  ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b1 , b2 ))[@inline][@hidden] in
  let sub#87nat -> nat -> bytes -> bytes =
    lambda (snat)nat -> bytes -> bytes return lambda (lnat)bytes -> bytes return lambda (bbytes)bytes return
  ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(
  ( s ,
    l ,
    b ))[@inline][@hidden] in
  let blake2b#88bytes -> bytes =
    lambda (bbytes)bytes return ([%Michelson {| { BLAKE2B } |}])@(b)[@inline][@hidden] in
  let sha256#89bytes -> bytes =
    lambda (bbytes)bytes return ([%Michelson {| { SHA256 } |}])@(b)[@inline][@hidden] in
  let sha512#90bytes -> bytes =
    lambda (bbytes)bytes return ([%Michelson {| { SHA512 } |}])@(b)[@inline][@hidden] in
  let sha3#91bytes -> bytes =
    lambda (bbytes)bytes return ([%Michelson {| { SHA3 } |}])@(b)[@inline][@hidden] in
  let keccak#92bytes -> bytes =
    lambda (bbytes)bytes return ([%Michelson {| { KECCAK } |}])@(b)[@inline][@hidden] in
  let hash_key#93key -> key_hash =
    lambda (kkey)key_hash return ([%Michelson {| { HASH_KEY } |}])@(k)[@inline][@hidden] in
  let check#94key -> signature -> bytes -> bool =
    lambda (kkey)signature -> bytes -> bool return lambda (ssignature)bytes -> bool return lambda (bbytes)bool return
  ([%Michelson {| { UNPAIR ; UNPAIR ; CHECK_SIGNATURE } |}])@(( k , s , b ))[@inline][@hidden] in
  let assertbool -> unit =
    lambda (bbool)unit return ([%Michelson {| { IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } } |}])@(b)[@inline][@private][@hidden] in
  let assert_somefuntype a : * . option (a) -> unit = Λ a ->
  lambda (voption (a))unit return ([%Michelson {| { IF_NONE { PUSH string "failed assert some" ; FAILWITH } { DROP ; UNIT } } |}])@(v)[@inline][@private][@hidden] in
  let assert_nonefuntype a : * . option (a) -> unit = Λ a ->
  lambda (voption (a))unit return ([%Michelson {| { IF_NONE { UNIT } { PUSH string "failed assert none" ; FAILWITH } } |}])@(v)[@inline][@private][@hidden] in
  let absint -> nat =
    lambda (iint)nat return ([%Michelson {| { ABS } |}])@(i)[@inline][@private][@hidden] in
  let is_natint -> option (nat) =
    lambda (iint)option (nat) return ([%Michelson {| { ISNAT } |}])@(i)[@inline][@private][@hidden] in
  let truebool = TRUE()[@inline][@private][@hidden] in
  let falsebool = FALSE()[@inline][@private][@hidden] in
  let unitunit = UNIT()[@inline][@private][@hidden] in
  let failwithfuntype a : * . funtype b : * . a -> b = Λ a ->
  Λ b ->  [%Michelson {|{ FAILWITH }|}][@inline][@private][@hidden] in
  let intfuntype a : * . a -> external_int (a) = Λ a ->
  lambda (va)external_int (a) return ([%Michelson {| { INT } |}])@(v)[@inline][@private][@hidden] in
  let assert_with_errorbool -> string -> unit =
    lambda (bbool)string -> unit return lambda (sstring)unit return ([%Michelson {| { UNPAIR ; IF { DROP ; UNIT } { FAILWITH } } |}])@(
                                                                    ( b ,
                                                                      s ))[@inline][@private][@hidden] in
  let assert_some_with_errorfuntype a : * . option (a) -> string -> unit =
    Λ a ->
  lambda (voption (a))string -> unit return lambda (sstring)unit return
  ([%Michelson {| { UNPAIR ; IF_NONE { FAILWITH } { DROP 2 ; UNIT } } |}])@(
  ( v ,
    s ))[@inline][@private][@hidden] in
  let assert_none_with_errorfuntype a : * . option (a) -> string -> unit =
    Λ a ->
  lambda (voption (a))string -> unit return lambda (sstring)unit return
  ([%Michelson {| { UNPAIR ; IF_NONE { DROP ; UNIT } { DROP ; FAILWITH } } |}])@(
  ( v ,
    s ))[@inline][@private][@hidden] in
  let edivfuntype a : * . funtype b : * . a -> b -> external_ediv (a , b) =
    Λ a ->
  Λ b ->
  lambda (la)b -> external_ediv (a , b) return lambda (rb)external_ediv (a ,
  b) return ([%Michelson {| { UNPAIR ; EDIV } |}])@(( l , r ))[@inline][@private][@hidden] in
  let stub#95funtype a : * . funtype b : * . a -> b = Λ a ->
  Λ b ->  lambda (xa)b return ([%Michelson {|{ FAILWITH }|}])@(x)[@inline][@private][@hidden] in
  let run#96funtype a : * . funtype b : * . a -> b -> a -> unit = Λ a ->
  Λ b ->
  lambda (_fa -> b)a -> unit return lambda (_va)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let eval#97funtype a : * . a -> unit = Λ a ->
  lambda (_xa)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let compile_value#98funtype a : * . a -> unit = Λ a ->
  lambda (_xa)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let get_total_voting_power#99unit -> nat =
    lambda (_uunit)nat return (stub#95@{unit}@{nat})@(unit)[@inline][@hidden] in
  let failwith#100funtype a : * . funtype b : * . a -> b = Λ a ->
  Λ b ->  lambda (_va)b return (stub#95@{unit}@{b})@(unit)[@inline][@hidden] in
  let to_contract#101funtype p : * . funtype s : * . unit -> contract (p) =
    Λ p ->
  Λ s ->
  lambda (_tunit)contract (p) return (stub#95@{unit}@{contract (p)})@(unit)[@inline][@hidden] in
  let set_source#102address -> unit =
    lambda (_aaddress)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let get_storage_of_address#103address -> unit =
    lambda (_aaddress)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let get_balance#104address -> tez =
    lambda (_aaddress)tez return (stub#95@{unit}@{tez})@(unit)[@inline][@hidden] in
  let print#105string -> unit =
    lambda (_vstring)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let eprint#106string -> unit =
    lambda (_vstring)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let get_voting_power#107key_hash -> nat =
    lambda (_khkey_hash)nat return (stub#95@{unit}@{nat})@(unit)[@inline][@hidden] in
  let nth_bootstrap_contract#108nat -> address =
    lambda (_inat)address return (stub#95@{unit}@{address})@(unit)[@inline][@hidden] in
  let nth_bootstrap_account#109int -> address =
    lambda (_iint)address return (stub#95@{unit}@{address})@(unit)[@inline][@hidden] in
  let get_bootstrap_account#110nat -> ( address * key * string ) =
    lambda (_nnat)( address * key * string ) return (stub#95@{unit}@{( address * key * string )})@(unit)[@inline][@hidden] in
  let nth_bootstrap_typed_address#111funtype a : * . funtype b : * . nat -> unit =
    Λ a ->
  Λ b ->  lambda (_nnat)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let last_originations#112unit -> map (address , list (address)) =
    lambda (_uunit)map (address ,
  list (address)) return (stub#95@{unit}@{map (address ,
                         list (address))})@(unit)[@inline][@hidden] in
  let random#113funtype a : * . unit -> a = Λ a ->
  lambda (_uunit)a return (stub#95@{unit}@{a})@(unit)[@inline][@hidden] in
  let new_account#114unit -> ( string * key ) =
    lambda (_uunit)( string * key ) return (stub#95@{unit}@{( string * key )})@(unit)[@inline][@hidden] in
  let decompile#115funtype a : * . unit -> a = Λ a ->
  lambda (_munit)a return (stub#95@{unit}@{a})@(unit)[@inline][@hidden] in
  let bake_until_n_cycle_end#116nat -> unit =
    lambda (_nnat)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let cast_address#117funtype a : * . funtype b : * . address -> unit =
    Λ a ->
  Λ b ->  lambda (_aaddress)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let register_delegate#118key_hash -> unit =
    lambda (_khkey_hash)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let register_constant#119unit -> string =
    lambda (_munit)string return (stub#95@{unit}@{string})@(unit)[@inline][@hidden] in
  let to_typed_address#120funtype a : * . funtype b : * . contract (a) -> unit =
    Λ a ->
  Λ b ->  lambda (_ccontract (a))unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let constant_to_michelson_program#121string -> unit =
    lambda (_sstring)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let restore_context#122unit -> unit =
    lambda (_uunit)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let save_context#123unit -> unit =
    lambda (_uunit)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let drop_context#124unit -> unit =
    lambda (_uunit)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let to_string#125funtype a : * . a -> string = Λ a ->
  lambda (_va)string return (stub#95@{unit}@{string})@(unit)[@inline][@hidden] in
  let get_storage#126funtype p : * . funtype s : * . unit -> s = Λ p ->
  Λ s ->  lambda (_tunit)s return (stub#95@{unit}@{s})@(unit)[@inline][@hidden] in
  let set_baker_policy#127unit -> unit =
    lambda (_bpunit)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let set_baker#128address -> unit =
    lambda (_aaddress)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let size#129unit -> int =
    lambda (_cunit)int return (stub#95@{unit}@{int})@(unit)[@inline][@hidden] in
  let compile_contract#130funtype p : * . funtype s : * . ( p * s ) ->
  ( list (operation) * s ) -> unit = Λ p ->
  Λ s ->
  lambda (_f( p * s ) -> ( list (operation) * s ))unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let read_contract_from_file#131string -> unit =
    lambda (_fnstring)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let chr#132nat -> option (string) =
    lambda (_nnat)option (string) return (stub#95@{unit}@{option (string)})@(unit)[@inline][@hidden] in
  let nl#133string = "NEWLINE"[@inline][@hidden] in
  let println#134string -> unit =
    lambda (_vstring)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let transfer#135address -> unit -> tez -> unit =
    lambda (_aaddress)unit -> tez -> unit return lambda (_sunit)tez -> unit return lambda (_ttez)unit return
  (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let transfer_exn#136address -> unit -> tez -> nat =
    lambda (_aaddress)unit -> tez -> nat return lambda (_sunit)tez -> nat return lambda (_ttez)nat return
  (stub#95@{unit}@{nat})@(unit)[@inline][@hidden] in
  let log#137funtype a : * . a -> unit = Λ a ->
  lambda (_va)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let reset_state#138nat -> list (tez) -> unit =
    lambda (_nnat)list (tez) -> unit return lambda (_llist (tez))unit return
  (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let reset_state_at#139timestamp -> nat -> list (tez) -> unit =
    lambda (_ttimestamp)nat -> list (tez) -> unit return lambda (_nnat)list (tez) -> unit return lambda (_llist (tez))unit return
  (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let bootstrap_contract#140funtype p : * . funtype s : * . ( p * s ) ->
  ( list (operation) * s ) -> s -> tez -> unit = Λ p ->
  Λ s ->
  lambda (_f( p * s ) -> ( list (operation) * s ))s -> tez -> unit return lambda (_ss)tez -> unit return lambda (_ttez)unit return
  (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let mutate_value#141funtype a : * . nat -> a -> option (( a * unit )) =
    Λ a ->
  lambda (_nnat)a -> option (( a * unit )) return lambda (_va)option (( a * unit )) return
  (stub#95@{unit}@{option (( a * unit ))})@(unit)[@inline][@hidden] in
  let save_mutation#142string -> unit -> option (string) =
    lambda (_sstring)unit -> option (string) return lambda (_munit)option (string) return
  (stub#95@{unit}@{option (string)})@(unit)[@inline][@hidden] in
  let mutation_test#143funtype a : * . funtype b : * . a -> a -> b -> option (
  ( b * unit )) = Λ a ->
  Λ b ->
  lambda (_va)a -> b -> option (( b * unit )) return lambda (_fa -> b)option (
  ( b * unit )) return (stub#95@{unit}@{option (( b * unit ))})@(unit)[@inline][@hidden] in
  let mutation_test_all#144funtype a : * . funtype b : * . a -> a -> b -> list (
  ( b * unit )) = Λ a ->
  Λ b ->
  lambda (_va)a -> b -> list (( b * unit )) return lambda (_fa -> b)list (
  ( b * unit )) return (stub#95@{unit}@{list (( b * unit ))})@(unit)[@inline][@hidden] in
  let sign#145string -> bytes -> signature =
    lambda (_skstring)bytes -> signature return lambda (_dbytes)signature return
  (stub#95@{unit}@{signature})@(unit)[@inline][@hidden] in
  let add_account#146string -> key -> unit =
    lambda (_sstring)key -> unit return lambda (_kkey)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let baker_account#147( string * key ) -> option (tez) -> unit =
    lambda (_p( string * key ))option (tez) -> unit return lambda (_ooption (tez))unit return
  (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let set_big_map#148funtype a : * . funtype b : * . int -> big_map (a ,
  b) -> unit = Λ a ->
  Λ b ->
  lambda (_iint)big_map (a , b) -> unit return lambda (_mbig_map (a ,
  b))unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let create_chest#149bytes -> nat -> ( chest * chest_key ) =
    lambda (_bbytes)nat -> ( chest * chest_key ) return lambda (_nnat)( chest * chest_key ) return
  (stub#95@{unit}@{( chest * chest_key )})@(unit)[@inline][@hidden] in
  let create_chest_key#150chest -> nat -> chest_key =
    lambda (_cchest)nat -> chest_key return lambda (_nnat)chest_key return
  (stub#95@{unit}@{chest_key})@(unit)[@inline][@hidden] in
  let transfer_to_contract#151funtype p : * . contract (p) -> p -> tez -> unit =
    Λ p ->
  lambda (_ccontract (p))p -> tez -> unit return lambda (_sp)tez -> unit return lambda (_ttez)unit return
  (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let transfer_to_contract_exn#152funtype p : * . contract (p) -> p -> tez -> nat =
    Λ p ->
  lambda (_ccontract (p))p -> tez -> nat return lambda (_sp)tez -> nat return lambda (_ttez)nat return
  (stub#95@{unit}@{nat})@(unit)[@inline][@hidden] in
  let michelson_equal#153unit -> unit -> bool =
    lambda (_m1unit)unit -> bool return lambda (_m2unit)bool return (stub#95@{unit}@{bool})@(unit)[@inline][@hidden] in
  let to_entrypoint#154funtype a : * . funtype b : * . funtype c : * . string -> unit -> contract (c) =
    Λ a ->
  Λ b ->
  Λ c ->
  lambda (_sstring)unit -> contract (c) return lambda (_tunit)contract (c) return
  (stub#95@{unit}@{contract (c)})@(unit)[@inline][@hidden] in
  let originate_contract#155unit -> unit -> tez -> address =
    lambda (_cunit)unit -> tez -> address return lambda (_sunit)tez -> address return lambda (_ttez)address return
  (stub#95@{unit}@{address})@(unit)[@inline][@hidden] in
  let originate#156funtype p : * . funtype s : * . ( p * s ) -> ( list (operation) * s ) -> s -> tez ->
  ( unit * unit * int ) = Λ p ->
  Λ s ->
  lambda (_f( p * s ) -> ( list (operation) * s ))s -> tez -> ( unit * unit * int ) return lambda (_ss)tez ->
  ( unit * unit * int ) return lambda (_ttez)( unit * unit * int ) return
  (stub#95@{unit}@{( unit * unit * int )})@(unit)[@inline][@hidden] in
  let compile_contract_from_file#157string -> string -> list (string) -> unit =
    lambda (_fnstring)string -> list (string) -> unit return lambda (_estring)list (string) -> unit return lambda (_vlist (string))unit return
  (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let originate_from_file#158string -> string -> list (string) -> unit -> tez ->
  ( address * unit * int ) =
    lambda (_fnstring)string -> list (string) -> unit -> tez -> ( address * unit * int ) return lambda (_estring)list (string) -> unit -> tez ->
  ( address * unit * int ) return lambda (_vlist (string))unit -> tez ->
  ( address * unit * int ) return lambda (_sunit)tez -> ( address * unit * int ) return lambda (_ttez)
  ( address * unit * int ) return (stub#95@{unit}@{( address * unit * int )})@(unit)[@inline][@hidden] in
  let v#159int = 40 in
  let v#160int = ADD(v#159 , 1) in
  let v#161int = ADD(v#160 , 1) in let xint = v#161 in unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias7.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias8.mligo" ] ;
  [%expect {xxx|
  let get_balance#8unit -> tez =
    lambda (_uunit)tez return ([%Michelson {| { DROP ; BALANCE } |}])@(unit)[@inline][@hidden] in
  let get_amount#9unit -> tez =
    lambda (_uunit)tez return ([%Michelson {| { DROP ; AMOUNT } |}])@(unit)[@inline][@hidden] in
  let get_now#10unit -> timestamp =
    lambda (_uunit)timestamp return ([%Michelson {| { DROP ; NOW } |}])@(unit)[@inline][@hidden] in
  let get_sender#11unit -> address =
    lambda (_uunit)address return ([%Michelson {| { DROP ; SENDER } |}])@(unit)[@inline][@hidden] in
  let get_source#12unit -> address =
    lambda (_uunit)address return ([%Michelson {| { DROP ; SOURCE } |}])@(unit)[@inline][@hidden] in
  let get_level#13unit -> nat =
    lambda (_uunit)nat return ([%Michelson {| { DROP ; LEVEL } |}])@(unit)[@inline][@hidden] in
  let get_self_address#14unit -> address =
    lambda (_uunit)address return SELF_ADDRESS()[@inline][@hidden] in
  let get_chain_id#15unit -> chain_id =
    lambda (_uunit)chain_id return ([%Michelson {| { DROP ; CHAIN_ID } |}])@(unit)[@inline][@hidden] in
  let get_total_voting_power#16unit -> nat =
    lambda (_uunit)nat return ([%Michelson {| { DROP ; TOTAL_VOTING_POWER } |}])@(unit)[@inline][@hidden] in
  let get_min_block_time#17unit -> nat =
    lambda (_uunit)nat return ([%Michelson {| { DROP; MIN_BLOCK_TIME } |}])@(unit)[@inline][@hidden] in
  let voting_power#18key_hash -> nat =
    lambda (khkey_hash)nat return ([%Michelson {| { VOTING_POWER } |}])@(kh)[@inline][@hidden] in
  let address#19funtype a : * . contract (a) -> address = Λ a ->
  lambda (ccontract (a))address return ADDRESS(c)[@inline][@hidden] in
  let implicit_account#20key_hash -> contract (unit) =
    lambda (khkey_hash)contract (unit) return IMPLICIT_ACCOUNT(kh)[@inline][@hidden] in
  let join_tickets#21funtype a : * . ( ticket (a) * ticket (a) ) -> option (ticket (a)) =
    Λ a ->
  lambda (t( ticket (a) * ticket (a) ))option (ticket (a)) return ([%Michelson {| { JOIN_TICKETS } |}])@(t)[@inline][@hidden] in
  let read_ticket#22funtype a : * . ticket (a) -> ( ( address * ( a * nat ) ) * ticket (a) ) =
    Λ a ->
  lambda (tticket (a))( ( address * ( a * nat ) ) * ticket (a) ) return
  ([%Michelson {| { READ_TICKET ; PAIR } |}])@(t)[@inline][@hidden] in
  let never#23funtype a : * . never -> a = Λ a ->
  lambda (nnever)a return ([%Michelson {| { NEVER } |}])@(n)[@inline][@hidden] in
  let pairing_check#24list (( bls12_381_g1 * bls12_381_g2 )) -> bool =
    lambda (llist (( bls12_381_g1 * bls12_381_g2 )))bool return ([%Michelson {| { PAIRING_CHECK } |}])@(l)[@inline][@hidden] in
  let constant#25funtype a : * . string -> a = Λ a ->
  lambda (sstring)a return GLOBAL_CONSTANT(s)[@inline][@hidden] in
  let set_delegate#26option (key_hash) -> operation =
    lambda (ooption (key_hash))operation return SET_DELEGATE(o)[@inline][@hidden] in
  let get_contract#27funtype a : * . address -> contract (a) = Λ a ->
  lambda (aaddress)contract (a) return CONTRACT(a)[@inline][@hidden] in
  let get_contract_opt#28funtype a : * . address -> option (contract (a)) =
    Λ a ->
  lambda (aaddress)option (contract (a)) return CONTRACT_OPT(a)[@inline][@hidden] in
  let get_contract_with_error#29funtype a : * . address -> string -> contract (a) =
    Λ a ->
  lambda (aaddress)string -> contract (a) return lambda (sstring)contract (a) return
  CONTRACT_WITH_ERROR(a , s)[@inline][@hidden] in
  let create_ticket#30funtype a : * . a -> nat -> ticket (a) = Λ a ->
  lambda (va)nat -> ticket (a) return lambda (nnat)ticket (a) return ([%Michelson {| { UNPAIR ; TICKET } |}])@(
                                                                     ( v ,
                                                                      n ))[@inline][@hidden] in
  let transaction#31funtype a : * . a -> tez -> contract (a) -> operation =
    Λ a ->
  lambda (aa)tez -> contract (a) -> operation return lambda (mutez)contract (a) -> operation return lambda (ccontract (a))operation return
  CALL(a , mu , c)[@inline][@hidden] in
  let open_chest#32chest_key -> chest -> nat -> sum[Fail_decrypt -> unit , Fail_timelock -> unit , Ok_opening -> bytes] =
    lambda (ckchest_key)chest -> nat -> sum[Fail_decrypt -> unit , Fail_timelock -> unit , Ok_opening -> bytes] return lambda (cchest)nat ->
  sum[Fail_decrypt -> unit , Fail_timelock -> unit , Ok_opening -> bytes] return lambda (nnat)
  sum[Fail_decrypt -> unit , Fail_timelock -> unit , Ok_opening -> bytes] return
  OPEN_CHEST(ck , c , n)[@inline][@hidden] in
  let call_view#33funtype a : * . funtype b : * . string -> a -> address -> option (b) =
    Λ a ->
  Λ b ->
  lambda (sstring)a -> address -> option (b) return lambda (xa)address -> option (b) return lambda (aaddress)option (b) return
  VIEW(s , x , a)[@inline][@hidden] in
  let split_ticket#34funtype a : * . ticket (a) -> ( nat * nat ) -> option (
  ( ticket (a) * ticket (a) )) = Λ a ->
  lambda (tticket (a))( nat * nat ) -> option (( ticket (a) * ticket (a) )) return lambda (p
  ( nat * nat ))option (( ticket (a) * ticket (a) )) return ([%Michelson {| { UNPAIR ; SPLIT_TICKET } |}])@(
                                                            ( t ,
                                                              p ))[@inline][@hidden] in
  let xor#35nat -> nat -> nat =
    lambda (lnat)nat -> nat return lambda (rnat)nat return XOR(l , r)[@inline][@hidden] in
  let shift_left#36nat -> nat -> nat =
    lambda (lnat)nat -> nat return lambda (rnat)nat return LSL(l , r)[@inline][@hidden] in
  let shift_right#37nat -> nat -> nat =
    lambda (lnat)nat -> nat return lambda (rnat)nat return LSR(l , r)[@inline][@hidden] in
  let empty#38funtype k : * . funtype v : * . big_map (k , v) = Λ k ->
  Λ v ->  BIG_MAP_EMPTY()[@inline][@hidden] in
  let mem#39funtype k : * . funtype v : * . k -> big_map (k , v) -> bool =
    Λ k ->
  Λ v ->
  lambda (kk)big_map (k , v) -> bool return lambda (mbig_map (k ,
  v))bool return ([%Michelson {| { UNPAIR ; MEM } |}])@(( k , m ))[@inline][@hidden] in
  let add#40funtype k : * . funtype v : * . k -> v -> big_map (k ,
  v) -> big_map (k , v) = Λ k ->
  Λ v ->
  lambda (kk)v -> big_map (k , v) -> big_map (k ,
  v) return lambda (vv)big_map (k , v) -> big_map (k ,
  v) return lambda (mbig_map (k , v))big_map (k , v) return MAP_ADD(k , v , m)[@inline][@hidden] in
  let remove#41funtype k : * . funtype v : * . k -> big_map (k ,
  v) -> big_map (k , v) = Λ k ->
  Λ v ->
  lambda (kk)big_map (k , v) -> big_map (k , v) return lambda (mbig_map (k ,
  v))big_map (k , v) return MAP_REMOVE(k , m)[@inline][@hidden] in
  let update#42funtype k : * . funtype v : * . k -> option (v) -> big_map (k ,
  v) -> big_map (k , v) = Λ k ->
  Λ v ->
  lambda (kk)option (v) -> big_map (k , v) -> big_map (k ,
  v) return lambda (voption (v))big_map (k , v) -> big_map (k ,
  v) return lambda (mbig_map (k , v))big_map (k ,
  v) return MAP_UPDATE(k , v , m)[@inline][@hidden] in
  let get_and_update#43funtype k : * . funtype v : * . k -> option (v) -> big_map (k ,
  v) -> ( option (v) * big_map (k , v) ) = Λ k ->
  Λ v ->
  lambda (kk)option (v) -> big_map (k ,
  v) -> ( option (v) * big_map (k , v) ) return lambda (voption (v))big_map (k ,
  v) -> ( option (v) * big_map (k , v) ) return lambda (mbig_map (k ,
  v))( option (v) * big_map (k , v) ) return BIG_MAP_GET_AND_UPDATE(k , v , m)[@inline][@hidden] in
  let find_opt#44funtype k : * . funtype v : * . k -> big_map (k ,
  v) -> option (v) = Λ k ->
  Λ v ->
  lambda (kk)big_map (k , v) -> option (v) return lambda (mbig_map (k ,
  v))option (v) return MAP_FIND_OPT(k , m)[@inline][@hidden] in
  let find#45funtype k : * . funtype v : * . k -> big_map (k , v) -> v =
    Λ k ->
  Λ v ->
  lambda (kk)big_map (k , v) -> v return lambda (mbig_map (k ,
  v))v return MAP_FIND(k , m)[@inline][@hidden] in
  let empty#46funtype k : * . funtype v : * . map (k , v) = Λ k ->
  Λ v ->  MAP_EMPTY()[@inline][@hidden] in
  let size#47funtype k : * . funtype v : * . map (k , v) -> nat = Λ k ->
  Λ v ->  lambda (mmap (k , v))nat return ([%Michelson {| { SIZE } |}])@(m)[@inline][@hidden] in
  let mem#48funtype k : * . funtype v : * . k -> map (k , v) -> bool = Λ k ->
  Λ v ->
  lambda (kk)map (k , v) -> bool return lambda (mmap (k ,
  v))bool return ([%Michelson {| { UNPAIR ; MEM } |}])@(( k , m ))[@inline][@hidden] in
  let add#49funtype k : * . funtype v : * . k -> v -> map (k , v) -> map (k ,
  v) = Λ k ->
  Λ v ->
  lambda (kk)v -> map (k , v) -> map (k , v) return lambda (vv)map (k ,
  v) -> map (k , v) return lambda (mmap (k , v))map (k ,
  v) return MAP_ADD(k , v , m)[@inline][@hidden] in
  let remove#50funtype k : * . funtype v : * . k -> map (k , v) -> map (k ,
  v) = Λ k ->
  Λ v ->
  lambda (kk)map (k , v) -> map (k , v) return lambda (mmap (k , v))map (k ,
  v) return MAP_REMOVE(k , m)[@inline][@hidden] in
  let update#51funtype k : * . funtype v : * . k -> option (v) -> map (k ,
  v) -> map (k , v) = Λ k ->
  Λ v ->
  lambda (kk)option (v) -> map (k , v) -> map (k ,
  v) return lambda (voption (v))map (k , v) -> map (k ,
  v) return lambda (mmap (k , v))map (k , v) return MAP_UPDATE(k , v , m)[@inline][@hidden] in
  let get_and_update#52funtype k : * . funtype v : * . k -> option (v) -> map (k ,
  v) -> ( option (v) * map (k , v) ) = Λ k ->
  Λ v ->
  lambda (kk)option (v) -> map (k ,
  v) -> ( option (v) * map (k , v) ) return lambda (voption (v))map (k ,
  v) -> ( option (v) * map (k , v) ) return lambda (mmap (k ,
  v))( option (v) * map (k , v) ) return MAP_GET_AND_UPDATE(k , v , m)[@inline][@hidden] in
  let find#53funtype k : * . funtype v : * . k -> map (k , v) -> v = Λ k ->
  Λ v ->
  lambda (kk)map (k , v) -> v return lambda (mmap (k ,
  v))v return MAP_FIND(k , m)[@inline][@hidden] in
  let find_opt#54funtype k : * . funtype v : * . k -> map (k ,
  v) -> option (v) = Λ k ->
  Λ v ->
  lambda (kk)map (k , v) -> option (v) return lambda (mmap (k ,
  v))option (v) return MAP_FIND_OPT(k , m)[@inline][@hidden] in
  let iter#55funtype k : * . funtype v : * . ( k * v ) -> unit -> map (k ,
  v) -> unit = Λ k ->
  Λ v ->
  lambda (f( k * v ) -> unit)map (k , v) -> unit return lambda (mmap (k ,
  v))unit return MAP_ITER(f , m)[@inline][@hidden] in
  let map#56funtype k : * . funtype v : * . funtype w : * . ( k * v ) -> w -> map (k ,
  v) -> map (k , w) = Λ k ->
  Λ v ->
  Λ w ->
  lambda (f( k * v ) -> w)map (k , v) -> map (k , w) return lambda (mmap (k ,
  v))map (k , w) return MAP_MAP(f , m)[@inline][@hidden] in
  let fold#57funtype k : * . funtype v : * . funtype c : * . ( c * ( k * v ) ) -> c -> map (k ,
  v) -> c -> c = Λ k ->
  Λ v ->
  Λ c ->
  lambda (f( c * ( k * v ) ) -> c)map (k ,
  v) -> c -> c return lambda (mmap (k ,
  v))c -> c return lambda (ic)c return MAP_FOLD(f , m , i)[@inline][@hidden] in
  let empty#58funtype a : * . set (a) = Λ a ->
  SET_EMPTY()[@inline][@hidden] in
  let size#59funtype a : * . set (a) -> nat = Λ a ->
  lambda (sset (a))nat return ([%Michelson {| { SIZE } |}])@(s)[@inline][@hidden] in
  let cardinal#60funtype a : * . set (a) -> nat = Λ a ->
  lambda (sset (a))nat return ([%Michelson {| { SIZE } |}])@(s)[@inline][@hidden] in
  let mem#61funtype a : * . a -> set (a) -> bool = Λ a ->
  lambda (xa)set (a) -> bool return lambda (sset (a))bool return SET_MEM(x , s)[@inline][@hidden] in
  let add#62funtype a : * . a -> set (a) -> set (a) = Λ a ->
  lambda (xa)set (a) -> set (a) return lambda (sset (a))set (a) return
  SET_ADD(x , s)[@inline][@hidden] in
  let remove#63funtype a : * . a -> set (a) -> set (a) = Λ a ->
  lambda (xa)set (a) -> set (a) return lambda (sset (a))set (a) return
  SET_REMOVE(x , s)[@inline][@hidden] in
  let update#64funtype a : * . a -> bool -> set (a) -> set (a) = Λ a ->
  lambda (xa)bool -> set (a) -> set (a) return lambda (bbool)set (a) -> set (a) return lambda (sset (a))set (a) return
  SET_UPDATE(x , b , s)[@inline][@hidden] in
  let iter#65funtype a : * . a -> unit -> set (a) -> unit = Λ a ->
  lambda (fa -> unit)set (a) -> unit return lambda (sset (a))unit return
  SET_ITER(f , s)[@inline][@hidden] in
  let fold#66funtype a : * . funtype b : * . ( b * a ) -> b -> set (a) -> b -> b =
    Λ a ->
  Λ b ->
  lambda (f( b * a ) -> b)set (a) -> b -> b return lambda (sset (a))b -> b return lambda (ib)b return
  SET_FOLD(f , s , i)[@inline][@hidden] in
  let fold_desc#67funtype a : * . funtype b : * . ( a * b ) -> b -> set (a) -> b -> b =
    Λ a ->
  Λ b ->
  lambda (f( a * b ) -> b)set (a) -> b -> b return lambda (sset (a))b -> b return lambda (ib)b return
  SET_FOLD_DESC(f , s , i)[@inline][@hidden] in
  let length#68funtype a : * . list (a) -> nat = Λ a ->
  lambda (xslist (a))nat return ([%Michelson {| { SIZE } |}])@(xs)[@inline][@hidden] in
  let size#69funtype a : * . list (a) -> nat = Λ a ->
  lambda (xslist (a))nat return ([%Michelson {| { SIZE } |}])@(xs)[@inline][@hidden] in
  let head_opt#70funtype a : * . list (a) -> option (a) = Λ a ->
  lambda (xslist (a))option (a) return  match xs with
                                         | Cons ctor_proj#2 ->
                                            match ctor_proj#2 with
                                             | ( x , _#2 ) ->
                                             Some(x)
                                         | Nil unit_proj#4 ->
                                           None(unit)[@inline][@hidden] in
  let tail_opt#71funtype a : * . list (a) -> option (list (a)) = Λ a ->
  lambda (xslist (a))option (list (a)) return  match xs with
                                                | Cons ctor_proj#5 ->
                                                   match ctor_proj#5 with
                                                    | ( _#3 , xs ) ->
                                                    Some(xs)
                                                | Nil unit_proj#7 ->
                                                  None(unit)[@inline][@hidden] in
  let map#72funtype a : * . funtype b : * . a -> b -> list (a) -> list (b) =
    Λ a ->
  Λ b ->
  lambda (fa -> b)list (a) -> list (b) return lambda (xslist (a))list (b) return
  LIST_MAP(f , xs)[@inline][@hidden] in
  let iter#73funtype a : * . a -> unit -> list (a) -> unit = Λ a ->
  lambda (fa -> unit)list (a) -> unit return lambda (xslist (a))unit return
  LIST_ITER(f , xs)[@inline][@hidden] in
  let fold#74funtype a : * . funtype b : * . ( b * a ) -> b -> list (a) -> b -> b =
    Λ a ->
  Λ b ->
  lambda (f( b * a ) -> b)list (a) -> b -> b return lambda (xslist (a))b -> b return lambda (ib)b return
  LIST_FOLD(f , xs , i)[@inline][@hidden] in
  let fold_left#75funtype a : * . funtype b : * . ( b * a ) -> b -> b -> list (a) -> b =
    Λ a ->
  Λ b ->
  lambda (f( b * a ) -> b)b -> list (a) -> b return lambda (ib)list (a) -> b return lambda (xslist (a))b return
  LIST_FOLD_LEFT(f , i , xs)[@inline][@hidden] in
  let fold_right#76funtype a : * . funtype b : * . ( a * b ) -> b -> list (a) -> b -> b =
    Λ a ->
  Λ b ->
  lambda (f( a * b ) -> b)list (a) -> b -> b return lambda (xslist (a))b -> b return lambda (ib)b return
  LIST_FOLD_RIGHT(f , xs , i)[@inline][@hidden] in
  let cons#77funtype a : * . a -> list (a) -> list (a) = Λ a ->
  lambda (xa)list (a) -> list (a) return lambda (xslist (a))list (a) return
  CONS(x , xs)[@inline][@hidden] in
  let length#78string -> nat =
    lambda (bstring)nat return ([%Michelson {| { SIZE } |}])@(b)[@inline][@hidden] in
  let concat#79string -> string -> string =
    lambda (b1string)string -> string return lambda (b2string)string return
  ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b1 , b2 ))[@inline][@hidden] in
  let sub#80nat -> nat -> string -> string =
    lambda (snat)nat -> string -> string return lambda (lnat)string -> string return lambda (bstring)string return
  ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(
  ( s ,
    l ,
    b ))[@inline][@hidden] in
  let unopt#81funtype a : * . option (a) -> a = Λ a ->
  lambda (voption (a))a return UNOPT(v)[@inline][@hidden] in
  let unopt_with_error#82funtype a : * . option (a) -> string -> a = Λ a ->
  lambda (voption (a))string -> a return lambda (sstring)a return UNOPT_WITH_ERROR
                                                                  (v ,
                                                                   s)[@inline][@hidden] in
  let pack#83funtype a : * . a -> bytes = Λ a ->
  lambda (va)bytes return ([%Michelson {| { PACK } |}])@(v)[@inline][@hidden] in
  let unpack#84funtype a : * . bytes -> option (a) = Λ a ->
  lambda (bbytes)option (a) return BYTES_UNPACK(b)[@inline][@hidden] in
  let length#85bytes -> nat =
    lambda (bbytes)nat return ([%Michelson {| { SIZE } |}])@(b)[@inline][@hidden] in
  let concat#86bytes -> bytes -> bytes =
    lambda (b1bytes)bytes -> bytes return lambda (b2bytes)bytes return
  ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b1 , b2 ))[@inline][@hidden] in
  let sub#87nat -> nat -> bytes -> bytes =
    lambda (snat)nat -> bytes -> bytes return lambda (lnat)bytes -> bytes return lambda (bbytes)bytes return
  ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(
  ( s ,
    l ,
    b ))[@inline][@hidden] in
  let blake2b#88bytes -> bytes =
    lambda (bbytes)bytes return ([%Michelson {| { BLAKE2B } |}])@(b)[@inline][@hidden] in
  let sha256#89bytes -> bytes =
    lambda (bbytes)bytes return ([%Michelson {| { SHA256 } |}])@(b)[@inline][@hidden] in
  let sha512#90bytes -> bytes =
    lambda (bbytes)bytes return ([%Michelson {| { SHA512 } |}])@(b)[@inline][@hidden] in
  let sha3#91bytes -> bytes =
    lambda (bbytes)bytes return ([%Michelson {| { SHA3 } |}])@(b)[@inline][@hidden] in
  let keccak#92bytes -> bytes =
    lambda (bbytes)bytes return ([%Michelson {| { KECCAK } |}])@(b)[@inline][@hidden] in
  let hash_key#93key -> key_hash =
    lambda (kkey)key_hash return ([%Michelson {| { HASH_KEY } |}])@(k)[@inline][@hidden] in
  let check#94key -> signature -> bytes -> bool =
    lambda (kkey)signature -> bytes -> bool return lambda (ssignature)bytes -> bool return lambda (bbytes)bool return
  ([%Michelson {| { UNPAIR ; UNPAIR ; CHECK_SIGNATURE } |}])@(( k , s , b ))[@inline][@hidden] in
  let assertbool -> unit =
    lambda (bbool)unit return ([%Michelson {| { IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } } |}])@(b)[@inline][@private][@hidden] in
  let assert_somefuntype a : * . option (a) -> unit = Λ a ->
  lambda (voption (a))unit return ([%Michelson {| { IF_NONE { PUSH string "failed assert some" ; FAILWITH } { DROP ; UNIT } } |}])@(v)[@inline][@private][@hidden] in
  let assert_nonefuntype a : * . option (a) -> unit = Λ a ->
  lambda (voption (a))unit return ([%Michelson {| { IF_NONE { UNIT } { PUSH string "failed assert none" ; FAILWITH } } |}])@(v)[@inline][@private][@hidden] in
  let absint -> nat =
    lambda (iint)nat return ([%Michelson {| { ABS } |}])@(i)[@inline][@private][@hidden] in
  let is_natint -> option (nat) =
    lambda (iint)option (nat) return ([%Michelson {| { ISNAT } |}])@(i)[@inline][@private][@hidden] in
  let truebool = TRUE()[@inline][@private][@hidden] in
  let falsebool = FALSE()[@inline][@private][@hidden] in
  let unitunit = UNIT()[@inline][@private][@hidden] in
  let failwithfuntype a : * . funtype b : * . a -> b = Λ a ->
  Λ b ->  [%Michelson {|{ FAILWITH }|}][@inline][@private][@hidden] in
  let intfuntype a : * . a -> external_int (a) = Λ a ->
  lambda (va)external_int (a) return ([%Michelson {| { INT } |}])@(v)[@inline][@private][@hidden] in
  let assert_with_errorbool -> string -> unit =
    lambda (bbool)string -> unit return lambda (sstring)unit return ([%Michelson {| { UNPAIR ; IF { DROP ; UNIT } { FAILWITH } } |}])@(
                                                                    ( b ,
                                                                      s ))[@inline][@private][@hidden] in
  let assert_some_with_errorfuntype a : * . option (a) -> string -> unit =
    Λ a ->
  lambda (voption (a))string -> unit return lambda (sstring)unit return
  ([%Michelson {| { UNPAIR ; IF_NONE { FAILWITH } { DROP 2 ; UNIT } } |}])@(
  ( v ,
    s ))[@inline][@private][@hidden] in
  let assert_none_with_errorfuntype a : * . option (a) -> string -> unit =
    Λ a ->
  lambda (voption (a))string -> unit return lambda (sstring)unit return
  ([%Michelson {| { UNPAIR ; IF_NONE { DROP ; UNIT } { DROP ; FAILWITH } } |}])@(
  ( v ,
    s ))[@inline][@private][@hidden] in
  let edivfuntype a : * . funtype b : * . a -> b -> external_ediv (a , b) =
    Λ a ->
  Λ b ->
  lambda (la)b -> external_ediv (a , b) return lambda (rb)external_ediv (a ,
  b) return ([%Michelson {| { UNPAIR ; EDIV } |}])@(( l , r ))[@inline][@private][@hidden] in
  let stub#95funtype a : * . funtype b : * . a -> b = Λ a ->
  Λ b ->  lambda (xa)b return ([%Michelson {|{ FAILWITH }|}])@(x)[@inline][@private][@hidden] in
  let run#96funtype a : * . funtype b : * . a -> b -> a -> unit = Λ a ->
  Λ b ->
  lambda (_fa -> b)a -> unit return lambda (_va)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let eval#97funtype a : * . a -> unit = Λ a ->
  lambda (_xa)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let compile_value#98funtype a : * . a -> unit = Λ a ->
  lambda (_xa)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let get_total_voting_power#99unit -> nat =
    lambda (_uunit)nat return (stub#95@{unit}@{nat})@(unit)[@inline][@hidden] in
  let failwith#100funtype a : * . funtype b : * . a -> b = Λ a ->
  Λ b ->  lambda (_va)b return (stub#95@{unit}@{b})@(unit)[@inline][@hidden] in
  let to_contract#101funtype p : * . funtype s : * . unit -> contract (p) =
    Λ p ->
  Λ s ->
  lambda (_tunit)contract (p) return (stub#95@{unit}@{contract (p)})@(unit)[@inline][@hidden] in
  let set_source#102address -> unit =
    lambda (_aaddress)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let get_storage_of_address#103address -> unit =
    lambda (_aaddress)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let get_balance#104address -> tez =
    lambda (_aaddress)tez return (stub#95@{unit}@{tez})@(unit)[@inline][@hidden] in
  let print#105string -> unit =
    lambda (_vstring)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let eprint#106string -> unit =
    lambda (_vstring)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let get_voting_power#107key_hash -> nat =
    lambda (_khkey_hash)nat return (stub#95@{unit}@{nat})@(unit)[@inline][@hidden] in
  let nth_bootstrap_contract#108nat -> address =
    lambda (_inat)address return (stub#95@{unit}@{address})@(unit)[@inline][@hidden] in
  let nth_bootstrap_account#109int -> address =
    lambda (_iint)address return (stub#95@{unit}@{address})@(unit)[@inline][@hidden] in
  let get_bootstrap_account#110nat -> ( address * key * string ) =
    lambda (_nnat)( address * key * string ) return (stub#95@{unit}@{( address * key * string )})@(unit)[@inline][@hidden] in
  let nth_bootstrap_typed_address#111funtype a : * . funtype b : * . nat -> unit =
    Λ a ->
  Λ b ->  lambda (_nnat)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let last_originations#112unit -> map (address , list (address)) =
    lambda (_uunit)map (address ,
  list (address)) return (stub#95@{unit}@{map (address ,
                         list (address))})@(unit)[@inline][@hidden] in
  let random#113funtype a : * . unit -> a = Λ a ->
  lambda (_uunit)a return (stub#95@{unit}@{a})@(unit)[@inline][@hidden] in
  let new_account#114unit -> ( string * key ) =
    lambda (_uunit)( string * key ) return (stub#95@{unit}@{( string * key )})@(unit)[@inline][@hidden] in
  let decompile#115funtype a : * . unit -> a = Λ a ->
  lambda (_munit)a return (stub#95@{unit}@{a})@(unit)[@inline][@hidden] in
  let bake_until_n_cycle_end#116nat -> unit =
    lambda (_nnat)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let cast_address#117funtype a : * . funtype b : * . address -> unit =
    Λ a ->
  Λ b ->  lambda (_aaddress)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let register_delegate#118key_hash -> unit =
    lambda (_khkey_hash)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let register_constant#119unit -> string =
    lambda (_munit)string return (stub#95@{unit}@{string})@(unit)[@inline][@hidden] in
  let to_typed_address#120funtype a : * . funtype b : * . contract (a) -> unit =
    Λ a ->
  Λ b ->  lambda (_ccontract (a))unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let constant_to_michelson_program#121string -> unit =
    lambda (_sstring)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let restore_context#122unit -> unit =
    lambda (_uunit)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let save_context#123unit -> unit =
    lambda (_uunit)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let drop_context#124unit -> unit =
    lambda (_uunit)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let to_string#125funtype a : * . a -> string = Λ a ->
  lambda (_va)string return (stub#95@{unit}@{string})@(unit)[@inline][@hidden] in
  let get_storage#126funtype p : * . funtype s : * . unit -> s = Λ p ->
  Λ s ->  lambda (_tunit)s return (stub#95@{unit}@{s})@(unit)[@inline][@hidden] in
  let set_baker_policy#127unit -> unit =
    lambda (_bpunit)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let set_baker#128address -> unit =
    lambda (_aaddress)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let size#129unit -> int =
    lambda (_cunit)int return (stub#95@{unit}@{int})@(unit)[@inline][@hidden] in
  let compile_contract#130funtype p : * . funtype s : * . ( p * s ) ->
  ( list (operation) * s ) -> unit = Λ p ->
  Λ s ->
  lambda (_f( p * s ) -> ( list (operation) * s ))unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let read_contract_from_file#131string -> unit =
    lambda (_fnstring)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let chr#132nat -> option (string) =
    lambda (_nnat)option (string) return (stub#95@{unit}@{option (string)})@(unit)[@inline][@hidden] in
  let nl#133string = "NEWLINE"[@inline][@hidden] in
  let println#134string -> unit =
    lambda (_vstring)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let transfer#135address -> unit -> tez -> unit =
    lambda (_aaddress)unit -> tez -> unit return lambda (_sunit)tez -> unit return lambda (_ttez)unit return
  (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let transfer_exn#136address -> unit -> tez -> nat =
    lambda (_aaddress)unit -> tez -> nat return lambda (_sunit)tez -> nat return lambda (_ttez)nat return
  (stub#95@{unit}@{nat})@(unit)[@inline][@hidden] in
  let log#137funtype a : * . a -> unit = Λ a ->
  lambda (_va)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let reset_state#138nat -> list (tez) -> unit =
    lambda (_nnat)list (tez) -> unit return lambda (_llist (tez))unit return
  (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let reset_state_at#139timestamp -> nat -> list (tez) -> unit =
    lambda (_ttimestamp)nat -> list (tez) -> unit return lambda (_nnat)list (tez) -> unit return lambda (_llist (tez))unit return
  (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let bootstrap_contract#140funtype p : * . funtype s : * . ( p * s ) ->
  ( list (operation) * s ) -> s -> tez -> unit = Λ p ->
  Λ s ->
  lambda (_f( p * s ) -> ( list (operation) * s ))s -> tez -> unit return lambda (_ss)tez -> unit return lambda (_ttez)unit return
  (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let mutate_value#141funtype a : * . nat -> a -> option (( a * unit )) =
    Λ a ->
  lambda (_nnat)a -> option (( a * unit )) return lambda (_va)option (( a * unit )) return
  (stub#95@{unit}@{option (( a * unit ))})@(unit)[@inline][@hidden] in
  let save_mutation#142string -> unit -> option (string) =
    lambda (_sstring)unit -> option (string) return lambda (_munit)option (string) return
  (stub#95@{unit}@{option (string)})@(unit)[@inline][@hidden] in
  let mutation_test#143funtype a : * . funtype b : * . a -> a -> b -> option (
  ( b * unit )) = Λ a ->
  Λ b ->
  lambda (_va)a -> b -> option (( b * unit )) return lambda (_fa -> b)option (
  ( b * unit )) return (stub#95@{unit}@{option (( b * unit ))})@(unit)[@inline][@hidden] in
  let mutation_test_all#144funtype a : * . funtype b : * . a -> a -> b -> list (
  ( b * unit )) = Λ a ->
  Λ b ->
  lambda (_va)a -> b -> list (( b * unit )) return lambda (_fa -> b)list (
  ( b * unit )) return (stub#95@{unit}@{list (( b * unit ))})@(unit)[@inline][@hidden] in
  let sign#145string -> bytes -> signature =
    lambda (_skstring)bytes -> signature return lambda (_dbytes)signature return
  (stub#95@{unit}@{signature})@(unit)[@inline][@hidden] in
  let add_account#146string -> key -> unit =
    lambda (_sstring)key -> unit return lambda (_kkey)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let baker_account#147( string * key ) -> option (tez) -> unit =
    lambda (_p( string * key ))option (tez) -> unit return lambda (_ooption (tez))unit return
  (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let set_big_map#148funtype a : * . funtype b : * . int -> big_map (a ,
  b) -> unit = Λ a ->
  Λ b ->
  lambda (_iint)big_map (a , b) -> unit return lambda (_mbig_map (a ,
  b))unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let create_chest#149bytes -> nat -> ( chest * chest_key ) =
    lambda (_bbytes)nat -> ( chest * chest_key ) return lambda (_nnat)( chest * chest_key ) return
  (stub#95@{unit}@{( chest * chest_key )})@(unit)[@inline][@hidden] in
  let create_chest_key#150chest -> nat -> chest_key =
    lambda (_cchest)nat -> chest_key return lambda (_nnat)chest_key return
  (stub#95@{unit}@{chest_key})@(unit)[@inline][@hidden] in
  let transfer_to_contract#151funtype p : * . contract (p) -> p -> tez -> unit =
    Λ p ->
  lambda (_ccontract (p))p -> tez -> unit return lambda (_sp)tez -> unit return lambda (_ttez)unit return
  (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let transfer_to_contract_exn#152funtype p : * . contract (p) -> p -> tez -> nat =
    Λ p ->
  lambda (_ccontract (p))p -> tez -> nat return lambda (_sp)tez -> nat return lambda (_ttez)nat return
  (stub#95@{unit}@{nat})@(unit)[@inline][@hidden] in
  let michelson_equal#153unit -> unit -> bool =
    lambda (_m1unit)unit -> bool return lambda (_m2unit)bool return (stub#95@{unit}@{bool})@(unit)[@inline][@hidden] in
  let to_entrypoint#154funtype a : * . funtype b : * . funtype c : * . string -> unit -> contract (c) =
    Λ a ->
  Λ b ->
  Λ c ->
  lambda (_sstring)unit -> contract (c) return lambda (_tunit)contract (c) return
  (stub#95@{unit}@{contract (c)})@(unit)[@inline][@hidden] in
  let originate_contract#155unit -> unit -> tez -> address =
    lambda (_cunit)unit -> tez -> address return lambda (_sunit)tez -> address return lambda (_ttez)address return
  (stub#95@{unit}@{address})@(unit)[@inline][@hidden] in
  let originate#156funtype p : * . funtype s : * . ( p * s ) -> ( list (operation) * s ) -> s -> tez ->
  ( unit * unit * int ) = Λ p ->
  Λ s ->
  lambda (_f( p * s ) -> ( list (operation) * s ))s -> tez -> ( unit * unit * int ) return lambda (_ss)tez ->
  ( unit * unit * int ) return lambda (_ttez)( unit * unit * int ) return
  (stub#95@{unit}@{( unit * unit * int )})@(unit)[@inline][@hidden] in
  let compile_contract_from_file#157string -> string -> list (string) -> unit =
    lambda (_fnstring)string -> list (string) -> unit return lambda (_estring)list (string) -> unit return lambda (_vlist (string))unit return
  (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let originate_from_file#158string -> string -> list (string) -> unit -> tez ->
  ( address * unit * int ) =
    lambda (_fnstring)string -> list (string) -> unit -> tez -> ( address * unit * int ) return lambda (_estring)list (string) -> unit -> tez ->
  ( address * unit * int ) return lambda (_vlist (string))unit -> tez ->
  ( address * unit * int ) return lambda (_sunit)tez -> ( address * unit * int ) return lambda (_ttez)
  ( address * unit * int ) return (stub#95@{unit}@{( address * unit * int )})@(unit)[@inline][@hidden] in
  let x#159int = 41 in
  let xint = 1 in
  let x#160int = x in
  let y#161int = x#159 in let uint = ADD(x#160 , y#161) in let xint = u in unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias8.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias9.mligo" ] ;
  [%expect {xxx|
  let get_balance#8unit -> tez =
    lambda (_uunit)tez return ([%Michelson {| { DROP ; BALANCE } |}])@(unit)[@inline][@hidden] in
  let get_amount#9unit -> tez =
    lambda (_uunit)tez return ([%Michelson {| { DROP ; AMOUNT } |}])@(unit)[@inline][@hidden] in
  let get_now#10unit -> timestamp =
    lambda (_uunit)timestamp return ([%Michelson {| { DROP ; NOW } |}])@(unit)[@inline][@hidden] in
  let get_sender#11unit -> address =
    lambda (_uunit)address return ([%Michelson {| { DROP ; SENDER } |}])@(unit)[@inline][@hidden] in
  let get_source#12unit -> address =
    lambda (_uunit)address return ([%Michelson {| { DROP ; SOURCE } |}])@(unit)[@inline][@hidden] in
  let get_level#13unit -> nat =
    lambda (_uunit)nat return ([%Michelson {| { DROP ; LEVEL } |}])@(unit)[@inline][@hidden] in
  let get_self_address#14unit -> address =
    lambda (_uunit)address return SELF_ADDRESS()[@inline][@hidden] in
  let get_chain_id#15unit -> chain_id =
    lambda (_uunit)chain_id return ([%Michelson {| { DROP ; CHAIN_ID } |}])@(unit)[@inline][@hidden] in
  let get_total_voting_power#16unit -> nat =
    lambda (_uunit)nat return ([%Michelson {| { DROP ; TOTAL_VOTING_POWER } |}])@(unit)[@inline][@hidden] in
  let get_min_block_time#17unit -> nat =
    lambda (_uunit)nat return ([%Michelson {| { DROP; MIN_BLOCK_TIME } |}])@(unit)[@inline][@hidden] in
  let voting_power#18key_hash -> nat =
    lambda (khkey_hash)nat return ([%Michelson {| { VOTING_POWER } |}])@(kh)[@inline][@hidden] in
  let address#19funtype a : * . contract (a) -> address = Λ a ->
  lambda (ccontract (a))address return ADDRESS(c)[@inline][@hidden] in
  let implicit_account#20key_hash -> contract (unit) =
    lambda (khkey_hash)contract (unit) return IMPLICIT_ACCOUNT(kh)[@inline][@hidden] in
  let join_tickets#21funtype a : * . ( ticket (a) * ticket (a) ) -> option (ticket (a)) =
    Λ a ->
  lambda (t( ticket (a) * ticket (a) ))option (ticket (a)) return ([%Michelson {| { JOIN_TICKETS } |}])@(t)[@inline][@hidden] in
  let read_ticket#22funtype a : * . ticket (a) -> ( ( address * ( a * nat ) ) * ticket (a) ) =
    Λ a ->
  lambda (tticket (a))( ( address * ( a * nat ) ) * ticket (a) ) return
  ([%Michelson {| { READ_TICKET ; PAIR } |}])@(t)[@inline][@hidden] in
  let never#23funtype a : * . never -> a = Λ a ->
  lambda (nnever)a return ([%Michelson {| { NEVER } |}])@(n)[@inline][@hidden] in
  let pairing_check#24list (( bls12_381_g1 * bls12_381_g2 )) -> bool =
    lambda (llist (( bls12_381_g1 * bls12_381_g2 )))bool return ([%Michelson {| { PAIRING_CHECK } |}])@(l)[@inline][@hidden] in
  let constant#25funtype a : * . string -> a = Λ a ->
  lambda (sstring)a return GLOBAL_CONSTANT(s)[@inline][@hidden] in
  let set_delegate#26option (key_hash) -> operation =
    lambda (ooption (key_hash))operation return SET_DELEGATE(o)[@inline][@hidden] in
  let get_contract#27funtype a : * . address -> contract (a) = Λ a ->
  lambda (aaddress)contract (a) return CONTRACT(a)[@inline][@hidden] in
  let get_contract_opt#28funtype a : * . address -> option (contract (a)) =
    Λ a ->
  lambda (aaddress)option (contract (a)) return CONTRACT_OPT(a)[@inline][@hidden] in
  let get_contract_with_error#29funtype a : * . address -> string -> contract (a) =
    Λ a ->
  lambda (aaddress)string -> contract (a) return lambda (sstring)contract (a) return
  CONTRACT_WITH_ERROR(a , s)[@inline][@hidden] in
  let create_ticket#30funtype a : * . a -> nat -> ticket (a) = Λ a ->
  lambda (va)nat -> ticket (a) return lambda (nnat)ticket (a) return ([%Michelson {| { UNPAIR ; TICKET } |}])@(
                                                                     ( v ,
                                                                      n ))[@inline][@hidden] in
  let transaction#31funtype a : * . a -> tez -> contract (a) -> operation =
    Λ a ->
  lambda (aa)tez -> contract (a) -> operation return lambda (mutez)contract (a) -> operation return lambda (ccontract (a))operation return
  CALL(a , mu , c)[@inline][@hidden] in
  let open_chest#32chest_key -> chest -> nat -> sum[Fail_decrypt -> unit , Fail_timelock -> unit , Ok_opening -> bytes] =
    lambda (ckchest_key)chest -> nat -> sum[Fail_decrypt -> unit , Fail_timelock -> unit , Ok_opening -> bytes] return lambda (cchest)nat ->
  sum[Fail_decrypt -> unit , Fail_timelock -> unit , Ok_opening -> bytes] return lambda (nnat)
  sum[Fail_decrypt -> unit , Fail_timelock -> unit , Ok_opening -> bytes] return
  OPEN_CHEST(ck , c , n)[@inline][@hidden] in
  let call_view#33funtype a : * . funtype b : * . string -> a -> address -> option (b) =
    Λ a ->
  Λ b ->
  lambda (sstring)a -> address -> option (b) return lambda (xa)address -> option (b) return lambda (aaddress)option (b) return
  VIEW(s , x , a)[@inline][@hidden] in
  let split_ticket#34funtype a : * . ticket (a) -> ( nat * nat ) -> option (
  ( ticket (a) * ticket (a) )) = Λ a ->
  lambda (tticket (a))( nat * nat ) -> option (( ticket (a) * ticket (a) )) return lambda (p
  ( nat * nat ))option (( ticket (a) * ticket (a) )) return ([%Michelson {| { UNPAIR ; SPLIT_TICKET } |}])@(
                                                            ( t ,
                                                              p ))[@inline][@hidden] in
  let xor#35nat -> nat -> nat =
    lambda (lnat)nat -> nat return lambda (rnat)nat return XOR(l , r)[@inline][@hidden] in
  let shift_left#36nat -> nat -> nat =
    lambda (lnat)nat -> nat return lambda (rnat)nat return LSL(l , r)[@inline][@hidden] in
  let shift_right#37nat -> nat -> nat =
    lambda (lnat)nat -> nat return lambda (rnat)nat return LSR(l , r)[@inline][@hidden] in
  let empty#38funtype k : * . funtype v : * . big_map (k , v) = Λ k ->
  Λ v ->  BIG_MAP_EMPTY()[@inline][@hidden] in
  let mem#39funtype k : * . funtype v : * . k -> big_map (k , v) -> bool =
    Λ k ->
  Λ v ->
  lambda (kk)big_map (k , v) -> bool return lambda (mbig_map (k ,
  v))bool return ([%Michelson {| { UNPAIR ; MEM } |}])@(( k , m ))[@inline][@hidden] in
  let add#40funtype k : * . funtype v : * . k -> v -> big_map (k ,
  v) -> big_map (k , v) = Λ k ->
  Λ v ->
  lambda (kk)v -> big_map (k , v) -> big_map (k ,
  v) return lambda (vv)big_map (k , v) -> big_map (k ,
  v) return lambda (mbig_map (k , v))big_map (k , v) return MAP_ADD(k , v , m)[@inline][@hidden] in
  let remove#41funtype k : * . funtype v : * . k -> big_map (k ,
  v) -> big_map (k , v) = Λ k ->
  Λ v ->
  lambda (kk)big_map (k , v) -> big_map (k , v) return lambda (mbig_map (k ,
  v))big_map (k , v) return MAP_REMOVE(k , m)[@inline][@hidden] in
  let update#42funtype k : * . funtype v : * . k -> option (v) -> big_map (k ,
  v) -> big_map (k , v) = Λ k ->
  Λ v ->
  lambda (kk)option (v) -> big_map (k , v) -> big_map (k ,
  v) return lambda (voption (v))big_map (k , v) -> big_map (k ,
  v) return lambda (mbig_map (k , v))big_map (k ,
  v) return MAP_UPDATE(k , v , m)[@inline][@hidden] in
  let get_and_update#43funtype k : * . funtype v : * . k -> option (v) -> big_map (k ,
  v) -> ( option (v) * big_map (k , v) ) = Λ k ->
  Λ v ->
  lambda (kk)option (v) -> big_map (k ,
  v) -> ( option (v) * big_map (k , v) ) return lambda (voption (v))big_map (k ,
  v) -> ( option (v) * big_map (k , v) ) return lambda (mbig_map (k ,
  v))( option (v) * big_map (k , v) ) return BIG_MAP_GET_AND_UPDATE(k , v , m)[@inline][@hidden] in
  let find_opt#44funtype k : * . funtype v : * . k -> big_map (k ,
  v) -> option (v) = Λ k ->
  Λ v ->
  lambda (kk)big_map (k , v) -> option (v) return lambda (mbig_map (k ,
  v))option (v) return MAP_FIND_OPT(k , m)[@inline][@hidden] in
  let find#45funtype k : * . funtype v : * . k -> big_map (k , v) -> v =
    Λ k ->
  Λ v ->
  lambda (kk)big_map (k , v) -> v return lambda (mbig_map (k ,
  v))v return MAP_FIND(k , m)[@inline][@hidden] in
  let empty#46funtype k : * . funtype v : * . map (k , v) = Λ k ->
  Λ v ->  MAP_EMPTY()[@inline][@hidden] in
  let size#47funtype k : * . funtype v : * . map (k , v) -> nat = Λ k ->
  Λ v ->  lambda (mmap (k , v))nat return ([%Michelson {| { SIZE } |}])@(m)[@inline][@hidden] in
  let mem#48funtype k : * . funtype v : * . k -> map (k , v) -> bool = Λ k ->
  Λ v ->
  lambda (kk)map (k , v) -> bool return lambda (mmap (k ,
  v))bool return ([%Michelson {| { UNPAIR ; MEM } |}])@(( k , m ))[@inline][@hidden] in
  let add#49funtype k : * . funtype v : * . k -> v -> map (k , v) -> map (k ,
  v) = Λ k ->
  Λ v ->
  lambda (kk)v -> map (k , v) -> map (k , v) return lambda (vv)map (k ,
  v) -> map (k , v) return lambda (mmap (k , v))map (k ,
  v) return MAP_ADD(k , v , m)[@inline][@hidden] in
  let remove#50funtype k : * . funtype v : * . k -> map (k , v) -> map (k ,
  v) = Λ k ->
  Λ v ->
  lambda (kk)map (k , v) -> map (k , v) return lambda (mmap (k , v))map (k ,
  v) return MAP_REMOVE(k , m)[@inline][@hidden] in
  let update#51funtype k : * . funtype v : * . k -> option (v) -> map (k ,
  v) -> map (k , v) = Λ k ->
  Λ v ->
  lambda (kk)option (v) -> map (k , v) -> map (k ,
  v) return lambda (voption (v))map (k , v) -> map (k ,
  v) return lambda (mmap (k , v))map (k , v) return MAP_UPDATE(k , v , m)[@inline][@hidden] in
  let get_and_update#52funtype k : * . funtype v : * . k -> option (v) -> map (k ,
  v) -> ( option (v) * map (k , v) ) = Λ k ->
  Λ v ->
  lambda (kk)option (v) -> map (k ,
  v) -> ( option (v) * map (k , v) ) return lambda (voption (v))map (k ,
  v) -> ( option (v) * map (k , v) ) return lambda (mmap (k ,
  v))( option (v) * map (k , v) ) return MAP_GET_AND_UPDATE(k , v , m)[@inline][@hidden] in
  let find#53funtype k : * . funtype v : * . k -> map (k , v) -> v = Λ k ->
  Λ v ->
  lambda (kk)map (k , v) -> v return lambda (mmap (k ,
  v))v return MAP_FIND(k , m)[@inline][@hidden] in
  let find_opt#54funtype k : * . funtype v : * . k -> map (k ,
  v) -> option (v) = Λ k ->
  Λ v ->
  lambda (kk)map (k , v) -> option (v) return lambda (mmap (k ,
  v))option (v) return MAP_FIND_OPT(k , m)[@inline][@hidden] in
  let iter#55funtype k : * . funtype v : * . ( k * v ) -> unit -> map (k ,
  v) -> unit = Λ k ->
  Λ v ->
  lambda (f( k * v ) -> unit)map (k , v) -> unit return lambda (mmap (k ,
  v))unit return MAP_ITER(f , m)[@inline][@hidden] in
  let map#56funtype k : * . funtype v : * . funtype w : * . ( k * v ) -> w -> map (k ,
  v) -> map (k , w) = Λ k ->
  Λ v ->
  Λ w ->
  lambda (f( k * v ) -> w)map (k , v) -> map (k , w) return lambda (mmap (k ,
  v))map (k , w) return MAP_MAP(f , m)[@inline][@hidden] in
  let fold#57funtype k : * . funtype v : * . funtype c : * . ( c * ( k * v ) ) -> c -> map (k ,
  v) -> c -> c = Λ k ->
  Λ v ->
  Λ c ->
  lambda (f( c * ( k * v ) ) -> c)map (k ,
  v) -> c -> c return lambda (mmap (k ,
  v))c -> c return lambda (ic)c return MAP_FOLD(f , m , i)[@inline][@hidden] in
  let empty#58funtype a : * . set (a) = Λ a ->
  SET_EMPTY()[@inline][@hidden] in
  let size#59funtype a : * . set (a) -> nat = Λ a ->
  lambda (sset (a))nat return ([%Michelson {| { SIZE } |}])@(s)[@inline][@hidden] in
  let cardinal#60funtype a : * . set (a) -> nat = Λ a ->
  lambda (sset (a))nat return ([%Michelson {| { SIZE } |}])@(s)[@inline][@hidden] in
  let mem#61funtype a : * . a -> set (a) -> bool = Λ a ->
  lambda (xa)set (a) -> bool return lambda (sset (a))bool return SET_MEM(x , s)[@inline][@hidden] in
  let add#62funtype a : * . a -> set (a) -> set (a) = Λ a ->
  lambda (xa)set (a) -> set (a) return lambda (sset (a))set (a) return
  SET_ADD(x , s)[@inline][@hidden] in
  let remove#63funtype a : * . a -> set (a) -> set (a) = Λ a ->
  lambda (xa)set (a) -> set (a) return lambda (sset (a))set (a) return
  SET_REMOVE(x , s)[@inline][@hidden] in
  let update#64funtype a : * . a -> bool -> set (a) -> set (a) = Λ a ->
  lambda (xa)bool -> set (a) -> set (a) return lambda (bbool)set (a) -> set (a) return lambda (sset (a))set (a) return
  SET_UPDATE(x , b , s)[@inline][@hidden] in
  let iter#65funtype a : * . a -> unit -> set (a) -> unit = Λ a ->
  lambda (fa -> unit)set (a) -> unit return lambda (sset (a))unit return
  SET_ITER(f , s)[@inline][@hidden] in
  let fold#66funtype a : * . funtype b : * . ( b * a ) -> b -> set (a) -> b -> b =
    Λ a ->
  Λ b ->
  lambda (f( b * a ) -> b)set (a) -> b -> b return lambda (sset (a))b -> b return lambda (ib)b return
  SET_FOLD(f , s , i)[@inline][@hidden] in
  let fold_desc#67funtype a : * . funtype b : * . ( a * b ) -> b -> set (a) -> b -> b =
    Λ a ->
  Λ b ->
  lambda (f( a * b ) -> b)set (a) -> b -> b return lambda (sset (a))b -> b return lambda (ib)b return
  SET_FOLD_DESC(f , s , i)[@inline][@hidden] in
  let length#68funtype a : * . list (a) -> nat = Λ a ->
  lambda (xslist (a))nat return ([%Michelson {| { SIZE } |}])@(xs)[@inline][@hidden] in
  let size#69funtype a : * . list (a) -> nat = Λ a ->
  lambda (xslist (a))nat return ([%Michelson {| { SIZE } |}])@(xs)[@inline][@hidden] in
  let head_opt#70funtype a : * . list (a) -> option (a) = Λ a ->
  lambda (xslist (a))option (a) return  match xs with
                                         | Cons ctor_proj#2 ->
                                            match ctor_proj#2 with
                                             | ( x , _#2 ) ->
                                             Some(x)
                                         | Nil unit_proj#4 ->
                                           None(unit)[@inline][@hidden] in
  let tail_opt#71funtype a : * . list (a) -> option (list (a)) = Λ a ->
  lambda (xslist (a))option (list (a)) return  match xs with
                                                | Cons ctor_proj#5 ->
                                                   match ctor_proj#5 with
                                                    | ( _#3 , xs ) ->
                                                    Some(xs)
                                                | Nil unit_proj#7 ->
                                                  None(unit)[@inline][@hidden] in
  let map#72funtype a : * . funtype b : * . a -> b -> list (a) -> list (b) =
    Λ a ->
  Λ b ->
  lambda (fa -> b)list (a) -> list (b) return lambda (xslist (a))list (b) return
  LIST_MAP(f , xs)[@inline][@hidden] in
  let iter#73funtype a : * . a -> unit -> list (a) -> unit = Λ a ->
  lambda (fa -> unit)list (a) -> unit return lambda (xslist (a))unit return
  LIST_ITER(f , xs)[@inline][@hidden] in
  let fold#74funtype a : * . funtype b : * . ( b * a ) -> b -> list (a) -> b -> b =
    Λ a ->
  Λ b ->
  lambda (f( b * a ) -> b)list (a) -> b -> b return lambda (xslist (a))b -> b return lambda (ib)b return
  LIST_FOLD(f , xs , i)[@inline][@hidden] in
  let fold_left#75funtype a : * . funtype b : * . ( b * a ) -> b -> b -> list (a) -> b =
    Λ a ->
  Λ b ->
  lambda (f( b * a ) -> b)b -> list (a) -> b return lambda (ib)list (a) -> b return lambda (xslist (a))b return
  LIST_FOLD_LEFT(f , i , xs)[@inline][@hidden] in
  let fold_right#76funtype a : * . funtype b : * . ( a * b ) -> b -> list (a) -> b -> b =
    Λ a ->
  Λ b ->
  lambda (f( a * b ) -> b)list (a) -> b -> b return lambda (xslist (a))b -> b return lambda (ib)b return
  LIST_FOLD_RIGHT(f , xs , i)[@inline][@hidden] in
  let cons#77funtype a : * . a -> list (a) -> list (a) = Λ a ->
  lambda (xa)list (a) -> list (a) return lambda (xslist (a))list (a) return
  CONS(x , xs)[@inline][@hidden] in
  let length#78string -> nat =
    lambda (bstring)nat return ([%Michelson {| { SIZE } |}])@(b)[@inline][@hidden] in
  let concat#79string -> string -> string =
    lambda (b1string)string -> string return lambda (b2string)string return
  ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b1 , b2 ))[@inline][@hidden] in
  let sub#80nat -> nat -> string -> string =
    lambda (snat)nat -> string -> string return lambda (lnat)string -> string return lambda (bstring)string return
  ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(
  ( s ,
    l ,
    b ))[@inline][@hidden] in
  let unopt#81funtype a : * . option (a) -> a = Λ a ->
  lambda (voption (a))a return UNOPT(v)[@inline][@hidden] in
  let unopt_with_error#82funtype a : * . option (a) -> string -> a = Λ a ->
  lambda (voption (a))string -> a return lambda (sstring)a return UNOPT_WITH_ERROR
                                                                  (v ,
                                                                   s)[@inline][@hidden] in
  let pack#83funtype a : * . a -> bytes = Λ a ->
  lambda (va)bytes return ([%Michelson {| { PACK } |}])@(v)[@inline][@hidden] in
  let unpack#84funtype a : * . bytes -> option (a) = Λ a ->
  lambda (bbytes)option (a) return BYTES_UNPACK(b)[@inline][@hidden] in
  let length#85bytes -> nat =
    lambda (bbytes)nat return ([%Michelson {| { SIZE } |}])@(b)[@inline][@hidden] in
  let concat#86bytes -> bytes -> bytes =
    lambda (b1bytes)bytes -> bytes return lambda (b2bytes)bytes return
  ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b1 , b2 ))[@inline][@hidden] in
  let sub#87nat -> nat -> bytes -> bytes =
    lambda (snat)nat -> bytes -> bytes return lambda (lnat)bytes -> bytes return lambda (bbytes)bytes return
  ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(
  ( s ,
    l ,
    b ))[@inline][@hidden] in
  let blake2b#88bytes -> bytes =
    lambda (bbytes)bytes return ([%Michelson {| { BLAKE2B } |}])@(b)[@inline][@hidden] in
  let sha256#89bytes -> bytes =
    lambda (bbytes)bytes return ([%Michelson {| { SHA256 } |}])@(b)[@inline][@hidden] in
  let sha512#90bytes -> bytes =
    lambda (bbytes)bytes return ([%Michelson {| { SHA512 } |}])@(b)[@inline][@hidden] in
  let sha3#91bytes -> bytes =
    lambda (bbytes)bytes return ([%Michelson {| { SHA3 } |}])@(b)[@inline][@hidden] in
  let keccak#92bytes -> bytes =
    lambda (bbytes)bytes return ([%Michelson {| { KECCAK } |}])@(b)[@inline][@hidden] in
  let hash_key#93key -> key_hash =
    lambda (kkey)key_hash return ([%Michelson {| { HASH_KEY } |}])@(k)[@inline][@hidden] in
  let check#94key -> signature -> bytes -> bool =
    lambda (kkey)signature -> bytes -> bool return lambda (ssignature)bytes -> bool return lambda (bbytes)bool return
  ([%Michelson {| { UNPAIR ; UNPAIR ; CHECK_SIGNATURE } |}])@(( k , s , b ))[@inline][@hidden] in
  let assertbool -> unit =
    lambda (bbool)unit return ([%Michelson {| { IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } } |}])@(b)[@inline][@private][@hidden] in
  let assert_somefuntype a : * . option (a) -> unit = Λ a ->
  lambda (voption (a))unit return ([%Michelson {| { IF_NONE { PUSH string "failed assert some" ; FAILWITH } { DROP ; UNIT } } |}])@(v)[@inline][@private][@hidden] in
  let assert_nonefuntype a : * . option (a) -> unit = Λ a ->
  lambda (voption (a))unit return ([%Michelson {| { IF_NONE { UNIT } { PUSH string "failed assert none" ; FAILWITH } } |}])@(v)[@inline][@private][@hidden] in
  let absint -> nat =
    lambda (iint)nat return ([%Michelson {| { ABS } |}])@(i)[@inline][@private][@hidden] in
  let is_natint -> option (nat) =
    lambda (iint)option (nat) return ([%Michelson {| { ISNAT } |}])@(i)[@inline][@private][@hidden] in
  let truebool = TRUE()[@inline][@private][@hidden] in
  let falsebool = FALSE()[@inline][@private][@hidden] in
  let unitunit = UNIT()[@inline][@private][@hidden] in
  let failwithfuntype a : * . funtype b : * . a -> b = Λ a ->
  Λ b ->  [%Michelson {|{ FAILWITH }|}][@inline][@private][@hidden] in
  let intfuntype a : * . a -> external_int (a) = Λ a ->
  lambda (va)external_int (a) return ([%Michelson {| { INT } |}])@(v)[@inline][@private][@hidden] in
  let assert_with_errorbool -> string -> unit =
    lambda (bbool)string -> unit return lambda (sstring)unit return ([%Michelson {| { UNPAIR ; IF { DROP ; UNIT } { FAILWITH } } |}])@(
                                                                    ( b ,
                                                                      s ))[@inline][@private][@hidden] in
  let assert_some_with_errorfuntype a : * . option (a) -> string -> unit =
    Λ a ->
  lambda (voption (a))string -> unit return lambda (sstring)unit return
  ([%Michelson {| { UNPAIR ; IF_NONE { FAILWITH } { DROP 2 ; UNIT } } |}])@(
  ( v ,
    s ))[@inline][@private][@hidden] in
  let assert_none_with_errorfuntype a : * . option (a) -> string -> unit =
    Λ a ->
  lambda (voption (a))string -> unit return lambda (sstring)unit return
  ([%Michelson {| { UNPAIR ; IF_NONE { DROP ; UNIT } { DROP ; FAILWITH } } |}])@(
  ( v ,
    s ))[@inline][@private][@hidden] in
  let edivfuntype a : * . funtype b : * . a -> b -> external_ediv (a , b) =
    Λ a ->
  Λ b ->
  lambda (la)b -> external_ediv (a , b) return lambda (rb)external_ediv (a ,
  b) return ([%Michelson {| { UNPAIR ; EDIV } |}])@(( l , r ))[@inline][@private][@hidden] in
  let stub#95funtype a : * . funtype b : * . a -> b = Λ a ->
  Λ b ->  lambda (xa)b return ([%Michelson {|{ FAILWITH }|}])@(x)[@inline][@private][@hidden] in
  let run#96funtype a : * . funtype b : * . a -> b -> a -> unit = Λ a ->
  Λ b ->
  lambda (_fa -> b)a -> unit return lambda (_va)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let eval#97funtype a : * . a -> unit = Λ a ->
  lambda (_xa)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let compile_value#98funtype a : * . a -> unit = Λ a ->
  lambda (_xa)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let get_total_voting_power#99unit -> nat =
    lambda (_uunit)nat return (stub#95@{unit}@{nat})@(unit)[@inline][@hidden] in
  let failwith#100funtype a : * . funtype b : * . a -> b = Λ a ->
  Λ b ->  lambda (_va)b return (stub#95@{unit}@{b})@(unit)[@inline][@hidden] in
  let to_contract#101funtype p : * . funtype s : * . unit -> contract (p) =
    Λ p ->
  Λ s ->
  lambda (_tunit)contract (p) return (stub#95@{unit}@{contract (p)})@(unit)[@inline][@hidden] in
  let set_source#102address -> unit =
    lambda (_aaddress)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let get_storage_of_address#103address -> unit =
    lambda (_aaddress)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let get_balance#104address -> tez =
    lambda (_aaddress)tez return (stub#95@{unit}@{tez})@(unit)[@inline][@hidden] in
  let print#105string -> unit =
    lambda (_vstring)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let eprint#106string -> unit =
    lambda (_vstring)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let get_voting_power#107key_hash -> nat =
    lambda (_khkey_hash)nat return (stub#95@{unit}@{nat})@(unit)[@inline][@hidden] in
  let nth_bootstrap_contract#108nat -> address =
    lambda (_inat)address return (stub#95@{unit}@{address})@(unit)[@inline][@hidden] in
  let nth_bootstrap_account#109int -> address =
    lambda (_iint)address return (stub#95@{unit}@{address})@(unit)[@inline][@hidden] in
  let get_bootstrap_account#110nat -> ( address * key * string ) =
    lambda (_nnat)( address * key * string ) return (stub#95@{unit}@{( address * key * string )})@(unit)[@inline][@hidden] in
  let nth_bootstrap_typed_address#111funtype a : * . funtype b : * . nat -> unit =
    Λ a ->
  Λ b ->  lambda (_nnat)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let last_originations#112unit -> map (address , list (address)) =
    lambda (_uunit)map (address ,
  list (address)) return (stub#95@{unit}@{map (address ,
                         list (address))})@(unit)[@inline][@hidden] in
  let random#113funtype a : * . unit -> a = Λ a ->
  lambda (_uunit)a return (stub#95@{unit}@{a})@(unit)[@inline][@hidden] in
  let new_account#114unit -> ( string * key ) =
    lambda (_uunit)( string * key ) return (stub#95@{unit}@{( string * key )})@(unit)[@inline][@hidden] in
  let decompile#115funtype a : * . unit -> a = Λ a ->
  lambda (_munit)a return (stub#95@{unit}@{a})@(unit)[@inline][@hidden] in
  let bake_until_n_cycle_end#116nat -> unit =
    lambda (_nnat)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let cast_address#117funtype a : * . funtype b : * . address -> unit =
    Λ a ->
  Λ b ->  lambda (_aaddress)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let register_delegate#118key_hash -> unit =
    lambda (_khkey_hash)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let register_constant#119unit -> string =
    lambda (_munit)string return (stub#95@{unit}@{string})@(unit)[@inline][@hidden] in
  let to_typed_address#120funtype a : * . funtype b : * . contract (a) -> unit =
    Λ a ->
  Λ b ->  lambda (_ccontract (a))unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let constant_to_michelson_program#121string -> unit =
    lambda (_sstring)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let restore_context#122unit -> unit =
    lambda (_uunit)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let save_context#123unit -> unit =
    lambda (_uunit)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let drop_context#124unit -> unit =
    lambda (_uunit)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let to_string#125funtype a : * . a -> string = Λ a ->
  lambda (_va)string return (stub#95@{unit}@{string})@(unit)[@inline][@hidden] in
  let get_storage#126funtype p : * . funtype s : * . unit -> s = Λ p ->
  Λ s ->  lambda (_tunit)s return (stub#95@{unit}@{s})@(unit)[@inline][@hidden] in
  let set_baker_policy#127unit -> unit =
    lambda (_bpunit)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let set_baker#128address -> unit =
    lambda (_aaddress)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let size#129unit -> int =
    lambda (_cunit)int return (stub#95@{unit}@{int})@(unit)[@inline][@hidden] in
  let compile_contract#130funtype p : * . funtype s : * . ( p * s ) ->
  ( list (operation) * s ) -> unit = Λ p ->
  Λ s ->
  lambda (_f( p * s ) -> ( list (operation) * s ))unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let read_contract_from_file#131string -> unit =
    lambda (_fnstring)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let chr#132nat -> option (string) =
    lambda (_nnat)option (string) return (stub#95@{unit}@{option (string)})@(unit)[@inline][@hidden] in
  let nl#133string = "NEWLINE"[@inline][@hidden] in
  let println#134string -> unit =
    lambda (_vstring)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let transfer#135address -> unit -> tez -> unit =
    lambda (_aaddress)unit -> tez -> unit return lambda (_sunit)tez -> unit return lambda (_ttez)unit return
  (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let transfer_exn#136address -> unit -> tez -> nat =
    lambda (_aaddress)unit -> tez -> nat return lambda (_sunit)tez -> nat return lambda (_ttez)nat return
  (stub#95@{unit}@{nat})@(unit)[@inline][@hidden] in
  let log#137funtype a : * . a -> unit = Λ a ->
  lambda (_va)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let reset_state#138nat -> list (tez) -> unit =
    lambda (_nnat)list (tez) -> unit return lambda (_llist (tez))unit return
  (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let reset_state_at#139timestamp -> nat -> list (tez) -> unit =
    lambda (_ttimestamp)nat -> list (tez) -> unit return lambda (_nnat)list (tez) -> unit return lambda (_llist (tez))unit return
  (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let bootstrap_contract#140funtype p : * . funtype s : * . ( p * s ) ->
  ( list (operation) * s ) -> s -> tez -> unit = Λ p ->
  Λ s ->
  lambda (_f( p * s ) -> ( list (operation) * s ))s -> tez -> unit return lambda (_ss)tez -> unit return lambda (_ttez)unit return
  (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let mutate_value#141funtype a : * . nat -> a -> option (( a * unit )) =
    Λ a ->
  lambda (_nnat)a -> option (( a * unit )) return lambda (_va)option (( a * unit )) return
  (stub#95@{unit}@{option (( a * unit ))})@(unit)[@inline][@hidden] in
  let save_mutation#142string -> unit -> option (string) =
    lambda (_sstring)unit -> option (string) return lambda (_munit)option (string) return
  (stub#95@{unit}@{option (string)})@(unit)[@inline][@hidden] in
  let mutation_test#143funtype a : * . funtype b : * . a -> a -> b -> option (
  ( b * unit )) = Λ a ->
  Λ b ->
  lambda (_va)a -> b -> option (( b * unit )) return lambda (_fa -> b)option (
  ( b * unit )) return (stub#95@{unit}@{option (( b * unit ))})@(unit)[@inline][@hidden] in
  let mutation_test_all#144funtype a : * . funtype b : * . a -> a -> b -> list (
  ( b * unit )) = Λ a ->
  Λ b ->
  lambda (_va)a -> b -> list (( b * unit )) return lambda (_fa -> b)list (
  ( b * unit )) return (stub#95@{unit}@{list (( b * unit ))})@(unit)[@inline][@hidden] in
  let sign#145string -> bytes -> signature =
    lambda (_skstring)bytes -> signature return lambda (_dbytes)signature return
  (stub#95@{unit}@{signature})@(unit)[@inline][@hidden] in
  let add_account#146string -> key -> unit =
    lambda (_sstring)key -> unit return lambda (_kkey)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let baker_account#147( string * key ) -> option (tez) -> unit =
    lambda (_p( string * key ))option (tez) -> unit return lambda (_ooption (tez))unit return
  (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let set_big_map#148funtype a : * . funtype b : * . int -> big_map (a ,
  b) -> unit = Λ a ->
  Λ b ->
  lambda (_iint)big_map (a , b) -> unit return lambda (_mbig_map (a ,
  b))unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let create_chest#149bytes -> nat -> ( chest * chest_key ) =
    lambda (_bbytes)nat -> ( chest * chest_key ) return lambda (_nnat)( chest * chest_key ) return
  (stub#95@{unit}@{( chest * chest_key )})@(unit)[@inline][@hidden] in
  let create_chest_key#150chest -> nat -> chest_key =
    lambda (_cchest)nat -> chest_key return lambda (_nnat)chest_key return
  (stub#95@{unit}@{chest_key})@(unit)[@inline][@hidden] in
  let transfer_to_contract#151funtype p : * . contract (p) -> p -> tez -> unit =
    Λ p ->
  lambda (_ccontract (p))p -> tez -> unit return lambda (_sp)tez -> unit return lambda (_ttez)unit return
  (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let transfer_to_contract_exn#152funtype p : * . contract (p) -> p -> tez -> nat =
    Λ p ->
  lambda (_ccontract (p))p -> tez -> nat return lambda (_sp)tez -> nat return lambda (_ttez)nat return
  (stub#95@{unit}@{nat})@(unit)[@inline][@hidden] in
  let michelson_equal#153unit -> unit -> bool =
    lambda (_m1unit)unit -> bool return lambda (_m2unit)bool return (stub#95@{unit}@{bool})@(unit)[@inline][@hidden] in
  let to_entrypoint#154funtype a : * . funtype b : * . funtype c : * . string -> unit -> contract (c) =
    Λ a ->
  Λ b ->
  Λ c ->
  lambda (_sstring)unit -> contract (c) return lambda (_tunit)contract (c) return
  (stub#95@{unit}@{contract (c)})@(unit)[@inline][@hidden] in
  let originate_contract#155unit -> unit -> tez -> address =
    lambda (_cunit)unit -> tez -> address return lambda (_sunit)tez -> address return lambda (_ttez)address return
  (stub#95@{unit}@{address})@(unit)[@inline][@hidden] in
  let originate#156funtype p : * . funtype s : * . ( p * s ) -> ( list (operation) * s ) -> s -> tez ->
  ( unit * unit * int ) = Λ p ->
  Λ s ->
  lambda (_f( p * s ) -> ( list (operation) * s ))s -> tez -> ( unit * unit * int ) return lambda (_ss)tez ->
  ( unit * unit * int ) return lambda (_ttez)( unit * unit * int ) return
  (stub#95@{unit}@{( unit * unit * int )})@(unit)[@inline][@hidden] in
  let compile_contract_from_file#157string -> string -> list (string) -> unit =
    lambda (_fnstring)string -> list (string) -> unit return lambda (_estring)list (string) -> unit return lambda (_vlist (string))unit return
  (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let originate_from_file#158string -> string -> list (string) -> unit -> tez ->
  ( address * unit * int ) =
    lambda (_fnstring)string -> list (string) -> unit -> tez -> ( address * unit * int ) return lambda (_estring)list (string) -> unit -> tez ->
  ( address * unit * int ) return lambda (_vlist (string))unit -> tez ->
  ( address * unit * int ) return lambda (_sunit)tez -> ( address * unit * int ) return lambda (_ttez)
  ( address * unit * int ) return (stub#95@{unit}@{( address * unit * int )})@(unit)[@inline][@hidden] in
  let x#159int = 41 in
  let x#160int = ADD(x#159 , 1) in let xint = x#160 in unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias9.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias10.mligo" ] ;
  [%expect {xxx|
  let get_balance#8unit -> tez =
    lambda (_uunit)tez return ([%Michelson {| { DROP ; BALANCE } |}])@(unit)[@inline][@hidden] in
  let get_amount#9unit -> tez =
    lambda (_uunit)tez return ([%Michelson {| { DROP ; AMOUNT } |}])@(unit)[@inline][@hidden] in
  let get_now#10unit -> timestamp =
    lambda (_uunit)timestamp return ([%Michelson {| { DROP ; NOW } |}])@(unit)[@inline][@hidden] in
  let get_sender#11unit -> address =
    lambda (_uunit)address return ([%Michelson {| { DROP ; SENDER } |}])@(unit)[@inline][@hidden] in
  let get_source#12unit -> address =
    lambda (_uunit)address return ([%Michelson {| { DROP ; SOURCE } |}])@(unit)[@inline][@hidden] in
  let get_level#13unit -> nat =
    lambda (_uunit)nat return ([%Michelson {| { DROP ; LEVEL } |}])@(unit)[@inline][@hidden] in
  let get_self_address#14unit -> address =
    lambda (_uunit)address return SELF_ADDRESS()[@inline][@hidden] in
  let get_chain_id#15unit -> chain_id =
    lambda (_uunit)chain_id return ([%Michelson {| { DROP ; CHAIN_ID } |}])@(unit)[@inline][@hidden] in
  let get_total_voting_power#16unit -> nat =
    lambda (_uunit)nat return ([%Michelson {| { DROP ; TOTAL_VOTING_POWER } |}])@(unit)[@inline][@hidden] in
  let get_min_block_time#17unit -> nat =
    lambda (_uunit)nat return ([%Michelson {| { DROP; MIN_BLOCK_TIME } |}])@(unit)[@inline][@hidden] in
  let voting_power#18key_hash -> nat =
    lambda (khkey_hash)nat return ([%Michelson {| { VOTING_POWER } |}])@(kh)[@inline][@hidden] in
  let address#19funtype a : * . contract (a) -> address = Λ a ->
  lambda (ccontract (a))address return ADDRESS(c)[@inline][@hidden] in
  let implicit_account#20key_hash -> contract (unit) =
    lambda (khkey_hash)contract (unit) return IMPLICIT_ACCOUNT(kh)[@inline][@hidden] in
  let join_tickets#21funtype a : * . ( ticket (a) * ticket (a) ) -> option (ticket (a)) =
    Λ a ->
  lambda (t( ticket (a) * ticket (a) ))option (ticket (a)) return ([%Michelson {| { JOIN_TICKETS } |}])@(t)[@inline][@hidden] in
  let read_ticket#22funtype a : * . ticket (a) -> ( ( address * ( a * nat ) ) * ticket (a) ) =
    Λ a ->
  lambda (tticket (a))( ( address * ( a * nat ) ) * ticket (a) ) return
  ([%Michelson {| { READ_TICKET ; PAIR } |}])@(t)[@inline][@hidden] in
  let never#23funtype a : * . never -> a = Λ a ->
  lambda (nnever)a return ([%Michelson {| { NEVER } |}])@(n)[@inline][@hidden] in
  let pairing_check#24list (( bls12_381_g1 * bls12_381_g2 )) -> bool =
    lambda (llist (( bls12_381_g1 * bls12_381_g2 )))bool return ([%Michelson {| { PAIRING_CHECK } |}])@(l)[@inline][@hidden] in
  let constant#25funtype a : * . string -> a = Λ a ->
  lambda (sstring)a return GLOBAL_CONSTANT(s)[@inline][@hidden] in
  let set_delegate#26option (key_hash) -> operation =
    lambda (ooption (key_hash))operation return SET_DELEGATE(o)[@inline][@hidden] in
  let get_contract#27funtype a : * . address -> contract (a) = Λ a ->
  lambda (aaddress)contract (a) return CONTRACT(a)[@inline][@hidden] in
  let get_contract_opt#28funtype a : * . address -> option (contract (a)) =
    Λ a ->
  lambda (aaddress)option (contract (a)) return CONTRACT_OPT(a)[@inline][@hidden] in
  let get_contract_with_error#29funtype a : * . address -> string -> contract (a) =
    Λ a ->
  lambda (aaddress)string -> contract (a) return lambda (sstring)contract (a) return
  CONTRACT_WITH_ERROR(a , s)[@inline][@hidden] in
  let create_ticket#30funtype a : * . a -> nat -> ticket (a) = Λ a ->
  lambda (va)nat -> ticket (a) return lambda (nnat)ticket (a) return ([%Michelson {| { UNPAIR ; TICKET } |}])@(
                                                                     ( v ,
                                                                      n ))[@inline][@hidden] in
  let transaction#31funtype a : * . a -> tez -> contract (a) -> operation =
    Λ a ->
  lambda (aa)tez -> contract (a) -> operation return lambda (mutez)contract (a) -> operation return lambda (ccontract (a))operation return
  CALL(a , mu , c)[@inline][@hidden] in
  let open_chest#32chest_key -> chest -> nat -> sum[Fail_decrypt -> unit , Fail_timelock -> unit , Ok_opening -> bytes] =
    lambda (ckchest_key)chest -> nat -> sum[Fail_decrypt -> unit , Fail_timelock -> unit , Ok_opening -> bytes] return lambda (cchest)nat ->
  sum[Fail_decrypt -> unit , Fail_timelock -> unit , Ok_opening -> bytes] return lambda (nnat)
  sum[Fail_decrypt -> unit , Fail_timelock -> unit , Ok_opening -> bytes] return
  OPEN_CHEST(ck , c , n)[@inline][@hidden] in
  let call_view#33funtype a : * . funtype b : * . string -> a -> address -> option (b) =
    Λ a ->
  Λ b ->
  lambda (sstring)a -> address -> option (b) return lambda (xa)address -> option (b) return lambda (aaddress)option (b) return
  VIEW(s , x , a)[@inline][@hidden] in
  let split_ticket#34funtype a : * . ticket (a) -> ( nat * nat ) -> option (
  ( ticket (a) * ticket (a) )) = Λ a ->
  lambda (tticket (a))( nat * nat ) -> option (( ticket (a) * ticket (a) )) return lambda (p
  ( nat * nat ))option (( ticket (a) * ticket (a) )) return ([%Michelson {| { UNPAIR ; SPLIT_TICKET } |}])@(
                                                            ( t ,
                                                              p ))[@inline][@hidden] in
  let xor#35nat -> nat -> nat =
    lambda (lnat)nat -> nat return lambda (rnat)nat return XOR(l , r)[@inline][@hidden] in
  let shift_left#36nat -> nat -> nat =
    lambda (lnat)nat -> nat return lambda (rnat)nat return LSL(l , r)[@inline][@hidden] in
  let shift_right#37nat -> nat -> nat =
    lambda (lnat)nat -> nat return lambda (rnat)nat return LSR(l , r)[@inline][@hidden] in
  let empty#38funtype k : * . funtype v : * . big_map (k , v) = Λ k ->
  Λ v ->  BIG_MAP_EMPTY()[@inline][@hidden] in
  let mem#39funtype k : * . funtype v : * . k -> big_map (k , v) -> bool =
    Λ k ->
  Λ v ->
  lambda (kk)big_map (k , v) -> bool return lambda (mbig_map (k ,
  v))bool return ([%Michelson {| { UNPAIR ; MEM } |}])@(( k , m ))[@inline][@hidden] in
  let add#40funtype k : * . funtype v : * . k -> v -> big_map (k ,
  v) -> big_map (k , v) = Λ k ->
  Λ v ->
  lambda (kk)v -> big_map (k , v) -> big_map (k ,
  v) return lambda (vv)big_map (k , v) -> big_map (k ,
  v) return lambda (mbig_map (k , v))big_map (k , v) return MAP_ADD(k , v , m)[@inline][@hidden] in
  let remove#41funtype k : * . funtype v : * . k -> big_map (k ,
  v) -> big_map (k , v) = Λ k ->
  Λ v ->
  lambda (kk)big_map (k , v) -> big_map (k , v) return lambda (mbig_map (k ,
  v))big_map (k , v) return MAP_REMOVE(k , m)[@inline][@hidden] in
  let update#42funtype k : * . funtype v : * . k -> option (v) -> big_map (k ,
  v) -> big_map (k , v) = Λ k ->
  Λ v ->
  lambda (kk)option (v) -> big_map (k , v) -> big_map (k ,
  v) return lambda (voption (v))big_map (k , v) -> big_map (k ,
  v) return lambda (mbig_map (k , v))big_map (k ,
  v) return MAP_UPDATE(k , v , m)[@inline][@hidden] in
  let get_and_update#43funtype k : * . funtype v : * . k -> option (v) -> big_map (k ,
  v) -> ( option (v) * big_map (k , v) ) = Λ k ->
  Λ v ->
  lambda (kk)option (v) -> big_map (k ,
  v) -> ( option (v) * big_map (k , v) ) return lambda (voption (v))big_map (k ,
  v) -> ( option (v) * big_map (k , v) ) return lambda (mbig_map (k ,
  v))( option (v) * big_map (k , v) ) return BIG_MAP_GET_AND_UPDATE(k , v , m)[@inline][@hidden] in
  let find_opt#44funtype k : * . funtype v : * . k -> big_map (k ,
  v) -> option (v) = Λ k ->
  Λ v ->
  lambda (kk)big_map (k , v) -> option (v) return lambda (mbig_map (k ,
  v))option (v) return MAP_FIND_OPT(k , m)[@inline][@hidden] in
  let find#45funtype k : * . funtype v : * . k -> big_map (k , v) -> v =
    Λ k ->
  Λ v ->
  lambda (kk)big_map (k , v) -> v return lambda (mbig_map (k ,
  v))v return MAP_FIND(k , m)[@inline][@hidden] in
  let empty#46funtype k : * . funtype v : * . map (k , v) = Λ k ->
  Λ v ->  MAP_EMPTY()[@inline][@hidden] in
  let size#47funtype k : * . funtype v : * . map (k , v) -> nat = Λ k ->
  Λ v ->  lambda (mmap (k , v))nat return ([%Michelson {| { SIZE } |}])@(m)[@inline][@hidden] in
  let mem#48funtype k : * . funtype v : * . k -> map (k , v) -> bool = Λ k ->
  Λ v ->
  lambda (kk)map (k , v) -> bool return lambda (mmap (k ,
  v))bool return ([%Michelson {| { UNPAIR ; MEM } |}])@(( k , m ))[@inline][@hidden] in
  let add#49funtype k : * . funtype v : * . k -> v -> map (k , v) -> map (k ,
  v) = Λ k ->
  Λ v ->
  lambda (kk)v -> map (k , v) -> map (k , v) return lambda (vv)map (k ,
  v) -> map (k , v) return lambda (mmap (k , v))map (k ,
  v) return MAP_ADD(k , v , m)[@inline][@hidden] in
  let remove#50funtype k : * . funtype v : * . k -> map (k , v) -> map (k ,
  v) = Λ k ->
  Λ v ->
  lambda (kk)map (k , v) -> map (k , v) return lambda (mmap (k , v))map (k ,
  v) return MAP_REMOVE(k , m)[@inline][@hidden] in
  let update#51funtype k : * . funtype v : * . k -> option (v) -> map (k ,
  v) -> map (k , v) = Λ k ->
  Λ v ->
  lambda (kk)option (v) -> map (k , v) -> map (k ,
  v) return lambda (voption (v))map (k , v) -> map (k ,
  v) return lambda (mmap (k , v))map (k , v) return MAP_UPDATE(k , v , m)[@inline][@hidden] in
  let get_and_update#52funtype k : * . funtype v : * . k -> option (v) -> map (k ,
  v) -> ( option (v) * map (k , v) ) = Λ k ->
  Λ v ->
  lambda (kk)option (v) -> map (k ,
  v) -> ( option (v) * map (k , v) ) return lambda (voption (v))map (k ,
  v) -> ( option (v) * map (k , v) ) return lambda (mmap (k ,
  v))( option (v) * map (k , v) ) return MAP_GET_AND_UPDATE(k , v , m)[@inline][@hidden] in
  let find#53funtype k : * . funtype v : * . k -> map (k , v) -> v = Λ k ->
  Λ v ->
  lambda (kk)map (k , v) -> v return lambda (mmap (k ,
  v))v return MAP_FIND(k , m)[@inline][@hidden] in
  let find_opt#54funtype k : * . funtype v : * . k -> map (k ,
  v) -> option (v) = Λ k ->
  Λ v ->
  lambda (kk)map (k , v) -> option (v) return lambda (mmap (k ,
  v))option (v) return MAP_FIND_OPT(k , m)[@inline][@hidden] in
  let iter#55funtype k : * . funtype v : * . ( k * v ) -> unit -> map (k ,
  v) -> unit = Λ k ->
  Λ v ->
  lambda (f( k * v ) -> unit)map (k , v) -> unit return lambda (mmap (k ,
  v))unit return MAP_ITER(f , m)[@inline][@hidden] in
  let map#56funtype k : * . funtype v : * . funtype w : * . ( k * v ) -> w -> map (k ,
  v) -> map (k , w) = Λ k ->
  Λ v ->
  Λ w ->
  lambda (f( k * v ) -> w)map (k , v) -> map (k , w) return lambda (mmap (k ,
  v))map (k , w) return MAP_MAP(f , m)[@inline][@hidden] in
  let fold#57funtype k : * . funtype v : * . funtype c : * . ( c * ( k * v ) ) -> c -> map (k ,
  v) -> c -> c = Λ k ->
  Λ v ->
  Λ c ->
  lambda (f( c * ( k * v ) ) -> c)map (k ,
  v) -> c -> c return lambda (mmap (k ,
  v))c -> c return lambda (ic)c return MAP_FOLD(f , m , i)[@inline][@hidden] in
  let empty#58funtype a : * . set (a) = Λ a ->
  SET_EMPTY()[@inline][@hidden] in
  let size#59funtype a : * . set (a) -> nat = Λ a ->
  lambda (sset (a))nat return ([%Michelson {| { SIZE } |}])@(s)[@inline][@hidden] in
  let cardinal#60funtype a : * . set (a) -> nat = Λ a ->
  lambda (sset (a))nat return ([%Michelson {| { SIZE } |}])@(s)[@inline][@hidden] in
  let mem#61funtype a : * . a -> set (a) -> bool = Λ a ->
  lambda (xa)set (a) -> bool return lambda (sset (a))bool return SET_MEM(x , s)[@inline][@hidden] in
  let add#62funtype a : * . a -> set (a) -> set (a) = Λ a ->
  lambda (xa)set (a) -> set (a) return lambda (sset (a))set (a) return
  SET_ADD(x , s)[@inline][@hidden] in
  let remove#63funtype a : * . a -> set (a) -> set (a) = Λ a ->
  lambda (xa)set (a) -> set (a) return lambda (sset (a))set (a) return
  SET_REMOVE(x , s)[@inline][@hidden] in
  let update#64funtype a : * . a -> bool -> set (a) -> set (a) = Λ a ->
  lambda (xa)bool -> set (a) -> set (a) return lambda (bbool)set (a) -> set (a) return lambda (sset (a))set (a) return
  SET_UPDATE(x , b , s)[@inline][@hidden] in
  let iter#65funtype a : * . a -> unit -> set (a) -> unit = Λ a ->
  lambda (fa -> unit)set (a) -> unit return lambda (sset (a))unit return
  SET_ITER(f , s)[@inline][@hidden] in
  let fold#66funtype a : * . funtype b : * . ( b * a ) -> b -> set (a) -> b -> b =
    Λ a ->
  Λ b ->
  lambda (f( b * a ) -> b)set (a) -> b -> b return lambda (sset (a))b -> b return lambda (ib)b return
  SET_FOLD(f , s , i)[@inline][@hidden] in
  let fold_desc#67funtype a : * . funtype b : * . ( a * b ) -> b -> set (a) -> b -> b =
    Λ a ->
  Λ b ->
  lambda (f( a * b ) -> b)set (a) -> b -> b return lambda (sset (a))b -> b return lambda (ib)b return
  SET_FOLD_DESC(f , s , i)[@inline][@hidden] in
  let length#68funtype a : * . list (a) -> nat = Λ a ->
  lambda (xslist (a))nat return ([%Michelson {| { SIZE } |}])@(xs)[@inline][@hidden] in
  let size#69funtype a : * . list (a) -> nat = Λ a ->
  lambda (xslist (a))nat return ([%Michelson {| { SIZE } |}])@(xs)[@inline][@hidden] in
  let head_opt#70funtype a : * . list (a) -> option (a) = Λ a ->
  lambda (xslist (a))option (a) return  match xs with
                                         | Cons ctor_proj#2 ->
                                            match ctor_proj#2 with
                                             | ( x , _#2 ) ->
                                             Some(x)
                                         | Nil unit_proj#4 ->
                                           None(unit)[@inline][@hidden] in
  let tail_opt#71funtype a : * . list (a) -> option (list (a)) = Λ a ->
  lambda (xslist (a))option (list (a)) return  match xs with
                                                | Cons ctor_proj#5 ->
                                                   match ctor_proj#5 with
                                                    | ( _#3 , xs ) ->
                                                    Some(xs)
                                                | Nil unit_proj#7 ->
                                                  None(unit)[@inline][@hidden] in
  let map#72funtype a : * . funtype b : * . a -> b -> list (a) -> list (b) =
    Λ a ->
  Λ b ->
  lambda (fa -> b)list (a) -> list (b) return lambda (xslist (a))list (b) return
  LIST_MAP(f , xs)[@inline][@hidden] in
  let iter#73funtype a : * . a -> unit -> list (a) -> unit = Λ a ->
  lambda (fa -> unit)list (a) -> unit return lambda (xslist (a))unit return
  LIST_ITER(f , xs)[@inline][@hidden] in
  let fold#74funtype a : * . funtype b : * . ( b * a ) -> b -> list (a) -> b -> b =
    Λ a ->
  Λ b ->
  lambda (f( b * a ) -> b)list (a) -> b -> b return lambda (xslist (a))b -> b return lambda (ib)b return
  LIST_FOLD(f , xs , i)[@inline][@hidden] in
  let fold_left#75funtype a : * . funtype b : * . ( b * a ) -> b -> b -> list (a) -> b =
    Λ a ->
  Λ b ->
  lambda (f( b * a ) -> b)b -> list (a) -> b return lambda (ib)list (a) -> b return lambda (xslist (a))b return
  LIST_FOLD_LEFT(f , i , xs)[@inline][@hidden] in
  let fold_right#76funtype a : * . funtype b : * . ( a * b ) -> b -> list (a) -> b -> b =
    Λ a ->
  Λ b ->
  lambda (f( a * b ) -> b)list (a) -> b -> b return lambda (xslist (a))b -> b return lambda (ib)b return
  LIST_FOLD_RIGHT(f , xs , i)[@inline][@hidden] in
  let cons#77funtype a : * . a -> list (a) -> list (a) = Λ a ->
  lambda (xa)list (a) -> list (a) return lambda (xslist (a))list (a) return
  CONS(x , xs)[@inline][@hidden] in
  let length#78string -> nat =
    lambda (bstring)nat return ([%Michelson {| { SIZE } |}])@(b)[@inline][@hidden] in
  let concat#79string -> string -> string =
    lambda (b1string)string -> string return lambda (b2string)string return
  ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b1 , b2 ))[@inline][@hidden] in
  let sub#80nat -> nat -> string -> string =
    lambda (snat)nat -> string -> string return lambda (lnat)string -> string return lambda (bstring)string return
  ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(
  ( s ,
    l ,
    b ))[@inline][@hidden] in
  let unopt#81funtype a : * . option (a) -> a = Λ a ->
  lambda (voption (a))a return UNOPT(v)[@inline][@hidden] in
  let unopt_with_error#82funtype a : * . option (a) -> string -> a = Λ a ->
  lambda (voption (a))string -> a return lambda (sstring)a return UNOPT_WITH_ERROR
                                                                  (v ,
                                                                   s)[@inline][@hidden] in
  let pack#83funtype a : * . a -> bytes = Λ a ->
  lambda (va)bytes return ([%Michelson {| { PACK } |}])@(v)[@inline][@hidden] in
  let unpack#84funtype a : * . bytes -> option (a) = Λ a ->
  lambda (bbytes)option (a) return BYTES_UNPACK(b)[@inline][@hidden] in
  let length#85bytes -> nat =
    lambda (bbytes)nat return ([%Michelson {| { SIZE } |}])@(b)[@inline][@hidden] in
  let concat#86bytes -> bytes -> bytes =
    lambda (b1bytes)bytes -> bytes return lambda (b2bytes)bytes return
  ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b1 , b2 ))[@inline][@hidden] in
  let sub#87nat -> nat -> bytes -> bytes =
    lambda (snat)nat -> bytes -> bytes return lambda (lnat)bytes -> bytes return lambda (bbytes)bytes return
  ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(
  ( s ,
    l ,
    b ))[@inline][@hidden] in
  let blake2b#88bytes -> bytes =
    lambda (bbytes)bytes return ([%Michelson {| { BLAKE2B } |}])@(b)[@inline][@hidden] in
  let sha256#89bytes -> bytes =
    lambda (bbytes)bytes return ([%Michelson {| { SHA256 } |}])@(b)[@inline][@hidden] in
  let sha512#90bytes -> bytes =
    lambda (bbytes)bytes return ([%Michelson {| { SHA512 } |}])@(b)[@inline][@hidden] in
  let sha3#91bytes -> bytes =
    lambda (bbytes)bytes return ([%Michelson {| { SHA3 } |}])@(b)[@inline][@hidden] in
  let keccak#92bytes -> bytes =
    lambda (bbytes)bytes return ([%Michelson {| { KECCAK } |}])@(b)[@inline][@hidden] in
  let hash_key#93key -> key_hash =
    lambda (kkey)key_hash return ([%Michelson {| { HASH_KEY } |}])@(k)[@inline][@hidden] in
  let check#94key -> signature -> bytes -> bool =
    lambda (kkey)signature -> bytes -> bool return lambda (ssignature)bytes -> bool return lambda (bbytes)bool return
  ([%Michelson {| { UNPAIR ; UNPAIR ; CHECK_SIGNATURE } |}])@(( k , s , b ))[@inline][@hidden] in
  let assertbool -> unit =
    lambda (bbool)unit return ([%Michelson {| { IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } } |}])@(b)[@inline][@private][@hidden] in
  let assert_somefuntype a : * . option (a) -> unit = Λ a ->
  lambda (voption (a))unit return ([%Michelson {| { IF_NONE { PUSH string "failed assert some" ; FAILWITH } { DROP ; UNIT } } |}])@(v)[@inline][@private][@hidden] in
  let assert_nonefuntype a : * . option (a) -> unit = Λ a ->
  lambda (voption (a))unit return ([%Michelson {| { IF_NONE { UNIT } { PUSH string "failed assert none" ; FAILWITH } } |}])@(v)[@inline][@private][@hidden] in
  let absint -> nat =
    lambda (iint)nat return ([%Michelson {| { ABS } |}])@(i)[@inline][@private][@hidden] in
  let is_natint -> option (nat) =
    lambda (iint)option (nat) return ([%Michelson {| { ISNAT } |}])@(i)[@inline][@private][@hidden] in
  let truebool = TRUE()[@inline][@private][@hidden] in
  let falsebool = FALSE()[@inline][@private][@hidden] in
  let unitunit = UNIT()[@inline][@private][@hidden] in
  let failwithfuntype a : * . funtype b : * . a -> b = Λ a ->
  Λ b ->  [%Michelson {|{ FAILWITH }|}][@inline][@private][@hidden] in
  let intfuntype a : * . a -> external_int (a) = Λ a ->
  lambda (va)external_int (a) return ([%Michelson {| { INT } |}])@(v)[@inline][@private][@hidden] in
  let assert_with_errorbool -> string -> unit =
    lambda (bbool)string -> unit return lambda (sstring)unit return ([%Michelson {| { UNPAIR ; IF { DROP ; UNIT } { FAILWITH } } |}])@(
                                                                    ( b ,
                                                                      s ))[@inline][@private][@hidden] in
  let assert_some_with_errorfuntype a : * . option (a) -> string -> unit =
    Λ a ->
  lambda (voption (a))string -> unit return lambda (sstring)unit return
  ([%Michelson {| { UNPAIR ; IF_NONE { FAILWITH } { DROP 2 ; UNIT } } |}])@(
  ( v ,
    s ))[@inline][@private][@hidden] in
  let assert_none_with_errorfuntype a : * . option (a) -> string -> unit =
    Λ a ->
  lambda (voption (a))string -> unit return lambda (sstring)unit return
  ([%Michelson {| { UNPAIR ; IF_NONE { DROP ; UNIT } { DROP ; FAILWITH } } |}])@(
  ( v ,
    s ))[@inline][@private][@hidden] in
  let edivfuntype a : * . funtype b : * . a -> b -> external_ediv (a , b) =
    Λ a ->
  Λ b ->
  lambda (la)b -> external_ediv (a , b) return lambda (rb)external_ediv (a ,
  b) return ([%Michelson {| { UNPAIR ; EDIV } |}])@(( l , r ))[@inline][@private][@hidden] in
  let stub#95funtype a : * . funtype b : * . a -> b = Λ a ->
  Λ b ->  lambda (xa)b return ([%Michelson {|{ FAILWITH }|}])@(x)[@inline][@private][@hidden] in
  let run#96funtype a : * . funtype b : * . a -> b -> a -> unit = Λ a ->
  Λ b ->
  lambda (_fa -> b)a -> unit return lambda (_va)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let eval#97funtype a : * . a -> unit = Λ a ->
  lambda (_xa)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let compile_value#98funtype a : * . a -> unit = Λ a ->
  lambda (_xa)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let get_total_voting_power#99unit -> nat =
    lambda (_uunit)nat return (stub#95@{unit}@{nat})@(unit)[@inline][@hidden] in
  let failwith#100funtype a : * . funtype b : * . a -> b = Λ a ->
  Λ b ->  lambda (_va)b return (stub#95@{unit}@{b})@(unit)[@inline][@hidden] in
  let to_contract#101funtype p : * . funtype s : * . unit -> contract (p) =
    Λ p ->
  Λ s ->
  lambda (_tunit)contract (p) return (stub#95@{unit}@{contract (p)})@(unit)[@inline][@hidden] in
  let set_source#102address -> unit =
    lambda (_aaddress)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let get_storage_of_address#103address -> unit =
    lambda (_aaddress)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let get_balance#104address -> tez =
    lambda (_aaddress)tez return (stub#95@{unit}@{tez})@(unit)[@inline][@hidden] in
  let print#105string -> unit =
    lambda (_vstring)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let eprint#106string -> unit =
    lambda (_vstring)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let get_voting_power#107key_hash -> nat =
    lambda (_khkey_hash)nat return (stub#95@{unit}@{nat})@(unit)[@inline][@hidden] in
  let nth_bootstrap_contract#108nat -> address =
    lambda (_inat)address return (stub#95@{unit}@{address})@(unit)[@inline][@hidden] in
  let nth_bootstrap_account#109int -> address =
    lambda (_iint)address return (stub#95@{unit}@{address})@(unit)[@inline][@hidden] in
  let get_bootstrap_account#110nat -> ( address * key * string ) =
    lambda (_nnat)( address * key * string ) return (stub#95@{unit}@{( address * key * string )})@(unit)[@inline][@hidden] in
  let nth_bootstrap_typed_address#111funtype a : * . funtype b : * . nat -> unit =
    Λ a ->
  Λ b ->  lambda (_nnat)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let last_originations#112unit -> map (address , list (address)) =
    lambda (_uunit)map (address ,
  list (address)) return (stub#95@{unit}@{map (address ,
                         list (address))})@(unit)[@inline][@hidden] in
  let random#113funtype a : * . unit -> a = Λ a ->
  lambda (_uunit)a return (stub#95@{unit}@{a})@(unit)[@inline][@hidden] in
  let new_account#114unit -> ( string * key ) =
    lambda (_uunit)( string * key ) return (stub#95@{unit}@{( string * key )})@(unit)[@inline][@hidden] in
  let decompile#115funtype a : * . unit -> a = Λ a ->
  lambda (_munit)a return (stub#95@{unit}@{a})@(unit)[@inline][@hidden] in
  let bake_until_n_cycle_end#116nat -> unit =
    lambda (_nnat)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let cast_address#117funtype a : * . funtype b : * . address -> unit =
    Λ a ->
  Λ b ->  lambda (_aaddress)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let register_delegate#118key_hash -> unit =
    lambda (_khkey_hash)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let register_constant#119unit -> string =
    lambda (_munit)string return (stub#95@{unit}@{string})@(unit)[@inline][@hidden] in
  let to_typed_address#120funtype a : * . funtype b : * . contract (a) -> unit =
    Λ a ->
  Λ b ->  lambda (_ccontract (a))unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let constant_to_michelson_program#121string -> unit =
    lambda (_sstring)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let restore_context#122unit -> unit =
    lambda (_uunit)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let save_context#123unit -> unit =
    lambda (_uunit)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let drop_context#124unit -> unit =
    lambda (_uunit)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let to_string#125funtype a : * . a -> string = Λ a ->
  lambda (_va)string return (stub#95@{unit}@{string})@(unit)[@inline][@hidden] in
  let get_storage#126funtype p : * . funtype s : * . unit -> s = Λ p ->
  Λ s ->  lambda (_tunit)s return (stub#95@{unit}@{s})@(unit)[@inline][@hidden] in
  let set_baker_policy#127unit -> unit =
    lambda (_bpunit)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let set_baker#128address -> unit =
    lambda (_aaddress)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let size#129unit -> int =
    lambda (_cunit)int return (stub#95@{unit}@{int})@(unit)[@inline][@hidden] in
  let compile_contract#130funtype p : * . funtype s : * . ( p * s ) ->
  ( list (operation) * s ) -> unit = Λ p ->
  Λ s ->
  lambda (_f( p * s ) -> ( list (operation) * s ))unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let read_contract_from_file#131string -> unit =
    lambda (_fnstring)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let chr#132nat -> option (string) =
    lambda (_nnat)option (string) return (stub#95@{unit}@{option (string)})@(unit)[@inline][@hidden] in
  let nl#133string = "NEWLINE"[@inline][@hidden] in
  let println#134string -> unit =
    lambda (_vstring)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let transfer#135address -> unit -> tez -> unit =
    lambda (_aaddress)unit -> tez -> unit return lambda (_sunit)tez -> unit return lambda (_ttez)unit return
  (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let transfer_exn#136address -> unit -> tez -> nat =
    lambda (_aaddress)unit -> tez -> nat return lambda (_sunit)tez -> nat return lambda (_ttez)nat return
  (stub#95@{unit}@{nat})@(unit)[@inline][@hidden] in
  let log#137funtype a : * . a -> unit = Λ a ->
  lambda (_va)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let reset_state#138nat -> list (tez) -> unit =
    lambda (_nnat)list (tez) -> unit return lambda (_llist (tez))unit return
  (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let reset_state_at#139timestamp -> nat -> list (tez) -> unit =
    lambda (_ttimestamp)nat -> list (tez) -> unit return lambda (_nnat)list (tez) -> unit return lambda (_llist (tez))unit return
  (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let bootstrap_contract#140funtype p : * . funtype s : * . ( p * s ) ->
  ( list (operation) * s ) -> s -> tez -> unit = Λ p ->
  Λ s ->
  lambda (_f( p * s ) -> ( list (operation) * s ))s -> tez -> unit return lambda (_ss)tez -> unit return lambda (_ttez)unit return
  (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let mutate_value#141funtype a : * . nat -> a -> option (( a * unit )) =
    Λ a ->
  lambda (_nnat)a -> option (( a * unit )) return lambda (_va)option (( a * unit )) return
  (stub#95@{unit}@{option (( a * unit ))})@(unit)[@inline][@hidden] in
  let save_mutation#142string -> unit -> option (string) =
    lambda (_sstring)unit -> option (string) return lambda (_munit)option (string) return
  (stub#95@{unit}@{option (string)})@(unit)[@inline][@hidden] in
  let mutation_test#143funtype a : * . funtype b : * . a -> a -> b -> option (
  ( b * unit )) = Λ a ->
  Λ b ->
  lambda (_va)a -> b -> option (( b * unit )) return lambda (_fa -> b)option (
  ( b * unit )) return (stub#95@{unit}@{option (( b * unit ))})@(unit)[@inline][@hidden] in
  let mutation_test_all#144funtype a : * . funtype b : * . a -> a -> b -> list (
  ( b * unit )) = Λ a ->
  Λ b ->
  lambda (_va)a -> b -> list (( b * unit )) return lambda (_fa -> b)list (
  ( b * unit )) return (stub#95@{unit}@{list (( b * unit ))})@(unit)[@inline][@hidden] in
  let sign#145string -> bytes -> signature =
    lambda (_skstring)bytes -> signature return lambda (_dbytes)signature return
  (stub#95@{unit}@{signature})@(unit)[@inline][@hidden] in
  let add_account#146string -> key -> unit =
    lambda (_sstring)key -> unit return lambda (_kkey)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let baker_account#147( string * key ) -> option (tez) -> unit =
    lambda (_p( string * key ))option (tez) -> unit return lambda (_ooption (tez))unit return
  (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let set_big_map#148funtype a : * . funtype b : * . int -> big_map (a ,
  b) -> unit = Λ a ->
  Λ b ->
  lambda (_iint)big_map (a , b) -> unit return lambda (_mbig_map (a ,
  b))unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let create_chest#149bytes -> nat -> ( chest * chest_key ) =
    lambda (_bbytes)nat -> ( chest * chest_key ) return lambda (_nnat)( chest * chest_key ) return
  (stub#95@{unit}@{( chest * chest_key )})@(unit)[@inline][@hidden] in
  let create_chest_key#150chest -> nat -> chest_key =
    lambda (_cchest)nat -> chest_key return lambda (_nnat)chest_key return
  (stub#95@{unit}@{chest_key})@(unit)[@inline][@hidden] in
  let transfer_to_contract#151funtype p : * . contract (p) -> p -> tez -> unit =
    Λ p ->
  lambda (_ccontract (p))p -> tez -> unit return lambda (_sp)tez -> unit return lambda (_ttez)unit return
  (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let transfer_to_contract_exn#152funtype p : * . contract (p) -> p -> tez -> nat =
    Λ p ->
  lambda (_ccontract (p))p -> tez -> nat return lambda (_sp)tez -> nat return lambda (_ttez)nat return
  (stub#95@{unit}@{nat})@(unit)[@inline][@hidden] in
  let michelson_equal#153unit -> unit -> bool =
    lambda (_m1unit)unit -> bool return lambda (_m2unit)bool return (stub#95@{unit}@{bool})@(unit)[@inline][@hidden] in
  let to_entrypoint#154funtype a : * . funtype b : * . funtype c : * . string -> unit -> contract (c) =
    Λ a ->
  Λ b ->
  Λ c ->
  lambda (_sstring)unit -> contract (c) return lambda (_tunit)contract (c) return
  (stub#95@{unit}@{contract (c)})@(unit)[@inline][@hidden] in
  let originate_contract#155unit -> unit -> tez -> address =
    lambda (_cunit)unit -> tez -> address return lambda (_sunit)tez -> address return lambda (_ttez)address return
  (stub#95@{unit}@{address})@(unit)[@inline][@hidden] in
  let originate#156funtype p : * . funtype s : * . ( p * s ) -> ( list (operation) * s ) -> s -> tez ->
  ( unit * unit * int ) = Λ p ->
  Λ s ->
  lambda (_f( p * s ) -> ( list (operation) * s ))s -> tez -> ( unit * unit * int ) return lambda (_ss)tez ->
  ( unit * unit * int ) return lambda (_ttez)( unit * unit * int ) return
  (stub#95@{unit}@{( unit * unit * int )})@(unit)[@inline][@hidden] in
  let compile_contract_from_file#157string -> string -> list (string) -> unit =
    lambda (_fnstring)string -> list (string) -> unit return lambda (_estring)list (string) -> unit return lambda (_vlist (string))unit return
  (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let originate_from_file#158string -> string -> list (string) -> unit -> tez ->
  ( address * unit * int ) =
    lambda (_fnstring)string -> list (string) -> unit -> tez -> ( address * unit * int ) return lambda (_estring)list (string) -> unit -> tez ->
  ( address * unit * int ) return lambda (_vlist (string))unit -> tez ->
  ( address * unit * int ) return lambda (_sunit)tez -> ( address * unit * int ) return lambda (_ttez)
  ( address * unit * int ) return (stub#95@{unit}@{( address * unit * int )})@(unit)[@inline][@hidden] in
  let x#159int = 42 in
  let x#160int = 2 in let y#161int = x#159 in let xint = y#161 in unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias10.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias11.mligo" ] ;
  [%expect {xxx|
  let get_balance#8unit -> tez =
    lambda (_uunit)tez return ([%Michelson {| { DROP ; BALANCE } |}])@(unit)[@inline][@hidden] in
  let get_amount#9unit -> tez =
    lambda (_uunit)tez return ([%Michelson {| { DROP ; AMOUNT } |}])@(unit)[@inline][@hidden] in
  let get_now#10unit -> timestamp =
    lambda (_uunit)timestamp return ([%Michelson {| { DROP ; NOW } |}])@(unit)[@inline][@hidden] in
  let get_sender#11unit -> address =
    lambda (_uunit)address return ([%Michelson {| { DROP ; SENDER } |}])@(unit)[@inline][@hidden] in
  let get_source#12unit -> address =
    lambda (_uunit)address return ([%Michelson {| { DROP ; SOURCE } |}])@(unit)[@inline][@hidden] in
  let get_level#13unit -> nat =
    lambda (_uunit)nat return ([%Michelson {| { DROP ; LEVEL } |}])@(unit)[@inline][@hidden] in
  let get_self_address#14unit -> address =
    lambda (_uunit)address return SELF_ADDRESS()[@inline][@hidden] in
  let get_chain_id#15unit -> chain_id =
    lambda (_uunit)chain_id return ([%Michelson {| { DROP ; CHAIN_ID } |}])@(unit)[@inline][@hidden] in
  let get_total_voting_power#16unit -> nat =
    lambda (_uunit)nat return ([%Michelson {| { DROP ; TOTAL_VOTING_POWER } |}])@(unit)[@inline][@hidden] in
  let get_min_block_time#17unit -> nat =
    lambda (_uunit)nat return ([%Michelson {| { DROP; MIN_BLOCK_TIME } |}])@(unit)[@inline][@hidden] in
  let voting_power#18key_hash -> nat =
    lambda (khkey_hash)nat return ([%Michelson {| { VOTING_POWER } |}])@(kh)[@inline][@hidden] in
  let address#19funtype a : * . contract (a) -> address = Λ a ->
  lambda (ccontract (a))address return ADDRESS(c)[@inline][@hidden] in
  let implicit_account#20key_hash -> contract (unit) =
    lambda (khkey_hash)contract (unit) return IMPLICIT_ACCOUNT(kh)[@inline][@hidden] in
  let join_tickets#21funtype a : * . ( ticket (a) * ticket (a) ) -> option (ticket (a)) =
    Λ a ->
  lambda (t( ticket (a) * ticket (a) ))option (ticket (a)) return ([%Michelson {| { JOIN_TICKETS } |}])@(t)[@inline][@hidden] in
  let read_ticket#22funtype a : * . ticket (a) -> ( ( address * ( a * nat ) ) * ticket (a) ) =
    Λ a ->
  lambda (tticket (a))( ( address * ( a * nat ) ) * ticket (a) ) return
  ([%Michelson {| { READ_TICKET ; PAIR } |}])@(t)[@inline][@hidden] in
  let never#23funtype a : * . never -> a = Λ a ->
  lambda (nnever)a return ([%Michelson {| { NEVER } |}])@(n)[@inline][@hidden] in
  let pairing_check#24list (( bls12_381_g1 * bls12_381_g2 )) -> bool =
    lambda (llist (( bls12_381_g1 * bls12_381_g2 )))bool return ([%Michelson {| { PAIRING_CHECK } |}])@(l)[@inline][@hidden] in
  let constant#25funtype a : * . string -> a = Λ a ->
  lambda (sstring)a return GLOBAL_CONSTANT(s)[@inline][@hidden] in
  let set_delegate#26option (key_hash) -> operation =
    lambda (ooption (key_hash))operation return SET_DELEGATE(o)[@inline][@hidden] in
  let get_contract#27funtype a : * . address -> contract (a) = Λ a ->
  lambda (aaddress)contract (a) return CONTRACT(a)[@inline][@hidden] in
  let get_contract_opt#28funtype a : * . address -> option (contract (a)) =
    Λ a ->
  lambda (aaddress)option (contract (a)) return CONTRACT_OPT(a)[@inline][@hidden] in
  let get_contract_with_error#29funtype a : * . address -> string -> contract (a) =
    Λ a ->
  lambda (aaddress)string -> contract (a) return lambda (sstring)contract (a) return
  CONTRACT_WITH_ERROR(a , s)[@inline][@hidden] in
  let create_ticket#30funtype a : * . a -> nat -> ticket (a) = Λ a ->
  lambda (va)nat -> ticket (a) return lambda (nnat)ticket (a) return ([%Michelson {| { UNPAIR ; TICKET } |}])@(
                                                                     ( v ,
                                                                      n ))[@inline][@hidden] in
  let transaction#31funtype a : * . a -> tez -> contract (a) -> operation =
    Λ a ->
  lambda (aa)tez -> contract (a) -> operation return lambda (mutez)contract (a) -> operation return lambda (ccontract (a))operation return
  CALL(a , mu , c)[@inline][@hidden] in
  let open_chest#32chest_key -> chest -> nat -> sum[Fail_decrypt -> unit , Fail_timelock -> unit , Ok_opening -> bytes] =
    lambda (ckchest_key)chest -> nat -> sum[Fail_decrypt -> unit , Fail_timelock -> unit , Ok_opening -> bytes] return lambda (cchest)nat ->
  sum[Fail_decrypt -> unit , Fail_timelock -> unit , Ok_opening -> bytes] return lambda (nnat)
  sum[Fail_decrypt -> unit , Fail_timelock -> unit , Ok_opening -> bytes] return
  OPEN_CHEST(ck , c , n)[@inline][@hidden] in
  let call_view#33funtype a : * . funtype b : * . string -> a -> address -> option (b) =
    Λ a ->
  Λ b ->
  lambda (sstring)a -> address -> option (b) return lambda (xa)address -> option (b) return lambda (aaddress)option (b) return
  VIEW(s , x , a)[@inline][@hidden] in
  let split_ticket#34funtype a : * . ticket (a) -> ( nat * nat ) -> option (
  ( ticket (a) * ticket (a) )) = Λ a ->
  lambda (tticket (a))( nat * nat ) -> option (( ticket (a) * ticket (a) )) return lambda (p
  ( nat * nat ))option (( ticket (a) * ticket (a) )) return ([%Michelson {| { UNPAIR ; SPLIT_TICKET } |}])@(
                                                            ( t ,
                                                              p ))[@inline][@hidden] in
  let xor#35nat -> nat -> nat =
    lambda (lnat)nat -> nat return lambda (rnat)nat return XOR(l , r)[@inline][@hidden] in
  let shift_left#36nat -> nat -> nat =
    lambda (lnat)nat -> nat return lambda (rnat)nat return LSL(l , r)[@inline][@hidden] in
  let shift_right#37nat -> nat -> nat =
    lambda (lnat)nat -> nat return lambda (rnat)nat return LSR(l , r)[@inline][@hidden] in
  let empty#38funtype k : * . funtype v : * . big_map (k , v) = Λ k ->
  Λ v ->  BIG_MAP_EMPTY()[@inline][@hidden] in
  let mem#39funtype k : * . funtype v : * . k -> big_map (k , v) -> bool =
    Λ k ->
  Λ v ->
  lambda (kk)big_map (k , v) -> bool return lambda (mbig_map (k ,
  v))bool return ([%Michelson {| { UNPAIR ; MEM } |}])@(( k , m ))[@inline][@hidden] in
  let add#40funtype k : * . funtype v : * . k -> v -> big_map (k ,
  v) -> big_map (k , v) = Λ k ->
  Λ v ->
  lambda (kk)v -> big_map (k , v) -> big_map (k ,
  v) return lambda (vv)big_map (k , v) -> big_map (k ,
  v) return lambda (mbig_map (k , v))big_map (k , v) return MAP_ADD(k , v , m)[@inline][@hidden] in
  let remove#41funtype k : * . funtype v : * . k -> big_map (k ,
  v) -> big_map (k , v) = Λ k ->
  Λ v ->
  lambda (kk)big_map (k , v) -> big_map (k , v) return lambda (mbig_map (k ,
  v))big_map (k , v) return MAP_REMOVE(k , m)[@inline][@hidden] in
  let update#42funtype k : * . funtype v : * . k -> option (v) -> big_map (k ,
  v) -> big_map (k , v) = Λ k ->
  Λ v ->
  lambda (kk)option (v) -> big_map (k , v) -> big_map (k ,
  v) return lambda (voption (v))big_map (k , v) -> big_map (k ,
  v) return lambda (mbig_map (k , v))big_map (k ,
  v) return MAP_UPDATE(k , v , m)[@inline][@hidden] in
  let get_and_update#43funtype k : * . funtype v : * . k -> option (v) -> big_map (k ,
  v) -> ( option (v) * big_map (k , v) ) = Λ k ->
  Λ v ->
  lambda (kk)option (v) -> big_map (k ,
  v) -> ( option (v) * big_map (k , v) ) return lambda (voption (v))big_map (k ,
  v) -> ( option (v) * big_map (k , v) ) return lambda (mbig_map (k ,
  v))( option (v) * big_map (k , v) ) return BIG_MAP_GET_AND_UPDATE(k , v , m)[@inline][@hidden] in
  let find_opt#44funtype k : * . funtype v : * . k -> big_map (k ,
  v) -> option (v) = Λ k ->
  Λ v ->
  lambda (kk)big_map (k , v) -> option (v) return lambda (mbig_map (k ,
  v))option (v) return MAP_FIND_OPT(k , m)[@inline][@hidden] in
  let find#45funtype k : * . funtype v : * . k -> big_map (k , v) -> v =
    Λ k ->
  Λ v ->
  lambda (kk)big_map (k , v) -> v return lambda (mbig_map (k ,
  v))v return MAP_FIND(k , m)[@inline][@hidden] in
  let empty#46funtype k : * . funtype v : * . map (k , v) = Λ k ->
  Λ v ->  MAP_EMPTY()[@inline][@hidden] in
  let size#47funtype k : * . funtype v : * . map (k , v) -> nat = Λ k ->
  Λ v ->  lambda (mmap (k , v))nat return ([%Michelson {| { SIZE } |}])@(m)[@inline][@hidden] in
  let mem#48funtype k : * . funtype v : * . k -> map (k , v) -> bool = Λ k ->
  Λ v ->
  lambda (kk)map (k , v) -> bool return lambda (mmap (k ,
  v))bool return ([%Michelson {| { UNPAIR ; MEM } |}])@(( k , m ))[@inline][@hidden] in
  let add#49funtype k : * . funtype v : * . k -> v -> map (k , v) -> map (k ,
  v) = Λ k ->
  Λ v ->
  lambda (kk)v -> map (k , v) -> map (k , v) return lambda (vv)map (k ,
  v) -> map (k , v) return lambda (mmap (k , v))map (k ,
  v) return MAP_ADD(k , v , m)[@inline][@hidden] in
  let remove#50funtype k : * . funtype v : * . k -> map (k , v) -> map (k ,
  v) = Λ k ->
  Λ v ->
  lambda (kk)map (k , v) -> map (k , v) return lambda (mmap (k , v))map (k ,
  v) return MAP_REMOVE(k , m)[@inline][@hidden] in
  let update#51funtype k : * . funtype v : * . k -> option (v) -> map (k ,
  v) -> map (k , v) = Λ k ->
  Λ v ->
  lambda (kk)option (v) -> map (k , v) -> map (k ,
  v) return lambda (voption (v))map (k , v) -> map (k ,
  v) return lambda (mmap (k , v))map (k , v) return MAP_UPDATE(k , v , m)[@inline][@hidden] in
  let get_and_update#52funtype k : * . funtype v : * . k -> option (v) -> map (k ,
  v) -> ( option (v) * map (k , v) ) = Λ k ->
  Λ v ->
  lambda (kk)option (v) -> map (k ,
  v) -> ( option (v) * map (k , v) ) return lambda (voption (v))map (k ,
  v) -> ( option (v) * map (k , v) ) return lambda (mmap (k ,
  v))( option (v) * map (k , v) ) return MAP_GET_AND_UPDATE(k , v , m)[@inline][@hidden] in
  let find#53funtype k : * . funtype v : * . k -> map (k , v) -> v = Λ k ->
  Λ v ->
  lambda (kk)map (k , v) -> v return lambda (mmap (k ,
  v))v return MAP_FIND(k , m)[@inline][@hidden] in
  let find_opt#54funtype k : * . funtype v : * . k -> map (k ,
  v) -> option (v) = Λ k ->
  Λ v ->
  lambda (kk)map (k , v) -> option (v) return lambda (mmap (k ,
  v))option (v) return MAP_FIND_OPT(k , m)[@inline][@hidden] in
  let iter#55funtype k : * . funtype v : * . ( k * v ) -> unit -> map (k ,
  v) -> unit = Λ k ->
  Λ v ->
  lambda (f( k * v ) -> unit)map (k , v) -> unit return lambda (mmap (k ,
  v))unit return MAP_ITER(f , m)[@inline][@hidden] in
  let map#56funtype k : * . funtype v : * . funtype w : * . ( k * v ) -> w -> map (k ,
  v) -> map (k , w) = Λ k ->
  Λ v ->
  Λ w ->
  lambda (f( k * v ) -> w)map (k , v) -> map (k , w) return lambda (mmap (k ,
  v))map (k , w) return MAP_MAP(f , m)[@inline][@hidden] in
  let fold#57funtype k : * . funtype v : * . funtype c : * . ( c * ( k * v ) ) -> c -> map (k ,
  v) -> c -> c = Λ k ->
  Λ v ->
  Λ c ->
  lambda (f( c * ( k * v ) ) -> c)map (k ,
  v) -> c -> c return lambda (mmap (k ,
  v))c -> c return lambda (ic)c return MAP_FOLD(f , m , i)[@inline][@hidden] in
  let empty#58funtype a : * . set (a) = Λ a ->
  SET_EMPTY()[@inline][@hidden] in
  let size#59funtype a : * . set (a) -> nat = Λ a ->
  lambda (sset (a))nat return ([%Michelson {| { SIZE } |}])@(s)[@inline][@hidden] in
  let cardinal#60funtype a : * . set (a) -> nat = Λ a ->
  lambda (sset (a))nat return ([%Michelson {| { SIZE } |}])@(s)[@inline][@hidden] in
  let mem#61funtype a : * . a -> set (a) -> bool = Λ a ->
  lambda (xa)set (a) -> bool return lambda (sset (a))bool return SET_MEM(x , s)[@inline][@hidden] in
  let add#62funtype a : * . a -> set (a) -> set (a) = Λ a ->
  lambda (xa)set (a) -> set (a) return lambda (sset (a))set (a) return
  SET_ADD(x , s)[@inline][@hidden] in
  let remove#63funtype a : * . a -> set (a) -> set (a) = Λ a ->
  lambda (xa)set (a) -> set (a) return lambda (sset (a))set (a) return
  SET_REMOVE(x , s)[@inline][@hidden] in
  let update#64funtype a : * . a -> bool -> set (a) -> set (a) = Λ a ->
  lambda (xa)bool -> set (a) -> set (a) return lambda (bbool)set (a) -> set (a) return lambda (sset (a))set (a) return
  SET_UPDATE(x , b , s)[@inline][@hidden] in
  let iter#65funtype a : * . a -> unit -> set (a) -> unit = Λ a ->
  lambda (fa -> unit)set (a) -> unit return lambda (sset (a))unit return
  SET_ITER(f , s)[@inline][@hidden] in
  let fold#66funtype a : * . funtype b : * . ( b * a ) -> b -> set (a) -> b -> b =
    Λ a ->
  Λ b ->
  lambda (f( b * a ) -> b)set (a) -> b -> b return lambda (sset (a))b -> b return lambda (ib)b return
  SET_FOLD(f , s , i)[@inline][@hidden] in
  let fold_desc#67funtype a : * . funtype b : * . ( a * b ) -> b -> set (a) -> b -> b =
    Λ a ->
  Λ b ->
  lambda (f( a * b ) -> b)set (a) -> b -> b return lambda (sset (a))b -> b return lambda (ib)b return
  SET_FOLD_DESC(f , s , i)[@inline][@hidden] in
  let length#68funtype a : * . list (a) -> nat = Λ a ->
  lambda (xslist (a))nat return ([%Michelson {| { SIZE } |}])@(xs)[@inline][@hidden] in
  let size#69funtype a : * . list (a) -> nat = Λ a ->
  lambda (xslist (a))nat return ([%Michelson {| { SIZE } |}])@(xs)[@inline][@hidden] in
  let head_opt#70funtype a : * . list (a) -> option (a) = Λ a ->
  lambda (xslist (a))option (a) return  match xs with
                                         | Cons ctor_proj#2 ->
                                            match ctor_proj#2 with
                                             | ( x , _#2 ) ->
                                             Some(x)
                                         | Nil unit_proj#4 ->
                                           None(unit)[@inline][@hidden] in
  let tail_opt#71funtype a : * . list (a) -> option (list (a)) = Λ a ->
  lambda (xslist (a))option (list (a)) return  match xs with
                                                | Cons ctor_proj#5 ->
                                                   match ctor_proj#5 with
                                                    | ( _#3 , xs ) ->
                                                    Some(xs)
                                                | Nil unit_proj#7 ->
                                                  None(unit)[@inline][@hidden] in
  let map#72funtype a : * . funtype b : * . a -> b -> list (a) -> list (b) =
    Λ a ->
  Λ b ->
  lambda (fa -> b)list (a) -> list (b) return lambda (xslist (a))list (b) return
  LIST_MAP(f , xs)[@inline][@hidden] in
  let iter#73funtype a : * . a -> unit -> list (a) -> unit = Λ a ->
  lambda (fa -> unit)list (a) -> unit return lambda (xslist (a))unit return
  LIST_ITER(f , xs)[@inline][@hidden] in
  let fold#74funtype a : * . funtype b : * . ( b * a ) -> b -> list (a) -> b -> b =
    Λ a ->
  Λ b ->
  lambda (f( b * a ) -> b)list (a) -> b -> b return lambda (xslist (a))b -> b return lambda (ib)b return
  LIST_FOLD(f , xs , i)[@inline][@hidden] in
  let fold_left#75funtype a : * . funtype b : * . ( b * a ) -> b -> b -> list (a) -> b =
    Λ a ->
  Λ b ->
  lambda (f( b * a ) -> b)b -> list (a) -> b return lambda (ib)list (a) -> b return lambda (xslist (a))b return
  LIST_FOLD_LEFT(f , i , xs)[@inline][@hidden] in
  let fold_right#76funtype a : * . funtype b : * . ( a * b ) -> b -> list (a) -> b -> b =
    Λ a ->
  Λ b ->
  lambda (f( a * b ) -> b)list (a) -> b -> b return lambda (xslist (a))b -> b return lambda (ib)b return
  LIST_FOLD_RIGHT(f , xs , i)[@inline][@hidden] in
  let cons#77funtype a : * . a -> list (a) -> list (a) = Λ a ->
  lambda (xa)list (a) -> list (a) return lambda (xslist (a))list (a) return
  CONS(x , xs)[@inline][@hidden] in
  let length#78string -> nat =
    lambda (bstring)nat return ([%Michelson {| { SIZE } |}])@(b)[@inline][@hidden] in
  let concat#79string -> string -> string =
    lambda (b1string)string -> string return lambda (b2string)string return
  ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b1 , b2 ))[@inline][@hidden] in
  let sub#80nat -> nat -> string -> string =
    lambda (snat)nat -> string -> string return lambda (lnat)string -> string return lambda (bstring)string return
  ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(
  ( s ,
    l ,
    b ))[@inline][@hidden] in
  let unopt#81funtype a : * . option (a) -> a = Λ a ->
  lambda (voption (a))a return UNOPT(v)[@inline][@hidden] in
  let unopt_with_error#82funtype a : * . option (a) -> string -> a = Λ a ->
  lambda (voption (a))string -> a return lambda (sstring)a return UNOPT_WITH_ERROR
                                                                  (v ,
                                                                   s)[@inline][@hidden] in
  let pack#83funtype a : * . a -> bytes = Λ a ->
  lambda (va)bytes return ([%Michelson {| { PACK } |}])@(v)[@inline][@hidden] in
  let unpack#84funtype a : * . bytes -> option (a) = Λ a ->
  lambda (bbytes)option (a) return BYTES_UNPACK(b)[@inline][@hidden] in
  let length#85bytes -> nat =
    lambda (bbytes)nat return ([%Michelson {| { SIZE } |}])@(b)[@inline][@hidden] in
  let concat#86bytes -> bytes -> bytes =
    lambda (b1bytes)bytes -> bytes return lambda (b2bytes)bytes return
  ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b1 , b2 ))[@inline][@hidden] in
  let sub#87nat -> nat -> bytes -> bytes =
    lambda (snat)nat -> bytes -> bytes return lambda (lnat)bytes -> bytes return lambda (bbytes)bytes return
  ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(
  ( s ,
    l ,
    b ))[@inline][@hidden] in
  let blake2b#88bytes -> bytes =
    lambda (bbytes)bytes return ([%Michelson {| { BLAKE2B } |}])@(b)[@inline][@hidden] in
  let sha256#89bytes -> bytes =
    lambda (bbytes)bytes return ([%Michelson {| { SHA256 } |}])@(b)[@inline][@hidden] in
  let sha512#90bytes -> bytes =
    lambda (bbytes)bytes return ([%Michelson {| { SHA512 } |}])@(b)[@inline][@hidden] in
  let sha3#91bytes -> bytes =
    lambda (bbytes)bytes return ([%Michelson {| { SHA3 } |}])@(b)[@inline][@hidden] in
  let keccak#92bytes -> bytes =
    lambda (bbytes)bytes return ([%Michelson {| { KECCAK } |}])@(b)[@inline][@hidden] in
  let hash_key#93key -> key_hash =
    lambda (kkey)key_hash return ([%Michelson {| { HASH_KEY } |}])@(k)[@inline][@hidden] in
  let check#94key -> signature -> bytes -> bool =
    lambda (kkey)signature -> bytes -> bool return lambda (ssignature)bytes -> bool return lambda (bbytes)bool return
  ([%Michelson {| { UNPAIR ; UNPAIR ; CHECK_SIGNATURE } |}])@(( k , s , b ))[@inline][@hidden] in
  let assertbool -> unit =
    lambda (bbool)unit return ([%Michelson {| { IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } } |}])@(b)[@inline][@private][@hidden] in
  let assert_somefuntype a : * . option (a) -> unit = Λ a ->
  lambda (voption (a))unit return ([%Michelson {| { IF_NONE { PUSH string "failed assert some" ; FAILWITH } { DROP ; UNIT } } |}])@(v)[@inline][@private][@hidden] in
  let assert_nonefuntype a : * . option (a) -> unit = Λ a ->
  lambda (voption (a))unit return ([%Michelson {| { IF_NONE { UNIT } { PUSH string "failed assert none" ; FAILWITH } } |}])@(v)[@inline][@private][@hidden] in
  let absint -> nat =
    lambda (iint)nat return ([%Michelson {| { ABS } |}])@(i)[@inline][@private][@hidden] in
  let is_natint -> option (nat) =
    lambda (iint)option (nat) return ([%Michelson {| { ISNAT } |}])@(i)[@inline][@private][@hidden] in
  let truebool = TRUE()[@inline][@private][@hidden] in
  let falsebool = FALSE()[@inline][@private][@hidden] in
  let unitunit = UNIT()[@inline][@private][@hidden] in
  let failwithfuntype a : * . funtype b : * . a -> b = Λ a ->
  Λ b ->  [%Michelson {|{ FAILWITH }|}][@inline][@private][@hidden] in
  let intfuntype a : * . a -> external_int (a) = Λ a ->
  lambda (va)external_int (a) return ([%Michelson {| { INT } |}])@(v)[@inline][@private][@hidden] in
  let assert_with_errorbool -> string -> unit =
    lambda (bbool)string -> unit return lambda (sstring)unit return ([%Michelson {| { UNPAIR ; IF { DROP ; UNIT } { FAILWITH } } |}])@(
                                                                    ( b ,
                                                                      s ))[@inline][@private][@hidden] in
  let assert_some_with_errorfuntype a : * . option (a) -> string -> unit =
    Λ a ->
  lambda (voption (a))string -> unit return lambda (sstring)unit return
  ([%Michelson {| { UNPAIR ; IF_NONE { FAILWITH } { DROP 2 ; UNIT } } |}])@(
  ( v ,
    s ))[@inline][@private][@hidden] in
  let assert_none_with_errorfuntype a : * . option (a) -> string -> unit =
    Λ a ->
  lambda (voption (a))string -> unit return lambda (sstring)unit return
  ([%Michelson {| { UNPAIR ; IF_NONE { DROP ; UNIT } { DROP ; FAILWITH } } |}])@(
  ( v ,
    s ))[@inline][@private][@hidden] in
  let edivfuntype a : * . funtype b : * . a -> b -> external_ediv (a , b) =
    Λ a ->
  Λ b ->
  lambda (la)b -> external_ediv (a , b) return lambda (rb)external_ediv (a ,
  b) return ([%Michelson {| { UNPAIR ; EDIV } |}])@(( l , r ))[@inline][@private][@hidden] in
  let stub#95funtype a : * . funtype b : * . a -> b = Λ a ->
  Λ b ->  lambda (xa)b return ([%Michelson {|{ FAILWITH }|}])@(x)[@inline][@private][@hidden] in
  let run#96funtype a : * . funtype b : * . a -> b -> a -> unit = Λ a ->
  Λ b ->
  lambda (_fa -> b)a -> unit return lambda (_va)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let eval#97funtype a : * . a -> unit = Λ a ->
  lambda (_xa)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let compile_value#98funtype a : * . a -> unit = Λ a ->
  lambda (_xa)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let get_total_voting_power#99unit -> nat =
    lambda (_uunit)nat return (stub#95@{unit}@{nat})@(unit)[@inline][@hidden] in
  let failwith#100funtype a : * . funtype b : * . a -> b = Λ a ->
  Λ b ->  lambda (_va)b return (stub#95@{unit}@{b})@(unit)[@inline][@hidden] in
  let to_contract#101funtype p : * . funtype s : * . unit -> contract (p) =
    Λ p ->
  Λ s ->
  lambda (_tunit)contract (p) return (stub#95@{unit}@{contract (p)})@(unit)[@inline][@hidden] in
  let set_source#102address -> unit =
    lambda (_aaddress)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let get_storage_of_address#103address -> unit =
    lambda (_aaddress)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let get_balance#104address -> tez =
    lambda (_aaddress)tez return (stub#95@{unit}@{tez})@(unit)[@inline][@hidden] in
  let print#105string -> unit =
    lambda (_vstring)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let eprint#106string -> unit =
    lambda (_vstring)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let get_voting_power#107key_hash -> nat =
    lambda (_khkey_hash)nat return (stub#95@{unit}@{nat})@(unit)[@inline][@hidden] in
  let nth_bootstrap_contract#108nat -> address =
    lambda (_inat)address return (stub#95@{unit}@{address})@(unit)[@inline][@hidden] in
  let nth_bootstrap_account#109int -> address =
    lambda (_iint)address return (stub#95@{unit}@{address})@(unit)[@inline][@hidden] in
  let get_bootstrap_account#110nat -> ( address * key * string ) =
    lambda (_nnat)( address * key * string ) return (stub#95@{unit}@{( address * key * string )})@(unit)[@inline][@hidden] in
  let nth_bootstrap_typed_address#111funtype a : * . funtype b : * . nat -> unit =
    Λ a ->
  Λ b ->  lambda (_nnat)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let last_originations#112unit -> map (address , list (address)) =
    lambda (_uunit)map (address ,
  list (address)) return (stub#95@{unit}@{map (address ,
                         list (address))})@(unit)[@inline][@hidden] in
  let random#113funtype a : * . unit -> a = Λ a ->
  lambda (_uunit)a return (stub#95@{unit}@{a})@(unit)[@inline][@hidden] in
  let new_account#114unit -> ( string * key ) =
    lambda (_uunit)( string * key ) return (stub#95@{unit}@{( string * key )})@(unit)[@inline][@hidden] in
  let decompile#115funtype a : * . unit -> a = Λ a ->
  lambda (_munit)a return (stub#95@{unit}@{a})@(unit)[@inline][@hidden] in
  let bake_until_n_cycle_end#116nat -> unit =
    lambda (_nnat)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let cast_address#117funtype a : * . funtype b : * . address -> unit =
    Λ a ->
  Λ b ->  lambda (_aaddress)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let register_delegate#118key_hash -> unit =
    lambda (_khkey_hash)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let register_constant#119unit -> string =
    lambda (_munit)string return (stub#95@{unit}@{string})@(unit)[@inline][@hidden] in
  let to_typed_address#120funtype a : * . funtype b : * . contract (a) -> unit =
    Λ a ->
  Λ b ->  lambda (_ccontract (a))unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let constant_to_michelson_program#121string -> unit =
    lambda (_sstring)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let restore_context#122unit -> unit =
    lambda (_uunit)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let save_context#123unit -> unit =
    lambda (_uunit)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let drop_context#124unit -> unit =
    lambda (_uunit)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let to_string#125funtype a : * . a -> string = Λ a ->
  lambda (_va)string return (stub#95@{unit}@{string})@(unit)[@inline][@hidden] in
  let get_storage#126funtype p : * . funtype s : * . unit -> s = Λ p ->
  Λ s ->  lambda (_tunit)s return (stub#95@{unit}@{s})@(unit)[@inline][@hidden] in
  let set_baker_policy#127unit -> unit =
    lambda (_bpunit)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let set_baker#128address -> unit =
    lambda (_aaddress)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let size#129unit -> int =
    lambda (_cunit)int return (stub#95@{unit}@{int})@(unit)[@inline][@hidden] in
  let compile_contract#130funtype p : * . funtype s : * . ( p * s ) ->
  ( list (operation) * s ) -> unit = Λ p ->
  Λ s ->
  lambda (_f( p * s ) -> ( list (operation) * s ))unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let read_contract_from_file#131string -> unit =
    lambda (_fnstring)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let chr#132nat -> option (string) =
    lambda (_nnat)option (string) return (stub#95@{unit}@{option (string)})@(unit)[@inline][@hidden] in
  let nl#133string = "NEWLINE"[@inline][@hidden] in
  let println#134string -> unit =
    lambda (_vstring)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let transfer#135address -> unit -> tez -> unit =
    lambda (_aaddress)unit -> tez -> unit return lambda (_sunit)tez -> unit return lambda (_ttez)unit return
  (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let transfer_exn#136address -> unit -> tez -> nat =
    lambda (_aaddress)unit -> tez -> nat return lambda (_sunit)tez -> nat return lambda (_ttez)nat return
  (stub#95@{unit}@{nat})@(unit)[@inline][@hidden] in
  let log#137funtype a : * . a -> unit = Λ a ->
  lambda (_va)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let reset_state#138nat -> list (tez) -> unit =
    lambda (_nnat)list (tez) -> unit return lambda (_llist (tez))unit return
  (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let reset_state_at#139timestamp -> nat -> list (tez) -> unit =
    lambda (_ttimestamp)nat -> list (tez) -> unit return lambda (_nnat)list (tez) -> unit return lambda (_llist (tez))unit return
  (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let bootstrap_contract#140funtype p : * . funtype s : * . ( p * s ) ->
  ( list (operation) * s ) -> s -> tez -> unit = Λ p ->
  Λ s ->
  lambda (_f( p * s ) -> ( list (operation) * s ))s -> tez -> unit return lambda (_ss)tez -> unit return lambda (_ttez)unit return
  (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let mutate_value#141funtype a : * . nat -> a -> option (( a * unit )) =
    Λ a ->
  lambda (_nnat)a -> option (( a * unit )) return lambda (_va)option (( a * unit )) return
  (stub#95@{unit}@{option (( a * unit ))})@(unit)[@inline][@hidden] in
  let save_mutation#142string -> unit -> option (string) =
    lambda (_sstring)unit -> option (string) return lambda (_munit)option (string) return
  (stub#95@{unit}@{option (string)})@(unit)[@inline][@hidden] in
  let mutation_test#143funtype a : * . funtype b : * . a -> a -> b -> option (
  ( b * unit )) = Λ a ->
  Λ b ->
  lambda (_va)a -> b -> option (( b * unit )) return lambda (_fa -> b)option (
  ( b * unit )) return (stub#95@{unit}@{option (( b * unit ))})@(unit)[@inline][@hidden] in
  let mutation_test_all#144funtype a : * . funtype b : * . a -> a -> b -> list (
  ( b * unit )) = Λ a ->
  Λ b ->
  lambda (_va)a -> b -> list (( b * unit )) return lambda (_fa -> b)list (
  ( b * unit )) return (stub#95@{unit}@{list (( b * unit ))})@(unit)[@inline][@hidden] in
  let sign#145string -> bytes -> signature =
    lambda (_skstring)bytes -> signature return lambda (_dbytes)signature return
  (stub#95@{unit}@{signature})@(unit)[@inline][@hidden] in
  let add_account#146string -> key -> unit =
    lambda (_sstring)key -> unit return lambda (_kkey)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let baker_account#147( string * key ) -> option (tez) -> unit =
    lambda (_p( string * key ))option (tez) -> unit return lambda (_ooption (tez))unit return
  (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let set_big_map#148funtype a : * . funtype b : * . int -> big_map (a ,
  b) -> unit = Λ a ->
  Λ b ->
  lambda (_iint)big_map (a , b) -> unit return lambda (_mbig_map (a ,
  b))unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let create_chest#149bytes -> nat -> ( chest * chest_key ) =
    lambda (_bbytes)nat -> ( chest * chest_key ) return lambda (_nnat)( chest * chest_key ) return
  (stub#95@{unit}@{( chest * chest_key )})@(unit)[@inline][@hidden] in
  let create_chest_key#150chest -> nat -> chest_key =
    lambda (_cchest)nat -> chest_key return lambda (_nnat)chest_key return
  (stub#95@{unit}@{chest_key})@(unit)[@inline][@hidden] in
  let transfer_to_contract#151funtype p : * . contract (p) -> p -> tez -> unit =
    Λ p ->
  lambda (_ccontract (p))p -> tez -> unit return lambda (_sp)tez -> unit return lambda (_ttez)unit return
  (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let transfer_to_contract_exn#152funtype p : * . contract (p) -> p -> tez -> nat =
    Λ p ->
  lambda (_ccontract (p))p -> tez -> nat return lambda (_sp)tez -> nat return lambda (_ttez)nat return
  (stub#95@{unit}@{nat})@(unit)[@inline][@hidden] in
  let michelson_equal#153unit -> unit -> bool =
    lambda (_m1unit)unit -> bool return lambda (_m2unit)bool return (stub#95@{unit}@{bool})@(unit)[@inline][@hidden] in
  let to_entrypoint#154funtype a : * . funtype b : * . funtype c : * . string -> unit -> contract (c) =
    Λ a ->
  Λ b ->
  Λ c ->
  lambda (_sstring)unit -> contract (c) return lambda (_tunit)contract (c) return
  (stub#95@{unit}@{contract (c)})@(unit)[@inline][@hidden] in
  let originate_contract#155unit -> unit -> tez -> address =
    lambda (_cunit)unit -> tez -> address return lambda (_sunit)tez -> address return lambda (_ttez)address return
  (stub#95@{unit}@{address})@(unit)[@inline][@hidden] in
  let originate#156funtype p : * . funtype s : * . ( p * s ) -> ( list (operation) * s ) -> s -> tez ->
  ( unit * unit * int ) = Λ p ->
  Λ s ->
  lambda (_f( p * s ) -> ( list (operation) * s ))s -> tez -> ( unit * unit * int ) return lambda (_ss)tez ->
  ( unit * unit * int ) return lambda (_ttez)( unit * unit * int ) return
  (stub#95@{unit}@{( unit * unit * int )})@(unit)[@inline][@hidden] in
  let compile_contract_from_file#157string -> string -> list (string) -> unit =
    lambda (_fnstring)string -> list (string) -> unit return lambda (_estring)list (string) -> unit return lambda (_vlist (string))unit return
  (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let originate_from_file#158string -> string -> list (string) -> unit -> tez ->
  ( address * unit * int ) =
    lambda (_fnstring)string -> list (string) -> unit -> tez -> ( address * unit * int ) return lambda (_estring)list (string) -> unit -> tez ->
  ( address * unit * int ) return lambda (_vlist (string))unit -> tez ->
  ( address * unit * int ) return lambda (_sunit)tez -> ( address * unit * int ) return lambda (_ttez)
  ( address * unit * int ) return (stub#95@{unit}@{( address * unit * int )})@(unit)[@inline][@hidden] in
  let x#159int = 19 in
  let y#160int = 22 in
  let xint =
    let xint = 1 in let uint = x#159 in let vint = y#160 in ADD(ADD(u , v) , x) in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias11.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias12.mligo" ] ;
  [%expect {xxx|
  let get_balance#8unit -> tez =
    lambda (_uunit)tez return ([%Michelson {| { DROP ; BALANCE } |}])@(unit)[@inline][@hidden] in
  let get_amount#9unit -> tez =
    lambda (_uunit)tez return ([%Michelson {| { DROP ; AMOUNT } |}])@(unit)[@inline][@hidden] in
  let get_now#10unit -> timestamp =
    lambda (_uunit)timestamp return ([%Michelson {| { DROP ; NOW } |}])@(unit)[@inline][@hidden] in
  let get_sender#11unit -> address =
    lambda (_uunit)address return ([%Michelson {| { DROP ; SENDER } |}])@(unit)[@inline][@hidden] in
  let get_source#12unit -> address =
    lambda (_uunit)address return ([%Michelson {| { DROP ; SOURCE } |}])@(unit)[@inline][@hidden] in
  let get_level#13unit -> nat =
    lambda (_uunit)nat return ([%Michelson {| { DROP ; LEVEL } |}])@(unit)[@inline][@hidden] in
  let get_self_address#14unit -> address =
    lambda (_uunit)address return SELF_ADDRESS()[@inline][@hidden] in
  let get_chain_id#15unit -> chain_id =
    lambda (_uunit)chain_id return ([%Michelson {| { DROP ; CHAIN_ID } |}])@(unit)[@inline][@hidden] in
  let get_total_voting_power#16unit -> nat =
    lambda (_uunit)nat return ([%Michelson {| { DROP ; TOTAL_VOTING_POWER } |}])@(unit)[@inline][@hidden] in
  let get_min_block_time#17unit -> nat =
    lambda (_uunit)nat return ([%Michelson {| { DROP; MIN_BLOCK_TIME } |}])@(unit)[@inline][@hidden] in
  let voting_power#18key_hash -> nat =
    lambda (khkey_hash)nat return ([%Michelson {| { VOTING_POWER } |}])@(kh)[@inline][@hidden] in
  let address#19funtype a : * . contract (a) -> address = Λ a ->
  lambda (ccontract (a))address return ADDRESS(c)[@inline][@hidden] in
  let implicit_account#20key_hash -> contract (unit) =
    lambda (khkey_hash)contract (unit) return IMPLICIT_ACCOUNT(kh)[@inline][@hidden] in
  let join_tickets#21funtype a : * . ( ticket (a) * ticket (a) ) -> option (ticket (a)) =
    Λ a ->
  lambda (t( ticket (a) * ticket (a) ))option (ticket (a)) return ([%Michelson {| { JOIN_TICKETS } |}])@(t)[@inline][@hidden] in
  let read_ticket#22funtype a : * . ticket (a) -> ( ( address * ( a * nat ) ) * ticket (a) ) =
    Λ a ->
  lambda (tticket (a))( ( address * ( a * nat ) ) * ticket (a) ) return
  ([%Michelson {| { READ_TICKET ; PAIR } |}])@(t)[@inline][@hidden] in
  let never#23funtype a : * . never -> a = Λ a ->
  lambda (nnever)a return ([%Michelson {| { NEVER } |}])@(n)[@inline][@hidden] in
  let pairing_check#24list (( bls12_381_g1 * bls12_381_g2 )) -> bool =
    lambda (llist (( bls12_381_g1 * bls12_381_g2 )))bool return ([%Michelson {| { PAIRING_CHECK } |}])@(l)[@inline][@hidden] in
  let constant#25funtype a : * . string -> a = Λ a ->
  lambda (sstring)a return GLOBAL_CONSTANT(s)[@inline][@hidden] in
  let set_delegate#26option (key_hash) -> operation =
    lambda (ooption (key_hash))operation return SET_DELEGATE(o)[@inline][@hidden] in
  let get_contract#27funtype a : * . address -> contract (a) = Λ a ->
  lambda (aaddress)contract (a) return CONTRACT(a)[@inline][@hidden] in
  let get_contract_opt#28funtype a : * . address -> option (contract (a)) =
    Λ a ->
  lambda (aaddress)option (contract (a)) return CONTRACT_OPT(a)[@inline][@hidden] in
  let get_contract_with_error#29funtype a : * . address -> string -> contract (a) =
    Λ a ->
  lambda (aaddress)string -> contract (a) return lambda (sstring)contract (a) return
  CONTRACT_WITH_ERROR(a , s)[@inline][@hidden] in
  let create_ticket#30funtype a : * . a -> nat -> ticket (a) = Λ a ->
  lambda (va)nat -> ticket (a) return lambda (nnat)ticket (a) return ([%Michelson {| { UNPAIR ; TICKET } |}])@(
                                                                     ( v ,
                                                                      n ))[@inline][@hidden] in
  let transaction#31funtype a : * . a -> tez -> contract (a) -> operation =
    Λ a ->
  lambda (aa)tez -> contract (a) -> operation return lambda (mutez)contract (a) -> operation return lambda (ccontract (a))operation return
  CALL(a , mu , c)[@inline][@hidden] in
  let open_chest#32chest_key -> chest -> nat -> sum[Fail_decrypt -> unit , Fail_timelock -> unit , Ok_opening -> bytes] =
    lambda (ckchest_key)chest -> nat -> sum[Fail_decrypt -> unit , Fail_timelock -> unit , Ok_opening -> bytes] return lambda (cchest)nat ->
  sum[Fail_decrypt -> unit , Fail_timelock -> unit , Ok_opening -> bytes] return lambda (nnat)
  sum[Fail_decrypt -> unit , Fail_timelock -> unit , Ok_opening -> bytes] return
  OPEN_CHEST(ck , c , n)[@inline][@hidden] in
  let call_view#33funtype a : * . funtype b : * . string -> a -> address -> option (b) =
    Λ a ->
  Λ b ->
  lambda (sstring)a -> address -> option (b) return lambda (xa)address -> option (b) return lambda (aaddress)option (b) return
  VIEW(s , x , a)[@inline][@hidden] in
  let split_ticket#34funtype a : * . ticket (a) -> ( nat * nat ) -> option (
  ( ticket (a) * ticket (a) )) = Λ a ->
  lambda (tticket (a))( nat * nat ) -> option (( ticket (a) * ticket (a) )) return lambda (p
  ( nat * nat ))option (( ticket (a) * ticket (a) )) return ([%Michelson {| { UNPAIR ; SPLIT_TICKET } |}])@(
                                                            ( t ,
                                                              p ))[@inline][@hidden] in
  let xor#35nat -> nat -> nat =
    lambda (lnat)nat -> nat return lambda (rnat)nat return XOR(l , r)[@inline][@hidden] in
  let shift_left#36nat -> nat -> nat =
    lambda (lnat)nat -> nat return lambda (rnat)nat return LSL(l , r)[@inline][@hidden] in
  let shift_right#37nat -> nat -> nat =
    lambda (lnat)nat -> nat return lambda (rnat)nat return LSR(l , r)[@inline][@hidden] in
  let empty#38funtype k : * . funtype v : * . big_map (k , v) = Λ k ->
  Λ v ->  BIG_MAP_EMPTY()[@inline][@hidden] in
  let mem#39funtype k : * . funtype v : * . k -> big_map (k , v) -> bool =
    Λ k ->
  Λ v ->
  lambda (kk)big_map (k , v) -> bool return lambda (mbig_map (k ,
  v))bool return ([%Michelson {| { UNPAIR ; MEM } |}])@(( k , m ))[@inline][@hidden] in
  let add#40funtype k : * . funtype v : * . k -> v -> big_map (k ,
  v) -> big_map (k , v) = Λ k ->
  Λ v ->
  lambda (kk)v -> big_map (k , v) -> big_map (k ,
  v) return lambda (vv)big_map (k , v) -> big_map (k ,
  v) return lambda (mbig_map (k , v))big_map (k , v) return MAP_ADD(k , v , m)[@inline][@hidden] in
  let remove#41funtype k : * . funtype v : * . k -> big_map (k ,
  v) -> big_map (k , v) = Λ k ->
  Λ v ->
  lambda (kk)big_map (k , v) -> big_map (k , v) return lambda (mbig_map (k ,
  v))big_map (k , v) return MAP_REMOVE(k , m)[@inline][@hidden] in
  let update#42funtype k : * . funtype v : * . k -> option (v) -> big_map (k ,
  v) -> big_map (k , v) = Λ k ->
  Λ v ->
  lambda (kk)option (v) -> big_map (k , v) -> big_map (k ,
  v) return lambda (voption (v))big_map (k , v) -> big_map (k ,
  v) return lambda (mbig_map (k , v))big_map (k ,
  v) return MAP_UPDATE(k , v , m)[@inline][@hidden] in
  let get_and_update#43funtype k : * . funtype v : * . k -> option (v) -> big_map (k ,
  v) -> ( option (v) * big_map (k , v) ) = Λ k ->
  Λ v ->
  lambda (kk)option (v) -> big_map (k ,
  v) -> ( option (v) * big_map (k , v) ) return lambda (voption (v))big_map (k ,
  v) -> ( option (v) * big_map (k , v) ) return lambda (mbig_map (k ,
  v))( option (v) * big_map (k , v) ) return BIG_MAP_GET_AND_UPDATE(k , v , m)[@inline][@hidden] in
  let find_opt#44funtype k : * . funtype v : * . k -> big_map (k ,
  v) -> option (v) = Λ k ->
  Λ v ->
  lambda (kk)big_map (k , v) -> option (v) return lambda (mbig_map (k ,
  v))option (v) return MAP_FIND_OPT(k , m)[@inline][@hidden] in
  let find#45funtype k : * . funtype v : * . k -> big_map (k , v) -> v =
    Λ k ->
  Λ v ->
  lambda (kk)big_map (k , v) -> v return lambda (mbig_map (k ,
  v))v return MAP_FIND(k , m)[@inline][@hidden] in
  let empty#46funtype k : * . funtype v : * . map (k , v) = Λ k ->
  Λ v ->  MAP_EMPTY()[@inline][@hidden] in
  let size#47funtype k : * . funtype v : * . map (k , v) -> nat = Λ k ->
  Λ v ->  lambda (mmap (k , v))nat return ([%Michelson {| { SIZE } |}])@(m)[@inline][@hidden] in
  let mem#48funtype k : * . funtype v : * . k -> map (k , v) -> bool = Λ k ->
  Λ v ->
  lambda (kk)map (k , v) -> bool return lambda (mmap (k ,
  v))bool return ([%Michelson {| { UNPAIR ; MEM } |}])@(( k , m ))[@inline][@hidden] in
  let add#49funtype k : * . funtype v : * . k -> v -> map (k , v) -> map (k ,
  v) = Λ k ->
  Λ v ->
  lambda (kk)v -> map (k , v) -> map (k , v) return lambda (vv)map (k ,
  v) -> map (k , v) return lambda (mmap (k , v))map (k ,
  v) return MAP_ADD(k , v , m)[@inline][@hidden] in
  let remove#50funtype k : * . funtype v : * . k -> map (k , v) -> map (k ,
  v) = Λ k ->
  Λ v ->
  lambda (kk)map (k , v) -> map (k , v) return lambda (mmap (k , v))map (k ,
  v) return MAP_REMOVE(k , m)[@inline][@hidden] in
  let update#51funtype k : * . funtype v : * . k -> option (v) -> map (k ,
  v) -> map (k , v) = Λ k ->
  Λ v ->
  lambda (kk)option (v) -> map (k , v) -> map (k ,
  v) return lambda (voption (v))map (k , v) -> map (k ,
  v) return lambda (mmap (k , v))map (k , v) return MAP_UPDATE(k , v , m)[@inline][@hidden] in
  let get_and_update#52funtype k : * . funtype v : * . k -> option (v) -> map (k ,
  v) -> ( option (v) * map (k , v) ) = Λ k ->
  Λ v ->
  lambda (kk)option (v) -> map (k ,
  v) -> ( option (v) * map (k , v) ) return lambda (voption (v))map (k ,
  v) -> ( option (v) * map (k , v) ) return lambda (mmap (k ,
  v))( option (v) * map (k , v) ) return MAP_GET_AND_UPDATE(k , v , m)[@inline][@hidden] in
  let find#53funtype k : * . funtype v : * . k -> map (k , v) -> v = Λ k ->
  Λ v ->
  lambda (kk)map (k , v) -> v return lambda (mmap (k ,
  v))v return MAP_FIND(k , m)[@inline][@hidden] in
  let find_opt#54funtype k : * . funtype v : * . k -> map (k ,
  v) -> option (v) = Λ k ->
  Λ v ->
  lambda (kk)map (k , v) -> option (v) return lambda (mmap (k ,
  v))option (v) return MAP_FIND_OPT(k , m)[@inline][@hidden] in
  let iter#55funtype k : * . funtype v : * . ( k * v ) -> unit -> map (k ,
  v) -> unit = Λ k ->
  Λ v ->
  lambda (f( k * v ) -> unit)map (k , v) -> unit return lambda (mmap (k ,
  v))unit return MAP_ITER(f , m)[@inline][@hidden] in
  let map#56funtype k : * . funtype v : * . funtype w : * . ( k * v ) -> w -> map (k ,
  v) -> map (k , w) = Λ k ->
  Λ v ->
  Λ w ->
  lambda (f( k * v ) -> w)map (k , v) -> map (k , w) return lambda (mmap (k ,
  v))map (k , w) return MAP_MAP(f , m)[@inline][@hidden] in
  let fold#57funtype k : * . funtype v : * . funtype c : * . ( c * ( k * v ) ) -> c -> map (k ,
  v) -> c -> c = Λ k ->
  Λ v ->
  Λ c ->
  lambda (f( c * ( k * v ) ) -> c)map (k ,
  v) -> c -> c return lambda (mmap (k ,
  v))c -> c return lambda (ic)c return MAP_FOLD(f , m , i)[@inline][@hidden] in
  let empty#58funtype a : * . set (a) = Λ a ->
  SET_EMPTY()[@inline][@hidden] in
  let size#59funtype a : * . set (a) -> nat = Λ a ->
  lambda (sset (a))nat return ([%Michelson {| { SIZE } |}])@(s)[@inline][@hidden] in
  let cardinal#60funtype a : * . set (a) -> nat = Λ a ->
  lambda (sset (a))nat return ([%Michelson {| { SIZE } |}])@(s)[@inline][@hidden] in
  let mem#61funtype a : * . a -> set (a) -> bool = Λ a ->
  lambda (xa)set (a) -> bool return lambda (sset (a))bool return SET_MEM(x , s)[@inline][@hidden] in
  let add#62funtype a : * . a -> set (a) -> set (a) = Λ a ->
  lambda (xa)set (a) -> set (a) return lambda (sset (a))set (a) return
  SET_ADD(x , s)[@inline][@hidden] in
  let remove#63funtype a : * . a -> set (a) -> set (a) = Λ a ->
  lambda (xa)set (a) -> set (a) return lambda (sset (a))set (a) return
  SET_REMOVE(x , s)[@inline][@hidden] in
  let update#64funtype a : * . a -> bool -> set (a) -> set (a) = Λ a ->
  lambda (xa)bool -> set (a) -> set (a) return lambda (bbool)set (a) -> set (a) return lambda (sset (a))set (a) return
  SET_UPDATE(x , b , s)[@inline][@hidden] in
  let iter#65funtype a : * . a -> unit -> set (a) -> unit = Λ a ->
  lambda (fa -> unit)set (a) -> unit return lambda (sset (a))unit return
  SET_ITER(f , s)[@inline][@hidden] in
  let fold#66funtype a : * . funtype b : * . ( b * a ) -> b -> set (a) -> b -> b =
    Λ a ->
  Λ b ->
  lambda (f( b * a ) -> b)set (a) -> b -> b return lambda (sset (a))b -> b return lambda (ib)b return
  SET_FOLD(f , s , i)[@inline][@hidden] in
  let fold_desc#67funtype a : * . funtype b : * . ( a * b ) -> b -> set (a) -> b -> b =
    Λ a ->
  Λ b ->
  lambda (f( a * b ) -> b)set (a) -> b -> b return lambda (sset (a))b -> b return lambda (ib)b return
  SET_FOLD_DESC(f , s , i)[@inline][@hidden] in
  let length#68funtype a : * . list (a) -> nat = Λ a ->
  lambda (xslist (a))nat return ([%Michelson {| { SIZE } |}])@(xs)[@inline][@hidden] in
  let size#69funtype a : * . list (a) -> nat = Λ a ->
  lambda (xslist (a))nat return ([%Michelson {| { SIZE } |}])@(xs)[@inline][@hidden] in
  let head_opt#70funtype a : * . list (a) -> option (a) = Λ a ->
  lambda (xslist (a))option (a) return  match xs with
                                         | Cons ctor_proj#2 ->
                                            match ctor_proj#2 with
                                             | ( x , _#2 ) ->
                                             Some(x)
                                         | Nil unit_proj#4 ->
                                           None(unit)[@inline][@hidden] in
  let tail_opt#71funtype a : * . list (a) -> option (list (a)) = Λ a ->
  lambda (xslist (a))option (list (a)) return  match xs with
                                                | Cons ctor_proj#5 ->
                                                   match ctor_proj#5 with
                                                    | ( _#3 , xs ) ->
                                                    Some(xs)
                                                | Nil unit_proj#7 ->
                                                  None(unit)[@inline][@hidden] in
  let map#72funtype a : * . funtype b : * . a -> b -> list (a) -> list (b) =
    Λ a ->
  Λ b ->
  lambda (fa -> b)list (a) -> list (b) return lambda (xslist (a))list (b) return
  LIST_MAP(f , xs)[@inline][@hidden] in
  let iter#73funtype a : * . a -> unit -> list (a) -> unit = Λ a ->
  lambda (fa -> unit)list (a) -> unit return lambda (xslist (a))unit return
  LIST_ITER(f , xs)[@inline][@hidden] in
  let fold#74funtype a : * . funtype b : * . ( b * a ) -> b -> list (a) -> b -> b =
    Λ a ->
  Λ b ->
  lambda (f( b * a ) -> b)list (a) -> b -> b return lambda (xslist (a))b -> b return lambda (ib)b return
  LIST_FOLD(f , xs , i)[@inline][@hidden] in
  let fold_left#75funtype a : * . funtype b : * . ( b * a ) -> b -> b -> list (a) -> b =
    Λ a ->
  Λ b ->
  lambda (f( b * a ) -> b)b -> list (a) -> b return lambda (ib)list (a) -> b return lambda (xslist (a))b return
  LIST_FOLD_LEFT(f , i , xs)[@inline][@hidden] in
  let fold_right#76funtype a : * . funtype b : * . ( a * b ) -> b -> list (a) -> b -> b =
    Λ a ->
  Λ b ->
  lambda (f( a * b ) -> b)list (a) -> b -> b return lambda (xslist (a))b -> b return lambda (ib)b return
  LIST_FOLD_RIGHT(f , xs , i)[@inline][@hidden] in
  let cons#77funtype a : * . a -> list (a) -> list (a) = Λ a ->
  lambda (xa)list (a) -> list (a) return lambda (xslist (a))list (a) return
  CONS(x , xs)[@inline][@hidden] in
  let length#78string -> nat =
    lambda (bstring)nat return ([%Michelson {| { SIZE } |}])@(b)[@inline][@hidden] in
  let concat#79string -> string -> string =
    lambda (b1string)string -> string return lambda (b2string)string return
  ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b1 , b2 ))[@inline][@hidden] in
  let sub#80nat -> nat -> string -> string =
    lambda (snat)nat -> string -> string return lambda (lnat)string -> string return lambda (bstring)string return
  ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(
  ( s ,
    l ,
    b ))[@inline][@hidden] in
  let unopt#81funtype a : * . option (a) -> a = Λ a ->
  lambda (voption (a))a return UNOPT(v)[@inline][@hidden] in
  let unopt_with_error#82funtype a : * . option (a) -> string -> a = Λ a ->
  lambda (voption (a))string -> a return lambda (sstring)a return UNOPT_WITH_ERROR
                                                                  (v ,
                                                                   s)[@inline][@hidden] in
  let pack#83funtype a : * . a -> bytes = Λ a ->
  lambda (va)bytes return ([%Michelson {| { PACK } |}])@(v)[@inline][@hidden] in
  let unpack#84funtype a : * . bytes -> option (a) = Λ a ->
  lambda (bbytes)option (a) return BYTES_UNPACK(b)[@inline][@hidden] in
  let length#85bytes -> nat =
    lambda (bbytes)nat return ([%Michelson {| { SIZE } |}])@(b)[@inline][@hidden] in
  let concat#86bytes -> bytes -> bytes =
    lambda (b1bytes)bytes -> bytes return lambda (b2bytes)bytes return
  ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b1 , b2 ))[@inline][@hidden] in
  let sub#87nat -> nat -> bytes -> bytes =
    lambda (snat)nat -> bytes -> bytes return lambda (lnat)bytes -> bytes return lambda (bbytes)bytes return
  ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(
  ( s ,
    l ,
    b ))[@inline][@hidden] in
  let blake2b#88bytes -> bytes =
    lambda (bbytes)bytes return ([%Michelson {| { BLAKE2B } |}])@(b)[@inline][@hidden] in
  let sha256#89bytes -> bytes =
    lambda (bbytes)bytes return ([%Michelson {| { SHA256 } |}])@(b)[@inline][@hidden] in
  let sha512#90bytes -> bytes =
    lambda (bbytes)bytes return ([%Michelson {| { SHA512 } |}])@(b)[@inline][@hidden] in
  let sha3#91bytes -> bytes =
    lambda (bbytes)bytes return ([%Michelson {| { SHA3 } |}])@(b)[@inline][@hidden] in
  let keccak#92bytes -> bytes =
    lambda (bbytes)bytes return ([%Michelson {| { KECCAK } |}])@(b)[@inline][@hidden] in
  let hash_key#93key -> key_hash =
    lambda (kkey)key_hash return ([%Michelson {| { HASH_KEY } |}])@(k)[@inline][@hidden] in
  let check#94key -> signature -> bytes -> bool =
    lambda (kkey)signature -> bytes -> bool return lambda (ssignature)bytes -> bool return lambda (bbytes)bool return
  ([%Michelson {| { UNPAIR ; UNPAIR ; CHECK_SIGNATURE } |}])@(( k , s , b ))[@inline][@hidden] in
  let assertbool -> unit =
    lambda (bbool)unit return ([%Michelson {| { IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } } |}])@(b)[@inline][@private][@hidden] in
  let assert_somefuntype a : * . option (a) -> unit = Λ a ->
  lambda (voption (a))unit return ([%Michelson {| { IF_NONE { PUSH string "failed assert some" ; FAILWITH } { DROP ; UNIT } } |}])@(v)[@inline][@private][@hidden] in
  let assert_nonefuntype a : * . option (a) -> unit = Λ a ->
  lambda (voption (a))unit return ([%Michelson {| { IF_NONE { UNIT } { PUSH string "failed assert none" ; FAILWITH } } |}])@(v)[@inline][@private][@hidden] in
  let absint -> nat =
    lambda (iint)nat return ([%Michelson {| { ABS } |}])@(i)[@inline][@private][@hidden] in
  let is_natint -> option (nat) =
    lambda (iint)option (nat) return ([%Michelson {| { ISNAT } |}])@(i)[@inline][@private][@hidden] in
  let truebool = TRUE()[@inline][@private][@hidden] in
  let falsebool = FALSE()[@inline][@private][@hidden] in
  let unitunit = UNIT()[@inline][@private][@hidden] in
  let failwithfuntype a : * . funtype b : * . a -> b = Λ a ->
  Λ b ->  [%Michelson {|{ FAILWITH }|}][@inline][@private][@hidden] in
  let intfuntype a : * . a -> external_int (a) = Λ a ->
  lambda (va)external_int (a) return ([%Michelson {| { INT } |}])@(v)[@inline][@private][@hidden] in
  let assert_with_errorbool -> string -> unit =
    lambda (bbool)string -> unit return lambda (sstring)unit return ([%Michelson {| { UNPAIR ; IF { DROP ; UNIT } { FAILWITH } } |}])@(
                                                                    ( b ,
                                                                      s ))[@inline][@private][@hidden] in
  let assert_some_with_errorfuntype a : * . option (a) -> string -> unit =
    Λ a ->
  lambda (voption (a))string -> unit return lambda (sstring)unit return
  ([%Michelson {| { UNPAIR ; IF_NONE { FAILWITH } { DROP 2 ; UNIT } } |}])@(
  ( v ,
    s ))[@inline][@private][@hidden] in
  let assert_none_with_errorfuntype a : * . option (a) -> string -> unit =
    Λ a ->
  lambda (voption (a))string -> unit return lambda (sstring)unit return
  ([%Michelson {| { UNPAIR ; IF_NONE { DROP ; UNIT } { DROP ; FAILWITH } } |}])@(
  ( v ,
    s ))[@inline][@private][@hidden] in
  let edivfuntype a : * . funtype b : * . a -> b -> external_ediv (a , b) =
    Λ a ->
  Λ b ->
  lambda (la)b -> external_ediv (a , b) return lambda (rb)external_ediv (a ,
  b) return ([%Michelson {| { UNPAIR ; EDIV } |}])@(( l , r ))[@inline][@private][@hidden] in
  let stub#95funtype a : * . funtype b : * . a -> b = Λ a ->
  Λ b ->  lambda (xa)b return ([%Michelson {|{ FAILWITH }|}])@(x)[@inline][@private][@hidden] in
  let run#96funtype a : * . funtype b : * . a -> b -> a -> unit = Λ a ->
  Λ b ->
  lambda (_fa -> b)a -> unit return lambda (_va)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let eval#97funtype a : * . a -> unit = Λ a ->
  lambda (_xa)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let compile_value#98funtype a : * . a -> unit = Λ a ->
  lambda (_xa)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let get_total_voting_power#99unit -> nat =
    lambda (_uunit)nat return (stub#95@{unit}@{nat})@(unit)[@inline][@hidden] in
  let failwith#100funtype a : * . funtype b : * . a -> b = Λ a ->
  Λ b ->  lambda (_va)b return (stub#95@{unit}@{b})@(unit)[@inline][@hidden] in
  let to_contract#101funtype p : * . funtype s : * . unit -> contract (p) =
    Λ p ->
  Λ s ->
  lambda (_tunit)contract (p) return (stub#95@{unit}@{contract (p)})@(unit)[@inline][@hidden] in
  let set_source#102address -> unit =
    lambda (_aaddress)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let get_storage_of_address#103address -> unit =
    lambda (_aaddress)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let get_balance#104address -> tez =
    lambda (_aaddress)tez return (stub#95@{unit}@{tez})@(unit)[@inline][@hidden] in
  let print#105string -> unit =
    lambda (_vstring)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let eprint#106string -> unit =
    lambda (_vstring)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let get_voting_power#107key_hash -> nat =
    lambda (_khkey_hash)nat return (stub#95@{unit}@{nat})@(unit)[@inline][@hidden] in
  let nth_bootstrap_contract#108nat -> address =
    lambda (_inat)address return (stub#95@{unit}@{address})@(unit)[@inline][@hidden] in
  let nth_bootstrap_account#109int -> address =
    lambda (_iint)address return (stub#95@{unit}@{address})@(unit)[@inline][@hidden] in
  let get_bootstrap_account#110nat -> ( address * key * string ) =
    lambda (_nnat)( address * key * string ) return (stub#95@{unit}@{( address * key * string )})@(unit)[@inline][@hidden] in
  let nth_bootstrap_typed_address#111funtype a : * . funtype b : * . nat -> unit =
    Λ a ->
  Λ b ->  lambda (_nnat)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let last_originations#112unit -> map (address , list (address)) =
    lambda (_uunit)map (address ,
  list (address)) return (stub#95@{unit}@{map (address ,
                         list (address))})@(unit)[@inline][@hidden] in
  let random#113funtype a : * . unit -> a = Λ a ->
  lambda (_uunit)a return (stub#95@{unit}@{a})@(unit)[@inline][@hidden] in
  let new_account#114unit -> ( string * key ) =
    lambda (_uunit)( string * key ) return (stub#95@{unit}@{( string * key )})@(unit)[@inline][@hidden] in
  let decompile#115funtype a : * . unit -> a = Λ a ->
  lambda (_munit)a return (stub#95@{unit}@{a})@(unit)[@inline][@hidden] in
  let bake_until_n_cycle_end#116nat -> unit =
    lambda (_nnat)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let cast_address#117funtype a : * . funtype b : * . address -> unit =
    Λ a ->
  Λ b ->  lambda (_aaddress)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let register_delegate#118key_hash -> unit =
    lambda (_khkey_hash)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let register_constant#119unit -> string =
    lambda (_munit)string return (stub#95@{unit}@{string})@(unit)[@inline][@hidden] in
  let to_typed_address#120funtype a : * . funtype b : * . contract (a) -> unit =
    Λ a ->
  Λ b ->  lambda (_ccontract (a))unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let constant_to_michelson_program#121string -> unit =
    lambda (_sstring)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let restore_context#122unit -> unit =
    lambda (_uunit)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let save_context#123unit -> unit =
    lambda (_uunit)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let drop_context#124unit -> unit =
    lambda (_uunit)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let to_string#125funtype a : * . a -> string = Λ a ->
  lambda (_va)string return (stub#95@{unit}@{string})@(unit)[@inline][@hidden] in
  let get_storage#126funtype p : * . funtype s : * . unit -> s = Λ p ->
  Λ s ->  lambda (_tunit)s return (stub#95@{unit}@{s})@(unit)[@inline][@hidden] in
  let set_baker_policy#127unit -> unit =
    lambda (_bpunit)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let set_baker#128address -> unit =
    lambda (_aaddress)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let size#129unit -> int =
    lambda (_cunit)int return (stub#95@{unit}@{int})@(unit)[@inline][@hidden] in
  let compile_contract#130funtype p : * . funtype s : * . ( p * s ) ->
  ( list (operation) * s ) -> unit = Λ p ->
  Λ s ->
  lambda (_f( p * s ) -> ( list (operation) * s ))unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let read_contract_from_file#131string -> unit =
    lambda (_fnstring)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let chr#132nat -> option (string) =
    lambda (_nnat)option (string) return (stub#95@{unit}@{option (string)})@(unit)[@inline][@hidden] in
  let nl#133string = "NEWLINE"[@inline][@hidden] in
  let println#134string -> unit =
    lambda (_vstring)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let transfer#135address -> unit -> tez -> unit =
    lambda (_aaddress)unit -> tez -> unit return lambda (_sunit)tez -> unit return lambda (_ttez)unit return
  (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let transfer_exn#136address -> unit -> tez -> nat =
    lambda (_aaddress)unit -> tez -> nat return lambda (_sunit)tez -> nat return lambda (_ttez)nat return
  (stub#95@{unit}@{nat})@(unit)[@inline][@hidden] in
  let log#137funtype a : * . a -> unit = Λ a ->
  lambda (_va)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let reset_state#138nat -> list (tez) -> unit =
    lambda (_nnat)list (tez) -> unit return lambda (_llist (tez))unit return
  (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let reset_state_at#139timestamp -> nat -> list (tez) -> unit =
    lambda (_ttimestamp)nat -> list (tez) -> unit return lambda (_nnat)list (tez) -> unit return lambda (_llist (tez))unit return
  (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let bootstrap_contract#140funtype p : * . funtype s : * . ( p * s ) ->
  ( list (operation) * s ) -> s -> tez -> unit = Λ p ->
  Λ s ->
  lambda (_f( p * s ) -> ( list (operation) * s ))s -> tez -> unit return lambda (_ss)tez -> unit return lambda (_ttez)unit return
  (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let mutate_value#141funtype a : * . nat -> a -> option (( a * unit )) =
    Λ a ->
  lambda (_nnat)a -> option (( a * unit )) return lambda (_va)option (( a * unit )) return
  (stub#95@{unit}@{option (( a * unit ))})@(unit)[@inline][@hidden] in
  let save_mutation#142string -> unit -> option (string) =
    lambda (_sstring)unit -> option (string) return lambda (_munit)option (string) return
  (stub#95@{unit}@{option (string)})@(unit)[@inline][@hidden] in
  let mutation_test#143funtype a : * . funtype b : * . a -> a -> b -> option (
  ( b * unit )) = Λ a ->
  Λ b ->
  lambda (_va)a -> b -> option (( b * unit )) return lambda (_fa -> b)option (
  ( b * unit )) return (stub#95@{unit}@{option (( b * unit ))})@(unit)[@inline][@hidden] in
  let mutation_test_all#144funtype a : * . funtype b : * . a -> a -> b -> list (
  ( b * unit )) = Λ a ->
  Λ b ->
  lambda (_va)a -> b -> list (( b * unit )) return lambda (_fa -> b)list (
  ( b * unit )) return (stub#95@{unit}@{list (( b * unit ))})@(unit)[@inline][@hidden] in
  let sign#145string -> bytes -> signature =
    lambda (_skstring)bytes -> signature return lambda (_dbytes)signature return
  (stub#95@{unit}@{signature})@(unit)[@inline][@hidden] in
  let add_account#146string -> key -> unit =
    lambda (_sstring)key -> unit return lambda (_kkey)unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let baker_account#147( string * key ) -> option (tez) -> unit =
    lambda (_p( string * key ))option (tez) -> unit return lambda (_ooption (tez))unit return
  (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let set_big_map#148funtype a : * . funtype b : * . int -> big_map (a ,
  b) -> unit = Λ a ->
  Λ b ->
  lambda (_iint)big_map (a , b) -> unit return lambda (_mbig_map (a ,
  b))unit return (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let create_chest#149bytes -> nat -> ( chest * chest_key ) =
    lambda (_bbytes)nat -> ( chest * chest_key ) return lambda (_nnat)( chest * chest_key ) return
  (stub#95@{unit}@{( chest * chest_key )})@(unit)[@inline][@hidden] in
  let create_chest_key#150chest -> nat -> chest_key =
    lambda (_cchest)nat -> chest_key return lambda (_nnat)chest_key return
  (stub#95@{unit}@{chest_key})@(unit)[@inline][@hidden] in
  let transfer_to_contract#151funtype p : * . contract (p) -> p -> tez -> unit =
    Λ p ->
  lambda (_ccontract (p))p -> tez -> unit return lambda (_sp)tez -> unit return lambda (_ttez)unit return
  (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let transfer_to_contract_exn#152funtype p : * . contract (p) -> p -> tez -> nat =
    Λ p ->
  lambda (_ccontract (p))p -> tez -> nat return lambda (_sp)tez -> nat return lambda (_ttez)nat return
  (stub#95@{unit}@{nat})@(unit)[@inline][@hidden] in
  let michelson_equal#153unit -> unit -> bool =
    lambda (_m1unit)unit -> bool return lambda (_m2unit)bool return (stub#95@{unit}@{bool})@(unit)[@inline][@hidden] in
  let to_entrypoint#154funtype a : * . funtype b : * . funtype c : * . string -> unit -> contract (c) =
    Λ a ->
  Λ b ->
  Λ c ->
  lambda (_sstring)unit -> contract (c) return lambda (_tunit)contract (c) return
  (stub#95@{unit}@{contract (c)})@(unit)[@inline][@hidden] in
  let originate_contract#155unit -> unit -> tez -> address =
    lambda (_cunit)unit -> tez -> address return lambda (_sunit)tez -> address return lambda (_ttez)address return
  (stub#95@{unit}@{address})@(unit)[@inline][@hidden] in
  let originate#156funtype p : * . funtype s : * . ( p * s ) -> ( list (operation) * s ) -> s -> tez ->
  ( unit * unit * int ) = Λ p ->
  Λ s ->
  lambda (_f( p * s ) -> ( list (operation) * s ))s -> tez -> ( unit * unit * int ) return lambda (_ss)tez ->
  ( unit * unit * int ) return lambda (_ttez)( unit * unit * int ) return
  (stub#95@{unit}@{( unit * unit * int )})@(unit)[@inline][@hidden] in
  let compile_contract_from_file#157string -> string -> list (string) -> unit =
    lambda (_fnstring)string -> list (string) -> unit return lambda (_estring)list (string) -> unit return lambda (_vlist (string))unit return
  (stub#95@{unit}@{unit})@(unit)[@inline][@hidden] in
  let originate_from_file#158string -> string -> list (string) -> unit -> tez ->
  ( address * unit * int ) =
    lambda (_fnstring)string -> list (string) -> unit -> tez -> ( address * unit * int ) return lambda (_estring)list (string) -> unit -> tez ->
  ( address * unit * int ) return lambda (_vlist (string))unit -> tez ->
  ( address * unit * int ) return lambda (_sunit)tez -> ( address * unit * int ) return lambda (_ttez)
  ( address * unit * int ) return (stub#95@{unit}@{( address * unit * int )})@(unit)[@inline][@hidden] in
  let a#159int = 42 in let x#160int = a#159 in let xint = x#160 in unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias12.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias13.mligo" ] ;
  [%expect {xxx|
  let get_balance#10unit -> tez =
    lambda (_uunit)tez return ([%Michelson {| { DROP ; BALANCE } |}])@(unit)[@inline][@hidden] in
  let get_amount#11unit -> tez =
    lambda (_uunit)tez return ([%Michelson {| { DROP ; AMOUNT } |}])@(unit)[@inline][@hidden] in
  let get_now#12unit -> timestamp =
    lambda (_uunit)timestamp return ([%Michelson {| { DROP ; NOW } |}])@(unit)[@inline][@hidden] in
  let get_sender#13unit -> address =
    lambda (_uunit)address return ([%Michelson {| { DROP ; SENDER } |}])@(unit)[@inline][@hidden] in
  let get_source#14unit -> address =
    lambda (_uunit)address return ([%Michelson {| { DROP ; SOURCE } |}])@(unit)[@inline][@hidden] in
  let get_level#15unit -> nat =
    lambda (_uunit)nat return ([%Michelson {| { DROP ; LEVEL } |}])@(unit)[@inline][@hidden] in
  let get_self_address#16unit -> address =
    lambda (_uunit)address return SELF_ADDRESS()[@inline][@hidden] in
  let get_chain_id#17unit -> chain_id =
    lambda (_uunit)chain_id return ([%Michelson {| { DROP ; CHAIN_ID } |}])@(unit)[@inline][@hidden] in
  let get_total_voting_power#18unit -> nat =
    lambda (_uunit)nat return ([%Michelson {| { DROP ; TOTAL_VOTING_POWER } |}])@(unit)[@inline][@hidden] in
  let get_min_block_time#19unit -> nat =
    lambda (_uunit)nat return ([%Michelson {| { DROP; MIN_BLOCK_TIME } |}])@(unit)[@inline][@hidden] in
  let voting_power#20key_hash -> nat =
    lambda (khkey_hash)nat return ([%Michelson {| { VOTING_POWER } |}])@(kh)[@inline][@hidden] in
  let address#21funtype a : * . contract (a) -> address = Λ a ->
  lambda (ccontract (a))address return ADDRESS(c)[@inline][@hidden] in
  let implicit_account#22key_hash -> contract (unit) =
    lambda (khkey_hash)contract (unit) return IMPLICIT_ACCOUNT(kh)[@inline][@hidden] in
  let join_tickets#23funtype a : * . ( ticket (a) * ticket (a) ) -> option (ticket (a)) =
    Λ a ->
  lambda (t( ticket (a) * ticket (a) ))option (ticket (a)) return ([%Michelson {| { JOIN_TICKETS } |}])@(t)[@inline][@hidden] in
  let read_ticket#24funtype a : * . ticket (a) -> ( ( address * ( a * nat ) ) * ticket (a) ) =
    Λ a ->
  lambda (tticket (a))( ( address * ( a * nat ) ) * ticket (a) ) return
  ([%Michelson {| { READ_TICKET ; PAIR } |}])@(t)[@inline][@hidden] in
  let never#25funtype a : * . never -> a = Λ a ->
  lambda (nnever)a return ([%Michelson {| { NEVER } |}])@(n)[@inline][@hidden] in
  let pairing_check#26list (( bls12_381_g1 * bls12_381_g2 )) -> bool =
    lambda (llist (( bls12_381_g1 * bls12_381_g2 )))bool return ([%Michelson {| { PAIRING_CHECK } |}])@(l)[@inline][@hidden] in
  let constant#27funtype a : * . string -> a = Λ a ->
  lambda (sstring)a return GLOBAL_CONSTANT(s)[@inline][@hidden] in
  let set_delegate#28option (key_hash) -> operation =
    lambda (ooption (key_hash))operation return SET_DELEGATE(o)[@inline][@hidden] in
  let get_contract#29funtype a : * . address -> contract (a) = Λ a ->
  lambda (aaddress)contract (a) return CONTRACT(a)[@inline][@hidden] in
  let get_contract_opt#30funtype a : * . address -> option (contract (a)) =
    Λ a ->
  lambda (aaddress)option (contract (a)) return CONTRACT_OPT(a)[@inline][@hidden] in
  let get_contract_with_error#31funtype a : * . address -> string -> contract (a) =
    Λ a ->
  lambda (aaddress)string -> contract (a) return lambda (sstring)contract (a) return
  CONTRACT_WITH_ERROR(a , s)[@inline][@hidden] in
  let create_ticket#32funtype a : * . a -> nat -> ticket (a) = Λ a ->
  lambda (va)nat -> ticket (a) return lambda (nnat)ticket (a) return ([%Michelson {| { UNPAIR ; TICKET } |}])@(
                                                                     ( v ,
                                                                      n ))[@inline][@hidden] in
  let transaction#33funtype a : * . a -> tez -> contract (a) -> operation =
    Λ a ->
  lambda (aa)tez -> contract (a) -> operation return lambda (mutez)contract (a) -> operation return lambda (ccontract (a))operation return
  CALL(a , mu , c)[@inline][@hidden] in
  let open_chest#34chest_key -> chest -> nat -> sum[Fail_decrypt -> unit , Fail_timelock -> unit , Ok_opening -> bytes] =
    lambda (ckchest_key)chest -> nat -> sum[Fail_decrypt -> unit , Fail_timelock -> unit , Ok_opening -> bytes] return lambda (cchest)nat ->
  sum[Fail_decrypt -> unit , Fail_timelock -> unit , Ok_opening -> bytes] return lambda (nnat)
  sum[Fail_decrypt -> unit , Fail_timelock -> unit , Ok_opening -> bytes] return
  OPEN_CHEST(ck , c , n)[@inline][@hidden] in
  let call_view#35funtype a : * . funtype b : * . string -> a -> address -> option (b) =
    Λ a ->
  Λ b ->
  lambda (sstring)a -> address -> option (b) return lambda (xa)address -> option (b) return lambda (aaddress)option (b) return
  VIEW(s , x , a)[@inline][@hidden] in
  let split_ticket#36funtype a : * . ticket (a) -> ( nat * nat ) -> option (
  ( ticket (a) * ticket (a) )) = Λ a ->
  lambda (tticket (a))( nat * nat ) -> option (( ticket (a) * ticket (a) )) return lambda (p
  ( nat * nat ))option (( ticket (a) * ticket (a) )) return ([%Michelson {| { UNPAIR ; SPLIT_TICKET } |}])@(
                                                            ( t ,
                                                              p ))[@inline][@hidden] in
  let xor#37nat -> nat -> nat =
    lambda (lnat)nat -> nat return lambda (rnat)nat return XOR(l , r)[@inline][@hidden] in
  let shift_left#38nat -> nat -> nat =
    lambda (lnat)nat -> nat return lambda (rnat)nat return LSL(l , r)[@inline][@hidden] in
  let shift_right#39nat -> nat -> nat =
    lambda (lnat)nat -> nat return lambda (rnat)nat return LSR(l , r)[@inline][@hidden] in
  let empty#40funtype k : * . funtype v : * . big_map (k , v) = Λ k ->
  Λ v ->  BIG_MAP_EMPTY()[@inline][@hidden] in
  let mem#41funtype k : * . funtype v : * . k -> big_map (k , v) -> bool =
    Λ k ->
  Λ v ->
  lambda (kk)big_map (k , v) -> bool return lambda (mbig_map (k ,
  v))bool return ([%Michelson {| { UNPAIR ; MEM } |}])@(( k , m ))[@inline][@hidden] in
  let add#42funtype k : * . funtype v : * . k -> v -> big_map (k ,
  v) -> big_map (k , v) = Λ k ->
  Λ v ->
  lambda (kk)v -> big_map (k , v) -> big_map (k ,
  v) return lambda (vv)big_map (k , v) -> big_map (k ,
  v) return lambda (mbig_map (k , v))big_map (k , v) return MAP_ADD(k , v , m)[@inline][@hidden] in
  let remove#43funtype k : * . funtype v : * . k -> big_map (k ,
  v) -> big_map (k , v) = Λ k ->
  Λ v ->
  lambda (kk)big_map (k , v) -> big_map (k , v) return lambda (mbig_map (k ,
  v))big_map (k , v) return MAP_REMOVE(k , m)[@inline][@hidden] in
  let update#44funtype k : * . funtype v : * . k -> option (v) -> big_map (k ,
  v) -> big_map (k , v) = Λ k ->
  Λ v ->
  lambda (kk)option (v) -> big_map (k , v) -> big_map (k ,
  v) return lambda (voption (v))big_map (k , v) -> big_map (k ,
  v) return lambda (mbig_map (k , v))big_map (k ,
  v) return MAP_UPDATE(k , v , m)[@inline][@hidden] in
  let get_and_update#45funtype k : * . funtype v : * . k -> option (v) -> big_map (k ,
  v) -> ( option (v) * big_map (k , v) ) = Λ k ->
  Λ v ->
  lambda (kk)option (v) -> big_map (k ,
  v) -> ( option (v) * big_map (k , v) ) return lambda (voption (v))big_map (k ,
  v) -> ( option (v) * big_map (k , v) ) return lambda (mbig_map (k ,
  v))( option (v) * big_map (k , v) ) return BIG_MAP_GET_AND_UPDATE(k , v , m)[@inline][@hidden] in
  let find_opt#46funtype k : * . funtype v : * . k -> big_map (k ,
  v) -> option (v) = Λ k ->
  Λ v ->
  lambda (kk)big_map (k , v) -> option (v) return lambda (mbig_map (k ,
  v))option (v) return MAP_FIND_OPT(k , m)[@inline][@hidden] in
  let find#47funtype k : * . funtype v : * . k -> big_map (k , v) -> v =
    Λ k ->
  Λ v ->
  lambda (kk)big_map (k , v) -> v return lambda (mbig_map (k ,
  v))v return MAP_FIND(k , m)[@inline][@hidden] in
  let empty#48funtype k : * . funtype v : * . map (k , v) = Λ k ->
  Λ v ->  MAP_EMPTY()[@inline][@hidden] in
  let size#49funtype k : * . funtype v : * . map (k , v) -> nat = Λ k ->
  Λ v ->  lambda (mmap (k , v))nat return ([%Michelson {| { SIZE } |}])@(m)[@inline][@hidden] in
  let mem#50funtype k : * . funtype v : * . k -> map (k , v) -> bool = Λ k ->
  Λ v ->
  lambda (kk)map (k , v) -> bool return lambda (mmap (k ,
  v))bool return ([%Michelson {| { UNPAIR ; MEM } |}])@(( k , m ))[@inline][@hidden] in
  let add#51funtype k : * . funtype v : * . k -> v -> map (k , v) -> map (k ,
  v) = Λ k ->
  Λ v ->
  lambda (kk)v -> map (k , v) -> map (k , v) return lambda (vv)map (k ,
  v) -> map (k , v) return lambda (mmap (k , v))map (k ,
  v) return MAP_ADD(k , v , m)[@inline][@hidden] in
  let remove#52funtype k : * . funtype v : * . k -> map (k , v) -> map (k ,
  v) = Λ k ->
  Λ v ->
  lambda (kk)map (k , v) -> map (k , v) return lambda (mmap (k , v))map (k ,
  v) return MAP_REMOVE(k , m)[@inline][@hidden] in
  let update#53funtype k : * . funtype v : * . k -> option (v) -> map (k ,
  v) -> map (k , v) = Λ k ->
  Λ v ->
  lambda (kk)option (v) -> map (k , v) -> map (k ,
  v) return lambda (voption (v))map (k , v) -> map (k ,
  v) return lambda (mmap (k , v))map (k , v) return MAP_UPDATE(k , v , m)[@inline][@hidden] in
  let get_and_update#54funtype k : * . funtype v : * . k -> option (v) -> map (k ,
  v) -> ( option (v) * map (k , v) ) = Λ k ->
  Λ v ->
  lambda (kk)option (v) -> map (k ,
  v) -> ( option (v) * map (k , v) ) return lambda (voption (v))map (k ,
  v) -> ( option (v) * map (k , v) ) return lambda (mmap (k ,
  v))( option (v) * map (k , v) ) return MAP_GET_AND_UPDATE(k , v , m)[@inline][@hidden] in
  let find#55funtype k : * . funtype v : * . k -> map (k , v) -> v = Λ k ->
  Λ v ->
  lambda (kk)map (k , v) -> v return lambda (mmap (k ,
  v))v return MAP_FIND(k , m)[@inline][@hidden] in
  let find_opt#56funtype k : * . funtype v : * . k -> map (k ,
  v) -> option (v) = Λ k ->
  Λ v ->
  lambda (kk)map (k , v) -> option (v) return lambda (mmap (k ,
  v))option (v) return MAP_FIND_OPT(k , m)[@inline][@hidden] in
  let iter#57funtype k : * . funtype v : * . ( k * v ) -> unit -> map (k ,
  v) -> unit = Λ k ->
  Λ v ->
  lambda (f( k * v ) -> unit)map (k , v) -> unit return lambda (mmap (k ,
  v))unit return MAP_ITER(f , m)[@inline][@hidden] in
  let map#58funtype k : * . funtype v : * . funtype w : * . ( k * v ) -> w -> map (k ,
  v) -> map (k , w) = Λ k ->
  Λ v ->
  Λ w ->
  lambda (f( k * v ) -> w)map (k , v) -> map (k , w) return lambda (mmap (k ,
  v))map (k , w) return MAP_MAP(f , m)[@inline][@hidden] in
  let fold#59funtype k : * . funtype v : * . funtype c : * . ( c * ( k * v ) ) -> c -> map (k ,
  v) -> c -> c = Λ k ->
  Λ v ->
  Λ c ->
  lambda (f( c * ( k * v ) ) -> c)map (k ,
  v) -> c -> c return lambda (mmap (k ,
  v))c -> c return lambda (ic)c return MAP_FOLD(f , m , i)[@inline][@hidden] in
  let empty#60funtype a : * . set (a) = Λ a ->
  SET_EMPTY()[@inline][@hidden] in
  let size#61funtype a : * . set (a) -> nat = Λ a ->
  lambda (sset (a))nat return ([%Michelson {| { SIZE } |}])@(s)[@inline][@hidden] in
  let cardinal#62funtype a : * . set (a) -> nat = Λ a ->
  lambda (sset (a))nat return ([%Michelson {| { SIZE } |}])@(s)[@inline][@hidden] in
  let mem#63funtype a : * . a -> set (a) -> bool = Λ a ->
  lambda (xa)set (a) -> bool return lambda (sset (a))bool return SET_MEM(x , s)[@inline][@hidden] in
  let add#64funtype a : * . a -> set (a) -> set (a) = Λ a ->
  lambda (xa)set (a) -> set (a) return lambda (sset (a))set (a) return
  SET_ADD(x , s)[@inline][@hidden] in
  let remove#65funtype a : * . a -> set (a) -> set (a) = Λ a ->
  lambda (xa)set (a) -> set (a) return lambda (sset (a))set (a) return
  SET_REMOVE(x , s)[@inline][@hidden] in
  let update#66funtype a : * . a -> bool -> set (a) -> set (a) = Λ a ->
  lambda (xa)bool -> set (a) -> set (a) return lambda (bbool)set (a) -> set (a) return lambda (sset (a))set (a) return
  SET_UPDATE(x , b , s)[@inline][@hidden] in
  let iter#67funtype a : * . a -> unit -> set (a) -> unit = Λ a ->
  lambda (fa -> unit)set (a) -> unit return lambda (sset (a))unit return
  SET_ITER(f , s)[@inline][@hidden] in
  let fold#68funtype a : * . funtype b : * . ( b * a ) -> b -> set (a) -> b -> b =
    Λ a ->
  Λ b ->
  lambda (f( b * a ) -> b)set (a) -> b -> b return lambda (sset (a))b -> b return lambda (ib)b return
  SET_FOLD(f , s , i)[@inline][@hidden] in
  let fold_desc#69funtype a : * . funtype b : * . ( a * b ) -> b -> set (a) -> b -> b =
    Λ a ->
  Λ b ->
  lambda (f( a * b ) -> b)set (a) -> b -> b return lambda (sset (a))b -> b return lambda (ib)b return
  SET_FOLD_DESC(f , s , i)[@inline][@hidden] in
  let length#70funtype a : * . list (a) -> nat = Λ a ->
  lambda (xslist (a))nat return ([%Michelson {| { SIZE } |}])@(xs)[@inline][@hidden] in
  let size#71funtype a : * . list (a) -> nat = Λ a ->
  lambda (xslist (a))nat return ([%Michelson {| { SIZE } |}])@(xs)[@inline][@hidden] in
  let head_opt#72funtype a : * . list (a) -> option (a) = Λ a ->
  lambda (xslist (a))option (a) return  match xs with
                                         | Cons ctor_proj#3 ->
                                            match ctor_proj#3 with
                                             | ( x , _#2 ) ->
                                             Some(x)
                                         | Nil unit_proj#5 ->
                                           None(unit)[@inline][@hidden] in
  let tail_opt#73funtype a : * . list (a) -> option (list (a)) = Λ a ->
  lambda (xslist (a))option (list (a)) return  match xs with
                                                | Cons ctor_proj#6 ->
                                                   match ctor_proj#6 with
                                                    | ( _#3 , xs ) ->
                                                    Some(xs)
                                                | Nil unit_proj#8 ->
                                                  None(unit)[@inline][@hidden] in
  let map#74funtype a : * . funtype b : * . a -> b -> list (a) -> list (b) =
    Λ a ->
  Λ b ->
  lambda (fa -> b)list (a) -> list (b) return lambda (xslist (a))list (b) return
  LIST_MAP(f , xs)[@inline][@hidden] in
  let iter#75funtype a : * . a -> unit -> list (a) -> unit = Λ a ->
  lambda (fa -> unit)list (a) -> unit return lambda (xslist (a))unit return
  LIST_ITER(f , xs)[@inline][@hidden] in
  let fold#76funtype a : * . funtype b : * . ( b * a ) -> b -> list (a) -> b -> b =
    Λ a ->
  Λ b ->
  lambda (f( b * a ) -> b)list (a) -> b -> b return lambda (xslist (a))b -> b return lambda (ib)b return
  LIST_FOLD(f , xs , i)[@inline][@hidden] in
  let fold_left#77funtype a : * . funtype b : * . ( b * a ) -> b -> b -> list (a) -> b =
    Λ a ->
  Λ b ->
  lambda (f( b * a ) -> b)b -> list (a) -> b return lambda (ib)list (a) -> b return lambda (xslist (a))b return
  LIST_FOLD_LEFT(f , i , xs)[@inline][@hidden] in
  let fold_right#78funtype a : * . funtype b : * . ( a * b ) -> b -> list (a) -> b -> b =
    Λ a ->
  Λ b ->
  lambda (f( a * b ) -> b)list (a) -> b -> b return lambda (xslist (a))b -> b return lambda (ib)b return
  LIST_FOLD_RIGHT(f , xs , i)[@inline][@hidden] in
  let cons#79funtype a : * . a -> list (a) -> list (a) = Λ a ->
  lambda (xa)list (a) -> list (a) return lambda (xslist (a))list (a) return
  CONS(x , xs)[@inline][@hidden] in
  let length#80string -> nat =
    lambda (bstring)nat return ([%Michelson {| { SIZE } |}])@(b)[@inline][@hidden] in
  let concat#81string -> string -> string =
    lambda (b1string)string -> string return lambda (b2string)string return
  ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b1 , b2 ))[@inline][@hidden] in
  let sub#82nat -> nat -> string -> string =
    lambda (snat)nat -> string -> string return lambda (lnat)string -> string return lambda (bstring)string return
  ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(
  ( s ,
    l ,
    b ))[@inline][@hidden] in
  let unopt#83funtype a : * . option (a) -> a = Λ a ->
  lambda (voption (a))a return UNOPT(v)[@inline][@hidden] in
  let unopt_with_error#84funtype a : * . option (a) -> string -> a = Λ a ->
  lambda (voption (a))string -> a return lambda (sstring)a return UNOPT_WITH_ERROR
                                                                  (v ,
                                                                   s)[@inline][@hidden] in
  let pack#85funtype a : * . a -> bytes = Λ a ->
  lambda (va)bytes return ([%Michelson {| { PACK } |}])@(v)[@inline][@hidden] in
  let unpack#86funtype a : * . bytes -> option (a) = Λ a ->
  lambda (bbytes)option (a) return BYTES_UNPACK(b)[@inline][@hidden] in
  let length#87bytes -> nat =
    lambda (bbytes)nat return ([%Michelson {| { SIZE } |}])@(b)[@inline][@hidden] in
  let concat#88bytes -> bytes -> bytes =
    lambda (b1bytes)bytes -> bytes return lambda (b2bytes)bytes return
  ([%Michelson {| { UNPAIR ; CONCAT } |}])@(( b1 , b2 ))[@inline][@hidden] in
  let sub#89nat -> nat -> bytes -> bytes =
    lambda (snat)nat -> bytes -> bytes return lambda (lnat)bytes -> bytes return lambda (bbytes)bytes return
  ([%Michelson {| { UNPAIR ; UNPAIR ; SLICE ; IF_NONE { PUSH string "SLICE" ; FAILWITH } {} } |}])@(
  ( s ,
    l ,
    b ))[@inline][@hidden] in
  let blake2b#90bytes -> bytes =
    lambda (bbytes)bytes return ([%Michelson {| { BLAKE2B } |}])@(b)[@inline][@hidden] in
  let sha256#91bytes -> bytes =
    lambda (bbytes)bytes return ([%Michelson {| { SHA256 } |}])@(b)[@inline][@hidden] in
  let sha512#92bytes -> bytes =
    lambda (bbytes)bytes return ([%Michelson {| { SHA512 } |}])@(b)[@inline][@hidden] in
  let sha3#93bytes -> bytes =
    lambda (bbytes)bytes return ([%Michelson {| { SHA3 } |}])@(b)[@inline][@hidden] in
  let keccak#94bytes -> bytes =
    lambda (bbytes)bytes return ([%Michelson {| { KECCAK } |}])@(b)[@inline][@hidden] in
  let hash_key#95key -> key_hash =
    lambda (kkey)key_hash return ([%Michelson {| { HASH_KEY } |}])@(k)[@inline][@hidden] in
  let check#96key -> signature -> bytes -> bool =
    lambda (kkey)signature -> bytes -> bool return lambda (ssignature)bytes -> bool return lambda (bbytes)bool return
  ([%Michelson {| { UNPAIR ; UNPAIR ; CHECK_SIGNATURE } |}])@(( k , s , b ))[@inline][@hidden] in
  let assertbool -> unit =
    lambda (bbool)unit return ([%Michelson {| { IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } } |}])@(b)[@inline][@private][@hidden] in
  let assert_somefuntype a : * . option (a) -> unit = Λ a ->
  lambda (voption (a))unit return ([%Michelson {| { IF_NONE { PUSH string "failed assert some" ; FAILWITH } { DROP ; UNIT } } |}])@(v)[@inline][@private][@hidden] in
  let assert_nonefuntype a : * . option (a) -> unit = Λ a ->
  lambda (voption (a))unit return ([%Michelson {| { IF_NONE { UNIT } { PUSH string "failed assert none" ; FAILWITH } } |}])@(v)[@inline][@private][@hidden] in
  let absint -> nat =
    lambda (iint)nat return ([%Michelson {| { ABS } |}])@(i)[@inline][@private][@hidden] in
  let is_natint -> option (nat) =
    lambda (iint)option (nat) return ([%Michelson {| { ISNAT } |}])@(i)[@inline][@private][@hidden] in
  let truebool = TRUE()[@inline][@private][@hidden] in
  let falsebool = FALSE()[@inline][@private][@hidden] in
  let unitunit = UNIT()[@inline][@private][@hidden] in
  let failwithfuntype a : * . funtype b : * . a -> b = Λ a ->
  Λ b ->  [%Michelson {|{ FAILWITH }|}][@inline][@private][@hidden] in
  let intfuntype a : * . a -> external_int (a) = Λ a ->
  lambda (va)external_int (a) return ([%Michelson {| { INT } |}])@(v)[@inline][@private][@hidden] in
  let assert_with_errorbool -> string -> unit =
    lambda (bbool)string -> unit return lambda (sstring)unit return ([%Michelson {| { UNPAIR ; IF { DROP ; UNIT } { FAILWITH } } |}])@(
                                                                    ( b ,
                                                                      s ))[@inline][@private][@hidden] in
  let assert_some_with_errorfuntype a : * . option (a) -> string -> unit =
    Λ a ->
  lambda (voption (a))string -> unit return lambda (sstring)unit return
  ([%Michelson {| { UNPAIR ; IF_NONE { FAILWITH } { DROP 2 ; UNIT } } |}])@(
  ( v ,
    s ))[@inline][@private][@hidden] in
  let assert_none_with_errorfuntype a : * . option (a) -> string -> unit =
    Λ a ->
  lambda (voption (a))string -> unit return lambda (sstring)unit return
  ([%Michelson {| { UNPAIR ; IF_NONE { DROP ; UNIT } { DROP ; FAILWITH } } |}])@(
  ( v ,
    s ))[@inline][@private][@hidden] in
  let edivfuntype a : * . funtype b : * . a -> b -> external_ediv (a , b) =
    Λ a ->
  Λ b ->
  lambda (la)b -> external_ediv (a , b) return lambda (rb)external_ediv (a ,
  b) return ([%Michelson {| { UNPAIR ; EDIV } |}])@(( l , r ))[@inline][@private][@hidden] in
  let stub#97funtype a : * . funtype b : * . a -> b = Λ a ->
  Λ b ->  lambda (xa)b return ([%Michelson {|{ FAILWITH }|}])@(x)[@inline][@private][@hidden] in
  let run#98funtype a : * . funtype b : * . a -> b -> a -> unit = Λ a ->
  Λ b ->
  lambda (_fa -> b)a -> unit return lambda (_va)unit return (stub#97@{unit}@{unit})@(unit)[@inline][@hidden] in
  let eval#99funtype a : * . a -> unit = Λ a ->
  lambda (_xa)unit return (stub#97@{unit}@{unit})@(unit)[@inline][@hidden] in
  let compile_value#100funtype a : * . a -> unit = Λ a ->
  lambda (_xa)unit return (stub#97@{unit}@{unit})@(unit)[@inline][@hidden] in
  let get_total_voting_power#101unit -> nat =
    lambda (_uunit)nat return (stub#97@{unit}@{nat})@(unit)[@inline][@hidden] in
  let failwith#102funtype a : * . funtype b : * . a -> b = Λ a ->
  Λ b ->  lambda (_va)b return (stub#97@{unit}@{b})@(unit)[@inline][@hidden] in
  let to_contract#103funtype p : * . funtype s : * . unit -> contract (p) =
    Λ p ->
  Λ s ->
  lambda (_tunit)contract (p) return (stub#97@{unit}@{contract (p)})@(unit)[@inline][@hidden] in
  let set_source#104address -> unit =
    lambda (_aaddress)unit return (stub#97@{unit}@{unit})@(unit)[@inline][@hidden] in
  let get_storage_of_address#105address -> unit =
    lambda (_aaddress)unit return (stub#97@{unit}@{unit})@(unit)[@inline][@hidden] in
  let get_balance#106address -> tez =
    lambda (_aaddress)tez return (stub#97@{unit}@{tez})@(unit)[@inline][@hidden] in
  let print#107string -> unit =
    lambda (_vstring)unit return (stub#97@{unit}@{unit})@(unit)[@inline][@hidden] in
  let eprint#108string -> unit =
    lambda (_vstring)unit return (stub#97@{unit}@{unit})@(unit)[@inline][@hidden] in
  let get_voting_power#109key_hash -> nat =
    lambda (_khkey_hash)nat return (stub#97@{unit}@{nat})@(unit)[@inline][@hidden] in
  let nth_bootstrap_contract#110nat -> address =
    lambda (_inat)address return (stub#97@{unit}@{address})@(unit)[@inline][@hidden] in
  let nth_bootstrap_account#111int -> address =
    lambda (_iint)address return (stub#97@{unit}@{address})@(unit)[@inline][@hidden] in
  let get_bootstrap_account#112nat -> ( address * key * string ) =
    lambda (_nnat)( address * key * string ) return (stub#97@{unit}@{( address * key * string )})@(unit)[@inline][@hidden] in
  let nth_bootstrap_typed_address#113funtype a : * . funtype b : * . nat -> unit =
    Λ a ->
  Λ b ->  lambda (_nnat)unit return (stub#97@{unit}@{unit})@(unit)[@inline][@hidden] in
  let last_originations#114unit -> map (address , list (address)) =
    lambda (_uunit)map (address ,
  list (address)) return (stub#97@{unit}@{map (address ,
                         list (address))})@(unit)[@inline][@hidden] in
  let random#115funtype a : * . unit -> a = Λ a ->
  lambda (_uunit)a return (stub#97@{unit}@{a})@(unit)[@inline][@hidden] in
  let new_account#116unit -> ( string * key ) =
    lambda (_uunit)( string * key ) return (stub#97@{unit}@{( string * key )})@(unit)[@inline][@hidden] in
  let decompile#117funtype a : * . unit -> a = Λ a ->
  lambda (_munit)a return (stub#97@{unit}@{a})@(unit)[@inline][@hidden] in
  let bake_until_n_cycle_end#118nat -> unit =
    lambda (_nnat)unit return (stub#97@{unit}@{unit})@(unit)[@inline][@hidden] in
  let cast_address#119funtype a : * . funtype b : * . address -> unit =
    Λ a ->
  Λ b ->  lambda (_aaddress)unit return (stub#97@{unit}@{unit})@(unit)[@inline][@hidden] in
  let register_delegate#120key_hash -> unit =
    lambda (_khkey_hash)unit return (stub#97@{unit}@{unit})@(unit)[@inline][@hidden] in
  let register_constant#121unit -> string =
    lambda (_munit)string return (stub#97@{unit}@{string})@(unit)[@inline][@hidden] in
  let to_typed_address#122funtype a : * . funtype b : * . contract (a) -> unit =
    Λ a ->
  Λ b ->  lambda (_ccontract (a))unit return (stub#97@{unit}@{unit})@(unit)[@inline][@hidden] in
  let constant_to_michelson_program#123string -> unit =
    lambda (_sstring)unit return (stub#97@{unit}@{unit})@(unit)[@inline][@hidden] in
  let restore_context#124unit -> unit =
    lambda (_uunit)unit return (stub#97@{unit}@{unit})@(unit)[@inline][@hidden] in
  let save_context#125unit -> unit =
    lambda (_uunit)unit return (stub#97@{unit}@{unit})@(unit)[@inline][@hidden] in
  let drop_context#126unit -> unit =
    lambda (_uunit)unit return (stub#97@{unit}@{unit})@(unit)[@inline][@hidden] in
  let to_string#127funtype a : * . a -> string = Λ a ->
  lambda (_va)string return (stub#97@{unit}@{string})@(unit)[@inline][@hidden] in
  let get_storage#128funtype p : * . funtype s : * . unit -> s = Λ p ->
  Λ s ->  lambda (_tunit)s return (stub#97@{unit}@{s})@(unit)[@inline][@hidden] in
  let set_baker_policy#129unit -> unit =
    lambda (_bpunit)unit return (stub#97@{unit}@{unit})@(unit)[@inline][@hidden] in
  let set_baker#130address -> unit =
    lambda (_aaddress)unit return (stub#97@{unit}@{unit})@(unit)[@inline][@hidden] in
  let size#131unit -> int =
    lambda (_cunit)int return (stub#97@{unit}@{int})@(unit)[@inline][@hidden] in
  let compile_contract#132funtype p : * . funtype s : * . ( p * s ) ->
  ( list (operation) * s ) -> unit = Λ p ->
  Λ s ->
  lambda (_f( p * s ) -> ( list (operation) * s ))unit return (stub#97@{unit}@{unit})@(unit)[@inline][@hidden] in
  let read_contract_from_file#133string -> unit =
    lambda (_fnstring)unit return (stub#97@{unit}@{unit})@(unit)[@inline][@hidden] in
  let chr#134nat -> option (string) =
    lambda (_nnat)option (string) return (stub#97@{unit}@{option (string)})@(unit)[@inline][@hidden] in
  let nl#135string = "NEWLINE"[@inline][@hidden] in
  let println#136string -> unit =
    lambda (_vstring)unit return (stub#97@{unit}@{unit})@(unit)[@inline][@hidden] in
  let transfer#137address -> unit -> tez -> unit =
    lambda (_aaddress)unit -> tez -> unit return lambda (_sunit)tez -> unit return lambda (_ttez)unit return
  (stub#97@{unit}@{unit})@(unit)[@inline][@hidden] in
  let transfer_exn#138address -> unit -> tez -> nat =
    lambda (_aaddress)unit -> tez -> nat return lambda (_sunit)tez -> nat return lambda (_ttez)nat return
  (stub#97@{unit}@{nat})@(unit)[@inline][@hidden] in
  let log#139funtype a : * . a -> unit = Λ a ->
  lambda (_va)unit return (stub#97@{unit}@{unit})@(unit)[@inline][@hidden] in
  let reset_state#140nat -> list (tez) -> unit =
    lambda (_nnat)list (tez) -> unit return lambda (_llist (tez))unit return
  (stub#97@{unit}@{unit})@(unit)[@inline][@hidden] in
  let reset_state_at#141timestamp -> nat -> list (tez) -> unit =
    lambda (_ttimestamp)nat -> list (tez) -> unit return lambda (_nnat)list (tez) -> unit return lambda (_llist (tez))unit return
  (stub#97@{unit}@{unit})@(unit)[@inline][@hidden] in
  let bootstrap_contract#142funtype p : * . funtype s : * . ( p * s ) ->
  ( list (operation) * s ) -> s -> tez -> unit = Λ p ->
  Λ s ->
  lambda (_f( p * s ) -> ( list (operation) * s ))s -> tez -> unit return lambda (_ss)tez -> unit return lambda (_ttez)unit return
  (stub#97@{unit}@{unit})@(unit)[@inline][@hidden] in
  let mutate_value#143funtype a : * . nat -> a -> option (( a * unit )) =
    Λ a ->
  lambda (_nnat)a -> option (( a * unit )) return lambda (_va)option (( a * unit )) return
  (stub#97@{unit}@{option (( a * unit ))})@(unit)[@inline][@hidden] in
  let save_mutation#144string -> unit -> option (string) =
    lambda (_sstring)unit -> option (string) return lambda (_munit)option (string) return
  (stub#97@{unit}@{option (string)})@(unit)[@inline][@hidden] in
  let mutation_test#145funtype a : * . funtype b : * . a -> a -> b -> option (
  ( b * unit )) = Λ a ->
  Λ b ->
  lambda (_va)a -> b -> option (( b * unit )) return lambda (_fa -> b)option (
  ( b * unit )) return (stub#97@{unit}@{option (( b * unit ))})@(unit)[@inline][@hidden] in
  let mutation_test_all#146funtype a : * . funtype b : * . a -> a -> b -> list (
  ( b * unit )) = Λ a ->
  Λ b ->
  lambda (_va)a -> b -> list (( b * unit )) return lambda (_fa -> b)list (
  ( b * unit )) return (stub#97@{unit}@{list (( b * unit ))})@(unit)[@inline][@hidden] in
  let sign#147string -> bytes -> signature =
    lambda (_skstring)bytes -> signature return lambda (_dbytes)signature return
  (stub#97@{unit}@{signature})@(unit)[@inline][@hidden] in
  let add_account#148string -> key -> unit =
    lambda (_sstring)key -> unit return lambda (_kkey)unit return (stub#97@{unit}@{unit})@(unit)[@inline][@hidden] in
  let baker_account#149( string * key ) -> option (tez) -> unit =
    lambda (_p( string * key ))option (tez) -> unit return lambda (_ooption (tez))unit return
  (stub#97@{unit}@{unit})@(unit)[@inline][@hidden] in
  let set_big_map#150funtype a : * . funtype b : * . int -> big_map (a ,
  b) -> unit = Λ a ->
  Λ b ->
  lambda (_iint)big_map (a , b) -> unit return lambda (_mbig_map (a ,
  b))unit return (stub#97@{unit}@{unit})@(unit)[@inline][@hidden] in
  let create_chest#151bytes -> nat -> ( chest * chest_key ) =
    lambda (_bbytes)nat -> ( chest * chest_key ) return lambda (_nnat)( chest * chest_key ) return
  (stub#97@{unit}@{( chest * chest_key )})@(unit)[@inline][@hidden] in
  let create_chest_key#152chest -> nat -> chest_key =
    lambda (_cchest)nat -> chest_key return lambda (_nnat)chest_key return
  (stub#97@{unit}@{chest_key})@(unit)[@inline][@hidden] in
  let transfer_to_contract#153funtype p : * . contract (p) -> p -> tez -> unit =
    Λ p ->
  lambda (_ccontract (p))p -> tez -> unit return lambda (_sp)tez -> unit return lambda (_ttez)unit return
  (stub#97@{unit}@{unit})@(unit)[@inline][@hidden] in
  let transfer_to_contract_exn#154funtype p : * . contract (p) -> p -> tez -> nat =
    Λ p ->
  lambda (_ccontract (p))p -> tez -> nat return lambda (_sp)tez -> nat return lambda (_ttez)nat return
  (stub#97@{unit}@{nat})@(unit)[@inline][@hidden] in
  let michelson_equal#155unit -> unit -> bool =
    lambda (_m1unit)unit -> bool return lambda (_m2unit)bool return (stub#97@{unit}@{bool})@(unit)[@inline][@hidden] in
  let to_entrypoint#156funtype a : * . funtype b : * . funtype c : * . string -> unit -> contract (c) =
    Λ a ->
  Λ b ->
  Λ c ->
  lambda (_sstring)unit -> contract (c) return lambda (_tunit)contract (c) return
  (stub#97@{unit}@{contract (c)})@(unit)[@inline][@hidden] in
  let originate_contract#157unit -> unit -> tez -> address =
    lambda (_cunit)unit -> tez -> address return lambda (_sunit)tez -> address return lambda (_ttez)address return
  (stub#97@{unit}@{address})@(unit)[@inline][@hidden] in
  let originate#158funtype p : * . funtype s : * . ( p * s ) -> ( list (operation) * s ) -> s -> tez ->
  ( unit * unit * int ) = Λ p ->
  Λ s ->
  lambda (_f( p * s ) -> ( list (operation) * s ))s -> tez -> ( unit * unit * int ) return lambda (_ss)tez ->
  ( unit * unit * int ) return lambda (_ttez)( unit * unit * int ) return
  (stub#97@{unit}@{( unit * unit * int )})@(unit)[@inline][@hidden] in
  let compile_contract_from_file#159string -> string -> list (string) -> unit =
    lambda (_fnstring)string -> list (string) -> unit return lambda (_estring)list (string) -> unit return lambda (_vlist (string))unit return
  (stub#97@{unit}@{unit})@(unit)[@inline][@hidden] in
  let originate_from_file#160string -> string -> list (string) -> unit -> tez ->
  ( address * unit * int ) =
    lambda (_fnstring)string -> list (string) -> unit -> tez -> ( address * unit * int ) return lambda (_estring)list (string) -> unit -> tez ->
  ( address * unit * int ) return lambda (_vlist (string))unit -> tez ->
  ( address * unit * int ) return lambda (_sunit)tez -> ( address * unit * int ) return lambda (_ttez)
  ( address * unit * int ) return (stub#97@{unit}@{( address * unit * int )})@(unit)[@inline][@hidden] in
  let current_turn#161nat -> nat = lambda (inat)nat return ADD(i , +1) in
  let other#162nat -> unit =
    lambda (nnat)unit return let current_turnnat = (current_turn#161)@(+1) in
                             (assert)@(EQ(n , current_turn)) in
  let main( unit * unit ) -> ( list (operation) * unit ) =
    lambda (gen#2( unit * unit ))( list (operation) * unit ) return  match
                                                                      gen#2 with
                                                                      |
                                                                      ( _p , _s ) ->
                                                                      ( LIST_EMPTY
                                                                      () ,
                                                                      (other#162)@(+2) ) in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "bug_alias13.mligo" ] ;
  [%expect {|
    { parameter unit ;
      storage unit ;
      code { DROP ;
             PUSH nat 1 ;
             PUSH nat 1 ;
             ADD ;
             PUSH nat 2 ;
             COMPARE ;
             EQ ;
             IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "effects.mligo" ] ;
  [%expect{|
    { parameter int ;
      storage int ;
      code { CDR ; PUSH string "foo" ; FAILWITH } } |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "pascaligo" ; "tm" ; "--init-file" ; contract "bug_module_record.ligo" ] ;
  [%expect{|
    (Pair 1 "b") |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "test" ; "--init-file" ; contract "bug_locally_bound_vars.mligo" ] ;
  [%expect{|
           42 |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "test2" ; "--init-file" ; contract "bug_locally_bound_vars.mligo" ] ;
  [%expect{|
    "hehe" |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "test" ; "--init-file" ; contract "bug_locally_bound_vars2.mligo" ] ;
  [%expect{|
    43 |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "test" ; "--init-file" ; contract "bug_locally_bound_vars3.mligo" ] ;
  [%expect{|
    2 |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "test" ; "--init-file" ; contract "bug_locally_bound_vars4.mligo" ] ;
  [%expect{|
    42 |}]
