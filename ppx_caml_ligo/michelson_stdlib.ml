let michelson_stdlib ~loc =
  [%str
    (* Ligo Constants *)
    (* TODO: better letters for constructors *)
    type operation [@@ligo.internal.predef]
    type nat [@@ligo.internal.predef]
    type tez [@@ligo.internal.predef]
    type address [@@ligo.internal.predef]
    type signature [@@ligo.internal.predef]
    type key [@@ligo.internal.predef]
    type key_hash [@@ligo.internal.predef]
    type timestamp [@@ligo.internal.predef]
    type chain_id [@@ligo.internal.predef]
    type ('k, 'v) map [@@ligo.internal.predef]
    type ('k, 'v) big_map [@@ligo.internal.predef]
    type 'v set [@@ligo.internal.predef]
    type 'a contract [@@ligo.internal.predef]
    type ('l, 'r) michelson_or [@@ligo.internal.predef]
    type ('l, 'r) michelson_pair [@@ligo.internal.predef]
    type baker_hash [@@ligo.internal.predef]
    type pvss_key [@@ligo.internal.predef]
    type 'a sapling_transaction [@@ligo.internal.predef]
    type 'a sapling_state [@@ligo.internal.predef]
    type baker_operation [@@ligo.internal.predef]
    type bls12_381_g1 [@@ligo.internal.predef]
    type bls12_381_g2 [@@ligo.internal.predef]
    type bls12_381_fr [@@ligo.internal.predef]
    type never [@@ligo.internal.predef]
    type 'd ticket [@@ligo.internal.predef]
    type ('a, 'b) dynamic_entrypoint [@@ligo.internal.predef]
    type michelson_program [@@ligo.internal.predef]
    type ('a, 'b) michelson_contract [@@ligo.internal.predef]
    type ('a, 'b) typed_address [@@ligo.internal.predef]
    type mutation [@@ligo.internal.predef]
    type tx_rollup_l2_address [@@ligo.internal.predef]
    type 'a pbt_gen [@@ligo.internal.predef]
    type 'a views [@@ligo.internal.predef]
    type chest [@@ligo.internal.predef]
    type chest_key [@@ligo.internal.predef]

    (* module Int = struct
      external add : int -> int -> int = "%ligo" [@@ligo.internal.constant "ADD"]

      let add x y = add x y

      external sub : int -> int -> int = "SUB" [@@ligo.internal.michelson]

      let f x y = add 1 2
    end *)]
