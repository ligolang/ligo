(**
   This file implement the TZIP-12 protocol (a.k.a FA2) for NFT on Tezos
   copyright Wulfman Corporation 2021
*)

#import "errors.mligo" "Errors"

(**
  This should be initialized at origination, conforming to either
  TZIP-12 : https://gitlab.com/tezos/tzip/-/blob/master/proposals/tzip-12/tzip-12.md#token-metadata
  or TZIP-16 : https://gitlab.com/tezos/tzip/-/blob/master/proposals/tzip-12/tzip-12.md#contract-metadata-tzip-016
*)
type data = {token_id:nat;token_info:(string,bytes)map}
type t = (nat, data) big_map
