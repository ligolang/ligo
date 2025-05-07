module Tezos = Tezos.Next

let check (kh : key_hash) : unit contract = Tezos.implicit_account kh
