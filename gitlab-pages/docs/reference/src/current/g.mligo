module Tezos = Tezos.Next

let check (p : unit) = Tezos.get_self_address ()