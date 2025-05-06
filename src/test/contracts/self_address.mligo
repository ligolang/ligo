module Tezos = Tezos.Next

let check (_ : unit) : address = Tezos.get_self_address ()
