type parameter is
    Call of unit -> operation | IsWhitelisted of address * contract (bool)

type storage is record [senders_whitelist : set (address)]

function main (const p : parameter; const s : storage) is {
  const op
  = case p of [
      Call (op) ->
        if Set.mem (Tezos.get_sender(), s.senders_whitelist)
        then op (Unit)
        else (failwith ("Sender is not whitelisted") : operation)
    | IsWhitelisted (addr_and_callback) -> {
          const addr = addr_and_callback.0;
          const callback_contract = addr_and_callback.1;
          const whitelisted = Set.mem (addr, s.senders_whitelist)
        } with Tezos.transaction (whitelisted, 0mutez, callback_contract)
    ]
} with (list [op], s)
