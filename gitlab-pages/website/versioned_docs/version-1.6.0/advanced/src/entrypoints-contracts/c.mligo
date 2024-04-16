type parameter = unit
type storage = unit
type result = operation list * storage

[@entry]
let no_tokens (action : parameter) (store : storage) : result =
  if Tezos.get_amount () > 0tez then
    failwith "This contract does not accept tokens."
  else ([], store)
let owner = ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx": address)

[@entry]
let owner_only (action : parameter) (store: storage) : result =
  if Tezos.get_sender () <> owner then failwith "Access denied."
  else ([], store)