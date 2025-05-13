(*
 * This contract was made only for testing `Tezos.open_chest` in LIGO,
 * and it should not be used.
 * Check
 *   https://gitlab.com/tezos/tezos/-/blob/master/src/proto_alpha/lib_protocol/contracts/timelock_flip.tz
 *)

module C = struct
  type storage = { level: nat; chest: chest; guess: bytes; result: bytes }

  [@entry] let finish (chest_key : chest_key) (s : storage) : operation list * storage =
    let result = match Tezos.open_chest chest_key s.chest 1024n with
      | Some b -> if b = s.guess then (0x00 : bytes) else (0x01 : bytes)
      | None -> (0x10 : bytes)
    in
    let s = { level = s.level; chest = s.chest; guess = s.guess; result } in
    ([], s)

  [@entry] let guess (guess : bytes) (s : storage) : operation list * storage =
    let s =
      if Tezos.get_level () <= s.level + 10n then
        { level = s.level; chest = s.chest; guess; result = (0xB0 : bytes) }
      else
        s
    in
    ([], s)

  [@entry] let initialize (chest : chest) (_ : storage) : operation list * storage =
    let message = (0xA0 : bytes) in
    let s = { level = Tezos.get_level (); chest; guess = message; result = message } in
    ([], s)
end
