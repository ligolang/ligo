type storage = (string, int ticket) big_map
type parameter = int
type result = operation list * storage

[@entry]
let main (i : parameter) (store : storage) : result =
  let my_ticket1 = Option.unopt (Tezos.create_ticket i 10n) in
  let _, x = Big_map.get_and_update "hello" (Some my_ticket1) store
  in [], x