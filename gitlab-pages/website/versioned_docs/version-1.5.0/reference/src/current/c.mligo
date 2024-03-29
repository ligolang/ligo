let today     = Tezos.get_now ()
let one_day   = 86_400
let in_24_hrs = today - one_day
let not_tomorrow = (Tezos.get_now () = in_24_hrs)