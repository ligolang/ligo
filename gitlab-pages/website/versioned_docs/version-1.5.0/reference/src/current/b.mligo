let today         = Tezos.get_now ()
let one_day       = 86_400
let in_24_hrs     = today + one_day
let some_date     = ("2000-01-01t10:10:10Z" : timestamp)
let one_day_later = some_date + one_day