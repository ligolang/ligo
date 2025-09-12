let today : timestamp = Tezos.get_now ()
let today : timestamp = Tezos.get_now ()
let one_day : int = 86_400
let in_24_hrs : timestamp = today + one_day
let some_date : timestamp = ("2000-01-01t10:10:10Z" : timestamp)
let one_day_later : timestamp = some_date + one_day
let today : timestamp = Tezos.get_now ()
let one_day : int = 86400
let in_24_hrs : timestamp = today - one_day
let today : timestamp = Tezos.get_now ()
let some_date : timestamp = ("2035-01-01t10:10:10Z" : timestamp)
let secs_until_some_date : int = some_date - today
let not_tomorrow : bool = (Tezos.get_now () = in_24_hrs)