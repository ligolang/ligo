let today : timestamp = Tezos.get_now ()
let one_day : int = 86_400
let in_24_hrs : timestamp = today + one_day
let some_date : timestamp = ("2000-01-01t10:10:10Z" : timestamp)
let one_day_later : timestamp = some_date + one_day