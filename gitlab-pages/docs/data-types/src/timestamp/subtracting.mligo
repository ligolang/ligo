let today : timestamp = Tezos.get_now ()
let some_date : timestamp = ("2035-01-01t10:10:10Z" : timestamp)
let secs_until_some_date : int = some_date - today