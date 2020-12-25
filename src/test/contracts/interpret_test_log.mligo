type storage = int
type return = operation list * storage
type parameter = One | Two

let main_log (action, store : parameter * storage) : return =
  ([] : operation list),
    (match action with
      | One -> let unit_ = Test.log action in 1
      | Two -> 2
    )

type some_record = { a : int ; b : nat ; c : string }

let log =
  let v = { a= 1 ; b = 2n ; c = "aaa" } in
  let unit_ = Test.log v in
  let addr1 = Test.originate main_log 0 in
  let unit_ = Test.external_call addr1 (One:parameter) 1tz in
  true