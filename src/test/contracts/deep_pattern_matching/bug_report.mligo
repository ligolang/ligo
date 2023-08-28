type storage =
  | Ongoing
  | Resolved_successful
  | Resolved_unsuccessful

type parameter =
  | Fund
  | Get_refund
  | Resolve of bool

[@entry]
let main (action : parameter) (storage : storage) : operation list * storage =
  match (storage, action) with
    Ongoing, Fund -> failwith "xx"
  | _, Fund -> [], storage
  | Resolved_successful, Get_refund -> failwith "refund no longer possible "
  | Resolved_unsuccessful, Get_refund -> failwith "unfinished"
  | Ongoing, Get_refund -> failwith "unfinished"
  | Ongoing, Resolve _yes_or_no -> failwith "todo implement resolution"
  | Resolved_unsuccessful, Resolve _ -> failwith "already resolved"
  | Resolved_successful, Resolve _ -> failwith "already resolved"
