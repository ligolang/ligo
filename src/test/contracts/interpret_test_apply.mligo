type storage = int
type return = operation list * storage
type parameter = One | Two

let main_create_contract (action, store : parameter * storage) : return =
  let (op,addr) : operation * address = Tezos.create_contract
    (fun (p, s : nat * string) -> (([] : operation list), s))
    (None: key_hash option)
    10tz
    "hello world"
  in
  let should_not_exist : parameter contract option = Tezos.get_contract_opt addr in
  match should_not_exist with
  | Some s -> ([op], 0)
  | None -> (failwith 1: return)

let test =
  let addr1 = Test.originate main_create_contract 1 in
  let unit_ = Test.set_balance addr1 10tz in
  Test.assert_failure (fun (u:unit) -> Test.external_call addr1 (One:parameter) 1tz)
