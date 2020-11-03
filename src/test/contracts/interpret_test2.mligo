type storage = int
type return = operation list * storage
type parameter = One of address | Two

let main_fail (action, store : parameter * storage) : return =
  (failwith "main fail !" : return)

let main1 (action, store : parameter * storage) : return =
  match action with
    | One (a) ->
      let c : parameter contract option = Tezos.get_contract_opt a in
      let ops = match c with
          Some (c) -> [ Tezos.transaction (Two:parameter) 10tez c ]
        | None     -> (failwith ("Contract not found") : operation list)
      in
      (ops, 1)
    | Two -> (([] : operation list), 2)

let main2 (action, store : parameter * storage) : return =
  ([] : operation list),
    (match action with
      | One (a) -> 1
      | Two -> 4
    )

let main_self (action, store : parameter * (parameter contract option)) : operation list * (parameter contract option) =
  ( ([] : operation list), Some (Tezos.self "%default" : parameter contract) )

let main_create_contract (action, store : parameter * address) : operation list * address =
  let (op,addr) : operation * address = Tezos.create_contract
    (fun (p, s : nat * string) -> (([] : operation list), s))
    (None: key_hash option)
    10tz
    "hello world"
  in
  ([op], addr)

let main_log (action, store : parameter * storage) : return =
  ([] : operation list),
    (match action with
      | One (a) -> let unit_ = Test.log action in 1
      | Two -> 2
    )

let diff_address =
  let addr1 = Test.originate main1 0 in
  let addr2 = Test.originate main_fail 0 in
  addr1 <> addr2

let assert_failure =
  let addr1 = Test.originate main_fail 0 in
  Test.assert_failure (fun (toto:unit) -> Test.external_call addr1 (One addr1) 1tz)

let assert_failure_internal =
  let addr1 = Test.originate main1 0 in
  let addr2 = Test.originate main_fail 0 in
  Test.assert_failure (fun (u:unit) -> Test.external_call addr1 (One addr2) 1tz)

let test1 =
  let addr1 = Test.originate main1 0 in
  let addr2 = Test.originate main2 0 in

  let unit_ = Test.set_balance addr1 10tz in
  let unit_ = Test.set_balance addr2 0tz in

  let unit_ = Test.set_now Tezos.now in
  let unit_ = Test.set_source addr1 in

  let unit_ = Test.external_call addr1 (One addr2) 1tz in

  let a : int  = Test.get_storage addr1 in
  let b : int  = Test.get_storage addr2 in
  let bal1 : tez = Test.get_balance addr1 in
  let bal2 : tez = Test.get_balance addr2 in
  (a = 1) && (b = 4) && (bal1 = 1tz) && (bal2 = 10tz)

let self =
  let addr1 = Test.originate main_self (None : parameter contract option) in
  let unit_ = Test.external_call addr1 (One addr1) 1tz in
  let c : parameter contract option = Test.get_storage addr1 in
  match c with
  | Some (addr) -> (Tezos.address addr = addr1)
  | None -> false

let create_contract =
  let fake_addr = ("tz1PpDGHRXFQq3sYDuH8EpLWzPm5PFpe1sLE": address) in
  let addr3 = Test.originate main_create_contract fake_addr in
  let unit_ = Test.set_balance addr3 10tz in
  let unit_ = Test.external_call addr3 (One fake_addr) 1tz in
  let addr_new : address = Test.get_storage addr3 in
  let hello : string = Test.get_storage addr_new in
  (hello = "hello world")