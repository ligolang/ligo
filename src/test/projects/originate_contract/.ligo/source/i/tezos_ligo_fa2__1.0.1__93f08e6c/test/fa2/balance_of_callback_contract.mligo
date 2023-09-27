type storage = nat list

type request = {
   owner    : address;
   token_id : nat;
}

type callback = [@layout:comb] {
   request : request;
   balance : nat;
}

type parameter = callback list

[@entry]
let main (responses:parameter) (_:storage) =
  let balances = List.map (fun (r : callback) -> r.balance) responses in
  ([]: operation list), balances