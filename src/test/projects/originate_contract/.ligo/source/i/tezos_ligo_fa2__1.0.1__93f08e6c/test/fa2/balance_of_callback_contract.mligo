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

let main ((responses,s):(parameter * storage)) =
  let balances = List.map (fun (r : callback) -> r.balance) responses in
  ([]: operation list), balances