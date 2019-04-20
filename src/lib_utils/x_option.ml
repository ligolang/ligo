include Tezos_stdlib.Option

let lr (a , b) = match (a , b) with
  | Some x , _ -> Some (`Left x)
  | None , Some x -> Some (`Right x)
  | _ -> None
