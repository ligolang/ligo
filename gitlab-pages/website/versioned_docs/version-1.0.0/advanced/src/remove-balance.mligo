type balances = (address, tez) map

let remove_balances_under (b:balances) (threshold:tez) : balances =
  Map.fold
    (fun ((acc, (k, v)) : balances * (address * tez)) -> if v < threshold then Map.remove k acc else acc)
    b b