type storage = {rewardsLeft : tez; beneficiaryAddress : address}

let treasury (p, s : unit * storage) =
  // We do our computations first
  let newStorage = {s with rewardsLeft = 0mutez} in

  // Then we find our beneficiary's `handleRewards` entrypoint:
  let beneficiaryOpt = Tezos.get_entrypoint_opt "%handleTransfer" s.beneficiaryAddress in
  let beneficiary =
    match beneficiaryOpt with
      Some contract -> contract
    | None -> failwith "Beneficiary does not exist" in

  // Then we prepare the internal operation we want to perform
  let operation = Tezos.transaction () s.rewardsLeft beneficiary in

  // ...and return both the operations and the updated storage
  ([operation], newStorage)