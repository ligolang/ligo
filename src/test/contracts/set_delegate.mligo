let main (p: key_hash) : operation list =
    let unused: operation = (Operation.set_delegate (Some p)) in ([]: operation list)

