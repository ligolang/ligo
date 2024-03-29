[@entry]
let main (parameter : bytes) (storage : int) : operation list * int =
  if parameter = 0xbc1ecb8e
  then [], storage + 1
  else
    if parameter = 0x36e44653
    then [], storage - 1
    else failwith "Unknown entrypoint"