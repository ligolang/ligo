[@entry]
let main () (s : int) : operation list * int =
  let _ = Test.log "Not so fast" in
  [], s
