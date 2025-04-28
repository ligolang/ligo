module Tezos = Tezos.Next

[@entry]
let emitEvents (_ : unit) (storage : int) : operation list * int =
  let event1 : operation = Tezos.Operation.emit "%emitEvents" "hi" in
  let event2 : operation = Tezos.Operation.emit "%emitEvents" 6 in
  [event1; event2], storage