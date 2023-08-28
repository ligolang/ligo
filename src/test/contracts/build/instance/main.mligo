#import "../common/errors.mligo" "Errors"
#import "../common/storage.mligo" "Storage"
[@entry]
let main (_ : unit) (_ : Storage.t) : operation list * Storage.t =
  [], Errors.undefined_token ^ Storage.s
