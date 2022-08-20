#import "../common/errors.mligo" "Errors"
#import "../common/storage.mligo" "Storage"

let main(_,_:unit*Storage.t) : operation list*Storage.t =
    [], Errors.undefined_token ^ Storage.s