#import "parameter.mligo" "Parameter"
#import "storage.mligo" "Storage"
[@entry]
let main (p : Parameter.t) (s : Storage.t) : operation list * Storage.t =
  match p with
    A n -> [], s + n
  | B _ -> [], s
  | C _ -> [], s
  | D _ -> [], s
