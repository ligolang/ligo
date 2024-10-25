module Errors = struct
  #include "../common/errors.mligo"
end

module Storage = struct
  #include "../common/storage.mligo"
end

[@entry]
let main (_ : unit) (_ : Storage.t) : operation list * Storage.t =
  [], Errors.undefined_token ^ Storage.s
