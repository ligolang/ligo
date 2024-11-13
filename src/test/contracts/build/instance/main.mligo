module Errors = Super__.Common.Errors

module Storage = Super__.Common.Storage

[@entry]
let main (_ : unit) (_ : Storage.t) : operation list * Storage.t =
  [], Errors.undefined_token ^ Storage.s
