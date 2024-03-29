#import "gitlab-pages/docs/advanced/src/entrypoints-contracts/contract_main.mligo" "C"

module Proxy = struct

  [@entry]
  let proxy (p : C.parameter) (s : C.storage) : operation list * C.storage =
    C.main p s

end