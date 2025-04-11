module C = Gitlab_pages.Docs.Syntax.Contracts.Src.Entrypoints.Contract_main

module Proxy = struct
  [@entry]
  let proxy (p : C.parameter) (s : C.storage) : operation list * C.storage =
    C.main p s
end