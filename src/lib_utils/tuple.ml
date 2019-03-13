let map2 f (a, b) = (f a, f b)
let apply2 f (a, b) = f a b
let list2 (a, b) = [a;b]

module Pair = struct
  let map = map2
  let apply f (a, b) = f a b
end
