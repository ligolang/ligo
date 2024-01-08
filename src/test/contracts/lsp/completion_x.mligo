module M = struct
  let x = 42
  type t = int

  module N = struct
    type t = string
    let y = 100
  end
end
