module A = struct
  let a = 1
end

(* This is to check that we do pass the scope when evaluating inner module *)
module B = struct
  module C = struct
    module D = struct
      module E = A
    end
  end
  let b = A.a
end

module B = struct
  let b =
    module A = struct
      let ba = 2
      let baa = ba
    end in
    A.ba
end

let x = B.A.a
