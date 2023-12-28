module type T = sig
  val x : int
end

module M : T = struct
  include struct
    include struct
      let x = 42
    end
  end
end

let x = M.x
