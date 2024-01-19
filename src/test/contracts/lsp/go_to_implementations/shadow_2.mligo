module type T = sig
  type t
end

module M : T = struct
  include struct
    include struct
      type t = string
    end
  end
  type t = int
end

type x = M.t
