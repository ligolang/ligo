module M = struct
  type t1 = A of int | B of int
end
module M = struct
  let y = 10
end

(* This will fail because A will not be found *)
(* let x = A 42 *)