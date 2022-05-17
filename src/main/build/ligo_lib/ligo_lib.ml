module Ligo_Stdlib = struct
  let get s = match Loaded.read s with Some x -> x | None -> failwith "ligo compile-time went wrong: please report to devs"
  let std_uncurry_jakarta = get "std_uncurry_jakarta.mligo"
  let std_uncurry_ithaca = get "std_uncurry_ithaca.mligo"
  let std_curry_jakarta = get "std_curry_jakarta.mligo"
  let std_curry_ithaca = get "std_curry_ithaca.mligo"
end
