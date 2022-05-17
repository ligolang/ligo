module Ligo_Stdlib = struct
  let get s = match Loaded.read s with Some x -> x | None -> failwith "Ligo_Stdlib missing (should only happen at ligo compile-time): please report to devs"
  let std_uncurry_jakarta = get "std_uncurry_jakarta.mligo"
  let std_uncurry_ithaca = get "std_uncurry_ithaca.mligo"
  let std_curry_jakarta = get "std_curry_jakarta.mligo"
  let std_curry_ithaca = get "std_curry_ithaca.mligo"
end

module Ligo_Testlib = struct
  let get s = match Loaded.read s with Some x -> x | None -> failwith "Ligo_Testlib missing (should only happen at ligo compile-time): please report to devs"
  let test_uncurry = get "test_uncurry.mligo"
  let test_curry = get "test_curry.mligo"
  let test_uncurry_stub = get "test_uncurry_stub.mligo"
  let test_curry_stub = get "test_curry_stub.mligo"
end