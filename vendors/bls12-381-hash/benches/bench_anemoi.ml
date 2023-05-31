open Core_bench

let t1 =
  let a, b = (Bls12_381.Fr.random (), Bls12_381.Fr.random ()) in
  let name = "Benchmark AnemoiJive128_1" in
  Bench.Test.create ~name (fun () ->
      ignore @@ Bls12_381_hash.Anemoi.jive128_1 a b)

let t2 l =
  let state_size = l * 2 in
  let mds =
    Array.init l (fun _ -> Array.init l (fun _ -> Bls12_381.Fr.random ()))
  in
  let state = Array.init state_size (fun _ -> Bls12_381.Fr.random ()) in
  let name =
    Printf.sprintf "Benchmark Anemoi [m = %d, security = 128]" state_size
  in
  let parameters =
    if state_size = 2 then
      Bls12_381_hash.Anemoi.Parameters.security_128_state_size_2
    else if state_size = 4 then
      Bls12_381_hash.Anemoi.Parameters.security_128_state_size_4
    else if state_size = 6 then
      Bls12_381_hash.Anemoi.Parameters.security_128_state_size_6
    else if state_size = 8 then
      Bls12_381_hash.Anemoi.Parameters.security_128_state_size_8
    else Bls12_381_hash.Anemoi.Parameters.create 128 state_size mds
  in
  let ctxt = Bls12_381_hash.Anemoi.allocate_ctxt parameters in
  let () = Bls12_381_hash.Anemoi.set_state ctxt state in
  Bench.Test.create ~name (fun () ->
      Bls12_381_hash.Anemoi.apply_permutation ctxt)

let command = Bench.make_command [t1; t2 1; t2 2; t2 3; t2 4; t2 5; t2 6; t2 10]

let () = Core.Command.run command
