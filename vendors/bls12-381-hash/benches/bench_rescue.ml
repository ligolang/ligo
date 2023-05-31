open Core_bench

let t_state_size_3 =
  let nb_rounds, state_size, mds, round_constants =
    Bls12_381_hash.Rescue.Parameters.state_size_3
  in
  let inputs = Array.init state_size (fun _ -> Bls12_381.Fr.random ()) in
  let ctxt =
    Bls12_381_hash.Rescue.allocate_ctxt mds round_constants nb_rounds state_size
  in
  let () = Bls12_381_hash.Rescue.set_state ctxt inputs in
  let name = "Benchmark one permutation of Rescue" in
  Bench.Test.create ~name (fun () ->
      Bls12_381_hash.Rescue.apply_permutation ctxt)

let command = Bench.make_command [t_state_size_3]

let () = Core.Command.run command
