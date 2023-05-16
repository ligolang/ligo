open Core_bench

let t params =
  let nb_rounds, state_size, constants, alpha_beta_s = params in
  let name =
    Printf.sprintf
      "Benchmark one permutation of Griffin (state size = %d)"
      state_size
  in
  let ctxt =
    Bls12_381_hash.Griffin.allocate_ctxt
      nb_rounds
      state_size
      constants
      alpha_beta_s
  in
  let () =
    Bls12_381_hash.Griffin.set_state
      ctxt
      (Array.init state_size (fun _ -> Bls12_381.Fr.random ()))
  in
  Bench.Test.create ~name (fun () ->
      let () = Bls12_381_hash.Griffin.apply_permutation ctxt in
      ())

let command =
  Bench.make_command
    [ t Bls12_381_hash.Griffin.Parameters.state_size_3;
      t Bls12_381_hash.Griffin.Parameters.state_size_4 ]

let () = Core.Command.run command
