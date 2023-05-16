open Core_bench

let t1 =
  let name = "Benchmark one permutation of Poseidon128" in
  let state_size, nb_full_rounds, nb_partial_rounds, batch_size, ark, mds =
    Bls12_381_hash.Poseidon.Parameters.state_size_128_3
  in
  let state = Array.init state_size (fun _ -> Bls12_381.Fr.random ()) in
  let ctxt =
    Bls12_381_hash.Poseidon.allocate_ctxt
      state_size
      nb_full_rounds
      nb_partial_rounds
      batch_size
      ark
      mds
  in
  let () = Bls12_381_hash.Poseidon.set_state ctxt state in
  Bench.Test.create ~name (fun () ->
      Bls12_381_hash.Poseidon.apply_permutation ctxt)

let create_bench nb_full_rounds nb_partial_rounds width batch_size =
  let ark_length = width * (nb_full_rounds + nb_partial_rounds) in
  let ark = Array.init ark_length (fun _ -> Bls12_381.Fr.random ()) in
  let mds =
    Array.init width (fun _ ->
        Array.init width (fun _ -> Bls12_381.Fr.random ()))
  in
  let state = Array.init width (fun _ -> Bls12_381.Fr.random ()) in
  let ctxt =
    Bls12_381_hash.Poseidon.allocate_ctxt
      width
      nb_full_rounds
      nb_partial_rounds
      batch_size
      ark
      mds
  in
  let () = Bls12_381_hash.Poseidon.set_state ctxt state in
  let name =
    Printf.sprintf
      "Benchmark Poseidon: width = %d, partial = %d, full = %d, batch size = %d"
      width
      nb_partial_rounds
      nb_full_rounds
      batch_size
  in
  let t =
    Bench.Test.create ~name (fun () ->
        Bls12_381_hash.Poseidon.apply_permutation ctxt)
  in
  t

let bench_neptunus =
  let width = 5 in
  let nb_full_rounds = 60 in
  let nb_partial_rounds = 0 in
  let batch_size = 1 in
  let ark_length = width * (nb_full_rounds + nb_partial_rounds) in
  let ark = Array.init ark_length (fun _ -> Bls12_381.Fr.random ()) in
  let mds =
    Array.init width (fun _ ->
        Array.init width (fun _ -> Bls12_381.Fr.random ()))
  in
  let state = Array.init width (fun _ -> Bls12_381.Fr.random ()) in
  let ctxt =
    Bls12_381_hash.Poseidon.allocate_ctxt
      width
      nb_full_rounds
      nb_partial_rounds
      batch_size
      ark
      mds
  in
  let () = Bls12_381_hash.Poseidon.set_state ctxt state in
  let name = "Benchmark Neptunus" in
  Bench.Test.create ~name (fun () ->
      Bls12_381_hash.Poseidon.apply_permutation ctxt)

let command = Bench.make_command (t1 :: [bench_neptunus])

let () = Core.Command.run command
