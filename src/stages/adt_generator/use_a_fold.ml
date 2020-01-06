open A
open Fold

(* TODO: how should we plug these into our test framework? *)

let () =
  let some_root : root = A { a1 = X (A { a1 = X (B 1) ; a2 = W () ; }) ; a2 = Z (W ()) ; } in
  let op = {
      no_op with
      a = fun the_a state continue_fold ->
          let (a1' , state') = continue_fold.ta1 the_a.a1 state in
          let (a2' , state'') = continue_fold.ta2 the_a.a2 state' in
          ({
              a1' = a1' ;
              a2' = a2' ;
            }, state'' + 1)
    } in
  let state = 0 in
  let (_, state) = fold_root op some_root state in
  if state != 2 then
    failwith (Printf.sprintf "Test failed: expected folder to count 2 nodes, but it counted %d nodes" state)
  else
    ()

let () =
  let some_root : root = A { a1 = X (A { a1 = X (B 1) ; a2 = W () ; }) ; a2 = Z (W ()) ; } in
  let op = { no_op with a_pre_state = fun _the_a state -> state + 1 } in
  let state = 0 in
  let (_, state) = fold_root op some_root state in
  if state != 2 then
    failwith (Printf.sprintf "Test failed: expected folder to count 2 nodes, but it counted %d nodes" state)
  else
    ()

let () =
  let some_root : root = A { a1 = X (A { a1 = X (B 1) ; a2 = W () ; }) ; a2 = Z (W ()) ; } in
  let op = { no_op with a_post_state = fun _the_a _new_a state -> state + 1 } in
  let state = 0 in
  let (_, state) = fold_root op some_root state in
  if state != 2 then
    failwith (Printf.sprintf "Test failed: expected folder to count 2 nodes, but it counted %d nodes" state)
  else
    ()


(* Test that the same fold_config can be ascibed with different 'a type arguments *)
let _noi : int fold_config = no_op (* (fun _ -> ()) *)
let _nob : bool fold_config = no_op (* (fun _ -> ()) *)
