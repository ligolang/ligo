open A
open Fold

let _ =
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
  Printf.printf "trilili %d" state


let _noi : int fold_config = no_op (* (fun _ -> ()) *)
let _nob : bool fold_config = no_op (* (fun _ -> ()) *)
