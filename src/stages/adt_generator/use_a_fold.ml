open A
open Fold

let _ =
  let some_root = ((failwith "assume we have some root") : root) in
  let op = {
      no_op with
      a = fun the_a state continue_fold ->
          let (a1' , state') = continue_fold.ta1 the_a.a1 state in
          let (a2' , state'') = continue_fold.ta2 the_a.a2 state' in
          ({
              a1' = a1' ;
              a2' = a2' ;
            }, state'')
    } in
  let state = () in
  fold_root op some_root state

