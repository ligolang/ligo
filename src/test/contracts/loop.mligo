(* Test functional iterators in CameLIGO *)

let aux_simple (i : int) : bool * int =
  if i < 100 then Loop.resume (i + 1) else Loop.stop i

let counter_simple (n : int) : int =
  Loop.fold_while aux_simple n

type sum_aggregator = {
  counter : int;
  sum : int
}

let counter (n : int) : int =
  let initial : sum_aggregator = {counter=0; sum=0} in
  let aggregate = fun (prev : sum_aggregator) ->
    if prev.counter <= n then
      Loop.resume {counter = prev.counter + 1;
                   sum = prev.counter + prev.sum}
    else
      Loop.stop {counter = prev.counter; sum = prev.sum} in
  let out : sum_aggregator =
    Loop.fold_while aggregate initial
  in out.sum

let aux_nest (prev : sum_aggregator) : bool * sum_aggregator =
  if prev.counter < 100 then
    let sum : int =
      prev.sum + Loop.fold_while aux_simple prev.counter
    in Loop.resume {counter = prev.counter + 1; sum = sum}
  else
    Loop.stop {counter = prev.counter; sum = prev.sum}

let counter_nest (n : int) : int =
  let initial : sum_aggregator = {counter=0; sum=0} in
  let out : sum_aggregator = Loop.fold_while aux_nest initial
  in out.sum
