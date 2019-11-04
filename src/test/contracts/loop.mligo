(* Test loops in CameLIGO *)

let aux_simple (i: int) : bool * int =
  if i < 100 then continue (i + 1) else stop i

let counter_simple (n: int) : int =
  Loop.fold_while n aux_simple

type sum_aggregator = {
  counter : int ;
  sum : int ;
}

let counter (n : int) : int =
  let initial : sum_aggregator = { counter = 0 ; sum = 0 } in
  let out : sum_aggregator = Loop.fold_while initial (fun (prev: sum_aggregator) ->
    if prev.counter <= n then
      continue ({ counter = prev.counter + 1 ; sum = prev.counter + prev.sum })
    else
      stop ({ counter = prev.counter ; sum = prev.sum })
  ) in out.sum

let aux_nest (prev: sum_aggregator) : bool * sum_aggregator =
  if prev.counter < 100 then
    continue ({ counter = prev.counter + 1 ;
                sum = prev.sum + Loop.fold_while prev.counter aux_simple})
  else
    stop ({ counter = prev.counter ; sum = prev.sum })

let counter_nest (n: int) : int =
  let initial : sum_aggregator = { counter = 0 ; sum = 0 } in
  let out : sum_aggregator = Loop.fold_while initial aux_nest
  in out.sum
