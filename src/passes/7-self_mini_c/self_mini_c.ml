open Mini_c
open Trace

(* Overly conservative for now: ok to treat pure things as impure,
   must not treat impure things as pure. *)
let is_pure : expression -> bool = fun e ->
  match e.content with
  | E_closure _ -> true
  | _ -> false

let rec elim_dead_lambdas : expression -> expression result = fun e ->
  let changed = ref false in (* ugh *)
  let mapper : Helpers.mapper = fun e ->
    match e.content with
    | E_let_in ((x, _), e1, e2) when is_pure e1 ->
      let fvs = Free_variables.expression [] e2 in
      if Free_variables.mem x fvs
      then ok e
      else
        (* pure e1 is not used, eliminate! *)
        (changed := true ; ok e2)
    | _ -> ok e in
  let%bind e = Helpers.map_expression mapper e in
  if !changed
  then elim_dead_lambdas e
  else ok e

let all_expression : expression -> expression result =
  elim_dead_lambdas
