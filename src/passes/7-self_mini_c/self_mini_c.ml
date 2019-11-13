open Mini_c
open Trace

(* Overly conservative purity test: ok to treat pure things as impure,
   must not treat impure things as pure. *)

(* true if the name names a pure constant -- i.e. if uses will be pure
   assuming arguments are pure *)
let is_pure_constant : string -> bool =
  function
  | "CAR"
  | "CDR"
  | "PAIR"
    -> true
  (* TODO... *)
  | _ -> false

let rec is_pure : expression -> bool = fun e ->
  match e.content with
  | E_literal _
  | E_closure _
  | E_skip
  | E_variable _
  | E_make_empty_map _
  | E_make_empty_list _
  | E_make_empty_set _
  | E_make_none _
    -> true

  | E_if_bool (cond, bt, bf)
  | E_if_none (cond, bt, (_, bf))
  | E_if_cons (cond, bt, (_, bf))
  | E_if_left (cond, (_, bt), (_, bf))
    -> List.for_all is_pure [ cond ; bt ; bf ]

  | E_let_in (_, e1, e2)
  | E_sequence (e1, e2)
    -> List.for_all is_pure [ e1 ; e2 ]

  | E_constant (c, args)
    -> is_pure_constant c && List.for_all is_pure args

  (* I'm not sure about these. Maybe can be tested better? *)
  | E_application _
  | E_iterator _
  | E_fold _
    -> false

  (* Could be pure, but, divergence is an effect, so halting problem
     is near... *)
  | E_while _ -> false

  (* definitely not pure *)
  | E_assignment _ -> false


(* Eliminate dead `let` with pure rhs *)

let rec elim_dead_code : expression -> expression result = fun e ->
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
  then elim_dead_code e
  else ok e

let all_expression : expression -> expression result =
  elim_dead_code
