open A

type root' =
  | A' of a'
  | B' of int
  | C' of string
and a' =
  {
    a1' : ta1' ;
    a2' : ta2' ;
  }
and ta1' =
  | X' of root'
  | Y' of ta2'
and ta2' =
  | Z' of ta2'
  | W' of unit

type 'state continue_fold =
  {
    root : root -> 'state -> (root' * 'state) ;
    root_A : a -> 'state -> (a' * 'state) ;
    root_B : int -> 'state -> (int * 'state) ;
    root_C : string -> 'state -> (string * 'state) ;
    a : a -> 'state -> (a' * 'state) ;
    a_a1 : ta1 -> 'state -> (ta1' * 'state) ;
    a_a2 : ta2 -> 'state -> (ta2' * 'state) ;
    ta1 : ta1 -> 'state -> (ta1' * 'state) ;
    ta1_X : root -> 'state -> (root' * 'state) ;
    ta1_Y : ta2 -> 'state -> (ta2' * 'state) ;
    ta2 : ta2 -> 'state -> (ta2' * 'state) ;
    ta2_Z : ta2 -> 'state -> (ta2' * 'state) ;
    ta2_W : unit -> 'state -> (unit * 'state) ;
  }

type 'state fold_config =
  {
    root : root -> 'state -> ('state continue_fold) -> (root' * 'state) ;
    root_pre_state : root -> 'state -> 'state ;
    root_post_state : root -> root' -> 'state -> 'state ;
    root_A : a -> 'state -> ('state continue_fold) -> (a' * 'state) ;
    root_B : int -> 'state -> ('state continue_fold) -> (int * 'state) ;
    root_C : string -> 'state -> ('state continue_fold) -> (string * 'state) ;
    a : a -> 'state -> ('state continue_fold) -> (a' * 'state) ;
    a_pre_state : a -> 'state -> 'state ;
    a_post_state : a -> a' -> 'state -> 'state ;
    a_a1 : ta1 -> 'state -> ('state continue_fold) -> (ta1' * 'state) ;
    a_a2 : ta2 -> 'state -> ('state continue_fold) -> (ta2' * 'state) ;
    ta1 : ta1 -> 'state -> ('state continue_fold) -> (ta1' * 'state) ;
    ta1_pre_state : ta1 -> 'state -> 'state ;
    ta1_post_state : ta1 -> ta1' -> 'state -> 'state ;
    ta1_X : root -> 'state -> ('state continue_fold) -> (root' * 'state) ;
    ta1_Y : ta2 -> 'state -> ('state continue_fold) -> (ta2' * 'state) ;
    ta2 : ta2 -> 'state -> ('state continue_fold) -> (ta2' * 'state) ;
    ta2_pre_state : ta2 -> 'state -> 'state ;
    ta2_post_state : ta2 -> ta2' -> 'state -> 'state ;
    ta2_Z : ta2 -> 'state -> ('state continue_fold) -> (ta2' * 'state) ;
    ta2_W : unit -> 'state -> ('state continue_fold) -> (unit * 'state) ;
  }

(* Curries the "visitor" argument to the folds (non-customizable traversal functions). *)
let rec mk_continue_fold : type state . state fold_config -> state continue_fold = fun visitor ->
  {
    root = fold_root visitor ;
    root_A = fold_root_A visitor ;
    root_B = fold_root_B visitor ;
    root_C = fold_root_C visitor ;
    a = fold_a visitor ;
    a_a1 = fold_a_a1 visitor ;
    a_a2 = fold_a_a2 visitor ;
    ta1 = fold_ta1 visitor ;
    ta1_X = fold_ta1_X visitor ;
    ta1_Y = fold_ta1_Y visitor ;
    ta2 = fold_ta2 visitor ;
    ta2_Z = fold_ta2_Z visitor ;
    ta2_W = fold_ta2_W visitor ;
}

and fold_root : type state . state fold_config -> root -> state -> (root' * state) = fun visitor x state ->
  let continue_fold : state continue_fold = mk_continue_fold visitor in
  let state = visitor.root_pre_state x state in
  let (new_x, state) = visitor.root x state continue_fold in
  let state = visitor.root_post_state x new_x state in
  (new_x, state)

and fold_root_A : type state . state fold_config -> a -> state -> (a' * state) = fun visitor x state ->
  let continue_fold : state continue_fold = mk_continue_fold visitor in
  visitor.root_A x state continue_fold

and fold_root_B : type state . state fold_config -> int -> state -> (int * state) = fun visitor x state ->
  let continue_fold : state continue_fold = mk_continue_fold visitor in
  visitor.root_B x state continue_fold

and fold_root_C : type state . state fold_config -> string -> state -> (string * state) = fun visitor x state ->
  let continue_fold : state continue_fold = mk_continue_fold visitor in
  visitor.root_C x state continue_fold

and fold_a : type state . state fold_config -> a -> state -> (a' * state) = fun visitor x state ->
  let continue_fold : state continue_fold = mk_continue_fold visitor in
  let state = visitor.a_pre_state x state in
  let (new_x, state) = visitor.a x state continue_fold in
  let state = visitor.a_post_state x new_x state in
  (new_x, state)

and fold_a_a1 : type state . state fold_config -> ta1 -> state -> (ta1' * state) = fun visitor x state ->
  let continue_fold : state continue_fold = mk_continue_fold visitor in
  visitor.a_a1 x state continue_fold

and fold_a_a2 : type state . state fold_config -> ta2 -> state -> (ta2' * state) = fun visitor x state ->
  let continue_fold : state continue_fold = mk_continue_fold visitor in
  visitor.a_a2 x state continue_fold

and fold_ta1 : type state . state fold_config -> ta1 -> state -> (ta1' * state) = fun visitor x state ->
  let continue_fold : state continue_fold = mk_continue_fold visitor in
  let state = visitor.ta1_pre_state x state in
  let (new_x, state) = visitor.ta1 x state continue_fold in
  let state = visitor.ta1_post_state x new_x state in
  (new_x, state)

and fold_ta1_X : type state . state fold_config -> root -> state -> (root' * state) = fun visitor x state ->
  let continue_fold : state continue_fold = mk_continue_fold visitor in
  visitor.ta1_X x state continue_fold

and fold_ta1_Y : type state . state fold_config -> ta2 -> state -> (ta2' * state) = fun visitor x state ->
  let continue_fold : state continue_fold = mk_continue_fold visitor in
  visitor.ta1_Y x state continue_fold

and fold_ta2 : type state . state fold_config -> ta2 -> state -> (ta2' * state) = fun visitor x state ->
  let continue_fold : state continue_fold = mk_continue_fold visitor in
  let state = visitor.ta2_pre_state x state in
  let (new_x, state) = visitor.ta2 x state continue_fold in
  let state = visitor.ta2_post_state x new_x state in
  (new_x, state)

and fold_ta2_Z : type state . state fold_config -> ta2 -> state -> (ta2' * state) = fun visitor x state ->
  let continue_fold : state continue_fold = mk_continue_fold visitor in
  visitor.ta2_Z x state continue_fold

and fold_ta2_W : type state . state fold_config -> unit -> state -> (unit * state) = fun visitor x state ->
  let continue_fold : state continue_fold = mk_continue_fold visitor in
  visitor.ta2_W x state continue_fold

let no_op : 'a fold_config = {
  root = (fun v state continue ->
    match v with
    | A v -> let (v, state) = continue.root_A v state in (A' v, state)
    | B v -> let (v, state) = continue.root_B v state in (B' v, state)
    | C v -> let (v, state) = continue.root_C v state in (C' v, state)
  );
  root_pre_state = (fun v state -> ignore v; state) ;
  root_post_state = (fun v new_v state -> ignore (v, new_v); state) ;
  root_A = (fun v state continue -> continue.a v state ) ;
  root_B = (fun v state continue -> ignore continue; (v, state) ) ;
  root_C = (fun v state continue -> ignore continue; (v, state) ) ;
  a = (fun v state continue ->
    match v with
      { a1; a2; } ->
      let (a1', state) = continue.a_a1 a1 state in
      let (a2', state) = continue.a_a2 a2 state in
      ({ a1'; a2'; }, state)
  );
  a_pre_state = (fun v state -> ignore v; state) ;
  a_post_state = (fun v new_v state -> ignore (v, new_v); state) ;
  a_a1 = (fun v state continue -> continue.ta1 v state ) ;
  a_a2 = (fun v state continue -> continue.ta2 v state ) ;
  ta1 = (fun v state continue ->
    match v with
    | X v -> let (v, state) = continue.ta1_X v state in (X' v, state)
    | Y v -> let (v, state) = continue.ta1_Y v state in (Y' v, state)
  );
  ta1_pre_state = (fun v state -> ignore v; state) ;
  ta1_post_state = (fun v new_v state -> ignore (v, new_v); state) ;
  ta1_X = (fun v state continue -> continue.root v state ) ;
  ta1_Y = (fun v state continue -> continue.ta2 v state ) ;
  ta2 = (fun v state continue ->
    match v with
    | Z v -> let (v, state) = continue.ta2_Z v state in (Z' v, state)
    | W v -> let (v, state) = continue.ta2_W v state in (W' v, state)
  );
  ta2_pre_state = (fun v state -> ignore v; state) ;
  ta2_post_state = (fun v new_v state -> ignore (v, new_v); state) ;
  ta2_Z = (fun v state continue -> continue.ta2 v state ) ;
  ta2_W = (fun v state continue -> ignore continue; (v, state) ) ;
}
