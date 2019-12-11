open A

type root' =
| A' of a'
| B' of int
| C' of string

and a' = {
  a1' : ta1' ;
  a2' : ta2' ;
}

and ta1' =
| X' of root'
| Y' of ta2'

and ta2' =
| Z' of ta2'

type 'state continue_fold = {
    a    : a    -> 'state -> (a'    * 'state) ;
    ta1  : ta1  -> 'state -> (ta1'  * 'state) ;
    ta2  : ta2  -> 'state -> (ta2'  * 'state) ;
    root : root -> 'state -> (root' * 'state) ;
}

type 'state fold_config = {
    root   : root   -> 'state -> ('state continue_fold) -> (root'  * 'state) ;
    root_a : a      -> 'state -> ('state continue_fold) -> (a'     * 'state) ;
    root_b : int    -> 'state -> ('state continue_fold) -> (int    * 'state) ;
    root_c : string -> 'state -> ('state continue_fold) -> (string * 'state) ;
    a      : a      -> 'state -> ('state continue_fold) -> (a'     * 'state) ;
    ta1    : ta1    -> 'state -> ('state continue_fold) -> (ta1'   * 'state) ;
    ta2    : ta2    -> 'state -> ('state continue_fold) -> (ta2'   * 'state) ;
  }

let rec mk_continue_fold : type state . state fold_config -> state continue_fold = fun visitor ->
  {
    a    = fold_a    visitor ;
    ta1  = fold_ta1  visitor ;
    ta2  = fold_ta2  visitor ;
    root = fold_root visitor ;
  }

and fold_a : type state . state fold_config -> a -> state -> (a' * state) = fun visitor x state ->
  let continue_fold : state continue_fold = mk_continue_fold visitor in
  match x with
  | { a1; a2 } ->
     let (a1', state) = visitor.ta1 a1 state continue_fold in
     let (a2', state) = visitor.ta2 a2 state continue_fold in
     ({ a1'; a2' }, state)

and fold_ta2 : type state . state fold_config -> ta2 -> state -> (ta2' * state) = fun visitor x state ->
  let continue_fold : state continue_fold = mk_continue_fold visitor in
  match x with
  | Z v -> let (v, state) = visitor.ta2  v state continue_fold in (Z' v, state)

and fold_ta1 : type state . state fold_config -> ta1 -> state -> (ta1' * state) = fun visitor x state ->
  let continue_fold : state continue_fold = mk_continue_fold visitor in
  match x with
  | X v -> let (v, state) = visitor.root v state continue_fold in (X' v , state)
  | Y v -> let (v, state) = visitor.ta2  v state continue_fold in (Y' v , state)

and fold_root : type state . state fold_config -> root -> state -> (root' * state) = fun visitor x state ->
  let continue_fold : state continue_fold = mk_continue_fold visitor in
  match x with
  | A v -> let (v, state) = visitor.a      v state continue_fold in (A' v , state)
  | B v -> let (v, state) = visitor.root_b v state continue_fold in (B' v , state)
  | C v -> let (v, state) = visitor.root_c v state continue_fold in (C' v , state)
let no_op = failwith "todo"
