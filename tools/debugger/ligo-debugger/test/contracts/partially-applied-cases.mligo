let f a b = a + b

let f_uncurry (a, b) c = a + b + c

let op (f : int -> int) a b = f a + b

let triple a b c d = a + b + c + d

let simple_case () =
  let applied = f 42 in
  let should_not_have_applied_args = applied 10 in
  ignore should_not_have_applied_args

let apply_vars () =
  let a = 42 in
  let applied = f a in
  ignore applied

let tuple_is_one_arg () =
  let applied = f_uncurry (42, 42) in
  ignore applied

let apply_expression () =
  let a = 100 in
  let b = a - 42 in
  let applied = f (a + b + a * b) in
  ignore applied

let apply_applied_function () =
  let f1 = f 42 in
  let apply = op f1 (1000 - 7) in
  ignore apply

let apply_sequence () =
  let f1 = triple 1 in
  let f2 = f1 2 in
  let f3 = f2 3 in
  let f4 = f3 4 in
  ignore f4

type some_foo = int -> int -> int

let type_aliases_have_children () =
  let type_aliased : some_foo = triple 1 2 in
  ignore type_aliased

[@entry]
let main () () : operation list * unit = begin
  simple_case();
  apply_vars();
  tuple_is_one_arg();
  apply_expression();
  apply_applied_function();
  apply_sequence();
  type_aliases_have_children();
  [], ()
end
