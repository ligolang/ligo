#import "./agg_foo.mligo" "Foo"

let case_cons_1 =
  Foo.case
    "cons"
    "cons of a value to an empty list should produce a list of one element"

let () = Foo.run_suite (Foo.suite "A simple list extension" [case_cons_1])
