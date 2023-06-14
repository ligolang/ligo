[@ppx_helpers] type dyn_param = Foo | Bar of int
let enum_foo : nat = enum_foo
let enum_bar : nat = enum_bar
let get_foo : dyn_param -> unit option = get_foo
let get_bar : dyn_param -> int option = get_bar
let make_foo : unit -> dyn_param = make_foo
let make_bar : int -> dyn_param = make_bar