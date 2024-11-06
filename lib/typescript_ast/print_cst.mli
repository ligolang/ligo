module Loc_map = Typescript_ast.Loc_map
module Ts_wrap = Typescript_ast.Ts_wrap

val print_program : string -> Loc_map.t -> Ts_wrap.ts_tree -> string
