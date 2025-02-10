module Loc_map = Typescript_ast.Loc_map
module Ts_wrap = Typescript_ast.Ts_wrap

val print_program : filename:string -> file:string -> Loc_map.t -> Ts_wrap.ts_tree -> string
