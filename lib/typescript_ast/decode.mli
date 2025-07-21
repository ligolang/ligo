module Region = Simple_utils.Region
module Loc_map = Typescript_ast.Loc_map
module Ts_wrap = Typescript_ast.Ts_wrap
module Ast = Typescript_ast.Ast

(* Decoding an input TypeScript program into an AST *)

val dec_program
  :  ?debug_arg:bool
  -> filename:string
  -> file:string
  -> Loc_map.t
  -> Ts_wrap.ts_tree
  -> (Ast.t, string Region.reg) result

(* The parameter [node] is the root of a Typescript CST, *not of an
   expression*. That's why we have to find the expression below the
   root. This is because tree-sitter does not provide the generated
   parsers with multiple entry-points. *)

val dec_standalone_expression
  :  file:string
  -> Loc_map.t
  -> Ts_wrap.ts_tree
  -> (Ast.expression, string Region.reg) result

(* The parameter [node] is the root of a Typescript CST, *not of a
   type expression*. tree-sitter does not provide the generated
   parsers with multiple entry-points, so, in order to parse a type
   expression, we assume that the input string starts with "type t = ",
   so we fetch the type in the produced CST (last child of the root,
   which is an type_alias_declaration). *)

val dec_standalone_type_expr
  :  Loc_map.t
  -> Ts_wrap.ts_tree
  -> (Ast.type_expr, string Region.reg) result
