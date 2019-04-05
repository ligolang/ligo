(* AST *)
(* Language *)

  (* Singletons *)
type variable = string

  (* Hierarchies *)
type arith =
| Let_in of (variable Location.wrap * arith Location.wrap * arith Location.wrap)
| Addition of (arith Location.wrap * arith Location.wrap)
| Substraction of (arith Location.wrap * arith Location.wrap)
| Multiplication of (arith Location.wrap * arith Location.wrap)
| Division of (arith Location.wrap * arith Location.wrap)
| List of ((arith Location.wrap list))
| Arith_variable of (variable Location.wrap)
  (* Entry point *)
type entry_point = arith


