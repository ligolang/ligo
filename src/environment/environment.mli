module Protocols = Protocols

(* Environment is a "prelude" to all modules in LIGO *)
type t
type signature

val add_declaration : Ast_typed.decl -> t -> t
val append : t -> Ast_typed.program -> t
val default : Protocols.t -> t
val fold : f:('a -> Ast_typed.decl -> 'a) -> init:'a -> t -> 'a

(* Code smell that comes from a bad design of the buildsystem *)
type core

val to_program : t -> Ast_typed.program
