module Protocols = Protocols


(* Environment records declarations already seen in reverse orders. Use for different kind of processes *)
type t 
val pp : Format.formatter -> t -> unit

val add_declaration : Ast_typed.declaration_loc -> t -> t
val add_module : ?public:unit -> Ast_typed.module_variable -> Ast_typed.module' -> t -> t
val append : Ast_typed.program -> t -> t
val init   : Ast_typed.program -> t

val default : Protocols.t -> t
val default_with_test : Protocols.t -> t

val fold : f:('a -> Ast_typed.declaration_loc -> 'a) -> init:'a -> t -> 'a

(* Code smell that comes from a bad design of the buildsystem *)
type core
val add_core_module : ?public:unit -> Ast_core.module_variable -> Ast_core.module' -> core -> core
val init_core       : Ast_core.module' -> core
val to_program      : t -> Ast_typed.program
val to_core_program : core -> Ast_core.module'
val append_core     : Ast_core.module' -> core -> core