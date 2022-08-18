module Protocols = Protocols


(* Environment records declarations already seen in reverse orders. Use for different kind of processes *)
type t
val pp : Format.formatter -> t -> unit

val add_declaration : Ast_typed.decl -> t -> t
val add_module : ?public:unit -> ?hidden:unit -> Stage_common.ModuleVar.t -> Ast_typed.module_ -> t -> t
val append : Ast_typed.program -> t -> t

val default : Protocols.t -> t
val default_with_test : Protocols.t -> t

val fold : f:('a -> Ast_typed.decl -> 'a) -> init:'a -> t -> 'a

(* Code smell that comes from a bad design of the buildsystem *)
type core
val add_core_module : ?public:unit -> ?hidden:unit -> Stage_common.ModuleVar.t -> Ast_core.module_ -> core -> core
val init_core       : Ast_core.program -> core
val to_module       : t -> Ast_typed.module_
val to_program      : t -> Ast_typed.program
val to_core_module  : core -> Ast_core.module_
val to_core_program : core -> Ast_core.program
val append_core     : Ast_core.program -> core -> core
