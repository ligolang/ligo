module Protocols = Protocols


(* Environment is a "prelude" to all modules in LIGO *)
type t
val pp : Format.formatter -> t -> unit

val add_declaration : Ast_typed.decl -> t -> t
val add_module : ?public:unit -> ?hidden:unit -> Ligo_prim.Module_var.t -> Ast_typed.module_ -> t -> t
val append : t -> Ast_typed.program -> t

val default : Protocols.t -> t
val default_with_test : Protocols.t -> t

val fold : f:('a -> Ast_typed.decl -> 'a) -> init:'a -> t -> 'a
val foldi : f:(int -> 'a -> Ast_typed.decl -> 'a) -> init:'a -> t -> 'a


(* Code smell that comes from a bad design of the buildsystem *)
type core
val add_core_module : ?public:unit -> ?hidden:unit -> Ligo_prim.Module_var.t -> Ast_core.module_ -> core -> core
val init_core       : Ast_core.program -> core
val to_module       : t -> Ast_typed.module_
val to_program      : t -> Ast_typed.program
val to_core_module  : core -> Ast_core.module_
val to_core_program : core -> Ast_core.program
val append_core    : core -> Ast_core.program -> core
