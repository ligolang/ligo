open Ast_typed
module Protocols = Protocols

(* This is an env use by repl and build *)
(* Environment records declarations already seen in reverse orders. Use for different kind of processes *)
type t = program
type signature = Ast_typed.signature

let add_declaration decl env = decl :: env
let append env (program : program) : t = List.rev_append program env
let fold ~f ~init (env : t) = List.fold ~f ~init (List.rev env)
let empty : t = []

(* Artifact for build system *)
type core = Ast_core.module_

let to_program (env : t) : program = List.rev env
