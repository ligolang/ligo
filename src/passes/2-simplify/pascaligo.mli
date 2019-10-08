(** Converts PascaLIGO programs to the Simplified Abstract Syntax Tree. *)

open Trace
open Ast_simplified

module Raw = Parser.Pascaligo.AST
module SMap = Map.String

module Errors : sig

  val bad_bytes : Location.t -> string -> unit -> error

  val unsupported_arith_op : Raw.expr -> unit -> error

  val unsupported_proc_calls : 'a Raw.reg -> unit -> error

end


(** Convert a concrete PascaLIGO expression AST to the simplified expression AST 
    used by the compiler. *)
val simpl_expression : Raw.expr -> expr result

(** Convert a concrete PascaLIGO program AST to the simplified program AST used 
    by the compiler. *)
val simpl_program : Raw.ast -> program result
