module Context : sig
  type context
  type t = context

  val run : (context -> 'k) -> ('k, exn) result
end

(* TODO: likely this Context.t return should be removed *)
val solve_module : Context.t -> Caml_core.program -> Context.t * Ast_core.program
