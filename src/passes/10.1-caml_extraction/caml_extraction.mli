module Context : sig
  type context
  type t = context

  val run : (context -> 'k) -> ('k, exn) result
end

val extract_str : Context.t -> Typedtree.structure -> Caml_core.program
