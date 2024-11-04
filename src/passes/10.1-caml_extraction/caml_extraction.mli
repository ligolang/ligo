open Caml_core

module Context : sig
  type context
  type t = context

  val run : (context -> 'k) -> ('k * error list, error) result
end

val extract_str : Context.t -> Typedtree.structure -> program
