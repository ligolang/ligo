(* Higher-order functions *)

(* Composition *)

val ( <@ ) : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c

(* Uncurrying *)

val uncurry : ('a -> 'b -> 'c) -> 'a * 'b -> 'c
