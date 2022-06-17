Local Set Warnings "-implicit-core-hint-db".
Set Implicit Arguments.
From Coq Require Import String List.
Import ListNotations.

(* Inductive D : Prop -> Prop := *)
(* | DD : D (exists P, D P) *)
(* . *)
(* Error: Non strictly positive occurrence of "D" in "D (exists P : Prop, D P)". *)

(* From ligo_coq Require Import micheline. *)

Section types.

Context {A : Set}.

(*
Local Open Scope string_scope.

(* This typeclass-like prop accepts types which are suitable arguments for the "iter" Michelson instruction.
   class iter_class item iterable
   instance a          (list a)
   instance a          (set a)
   instance (pair k v) (map k v) *)
Inductive iter_class : node A string -> node A string -> Prop :=
| Iter_list {l a n} : iter_class a (Prim l "list" [a] n)
| Iter_set {l a n} : iter_class a (Prim l "set" [a] n)
| Iter_map {l1 l2 k v n1 n2} : iter_class (Prim l1 "pair" [k; v] n1) (Prim l2 "map" [k; v] n2)
.

(* This typeclass-like prop accepts types which are suitable arguments for the "map" Michelson instruction.
   class iter_class item result_item collection result_collection
   instance a          b (list a)  (list b)   -- Mapping an (a -> b)   function on a (list a)  gives a (list b)
   instance (pair k v) r (map k v) (map k r)  -- Mapping a  (k*v -> r) function on a (map k v) gives a (map k r) *)
Inductive map_class : node A string -> node A string -> node A string -> node A string -> Prop :=
| Map_list {l1 l2 a b n1 n2} : map_class a b (Prim l1 "list" [a] n1) (Prim l2 "list" [b] n2)
| Map_map {l1 l2 l3 k v r n1 n2 n3} : map_class (Prim l1 "pair" [k; v] n1) r (Prim l2 "map" [k; v] n2) (Prim l3 "map" [k; r] n3)
.

Hint Constructors iter_class.
Hint Constructors map_class.
*)

End types.
