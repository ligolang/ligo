Set Implicit Arguments.
From Coq Require Import List String ZArith.
Import ListNotations.
Require Extraction.
Require ExtrOcamlBasic.

From ligo_coq Require Import bytes.

Definition annot := list string.

(* AST of Micheline types.
   l can contain arbitrary metadata (l stands for location, which is commonly used as metadata on nodes)
   p is the inductive type listing the primitive type constructors, e.g. List | Set | Map. *)
Inductive node (l p : Set) : Set :=
| Int : l -> Z.t -> node l p
| String : l -> string -> node l p
| Bytes : l -> bytes -> node l p
| Prim : l -> p -> list (node l p) -> annot -> node l p
| Seq : l -> list (node l p) -> node l p
.

Arguments Int {l p}.
Arguments String {l p}.
Arguments Bytes {l p}.

(* annotate a type with the given string if it is a Prim node (leave unchanged otherwise) *)
Definition annotate {l p} (x : node l p) (ann : string) : node l p :=
  match x with
    | Prim l p args annot => Prim l p args (ann :: annot)
    | _ => x
  end.

(* In order to avoid a dune/coq bug, this development currently does
   not depend on Tezos. Instead, this file will define a new copy of
   Micheline's node type, and we will convert back and forth between
   the two types outside of the extracted code. See also
   src/coq_ocaml/micheline_wrapper.ml *)
(*
Extract Inductive node => "Tezos_micheline.Micheline.node"
  ["Tezos_micheline.Micheline.Int"
   "Tezos_micheline.Micheline.String"
   "Tezos_micheline.Micheline.Bytes"
   "Tezos_micheline.Micheline.Prim"
   "Tezos_micheline.Micheline.Seq"].
*)
