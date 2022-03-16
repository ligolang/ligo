Set Implicit Arguments.
From Coq Require Import List.
Import ListNotations.

(* Induction on lists built by appending elements one by one (xs ++ [x]), also called snoc xs x, the opposite of cons x xs.
   Given a prop that accepts a list of A (P : list A -> Prop),
   If the prop is satisfied for the empty list (P [])
   And appending any single A to any list of A preserves the prop (forall x xs, P xs -> P (xs ++ [x])),
   Then the prop holds for any list of A. *)
Lemma snoc_list_ind :
  forall {A : Set} (P : list A -> Prop),
    P [] -> (forall x xs, P xs -> P (xs ++ [x])) ->
    forall xs, P xs.
Proof.
  intros A' P Pnil Psnoc xs.
  enough (P (rev (rev xs))). rewrite rev_involutive in H. assumption.
  apply rev_list_ind.
  - assumption.
  - enough (forall a l, P (rev (rev l)) -> P (rev (a :: rev l))).
    { intros a l Prl. specialize (H a (rev l)).
      rewrite rev_involutive in H. specialize (H Prl).
      rewrite rev_involutive in H. assumption. }
    intros a l Pl. rewrite rev_involutive in Pl.
    simpl. rewrite rev_involutive.
    apply Psnoc; assumption.
Qed.


(* The length of (xs ++ [x]) a.k.a. (snoc xs x), the opposite of (cons x xs),
   is equal to 1 + (length xs) *)
Lemma length_snoc {A : Type} {xs : list A} {x : A} :
  length (xs ++ [x]) = S (length xs).
Proof. induction xs; simpl; auto. Qed.
