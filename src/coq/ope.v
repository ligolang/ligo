From Coq Require Import List Lia.
From Coq Require Arith.Peano_dec.
Import ListNotations.
Unset Lia Cache.

(* Definitions and lemmas related to order-preserving embeddings
   (OPEs).

   The central concept below is slice categories of categories of
   OPEs.

   See for example "Everybody's Got To Be Somewhere", Conor McBride:
   https://arxiv.org/abs/1807.04085

   An order-preserving embedding from a list [xs] into a list [ys] is
   a witness that [xs] is a (not necessarily continiguous) subsequence
   of [ys]. There can be more than one witness, for example [x] embeds
   into [x; x] in two different ways.

   Here we will work with an extrinsically-typed version of these
   concepts. In particular, we represent objects in the slice category
   of OPEs as lists of booleans: *)

Notation ope := (list bool).

(* Each element of the list says whether to keep (true) or drop
   (false) the corresponding element of the codomain list [ys]. We can
   thus compute the selected list [xs]: *)

Fixpoint select {A : Set} (r : ope) (ys : list A) : list A :=
  match r with
  | [] => []
  | u :: r =>
      match ys with
      | [] => []
      | y :: ys =>
          if u
          then y :: select r ys
          else select r ys
      end
  end.

(* This representation is somewhat improper; a given [list bool] may
   contain more or fewer elements than the list [ys]. Assuming an
   additional length restriction, we could define an isomorphism:

   {r : list bool | length r = length ys} ~= slice category over ys

   Instead, I will treat a too-short list as if it were padded with
   [false] on the right. I will usually simply ignore the possibility
   that the OPE is too long. In some specific cases we will need to
   rule this out, which motivates proving the "length" lemmas below
   (comp_length etc.) *)


(* Now, we define various operations and lemmas on the slice
   category. *)

(* Selecting from nothing gives nothing. *)
Lemma select_nothing {A : Set} (r : ope) : @select A r [] = [].
Proof. induction r; auto. Qed.

(* Select distributes over list concatenation *)
Lemma select_app {A : Set} (r1 r2 : ope) (xs ys : list A) :
  length r1 = length xs ->
  select (r1 ++ r2) (xs ++ ys) = (select r1 xs ++ select r2 ys)%list.
Proof.
  revert r2 xs ys;
    induction r1;
    intros;
    destruct xs;
    try discriminate H;
    try destruct a;
    simpl;
    try rewrite IHr1;
    auto.
Qed.

(* (repeat true) selects everything *)
Lemma select_repeat_true {A : Set} (xs : list A) :
  select (repeat true (length xs)) xs = xs.
Proof. induction xs; simpl; try rewrite IHxs; auto. Qed.

Lemma select_repeat_true' {A : Set} (xs : list A) {n} :
  n >= length xs ->
  select (repeat true n) xs = xs.
Proof.
  revert n; induction xs; intros n H1; simpl.
  - rewrite select_nothing; reflexivity.
  - simpl in H1; destruct n; try solve [inversion H1]; simpl; apply f_equal, IHxs; lia.
Qed.

(* (repeat false) selects nothing *)
Lemma select_repeat_false {A : Set} (xs : list A) {n} :
  select (repeat false n) xs = [].
Proof.
  revert n; induction xs; intros n; simpl.
  - rewrite select_nothing; reflexivity.
  - destruct n; try reflexivity; eapply IHxs.
Qed.

(* Composition, in some sense. See lemma [select_comp] below. *)
Fixpoint comp (us vs : ope) : ope :=
  match vs with
  | [] => []
  | v :: vs =>
      if v
      then
        match us with
        | [] => []
        | u :: us => u :: comp us vs
        end
      else false :: comp us vs
  end.

Lemma comp_length (us vs : ope) :
  length (comp us vs) <= length vs.
Proof.
  revert us; induction vs as [|v vs];
    intros us;
    simpl in *;
    try lia;
    destruct v;
    destruct us;
    simpl in *;
    try lia;
    try (eapply le_n_S, IHvs; lia).
Qed.

(* Selection of composition is composition of selections: *)
Lemma select_comp {A : Set} (us vs : ope) (xs : list A) :
  select (comp us vs) xs = select us (select vs xs).
Proof.
  revert us xs;
    induction vs as [|v vs];
    intros [|u us] [|x xs];
    try destruct u;
    try destruct v;
    try reflexivity;
    solve [simpl; rewrite IHvs; auto].
Qed.

(* The coproduct in the slice category, as described by
   McBride. [union us vs] selects everything selected by _either_ [us]
   or [vs]. See the lemmas [select_inj1] and [select_inj2] below. *)
Fixpoint union (us vs : ope) : ope :=
  match (us, vs) with
  | ([], vs) => vs
  | (us, []) => us
  | (u :: us, v :: vs) => orb u v :: union us vs
  end.

Lemma union_nil_r (us : ope) : union us [] = us.
Proof. induction us; auto; simpl; rewrite IHus; auto. Qed.

Lemma union_nil_l (us : ope) : union [] us = us.
Proof. reflexivity. Qed.

Lemma union_length (us vs : ope) : length (union us vs) = max (length us) (length vs).
Proof.
  revert vs; induction us as [|u us]; intros [|v vs]; simpl;
    try reflexivity;
    eapply f_equal, IHus.
Qed.

(* The "injections" into the coproduct. [inj1 us vs] selects the
   'left' part of the union. See also the lemmas [select_inj1] and
   [select_inj2] below. *)
Fixpoint inj1 (us vs : ope) : ope :=
  match (us, vs) with
  | (us, []) => filter id us
  | ([], vs) => repeat false (length (filter id vs))
  | (u :: us, v :: vs) =>
      if orb u v
      then u :: inj1 us vs
      else inj1 us vs
  end.

(* [inj2] selects the 'right' part of the union. *)
Fixpoint inj2 (us vs : ope) : ope :=
  match (us, vs) with
  | ([], vs) => filter id vs
  | (us, []) => repeat false (length (filter id us))
  | (u :: us, v :: vs) =>
      if orb u v
      then v :: inj2 us vs
      else inj2 us vs
  end.

(* Side lemma *)
Lemma select_filter_id {A : Set} (us : ope) (xs : list A) :
  select (filter id us) (select us xs) = select us xs.
Proof.
  revert xs;
    induction us as [|u us];
    intros [|x xs];
    try destruct u;
    simpl;
    repeat rewrite select_nothing;
    auto;
    rewrite IHus; auto.
Qed.

Lemma select_inj1 {A : Set} (us vs : ope) (xs : list A) :
  select (inj1 us vs) (select (union us vs) xs) = select us xs.
Proof.
  revert vs xs;
    induction us as [|u us];
    intros [|v vs] [|x xs];
    try destruct u;
    try destruct v;
    repeat rewrite select_nothing; simpl;
    repeat rewrite select_filter_id; simpl;
    repeat rewrite select_repeat_false; simpl;
    try rewrite IHus;
    auto;
    destruct (select vs xs);
    try reflexivity;
    rewrite select_repeat_false;
    reflexivity.
Qed.
  
Lemma select_inj2 {A : Set} (us vs : ope) (xs : list A) :
  select (inj2 us vs) (select (union us vs) xs) = select vs xs.
Proof.
  revert vs xs;
    induction us as [|u us];
    intros [|v vs] [|x xs];
    try destruct u;
    try destruct v;
    repeat rewrite select_nothing; simpl;
    repeat rewrite select_filter_id; simpl;
    repeat rewrite select_repeat_false; simpl;
    try rewrite IHus;
    auto;
    destruct (select us xs);
    try reflexivity;
    rewrite select_repeat_false;
    reflexivity.
Qed.

Lemma inj1_length (us vs : ope) :
  length (inj1 us vs) = length (filter id (union us vs)).
Proof.
  revert vs; induction us as [|[|] us];
    intros [|[|] vs];
    simpl;
    try reflexivity;
    try specialize (IHus vs);
    try lia;
    try rewrite repeat_length;
    reflexivity.
Qed.

Lemma inj2_length (us vs : ope) :
  length (inj2 us vs) = length (filter id (union us vs)).
Proof.
  revert vs; induction us as [|[|] us];
    intros [|[|] vs];
    simpl;
    try reflexivity;
    try specialize (IHus vs);
    try lia;
    try rewrite repeat_length;
    reflexivity.
Qed.

(* [only] selects one element by index *)
Fixpoint only (n : nat) : ope :=
  match n with
  | O => [true]
  | S n => false :: only n
  end.

Lemma select_only {A : Set} (n : nat) (xs : list A) :
  select (only n) xs = match List.nth_error xs n with None => [] | Some x => [x] end.
Proof. revert xs; induction n; destruct xs; simpl; auto. Qed.

(* right inverse to [only]: first selected index, or 0. *)
Fixpoint one (us : ope) : nat :=
  match us with
  | [] => O
  | u :: us =>
    if u
    then O
    else S (one us)
  end.

Lemma one_only (n : nat) : one (only n) = n.
Proof. induction n; simpl; auto. Qed.

Lemma nth_error_one {A : Set} (us : ope) (x : A) (xs : list A) :
  select us xs = [x] ->
  nth_error xs (one us) = Some x.
Proof.
  revert x xs;
    induction us as [|u us];
    intros;
    destruct xs as [|x' xs]; simpl in *;
      try discriminate H;
      destruct u; simpl in *;
        try (inversion H; subst; reflexivity);
        apply IHus; auto.
Qed.

(* Action of OPEs on de Bruijn indices *)
Fixpoint embed (r : ope) (n : nat) : nat :=
  match r with
  | [] => O
  | false :: r => S (embed r n)
  | true :: r =>
      match n with
      | O => O
      | S n => S (embed r n)
      end
  end.

Lemma nth_error_embed {A : Set} (r : ope) (ys : list A) (n : nat) :
  nth_error (select r ys) n <> None ->
  nth_error ys (embed r n) = nth_error (select r ys) n.
Proof.
  revert ys n;
    induction r; intros; simpl.
  - destruct ys;
      exfalso; contradiction H; destruct n; auto.
  - destruct a; destruct ys;
      try solve [exfalso; contradiction H; destruct n; auto].
    + destruct n; try reflexivity; eapply IHr; auto.
    + eapply IHr; auto.
Qed.


(* like firstn, but pads with false so that the result has at least n elements *)
Fixpoint split_left (n : nat) (r : ope) : ope :=
  match n with
  | O => []
  | S n =>
      match r with
      | [] => repeat false (S n)
      | u :: r =>
          u :: split_left n r
      end
  end.

Definition split_right := @skipn bool.

Lemma split_left_length {n r} :
  length (split_left n r) = n.
Proof.
  revert r;
    induction n;
    intros r;
    simpl; try reflexivity.
  - destruct r; simpl.
    + rewrite repeat_length; reflexivity.
    + rewrite IHn; reflexivity.
Qed.

Lemma split_right_length {n r} :
  length (split_right n r) = length r - n.
Proof.
  unfold split_right; apply skipn_length.
Qed.

Lemma split_left_nil {n} :
  split_left n [] = repeat false n.
Proof. destruct n; eauto. Qed.

Lemma split_right_nil {n} :
  split_right n [] = [].
Proof. destruct n; eauto. Qed.

(* "Weight" of an OPE: number of elements selected. *)
Definition weight (r : ope) : nat := length (filter id r).

Lemma select_weight {A : Set} {r : ope} {xs : list A} :
  length r <= length xs ->
  length (select r xs) = weight r.
Proof.
  revert xs; induction r as [|u r]; intros xs H1;
    unfold weight; simpl; try reflexivity;
    destruct xs as [|x xs];
    destruct u; simpl; simpl in H1.
  - exfalso; lia.
  - exfalso; lia.
  - apply f_equal, IHr; lia.
  - apply IHr; lia.
Qed.

Lemma select_length_le_weight {A : Set} {r : ope} {xs : list A} :
  length (select r xs) <= weight r.
Proof.
  revert xs; induction r as [|[] r]; intros [|x xs];
    unfold weight; simpl; try reflexivity; try lia;
    try apply le_n_S;
    apply IHr.
Qed.

Lemma select_length_le_length {A : Set} {r : ope} {xs : list A} :
  length (select r xs) <= length xs.
Proof.
  revert xs; induction r as [|[] r]; intros [|x xs];
    unfold weight; simpl; try reflexivity; try lia;
    first [apply le_n_S | apply le_S];
    apply IHr.
Qed.

Lemma select_split {A : Set} {r : ope} {xs ys : list A} :
  select r (xs ++ ys) =
    select (split_left (length xs) r) xs ++ select (split_right (length xs) r) ys.
Proof.
  revert xs ys;
    induction r as [|u r];
    intros xs ys.
  - rewrite split_left_nil, select_repeat_false, split_right_nil; reflexivity.
  - destruct u; destruct xs as [|x xs];
      try reflexivity;
      try (simpl; try apply f_equal; apply IHr).
Qed.

Lemma select_split' {A : Set} {r : ope} {xs : list A} {n : nat} :
  select (split_left n r ++ split_right n r) xs = select r xs.
Proof.
  revert r n; induction xs; intros r n.
  - repeat rewrite select_nothing; auto.
  - destruct n; try reflexivity;
      destruct r as [|[|] r];
      simpl.
    + rewrite app_nil_r, select_repeat_false; auto.
    + apply f_equal, IHxs.
    + apply IHxs.
Qed.

Definition ope_hd (r : ope) : bool :=
  match r with
  | [] => false
  | u :: r => u
  end.



(* Definitions and lemmas about equivalence of OPEs *)

Inductive null_ope : ope -> Prop :=
| Null_ope_nil :
  null_ope []
| Null_ope_drop {r} :
  null_ope r ->
  null_ope (false :: r)
.

Lemma null_ope_iff_select {r} :
  null_ope r <->
  (forall {A : Set} {xs : list A}, select r xs = []).
Proof.
  split.
  - intros H; induction H; intros;
      try reflexivity;
      destruct xs as [|x xs];
      try reflexivity;
      eapply IHnull_ope.
  - intros H; induction r as [|[|] r].
    + constructor.
    + discriminate (H unit [tt]).
    + constructor;
        eapply IHr;
        intros;
        destruct xs as [|x xs].
      * apply select_nothing.
      * exact (H _ (x :: x :: xs)).
Qed.

Inductive equiv_ope : ope -> ope -> Prop :=
| Equiv_ope_nil :
  equiv_ope [] []
| Equiv_ope_drop_nil {r1} :
  equiv_ope r1 [] ->
  equiv_ope (false :: r1) []
| Equiv_ope_nil_drop {r2} :
  equiv_ope [] r2 ->
  equiv_ope [] (false :: r2)
| Equiv_ope_drop {r1 r2} :
  equiv_ope r1 r2 ->
  equiv_ope (false :: r1) (false :: r2)
| Equiv_ope_keep {r1 r2} :
  equiv_ope r1 r2 ->
  equiv_ope (true :: r1) (true :: r2)
.

Definition ext_equiv_ope (r1 r2 : ope) : Prop :=
  forall (A : Set) (xs : list A), select r1 xs = select r2 xs.

Lemma equiv_ope_iff_select {r1 r2} :
  equiv_ope r1 r2 <-> ext_equiv_ope r1 r2.
Proof.
  unfold ext_equiv_ope;
    split.
  - intros H; induction H; intros;
      try destruct xs;
      try reflexivity;
      simpl;
      try eapply IHequiv_ope;
      rewrite IHequiv_ope;
      reflexivity.
  - revert r2;
      induction r1 as [|u1 r1];
      intros r2;
      induction r2 as [|u2 r2];
      intros H1.
    + constructor.
    + destruct u2.
      * discriminate (H1 unit [tt]).
      * constructor; eapply IHr2; intros;
          destruct xs as [|x xs];
          try (simpl; rewrite select_nothing; reflexivity);
          exact (H1 _ (x :: x :: xs)).
    + destruct u1.
      * discriminate (H1 unit [tt]).
      * constructor; eapply IHr1; intros;
          destruct xs as [|x xs];
          try (simpl; rewrite select_nothing; reflexivity);
          exact (H1 _ (x :: x :: xs)).
    + destruct u1, u2.
      * { constructor; eapply IHr1; intros;
            destruct xs as [|x xs].
          - repeat rewrite select_nothing; reflexivity.
          - specialize (H1 _ (x :: x :: xs));
              exact (f_equal (@tl _) H1). }
      * specialize (H1 unit [tt]);
          simpl in H1;
          repeat rewrite select_nothing in H1;
          discriminate H1.
      * specialize (H1 unit [tt]);
          simpl in H1;
          repeat rewrite select_nothing in H1;
          discriminate H1.
      * { constructor; eapply IHr1; intros;
            destruct xs as [|x xs].
          - repeat rewrite select_nothing; reflexivity.
          - exact (H1 _ (x :: x :: xs)). }
Qed.

Definition equiv_ope_refl {r} :
  equiv_ope r r.
Proof. induction r as [|[|] r]; constructor; auto. Defined.

Definition equiv_ope_sym {r1 r2} :
  equiv_ope r1 r2 ->
  equiv_ope r2 r1.
Proof. intros H; induction H; constructor; auto. Defined.
Lemma null_ope_iff_equiv_ope_nil_r {r} : null_ope r <-> equiv_ope r [].
Proof.
  split; rewrite null_ope_iff_select, equiv_ope_iff_select; auto.
Qed.

Lemma null_ope_iff_equiv_ope_nil_l {r} : null_ope r <-> equiv_ope [] r.
Proof.
  split; rewrite null_ope_iff_select, equiv_ope_iff_select; unfold ext_equiv_ope; simpl; auto.
Qed.

Fixpoint bnull_ope (r : list bool) : bool :=
  match r with
  | [] => true
  | u :: r => if u then false else (bnull_ope r)
  end.

Lemma reflect_bnull_ope {r} : Bool.reflect (null_ope r) (bnull_ope r).
Proof.
  induction r as [|[|] r].
  - repeat constructor.
  - constructor; intros H; inversion H.
  - simpl; destruct IHr.
    + repeat constructor; auto.
    + repeat constructor; intros H; inversion H; auto.
Qed.

Fixpoint bequiv_ope (r1 r2 : list bool) {struct r1} : bool :=
  match (r1, r2) with
  | ([], r2) => bnull_ope r2
  | (r1, []) => bnull_ope r1
  | (u1 :: r1, u2 :: r2) =>
      andb (Bool.eqb u1 u2) (bequiv_ope r1 r2)
  end.

Lemma bequiv_ope_nil_r (r : ope) : bequiv_ope r [] = bnull_ope r.
Proof. induction r as [|[|] r]; eauto. Qed.

Lemma bnull_ope_sound {r} :
  Bool.Is_true (bnull_ope r) -> null_ope r.
Proof.
  induction r as [|[] r];
    simpl;
    intros H;
    try contradiction;
    constructor;
    auto.
Defined.

Lemma bnull_ope_complete {r} :
  null_ope r -> Bool.Is_true (bnull_ope r).
Proof. intros H; induction H; simpl; auto. Defined.

Lemma null_ope_implies_equiv_ope_r' {r1 r2} :
  null_ope r2 -> r1 = [] -> equiv_ope r1 r2.
Proof.
  intros H1; revert r1; induction H1; intros r1 H2;
    rewrite H2; constructor;
    auto.
Defined.

Lemma equiv_ope_r_implies_null_ope' {r1 r2} :
  equiv_ope r1 r2 -> r1 = [] -> null_ope r2.
Proof.
  intros H1; induction H1; intros H2;
    try constructor;
    try discriminate H2;
    auto.
Defined.

Lemma equiv_ope_l_implies_null_ope' {r1 r2} :
  equiv_ope r1 r2 -> r2 = [] -> null_ope r1.
Proof.
  intros H1; induction H1; intros H2;
    try constructor;
    try discriminate H2;
    auto.
Defined.

Lemma null_ope_implies_equiv_ope_l' {r1 r2} :
  null_ope r1 -> r2 = [] -> equiv_ope r1 r2.
Proof.
  intros H1; revert r2;
    induction H1; intros r2 H2;
    rewrite H2; constructor;
    auto.
Defined.

Lemma null_ope_implies_equiv_ope_r {r} :
  null_ope r -> equiv_ope [] r.
Proof. intros H; eapply null_ope_implies_equiv_ope_r'; auto. Defined.

Lemma equiv_ope_r_implies_null_ope {r} :
  equiv_ope [] r -> null_ope r.
Proof. intros  H; eapply equiv_ope_r_implies_null_ope'; eauto. Defined.

Lemma null_ope_implies_equiv_ope_l {r} :
  null_ope r -> equiv_ope r [].
Proof. intros H; eapply null_ope_implies_equiv_ope_l'; auto. Defined.

Lemma equiv_ope_l_implies_null_ope {r} :
  equiv_ope r [] -> null_ope r.
Proof. intros H; eapply equiv_ope_l_implies_null_ope'; eauto. Defined.

Lemma bequiv_ope_sound {r1 r2} :
  Bool.Is_true (bequiv_ope r1 r2) -> equiv_ope r1 r2.
Proof.
  revert r2;
    induction r1 as [|[] r1];
    intros r2;
    simpl; intros H;
    try contradiction.
  - apply null_ope_implies_equiv_ope_r, bnull_ope_sound;
      assumption.
  - destruct r2 as [|[] r2];
      simpl in H;
      try contradiction;
      constructor;
      auto.
  - destruct r2 as [|[] r2];
      simpl in H;
      try contradiction;
      constructor;
      auto;
      eapply null_ope_implies_equiv_ope_l, bnull_ope_sound;
      auto.
Defined.

Lemma bequiv_ope_complete {r1 r2} :
  equiv_ope r1 r2 -> Bool.Is_true (bequiv_ope r1 r2).
Proof.
  intros H; induction H; simpl in *;
    try trivial;
    eapply bnull_ope_complete, equiv_ope_l_implies_null_ope; auto.
Defined.

Lemma is_true_iff {b : bool} : Bool.Is_true b <-> b = true.
Proof.
  split.
  - destruct b; intros H; try contradiction H; reflexivity.
  - intros; subst; constructor.
Defined.

Lemma reflect_bequiv_ope {r1 r2} : Bool.reflect (equiv_ope r1 r2) (bequiv_ope r1 r2).
Proof.
  eapply Bool.iff_reflect; split; intros H.
  - eapply bequiv_ope_complete in H;
      eapply is_true_iff;
      auto.
  - eapply is_true_iff in H;
      eapply bequiv_ope_sound;
      auto.
Defined.

Fixpoint trim (r : ope) (n : nat) : ope :=
  match n with
  | O => []
  | S n =>
      match r with
      | [] => []
      | u :: r =>
          u :: trim r (if u then n else S n)
      end
  end.

Lemma trim_zero {r : ope} : trim r O = [].
Proof. destruct r; reflexivity. Qed.

Lemma trim_ok {A : Set} {r : ope} {n : nat} {xs : list A} :
  n >= length (select r xs) ->
  select (trim r n) xs = select r xs.
Proof.
  revert n xs; induction r as [|[] r]; intros [|n] [|x xs] H1;
    try reflexivity;
    simpl in *;
    try (exfalso; lia).
  - apply f_equal, IHr; lia.
  - destruct (select r xs); try reflexivity; inversion H1.
  (* whaat *)
  - destruct r as [|[] r]; try reflexivity;
      destruct xs as [|x2 xs];
      simpl in *;
      try reflexivity.
    + eapply f_equal; specialize (IHr (S n) (x2 :: xs) H1); simpl in IHr; inversion IHr; reflexivity.
    + specialize (IHr (S n) (x2 :: xs)); simpl in IHr; eapply IHr; eauto.
Qed.

Lemma trim_weight {r : ope} {n : nat} :
  weight (trim r n) <= n.
Proof.
  revert n; induction r as [|[] r]; intros [|n];
    try reflexivity;
    unfold weight;
    simpl;
    try lia;
    try apply le_n_S;
    try apply IHr.
Qed.

Fixpoint normalize (r : ope) : ope :=
  match r with
  | [] => []
  | false :: r =>
      match normalize r with
      | [] => []
      | r' => false :: r'
      end
  | true :: r =>
      true :: normalize r
  end.

Lemma normalize_ok (r : ope) : ext_equiv_ope r (normalize r).
Proof.
  unfold ext_equiv_ope;
    induction r as [|[] r];
    intros A xs;
    destruct xs as [|x xs];
    simpl;
    try reflexivity;
    try apply f_equal;
    try apply IHr;
    try (rewrite select_nothing; reflexivity);
    destruct (normalize r);
    rewrite IHr; reflexivity.
Qed.

Definition ope_valid (r : ope) (n : nat) := length (normalize r) <= n.

Lemma length_normalize_trim {A : Set} (r : ope) (xs : list A) :
  length (normalize (trim r (length (select r xs)))) <= length xs.
Proof.
  revert xs; induction r as [|[] r]; intros [|x xs];
    simpl; try lia.
  - eapply le_n_S; auto.
  - specialize (IHr xs);
      destruct (select r xs) eqn:H;
      simpl in *; try lia;
      destruct (normalize (trim r (S (length l))));
      simpl in *; lia.
Qed.

Lemma trim_valid {A : Set} (r : ope) (xs : list A) :
  ope_valid (trim r (Datatypes.length (select r xs))) (Datatypes.length xs).
Proof.
  unfold ope_valid; (* rewrite width_ok; *) eapply length_normalize_trim.
Qed.

Lemma repeat_valid (n : nat) :
  ope_valid (repeat true n) n.
Proof.
  induction n; unfold ope_valid; (* rewrite width_ok; *) simpl;
    auto; eapply le_n_S; auto.
Qed.

(* Finding a fixed point of a function on OPEs by iterating the
   function, using the Braga method (see Dominique Larchey-Wendling
   and Jean François-Monin, 2020.)

   This is used to "strengthen" Michelson loops in [strengthen_instr]
   in compiler.v, doing a kind of data-flow fixed-point analysis.

   [fix_ope] will take a function [f] and starting point [x], and will
   iterate [f], taking [x, f x, f (f x)...] until a fixed point is
   found: some x' with [equiv_ope (f x') x'].

   First, the domain predicate [fix_ope_dom] picks out [f] and [x]
   where this process will terminate: *)
Inductive fix_ope_dom (f : ope -> ope) (x : ope) : Prop :=
| Fix_ope_done :
  equiv_ope (f x) x ->
  fix_ope_dom f x
| Fix_ope_step :
  fix_ope_dom f (f x) ->
  fix_ope_dom f x
.

Scheme fix_ope_dom_ind' := Induction for fix_ope_dom Sort Prop.

(* Helper for when we found a fixed point: *)
Lemma fix_ope_done {f x} : bequiv_ope (f x) x = true -> {y : ope | equiv_ope (f y) y}.
Proof.
  intros H1;
    exists x;
    rewrite <- (Bool.reflect_iff _ _ (@reflect_bequiv_ope (f x) x)) in H1;
    exact H1.
Defined.

(* Inversion lemma as per Braga method: *)
Lemma fix_ope_step_inv {f x} :
  bequiv_ope (f x) x = false ->
  fix_ope_dom f x ->
  fix_ope_dom f (f x).
Proof.
  intros H1 H2; inversion H2.
  - pose proof (@reflect_bequiv_ope (f x) x) as H3;
      rewrite H1 in H3;
      inversion H3;
      contradiction.
  - assumption.
Defined.

(* Now the Braga magic: Coq believes [fix_ope] is structurally
   recursive on the domain predicate *)
Fixpoint fix_ope (f : ope -> ope) (x : ope) (D : fix_ope_dom f x) {struct D} :
  {y : ope | equiv_ope (f y) y} :=
  (if bequiv_ope (f x) x as b return (bequiv_ope (f x) x = b -> {y : ope | equiv_ope (f y) y})
   then fix_ope_done
   else fun Hbeq => fix_ope f (f x) (fix_ope_step_inv Hbeq D))
    eq_refl.

(* Copy of fix_ope generalized over [bequiv_ope (f x) x] with an
   equation, to help below. *)
Definition fix_ope' (f : ope -> ope) (x : ope) (D : fix_ope_dom f x) (b : bool) (H : bequiv_ope (f x) x = b) :
  {y : ope | equiv_ope (f y) y} :=
  (if b return (bequiv_ope (f x) x = b -> {y : ope | equiv_ope (f y) y})
   then fix_ope_done
   else fun Hbeq => fix_ope f (f x) (fix_ope_step_inv Hbeq D))
    H.

(* If the function [f] preserves some property of OPEs, then so does
   [fix_ope f]. This can be used to prove correctness for code emitted
   using a [fix_ope]: prove correctness using [x] and prove that
   correctness is preserved by [f], then conclude correctness using
   [fix_ope f x]. *)
Lemma fix_ope_preserves (P : ope -> Prop)
      {f : ope -> ope} {x : ope} {D : fix_ope_dom f x} :
  (forall x, P x -> P (f x)) -> P x ->
  P (proj1_sig (fix_ope f x D)).
Proof.
  induction D using fix_ope_dom_ind';
    intros H1 H2.
  - enough (G : forall b H, P (proj1_sig (fix_ope' f x (Fix_ope_done f x e) b H)))
      by (exact (G _ eq_refl));
      intros b H3;
      destruct b.
    + assumption.
    + exfalso;
        rewrite (Bool.reflect_iff _ _ (@reflect_bequiv_ope (f x) x)) in e;
        rewrite H3 in e;
        discriminate e.
  - enough (G : forall b H, P (proj1_sig (fix_ope' f x (Fix_ope_step f x D) b H)))
      by (exact (G _ eq_refl));
      intros b H3;
      destruct b.
    + assumption.
    + eapply IHD; eauto.
Qed.
