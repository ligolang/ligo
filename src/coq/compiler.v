Local Set Warnings "-implicit-core-hint-db".
Set Implicit Arguments.
From Coq Require Import String List Arith ZArith Program.Tactics micromega.Lia micromega.Zify.
From Coq Require Extraction.
Import ListNotations.
Open Scope string_scope.

From ligo_coq Require Import ope.

(* http://poleiro.info/posts/2018-10-15-checking-for-constructors.html *)
Ltac head t :=
  (* slightly modified, we will evaluate to get a head *)
  let t := eval hnf in t in
  match t with
  | ?t' _ => head t'
  | _ => t
  end.

Ltac head_constructor t :=
  let t' := head t in is_constructor t'.

Ltac invert H := inversion H; subst; clear H.

Ltac invert_pred p :=
  match goal with
  | [H : p |- _] => invert H
  | [H : p _ |- _] => invert H
  | [H : p _ _ |- _] => invert H
  | [H : p _ _ _ |- _] => invert H
  | [H : p _ _ _ _ |- _] => invert H
  | [H : p _ _ _ _ _ |- _] => invert H
  | [H : p _ _ _ _ _ _ |- _] => invert H
  | [H : p _ _ _ _ _ _ _ |- _] => invert H
  | [H : p _ _ _ _ _ _ _ _ |- _] => invert H
  | [H : p _ _ _ _ _ _ _ _ _ |- _] => invert H
  | [H : p _ _ _ _ _ _ _ _ _ _ |- _] => invert H
  | [H : p _ _ _ _ _ _ _ _ _ _ _ |- _] => invert H
  | [H : p _ _ _ _ _ _ _ _ _ _ _ _ |- _] => invert H
  | [H : p _ _ _ _ _ _ _ _ _ _ _ _ _ |- _] => invert H
  | [H : p _ _ _ _ _ _ _ _ _ _ _ _ _ _ |- _] => invert H
  | [H : p _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ |- _] => invert H
  | [H : p _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ |- _] => invert H
  | [H : p _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ |- _] => invert H
  | [H : p _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ |- _] => invert H
  | [H : p _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ |- _] => invert H
  | [H : p _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ |- _] => invert H
  end.

Definition done (T : Type) : Type := T.

Ltac mark_done H :=
  let A := type of H in
  change (done A) in H.

Ltac clear_done :=
  repeat
    match goal with
    | [H : done ?A |- _] =>
      change A in H
    end.

Axiom bytes : Set.
Extract Inlined Constant bytes => "bytes".

(* I am sorry this is one big section. It seemed like the easiest,
   stupidest way to deal with the Context assumptions below. If you
   find a way to break this up into multiple files, please do it.

   Maybe most of the Context should instead be Axioms with custom
   Extraction? *)
Section compiler.

Local Open Scope list.

Context {meta : Set}. (* metadata for expressions (source location, etc) *)
Context {null : meta}. (* null metadata *)
Context {base_type : Set}. (* arbitrary base types (probably Micheline) *)

(* Types: essentially Michelson's types, shared by source and target
   stages here *)
Inductive ty : Set :=
| T_base : meta -> base_type -> ty

| T_unit : meta -> ty
| T_pair : meta -> option string -> option string -> ty -> ty -> ty

| T_or : meta -> option string -> option string -> ty -> ty -> ty

| T_func : meta -> ty -> ty -> ty
| T_lambda : meta -> ty -> ty -> ty

| T_option : meta -> ty -> ty
| T_list : meta -> ty -> ty
| T_set : meta -> ty -> ty
| T_map : meta -> ty -> ty -> ty
| T_big_map : meta -> ty -> ty -> ty
| T_ticket : meta -> ty -> ty
| T_contract : meta -> ty -> ty
(* TODO delete the types we don't need *)
| T_bool : meta -> ty
| T_int : meta -> ty
| T_nat : meta -> ty
| T_mutez : meta -> ty
| T_string : meta -> ty
| T_bytes : meta -> ty
(* these only exist for CREATE_CONTRACT *)
| T_address : meta -> ty
| T_key_hash : meta -> ty
| T_operation : meta -> ty
.

Context {with_var_names : list ty -> meta -> meta}.
Context {lit micheline : Set}.
Context {lit_code : meta -> lit -> list micheline}.
Context {global_constant : meta -> string -> list micheline}.


(* Source language: de Bruijn LIGO expressions. For comments about
   specific constructors, see the typing derivations [expr_typed]
   below.

   The expressions are currently given as three mutual inductive
   types: [expr] for expressions, [binds] for binding forms in
   expressions (lambda bodies, match cases, etc,) and [args] for
   tuples of expressions.

   [args] is isomorphic to [list expr], but is defined mutually in
   order to avoid nested induction. Compare this to the different
   choice adopted for Michelson below, where [prog] _is_ just [list
   instr], and so nested induction is required to define
   [strengthen_instr]. *)
Inductive expr : Set :=
| E_var : meta -> nat -> expr
| E_let_in : meta -> expr -> binds -> expr

| E_tuple : meta -> args -> expr
| E_let_tuple : meta -> expr -> binds -> expr
| E_proj : meta -> expr -> nat -> nat -> expr
| E_update : meta -> args -> nat -> nat -> expr

| E_app : meta -> args -> expr
| E_lam : meta -> binds -> ty -> expr
| E_rec : meta -> binds -> ty -> expr

| E_literal : meta -> lit -> expr

| E_pair : meta -> args -> expr
| E_car : meta -> expr -> expr
| E_cdr : meta -> expr -> expr

| E_unit : meta -> expr

| E_left : meta -> ty -> expr -> expr
| E_right : meta -> ty -> expr -> expr

| E_if_left : meta -> expr -> binds -> binds -> expr
| E_if_bool : meta -> expr -> expr -> expr -> expr
| E_if_none : meta -> expr -> expr -> binds -> expr
| E_if_cons : meta -> expr -> binds -> expr -> expr

| E_iter : meta -> binds -> expr -> expr
| E_map : meta -> binds -> expr -> expr
| E_loop_left : meta -> binds -> ty -> expr -> expr
| E_fold : meta -> expr -> expr -> binds -> expr
| E_fold_right : meta -> ty -> expr -> expr -> binds -> expr

(* TODO add typing rules for these ...maybe ;) *)
| E_deref : meta -> nat -> expr
| E_let_mut_in : meta -> expr -> binds -> expr
| E_assign : meta -> nat -> expr -> expr
| E_for : meta -> args -> binds -> expr
| E_for_each : meta -> expr -> binds -> expr
| E_while : meta -> expr -> expr -> expr

| E_failwith : meta -> expr -> expr

| E_raw_michelson : meta -> ty -> ty -> list micheline -> expr
| E_inline_michelson : meta -> list micheline -> args -> expr
| E_global_constant : meta -> ty -> string -> args -> expr
| E_create_contract : meta -> ty -> ty -> binds -> args -> expr

with binds : Set :=
| Binds : meta -> list ty -> expr -> binds

with args : Set :=
| Args_nil : meta -> args
| Args_cons : meta -> expr -> args -> args
.

Scheme expr_ind' := Induction for expr Sort Prop
with binds_ind' := Induction for binds Sort Prop
with args_ind' := Induction for args Sort Prop.
Combined Scheme expr_mutind from expr_ind', binds_ind', args_ind'.

Fixpoint args_length (e : args) : nat :=
  match e with
  | Args_nil _ => O
  | Args_cons _ _ e => S (args_length e)
  end.

Definition binds_length (e : binds) : nat :=
  match e with
  | Binds _ ts _ => length ts
  end.

Local Generalizable Variable l n.

Inductive tuple : list ty -> ty -> Prop :=
| Tuple_nil :
  `{tuple [] (T_unit l)}
| Tuple_one {a} :
  tuple [a] a
| Tuple_cons {a1 a2 az a2z'} :
  `{tuple (a2 :: az) a2z' ->
    tuple (a1 :: a2 :: az) (T_pair l n1 n2 a1 a2z')}
.

Inductive iter_class : ty -> ty -> Prop :=
| Iter_list {a} :
  `{iter_class a (T_list l a)}
| Iter_set {a} :
  `{iter_class a (T_set l a)}
| Iter_map {k v} :
  `{iter_class (T_pair l1 n1 n2 k v) (T_map l2 k v)}
.

Inductive map_class : ty -> ty -> ty -> ty -> Prop :=
| Map_list {a b} :
  `{map_class a b (T_list l1 a) (T_list l2 b)}
| Map_map {k v r} :
  `{map_class (T_pair l1 n1 n2 k v) r (T_map l2 k v) (T_map l3 k r)}
.

Hint Constructors iter_class : michelson.
Hint Constructors map_class : michelson.

Generalizable Variable g. (* TODO *)

Inductive expr_typed : list ty -> expr -> ty -> Prop :=
| E_var_typed {a} :
    `{List.nth_error g n = Some a ->
      expr_typed g (E_var l n) a}
| E_let_in_typed {e1 e2 a b} :
    `{expr_typed g e1 a ->
      binds_typed g e2 [a] b ->
      expr_typed g (E_let_in l e1 e2) b}
| E_tuple_typed {args az t} :
    `{tuple az t ->
      args_typed g args az ->
      expr_typed g (E_tuple l1 args) t}
| E_let_tuple_typed {az azt c e1 e2} :
    `{tuple az azt ->
      expr_typed g e1 azt ->
      binds_typed g e2 az c ->
      expr_typed g (E_let_tuple l3 e1 e2) c}
| E_proj_typed {az azt a e i n} :
    `{tuple az azt ->
      List.nth_error az i = Some a ->
      List.length az = n ->
      expr_typed g e azt ->
      expr_typed g (E_proj l1 e i n) a}
| E_update_typed {a az azt args i n} :
    `{tuple az azt ->
      List.nth_error az i = Some a ->
      List.length az = n ->
      args_typed g args [azt; a] ->
      expr_typed g (E_update l1 args i n) azt}
| E_app_typed {args a b} :
    `{args_typed g args [T_func l1 a b; a] ->
      expr_typed g (E_app l2 args) b}
| E_lam_typed {a b e} :
    `{binds_typed g e [a] b ->
      expr_typed g (E_lam l1 e b)  (T_func l2 a b)}
| E_literal_typed {lit a} :
    `{(* TODO postulate some typing *)
      expr_typed g (E_literal l lit) a}
| E_pair_typed {args a b} :
    `{args_typed g args [a; b] ->
      expr_typed g (E_pair l1 args) (T_pair l2 n1 n2 a b)}
| E_car_typed {e a b} :
    `{expr_typed g e (T_pair l1 n1 n2 a b) ->
      expr_typed g (E_car l2 e) a}
| E_cdr_typed {e a b} :
    `{expr_typed g e (T_pair l1 n1 n2 a b) ->
      expr_typed g (E_cdr l2 e) b}
| E_unit_typed :
    `{expr_typed g (E_unit l1) (T_unit l2)}
| E_left_typed {e a b} :
    `{expr_typed g e a ->
      expr_typed g (E_left l1 b e) (T_or l2 n1 n2 a b)}
| E_right_typed {e a b} :
    `{expr_typed g e b ->
      expr_typed g (E_right l1 a e) (T_or l2 n1 n2 a b)}
| E_if_left_typed {e1 e2 e3 a b c} :
    `{expr_typed g e1 (T_or l1 n1 n2 a b) ->
      binds_typed g e2 [a] c ->
      binds_typed g e3 [b] c ->
      expr_typed g (E_if_left l2 e1 e2 e3) c}
| E_if_bool_typed {e1 e2 e3 c} :
    `{expr_typed g e1 (T_bool l1) ->
      expr_typed g e2 c ->
      expr_typed g e3 c ->
      expr_typed g (E_if_bool l2 e1 e2 e3) c}
| E_if_none_typed {e1 e2 e3 a c} :
    `{expr_typed g e1 (T_option l1 a) ->
      expr_typed g e2 c ->
      binds_typed g e3 [a] c ->
      expr_typed g (E_if_none l2 e1 e2 e3) c}
| E_if_cons_typed {e1 b2 e3 a c} :
    `{expr_typed g e1 (T_list l1 a) ->
      binds_typed g b2 [a; T_list l2 a] c ->
      expr_typed g e3 c ->
      expr_typed g (E_if_cons l3 e1 b2 e3) c}
| E_iter_typed {elem coll e1 e2} :
    `{iter_class elem coll ->
      binds_typed g e1 [elem] (T_unit l1) ->
      expr_typed g e2 coll ->
      expr_typed g (E_iter l2 e1 e2) (T_unit l3)}
| E_map_typed {elem elem' coll coll' e1 e2} :
    `{map_class elem elem' coll coll' ->
      binds_typed g e1 [elem] elem' ->
      expr_typed g e2 coll ->
      expr_typed g (E_map l e1 e2) coll'}
| E_loop_left_typed {a b e1 e2} :
    `{binds_typed g e1 [a] (T_or l1 n1 n2 a b) ->
      expr_typed g e2 a ->
      expr_typed g (E_loop_left l2 e1 b e2) b}
| E_fold_typed {elem coll ret e1 e2 e3} :
    `{iter_class elem coll ->
      expr_typed g e1 ret ->
      expr_typed g e2 coll ->
      binds_typed g e3 [T_pair l1 n1 n2 ret elem] ret ->
      expr_typed g (E_fold l2 e1 e2 e3) ret}
| E_fold_right_typed {elem coll ret e1 e2 e3} :
    `{iter_class elem coll ->
      expr_typed g e1 ret ->
      expr_typed g e2 coll ->
      binds_typed g e3 [T_pair l1 n1 n2 elem ret] ret ->
      expr_typed g (E_fold_right l2 elem e1 e2 e3) ret}
| E_failwith_typed {a b e} :
    `{expr_typed g e a ->
      expr_typed g (E_failwith l1 e) b}
(* E_raw_michelson emits a Michelson lambda directly. It is used for
   [%Michelson ({| ... |} : a -> b)] in LIGO. Probably we should
   delete it and migrate to E_inline_michelson. *)
| E_raw_michelson_typed {code a b} :
    `{(* TODO should postulate some typing *)
      expr_typed g (E_raw_michelson l1 a b code) (T_lambda l2 a b)}
(* E_inline_michelson inlines Michelson code directly, applied to some
   given arguments. It is currently only used for the "predefined
   constants" in predefined.ml, but could be exposed to users
   someday. TODO we should probably have a [ty] in the syntax for the
   return type [b], since it cannot be inferred? *)
| E_inline_michelson_typed {code args az b} :
    `{args_typed g args az ->
      (* TODO should postulate some typing *)
      expr_typed g (E_inline_michelson l1 code args) b}
(* E_global_constant is for Tezos "global constants". It is very
   similar to E_inline_michelson, but accepts the string hash of a
   Tezos "global constant" in place of the Michelson code. *)
| E_global_constant_typed {az b hash args} :
    `{args_typed g args az ->
      (* TODO should postulate some typing *)
      expr_typed g (E_global_constant l1 b hash args) b}
(* E_create_contract only exists here because it seemed easiest to put
   it here.

   It is not easy to handle in an earlier pass using
   E_inline_michelson, because that earlier pass would need access to
   the later passes of the compiler, in order to compile, optimize,
   and render the given contract into Micheline.

   A previous version of this Coq compiler had a more complicated
   interaction with the predefineds which made it possible to handle
   CREATE_CONTRACT using a predefined, but that was very confusing,
   and didn't work well with the current choices here. *)
| E_create_contract_typed {p s script args} :
    `{binds_typed [] script [T_pair l1 n1 n2 p s] (T_pair l2 n3 n4 (T_operation l3) s) ->
      args_typed g args [T_option l4 (T_key_hash l5); T_mutez l6; s] ->
      expr_typed g (E_create_contract l7 p s script args) (T_pair l8 n5 n6 (T_operation l9) (T_address l10))}
with binds_typed : list ty -> binds -> list ty -> ty -> Prop :=
| Binds_typed {a e ts} :
    `{expr_typed (ts ++ g) e a ->
      binds_typed g (Binds l ts e) ts a}
with args_typed : list ty -> args -> list ty -> Prop :=
| Args_nil_typed :
    `{args_typed g (Args_nil l) []}
| Args_cons_typed {e args d a} :
    `{expr_typed g e a ->
      args_typed g args d ->
      args_typed g (Args_cons l e args) (d ++ [a])}
.

Hint Constructors expr_typed : ligo.
Hint Constructors binds_typed : ligo.
Hint Constructors args_typed : ligo.



(*************
 * Michelson *
 *************)

Reserved Notation "'prog'".

Inductive instr : Set :=
| I_RAW : meta -> nat -> list micheline -> instr

| I_SEQ : meta -> prog -> instr
| I_DIP : meta -> prog -> instr
| I_DIG : meta -> nat -> instr
| I_DUG : meta -> nat -> instr
| I_DUP : meta -> nat -> instr
| I_DROP : meta -> nat -> instr
| I_SWAP : meta -> instr

| I_UNIT : meta -> instr
| I_TRUE : meta -> instr (* fictional version of PUSH bool True, for impl of E_while... *)

| I_LEFT : meta -> ty -> instr
| I_RIGHT : meta -> ty -> instr
| I_IF_LEFT : meta -> prog -> prog -> instr

| I_PAIR : meta -> nat -> instr
| I_UNPAIR : meta -> nat -> instr
| I_GET : meta -> nat -> instr
| I_UPDATE : meta -> nat -> instr
| I_CAR : meta -> instr
| I_CDR : meta -> instr

| I_IF : meta -> prog -> prog -> instr

| I_IF_NONE : meta -> prog -> prog -> instr

| I_NIL : meta -> ty -> instr
| I_CONS : meta -> instr
| I_IF_CONS : meta -> prog -> prog -> instr

| I_FUNC : meta -> list ty -> ty -> ty -> list bool -> list bool -> prog -> instr (* VERY FICTION *)
| I_REC_FUNC : meta -> list ty -> ty -> ty -> list bool -> list bool -> prog -> instr (* VERY FICTION *)
| I_LAMBDA : meta -> ty -> ty -> prog -> instr
| I_EXEC : meta -> instr (* func or lambda *)
| I_APPLY_LAMBDA : meta -> ty -> instr (* FICTION (APPLY but with a type arg) *)

| I_LOOP : meta -> prog -> instr
| I_LOOP_LEFT : meta -> prog -> instr

| I_FAILWITH : meta -> instr

| I_ITER : meta -> prog -> instr
| I_MAP : meta -> prog -> instr

(* convenient fictional instruction for implementing E_for (arithmetic
   progression loops) without exposing arithmetic here right now... *)
| I_FOR : meta -> prog -> instr

| I_CREATE_CONTRACT : meta -> ty -> ty -> prog -> instr
where
"'prog'" := (list instr).

Inductive packable : ty -> Prop :=
| Packable_unit : `{packable (T_unit l)}
| Packable_pair {a b} :
  `{packable a -> packable b ->
    packable (T_pair l n1 n2 a b)}
| Packable_or {a b} :
    `{packable a -> packable b ->
      packable (T_or l n1 n2 a b)}
(* T_func is not packable, T_lambda is: *)
| Packable_lambda {a b} :
  `{packable (T_lambda l a b)}
| Packable_int :
  `{packable (T_int l)}
| Packable_nat :
  `{packable (T_nat l)}
| Packable_mutez :
  `{packable (T_mutez l)}
| Packable_bool :
  `{packable (T_bool l)}
| Packable_bytes :
  `{packable (T_bytes l)}
(* TODO *)
.

Hint Constructors packable : michelson.

Inductive comparable : ty -> Prop :=
| Comparable_int : `{comparable (T_int l)}
| Comparable_nat : `{comparable (T_nat l)}
| Comparable_mutez : `{comparable (T_mutez l)}
(* TODO *)
.

Hint Constructors comparable : michelson.

(* Characterization of Michelson "comb types", which must have at
   least 2 "fields" *)
Inductive comb_ty : ty -> list ty -> Prop :=
| Comb_two {a b} :
    `{comb_ty (T_pair l n1 n2 a b) [a; b]}
| Comb_cons {c ts t} :
    `{comb_ty c ts ->
      comb_ty (T_pair l n1 n2 t c) (t :: ts)}
.

Hint Constructors comb_ty : michelson.

(* Characterization of comb [GET k] typing *)
Inductive comb_get_ty : ty -> nat -> ty -> Prop :=
| Comb_get_zero' {a} :
  comb_get_ty a O a
| Comb_get_one' {x y} :
  `{comb_get_ty (T_pair l1 n1 n2 x y) 1 x}
| Comb_get_plustwo {n x y z} :
  `{comb_get_ty y n z ->
    comb_get_ty (T_pair l1 n1 n2 x y) (S (S n)) z}
.

(* Characterization of comb [UPDATE k] typing *)
Inductive comb_update_ty : ty -> ty -> nat -> ty -> Prop :=
| Comb_update_zero {t x}:
  comb_update_ty t x O x
| Comb_update_one {a1 a2 b} :
  `{comb_update_ty (T_pair l1 n1 n2 a1 b) a2 1 (T_pair l2 n3 n4 a2 b)}
| Comb_update_plustwo {n a b1 b2 c} :
  `{comb_update_ty b1 c n b2 ->
    comb_update_ty (T_pair l1 n1 n2 a b1) c (S (S n)) (T_pair l2 n3 n4 a b2)}
.

Definition mutez_bound : Z.t := Z.pow 2 63.

Inductive instr_typed : instr -> list ty -> list ty -> Prop :=
(* I_RAW allows embedding raw Michelson. It's like an instruction [RAW n { code }] which means to apply the code *)
| Raw_typed {p s1 s2 b} :
  `{(* raw_typed p s1 b -> *)
      length s1 = n ->
      instr_typed (I_RAW l n p) (s1 ++ s2) (b :: s2)}
(* Structural stuff *)
| Seq_typed {p s1 s2} :
    `{prog_typed p s1 s2 ->
      instr_typed (I_SEQ l p) s1 s2}
| Dip_typed {p a s1 s2} :
    `{prog_typed p s1 s2 ->
      instr_typed (I_DIP l p) (a :: s1) (a :: s2)}
| Dig_typed {n a s1 s2} :
    `{length s1 = n ->
      instr_typed (I_DIG l n) (s1 ++ a :: s2) (a :: s1 ++ s2)}
| Dug_typed {n a s1 s2} :
    `{length s1 = n ->
      instr_typed (I_DUG l n) (a :: s1 ++ s2) (s1 ++ a :: s2)}
| Dup_typed {n s t} :
    `{List.nth_error s n = Some t ->
      instr_typed (I_DUP l (S n)) s (t :: s)}
| Drop_typed {s1 s2} :
    `{length s1 = n ->
      instr_typed (I_DROP l n) (s1 ++ s2) s2}
| Swap_typed {a b s} :
    `{instr_typed (I_SWAP l) (a :: b :: s) (b :: a :: s)}

(* Unit *)
| Unit_typed {s} :
    `{instr_typed (I_UNIT l1) s (T_unit l2 :: s)}
| True_typed {s} :
    `{instr_typed (I_TRUE l1) s (T_bool l2 :: s)}

(* Or *)
| Left_typed {a b s} :
    `{instr_typed (I_LEFT l1 b) (a :: s) (T_or l2 n1 n2 a b :: s)}
| Right_typed {a b s} :
    `{instr_typed (I_RIGHT l1 a) (b :: s) (T_or l2 n1 n2 a b :: s)}
| If_left_typed {bt bf a b s1 s2} :
    `{prog_typed bt (a :: s1) s2 ->
      prog_typed bf (b :: s1) s2 ->
      instr_typed (I_IF_LEFT l1 bt bf) (T_or l2 n1 n2 a b :: s1) s2}

(* Pairs *)
| Pair_typed {n t ts s} :
    `{comb_ty t ts ->
      length ts = n ->
      instr_typed (I_PAIR l1 n) (ts ++ s) (t :: s)}
| Unpair_typed {n t ts s} :
    `{comb_ty t ts ->
      length ts = n ->
      instr_typed (I_UNPAIR l1 n) (t :: s) (ts ++ s)}

| Get_typed {t n x s} :
  `{comb_get_ty t n x ->
    instr_typed (I_GET l1 n) (t :: s) (x :: s)}
| Update_typed {t x n t' s} :
  `{comb_update_ty t x n t' ->
    instr_typed (I_UPDATE l1 n) (x :: t :: s) (t' :: s)}

| Car_typed {a b s} :
    `{instr_typed (I_CAR l1) (T_pair l2 n1 n2 a b :: s) (a :: s)}
| Cdr_typed {a b s} :
    `{instr_typed (I_CDR l1) (T_pair l2 n1 n2 a b :: s) (b :: s)}

(* Bools *)
| If_typed {bt bf s1 s2} :
    `{prog_typed bt s1 s2 ->
      prog_typed bf s1 s2 ->
      instr_typed (I_IF l1 bt bf) (T_bool l2 :: s1) s2}

(* Option *)
| If_none_typed {bt bf a s1 s2} :
    `{prog_typed bt s1 s2 ->
      prog_typed bf (a :: s1) s2 ->
      instr_typed (I_IF_NONE l1 bt bf) (T_option l2 a :: s1) s2}

(* List *)
| Nil_instr_typed {a s1} :
  `{instr_typed (I_NIL l1 a) s1 (T_list l2 a :: s1)}
| Cons_instr_typed {a s} :
  `{instr_typed (I_CONS l1) (a :: T_list l1 a :: s) (T_list l2 a :: s)}
| If_cons_typed {bt bf a s1 s2} :
    `{prog_typed bt (a :: T_list l1 a :: s1) s2 ->
      prog_typed bf s1 s2 ->
      instr_typed (I_IF_CONS l2 bt bf) (T_list l3 a :: s1) s2}

(* Functions *)
| Func_typed {a b r1 r2 code s d1 d2} :
    `{prog_typed code (a :: d1) (b :: d2) ->
      ope_valid r1 (length s) ->
      select r1 s = d1 ->
      ope_valid r2 (length d1) ->
      select r2 d1 = d2 ->
      instr_typed (I_FUNC l1 d1 a b r1 r2 code) s (T_func l2 a b :: s)}
| Recfunc_typed {a b r1 r2 code s d1 d2} :
    `{prog_typed code (a :: (T_lambda l1 a b) :: d1) (b :: d2) ->
      ope_valid r1 (length s) ->
      select r1 s = d1 ->
      ope_valid r2 (length d1) ->
      select r2 d1 = d2 ->
      instr_typed (I_REC_FUNC l1 d1 a b r1 r2 code) s (T_func l2 a b :: s)}
| Exec_func_typed {a b s} :
    `{instr_typed (I_EXEC l1) (a :: T_func l2 a b :: s) (b :: s)}

(* Lambdas *)
| Lambda_typed {a b code s} :
    `{prog_typed code [a] [b] ->
      instr_typed (I_LAMBDA l1 a b code) s (T_lambda l2 a b :: s)}
| Exec_lambda_typed {a b s} :
    `{instr_typed (I_EXEC l1) (a :: T_lambda l2 a b :: s) (b :: s)}
| Apply_lambda_typed {a b c s} :
    `{packable a ->
      instr_typed (I_APPLY_LAMBDA l1 a) (a :: T_lambda l2 (T_pair l3 n1 n2 a b) c :: s) (T_lambda l4 b c :: s)}

(* Loops *)
| Loop_typed {body s} :
    `{prog_typed body s (T_bool l1 :: s) ->
      instr_typed (I_LOOP l2 body) (T_bool l3 :: s) s}
| Loop_left_typed {a b body s} :
    `{prog_typed body (a :: s) (T_or l1 n1 n2 a b :: s) ->
      instr_typed (I_LOOP_LEFT l2 body) (T_or l3 n1 n2 a b :: s) (b :: s)}
| Iter_typed_list {body a s} :
  `{prog_typed body (a :: s) s ->
    instr_typed (I_ITER l1 body) (T_list l2 a :: s) s}
| Map_typed_list {body a b s} :
    `{prog_typed body (a :: s) (b :: s) ->
      instr_typed (I_MAP l1 body) (T_list l2 a :: s) (T_list l3 b :: s)}
(* FICTION *)
| For_typed {body s} :
  `{prog_typed body (T_int l1 :: s) s ->
    instr_typed (I_FOR l2 body) (T_int l3 :: T_int l4 :: T_int l5 :: s) s}

(* Failure *)
| Failwith_typed {a s1 s2} :
    `{instr_typed (I_FAILWITH l) (a :: s1) s2}

(* CREATE_CONTRACT *)
| Create_contract_typed_list {p' s' code s1} :
    `{prog_typed code [T_pair l1 n1 n2 p' s'] [T_pair l2 n3 n4 (T_list l3 (T_operation l4)) s'] ->
      instr_typed (I_CREATE_CONTRACT l5 p' s' code) (T_option l6 (T_key_hash l7) :: T_mutez l8 :: s' :: s1) (T_operation l9 :: T_address l10 :: s1)}

with prog_typed : prog -> list ty -> list ty -> Prop :=
| Nil_typed {s} :
    prog_typed [] (* P_nil *) s s
| Cons_typed {i p s1 s2 s3} :
    instr_typed i s1 s2 ->
    prog_typed p s2 s3 ->
    prog_typed (i :: p) (* (P_cons i p) *) s1 s3
.

Hint Constructors instr_typed : michelson.
Hint Constructors prog_typed : michelson.

Scheme instr_typed_ind' := Induction for instr_typed Sort Prop
with prog_typed_ind' := Induction for prog_typed Sort Prop.

Combined Scheme instr_typed_mutind from
  instr_typed_ind', prog_typed_ind'.

Lemma dig_0_typed {l x s1} : instr_typed (I_DIG l 0) (x :: s1) (x :: s1).
Proof.
  change (instr_typed (I_DIG l 0) ([] ++ x :: s1) (x :: [] ++ s1));
    eauto with michelson.
Defined.

Lemma drop_1_typed {l x s1} : instr_typed (I_DROP l 1) (x :: s1) s1.
Proof.
  change (instr_typed (I_DROP l 1) ([x] ++ s1) s1);
    eauto with michelson.
Defined.

Lemma pair_2_typed {x y s1} : `{instr_typed (I_PAIR l1 2) (x :: y :: s1) (T_pair l2 n1 n2 x y :: s1)}.
Proof.
  intros;
    change (instr_typed (I_PAIR l1 2) ([x; y] ++ s1) (T_pair l2 n1 n2 x y :: s1));
    eauto with michelson.
Defined.

Hint Resolve dig_0_typed : michelson.
Hint Resolve drop_1_typed : michelson.
Hint Resolve pair_2_typed : michelson.

Lemma prog_typed_app {p1 p2 s1 s2 s3} :
  prog_typed p1 s1 s2 ->
  prog_typed p2 s2 s3 ->
  prog_typed (p1 ++ p2) s1 s3.
Proof.
  intros H;
    revert p2 s3;
    induction H;
    intros;
    simpl; eauto with michelson.
Qed.

Hint Resolve prog_typed_app : michelson.

Notation stack_ty := (list ty).

(************
 * Compiler *
 ************)

Fixpoint compile_ope_aux (n : nat) (us : ope) : prog :=
  match us with
  | [] => []
  | false :: us => [I_DIG null n;
                    I_SEQ null (compile_ope_aux (S n) us);
                    I_DROP null 1]
  | true :: us => compile_ope_aux (S n) us
  end.

Definition compile_ope (us : ope) : prog :=
  compile_ope_aux O us.

Lemma compile_ope_aux_typed :
  forall us n s1 s2,
    length s1 = n ->
    length us = length s2 ->
    prog_typed (compile_ope_aux n us) (s1 ++ s2) (s1 ++ select us s2).
Proof.
  intros us; induction us as [|u us]; intros n s1 s2 H1 H2; eauto;
    destruct s2; try (simpl in H2; exfalso; lia);
    simpl; eauto with michelson;
    destruct u; simpl; eauto.
  - enough (H : prog_typed (compile_ope_aux (S n) us) ((s1 ++ [t]) ++ s2) ((s1 ++ [t]) ++ select us s2))
      by (repeat rewrite <- app_assoc in H; exact H);
      eapply IHus.
    + rewrite app_length; simpl; lia.
    + simpl in H2; lia.
  - repeat econstructor; eauto.
    + enough (H : prog_typed (compile_ope_aux (S n) us) (t :: s1 ++ s2) ([t] ++ s1 ++ select us s2)) by exact H;
        change (prog_typed (compile_ope_aux (S n) us) ((t :: s1) ++ s2) ((t :: s1) ++ select us s2));
        eapply IHus; simpl; eauto.
    + eauto.
Qed.

Lemma compile_ope_typed :
  forall {us g g'},
    length us = length g ->
    select us g = g' ->
    prog_typed (compile_ope us) g g'.
Proof.
  intros; subst; apply compile_ope_aux_typed with (s1 := []); auto.
Qed.

Lemma compile_ope_aux_weak_lemma1  :
  forall us m n, compile_ope_aux n (us ++ repeat true m) = compile_ope_aux n us.
Proof.
  intros us; induction us as [|u us]; intros m n; simpl.
  - revert n; induction m; intros n.
    + reflexivity.
    + eapply IHm.
  - destruct u; rewrite IHus; reflexivity.
Qed.

Lemma compile_ope_weak_lemma1  :
  forall us m, compile_ope (us ++ repeat true m) = compile_ope us.
Proof.
  unfold compile_ope; intros; eapply compile_ope_aux_weak_lemma1.
Qed.

Lemma compile_ope_weak_typed :
  forall {us g g' d},
    length us = length g ->
    select us g = g' ->
    prog_typed (compile_ope us) (g ++ d) (g' ++ d).
Proof.
  intros us g g' d H1 H2;
    assert (H3 : prog_typed (compile_ope (us ++ repeat true (length d))) (g ++ d) (g' ++ d))
      by (eapply compile_ope_typed;
          [ repeat rewrite app_length; rewrite repeat_length; eauto
          | rewrite select_app; eauto; rewrite select_repeat_true; rewrite H2; reflexivity ]);
    rewrite compile_ope_weak_lemma1 in H3;
    exact H3.
Qed.

Fixpoint comb (az : list ty) : ty :=
  match az with
  | [] => T_unit null
  | [a] => a
  | a :: az => T_pair null None None a (comb az)
  end.

Definition PAIR (n : nat) : prog :=
  match n with
  | 0 => [I_UNIT null]
  | 1 => []
  | _ => [I_PAIR null n]
  end.

Definition UNPAIR (n : nat) : prog :=
  match n with
  | 0 => [I_DROP null 1]
  | 1 => []
  | _ => [I_UNPAIR null n]
  end.

(* hmm, reusing the names GET and UPDATE is confusing because these are different *)
Definition GET (i n : nat) : prog :=
  let i := if beq_nat (S i) n
           then 2 * i
           else 2 * i + 1 in
  [I_GET null i].

Definition UPDATE (i n : nat) : prog :=
  let i := if beq_nat (S i) n
           then 2 * i
           else 2 * i + 1 in
  [I_UPDATE null i].

Fixpoint compile_expr (r : ope) (env : list ty) (e : expr) {struct e} : prog :=
  match e with
  | E_var l n =>
      [I_SEQ l [I_DUP null (S (embed r n))]]
  | E_let_in l e1 e2 =>
      let e1' := compile_expr r env e1 in
      let e2' := compile_binds r env e2 in
      [I_SEQ l [I_SEQ null e1';
                I_SEQ null e2']]
  | E_deref l n => [I_SEQ l [I_DUP null (S (embed r n))]]
  | E_let_mut_in l e1 e2 =>
      let e1' := compile_expr r env e1 in
      let e2' := compile_binds r env e2 in
      [I_SEQ l [I_SEQ null e1';
                I_SEQ null e2']]
  | E_assign l n e1 =>
      let e1' := compile_expr r env e1 in
      [I_SEQ l [I_SEQ null e1';
                I_DUG null (embed r n);
                I_DIG null (S (embed r n));
                I_DROP null 1;
                I_UNIT null]]
  | E_for l args body =>
      let args' := compile_args r env args in
      let body' := compile_binds r env body in
      [I_SEQ l [I_SEQ null args';
                I_FOR null [I_SEQ null body'; I_DROP null 1];
                I_UNIT null]]
  | E_for_each l coll body =>
      let coll' := compile_expr r env coll in
      let body' := compile_binds r env body in
      (* TODO? this is a hack to make map iteration work -- Michelson
         gives you the key and value in a pair, but LIGO things here
         are set up to expect two stack elements, so insert an UNPAIR
         in the map case: *)
      if beq_nat 2 (binds_length body)
      then
        [I_SEQ l [I_SEQ null coll';
                  I_ITER null [I_UNPAIR null 2; I_SEQ null body'; I_DROP null 1];
                  I_UNIT null]]
      else
        [I_SEQ l [I_SEQ null coll';
                  I_ITER null [I_SEQ null body'; I_DROP null 1];
                  I_UNIT null]]
  | E_while l cond body =>
      let cond' := compile_expr r env cond in
      let body' := compile_expr (false :: r) env body in
      [I_TRUE null;
       I_LOOP l [I_SEQ null cond';
                 I_DUP null 1;
                 I_IF null [I_SEQ null body'; I_DROP null 1] []];
       I_UNIT null]
  | E_tuple l args =>
      [I_SEQ l [I_SEQ null (compile_args r env args);
                I_SEQ null (PAIR (args_length args))]]
  | E_let_tuple l e1 e2 =>
      [I_SEQ l [I_SEQ null (compile_expr r env e1);
                I_SEQ null (UNPAIR (binds_length e2));
                I_SEQ null (compile_binds r env e2)]]
  | E_proj l e i n =>
      [I_SEQ l [I_SEQ null (compile_expr r env e);
                I_SEQ null (GET i n)]]
  | E_update l args i n =>
      [I_SEQ l [I_SEQ null (compile_args r env args);
                I_SEQ null (UPDATE i n)]]
  | E_app l e =>
      [I_SEQ l [I_SEQ null (compile_args r env e);
                I_SWAP null;
                I_EXEC null]]
  | E_lam l e b =>
      let a := match e with
               | Binds _ [a] _ => a
               | _ => T_unit null
               end in
      [I_SEQ l [I_FUNC null env a b (trim r (length env)) (repeat true (length env))
                       (compile_binds (repeat true (length env)) env e)]]
  | E_rec l e b =>
      let a := match e with
               | Binds _ [a;_] _ => a
               | _ => T_unit null
               end in
      [I_SEQ l [I_REC_FUNC null env a b (trim r (length env)) (repeat true (length env))
                       (compile_binds (repeat true (length env)) env e)]]
  | E_literal l lit =>
      [I_SEQ l [I_RAW null O (lit_code null lit)]]
  | E_pair l e =>
      [I_SEQ l [I_SEQ null (compile_args r env e);
                I_PAIR null 2]]
  | E_car l e =>
      [I_SEQ l [I_SEQ null (compile_expr r env e);
                I_CAR null]]
  | E_cdr l e =>
      [I_SEQ l [I_SEQ null (compile_expr r env e);
                I_CDR null]]
  | E_unit l =>
      [I_SEQ l [I_UNIT null]]
  | E_left l b e =>
      [I_SEQ l [I_SEQ null (compile_expr r env e);
                I_LEFT null b]]
  | E_right l a e =>
      [I_SEQ l [I_SEQ null (compile_expr r env e);
                I_RIGHT null a]]
  | E_if_bool l e1 e2 e3 =>
      [I_SEQ l [I_SEQ null (compile_expr r env e1);
                I_IF null (compile_expr r env e2) (compile_expr r env e3)]]
  | E_if_none l e1 e2 b3 =>
      [I_SEQ l [I_SEQ null (compile_expr r env e1);
                I_IF_NONE null (compile_expr r env e2) (compile_binds r env b3)]]
  | E_if_cons l e1 b2 e3 =>
      [I_SEQ l [I_SEQ null (compile_expr r env e1);
                I_IF_CONS null (compile_binds r env b2) (compile_expr r env e3)]]
  | E_if_left l e1 b2 b3 =>
      [I_SEQ l [I_SEQ null (compile_expr r env e1);
                I_IF_LEFT null (compile_binds r env b2) (compile_binds r env b3)]]
  | E_iter l e1 e2 =>
      [I_SEQ l [I_SEQ null (compile_expr r env e2);
                I_ITER null (compile_binds r env e1 ++ [I_DROP l 1]);
                I_UNIT null]]
  | E_map l e1 e2 =>
      [I_SEQ l [I_SEQ null (compile_expr r env e2);
                I_MAP null (compile_binds r env e1)]]
  | E_loop_left l e1 b e2 =>
      [I_SEQ l [I_SEQ null (compile_expr r env e2);
                I_LEFT null b;
                I_LOOP_LEFT null (compile_binds r env e1)]]
  | E_fold l e1 e2 e3 =>
      [I_SEQ l [I_SEQ null (compile_expr r env e1);
                I_SEQ null (compile_expr (false :: r) env e2);
                I_ITER null [I_SWAP null; I_PAIR null 2;
                             I_SEQ null (compile_binds r env e3)]]]
  | E_fold_right l elem e1 e2 e3 =>
      [I_SEQ l [I_SEQ null (compile_expr r env e1);
                I_SEQ null (compile_expr (false :: r) env e2);
                I_NIL null elem; I_SWAP null; I_ITER null [I_CONS null];
                I_ITER null [I_PAIR null 2;
                             I_SEQ null (compile_binds r env e3)]]]
  | E_failwith l e =>
      [I_SEQ l [I_SEQ null (compile_expr r env e);
                I_FAILWITH null]]
  | E_raw_michelson l a b code =>
      [I_SEQ l [I_LAMBDA null a b [I_RAW null 1 code]]]
  | E_inline_michelson l code args =>
      [I_SEQ l (compile_args r env args ++ [I_RAW null (args_length args) code])]
  | E_global_constant l b hash args =>
      [I_SEQ l [I_SEQ null (compile_args r env args);
                I_RAW null (args_length args) (global_constant null hash)]]
  | E_create_contract l p s script args =>
      [I_SEQ l [I_SEQ null (compile_args r env args);
                (* following CREATE_CONTRACT is also given location l
                   because "view_restrictions" error messages depended
                   on it, hmm *)
                I_CREATE_CONTRACT l p s (compile_binds [true] [T_pair null None None p s] script);
                I_PAIR null 2]]
  end
with
compile_args
  (r : ope) (env : list ty) (e : args) {struct e} : prog :=
  match e with
  | Args_nil l => []
  | Args_cons l e args =>
    [I_SEQ null (compile_expr r env e);
     I_SEQ null (compile_args (false :: r) env args)]
  end
with
compile_binds
  (r : ope) (env : list ty) (e : binds) {struct e} : prog :=
  match e with
  | Binds l az e =>
      let env' := az ++ env in
      let r' := repeat true (length az) ++ r in
      app [I_SEQ (with_var_names env' null) []]
          (app (compile_expr r' env' e)
               [I_DIP null [I_DROP null (length az)]])
  end.

Definition compile_expr_typed (e : expr) : Prop :=
  forall g a,
    expr_typed g e a ->
    forall r d,
      select r d = g ->
      prog_typed (compile_expr r g e) d (a :: d).

Definition compile_args_typed (e : args) : Prop :=
  forall g az,
    args_typed g e az ->
    forall r d,
      select r d = g ->
      prog_typed (compile_args r g e) d (az ++ d).

Definition compile_binds_typed (e : binds) : Prop :=
  forall g az b,
    binds_typed g e az b ->
    forall r d,
      select r d = g ->
      prog_typed (compile_binds r g e) (az ++ d) (b :: d).

Ltac Zify.zify_pre_hook ::=
  repeat match goal with
         | [H : @eq (list _) _ _ |- _] =>
             eapply (f_equal (@length _)) in H;
             repeat rewrite app_length, plus_comm in H;
             simpl in H
         end.

Lemma type_preservation :
  (forall e, compile_expr_typed e) /\
    (forall e, compile_binds_typed e) /\
    (forall e, compile_args_typed e).
Proof.
  eapply expr_mutind;
    unfold compile_expr_typed, compile_binds_typed, compile_args_typed;
    intros;
    first[ invert_pred expr_typed
         | invert_pred args_typed
         | invert_pred binds_typed
         ];
    repeat match goal with
           | [H1 : forall g b, expr_typed g ?e b -> forall r d, select r d = g -> prog_typed (compile_expr r g ?e) d (b :: d),
                H2: expr_typed ?g ?e ?b |- _] =>
               first [epose proof (H1 _ _ H2 [] _ eq_refl)
                     |epose proof (H1 _ _ H2 _ _ eq_refl)];
               mark_done H2
           | [H1 : forall g az b, binds_typed g ?e az b -> forall r d, select r d = g -> prog_typed (compile_binds r g ?e) (az ++ d) (b :: d),
                H2: binds_typed ?g ?e ?az ?b |- _] =>
               first [epose proof (H1 _ _ _ H2 [] _ eq_refl)
                     |epose proof (H1 _ _ _ H2 _ _ eq_refl)];
               mark_done H2
           | [H1 : forall g az, args_typed g ?e az -> forall r d, select r d = g -> prog_typed (compile_args r g ?e) d (az ++ d),
                H2: args_typed ?g ?e ?az |- _] =>
               first [epose proof (H1 _ _ H2 [] _ eq_refl)
                     |epose proof (H1 _ _ H2 _ _ eq_refl)];
               mark_done H2
           end;
    clear_done;
    simpl.
  (* E_var *)
  - repeat econstructor;
      rewrite nth_error_embed;
      auto; congruence.
  (* E_let_in *)
  - eauto with michelson.
    admit.
  (* E_tuple *)
  - admit.
  (* E_let_tuple *)
  - admit.
  (* E_proj *)
  - admit.
  (* E_update *)
  - admit.
  (* E_app *)
  - admit.
  (* E_lam *)
  - admit.
  (* E_literal *)
  - admit.
  (* E_pair *)
  - admit.
  (* E_car *)
  - admit.
  (* E_cdr *)
  - admit.
  (* E_unit *)
  - admit.
  (* E_left *)
  - admit.
  (* E_right *)
  - admit.
  (* E_if_left *)
  - admit.
  (* E_if_bool *)
  - admit.
  (* E_if_none *)
  - admit.
  (* E_if_cons *)
  - admit.
  (* E_iter *)
  - admit.
  (* E_map *)
  - admit.
  (* E_loop_left *)
  - admit.
  (* E_fold *)
  - invert_pred iter_class.
    + eauto 20 with michelson.
    + admit.
    + admit.
  (* E_fold_right *)
  - invert_pred iter_class.
    + eauto 20 with michelson.
    + admit.
    + admit.
  (* E_failwith *)
  - eauto with michelson.
    admit.
  (* E_raw_michelson *)
  - admit.
  (* E_inline_michelson *)
  - admit.
  (* E_global_constant *)
  - admit.
  (* E_create_contract *)
  - admit.
  (* Binds *)
  - specialize (H _ _ H8 (repeat true (length az) ++ r) (az ++ d));
      rewrite select_app in H by (rewrite repeat_length; auto);
      rewrite select_repeat_true in H;
      specialize (H eq_refl);
      econstructor; [repeat econstructor|];
      eapply prog_typed_app;
      eauto with michelson.
  (* Args_nil *)
  - eauto with michelson.
  (* Args_cons *)
  - eauto with michelson.
Admitted.

(* This is used to postulate termination for strengthening loops. Should
   probably replace it with specific Axioms? *)
Definition TRUST_ME_IT_TERMINATES {A : Set} : A.
Proof. admit. Admitted.

Reserved Notation "'strengthen_progg'".

Context {strengthen_meta : ope -> meta -> meta}.

Fixpoint
  strengthen_instr (i : instr) (r : list bool) {struct i} : list bool * prog :=
  match i with
  | I_RAW l n code =>
      if ope_hd r
      then (repeat true n ++ tl r,
             [I_RAW l n code])
      (* if we knew that the code is pure, we could do this: *)
      (* else (repeat false n ++ tl r, []) *)
      (* but instead we have to settle for: *)
      else (repeat true n ++ tl r,
             [I_RAW l n code; I_DROP null 1])
  | I_SEQ l p =>
      let rp' := strengthen_progg p r in
      let r' := fst rp' in
      let p' := snd rp' in
      let l' := strengthen_meta r'  l in
      (r',
        [I_SEQ l' p'])
  | I_DIP l p =>
      let rp' := strengthen_progg p (tl r) in
      let r' := fst rp' in
      let p' := snd rp' in
      (ope_hd r :: r',
        if ope_hd r
        then [I_DIP l p']
        else p')
  | I_DIG l n =>
      if ope_hd r
      then
        let r := tl r in
        (split_left n r ++ true :: split_right n r,
          [I_DIG l (weight (split_left n r))])
      else
        let r := tl r in
        (split_left n r ++ false :: split_right n r,
          [])
  | I_DUG l n =>
      let r1 := split_left n r in
      let r2 := split_right n r in
      if ope_hd r2
      then
        let r2 := tl r2 in
        (true :: r1 ++ r2,
          [I_DUG l (weight r1)])
      else
        let r2 := tl r2 in
        (false :: r1 ++ r2,
          [])
  | I_DUP l n =>
      let r1 := split_left n r in
      let r2 := split_right n r in
      if ope_hd r1
      then
        (tl r1 ++ true :: tl r2,
          if ope_hd r2
          then [I_DUP l (S (weight (tl r1)))]
          else [I_DIG l (weight (tl r1))])
      else
        (tl r1 ++ r2, [])
  | I_DROP l n =>
      (repeat false n ++ r, []) (* :D *)
  | I_SWAP l =>
      (ope_hd (tl r) :: ope_hd r :: tl (tl r),
        if andb (ope_hd r) (ope_hd (tl r))
        then [I_SWAP l]
        else [])
  | I_UNIT l =>
      (tl r,
        if ope_hd r
        then [I_UNIT l]
        else [])
  | I_TRUE l =>
      (tl r,
        if ope_hd r
        then [I_TRUE l]
        else [])
  | I_PAIR l n =>
      if ope_hd r
      then (repeat true n ++ tl r, [I_PAIR l n])
      else (repeat false n ++ tl r, [])
  | I_UNPAIR l n =>
      let r1 := split_left n r in
      let r2 := split_right n r in
      if leb 1 (weight r1)
      then
        if leb (weight r1) 1
        then
          (* only one thing selected, use GET *)
          let idx := (fix todo (n : nat) (r1 : ope) {struct r1} : nat :=
                        match r1 with
                        | [] => 0
                        | b :: r1 => if b then n else todo (S n) r1
                        end) 0 r1 in
          (* comb elements are indexed with GET 2i+1, except the last
             element which is GET 2i. *)
          let zero_or_one := if (beq_nat idx (n-1)) then 0 else 1 in
          (true :: r2, [I_GET l (zero_or_one + 2 * idx)])
        else
          (* more than one thing selected, use UNPAIR *)
          (true :: r2, I_UNPAIR l n :: compile_ope r1)
      else
        (* nothing selected *)
        (false :: r2, [])
  | I_GET l n =>
      (r,
        if ope_hd r
        then [I_GET l n]
        else [])
  | I_UPDATE l n =>
      (ope_hd r :: r,
        if ope_hd r
        then [I_UPDATE l n]
        else [])
  | I_CAR l =>
      (r,
        if ope_hd r
        then [I_CAR l]
        else [])
  | I_CDR l =>
      (r,
        if ope_hd r
        then [I_CDR l]
        else [])
  | I_LEFT l b =>
      (r,
        if ope_hd r
        then [I_LEFT l b]
        else [])
  | I_RIGHT l a =>
      (r,
        if ope_hd r
        then [I_RIGHT l a]
        else [])
  | I_IF_LEFT l bt bf =>
      let (rt, bt') := strengthen_progg bt r in
      let (rf, bf') := strengthen_progg bf r in
      let r' := true :: union (tl rt) (tl rf) in
      (r',
        [I_IF_LEFT l
                   (compile_ope (ope_hd rt :: inj1 (tl rt) (tl rf)) ++ bt')
                   (compile_ope (ope_hd rf :: inj2 (tl rt) (tl rf)) ++ bf')])

  | I_IF l bt bf =>
      let (rt, bt') := strengthen_progg bt r in
      let (rf, bf') := strengthen_progg bf r in
      let r' := true :: union rt rf in
      (r',
        [I_IF l
              (compile_ope (inj1 rt rf) ++ bt')
              (compile_ope (inj2 rt rf) ++ bf')])

  | I_IF_NONE l bt bf =>
      let (rt, bt') := strengthen_progg bt r in
      let (rf, bf') := strengthen_progg bf r in
      let r' := true :: union rt (tl rf) in
      (r',
        [I_IF_NONE l
                   (compile_ope (inj1 rt (tl rf)) ++ bt')
                   (compile_ope (ope_hd rf :: inj2 rt (tl rf)) ++ bf')])

  | I_NIL l a =>
      (tl r,
        if ope_hd r
        then [I_NIL l a]
        else [])
  | I_CONS l =>
      (ope_hd r :: ope_hd r :: tl r,
        if ope_hd r
        then [I_CONS l]
        else [])
  | I_IF_CONS l bt bf =>
      let (rt, bt') := strengthen_progg bt r in
      let (rf, bf') := strengthen_progg bf r in
      let r' := true :: union (tl (tl rt)) rf in
      (r',
        [I_IF_CONS l
                   (compile_ope (ope_hd rt :: ope_hd (tl rt) :: inj1 (tl (tl rt)) rf) ++ bt')
                   (compile_ope (inj2 (tl (tl rt)) rf) ++ bf')])

  | I_FUNC l cs a b r1 r2 body =>
      if ope_hd r
      then
        let (rb, body') := strengthen_progg body (true :: repeat false (weight r2)) in
        (union (comp (tl rb) r1) (tl r),
          I_FUNC l
                 (select (tl rb) cs)
                 a b
                 (inj1 (comp (tl rb) r1) (tl r))
                 (repeat false (weight (tl rb)))
                 (compile_ope [ope_hd rb] ++ body')
            :: compile_ope (true :: inj2 (comp (tl rb) r1) (tl r)))
      else (tl r, [])
  | I_REC_FUNC l cs a b r1 r2 body =>
      if ope_hd r
      then
        let (rb, body') := strengthen_progg body (true :: repeat false (weight r2)) in
        (union (comp (tl (tl rb)) r1) (tl r),
          I_REC_FUNC l
                 (select (tl (tl rb)) cs)
                 a b
                 (inj1 (comp (tl (tl rb)) r1) (tl r))
                 (repeat false (weight (tl (tl rb))))
                 (compile_ope [ope_hd rb; ope_hd (tl rb)] ++ body')
            :: compile_ope (true :: inj2 (comp (tl (tl rb)) r1) (tl r)))
      else (tl r, [])
  | I_LAMBDA l a b body =>
      (tl r,
        if ope_hd r
        then
          let (rb, body') := strengthen_progg body [true] in
          [I_LAMBDA l a b (compile_ope [ope_hd rb] ++ body')]
        else [])
  | I_EXEC l =>
      (true :: true :: tl r,
        I_EXEC l :: compile_ope [ope_hd r])
  | I_APPLY_LAMBDA l c =>
      (ope_hd r :: ope_hd r :: tl r,
        if ope_hd r
        then [I_APPLY_LAMBDA l c]
        else [])

  | I_FAILWITH l =>
      ([true],
        [I_FAILWITH l])

  | I_LOOP l body =>
      let rfix := proj1_sig (fix_ope (fun x => union (fst (strengthen_progg body (true :: x))) r) [] TRUST_ME_IT_TERMINATES) in
      let (rb, body') := strengthen_progg body (true :: rfix) in
      (true :: rfix,
        I_LOOP l (compile_ope (inj1 rb r) ++ body') :: compile_ope (inj2 rb r))
  | I_LOOP_LEFT l body =>
      let rfix := proj1_sig (fix_ope (fun x => union (tl (fst (strengthen_progg body (true :: x)))) (tl r)) [] TRUST_ME_IT_TERMINATES) in
      let (rb, body') := strengthen_progg body (true :: rfix) in
      (true :: rfix,
        I_LOOP_LEFT l (compile_ope (ope_hd rb :: inj1 (tl rb) (tl r)) ++ body') :: compile_ope (ope_hd r :: inj2 (tl rb) (tl r)))
  | I_ITER l body =>
      let rfix := proj1_sig (fix_ope (fun x => union (tl (fst (strengthen_progg body x))) r) [] TRUST_ME_IT_TERMINATES) in
      let (rb, body') := strengthen_progg body rfix in
      (true :: rfix,
        I_ITER l (compile_ope (ope_hd rb :: inj1 (tl rb) r) ++ body') :: compile_ope (inj2 (tl rb) r))
  | I_MAP l body =>
      let rfix := proj1_sig (fix_ope (fun x => union (tl (fst (strengthen_progg body (true :: x)))) (tl r)) [] TRUST_ME_IT_TERMINATES) in
      let (rb, body') := strengthen_progg body (true :: rfix) in
      (true :: rfix,
        I_MAP l (compile_ope (true :: inj1 (tl rb) (tl r)) ++ body') :: compile_ope (ope_hd r :: inj2 (tl rb) (tl r)))
  | I_FOR l body =>
      let rfix := proj1_sig (fix_ope (fun x => union (tl (fst (strengthen_progg body x))) r) [] TRUST_ME_IT_TERMINATES) in
      let (rb, body') := strengthen_progg body rfix in
      (true :: true :: true :: rfix,
        I_FOR l (compile_ope (ope_hd rb :: inj1 (tl rb) r) ++ body') :: compile_ope (inj2 (tl rb) r))
  | I_CREATE_CONTRACT l p s script =>
      let (rs, script') := strengthen_progg script [true] in
      (true :: true :: true :: tl (tl r),
        [I_CREATE_CONTRACT l p s (compile_ope [ope_hd rs] ++ script')])
        
  end
(* this notation deals with the nested induction problem here, with
   instrs containing lists of instrs *)
where "'strengthen_progg'" :=
  (fix strengthen_prog (p : prog) (r : ope) {struct p} : list bool * prog :=
     match p with
     | [] => (r, [])
     | i :: p =>
         let r1p' := strengthen_prog p r in
         let r1 := fst r1p' in
         let p' := snd r1p' in
         let r2i' := strengthen_instr i r1 in
         let r2 := fst r2i' in
         let i' := snd r2i' in
         (r2, i' ++ p')
     end).

(* Exposing the 'strengthen_progg' notation for extraction: *)
Definition strengthen_prog (p : prog) (r : ope) : list bool * prog :=
  strengthen_progg p r.

Definition strengthen_instr_preservation :
  forall i s1 s2, instr_typed i s1 s2 -> Prop :=
  fun i s1 s2 _ => 
    forall r,
      (* TODO use ope_valid here? *)
      length r <= length s2 ->
      length (fst (strengthen_instr i r)) <= length s1 /\
      prog_typed (snd (strengthen_instr i r))
                 (select (fst (strengthen_instr i r)) s1)
                 (select r s2).

Definition strengthen_prog_preservation :
  forall p s1 s2, prog_typed p s1 s2 -> Prop :=
  fun p s1 s2 _ => 
    forall r,
      length r <= length s2 ->
      length (fst (strengthen_progg p r)) <= length s1 /\
      prog_typed (snd (strengthen_progg p r))
                 (select (fst (strengthen_progg p r)) s1)
                 (select r s2).

Lemma tl_length {A : Set} (xs : list A) :
  length (tl xs) = length xs - 1.
Proof. destruct xs; simpl; try rewrite Nat.sub_0_r; reflexivity. Qed.

Ltac strengthen_rewrite :=
  subst; simpl; simpl in *;
  repeat first [rewrite app_length in *
               |rewrite repeat_length in *
               |rewrite tl_length in *
               |rewrite inj1_length in *
               |rewrite inj2_length in *
               |rewrite union_length in *
               |rewrite select_inj1 in *
               |rewrite select_inj2 in *
               |rewrite select_app in *
               |rewrite select_filter_id in *
               |rewrite select_repeat_false in *
               |rewrite select_repeat_true' in *
               |rewrite split_left_length in *
               |rewrite split_right_length in *
               |rewrite split_left_nil in *
               |rewrite split_right_nil in *
               |rewrite select_split in *
               |rewrite select_weight in *
               |simpl in *].

Opaque compile_ope.

Lemma strengthen_instr_typed :
  (forall i s1 s2 (H : instr_typed i s1 s2), strengthen_instr_preservation H) /\
    (forall p s1 s2 (H : prog_typed p s1 s2), strengthen_prog_preservation H).
Proof with try split; try lia; eauto 15 with michelson.
  eapply (@instr_typed_mutind
            strengthen_instr_preservation
            strengthen_prog_preservation);
    unfold strengthen_prog_preservation;
    unfold strengthen_instr_preservation;
    simpl; intros;
    match goal with
    | [|- True] => trivial
    | _ => idtac
    end.
  (* I_RAW *)
  - destruct r as [|[|] r];
      strengthen_rewrite...
  (* I_SEQ *)
  - specialize (H r H0); destruct_conjs; eauto with michelson.
  (* I_DIP *)
  - specialize (H (tl r));
      rewrite tl_length in H;
      specialize (H ltac:(lia));
      destruct_conjs;
      remember (strengthen_progg p (tl r)) as p';
      destruct r as [|[|] r];
      split;
      try lia;
      try eassumption;
      simpl;
      eauto with michelson.
  (* I_DIG *)
  - destruct r as [|[|] r];
      strengthen_rewrite;
      try split; try lia; eauto with michelson;
      econstructor; econstructor;
      strengthen_rewrite;
      auto.
  (* I_DUG *)
  - remember (split_right (length s1) r) as rr;
      destruct rr as [|[|] rr];
      try destruct u;
      subst; rewrite <- Heqrr in *;
      strengthen_rewrite;
      try rewrite <- Heqrr in *;
      try split; try lia; eauto with michelson;
      pose proof (f_equal (@length _) Heqrr) as Hlen;
      simpl in Hlen;
      rewrite split_right_length in Hlen;
      try lia;
      econstructor; econstructor;
      strengthen_rewrite; auto.
  (* I_DUP *)
  - pose proof (nth_error_split _ _ e);
      destruct_conjs;
      destruct r as [|[|] r];
      strengthen_rewrite;
      try split; try lia; eauto with michelson;
      remember (split_right (length H0) r) as rr;
      destruct rr as [|[|] rr];
      strengthen_rewrite;
      econstructor; econstructor;
      strengthen_rewrite;
      auto;
      rewrite nth_error_app2 by (rewrite select_weight; auto; rewrite split_left_length; auto);
      rewrite select_weight by (rewrite split_left_length; auto);
      rewrite Nat.sub_diag;
      reflexivity.
  (* I_DROP *)
  - strengthen_rewrite...
  (* I_SWAP *)
  - destruct r as [|[|] [|[|] r]]; strengthen_rewrite...
  (* I_UNIT *)
  - destruct r as [|[|] r]; strengthen_rewrite...
  (* I_TRUE *)
  - destruct r as [|[|] r]; strengthen_rewrite...
  (* I_LEFT *)
  - destruct r as [|[|] r]; strengthen_rewrite...
  (* I_RIGHT *)
  - destruct r as [|[|] r]; strengthen_rewrite...
  (* I_IF_LEFT *)
  - specialize (H r);
      specialize (H0 r);
      remember (strengthen_progg bt r) as bt';
      destruct bt' as [[|[|] rt] bt'];
      remember (strengthen_progg bf r) as bf';
      destruct bf' as [[|[|] rf] bf'];
      simpl;
      specialize (H H1);
      specialize (H0 H1);
      destruct_conjs;
      strengthen_rewrite;
      try split;
      repeat rewrite union_nil_r;
      try lia;
      try solve [rewrite union_length; lia];
      try solve [econstructor; econstructor;
                 eapply prog_typed_app;
                 try eassumption;
                 eapply compile_ope_typed;
                 simpl;
                 first[ rewrite inj1_length;
                        rewrite select_weight by (rewrite union_length; lia);
                        reflexivity
                      | rewrite select_inj1; reflexivity
                      | rewrite inj2_length;
                        rewrite select_weight by (rewrite union_length; lia);
                        reflexivity
                      | rewrite select_inj2; reflexivity ]].
    + econstructor; econstructor;
      eapply prog_typed_app;
      try eassumption;
      eapply compile_ope_typed;
      auto.
    + assert (hmm : match rf with
                    | [] => []
                    | _ :: _ => repeat false (Datatypes.length (filter id rf))
                    end = repeat false (Datatypes.length (filter id rf)))
        by (destruct rf; reflexivity);
        rewrite hmm; clear hmm;
        econstructor; econstructor;
        eapply prog_typed_app;
        try eassumption;
        eapply compile_ope_typed;
        simpl;
        try rewrite repeat_length;
        try rewrite select_weight;
        try rewrite select_repeat_false;
        try rewrite select_filter_id;
        auto; lia.
    + assert (hmm : match rf with
                    | [] => []
                    | _ :: _ => repeat false (Datatypes.length (filter id rf))
                    end = repeat false (Datatypes.length (filter id rf)))
        by (destruct rf; reflexivity);
        rewrite hmm; clear hmm;
        econstructor; econstructor;
        eapply prog_typed_app;
        try eassumption;
        eapply compile_ope_typed;
        simpl;
        try rewrite repeat_length;
        try rewrite select_weight;
        try rewrite select_repeat_false;
        try rewrite select_filter_id;
        auto; lia.
    + econstructor; econstructor;
        eapply prog_typed_app;
        try eassumption;
        eapply compile_ope_typed;
        simpl;
        try rewrite repeat_length;
        try rewrite select_weight;
        try rewrite select_repeat_false;
        try rewrite select_filter_id;
        auto; try lia;
        repeat rewrite inj1_length;
        repeat rewrite inj2_length;
        repeat rewrite union_nil_r;
        repeat rewrite union_nil_l;
        auto;
        match goal with
        | [|- context G [select (inj1 ?r []) (select ?r ?s)]] =>
            let H := fresh "H" in
            let g := context G [select (inj1 r []) (select (union r []) s)] in
            enough (H : g) by (rewrite union_nil_r in H; exact H);
            rewrite select_inj1
        | [|- context G [select (inj2 ?r []) (select ?r ?s)]] =>
            let H := fresh "H" in
            let g := context G [select (inj2 r []) (select (union r []) s)] in
            enough (H : g) by (rewrite union_nil_r in H; exact H);
            rewrite select_inj2
        end;
        reflexivity.
    + econstructor; econstructor;
        eapply prog_typed_app;
        try eassumption;
        eapply compile_ope_typed;
        simpl.
      * rewrite inj1_length; rewrite union_nil_r;
          rewrite select_weight by lia; auto.
      * rewrite <- (union_nil_r rt) at 2;
          rewrite select_inj1; auto.
      * rewrite inj2_length, union_nil_r;
          rewrite select_weight by lia; auto.
      * rewrite <- (union_nil_r rt) at 2;
          rewrite select_inj2; auto.
  (* I_PAIR *)
  - destruct r as [|[|] r];
      strengthen_rewrite...
  (* I_UNPAIR *)
  - admit.
  (* I_GET *)
  - destruct r as [|[|] r]; strengthen_rewrite...
  (* I_UPDATE *)
  - destruct r as [|[|] r]; strengthen_rewrite...
  (* I_CAR *)
  - destruct r as [|[|] r]; strengthen_rewrite...
  (* I_CDR *)
  - destruct r as [|[|] r]; strengthen_rewrite...
  (* I_IF *)
  - specialize (H r);
      specialize (H0 r);
      remember (strengthen_progg bt r) as bt';
      destruct bt' as [[|[|] rt] bt'];
      remember (strengthen_progg bf r) as bf';
      destruct bf' as [[|[|] rf] bf'];
      simpl;
      specialize (H H1);
      specialize (H0 H1);
      destruct_conjs;
      strengthen_rewrite;
      try split;
      repeat rewrite union_nil_r;
      try lia;
      try solve [rewrite union_length; lia];
      econstructor; econstructor;
        eapply prog_typed_app;
        try eassumption;
        eapply compile_ope_typed;
      try reflexivity;
      destruct s1; strengthen_rewrite...
  (* I_IF_NONE *)
  - specialize (H r);
      specialize (H0 r);
      remember (strengthen_progg bt r) as bt';
      destruct bt' as [rt bt'];
      remember (strengthen_progg bf r) as bf';
      destruct bf' as [[|[|] rf] bf'];
      simpl;
      specialize (H H1);
      specialize (H0 H1);
      destruct_conjs;
      strengthen_rewrite;
      try split;
      try lia;
      try eassumption;
      econstructor; try econstructor;
      try eapply prog_typed_app;
      try eapply compile_ope_typed;
      strengthen_rewrite...
  (* I_NIL *)
  - split; destruct r as [|[|] r]; strengthen_rewrite...
  (* I_CONS *)
  - admit.
  (* I_IF_CONS *)
  - specialize (H r);
      specialize (H0 r);
      remember (strengthen_progg bt r) as bt';
      destruct bt' as [[|[|] [|[|] rt]] bt'];
      remember (strengthen_progg bf r) as bf';
      destruct bf' as [rf bf'];
      specialize (H H1);
      specialize (H0 H1);
      destruct_conjs;
      strengthen_rewrite;
      try split;
      try lia;
      try eassumption;
      econstructor; econstructor;
      eapply prog_typed_app;
      try eassumption;
      try eapply compile_ope_typed;
      try destruct rf;
      try destruct b;
      try destruct s1;
      strengthen_rewrite...
  (* I_FUNC *)
  - admit.
  (* I_REC_FUNC *)
  - admit.
  (* I_EXEC *)
  - destruct r;
      split;
      simpl in *;
      try lia;
      repeat econstructor.
    + eapply compile_ope_typed; auto.
    + destruct b0.
      * apply (@compile_ope_weak_typed [true] [b] [b]); auto.
      * apply (@compile_ope_weak_typed [false] [b] []); auto.
  (* I_LAMBDA *)
  - specialize (H [true] ltac:(simpl; lia));
      remember (strengthen_progg code [true]) as code';
      destruct code' as [rcode code'];
      destruct r as [|[|] r];
      strengthen_rewrite...
      destruct_conjs;
    econstructor; econstructor;
        eapply prog_typed_app;
        try eapply compile_ope_typed;
        try eassumption;
        try reflexivity;
        destruct rcode as [|[|] rcode];
        try reflexivity;
        simpl; rewrite select_nothing;
        reflexivity.
  (* I_EXEC *)
  - destruct r as [|[|] r]; split; simpl in *; try lia;
      try (repeat econstructor; eapply compile_ope_typed; reflexivity);
      repeat econstructor; eapply (@compile_ope_weak_typed [false] [b] []); reflexivity.
  (* I_APPLY_LAMBDA *)
  - destruct r as [|[|] r]; strengthen_rewrite...
  (* I_LOOP *)
  - match goal with [|- context[fix_ope ?f ?x ?d]] => remember (fix_ope f x d) as rfix end;
    destruct rfix as [rfix rfixed];
    simpl;
    pose proof (union_length (fst (strengthen_progg body (true :: rfix))) r) as Hlen1;
    assert (Hlen2 : length rfix <= length s).
    { eapply (f_equal (@proj1_sig _ _)) in Heqrfix;
        simpl in Heqrfix;
        rewrite Heqrfix;
        eapply fix_ope_preserves.
      - intros x Hx;
        rewrite union_length;
        pose proof (proj1 (H (true :: x) (le_n_S _ _ Hx)));
        lia.
      - simpl; lia. }
    pose proof (H (true :: rfix) (le_n_S _ _ Hlen2)) as [H1 H2];
      repeat match goal with
             | [|- context[let (x, y) := ?z in _]] =>
                 rewrite (surjective_pairing z)
             end;
      remember (strengthen_progg body (true :: rfix)) as rbody';
      simpl;
      split; [lia|];
      repeat econstructor;
      try eapply prog_typed_app;
      try eapply compile_ope_typed;
      try rewrite inj1_length;
      try rewrite inj2_length.
    + admit.
    + assert (HH : select (inj1 (fst rbody') r) (select rfix s) =
                     select (inj1 (fst rbody') r) (select (union (fst rbody') r) s)) by admit.
      rewrite HH, select_inj1; reflexivity.
    + exact H2.
    + admit.
    + assert (HH : select (inj2 (fst rbody') r) (select rfix s) =
                     select (inj2 (fst rbody') r) (select (union (fst rbody') r) s)) by admit.
      rewrite HH, select_inj2; reflexivity.
  (* I_LOOP_LEFT *)
  - admit.
  (* I_ITER *)
  - admit.
  (* I_MAP *)
  - admit.
  (* I_FOR *)
  - admit.
  (* I_FAILWITH *)
  - idtac...
  (* I_CREATE_CONTRACT *)
  - admit.
  (* nil prog *)
  - split...
  (* cons prog *)
  - specialize (H0 r H1);
      destruct_conjs;
      specialize (H (fst (strengthen_progg p r)) H0);
      destruct_conjs;
      split; try lia;
      remember (strengthen_progg p r) as p';
      destruct p' as [r' p'];
      try lia;
      eapply prog_typed_app...
Admitted.

End compiler.
