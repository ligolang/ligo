open Proto_alpha_utils.Memory_proto_alpha
module AC = Alpha_context

module Types = Contract.Types
module Option = Simple_utils.Option
module MBytes = Alpha_environment.MBytes

module Stack = struct
  open Script_typed_ir

  let descr bef aft instr =
    {
      loc = 0 ;
      bef ; aft ; instr
    }

  type nonrec 'a ty = 'a ty
  type 'a t = 'a stack_ty
  type nonrec ('a, 'b) descr = ('a, 'b) descr
  type ('a, 'b) code = ('a t) -> ('a, 'b) descr

  type ex_stack_ty = Ex_stack_ty : 'a t -> ex_stack_ty
  type ex_descr = Ex_descr : ('a, 'b) descr -> ex_descr
  type ex_code = Ex_code : ('a, 'b) code -> ex_code

  let stack ?annot a b = Item_t (a, b, annot)
  let unstack (item: (('a * 'rest) stack_ty)) : ('a ty * 'rest stack_ty) =
    let Item_t (hd, tl, _) = item in
    (hd, tl)

  let nil = Empty_t
  let head x = fst @@ unstack x
  let tail x = snd @@ unstack x

  let seq a b bef =
    let a_descr = a bef in
    let b_descr = b a_descr.aft in
    let aft = b_descr.aft in
    descr bef aft @@ Seq (a_descr, b_descr)

  let (@>) (stack : 'b t) (code : ('a, 'b) code) = code stack
  let (@|) = seq
  let (@:) = stack

  let (!:) : ('a, 'b) descr -> ('a, 'b) code = fun d _ -> d

  let (<.) (stack:'a t) (code: ('a, 'b) code): ('a, 'b) descr = code stack

  let (<::) : ('a, 'b) descr -> ('b, 'c) descr -> ('a, 'c) descr = fun ab bc ->
    descr ab.bef bc.aft @@ Seq(ab, bc)

  let (<:) (ab_descr:('a, 'b) descr) (code:('b, 'c) code) : ('a, 'c) descr =
    let bc_descr = code ab_descr.aft in
    ab_descr <:: bc_descr

end

open Stack

type nat = AC.Script_int.n AC.Script_int.num
type int_num = AC.Script_int.z AC.Script_int.num
type bytes = MBytes.t
type address = AC.Contract.t Script_typed_ir.ty
type mutez = AC.Tez.t Script_typed_ir.ty


module Stack_ops = struct
  open Script_typed_ir
  let dup : ('a * 'rest, 'a * ('a * 'rest)) code = fun bef ->
    let Item_t (ty, rest, _) = bef in
    descr bef (Item_t (ty, Item_t (ty, rest, None), None)) Dup

  let drop : ('a * 'rest, 'rest) code = fun bef ->
    let aft = snd @@ unstack bef in
    descr bef aft Drop

  let swap (bef : (('a * ('b * 'c)) stack_ty)) =
    let Item_t (a, Item_t (b, rest, _), _) = bef in
    descr bef (Item_t (b, (Item_t (a, rest, None)), None)) Swap

  let dip code (bef : ('ty * 'rest) stack_ty) =
    let Item_t (ty, rest, _) = bef in
    let applied = code rest in
    let aft = Item_t (ty, applied.aft, None) in
    descr bef aft (Dip (code rest))

  let noop : ('r, 'r) code = fun bef ->
    descr bef bef Nop

  let exec : (_, _) code = fun bef ->
    let lambda = head @@ tail bef in
    let (_, ret) = Types.assert_lambda lambda in
    let aft = ret @: (tail @@ tail bef) in
    descr bef aft Exec

  let fail aft : ('a * 'r, 'b) code = fun bef ->
    let head = fst @@ unstack bef in
    descr bef aft (Failwith head)

  let push_string str (bef : 'rest stack_ty) : (_, (string * 'rest)) descr =
    let aft = Item_t (Types.string, bef, None) in
    descr bef aft (Const (str))

  let push_none (a:'a ty) : ('rest, 'a option * 'rest) code = fun r ->
    let aft = stack (Types.option a) r in
    descr r aft (Const None)

  let push_unit : ('rest, unit * 'rest) code = fun r ->
    let aft = stack Types.unit r in
    descr r aft (Const ())

  let push_nat n (bef : 'rest stack_ty) : (_, (nat * 'rest)) descr =
    let aft = Item_t (Types.nat, bef, None) in
    descr bef aft (Const (Contract.Values.nat n))

  let push_int n (bef : 'rest stack_ty) : (_, (int_num * 'rest)) descr =
    let aft = Types.int @: bef in
    descr bef aft (Const (Contract.Values.int n))

  let push_tez n (bef : 'rest stack_ty) : (_, (AC.Tez.tez * 'rest)) descr =
    let aft = Types.mutez @: bef in
    descr bef aft (Const (Contract.Values.tez n))

  let push_bool b : ('s, bool * 's) code = fun bef ->
    let aft = stack Types.bool bef in
    descr bef aft (Const b)

  let push_generic ty v : ('s, _ * 's) code = fun bef ->
    let aft = stack ty bef in
    descr bef aft (Const v)

  let failstring str aft =
    push_string str @| fail aft

end

module Stack_shortcuts = struct
  open Stack_ops

  let diip c x = dip (dip c) x
  let diiip c x = dip (diip c) x
  let diiiip c x = dip (diiip c) x

  let bubble_1 = swap
  let bubble_down_1 = swap

  let bubble_2 : ('a * ('b * ('c * 'r)), 'c * ('a * ('b * 'r))) code = fun bef ->
    bef <. dip swap <: swap
  let bubble_down_2 : ('a * ('b * ('c * 'r)), ('b * ('c * ('a * 'r)))) code = fun bef ->
    bef <. swap <: dip swap

  let bubble_3 : ('a * ('b * ('c * ('d * 'r))), 'd * ('a * ('b * ('c * 'r)))) code = fun bef ->
    bef <. diip swap <: dip swap <: swap

  let keep_1 : type r s . ('a * r, s) code -> ('a * r, 'a * s) code = fun code bef ->
    bef <. dup <: dip code

  let save_1_1 : type r . ('a * r, 'b * r) code -> ('a * r, 'b * ('a * r)) code = fun code s ->
    s <. keep_1 code <: swap

  let keep_2 : type r s . ('a * ('b * r), s) code -> ('a * ('b * r), ('a * ('b * s))) code = fun code bef ->
    (dup @| dip (swap @| dup @| dip (swap @| code))) bef

  let keep_2_1 : type r s . ('a * ('b * r), s) code -> ('a * ('b * r), 'b * s) code = fun code bef ->
    (dip dup @| swap @| dip code) bef

  let relativize_1_1 : ('a * unit, 'b * unit) descr -> ('a * 'r, 'b * 'r) code = fun d s ->
    let aft = head d.aft @: tail s in
    descr s aft d.instr

end

module Pair_ops = struct
  let car (bef : (('a * 'b) * 'rest) Stack.t) =
    let (pair, rest) = unstack bef in
    let (a, _) = Contract.Types.assert_pair pair in
    descr bef (stack a rest) Car

  let cdr (bef : (('a * 'b) * 'rest) Stack.t) =
    let (pair, rest) = unstack bef in
    let (_, b) = Contract.Types.assert_pair pair in
    descr bef (stack b rest) Cdr

  let pair (bef : ('a * ('b * 'rest)) Stack.t) =
    let (a, rest) = unstack bef in
    let (b, rest) = unstack rest in
    let aft = (Types.pair a b) @: rest in
    descr bef aft Cons_pair

  open Stack_ops
  let carcdr s = s <. car <: Stack_ops.dip cdr

  let cdrcar s = s <. cdr <: dip car

  let cdrcdr s = s <. cdr <: dip cdr

  let carcar s = s <. car <: dip car

  let cdar s = s <. cdr <: car

  let unpair s = s <. dup <: car <: dip cdr
end

module Option_ops = struct
  open Script_typed_ir

  let cons bef =
    let (hd, tl) = unstack bef in
    descr bef (stack (Contract.Types.option hd) tl) Cons_some

  let cond ?target none_branch some_branch : ('a option * 'r, 'b) code = fun bef ->
    let (a_opt, base) = unstack bef in
    let a = Types.assert_option a_opt in
    let target = Option.unopt ~default:(none_branch base).aft target in
    descr bef target (If_none (none_branch base, some_branch (stack a base)))

  let force_some ?msg : ('a option * 'r, 'a * 'r) code = fun s ->
    let (a_opt, base) = unstack s in
    let a = Types.assert_option a_opt in
    let target = a @: base in
    cond ~target
      (Stack_ops.failstring ("force_some : " ^ Option.unopt ~default:"" msg) target)
      Stack_ops.noop s
end

module Union_ops = struct
  open Script_typed_ir

  let left (b:'b ty) : ('a * 'r, ('a, 'b) union * 'r) code = fun bef ->
    let (a, base) = unstack bef in
    let aft = Types.union a b @: base in
    descr bef aft Left

  let right (a:'a ty) : ('b * 'r, ('a, 'b) union * 'r) code = fun bef ->
    let (b, base) = unstack bef in
    let aft = Types.union a b @: base in
    descr bef aft Right


  let loop ?after (code: ('a * 'r, ('a, 'b) union * 'r) code): (('a, 'b) union * 'r, 'b * 'r) code = fun bef ->
    let (union, base) = unstack bef in
    let (a, b) = Types.assert_union union in
    let code_stack = a @: base in
    let aft = Option.unopt ~default:(b @: base) after in
    descr bef aft (Loop_left (code code_stack))

end

module Arithmetic = struct
  let neq : (int_num * 'r, bool *'r) code = fun bef ->
    let aft = stack Types.bool @@ snd @@ unstack bef in
    descr bef aft Neq

  let neg : (int_num * 'r, int_num *'r) code = fun bef ->
    let aft = stack Types.int @@ snd @@ unstack bef in
    descr bef aft Neg_int

  let abs : (int_num * 'r, nat *'r) code = fun bef ->
    let aft = stack Types.nat @@ snd @@ unstack bef in
    descr bef aft Abs_int

  let int : (nat * 'r, int_num*'r) code = fun bef ->
    let aft = stack Types.int @@ snd @@ unstack bef in
    descr bef aft Int_nat

  let nat_opt : (int_num * 'r, nat option * 'r) code = fun bef ->
    let aft = stack Types.(option nat) @@ tail bef in
    descr bef aft Is_nat

  let nat_neq = fun s -> (int @| neq) s

  let add_natnat (bef : (nat * (nat * 'rest)) Stack.t) =
    let (nat, rest) = unstack bef in
    let rest = tail rest in
    let aft = stack nat rest in
    descr bef aft Add_natnat

  let add_intint (bef : (int_num * (int_num * 'rest)) Stack.t) =
    let (nat, rest) = unstack bef in
    let rest = tail rest in
    let aft = stack nat rest in
    descr bef aft Add_intint

  let add_teztez : (AC.Tez.tez * (AC.Tez.tez * 'rest), _) code = fun bef ->
    let aft = tail bef in
    descr bef aft Add_tez

  let mul_natnat (bef : (nat * (nat * 'rest)) Stack.t) =
    let nat = head bef in
    let rest = tail @@ tail bef in
    let aft = stack nat rest in
    descr bef aft Mul_natnat

  let mul_intint (bef : (int_num * (int_num * 'rest)) Stack.t) =
    let nat = head bef in
    let rest = tail @@ tail bef in
    let aft = stack nat rest in
    descr bef aft Mul_intint

  let sub_intint : (int_num * (int_num * 'r), int_num * 'r) code = fun bef ->
    let aft = tail bef in
    descr bef aft Sub_int

  let sub_natnat : (nat * (nat * 'r), int_num * 'r) code =
    fun bef -> bef <. int <: Stack_ops.dip int <: sub_intint

  let ediv : (nat * (nat * 'r), (nat * nat) option * 'r) code = fun s ->
    let (n, base) = unstack @@ snd @@ unstack s in
    let aft = Types.option (Types.pair n n) @: base in
    descr s aft Ediv_natnat

  let ediv_tez = fun s ->
    let aft = Types.(option @@ pair (head s) (head s)) @: tail @@ tail s in
    descr s aft Ediv_teznat

  open Option_ops
  let force_ediv x = x <. ediv <: force_some
  let force_ediv_tez x = (ediv_tez @| force_some) x

  open Pair_ops
  let div x = x <. force_ediv <: car

  open Stack_ops
  let div_n n s = s <. push_nat n <: swap <: div
  let add_n n s = s <. push_nat n <: swap <: add_natnat
  let add_teztez_n n s = s <. push_tez n <: swap <: add_teztez
  let sub_n n s = s <. push_nat n <: swap <: sub_natnat

  let force_nat s = s <. nat_opt <: force_some ~msg:"force nat"
end

module Boolean = struct
  let bool_and (type r) : (bool * (bool * r), bool * r) code = fun bef ->
    let aft = Types.bool @: tail @@ tail bef in
    descr bef aft And

  let bool_or (type r) : (bool * (bool * r), bool * r) code = fun bef ->
    let aft = Types.bool @: tail @@ tail bef in
    descr bef aft Or

  open Script_typed_ir
  let cond ?target true_branch false_branch : (bool * 'r, 's) code = fun bef ->
    let base = tail bef in
    let aft = Option.unopt ~default:((true_branch base).aft) target in
    descr bef aft (If (true_branch base, false_branch base))

  let loop (code : ('s, bool * 's) code) : ((bool * 's), 's) code = fun bef ->
    let aft = tail bef in
    descr bef aft @@ Loop (code aft)

end

module Comparison_ops = struct
  let cmp c_ty : _ code = fun bef ->
    let aft = stack Contract.Types.int @@ tail @@ tail @@ bef in
    descr bef aft (Compare c_ty)

  let cmp_bytes = fun x -> cmp (Bytes_key None) x

  let eq : (int_num * 'r, bool *'r) code = fun bef ->
    let aft = stack Contract.Types.bool @@ snd @@ unstack bef in
    descr bef aft Eq

  open Arithmetic
  let eq_n n s = s <. sub_n n <: eq

  let ge : (int_num * 'r, bool * 'r) code = fun bef ->
    let base = tail bef in
    let aft = stack Types.bool base in
    descr bef aft Ge

  let gt : (int_num * 'r, bool * 'r) code = fun bef ->
    let base = tail bef in
    let aft = stack Types.bool base in
    descr bef aft Gt

  let lt : (int_num * 'r, bool * 'r) code = fun bef ->
    let base = tail bef in
    let aft = stack Types.bool base in
    descr bef aft Lt

  let gt_nat s = s <. int <: gt

  open Stack_ops
  let assert_positive_nat s = s <. dup <: gt_nat <: Boolean.cond noop (failstring "positive" s)

  let cmp_ge_nat : (nat * (nat * 'r), bool * 'r) code = fun bef ->
    bef <. sub_natnat <: ge

  let cmp_ge_timestamp : (AC.Script_timestamp.t * (AC.Script_timestamp.t * 'r), bool * 'r) code = fun bef ->
    bef <. cmp Types.timestamp_k <: ge

  let assert_cmp_ge_nat : (nat * (nat * 'r), 'r) code = fun bef ->
    bef <. cmp_ge_nat <: Boolean.cond noop (failstring "assert cmp ge nat" (tail @@ tail bef))

  let assert_cmp_ge_timestamp : (AC.Script_timestamp.t * (AC.Script_timestamp.t * 'r), 'r) code = fun bef ->
    bef <. cmp_ge_timestamp <: Boolean.cond noop (failstring "assert cmp ge timestamp" (tail @@ tail bef))
end


module Bytes = struct

  open Script_typed_ir

  let pack (ty:'a ty) : ('a * 'r, bytes * 'r) code = fun bef ->
    let aft = stack Types.bytes @@ tail bef in
    descr bef aft (Pack ty)

  let unpack_opt : type a . a ty -> (bytes * 'r, a option * 'r) code = fun ty bef ->
    let aft = stack (Types.option ty) (tail bef) in
    descr bef aft (Unpack ty)

  let unpack ty s = s <. unpack_opt ty <: Option_ops.force_some

  let concat : (MBytes.t * (MBytes.t * 'rest), MBytes.t * 'rest) code = fun bef ->
    let aft = tail bef in
    descr bef aft Concat_bytes_pair

  let sha256 : (MBytes.t * 'rest, MBytes.t * 'rest) code = fun bef ->
    descr bef bef Sha256

  let blake2b : (MBytes.t * 'rest, MBytes.t * 'rest) code = fun bef ->
    descr bef bef Blake2b
end


module Map = struct
  open Script_typed_ir

  type ('a, 'b) t = ('a, 'b) map

  let empty c_ty = Script_ir_translator.empty_map c_ty
  let set (type a b) m (k:a) (v:b) = Script_ir_translator.map_update k (Some v) m

  module Ops = struct
    let update (bef : (('a * ('b option * (('a, 'b) map * ('rest)))) Stack.t)) : (_, ('a, 'b) map * 'rest) descr =
      let Item_t (_, Item_t (_, Item_t (map, rest, _), _), _) = bef in
      let aft = Item_t (map, rest, None) in
      descr bef aft Map_update

    let get : ?a:('a ty) -> 'b ty -> ('a * (('a, 'b) map * 'r), 'b option * 'r) code = fun ?a b bef ->
      let _ = a in
      let base = snd @@ unstack @@ snd @@ unstack bef in
      let aft = stack (Types.option b) base in
      descr bef aft Map_get

    let big_get : 'a ty -> 'b ty -> ('a * (('a, 'b) big_map * 'r), 'b option * 'r) code = fun _a b bef ->
      let base = snd @@ unstack @@ snd @@ unstack bef in
      let aft = stack (Types.option b) base in
      descr bef aft Big_map_get

    let big_update : ('a * ('b option * (('a, 'b) big_map * 'r)), ('a, 'b) big_map * 'r) code = fun bef ->
      let base = tail @@ tail bef in
      descr bef base Big_map_update
  end
end

module List_ops = struct
  let nil ele bef =
    let aft = stack (Types.list ele) bef in
    descr bef aft Nil

  let cons bef =
    let aft = tail bef in
    descr bef aft Cons_list

  let cond ~target cons_branch nil_branch bef =
    let (lst, aft) = unstack bef in
    let a = Types.assert_list lst in
    let cons_descr = cons_branch (a @: Types.list a @: aft) in
    let nil_descr = nil_branch aft in
    descr bef target (If_cons (cons_descr, nil_descr))

  let list_iter : type a r . (a * r, r) code -> (a list * r, r) code = fun code bef ->
    let (a_lst, aft) = unstack bef in
    let a = Types.assert_list a_lst in
    descr bef aft (List_iter (code (a @: aft)))

end

module Tez = struct

  let amount : ('r, AC.Tez.t * 'r) code = fun bef ->
    let aft = Types.mutez @: bef in
    descr bef aft Amount

  open Bytes

  let tez_nat s = s <. pack Types.mutez <: unpack Types.nat
  let amount_nat s = s <. amount <: pack Types.mutez <: unpack Types.nat
end

module Misc = struct

  open Stack_ops
  open Stack_shortcuts
  open Comparison_ops
  let min_nat : (nat * (nat * 'r), nat * 'r) code = fun s ->
    s <.
    keep_2 cmp_ge_nat <: bubble_2 <:
    Boolean.cond drop (dip drop)

  let debug ~msg () s = s <. push_string msg <: push_string "_debug" <: noop <: drop <: drop

  let debug_msg msg = debug ~msg ()

  let now : ('r, AC.Script_timestamp.t * 'r) code = fun bef ->
    let aft = stack Types.timestamp bef in
    descr bef aft Now

end



