open Main_errors

module Core = Typesystem.Core
open Ast_typed.Types
(* open Typesystem.Solver_types *)
open Trace
(* open Typer_common.Errors *)
module Map = RedBlackTrees.PolyMap
module Set = RedBlackTrees.PolySet

(* TODO: use the rope from the heuristic instead *)
type 'a tree = Leaf of 'a | Empty | Pair of 'a tree * 'a tree

(* For comparisons, None means different, Some tree means equality
   modulo variables and flattening the tree gives the pairs of type
   variables in the order in which they appear. *)

let (<?) ca cb =
  match ca with
  | Some t1 ->
    (match cb () with
     | Some t2 -> Some (Pair (t1, t2))
     | None -> None)
  | None -> None

type 'a cmp = 'a -> 'a -> (type_variable * type_variable) tree option

let use_generated g : 'a cmp = fun expected actual ->
  if g expected actual = 0 then Some Empty else None

let list ~compare : 'a cmp = fun expected actual ->
  if List.compare_lengths expected actual != 0
  then None
  else  List.fold_left2 (function
      | None -> fun _ _ -> None
      | Some t1 -> fun a b ->
        match compare a b with
        | None -> None
        | Some t2 -> Some (Pair (t1, t2)))
      (Some Empty)
      expected
      actual

let rec c_equation : c_equation cmp = fun expected actual ->
  let { aval=a1; bval=a2 } = expected in
  let { aval=b1; bval=b2 } = actual in
  type_value a1 b1 <? fun () -> type_value a2 b2

and tc_allowed : tc_allowed cmp = fun expected actual ->
  list ~compare:type_value expected actual

and typeclass : typeclass cmp = fun expected actual ->
  list ~compare:tc_allowed expected actual

and tc_args : tc_args cmp = fun expected actual ->
  list ~compare:type_value expected actual

and c_typeclass : c_typeclass cmp = fun expected actual ->
  let { tc_args=a1; typeclass=a2 } = expected in
  let { tc_args=b1; typeclass=b2 } = actual in
  tc_args a1 b1 <? fun () -> typeclass a2 b2

and c_access_label : c_access_label cmp = fun expected actual ->
  let { c_access_label_tval=a1; accessor=a2; c_access_label_tvar=a3 } = expected in
  let { c_access_label_tval=b1; accessor=b2; c_access_label_tvar=b3 } = actual in
  type_value a1 b1 <? fun () -> use_generated Ast_typed.Compare.label a2 b2 <? fun () -> type_variable a3 b3

and type_constaint_ : type_constraint_ cmp = fun expected actual ->
  match expected, actual with
  | Ast_typed.Types.C_equation     a , Ast_typed.Types.C_equation     b -> c_equation a b
  | Ast_typed.Types.C_typeclass    a , Ast_typed.Types.C_typeclass    b -> c_typeclass a b
  | Ast_typed.Types.C_access_label a , Ast_typed.Types.C_access_label b -> c_access_label a b
  | (a, b) ->
    let different = use_generated Ast_typed.Compare.type_constraint_ a b in
    assert (match different with None -> true | _ -> false); different
and type_constraint : type_constraint cmp = fun expected actual ->
  let { reason=_; c=a1 } = expected in
  let { reason=_; c=b1 } = actual in
  type_constaint_ a1 b1

and p_constraints : p_constraints cmp = fun expected actual ->
  list ~compare:type_constraint expected actual

and type_variable : type_variable cmp = fun expected actual ->
  (* We compare type variables during a later pass. *)
  Some ( Leaf ( expected , actual ) )

and p_forall : p_forall cmp = fun expected actual ->
  let { binder=_; constraints=a1; body=a2 } = expected in
  let { binder=_; constraints=b1; body=b2 } = actual in
  p_constraints a1 b1 <? fun () ->
    type_value a2 b2

and p_ctor_args : p_ctor_args cmp = fun expected actual ->
  list ~compare:type_value expected actual

and p_constant : p_constant cmp = fun expected actual ->
  let { p_ctor_tag = a1; p_ctor_args = a2 } = expected in
  let { p_ctor_tag = b1; p_ctor_args = b2 } = actual in
  use_generated Ast_typed.Compare.constant_tag a1 b1 <? fun () ->
    p_ctor_args a2 b2

and p_apply : p_apply cmp = fun expected actual ->
  let { tf=a1; targ=a2 } = expected in
  let { tf=b1; targ=b2 } = actual in
  type_value a1 b1 <? fun () ->
    type_value a2 b2

and lmap_type_value_pair : (label * type_value) cmp = fun expected actual ->
  let a1, a2 = expected in
  let b1, b2 = actual in
  use_generated Ast_typed.Compare.label a1 b1 <? fun () -> type_value a2 b2

and p_row : p_row cmp = fun expected actual ->
  let { p_row_tag = a1; p_row_args = a2 } = expected in
  let { p_row_tag = b1; p_row_args = b2 } = actual in
  use_generated Ast_typed.Compare.row_tag a1 b1 <? fun () ->
    list ~compare:lmap_type_value_pair (LMap.to_kv_list a2) (LMap.to_kv_list b2)

and type_value_ : type_value_ cmp = fun expected actual ->
  match expected, actual with
  | (Ast_typed.Types.P_forall   a , Ast_typed.Types.P_forall   b) -> p_forall a b
  | (Ast_typed.Types.P_variable a , Ast_typed.Types.P_variable b) -> type_variable a b
  | (Ast_typed.Types.P_constant a , Ast_typed.Types.P_constant b) -> p_constant a b
  | (Ast_typed.Types.P_apply    a , Ast_typed.Types.P_apply    b) -> p_apply a b
  | (Ast_typed.Types.P_row      a , Ast_typed.Types.P_row      b) -> p_row a b
  | (a, b) ->
    let different = use_generated Ast_typed.Compare.type_value_ a b in
    assert (match different with None -> true | _ -> false); different

and type_value : type_value cmp = fun expected actual ->
  let { tsrc=_; t=a1 } = expected in
  let { tsrc=_; t=b1 } = actual in
  type_value_ a1 b1

and type_variable_list : type_variable_list cmp = fun expected actual ->
  list ~compare:type_variable expected actual

and c_typeclass_simpl : c_typeclass_simpl cmp = fun expected actual ->
  let { reason_typeclass_simpl=_; tc=a1; args=a2 } = expected in
  let { reason_typeclass_simpl=_; tc=b1; args=b2 } = actual in
  typeclass a1 b1 <? fun () -> type_variable_list a2 b2

and c_constructor_simpl : c_constructor_simpl cmp = fun expected actual ->
  let { reason_constr_simpl=_; tv=a1; c_tag=a2; tv_list=a3 } = expected in
  let { reason_constr_simpl=_; tv=b1; c_tag=b2; tv_list=b3 } = actual in
  type_variable a1 b1 <? fun () ->
    use_generated Ast_typed.Compare.constant_tag a2 b2 <? fun () ->
      type_variable_list a3 b3

and deduce_and_clean_result : deduce_and_clean_result cmp = fun expected actual ->
  let { deduced = a1 ; cleaned = a2 } = expected in
  let { deduced = b1 ; cleaned = b2 } = actual in
  c_constructor_simpl_list a1 b1 <? fun () -> c_typeclass_simpl a2 b2

and c_constructor_simpl_list : c_constructor_simpl_list cmp = fun expected actual ->
  list ~compare:c_constructor_simpl expected actual

let rec flatten_tree : _ tree -> _ list -> _ list = fun t acc ->
  match t with
  | Pair (Leaf a,     rest) -> flatten_tree rest                         (a :: acc)
  | Pair (Empty,      rest) -> flatten_tree rest                               acc
  | Pair (Pair (l,r), rest) -> flatten_tree (Pair (l, (Pair (r, rest))))       acc
  | Leaf a                  ->                                            a :: acc
  | Empty                   ->                                                 acc

let flatten_tree : _ tree -> _ list = fun t -> List.rev @@ flatten_tree t []

let compare_and_check_vars = fun ~(compare : 'a cmp) ~print_whole whole_expected whole_actual ->
  let aux seen (expected,actual) =
    match Map.find_opt expected seen with
      None -> ok @@ Map.add expected actual seen
    | Some substitution ->
      if Ast_typed.Compare.type_variable actual substitution = 0
      then ok seen            (* we saw the same substitution for the same expected variable, all fine  *)
      else fail (test_internal_msg __LOC__
                 @@ Format.asprintf "expected (unification?) type variable %a but got %a, while comparing the expected test result %a with the actual %a"
                   Ast_typed.PP.type_variable substitution
                   Ast_typed.PP.type_variable actual
                   print_whole whole_expected
                   print_whole whole_actual)

  in
  match compare whole_expected whole_actual with
  | None ->
    fail (test_internal_msg __LOC__
          @@ Format.asprintf "expected test result\n%a\nbut got actual\n%a\n"
            print_whole whole_expected
            print_whole whole_actual)
  | Some t ->
    let%bind _seen = bind_fold_list aux
        (RedBlackTrees.PolyMap.create ~cmp:Ast_typed.Compare.type_variable)
        (flatten_tree t)
    in ok ()

let compare_and_check_vars_deduce_and_clean_result expected actual =
  compare_and_check_vars
    ~compare:deduce_and_clean_result
    ~print_whole:Ast_typed.PP.deduce_and_clean_result
    expected
    actual
