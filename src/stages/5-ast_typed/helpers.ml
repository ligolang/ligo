module Location    = Simple_utils.Location
module List        = Simple_utils.List
module Ligo_string = Simple_utils.Ligo_string

open Types

let range i j =
  let rec aux i j acc = if i >= j then acc else aux i (j-1) (j-1 :: acc) in
  aux i j []

let label_range i j =
  List.map ~f:(fun i -> Label (string_of_int i)) @@ range i j

let is_tuple_lmap m =
  List.for_all ~f:(fun i -> LMap.mem i m) @@ (label_range 0 (LMap.cardinal m))

let get_pair m =
  match (LMap.find_opt (Label "0") m , LMap.find_opt (Label "1") m) with
  | Some {associated_type=e1;_}, Some {associated_type=e2;_} -> Some (e1,e2)
  | _ -> None

let tuple_of_record (m: _ LMap.t) =
  let aux i =
    let label = Label (string_of_int i) in
    let opt = LMap.find_opt (label) m in
    Option.bind ~f: (fun opt -> Some ((label,opt),i+1)) opt
  in
  Base.Sequence.to_list @@ Base.Sequence.unfold ~init:0 ~f:aux


let remove_empty_annotation (ann : string option) : string option =
  match ann with
  | Some "" -> None
  | Some ann -> Some ann
  | None -> None


(* This function transforms a type `∀ v1 ... vn . t` into the pair `([ v1 ; .. ; vn ] , t)` *)
let destruct_for_alls (t : type_expression) =
  let rec destruct_for_alls type_vars (t : type_expression) = match t.type_content with
    | T_for_all { ty_binder ; type_ ; _ } ->
       destruct_for_alls (ty_binder :: type_vars) type_
    | _ -> (type_vars, t)
  in destruct_for_alls [] t

(* This function transforms a type `t1 -> ... -> tn -> t` into the pair `([ t1 ; .. ; tn ] , t)` *)
let destruct_arrows_n (t : type_expression) (n : int) =
  let rec destruct_arrows type_vars (t : type_expression) = match t.type_content with
    | T_arrow { type1 ; type2 } when List.length type_vars < n ->
       destruct_arrows (type1 :: type_vars) type2
    | _ -> (type_vars, t)
  in destruct_arrows [] t

(* This function takes an expression l and a list of arguments [e1; ...; en] and constructs `l e1 ... en`,
   but it checks that types make sense (i.e. l has a function type with enough arguments) *)
let build_applications_opt (lamb : expression) (args : expression list) =
  let rec aux lamb' (args : expression list) (t : type_expression) = match args, t.type_content with
    | arg :: args', T_arrow { type1 = _; type2 }  ->
       aux (Combinators.make_e (E_application {lamb=lamb';args=arg}) type2) args' type2
    | [], _ ->
       Some {lamb' with type_expression = t}
    | _, _ ->
       None in
  aux lamb args lamb.type_expression

(* These tables are used during inference / for substitution *)
module TMap = Simple_utils.Map.Make(TypeVar)

(* Free type variables in a type *)
module VarSet = Caml.Set.Make(TypeVar)
let rec get_fv_type_expression : type_expression -> VarSet.t = fun u ->
  let self = get_fv_type_expression in
  match u.type_content with
  | T_variable v -> VarSet.singleton v
  | T_arrow { type1 ; type2 } ->
     let type1 = self type1 in
     let type2 = self type2 in
     VarSet.union type1 type2
  | T_abstraction { ty_binder ; kind = _ ; type_ } ->
     let type_ = self type_ in
     VarSet.remove ty_binder type_
  | T_for_all { ty_binder ; kind = _ ; type_ } ->
     let type_ = self type_ in
     VarSet.remove ty_binder type_
  | T_constant {language = _ ; injection = _ ; parameters} ->
     let parameters = List.map ~f:self parameters in
     List.fold_right ~f:VarSet.union ~init:VarSet.empty parameters
  | T_sum { content ; layout = _ } ->
     let content = List.map ~f:(fun { associated_type ; _ } -> self associated_type) @@ LMap.values content in
     List.fold_right ~f:VarSet.union ~init:VarSet.empty content
  | T_record { content ; layout = _ } ->
     let content = List.map ~f:(fun { associated_type ; _ } -> self associated_type) @@ LMap.values content in
     List.fold_right ~f:VarSet.union ~init:VarSet.empty content
  | _ -> VarSet.empty

(* Substitutes a type variable `v` for a type `t` in the type `u`. In
   principle, variables could be captured. But in case a binder
   (forall, abstraction) is found in `fv`, a new (fresh) binder is
   generated and subtituted to prevent capture. *)
let rec subst_type ?(fv = VarSet.empty) v t (u : type_expression) =
  let self = subst_type ~fv in
  match u.type_content with
  | T_variable v' when TypeVar.equal v v' -> t
  | T_arrow {type1;type2} ->
     let type1 = self v t type1 in
     let type2 = self v t type2 in
     { u with type_content = T_arrow {type1;type2} }
  | T_abstraction {ty_binder;kind;type_} when (VarSet.mem ty_binder fv) ->
     let ty_binder' = TypeVar.fresh () in
     let type_ = self ty_binder (Combinators.t_variable ty_binder' ()) type_ in
     let ty_binder = ty_binder' in
     self v t { u with type_content = T_abstraction {ty_binder;kind;type_} }
  | T_abstraction {ty_binder;kind;type_} when not (TypeVar.equal ty_binder v) ->
     let type_ = self v t type_ in
     { u with type_content = T_abstraction {ty_binder;kind;type_} }
  | T_for_all {ty_binder;kind;type_} when (VarSet.mem ty_binder fv) ->
     let ty_binder' = TypeVar.fresh () in
     let type_ = self ty_binder (Combinators.t_variable ty_binder' ()) type_ in
     let ty_binder = ty_binder' in
     self v t { u with type_content = T_for_all {ty_binder;kind;type_} }
  | T_for_all {ty_binder;kind;type_} when not (TypeVar.equal ty_binder v) ->
     let type_ = self v t type_ in
     { u with type_content = T_for_all {ty_binder;kind;type_} }
  | T_constant {language;injection;parameters} ->
     let parameters = List.map ~f:(self v t) parameters in
     { u with type_content = T_constant {language;injection;parameters} }
  | T_sum {content; layout} ->
     let content = LMap.map (fun {associated_type; michelson_annotation; decl_pos} : row_element ->
                       {associated_type = self v t associated_type; michelson_annotation;decl_pos}) content in
     { u with type_content = T_sum {content; layout} }
  | T_record {content; layout} ->
     let content = LMap.map (fun {associated_type; michelson_annotation; decl_pos} : row_element ->
                       {associated_type = self v t associated_type; michelson_annotation;decl_pos}) content in
     { u with type_content = T_record {content; layout} }
  | _ -> u

(* Substitution as `subst_type`, but does not capture variables in
   `t`, by using `fv` = free variables of `t`. *)
let subst_no_capture_type v t (u : type_expression) =
  let fv = get_fv_type_expression t in
  subst_type ~fv v t u

(* Parallel substitution, it takes a map of variables pointing to
   expressions. Variables can be captured. *)
let rec psubst_type t (u : type_expression) =
  let self = psubst_type t in
  match u.type_content with
  | T_variable v' -> (
     match TMap.find_opt v' t with
     | Some t -> t
     | None -> u
  )
  | T_arrow {type1;type2} ->
     let type1 = self type1 in
     let type2 = self type2 in
     { u with type_content = T_arrow {type1;type2} }
  | T_abstraction {ty_binder;kind;type_} when not (TMap.mem ty_binder t) ->
     let type_ = self type_ in
     { u with type_content = T_abstraction {ty_binder;kind;type_} }
  | T_for_all {ty_binder;kind;type_} when not (TMap.mem ty_binder t) ->
     let type_ = self type_ in
     { u with type_content = T_for_all {ty_binder;kind;type_} }
  | T_constant {language;injection;parameters} ->
     let parameters = List.map ~f:self parameters in
     { u with type_content = T_constant {language;injection;parameters} }
  | T_sum {content; layout} ->
     let content = LMap.map (fun {associated_type; michelson_annotation; decl_pos} : row_element ->
                       {associated_type = self associated_type; michelson_annotation;decl_pos}) content in
     { u with type_content = T_sum {content; layout} }
  | T_record {content; layout} ->
     let content = LMap.map (fun {associated_type; michelson_annotation; decl_pos} : row_element ->
                       {associated_type = self associated_type; michelson_annotation;decl_pos}) content in
     { u with type_content = T_record {content; layout} }
  | _ -> u
