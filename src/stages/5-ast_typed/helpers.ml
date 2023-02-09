module Location = Simple_utils.Location
module List = Simple_utils.List
module Ligo_string = Simple_utils.Ligo_string
module Pair = Simple_utils.Pair
open Ligo_prim
open Types

let get_pair m =
  match Map.find m (Label.of_int 0), Map.find m (Label.of_int 1) with
  | Some e1, Some e2 -> Some (e1, e2)
  | _ -> None


let tuple_of_record (m : _ Record.t) =
  let aux i =
    let label = Label.of_int i in
    let opt = Record.find_opt m label in
    Option.bind ~f:(fun opt -> Some ((label, opt), i + 1)) opt
  in
  Base.Sequence.to_list @@ Base.Sequence.unfold ~init:0 ~f:aux


let remove_empty_annotation (ann : string option) : string option =
  match ann with
  | Some "" -> None
  | Some ann -> Some ann
  | None -> None


(* This function transforms a type `fun v1 ... vn . t` into the pair `([ v1 ; .. ; vn ] , t)` *)
let destruct_type_abstraction (t : type_expression) =
  let rec destruct_type_abstraction type_vars (t : type_expression) =
    match t.type_content with
    | T_abstraction { ty_binder; type_; _ } ->
      destruct_type_abstraction (ty_binder :: type_vars) type_
    | _ -> List.rev type_vars, t
  in
  destruct_type_abstraction [] t


(* This function transforms a type `∀ v1 ... vn . t` into the pair `([ v1 ; .. ; vn ] , t)` *)
let destruct_for_alls (t : type_expression) =
  let rec destruct_for_alls type_vars (t : type_expression) =
    match t.type_content with
    | T_for_all { ty_binder; type_; _ } ->
      destruct_for_alls (ty_binder :: type_vars) type_
    | _ -> List.rev type_vars, t
  in
  destruct_for_alls [] t


(* This function transforms a type `t1 -> ... -> tn -> t` into the pair `([ t1 ; .. ; tn ] , t)` *)
let destruct_arrows_n (t : type_expression) (n : int) =
  let rec destruct_arrows type_vars (t : type_expression) =
    match t.type_content with
    | T_arrow { type1; type2 } when List.length type_vars < n ->
      destruct_arrows (type1 :: type_vars) type2
    | _ -> List.rev type_vars, t
  in
  destruct_arrows [] t


(* This function transforms a type `t1 -> ... -> tn -> t` into the pair `([ t1 ; .. ; tn ] , t)` *)
let destruct_arrows (t : type_expression) =
  let rec destruct_arrows type_vars (t : type_expression) =
    match t.type_content with
    | T_arrow { type1; type2 } -> destruct_arrows (type1 :: type_vars) type2
    | _ -> List.rev type_vars, t
  in
  destruct_arrows [] t


let destruct_tuple (t : type_expression) =
  match t.type_content with
  | T_record row -> Row.to_tuple row
  | _ -> [ t ]


let destruct_tuples (t : type_expression list) = List.concat_map ~f:destruct_tuple t

(* This function takes an expression l and a list of arguments [e1; ...; en] and constructs `l e1 ... en`,
   but it checks that types make sense (i.e. l has a function type with enough arguments) *)
let build_applications_opt (lamb : expression) (args : expression list) =
  let rec aux lamb' (args : expression list) (t : type_expression) =
    match args, t.type_content with
    | arg :: args', T_arrow { type1 = _; type2 } ->
      let loc = Location.cover lamb'.location arg.location in
      aux
        (Combinators.make_e ~loc (E_application { lamb = lamb'; args = arg }) type2)
        args'
        type2
    | [], _ -> Some { lamb' with type_expression = t }
    | _, _ -> None
  in
  aux lamb args lamb.type_expression


(* This function re-builds a term prefixed with E_type_abstraction:
   given an expression e and a list of type variables [t1; ...; tn],
   it constructs an expression /\ t1 . ... . /\ tn . e *)
let rec build_type_abstractions e = function
  | [] -> e
  | abs_var :: abs_vars ->
    let e = build_type_abstractions e abs_vars in
    { e with
      expression_content = E_type_abstraction { type_binder = abs_var; result = e }
    ; type_expression =
        Combinators.t_for_all ~loc:e.location abs_var Type e.type_expression
    }


(* These tables are used during inference / for substitution *)
module TMap = Simple_utils.Map.Make (Type_var)

(* Free type variables in a type *)
module VarSet = Caml.Set.Make (Type_var)

let rec get_fv_type_expression : type_expression -> VarSet.t =
 fun u ->
  let self = get_fv_type_expression in
  match u.type_content with
  | T_variable v -> VarSet.singleton v
  | T_arrow { type1; type2 } ->
    let type1 = self type1 in
    let type2 = self type2 in
    VarSet.union type1 type2
  | T_abstraction { ty_binder; kind = _; type_ } ->
    let type_ = self type_ in
    VarSet.remove ty_binder type_
  | T_for_all { ty_binder; kind = _; type_ } ->
    let type_ = self type_ in
    VarSet.remove ty_binder type_
  | T_constant { language = _; injection = _; parameters } ->
    let parameters = List.map ~f:self parameters in
    List.fold_right ~f:VarSet.union ~init:VarSet.empty parameters
  | T_sum row | T_record row ->
    Row.fold (fun var_set type_ -> VarSet.union var_set (self type_)) VarSet.empty row
  | _ -> VarSet.empty


(* Substitutes a type variable `v` for a type `t` in the type `u`. In
   principle, variables could be captured. But in case a binder
   (forall, abstraction) is found in `fv`, a new (fresh) binder is
   generated and subtituted to prevent capture. *)
let rec subst_type ?(fv = VarSet.empty) v t (u : type_expression) =
  let self = subst_type ~fv in
  let loc = u.location in
  match u.type_content with
  | T_variable v' when Type_var.equal v v' -> t
  | T_arrow { type1; type2 } ->
    let type1 = self v t type1 in
    let type2 = self v t type2 in
    { u with type_content = T_arrow { type1; type2 } }
  | T_abstraction { ty_binder; kind; type_ } when VarSet.mem ty_binder fv ->
    let ty_binder' = Type_var.fresh ~loc () in
    let type_ = self ty_binder (Combinators.t_variable ~loc ty_binder' ()) type_ in
    let ty_binder = ty_binder' in
    self v t { u with type_content = T_abstraction { ty_binder; kind; type_ } }
  | T_abstraction { ty_binder; kind; type_ } when not (Type_var.equal ty_binder v) ->
    let type_ = self v t type_ in
    { u with type_content = T_abstraction { ty_binder; kind; type_ } }
  | T_for_all { ty_binder; kind; type_ } when VarSet.mem ty_binder fv ->
    let ty_binder' = Type_var.fresh ~loc () in
    let type_ = self ty_binder (Combinators.t_variable ~loc ty_binder' ()) type_ in
    let ty_binder = ty_binder' in
    self v t { u with type_content = T_for_all { ty_binder; kind; type_ } }
  | T_for_all { ty_binder; kind; type_ } when not (Type_var.equal ty_binder v) ->
    let type_ = self v t type_ in
    { u with type_content = T_for_all { ty_binder; kind; type_ } }
  | T_constant { language; injection; parameters } ->
    let parameters = List.map ~f:(self v t) parameters in
    { u with type_content = T_constant { language; injection; parameters } }
  | T_sum row ->
    let row = Row.map (self v t) row in
    { u with type_content = T_sum row }
  | T_record row ->
    let row = Row.map (self v t) row in
    { u with type_content = T_record row }
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
  | T_variable v' ->
    (match TMap.find_opt v' t with
    | Some t -> t
    | None -> u)
  | T_arrow { type1; type2 } ->
    let type1 = self type1 in
    let type2 = self type2 in
    { u with type_content = T_arrow { type1; type2 } }
  | T_abstraction { ty_binder; kind; type_ } when not (TMap.mem ty_binder t) ->
    let type_ = self type_ in
    { u with type_content = T_abstraction { ty_binder; kind; type_ } }
  | T_for_all { ty_binder; kind; type_ } when not (TMap.mem ty_binder t) ->
    let type_ = self type_ in
    { u with type_content = T_for_all { ty_binder; kind; type_ } }
  | T_constant { language; injection; parameters } ->
    let parameters = List.map ~f:self parameters in
    { u with type_content = T_constant { language; injection; parameters } }
  | T_sum row ->
    let row = Row.map self row in
    { u with type_content = T_sum row }
  | T_record row ->
    let row = Row.map self row in
    { u with type_content = T_record row }
  | _ -> u


type 'a fold_mapper = 'a -> expression -> bool * 'a * expression

let rec fold_map_expression : 'a fold_mapper -> 'a -> expression -> 'a * expression =
 fun f a e ->
  let self = fold_map_expression f in
  let self_type acc t = acc, t in
  let continue, init, e' = f a e in
  if not continue
  then init, e'
  else (
    let return expression_content = { e' with expression_content } in
    match e'.expression_content with
    | E_matching { matchee = e; cases } ->
      let res, e' = self init e in
      let res, cases' = fold_map_cases f res cases in
      res, return @@ E_matching { matchee = e'; cases = cases' }
    | E_accessor { struct_; path } ->
      let res, struct_ = self init struct_ in
      res, return @@ E_accessor { struct_; path }
    | E_record m ->
      let res, m' = Record.fold_map ~f:(fun res e -> self res e) ~init m in
      res, return @@ E_record m'
    | E_update { struct_; path; update } ->
      let res, struct_ = self init struct_ in
      let res, update = self res update in
      res, return @@ E_update { struct_; path; update }
    | E_constructor c ->
      let res, e' = self init c.element in
      res, return @@ E_constructor { c with element = e' }
    | E_application { lamb; args } ->
      let ab = lamb, args in
      let res, (a, b) = Pair.fold_map ~f:self ~init ab in
      res, return @@ E_application { lamb = a; args = b }
    | E_let_in { let_binder; rhs; let_result; attributes } ->
      let res, rhs = self init rhs in
      let res, let_result = self res let_result in
      res, return @@ E_let_in { let_binder; rhs; let_result; attributes }
    | E_mod_in { module_binder; rhs; let_result } ->
      let res, let_result = self init let_result in
      let res, rhs = fold_map_expression_in_module_expr f res rhs in
      res, return @@ E_mod_in { module_binder; rhs; let_result }
    | E_type_inst { forall; type_ } ->
      let res, forall = self init forall in
      res, return @@ E_type_inst { forall; type_ }
    | E_lambda l ->
      let res, l = Lambda.fold_map self self_type init l in
      res, return @@ E_lambda l
    | E_type_abstraction ta ->
      let res, ta = Type_abs.fold_map self init ta in
      res, return @@ E_type_abstraction ta
    | E_recursive r ->
      let res, r = Recursive.fold_map self self_type init r in
      res, return @@ E_recursive r
    | E_constant c ->
      let res, args = List.fold_map ~f:self ~init c.arguments in
      res, return @@ E_constant { c with arguments = args }
    | E_raw_code { language; code } ->
      let res, code = self init code in
      res, return @@ E_raw_code { language; code }
    | E_assign a ->
      let res, a = Assign.fold_map self self_type init a in
      res, return @@ E_assign a
    | E_let_mut_in { let_binder; rhs; let_result; attributes } ->
      let res, rhs = self init rhs in
      let res, let_result = self res let_result in
      res, return @@ E_let_mut_in { let_binder; rhs; let_result; attributes }
    | E_for f ->
      let res, f = For_loop.fold_map self init f in
      res, return @@ E_for f
    | E_for_each fe ->
      let res, fe = For_each_loop.fold_map self init fe in
      res, return @@ E_for_each fe
    | E_while w ->
      let res, w = While_loop.fold_map self init w in
      res, return @@ E_while w
    | (E_deref _ | E_literal _ | E_variable _ | E_module_accessor _) as e' ->
      init, return e')


and fold_map_case
    :  'a fold_mapper -> 'a -> (expression, type_expression) Types.Match_expr.match_case
    -> 'a * (expression, type_expression) Types.Match_expr.match_case
  =
 fun f init { pattern; body } ->
  let init, body = fold_map_expression f init body in
  init, { pattern; body }


and fold_map_cases
    :  'a fold_mapper -> 'a
    -> (expression, type_expression) Types.Match_expr.match_case list
    -> 'a * (expression, type_expression) Types.Match_expr.match_case list
  =
 fun f init ms -> List.fold_map ms ~init ~f:(fold_map_case f)


and fold_map_declaration m acc (x : declaration) =
  match Location.unwrap x with
  | D_value { binder; expr; attr } ->
    let acc', expr = fold_map_expression m acc expr in
    let wrap_content = D_value { binder; expr; attr } in
    acc', { x with wrap_content }
  | D_irrefutable_match { pattern; expr; attr } ->
    let acc', expr = fold_map_expression m acc expr in
    let wrap_content = D_irrefutable_match { pattern; expr; attr } in
    acc', { x with wrap_content }
  | D_type t ->
    let wrap_content = D_type t in
    acc, { x with wrap_content }
  | D_module { module_binder; module_; module_attr } ->
    let acc', module_ = (fold_map_expression_in_module_expr m) acc module_ in
    let wrap_content = D_module { module_binder; module_; module_attr } in
    acc', { x with wrap_content }


and fold_map_decl m = fold_map_declaration m

and fold_map_module : 'a fold_mapper -> 'a -> module_ -> 'a * module_ =
 fun m init -> List.fold_map ~f:(fold_map_decl m) ~init


and fold_map_expression_in_module_expr
    : 'a fold_mapper -> 'a -> module_expr -> 'a * module_expr
  =
 fun fold_mapper acc x ->
  let return r wrap_content = r, { x with wrap_content } in
  match x.wrap_content with
  | M_struct decls ->
    let res, decls = fold_map_module fold_mapper acc decls in
    return res (Module_expr.M_struct decls)
  | M_module_path _ as x -> return acc x
  | M_variable _ as x -> return acc x


let fold_map_program : 'a fold_mapper -> 'a -> program -> 'a * program =
 fun m init -> List.fold_map ~f:(fold_map_declaration m) ~init


let rec fold_type_expression
    : type a. type_expression -> init:a -> f:(a -> type_expression -> a) -> a
  =
 fun te ~init ~f ->
  let self te = fold_type_expression te ~f in
  let init = f init te in
  match te.type_content with
  | T_variable _ -> init
  | T_constant { parameters; _ } -> List.fold parameters ~init ~f
  | T_sum row | T_record row -> Row.fold f init row
  | T_arrow { type1; type2 } -> self type2 ~init:(self type1 ~init)
  | T_singleton _ -> init
  | T_abstraction { type_; _ } | T_for_all { type_; _ } -> self type_ ~init


(* An [IdMap] is a [Map] augmented with an [id] field (which is wrapped around the map [value] field).
  Using a map instead of a list makes shadowed modules inaccessible,
  since they are overwritten from the map when adding the shadower, whilst they were kept when using lists.
  The [id] field in the map values is used to infer the type to which a constructor belong when they are not annotated
  e.g. we need to keep the declaration order to infer that 'c' has type z in:

    type x = A of int | B
    module M = struct
      type y = A of int
    end
    type z = A of int | AA

    let c = A 2
*)

let global_id = ref 0

module IdMap = struct
  module type OrderedType = Caml.Map.OrderedType

  module type IdMapSig = sig
    type key
    type 'a t
    type 'a kvi_list = (key * 'a * int) list

    val empty : 'a t
    val add : 'a t -> key -> 'a -> 'a t

    (* In case of merge conflict between two values with same keys, this merge function keeps the value with the highest id.
        This follows the principle that this map always keeps the latest value in case of conflict *)
    val merge : 'a t -> 'a t -> 'a t

    (* Converts the map into an unsorted (key * value * id) 3-uple *)
    val to_kvi_list : 'a t -> (key * 'a * int) list

    (* Converts the kvi_list into a id-sorted kv_list *)
    val sort_to_kv_list : (key * 'a * int) list -> (key * 'a) list
    val to_kv_list : 'a t -> (key * 'a) list
  end
  (* of module type S *)

  module Make (Ord : OrderedType) : IdMapSig with type key = Ord.t = struct
    module Map = Simple_utils.Map.Make (Ord)

    type key = Ord.t

    type 'a id_wrapped =
      { id : int
            (* This is is used in [filter_values], to return a list of matching values in chronological order *)
      ; value : 'a
      }

    type 'a t = 'a id_wrapped Map.t
    type 'a kvi_list = (key * 'a * int) list

    let empty = Map.empty

    let add : 'a t -> key -> 'a -> 'a t =
     fun map key value ->
      global_id := !global_id + 1;
      let id = !global_id in
      let id_value = { id; value } in
      Map.add key id_value map


    let merge : 'a t -> 'a t -> 'a t =
     fun m1 m2 ->
      let merger
          : key -> 'a id_wrapped option -> 'a id_wrapped option -> 'a id_wrapped option
        =
       fun _ v1 v2 ->
        match v1, v2 with
        | None, None -> None
        | Some v, None -> Some v
        | None, Some v -> Some v
        | Some v1, Some v2 -> if v1.id > v2.id then Some v1 else Some v2
      in
      Map.merge merger m1 m2


    let to_kvi_list : 'a t -> (key * 'a * int) list =
     fun map ->
      List.map ~f:(fun (key, value) -> key, value.value, value.id) @@ Map.to_kv_list map


    let sort_to_kv_list : (key * 'a * int) list -> (key * 'a) list =
     fun list ->
      let sorted_list =
        List.sort list ~compare:(fun (_, _, id1) (_, _, id2) -> Int.compare id2 id1)
      in
      List.map ~f:(fun (k, v, _) -> k, v) sorted_list


    let to_kv_list : 'a t -> (key * 'a) list =
     fun map -> sort_to_kv_list @@ to_kvi_list map
  end
  (* of module IdMap.Make*)
end
(* of module IdMap *)

(*
    Add the shadowed t_sum types nested in the fetched types.

    After using [get_modules_types], we have the ctxt types, i.e. all types declared current scope and submodules.
    There is no shadowed type in ctxt types (since ctxt is a map, shadowed types are removed when adding the shadower).
    However we want shadowed types when they are nested in another type :
      type a = Foo of int | Bar of string
      type a = a list
    Here, we want [Foo of int | Bar of string] to be found
    But we want to add nested t_sum types _only_ if they are shadowed, we don't want to add them in that case for example :
      type foo_variant = Foo of int | Bar of string
      type foo_record = { foo : foo_variant ; bar : foo_variant}
    Because [foo_variant] would appear three times in the list instead of one.

    NOTE : We could append nested types on top of the [module_types] list we have so far,
    but having a final list with all nested-types before toplevel types triggers some errors.

    NOTE : We can't just do a final id-sort of the list to have everything in declarartion order
    because the fetched nested types don't have id, only the ones retrieved from the ctxt do.

    So, if we have ctxt types :
      [t1; t2; t3]
    After adding the shadowed t_sums, we want the final list :
      [t1; tsum_shadowed_by_t1; t2; tsum_shadowed_by_t2; t3; tsum_shadowed_by_t3]

    NOTE : When [fold_type_expression] is used on t1, it will add tsum types nested in t1,
    but it might also add t1 (or not), we don't know.
    However, we want to make sure t1 is in the final list *exactly once*.
      - If it's not here, we'll lose a type and have incorrect "type [t1] not found" errors
      - If it's here more than once, we'll have a false "warning, [t1] inferred but could also be of type [t1]"
    To ensure [t1] appears once exactly, we tweak the fold function by passing a [is_top] boolean
    to ensure it will fold over all nested type in [t1] but not the toplevel one (i.e. [t1]),
    we then add [t1] manually to the list.
*)
let add_shadowed_nested_t_sum tsum_list (tv, te) =
  let add_if_shadowed_t_sum
      :  Type_var.t -> (Type_var.t * type_expression) list * bool -> type_expression
      -> (Type_var.t * type_expression) list * bool
    =
   fun shadower_tv (accu, is_top) te ->
    let ret x = x, false in
    match te.type_content, te.orig_var with
    | T_sum _, Some tv ->
      if Type_var.equal tv shadower_tv && not is_top
      then ret ((tv, te) :: accu)
      else ret accu
    | T_sum _, None ->
      ret accu (* TODO : What should we do with those sum types with no binder ? *)
    | _ -> ret accu
  in
  let (nested_t_sums, _) : (Type_var.t * type_expression) list * bool =
    fold_type_expression te ~init:(tsum_list, true) ~f:(add_if_shadowed_t_sum tv)
  in
  (tv, te) :: nested_t_sums


(* get_views [p] looks for top-level declaration annotated with [@view] in program [p] and return declaration data *)
let get_views : program -> (Value_var.t * Location.t) list =
 fun p ->
  let f
      : declaration -> (Value_var.t * Location.t) list -> (Value_var.t * Location.t) list
    =
   fun { wrap_content = decl; location = _ } acc ->
    match decl with
    | D_value { binder; expr = _; attr } when attr.view ->
      let var = Binder.get_var binder in
      (var, Value_var.get_location var) :: acc
    | D_irrefutable_match { pattern = { wrap_content = P_var binder; _ }; expr = _; attr }
      when attr.view ->
      let var = Binder.get_var binder in
      (var, Value_var.get_location var) :: acc
    (* TODO: exhaustive here ... *)
    | D_type _ | D_module _ | D_value _ | D_irrefutable_match _ -> acc
  in
  List.fold_right ~init:[] ~f p


let fetch_view_type : declaration -> (type_expression * type_expression Binder.t) option =
 fun declt ->
  match Location.unwrap declt with
  | D_value { binder; expr; attr }
  | D_irrefutable_match { pattern = { wrap_content = P_var binder; _ }; expr; attr }
    when attr.view ->
    Some (expr.type_expression, Binder.map (fun _ -> expr.type_expression) binder)
  | D_value _ | D_irrefutable_match _ | D_type _ | D_module _ -> None


(* Wrap a variable `f` of type `parameter -> storage -> return`
   to an expression `fun (p, s) -> f p s : parameter * storage -> return` *)
let uncurry_wrap ~loc ~type_ var =
  let open Combinators in
  let open Simple_utils.Option in
  let* { type1 = input_ty; type2 = output_ty } = get_t_arrow type_ in
  let* { type1 = storage; type2 = output_ty } = get_t_arrow output_ty in
  (* We create a wrapper to uncurry it: *)
  let parameter = input_ty in
  let p_var = Value_var.fresh ~loc ~name:"parameter" () in
  let s_var = Value_var.fresh ~loc ~name:"storage" () in
  let ps_var = Value_var.fresh ~loc ~name:"input" () in
  let p_binder = Binder.make p_var parameter in
  let s_binder = Binder.make s_var storage in
  let ps_param = Param.make ps_var (t_pair ~loc parameter storage) in
  let p_expr = e_a_variable ~loc p_var parameter in
  let s_expr = e_a_variable ~loc s_var storage in
  let ps_expr = e_a_variable ~loc ps_var (t_pair ~loc parameter storage) in
  (* main(p) *)
  let expr =
    e_a_application
      ~loc
      (e_a_variable ~loc var type_)
      p_expr
      (t_arrow ~loc storage output_ty ())
  in
  (* main(p)(s) *)
  let expr = e_a_application ~loc expr s_expr output_ty in
  (* match ps with (p, s) -> main(p)(s) *)
  let expr =
    e_a_matching
      ~loc:Location.generated
      ps_expr
      [ { pattern =
            Location.wrap
              ~loc:Location.generated
              Pattern.(
                P_tuple
                  [ Location.wrap ~loc:Location.generated @@ P_var p_binder
                  ; Location.wrap ~loc:Location.generated @@ P_var s_binder
                  ])
        ; body = expr
        }
      ]
      output_ty
  in
  (* fun ps -> match ps with (p, s) -> main(p)(s) *)
  let expr =
    e_a_lambda
      ~loc
      { binder = ps_param; output_type = output_ty; result = expr }
      (t_pair ~loc parameter storage)
      output_ty
  in
  some @@ expr


let should_curry_view view_ty =
  match Combinators.get_t_arrow view_ty with
  | Some { type1 = tin; type2 = return } ->
    (match Combinators.get_t_tuple tin with
    | Some [ arg; storage ] -> `No (arg, storage, return)
    | _ ->
      (match Combinators.get_t_arrow return with
      | Some { type1 = storage; type2 = return } -> `Yes (tin, storage, return)
      | None -> `Bad))
  | None -> `Bad


let fetch_views_in_program
    : program -> program * (type_expression * type_expression Binder.t) list
  =
 fun prog ->
  let aux declt ((prog, views) : program * _) =
    let return () = declt :: prog, views in
    let loc = Location.get_location declt in
    match Location.unwrap declt with
    | D_value ({ binder; expr; attr } as dvalue) when attr.view ->
      let var = Binder.get_var binder in
      (match should_curry_view expr.type_expression with
      | `Yes _ ->
        let expr =
          Option.value_exn @@ uncurry_wrap ~loc ~type_:expr.type_expression var
        in
        let binder = Binder.set_var binder (Value_var.fresh_like var) in
        let binder = Binder.set_ascr binder expr.type_expression in
        (* Add both `main` and the new `main#FRESH` version that calls `main` but it's curried *)
        ( (Location.wrap ~loc:declt.location @@ D_value dvalue)
          :: (Location.wrap ~loc:declt.location @@ D_value { dvalue with binder; expr })
          :: prog
        , (expr.type_expression, Binder.map (fun _ -> expr.type_expression) binder)
          :: views )
      | `No _ | `Bad ->
        ( (Location.wrap ~loc:declt.location @@ D_value dvalue) :: prog
        , (expr.type_expression, Binder.map (fun _ -> expr.type_expression) binder)
          :: views ))
    | D_irrefutable_match
        ({ pattern = { wrap_content = P_var binder; _ } as pattern; expr; attr } as
        dirref)
      when attr.view ->
      let var = Binder.get_var binder in
      (match should_curry_view expr.type_expression with
      | `Yes _ ->
        let expr =
          Option.value_exn @@ uncurry_wrap ~loc ~type_:expr.type_expression var
        in
        let binder = Binder.set_var binder (Value_var.fresh_like var) in
        let binder = Binder.set_ascr binder expr.type_expression in
        let pattern = Pattern.{ pattern with wrap_content = P_var binder } in
        (* Add both `main` and the new `main#FRESH` version that calls `main` but it's curried *)
        ( (Location.wrap ~loc:declt.location @@ D_irrefutable_match dirref)
          :: (Location.wrap ~loc:declt.location
             @@ D_irrefutable_match { dirref with expr; pattern })
          :: prog
        , (expr.type_expression, Binder.map (fun _ -> expr.type_expression) binder)
          :: views )
      | `No _ | `Bad ->
        ( (Location.wrap ~loc:declt.location @@ D_irrefutable_match dirref) :: prog
        , (expr.type_expression, Binder.map (fun _ -> expr.type_expression) binder)
          :: views ))
    | D_irrefutable_match _ | D_type _ | D_module _ | D_value _ -> return ()
  in
  List.fold_right ~f:aux ~init:([], []) prog
