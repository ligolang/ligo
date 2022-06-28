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


(* This function transforms a type `fun v1 ... vn . t` into the pair `([ v1 ; .. ; vn ] , t)` *)
let destruct_type_abstraction (t : type_expression) =
  let rec destruct_type_abstraction type_vars (t : type_expression) = match t.type_content with
    | T_abstraction { ty_binder ; type_ ; _ } ->
      destruct_type_abstraction (ty_binder :: type_vars) type_
    | _ -> (List.rev type_vars, t)
  in destruct_type_abstraction [] t

(* This function transforms a type `∀ v1 ... vn . t` into the pair `([ v1 ; .. ; vn ] , t)` *)
let destruct_for_alls (t : type_expression) =
  let rec destruct_for_alls type_vars (t : type_expression) = match t.type_content with
    | T_for_all { ty_binder ; type_ ; _ } ->
       destruct_for_alls (ty_binder :: type_vars) type_
    | _ -> (List.rev type_vars, t)
  in destruct_for_alls [] t

(* This function transforms a type `t1 -> ... -> tn -> t` into the pair `([ t1 ; .. ; tn ] , t)` *)
let destruct_arrows_n (t : type_expression) (n : int) =
  let rec destruct_arrows type_vars (t : type_expression) = match t.type_content with
    | T_arrow { type1 ; type2 } when List.length type_vars < n ->
       destruct_arrows (type1 :: type_vars) type2
    | _ -> (List.rev type_vars, t)
  in destruct_arrows [] t

(* This function transforms a type `t1 -> ... -> tn -> t` into the pair `([ t1 ; .. ; tn ] , t)` *)
let destruct_arrows (t : type_expression) =
  let rec destruct_arrows type_vars (t : type_expression) = match t.type_content with
    | T_arrow { type1 ; type2 } ->
       destruct_arrows (type1 :: type_vars) type2
    | _ -> (List.rev type_vars, t)
  in destruct_arrows [] t

let destruct_tuple (t : type_expression) =
  match t.type_content with
  | T_record { content ; _ } ->
     let f ({associated_type;_} : row_element) = associated_type in
     let fields = LMap.values content in
     let fields = List.map ~f fields in
     fields
  | _ -> [t]

let destruct_tuples (t : type_expression list) =
  List.concat_map ~f:destruct_tuple t

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

open Stage_common
module Pair = Simple_utils.Pair

type 'a fold_mapper = 'a -> expression -> bool * 'a * expression
let rec fold_map_expression : 'a fold_mapper -> 'a -> expression -> 'a * expression = fun f a e ->
  let self = fold_map_expression f in
  let (continue, init,e') = f a e in
  if (not continue) then (init,e')
  else
  let return expression_content = { e' with expression_content } in
  match e'.expression_content with
  | E_matching {matchee=e;cases} -> (
      let (res, e') = self init e in
      let (res,cases') = fold_map_cases f res cases in
      (res, return @@ E_matching {matchee=e';cases=cases'})
    )
  | E_record_accessor {record; path} -> (
      let (res, record) = self init record in
      (res, return @@ E_record_accessor {record; path})
    )
  | E_record m -> (
    let (res,m') = LMap.fold_map ~f:(fun _ e res -> self res e) ~init m in
    (res, return @@ E_record m')
  )
  | E_record_update {record; path; update} -> (
    let (res, record) = self init record in
    let (res, update) = self res update in
    (res, return @@ E_record_update {record;path;update})
  )
  | E_constructor c -> (
      let (res,e') = self init c.element in
      (res, return @@ E_constructor {c with element = e'})
  )
  | E_application {lamb;args} -> (
      let ab = (lamb, args) in
      let (res,(a,b)) = Pair.fold_map ~f:self ~init ab in
      (res, return @@ E_application {lamb=a;args=b})
    )
  | E_let_in { let_binder ; rhs ; let_result; attr } -> (
      let (res,rhs) = self init rhs in
      let (res,let_result) = self res let_result in
      (res, return @@ E_let_in { let_binder ; rhs ; let_result ; attr })
    )
  | E_mod_in { module_binder ; rhs ; let_result } -> (
    let (res,let_result) = self init let_result in
    let (res,rhs) = fold_map_expression_in_module_expr f res rhs in
    (res, return @@ E_mod_in { module_binder ; rhs ; let_result })
  )
  | E_type_inst { forall ; type_ } -> (
    let (res, forall) = self init forall in
    ( res, return @@ E_type_inst { forall ; type_ })
  )
  | E_lambda { binder ; result } -> (
      let (res,result) = self init result in
      ( res, return @@ E_lambda { binder ; result })
    )
  | E_type_abstraction ta -> (
      let res, ta = Fold_maps.type_abs self init ta in
      res, return @@ E_type_abstraction ta
    )
  | E_recursive { fun_name; fun_type; lambda={binder;result}} -> (
      let (res,result) = self init result in
      (res, return @@ E_recursive {fun_name; fun_type; lambda={binder;result}})
    )
  | E_constant c -> (
      let (res,args) = List.fold_map ~f:self ~init c.arguments in
      (res, return @@ E_constant {c with arguments=args})
    )
  | E_raw_code {language;code} -> (
    let (res,code) = self init code in
    (res, return @@ E_raw_code { language ; code }))
  | E_assign a ->
    let (res,a) = Fold_maps.assign self (fun a b -> a,b) init a in
    (res, return @@ E_assign a)
  | E_literal _ | E_variable _  | E_module_accessor _ as e' -> (init, return e')

and fold_map_cases : 'a fold_mapper -> 'a -> matching_expr -> 'a * matching_expr = fun f init m ->
  match m with
  | Match_variant {cases ; tv} -> (
      let aux init {constructor ; pattern ; body} =
        let (init, body) = fold_map_expression f init body in
        (init, {constructor; pattern ; body})
      in
      let (init,cases) = List.fold_map ~f:aux ~init cases in
      (init, Match_variant {cases ; tv})
    )
  | Match_record { fields; body; tv } ->
      let (init, body) = fold_map_expression f init body in
      (init, Match_record { fields ; body ; tv })

and fold_map_module : 'a fold_mapper -> 'a -> module_ -> 'a * module_ = fun m init p ->
  let aux = fun acc (x : declaration) ->
    match Location.unwrap x with
    | Declaration_constant {binder ; expr ; attr } -> (
      let (acc', expr) = fold_map_expression m acc expr in
      let wrap_content = Declaration_constant {binder ; expr ; attr} in
      (acc', {x with wrap_content})
    )
    | Declaration_type t -> (
      let wrap_content = Declaration_type t in
      (acc, {x with wrap_content})
    )
    | Declaration_module {module_binder; module_; module_attr} -> (
      let (acc', module_) = fold_map_expression_in_module_expr m acc module_ in
      let wrap_content = Declaration_module {module_binder; module_; module_attr} in
      (acc', {x with wrap_content})
    )
  in
  let (a,p) = List.fold_map ~f:aux ~init p in
  (a, p)

and fold_map_expression_in_module_expr : 'a fold_mapper -> 'a -> module_expr -> 'a * module_expr = fun fold_mapper acc x ->
  let return r wrap_content = (r, { x with wrap_content }) in
  match x.wrap_content with
  | M_struct decls ->
    let res,decls = fold_map_module fold_mapper acc decls in
    return res (M_struct decls)
  | M_module_path _ as x -> return acc x
  | M_variable _ as x -> return acc x

let rec fold_type_expression : type a . type_expression -> init:a -> f:(a -> type_expression -> a) -> a =
  fun te ~init ~f ->
    let self te = fold_type_expression te ~f in
    let init = f init te in
    match te.type_content with
    | T_variable _ -> init
    | T_constant {parameters; _} -> (
        List.fold parameters ~init ~f
      )
    | T_sum {content; _}
    | T_record {content; _} -> (
        LMap.fold (fun _ row acc -> self ~init:acc row.associated_type) content init
      )
    | T_arrow {type1; type2} -> (
        self type2 ~init:(self type1 ~init)
      )
    | T_singleton _ -> init
    | T_abstraction {type_; _}
    | T_for_all {type_; _} -> (
        self type_ ~init
      )


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
  end (* of module type S *)

  module Make (Ord : OrderedType) : IdMapSig with type key = Ord.t = struct
    module Map = Simple_utils.Map.Make(Ord)

    type key = Ord.t

    type 'a id_wrapped = {
      id : int; (* This is is used in [filter_values], to return a list of matching values in chronological order *)
      value : 'a
    }
    type 'a t = ('a id_wrapped) Map.t
    type 'a kvi_list = (key * 'a * int) list

    let empty = Map.empty

    let add : 'a t -> key -> 'a -> 'a t =
      fun map key value ->
        global_id := !global_id + 1;
        let id = !global_id in
        let id_value = { id ; value } in
        Map.add key id_value map

    let merge : 'a t -> 'a t -> 'a t =
      fun m1 m2 ->
        let merger : key -> 'a id_wrapped option -> 'a id_wrapped option -> 'a id_wrapped option =
          fun _ v1 v2 ->
            match (v1, v2) with
            | None,   None   -> None
            | Some v, None   -> Some v
            | None,   Some v -> Some v
            | Some v1, Some v2 ->
              if v1.id > v2.id then Some v1 else Some v2
        in
        Map.merge merger m1 m2

    let to_kvi_list : 'a t -> (key * 'a * int) list =
      fun map ->
        List.map ~f:(fun (key, value) -> (key, value.value, value.id)) @@ Map.to_kv_list map

    let sort_to_kv_list : (key * 'a * int) list -> (key * 'a) list =
      fun list ->
        let sorted_list = List.sort list ~compare:(fun (_, _, id1) (_, _, id2) -> Int.compare id2 id1) in
        List.map ~f:(fun (k, v, _) -> (k, v)) sorted_list

    let to_kv_list : 'a t -> (key * 'a) list =
      fun map ->
        sort_to_kv_list @@ to_kvi_list map

  end (* of module IdMap.Make*)

end (* of module IdMap *)

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
let add_shadowed_nested_t_sum = fun tsum_list (tv, te) ->
  let add_if_shadowed_t_sum :
    type_variable -> (type_variable * type_expression) list * bool -> type_expression -> (type_variable * type_expression) list * bool =
    fun shadower_tv (accu, is_top) te ->
      let ret x = (x, false) in
      match (te.type_content, te.orig_var) with
      | T_sum _, Some tv -> (
          if (TypeVar.equal tv shadower_tv) && (not is_top)
          then ret ((tv, te) :: accu)
          else ret accu
        )
      | T_sum _, None -> ret accu (* TODO : What should we do with those sum types with no binder ? *)
      | _ -> ret accu

  in
  let (nested_t_sums, _) : (type_variable * type_expression) list * bool =
    fold_type_expression
    te
    ~init:(tsum_list, true)
    ~f:(add_if_shadowed_t_sum tv)
  in
  (tv, te) :: nested_t_sums
