open Trace

module Core = Typesystem.Core

module Wrap = struct
  module I = Ast_simplified
  module O = Core

  type constraints = O.type_constraint list

  (* let add_type state t = *)
  (*   let constraints = Wrap.variable type_name t in *)
  (*   let%bind state' = aggregate_constraints state constraints in *)
  (*   ok state' in *)
  (* let return_add_type ?(state = state) expr t = *)
  (*   let%bind state' = add_type state t in *)
  (*   return expr state' in *)

  let rec type_expression_to_type_value : I.type_expression -> O.type_value = fun te ->
    match te with
    | T_tuple types ->
      P_constant (C_tuple, List.map type_expression_to_type_value types)
    | T_sum kvmap ->
      P_constant (C_variant, Map.String.to_list @@ Map.String.map type_expression_to_type_value kvmap)
    | T_record kvmap ->
      P_constant (C_record, Map.String.to_list @@ Map.String.map type_expression_to_type_value kvmap)
    | T_function (arg , ret) ->
      P_constant (C_arrow, List.map type_expression_to_type_value [ arg ; ret ])
    | T_variable type_name -> P_variable type_name
    | T_constant (type_name , args) ->
      let csttag = Core.(match type_name with
          | "arrow"  -> C_arrow
          | "option" -> C_option
          | "tuple"  -> C_tuple
          | "map"    -> C_map
          | "list"   -> C_list
          | "set"    -> C_set
          | "unit"   -> C_unit
          | "bool"   -> C_bool
          | "string" -> C_string
          | _        -> failwith "TODO")
      in
      P_constant (csttag, List.map type_expression_to_type_value args)

  (** TODO *)
  let type_declaration : I.declaration -> constraints = fun td ->
    match td with
    | Declaration_type (name , te) ->
      let pattern = type_expression_to_type_value te in
      [C_equation (P_variable (name) , pattern)] (* TODO: this looks wrong. If this is a type declaration, it should not set any constraints. *)
    | Declaration_constant (name, te, _) ->(
        match te with 
        | Some (exp) ->
          let pattern = type_expression_to_type_value exp in
          [C_equation (P_variable (name) , pattern)] (* TODO: this looks wrong. If this is a type declaration, it should not set any constraints. *)
        | None -> 
          (** TODO *)
          [] 
      )

  (* TODO: this should be renamed to failwith_ *)
  let failwith : unit -> (constraints * O.type_variable) = fun () ->
    let type_name = Core.fresh_type_variable () in
    [] , type_name

  let variable : I.name -> I.type_expression -> (constraints * O.type_variable) = fun _name expr ->
    let pattern = type_expression_to_type_value expr in
    let type_name = Core.fresh_type_variable () in
    [C_equation (P_variable (type_name) , pattern)] , type_name

  let literal : I.type_expression -> (constraints * O.type_variable) = fun t ->
    let pattern = type_expression_to_type_value t in
    let type_name = Core.fresh_type_variable () in
    [C_equation (P_variable (type_name) , pattern)] , type_name

  (*
  let literal_bool : unit -> (constraints * O.type_variable) = fun () ->
    let pattern = type_expression_to_type_value I.t_bool in
    let type_name = Core.fresh_type_variable () in
    [C_equation (P_variable (type_name) , pattern)] , type_name

  let literal_string : unit -> (constraints * O.type_variable) = fun () ->
    let pattern = type_expression_to_type_value I.t_string in
    let type_name = Core.fresh_type_variable () in
    [C_equation (P_variable (type_name) , pattern)] , type_name
   *)

  let tuple : I.type_expression list -> (constraints * O.type_variable) = fun tys ->
    let patterns = List.map type_expression_to_type_value tys in
    let pattern = O.(P_constant (C_tuple , patterns)) in
    let type_name = Core.fresh_type_variable () in
    [C_equation (P_variable (type_name) , pattern)] , type_name

  (* let t_tuple         = ('label:int, 'v) … -> record ('label : 'v) … *)
  (* let t_constructor   = ('label:string, 'v) -> variant ('label : 'v) *)
  (* let t_record        = ('label:string, 'v) … -> record ('label : 'v) … with independent choices for each 'label and 'v *)
  (* let t_variable      = t_of_var_in_env *)
  (* let t_access_int    = record ('label:int ,    'v) … -> 'label:int    -> 'v *)
  (* let t_access_string = record ('label:string , 'v) … -> 'label:string -> 'v *)

  module Prim_types = struct
    open Typesystem.Shorthands

    let t_cons           = forall "v" @@ fun v -> v --> list v --> list v                 (* was: list *)
    let t_setcons        = forall "v" @@ fun v -> v --> set v  --> set v                  (* was: set  *)
    let t_mapcons        = forall2 "k" "v" @@ fun k v -> (k * v) --> map k v --> map k v  (* was: map  *)
    let t_failwith       = forall "a" @@ fun a -> a
    (* let t_literal_t   = t *)
    let t_literal_bool   = bool
    let t_literal_string = string
    let t_access_map     = forall2 "k" "v" @@ fun k v -> map k v --> k --> v
    let t_application    = forall2 "a" "b" @@ fun a b -> (a --> b) --> a --> b
    let t_look_up        = forall2 "ind" "v" @@ fun ind v -> map ind v --> ind --> option v
    let t_sequence       = forall "b" @@ fun b -> unit --> b --> b
    let t_loop           = bool --> unit --> unit
  end

  (* TODO: I think we should take an I.expression for the base+label *)
  let access_label ~base ~label : (constraints * O.type_variable) =
    let base' = type_expression_to_type_value base in
    let expr_type = Core.fresh_type_variable () in
    [O.C_access_label (base' , label , expr_type)] , expr_type

  let access_int ~base ~index = access_label ~base ~label:(L_int index)
  let access_string ~base ~property = access_label ~base ~label:(L_string property)

  let access_map : base:I.type_expression -> key:I.type_expression -> (constraints * O.type_variable) =
    let mk_map_type key_type element_type =
      O.P_constant O.(C_map , [P_variable element_type; P_variable key_type]) in
    fun ~base ~key ->
      let key_type = Core.fresh_type_variable () in
      let element_type = Core.fresh_type_variable () in
      let base' = type_expression_to_type_value base in
      let key' = type_expression_to_type_value key in
      let base_expected = mk_map_type key_type element_type in
      let expr_type = Core.fresh_type_variable () in
      O.[C_equation (base' , base_expected);
         C_equation (key' , P_variable key_type);
         C_equation (P_variable expr_type , P_variable element_type)] , expr_type

  let constructor
    : I.type_expression -> I.type_expression -> I.type_expression -> (constraints * O.type_variable)
    = fun t_arg c_arg sum ->
      let t_arg = type_expression_to_type_value t_arg in
      let c_arg = type_expression_to_type_value c_arg in
      let sum = type_expression_to_type_value sum in
      let whole_expr = Core.fresh_type_variable () in
      [
        C_equation (P_variable (whole_expr) , sum) ;
        C_equation (t_arg , c_arg)
      ] , whole_expr

  let record : I.type_expression I.type_name_map -> (constraints * O.type_variable) = fun fields ->
    let record_type = type_expression_to_type_value (I.t_record fields) in
    let whole_expr = Core.fresh_type_variable () in
    [C_equation (P_variable whole_expr , record_type)] , whole_expr

  let collection : O.constant_tag -> I.type_expression list -> (constraints * O.type_variable) =
    fun ctor element_tys ->
      let elttype = O.P_variable (Core.fresh_type_variable ()) in
      let aux elt =
        let elt' = type_expression_to_type_value elt
        in O.C_equation (elttype , elt') in
      let equations = List.map aux element_tys in
      let whole_expr = Core.fresh_type_variable () in
      O.[
        C_equation (P_variable whole_expr , O.P_constant (ctor , [elttype]))
      ] @ equations , whole_expr

  let list = collection O.C_list
  let set  = collection O.C_set

  let map : (I.type_expression * I.type_expression) list -> (constraints * O.type_variable) =
    fun kv_tys ->
      let k_type = O.P_variable (Core.fresh_type_variable ()) in
      let v_type = O.P_variable (Core.fresh_type_variable ()) in
      let aux_k (k , _v) =
        let k' = type_expression_to_type_value k in
        O.C_equation (k_type , k') in
      let aux_v (_k , v) =
        let v' = type_expression_to_type_value v in
        O.C_equation (v_type , v') in
      let equations_k = List.map aux_k kv_tys in
      let equations_v = List.map aux_v kv_tys in
      let whole_expr = Core.fresh_type_variable () in
      O.[
        C_equation (P_variable whole_expr , O.P_constant (C_map , [k_type ; v_type]))
      ] @ equations_k @ equations_v , whole_expr

  let application : I.type_expression -> I.type_expression -> (constraints * O.type_variable) =
    fun f arg ->
      let whole_expr = Core.fresh_type_variable () in
      let f'   = type_expression_to_type_value f in
      let arg' = type_expression_to_type_value arg in
      O.[
        C_equation (f' , P_constant (C_arrow , [arg' ; P_variable whole_expr]))
      ] , whole_expr

  let look_up : I.type_expression -> I.type_expression -> (constraints * O.type_variable) =
    fun ds ind ->
      let ds'  = type_expression_to_type_value ds in
      let ind' = type_expression_to_type_value ind in
      let whole_expr = Core.fresh_type_variable () in
      let v = Core.fresh_type_variable () in
      O.[
        C_equation (ds' , P_constant (C_map, [ind' ; P_variable v])) ;
        C_equation (P_variable whole_expr , P_constant (C_option , [P_variable v]))
      ] , whole_expr

  let sequence : I.type_expression -> I.type_expression -> (constraints * O.type_variable) =
    fun a b ->
      let a' = type_expression_to_type_value a in
      let b' = type_expression_to_type_value b in
      let whole_expr = Core.fresh_type_variable () in
      O.[
        C_equation (a' , P_constant (C_unit , [])) ;
        C_equation (b' , P_variable whole_expr)
      ] , whole_expr

  let loop : I.type_expression -> I.type_expression -> (constraints * O.type_variable) =
    fun expr body ->
      let expr' = type_expression_to_type_value expr in
      let body' = type_expression_to_type_value body in
      let whole_expr = Core.fresh_type_variable () in
      O.[
        C_equation (expr'                 , P_constant (C_bool , [])) ;
        C_equation (body'                 , P_constant (C_unit , [])) ;
        C_equation (P_variable whole_expr , P_constant (C_unit , []))
      ] , whole_expr

  let let_in : I.type_expression -> I.type_expression option -> I.type_expression -> (constraints * O.type_variable) =
    fun rhs rhs_tv_opt result ->
      let rhs'        = type_expression_to_type_value rhs in
      let result'     = type_expression_to_type_value result in
      let rhs_tv_opt' = match rhs_tv_opt with
          None -> []
        | Some annot -> O.[C_equation (rhs' , type_expression_to_type_value annot)] in
      let whole_expr = Core.fresh_type_variable () in
      O.[
        C_equation (result' , P_variable whole_expr)
      ] @ rhs_tv_opt', whole_expr

  let assign : I.type_expression -> I.type_expression -> (constraints * O.type_variable) =
    fun v e ->
      let v' = type_expression_to_type_value v in
      let e' = type_expression_to_type_value e in
      let whole_expr = Core.fresh_type_variable () in
      O.[
        C_equation (v' , e') ;
        C_equation (P_variable whole_expr , P_constant (C_unit , []))
      ] , whole_expr

  let annotation : I.type_expression -> I.type_expression -> (constraints * O.type_variable) =
    fun e annot ->
      let e' = type_expression_to_type_value e in
      let annot' = type_expression_to_type_value annot in
      let whole_expr = Core.fresh_type_variable () in
      O.[
        C_equation (e' , annot') ;
        C_equation (e' , P_variable whole_expr)
      ] , whole_expr

  let matching : I.type_expression list -> (constraints * O.type_variable) =
    fun es ->
      let whole_expr = Core.fresh_type_variable () in
      let type_values = (List.map type_expression_to_type_value es) in
      let cs = List.map (fun e -> O.C_equation (P_variable whole_expr , e)) type_values
      in cs, whole_expr

  let fresh_binder () =
    Core.fresh_type_variable ()

  let lambda
    : I.type_expression ->
      I.type_expression option ->
      I.type_expression option ->
      (constraints * O.type_variable) =
    fun fresh arg body ->
      let whole_expr = Core.fresh_type_variable () in
      let unification_arg = Core.fresh_type_variable () in
      let unification_body = Core.fresh_type_variable () in
      let arg'  = match arg with
          None -> []
        | Some arg -> O.[C_equation (P_variable unification_arg , type_expression_to_type_value arg)] in
      let body'  = match body with
          None -> []
        | Some body -> O.[C_equation (P_variable unification_body , type_expression_to_type_value body)]
      in O.[
          C_equation (type_expression_to_type_value fresh , P_variable unification_arg) ;
          C_equation (P_variable whole_expr ,
                      P_constant (C_arrow , [P_variable unification_arg ;
                                             P_variable unification_body]))
        ] @ arg' @ body' , whole_expr

end

(* begin unionfind *)

module TV =
struct
  type t = Core.type_variable
  let compare = String.compare
  let to_string = (fun s -> s)
end

module UF = Union_find.Partition0.Make(TV)

type unionfind = UF.t

let empty = UF.empty                           (* DEMO *)
let representative_toto = UF.repr "toto" empty (* DEMO *)
let merge x y = UF.equiv x y                   (* DEMO *)

(* end unionfind *)

(* representant for an equivalence class of type variables *)
module TypeVariable = String
module TypeVariableMap = Map.Make(TypeVariable)


(*

Components:
* assignments (passive data structure).
  Now: just a map from unification vars to types (pb: what about partial types?)
  maybe just local assignments (allow only vars as children of pair(α,β))
* constraint propagation: (buch of constraints) → (new constraints * assignments)
  * sub-component: constraint selector (worklist / dynamic queries)
    * sub-sub component: constraint normalizer: remove dupes and give structure
      right now: union-find of unification vars
      later: better database-like organisation of knowledge
    * sub-sub component: lazy selector (don't re-try all selectors every time)
      For now: just re-try everytime
  * sub-component: propagation rule
    For now: break pair(a, b) = pair(c, d) into a = c, b = d
* generalizer
  For now: ?

Workflow:
  Start with empty assignments and structured database
  Receive a new constraint
  For each normalizer:
    Use the pre-selector to see if it can be applied
    Apply the normalizer, get some new items to insert in the structured database
  For each propagator:
    Use the selector to query the structured database and see if it can be applied
    Apply the propagator, get some new constraints and assignments
  Add the new assignments to the data structure.

  At some point (when?)
  For each generalizer:
    Use the generalizer's selector to see if it can be applied
    Apply the generalizer to produce a new type, possibly with some ∀s injected

*)

open Core

type structured_dbs = {
  all_constraints     : type_constraint_simpl list ;
  aliases             : unionfind ;
  (* assignments (passive data structure).
     Now: just a map from unification vars to types (pb: what about partial types?)
     maybe just local assignments (allow only vars as children of pair(α,β)) *)
  assignments         : c_constructor_simpl TypeVariableMap.t ;
  grouped_by_variable : constraints TypeVariableMap.t ; (* map from (unionfind) variables to constraints containing them *)
  cycle_detection_toposort : unit ; (* example of structured db that we'll add later *)
}

and constraints = {
  constructor : c_constructor_simpl list ;
  tc          : c_typeclass_simpl   list ;
}

and c_constructor_simpl = {
  tv : type_variable;
  c_tag : constant_tag;
  tv_list : type_variable list;
}
(* copy-pasted from core.ml *)
and c_const = (type_variable * type_value)
and c_equation = (type_value * type_value)
and c_typeclass_simpl = {
  tc   : typeclass          ;
  args : type_variable list ;
}
and type_constraint_simpl =
    SC_Constructor of c_constructor_simpl             (* α = ctor(β, …) *)
  | SC_Alias       of (type_variable * type_variable) (* α = β *)
  | SC_Typeclass   of c_typeclass_simpl               (* TC(α, …) *)

module UnionFindWrapper = struct
  (* TODO: API for the structured db, to access it modulo unification variable aliases. *)
  let get_constraints_related_to : type_variable -> structured_dbs -> constraints =
    fun variable dbs ->
      let variable , aliases = UF.get_or_set variable dbs.aliases in
      let dbs = { dbs with aliases } in
      match TypeVariableMap.find_opt variable dbs.grouped_by_variable with
        Some l -> l
      | None -> {
          constructor = [] ;
          tc          = [] ;
        }
  let add_constraints_related_to : type_variable -> constraints -> structured_dbs -> structured_dbs =
    fun variable c dbs ->
      (* let (variable_repr , _height) , aliases = UF.get_or_set variable dbs.aliases in
         let dbs = { dbs with aliases } in *)
      let variable_repr , aliases = UF.get_or_set variable dbs.aliases in
      let dbs = { dbs with aliases } in
      let grouped_by_variable = TypeVariableMap.update variable_repr (function
            None -> Some c
          | Some x -> Some {
              constructor = c.constructor @ x.constructor ;
              tc          = c.tc @ x.tc ;
            })
          dbs.grouped_by_variable
      in
      let dbs = { dbs with grouped_by_variable } in
      dbs
  let merge_variables : type_variable -> type_variable -> structured_dbs -> structured_dbs =
    fun variable_a variable_b dbs ->
      let variable_repr_a , aliases = UF.get_or_set variable_a dbs.aliases in
      let dbs = { dbs with aliases } in
      let variable_repr_b , aliases = UF.get_or_set variable_b dbs.aliases in
      let dbs = { dbs with aliases } in
      let default d = function None -> d | Some y -> y in
      let get_constraints ab =
        TypeVariableMap.find_opt ab dbs.grouped_by_variable
        |> default { constructor = [] ; tc = [] } in
      let constraints_a = get_constraints variable_repr_a in
      let constraints_b = get_constraints variable_repr_b in
      let all_constraints = {
        (* TODO: should be a Set.union, not @ *)
        constructor = constraints_a.constructor @ constraints_b.constructor ;
        tc          = constraints_a.tc          @ constraints_b.tc          ;
      } in
      let grouped_by_variable =
        TypeVariableMap.add variable_repr_a all_constraints dbs.grouped_by_variable in
      let dbs = { dbs with grouped_by_variable} in
      let grouped_by_variable =
        TypeVariableMap.remove variable_repr_b dbs.grouped_by_variable in
      let dbs = { dbs with grouped_by_variable} in
      dbs
end

(* sub-sub component: constraint normalizer: remove dupes and give structure
 * right now: union-find of unification vars
 * later: better database-like organisation of knowledge *)

(* Each normalizer returns a  *)
type ('a , 'b) normalizer = structured_dbs -> 'a -> (structured_dbs * 'b list)

let normalizer_all_constraints : (type_constraint_simpl , type_constraint_simpl) normalizer =
  fun dbs new_constraint ->
    ({ dbs with all_constraints = new_constraint :: dbs.all_constraints } , [new_constraint])

let normalizer_grouped_by_variable : (type_constraint_simpl , type_constraint_simpl) normalizer =
  fun dbs new_constraint ->
    let store_constraint tvars constraints =
      let aux dbs (tvar : type_variable) =
        UnionFindWrapper.add_constraints_related_to tvar constraints dbs
      in List.fold_left aux dbs tvars
    in
    let merge_constraints a b =
      UnionFindWrapper.merge_variables a b dbs in
    let dbs = match new_constraint with
        SC_Constructor ({tv ; c_tag = _ ; tv_list} as c) -> store_constraint (tv :: tv_list) {constructor = [c] ; tc = []}
      | SC_Typeclass   ({tc = _ ; args}            as c) -> store_constraint args            {constructor = [] ; tc = [c]}
      | SC_Alias (a , b) -> merge_constraints a b
    in (dbs , [new_constraint])

(* Stores the first assinment ('a = ctor('b, …)) seen *)
let normalizer_assignments : (type_constraint_simpl , type_constraint_simpl) normalizer =
  fun dbs new_constraint ->
    match new_constraint with
    | SC_Constructor ({tv ; c_tag = _ ; tv_list = _} as c) ->
      let assignments = TypeVariableMap.update tv (function None -> Some c | e -> e) dbs.assignments in
      let dbs = {dbs with assignments} in
      (dbs , [new_constraint])
    | _ ->
      (dbs , [new_constraint])

let rec normalizer_simpl : (type_constraint , type_constraint_simpl) normalizer =
  fun dbs new_constraint ->
    match new_constraint with
    | C_equation (P_forall _, P_forall _)                   -> failwith "TODO"
    | C_equation ((P_forall _ as a), (P_variable _ as b))   -> normalizer_simpl dbs (C_equation (b , a))
    | C_equation (P_forall _, P_constant _)                 -> failwith "TODO"
    | C_equation (P_variable _, P_forall _)                 -> failwith "TODO"
    | C_equation (P_variable a, P_variable b)               -> (dbs , [SC_Alias (a, b)])
    | C_equation (P_variable a, P_constant (c_tag, args))   ->
      let fresh_vars = List.map (fun _ -> Core.fresh_type_variable ()) args in
      let fresh_eqns = List.map (fun (v,t) -> C_equation (P_variable v, t)) (List.combine fresh_vars args) in
      let (dbs , recur) = List.fold_map_acc normalizer_simpl dbs fresh_eqns in
      (dbs , [SC_Constructor {tv=a;c_tag;tv_list=fresh_vars}] @ List.flatten recur)
    | C_equation (P_constant _, P_forall _)                 -> failwith "TODO"
    | C_equation ((P_constant _ as a), (P_variable _ as b)) -> normalizer_simpl dbs (C_equation (b , a))
    | C_equation ((P_constant _ as a), (P_constant _ as b)) ->
      (* break down c(args) = c'(args') into 'a = c(args) and 'a = c'(args') *)
      let fresh = Core.fresh_type_variable () in
      let (dbs , cs1) = normalizer_simpl dbs (C_equation (P_variable fresh, a)) in
      let (dbs , cs2) = normalizer_simpl dbs (C_equation (P_variable fresh, b)) in
      (dbs , cs1 @ cs2) (* TODO: O(n) concatenation! *)
    | C_typeclass (args, tc)                                ->
      (* break down TC(args) into TC('a, …) and ('a = arg) … *)
      let fresh_vars = List.map (fun _ -> Core.fresh_type_variable ()) args in
      let fresh_eqns = List.map (fun (v,t) -> C_equation (P_variable v, t)) (List.combine fresh_vars args) in
      let (dbs , recur) = List.fold_map_acc normalizer_simpl dbs fresh_eqns in
      (dbs, [SC_Typeclass { tc ; args = fresh_vars }] @ List.flatten recur)
    | C_access_label (tv, label, result) -> let _todo = ignore (tv, label, result) in failwith "TODO"

type ('state, 'elt) state_list_monad = { state: 'state ; list : 'elt list }
let lift_state_list_monad ~state ~list = { state ; list }
let lift f =
  fun { state ; list } ->
    let (new_state , new_lists) = List.fold_map_acc f state list in
    { state = new_state ; list = List.flatten new_lists }

(* TODO: move this to the List module *)
let named_fold_left f ~acc ~lst = List.fold_left (fun acc lst -> f ~acc ~lst) acc lst

(* TODO: place the list of normalizers in a map *)
(* (\* cons for heterogeneous lists *\)
 * type 'b f = { f : 'a . ('a -> 'b) -> 'a -> 'b }
 * type ('hd , 'tl) hcons = { hd : 'hd ; tl : 'tl ; map : 'b . 'b f -> ('b , 'tl) hcons }
 * let (+::) hd tl = { hd ; tl ; map = fun x ->  }
 *
 * let list_of_normalizers =
 *   normalizer_simpl +::
 *   normalizer_all_constraints +::
 *   normalizer_assignments +::
 *   normalizer_grouped_by_variable +::
 *   () *)

module Fun = struct let id x = x end (* in stdlib as of 4.08, we're in 4.07 for now *)

let normalizers : type_constraint -> structured_dbs -> (structured_dbs , 'modified_constraint) state_list_monad =
  fun new_constraint dbs ->
    Fun.id
    @@ lift normalizer_grouped_by_variable
    @@ lift normalizer_assignments
    @@ lift normalizer_all_constraints
    @@ lift normalizer_simpl
    @@ lift_state_list_monad ~state:dbs ~list:[new_constraint]

(* sub-sub component: lazy selector (don't re-try all selectors every time)
 * For now: just re-try everytime *)

type todo = unit
let todo : todo = ()
type 'old_constraint_type selector_input = 'old_constraint_type (* some info about the constraint just added, so that we know what to look for *)
type 'selector_output selector_outputs =
    WasSelected of 'selector_output list
  | WasNotSelected
type new_constraints = type_constraint list
type new_assignments = c_constructor_simpl list

type ('old_constraint_type, 'selector_output) selector = 'old_constraint_type selector_input -> structured_dbs -> 'selector_output selector_outputs

(* selector / propagation rule for breaking down composite types
 * For now: do something with ('a = 'b) constraints.

   Or maybe this one should be a normalizer. *)

(* selector / propagation rule for breaking down composite types
 * For now: break pair(a, b) = pair(c, d) into a = c, b = d *)

type output_break_ctor = < a_k_var : c_constructor_simpl ; a_k'_var' : c_constructor_simpl >
let selector_break_ctor : (type_constraint_simpl, output_break_ctor) selector =
  (* find two rules with the shape a = k(var …) and a = k'(var' …) *)
  fun todo dbs ->
    match todo with
      SC_Constructor c ->
      let other_cs = (UnionFindWrapper.get_constraints_related_to c.tv dbs).constructor in
      let cs_pairs = List.map (fun x -> object method a_k_var = c method a_k'_var' = x end) other_cs in
      WasSelected cs_pairs
    | SC_Alias       _                -> WasNotSelected (* TODO: ??? *)
    | SC_Typeclass   _                -> WasNotSelected

type 'selector_output propagator = 'selector_output -> structured_dbs -> new_constraints * new_assignments

let propagator_break_ctor : output_break_ctor propagator =
  fun selected dbs ->
    let () = ignore (dbs) in (* this propagator doesn't need to use the dbs *)
    let a = selected#a_k_var in
    let b = selected#a_k'_var' in
    (* produce constraints: *)

    (* a.tv = b.tv *)
    let eq1 = C_equation (P_variable a.tv, P_variable b.tv) in
    (* a.c_tag = b.c_tag *)
    if a.c_tag <> b.c_tag then
      failwith "type error: incompatible types, not same ctor (TODO error message)"
    else
      (* a.tv_list = b.tv_list *)
    if List.length a.tv_list <> List.length b.tv_list then
      failwith "type error: incompatible types, not same length (TODO error message)"
    else
      let eqs3 = List.map2 (fun aa bb -> C_equation (P_variable aa, P_variable bb)) a.tv_list b.tv_list in
      let eqs = eq1 :: eqs3 in
      (eqs , []) (* no new assignments *)

let select_and_propagate : ('old_input, 'selector_output) selector -> 'selector_output propagator -> 'a -> structured_dbs -> new_constraints * new_assignments =
  fun selector propagator ->
  fun todo dbs ->
    match selector todo dbs with
      WasSelected selected_outputs ->
      (* Call the propagation rule *)
      let new_contraints_and_assignments = List.map (fun s -> propagator s dbs) selected_outputs in
      let (new_constraints , new_assignments) = List.split new_contraints_and_assignments in
      (* return so that the new constraints are pushed to some kind of work queue and the new assignments stored *)
      (List.flatten new_constraints , List.flatten new_assignments)
    | WasNotSelected ->
      ([] , [])

let select_and_propagate_break_ctor = select_and_propagate selector_break_ctor propagator_break_ctor

let select_and_propagate_all' : type_constraint_simpl selector_input -> structured_dbs -> 'todo_result =
  fun new_constraint dbs ->
    let (new_constraints, new_assignments) = select_and_propagate_break_ctor new_constraint dbs in
    let assignments = List.fold_left (fun acc ({tv;c_tag=_;tv_list=_} as ele) -> TypeVariableMap.update tv (function None -> Some ele | x -> x) acc) dbs.assignments new_assignments in
    let dbs = { dbs with assignments } in
    (* let blah2 = select_ … in … *)
    (* We should try each selector in turn. If multiple selectors work, what should we do? *)
    (new_constraints , dbs)

let rec select_and_propagate_all : type_constraint selector_input list -> structured_dbs -> 'todo_result =
  fun new_constraints dbs ->
    match new_constraints with
    | [] -> dbs
    | new_constraint :: tl ->
      let { state = dbs ; list = modified_constraints } = normalizers new_constraint dbs in
      let (new_constraints' , dbs) =
        List.fold_left
          (fun (nc , dbs) c ->
             let (new_constraints' , dbs) = select_and_propagate_all' c dbs in
             (new_constraints' @ nc , dbs))
          ([] , dbs)
          modified_constraints in
      let new_constraints = new_constraints' @ tl in
      select_and_propagate_all new_constraints dbs

(* sub-component: constraint selector (worklist / dynamic queries) *)

(* constraint propagation: (buch of constraints) → (new constraints * assignments) *)





(* Below is a draft *)

type state = {
  (* when α-renaming x to y, we put them in the same union-find class *)
  unification_vars : unionfind ;

  (* assigns a value to the representant in the unionfind *)
  assignments : type_value TypeVariableMap.t ;

  (* constraints related to a type variable *)
  constraints : constraints TypeVariableMap.t ;
}

let initial_state : state = {
  unification_vars = UF.empty ;
  constraints = TypeVariableMap.empty ;
  assignments = TypeVariableMap.empty ;
}

(* let replace_var_in_state = fun (v : type_variable) (state : state) -> *)
(*   let aux_tv : type_value -> _ = function *)
(*     | P_forall    (w  , cs , tval) -> failwith "TODO" *)
(*     | P_variable  (w)              -> *)
(*       if w = v then *)
(*       (*…*) *)
(*       else *)
(*       (*…*) *)
(*     | P_constant     (c  , args)      -> failwith "TODO" *)
(*     | P_access_label (tv , label)     -> failwith "TODO" in *)
(*   let aux_tc tc = *)
(*     List.map (fun l -> List.map aux_tv l) tc in *)
(*   let aux : type_constraint -> _ = function *)
(*     | C_equation  (l , r)          -> C_equation  (aux_tv l , aux_tv r) *)
(*     | C_typeclass (l , rs)         -> C_typeclass (List.map aux_tv l , aux_tc rs) *)
(*   in List.map aux state *)

(* let check_equal       a  b  = failwith "TODO"
 * let check_same_length l1 l2 = failwith "TODO"
 * 
 * let rec unify : type_value * type_value -> type_constraint list result = function
 *   | (P_variable v           , P_constant (y , argsy)) ->
 *     failwith "TODO: replace v with the constant everywhere."
 *   | (P_constant (x , argsx) , P_variable w) ->
 *     failwith "TODO: "
 *   | (P_variable v           , P_variable w) ->
 *     failwith "TODO: replace v with w everywhere"
 *   | (P_constant (x , argsx) , P_constant (y , argsy)) ->
 *     let%bind () = check_equal x y in
 *     let%bind () = check_same_length argsx argsy in
 *     let%bind _ =  bind_map_list unify (List.combine argsx argsy) in
 *     ok []
 *   | _ -> failwith "TODO" *)

(* (\* unify a and b, possibly produce new constraints *\) *)
(* let () = ignore (a,b) in *)
(* ok [] *)

(* This is the solver *)
let aggregate_constraints : state -> type_constraint list -> state result = fun state newc ->
  (* TODO: Iterate over constraints *)
  (* TODO: try to unify things:
             if we have a = X and b = Y, try to unify X and Y *)
  let _todo = ignore (state, newc) in
  failwith "TODO"
(*let { constraints ; eqv } = state in
  ok { constraints = constraints @ newc ; eqv }*)
