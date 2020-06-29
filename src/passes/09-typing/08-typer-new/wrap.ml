open Ast_typed.Misc
module Core = Typesystem.Core

module I = Ast_core
module T = Ast_typed
module O = Core

type constraints = O.type_constraint list

(* let add_type state t = *)
(*   let constraints = Wrap.variable type_name t in *)
(*   let%bind state' = aggregate_constraints state constraints in *)
(*   ok state' in *)
(* let return_add_type ?(state = state) expr t = *)
(*   let%bind state' = add_type state t in *)
(*   return expr state' in *)

let rec type_expression_to_type_value : T.type_expression -> O.type_value = fun te ->
  match te.type_content with
  | T_sum kvmap ->
     let () = failwith "fixme: don't use to_list, it drops the variant keys, rows have a differnt kind than argument lists for now!" in
     let tlist = List.map (fun ({ctor_type;_}:T.ctor_content) -> ctor_type) (T.CMap.to_list kvmap) in
     p_constant C_variant (List.map type_expression_to_type_value tlist)
  | T_record kvmap ->
     let () = failwith "fixme: don't use to_list, it drops the record keys, rows have a differnt kind than argument lists for now!" in
     let tlist = List.map (fun ({field_type;_}:T.field_content) -> field_type) (T.LMap.to_list kvmap) in
     p_constant C_record (List.map type_expression_to_type_value tlist)
  | T_arrow {type1;type2} ->
     p_constant C_arrow (List.map type_expression_to_type_value [ type1 ; type2 ])

  | T_variable (type_name) -> { tsrc = "wrap: from source code maybe?" ; t = P_variable type_name }
  | T_constant (type_name) ->
     let csttag = T.(match type_name with
                        | TC_unit      -> C_unit
                        | TC_string    -> C_string
                        | TC_nat       -> C_nat
                        | TC_mutez     -> C_mutez
                        | TC_timestamp -> C_timestamp
                        | TC_int       -> C_int
                        | TC_address   -> C_address
                        | TC_bytes     -> C_bytes
                        | TC_key_hash  -> C_key_hash
                        | TC_key       -> C_key
                        | TC_signature -> C_signature
                        | TC_operation -> C_operation
                        | TC_chain_id  -> C_unit    (* TODO : replace  with chain_id *)
                        | TC_void      -> C_unit    (* TODO : replace with void *)
                  )
     in
     p_constant csttag []
  | T_operator (type_operator) ->
     let (csttag, args) = T.(match type_operator with
                                | TC_option o                -> (C_option, [o])
                                | TC_set s                   -> (C_set, [s])
                                | TC_map { k ; v }           -> (C_map, [k;v])
                                | TC_big_map { k ; v }       -> (C_big_map, [k;v])
                                | TC_map_or_big_map { k ; v } -> (C_map, [k;v])
                                | TC_list l                  -> (C_list, [l])
                                | TC_contract c              -> (C_contract, [c])
                          )
     in
     p_constant csttag (List.map type_expression_to_type_value args)

let rec type_expression_to_type_value_copypasted : I.type_expression -> O.type_value = fun te ->
  match te.content with
  | T_sum kvmap ->
     let () = failwith "fixme: don't use to_list, it drops the variant keys, rows have a differnt kind than argument lists for now!" in
     let tlist = List.map (fun ({ctor_type;_}:I.ctor_content) -> ctor_type) (I.CMap.to_list kvmap) in
     p_constant C_variant (List.map type_expression_to_type_value_copypasted tlist)
  | T_record kvmap ->
     let () = failwith "fixme: don't use to_list, it drops the record keys, rows have a differnt kind than argument lists for now!" in
     let tlist = List.map (fun ({field_type;_}:I.field_content) -> field_type) (I.LMap.to_list kvmap) in
     p_constant C_record (List.map type_expression_to_type_value_copypasted tlist)
  | T_arrow {type1;type2} ->
     p_constant C_arrow (List.map type_expression_to_type_value_copypasted [ type1 ; type2 ])
  | T_variable type_name -> { tsrc = "wrap: from source code maybe?" ; t = P_variable type_name }
  | T_constant (type_name) ->
     let csttag = T.(match type_name with
                        | TC_unit   -> C_unit
                        | TC_string -> C_string
                        | _        -> failwith "unknown type constructor")
     in
     p_constant csttag []
  | T_operator (type_name, args) ->
     let csttag = T.(match type_name with
                                | TC_option                    -> C_option 
                                | TC_list                      -> C_list   
                                | TC_set                       -> C_set    
                                | TC_map                       -> C_map    
                                | TC_big_map                   -> C_big_map
                                | TC_map_or_big_map            -> C_map
                                | TC_contract                  -> C_contract
                                | TC_michelson_pair
                                | TC_michelson_or
                                | TC_michelson_pair_right_comb -> C_record
                                | TC_michelson_pair_left_comb  -> C_record
                                | TC_michelson_or_right_comb   -> C_record
                                | TC_michelson_or_left_comb    -> C_record
                          )
     in
     p_constant csttag (List.map type_expression_to_type_value_copypasted args)

let failwith_ : unit -> (constraints * O.type_variable) = fun () ->
  let type_name = Core.fresh_type_variable () in
  [] , type_name

let variable : I.expression_variable -> T.type_expression -> (constraints * T.type_variable) = fun _name expr ->
  let pattern = type_expression_to_type_value expr in
  let type_name = Core.fresh_type_variable () in
  [{ c = C_equation { aval = { tsrc = "wrap: variable: whole" ; t = P_variable type_name } ; bval = pattern } ; reason = "wrap: variable" }] , type_name

let literal : T.type_expression -> (constraints * T.type_variable) = fun t ->
  let pattern = type_expression_to_type_value t in
  let type_name = Core.fresh_type_variable () in
  [{ c = C_equation { aval = { tsrc = "wrap: literal: whole" ; t = P_variable type_name } ; bval = pattern } ; reason = "wrap: literal" }] , type_name

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

let tuple : T.type_expression list -> (constraints * T.type_variable) = fun tys ->
  let patterns = List.map type_expression_to_type_value tys in
  let pattern = p_constant C_record patterns in
  let type_name = Core.fresh_type_variable () in
  [{ c = C_equation { aval = { tsrc = "wrap: tuple: whole" ; t = P_variable type_name } ; bval = pattern} ; reason = "wrap: tuple" }] , type_name

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
  let t_application    = forall2 "a" "b" @@ fun a b -> (a --> b) --> a --> b
  let t_look_up        = forall2 "ind" "v" @@ fun ind v -> map ind v --> ind --> option v
  let t_sequence       = forall "b" @@ fun b -> unit --> b --> b
  let t_loop           = bool --> unit --> unit
end

(* TODO: I think we should take an I.expression for the base+label *)
let access_label ~(base : T.type_expression) ~(label : O.accessor) : (constraints * T.type_variable) =
  let base' = type_expression_to_type_value base in
  let expr_type = Core.fresh_type_variable () in
  [{ c = C_access_label { c_access_label_tval = base' ; accessor = label ; c_access_label_tvar = expr_type } ; reason = "wrap: access_label" }] , expr_type

open Ast_typed.Misc
let constructor
    : T.type_expression -> T.type_expression -> T.type_expression -> (constraints * T.type_variable)
  = fun t_arg c_arg sum ->
  let t_arg = type_expression_to_type_value t_arg in
  let c_arg = type_expression_to_type_value c_arg in
  let sum = type_expression_to_type_value sum in
  let whole_expr = Core.fresh_type_variable () in
  [
    c_equation { tsrc = "wrap: constructor: whole" ; t = P_variable whole_expr } sum "wrap: constructor: whole" ;
    c_equation t_arg c_arg "wrap: construcotr: arg" ;
  ] , whole_expr

let record : T.field_content T.label_map -> (constraints * T.type_variable) = fun fields ->
  let record_type = type_expression_to_type_value (T.t_record fields ()) in
  let whole_expr = Core.fresh_type_variable () in
  [c_equation { tsrc = "wrap: record: whole" ; t = P_variable whole_expr } record_type "wrap: record: whole"] , whole_expr

let collection : O.constant_tag -> T.type_expression list -> (constraints * T.type_variable) =
  fun ctor element_tys ->
  let elttype = T.{ tsrc = "wrap: collection: p_variable" ; t = P_variable (Core.fresh_type_variable ()) } in
  let aux elt =
    let elt' = type_expression_to_type_value elt
    in c_equation elttype elt' "wrap: collection: elt" in
  let equations = List.map aux element_tys in
  let whole_expr = Core.fresh_type_variable () in
  [
      c_equation { tsrc = "wrap: collection: whole" ; t = P_variable whole_expr} (p_constant ctor [elttype]) "wrap: collection: whole" ;
  ] @ equations , whole_expr

let list = collection T.C_list
let set  = collection T.C_set

let map : (T.type_expression * T.type_expression) list -> (constraints * T.type_variable) =
  fun kv_tys ->
  let k_type = T.{ tsrc = "wrap: map: k" ; t = P_variable (Core.fresh_type_variable ()) } in
  let v_type = T.{ tsrc = "wrap: map: v" ; t = P_variable (Core.fresh_type_variable ()) } in
  let aux_k (k , _v) =
    let k' = type_expression_to_type_value k in
    c_equation k_type k' "wrap: map: key" in
  let aux_v (_k , v) =
    let v' = type_expression_to_type_value v in
    c_equation v_type v' "wrap: map: value" in
  let equations_k = List.map aux_k kv_tys in
  let equations_v = List.map aux_v kv_tys in
  let whole_expr = Core.fresh_type_variable () in
  [
      c_equation ({ tsrc = "wrap: map: whole" ; t = P_variable whole_expr }) (p_constant C_map [k_type ; v_type]) "wrap: map: whole" ;
  ] @ equations_k @ equations_v , whole_expr

let big_map : (T.type_expression * T.type_expression) list -> (constraints * T.type_variable) =
  fun kv_tys ->
  let k_type = T.{ tsrc = "wrap: big_map: k" ; t = P_variable (Core.fresh_type_variable ()) } in
  let v_type = T.{ tsrc = "wrap: big_map: v" ; t = P_variable (Core.fresh_type_variable ()) } in
  let aux_k (k , _v) =
    let k' = type_expression_to_type_value k in
    c_equation k_type k' "wrap: big_map: key" in
  let aux_v (_k , v) =
    let v' = type_expression_to_type_value v in
    c_equation v_type v' "wrap: big_map: value" in
  let equations_k = List.map aux_k kv_tys in
  let equations_v = List.map aux_v kv_tys in
  let whole_expr = Core.fresh_type_variable () in
  [
      (* TODO: this doesn't tag big_maps uniquely (i.e. if two
           big_map have the same type, they can be swapped. *)
      c_equation ({ tsrc = "wrap: big_map: whole" ; t = P_variable whole_expr}) (p_constant C_big_map [k_type ; v_type]) "wrap: big_map: whole" ;
  ] @ equations_k @ equations_v , whole_expr

let application : T.type_expression -> T.type_expression -> (constraints * T.type_variable) =
  fun f arg ->
  let whole_expr = Core.fresh_type_variable () in
  let f'   = type_expression_to_type_value f in
  let arg' = type_expression_to_type_value arg in
  [
      c_equation f' (p_constant C_arrow [arg' ; { tsrc = "wrap: application: whole" ; t = P_variable whole_expr }]) "wrap: application: f" ;
  ] , whole_expr

let look_up : T.type_expression -> T.type_expression -> (constraints * T.type_variable) =
  fun ds ind ->
  let ds'  = type_expression_to_type_value ds in
  let ind' = type_expression_to_type_value ind in
  let whole_expr = Core.fresh_type_variable () in
  let v = T.{ tsrc = "wrap: look_up: ds" ; t = P_variable (Core.fresh_type_variable ()) } in
  [
      c_equation ds' (p_constant C_map [ind' ; v]) "wrap: look_up: map" ;
      c_equation ({ tsrc = "wrap: look_up: whole" ; t = P_variable whole_expr }) (p_constant C_option [v]) "wrap: look_up: whole" ;
  ] , whole_expr

let sequence : T.type_expression -> T.type_expression -> (constraints * T.type_variable) =
  fun a b ->
  let a' = type_expression_to_type_value a in
  let b' = type_expression_to_type_value b in
  let whole_expr = Core.fresh_type_variable () in
  [
      c_equation a' (p_constant C_unit []) "wrap: sequence: first" ;
      c_equation b' ({ tsrc = "wrap: sequence: whole" ; t = P_variable whole_expr}) "wrap: sequence: second (whole)" ;
  ] , whole_expr

let loop : T.type_expression -> T.type_expression -> (constraints * T.type_variable) =
  fun expr body ->
  let expr' = type_expression_to_type_value expr in
  let body' = type_expression_to_type_value body in
  let whole_expr = Core.fresh_type_variable () in
  [
      c_equation expr'                   ({ tsrc = "built-in type" ; t = P_variable Stage_common.Constant.t_bool }) "wrap: loop: expr" ;
      c_equation body'                   (p_constant C_unit []) "wrap: loop: body" ;
      c_equation (p_constant C_unit [])  ({ tsrc = "wrap: loop: whole" ; t = P_variable whole_expr}) "wrap: loop: whole (unit)" ;
  ] , whole_expr

let let_in : T.type_expression -> T.type_expression option -> T.type_expression -> (constraints * T.type_variable) =
  fun rhs rhs_tv_opt result ->
  let rhs'        = type_expression_to_type_value rhs in
  let result'     = type_expression_to_type_value result in
  let rhs_tv_opt' = match rhs_tv_opt with
      None -> []
    | Some annot -> [c_equation rhs' (type_expression_to_type_value annot) "wrap: let_in: rhs"] in
  let whole_expr = Core.fresh_type_variable () in
  [
      c_equation result' { tsrc = "wrap: let_in: whole" ; t = P_variable whole_expr } "wrap: let_in: result (whole)" ;
  ] @ rhs_tv_opt', whole_expr

let recursive : T.type_expression -> (constraints * T.type_variable) =
  fun fun_type ->
  let fun_type = type_expression_to_type_value fun_type in
  let whole_expr = Core.fresh_type_variable () in
  [
      c_equation fun_type ({ tsrc = "wrap: recursive: whole" ; t = P_variable whole_expr }) "wrap: recursive: fun_type (whole)" ;
  ], whole_expr

let raw_code : T.type_expression -> (constraints * T.type_variable) =
  fun type_anno -> 
  let type_anno = type_expression_to_type_value type_anno in
  let whole_expr = Core.fresh_type_variable () in
  [
      c_equation type_anno  ({ tsrc = "wrap: raw_code: whole"; t = P_variable whole_expr }) "wrap: raw_code: type_anno (whole)" ;
  ], whole_expr

let assign : T.type_expression -> T.type_expression -> (constraints * T.type_variable) =
  fun v e ->
  let v' = type_expression_to_type_value v in
  let e' = type_expression_to_type_value e in
  let whole_expr = Core.fresh_type_variable () in
  [
      c_equation v'  e' "wrap: assign: var type must eq rhs type" ;
      c_equation { tsrc = "wrap: assign: whole" ; t = P_variable whole_expr } (p_constant C_unit []) "wrap: assign: unit (whole)" ;
  ] , whole_expr

let annotation : T.type_expression -> T.type_expression -> (constraints * T.type_variable) =
  fun e annot ->
  let e' = type_expression_to_type_value e in
  let annot' = type_expression_to_type_value annot in
  let whole_expr = Core.fresh_type_variable () in
  [
      c_equation e' annot' "wrap: annotation: expr type must eq annot" ;
      c_equation e' { tsrc = "wrap: annotation: whole" ; t = P_variable whole_expr } "wrap: annotation: whole" ;
  ] , whole_expr

let matching : T.type_expression list -> (constraints * T.type_variable) =
  fun es ->
  let whole_expr = Core.fresh_type_variable () in
  let type_expressions = (List.map type_expression_to_type_value es) in
  let cs = List.map (fun e -> c_equation { tsrc = "wrap: matching: case" ; t = P_variable whole_expr } e "wrap: matching: case (whole)") type_expressions
  in cs, whole_expr

let fresh_binder () =
  Core.fresh_type_variable ()

let lambda
    : T.type_expression ->
      T.type_expression option ->
      T.type_expression option ->
      T.type_expression ->
      (constraints * T.type_variable) =
  fun fresh arg output result ->
  let whole_expr = Core.fresh_type_variable () in
  let unification_arg = T.{ tsrc = "wrap: lambda: arg" ; t = P_variable (Core.fresh_type_variable ()) } in
  let unification_output = T.{ tsrc = "wrap: lambda: whole" ; t = P_variable (Core.fresh_type_variable ()) } in
  let result' = type_expression_to_type_value result in
  let arg'  = match arg with
      None -> []
    | Some arg -> [c_equation unification_arg (type_expression_to_type_value arg) "wrap: lambda: arg annot"] in
  let output'  = match output with
      None -> []
    | Some output -> [c_equation unification_output (type_expression_to_type_value output) "wrap: lambda: output annot"]
  in [
      c_equation unification_output result' "wrap: lambda: result" ;
      c_equation (type_expression_to_type_value fresh) unification_arg "wrap: lambda: arg" ;
      c_equation ({ tsrc = "wrap: lambda: whole" ; t = P_variable whole_expr })
                 (p_constant C_arrow ([unification_arg ; unification_output]))
                 "wrap: lambda: arrow (whole)"
     ] @ arg' @ output' , whole_expr

(* This is pretty much a wrapper for an n-ary function. *)
let constant : O.type_value -> T.type_expression list -> (constraints * T.type_variable) =
  fun f args ->
  let whole_expr = Core.fresh_type_variable () in
  let args'      = List.map type_expression_to_type_value args in
  let args_tuple = p_constant C_record args' in
  [
      c_equation f (p_constant C_arrow ([args_tuple ; { tsrc = "wrap: lambda: whole" ; t = P_variable whole_expr }])) "wrap: constant: as declared for built-in"
  ] , whole_expr
