open Trace
module Core = Typesystem.Core

module I = Ast_core
module T = Ast_typed
module O = Core

module Errors = struct

  let unknown_type_constructor (ctor : string) (te : T.type_expression) () =
    let title = (thunk "unknown type constructor") in
    (* TODO: sanitize the "ctor" argument before displaying it. *)
    let message () = ctor in
    let data = [
        ("ctor" , fun () -> ctor) ;
        ("expression" , fun () -> Format.asprintf "%a"  T.PP.type_expression te) ;
        (* ("location" , fun () -> Format.asprintf "%a" Location.pp te.location) *) (* TODO *)
      ] in
    error ~data title message ()
end


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
     P_constant (C_variant, List.map type_expression_to_type_value tlist)
  | T_record kvmap ->
     let () = failwith "fixme: don't use to_list, it drops the record keys, rows have a differnt kind than argument lists for now!" in
     let tlist = List.map (fun ({field_type;_}:T.field_content) -> field_type) (T.LMap.to_list kvmap) in
     P_constant (C_record, List.map type_expression_to_type_value tlist)
  | T_arrow {type1;type2} ->
     P_constant (C_arrow, List.map type_expression_to_type_value [ type1 ; type2 ])

  | T_variable (type_name) -> P_variable type_name
  | T_constant (type_name) ->
     let csttag = Core.(match type_name with
                        | TC_unit      -> C_unit
                        | TC_bool      -> C_bool
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
     P_constant (csttag, [])
  | T_operator (type_operator) ->
     let (csttag, args) = Core.(match type_operator with
                                | TC_option o                -> (C_option, [o])
                                | TC_set s                   -> (C_set, [s])
                                | TC_map { k ; v }           -> (C_map, [k;v])
                                | TC_big_map { k ; v }       -> (C_big_map, [k;v])
                                | TC_map_or_big_map { k ; v } -> (C_map, [k;v])
                                | TC_arrow { type1 ; type2 } -> (C_arrow, [ type1 ; type2 ])
                                | TC_list l                  -> (C_list, [l])
                                | TC_contract c              -> (C_contract, [c])
                          )
     in
     P_constant (csttag, List.map type_expression_to_type_value args)

let rec type_expression_to_type_value_copypasted : I.type_expression -> O.type_value = fun te ->
  match te.type_content with
  | T_sum kvmap ->
     let () = failwith "fixme: don't use to_list, it drops the variant keys, rows have a differnt kind than argument lists for now!" in
     let tlist = List.map (fun ({ctor_type;_}:I.ctor_content) -> ctor_type) (I.CMap.to_list kvmap) in
     P_constant (C_variant, List.map type_expression_to_type_value_copypasted tlist)
  | T_record kvmap ->
     let () = failwith "fixme: don't use to_list, it drops the record keys, rows have a differnt kind than argument lists for now!" in
     let tlist = List.map (fun ({field_type;_}:I.field_content) -> field_type) (I.LMap.to_list kvmap) in
     P_constant (C_record, List.map type_expression_to_type_value_copypasted tlist)
  | T_arrow {type1;type2} ->
     P_constant (C_arrow, List.map type_expression_to_type_value_copypasted [ type1 ; type2 ])
  | T_variable type_name -> P_variable (type_name) (* eird stuff*)
  | T_constant (type_name) ->
     let csttag = Core.(match type_name with
                        | TC_unit   -> C_unit
                        | TC_bool   -> C_bool
                        | TC_string -> C_string
                        | _        -> failwith "unknown type constructor")
     in
     P_constant (csttag,[])
  | T_operator (type_name) ->
     let (csttag, args) = Core.(match type_name with
                                | TC_option o            -> (C_option , [o])
                                | TC_list l              -> (C_list   , [l])
                                | TC_set  s              -> (C_set    , [s])
                                | TC_map  ( k , v )      -> (C_map    , [k;v])
                                | TC_big_map  ( k , v )  -> (C_big_map, [k;v])
                                | TC_map_or_big_map ( k , v) -> (C_map, [k;v])
                                | TC_contract c          -> (C_contract, [c])
                                | TC_arrow ( arg , ret ) -> (C_arrow, [ arg ; ret ])
                          )
     in
     P_constant (csttag, List.map type_expression_to_type_value_copypasted args)

let failwith_ : unit -> (constraints * O.type_variable) = fun () ->
  let type_name = Core.fresh_type_variable () in
  [] , type_name

let variable : I.expression_variable -> T.type_expression -> (constraints * T.type_variable) = fun _name expr ->
  let pattern = type_expression_to_type_value expr in
  let type_name = Core.fresh_type_variable () in
  [C_equation (P_variable (type_name) , pattern)] , type_name

let literal : T.type_expression -> (constraints * T.type_variable) = fun t ->
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

let tuple : T.type_expression list -> (constraints * T.type_variable) = fun tys ->
  let patterns = List.map type_expression_to_type_value tys in
  let pattern = O.(P_constant (C_record , patterns)) in
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
  let t_application    = forall2 "a" "b" @@ fun a b -> (a --> b) --> a --> b
  let t_look_up        = forall2 "ind" "v" @@ fun ind v -> map ind v --> ind --> option v
  let t_sequence       = forall "b" @@ fun b -> unit --> b --> b
  let t_loop           = bool --> unit --> unit
end

(* TODO: I think we should take an I.expression for the base+label *)
let access_label ~(base : T.type_expression) ~(label : O.accessor) : (constraints * T.type_variable) =
  let base' = type_expression_to_type_value base in
  let expr_type = Core.fresh_type_variable () in
  [O.C_access_label (base' , label , expr_type)] , expr_type

let constructor
    : T.type_expression -> T.type_expression -> T.type_expression -> (constraints * T.type_variable)
  = fun t_arg c_arg sum ->
  let t_arg = type_expression_to_type_value t_arg in
  let c_arg = type_expression_to_type_value c_arg in
  let sum = type_expression_to_type_value sum in
  let whole_expr = Core.fresh_type_variable () in
  [
    C_equation (P_variable (whole_expr) , sum) ;
    C_equation (t_arg , c_arg)
  ] , whole_expr

let record : T.field_content T.label_map -> (constraints * T.type_variable) = fun fields ->
  let record_type = type_expression_to_type_value (T.t_record fields ()) in
  let whole_expr = Core.fresh_type_variable () in
  [C_equation (P_variable whole_expr , record_type)] , whole_expr

let collection : O.constant_tag -> T.type_expression list -> (constraints * T.type_variable) =
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

let map : (T.type_expression * T.type_expression) list -> (constraints * T.type_variable) =
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

let big_map : (T.type_expression * T.type_expression) list -> (constraints * T.type_variable) =
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
      (* TODO: this doesn't tag big_maps uniquely (i.e. if two
           big_map have the same type, they can be swapped. *)
      C_equation (P_variable whole_expr , O.P_constant (C_big_map , [k_type ; v_type]))
  ] @ equations_k @ equations_v , whole_expr

let application : T.type_expression -> T.type_expression -> (constraints * T.type_variable) =
  fun f arg ->
  let whole_expr = Core.fresh_type_variable () in
  let f'   = type_expression_to_type_value f in
  let arg' = type_expression_to_type_value arg in
  O.[
      C_equation (f' , P_constant (C_arrow , [arg' ; P_variable whole_expr]))
  ] , whole_expr

let look_up : T.type_expression -> T.type_expression -> (constraints * T.type_variable) =
  fun ds ind ->
  let ds'  = type_expression_to_type_value ds in
  let ind' = type_expression_to_type_value ind in
  let whole_expr = Core.fresh_type_variable () in
  let v = Core.fresh_type_variable () in
  O.[
      C_equation (ds' , P_constant (C_map, [ind' ; P_variable v])) ;
      C_equation (P_variable whole_expr , P_constant (C_option , [P_variable v]))
  ] , whole_expr

let sequence : T.type_expression -> T.type_expression -> (constraints * T.type_variable) =
  fun a b ->
  let a' = type_expression_to_type_value a in
  let b' = type_expression_to_type_value b in
  let whole_expr = Core.fresh_type_variable () in
  O.[
      C_equation (a' , P_constant (C_unit , [])) ;
      C_equation (b' , P_variable whole_expr)
  ] , whole_expr

let loop : T.type_expression -> T.type_expression -> (constraints * T.type_variable) =
  fun expr body ->
  let expr' = type_expression_to_type_value expr in
  let body' = type_expression_to_type_value body in
  let whole_expr = Core.fresh_type_variable () in
  O.[
      C_equation (expr'                 , P_constant (C_bool , [])) ;
      C_equation (body'                 , P_constant (C_unit , [])) ;
      C_equation (P_variable whole_expr , P_constant (C_unit , []))
  ] , whole_expr

let let_in : T.type_expression -> T.type_expression option -> T.type_expression -> (constraints * T.type_variable) =
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

let recursive : T.type_expression -> (constraints * T.type_variable) =
  fun fun_type ->
  let fun_type = type_expression_to_type_value fun_type in
  let whole_expr = Core.fresh_type_variable () in
  O.[
      C_equation (fun_type, P_variable whole_expr)
  ], whole_expr

let assign : T.type_expression -> T.type_expression -> (constraints * T.type_variable) =
  fun v e ->
  let v' = type_expression_to_type_value v in
  let e' = type_expression_to_type_value e in
  let whole_expr = Core.fresh_type_variable () in
  O.[
      C_equation (v' , e') ;
      C_equation (P_variable whole_expr , P_constant (C_unit , []))
  ] , whole_expr

let annotation : T.type_expression -> T.type_expression -> (constraints * T.type_variable) =
  fun e annot ->
  let e' = type_expression_to_type_value e in
  let annot' = type_expression_to_type_value annot in
  let whole_expr = Core.fresh_type_variable () in
  O.[
      C_equation (e' , annot') ;
      C_equation (e' , P_variable whole_expr)
  ] , whole_expr

let matching : T.type_expression list -> (constraints * T.type_variable) =
  fun es ->
  let whole_expr = Core.fresh_type_variable () in
  let type_expressions = (List.map type_expression_to_type_value es) in
  let cs = List.map (fun e -> O.C_equation (P_variable whole_expr , e)) type_expressions
  in cs, whole_expr

let fresh_binder () =
  Core.fresh_type_variable ()

let lambda
    : T.type_expression ->
      T.type_expression option ->
      T.type_expression option ->
      (constraints * T.type_variable) =
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

(* This is pretty much a wrapper for an n-ary function. *)
let constant : O.type_value -> T.type_expression list -> (constraints * T.type_variable) =
  fun f args ->
  let whole_expr = Core.fresh_type_variable () in
  let args'      = List.map type_expression_to_type_value args in
  let args_tuple = O.P_constant (C_record , args') in
  O.[
      C_equation (f , P_constant (C_arrow , [args_tuple ; P_variable whole_expr]))
  ] , whole_expr
