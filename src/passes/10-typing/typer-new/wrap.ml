open Ast_typed.Misc
module Core = Typesystem.Core

module I = Ast_core
module T = Ast_typed
module O = Core

type constraints = O.type_constraint list

(* todo : use in the file *)
let fresh_binder () = Core.fresh_type_variable ()

let rec type_expression_to_type_value : T.type_expression -> O.type_value = fun te ->
  match te.type_content with
  | T_variable tv -> T.Reasons.wrap (Todo "wrap: from source code maybe?") @@ T.P_variable tv
  | T_singleton _ -> failwith "what about singleton ?"
  | T_sum {content=kvmap ; layout=_} ->
    let tmap = T.LMap.map (fun ({associated_type;_}:T.row_element) -> associated_type) kvmap in
    p_row C_variant @@ T.LMap.map type_expression_to_type_value tmap
  | T_record {content=kvmap;layout=_} ->
    let tmap = T.LMap.map (fun ({associated_type;_}:T.row_element) -> associated_type) kvmap in
    p_row C_record @@ T.LMap.map type_expression_to_type_value tmap
  | T_arrow {type1;type2} ->
    p_constant C_arrow @@ List.map type_expression_to_type_value [ type1 ; type2 ]
  | T_module_accessor {module_name=_; element} ->
    type_expression_to_type_value element
  | T_constant {language=_;injection;parameters} -> (
    let open Stage_common.Constant in
    let (csttag, args) = Option.unopt_exn @@ (* This will be removed later *)
    T.(match (Ligo_string.extract injection , parameters) with
      | ( s , [] ) when String.equal s unit_name -> Some (C_unit , [])
      | ( s , [] ) when String.equal s string_name-> Some (C_string , [])
      | ( s , [] ) when String.equal s nat_name-> Some (C_nat , [])
      | ( s , [] ) when String.equal s tez_name-> Some (C_mutez , [])
      | ( s , [] ) when String.equal s timestamp_name-> Some (C_timestamp , [])
      | ( s , [] ) when String.equal s int_name-> Some (C_int , [])
      | ( s , [] ) when String.equal s address_name-> Some (C_address , [])
      | ( s , [] ) when String.equal s bytes_name-> Some (C_bytes , [])
      | ( s , [] ) when String.equal s key_hash_name-> Some (C_key_hash , [])
      | ( s , [] ) when String.equal s key_name-> Some (C_key , [])
      | ( s , [] ) when String.equal s signature_name-> Some (C_signature , [])
      | ( s , [] ) when String.equal s operation_name-> Some (C_operation , [])
      | ( s , [] ) when String.equal s chain_id_name-> Some (C_chain_id , [])
      | ( s , [o] ) when String.equal s option_name -> Some (C_option, [o])
      | ( s , [p] ) when String.equal s set_name -> Some (C_set, [p])
      | ( s , [ k ; v ]) when String.equal s map_name -> Some (C_map, [k;v])
      | ( s , [ k ; v ]) when String.equal s big_map_name -> Some (C_big_map, [k;v])
      | ( s , [ k ; v ]) when String.equal s map_or_big_map_name -> Some (C_map, [k;v])
      | ( s , [l] ) when String.equal s list_name -> Some (C_list, [l])
      | ( s , [c] ) when String.equal s contract_name -> Some (C_contract, [c])
      | ( _ , _ ) -> None
      )
    in
    p_constant csttag @@ List.map type_expression_to_type_value args
  )

let variable : T.type_expression -> (constraints * T.type_variable) = fun expr ->
  let pattern = type_expression_to_type_value expr in
  let type_name = Core.fresh_type_variable () in
  let aval = T.Reasons.(wrap (Todo "wrap: variable: whole") (T.P_variable type_name)) in
  [{ c = C_equation { aval ; bval = pattern } ; reason = "wrap: variable" }] , type_name

let literal : T.type_expression -> (constraints * T.type_variable) = fun t ->
  let pattern = type_expression_to_type_value t in
  let type_name = Core.fresh_type_variable () in
  let aval = T.Reasons.(wrap (Todo "wrap: literal: whole") (T.P_variable type_name)) in
  [{ c = C_equation { aval ; bval = pattern } ; reason = "wrap: literal" }] , type_name

(* TODO : move to common *)
let lmap_of_tuple lst =
  let aux i e = (i+1,(T.Label (string_of_int i),e)) in
  T.LMap.of_list @@ List.fold_map aux 0 lst

(* This is pretty much a wrapper for an n-ary function. *)
(* TODO: change working of constant in ligo *)
let constant : O.type_value -> T.type_expression list -> (constraints * T.type_variable) =
  fun f args ->
  let whole_expr = Core.fresh_type_variable () in
  let args'      = lmap_of_tuple @@ List.map type_expression_to_type_value args in
  let args_tuple = p_row C_record args' in
  [
      c_equation f (p_constant C_arrow ([args_tuple ; (T.Reasons.wrap (Todo "wrap: lambda: whole") (T.P_variable whole_expr))])) "wrap: constant: as declared for built-in"
  ] , whole_expr

(* TODO : change type of lambda *)
let lambda
    : T.type_expression ->
      T.type_expression option ->
      T.type_expression option ->
      T.type_expression ->
      (constraints * T.type_variable) =
  fun fresh arg output result ->
  let whole_expr = Core.fresh_type_variable () in
  let unification_arg = T.( Reasons.wrap (Todo "wrap: lambda: arg") @@ P_variable (Core.fresh_type_variable ()) ) in
  let unification_output = T.( Reasons.wrap  (Todo "wrap: lambda: whole") @@ P_variable (Core.fresh_type_variable ()) ) in
  let result' = type_expression_to_type_value result in
  let arg'  = match arg with
      None -> []
    | Some arg -> [c_equation unification_arg (type_expression_to_type_value arg) "wrap: lambda: arg annot"] in
  let output'  = match output with
      None -> []
    | Some output -> [c_equation unification_output (type_expression_to_type_value output) "wrap: lambda: output annot"]
  in
    [
      c_equation unification_output result' "wrap: lambda: result" ;
      c_equation (type_expression_to_type_value fresh) unification_arg "wrap: lambda: arg" ;
      c_equation ((T.Reasons.wrap (Todo "wrap: lambda: whole") @@ T.P_variable whole_expr ))
                 (p_constant C_arrow ([unification_arg ; unification_output]))
                 "wrap: lambda: arrow (whole)"
    ] @ arg' @ output' , whole_expr

let application : T.type_expression -> T.type_expression -> (constraints * T.type_variable) =
  fun f arg ->
  let whole_expr = Core.fresh_type_variable () in
  let f'   = type_expression_to_type_value f in
  let arg' = type_expression_to_type_value arg in
  [
    c_equation f' (p_constant C_arrow [arg' ; T.Reasons.wrap (Todo "wrap: application: whole") @@ T.P_variable whole_expr ]) "wrap: application: f" ;
  ] , whole_expr

let constructor : T.type_expression -> T.type_expression -> T.type_expression -> (constraints * T.type_variable) = fun t_arg c_arg sum ->
  let t_arg = type_expression_to_type_value t_arg in
  let c_arg = type_expression_to_type_value c_arg in
  let sum = type_expression_to_type_value sum in
  let whole_expr = Core.fresh_type_variable () in
  [
    c_equation ( T.Reasons.wrap (Todo "wrap: constructor: whole") @@ T.P_variable whole_expr ) sum "wrap: constructor: whole" ;
    c_equation t_arg c_arg "wrap: construcotr: arg" ;
  ] , whole_expr

(* Constraint all branch of the matching to be equal *)
(* TODO : missing constraint that the matchee is equal to the cases ? *)
let matching : T.type_expression list -> (constraints * T.type_variable) =
  fun es ->
  let whole_expr = Core.fresh_type_variable () in
  let type_expressions = (List.map type_expression_to_type_value es) in
  let cs = List.map (fun e -> c_equation (T.Reasons.wrap (Todo "wrap: matching: case") @@ T.P_variable whole_expr) e "wrap: matching: case (whole)") type_expressions
  in cs, whole_expr

let record : T.rows -> (constraints * T.type_variable) = fun {content;layout} ->
  let record_type = type_expression_to_type_value (T.t_record ~layout content) in
  let whole_expr = Core.fresh_type_variable () in
  [c_equation (T.Reasons.wrap (Todo "wrap: record: whole") @@ T.P_variable whole_expr) record_type "wrap: record: whole"] , whole_expr

let access_label ~(base : T.type_expression) ~(label : O.accessor) : (constraints * T.type_variable) =
  let base' = type_expression_to_type_value base in
  let expr_type = Core.fresh_type_variable () in
  [{ c = C_access_label { c_access_label_tval = base' ; accessor = label ; c_access_label_tvar = expr_type } ; reason = "wrap: access_label" }] , expr_type

let module_access (expr : T.type_expression) : (constraints * T.type_variable) =
  let expr' = type_expression_to_type_value expr in
  let whole_expr = Core.fresh_type_variable () in
  [c_equation (T.Reasons.wrap (Todo "wrap: module: whole") @@ T.P_variable whole_expr) expr' "wrap: module: whole"] , whole_expr

let let_in : T.type_expression -> T.type_expression option -> T.type_expression -> (constraints * T.type_variable) =
  fun rhs rhs_tv_opt result ->
  let rhs'        = type_expression_to_type_value rhs in
  let result'     = type_expression_to_type_value result in
  let rhs_tv_opt' = match rhs_tv_opt with
      None -> []
    | Some annot -> [c_equation rhs' (type_expression_to_type_value annot) "wrap: let_in: rhs"] in
  let whole_expr = Core.fresh_type_variable () in
    c_equation result' (T.Reasons.wrap (Todo "wrap: let_in: whole") @@ T.P_variable whole_expr) "wrap: let_in: result (whole)"
  :: rhs_tv_opt', whole_expr

let type_in : T.type_expression -> (constraints * T.type_variable) =
  fun result ->
  let result'     = type_expression_to_type_value result in
  let whole_expr = Core.fresh_type_variable () in
  [
    c_equation result' (T.Reasons.wrap (Todo "wrap: type_in: whole") @@ T.P_variable whole_expr) "wrap: type_in: result (whole)"
  ], whole_expr

let mod_in : T.type_expression -> (constraints * T.type_variable) =
  fun result ->
  let result'     = type_expression_to_type_value result in
  let whole_expr = Core.fresh_type_variable () in
  [
    c_equation result' (T.Reasons.wrap (Todo "wrap: mod_in: whole") @@ T.P_variable whole_expr) "wrap: mod_in: result (whole)"
  ], whole_expr

let mod_alias : T.type_expression -> (constraints * T.type_variable) =
  fun result ->
  let result'     = type_expression_to_type_value result in
  let whole_expr = Core.fresh_type_variable () in
  [
    c_equation result' (T.Reasons.wrap (Todo "wrap: mod_alias: whole") @@ T.P_variable whole_expr) "wrap: mod_alias: result (whole)"
  ], whole_expr

let recursive : T.type_expression -> (constraints * T.type_variable) =
  fun fun_type ->
  let fun_type = type_expression_to_type_value fun_type in
  let whole_expr = Core.fresh_type_variable () in
  [
    c_equation fun_type (T.Reasons.wrap (Todo "wrap: recursive: whole") @@ T.P_variable whole_expr) "wrap: recursive: fun_type (whole)" ;
  ], whole_expr

let raw_code : T.type_expression -> (constraints * T.type_variable) =
  fun type_anno ->
  let type_anno = type_expression_to_type_value type_anno in
  let whole_expr = Core.fresh_type_variable () in
  [
    c_equation type_anno  (T.Reasons.wrap (Todo "wrap: raw_code: whole") @@ T.P_variable whole_expr) "wrap: raw_code: type_anno (whole)" ;
  ], whole_expr

let annotation : T.type_expression -> T.type_expression -> (constraints * T.type_variable) =
  fun e annot ->
  let e' = type_expression_to_type_value e in
  let annot' = type_expression_to_type_value annot in
  let whole_expr = Core.fresh_type_variable () in
  [
    c_equation e' annot' "wrap: annotation: expr type must eq annot" ;
    c_equation e' (T.Reasons.wrap (Todo "wrap: annotation: whole") @@ T.P_variable whole_expr) "wrap: annotation: whole" ;
  ] , whole_expr
