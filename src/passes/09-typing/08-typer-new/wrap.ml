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
  | T_sum kvmap ->
    let tmap = T.LMap.map (fun ({associated_type;_}:T.row_element) -> associated_type) kvmap in
    p_row C_variant @@ T.LMap.map type_expression_to_type_value tmap
  | T_record kvmap ->
    let tmap = T.LMap.map (fun ({associated_type;_}:T.row_element) -> associated_type) kvmap in
    p_row C_record @@ T.LMap.map type_expression_to_type_value tmap
  | T_arrow {type1;type2} ->
    p_constant C_arrow @@ List.map type_expression_to_type_value [ type1 ; type2 ]

  | T_variable (type_name) -> { tsrc = "wrap: from source code maybe?" ; t = P_variable type_name }
  | T_wildcard -> { tsrc = "wrap: from source code" ; t = P_variable (Var.fresh ()) }
  | T_constant {type_constant; arguments} ->
    let (csttag, args) = Option.unopt_exn @@ (* This will be removed later *)
      T.(match type_constant,arguments with
        | TC_unit          , []         -> Some (C_unit , [])
        | TC_string        , []         -> Some (C_string , [])
        | TC_nat           , []         -> Some (C_nat , [])
        | TC_mutez         , []         -> Some (C_mutez , [])
        | TC_timestamp     , []         -> Some (C_timestamp , [])
        | TC_int           , []         -> Some (C_int , [])
        | TC_address       , []         -> Some (C_address , [])
        | TC_bytes         , []         -> Some (C_bytes , [])
        | TC_key_hash      , []         -> Some (C_key_hash , [])
        | TC_key           , []         -> Some (C_key , [])
        | TC_signature     , []         -> Some (C_signature , [])
        | TC_operation     , []         -> Some (C_operation , [])
        | TC_chain_id      , []         -> failwith "TODO : figure out what to do with chain_id; ask Tom Jack"
        | TC_option        , [o]        -> Some (C_option, [o])
        | TC_set           , [s]        -> Some (C_set, [s])
        | TC_map           , [ k ; v ]  -> Some (C_map, [k;v])
        | TC_big_map       , [ k ; v ]  -> Some (C_big_map, [k;v])
        | TC_map_or_big_map, [ k ; v ]  -> Some (C_map, [k;v])
        | TC_list          , [l]        -> Some (C_list, [l])
        | TC_contract      , [c]        -> Some (C_contract, [c])
        | _ -> None
      )
    in
    p_constant csttag @@ List.map type_expression_to_type_value args

let variable : T.type_expression -> (constraints * T.type_variable) = fun expr ->
  let pattern = type_expression_to_type_value expr in
  let type_name = Core.fresh_type_variable () in
  [{ c = C_equation { aval = { tsrc = "wrap: variable: whole" ; t = P_variable type_name } ; bval = pattern } ; reason = "wrap: variable" }] , type_name

let literal : T.type_expression -> (constraints * T.type_variable) = fun t ->
  let pattern = type_expression_to_type_value t in
  let type_name = Core.fresh_type_variable () in
  [{ c = C_equation { aval = { tsrc = "wrap: literal: whole" ; t = P_variable type_name } ; bval = pattern } ; reason = "wrap: literal" }] , type_name

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
      c_equation f (p_constant C_arrow ([args_tuple ; { tsrc = "wrap: lambda: whole" ; t = P_variable whole_expr }])) "wrap: constant: as declared for built-in"
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
  let unification_arg = T.{ tsrc = "wrap: lambda: arg" ; t = P_variable (Core.fresh_type_variable ()) } in
  let unification_output = T.{ tsrc = "wrap: lambda: whole" ; t = P_variable (Core.fresh_type_variable ()) } in
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
      c_equation ({ tsrc = "wrap: lambda: whole" ; t = P_variable whole_expr })
                 (p_constant C_arrow ([unification_arg ; unification_output]))
                 "wrap: lambda: arrow (whole)"
    ] @ arg' @ output' , whole_expr

let application : T.type_expression -> T.type_expression -> (constraints * T.type_variable) =
  fun f arg ->
  let whole_expr = Core.fresh_type_variable () in
  let f'   = type_expression_to_type_value f in
  let arg' = type_expression_to_type_value arg in
  [
      c_equation f' (p_constant C_arrow [arg' ; { tsrc = "wrap: application: whole" ; t = P_variable whole_expr }]) "wrap: application: f" ;
  ] , whole_expr

let constructor : T.type_expression -> T.type_expression -> T.type_expression -> (constraints * T.type_variable) = fun t_arg c_arg sum ->
  let t_arg = type_expression_to_type_value t_arg in
  let c_arg = type_expression_to_type_value c_arg in
  let sum = type_expression_to_type_value sum in
  let whole_expr = Core.fresh_type_variable () in
  [
    c_equation { tsrc = "wrap: constructor: whole" ; t = P_variable whole_expr } sum "wrap: constructor: whole" ;
    c_equation t_arg c_arg "wrap: construcotr: arg" ;
  ] , whole_expr

(* Constraint all branch of the matching to be equal *)
(* TODO : missing constraint that the matchee is equal to the cases ? *)
let matching : T.type_expression list -> (constraints * T.type_variable) =
  fun es ->
  let whole_expr = Core.fresh_type_variable () in
  let type_expressions = (List.map type_expression_to_type_value es) in
  let cs = List.map (fun e -> c_equation { tsrc = "wrap: matching: case" ; t = P_variable whole_expr } e "wrap: matching: case (whole)") type_expressions
  in cs, whole_expr

let record : T.row_element T.label_map -> (constraints * T.type_variable) = fun fields ->
  let record_type = type_expression_to_type_value (T.t_record fields ()) in
  let whole_expr = Core.fresh_type_variable () in
  [c_equation { tsrc = "wrap: record: whole" ; t = P_variable whole_expr } record_type "wrap: record: whole"] , whole_expr

let access_label ~(base : T.type_expression) ~(label : O.accessor) : (constraints * T.type_variable) =
  let base' = type_expression_to_type_value base in
  let expr_type = Core.fresh_type_variable () in
  [{ c = C_access_label { c_access_label_tval = base' ; accessor = label ; c_access_label_tvar = expr_type } ; reason = "wrap: access_label" }] , expr_type

let let_in : T.type_expression -> T.type_expression option -> T.type_expression -> (constraints * T.type_variable) =
  fun rhs rhs_tv_opt result ->
  let rhs'        = type_expression_to_type_value rhs in
  let result'     = type_expression_to_type_value result in
  let rhs_tv_opt' = match rhs_tv_opt with
      None -> []
    | Some annot -> [c_equation rhs' (type_expression_to_type_value annot) "wrap: let_in: rhs"] in
  let whole_expr = Core.fresh_type_variable () in
    c_equation result' { tsrc = "wrap: let_in: whole" ; t = P_variable whole_expr } "wrap: let_in: result (whole)"
  :: rhs_tv_opt', whole_expr

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

let annotation : T.type_expression -> T.type_expression -> (constraints * T.type_variable) =
  fun e annot ->
  let e' = type_expression_to_type_value e in
  let annot' = type_expression_to_type_value annot in
  let whole_expr = Core.fresh_type_variable () in
  [
      c_equation e' annot' "wrap: annotation: expr type must eq annot" ;
      c_equation e' { tsrc = "wrap: annotation: whole" ; t = P_variable whole_expr } "wrap: annotation: whole" ;
  ] , whole_expr
