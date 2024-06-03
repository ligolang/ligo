(*
 This algorithm remove duplicate variables name in the same scope to remove shadowing.
*)
open Ligo_prim
open Ast_aggregated
open Errors

module Scope : sig
  type t

  val empty : t
  val new_value_var : t -> Value_var.t -> t * Value_var.t
  val get_value_var : t -> Value_var.t -> Value_var.t
  val get_type_var : t -> Type_var.t -> Type_var.t
  val new_mut_var : t -> Value_var.t -> t * Value_var.t
  val get_mut_var : t -> Value_var.t -> Value_var.t

  type swapper =
    { value : Value_var.t -> Value_var.t
    ; type_ : Type_var.t -> Type_var.t
    ; mut : Value_var.t -> Value_var.t
    }

  val make_swapper : t -> swapper
end = struct
  module VMap = Simple_utils.Map.Make (Value_var)
  module TMap = Simple_utils.Map.Make (Type_var)

  type t =
    { value : Value_var.t VMap.t
    ; type_ : Type_var.t TMap.t
    ; mut : Value_var.t VMap.t
    }

  let empty = { value = VMap.empty; type_ = TMap.empty; mut = VMap.empty }

  let new_value_var map var =
    let var' =
      match VMap.find_opt var map.value with
      | Some v -> Value_var.fresh_like ~loc:(Value_var.get_location var) v
      | None -> Value_var.fresh_like var
    in
    let value = VMap.add var var' map.value in
    { map with value }, var'


  let get_value_var map var =
    (* The default value is for variable coming from other files *)
    Option.value ~default:var @@ VMap.find_opt var map.value
    |> Value_var.set_location @@ Value_var.get_location var


  let get_type_var (map : t) var =
    (* The default value is for variable coming from other files *)
    Option.value ~default:var @@ TMap.find_opt var map.type_
    |> Type_var.set_location @@ Type_var.get_location var


  type swapper =
    { value : Value_var.t -> Value_var.t
    ; type_ : Type_var.t -> Type_var.t
    ; mut : Value_var.t -> Value_var.t
    }

  let make_swapper (scope : t) : swapper =
    let swap_value = List.map ~f:(fun (k, v) -> v, k) in
    let value = VMap.of_list @@ swap_value @@ VMap.to_kv_list scope.value in
    let type_ = TMap.of_list @@ swap_value @@ TMap.to_kv_list scope.type_ in
    let mut = VMap.of_list @@ swap_value @@ VMap.to_kv_list scope.mut in
    { value = (fun map v -> Option.value ~default:v @@ VMap.find_opt v map) value
    ; type_ = (fun map v -> Option.value ~default:v @@ TMap.find_opt v map) type_
    ; mut = (fun map v -> Option.value ~default:v @@ VMap.find_opt v map) mut
    }


  let get_mut_var (map : t) var =
    Option.value ~default:var @@ VMap.find_opt var map.mut
    |> Value_var.set_location @@ Value_var.get_location var


  let new_mut_var (map : t) var =
    let var' =
      match VMap.find_opt var map.mut with
      | Some v -> Value_var.fresh_like ~loc:(Value_var.get_location var) v
      | None -> Value_var.fresh_like var
    in
    let mut = VMap.add var var' map.mut in
    { map with mut }, var'
end

let rec swap_type_expression ~(raise : _ Trace.raise)
    : Scope.swapper -> type_expression -> type_expression
  =
 fun swaper te ->
  let self = swap_type_expression ~raise swaper in
  let return type_content = { te with type_content } in
  match te.type_content with
  | T_variable ty_var ->
    let ty_var = swaper.type_ ty_var in
    return @@ T_variable ty_var
  | T_sum (row, orig_name) ->
    let row = Row.map self row in
    return @@ T_sum (row, orig_name)
  | T_record row ->
    let row = Row.map self row in
    return @@ T_record row
  | T_arrow { type1; type2; param_names } ->
    let type1 = self type1 in
    let type2 = self type2 in
    return @@ T_arrow { type1; type2; param_names }
  | T_constant { language; injection; parameters } ->
    let parameters = List.map ~f:self parameters in
    return @@ T_constant { language; injection; parameters }
  | T_singleton literal -> return @@ T_singleton literal
  | T_for_all { ty_binder; kind; type_ } ->
    let ty_binder = swaper.type_ ty_binder in
    let type_ = self type_ in
    return @@ T_for_all { ty_binder; kind; type_ }
  | T_abstraction { ty_binder; kind; type_ } ->
    let ty_binder = swaper.type_ ty_binder in
    let type_ = self type_ in
    return @@ T_abstraction { ty_binder; kind; type_ }
  | T_exists _ -> raise.error @@ unexpected_texists te te.location


let swap_binder ~(raise : _ Trace.raise) : Scope.swapper -> _ Binder.t -> _ Binder.t =
 fun swaper binder ->
  let self_type = swap_type_expression ~raise swaper in
  let var = Binder.apply swaper.value binder in
  let binder = Binder.map self_type binder in
  let binder = Binder.set_var binder var in
  binder


let swap_param ~(raise : _ Trace.raise) : Scope.swapper -> _ Param.t -> _ Param.t =
 fun swaper param ->
  let self_type = swap_type_expression ~raise swaper in
  let var_swapper =
    match Param.get_mut_flag param with
    | Immutable -> swaper.value
    | Mutable -> swaper.mut
  in
  let param = Param.map self_type param in
  let param = Param.set_var param (var_swapper @@ Param.get_var param) in
  param


let swap_mut_binder ~(raise : _ Trace.raise) : Scope.swapper -> _ Binder.t -> _ Binder.t =
 fun swaper binder ->
  let self_type = swap_type_expression ~raise swaper in
  let var = Binder.apply swaper.mut binder in
  let binder = Binder.map self_type binder in
  let binder = Binder.set_var binder var in
  binder


let swap_pattern ~(raise : _ Trace.raise) : Scope.swapper -> _ Pattern.t -> _ Pattern.t =
 fun swaper binder ->
  Pattern.map_pattern
    (fun p ->
      match p.wrap_content with
      | P_var b -> { p with wrap_content = P_var (swap_binder ~raise swaper b) }
      | _ -> p)
    binder


let swap_mut_pattern ~(raise : _ Trace.raise)
    : Scope.swapper -> _ Pattern.t -> _ Pattern.t
  =
 fun swaper binder ->
  Pattern.map_pattern
    (fun p ->
      match p.wrap_content with
      | P_var b -> { p with wrap_content = P_var (swap_mut_binder ~raise swaper b) }
      | _ -> p)
    binder


let rec swap_expression ~(raise : _ Trace.raise)
    : Scope.swapper -> expression -> expression
  =
 fun swaper e ->
  let self = swap_expression ~raise swaper in
  let self_type = swap_type_expression ~raise swaper in
  let return expression_content = { e with expression_content } in
  match e.expression_content with
  | E_literal literal -> return @@ E_literal literal
  | E_constant { cons_name; arguments } ->
    let arguments = List.map ~f:self arguments in
    return @@ E_constant { cons_name; arguments }
  | E_variable variable ->
    let variable = swaper.value variable in
    return @@ E_variable variable
  | E_application { lamb; args } ->
    let lamb = self lamb in
    let args = self args in
    return @@ E_application { lamb; args }
  | E_lambda { binder; output_type; result } ->
    let binder = swap_param ~raise swaper binder in
    let output_type = self_type output_type in
    let result = self result in
    return @@ E_lambda { binder; output_type; result }
  | E_type_abstraction { type_binder; result } ->
    let type_binder = swaper.type_ type_binder in
    let result = self result in
    return @@ E_type_abstraction { type_binder; result }
  | E_recursive
      { fun_name; fun_type; lambda = { binder; output_type; result }; force_lambdarec } ->
    let fun_name = swaper.value fun_name in
    let fun_type = self_type fun_type in
    let binder = swap_param ~raise swaper binder in
    let output_type = self_type output_type in
    let result = self result in
    return
    @@ E_recursive
         { fun_name; fun_type; lambda = { binder; output_type; result }; force_lambdarec }
  | E_let_in { let_binder; rhs; let_result; attributes } ->
    let let_binder = swap_pattern ~raise swaper let_binder in
    let rhs = self rhs in
    let let_result = self let_result in
    return @@ E_let_in { let_binder; rhs; let_result; attributes }
  | E_type_inst { forall; type_ } ->
    let forall = self forall in
    let type_ = self_type type_ in
    return @@ E_type_inst { forall; type_ }
  | E_raw_code { language; code } ->
    let code = self code in
    return @@ E_raw_code { language; code }
  | E_constructor { constructor; element } ->
    let element = self element in
    return @@ E_constructor { constructor; element }
  | E_matching { matchee; disc_label; cases } ->
    let matchee = self matchee in
    let cases = matching_cases ~raise swaper cases in
    return @@ E_matching { matchee; disc_label; cases }
  | E_record record ->
    let record = Record.map ~f:self record in
    return @@ E_record record
  | E_accessor { struct_; path } ->
    let struct_ = self struct_ in
    return @@ E_accessor { struct_; path }
  | E_update { struct_; path; update } ->
    let struct_ = self struct_ in
    let update = self update in
    return @@ E_update { struct_; path; update }
  | E_assign { binder; expression } ->
    let binder = swap_mut_binder ~raise swaper binder in
    let expression = self expression in
    return @@ E_assign { binder; expression }
  | E_coerce { anno_expr; type_annotation } ->
    let anno_expr = self anno_expr in
    let type_annotation = self_type type_annotation in
    return @@ E_coerce { anno_expr; type_annotation }
  | E_let_mut_in { let_binder; rhs; let_result; attributes } ->
    let let_binder = swap_mut_pattern ~raise swaper let_binder in
    let rhs = self rhs in
    let let_result = self let_result in
    return @@ E_let_mut_in { let_binder; rhs; let_result; attributes }
  | E_while while_loop ->
    let while_loop = While_loop.map self while_loop in
    return @@ E_while while_loop
  | E_for { binder; start; final; incr; f_body } ->
    let binder = swaper.value binder in
    let start = self start
    and final = self final
    and incr = self incr
    and f_body = self f_body in
    return @@ E_for { binder; start; final; incr; f_body }
  | E_for_each { fe_binder = binder1, binder2; collection; collection_type; fe_body } ->
    let fe_binder = swaper.value binder1, Option.map binder2 ~f:swaper.value in
    let collection = self collection
    and fe_body = self fe_body in
    return @@ E_for_each { fe_binder; collection; collection_type; fe_body }
  | E_deref mut_var -> return @@ E_deref (swaper.mut mut_var)


and matching_cases ~(raise : _ Trace.raise)
    :  Scope.swapper -> ('e, 'ty) Match_expr.match_case list
    -> ('e, 'ty) Match_expr.match_case list
  =
 fun swaper cases ->
  List.map cases ~f:(fun { pattern; body } ->
      let body = swap_expression ~raise swaper body in
      let pattern = swap_pattern ~raise swaper pattern in
      ({ pattern; body } : _ Match_expr.match_case))


let rec type_expression ~(raise : _ Trace.raise)
    : Scope.t -> type_expression -> type_expression
  =
 fun scope te ->
  let self ?(scope = scope) = type_expression ~raise scope in
  let return type_content = { te with type_content } in
  match te.type_content with
  | T_variable ty_var ->
    let ty_var = Scope.get_type_var scope ty_var in
    return @@ T_variable ty_var
  | T_sum (row, orig_name) ->
    let row = Row.map self row in
    return @@ T_sum (row, orig_name)
  | T_record row ->
    let row = Row.map self row in
    return @@ T_record row
  | T_arrow { type1; type2; param_names } ->
    let type1 = self type1 in
    let type2 = self type2 in
    return @@ T_arrow { type1; type2; param_names }
  | T_constant { language; injection; parameters } ->
    let parameters = List.map ~f:self parameters in
    return @@ T_constant { language; injection; parameters }
  | T_singleton literal -> return @@ T_singleton literal
  | T_for_all { ty_binder; kind; type_ } ->
    (* With current implementation of polymorphism, deshadowing type var breaks stuff *)
    (* let scope,ty_binder = Scope.new_type_var scope ty_binder in *)
    let type_ = self ~scope type_ in
    return @@ T_for_all { ty_binder; kind; type_ }
  | T_abstraction { ty_binder; kind; type_ } ->
    let type_ = self ~scope type_ in
    return @@ T_abstraction { ty_binder; kind; type_ }
  | T_exists _ -> raise.error @@ unexpected_texists te te.location


let binder_new ~(raise : _ Trace.raise) : Scope.t -> _ Binder.t -> Scope.t * _ Binder.t =
 fun scope binder ->
  let self_type ?(scope = scope) = type_expression ~raise scope in
  let scope, var = Binder.apply (Scope.new_value_var scope) binder in
  let binder = Binder.map self_type binder in
  let binder = Binder.set_var binder var in
  scope, binder


let pattern_new ~(raise : _ Trace.raise) : Scope.t -> _ Pattern.t -> Scope.t * _ Pattern.t
  =
 fun scope pattern ->
  Pattern.fold_map_pattern
    (fun scope pattern ->
      match pattern.wrap_content with
      | P_var binder ->
        let scope, binder = binder_new ~raise scope binder in
        scope, { pattern with wrap_content = P_var binder }
      | _ -> scope, pattern)
    scope
    pattern


let mut_binder_new ~(raise : _ Trace.raise)
    : Scope.t -> _ Binder.t -> Scope.t * _ Binder.t
  =
 fun scope binder ->
  let self_type ?(scope = scope) = type_expression ~raise scope in
  let scope, var = Binder.apply (Scope.new_mut_var scope) binder in
  let binder = Binder.map self_type binder in
  let binder = Binder.set_var binder var in
  scope, binder


let mut_pattern_new ~(raise : _ Trace.raise)
    : Scope.t -> _ Pattern.t -> Scope.t * _ Pattern.t
  =
 fun scope pattern ->
  Pattern.fold_map_pattern
    (fun scope pattern ->
      match pattern.wrap_content with
      | P_var binder ->
        let scope, binder = mut_binder_new ~raise scope binder in
        scope, { pattern with wrap_content = P_var binder }
      | _ -> scope, pattern)
    scope
    pattern


let param_new ~(raise : _ Trace.raise) : Scope.t -> _ Param.t -> Scope.t * _ Param.t =
 fun scope param ->
  let self_type ?(scope = scope) = type_expression ~raise scope in
  let new_var =
    match Param.get_mut_flag param with
    | Immutable -> Scope.new_value_var
    | Mutable -> Scope.new_mut_var
  in
  let scope, var = new_var scope @@ Param.get_var param in
  let param = Param.map self_type param in
  let param = Param.set_var param var in
  scope, param


let mut_binder_get ~(raise : _ Trace.raise) : Scope.t -> _ Binder.t -> _ Binder.t =
 fun scope binder ->
  let self_type ?(scope = scope) = type_expression ~raise scope in
  let var = Binder.apply (Scope.get_mut_var scope) binder in
  let binder = Binder.map self_type binder in
  Binder.set_var binder var


let rec expression ~(raise : _ Trace.raise)
    : Scope.t -> expression -> Scope.t * expression
  =
 fun scope e ->
  let self ?(scope = scope) = expression ~raise scope in
  let self_type ?(scope = scope) = type_expression ~raise scope in
  let return ?(scope = scope) expression_content = scope, { e with expression_content } in
  match e.expression_content with
  | E_literal literal -> return @@ E_literal literal
  | E_constant { cons_name; arguments } ->
    let _, arguments = List.unzip @@ List.map ~f:self arguments in
    return @@ E_constant { cons_name; arguments }
  | E_variable variable ->
    let variable = Scope.get_value_var scope variable in
    return @@ E_variable variable
  | E_application { lamb; args } ->
    let _, lamb = self lamb in
    let _, args = self args in
    return @@ E_application { lamb; args }
  | E_lambda { binder; output_type; result } ->
    let scope, binder = param_new ~raise scope binder in
    let output_type = self_type output_type in
    let _, result = self ~scope result in
    return @@ E_lambda { binder; output_type; result }
  | E_type_abstraction { type_binder; result } ->
    (* With current implementation of polymorphism, deshadowing type var breaks stuff *)
    (* let scope,type_binder = Scope.new_type_var scope type_binder in *)
    let _, result = self ~scope result in
    return @@ E_type_abstraction { type_binder; result }
  | E_recursive
      { fun_name; fun_type; lambda = { binder; output_type; result }; force_lambdarec } ->
    let fun_name = Scope.get_value_var scope fun_name in
    let fun_type = self_type fun_type in
    let scope, binder = param_new ~raise scope binder in
    let output_type = self_type output_type in
    let _, result = self ~scope result in
    return
    @@ E_recursive
         { fun_name; fun_type; lambda = { binder; output_type; result }; force_lambdarec }
  | E_let_in
      { let_binder
      ; rhs = { expression_content = E_recursive _ } as rhs
      ; let_result
      ; attributes
      } ->
    let scope, let_binder = pattern_new ~raise scope let_binder in
    let _, rhs = self ~scope rhs in
    let scope, let_result = self ~scope let_result in
    return ~scope @@ E_let_in { let_binder; rhs; let_result; attributes }
  | E_let_in { let_binder; rhs; let_result; attributes } ->
    let scope, let_binder = pattern_new ~raise scope let_binder in
    let _, rhs = self rhs in
    let scope, let_result = self ~scope let_result in
    return ~scope @@ E_let_in { let_binder; rhs; let_result; attributes }
  | E_type_inst { forall; type_ } ->
    let _, forall = self forall in
    let type_ = self_type type_ in
    return @@ E_type_inst { forall; type_ }
  | E_raw_code { language; code } ->
    let _, code = self code in
    return @@ E_raw_code { language; code }
  | E_constructor { constructor; element } ->
    let _, element = self element in
    return @@ E_constructor { constructor; element }
  | E_matching { matchee; disc_label; cases } ->
    let _, matchee = self matchee in
    let cases = matching_cases ~raise scope cases in
    return @@ E_matching { matchee; disc_label; cases }
  | E_record record ->
    let record = Record.map ~f:(fun elem -> snd @@ self elem) record in
    return @@ E_record record
  | E_accessor { struct_; path } ->
    let _, struct_ = self struct_ in
    return @@ E_accessor { struct_; path }
  | E_update { struct_; path; update } ->
    let _, struct_ = self struct_ in
    let _, update = self update in
    return @@ E_update { struct_; path; update }
  | E_assign { binder; expression } ->
    let binder = mut_binder_get ~raise scope binder in
    let _, expression = self expression in
    return @@ E_assign { binder; expression }
  | E_coerce { anno_expr; type_annotation } ->
    let _, anno_expr = self anno_expr in
    let type_annotation = self_type type_annotation in
    return @@ E_coerce { anno_expr; type_annotation }
  | E_let_mut_in { let_binder; rhs; let_result; attributes } ->
    let scope, let_binder = mut_pattern_new ~raise scope let_binder in
    let _, rhs = self rhs in
    let scope, let_result = self ~scope let_result in
    return ~scope @@ E_let_mut_in { let_binder; rhs; let_result; attributes }
  | E_while { cond; body } ->
    let _, cond = self cond in
    let _, body = self body in
    return @@ E_while { cond; body }
  | E_for { binder; start; final; incr; f_body } ->
    let scope, binder = Scope.new_value_var scope binder in
    let _, start = self start
    and _, final = self final
    and _, incr = self incr
    and scope, f_body = self ~scope f_body in
    return ~scope @@ E_for { binder; start; final; incr; f_body }
  | E_for_each { fe_binder = binder1, binder2; collection; collection_type; fe_body } ->
    let scope, binder1 = Scope.new_value_var scope binder1 in
    let scope, binder2 = Option.fold_map Scope.new_value_var scope binder2 in
    let _, collection = self collection
    and scope, fe_body = self ~scope fe_body in
    return ~scope
    @@ E_for_each { fe_binder = binder1, binder2; collection; collection_type; fe_body }
  | E_deref mut_var -> return @@ E_deref (Scope.get_mut_var scope mut_var)


and matching_cases ~(raise : _ Trace.raise)
    : Scope.t -> _ Match_expr.match_case list -> _ Match_expr.match_case list
  =
 fun scope cases ->
  let self ?(scope = scope) = expression ~raise scope in
  List.map cases ~f:(fun { pattern; body } ->
      let scope, pattern = pattern_new ~raise scope pattern in
      let _, body = self ~scope body in
      ({ pattern; body } : _ Match_expr.match_case))


let program ~(raise : _ Trace.raise) : expression -> expression =
 fun e ->
  let scope = Scope.empty in
  let scope, e = expression ~raise scope e in
  let swapper = Scope.make_swapper scope in
  swap_expression ~raise swapper e
