module FV = Ast_expanded.Helpers.Free_variables
open Ligo_prim
open Ast_expanded

type position = [`Tail | `Non_tail]
let is_in_tail : position -> bool = function `Tail -> true | `Non_tail -> false

let rec is_tail_recursive ~position : Value_var.t -> expression -> bool =
 fun n e ->
  match e.expression_content with
  | E_literal _ -> true
  | E_constant c -> List.for_all ~f:(is_tail_recursive ~position:`Non_tail n) c.arguments
  | E_variable v -> (is_in_tail position || not (Value_var.equal n v))
  | E_application { lamb; args } ->
    is_tail_recursive ~position n lamb &&
    is_tail_recursive ~position:`Non_tail n args
  | E_lambda { result; _ } -> is_tail_recursive ~position n result
  | E_type_abstraction { result; _ } -> is_tail_recursive ~position n result
  | E_recursive { fun_name = _; fun_type = _; lambda; force_lambdarec = _ } ->
    is_tail_recursive ~position n lambda.result
  | E_let_in { rhs; let_result; _ } ->
    is_tail_recursive ~position:`Non_tail n rhs &&
    is_tail_recursive ~position n let_result
  | E_raw_code _ -> true
  | E_constructor { element; _ } -> is_tail_recursive ~position:`Non_tail n element
  | E_matching { matchee; cases } ->
    is_tail_recursive ~position:`Non_tail n matchee &&
    is_tail_recursive_in_matching ~position n cases
  | E_record elm ->
    List.for_all ~f:(is_tail_recursive ~position:`Non_tail n) @@ Record.values elm
  | E_accessor { struct_; _ } -> is_tail_recursive ~position:`Non_tail n struct_
  | E_update { struct_; update; _ } ->
    is_tail_recursive ~position:`Non_tail n struct_ &&
    is_tail_recursive ~position:`Non_tail n update
  | E_type_inst { forall ; type_ = _ } ->
    is_tail_recursive ~position n forall
  (* Mutation *)
  | E_assign { expression; _ } ->
    is_tail_recursive ~position:`Non_tail n expression
  | E_let_mut_in { rhs; let_result; _ } ->
    is_tail_recursive ~position:`Non_tail n rhs &&
    is_tail_recursive ~position n let_result
  | E_deref _ -> true
  (* Loops *)
  | E_for _ -> false
  | E_for_each _ -> false
  | E_while _ -> false


and is_tail_recursive_in_matching ~position n ms =
  let aux_variant { constructor = _ ; pattern = _; body } =
    is_tail_recursive ~position n body
  in
  match ms with
  | Match_variant { cases ; tv = _ } -> List.for_all ~f:aux_variant cases
  | Match_record { fields = _ ; body ; tv = _ } -> is_tail_recursive ~position n body

let is_rec_binder_shadowed ~fun_name ~(lambda : _ Lambda.t) =
  let fv = FV.expression lambda.result in
  let is_binder_shadowed_in_body = not @@ List.mem fv fun_name ~equal:Value_var.equal in
  Value_var.equal fun_name (Param.get_var lambda.binder) || is_binder_shadowed_in_body

let is_tail_recursive : _ -> _ -> bool =
 fun fun_name lambda ->
  is_rec_binder_shadowed ~fun_name ~lambda || is_tail_recursive ~position:`Tail fun_name lambda.result
