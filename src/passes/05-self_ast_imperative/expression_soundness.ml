module Var = Simple_utils.Var
module Trace = Simple_utils.Trace
open Ligo_prim
open Ast_imperative
open Errors
open Simple_utils.Trace

let check_linearity_pattern ~raise p =
  let c = List.contains_dup ~compare:Binder.compare_var (Pattern.binders p) in
  if c then raise.error (non_linear_pattern p)


let check_linearity_record_fields ~raise : expression -> unit =
 fun exp ->
  match exp.expression_content with
  | E_record x ->
    if List.contains_dup ~compare:Label.compare @@ List.map ~f:fst x
    then raise.error (non_linear_record exp)
  | _ -> ()


let check_linearity_patterns ~raise : expression -> unit =
 fun exp ->
  match exp.expression_content with
  | E_let_in { let_binder; _ } | E_let_mut_in { let_binder; _ } ->
    check_linearity_pattern ~raise let_binder
  | E_matching x ->
    let ps = List.map ~f:(fun x -> x.pattern) x.cases in
    List.iter ps ~f:(check_linearity_pattern ~raise)
  | _ -> ()


let checks_linearity ~raise : expression -> unit =
 fun x ->
  check_linearity_record_fields ~raise x;
  check_linearity_patterns ~raise x;
  ()


let linearity_prg ~raise : program -> program =
 fun x ->
  let f : declaration -> unit =
   fun x ->
    match x.wrap_content with
    | D_irrefutable_match { pattern; _ } -> check_linearity_pattern ~raise pattern
    (* Shouldn't we be checking  *)
    | D_value _ | D_type _ | D_module _ | D_contract _ -> ()
  in
  List.iter ~f x;
  x


let linearity ~raise m =
  checks_linearity ~raise m;
  m
