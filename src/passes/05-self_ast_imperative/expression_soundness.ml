module Var = Simple_utils.Var
module Trace = Simple_utils.Trace
open Ast_imperative
open Errors
open Simple_utils.Trace

let check_linearity_record_fields ~raise : expression -> unit = fun exp ->
  match exp.expression_content with
  | E_record x ->
    let labels = List.map ~f:(fun (Label x,_) -> x) x in
    if List.contains_dup ~compare:String.compare labels then raise.raise (non_linear_record exp)
  | _ -> ()

let check_linearity_patterns ~raise : expression -> unit = fun exp ->
  match exp.expression_content with
  | E_matching x ->
    let _patterns = List.map ~f:(fun x -> x.pattern) x.cases in
    let rec aux : expression_variable list -> type_expression pattern -> expression_variable list = fun vlst p ->
      match p.wrap_content with
      | P_var (x : type_expression binder) -> x.var::vlst
      | P_unit -> vlst
      | P_variant (_,p) -> aux vlst p
      | P_list (Cons (p1,p2)) ->
        List.fold ~init:vlst ~f:aux [p1;p2]
      | P_list (List lst) | P_tuple lst | P_record (_,lst) ->
        List.fold ~init:vlst ~f:aux lst
    in
    List.iter _patterns
      ~f:(fun p ->
        let lst = aux [] p in
        if List.contains_dup ~compare:Var.compare lst then raise.raise (non_linear_pattern p)
      )
  | _ -> ()

let checks_linearity : raise:[<Errors.self_ast_imperative_error] Trace.raise -> expression -> unit =
  fun ~raise x ->
    check_linearity_record_fields ~raise x;
    check_linearity_patterns ~raise x;
    ()
  

let linearity ~(raise:[<Errors.self_ast_imperative_error] Trace.raise) m = (fun x -> checks_linearity ~raise x ; x) m