module Var = Simple_utils.Var
module Trace = Simple_utils.Trace
open Ast_imperative
open Errors
open Simple_utils.Trace
open Ligo_prim

let check_linearity_record_fields ~raise : expression -> unit = fun exp ->
  match exp.expression_content with
  | E_record x ->
    if List.contains_dup ~compare:Label.compare @@ List.map ~f:fst x
      then raise.error (non_linear_record exp)
  | _ -> ()

let check_linearity_patterns ~raise : expression -> unit = fun exp ->
  match exp.expression_content with
  | E_matching x ->
    let _patterns = List.map ~f:(fun x -> x.pattern) x.cases in
    let rec aux : Value_var.t list -> type_expression option Pattern.t -> Value_var.t list = fun vlst p ->
      match p.wrap_content with
      | P_var (x : type_expression option Binder.t) -> Binder.get_var x::vlst
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
        if List.contains_dup ~compare:Value_var.compare lst then raise.error (non_linear_pattern p)
      )
  | _ -> ()

let checks_linearity : raise:([<Errors.self_ast_imperative_error],_) Trace.raise -> expression -> unit =
  fun ~raise x ->
    check_linearity_record_fields ~raise x;
    check_linearity_patterns ~raise x;
    ()


let linearity ~(raise:([<Errors.self_ast_imperative_error],_) Trace.raise) m = (fun x -> checks_linearity ~raise x ; x) m
