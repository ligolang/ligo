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

let reserved_names = (* Part of names in that list would be caught by some syntaxes *)
  List.map ~f:Value_var.of_input_var
  [ "get_force" ; "get_entrypoint";
    "bitwise_or"; "bitwise_and"; "bitwise_xor"; "string_concat"; "string_slice"; "crypto_check"; "crypto_hash_key";
    "bytes_concat"; "bytes_slice"; "bytes_pack"; "bytes_unpack"; "set_empty"; "set_mem"; "set_add"; "set_remove"; "set_iter"; "set_fold"; "list_iter";
    "list_fold"; "list_fold_left"; "list_fold_right"; "list_map"; "map_iter"; "map_map"; "map_fold"; "map_remove"; "map_update"; "map_get"; "map_mem";
    "continue";  "gas"; "hash"; "stop"; "time";
    "continue"; "debugger"; "do";
  ]
let check_reserved ~raise ~loc var =
  match List.find ~f:(Value_var.equal var) reserved_names with
  | Some v ->
    let str : string = Format.asprintf "%a" Value_var.pp v in
    let loc : Location.t = loc in
    raise.error (reserved_name str loc)
  | None -> ()

let reserved_names_exp ~raise : expression -> expression = fun exp ->
  match exp.expression_content with
  | E_let_in {let_binder ; _ } ->
    check_reserved ~raise ~loc:exp.location @@ Binder.get_var let_binder ;
    exp
  | E_lambda {binder ; _} ->
    check_reserved ~raise ~loc:exp.location @@ Param.get_var binder ;
    exp
  | E_matching { cases ; _ } ->
    let rec aux : type_expression option Pattern.t -> unit = fun p ->
      match p.wrap_content with
      | P_unit -> ()
      | P_var binder -> check_reserved ~raise ~loc:p.location @@ Binder.get_var binder
      | P_list (Cons (p1,p2)) ->
        let () = aux p1 in
        aux p2
      | P_list (List lst) | P_tuple lst | P_record (_,lst) ->
        let () = List.iter ~f:aux lst in
        ()
      | P_variant (_,p) ->
        aux p
    in
    List.iter ~f:aux (List.map ~f:(fun (x: _ Match_expr.match_case) -> x.pattern) cases) ;
    exp
  | _ -> exp

let reserved_names_program ~raise : program -> program = fun m ->
  let aux d = match d with
    | Location.{wrap_content = D_value {binder ; expr ; _ }; location = loc } ->
      check_reserved ~raise ~loc @@ Binder.get_var binder ;
      let _ : expression = reserved_names_exp ~raise expr in
      ()
    | {wrap_content = D_type _; _} -> ()
    | {wrap_content = D_module _ ; _} -> ()
  in
  List.iter ~f:aux m ;
  m
