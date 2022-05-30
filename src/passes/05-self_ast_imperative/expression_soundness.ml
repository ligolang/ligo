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
        if List.contains_dup ~compare:ValueVar.compare lst then raise.raise (non_linear_pattern p)
      )
  | _ -> ()

let checks_linearity : raise:[<Errors.self_ast_imperative_error] Trace.raise -> expression -> unit =
  fun ~raise x ->
    check_linearity_record_fields ~raise x;
    check_linearity_patterns ~raise x;
    ()


let linearity ~(raise:[<Errors.self_ast_imperative_error] Trace.raise) m = (fun x -> checks_linearity ~raise x ; x) m

let reserved_names = (* Part of names in that list would be caught by some syntaxes *)
  List.map ~f:ValueVar.of_input_var
  [ "get_force" ; "get_entrypoint";
    "bitwise_or"; "bitwise_and"; "bitwise_xor"; "string_concat"; "string_slice"; "crypto_check"; "crypto_hash_key";
    "bytes_concat"; "bytes_slice"; "bytes_pack"; "bytes_unpack"; "set_empty"; "set_mem"; "set_add"; "set_remove"; "set_iter"; "set_fold"; "list_iter";
    "list_fold"; "list_fold_left"; "list_fold_right"; "list_map"; "map_iter"; "map_map"; "map_fold"; "map_remove"; "map_update"; "map_get"; "map_mem";
    "continue";  "gas"; "hash"; "stop"; "time";
    "continue"; "debugger"; "do";
  ]
let check_reserved ~raise ~loc binder =
  match List.find ~f:(fun reserved -> ValueVar.equal binder.var reserved) reserved_names with
  | Some v ->
    let str : string = Format.asprintf "%a" ValueVar.pp v in
    let loc : Location.t = loc in
    raise.raise (reserved_name str loc)
  | None -> ()

let reserved_names_exp ~raise : expression -> expression = fun exp ->
  match exp.expression_content with
  | E_let_in {let_binder ; _ } ->
    check_reserved ~raise ~loc:exp.location let_binder ;
    exp
  | E_lambda {binder ; _} ->
    check_reserved ~raise ~loc:exp.location binder ;
    exp
  | E_matching { cases ; _ } ->
    let rec aux : type_expression pattern -> unit = fun p ->
      match p.wrap_content with
      | P_unit -> ()
      | P_var binder -> check_reserved ~raise ~loc:p.location binder
      | P_list (Cons (p1,p2)) ->
        let () = aux p1 in
        aux p2
      | P_list (List lst) | P_tuple lst | P_record (_,lst) ->
        let () = List.iter ~f:aux lst in
        ()
      | P_variant (_,p) ->
        aux p
    in
    List.iter ~f:aux (List.map ~f:(fun (x: _ match_case) -> x.pattern) cases) ;
    exp
  | _ -> exp

let reserved_names_mod ~raise : module_ -> module_ = fun m ->
  let aux  = function
    | Location.{wrap_content = Declaration_type _; _} -> ()
    | {wrap_content = Declaration_constant {binder ; expr ; _ }; location = loc } ->
      check_reserved ~raise ~loc binder ;
      let _ : expression = reserved_names_exp ~raise expr in
      ()
    | {wrap_content = Declaration_module _ ; _} -> ()
  in
  List.iter ~f:aux m ;
  m
