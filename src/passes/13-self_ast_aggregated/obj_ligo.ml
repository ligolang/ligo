module AST = Ast_aggregated
open Stage_common

type 'err ty_exp_mapper = AST.type_expression -> unit

let rows : ('a -> unit) -> AST.rows -> unit = fun g {fields; _} ->
  let _ = Record.LMap.map (fun ({associated_type ; _} : AST.row_element) ->
              let () = g associated_type in
              ()) fields in
  ()

let rec traverse_type_expression : 'err ty_exp_mapper -> AST.type_expression -> unit  = fun f te ->
  let open Stage_common in
  let self = traverse_type_expression f in
  let () = f te in
  match te.type_content with
  | T_sum temap -> rows self temap
  | T_for_all x -> self x.type_
  | T_record temap -> rows self temap
  | T_arrow arr ->
     let _ = Arrow.map self arr in
     ()
  | T_variable _ -> ()
  | T_singleton _ -> ()
  | T_constant { parameters } ->
     let _ = List.map ~f:self parameters in
     ()

(* Adapted from lib_protocol/script_string_repr.ml *)
let check_string v =
  let rec check_printable_ascii i =
    if Int.(i < 0) then true
    else
      match v.[i] with
      | '\n' | '\x20' .. '\x7E' -> check_printable_ascii (i - 1)
      | _ -> false
  in
  check_printable_ascii (String.length v - 1)

let check_obj_ligo ~raise (t : AST.expression) =
  let folder_constant () expr = match expr.AST.expression_content with
    | E_constant {cons_name}
         when Constant.ppx_is_only_interpreter cons_name ->
       raise.Trace.error @@ Errors.expected_obj_ligo expr.location
    | E_literal (Literal_string s) when not (check_string @@ Ligo_string.extract s) ->
       raise.Trace.error @@ Errors.expected_obj_ligo expr.location
    | _ -> () in
  let traverser_types loc expr = match expr.AST.type_content with
    | T_constant { injection = Literal_types.Michelson_program ; _ }
    | T_constant { injection = Literal_types.Typed_address     ; _ }
    | T_constant { injection = Literal_types.Mutation          ; _ }
        -> raise.error @@ Errors.expected_obj_ligo loc
    | _ -> () in
  let folder_types () (expr : AST.expression) =
    traverse_type_expression (traverser_types expr.location) expr.type_expression in
  let () = Helpers.fold_expression folder_constant () t in
  let () = Helpers.fold_expression folder_types () t in
  t
