module AST = Ast_aggregated

let type_constants =
  let open Stage_common.Constant in
  [test_michelson_name; account_name; time_name ; typed_address_name ; mutation_name ; failure_name]

type 'err ty_exp_mapper = AST.type_expression -> unit

let rows : ('a -> unit) -> AST.rows -> unit = fun g {content; _} ->
  let _ = AST.LMap.map (fun {AST.associated_type ; _} ->
              let () = g associated_type in
              ()) content in
  ()

let rec traverse_type_expression : 'err ty_exp_mapper -> AST.type_expression -> unit  = fun f te ->
  let open Stage_common in
  let self = traverse_type_expression f in
  let () = f te in
  match te.type_content with
  | T_sum temap -> rows self temap
  | T_abstraction x -> self x.type_
  | T_for_all x -> self x.type_
  | T_record temap -> rows self temap
  | T_arrow arr ->
     let _ = Maps.arrow self arr in
     ()
  | T_variable _ -> ()
  | T_singleton _ -> ()
  | T_constant { parameters } ->
     let _ = List.map ~f:self parameters in
     ()

let check_obj_ligo ~raise (t : AST.expression) =
  let folder_constant () expr = match expr.AST.expression_content with
    | E_constant {cons_name}
         when AST.ppx_is_only_interpreter cons_name ->
       raise.Trace.raise @@ Errors.expected_obj_ligo expr.location
    | _ -> () in
  let traverser_types loc expr = match expr.AST.type_content with
    | T_constant { injection ; _ } when List.mem type_constants (Ligo_string.extract injection) ~equal:String.equal ->
       raise.raise @@ Errors.expected_obj_ligo loc
    | _ -> () in
  let folder_types () (expr : AST.expression) =
    traverse_type_expression (traverser_types expr.type_expression.location) expr.type_expression in
  let () = Helpers.fold_expression folder_constant () t in
  let () = Helpers.fold_expression folder_types () t in
  t
