open Errors
open Simple_utils.Trace

module O = Ast_typed

module TMap = O.Helpers.TMap
open Ligo_prim

let rec infer_type_application ~raise ~loc ?(default_error = fun loc t t' -> assert_equal loc t t') dom table (type_matched : O.type_expression) (type_ : O.type_expression) =
  let open O in
  let self = infer_type_application ~raise ~loc ~default_error in
  let default_error = default_error loc type_matched type_ in
  let inj_mod_equal a b = (* TODO: cleanup with polymorphic functions in value env *)
    Literal_types.equal a b
  in
  match type_matched.type_content, type_.type_content with
  | T_variable v, _ when List.mem dom v ~equal:TypeVar.equal -> (
     match TMap.find_opt v table with
     | Some t -> trace_option ~raise (not_matching loc t type_) (assert_type_expression_eq (type_, t));
                 table
     | None -> TMap.add v type_ table)
  | T_variable v, T_variable w -> (
    Assert.assert_true ~raise (not_matching loc type_matched type_) (TypeVar.equal v w);
    table)
  | T_arrow {type1;type2}, T_arrow {type1=type1_;type2=type2_} ->
     let table = self dom table type1 type1_ in
     let table = self dom table type2 type2_ in
     table
  | T_constant {language;injection;parameters}, T_constant {language=language';injection=injection';parameters=parameters'} ->
     if String.equal language language' && inj_mod_equal injection injection' && Int.equal (List.length parameters) (List.length parameters') then
       let table = List.fold_right (List.zip_exn parameters parameters') ~f:(fun (t, t') table ->
                       self dom table t t') ~init:table in
       table
     else
       raise.error default_error
  | T_record {fields; layout}, T_record {fields=content'; layout=layout'} ->
     let content_kv = Record.LMap.to_kv_list fields in
     let content'_kv = Record.LMap.to_kv_list content' in
     if Layout.equal layout layout' &&
         List.equal Label.equal (List.map content_kv ~f:fst) (List.map content'_kv ~f:fst) then
       let elements = List.zip_exn content_kv content'_kv in
       let aux ((_, ({associated_type;michelson_annotation;decl_pos=_}:  _ Rows.row_element_mini_c)), (_, ({associated_type=associated_type';michelson_annotation=michelson_annotation';decl_pos=_} : row_element))) table =
         if Option.equal String.equal michelson_annotation michelson_annotation' then
           self dom table associated_type associated_type'
         else
           raise.error default_error in
       let table = List.fold_right elements ~f:aux ~init:table in
       table
     else
       raise.error default_error
  | T_sum {fields; layout}, T_sum {fields=content'; layout=layout'} ->
     let content_kv = Record.LMap.to_kv_list fields in
     let content'_kv = Record.LMap.to_kv_list content' in
     if Layout.equal layout layout' &&
         List.equal Label.equal (List.map content_kv ~f:fst) (List.map content'_kv ~f:fst) then
       let elements = List.zip_exn content_kv content'_kv in
       let aux ((_, ({associated_type;michelson_annotation;decl_pos=_}:  _ Rows.row_element_mini_c)), (_, ({associated_type=associated_type';michelson_annotation=michelson_annotation';decl_pos=_} : row_element))) table =
         if Option.equal String.equal michelson_annotation michelson_annotation' then
           self dom table associated_type associated_type'
         else
           raise.error default_error in
       let table = List.fold_right elements ~f:aux ~init:table in
       table
     else
       raise.error default_error
  | T_singleton l, T_singleton l' when Int.equal 0 (Literal_value.compare l l') -> table
  | (T_arrow _ | T_record _ | T_sum _ | T_constant _ | T_singleton _ | T_abstraction _ | T_for_all _ | T_variable _),
    (T_arrow _ | T_record _ | T_sum _ | T_constant _ | T_singleton _ | T_abstraction _ | T_for_all _ | T_variable _)
    -> raise.error default_error

(* This function does some inference for applications: it takes a type
   `typed_matched` of the form `t1 -> ... -> tn -> t`, a list of types
   `args` of the form `[t'1;...;t'n]` (representing types on which the
   function is applied) and possibly a final type `tv_opt` of the form
   `t'` (representing an annotation for the final result).
   It will try to infer a table s.t. when substituting variables in
   `t1 -> ... -> tn -> t`, we get `t'1 -> ... > t'n -> t'`. It works
   by matching iteratively on each type: `t1` with `t'1`, ..., `tn`
   with `t'n`, and finally `t` with `t'`. *)
let infer_type_applications ~raise ~loc ?(default_error = (fun loc t t' -> assert_equal loc t' t)) dom type_matched args tv_opt =
  let table, type_matched = List.fold_left args ~init:(TMap.empty, type_matched) ~f:(fun ((table, type_matched) : _ TMap.t * O.type_expression) matched ->
                  match type_matched.type_content with
                  | T_arrow { type1 ; type2 } ->
                     infer_type_application ~raise ~loc dom table type1 matched, type2
                  | (T_record _ | T_sum _ | T_constant _ | T_singleton _ | T_abstraction _ | T_for_all _ | T_variable _) ->
                     table, type_matched) in
  match tv_opt with
  | Some t -> infer_type_application ~raise ~loc ~default_error dom table type_matched t
  | None -> table

(* This wraps a `∀ a . (∀ b . (∀ c . some_type))` with type instantiations,
   e.g. given the table `[a ↦ int; b ↦ string; c ↦ bool` it will return
   `(((∀ a . (∀ b . (∀ c . some_type))) @@ int) @@ string) @@ bool` *)
let build_type_insts ~raise ~loc (forall : O.expression) table bound_variables =
  let rec build_type_insts (forall : O.expression) = function
    | [] -> forall
    | av :: avs' ->
       let type_ = trace_option ~raise (Errors.not_annotated loc) @@ O.Helpers.TMap.find_opt av table in
       let Abstraction.{ ty_binder ; type_ = t ; kind = _ } = trace_option ~raise (corner_case "Expected a for all type quantifier") @@ O.get_t_for_all forall.type_expression in
       build_type_insts O.(make_e (E_type_inst {forall ; type_ }) (Ast_typed.Helpers.subst_no_capture_type ty_binder type_ t)) avs' in
  build_type_insts forall bound_variables

let build_type_insts_function ~raise ~loc (forall : O.expression) table bound_variables =
  let _avs, t = O.Helpers.destruct_for_alls forall.type_expression in
  let forall = { forall with type_expression = t } in
  let rec build_type_insts (forall : O.expression) = function
    | [] -> forall
    | av :: avs' ->
       let type_ = trace_option ~raise (Errors.not_annotated loc) @@ O.Helpers.TMap.find_opt av table in
       let Arrow.{ type1 = _ ; type2 } = trace_option ~raise (corner_case "Expected an arrow type") @@ O.get_t_arrow forall.type_expression in
       build_type_insts O.(make_e (E_type_inst {forall ; type_ }) (Ast_typed.Helpers.subst_no_capture_type av type_ type2)) avs' in
  build_type_insts forall bound_variables

let build_type_insts_ufunction ~raise ~loc (forall : O.expression) table bound_variables =
  let _avs, t = O.Helpers.destruct_for_alls forall.type_expression in
  let forall = { forall with type_expression = t } in
  let rec build_type_insts (forall : O.expression) = function
    | [] -> forall
    | av :: avs' ->
       let type_ = trace_option ~raise (Errors.not_annotated loc) @@ O.Helpers.TMap.find_opt av table in
       build_type_insts O.(make_e (E_type_inst {forall ; type_ }) (Ast_typed.Helpers.subst_no_capture_type av type_ forall.type_expression)) avs' in
  build_type_insts forall bound_variables
