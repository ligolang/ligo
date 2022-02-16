open Simple_utils.Trace
module Errors=Errors
open Errors

module I = Ast_core
module O = Ast_typed
open O.Combinators

type protocol_version = Environment.Protocols.t
module Pair = Simple_utils.Pair

type context = Context.t

let assert_type_expression_eq = Helpers.assert_type_expression_eq

(* The `table` represents the substitutions that have been inferred.
   For example, if matching `a -> b -> a` with `int -> bool -> int`,
   it should have information such as `[a ↦ int; b ↦ bool]`. *)
module TMap = Simple_utils.Map.Make(struct type t = O.type_variable let compare x y = I.Var.compare x y end)

let rec infer_type_application ~raise ~loc ?(default_error = fun loc t t' -> assert_equal loc t t') table (type_matched : O.type_expression) (type_ : O.type_expression) =
  let open O in
  let self = infer_type_application ~raise ~loc ~default_error in
  let default_error = default_error loc type_matched type_ in
  let inj_mod_equal a b = (* TODO: cleanup with polymorphic functions in value env *)
    let a = Ligo_string.extract a in
    let b = Ligo_string.extract b in
    let ad_hoc_maps_unification a b = match a,b with
      | "map_or_big_map", x -> (x,x)
      | x, "map_or_big_map" -> (x,x)
      | _ -> a,b
    in
    let (a,b) = ad_hoc_maps_unification a b in
    String.equal a b
  in
  match type_matched.type_content, type_.type_content with
  | T_variable v, _ -> (
     match TMap.find_opt v table with
     | Some t -> trace_option ~raise (not_matching loc t type_) (assert_type_expression_eq (type_, t));
                 table
     | None -> TMap.add v type_ table)
  | T_arrow {type1;type2}, T_arrow {type1=type1_;type2=type2_} ->
     let table = self table type1 type1_ in
     let table = self table type2 type2_ in
     table
  | T_constant {language;injection;parameters}, T_constant {language=language';injection=injection';parameters=parameters'} ->
     if String.equal language language' && inj_mod_equal injection injection' && Int.equal (List.length parameters) (List.length parameters') then
       let table = List.fold_right (List.zip_exn parameters parameters') ~f:(fun (t, t') table ->
                       self table t t') ~init:table in
       table
     else
       raise.raise default_error
  | T_record {content; layout}, T_record {content=content'; layout=layout'} ->
     let content_kv = O.LMap.to_kv_list content in
     let content'_kv = O.LMap.to_kv_list content' in
     if layout_eq layout layout' &&
          List.equal equal_label (List.map content_kv ~f:fst) (List.map content'_kv ~f:fst) then
       let elements = List.zip_exn content_kv content'_kv in
       let aux ((_, {associated_type;michelson_annotation;decl_pos}), (_, {associated_type=associated_type';michelson_annotation=michelson_annotation';decl_pos=decl_pos'})) table =
         if Int.equal decl_pos decl_pos' && Option.equal String.equal michelson_annotation michelson_annotation' then
           self table associated_type associated_type'
         else
           raise.raise default_error in
       let table = List.fold_right elements ~f:aux ~init:table in
       table
     else
       raise.raise default_error
  | T_sum {content; layout}, T_sum {content=content'; layout=layout'} ->
     let content_kv = O.LMap.to_kv_list content in
     let content'_kv = O.LMap.to_kv_list content' in
     if layout_eq layout layout' &&
          List.equal equal_label (List.map content_kv ~f:fst) (List.map content'_kv ~f:fst) then
       let elements = List.zip_exn content_kv content'_kv in
       let aux ((_, {associated_type;michelson_annotation;decl_pos}), (_, {associated_type=associated_type';michelson_annotation=michelson_annotation';decl_pos=decl_pos'})) table =
         if Int.equal decl_pos decl_pos' && Option.equal String.equal michelson_annotation michelson_annotation' then
           self table associated_type associated_type'
         else
           raise.raise default_error in
       let table = List.fold_right elements ~f:aux ~init:table in
       table
     else
       raise.raise default_error
  | T_singleton l, T_singleton l' when Int.equal 0 (Stage_common.Enums.compare_literal l l') -> table
  | (T_arrow _ | T_record _ | T_sum _ | T_constant _ | T_module_accessor _ | T_singleton _ | T_abstraction _ | T_for_all _),
    (T_arrow _ | T_record _ | T_sum _ | T_constant _ | T_module_accessor _ | T_singleton _ | T_abstraction _ | T_for_all _ | T_variable _)
    -> raise.raise default_error

(* This function does some inference for applications: it takes a type
   `typed_matched` of the form `t1 -> ... -> tn -> t`, a list of types
   `args` of the form `[t'1;...;t'n]` (representing types on which the
   function is applied) and possibly a final type `tv_opt` of the form
   `t'` (representing an annotation for the final result).
   It will try to infer a table s.t. when substituting variables in
   `t1 -> ... -> tn -> t`, we get `t'1 -> ... > t'n -> t'`. It works
   by matching iteratively on each type: `t1` with `t'1`, ..., `tn`
   with `t'n`, and finally `t` with `t'`. *)
let infer_type_applications ~raise ~loc type_matched args tv_opt =
  let table, type_matched = List.fold_left args ~init:(TMap.empty, type_matched) ~f:(fun ((table, type_matched) : _ TMap.t * O.type_expression) matched ->
                  match type_matched.type_content with
                  | T_arrow { type1 ; type2 } ->
                     infer_type_application ~raise ~loc table type1 matched, type2
                  | (T_record _ | T_sum _ | T_constant _ | T_module_accessor _ | T_singleton _ | T_abstraction _ | T_for_all _ | T_variable _) ->
                     table, type_matched) in
  match tv_opt with
  | Some t -> infer_type_application ~raise ~loc ~default_error:(fun loc t t' -> assert_equal loc t' t) table type_matched t
  | None -> table

(* This wraps a `∀ a . (∀ b . (∀ c . some_type))` with type instantiations,
   e.g. given the table `[a ↦ int; b ↦ string; c ↦ bool` it will return
   `(((∀ a . (∀ b . (∀ c . some_type))) @@ int) @@ string) @@ bool` *)
let build_type_insts ~raise ~loc (forall : O.expression) table bound_variables =
  let bound_variables = List.rev bound_variables in
  let rec build_type_insts (forall : O.expression) = function
    | [] -> forall
    | av :: avs' ->
       let O.{ ty_binder ; type_ = t ; kind = _ } = trace_option ~raise (corner_case "Expected a for all type quantifier") @@ O.get_t_for_all forall.type_expression in
       assert (I.Var.equal ty_binder av);
       let type_ = trace_option ~raise (Errors.not_annotated loc) @@ TMap.find_opt av table in
       build_type_insts (make_e (E_type_inst {forall ; type_ }) (Ast_typed.Helpers.subst_type av type_ t)) avs' in
  build_type_insts forall bound_variables

let rec type_module ~raise ~test ~init_context ~protocol_version (p:I.module_) : O.module_ =
  let aux (c, acc:(context * O.declaration Location.wrap list)) (d:I.declaration Location.wrap) =
    let (c, d') = type_declaration' ~raise ~test ~protocol_version c d in
    (c, d' :: acc)
  in
  (* This context use all the declaration so you can use private declaration to type the module. It should not be returned*)
  let (_c, lst) =
      List.fold ~f:aux ~init:(init_context, []) p in
  List.rev lst


and type_declaration' : raise: typer_error raise -> protocol_version:protocol_version -> test: bool -> context -> I.declaration Location.wrap -> context * O.declaration Location.wrap =
fun ~raise ~protocol_version ~test c d ->
let loc = d.location in
let return ?(loc = loc) c (d : O.declaration) = c,Location.wrap ~loc d in
match Location.unwrap d with
  | Declaration_type {type_binder ; _} when Ast_core.Helpers.is_generalizable_variable type_binder ->
    raise.raise (wrong_generalizable d.location type_binder)
  | Declaration_type {type_binder ; type_expr; type_attr={public}} -> (
    let tv = evaluate_type ~raise c type_expr in
    let tv = {tv with orig_var = Some type_binder} in
    let env' = Context.add_type c type_binder tv in
    return env' @@ Declaration_type { type_binder ; type_expr = tv; type_attr={public} }
  )
  | Declaration_constant { binder = { ascr = None ; var ; attributes=_ } ; attr  ; expr} -> (
    let av, expr = Ast_core.Combinators.get_type_abstractions expr in
    let c = List.fold_right av ~f:(fun v c -> Context.add_type_var c v ()) ~init:c in
    let expr =
      trace ~raise (constant_declaration_tracer loc var expr None) @@
      type_expression' ~test ~protocol_version c expr in
    let rec aux t = function
      | [] -> t
      | (abs_var :: abs_vars) -> t_for_all abs_var () (aux t abs_vars) in
    let type_expression = aux expr.type_expression (List.rev av) in
    let expr = { expr with type_expression } in
    let binder : O.expression_variable = var in
    let post_env = Context.add_value c binder expr.type_expression in
    return post_env @@ Declaration_constant { binder ; expr ; attr }
  )
  | Declaration_constant { binder = { ascr = Some tv ; var ; attributes=_ } ; attr ; expr } ->
    let av, tv = Ast_core.Helpers.destruct_for_alls tv in
    let av', expr = Ast_core.Combinators.get_type_abstractions expr in
    let av = av @ av' in
    let env = List.fold_right av ~f:(fun v c -> Context.add_type_var c v ()) ~init:c in
    let tv = evaluate_type ~raise env tv in
    let expr =
      trace ~raise (constant_declaration_tracer loc var expr (Some tv)) @@
      type_expression' ~test ~protocol_version ~tv_opt:tv env expr in
    let rec aux t = function
      | [] -> t
      | (abs_var :: abs_vars) -> t_for_all abs_var () (aux t abs_vars) in
    let type_expression = aux expr.type_expression (List.rev av) in
    let expr = { expr with type_expression } in
    let binder : O.expression_variable = var in
    let c = Context.add_value c binder expr.type_expression in
    return c @@ Declaration_constant { binder ; expr ; attr }
  | Declaration_module {module_binder;module_; module_attr = {public}} -> (
    let module_ = type_module ~raise ~test ~protocol_version ~init_context:c module_ in
    let post_env = Context.add_ez_module c module_binder module_ in
    return post_env @@ Declaration_module { module_binder; module_; module_attr = {public}}
  )
  | Module_alias {alias;binders} -> (
    let f context binder =
      trace_option ~raise (unbound_module_variable binder d.location)
      @@ Context.get_module context binder
    in
    let (hd, tl) = binders in
    let e = List.fold_left ~f ~init:(f c hd) tl in
    let post_env = Context.add_module c alias e in
    return post_env @@ Module_alias { alias; binders}
  )

and evaluate_otype ~raise (c:context) (t:O.type_expression) : O.type_expression =
  (* NOTE: this is similar to evaluate_type, but just look up for variables in environemnt
    feels wrong, but that's to allow re-evaluate body of T_abstractions *)
  let return tv' = make_t ~loc:t.location tv' t.type_meta in
  match t.type_content with
  | T_constant { language; injection; parameters } ->
    let parameters = List.map ~f:(evaluate_otype ~raise c) parameters in
    return (T_constant { language; injection; parameters })
  | T_arrow {type1;type2} ->
      let type1 = evaluate_otype ~raise c type1 in
      let type2 = evaluate_otype ~raise c type2 in
      return (T_arrow {type1;type2})
  | T_sum m -> (
    let lmap =
      let aux ({associated_type;michelson_annotation;decl_pos} : O.row_element) =
        let associated_type = evaluate_otype ~raise c associated_type in
        ({associated_type;michelson_annotation;decl_pos} : O.row_element)
      in
      O.LMap.map aux m.content
    in
    let sum : O.rows  = match Context.get_sum lmap c with
      | None ->
        let layout = m.layout in
        {content = lmap; layout}
      | Some r -> r
    in
    let ty = make_t (T_sum sum) None in
    let () =
      let aux k _v acc = match Context.get_constructor k c with
          | Some (_,type_) ->
            if Ast_typed.Misc.type_expression_eq (acc,type_) then type_
            else if I.LMap.mem (Label "M_left") m.content || I.LMap.mem (Label "M_right") m.content then type_
            else raise.raise (redundant_constructor k t.location)
          | None -> acc in
      let _ = O.LMap.fold aux m.content ty in ()
    in
    return @@ T_sum sum
  )
  | T_record m -> (
    let aux ({associated_type;michelson_annotation;decl_pos}: O.row_element) =
      let associated_type = evaluate_otype ~raise c associated_type in
      ({associated_type;michelson_annotation;decl_pos} : O.row_element)
    in
    let lmap = O.LMap.map aux m.content in
    let record : O.rows = match Context.get_record lmap c with
    | None ->
      let layout = m.layout in
      {content=lmap;layout}
    | Some (_,r) ->  r
    in
    return @@ T_record record
  )
  | T_variable name -> (
    match Context.get_type c name with
    | Some x -> x
    | None -> raise.raise (unbound_type_variable name t.location)
  )
  | T_module_accessor {module_name; element} ->
    let module_ = match Context.get_module c module_name with
      Some m -> m
    | None   -> raise.raise @@ unbound_module_variable module_name t.location
    in
    evaluate_otype ~raise module_ element
  | T_singleton x -> return (T_singleton x)
  | T_abstraction x ->
    let c = Context.add_kind c x.ty_binder () in
    let type_ = evaluate_otype ~raise c x.type_ in
    return (T_abstraction {x with type_})
  | T_for_all x ->
    let c = Context.add_type_var c x.ty_binder () in
    let type_ = evaluate_otype ~raise c x.type_ in
    return (T_for_all {x with type_})

and evaluate_type ~raise (c:context) (t:I.type_expression) : O.type_expression =
  let return tv' = make_t ~loc:t.location tv' (Some t) in
  match t.type_content with
  | T_arrow {type1;type2} ->
      let type1 = evaluate_type ~raise c type1 in
      let type2 = evaluate_type ~raise c type2 in
      return (T_arrow {type1;type2})
  | T_sum m -> (
    let lmap =
      let aux ({associated_type;michelson_annotation;decl_pos} : I.row_element) =
        let associated_type = evaluate_type ~raise c associated_type in
        ({associated_type;michelson_annotation;decl_pos} : O.row_element)
      in
      O.LMap.map aux m.fields
    in
    let sum : O.rows  = match Context.get_sum lmap c with
      | None ->
        let layout = Option.value ~default:default_layout m.layout in
        {content = lmap; layout}
      | Some r -> r
    in
    let ty = make_t (T_sum sum) None in
    let () =
      let aux k _v acc = match Context.get_constructor k c with
          | Some (_,type_) ->
            if Ast_typed.Misc.type_expression_eq (acc,type_) then type_
            else if I.LMap.mem (Label "M_left") m.fields || I.LMap.mem (Label "M_right") m.fields then type_
            else raise.raise (redundant_constructor k t.location)
          | None -> acc in
      let _ = O.LMap.fold aux m.fields ty in ()
    in
    return @@ T_sum sum
  )
  | T_record m -> (
    let aux ({associated_type;michelson_annotation;decl_pos}: I.row_element) =
      let associated_type = evaluate_type ~raise c associated_type in
      ({associated_type;michelson_annotation;decl_pos} : O.row_element)
    in
    let lmap = O.LMap.map aux m.fields in
    let record : O.rows = match Context.get_record lmap c with
    | None ->
      let layout = Option.value ~default:default_layout m.layout in
      {content=lmap;layout}
    | Some (_,r) ->  r
    in
    return @@ T_record record
  )
  | T_variable name -> (
    match Context.get_type c name with
    | Some x -> x
    | None when I.Var.is_generalizable name ->
       (* Case happening when trying to use a variable that is not in
          the context, but it is generalizable: we hint the user
          that the variable could be put in the extended context
          via an annotation *)
       raise.raise (not_annotated t.location)
    | None -> raise.raise (unbound_type_variable name t.location)
  )
  | T_app {type_operator;arguments} -> (
    let operator = trace_option ~raise (unbound_type_variable type_operator t.location) @@
      Context.get_type c type_operator
    in
    let is_fully_applied location (t:O.type_expression) =
      match t.type_content with
      | T_abstraction x ->
        let rec aux : (O.type_expression * int) -> (O.type_expression * int) =
          fun (t,i) -> match t.type_content with T_abstraction x -> aux (x.type_,i+1) | _ -> (t,i)
        in
        let expected = snd @@ aux (x.type_,1) in
        raise.raise (type_constant_wrong_number_of_arguments None expected 0 location)
      | _ -> ()
    in
    let rec aux : O.type_expression * I.type_variable list -> O.type_expression * I.type_variable list =
      fun (op, lst) ->
        match op.type_content with
        | O.T_abstraction {ty_binder;kind=_;type_} -> (
          aux (type_ , lst @ [ty_binder])
        )
        | _ -> (op, lst)
    in
    let (ty_body,vars) = aux (operator, []) in
    let vargs =
      match List.zip vars arguments with
      | Unequal_lengths ->
        let actual = List.length arguments in
        let expected = List.length vars in
        raise.raise (type_constant_wrong_number_of_arguments (Some type_operator) expected actual t.location)
      | Ok x -> x
    in
    let aux : context -> (I.type_variable * I.type_expression) -> context =
      fun c (ty_binder,arg) ->
        let arg' = evaluate_type ~raise c arg in
        let () = is_fully_applied arg.location arg' in
        Context.add_type c ty_binder arg'
    in
    let env' = List.fold_left ~f:aux ~init:c vargs in
    match ty_body.type_content with
    | T_constant {language;injection;parameters} ->
      let aux : O.type_expression -> O.type_expression =
        fun t ->
          let t' = evaluate_otype ~raise env' t in
          let () = is_fully_applied t.location t' in
          t'
      in
      let args = List.map ~f:aux parameters in
      return (T_constant {language;injection;parameters=args})
    | _ -> evaluate_otype ~raise env' ty_body
  )
  | T_module_accessor {module_name; element} ->
    let module_ = match Context.get_module c module_name with
      Some m -> m
    | None   -> raise.raise @@ unbound_module_variable module_name t.location
    in
    evaluate_type ~raise module_ element
  | T_singleton x -> return (T_singleton x)
  | T_abstraction x ->
    let c = Context.add_kind c x.ty_binder () in
    let type_ = evaluate_type ~raise c x.type_ in
    return (T_abstraction {x with type_})
  | T_for_all x ->
    let c = Context.add_type_var c x.ty_binder () in
    let type_ = evaluate_type ~raise c x.type_ in
    return (T_for_all {x with type_})

and type_expression ~raise ~test ~protocol_version : ?env:Environment.t -> ?tv_opt:O.type_expression -> I.expression -> O.expression
  = fun ?env ?tv_opt e ->
    let c   = Context.init ?env () in
    let res = type_expression' ~raise ~test ~protocol_version c ?tv_opt e in
    res

and type_expression' ~raise ~test ~protocol_version ?(args = []) ?last : context -> ?tv_opt:O.type_expression -> I.expression -> O.expression = fun context ?tv_opt e ->
  let return expr tv =
    let () =
      match tv_opt with
      | None -> ()
      | Some tv' -> assert_type_expression_eq ~raise e.location (tv' , tv) in
    let location = e.location in
    make_e ~location expr tv in
  let return_e (expr : O.expression) = return expr.expression_content expr.type_expression in
  trace ~raise (expression_tracer e) @@
  fun ~raise -> match e.expression_content with
  (* Basic *)
  | E_variable name ->
      let tv' =
        trace_option ~raise (unbound_variable name e.location)
        @@ Context.get_value context name in
      (match tv' with
       | { type_content = T_for_all _ ; type_meta = _; orig_var=_ ; location=_} ->
          (* TODO: This is some inference, and we should reconcile it with the inference pass. *)
          let avs, type_ = O.Helpers.destruct_for_alls tv' in
          let table = infer_type_applications ~raise ~loc:e.location type_ (List.map ~f:(fun ({type_expression;_} : O.expression) -> type_expression) args) last in
          let lamb = make_e ~location:e.location (E_variable name) tv' in
          return_e @@ build_type_insts ~raise ~loc:e.location lamb table avs
       | _ ->
          return (E_variable name) tv')
  | E_literal Literal_unit ->
      return (E_literal (Literal_unit)) (t_unit ())
  | E_literal (Literal_string s) ->
      return (E_literal (Literal_string s)) (t_string ())
  | E_literal (Literal_key s) ->
      return (E_literal (Literal_key s)) (t_key ())
  | E_literal (Literal_key_hash s) ->
      return (E_literal (Literal_key_hash s)) (t_key_hash ())
  | E_literal (Literal_chain_id s) ->
      return (E_literal (Literal_chain_id s)) (t_chain_id ())
  | E_literal (Literal_signature s) ->
      return (E_literal (Literal_signature s)) (t_signature ())
  | E_literal (Literal_bytes s) ->
      return (E_literal (Literal_bytes s)) (t_bytes ())
  | E_literal (Literal_int n) ->
      return (E_literal (Literal_int n)) (t_int ())
  | E_literal (Literal_nat n) ->
      return (E_literal (Literal_nat n)) (t_nat ())
  | E_literal (Literal_timestamp n) ->
      return (E_literal (Literal_timestamp n)) (t_timestamp ())
  | E_literal (Literal_mutez n) ->
      return (E_literal (Literal_mutez n)) (t_mutez ())
  | E_literal (Literal_address s) ->
      return (e_address s) (t_address ())
  | E_literal (Literal_operation op) ->
      return (e_operation op) (t_operation ())
  | E_literal (Literal_bls12_381_g1 b) ->
      return (e_bls12_381_g1 b) (t_bls12_381_g1 ())
  | E_literal (Literal_bls12_381_g2 b) ->
      return (e_bls12_381_g2 b) (t_bls12_381_g2 ())
  | E_literal (Literal_bls12_381_fr b) ->
      return (e_bls12_381_fr b) (t_bls12_381_fr ())
  | E_literal (Literal_chest _ | Literal_chest_key _) -> failwith "chest / chest_key not allowed in the syntax (only tests need this type)"
  | E_record_accessor {record;path} ->
      let e' = type_expression' ~raise ~test ~protocol_version context record in
      let aux (prev:O.expression) (a:I.label) : O.expression =
          let property = a in
          let r_tv = trace_option ~raise (expected_record e.location @@ get_type prev) @@
            get_t_record prev.type_expression in
          let tv =
            trace_option ~raise (bad_record_access property prev e.location) @@
            O.LMap.find_opt property r_tv.content in
          let location = e.location in
          make_e ~location (E_record_accessor {record=prev; path=property}) tv.associated_type
      in
      let e = aux e' path in
      (* check type annotation of the final accessed element *)
      let () =
        match tv_opt with
        | None -> ()
        | Some tv' -> assert_type_expression_eq ~raise e.location (tv' , e.type_expression) in
      e
  | E_constructor {constructor = Label s as constructor ; element} when String.equal s "M_left" || String.equal s "M_right" -> (
    let t = trace_option ~raise (michelson_or_no_annotation constructor e.location) @@ tv_opt in
    let expr' = type_expression' ~raise ~test ~protocol_version context element in
    ( match t.type_content with
      | T_sum c ->
        let {associated_type ; _} : O.row_element = O.LMap.find (Label s) c.content in
        let () = assert_type_expression_eq ~raise expr'.location (associated_type, expr'.type_expression) in
        return (E_constructor {constructor = Label s; element=expr'}) t
      | _ -> raise.raise (michelson_or_no_annotation constructor e.location)
    )
  )
  (* Sum *)
  | E_constructor {constructor; element} ->
      let (avs, c_tv, sum_tv) = trace_option ~raise (unbound_constructor constructor e.location) @@
        Context.get_constructor_parametric constructor context in
      let expr' = type_expression' ~raise ~test ~protocol_version context element in
      let table = infer_type_application ~raise ~loc:element.location TMap.empty c_tv expr'.type_expression in
      let table = match tv_opt with
        | Some tv_opt -> infer_type_application ~raise ~loc:e.location ~default_error:(fun loc t t' -> assert_equal loc t' t) table sum_tv tv_opt
        | None -> table in
      let () = trace_option ~raise (not_annotated e.location) @@
                 if (List.for_all avs ~f:(fun v -> TMap.mem v table)) then Some () else None in
      let c_tv = TMap.fold (fun tv t r -> Ast_typed.Helpers.subst_type tv t r) table c_tv in
      let sum_tv = TMap.fold (fun tv t r -> Ast_typed.Helpers.subst_type tv t r) table sum_tv in
      let () = assert_type_expression_eq ~raise expr'.location (c_tv, expr'.type_expression) in
      return (E_constructor {constructor; element=expr'}) sum_tv
  (* Record *)
  | E_record m ->
      let m' = O.LMap.map (type_expression' ~raise ~test ~protocol_version context) m in
      let _,lmap = O.LMap.fold_map ~f:(
        fun (Label k) e i ->
          let decl_pos = match int_of_string_opt k with Some i -> i | None -> i in
          i+1,({associated_type = get_type e ; michelson_annotation = None ; decl_pos}: O.row_element)
        ) m' ~init:0 in
      let record_type = match Context.get_record lmap context with
        | None -> t_record ~layout:default_layout lmap
        | Some (orig_var,r) -> make_t_orig_var (T_record r) None orig_var
      in
      return (E_record m') record_type
  | E_record_update {record; path; update} ->
    let record = type_expression' ~raise ~test ~protocol_version context record in
    let update = type_expression' ~raise ~test ~protocol_version context update in
    let wrapped = get_type record in
    let tv =
      match wrapped.type_content with
      | T_record {content;_} -> (
          let O.{associated_type;_} = trace_option ~raise (bad_record_access path record update.location) @@
            O.LMap.find_opt path content in
          associated_type
      )
      | _ -> failwith (Format.asprintf "Update an expression which is not a record %a" O.PP.type_expression wrapped)
    in
    let () = assert_type_expression_eq ~raise update.location (tv, get_type update) in
    return (E_record_update {record; path; update}) wrapped
  (* Data-structure *)
  | E_lambda lambda ->
     let lambda =
         match tv_opt with
         | None -> lambda
         | Some tv' ->
            match O.get_t_arrow tv' with
            | None -> lambda
            | Some { type1 = input_type ; type2 = _ } ->
               let input_type = Untyper.untype_type_expression_nofail input_type in
               match lambda.binder.ascr with
               | None -> let binder = {lambda.binder with ascr = Some input_type } in
                         { lambda with binder = binder }
               | Some _ -> lambda in
     let (lambda,lambda_type) = type_lambda ~raise ~test ~protocol_version context lambda in
     return (E_lambda lambda ) lambda_type
  | I.E_type_abstraction {type_binder;result} ->
    let context = Context.add_type_var context type_binder () in
    let result  = type_expression' ~raise ~test ~protocol_version context result in
    return (E_type_abstraction {type_binder;result}) result.type_expression
  | E_constant {cons_name=( C_LIST_FOLD | C_MAP_FOLD | C_SET_FOLD | C_FOLD) as opname ;
                arguments=[
                    ( { expression_content = (I.E_lambda { binder = {var=lname ; ascr = None;attributes=_};
                                                   output_type = None ;
                                                   result }) ;
                        location = _ ; sugar=_}) as _lambda ;
                    collect ;
                    init_record ;
                  ]} ->
      let open Stage_common.Constant in
      (* this special case is here to force annotation of the untyped lambda
         generated by pascaligo's for_collect loop *)
      let (v_col , v_initr ) = Pair.map ~f:(type_expression' ~raise ~test ~protocol_version context) (collect , init_record ) in
      let tv_col = get_type v_col   in (* this is the type of the collection  *)
      let tv_out = get_type v_initr in (* this is the output type of the lambda*)
      let input_type = match tv_col.type_content with
        | O.T_constant {language=_ ; injection ; parameters=[t]}
            when String.equal (Ligo_string.extract injection) list_name
              || String.equal (Ligo_string.extract injection) set_name ->
          make_t_ez_record (("0",tv_out)::[("1",t)])
        | O.T_constant {language=_ ; injection ; parameters=[k;v]}
          when String.equal (Ligo_string.extract injection) map_name
            || String.equal (Ligo_string.extract injection) big_map_name ->
          make_t_ez_record (("0",tv_out)::[("1",make_t_ez_record [("0",k);("1",v)])])
        | _ -> raise.raise @@ bad_collect_loop tv_col e.location in
      let e' = Context.add_value context lname input_type in
      let body = type_expression' ~raise ~test ~protocol_version ?tv_opt:(Some tv_out) e' result in
      let output_type = body.type_expression in
      let lambda' = make_e (E_lambda {binder = lname ; result=body}) (t_arrow input_type output_type ()) in
      let lst' = [lambda'; v_col; v_initr] in
      let tv_lst = List.map ~f:get_type lst' in
      let (opname', tv) =
        type_constant ~raise ~test ~protocol_version opname e.location tv_lst tv_opt in
      return (E_constant {cons_name=opname';arguments=lst'}) tv
  | E_constant {cons_name=C_FOLD_WHILE as opname;
                arguments = [
                    ( { expression_content = (I.E_lambda { binder = {var=lname ; ascr = None ; attributes=_};
                                                   output_type = None ;
                                                   result }) ;
                        location = _ ; sugar=_}) as _lambda ;
                    init_record ;
                ]} ->
      let v_initr = type_expression' ~raise ~test ~protocol_version context init_record in
      let tv_out = get_type v_initr in
      let input_type  = tv_out in
      let e' = Context.add_value context lname input_type in
      let body = type_expression' ~raise ~test ~protocol_version e' result in
      let output_type = body.type_expression in
      let lambda' = make_e (E_lambda {binder = lname ; result=body}) (t_arrow input_type output_type ()) in
      let lst' = [lambda';v_initr] in
      let tv_lst = List.map ~f:get_type lst' in
      let (opname',tv) = type_constant ~raise ~test ~protocol_version opname e.location tv_lst tv_opt in
      return (E_constant {cons_name=opname';arguments=lst'}) tv
  | E_constant {cons_name=C_CREATE_CONTRACT as cons_name;arguments} ->
      let lst' = List.map ~f:(type_expression' ~raise ~test ~protocol_version context) arguments in
      let () = match lst' with
        | { expression_content = O.E_lambda l ; _ } :: _ ->
          let open Ast_typed.Misc in
          let fvs = Free_variables.lambda [] l in
          if Int.equal (List.length fvs) 0 then ()
          else raise.raise @@ fvs_in_create_contract_lambda e (List.hd_exn fvs)
        | _ -> raise.raise @@ create_contract_lambda S.C_CREATE_CONTRACT e
      in
      let tv_lst = List.map ~f:get_type lst' in
      let (name', tv) =
        type_constant ~raise ~test ~protocol_version cons_name e.location tv_lst tv_opt in
      return (E_constant {cons_name=name';arguments=lst'}) tv
  | E_constant {cons_name=C_SET_ADD|C_CONS as cst;arguments=[key;set]} ->
      let key' =  type_expression' ~raise ~test ~protocol_version context key in
      let tv_key = get_type key' in
      let tv = match tv_opt with
          Some tv -> tv
        | None -> match cst with
            C_SET_ADD -> t_set tv_key
          | C_CONS -> t_list tv_key
          | _ -> failwith "Only C_SET_ADD and C_CONS are possible because those were the two cases matched above"
      in
      let set' =  type_expression' ~raise ~test ~protocol_version context ~tv_opt:tv set in
      let tv_set = get_type set' in
      let tv_lst = [tv_key;tv_set] in
      let (name', tv) = type_constant ~raise ~test ~protocol_version cst e.location tv_lst tv_opt in
      return (E_constant {cons_name=name';arguments=[key';set']}) tv
  | E_constant {cons_name=C_MAP_ADD as cst; arguments=[key;value;map]} ->
      let key' = type_expression' ~raise ~test ~protocol_version context key in
      let val' = type_expression' ~raise ~test ~protocol_version context value in
      let tv_key = get_type key' in
      let tv_val = get_type val' in
      let tv = match tv_opt with
          Some tv -> tv
        | None -> t_map_or_big_map tv_key tv_val
      in
      let map' =  type_expression' ~raise ~test ~protocol_version context ~tv_opt:tv map in
      let tv_map = get_type map' in
      let tv_lst = [tv_key;tv_val;tv_map] in
      let (name', tv) = type_constant ~raise ~test ~protocol_version cst e.location tv_lst tv_opt in
      return (E_constant {cons_name=name';arguments=[key';val';map']}) tv
  | E_constant {cons_name = C_POLYMORPHIC_ADD;arguments} ->
      let lst' = List.map ~f:(type_expression' ~raise ~test ~protocol_version context) arguments in
      let tv_lst = List.map ~f:get_type lst' in
      let decide = function
        | {O.expression_content = E_literal (Literal_string _); _ } -> Some S.C_CONCAT
        | {expression_content = E_constant {cons_name = C_ADD; _ }; _ } -> Some C_ADD
        | {expression_content = E_constant {cons_name = C_CONCAT; _ }; _ } -> Some C_CONCAT
        | {expression_content = E_constant {cons_name = C_SLICE; _ }; _ } -> Some C_CONCAT
        | {expression_content = E_literal (Literal_int _); _ } -> Some C_ADD
        | {expression_content = E_record_accessor {record; path}; _ } ->
            (let x = get_record_field_type record.type_expression path in
            match x with
            Some s when is_t_string s ->
              Some C_CONCAT
            | _ -> None )
        | {expression_content = E_variable _; type_expression = texpr ; location = _} ->
            if is_t_string texpr then
              Some C_CONCAT
            else
              None
        | _ -> None in
      let cst =
        Option.value ~default:S.C_ADD @@ List.find_map lst' ~f:decide in
      let (name', tv) =
        type_constant ~raise ~test ~protocol_version cst e.location tv_lst tv_opt in
      return (E_constant {cons_name=name';arguments=lst'}) tv
  | E_constant {cons_name;arguments} ->
      let lst' = List.map ~f:(type_expression' ~raise ~test ~protocol_version context) arguments in
      let tv_lst = List.map ~f:get_type lst' in
      let (name', tv) =
        type_constant ~raise ~test ~protocol_version cons_name e.location tv_lst tv_opt in
      return (E_constant {cons_name=name';arguments=lst'}) tv
  | E_application { lamb = ilamb ; args=_} ->
     (* TODO: This currently does not handle constraints (as those in inference). *)
     (* Get lambda and applications: (..((lamb arg1) arg2) ...) argk) *)
     let lamb, args = I.Helpers.destruct_applications e in
     (* Type-check all the involved subexpressions *)
     let args = List.map ~f:(type_expression' ~raise ~protocol_version ~test context) args in
     let lamb = type_expression' ~raise ~protocol_version ~test ~args ?last:tv_opt context lamb in
     (* Remove and save prefix for_alls in the lambda *)
     let avs, lamb_type = O.Helpers.destruct_for_alls lamb.type_expression in
     (* Try to infer/check types for the type variables *)
     let table = infer_type_applications ~raise ~loc:e.location lamb_type (List.map ~f:(fun v -> v.type_expression) args) tv_opt in
     (* Build lambda with type instantiations *)
     let lamb = build_type_insts ~raise ~loc:e.location lamb table avs in
     (* Re-build term (i.e. re-add applications) *)
     let app = trace_option ~raise (should_be_a_function_type lamb.type_expression ilamb) @@
                 O.Helpers.build_applications_opt lamb args in
     return_e app
  (* Advanced *)
  | E_matching {matchee;cases} -> (
    let matchee' = type_expression' ~raise ~test ~protocol_version context matchee in
    let aux : (I.expression, I.type_expression) I.match_case -> ((I.type_expression I.pattern * O.type_expression) list * (I.expression * context)) =
      fun {pattern ; body} -> ([(pattern,matchee'.type_expression)], (body,context))
    in
    let eqs = List.map ~f:aux cases in
    match matchee.expression_content with
    | E_variable matcheevar ->
      let case_exp = Pattern_matching.compile_matching ~raise ~err_loc:e.location ~type_f:(type_expression' ~test ~protocol_version ~args:[] ?last:None) ~body_t:(tv_opt) matcheevar eqs in
      let case_exp = { case_exp with location = e.location } in
      return case_exp.expression_content case_exp.type_expression
    | _ ->
      let matcheevar = I.Var.fresh () in
      let case_exp = Pattern_matching.compile_matching ~raise ~err_loc:e.location ~type_f:(type_expression' ~test ~protocol_version ~args:[] ?last:None) ~body_t:(tv_opt) matcheevar eqs in
      let case_exp = { case_exp with location = e.location } in
      let x = O.E_let_in { let_binder = matcheevar ; rhs = matchee' ; let_result = case_exp ; attr = {inline = false; no_mutation = false; public = true ; view= false } } in
      return x case_exp.type_expression
  )
  | E_let_in {let_binder = {var ; ascr = None ; attributes=_} ; rhs ; let_result; attr } ->
     let av, rhs = Ast_core.Combinators.get_type_abstractions rhs in
     let context = List.fold_right av ~f:(fun v c -> Context.add_type_var c v ()) ~init:context in
     let rhs = type_expression' ~raise ~protocol_version ~test context rhs in
     let binder = var in
     let rec aux t = function
       | [] -> t
       | (abs_var :: abs_vars) -> t_for_all abs_var () (aux t abs_vars) in
     let type_expression = aux rhs.type_expression (List.rev av) in
     let rhs = { rhs with type_expression } in
     let e' = Context.add_value context binder rhs.type_expression in
     let let_result = type_expression' ~raise ~protocol_version ~test e' let_result in
     return (E_let_in {let_binder = binder; rhs; let_result; attr }) let_result.type_expression
  | E_let_in {let_binder = {var ; ascr = Some tv ; attributes=_} ; rhs ; let_result; attr } ->
    let av, tv = Ast_core.Helpers.destruct_for_alls tv in
    let av', rhs = Ast_core.Combinators.get_type_abstractions rhs in
    let av = av @ av' in
    let pre_context = context in
    let context = List.fold_right av ~f:(fun v c -> Context.add_type_var c v ()) ~init:context in
    let tv = evaluate_type ~raise context tv in
    let rhs = type_expression' ~raise ~protocol_version ~test ~tv_opt:tv context rhs in
    let rec aux t = function
      | [] -> t
      | (abs_var :: abs_vars) -> t_for_all abs_var () (aux t abs_vars) in
    let type_expression = aux rhs.type_expression (List.rev av) in
    let rhs = { rhs with type_expression } in
    let binder  = var in
    let context = Context.add_value pre_context binder type_expression in
    let let_result = type_expression' ~raise ~protocol_version ~test context let_result in
    return (E_let_in {let_binder = binder; rhs; let_result; attr }) let_result.type_expression
  | E_type_in {type_binder; _} when Ast_core.Helpers.is_generalizable_variable type_binder ->
    raise.raise (wrong_generalizable e.location type_binder)
  | E_type_in {type_binder; rhs ; let_result} ->
    let rhs = evaluate_type ~raise context rhs in
    let e' = Context.add_type context type_binder rhs in
    let let_result = type_expression' ~raise ~protocol_version ~test e' let_result in
    return (E_type_in {type_binder; rhs; let_result}) let_result.type_expression
  | E_mod_in {module_binder; rhs; let_result} ->
    let rhs = type_module ~raise ~protocol_version ~test ~init_context:context rhs in
    let e' = Context.add_ez_module context module_binder rhs in
    let let_result = type_expression' ~raise ~protocol_version ~test e' let_result in
    return (E_mod_in {module_binder; rhs; let_result}) let_result.type_expression
  | E_mod_alias {alias; binders; result} ->
    let aux c binder =
      trace_option ~raise (unbound_module_variable binder e.location) @@
      Context.get_module c binder in
    let env = List.Ne.fold_left ~f:aux ~init:context binders in
    let e' = Context.add_module context alias env in
    let result = type_expression' ~raise ~test ~protocol_version e' result in
    return (E_mod_alias {alias; binders; result}) result.type_expression
  | E_raw_code {language;code} ->
    let (code,type_expression) = trace_option ~raise (expected_ascription code) @@
      I.get_e_ascription code.expression_content in
    let code = type_expression' ~raise ~test ~protocol_version context code in
    let type_expression = evaluate_type ~raise context type_expression in
    let code = {code with type_expression} in
    return (E_raw_code {language;code}) code.type_expression
  | E_recursive {fun_name; fun_type; lambda} ->
    let type_env = Context.get_type_vars context in
    let av = Ast_core.Helpers.Free_type_variables.type_expression type_env fun_type in
    let fun_type = evaluate_type ~raise context fun_type in
    let e' = Context.add_value context fun_name fun_type in
    let e' = List.fold_left av ~init:e' ~f:(fun e v -> Context.add_type_var e v ()) in
    let (lambda,lambda_type) = type_lambda ~raise ~test ~protocol_version e' lambda in
    let () = assert_type_expression_eq ~raise fun_type.location (fun_type,lambda_type) in
    return (E_recursive {fun_name;fun_type;lambda}) fun_type
  | E_ascription {anno_expr; type_annotation} ->
    let tv = evaluate_type ~raise context type_annotation in
    let expr' = type_expression' ~raise ~protocol_version ~test ~last:tv ~tv_opt:tv context anno_expr in
    let type_annotation =
      trace_option ~raise (corner_case "merge_annotations (Some ...) (Some ...) failed") @@
      O.merge_annotation
        (Some tv)
        (Some expr'.type_expression)
        O.assert_type_expression_eq in
    (* check type annotation of the expression as a whole (e.g. let x : t = (v : t') ) *)
    let () =
      match tv_opt with
      | None -> ()
      | Some tv' -> assert_type_expression_eq ~raise anno_expr.location (tv' , type_annotation) in
    {expr' with type_expression=type_annotation}
  | E_module_accessor {module_name; element} ->
    let module_env = match Context.get_module context module_name with
      Some m -> m
    | None   -> raise.raise @@ unbound_module_variable module_name e.location
    in
    let element = type_expression' ~raise ~test  ~protocol_version ~args ?last ?tv_opt module_env element in
    return (E_module_accessor {module_name; element}) element.type_expression


and type_lambda ~raise ~test ~protocol_version e {
      binder ;
      output_type ;
      result ;
    } =
      let input_type =
        Option.map ~f:(evaluate_type ~raise e) binder.ascr in
      let output_type =
        Option.map ~f:(evaluate_type ~raise e) output_type
      in
      let binder = binder.var in
      let input_type = trace_option ~raise (missing_funarg_annotation binder) input_type in
      let e' = Context.add_value e binder input_type in
      let body = type_expression' ~raise ~test ~protocol_version ?tv_opt:output_type e' result in
      let output_type = body.type_expression in
      (({binder; result=body}:O.lambda),(t_arrow input_type output_type ()))



and type_constant ~raise ~test ~protocol_version (name:I.constant') (loc:Location.t) (lst:O.type_expression list) (tv_opt:O.type_expression option) : O.constant' * O.type_expression =
  let typer = Constant_typers.constant_typers ~raise ~test ~protocol_version loc name in
  let tv = typer lst tv_opt in
  (name, tv)

let type_program ~raise ~test ~protocol_version ?env m = type_module ~raise ~test ~init_context:(Context.init ?env ()) ~protocol_version m
let type_declaration ~raise ~test ~protocol_version ?env d = snd @@ type_declaration' ~raise ~test (Context.init ?env ()) ~protocol_version d
let untype_literal (l:O.literal) : I.literal =
  let open I in
  match l with
  | Literal_unit -> Literal_unit
  | Literal_nat n -> (Literal_nat n)
  | Literal_timestamp n -> (Literal_timestamp n)
  | Literal_mutez n -> (Literal_mutez n)
  | Literal_int n -> (Literal_int n)
  | Literal_string s -> (Literal_string s)
  | Literal_signature s -> (Literal_signature s)
  | Literal_key s -> (Literal_key s)

  | Literal_key_hash s -> (Literal_key_hash s)
  | Literal_chain_id s -> (Literal_chain_id s)
  | Literal_bytes b -> (Literal_bytes b)
  | Literal_address s -> (Literal_address s)
  | Literal_operation s -> (Literal_operation s)
  | Literal_bls12_381_g1 b -> (Literal_bls12_381_g1 b)
  | Literal_bls12_381_g2 b -> (Literal_bls12_381_g2 b)
  | Literal_bls12_381_fr b -> (Literal_bls12_381_fr b)
  | Literal_chest b -> Literal_chest b
  | Literal_chest_key b -> Literal_chest_key b

let rec untype_type_expression (t:O.type_expression) : I.type_expression =
  let self = untype_type_expression in
  let return t = I.make_t t in
  match t.type_content with
  | O.T_sum {content ; layout} ->
     let aux ({associated_type ; michelson_annotation ; decl_pos} : O.row_element) =
       let associated_type = untype_type_expression associated_type in
       let v' = ({associated_type ; michelson_annotation ; decl_pos} : I.row_element) in
       v' in
     let x' = I.LMap.map aux content in
     return @@ I.T_sum { fields = x' ; layout = Some layout }
  | O.T_record {content;layout} -> (
    let aux ({associated_type ; michelson_annotation ; decl_pos} : O.row_element) =
      let associated_type = untype_type_expression associated_type in
      let v' = ({associated_type ; michelson_annotation ; decl_pos} : I.row_element) in
      v' in
    let x' = I.LMap.map aux content in
    return @@ I.T_record {fields = x' ; layout = Some layout}
  )
  | O.T_variable name -> return @@ I.T_variable name
  | O.T_arrow arr ->
    let arr = Stage_common.Maps.arrow self arr in
    return @@ I.T_arrow arr
  | O.T_constant {language=_;injection;parameters} ->
    let arguments = List.map ~f:self parameters in
    let type_operator = I.Var.fresh ~name:(Ligo_string.extract injection) () in
    return @@ I.T_app {type_operator;arguments}
  | O.T_module_accessor ma ->
    let ma = Stage_common.Maps.module_access self ma in
    return @@ I.T_module_accessor ma
  | O.T_singleton l ->
    return @@ I.T_singleton l
  | O.T_abstraction x ->
    let type_ = untype_type_expression x.type_ in
    return @@ T_abstraction {x with type_}
  | O.T_for_all x ->
    let type_ = untype_type_expression x.type_ in
    return @@ T_for_all {x with type_}

let rec untype_expression (e:O.expression) : I.expression =
  untype_expression_content e.type_expression e.expression_content
and untype_expression_content ty (ec:O.expression_content) : I.expression =
  let open I in
  let return e = e in
  match ec with
  | E_literal l ->
      let l = untype_literal l in
      return (e_literal l)
  | E_constant {cons_name;arguments} ->
      let lst' = List.map ~f:untype_expression arguments in
      return (e_constant cons_name lst')
  | E_variable n ->
      return (e_variable (n))
  | E_application {lamb;args} ->
      let f' = untype_expression lamb in
      let arg' = untype_expression args in
      return (e_application f' arg')
  | E_lambda {binder ; result} -> (
      let _, ty = Ast_typed.Helpers.destruct_for_alls ty in
      let { type1 ; type2 } = O.get_t_arrow_exn ty in
      let (input_type , output_type) =
        Pair.map   ~f:Untyper.untype_type_expression (type1, type2) in
      let result = untype_expression result in
      return (e_lambda {var=binder;ascr=Some input_type;attributes=Stage_common.Helpers.empty_attribute} (Some output_type) result)
    )
  | E_type_abstraction {type_binder;result} -> (
    let result = untype_expression result in
    return (e_type_abs type_binder result)
  )
  | E_constructor {constructor; element} ->
      let p' = untype_expression element in
      return (e_constructor constructor p')
  | E_record r ->
    let r' = LMap.map untype_expression r in
    return (e_record r' ())
  | E_record_accessor {record; path} ->
      let r' = untype_expression record in
      let Label s = path in
      return (e_record_accessor r' (Label s))
  | E_record_update {record=r; path=Label l; update=e} ->
    let r' = untype_expression r in
    let e = untype_expression e in
    return (e_record_update r' (I.Label l) e)
  | E_matching {matchee;cases} -> (
    let matchee = untype_expression matchee in
    match cases with
    | Match_variant {cases ; tv} ->
      (*
        If one day this code is actually executed, and if the list type is still not a tuple type.
        A special case for lists might be required here
      *)
      let aux : Ast_typed.matching_content_case -> _ match_case =
        fun { constructor ; pattern ; body } -> (
          let pattern =
            match tv with
            | _ ->
              let proj = Location.wrap @@ P_var { ascr = None ; var = pattern ; attributes = Stage_common.Helpers.empty_attribute } in
              Location.wrap @@ P_variant (constructor, proj)
          in
          let body = untype_expression body in
          ({pattern ; body } : (Ast_core.expression, Ast_core.type_expression) match_case)
        )
      in
      let cases = List.map ~f:aux cases in
      return (e_matching matchee cases)
    | Match_record {fields ; body ; tv=_} -> (
      let aux : (Ast_typed.label * (Ast_typed.expression_variable * _)) -> label * Ast_core.type_expression pattern =
        fun (Ast_typed.Label label, (proj,_)) -> (
          let proj = Location.wrap @@ P_var { ascr = None ; var = proj ; attributes = Stage_common.Helpers.empty_attribute } in
          (Label label, proj)
        )
      in
      let (labels,patterns) = List.unzip @@ List.map ~f:aux (LMap.to_kv_list fields) in
      let body = untype_expression body in
      let case = match Ast_typed.Helpers.is_tuple_lmap fields with
        | false ->
          let pattern = Location.wrap (P_record (labels,patterns)) in
          ({ pattern ; body } : _ Ast_core.match_case)
        | true ->
          let pattern = Location.wrap (P_tuple patterns) in
          ({ pattern ; body } : _ Ast_core.match_case)
      in
      return (e_matching matchee [case])
    )
  )
  | E_let_in {let_binder;rhs;let_result; attr} ->
      let tv = Untyper.untype_type_expression rhs.type_expression in
      let rhs = untype_expression rhs in
      let result = untype_expression let_result in
      return (e_let_in {var=let_binder ; ascr=(Some tv) ; attributes = Stage_common.Helpers.empty_attribute} rhs result attr)
  | E_type_in {type_binder;rhs;let_result} ->
      let rhs = Untyper.untype_type_expression rhs in
      let let_result = untype_expression let_result in
      return @@ make_e @@ E_type_in {type_binder; rhs; let_result }
  | E_mod_in {module_binder;rhs;let_result} ->
      let rhs = untype_module rhs in
      let result = untype_expression let_result in
      return @@ e_mod_in module_binder rhs result
  | E_mod_alias ma ->
      let ma = Stage_common.Maps.mod_alias untype_expression ma in
      return @@ make_e @@ E_mod_alias ma
  | E_raw_code {language; code} ->
      let code = untype_expression code in
      return (e_raw_code language code)
  | E_recursive {fun_name;fun_type; lambda} ->
      let fun_type = Untyper.untype_type_expression fun_type in
      let unty_expr= untype_expression_content ty @@ E_lambda lambda in
      let lambda = match unty_expr.expression_content with I.E_lambda l -> l | _ -> failwith "impossible case" in
      return @@ e_recursive fun_name fun_type lambda
  | E_module_accessor ma ->
    let ma = Stage_common.Maps.module_access untype_expression ma in
    return @@ I.make_e @@ E_module_accessor ma
  | E_type_inst {forall;type_=type_inst} ->
    match forall.type_expression.type_content with
    | T_for_all {ty_binder;type_;kind=_} ->
       let type_ = Ast_typed.Helpers.subst_type ty_binder type_inst type_ in
       let forall = { forall with type_expression = type_ } in
       untype_expression forall
    | _ ->
     failwith "Impossible case: cannot untype a type instance of a non polymorphic type"

and untype_declaration : O.declaration -> I.declaration =
let return (d: I.declaration) = d in
function
  Declaration_type {type_binder; type_expr;type_attr={public}} ->
  let type_expr = untype_type_expression type_expr in
  return @@ Declaration_type {type_binder; type_expr;type_attr={public}}
| Declaration_constant {binder;expr;attr={inline;no_mutation;public;view}} ->
  let ty = untype_type_expression expr.type_expression in
  let var = binder in
  let expr = untype_expression expr in
  return @@ Declaration_constant {binder={var;ascr=Some ty;attributes = Stage_common.Helpers.empty_attribute};expr;attr={inline;no_mutation;view;public}}
| Declaration_module {module_binder;module_;module_attr={public}} ->
  let module_ = untype_module module_ in
  return @@ Declaration_module {module_binder;module_; module_attr={public}}
| Module_alias ma ->
  return @@ Module_alias ma

and untype_module : O.module_ -> I.module_ = fun p -> List.map ~f:(Location.map untype_declaration) p

let untype_program = untype_module
