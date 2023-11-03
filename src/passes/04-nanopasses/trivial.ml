module Location = Simple_utils.Location
module Row = Ligo_prim.Row.With_optional_layout
module Value_attr = Ligo_prim.Value_attr
module Type_or_module_attr = Ligo_prim.Type_or_module_attr
module Signature_attr = Ligo_prim.Signature_attr
open Simple_utils
open Simple_utils.Trace

(*
  To_core and From_core module help moving from a "fixpoint" AST representation (in the style of AST_unified)
  to a "classical" AST representation (in the style of AST_core).
  It must be trivial and no code transformation should happen at this point


  Note/TODO : We are handling attributes here. We could (and should ?) do it in a nanopass ? but it would
  add quite a lot of nodes to AST_unified for not much ...
*)
let ig _ = Sexp.Atom "XXX"

module To_core : sig
  val program
    :  raise:(Passes.Errors.t, Main_warnings.all) Simple_utils.Trace.raise
    -> Ast_unified.program
    -> Ast_core.program

  val expression
    :  raise:(Passes.Errors.t, Main_warnings.all) Simple_utils.Trace.raise
    -> Ast_unified.expr
    -> Ast_core.expression

  val type_expression
    :  raise:(Passes.Errors.t, Main_warnings.all) Simple_utils.Trace.raise
    -> Ast_unified.ty_expr
    -> Ast_core.type_expression
end = struct
  module O = Ast_core
  module I = Ast_unified

  type statement = unit
  type block = unit
  type instruction = unit

  let rec folder ~raise =
    I.Catamorphism.
      { expr = expr ~raise
      ; ty_expr = ty_expr ~raise
      ; pattern = pattern ~raise
      ; statement = statement ~raise
      ; block = block ~raise
      ; mod_expr
      ; instruction = instruction ~raise
      ; declaration = declaration ~raise
      ; program_entry = program_entry ~raise
      ; program = Fun.id
      ; sig_expr
      ; sig_entry = sig_entry ~raise
      }


  and expression ~raise e = I.Catamorphism.cata_expr ~f:(folder ~raise) e
  and program ~raise p = I.Catamorphism.cata_program ~f:(folder ~raise) p
  and type_expression ~raise p = I.Catamorphism.cata_ty_expr ~f:(folder ~raise) p

  and dummy_top_level () =
    (* directive are translated as let _ = () *)
    let loc = Location.generated in
    O.D_value
      { binder = Ligo_prim.(Binder.make (Value_var.fresh ~loc ()) None)
      ; expr = O.e_unit ~loc ()
      ; attr = { Value_attr.default_attributes with hidden = true }
      }


  and conv_vdecl_attr ~raise : Location.t -> Value_attr.t -> I.Attribute.t -> Value_attr.t
    =
   fun loc o_attr i_attr ->
    match i_attr with
    | { key = "inline"; value = None } -> { o_attr with inline = true }
    | { key = "no_mutation"; value = None } -> { o_attr with no_mutation = true }
    | { key = "view"; value = None } -> { o_attr with view = true }
    | { key = "private"; value = None } -> { o_attr with public = false }
    | { key = "public"; value = None } -> { o_attr with public = true }
    | { key = "hidden"; value = None } -> { o_attr with hidden = true }
    | { key = "thunk"; value = None } -> { o_attr with thunk = true }
    | { key = "entry"; value = None } -> { o_attr with entry = true }
    | { key = "comment"; value = _ } -> o_attr (* TODO: We might want to keep it *)
    | { key = "dyn_entry"; value = None } -> { o_attr with dyn_entry = true }
    | { key = "deprecated"; value } -> { o_attr with deprecated = value }
    | _ ->
      raise.warning (`Nanopasses_attribute_ignored loc);
      Value_attr.default_attributes


  and conv_vsigitem_attr ~raise
      : Location.t -> O.sig_item_attribute -> I.Attribute.t -> O.sig_item_attribute
    =
   fun loc o_attr i_attr ->
    match i_attr with
    | { key = "view"; value = None } -> { o_attr with view = true }
    | { key = "entry"; value = None } -> { o_attr with entry = true }
    | { key = "dyn_entry"; value = None } -> { o_attr with dyn_entry = true }
    | { key = "comment"; value = _ } -> o_attr (* TODO: We might want to keep it *)
    | _ ->
      raise.warning (`Nanopasses_attribute_ignored loc);
      let default : O.sig_item_attribute =
        { entry = false; view = false; dyn_entry = false; optional = false }
      in
      default


  and conv_exp_attr ~raise : Location.t -> Value_attr.t -> I.Attribute.t -> Value_attr.t =
   fun loc o_attr i_attr ->
    match i_attr with
    | { key = "inline"; value = None } -> { o_attr with inline = true }
    | { key = "no_mutation"; value = None } -> { o_attr with no_mutation = true }
    | { key = "thunk"; value = None } -> { o_attr with thunk = true }
    | { key = "private"; value = None } -> { o_attr with public = false }
    | { key = "public"; value = None } -> { o_attr with public = true }
    | { key = "comment"; value = _ } -> o_attr (* TODO: We might want to keep it *)
    | { key = "deprecated"; value } -> { o_attr with deprecated = value }
    | _ ->
      raise.warning (`Nanopasses_attribute_ignored loc);
      Value_attr.default_attributes


  and conv_modtydecl_attr ~raise
      : Location.t -> Type_or_module_attr.t -> I.Attribute.t -> Type_or_module_attr.t
    =
   fun loc o_attr i_attr ->
    match i_attr with
    | { key = "private"; value = None } -> { o_attr with public = false }
    | { key = "public"; value = None } -> { o_attr with public = true }
    | { key = "hidden"; value = None } -> { o_attr with hidden = true }
    | { key = "comment"; value = _ } -> o_attr (* TODO: We might want to keep it *)
    | _ ->
      raise.warning (`Nanopasses_attribute_ignored loc);
      Type_or_module_attr.default_attributes


  and conv_signature_attr ~raise
      : Location.t -> Signature_attr.t -> I.Attribute.t -> Signature_attr.t
    =
   fun loc o_attr i_attr ->
    match i_attr with
    | { key = "private"; value = None } -> { public = false }
    | { key = "public"; value = None } -> { public = true }
    | _ ->
      raise.warning (`Nanopasses_attribute_ignored loc);
      o_attr


  and declaration ~raise
      :  ( O.declaration
         , O.expression
         , O.type_expression
         , O.type_expression option O.Pattern.t
         , O.module_expr
         , O.signature_expr )
         I.declaration_
      -> O.declaration
    =
   fun d ->
    let location = Location.get_location d in
    let ret wrap_content : O.declaration = { wrap_content; location } in
    match Location.unwrap d with
    | D_attr (attr, O.{ wrap_content = D_value x; _ }) ->
      ret @@ D_value { x with attr = conv_vdecl_attr ~raise location x.attr attr }
    | D_attr (attr, O.{ wrap_content = D_irrefutable_match x; _ }) ->
      ret
      @@ D_irrefutable_match { x with attr = conv_vdecl_attr ~raise location x.attr attr }
    | D_attr (attr, O.{ wrap_content = D_type x; _ }) ->
      ret
      @@ D_type
           { x with type_attr = conv_modtydecl_attr ~raise location x.type_attr attr }
    | D_attr (attr, O.{ wrap_content = D_module x; _ }) ->
      ret
      @@ D_module
           { x with module_attr = conv_modtydecl_attr ~raise location x.module_attr attr }
    | D_attr (attr, O.{ wrap_content = D_signature x; _ }) ->
      ret
      @@ D_signature
           { x with
             signature_attr = conv_signature_attr ~raise location x.signature_attr attr
           }
    | D_const { type_params = None; pattern; rhs_type; let_rhs }
    | D_var { type_params = None; pattern; rhs_type; let_rhs } ->
      let let_rhs =
        Option.value_map rhs_type ~default:let_rhs ~f:(fun ty ->
            O.e_ascription ~loc:let_rhs.location let_rhs ty)
      in
      (match pattern with
      | { wrap_content = P_var binder; _ } ->
        (* REMITODO : make it so it's emited this way in nanopass *)
        ret @@ D_value { binder; expr = let_rhs; attr = Value_attr.default_attributes }
      | _ ->
        ret
        @@ D_irrefutable_match
             { pattern; expr = let_rhs; attr = Value_attr.default_attributes })
    | D_directive _ -> ret (dummy_top_level ())
    | D_module { name; mod_expr; annotation = { signatures = []; filter = _ } } ->
      ret
      @@ D_module
           { module_binder = name
           ; module_ = mod_expr
           ; annotation = None
           ; module_attr = Type_or_module_attr.default_attributes
           }
    | D_module { name; mod_expr; annotation = { signatures = annotation; filter } } ->
      let items : O.sig_item list =
        List.map ~f:(fun sig_ -> O.S_include sig_) annotation
      in
      let signature : O.signature_expr =
        Location.wrap ~loc:Location.generated (O.S_sig { items })
      in
      ret
      @@ D_module
           { module_binder = name
           ; module_ = mod_expr
           ; annotation = Some { signature; filter }
           ; module_attr = O.TypeOrModuleAttr.default_attributes
           }
    | D_signature { name; sig_expr; extends } ->
      let items : O.sig_item list = List.map ~f:(fun sig_ -> O.S_include sig_) extends in
      let loc = Location.get_location sig_expr in
      let sig_ = Location.unwrap sig_expr in
      let items = items @ [ O.S_include (Location.wrap ~loc sig_) ] in
      let signature = Location.wrap ~loc @@ O.S_sig { items } in
      ret
      @@ D_signature
           { signature_binder = name
           ; signature
           ; signature_attr = Signature_attr.default_attributes
           }
    | D_type { name; type_expr } ->
      ret
      @@ D_type
           { type_binder = name
           ; type_expr
           ; type_attr = Type_or_module_attr.default_attributes
           }
    | D_irrefutable_match { pattern; expr } ->
      ret @@ D_irrefutable_match { pattern; expr; attr = Value_attr.default_attributes }
    | D_module_include x -> ret @@ D_module_include x
    | D_let _ | D_import _ | D_export _ | D_var _ | D_multi_const _ | D_multi_var _
    | D_const { type_params = Some _; _ }
    | _ ->
      raise.error
        (Passes.Errors.invariant_trivial location
        @@ Format.asprintf "%a" Sexp.pp_hum (I.sexp_of_declaration_ ig ig ig ig ig ig d))


  and expr ~raise
      :  ( O.expression
         , O.type_expression
         , O.type_expression option O.Pattern.t
         , block
         , O.module_expr )
         I.expression_
      -> O.expression
    =
   fun e ->
    let location = Location.get_location e in
    let ret ?(location = location) expression_content : O.expression =
      O.{ expression_content; location }
    in
    match Location.unwrap e with
    | E_attr (attr, { expression_content = E_let_in x; location; _ }) ->
      ret ~location
      @@ E_let_in { x with attributes = conv_exp_attr ~raise location x.attributes attr }
    | E_attr (attr, { expression_content = E_let_mut_in x; location; _ }) ->
      ret ~location
      @@ E_let_mut_in
           { x with attributes = conv_exp_attr ~raise location x.attributes attr }
    | E_literal x -> ret @@ E_literal x
    | E_variable x -> ret @@ E_variable x
    | E_contract x -> ret @@ E_contract x
    | E_record_pun fields ->
      let x =
        List.map
          ~f:(function
            | Complete (l, r) -> l, r
            | Punned l ->
              let loc = Location.get_location l in
              let label = Location.unwrap l in
              ( label
              , O.e_variable ~loc (I.Variable.of_input_var ~loc (I.Label.to_string label))
              ))
          fields
      in
      ret @@ E_record (Ligo_prim.Record.of_list x)
    | E_module_access { module_path; field; _ } ->
      ret
      @@ E_module_accessor { module_path = List.Ne.to_list module_path; element = field }
    | E_match { expr; cases } ->
      ret
      @@ E_matching
           { matchee = expr
           ; cases =
               List.map (List.Ne.to_list cases) ~f:(function I.Case.{ pattern; rhs } ->
                   let default : O.type_expression option O.Pattern.t =
                     Location.wrap
                       ~loc:rhs.location
                       O.Pattern.(
                         P_var
                           Ligo_prim.Binder.(
                             make
                               (I.Variable.fresh ~loc:rhs.location ~name:"default" ())
                               None))
                   in
                   let pattern = Option.value ~default pattern in
                   O.Match_expr.{ pattern; body = rhs })
           }
    | E_annot (anno_expr, type_annotation) ->
      ret @@ E_ascription { anno_expr; type_annotation }
    | E_type_in { type_decl = { name; params = None; type_expr }; body } ->
      ret @@ E_type_in { type_binder = name; rhs = type_expr; let_result = body }
    | E_mod_in { module_name; rhs; body } ->
      ret @@ E_mod_in { module_binder = module_name; rhs; let_result = body }
    | E_raw_code x -> ret @@ E_raw_code x
    | E_assign_unitary x -> ret @@ E_assign x
    | E_while { cond; block } -> ret @@ E_while { cond; body = block }
    | E_for { index; init; bound; step; block } ->
      let incr =
        (* REMITODO : dedicated pass for this *)
        Option.value_map step ~default:(O.e_int ~loc:Location.generated Z.one) ~f:Fun.id
      in
      ret @@ E_for { binder = index; start = init; final = bound; incr; f_body = block }
    | E_for_in (ForMap { binding = k, v; collection; block }) ->
      ret
      @@ E_for_each
           { fe_binder = k, Some v; collection; collection_type = Map; fe_body = block }
    | E_for_in (ForSetOrList { var; for_kind; collection; block }) ->
      let collection_type : Ligo_prim.For_each_loop.collect_type =
        match for_kind with
        | `Set -> Set
        | `List -> List
      in
      ret
      @@ E_for_each
           { fe_binder = var, None; collection; collection_type; fe_body = block }
    | E_for_in (ForAny { pattern; collection; block }) ->
      let fe_binder =
        (* REMITODO : specific pass for E_for_in*)
        match Location.unwrap pattern with
        | P_var var -> Ligo_prim.Binder.get_var var, None
        | P_tuple [ { wrap_content = P_var vl; _ }; { wrap_content = P_var vr; _ } ] ->
          Ligo_prim.Binder.get_var vl, Some (Ligo_prim.Binder.get_var vr)
        | _ -> assert false
      in
      ret @@ E_for_each { fe_binder; collection; collection_type = Any; fe_body = block }
    | E_constant x -> ret @@ E_constant x
    | E_applied_constructor x -> ret @@ E_constructor x
    | E_simple_let_in { binder; rhs; let_result } ->
      ret
      @@ E_let_in
           { let_binder = binder
           ; rhs
           ; let_result
           ; attributes = Value_attr.default_attributes
           }
    | E_recursive { fun_name; fun_type; lambda } ->
      ret @@ E_recursive { fun_name; fun_type; lambda; force_lambdarec = false }
    | E_lambda x -> ret @@ E_lambda x
    | E_application x -> ret @@ E_application x
    | E_type_abstraction { type_binder; result } ->
      ret @@ E_type_abstraction { type_binder; result }
    | E_record_update { struct_; label; update } ->
      ret @@ E_update { struct_; path = label; update }
    | E_record_access { struct_; label } -> ret @@ E_accessor { struct_; path = label }
    | E_let_mut_in
        { is_rec = false; type_params = None; lhs = let_binder, []; rhs_type; rhs; body }
      ->
      let rhs =
        Option.value_map rhs_type ~default:rhs ~f:(fun ty ->
            ret @@ E_ascription { anno_expr = rhs; type_annotation = ty })
      in
      ret
      @@ E_let_mut_in
           { let_binder
           ; rhs
           ; let_result = body
           ; attributes = Value_attr.default_attributes
           }
    | _ ->
      raise.error
        (Passes.Errors.invariant_trivial location
        @@ Format.asprintf "%a" Sexp.pp_hum (I.sexp_of_expression_ ig ig ig ig ig e))


  and ty_expr ~raise : O.type_expression I.ty_expr_ -> O.type_expression =
   fun t ->
    let location = Location.get_location t in
    let ret type_content : O.type_expression = O.{ type_content; location } in
    match Location.unwrap t with
    | T_attr (_, ty) -> ty
    | T_var v -> ret @@ T_variable v
    | T_contract_parameter x -> ret @@ T_contract_parameter x
    | T_constant t ->
      (match Ligo_prim.Literal_types.of_string_opt t with
      | Some t -> ret @@ T_constant (t, Ligo_prim.Literal_types.to_arity t)
      | None -> failwith @@ Format.asprintf "Type constant %s is unknown." t)
    | T_module_app { constr = { module_path; field; _ }; type_args } ->
      ret
      @@ T_app
           { type_operator =
               { module_path = List.Ne.to_list module_path; element = field }
           ; arguments = List.Ne.to_list type_args
           }
    | T_app { constr; type_args } ->
      (match constr with
      | { type_content = T_variable type_operator; _ } ->
        ret
        @@ T_app
             { type_operator = { module_path = []; element = type_operator }
             ; arguments = List.Ne.to_list type_args
             }
      | _ -> raise.error (Passes.Errors.invariant_trivial location "type"))
    | T_fun (type1, type2) -> ret @@ T_arrow { type1; type2 }
    | T_string str -> ret @@ T_singleton (Literal_string (Ligo_string.standard str))
    | T_int (_, x) -> ret @@ T_singleton (Literal_int x)
    | T_module_access { module_path; field; _ } ->
      ret
      @@ T_module_accessor { module_path = List.Ne.to_list module_path; element = field }
    | T_sum r -> ret @@ T_sum r
    | T_record r -> ret @@ T_record r
    | T_abstraction abs -> ret @@ T_abstraction abs
    | T_for_all forall -> ret @@ T_for_all forall
    | _ ->
      raise.error
        (Passes.Errors.invariant_trivial location
        @@ Format.asprintf "To_core : %a" Sexp.pp_hum (I.sexp_of_ty_expr_ ig t))


  and pattern ~raise
      :  (O.type_expression option O.Pattern.t, O.type_expression) I.pattern_
      -> O.type_expression option O.Pattern.t
    =
   fun p ->
    let location = Location.get_location p in
    let ret wrap_content : O.type_expression option O.Pattern.t =
      { wrap_content; location }
    in
    match Location.unwrap p with
    | P_attr (_, p) ->
      (* should warn about ignored attribute ? *)
      p
    | P_unit -> ret @@ P_unit
    | P_typed (ty, { wrap_content = P_var x; _ }) ->
      ret @@ P_var (Ligo_prim.Binder.set_ascr x (Some ty))
    | P_var x -> ret @@ P_var (Ligo_prim.Binder.make x None)
    | P_var_typed (ty, x) -> ret @@ P_var (Ligo_prim.Binder.make x (Some ty))
    | P_list (List lst) -> ret @@ P_list (List lst)
    | P_list (Cons (l, r)) -> ret @@ P_list (Cons (l, r))
    | P_variant (l, Some p) -> ret @@ P_variant (l, p)
    | P_variant (l, None) ->
      ret @@ P_variant (l, Location.wrap ~loc:location O.Pattern.P_unit)
    | P_tuple lst -> ret @@ P_tuple lst
    | P_pun_record lst
      when List.for_all lst ~f:(function
               | Punned _ -> false
               | Complete _ -> true) ->
      let lst =
        List.map
          ~f:(function
            | Complete x -> x
            | Punned _ -> raise.error (Passes.Errors.invariant_trivial location "pattern"))
          lst
      in
      ret @@ P_record (Ligo_prim.Record.of_list lst)
    | _ ->
      raise.error
        (Passes.Errors.invariant_trivial location
        @@ Format.asprintf "%a" Sexp.pp_hum (I.sexp_of_pattern_ ig ig p))


  and statement ~raise : _ I.statement_ -> statement =
   fun s -> raise.error (Passes.Errors.invariant_trivial (Location.get_location s) "stmt")


  and block ~raise : _ I.block_ -> statement =
   fun b ->
    raise.error (Passes.Errors.invariant_trivial (Location.get_location b) "block")


  and instruction ~raise : _ I.instruction_ -> instruction =
   fun i ->
    raise.error (Passes.Errors.invariant_trivial (Location.get_location i) "instr")


  and program_entry ~raise
      : (O.declaration, O.declaration, unit) I.program_entry_ -> O.declaration
    =
    let ret location wrap_content : O.declaration = { wrap_content; location } in
    function
    | PE_attr (attr, O.{ wrap_content = D_value x; location }) ->
      ret location
      @@ D_value { x with attr = conv_vdecl_attr ~raise location x.attr attr }
    | PE_attr (attr, O.{ wrap_content = D_irrefutable_match x; location }) ->
      ret location
      @@ D_irrefutable_match { x with attr = conv_vdecl_attr ~raise location x.attr attr }
    | PE_attr (attr, O.{ wrap_content = D_type x; location }) ->
      ret location
      @@ D_type
           { x with type_attr = conv_modtydecl_attr ~raise location x.type_attr attr }
    | PE_attr (attr, O.{ wrap_content = D_module x; location }) ->
      ret location
      @@ D_module
           { x with module_attr = conv_modtydecl_attr ~raise location x.module_attr attr }
    | PE_attr (_, (O.{ wrap_content = D_module_include _; location } as d)) ->
      raise.warning (`Nanopasses_attribute_ignored location);
      program_entry ~raise (PE_declaration d)
    | PE_attr (attr, O.{ wrap_content = D_signature x; location }) ->
      ret location
      @@ D_signature
           { x with
             signature_attr = conv_signature_attr ~raise location x.signature_attr attr
           }
    | PE_declaration d -> d
    | PE_preproc_directive _ -> Location.wrap ~loc:Location.generated (dummy_top_level ())
    | PE_top_level_instruction _ ->
      raise.error (Passes.Errors.invariant_trivial Location.generated "pe")
    | PE_export _ -> raise.error (Passes.Errors.invariant_trivial Location.generated "pe")


  and mod_expr : (O.module_expr, O.program) I.mod_expr_ -> O.module_expr =
   fun m ->
    let location = Location.get_location m in
    let ret wrap_content : O.module_expr = { wrap_content; location } in
    match Location.unwrap m with
    | I.M_body x -> ret @@ M_struct x
    | I.M_path x -> ret @@ M_module_path x
    | I.M_var x -> ret @@ M_variable x


  and sig_expr
      :  (O.signature_expr, O.sig_item, O.type_expression) I.Types.sig_expr_
      -> O.signature_expr
    =
   fun se ->
    let sig_expr
        (se : (O.signature_expr, O.sig_item, O.type_expression) I.Types.sig_expr_)
      =
      let loc = Location.get_location se in
      match Location.unwrap se with
      | I.Types.S_body m -> Location.wrap ~loc @@ O.S_sig { items = m }
      | S_path p -> Location.wrap ~loc @@ O.S_path p
    in
    sig_expr se


  and sig_entry ~raise
      : (O.signature_expr, O.sig_item, O.type_expression) I.Types.sig_entry_ -> O.sig_item
    =
   fun item ->
    let attr optional : O.sig_item_attribute =
      { entry = false; view = false; dyn_entry = false; optional }
    in
    match Location.unwrap item with
    | S_value (v, ty, optional) -> S_value (v, ty, attr optional)
    | S_type (v, ty) -> S_type (v, ty)
    | S_type_var v -> S_type_var v
    | S_attr (attr', S_value (v, ty, attr)) ->
      let location = Location.get_location item in
      let attr = conv_vsigitem_attr ~raise location attr attr' in
      S_value (v, ty, attr)
    | S_include se -> S_include se
    | _ ->
      raise.error
        (Passes.Errors.invariant_trivial (Location.get_location item)
        @@ Format.asprintf "%a" Sexp.pp_hum (I.sexp_of_sig_entry_ ig ig ig item))
end

module From_core : sig
  val program
    :  raise:(Passes.Errors.t, Main_warnings.all) Simple_utils.Trace.raise
    -> Ast_core.program
    -> Ast_unified.program

  val pattern
    :  raise:(Passes.Errors.t, Main_warnings.all) Simple_utils.Trace.raise
    -> Ast_core.type_expression option Ast_core.Pattern.t
    -> Ast_unified.pattern

  val expression
    :  raise:(Passes.Errors.t, Main_warnings.all) Simple_utils.Trace.raise
    -> Ast_core.expression
    -> Ast_unified.expr

  val type_expression
    :  raise:(Passes.Errors.t, Main_warnings.all) Simple_utils.Trace.raise
    -> Ast_core.type_expression
    -> Ast_unified.ty_expr

  val signature
    :  raise:(Passes.Errors.t, Main_warnings.all) Simple_utils.Trace.raise
    -> Ast_core.signature_expr
    -> Ast_unified.sig_expr
end = struct
  module I = Ast_core
  module O = Ast_unified

  type block = unit
  (* type instruction = unit
  type statement = unit *)

  let rec unfolder ~raise =
    Ast_unified.Anamorphism.
      { expr = expr ~raise
      ; ty_expr = ty_expr ~raise
      ; pattern = pattern_
      ; statement = (fun _ -> assert false)
      ; block = (fun _ -> assert false)
      ; mod_expr = (fun _ -> assert false)
      ; instruction = (fun _ -> assert false)
      ; declaration = (fun _ -> assert false)
      ; program_entry = (fun _ -> assert false)
      ; program = Fun.id
      ; sig_expr
      ; sig_entry
      }


  and program ~raise p = Ast_unified.Anamorphism.ana_program ~f:(unfolder ~raise) p
  and expression ~raise e = Ast_unified.Anamorphism.ana_expr ~f:(unfolder ~raise) e
  and pattern ~raise p = Ast_unified.Anamorphism.ana_pattern ~f:(unfolder ~raise) p

  and type_expression ~raise t =
    Ast_unified.Anamorphism.ana_ty_expr ~f:(unfolder ~raise) t


  and signature ~raise s = Ast_unified.Anamorphism.ana_sig_expr ~f:(unfolder ~raise) s

  and conv_row_attr : string option -> O.Attribute.t list = function
    | None -> []
    | Some annot -> [ { key = "annot"; value = Some annot } ]


  and conv_sig_entry_attr
      : I.sig_item_attribute -> (O.Attribute.t * I.sig_item_attribute) option
    =
   fun ({ dyn_entry; entry; view; optional } as i_attr) ->
    if dyn_entry
    then Some ({ key = "dyn_entry"; value = None }, { i_attr with dyn_entry = false })
    else if entry
    then Some ({ key = "entry"; value = None }, { i_attr with entry = false })
    else if view
    then Some ({ key = "view"; value = None }, { i_attr with view = false })
    else if optional
    then Some ({ key = "optional"; value = None }, { i_attr with optional = false })
    else None


  and sig_expr
      : I.signature_expr -> (I.signature_expr, I.sig_item, I.type_expression) O.sig_expr_
    =
   fun { wrap_content = content; location = loc } ->
    match content with
    | S_sig { items } -> Location.wrap ~loc (O.S_body items)
    | S_path path -> Location.wrap ~loc (O.S_path path)


  and sig_entry
      : I.sig_item -> (I.signature_expr, I.sig_item, I.type_expression) O.sig_entry_
    =
   fun sig_item ->
    let ret ~loc content : _ O.sig_entry_ = Location.wrap ~loc content in
    match sig_item with
    | I.S_value (v, ty, attr) ->
      (match conv_sig_entry_attr attr with
      | None -> ret ~loc:ty.location (S_value (v, ty, attr.optional))
      | Some (attr, attr_rest) ->
        ret ~loc:ty.location (S_attr (attr, I.S_value (v, ty, attr_rest))))
    | I.S_type (v, ty) -> ret ~loc:ty.location (S_type (v, ty))
    | I.S_type_var v -> ret ~loc:(O.Ty_variable.get_location v) (S_type_var v)
    | I.S_include se -> ret ~loc:se.location (S_include se)
    | I.S_module (_, _) | I.S_module_type (_, _) -> failwith "Impossible"


  and expr ~raise
      :  I.expression
      -> ( I.expression
         , I.type_expression
         , I.type_expression option I.Pattern.t
         , block
         , I.module_expr )
         O.expression_
    =
   fun e ->
    let ret content : _ O.expression_ =
      let loc = e.location in
      Location.wrap ~loc content
    in
    match e.expression_content with
    | E_literal x -> ret @@ E_literal x
    | E_variable x -> ret @@ E_variable x
    | E_record fields ->
      let lst = Ligo_prim.Record.to_list fields in
      ret @@ E_record_pun (List.map lst ~f:(fun (l, e) -> O.Field.Complete (l, e)))
    | E_module_accessor { module_path; element } ->
      let module_path =
        match List.Ne.of_list_opt module_path with
        | Some x -> x
        | None ->
          raise.error
            (Passes.Errors.invariant_trivial e.location "module accessor decompilation")
      in
      ret @@ E_module_access { module_path; field = element; field_as_open = false }
    | E_matching { matchee; cases } ->
      let cases = List.map cases ~f:(fun _ -> assert false) in
      ret @@ E_match { expr = matchee; cases = List.Ne.of_list cases }
    | E_ascription { anno_expr; type_annotation } ->
      ret @@ E_annot (anno_expr, type_annotation)
    | E_type_in { type_binder; rhs; let_result } ->
      ret
      @@ E_type_in
           { type_decl = { name = type_binder; params = None; type_expr = rhs }
           ; body = let_result
           }
    | E_mod_in { module_binder; rhs; let_result } ->
      ret @@ E_mod_in { module_name = module_binder; rhs; body = let_result }
    | E_raw_code x -> ret @@ E_raw_code x
    | E_assign x -> ret @@ E_assign_unitary x
    | E_while { cond; body } -> ret @@ E_while { cond; block = body }
    | E_for { binder; start; final; incr; f_body } ->
      ret
      @@ E_for
           { index = binder
           ; init = start
           ; bound = final
           ; step = Some incr
           ; block = f_body
           }
    | E_for_each { fe_binder = k, Some v; collection; collection_type = Map; fe_body } ->
      ret @@ E_for_in (ForMap { binding = k, v; collection; block = fe_body })
    | E_for_each { fe_binder = var, None; collection; collection_type = Set; fe_body } ->
      ret @@ E_for_in (ForSetOrList { var; for_kind = `Set; collection; block = fe_body })
    | E_for_each { fe_binder = var, None; collection; collection_type = List; fe_body } ->
      ret
      @@ E_for_in (ForSetOrList { var; for_kind = `List; collection; block = fe_body })
    | E_for_each { fe_binder; collection; collection_type = Any; fe_body } ->
      let loc = Location.dummy in
      let pattern =
        match fe_binder with
        | v, None -> I.Pattern.var ~loc (Ligo_prim.Binder.make v None)
        | k, Some v ->
          let k = I.Pattern.var ~loc (Ligo_prim.Binder.make k None) in
          let v = I.Pattern.var ~loc (Ligo_prim.Binder.make v None) in
          Location.wrap ~loc @@ I.Pattern.P_tuple [ k; v ]
      in
      ret @@ E_for_in (ForAny { pattern; collection; block = fe_body })
    | E_constant x -> ret @@ E_constant x
    | E_application { lamb; args } -> ret @@ E_application { lamb; args }
    | _ ->
      (* TODO : one day :) *)
      assert false


  and ty_expr ~raise : I.type_expression -> I.type_expression O.ty_expr_ =
   fun ty ->
    let loc = Location.dummy in
    let ret content : _ O.ty_expr_ =
      let loc = ty.location in
      Location.wrap ~loc content
    in
    let conv_fields (x : I.type_expression O.Record.t) =
      List.mapi (O.Record.to_list x) ~f:(fun decl_pos (label, associated_type) ->
          ( label
          , O.Non_linear_rows.
              { associated_type = Some associated_type; attributes = []; decl_pos } ))
    in
    match ty.type_content with
    | T_variable v -> ret @@ T_var v
    | T_contract_parameter x -> ret @@ T_contract_parameter x
    | T_constant (t, _) -> ret @@ T_constant (Ligo_prim.Literal_types.to_string t)
    | T_app { type_operator; arguments } ->
      ignore type_operator.module_path;
      (* TODO *)
      let constr = I.make_t ~loc (T_variable type_operator.element) in
      ret @@ T_app { constr; type_args = List.Ne.of_list arguments }
    | T_arrow { type1; type2 } -> ret @@ T_fun (type1, type2)
    | T_singleton (Literal_string x) -> ret @@ T_string (Ligo_string.extract x)
    | T_singleton (Literal_int x) -> ret @@ T_int (Z.to_string x, x)
    | T_singleton _ ->
      raise.error (Passes.Errors.invariant_trivial ty.location "unknown singleton")
    | T_module_accessor { module_path; element } ->
      ret
      @@ T_module_access
           { module_path = List.Ne.of_list module_path
           ; field = element
           ; field_as_open = false
           }
    | T_sum _ when is_some (I.get_t_bool ty) ->
      ret @@ T_var (O.Ty_variable.of_input_var ~loc "bool")
    | T_sum { fields; layout = _ } when is_some (I.get_t_option ty) ->
      let constr = I.make_t ~loc (T_variable (O.Ty_variable.of_input_var ~loc "option"))
      and arg = Ligo_prim.Label.Map.find_exn fields (Label "Some") in
      ret
      @@ T_app
           { constr
           ; type_args =
               List.Ne.singleton arg
               (* XXX for some reason matching on [I.get_t_option ty] transforms "int option"
                        to "a option" so we have manual matching here instead *)
           }
    | T_sum { fields; layout } ->
      ignore layout;
      (* TODO .. ? how ? *)
      ignore conv_row_attr;
      ret @@ T_sum_raw (conv_fields fields)
      (* ret @@ T_attr (attr, I.make_t ~loc @@ T_record { recc with layout = None }) *)
    | T_record row when Row.is_tuple row ->
      let t =
        match Row.to_tuple row with
        | [] -> raise.error (Passes.Errors.invariant_trivial ty.location "empty record")
        | a :: b -> a, b
      in
      ret @@ T_prod t
    | T_record { fields; layout } ->
      ignore layout;
      (* TODO .. ? how ? *)
      ignore conv_row_attr;
      ret @@ T_record_raw (conv_fields fields)
    | T_abstraction x -> ret @@ T_abstraction x
    | T_for_all x -> ret @@ T_for_all x


  and pattern_
      :  I.type_expression option I.Pattern.t
      -> (I.type_expression option I.Pattern.t, I.type_expression) O.pattern_
    =
   fun p ->
    let ret content : _ O.pattern_ =
      let loc = Location.get_location p in
      Location.wrap ~loc content
    in
    match Location.unwrap p with
    | P_unit -> ret @@ P_unit
    | P_var v -> ret @@ P_var (Ligo_prim.Binder.get_var v)
    | P_list (List lst) -> ret @@ P_list (List lst)
    | P_list (Cons (l, r)) -> ret @@ P_list (Cons (l, r))
    | P_variant (l, p) -> ret @@ P_variant (l, Some p)
    | P_tuple lst -> ret @@ P_tuple lst
    | P_record lst ->
      let lst =
        List.map ~f:(fun (l, p) -> O.Field.Complete (l, p)) (O.Record.to_list lst)
      in
      ret @@ P_pun_record lst
end
