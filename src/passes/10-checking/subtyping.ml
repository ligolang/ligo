module Well_formed = Context.Well_formed
module Exists_var = Context.Exists_var
module Trace = Simple_utils.Trace
open Trace
module Errors = Errors
open Errors
open Ast_typed
open Ligo_prim

let occurs_check ~raise ~loc ~evar type_ =
  let fail () = raise.error (occurs_check_failed loc evar type_) in
  let rec loop type_ =
    match type_.type_content with
    | T_variable tvar' ->
      (match Exists_var.of_type_var tvar' with
       | Some evar' -> if Exists_var.equal evar evar' then fail ()
       | None -> ())
    | T_arrow { type1; type2 } ->
      loop type1;
      loop type2
    | T_for_all { type_; _ } | T_abstraction { type_; _ } -> loop type_
    | T_constant { parameters; _ } -> List.iter parameters ~f:loop
    | T_record rows | T_sum rows ->
      Record.LMap.iter
        (fun _label ({ associated_type; _ } : _ Rows.row_element_mini_c) ->
          loop associated_type)
        rows.fields
    | T_singleton _ -> ()
  in
  loop type_


module Mode = struct
  type t =
    | Covariant (* + *)
    | Contravariant (* - *)
    | Invariant (* +- *)

  let invert t =
    match t with
    | Covariant -> Contravariant
    | Contravariant -> Covariant
    | Invariant -> Invariant
end

let t_subst t ~tvar ~type_ =
  Ast_typed.Helpers.subst_no_capture_type tvar type_ t


let t_subst_var ~loc t ~tvar ~tvar' =
  t_subst t ~tvar ~type_:(t_variable ~loc tvar' ())


let t_exists ~loc (evar : Exists_var.t) =
  t_variable ~loc (evar :> type_variable) ()


let rec lift
  ~raise
  ~loc
  ~ctx
  ~(mode : Mode.t)
  ~kind
  ~evar
  (type_ : type_expression)
  : Context.t * type_expression
  =
  let self ?(ctx = ctx) ~mode = lift ~raise ~loc ~ctx ~mode ~kind ~evar in
  let self_row ~ctx = lift_row ~raise ~loc ~ctx ~kind ~evar in
  let return content = { type_ with type_content = content } in
  match type_.type_content with
  | T_variable tvar' ->
    (match Exists_var.of_type_var tvar' with
     | Some evar' ->
       let ctx1, ctx2 = Context.split_at ctx ~at:(C_exists_var (evar, kind)) in
       if List.mem ~equal:Exists_var.equal (Context.get_exists_vars ctx1) evar'
       then ctx, type_
       else (
         let evar'' = Exists_var.fresh ~loc () in
         let type_ = t_exists ~loc evar'' in
         ( Context.(
             ctx1
             |:: C_exists_var (evar'', kind)
             |:: C_exists_var (evar, kind)
             |@ add_exists_eq ctx2 evar' kind type_)
         , type_ ))
     | None -> ctx, type_)
  | T_for_all { ty_binder = tvar'; kind = kind'; type_ } ->
    (match mode with
     | Contravariant ->
       let ctx1, ctx2 = Context.split_at ctx ~at:(C_exists_var (evar, kind)) in
       let evar' = Exists_var.fresh ~loc () in
       self
         ~ctx:
           Context.(
             ctx1
             |:: C_exists_var (evar', kind')
             |:: C_exists_var (evar, kind)
             |@ ctx2)
         ~mode:Contravariant
         (t_subst ~tvar:tvar' ~type_:(t_exists ~loc evar') type_)
     | Covariant ->
       let ctx, pos = Context.mark ctx ~mut:false in
       let ctx, type_ =
         self
           ~ctx:Context.(ctx |:: C_type_var (tvar', kind'))
           ~mode:Covariant
           type_
       in
       Context.drop_until ctx ~pos, type_
     | Invariant ->
       let ctx, pos = Context.mark ctx ~mut:false in
       let ctx, type_ =
         self
           ~ctx:Context.(ctx |:: C_type_var (tvar', kind'))
           ~mode:Invariant
           type_
       in
       ( Context.drop_until ctx ~pos
       , return @@ T_for_all { ty_binder = tvar'; kind = kind'; type_ } ))
  | T_abstraction { ty_binder = tvar'; kind; type_ } ->
    let tvar'' = Type_var.fresh ~loc () in
    let type_ = t_subst_var ~loc type_ ~tvar:tvar' ~tvar':tvar'' in
    let ctx, pos = Context.mark ctx ~mut:false in
    let ctx, type_ =
      self ~ctx:Context.(ctx |:: C_type_var (tvar'', kind)) ~mode type_
    in
    Context.drop_until ctx ~pos, type_
  | T_arrow { type1; type2 } ->
    let ctx, type1 = self ~ctx ~mode:(Mode.invert mode) type1 in
    let ctx, type2 = self ~ctx ~mode (Context.apply ctx type2) in
    ctx, return @@ T_arrow { type1; type2 }
  | T_sum { fields; layout } ->
    let ctx, fields = self_row ~ctx fields in
    ctx, return @@ T_sum { fields; layout }
  | T_record { fields; layout } ->
    let ctx, fields = self_row ~ctx fields in
    ctx, return @@ T_record { fields; layout }
  | T_constant inj ->
    let ctx, parameters =
      List.fold_map inj.parameters ~init:ctx ~f:(fun ctx param ->
        self ~ctx ~mode:Invariant (Context.apply ctx param))
    in
    ctx, return @@ T_constant { inj with parameters }
  | T_singleton _ -> ctx, type_


and lift_row ~raise ~loc ~ctx ~kind ~evar fields =
  Record.LMap.fold_map
    fields
    ~init:ctx
    ~f:(fun _label (row_elem : _ Rows.row_element_mini_c) ctx ->
    (* TODO: Formalize addition of rows to calculus (including treatment of variance) *)
    let ctx, associated_type =
      lift
        ~raise
        ~ctx
        ~mode:Invariant
        ~kind
        ~evar
        ~loc
        (Context.apply ctx row_elem.associated_type)
    in
    ctx, { row_elem with associated_type })


let equal_literal lit1 lit2 = Literal_value.compare lit1 lit2

let consistent_injections
  { language = lang1; injection = inj1; _ }
  { language = lang2; injection = inj2; _ }
  =
  String.(lang1 = lang2) && Literal_types.equal inj1 inj2


let equal_domains lmap1 lmap2 =
  let open Record in
  LSet.(equal (of_list (LMap.keys lmap1)) (of_list (LMap.keys lmap2)))


let rec unify
  ~raise
  ~loc
  ~(ctx : Context.t)
  (type1 : type_expression)
  (type2 : type_expression)
  : Context.t
  =
  let unify = unify ~loc in
  let self ?(ctx = ctx) type1 type2 = unify ~raise ~ctx type1 type2 in
  let fail () = raise.error (cannot_unify loc type1 type2) in
  let unify_evar evar type_ =
    occurs_check ~raise ~loc ~evar type_;
    let kind =
      Context.get_exists_var ctx evar
      |> trace_option
           ~raise
           (unbound_exists_variable (Exists_var.loc evar) evar)
    in
    let ctx, type_ = lift ~raise ~loc ~ctx ~mode:Invariant ~evar ~kind type_ in
    if not
         (match Well_formed.type_expr ~ctx type_ with
          | Some kind' -> Kind.equal kind kind'
          | _ -> false)
    then raise.error (ill_formed_type type_.location type_);
    Context.add_exists_eq ctx evar kind type_
  in
  match type1.type_content, type2.type_content with
  | T_singleton lit1, T_singleton lit2 when Literal_value.equal lit1 lit2 -> ctx
  | T_constant inj1, T_constant inj2 when consistent_injections inj1 inj2 ->
    (match
       List.fold2
         inj1.parameters
         inj2.parameters
         ~init:ctx
         ~f:(fun ctx param1 param2 ->
         self ~ctx (Context.apply ctx param1) (Context.apply ctx param2))
     with
     | Ok ctx -> ctx
     | Unequal_lengths ->
       raise.error
       @@ corner_case
            "Cannot occur since injections are consistent and fully applied")
  | T_variable tvar1, T_variable tvar2 when Type_var.equal tvar1 tvar2 -> ctx
  | T_variable tvar1, _ when Type_var.is_exists tvar1 ->
    unify_evar (Exists_var.of_type_var_exn tvar1) type2
  | _, T_variable tvar2 when Type_var.is_exists tvar2 ->
    unify_evar (Exists_var.of_type_var_exn tvar2) type1
  | ( T_arrow { type1 = type11; type2 = type12 }
    , T_arrow { type1 = type21; type2 = type22 } ) ->
    let ctx = self ~ctx type11 type21 in
    self ~ctx (Context.apply ctx type12) (Context.apply ctx type22)
  | ( T_for_all { ty_binder = tvar1; kind = kind1; type_ = type1 }
    , T_for_all { ty_binder = tvar2; kind = kind2; type_ = type2 } )
  | ( T_abstraction { ty_binder = tvar1; kind = kind1; type_ = type1 }
    , T_abstraction { ty_binder = tvar2; kind = kind2; type_ = type2 } )
    when Kind.equal kind1 kind2 ->
    let tvar = Type_var.fresh_like ~loc tvar1 in
    let type1 = t_subst_var ~loc ~tvar:tvar1 ~tvar':tvar type1 in
    let type2 = t_subst_var ~loc ~tvar:tvar2 ~tvar':tvar type2 in
    let ctx, pos = Context.mark ctx ~mut:false in
    self ~ctx:Context.(ctx |:: C_type_var (tvar, kind1)) type1 type2
    |> Context.drop_until ~pos
  | ( T_sum { fields = content1; layout = layout1 }
    , T_sum { fields = content2; layout = layout2 } )
  | ( T_record { fields = content1; layout = layout1 }
    , T_record { fields = content2; layout = layout2 } )
    when equal_domains content1 content2 ->
    (* if not (Layout.equal layout1 layout2)
    then raise.error (cannot_unify_diff_layout loc type1 type2 layout1 layout2); *)
    ignore (layout1, layout2);
    (* Naive unification. Layout and content must be consistent *)
    Record.LMap.fold
      (fun label
           ({ associated_type = type1; _ } : _ Rows.row_element_mini_c)
           ctx ->
        let ({ associated_type = type2; _ } : _ Rows.row_element_mini_c) =
          Record.LMap.find label content2
        in
        self ~ctx (Context.apply ctx type1) (Context.apply ctx type2))
      content1
      ctx
  | _ -> fail ()


let rec subtype
  ~raise
  ~loc
  ~ctx
  ~(received : type_expression)
  ~(expected : type_expression)
  : Context.t * (expression -> expression)
  =
  (* Format.printf "Subtype: %a, %a\n" PP.type_expression received PP.type_expression expected; *)
  let self ?(ctx = ctx) received expected =
    subtype ~raise ~loc ~ctx ~received ~expected
  in
  let subtype_evar ~mode evar type_ =
    let kind =
      Context.get_exists_var ctx evar
      |> trace_option ~raise (unbound_exists_variable loc evar)
    in
    let ctx, type_ = lift ~raise ~loc ~ctx ~mode ~evar ~kind type_ in
    occurs_check ~raise ~loc ~evar type_;
    Context.add_exists_eq ctx evar kind type_, fun x -> x
  in
  match received.type_content, expected.type_content with
  | ( T_arrow { type1 = type11; type2 = type12 }
    , T_arrow { type1 = type21; type2 = type22 } ) ->
    let ctx, f1 = self ~ctx type21 type11 in
    let ctx, f2 =
      self ~ctx (Context.apply ctx type12) (Context.apply ctx type22)
    in
    if type_expression_eq (Context.apply ctx type11, Context.apply ctx type21)
       && type_expression_eq (Context.apply ctx type12, Context.apply ctx type22)
    then ctx, fun hole -> hole
    else
      ( ctx
      , fun hole ->
          let x = Value_var.fresh ~name:"_sub" () in
          let args = f1 (e_variable x type21) in
          let binder = Param.make x type21 in
          let result = f2 (e_application { lamb = hole; args } type12) in
          e_a_lambda { binder; result; output_type = type22 } type21 type22 )
  | T_for_all { ty_binder = tvar; kind; type_ }, _ ->
    let evar = Exists_var.fresh ~loc () in
    let type' = t_subst type_ ~tvar ~type_:(t_exists ~loc evar) in
    let ctx, pos = Context.mark ctx ~mut:false in
    let ctx, f =
      self ~ctx:Context.(ctx |:: C_exists_var (evar, kind)) type' expected
    in
    ( Context.drop_until ctx ~pos
    , fun hole ->
        f (e_type_inst { forall = hole; type_ = t_exists ~loc evar } type') )
  | _, T_for_all { ty_binder = tvar; kind; type_ } ->
    let tvar' = Type_var.fresh_like ~loc tvar in
    let ctx, pos = Context.mark ctx ~mut:false in
    let ctx, f =
      self
        ~ctx:Context.(ctx |:: C_type_var (tvar', kind))
        received
        (t_subst_var ~loc type_ ~tvar ~tvar')
    in
    ( Context.drop_until ctx ~pos
    , fun hole ->
        e_type_abstraction { type_binder = tvar'; result = f hole } expected )
  | T_variable tvar1, T_variable tvar2 when Type_var.equal tvar1 tvar2 ->
    ctx, fun x -> x
  | T_variable tvar1, _ when Type_var.is_exists tvar1 ->
    subtype_evar ~mode:Contravariant (Exists_var.of_type_var_exn tvar1) expected
  | _, T_variable tvar2 when Type_var.is_exists tvar2 ->
    subtype_evar ~mode:Covariant (Exists_var.of_type_var_exn tvar2) received
  | _, _ -> unify ~raise ~loc ~ctx received expected, fun x -> x
