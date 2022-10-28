module Location = Simple_utils.Location
module Trace = Simple_utils.Trace
open Trace
open Errors
module List = Simple_utils.List
open Ligo_prim

module State = struct
  type t = Context.t * Substitution.t
end

type ('a, 'err, 'wrn) t =
  raise:('err, 'wrn) raise -> loc:Location.t -> State.t -> State.t * 'a

include Monad.Make3 (struct
  type nonrec ('a, 'err, 'wrn) t = ('a, 'err, 'wrn) t

  let return result ~raise:_ ~loc:_ state = state, result

  let bind t ~f ~raise ~loc state =
    let state, result = t ~raise ~loc state in
    f result ~raise ~loc state


  let map = `Define_using_bind
end)

type 'a with_loc = Location.t -> 'a

let all_lmap _ = assert false
let all_lmap_unit _ = assert false

let context () : (Context.t, _, _) t =
 fun ~raise:_ ~loc:_ (ctx, subst) -> (ctx, subst), ctx


let loc () : (Location.t, _, _) t = fun ~raise:_ ~loc state -> state, loc

let set_loc loc (in_ : ('a, 'err, 'wrn) t) : ('a, 'err, 'wrn) t =
 fun ~raise ~loc:_ state -> in_ ~raise ~loc state


let set_context ctx : (unit, _, _) t =
 fun ~raise:_ ~loc:_ (_ctx, subst) -> (ctx, subst), ()


let lift_raise f : _ t = fun ~raise ~loc:_ state -> state, f raise

let raise_result result ~error : _ t =
 fun ~raise ~loc state ->
  match result with
  | Ok result -> state, result
  | Error err -> raise.error (error err loc)


let raise_opt opt ~error : _ t =
 fun ~raise ~loc state -> state, trace_option ~raise (error loc) opt


let raise err : _ t = fun ~raise ~loc _state -> raise.error (err loc)
let raise_l ~loc err : _ t = fun ~raise ~loc:_ _state -> raise.error (err loc)

let warn wrn : _ t =
 fun ~raise ~loc state ->
  raise.warning (wrn loc);
  state, ()


let fresh_lexists () =
  let open Let_syntax in
  let%bind loc = loc () in
  let lvar = Layout_var.fresh ~loc () in
  return (lvar, Type.L_exists lvar)


let fresh_texists () =
  let open Let_syntax in
  let%bind loc = loc () in
  let tvar = Type_var.fresh ~loc () in
  return (tvar, Type.t_exists ~loc tvar ())


let fresh_type_var () =
  let open Let_syntax in
  let%bind loc = loc () in
  let tvar = Type_var.fresh ~loc () in
  return tvar


type (_, _) exit =
  | Drop : ('a, 'a) exit
  | Lift_type : (Type.t * 'a, Type.t * 'a) exit
  | Lift_sig : (Context.Signature.t * 'a, Context.Signature.t * 'a) exit

module Context_ = Context

module Context : sig
  module Signature = Context.Signature

  val lift_lvar
    :  at:Context.item
    -> lvar':Layout_var.t
    -> Type.layout
    -> (Type.layout, 'err, 'wrn) t

  val lift_tvar
    :  at:Context.item
    -> tvar':Type_var.t
    -> kind:Kind.t
    -> Type.t
    -> (Type.t, 'err, 'wrn) t

  val insert_at
    :  at:Context.item
    -> hole:Context.item list
    -> (unit, 'err, 'wrn) t

  val lock
    :  on_exit:('a, 'b) exit
    -> in_:('a, 'err, 'wrn) t
    -> ('b, 'err, 'wrn) t

  val add
    :  Context.item list
    -> on_exit:('a, 'b) exit
    -> in_:('a, 'err, 'wrn) t
    -> ('b, 'err, 'wrn) t

  val get_value
    :  Value_var.t
    -> error:([ `Mut_var_captured | `Not_found ] -> 'err with_loc)
    -> (Context.mutable_flag * Type.t, 'err, 'wrn) t

  val get_imm : Value_var.t -> error:'err with_loc -> (Type.t, 'err, 'wrn) t
  val get_mut : Value_var.t -> error:'err with_loc -> (Type.t, 'err, 'wrn) t
  val get_type_var : Type_var.t -> error:'err with_loc -> (Kind.t, 'err, 'wrn) t
  val get_type : Type_var.t -> error:'err with_loc -> (Type.t, 'err, 'wrn) t

  val get_module
    :  Module_var.t
    -> error:'err with_loc
    -> (Signature.t, 'err, 'wrn) t

  val get_signature
    :  Module_var.t List.Ne.t
    -> error:'err with_loc
    -> (Signature.t, 'err, 'wrn) t

  val get_texists_var
    :  Type_var.t
    -> error:'err with_loc
    -> (Kind.t, 'err, 'wrn) t

  val get_sum
    :  Label.t
    -> ((Type_var.t * Type_var.t list * Type.t * Type.t) list, 'err, 'wrn) t

  val get_record
    :  Type.row_element Record.t
    -> ((Type_var.t option * Type.row) option, 'err, 'wrn) t

  val add_texists_eq : Type_var.t -> Kind.t -> Type.t -> (unit, 'err, 'wrn) t
  val add_lexists_eq : Layout_var.t -> Type.layout -> (unit, 'err, 'wrn) t
  val push : Context.item list -> (unit, 'err, 'wrn) t
  val tapply : Type.t -> (Type.t, 'err, 'wrn) t

  module Well_formed : sig
    val context : unit -> (bool, 'err, 'wrn) t
    val type_ : Type.t -> (Kind.t option, 'err, 'wrn) t
  end
end = struct
  module Signature = Context.Signature

  let lift_var ~get_vars ~add_var ~add_eq ~at ~fresh ~var' t =
    let open Let_syntax in
    let%bind ctx = context () in
    let ctx1, ctx2 = Context.split_at ctx ~at in
    if not @@ Set.mem (get_vars ctx1) var'
    then (
      let%bind var'', t' = fresh () in
      let%bind () =
        set_context
        @@ Context.(add_var ctx1 var'' |:: at |@ add_eq ctx2 var' t')
      in
      return t')
    else return t


  let lift_lvar ~at ~lvar' layout =
    lift_var
      ~get_vars:Context.get_lexists_vars
      ~add_var:Context.add_lexists_var
      ~add_eq:Context.add_lexists_eq
      ~at
      ~fresh:fresh_lexists
      ~var':lvar'
      layout


  let lift_tvar ~at ~tvar' ~kind type_ =
    lift_var
      ~get_vars:Context.get_texists_vars
      ~add_var:(fun ctx tvar' -> Context.add_texists_var ctx tvar' kind)
      ~add_eq:(fun ctx tvar' -> Context.add_texists_eq ctx tvar' kind)
      ~at
      ~fresh:fresh_texists
      ~var':tvar'
      type_


  let insert_at ~at ~hole : _ t =
    let open Let_syntax in
    let%bind ctx = context () in
    set_context @@ Context.insert_at ctx ~at ~hole:(Context.of_list hole)


  let lock (type a b) ~(on_exit : (a, b) exit) ~(in_ : (a, _, _) t)
      : (b, _, _) t
    =
   fun ~raise ~loc (ctx, subst) ->
    let ctx, lock = Context.lock ctx in
    let (ctx, subst), result = in_ ~raise ~loc (ctx, subst) in
    let ctx, subst', (result : b) =
      match on_exit, result with
      | Drop, result ->
        let ctx, subst' = Context.unlock ctx ~on_exit:Drop ~lock in
        ctx, subst', result
      | Lift_type, (type_, result) ->
        let (ctx, type_), subst' =
          Context.unlock (ctx, type_) ~on_exit:(Lift Context.Apply.type_) ~lock
        in
        ctx, subst', (type_, result)
      | Lift_sig, (sig_, result) ->
        let (ctx, sig_), subst' =
          Context.unlock (ctx, sig_) ~on_exit:(Lift Context.Apply.sig_) ~lock
        in
        ctx, subst', (sig_, result)
    in
    let subst = Substitution.merge subst subst' in
    (ctx, subst), result


  let add (type a b) items ~(on_exit : (a, b) exit) ~(in_ : (a, _, _) t)
      : (b, _, _) t
    =
   fun ~raise ~loc (ctx, subst) ->
    let ctx, pos = Context.mark ctx in
    let ctx =
      List.fold_right items ~init:ctx ~f:(fun item ctx -> Context.add ctx item)
    in
    let (ctx, subst), result = in_ ~raise ~loc (ctx, subst) in
    let ctx, subst', (result : b) =
      match on_exit, result with
      | Drop, result ->
        let ctx, subst' = Context.drop_until ctx ~on_exit:Drop ~pos in
        ctx, subst', result
      | Lift_type, (type_, result) ->
        let (ctx, type_), subst' =
          Context.drop_until
            (ctx, type_)
            ~on_exit:(Lift Context.Apply.type_)
            ~pos
        in
        ctx, subst', (type_, result)
      | Lift_sig, (sig_, result) ->
        let (ctx, sig_), subst' =
          Context.drop_until (ctx, sig_) ~on_exit:(Lift Context.Apply.sig_) ~pos
        in
        ctx, subst', (sig_, result)
    in
    let subst = Substitution.merge subst subst' in
    (ctx, subst), result


  let push items : _ t =
   fun ~raise:_ ~loc:_ (ctx, subst) ->
    (Context.(ctx |@ of_list items), subst), ()


  let lift_ctx f : _ t =
    let open Let_syntax in
    let%map ctx = context () in
    f ctx


  let get_value var ~error : _ t =
    lift_ctx (fun ctx -> Context.get_value ctx var) >>= raise_result ~error


  let get_imm var ~error : _ t =
    lift_ctx (fun ctx -> Context.get_imm ctx var) >>= raise_opt ~error


  let get_mut var ~error : _ t =
    lift_ctx (fun ctx -> Context.get_mut ctx var) >>= raise_opt ~error


  let get_type_var tvar ~error =
    lift_ctx (fun ctx -> Context.get_type_var ctx tvar) >>= raise_opt ~error


  let get_type tvar ~error =
    lift_ctx (fun ctx -> Context.get_type ctx tvar) >>= raise_opt ~error


  let get_texists_var tvar ~error : _ t =
    lift_ctx (fun ctx -> Context.get_texists_var ctx tvar) >>= raise_opt ~error


  let get_signature path ~error : _ t =
    lift_ctx (fun ctx -> Context.get_signature ctx path) >>= raise_opt ~error


  let get_module mvar ~error : _ t =
    lift_ctx (fun ctx -> Context.get_module ctx mvar) >>= raise_opt ~error


  let get_sum constr : _ t = lift_ctx (fun ctx -> Context.get_sum ctx constr)

  let get_record fields : _ t =
    lift_ctx (fun ctx -> Context.get_record ctx fields)


  let add_texists_eq tvar kind type_ : _ t =
   fun ~raise:_ ~loc:_ (ctx, subst) ->
    (Context.add_texists_eq ctx tvar kind type_, subst), ()


  let add_lexists_eq lvar layout : _ t =
   fun ~raise:_ ~loc:_ (ctx, subst) ->
    (Context.add_lexists_eq ctx lvar layout, subst), ()


  module Apply = struct
    let type_ type' : _ t =
     fun ~raise:_ ~loc:_ (ctx, subst) ->
      (ctx, subst), Context.Apply.type_ ctx type'
  end

  module Well_formed = struct
    let type_ type_ =
      let open Let_syntax in
      let%map ctx = context () in
      Context.Well_formed.type_ ~ctx type_


    let context () =
      let open Let_syntax in
      let%map ctx = context () in
      Context.Well_formed.context ctx
  end

  let tapply = Apply.type_
end

let occurs_check ~tvar (type_ : Type.t) =
  let open Let_syntax in
  let%bind loc = loc () in
  lift_raise
  @@ fun raise ->
  let fail () = raise.error (occurs_check_failed tvar type_ loc) in
  let rec loop (type_ : Type.t) =
    match type_.content with
    | T_variable _tvar' -> ()
    | T_exists tvar' -> if Type_var.equal tvar tvar' then fail ()
    | T_arrow { type1; type2 } ->
      loop type1;
      loop type2
    | T_for_all { type_; _ } | T_abstraction { type_; _ } -> loop type_
    | T_construct { parameters; _ } -> List.iter parameters ~f:loop
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

let lift_layout ~at (layout : Type.layout) : (Type.layout, _, _) t =
  let open Let_syntax in
  match layout with
  | L_tree | L_comb -> return layout
  | L_exists lvar' -> Context.lift_lvar ~at ~lvar' layout


let rec lift ~(mode : Mode.t) ~kind ~tvar (type_ : Type.t) : (Type.t, _, _) t =
  let open Let_syntax in
  let lift ~mode type_ = lift ~mode ~kind ~tvar type_ in
  let lift_row row = lift_row ~kind ~tvar row in
  let const content = return { type_ with content } in
  match type_.content with
  | T_variable tvar' -> const @@ T_variable tvar'
  | T_exists tvar' ->
    Context.lift_tvar ~at:(C_texists_var (tvar, kind)) ~tvar' ~kind type_
  | T_for_all { ty_binder = tvar'; kind = kind'; type_ } ->
    (match mode with
    | Contravariant ->
      let%bind tvar', type' = fresh_texists () in
      let%bind () =
        Context.insert_at
          ~at:(C_texists_var (tvar, kind))
          ~hole:[ C_texists_var (tvar', kind'); C_texists_var (tvar, kind) ]
      in
      lift ~mode:Contravariant (Type.subst ~tvar:tvar' ~type_:type' type_)
    | Covariant ->
      Context.add
        [ C_type_var (tvar', kind') ]
        ~on_exit:Drop
        ~in_:(lift ~mode:Covariant type_)
    | Invariant ->
      Context.add
        [ C_type_var (tvar', kind') ]
        ~on_exit:Drop
        ~in_:
          (let%bind type_ = lift ~mode:Invariant type_ in
           const @@ T_for_all { ty_binder = tvar'; kind = kind'; type_ }))
  | T_abstraction { ty_binder = tvar'; kind; type_ } ->
    let%bind tvar'' = fresh_type_var () in
    let type_ = Type.subst_var type_ ~tvar:tvar' ~tvar':tvar'' in
    let%bind type_ =
      Context.add
        [ C_type_var (tvar'', kind) ]
        ~on_exit:Drop
        ~in_:(lift ~mode type_)
    in
    const @@ T_abstraction { ty_binder = tvar'; kind; type_ }
  | T_arrow { type1; type2 } ->
    let%bind type1 = lift ~mode:(Mode.invert mode) type1 in
    let%bind type2 = Context.tapply type2 >>= lift ~mode in
    const @@ T_arrow { type1; type2 }
  | T_sum row ->
    let%bind row = lift_row row in
    const @@ T_sum row
  | T_record row ->
    let%bind row = lift_row row in
    const @@ T_record row
  | T_construct construct ->
    let%bind parameters =
      construct.parameters
      |> List.map ~f:(fun param ->
             let%bind param = Context.tapply param in
             lift ~mode:Invariant param)
      |> all
    in
    const @@ T_construct { construct with parameters }
  | T_singleton _ -> return type_


and lift_row ~kind ~tvar ({ fields; layout } : Type.row) : (Type.row, _, _) t =
  let open Let_syntax in
  let%bind layout = lift_layout ~at:(C_texists_var (tvar, kind)) layout in
  let%bind fields =
    fields
    |> Record.map ~f:(fun (row_elem : Type.row_element) ->
           Context.tapply row_elem.associated_type
           >>= lift ~mode:Invariant ~kind ~tvar)
    |> all_lmap
  in
  return { Type.fields; layout }


let unify_texists tvar type_ =
  let open Let_syntax in
  let%bind () = occurs_check ~tvar type_ in
  let%bind kind =
    Context.get_texists_var tvar ~error:(unbound_texists_var tvar)
  in
  let%bind type_ = lift ~mode:Invariant ~tvar ~kind type_ in
  if%bind
    match%map Context.Well_formed.type_ type_ with
    | Some kind' -> Kind.equal kind kind'
    | _ -> false
  then Context.add_texists_eq tvar kind type_
  else raise_l ~loc:type_.location (ill_formed_type type_)


let unify_layout type1 type2 (layout1 : Type.layout) (layout2 : Type.layout) =
  let open Let_syntax in
  match layout1, layout2 with
  | L_comb, L_tree | L_tree, L_comb ->
    raise (cannot_unify_diff_layout type1 type2 layout1 layout2)
  | L_comb, L_comb | L_tree, L_tree -> return ()
  | L_exists lvar1, L_exists lvar2 when Layout_var.equal lvar1 lvar2 ->
    return ()
  | L_exists lvar, layout | layout, L_exists lvar ->
    let%bind layout = lift_layout ~at:(C_lexists_var lvar) layout in
    Context.add_lexists_eq lvar layout


let equal_domains lmap1 lmap2 =
  let open Record in
  (* One day this will be removed when we use [Core] maps *)
  LSet.(equal (of_list (LMap.keys lmap1)) (of_list (LMap.keys lmap2)))


type unify_error =
  [ `Typer_cannot_unify of Type.t * Type.t * Location.t
  | `Typer_cannot_unify_diff_layout of
    Type.t * Type.t * Type.layout * Type.layout * Location.t
  | `Typer_ill_formed_type of Type.t * Location.t
  | `Typer_occurs_check_failed of Type_var.t * Type.t * Location.t
  | `Typer_unbound_texists_var of Type_var.t * Location.t
  ]

let rec unify (type1 : Type.t) (type2 : Type.t) =
  let open Let_syntax in
  let unify_ type1 type2 =
    let%bind type1 = Context.tapply type1 in
    let%bind type2 = Context.tapply type2 in
    unify type1 type2
  in
  let fail () = raise (cannot_unify type1 type2) in
  match type1.content, type2.content with
  | T_singleton lit1, T_singleton lit2 when Literal_value.equal lit1 lit2 ->
    return ()
  | T_variable tvar1, T_variable tvar2 when Type_var.equal tvar1 tvar2 ->
    return ()
  | T_exists tvar1, T_exists tvar2 when Type_var.equal tvar1 tvar2 -> return ()
  | _, T_exists tvar2 -> unify_texists tvar2 type1
  | T_exists tvar1, _ -> unify_texists tvar1 type2
  | ( T_construct
        { language = lang1; constructor = constr1; parameters = params1 }
    , T_construct
        { language = lang2; constructor = constr2; parameters = params2 } )
    when String.(lang1 = lang2) && Literal_types.equal constr1 constr2 ->
    (match List.map2 params1 params2 ~f:unify_ with
    | Ok ts -> all_unit ts
    | Unequal_lengths -> raise (assert false))
  | ( T_arrow { type1 = type11; type2 = type12 }
    , T_arrow { type1 = type21; type2 = type22 } ) ->
    let%bind () = unify type11 type21 in
    unify_ type12 type22
  | ( T_for_all { ty_binder = tvar1; kind = kind1; type_ = type1 }
    , T_for_all { ty_binder = tvar2; kind = kind2; type_ = type2 } )
  | ( T_abstraction { ty_binder = tvar1; kind = kind1; type_ = type1 }
    , T_abstraction { ty_binder = tvar2; kind = kind2; type_ = type2 } )
    when Kind.equal kind1 kind2 ->
    let%bind tvar = fresh_type_var () in
    let type1 = Type.subst_var type1 ~tvar:tvar1 ~tvar':tvar in
    let type2 = Type.subst_var type2 ~tvar:tvar2 ~tvar':tvar in
    Context.add
      [ C_type_var (tvar, kind1) ]
      ~on_exit:Drop
      ~in_:(unify type1 type2)
  | ( T_sum { fields = fields1; layout = layout1 }
    , T_sum { fields = fields2; layout = layout2 } )
  | ( T_record { fields = fields1; layout = layout1 }
    , T_record { fields = fields2; layout = layout2 } )
    when equal_domains fields1 fields2 ->
    let%bind () = unify_layout type1 type2 layout1 layout2 in
    (* TODO: This should be replaced by [map2] or smth *)
    fields1
    |> Record.LMap.mapi (fun label (row_elem1 : Type.row_element) ->
           let row_elem2 = Record.LMap.find label fields2 in
           unify_ row_elem1.associated_type row_elem2.associated_type)
    |> all_lmap_unit
  | _ -> fail ()


type subtype_error = unify_error

module O = Ast_typed
module E = Elaboration

let rec subtype ~(received : Type.t) ~(expected : Type.t)
    : (O.expression -> O.expression Elaboration.t, _, _) t
  =
  let open Let_syntax in
  let subtype received expected = subtype ~received ~expected in
  let subtype_texists ~mode tvar type_ =
    let%bind () = occurs_check ~tvar type_ in
    let%bind kind =
      Context.get_texists_var tvar ~error:(unbound_texists_var tvar)
    in
    let%bind type_ = lift ~mode ~tvar ~kind type_ in
    let%bind () = Context.add_texists_eq tvar kind type_ in
    return E.return
  in
  let%bind loc = loc () in
  match received.content, expected.content with
  | ( T_arrow { type1 = type11; type2 = type12 }
    , T_arrow { type1 = type21; type2 = type22 } ) ->
    let%bind f1 = subtype type21 type11 in
    let%bind type12 = Context.tapply type12 in
    let%bind type22 = Context.tapply type22 in
    let%bind f2 = subtype type12 type22 in
    return
      E.(
        fun hole ->
          let%bind type11 = decode type11
          and type12 = decode type12
          and type21 = decode type21
          and type22 = decode type22 in
          if O.compare_type_expression type11 type21 = 0
             && O.compare_type_expression type12 type22 = 0
          then return hole
          else (
            let x = Value_var.fresh ~loc ~name:"_sub" () in
            let%bind arg = f1 (O.e_variable x type21) in
            let binder = Param.make x type21 in
            let%bind result = f2 (O.e_a_application hole arg type12) in
            return
            @@ O.e_a_lambda
                 { binder; result; output_type = type22 }
                 type21
                 type22))
  | T_for_all { ty_binder = tvar; kind; type_ }, _ ->
    let%bind tvar', texists = fresh_texists () in
    let type' = Type.subst type_ ~tvar ~type_:texists in
    let%bind f =
      Context.add
        [ C_texists_var (tvar', kind) ]
        ~on_exit:Drop
        ~in_:(subtype type' expected)
    in
    return
      E.(
        fun hole ->
          let%bind type' = decode type' in
          let%bind texists = decode texists in
          f (O.e_type_inst { forall = hole; type_ = texists } type'))
  | _, T_for_all { ty_binder = tvar; kind; type_ } ->
    let%bind tvar' = fresh_type_var () in
    let%bind f =
      Context.add
        [ C_type_var (tvar', kind) ]
        ~on_exit:Drop
        ~in_:(subtype received (Type.subst_var type_ ~tvar ~tvar'))
    in
    return
      E.(
        fun hole ->
          let%bind result = f hole in
          let%bind expected = decode expected in
          return
          @@ O.e_type_abstraction { type_binder = tvar'; result } expected)
  | (T_variable tvar1, T_variable tvar2 | T_exists tvar1, T_exists tvar2)
    when Type_var.equal tvar1 tvar2 -> return E.return
  | T_exists tvar1, _ -> subtype_texists ~mode:Contravariant tvar1 expected
  | _, T_exists tvar2 -> subtype_texists ~mode:Covariant tvar2 received
  | _, _ ->
    let%bind () = unify received expected in
    return E.return


let exists kind =
  let open Let_syntax in
  let%bind tvar, texists = fresh_texists () in
  let%bind () = Context.push [ C_texists_var (tvar, kind) ] in
  return texists


let for_all kind =
  let open Let_syntax in
  let%bind tvar = fresh_type_var () in
  let%bind () = Context.push [ C_type_var (tvar, kind) ] in
  return (Type.t_variable ~loc:(Type_var.get_location tvar) tvar ())


let def bindings ~on_exit ~in_ =
  Context.add
    (List.map bindings ~f:(fun (var, mut_flag, type_) ->
         Context_.C_value (var, mut_flag, type_)))
    ~in_
    ~on_exit


let def_module bindings ~on_exit ~in_ =
  Context.add
    (List.map bindings ~f:(fun (mvar, sig_) -> Context_.C_module (mvar, sig_)))
    ~on_exit
    ~in_


let def_type bindings ~on_exit ~in_ =
  Context.add
    (List.map bindings ~f:(fun (tvar, type_) -> Context_.C_type (tvar, type_)))
    ~in_
    ~on_exit


let def_type_var bindings ~on_exit ~in_ =
  Context.add
    (List.map bindings ~f:(fun (tvar, kind) -> Context_.C_type_var (tvar, kind)))
    ~in_
    ~on_exit


let def_sig_item sig_items ~on_exit ~in_ =
  Context.add
    (List.map sig_items ~f:Context_.item_of_signature_item)
    ~in_
    ~on_exit


let assert_ cond ~error =
  let open Let_syntax in
  if cond then return () else raise error


let hash_context () =
  let open Let_syntax in
  let%bind ctx = context () in
  Context_.Hashes.set_context ctx;
  return ()


let generalize _ = assert false

let create_type ?meta (constr : Type.constr) =
  let open Let_syntax in
  let%bind loc = loc () in
  return (constr ?meta ~loc ())


let try_ (body : ('a, 'err, 'wrn) t) ~(with_ : 'err -> ('a, 'err, 'wrn) t)
    : ('a, 'err, 'wrn) t
  =
 fun ~raise ~loc state ->
  Trace.try_with
    (fun ~raise ~catch:_ -> body ~raise ~loc state)
    (fun ~catch:_ err -> with_ err ~raise ~loc state)


module With_frag = struct
  type fragment = (Value_var.t * Param.mutable_flag * Type.t) list
  type nonrec ('a, 'err, 'wrn) t = (fragment * 'a, 'err, 'wrn) t

  let lift t = t >>| fun x -> [], x

  let lift_reader f t =
    let open Let_syntax in
    let%bind frag, x = t in
    let%bind y = f (return x) in
    return (frag, y)


  let extend frag = return (frag, ())
  let run t = t

  include Monad.Make3 (struct
    type nonrec ('a, 'err, 'wrn) t = ('a, 'err, 'wrn) t

    let return x = return ([], x)

    let bind t ~f =
      let open Let_syntax in
      let%bind frag1, x = t in
      let%bind frag2, y = f x in
      return (frag1 @ frag2, y)


    let map = `Define_using_bind
  end)

  let all_lmap _ = assert false
  let all_lmap_unit _ = assert false
  let loc () = lift (loc ())
  let set_loc loc = lift_reader (set_loc loc)
  let raise_result result ~error = lift (raise_result result ~error)
  let raise_opt opt ~error = lift (raise_opt opt ~error)
  let raise_l ~loc error = lift (raise_l ~loc error)
  let raise error = lift (raise error)
  let warn warning = lift (warn warning)
  let assert_ cond ~error = lift (assert_ ~error cond)
  let create_type ?meta type_ = lift (create_type ?meta type_)

  module Context = struct
    let get_sum label = lift (Context.get_sum label)
    let get_record row = lift (Context.get_record row)
    let tapply type_ = lift (Context.tapply type_)
  end

  let exists kind = lift (exists kind)
  let unify type1 type2 = lift (unify type1 type2)
  let subtype ~received ~expected = lift (subtype ~received ~expected)
end