open Core
open Ligo_prim
module Location = Simple_utils.Location
module Trace = Simple_utils.Trace
module Row = Type.Row

module State = struct
  type t = Context.t * Substitution.t
end

module T = struct
  type ('a, 'err, 'wrn) t =
    raise:('err, 'wrn) Trace.raise
    -> options:Compiler_options.middle_end
    -> loc:Location.t
    -> path:Module_var.t list
    -> poly_name_tbl:Type.Type_var_name_tbl.t
    -> refs_tbl:Context.Refs_tbl.t
    -> State.t
    -> State.t * 'a

  let return result ~raise:_ ~options:_ ~loc:_ ~path:_ ~poly_name_tbl:_ ~refs_tbl:_ state =
    state, result


  let bind t ~f ~raise ~options ~loc ~path ~poly_name_tbl ~refs_tbl state =
    let state, result = t ~raise ~options ~loc ~path ~poly_name_tbl ~refs_tbl state in
    f result ~raise ~options ~loc ~path ~poly_name_tbl ~refs_tbl state


  let map = `Define_using_bind
end

include T
include Monad.Make3 (T)

let rec encode ~(raise : _ Trace.raise) (type_ : Ast_typed.type_expression) : Type.t =
  let encode = encode ~raise in
  let return content : Type.t =
    { content
    ; abbrev =
        Option.map type_.abbrev ~f:(fun { orig_var; applied_types } ->
            Type.{ orig_var; applied_types = List.map ~f:encode applied_types })
    ; location = type_.location
    }
  in
  match type_.type_content with
  | T_variable tvar -> return @@ T_variable tvar
  | T_exists tvar ->
    let () = raise.log_error @@ Errors.cannot_encode_texists tvar type_.location in
    return @@ T_exists tvar
  | T_arrow arr ->
    let arr = Arrow.map encode arr in
    return @@ T_arrow arr
  | T_singleton lit -> return @@ T_singleton lit
  | T_abstraction abs ->
    let abs = Abstraction.map encode abs in
    return @@ T_abstraction abs
  | T_for_all abs ->
    let abs = Abstraction.map encode abs in
    return @@ T_for_all abs
  | T_constant { language; injection; parameters } ->
    let parameters = List.map parameters ~f:encode in
    return @@ T_construct { language; constructor = injection; parameters }
  | T_sum row ->
    let row = encode_row ~raise row in
    return @@ T_sum row
  | T_union union ->
    let union = Union.map encode union in
    return @@ T_union union
  | T_record row ->
    let row = encode_row ~raise row in
    return @@ T_record row


and encode_row ~raise ({ fields; layout } : Ast_typed.row) : Type.row =
  let fields = Map.map ~f:(encode ~raise) fields in
  let layout = encode_layout layout in
  Row.{ fields; layout }


and encode_layout (layout : Layout.t) : Type.layout = L_concrete layout

and encode_sig_item ~raise (item : Ast_typed.sig_item)
    : Context.Signature.item Location.wrap
  =
  Location.wrap ~loc:(Location.get_location item)
  @@
  match Location.unwrap item with
  | Ast_typed.S_value (v, ty, attr) ->
    Context.Signature.S_value (v, encode ~raise ty, encode_sig_item_attribute attr)
  | S_type (v, ty, attr) ->
    Context.Signature.S_type
      ( v
      , encode ~raise ty
      , { Context.Attrs.Type.default with leading_comments = attr.leading_comments } )
  | S_type_var (v, attr) ->
    Context.Signature.S_type_var
      (v, { Context.Attrs.Type.default with leading_comments = attr.leading_comments })
  | S_module (v, sig_) ->
    Context.Signature.S_module
      (v, encode_signature ~raise sig_, Context.Attrs.Module.default)
  | S_module_type (v, sig_) ->
    Context.Signature.S_module_type
      (v, encode_signature ~raise sig_, Context.Attrs.Signature.default)


and encode_sig_sort ~raise (sort : Ast_typed.signature_sort) : Context.Signature.sort =
  match sort with
  | Ss_module -> Ss_module
  | Ss_contract { storage; parameter } ->
    Ss_contract { storage = encode ~raise storage; parameter = encode ~raise parameter }


and encode_signature ~raise (sig_ : Ast_typed.signature) : Context.Signature.t =
  { items = List.map ~f:(encode_sig_item ~raise) sig_.sig_items
  ; sort = encode_sig_sort ~raise sig_.sig_sort
  }


and encode_sig_item_attribute (attr : Sig_item_attr.t) : Context.Attrs.Value.t =
  { view = attr.view
  ; entry = attr.entry
  ; dyn_entry = attr.dyn_entry
  ; public = true
  ; optional = attr.optional
  ; leading_comments = attr.leading_comments
  }


(* Load context from the outside declarations *)
let ctx_init_of_env ~raise ?env () =
  match env with
  | None -> Context.empty
  | Some (env : Persistent_env.t) ->
    let add_items ctx decl =
      match Location.unwrap decl with
      | Ast_typed.S_value (v, ty, _attr) -> Context.add_imm ctx v (encode ~raise ty)
      | S_type (v, ty, _) -> Context.add_type ctx v (encode ~raise ty)
      | S_type_var (v, _) -> Context.add_type_var ctx v Kind.Type
      | S_module (v, sig_) -> Context.add_module ctx v (encode_signature ~raise sig_)
      | S_module_type (v, sig_) ->
        Context.add_module_type ctx v (encode_signature ~raise sig_)
    in
    let add_module ctx (mv, sig_) =
      (* envs should be modules not contracts *)
      (* assert ( *)
      (*   match sig_.Ast_typed.sig_sort with *)
      (*   | Ss_contract _ -> false *)
      (*   | Ss_module -> true); *)
      Context.add_module ctx mv (encode_signature ~raise sig_)
    in
    let env_items, modules = Persistent_env.get_signatures env in
    let init = List.fold modules ~init:Context.empty ~f:add_module in
    List.fold env_items ~init ~f:add_items


let run_elab_with_refs t ~raise ~options ~loc ~path ~refs_tbl ?env () =
  let ctx = ctx_init_of_env ~raise ?env () in
  (* Format.printf "@[Context:@.%a@]" Context.pp ctx; *)
  let ctx, pos = Context.mark ctx in
  let poly_name_tbl = Type.Type_var_name_tbl.create () in
  let (ctx, subst), elab =
    t ~raise ~options ~loc ~path ~poly_name_tbl ~refs_tbl (ctx, Substitution.empty)
  in
  (* Drop to get any remaining equations that relate to elaborated thing *)
  let ctx, subst' = Context.drop_until ctx ~pos ~on_exit:Drop in
  Elaboration.run elab ~path ~raise ~options (Substitution.merge subst subst')


let run_elab t ~raise ~options ~loc ~path ?env () =
  run_elab_with_refs
    t
    ~raise
    ~options
    ~loc
    ~path
    ~refs_tbl:(Context.Refs_tbl.create ())
    ?env
    ()


let lift_elab t ~raise ~options ~loc:_ ~path ~poly_name_tbl:_ ~refs_tbl:_ st =
  st, Elaboration.run t ~options ~path ~raise (Tuple2.get2 st)


module Make_all (T : sig
  type 'a t

  val fold_map : ('acc -> 'a1 -> 'acc * 'a2) -> 'acc -> 'a1 t -> 'acc * 'a2 t
end) =
struct
  let all (t : ('a, 'err, 'wrn) t T.t) : ('a T.t, 'err, 'wrn) t =
   fun ~raise ~options ~loc ~path ~poly_name_tbl ~refs_tbl state ->
    let f state (x : ('a, 'err, 'wrn) t) =
      x ~raise ~options ~loc ~path ~poly_name_tbl ~refs_tbl state
    in
    T.fold_map f state t
end

let try_ (body : ('a, 'err, 'wrn) t) ~(with_ : 'err list -> ('a, 'err, 'wrn) t)
    : ('a, 'err, 'wrn) t
  =
 fun ~raise ~options ~loc ~path ~poly_name_tbl ~refs_tbl state ->
  let body = body ~options ~loc ~path ~poly_name_tbl ~refs_tbl state in
  match
    if options.typer_error_recovery
    then Trace.to_stdlib_result ~fast_fail:No_fast_fail body
    else Trace.cast_fast_fail_result @@ Trace.to_stdlib_result ~fast_fail:Fast_fail body
  with
  | Ok (result, _es, _ws) -> result
  | Error (es, _ws) -> with_ es ~raise ~options ~loc ~path ~poly_name_tbl ~refs_tbl state


let try_with_diagnostics
    (body : ('a, 'err, 'wrn) t)
    ~(with_ : ('a, 'err, 'wrn) t)
    ~(diagnostics : 'err list -> 'wrn list -> (unit, 'err, 'wrn) t)
    : ('a, 'err, 'wrn) t
  =
 fun ~raise ~options ~loc ~path ~poly_name_tbl ~refs_tbl state ->
  let body = body ~options ~loc ~path ~poly_name_tbl ~refs_tbl state in
  let (state, result), es, ws =
    match
      if options.typer_error_recovery
      then Trace.to_stdlib_result ~fast_fail:No_fast_fail body
      else Trace.cast_fast_fail_result @@ Trace.to_stdlib_result ~fast_fail:Fast_fail body
    with
    | Ok (result, es, ws) -> result, es, ws
    | Error (es, ws) ->
      let result = with_ ~raise ~options ~loc ~path ~poly_name_tbl ~refs_tbl state in
      result, es, ws
  in
  let state, () =
    diagnostics es ws ~raise ~options ~loc ~path ~poly_name_tbl ~refs_tbl state
  in
  state, result


let try_both t1 t2 ~raise ~options ~loc ~path ~poly_name_tbl ~refs_tbl state =
  let with_args t ~raise = t ~raise ~options ~loc ~path ~poly_name_tbl ~refs_tbl state in
  Trace.bind_or ~raise (with_args t1) (with_args t2)


let try_all (ts : ('a, 'err, 'wrn) t Nonempty_list.t) : ('a, 'err, 'wrn) t =
 fun ~raise ~options ~loc ~path ~poly_name_tbl ~refs_tbl state ->
  Trace.bind_exists
    ~raise
    (Nonempty_list.map
       ~f:(fun t ~raise -> t ~raise ~options ~loc ~path ~poly_name_tbl ~refs_tbl state)
       ts)


let all_lmap (lmap : ('a, 'err, 'wrn) t Label.Map.t) : ('a Label.Map.t, 'err, 'wrn) t =
 fun ~raise ~options ~loc ~path ~poly_name_tbl ~refs_tbl state ->
  Label.Map.fold_map lmap ~init:state ~f:(fun ~key:_label ~data:t state ->
      t ~raise ~options ~loc ~path ~poly_name_tbl ~refs_tbl state)


let all_lmap_unit (lmap : (unit, 'err, 'wrn) t Label.Map.t) : (unit, 'err, 'wrn) t =
 fun ~raise ~options ~loc ~path ~poly_name_tbl ~refs_tbl state ->
  let state =
    Map.fold
      ~f:(fun ~key:_label ~data:t state ->
        let state, () = t ~raise ~options ~loc ~path ~poly_name_tbl ~refs_tbl state in
        state)
      lmap
      ~init:state
  in
  state, ()


let context () : (Context.t, _, _) t =
 fun ~raise:_ ~options:_ ~loc:_ ~path:_ ~poly_name_tbl:_ ~refs_tbl:_ (ctx, subst) ->
  (ctx, subst), ctx


let options () : (Compiler_options.middle_end, _, _) t =
 fun ~raise:_ ~options ~loc:_ ~path:_ ~poly_name_tbl:_ ~refs_tbl:_ state -> state, options


let loc () : (Location.t, _, _) t =
 fun ~raise:_ ~options:_ ~loc ~path:_ ~poly_name_tbl:_ ~refs_tbl:_ state -> state, loc


let path () : (Module_var.t list, _, _) t =
 fun ~raise:_ ~options:_ ~loc:_ ~path ~poly_name_tbl:_ ~refs_tbl:_ state -> state, path


let poly_name_tbl () : (Type.Type_var_name_tbl.t, _, _) t =
 fun ~raise:_ ~options:_ ~loc:_ ~path:_ ~poly_name_tbl ~refs_tbl:_ state ->
  state, poly_name_tbl


let refs_tbl () : (Context.Refs_tbl.t, _, _) t =
 fun ~raise:_ ~options:_ ~loc:_ ~path:_ ~poly_name_tbl:_ ~refs_tbl state ->
  state, refs_tbl


let set_loc loc (in_ : ('a, 'err, 'wrn) t) : ('a, 'err, 'wrn) t =
 fun ~raise ~options ~loc:_ ~path ~poly_name_tbl ~refs_tbl state ->
  in_ ~raise ~options ~loc ~path ~poly_name_tbl ~refs_tbl state


let set_path path (in_ : ('a, 'err, 'wrn) t) : ('a, 'err, 'wrn) t =
 fun ~raise ~options ~loc ~path:_ ~poly_name_tbl ~refs_tbl state ->
  in_ ~raise ~options ~loc ~path ~poly_name_tbl ~refs_tbl state


let set_poly_name_tbl poly_name_tbl (in_ : ('a, 'err, 'wrn) t) : ('a, 'err, 'wrn) t =
 fun ~raise ~options ~loc ~path ~poly_name_tbl:_ ~refs_tbl state ->
  in_ ~raise ~options ~loc ~path ~poly_name_tbl ~refs_tbl state


let set_refs_tbl refs_tbl (in_ : ('a, 'err, 'wrn) t) : ('a, 'err, 'wrn) t =
 fun ~raise ~options ~loc ~path ~poly_name_tbl ~refs_tbl:_ state ->
  in_ ~raise ~options ~loc ~path ~poly_name_tbl ~refs_tbl state


let set_context ctx : (unit, _, _) t =
 fun ~raise:_ ~options:_ ~loc:_ ~path:_ ~poly_name_tbl:_ ~refs_tbl:_ (_ctx, subst) ->
  (ctx, subst), ()


let lift_raise f : _ t =
 fun ~raise ~options:_ ~loc:_ ~path:_ ~poly_name_tbl:_ ~refs_tbl:_ state -> state, f raise


let raise_result result ~error : _ t =
 fun ~raise ~options:_ ~loc ~path:_ ~poly_name_tbl:_ ~refs_tbl:_ state ->
  match result with
  | Ok result -> state, result
  | Error err -> raise.error (error err loc)


let raise_opt opt ~error : _ t =
 fun ~raise ~options:_ ~loc ~path:_ ~poly_name_tbl:_ ~refs_tbl:_ state ->
  state, Trace.trace_option ~raise (error loc) opt


let raise err : _ t =
 fun ~raise ~options:_ ~loc ~path:_ ~poly_name_tbl:_ ~refs_tbl:_ _state ->
  raise.error (err loc)


let log_error err : _ t =
 fun ~raise ~options:_ ~loc ~path:_ ~poly_name_tbl:_ ~refs_tbl:_ state ->
  state, raise.log_error (err loc)


let raise_l ~loc err : _ t =
 fun ~raise ~options:_ ~loc:_ ~path:_ ~poly_name_tbl:_ ~refs_tbl:_ _state ->
  raise.error (err loc)


let log_error_l ~loc err : _ t =
 fun ~raise ~options:_ ~loc:_ ~path:_ ~poly_name_tbl:_ ~refs_tbl:_ state ->
  state, raise.log_error (err loc)


let warn wrn : _ t =
 fun ~raise ~options:_ ~loc ~path:_ ~poly_name_tbl:_ ~refs_tbl:_ state ->
  raise.warning (wrn loc);
  state, ()


let fresh_lexists () =
  let open Let_syntax in
  let%bind loc = loc () in
  let lvar = Layout_var.fresh ~loc () in
  return (lvar, Type.Layout.L_exists lvar)


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


module Options = struct
  let test () =
    let open Let_syntax in
    let%map options = options () in
    options.test


  let syntax () =
    let open Let_syntax in
    let%map options = options () in
    options.syntax_for_errors


  let no_color () =
    let open Let_syntax in
    let%map options = options () in
    options.no_colour


  let array_as_list () =
    let open Let_syntax in
    let%map options = options () in
    options.array_as_list
end

type 'a exit =
  | Drop : 'a exit
  | Lift_type : (Type.t * 'a) exit
  | Lift_sig : (Context.Signature.t * 'a) exit

module Context_ = Context

module Context = struct
  module Attr = Context.Attrs.Value
  module Signature = Context.Signature

  let lift_var ~get_vars ~add_var ~add_eq ~at ~fresh ~var' t =
    let open Let_syntax in
    let%bind ctx = context () in
    let ctx1, ctx2 = Context.split_at ctx ~at in
    if not @@ Set.mem (get_vars ctx1) var'
    then (
      let%bind var'', t' = fresh () in
      let%bind () =
        set_context @@ Context.(add_var ctx1 var'' |:: at |@ add_eq ctx2 var' t')
      in
      return t')
    else return t


  let lift_lvar ~at ~lvar' ~fields layout =
    lift_var
      ~get_vars:Context.get_lexists_vars
      ~add_var:(fun ctx lvar' -> Context.add_lexists_var ctx lvar' fields)
      ~add_eq:(fun ctx lvar' layout -> Context.add_lexists_eq ctx lvar' fields layout)
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


  let lock (type a) ~(on_exit : a exit) ~(in_ : (a, _, _) t) : (a, _, _) t =
   fun ~raise ~options ~loc ~path ~poly_name_tbl ~refs_tbl (ctx, subst) ->
    let ctx, lock = Context.lock ctx in
    let (ctx, subst), result =
      in_ ~raise ~options ~loc ~path ~poly_name_tbl ~refs_tbl (ctx, subst)
    in
    let ctx, subst', (result : a) =
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


  let add (type a) items ~(on_exit : a exit) ~(in_ : (a, _, _) t) : (a, _, _) t =
   fun ~raise ~options ~loc ~path ~poly_name_tbl ~refs_tbl (ctx, subst) ->
    let ctx, pos = Context.mark ctx in
    let ctx = List.fold_right items ~init:ctx ~f:(fun item ctx -> Context.add ctx item) in
    let (ctx, subst), result =
      in_ ~raise ~options ~loc ~path ~poly_name_tbl ~refs_tbl (ctx, subst)
    in
    let ctx, subst', (result : a) =
      match on_exit, result with
      | Drop, result ->
        let ctx, subst' = Context.drop_until ctx ~on_exit:Drop ~pos in
        ctx, subst', result
      | Lift_type, (type_, result) ->
        let (ctx, type_), subst' =
          Context.drop_until (ctx, type_) ~on_exit:(Lift Context.Apply.type_) ~pos
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
   fun ~raise:_ ~options:_ ~loc:_ ~path:_ ~poly_name_tbl:_ ~refs_tbl:_ (ctx, subst) ->
    (Context.(ctx |@ of_list items), subst), ()


  let lift_ctx f : _ t =
    let open Let_syntax in
    let%bind refs_tbl = refs_tbl () in
    let%map ctx = context () in
    f ctx ~refs_tbl


  let get_value var : _ t =
    lift_ctx (fun ctx ~refs_tbl -> Context.get_value ~refs_tbl ctx var)


  let get_imm var : _ t =
    lift_ctx (fun ctx ~refs_tbl -> Context.get_imm ~refs_tbl ctx var)


  let get_mut var : _ t =
    lift_ctx (fun ctx ~refs_tbl -> Context.get_mut ~refs_tbl ctx var)


  let get_type_var tvar : _ t =
    lift_ctx (fun ctx ~refs_tbl -> Context.get_type_var ~refs_tbl ctx tvar)


  let get_type tvar : _ t =
    lift_ctx (fun ctx ~refs_tbl -> Context.get_type ~refs_tbl ctx tvar)


  let get_type_or_type_var tvar : _ t =
    lift_ctx (fun ctx ~refs_tbl -> Context.get_type_or_type_var ~refs_tbl ctx tvar)


  let get_texists_var tvar ~error : _ t =
    lift_ctx (fun ctx ~refs_tbl:_ -> Context.get_texists_var ctx tvar)
    >>= raise_opt ~error


  let get_module_of_path path : _ t =
    lift_ctx (fun ctx ~refs_tbl -> Context.get_module_of_path ~refs_tbl ctx path)


  let get_module_type_of_path path : _ t =
    lift_ctx (fun ctx ~refs_tbl -> Context.get_module_type_of_path ~refs_tbl ctx path)


  let get_module mvar : _ t =
    lift_ctx (fun ctx ~refs_tbl -> Context.get_module ~refs_tbl ctx mvar)


  let get_sum constr : _ t =
    lift_ctx (fun ctx ~refs_tbl -> Context.get_sum ~refs_tbl ctx constr)


  let get_record fields : _ t =
    lift_ctx (fun ctx ~refs_tbl:_ -> Context.get_record ctx fields)


  let add_texists_eq tvar kind type_ : _ t =
   fun ~raise:_ ~options:_ ~loc:_ ~path:_ ~poly_name_tbl:_ ~refs_tbl:_ (ctx, subst) ->
    (Context.add_texists_eq ctx tvar kind type_, subst), ()


  let add_lexists_eq lvar fields layout : _ t =
   fun ~raise:_ ~options:_ ~loc:_ ~path:_ ~poly_name_tbl:_ ~refs_tbl:_ (ctx, subst) ->
    (Context.add_lexists_eq ctx lvar fields layout, subst), ()


  module Apply = struct
    let type_ type' : _ t =
     fun ~raise:_ ~options:_ ~loc:_ ~path:_ ~poly_name_tbl:_ ~refs_tbl:_ (ctx, subst) ->
      (ctx, subst), Context.Apply.type_ ctx type'
  end

  module Well_formed = struct
    let type_ type_ =
      let open Let_syntax in
      let%bind refs_tbl = refs_tbl () in
      let%map ctx = context () in
      Context.Well_formed.type_ ~ctx ~refs_tbl type_


    let context () =
      let open Let_syntax in
      let%bind refs_tbl = refs_tbl () in
      let%map ctx = context () in
      Context.Well_formed.context ~refs_tbl ctx
  end

  let tapply = Apply.type_

  (* Useful debugging function :) *)
  let[@warning "-32"] trace fn in_ =
    let open Let_syntax in
    let%bind ctx_before = context () in
    let%bind result = in_ in
    let%bind ctx_after = context () in
    fn (fun ppf () -> Context_.Diff.pp ppf (ctx_before, ctx_after));
    return result
end

let occurs_check ~tvar (type_ : Type.t) =
  let open Let_syntax in
  let%bind loc = loc () in
  lift_raise
  @@ fun raise ->
  let fail () = raise.log_error (Errors.occurs_check_failed tvar type_ loc) in
  let rec loop (type_ : Type.t) =
    match type_.content with
    | T_variable _tvar' -> ()
    | T_exists tvar' -> if Type_var.equal tvar tvar' then fail ()
    | T_arrow { type1; type2; param_names = _ } ->
      loop type1;
      loop type2
    | T_for_all { type_; _ } | T_abstraction { type_; _ } -> loop type_
    | T_construct { parameters; _ } -> List.iter parameters ~f:loop
    | T_record row | T_sum row -> Map.iter row.fields ~f:loop
    | T_union union -> Union.iter loop union
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

let lift_layout ~at ~fields (layout : Type.layout) : (Type.layout, _, _) t =
  let open Let_syntax in
  match layout with
  | L_concrete _ -> return layout
  | L_exists lvar' -> Context.lift_lvar ~at ~lvar' ~fields layout


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
      Context.add [ C_type_var (tvar'', kind) ] ~on_exit:Drop ~in_:(lift ~mode type_)
    in
    const @@ T_abstraction { ty_binder = tvar'; kind; type_ }
  | T_arrow { type1; type2; param_names } ->
    let%bind type1 = lift ~mode:(Mode.invert mode) type1 in
    let%bind type2 = Context.tapply type2 >>= lift ~mode in
    const @@ T_arrow { type1; type2; param_names }
  | T_sum row ->
    let%bind row = lift_row row in
    const @@ T_sum row
  | T_union union ->
    let module Comp_union = Make_all (Union) in
    let%bind union =
      union |> Union.map (fun ty -> Context.tapply ty >>= lift ~mode) |> Comp_union.all
    in
    const @@ T_union union
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
  let%bind layout =
    lift_layout ~at:(C_texists_var (tvar, kind)) ~fields:(Map.key_set fields) layout
  in
  let%bind fields =
    fields
    |> Map.map ~f:(fun row_elem ->
           Context.tapply row_elem >>= lift ~mode:Invariant ~kind ~tvar)
    |> all_lmap
  in
  return { Type.Row.fields; layout }


let unify_texists tvar type_ =
  let open Let_syntax in
  let%bind () = occurs_check ~tvar type_ in
  let%bind kind = Context.get_texists_var tvar ~error:(Errors.unbound_texists_var tvar) in
  let%bind type_ = lift ~mode:Invariant ~tvar ~kind type_ in
  if%bind
    match%map Context.Well_formed.type_ type_ with
    | Some kind' -> Kind.equal kind kind'
    | _ -> false
  then Context.add_texists_eq tvar kind type_
  else raise_l ~loc:type_.location (Errors.ill_formed_type type_)


let unify_layout type1 type2 ~fields (layout1 : Type.layout) (layout2 : Type.layout) =
  let open Let_syntax in
  match layout1, layout2 with
  | L_concrete layout1, L_concrete layout2 when Layout.equal layout1 layout2 -> return ()
  | L_concrete _, L_concrete _ ->
    log_error (Errors.cannot_unify_local_diff_layout type1 type2 layout1 layout2)
  | L_exists lvar1, L_exists lvar2 when Layout_var.equal lvar1 lvar2 -> return ()
  | L_exists lvar, layout | layout, L_exists lvar ->
    let%bind layout = lift_layout ~at:(C_lexists_var (lvar, fields)) ~fields layout in
    Context.add_lexists_eq lvar fields layout


let equal_domains lmap1 lmap2 = Set.equal (Map.key_set lmap1) (Map.key_set lmap2)

let rec unify_aux (type1 : Type.t) (type2 : Type.t)
    : (unit, Errors.local_unify_error, 'wrn) t
  =
  let open Let_syntax in
  let unify_ type1 type2 =
    let%bind type1 = Context.tapply type1 in
    let%bind type2 = Context.tapply type2 in
    unify_aux type1 type2
  in
  let fail () =
    let%bind no_color = Options.no_color () in
    log_error (Errors.cannot_unify_local no_color type1 type2)
  in
  match type1.content, type2.content with
  | T_singleton lit1, T_singleton lit2 when Literal_value.equal lit1 lit2 -> return ()
  | T_variable tvar1, T_variable tvar2 when Type_var.equal tvar1 tvar2 -> return ()
  | T_exists tvar1, T_exists tvar2 when Type_var.equal tvar1 tvar2 -> return ()
  | _, T_exists tvar2 -> unify_texists tvar2 type1
  | T_exists tvar1, _ -> unify_texists tvar1 type2
  | ( T_construct { language = lang1; constructor = constr1; parameters = params1 }
    , T_construct { language = lang2; constructor = constr2; parameters = params2 } )
    when String.(lang1 = lang2) && Literal_types.equal constr1 constr2 ->
    (match List.map2 params1 params2 ~f:unify_ with
    | Ok ts -> all_unit ts
    | Unequal_lengths -> log_error (assert false))
  | ( T_arrow { type1 = type11; type2 = type12; param_names = _ }
    , T_arrow { type1 = type21; type2 = type22; param_names = _ } ) ->
    let%bind () = unify_aux type11 type21 in
    unify_ type12 type22
  | ( T_for_all { ty_binder = tvar1; kind = kind1; type_ = type1 }
    , T_for_all { ty_binder = tvar2; kind = kind2; type_ = type2 } )
  | ( T_abstraction { ty_binder = tvar1; kind = kind1; type_ = type1 }
    , T_abstraction { ty_binder = tvar2; kind = kind2; type_ = type2 } )
    when Kind.equal kind1 kind2 ->
    let%bind tvar = fresh_type_var () in
    let type1 = Type.subst_var type1 ~tvar:tvar1 ~tvar':tvar in
    let type2 = Type.subst_var type2 ~tvar:tvar2 ~tvar':tvar in
    Context.add [ C_type_var (tvar, kind1) ] ~on_exit:Drop ~in_:(unify_aux type1 type2)
  | ( T_sum { fields = fields1; layout = layout1 }
    , T_sum { fields = fields2; layout = layout2 } )
  | ( T_record { fields = fields1; layout = layout1 }
    , T_record { fields = fields2; layout = layout2 } )
    when equal_domains fields1 fields2 ->
    (* Invariant [Map.key_set fields1 = Map.key_set fields2] *)
    let%bind () =
      unify_layout type1 type2 ~fields:(Map.key_set fields1) layout1 layout2
    in
    (* TODO: This should be replaced by [map2] or smth *)
    fields1
    |> Map.mapi ~f:(fun ~key:label ~data:row_elem1 ->
           let row_elem2 = Map.find_exn fields2 label in
           unify_ row_elem1 row_elem2)
    |> all_lmap_unit
  | _ -> fail ()


let unify (type1 : Type.t) (type2 : Type.t) : (unit, [> Errors.unify_error ], 'wrn) t =
  let open Let_syntax in
  let%bind unification_loc = loc () in
  Trace.map_error
    ~f:(fun err -> Errors.cannot_unify err type1 type2 unification_loc)
    (unify_aux type1 type2)


let rec eq (type1 : Type.t) (type2 : Type.t) =
  let open Let_syntax in
  let eq_ type1 type2 =
    let%bind type1 = Context.tapply type1 in
    let%bind type2 = Context.tapply type2 in
    eq type1 type2
  in
  match type1.content, type2.content with
  | T_singleton lit1, T_singleton lit2 when Literal_value.equal lit1 lit2 -> return true
  | T_variable tvar1, T_variable tvar2 when Type_var.equal tvar1 tvar2 -> return true
  | ( T_construct { language = lang1; constructor = constr1; parameters = params1 }
    , T_construct { language = lang2; constructor = constr2; parameters = params2 } )
    when String.(lang1 = lang2) && Literal_types.equal constr1 constr2 ->
    (match List.map2 params1 params2 ~f:eq_ with
    | Ok ts ->
      let%bind ts = all ts in
      return (List.for_all ~f:Fn.id ts)
    | Unequal_lengths -> raise (assert false))
  | ( T_arrow { type1 = type11; type2 = type12; param_names = _ }
    , T_arrow { type1 = type21; type2 = type22; param_names = _ } ) ->
    let%bind b1 = eq type11 type21 in
    let%bind b2 = eq_ type12 type22 in
    return (b1 && b2)
  | ( T_for_all { ty_binder = tvar1; kind = kind1; type_ = type1 }
    , T_for_all { ty_binder = tvar2; kind = kind2; type_ = type2 } )
  | ( T_abstraction { ty_binder = tvar1; kind = kind1; type_ = type1 }
    , T_abstraction { ty_binder = tvar2; kind = kind2; type_ = type2 } )
    when Kind.equal kind1 kind2 ->
    let%bind tvar = fresh_type_var () in
    let type1 = Type.subst_var type1 ~tvar:tvar1 ~tvar':tvar in
    let type2 = Type.subst_var type2 ~tvar:tvar2 ~tvar':tvar in
    Context.add [ C_type_var (tvar, kind1) ] ~on_exit:Drop ~in_:(eq type1 type2)
  | ( T_sum { fields = fields1; layout = layout1 }
    , T_sum { fields = fields2; layout = layout2 } )
  | ( T_record { fields = fields1; layout = layout1 }
    , T_record { fields = fields2; layout = layout2 } )
    when equal_domains fields1 fields2 ->
    (* Invariant [Map.key_set fields1 = Map.key_set fields2] *)
    let%bind () =
      unify_layout type1 type2 ~fields:(Map.key_set fields1) layout1 layout2
    in
    (* TODO: This should be replaced by [map2] or smth *)
    let%bind bs =
      fields1
      |> Map.mapi ~f:(fun ~key:label ~data:row_elem1 ->
             let row_elem2 = Map.find_exn fields2 label in
             eq_ row_elem1 row_elem2)
      |> all_lmap
    in
    return (List.for_all ~f:Fn.id @@ Core.Map.data bs)
  | _ -> return false


module O = Ast_typed
module E = Elaboration

let rec subtype_aux ~(received : Type.t) ~(expected : Type.t)
    : (O.expression -> O.expression Elaboration.t, _, _) t
  =
  let open Let_syntax in
  let subtype received expected = subtype_aux ~received ~expected in
  let subtype_texists ~mode tvar type_ =
    let%bind () = occurs_check ~tvar type_ in
    let%bind kind =
      Context.get_texists_var tvar ~error:(Errors.unbound_texists_var tvar)
    in
    let%bind type_ = lift ~mode ~tvar ~kind type_ in
    let%bind () = Context.add_texists_eq tvar kind type_ in
    return E.return
  in
  let%bind loc = loc () in
  match received.content, expected.content with
  | ( T_arrow { type1 = type11; type2 = type12; param_names = _ }
    , T_arrow { type1 = type21; type2 = type22; param_names = _ } ) ->
    let%bind f1 = subtype type21 type11 in
    let%bind type12 = Context.tapply type12 in
    let%bind type22 = Context.tapply type22 in
    let%bind f2 = subtype type12 type22 in
    return
      E.(
        fun hole ->
          let open E.Let_syntax in
          let%bind type11 = decode type11
          and type12 = decode type12
          and type21 = decode type21
          and type22 = decode type22 in
          if O.compare_type_expression type11 type21 = 0
             && O.compare_type_expression type12 type22 = 0
          then return hole
          else (
            let x = Value_var.fresh ~loc ~name:"_sub" () in
            let%bind arg = f1 (O.e_variable ~loc x type21) in
            let binder = Param.make x type21 in
            let%bind result = f2 (O.e_a_application ~loc hole arg type12) in
            return
            @@ O.e_a_lambda ~loc { binder; result; output_type = type22 } type21 type22))
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
          let open E.Let_syntax in
          let%bind type' = decode type' in
          let%bind texists = decode texists in
          f (O.e_type_inst ~loc { forall = hole; type_ = texists } type'))
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
          let open E.Let_syntax in
          let%bind result = f hole in
          let%bind expected = decode expected in
          return @@ O.e_type_abstraction ~loc { type_binder = tvar'; result } expected)
  | (T_variable tvar1, T_variable tvar2 | T_exists tvar1, T_exists tvar2)
    when Type_var.equal tvar1 tvar2 -> return E.return
  | T_exists tvar1, _ -> subtype_texists ~mode:Contravariant tvar1 expected
  | _, T_exists tvar2 -> subtype_texists ~mode:Covariant tvar2 received
  | T_construct { constructor = Nat; _ }, _
  | T_construct { constructor = Int; _ }, _
  | T_construct { constructor = Tez; _ }, _
  | T_construct { constructor = String; _ }, _
  | T_construct { constructor = Bytes; _ }, _
  | T_construct { constructor = List; _ }, _
  | T_construct { constructor = Set; _ }, _
  | T_construct { constructor = Map; _ }, _
    when Option.is_some (Type.get_t_bool expected) ->
    return
      E.(
        fun hole ->
          let open E.Let_syntax in
          let%bind expected = decode expected in
          return
          @@ O.e_coerce ~loc { anno_expr = hole; type_annotation = expected } expected)
  | T_singleton lit, T_construct _
    when Option.is_some (Type.get_t_base_inj expected (Literal_value.typeof lit)) ->
    return E.return
  | T_union received_as_union, T_union expected_as_union ->
    if Union.equal Type.equal received_as_union expected_as_union
    then return E.return
    else
      try_both
        (subtype_expected_union ~received ~expected ~expected_as_union)
        (subtype_received_union ~received ~received_as_union ~expected)
  | _, T_union expected_as_union ->
    subtype_expected_union ~received ~expected ~expected_as_union
  | T_union received_as_union, _ ->
    subtype_received_union ~received ~received_as_union ~expected
  | _, _ ->
    let%bind () = unify_aux received expected in
    return E.return


and subtype_expected_union ~expected ~expected_as_union ~received =
  let open Let_syntax in
  let module Elab_union_injection = E.Make_all (Union.Injection) in
  let%bind coercion, injection =
    let injections = Union.Injection.injections_of_union expected_as_union in
    let coercions_and_injections =
      injections
      |> List.map ~f:(fun injection ->
             let summand = Union.Injection.source injection in
             let%bind coercion = subtype_aux ~received ~expected:summand in
             return (coercion, injection))
    in
    match Nonempty_list.of_list coercions_and_injections with
    | None ->
      let%bind no_color = Options.no_color () in
      raise (fun loc -> Errors.cannot_unify_local no_color received expected loc)
    | Some coercions_and_injections_ne -> try_all coercions_and_injections_ne
  in
  return
    E.(
      fun hole ->
        let open Let_syntax in
        let%bind hole_in_summand = coercion hole in
        let%bind injection =
          injection |> Union.Injection.map decode |> Elab_union_injection.all
        in
        let hole_in_union =
          Union.Injected.make ~expr_in_source:hole_in_summand ~injection
        in
        let%bind expected = decode expected in
        return @@ O.e_union_injected hole_in_union expected ~loc:expected.location)


and subtype_received_union ~received ~received_as_union ~expected =
  let open Let_syntax in
  let open Union in
  let module Elab_union_injection = E.Make_all (Union.Injection) in
  let%bind branches =
    received_as_union
    |> Injection.injections_of_union
    |> List.map ~f:(fun injection ->
           let summand = Injection.source injection in
           let%bind coercion = subtype_aux ~received:summand ~expected in
           let var = Value_var.fresh ~generated:true ~loc:Location.generated () in
           return
             E.(
               let open Let_syntax in
               let%bind injection =
                 injection |> Injection.map decode |> Elab_union_injection.all
               in
               let pattern = Match.Pattern.make ~var ~injection in
               let%bind body =
                 let summand = Injection.source injection in
                 let var_as_expr = O.e_variable var summand ~loc:Location.generated in
                 coercion var_as_expr
               in
               let branch = Match.Branch.make ~pattern ~body in
               return branch))
    |> all
    >>| E.all
  in
  return
    E.(
      fun hole ->
        let open Let_syntax in
        let%bind expected = decode expected in
        let before_expansion =
          O.e_coerce
            Ascription.{ anno_expr = hole; type_annotation = expected }
            expected
            ~loc:expected.location
        in
        let%bind branches = branches in
        let after_expansion =
          O.e_union_match
            (Match.make ~matchee:hole ~branches)
            expected
            ~loc:expected.location
        in
        return
        @@ O.e_union_use
             (Use.make ~before_expansion ~after_expansion)
             expected
             ~loc:expected.location)


let subtype ~(received : Type.t) ~(expected : Type.t)
    : (O.expression -> O.expression Elaboration.t, _, _) t
  =
  let open Let_syntax in
  let%bind subtyping_loc = loc () in
  Trace.map_error
    ~f:(fun err -> Errors.cannot_subtype err received expected subtyping_loc)
    (subtype_aux ~received ~expected)


let subtype_opt ~(received : Type.t) ~(expected : Type.t)
    : ((Ast_typed.expression -> Ast_typed.expression Elaboration.t) option, 'b, 'a) t
  =
  let open Let_syntax in
  try_
    (let%bind res = subtype ~received ~expected in
     return (Some res))
    ~with_:(fun _ -> return None)


let exists kind =
  let open Let_syntax in
  let%bind tvar, texists = fresh_texists () in
  let%bind () = Context.push [ C_texists_var (tvar, kind) ] in
  return texists


let for_all kind =
  let open Let_syntax in
  let%bind tvar = fresh_type_var () in
  let%bind () = Context.push [ C_type_var (tvar, kind) ] in
  let%bind path = path () in
  return (Type.t_variable ~loc:(Type_var.get_location tvar) tvar ())


let lexists fields =
  let open Let_syntax in
  let%bind lvar, layout = fresh_lexists () in
  let%bind () = Context.push [ C_lexists_var (lvar, fields) ] in
  return layout


module Error_recovery = struct
  open Let_syntax

  let is_enabled ~raise:_ ~options ~loc:_ ~path:_ ~poly_name_tbl:_ ~refs_tbl:_ state =
    state, options.Compiler_options.typer_error_recovery


  let raise_or_use_default ~error ~default =
    if%bind is_enabled
    then (
      let%bind () = log_error error in
      default)
    else raise error


  let raise_or_use_default_opt ~error ~default =
    Option.value_map ~default:(raise_or_use_default ~error ~default) ~f:return


  let raise_or_use_default_result ~ok ~error ~default = function
    | Ok value -> ok value
    | Error err -> raise_or_use_default ~error:(Fn.flip error err) ~default


  let wildcard_type ~raise ~options ~loc ~path ~poly_name_tbl ~refs_tbl state =
    exists Type ~raise ~options ~loc ~path ~poly_name_tbl ~refs_tbl state


  let raise_or_use_default_type ~error =
    raise_or_use_default ~error ~default:wildcard_type


  let row ~raise:_ ~options:_ ~loc:_ ~path:_ ~poly_name_tbl:_ ~refs_tbl:_ state =
    state, { Type.Row.fields = Record.empty; layout = Type.Layout.default [] }


  let sig_ ~raise:_ ~options:_ ~loc:_ ~path:_ ~poly_name_tbl:_ ~refs_tbl:_ state =
    state, { Context.Signature.items = []; sort = Ss_module }


  module Get = struct
    let get_opt_or_exn getter key ~error ~default : _ t =
      let open Let_syntax in
      match%bind getter key with
      | None -> raise_or_use_default ~error ~default
      | Some value -> return value


    let get_result_or_exn getter key ~error ~default : _ t =
      let open Let_syntax in
      match%bind getter key with
      | Ok value -> return value
      | Error err -> raise_or_use_default ~error:(error err) ~default


    let value var ~error : _ t =
      get_result_or_exn
        Context.get_value
        var
        ~error
        ~default:(wildcard_type >>| fun t -> Param.Mutable, t, Context.Attr.default)


    let imm var ~error : _ t =
      get_opt_or_exn
        Context.get_imm
        var
        ~error
        ~default:(wildcard_type >>| fun t -> t, Context.Attr.default)


    let mut var ~error : _ t =
      get_result_or_exn Context.get_mut var ~error ~default:wildcard_type


    let type_var tvar ~error =
      get_opt_or_exn Context.get_type_var tvar ~error ~default:(return Kind.Type)


    let type_or_type_var tvar ~error =
      get_opt_or_exn
        Context.get_type_or_type_var
        tvar
        ~error
        ~default:(wildcard_type >>| fun t -> `Type t)


    let type_ tvar ~error =
      get_opt_or_exn Context.get_type tvar ~error ~default:wildcard_type


    let module_of_path path ~error : _ t =
      get_opt_or_exn Context.get_module_of_path path ~error ~default:sig_


    let module_type_of_path path ~error : _ t =
      get_opt_or_exn Context.get_module_type_of_path path ~error ~default:sig_


    let module_ mvar ~error : _ t =
      get_opt_or_exn Context.get_module mvar ~error ~default:sig_
  end
end

let def bindings ~on_exit ~in_ =
  Context.add
    (List.map bindings ~f:(fun (var, mut_flag, type_, attr) ->
         Context_.C_value (var, mut_flag, type_, attr)))
    ~in_
    ~on_exit


let def_module bindings ~on_exit ~in_ =
  Context.add
    (List.map bindings ~f:(fun (mvar, sig_) -> Context_.C_module (mvar, sig_)))
    ~on_exit
    ~in_


let def_module_type bindings ~on_exit ~in_ =
  Context.add
    (List.map bindings ~f:(fun (mvar, sig_) -> Context_.C_module_type (mvar, sig_)))
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
  Context.add (List.map sig_items ~f:Context_.item_of_signature_item) ~in_ ~on_exit


let assert_ cond ~error =
  let open Let_syntax in
  if cond then return () else log_error error


let hash_context () =
  let open Let_syntax in
  let%bind ctx = context () in
  Context_.Hashes.set_context ctx;
  return ()


let generalize (t : (Type.t * 'a, _, _) t)
    : (Type.t * (Type_var.t * Kind.t) list * 'a, _, _) t
  =
 fun ~raise ~options ~loc ~path ~poly_name_tbl ~refs_tbl (ctx, subst) ->
  let ctx, pos = Context_.mark ctx in
  let (ctx, subst), (type_, result) =
    t ~raise ~options ~loc ~path ~poly_name_tbl ~refs_tbl (ctx, subst)
  in
  let ctx, type_, tvars, subst' =
    Context_.generalize ctx type_ ~pos ~loc ~poly_name_tbl
  in
  (ctx, Substitution.merge subst subst'), (type_, tvars, result)


let create_type (constr : Type.constr) =
  let open Let_syntax in
  let%bind loc = loc () in
  return (constr ~loc ())


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
  let map_error ~f t ~raise = Trace.map_error ~f ~raise t

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

  let all_lmap (lmap : ('a, 'err, 'wrn) t Label.Map.t) : ('a Label.Map.t, 'err, 'wrn) t =
   fun ~raise ~options ~loc ~path ~poly_name_tbl ~refs_tbl state ->
    let (state, frag), lmap =
      Label.Map.fold_map
        lmap
        ~init:(state, [])
        ~f:(fun ~key:_label ~data:t (state, frag) ->
          let state, (frag', result) =
            t ~raise ~options ~loc ~path ~poly_name_tbl ~refs_tbl state
          in
          (state, frag @ frag'), result)
    in
    state, (frag, lmap)


  let all_lmap_unit (lmap : (unit, 'err, 'wrn) t Label.Map.t) : (unit, 'err, 'wrn) t =
   fun ~raise ~options ~loc ~path ~poly_name_tbl ~refs_tbl state ->
    let state, frag =
      Core.Map.fold
        ~f:(fun ~key:_label ~data:t (state, frag) ->
          let state, (frag', ()) =
            t ~raise ~options ~loc ~path ~poly_name_tbl ~refs_tbl state
          in
          state, frag @ frag')
        lmap
        ~init:(state, [])
    in
    state, (frag, ())


  let loc () = lift (loc ())
  let set_loc loc = lift_reader (set_loc loc)
  let raise_result result ~error = lift (raise_result result ~error)
  let raise_opt opt ~error = lift (raise_opt opt ~error)
  let raise_l ~loc error = lift (raise_l ~loc error)
  let raise error = lift (raise error)
  let warn warning = lift (warn warning)
  let assert_ cond ~error = lift (assert_ ~error cond)
  let create_type type_ = lift (create_type type_)

  module Context = struct
    let get_sum label = lift (Context.get_sum label)
    let get_record row = lift (Context.get_record row)
    let tapply type_ = lift (Context.tapply type_)
  end

  let exists kind = lift (exists kind)
  let lexists fields = lift (lexists fields)
  let unify type1 type2 = lift (unify type1 type2)
  let subtype ~received ~expected = lift (subtype ~received ~expected)
end

let rec lub2_without_union (typ1 : Type.t) (typ2 : Type.t) =
  let open Let_syntax in
  let%bind typ1 = Context.tapply typ1 in
  let%bind typ2 = Context.tapply typ2 in
  match typ1.content, typ2.content with
  | _, _ when Type.equal typ1 typ2 -> return (Some (typ1, E.return, E.return))
  | T_singleton lit, _ ->
    let%bind typ1' =
      let typ = Literal_value.typeof lit in
      let constr = Type.t_construct typ [] in
      create_type constr
    in
    let%bind typ1_to_typ1' =
      subtype_opt ~received:typ1 ~expected:typ1'
      >>| Option.value_or_thunk ~default:(fun () ->
              (* Invariant: if [t : singleton_type(t')] and [t' : A] then [t : A] *)
              assert false)
    in
    (match%bind lub2_without_union typ1' typ2 with
    | None -> return None
    | Some (lub, typ1'_to_lub, typ2_to_lub) ->
      let typ1_to_lub expr = E.(expr |> typ1_to_typ1' >>= typ1'_to_lub) in
      return @@ Option.some (lub, typ1_to_lub, typ2_to_lub))
  | _, T_singleton _ ->
    lub2_without_union typ2 typ1
    >>| Option.map ~f:(fun (lub, typ2_to_lub, typ1_to_lub) ->
            lub, typ1_to_lub, typ2_to_lub)
  | _, _ -> return None


let rec lub_without_union (types : Type.t list) =
  let open Let_syntax in
  match types with
  | [] -> return None
  | [ typ ] -> return (Some (typ, [ typ, E.return ]))
  | typ :: types' ->
    (match%bind lub_without_union types' with
    | None -> return None
    | Some (lub', types'_to_lub') ->
      (match%bind lub2_without_union typ lub' with
      | None -> return None
      | Some (lub, typ_to_lub, lub'_to_lub) ->
        let types'_to_lub =
          types'_to_lub'
          |> List.map ~f:(fun (typ', typ'_to_lub') ->
                 let typ'_to_lub expr =
                   let open E in
                   expr |> typ'_to_lub' >>= lub'_to_lub
                 in
                 typ', typ'_to_lub)
        in
        let types_to_lub = (typ, typ_to_lub) :: types'_to_lub in
        return (Some (lub, types_to_lub))))


let lub_union (types : Type.t list) =
  let open Let_syntax in
  let union = Union.make types in
  let lub = Type.t_union union ~loc:Location.generated () in
  let%bind coercions =
    types
    |> List.map ~f:(fun typ ->
           let%bind coercion = subtype ~received:typ ~expected:lub in
           return (typ, coercion))
    |> all
  in
  return (lub, coercions)


let lub types =
  let open Let_syntax in
  let x = try_ (lub_without_union types) ~with_:(fun _ -> return None) in
  match%bind x with
  | Some lub -> return lub
  | None -> lub_union types
