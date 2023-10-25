module Location = Simple_utils.Location
module List = Simple_utils.List
module Ligo_string = Simple_utils.Ligo_string
open Simple_utils
open Ligo_prim
open Types

let assert_same_size a b = if List.length a = List.length b then Some () else None
let constant_compare ia ib = Literal_types.compare ia ib

let assert_no_type_vars (t : type_expression) : unit option =
  let f r te =
    let open Option in
    let* () = r in
    match te.type_content with
    | T_variable _ | T_for_all _ -> None
    | _ -> return ()
  in
  Helpers.fold_type_expression t ~init:(Some ()) ~f


let rec assert_type_expression_eq ((a, b) : type_expression * type_expression)
    : unit option
  =
  let open Option in
  match a.type_content, b.type_content with
  | ( T_constant { language = la; injection = ia; parameters = lsta }
    , T_constant { language = lb; injection = ib; parameters = lstb } ) ->
    if String.equal la lb && constant_compare ia ib = 0
    then
      let* _ = assert_same_size lsta lstb in
      List.fold_left
        ~f:(fun acc p ->
          match acc with
          | None -> None
          | Some () -> assert_type_expression_eq p)
        ~init:(Some ())
        (List.zip_exn lsta lstb)
    else None
  | T_constant _, _ -> None
  | T_sum row1, T_sum row2 | T_record row1, T_record row2 ->
    Option.some_if
      (Row.equal
         (fun t1 t2 -> Option.is_some @@ assert_type_expression_eq (t1, t2))
         row1
         row2)
      ()
  | T_record _, _ -> None
  | T_sum _, _ -> None
  | T_arrow { type1; type2 }, T_arrow { type1 = type1'; type2 = type2' } ->
    let* _ = assert_type_expression_eq (type1, type1') in
    assert_type_expression_eq (type2, type2')
  | T_arrow _, _ -> None
  | T_variable x, T_variable y ->
    (* TODO : we must check that the two types were bound at the same location (even if they have the same name), i.e. use something like De Bruijn indices or a propper graph encoding *)
    if Type_var.equal x y then Some () else None
  | T_variable _, _ -> None
  | T_singleton a, T_singleton b -> assert_literal_eq (a, b)
  | T_singleton _, _ -> None
  | T_abstraction a, T_abstraction b ->
    assert_type_expression_eq (a.type_, b.type_)
    >>= fun _ -> Some (assert (Kind.equal a.kind b.kind))
  | T_for_all a, T_for_all b ->
    assert_type_expression_eq (a.type_, b.type_)
    >>= fun _ -> Some (assert (Kind.equal a.kind b.kind))
  | T_abstraction _, _ -> None
  | T_for_all _, _ -> None


and type_expression_eq ab = Option.is_some @@ assert_type_expression_eq ab

and assert_literal_eq ((a, b) : Literal_value.t * Literal_value.t) : unit option =
  match a, b with
  | Literal_int a, Literal_int b when Z.equal a b -> Some ()
  | Literal_int _, Literal_int _ -> None
  | Literal_int _, _ -> None
  | Literal_nat a, Literal_nat b when Z.equal a b -> Some ()
  | Literal_nat _, Literal_nat _ -> None
  | Literal_nat _, _ -> None
  | Literal_timestamp a, Literal_timestamp b when Z.equal a b -> Some ()
  | Literal_timestamp _, Literal_timestamp _ -> None
  | Literal_timestamp _, _ -> None
  | Literal_mutez a, Literal_mutez b when Z.equal a b -> Some ()
  | Literal_mutez _, Literal_mutez _ -> None
  | Literal_mutez _, _ -> None
  | Literal_string a, Literal_string b when Ligo_string.equal a b -> Some ()
  | Literal_string _, Literal_string _ -> None
  | Literal_string _, _ -> None
  | Literal_bytes a, Literal_bytes b when Bytes.equal a b -> Some ()
  | Literal_bytes _, Literal_bytes _ -> None
  | Literal_bytes _, _ -> None
  | Literal_unit, Literal_unit -> Some ()
  | Literal_unit, _ -> None
  | Literal_address a, Literal_address b when String.equal a b -> Some ()
  | Literal_address _, Literal_address _ -> None
  | Literal_address _, _ -> None
  | Literal_signature a, Literal_signature b when String.equal a b -> Some ()
  | Literal_signature _, Literal_signature _ -> None
  | Literal_signature _, _ -> None
  | Literal_key a, Literal_key b when String.equal a b -> Some ()
  | Literal_key _, Literal_key _ -> None
  | Literal_key _, _ -> None
  | Literal_key_hash a, Literal_key_hash b when String.equal a b -> Some ()
  | Literal_key_hash _, Literal_key_hash _ -> None
  | Literal_key_hash _, _ -> None
  | Literal_chain_id a, Literal_chain_id b when String.equal a b -> Some ()
  | Literal_chain_id _, Literal_chain_id _ -> None
  | Literal_chain_id _, _ -> None
  | Literal_operation _, Literal_operation _ -> None
  | Literal_operation _, _ -> None
  | Literal_bls12_381_g1 a, Literal_bls12_381_g1 b when Bytes.equal a b -> Some ()
  | Literal_bls12_381_g1 _, Literal_bls12_381_g1 _ -> None
  | Literal_bls12_381_g1 _, _ -> None
  | Literal_bls12_381_g2 a, Literal_bls12_381_g2 b when Bytes.equal a b -> Some ()
  | Literal_bls12_381_g2 _, Literal_bls12_381_g2 _ -> None
  | Literal_bls12_381_g2 _, _ -> None
  | Literal_bls12_381_fr a, Literal_bls12_381_fr b when Bytes.equal a b -> Some ()
  | Literal_bls12_381_fr _, Literal_bls12_381_fr _ -> None
  | Literal_bls12_381_fr _, _ -> None


let rec get_entry (lst : module_) (name : Value_var.t) : expression option =
  let aux x =
    match Location.unwrap x with
    | D_value
        { binder
        ; expr
        ; attr =
            { inline = _
            ; no_mutation = _
            ; view = _
            ; dyn_entry = _
            ; public = _
            ; hidden = _
            ; thunk = _
            ; entry = _
            ; deprecated = _
            }
        } -> if Binder.apply (Value_var.equal name) binder then Some expr else None
    | D_module_include { module_content = M_struct x; _ } -> get_entry x name
    | D_module_include _ | D_irrefutable_match _ | D_type _ | D_module _ | D_signature _
      -> None
  in
  List.find_map ~f:aux (List.rev lst)


let get_type_of_contract ty =
  match Combinators.get_t_arrow ty with
  | Some { type1; type2 } ->
    (match Combinators.get_t_pair type1, Combinators.get_t_pair type2 with
    | Some (parameter, storage), Some (listop, storage') ->
      let open Simple_utils.Option in
      let* () = Combinators.assert_t_list_operation listop in
      let* () = assert_type_expression_eq (storage, storage') in
      (* TODO: on storage/parameter : asert_storable, assert_passable ? *)
      return (parameter, storage)
    | _ -> None)
  | _ -> None


let get_type_of_entrypoint ty =
  let is_t_list_operation listop =
    Option.is_some @@ Combinators.assert_t_list_operation listop
  in
  match Combinators.get_t_arrow ty with
  | Some { type1 = tin; type2 = return } ->
    let parameter = tin in
    (match Combinators.get_t_arrow return with
    | Some { type1 = storage; type2 = return } ->
      (match Combinators.get_t_pair return with
      | Some (listop, storage') ->
        if is_t_list_operation listop && type_expression_eq (storage, storage')
        then Some (parameter, storage)
        else None
      | _ -> None)
    | None -> None)
  | None -> None


let build_entry_type p_ty s_ty =
  let open Combinators in
  let loc = Location.generated in
  t_arrow
    ~loc
    (t_pair ~loc p_ty s_ty)
    (t_pair ~loc (t_list ~loc (t_operation ~loc ())) s_ty)
    ()


let should_uncurry_entry entry_ty =
  let is_t_list_operation listop =
    Option.is_some @@ Combinators.assert_t_list_operation listop
  in
  match Combinators.get_t_arrow entry_ty with
  | Some { type1 = tin; type2 = return } ->
    let parameter = tin in
    (match Combinators.get_t_arrow return with
    | Some { type1 = storage; type2 = return } ->
      (match Combinators.get_t_pair return with
      | Some (listop, storage') ->
        if is_t_list_operation listop && type_expression_eq (storage, storage')
        then `Yes (parameter, storage)
        else `Bad
      | _ -> `Bad)
    | None -> `Bad)
  | None -> `Bad


(* We cannot really tell the difference, this is just an heuristic when we try with curried first *)
let should_uncurry_view ~storage_ty view_ty =
  match Combinators.get_t_arrow view_ty with
  | Some { type1 = tin; type2 = return } ->
    (match Combinators.get_t_arrow return with
    | Some { type1 = storage; type2 = return }
      when Option.is_some (assert_type_expression_eq (storage_ty, storage)) ->
      `Yes (tin, storage, return)
    | _ -> `Bad)
  | None -> `Bad_not_function


let parameter_from_entrypoints
    :  (Value_var.t * type_expression) List.Ne.t
    -> ( type_expression * type_expression
       , [> `Not_entry_point_form of Types.expression_variable * Types.type_expression
         | `Storage_does_not_match of
           Value_var.t * Types.type_expression * Value_var.t * Types.type_expression
         ] )
       result
  =
 fun ((entrypoint, entrypoint_type), rest) ->
  let open Result in
  let* parameter, storage =
    match should_uncurry_entry entrypoint_type with
    | `Yes (parameter, storage) | `No (parameter, storage) ->
      Result.Ok (parameter, storage)
    | `Bad -> Result.Error (`Not_entry_point_form (entrypoint, entrypoint_type))
  in
  let* parameter_list =
    List.fold_result
      ~init:[ String.capitalize (Value_var.to_name_exn entrypoint), parameter ]
      ~f:(fun parameters (ep, ep_type) ->
        let* parameter_, storage_ =
          match should_uncurry_entry ep_type with
          | `Yes (parameter, storage) | `No (parameter, storage) ->
            Result.Ok (parameter, storage)
          | `Bad -> Result.Error (`Not_entry_point_form (ep, entrypoint_type))
        in
        let* () =
          Result.of_option
            ~error:(`Storage_does_not_match (entrypoint, storage, ep, storage_))
          @@ assert_type_expression_eq (storage_, storage)
        in
        return ((String.capitalize (Value_var.to_name_exn ep), parameter_) :: parameters))
      rest
  in
  return
    ( Combinators.t_sum_ez
        ~loc:Location.generated
        ~layout:Combinators.default_layout
        parameter_list
    , storage )


(* Wrap a variable `f` of type `parameter -> storage -> return`
   to an expression `fun (p, s) -> f p s : parameter * storage -> return` *)
let uncurry_wrap ~loc ~type_ var =
  let open Combinators in
  let open Simple_utils.Option in
  let* { type1 = input_ty; type2 = output_ty } = get_t_arrow type_ in
  let* { type1 = storage; type2 = output_ty } = get_t_arrow output_ty in
  (* We create a wrapper to uncurry it: *)
  let parameter = input_ty in
  let p_var = Value_var.fresh ~loc ~name:"parameter" () in
  let s_var = Value_var.fresh ~loc ~name:"storage" () in
  let ps_var = Value_var.fresh ~loc ~name:"input" () in
  let p_binder = Binder.make p_var parameter in
  let s_binder = Binder.make s_var storage in
  let ps_param = Param.make ps_var (t_pair ~loc parameter storage) in
  let p_expr = e_a_variable ~loc p_var parameter in
  let s_expr = e_a_variable ~loc s_var storage in
  let ps_expr = e_a_variable ~loc ps_var (t_pair ~loc parameter storage) in
  (* main(p) *)
  let expr =
    e_a_application
      ~loc
      (e_a_variable ~loc var type_)
      p_expr
      (t_arrow ~loc storage output_ty ())
  in
  (* main(p)(s) *)
  let expr = e_a_application ~loc expr s_expr output_ty in
  (* match ps with (p, s) -> main(p)(s) *)
  let expr =
    e_a_matching
      ~loc:Location.generated
      ps_expr
      [ { pattern =
            Location.wrap
              ~loc:Location.generated
              Pattern.(
                P_tuple
                  [ Location.wrap ~loc:Location.generated @@ P_var p_binder
                  ; Location.wrap ~loc:Location.generated @@ P_var s_binder
                  ])
        ; body = expr
        }
      ]
      output_ty
  in
  (* fun ps -> match ps with (p, s) -> main(p)(s) *)
  let expr =
    e_a_lambda
      ~loc
      { binder = ps_param; output_type = output_ty; result = expr }
      (t_pair ~loc parameter storage)
      output_ty
  in
  some @@ expr


let rec fetch_views_in_module ~storage_ty
    : module_ -> module_ * (type_expression * type_expression Binder.t) list
  =
 fun prog ->
  let aux declt ((prog, views) : module_ * _) =
    let return () = declt :: prog, views in
    let loc = Location.get_location declt in
    match Location.unwrap declt with
    | D_value ({ binder; expr; attr } as dvalue) when attr.view ->
      let var = Binder.get_var binder in
      (match should_uncurry_view ~storage_ty expr.type_expression with
      | `Yes _ ->
        let expr =
          Option.value_exn @@ uncurry_wrap ~loc ~type_:expr.type_expression var
        in
        let binder = Binder.set_var binder (Value_var.fresh_like var) in
        let binder = Binder.set_ascr binder expr.type_expression in
        (* Add both `main` and the new `main#FRESH` version that calls `main` but it's curried *)
        ( (Location.wrap ~loc:declt.location @@ D_value dvalue)
          :: (Location.wrap ~loc:declt.location @@ D_value { dvalue with binder; expr })
          :: prog
        , (expr.type_expression, Binder.map (fun _ -> expr.type_expression) binder)
          :: views )
      | `No _ | `Bad | `Bad_not_function | `Bad_storage _ ->
        ( (Location.wrap ~loc:declt.location @@ D_value dvalue) :: prog
        , (expr.type_expression, Binder.map (fun _ -> expr.type_expression) binder)
          :: views ))
    | D_irrefutable_match
        ({ pattern = { wrap_content = P_var binder; _ } as pattern; expr; attr } as
        dirref)
      when attr.view ->
      let var = Binder.get_var binder in
      (match should_uncurry_view ~storage_ty expr.type_expression with
      | `Yes _ ->
        let expr =
          Option.value_exn @@ uncurry_wrap ~loc ~type_:expr.type_expression var
        in
        let binder = Binder.set_var binder (Value_var.fresh_like var) in
        let binder = Binder.set_ascr binder expr.type_expression in
        let pattern = Pattern.{ pattern with wrap_content = P_var binder } in
        (* Add both `main` and the new `main#FRESH` version that calls `main` but it's curried *)
        ( (Location.wrap ~loc:declt.location @@ D_irrefutable_match dirref)
          :: (Location.wrap ~loc:declt.location
             @@ D_irrefutable_match { dirref with expr; pattern })
          :: prog
        , (expr.type_expression, Binder.map (fun _ -> expr.type_expression) binder)
          :: views )
      | `No _ | `Bad | `Bad_not_function | `Bad_storage _ ->
        ( (Location.wrap ~loc:declt.location @@ D_irrefutable_match dirref) :: prog
        , (expr.type_expression, Binder.map (fun _ -> expr.type_expression) binder)
          :: views ))
    | D_module_include { module_content = M_struct x; _ } ->
      fetch_views_in_module ~storage_ty x
    | D_module_include _
    | D_irrefutable_match _
    | D_type _
    | D_module _
    | D_value _
    | D_signature _ -> return ()
  in
  List.fold_right ~f:aux ~init:([], []) prog


let get_path_signature : signature -> Module_var.t list -> signature option =
 fun prg_sig mods ->
  let open Simple_utils.Option in
  List.fold
    mods
    ~f:(fun acc el ->
      let* acc in
      List.find_map acc.sig_items ~f:(function
          | S_module (m, sig_) when Module_var.equal el m -> Some sig_
          | _ -> None))
    ~init:(Some prg_sig)


let get_contract_signature
    : signature -> Module_var.t list -> (signature * contract_sig) option
  =
 fun prg_sig mods ->
  let open Simple_utils.Option in
  let* sig_ = get_path_signature prg_sig mods in
  match sig_.sig_sort with
  | Ss_contract csig -> return @@ (sig_, csig)
  | _ -> None


let get_sig_value
    :  Module_var.t list -> Value_var.t -> signature
    -> (ty_expr * sig_item_attribute) option
  =
 fun path v sig_ ->
  let open Simple_utils.Option in
  let* sig_ = get_path_signature sig_ path in
  List.find_map sig_.sig_items ~f:(function
      | S_value (v', ty, attr) when Value_var.equal v v' -> Some (ty, attr)
      | _ -> None)


let get_entrypoint_parameter_type
    : Label.t option -> type_expression -> type_expression option
  =
 fun label parameter_ty ->
  let open Simple_utils.Option in
  let* rows = Combinators.get_t_sum parameter_ty in
  let lst = Row.to_alist rows in
  match lst with
  | [ (_single_entry, ty) ] when Option.is_none label -> Some ty
  | _ ->
    let* label in
    Row.find_type rows label


let get_entrypoint_storage_type : program -> Module_var.t list -> type_expression option =
 fun prg mods ->
  let open Simple_utils.Option in
  let* _, { storage; _ } = get_contract_signature prg.pr_sig mods in
  return storage


let to_sig_items (module_ : module_) : sig_item list =
  List.fold module_ ~init:[] ~f:(fun ctx decl ->
      match Location.unwrap decl with
      | D_irrefutable_match { pattern; expr = _; attr = { view; entry; dyn_entry; _ } } ->
        List.fold (Pattern.binders pattern) ~init:ctx ~f:(fun ctx x ->
            ctx
            @ [ S_value
                  ( Binder.get_var x
                  , Binder.get_ascr x
                  , { dyn_entry; view; entry; optional = false } )
              ])
      | D_value { binder; expr; attr = { view; entry; dyn_entry; _ } } ->
        ctx
        @ [ S_value
              ( Binder.get_var binder
              , expr.type_expression
              , { view; entry; dyn_entry; optional = false } )
          ]
      | D_type { type_binder; type_expr; type_attr = _ } ->
        ctx @ [ S_type (type_binder, type_expr) ]
      | D_module_include x -> x.signature.sig_items
      | D_module { module_binder; module_; module_attr = _; annotation = () } ->
        ctx @ [ S_module (module_binder, module_.signature) ]
      | D_signature { signature_binder; signature; signature_attr } ->
        ctx @ [ S_module_type (signature_binder, signature) ])


let to_signature (module_ : module_) : signature =
  let sig_items = to_sig_items module_ in
  { sig_items; sig_sort = Ss_module }


let to_extended_signature (prg : program) : signature =
  { sig_items = to_sig_items prg.pr_module; sig_sort = prg.pr_sig.sig_sort }
