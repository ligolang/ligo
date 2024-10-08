open Ligo_prim
module PP_helpers = Simple_utils.PP_helpers
module Ligo_option = Simple_utils.Ligo_option
include Type_def

type content = [%import: Type_def.content]
[@@deriving
  ez
    { prefixes =
        [ ( "make_t"
          , fun ~loc ?orig_var ?(applied_types = []) content : t ->
              { content
              ; location = loc
              ; abbrev =
                  Option.map orig_var ~f:(fun orig_var -> { orig_var; applied_types })
              } )
        ; ("get", fun x -> x.content)
        ]
    ; wrap_constructor = ("content", fun type_content ~loc () -> make_t ~loc type_content)
    ; wrap_get = "content", get
    ; default_get = `Option
    }]

type constr = loc:Location.t -> unit -> t

let set_orig_var ~orig_var t =
  { t with
    abbrev =
      (match t.abbrev with
      | None -> Some { orig_var; applied_types = [] }
      | Some abbrev -> Some { abbrev with orig_var })
  }


let set_applied_types ~applied_types t =
  { t with abbrev = Option.map t.abbrev ~f:(fun abbrev -> { abbrev with applied_types }) }


let map_applied_types ~f t =
  { t with
    abbrev =
      Option.map t.abbrev ~f:(fun abbrev ->
          { abbrev with applied_types = List.map ~f abbrev.applied_types })
  }


let rec free_vars t =
  let module Set = Type_var.Set in
  match t.content with
  | T_variable tvar -> Set.singleton tvar
  | T_exists _ -> Set.empty
  | T_construct { parameters; _ } -> parameters |> List.map ~f:free_vars |> Set.union_list
  | T_sum row | T_record row -> free_vars_row row
  | T_union union -> union |> Union.map free_vars |> Union.fold Core.Set.union Set.empty
  | T_arrow arr -> arr |> Arrow.map free_vars |> Arrow.fold Core.Set.union Set.empty
  | T_for_all abs | T_abstraction abs ->
    abs |> Abstraction.map free_vars |> Abstraction.fold Core.Set.union Set.empty
  | T_singleton _ -> Set.empty


and free_vars_row row =
  Row.fold (fun fvs row_elem -> Set.union (free_vars row_elem) fvs) Type_var.Set.empty row


let rec subst ?(free_vars = Type_var.Set.empty) t ~tvar ~type_ =
  let subst t = subst t ~free_vars ~tvar ~type_ in
  let subst_abstraction abs = subst_abstraction abs ~free_vars ~tvar ~type_ in
  let subst_row row = subst_row row ~free_vars ~tvar ~type_ in
  let return content = map_applied_types ~f:subst { t with content } in
  match t.content with
  | T_variable tvar' ->
    if Type_var.(tvar = tvar') then type_ else return @@ T_variable tvar'
  | T_exists tvar' -> return @@ T_exists tvar'
  | T_construct { language; constructor; parameters } ->
    let parameters = List.map ~f:subst parameters in
    return @@ T_construct { language; constructor; parameters }
  | T_sum row ->
    let row = subst_row row in
    return @@ T_sum row
  | T_union union ->
    let union = Union.map subst union in
    return @@ T_union union
  | T_record row ->
    let row = subst_row row in
    return @@ T_record row
  | T_singleton literal -> return @@ T_singleton literal
  | T_arrow arr ->
    let arr = Arrow.map subst arr in
    return @@ T_arrow arr
  | T_abstraction abs ->
    let abs = subst_abstraction abs in
    return @@ T_abstraction abs
  | T_for_all abs ->
    let abs = subst_abstraction abs in
    return @@ T_for_all abs


and subst_var t ~tvar ~tvar' =
  subst
    t
    ~free_vars:(Type_var.Set.singleton tvar')
    ~tvar
    ~type_:(t_variable ~loc:(Type_var.get_location tvar') tvar' ())


and subst_abstraction
    ?(free_vars = Type_var.Set.empty)
    { ty_binder; kind; type_ }
    ~tvar
    ~type_:type_'
    : _ Abstraction.t
  =
  let subst t = subst t ~free_vars:(Set.add free_vars ty_binder) ~tvar ~type_:type_' in
  if Type_var.(tvar = ty_binder)
  then { ty_binder; kind; type_ }
  else if Set.mem free_vars ty_binder
  then (
    let ty_binder' = Type_var.fresh_like ty_binder in
    let type_ = subst_var type_ ~tvar:ty_binder ~tvar':ty_binder' in
    { ty_binder = ty_binder'; kind; type_ = subst type_ })
  else { ty_binder; kind; type_ = subst type_ }


and subst_row ?(free_vars = Type_var.Set.empty) row ~tvar ~type_ =
  Row.map (subst ~free_vars ~tvar ~type_) row


let subst =
  let free_vars_type = free_vars in
  let subst ?free_vars t ~tvar ~type_ =
    let free_vars =
      match free_vars with
      | None -> free_vars_type t
      | Some free_vars -> free_vars
    in
    subst ~free_vars t ~tvar ~type_
  in
  subst


let rec fold : type a. t -> init:a -> f:(a -> t -> a) -> a =
 fun t ~init ~f ->
  let fold acc t = fold t ~f ~init:acc in
  let init = f init t in
  match t.content with
  | T_variable _ | T_exists _ | T_singleton _ -> init
  | T_construct { parameters; _ } -> List.fold parameters ~init ~f
  | T_sum row | T_record row -> fold_row row ~init ~f
  | T_union union -> Union.fold fold init union
  | T_arrow arr -> Arrow.fold fold init arr
  | T_abstraction abs | T_for_all abs -> Abstraction.fold fold init abs


and fold_row : type a. row -> init:a -> f:(a -> t -> a) -> a =
 fun row ~init ~f -> Row.fold f init row


let destruct_type_abstraction t =
  let rec loop binders t =
    match t.content with
    | T_abstraction { ty_binder; type_; _ } -> loop (ty_binder :: binders) type_
    | _ -> List.rev binders, t
  in
  loop [] t


let texists_vars t =
  fold t ~init:Type_var.Set.empty ~f:(fun texists_vars t ->
      match t.content with
      | T_exists tvar -> Set.add texists_vars tvar
      | _ -> texists_vars)


let default_layout = Layout.default

let default_layout_from_field_set fields =
  default_layout
    (fields |> Set.to_list |> List.map ~f:(fun name -> { Layout.name; annot = None }))


let fields_with_no_annot fields =
  List.map ~f:(fun (name, _) -> Layout.{ name; annot = None }) fields


let t_construct constructor parameters ~loc () : t =
  make_t ~loc (T_construct { language = Backend.Michelson.name; constructor; parameters })


let t__type_ ~loc () : t = t_construct Literal_types._type_ [] ~loc ()
  [@@map
    _type_
    , ( "signature"
      , "chain_id"
      , "string"
      , "bytes"
      , "key"
      , "key_hash"
      , "int"
      , "address"
      , "operation"
      , "nat"
      , "tez"
      , "timestamp"
      , "unit"
      , "bls12_381_g1"
      , "bls12_381_g2"
      , "bls12_381_fr"
      , "chest"
      , "chest_key"
      , "never"
      , "mutation"
      , "pvss_key"
      , "baker_hash"
      , "tx_rollup_l2_address"
      , "int64"
      , "michelson_program" )]


let t_michelson_code = t_michelson_program

let t__type_ t ~loc () : t = t_construct ~loc Literal_types._type_ [ t ] ()
  [@@map
    _type_
    , ( "list"
      , "set"
      , "contract"
      , "ticket"
      , "sapling_state"
      , "sapling_transaction"
      , "gen"
      , "views" )]


let t__type_ t t' ~loc () : t = t_construct ~loc Literal_types._type_ [ t; t' ] ()
  [@@map
    _type_, ("map", "big_map", "typed_address", "dynamic_entrypoint", "michelson_contract")]


let row_ez fields ?(layout = default_layout) () =
  let fields = List.map fields ~f:(fun (name, type_) -> Label.of_string name, type_) in
  let layout = layout @@ fields_with_no_annot fields in
  Row.of_alist_exn ~layout fields


let t_record_ez fields ~loc ?layout () = t_record ~loc (row_ez fields ?layout ()) ()

let t_tuple ts ~loc () =
  t_record_ez (List.mapi ts ~f:(fun i t -> Int.to_string i, t)) ~loc ()


let t_pair t1 t2 ~loc () = t_tuple [ t1; t2 ] ~loc ()
let t_triplet t1 t2 t3 ~loc () = t_tuple [ t1; t2; t3 ] ~loc ()
let t_sum_ez fields ~loc ?layout () = t_sum ~loc (row_ez fields ?layout ()) ()
let t_bool ~loc () = t_sum_ez ~loc [ "False", t_unit ~loc (); "True", t_unit ~loc () ] ()
let t_option t ~loc () = t_sum_ez ~loc [ "None", t_unit ~loc (); "Some", t ] ()

let t_arrow param result ~loc ?(param_names = []) () : t =
  t_arrow ~loc { type1 = param; type2 = result; param_names } ()


let t_mutez = t_tez

let t_test_baker_policy ~loc () =
  t_sum_ez
    ~loc
    [ "By_account", t_address ~loc ()
    ; "By_round", t_int ~loc ()
    ; "Excluding", t_list ~loc (t_address ~loc ()) ()
    ]
    ()


let t_test_exec_error ~loc () =
  t_sum_ez
    ~loc
    [ ( "Balance_too_low"
      , t_record_ez
          ~loc
          [ "contract_balance", t_mutez ~loc ()
          ; "contract_too_low", t_address ~loc ()
          ; "spend_request", t_mutez ~loc ()
          ]
          () )
    ; "Other", t_string ~loc ()
    ; "Rejected", t_pair ~loc (t_michelson_code ~loc ()) (t_address ~loc ()) ()
    ]
    ()


let t_test_exec_result ~loc () =
  t_sum_ez ~loc [ "Fail", t_test_exec_error ~loc (); "Success", t_nat ~loc () ] ()


let get_t_construct t constr =
  match t.content with
  | T_construct { constructor = constr'; parameters; _ }
    when Literal_types.equal constr constr' -> Some parameters
  | _ -> None


let get_t_nullary_construct t constr =
  match get_t_construct t constr with
  | Some [] -> Some ()
  | _ -> None


let get_t_unary_construct t constr =
  match get_t_construct t constr with
  | Some [ a ] -> Some a
  | _ -> None


let get_t_binary_construct t constr =
  match get_t_construct t constr with
  | Some [ a; b ] -> Some (a, b)
  | _ -> None


let get_t_unit t = get_t_nullary_construct t Literal_types.Unit
let is_t_unit t = Option.is_some (get_t_unit t)

let get_t__type_ t = get_t_unary_construct t Literal_types._type_
  [@@map
    _type_, ("contract", "list", "set", "ticket", "sapling_state", "sapling_transaction")]


let get_t__type_ t = get_t_binary_construct t Literal_types._type_
  [@@map _type_, ("map", "big_map")]


let get_t_bool (t : t) : unit option =
  match t.content with
  | T_sum { fields; _ } ->
    let keys = Map.key_set fields in
    if Set.length keys = 2
       && Set.mem keys (Label.of_string "True")
       && Set.mem keys (Label.of_string "False")
    then Some ()
    else None
  | _ -> None


let get_t_tuple (t : t) : t list option =
  let tuple_of_record row = Row.to_tuple row in
  match t.content with
  | T_record row when Row.is_tuple row -> Some (tuple_of_record row)
  | _ -> None


let get_t_option (t : t) : t option =
  let some = Label.of_string "Some" in
  let none = Label.of_string "None" in
  match t.content with
  | T_sum { fields; _ } ->
    let keys = Map.key_set fields in
    if Set.length keys = 2 && Set.mem keys some && Set.mem keys none
    then Map.find fields some
    else None
  | _ -> None


let get_t_pair (t : t) : (t * t) option =
  match t.content with
  | T_record m when Row.is_tuple m ->
    (match Row.to_tuple m with
    | [ t1; t2 ] -> Some (t1, t2)
    | _ -> None)
  | _ -> None


let rec get_arrows_result t =
  match t.content with
  | T_arrow { type1 = _; type2; param_names = _ } -> get_arrows_result type2
  | _ -> t


module Type_var_name_tbl : sig
  type t

  (** [create ()] creates a new type variable table. *)
  val create : unit -> t

  val pp : t Fmt.t

  (** [clear t] clears the table [t]. *)
  val clear : t -> unit

  (** [name_of t tvar] returns the human readable name of [tvar]. *)
  val name_of : t -> Type_var.t -> string

  module Exists : sig
    (** [clear ()] clears the table used for existential variables. *)
    val clear : unit -> unit

    (** [name_of tvar] returns the human readable name for the existential variable [tvar] *)
    val name_of : Type_var.t -> string
  end
end = struct
  type t =
    { name_tbl : (Type_var.t, string) Hashtbl.t
          (* [name_tbl] is the mapping from type variables to names *)
    ; names : string Hash_set.t
          (* [names] is the set of existing names (superset of [Hashtbl.data name_tbl]) *)
    ; mutable name_counter : int
          (* [name_counter] is a counter used to generate unique variable names *)
    }

  let pp ppf { name_tbl; names; name_counter } =
    Format.fprintf
      ppf
      "name_tbl: %a\n\nnames: %a\n\nname_counter: %d\n\n"
      Fmt.Dump.(list @@ pair Type_var.pp String.pp)
      (Hashtbl.to_alist name_tbl)
      (Fmt.Dump.list String.pp)
      (Hash_set.to_list names)
      name_counter


  let create () =
    { name_tbl = Hashtbl.create (module Type_var)
    ; names = Hash_set.create (module String)
    ; name_counter = 0
    }


  let clear t =
    Hashtbl.clear t.name_tbl;
    Hash_set.clear t.names;
    t.name_counter <- 0


  let exists_tbl = create ()
  let is_used t name = Hash_set.mem t.names name || Hash_set.mem exists_tbl.names name
  let incr_name_counter t = t.name_counter <- t.name_counter + 1

  let rec create_name t =
    let name =
      if t.name_counter < 26
      then String.of_char (Char.of_int_exn (97 + t.name_counter))
      else
        String.of_char (Char.of_int_exn (97 + (t.name_counter mod 26)))
        ^ Int.to_string (t.name_counter / 26)
    in
    incr_name_counter t;
    if is_used t name then create_name t else name


  let add_name t tvar name =
    Hashtbl.add_exn t.name_tbl ~key:tvar ~data:name;
    Hash_set.add t.names name


  let name_of t tvar =
    match Hashtbl.find t.name_tbl tvar with
    | Some name -> name
    | None ->
      let name =
        if Type_var.is_generated tvar
        then create_name t
        else (
          (* User-defined name. We'd like to try keep the name. However
             a collision could occur if we've previously used this name.

             We resolve the collision by adding a number to the end until we reach
             a unique name *)
          let name = Type_var.to_name_exn tvar in
          let curr_name = ref name in
          let i = ref 0 in
          while is_used t !curr_name do
            curr_name := name ^ Int.to_string !i;
            Int.incr i
          done;
          !curr_name)
      in
      add_name t tvar name;
      (* Invariant: [name] is unique (wrt table [t]) *)
      name


  module Exists = struct
    let clear () = clear exists_tbl
    let name_of tvar = name_of exists_tbl tvar
  end
end

let pp_layout = Layout.pp

let rec pp ~name_of_tvar ~name_of_exists ppf t =
  let pp = pp ~name_of_tvar ~name_of_exists in
  if Option.is_some (get_t_bool t)
  then bool ppf
  else if Option.is_some (get_t_option t)
  then option ~name_of_tvar ~name_of_exists ppf t
  else (
    match t.content with
    | T_variable tvar -> Format.fprintf ppf "%s" (name_of_tvar tvar)
    | T_exists tvar -> Format.fprintf ppf "^%s" (name_of_exists tvar)
    | T_arrow arr -> Arrow.pp pp ppf arr
    | T_construct construct -> pp_construct ~name_of_tvar ~name_of_exists ppf construct
    | T_singleton lit -> Literal_value.pp ppf lit
    | T_abstraction abs -> pp_type_abs ~name_of_tvar ~name_of_exists ppf abs
    | T_for_all for_all -> pp_forall ~name_of_tvar ~name_of_exists ppf for_all
    | T_sum row -> Row.PP.sum_type pp (fun _ _ -> ()) ppf row
    | T_union union -> Union.pp pp ppf union
    | T_record row -> Row.PP.record_type pp (fun _ _ -> ()) ppf row)


and pp_construct ~name_of_tvar ~name_of_exists ppf { constructor; parameters; _ } =
  Format.fprintf
    ppf
    "%s%a"
    (Literal_types.to_string constructor)
    (PP_helpers.list_sep_d_par (pp ~name_of_tvar ~name_of_exists))
    parameters


and pp_forall
    ~name_of_tvar
    ~name_of_exists
    ppf
    ({ ty_binder; kind; type_ } : _ Abstraction.t)
    : unit
  =
  Format.fprintf
    ppf
    "∀ %s : %a . %a"
    (name_of_tvar ty_binder)
    Kind.pp
    kind
    (pp ~name_of_tvar ~name_of_exists)
    type_


and pp_type_abs
    ~name_of_tvar
    ~name_of_exists
    ppf
    ({ ty_binder; kind; type_ } : _ Abstraction.t)
    : unit
  =
  Format.fprintf
    ppf
    "funtype %s : %a . %a"
    (name_of_tvar ty_binder)
    Kind.pp
    kind
    (pp ~name_of_tvar ~name_of_exists)
    type_


and bool ppf : unit = Format.fprintf ppf "bool"

and option ~name_of_tvar ~name_of_exists ppf t : unit =
  match get_t_option t with
  | Some t -> Format.fprintf ppf "option (%a)" (pp ~name_of_tvar ~name_of_exists) t
  | None -> Format.fprintf ppf "option ('a)"


let pp_with_name_tbl ~tbl ppf t =
  let name_of_tvar = Type_var_name_tbl.name_of tbl in
  let name_of_exists = Type_var_name_tbl.Exists.name_of in
  pp ~name_of_tvar ~name_of_exists ppf t


(* let name_of tvar = Format.asprintf "%a" Type_var.pp tvar in
  pp ~name_of_tvar:name_of ~name_of_exists:name_of ppf t *)

let pp =
  let name_of tvar = Format.asprintf "%a" Type_var.pp tvar in
  pp ~name_of_tvar:name_of ~name_of_exists:name_of


(* Helpers for generators *)
let t_entrypoint p_ty s_ty =
  let loc = Location.generated in
  t_arrow
    ~loc
    (t_pair ~loc p_ty s_ty ())
    (t_pair ~loc (t_list ~loc (t_operation ~loc ()) ()) s_ty ())
    ()


let t_contract_of p_ty s_ty =
  let loc = Location.generated in
  t_tuple
    ~loc
    [ t_entrypoint p_ty s_ty
    ; t_views ~loc s_ty ()
    ; t_option ~loc (t_big_map ~loc (t_nat ~loc ()) (t_bytes ~loc ()) ()) ()
    ]
    ()


let get_t_inj (t : t) (v : Literal_types.t) : t list option =
  match t.content with
  | T_construct { language = _; constructor; parameters }
    when Literal_types.equal constructor v -> Some parameters
  | _ -> None


let get_t_base_inj (t : t) (v : Literal_types.t) : unit option =
  match get_t_inj t v with
  | Some [] -> Some ()
  | _ -> None


let assert_t_list_operation (t : t) : unit option =
  match get_t_list t with
  | Some t' -> get_t_base_inj t' Literal_types.Operation
  | None -> None


let get_entrypoint ty =
  let ty_eq a b = if equal a b then Some () else None in
  let open Option.Let_syntax in
  let%bind { type1 = parameter; type2; param_names = _ } = get_t_arrow ty in
  let%bind { type1 = storage; type2; param_names = _ } = get_t_arrow type2 in
  let%bind list_op, storage' = get_t_pair type2 in
  let%bind () = assert_t_list_operation list_op in
  let%bind () = ty_eq storage storage' in
  return (parameter, storage)


let dynamic_entrypoint : t -> (t, [> `Not_entry_point_form of t ]) result =
 fun ty ->
  Result.of_option
    Ligo_option.(
      let* p, s = get_entrypoint ty in
      Option.return @@ t_construct ~loc:ty.location Dynamic_entrypoint [ p; s ] ())
    ~error:(`Not_entry_point_form ty)


let parameter_from_entrypoints
    :  (Value_var.t * t) Nonempty_list.t
    -> ( t * t
       , [> `Not_entry_point_form of Value_var.t * t
         | `Storage_does_not_match of Value_var.t * t * Value_var.t * t
         | `Duplicate_entrypoint of Value_var.t
         | `Wrong_dynamic_storage_definition of t
         ] )
       result
  =
 fun entrypoints ->
  let equal_t = equal in
  let open Result.Let_syntax in
  let%bind () =
    (* check entrypoints have no duplicates *)
    entrypoints
    |> Nonempty_list.to_list
    |> List.find_a_dup ~compare:(fun a b -> Value_var.compare (fst a) (fst b))
    |> Option.value_map ~default:(Result.Ok ()) ~f:(fun (x, _ty) ->
           Result.Error (`Duplicate_entrypoint x))
  in
  let ((entrypoint, entrypoint_type) :: rest) = entrypoints in
  let%bind parameter, storage =
    Result.of_option
      (get_entrypoint entrypoint_type)
      ~error:(`Not_entry_point_form (entrypoint, entrypoint_type))
  in
  let%bind parameter_list =
    List.fold_result
      ~init:[ String.capitalize (Value_var.to_name_exn entrypoint), parameter ]
      ~f:(fun parameters (ep, ep_type) ->
        let%bind parameter_, storage_ =
          Result.of_option
            (get_entrypoint ep_type)
            ~error:(`Not_entry_point_form (ep, ep_type))
        in
        let%bind () =
          Result.of_option
            ~error:(`Storage_does_not_match (entrypoint, storage, ep, storage_))
          @@ if equal_t storage_ storage then Some () else None
        in
        return ((String.capitalize (Value_var.to_name_exn ep), parameter_) :: parameters))
      rest
  in
  let%bind () =
    (* If storage as a `dynamic_entrypoints` field, we expect only one extra field `storage` *)
    let opt =
      Ligo_option.(
        let* rows = get_t_record storage in
        let* _ = Row.find_type rows (Label.of_string "dynamic_entrypoints") in
        Option.return rows)
    in
    match opt with
    | None -> return ()
    | Some rows_with_dyns
      when Row.mem rows_with_dyns (Label.of_string "storage")
           && Int.equal (Row.length rows_with_dyns) 2 -> return ()
    | _ -> Error (`Wrong_dynamic_storage_definition storage)
  in
  return
    (t_sum_ez ~loc:Location.generated ~layout:default_layout parameter_list (), storage)


let get_param_names (ty : t) : string list =
  match ty.content with
  | T_arrow arr -> arr.param_names
  | _ -> []


let set_param_names (names : string list) (ty : t) : t =
  match ty.content with
  | T_arrow arr -> { ty with content = T_arrow { arr with param_names = names } }
  | _ -> ty
