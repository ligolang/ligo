module Location = Simple_utils.Location
module List = Simple_utils.List
module Ligo_string = Simple_utils.Ligo_string
open Simple_utils
open Types
open Ligo_prim

(* TODO: does that need to be cleaned-up ? *)
module Free_variables = struct
  type bindings = Value_var.t list

  let mem : bindings -> Value_var.t -> bool = List.mem ~equal:Value_var.equal
  let singleton : Value_var.t -> bindings = fun s -> [ s ]
  let union : bindings -> bindings -> bindings = ( @ )
  let unions : bindings list -> bindings = List.concat
  let empty : bindings = []

  let rec expression_content : bindings -> expression_content -> bindings =
   fun b ec ->
    let self = expression b in
    match ec with
    | E_lambda l -> lambda b l
    | E_literal _ -> empty
    | E_constant { arguments; _ } -> unions @@ List.map ~f:self arguments
    | E_variable name ->
      (match mem b name with
      | true -> empty
      | false -> singleton name)
    | E_application { lamb; args } -> unions @@ List.map ~f:self [ lamb; args ]
    | E_constructor { element; _ } -> self element
    | E_record m -> unions @@ List.map ~f:self @@ Record.LMap.to_list m
    | E_accessor { struct_; _ } -> self struct_
    | E_update { struct_; update; _ } -> union (self struct_) @@ self update
    | E_matching { matchee; cases; _ } ->
      union (self matchee) (matching_expression b cases)
    | E_let_in { let_binder; rhs; let_result; _ } ->
      let b' =
        List.fold
          (Types.Pattern.binders let_binder)
          ~f:(fun b pb -> union (Binder.apply singleton pb) b)
          ~init:b
      in
      union (expression b' let_result) (self rhs)
    | E_type_abstraction { type_binder = _; result } -> self result
    | E_mod_in { module_binder = _; rhs = _; let_result } -> self let_result
    | E_raw_code _ -> empty
    | E_type_inst { type_ = _; forall } -> self forall
    | E_recursive { fun_name; lambda; _ } ->
      let b' = union (singleton fun_name) b in
      expression_content b' @@ E_lambda lambda
    | E_module_accessor _ -> empty
    | E_let_mut_in { rhs; let_result; _ } -> union (self let_result) (self rhs)
    | E_assign { expression = e; _ } -> expression b e
    | E_deref _ -> empty
    | E_for { binder; start; final; incr; f_body } ->
      let b' = union (singleton binder) b in
      unions [ self start; self final; expression b' incr; expression b' f_body ]
    | E_for_each { fe_binder = binder, None; collection; fe_body; collection_type = _ } ->
      unions [ self collection; expression (union (singleton binder) b) fe_body ]
    | E_for_each { fe_binder = binder, Some binder'; collection; fe_body; _ } ->
      let b' = union [ binder; binder' ] b in
      unions [ self collection; expression b' fe_body ]
    | E_while { cond; body } -> union (self cond) (self body)


  and lambda : bindings -> (expr, ty_expr) Lambda.t -> bindings =
   fun b l ->
    let b' = union (singleton (Param.get_var l.binder)) b in
    expression b' l.result


  and expression : bindings -> expression -> bindings =
   fun b e -> expression_content b e.expression_content


  and matching_case
      :  (bindings -> expression -> bindings) -> bindings -> _ Types.Match_expr.match_case
      -> bindings
    =
   fun f b m ->
    let b' = Types.Pattern.binders m.pattern |> List.map ~f:Binder.get_var in
    let bs = union b b' in
    f bs m.body


  and matching
      :  (bindings -> expression -> bindings) -> bindings
      -> _ Types.Match_expr.match_case list -> bindings
    =
   fun f b ms -> List.fold_left ms ~init:b ~f:(matching_case f)


  and matching_expression x = matching expression x
end

let assert_eq a b = if Caml.( = ) a b then Some () else None
let assert_same_size a b = if List.length a = List.length b then Some () else None

let rec assert_list_eq f a b =
  match a, b with
  | [], [] -> Some ()
  | [], _ -> None
  | _, [] -> None
  | hda :: tla, hdb :: tlb ->
    Option.(
      let* () = f hda hdb in
      assert_list_eq f tla tlb)


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
  | T_sum sa, T_sum sb ->
    let sa' = Record.LMap.to_kv_list_rev sa.fields in
    let sb' = Record.LMap.to_kv_list_rev sb.fields in
    let aux
        ( (ka, ({ associated_type = va; _ } : row_element))
        , (kb, ({ associated_type = vb; _ } : row_element)) )
      =
      let* _ = assert_eq ka kb in
      assert_type_expression_eq (va, vb)
    in
    let* _ = assert_same_size sa' sb' in
    let* _ = assert_eq sa.layout sb.layout in
    List.fold_left
      ~f:(fun acc p ->
        match acc with
        | None -> None
        | Some () -> aux p)
      ~init:(Some ())
      (List.zip_exn sa' sb')
  | T_sum _, _ -> None
  | T_record ra, T_record rb
    when Bool.( <> ) (Record.is_tuple ra.fields) (Record.is_tuple rb.fields) -> None
  | T_record ra, T_record rb ->
    let sort_lmap r' = List.sort ~compare:(fun (a, _) (b, _) -> Label.compare a b) r' in
    let ra' = sort_lmap @@ Record.LMap.to_kv_list_rev ra.fields in
    let rb' = sort_lmap @@ Record.LMap.to_kv_list_rev rb.fields in
    let aux
        ( (ka, ({ associated_type = va; _ } : row_element))
        , (kb, ({ associated_type = vb; _ } : row_element)) )
      =
      let* _ = assert_eq ka kb in
      assert_type_expression_eq (va, vb)
    in
    let* _ = assert_eq ra.layout rb.layout in
    let* _ = assert_same_size ra' rb' in
    let* _ = assert_eq ra.layout rb.layout in
    List.fold_left
      ~f:(fun acc p ->
        match acc with
        | None -> None
        | Some () -> aux p)
      ~init:(Some ())
      (List.zip_exn ra' rb')
  | T_record _, _ -> None
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
  | Literal_chest a, Literal_chest b when Bytes.equal a b -> Some ()
  | Literal_chest _, Literal_chest _ -> None
  | Literal_chest _, _ -> None
  | Literal_chest_key a, Literal_chest_key b when Bytes.equal a b -> Some ()
  | Literal_chest_key _, Literal_chest_key _ -> None
  | Literal_chest_key _, _ -> None


let get_entry (lst : program) (name : Value_var.t) : expression option =
  let aux x =
    match Location.unwrap x with
    | D_value
        { binder
        ; expr
        ; attr =
            { inline = _; no_mutation = _; view = _; public = _; hidden = _; thunk = _ }
        } -> if Binder.apply (Value_var.equal name) binder then Some expr else None
    | D_irrefutable_match _ | D_type _ | D_module _ -> None
  in
  List.find_map ~f:aux (List.rev lst)


let get_type_of_contract ty =
  match ty with
  | T_arrow { type1; type2 } ->
    (match type1.type_content, type2.type_content with
    | T_record tin, T_record tout
      when Record.is_tuple tin.fields && Record.is_tuple tout.fields ->
      let open Simple_utils.Option in
      let* parameter, storage = Combinators.get_t_pair type1 in
      let* listop, storage' = Combinators.get_t_pair type2 in
      let* () = Combinators.assert_t_list_operation listop in
      let* () = assert_type_expression_eq (storage, storage') in
      (* TODO: on storage/parameter : asert_storable, assert_passable ? *)
      return (parameter, storage)
    | _ -> None)
  | _ -> None
