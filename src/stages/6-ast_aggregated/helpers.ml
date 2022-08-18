open Types
open Stage_common

let kv_list_of_t_sum ?(layout = Layout.L_tree) (m: row_element Record.t) =
  let lst = Record.LMap.to_kv_list m in
  match layout with
  | L_tree -> lst
  | L_comb -> (
      let aux (_ , ({ associated_type = _ ; decl_pos = a ; _ }: row_element)) (_ , ({ associated_type = _ ; decl_pos = b ; _ } : row_element)) = Int.compare a b in
      List.sort ~compare:aux lst
    )

let kv_list_of_t_record_or_tuple ?(layout = Layout.L_tree) (m: row_element Record.t) =
  let lst =
    if (Record.is_tuple m)
    then Record.tuple_of_record m
    else Record.LMap.to_kv_list m
  in
  match layout with
  | L_tree -> lst
  | L_comb -> (
      let aux (_ , ({ associated_type = _ ; decl_pos = a ; _ }: row_element)) (_ , ({ associated_type = _ ; decl_pos = b ; _ } : row_element)) = Int.compare a b in
      List.sort ~compare:aux lst
    )

let kv_list_of_record_or_tuple ~layout record_t_content record =
  let exps =
    if (Record.is_tuple record)
    then Record.tuple_of_record record
    else Record.LMap.to_kv_list record
  in
  match (layout : Layout.t) with
  | L_tree -> List.map ~f:snd exps
  | L_comb -> (
    let types = if (Record.is_tuple record)
                then Record.tuple_of_record record_t_content
                else Record.LMap.to_kv_list record_t_content in
    let te = List.map ~f:(fun ((label_t,t),(label_e,e)) ->
      assert (Label.equal label_t label_e) ; (*TODO TEST*)
      (t,e)) (List.zip_exn types exps) in
    let s = List.sort ~compare:(fun (({ associated_type = _ ; decl_pos = a ; _ }: row_element),_) ({ associated_type = _ ; decl_pos = b ; _ },_) -> Int.compare a b) te in
    List.map ~f:snd s
  )

let remove_empty_annotation (ann : string option) : string option =
  match ann with
  | Some "" -> None
  | Some ann -> Some ann
  | None -> None

let is_michelson_or (t: _ Record.t) =
  let s = List.sort ~compare:(fun (k1, _) (k2, _) -> Label.compare k1 k2) @@
    Record.LMap.to_kv_list t in
  match s with
  | [ (Label "M_left", ta) ; (Label "M_right", tb) ] -> Some (ta,tb)
  | _ -> None

let is_michelson_pair (t: row_element Record.t) : (row_element * row_element) option =
  match Record.LMap.to_list t with
  | [ a ; b ] -> (
      if List.for_all ~f:(fun i -> Record.LMap.mem i t) @@ (Label.range 0 2)
      && Option.(
        is_some a.michelson_annotation || is_some b.michelson_annotation
      )
      then Some (a , b)
      else None
    )
  | _ -> None

(* This function parse te and replace all occurence of binder by value *)
let rec subst_type (binder : TypeVar.t) (value : type_expression) (te : type_expression) =
  let self = subst_type binder value in
  let return type_content = {te with type_content} in
  match te.type_content with
    T_variable var when TypeVar.equal binder var -> value
  | T_variable  _ -> te
  | T_constant  {language;injection;parameters} ->
      let parameters = List.map ~f:self parameters in
      return @@ T_constant {language;injection;parameters}
  | T_singleton _ -> te
  | T_arrow {type1;type2} ->
      let type1 = self type1 in
      let type2 = self type2 in
      return @@ T_arrow {type1;type2}
  | T_sum m -> (
    let aux ({associated_type;michelson_annotation;decl_pos} : row_element) =
      let associated_type = self associated_type in
      ({associated_type;michelson_annotation;decl_pos} : row_element)
    in
    return @@ T_sum { m with fields = Record.map aux m.fields }
  )
  | T_record m -> (
    let aux ({associated_type;michelson_annotation;decl_pos} : row_element) =
      let associated_type = self associated_type in
      ({associated_type;michelson_annotation;decl_pos} : row_element)
    in
    return @@ T_record { m with fields = Record.map aux m.fields }
  )
  | T_for_all {ty_binder;kind;type_} ->
    let type_ = self type_ in
    return @@ T_for_all {ty_binder;kind;type_}

(* This function transforms a type `∀ v1 ... vn . t` into the pair `([ v1 ; .. ; vn ] , t)` *)
let destruct_for_alls (t : type_expression) =
  let rec destruct_for_alls type_vars (t : type_expression) = match t.type_content with
    | T_for_all { ty_binder ; type_ ; _ } ->
       destruct_for_alls (ty_binder :: type_vars) type_
    | _ -> (type_vars, t)
  in destruct_for_alls [] t

(* This function transforms a type `t1 -> ... -> tn -> t` into the pair `([ t1 ; .. ; tn ] , t)` *)
let destruct_arrows_n (t : type_expression) (n : int) =
  let rec destruct_arrows type_vars (t : type_expression) = match t.type_content with
    | T_arrow { type1 ; type2 } when List.length type_vars < n ->
       destruct_arrows (type1 :: type_vars) type2
    | _ -> (List.rev type_vars, t)
  in destruct_arrows [] t

(* This function transforms a type `t1 -> ... -> tn -> t` into the pair `([ t1 ; .. ; tn ] , t)` *)
let destruct_arrows (t : type_expression) =
  let rec destruct_arrows type_vars (t : type_expression) = match t.type_content with
    | T_arrow { type1 ; type2 } ->
       destruct_arrows (type1 :: type_vars) type2
    | _ -> (List.rev type_vars, t)
  in destruct_arrows [] t

let assert_eq = fun a b -> if Caml.(=) a b then Some () else None
let assert_same_size = fun a b -> if (List.length a = List.length b) then Some () else None

(* ~unforged_tickets allows type containing tickets to be decompiled to 'unforged' tickets (e.g. `int * int ticket` |-> `int * {ticketer : address ; value : int ; amount : nat }
   TODO: we could think of a better way to inject those "comparison expeptions" to assert_type_expression `*)
let rec assert_type_expression_eq ?(unforged_tickets=false)(a, b: (type_expression * type_expression)) : unit option =
  let open Simple_utils.Option in
  match (a.type_content, b.type_content) with
  | T_constant {language=_;injection=Stage_common.Literal_types.Ticket ;parameters=[_ty]} , _human_t | _human_t , T_constant {language=_;injection=Stage_common.Literal_types.Ticket;parameters=[_ty]} -> (
    if unforged_tickets then
      Some ()
    else
      None
  )
  | T_constant {language=la;injection=ia;parameters=lsta}, T_constant {language=lb;injection=ib;parameters=lstb} -> (
    if (String.equal la lb) && (Literal_types.equal ia ib) then (
      let* _ = assert_same_size lsta lstb in
      List.fold_left ~f:(fun acc p -> match acc with | None -> None | Some () -> assert_type_expression_eq ~unforged_tickets p) ~init:(Some ()) (List.zip_exn lsta lstb)
    ) else
      None
  )
  | T_constant _, _ -> None
  | T_sum sa, T_sum sb -> (
      let sa' = Record.LMap.to_kv_list_rev sa.fields in
      let sb' = Record.LMap.to_kv_list_rev sb.fields in
      let aux ((ka, ({associated_type=va;_} : row_element)), (kb, ({associated_type=vb;_} : row_element))) =
        let* _ = assert_eq ka kb in
        assert_type_expression_eq ~unforged_tickets (va, vb)
      in
      let* _ = assert_same_size sa' sb' in
      List.fold_left ~f:(fun acc p -> match acc with | None -> None | Some () -> aux p) ~init:(Some ()) (List.zip_exn sa' sb')
    )
  | T_sum _, _ -> None
  | T_record ra, T_record rb
       when Bool.(<>) (Record.is_tuple ra.fields) (Record.is_tuple rb.fields) -> None
  | T_record ra, T_record rb -> (
      let sort_lmap r' = List.sort ~compare:(fun (a,_) (b,_) -> Label.compare a b) r' in
      let ra' = sort_lmap @@ Record.LMap.to_kv_list_rev ra.fields in
      let rb' = sort_lmap @@ Record.LMap.to_kv_list_rev rb.fields in
      let aux ((ka, ({associated_type=va;_}: row_element)), (kb, ({associated_type=vb;_}: row_element))) =
        let* _ = assert_eq ka kb in
        assert_type_expression_eq ~unforged_tickets (va, vb)
      in
      let* _ = assert_eq ra.layout rb.layout in
      let* _ = assert_same_size ra' rb' in
      List.fold_left ~f:(fun acc p -> match acc with | None -> None | Some () -> aux p) ~init:(Some ()) (List.zip_exn ra' rb')

    )
  | T_record _, _ -> None
  | T_arrow {type1;type2}, T_arrow {type1=type1';type2=type2'} ->
    let* _ = assert_type_expression_eq ~unforged_tickets (type1, type1') in
    assert_type_expression_eq ~unforged_tickets (type2, type2')
  | T_arrow _, _ -> None
  | T_variable x, T_variable y ->
     (* TODO : we must check that the two types were bound at the same location (even if they have the same name), i.e. use something like De Bruijn indices or a propper graph encoding *)
     if TypeVar.equal x y then Some () else None
  | T_variable _, _ -> None
  | T_singleton a , T_singleton b -> assert_literal_eq (a , b)
  | T_singleton _ , _ -> None
  | T_for_all a , T_for_all b ->
    assert_type_expression_eq ~unforged_tickets (a.type_, b.type_) >>= fun _ ->
    Some (assert (Kind.equal a.kind b.kind))
  | T_for_all _ , _ -> None

and type_expression_eq ab = Option.is_some @@ assert_type_expression_eq ab


and assert_literal_eq (a, b : Literal_value.t * Literal_value.t) : unit option =
  if Literal_value.equal a b then Some () else None
