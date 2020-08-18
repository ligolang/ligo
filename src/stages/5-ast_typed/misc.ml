open Types

module Free_variables = struct

  type bindings = expression_variable list
  let var_compare = Location.compare_content ~compare:Var.compare
  let mem : expression_variable -> bindings -> bool = List.mem ~compare:var_compare
  let singleton : expression_variable -> bindings = fun s -> [ s ]
  let union : bindings -> bindings -> bindings = (@)
  let unions : bindings list -> bindings = List.concat
  let empty : bindings = []
  let of_list : expression_variable list -> bindings = fun x -> x

  let rec expression_content : bindings -> expression_content -> bindings = fun b ec ->
    let self = expression b in
    match ec with
    | E_lambda l -> lambda b l
    | E_literal _ -> empty
    | E_constant {arguments;_} -> unions @@ List.map self arguments
    | E_variable name -> (
        match mem name b with
        | true -> empty
        | false -> singleton name
      )
    | E_application {lamb;args} -> unions @@ List.map self [ lamb ; args ]
    | E_constructor {element;_} -> self element
    | E_record m -> unions @@ List.map self @@ LMap.to_list m
    | E_record_accessor {record;_} -> self record
    | E_record_update {record; update;_} -> union (self record) @@ self update
    | E_matching {matchee; cases;_} -> union (self matchee) (matching_expression b cases)
    | E_let_in { let_binder; rhs; let_result; _} ->
      let b' = union (singleton let_binder) b in
      union
        (expression b' let_result)
        (self rhs)
    | E_raw_code _ -> empty
    | E_recursive {fun_name;lambda;_} ->
      let b' = union (singleton fun_name) b in
      expression_content b' @@ E_lambda lambda

  and lambda : bindings -> lambda -> bindings = fun b l ->
    let b' = union (singleton l.binder) b in
    expression b' l.result

  and expression : bindings -> expression -> bindings = fun b e ->
    expression_content b e.expression_content

  and matching_variant_case : (bindings -> expression -> bindings) -> bindings -> matching_content_case -> bindings  = fun f b { constructor=_ ; pattern ; body } ->
    f (union (singleton pattern) b) body

  and matching : (bindings -> expression -> bindings) -> bindings -> matching_expr -> bindings = fun f b m ->
    match m with
    | Match_list { match_nil = n ; match_cons = {hd; tl; body; tv=_} } -> union (f b n) (f (union (of_list [hd ; tl]) b) body)
    | Match_option { match_none = n ; match_some = {opt; body; tv=_} } -> union (f b n) (f (union (singleton opt) b) body)
    | Match_variant { cases ; tv=_ } -> unions @@ List.map (matching_variant_case f b) cases

  and matching_expression = fun x -> matching expression x

end


let assert_eq = fun a b -> if (a = b) then Some () else None
let assert_same_size = fun a b -> if (List.length a = List.length b) then Some () else None

let rec assert_type_expression_eq (a, b: (type_expression * type_expression)) : unit option =
  let open Option in
  match (a.type_content, b.type_content) with
  | T_wildcard, _ -> Some ()
  | _, T_wildcard -> Some ()
  | T_constant {type_constant=ca;arguments=la}, T_constant {type_constant=cb;arguments=lb} -> (
    let aux = fun lsta lstb ->
      if List.length lsta <> List.length lstb then None
      else
        List.fold_left
          (fun acc (a,b) -> 
            match acc with | None -> None | Some () -> assert_type_expression_eq (a,b))
          (Some ())
          (List.combine lsta lstb) in
    match (ca, cb) with
      | TC_option, TC_option
      | TC_list, TC_list
      | TC_contract, TC_contract
      | TC_set, TC_set
      | (TC_map | TC_map_or_big_map) , (TC_map | TC_map_or_big_map)
      | (TC_big_map | TC_map_or_big_map ), (TC_big_map | TC_map_or_big_map) ->
        aux la lb
      | (TC_option | TC_list | TC_contract | TC_set | TC_map | TC_big_map | TC_map_or_big_map | TC_michelson_pair|TC_michelson_or|TC_michelson_pair_right_comb | TC_michelson_pair_left_comb|TC_michelson_or_right_comb| TC_michelson_or_left_comb ),
        (TC_option | TC_list | TC_contract | TC_set | TC_map | TC_big_map | TC_map_or_big_map | TC_michelson_pair|TC_michelson_or|TC_michelson_pair_right_comb | TC_michelson_pair_left_comb|TC_michelson_or_right_comb| TC_michelson_or_left_comb )
        -> None
      | _ -> assert_eq ca cb
  )
  | T_constant _, _ -> None
  | T_sum sa, T_sum sb -> (
      let sa' = LMap.to_kv_list sa in
      let sb' = LMap.to_kv_list sb in
      let aux ((ka, {associated_type=va;_}), (kb, {associated_type=vb;_})) =
        assert_eq ka kb >>= fun _ ->
          assert_type_expression_eq (va, vb)
      in
      assert_same_size sa' sb' >>= fun _ ->
      List.fold_left (fun acc p -> match acc with | None -> None | Some () -> aux p) (Some ()) (List.combine sa' sb')
    )
  | T_sum _, _ -> None
  | T_record ra, T_record rb
       when Helpers.is_tuple_lmap ra <> Helpers.is_tuple_lmap rb -> None
  | T_record ra, T_record rb -> (
      let sort_lmap r' = List.sort (fun (Label a,_) (Label b,_) -> String.compare a b) r' in
      let ra' = sort_lmap @@ LMap.to_kv_list ra in
      let rb' = sort_lmap @@ LMap.to_kv_list rb in
      let aux ((ka, {associated_type=va;_}), (kb, {associated_type=vb;_})) =
        let Label ka = ka in
        let Label kb = kb in
        assert_eq ka kb >>= fun _ ->
        assert_type_expression_eq (va, vb)
      in
      assert_same_size ra' rb' >>= fun _ ->
      List.fold_left (fun acc p -> match acc with | None -> None | Some () -> aux p) (Some ()) (List.combine ra' rb')

    )
  | T_record _, _ -> None
  | T_arrow {type1;type2}, T_arrow {type1=type1';type2=type2'} ->
    assert_type_expression_eq (type1, type1') >>= fun _ ->
    assert_type_expression_eq (type2, type2')
  | T_arrow _, _ -> None
  | T_variable x, T_variable y -> let _ = (x = y) in failwith "TODO : we must check that the two types were bound at the same location (even if they have the same name), i.e. use something like De Bruijn indices or a propper graph encoding"
  | T_variable _, _ -> None

let type_expression_eq ab = Option.is_some @@ assert_type_expression_eq ab

let assert_literal_eq (a, b : literal * literal) : unit option =
  match (a, b) with
  | Literal_int a, Literal_int b when a = b -> Some ()
  | Literal_int _, Literal_int _ -> None
  | Literal_int _, _ -> None
  | Literal_nat a, Literal_nat b when a = b -> Some ()
  | Literal_nat _, Literal_nat _ -> None
  | Literal_nat _, _ -> None
  | Literal_timestamp a, Literal_timestamp b when a = b -> Some ()
  | Literal_timestamp _, Literal_timestamp _ -> None
  | Literal_timestamp _, _ -> None
  | Literal_mutez a, Literal_mutez b when a = b -> Some ()
  | Literal_mutez _, Literal_mutez _ -> None
  | Literal_mutez _, _ -> None
  | Literal_string a, Literal_string b when a = b -> Some ()
  | Literal_string _, Literal_string _ -> None
  | Literal_string _, _ -> None
  | Literal_bytes a, Literal_bytes b when a = b -> Some ()
  | Literal_bytes _, Literal_bytes _ -> None
  | Literal_bytes _, _ -> None
  | Literal_unit, Literal_unit -> Some ()
  | Literal_unit, _ -> None
  | Literal_address a, Literal_address b when a = b -> Some ()
  | Literal_address _, Literal_address _ -> None
  | Literal_address _, _ -> None
  | Literal_signature a, Literal_signature b when a = b -> Some ()
  | Literal_signature _, Literal_signature _ -> None
  | Literal_signature _, _ -> None
  | Literal_key a, Literal_key b when a = b -> Some ()
  | Literal_key _, Literal_key _ -> None
  | Literal_key _, _ -> None
  | Literal_key_hash a, Literal_key_hash b when a = b -> Some ()
  | Literal_key_hash _, Literal_key_hash _ -> None
  | Literal_key_hash _, _ -> None
  | Literal_chain_id a, Literal_chain_id b when a = b -> Some ()
  | Literal_chain_id _, Literal_chain_id _ -> None
  | Literal_chain_id _, _ -> None
  | Literal_operation _, Literal_operation _ -> None
  | Literal_operation _, _ -> None


let rec assert_value_eq (a, b: (expression*expression)) : unit option =
  let open Option in
  match (a.expression_content, b.expression_content) with
  | E_literal a, E_literal b ->
      assert_literal_eq (a, b)
  | E_constant {cons_name=ca;arguments=lsta}, E_constant {cons_name=cb;arguments=lstb} when ca = cb -> (
    assert_same_size lsta lstb >>= fun _ -> 
    List.fold_left (fun acc p -> match acc with | None -> None | Some () -> assert_value_eq p) (Some ()) (List.combine lsta lstb)
  )
  | E_constant _, E_constant _ -> None
  | E_constant _, _ -> None
  | E_constructor {constructor=ca;element=a}, E_constructor {constructor=cb;element=b} when ca = cb -> (
      assert_value_eq (a, b)
  )
  | E_constructor _, E_constructor _ -> None
  | E_constructor _, _ -> None
  | E_record sma, E_record smb -> (
      let aux (Label _k) a b =
        match a, b with
        | Some a, Some b -> assert_value_eq (a, b)
        | _              -> None
      in
      let all = LMap.merge aux sma smb in
      if    ((LMap.cardinal all) = (LMap.cardinal sma)) 
         || ((LMap.cardinal all) = (LMap.cardinal smb)) then
        Some ()
      else None
    )
  | E_record _, _
  | (E_literal _, _) | (E_variable _, _) | (E_application _, _)
  | (E_lambda _, _) | (E_let_in _, _) | (E_raw_code _, _) | (E_recursive _, _)
  | (E_record_accessor _, _) | (E_record_update _,_)
  | (E_matching _, _)
  -> None

let merge_annotation (a:type_expression option) (b:type_expression option) assert_eq_fun : type_expression option =
  let open Option in
  match a, b with
  | None, None -> None
  | Some a, None -> Some a
  | None, Some b -> Some b
  | Some a, Some b ->
      assert_eq_fun (a, b) >>= fun _ ->
      match a.type_meta, b.type_meta with
      | _, None -> Some a
      | _, Some _ -> Some b

let get_entry (lst : program) (name : string) : expression option =
  let aux x =
    match Location.unwrap x with
    | Declaration_constant { binder ; expr ; inline=_ } -> (
      if Var.equal binder.wrap_content (Var.of_name name)
      then Some expr
      else None
    )
    | Declaration_type _ -> None
  in
  List.find_map aux lst

let equal_variables a b : bool =
  match a.expression_content, b.expression_content with
  | E_variable a, E_variable b -> Var.equal a.wrap_content b.wrap_content
  |  _, _ -> false

let p_constant (p_ctor_tag : constant_tag) (p_ctor_args : p_ctor_args) = {
  tsrc = "misc.ml/p_constant" ;
  t = P_constant {
      p_ctor_tag : constant_tag ;
      p_ctor_args : p_ctor_args ;
    }
}

let p_row (p_row_tag : row_tag) (p_row_args : tv_lmap ) = {
  tsrc = "misc.ml/p_constant" ;
  t = P_row {
      p_row_tag ;
      p_row_args ;
    }
}

let p_row_ez (p_row_tag : row_tag) (p_row_args : (string * type_value) list ) =
  let p_row_args = LMap.of_list @@ List.map (fun (x,y) -> Label x,y) p_row_args in
  p_row p_row_tag p_row_args

let c_equation aval bval reason = { c = C_equation { aval ; bval }; reason }

let reason_simpl : type_constraint_simpl -> string = function
  | SC_Constructor { reason_constr_simpl=reason; _ }
  | SC_Row { reason_row_simpl=reason; _ }
  | SC_Alias { reason_alias_simpl=reason; _ }
  | SC_Poly { reason_poly_simpl=reason; _ }
  | SC_Typeclass { reason_typeclass_simpl=reason; _ }
  -> reason
