open Ast_typed
open Trace
open Typer_common.Errors

let rec assert_type_expression_eq (a, b: (type_expression * type_expression)) : (unit, typer_error) result = match (a.type_content, b.type_content) with
  | T_wildcard , _ -> ok ()
  | _ , T_wildcard -> ok ()
  | T_constant {type_constant=ca;arguments=lsta}, T_constant {type_constant=cb;arguments=lstb} -> (
    let%bind _ = match (ca, cb) with
      | TC_option, TC_option
      | TC_list, TC_list
      | TC_contract, TC_contract
      | TC_set, TC_set 
      | (TC_map | TC_map_or_big_map), (TC_map | TC_map_or_big_map)
      | (TC_big_map | TC_map_or_big_map), (TC_big_map | TC_map_or_big_map)
        -> ok @@ () 
      | (TC_option | TC_list | TC_contract | TC_set | TC_map | TC_big_map | TC_michelson_or | TC_michelson_or_left_comb | TC_michelson_or_right_comb | TC_michelson_pair | TC_michelson_pair_left_comb | TC_michelson_pair_right_comb | TC_map_or_big_map ),
        (TC_option | TC_list | TC_contract | TC_set | TC_map | TC_big_map | TC_michelson_or | TC_michelson_or_left_comb | TC_michelson_or_right_comb | TC_michelson_pair | TC_michelson_pair_left_comb | TC_michelson_pair_right_comb | TC_map_or_big_map )
        -> fail @@ different_constants ca cb
      | _ -> 
        Assert.assert_true (different_constants ca cb) (ca = cb)
    in
    if List.length lsta <> List.length lstb then
      fail @@ different_type_constant_number_of_arguments ca cb (List.length lsta) (List.length lstb)
    else
      trace (different_types "arguments to type type_constants" a b)
      @@ bind_list_iter (fun (a,b) -> assert_type_expression_eq (a,b) )(List.combine lsta lstb)
  )
  | T_constant _, _ -> fail @@ different_kinds a b
  | T_sum sa, T_sum sb -> (
      let sa' = LMap.to_kv_list sa in
      let sb' = LMap.to_kv_list sb in
      let aux ((ka, {associated_type=va;_}), (kb, {associated_type=vb;_})) =
        let%bind _ =
          Assert.assert_true (corner_case "different keys in sum types")
          @@ (ka = kb) in
        assert_type_expression_eq (va, vb)
      in
      let%bind _ =
        Assert.assert_list_same_size (different_size_sums a b)
        sa' sb'
      in
      trace (different_types "sum type" a b) @@
      bind_list_iter aux (List.combine sa' sb')
    )
  | T_sum _, _ -> fail @@ different_kinds a b
  | T_record ra, T_record rb
       when Helpers.is_tuple_lmap ra <> Helpers.is_tuple_lmap rb -> (
    fail @@ different_kind_record_tuple a b ra rb
  )
  | T_record ra, T_record rb -> (
      let sort_lmap r' = List.sort (fun (Label a,_) (Label b,_) -> String.compare a b) r' in
      let ra' = sort_lmap @@ LMap.to_kv_list ra in
      let rb' = sort_lmap @@ LMap.to_kv_list rb in
      let aux ((ka, {associated_type=va;_}), (kb, {associated_type=vb;_})) =
        let%bind _ =
          trace (different_types "records" a b) @@
          let Label ka = ka in
          let Label kb = kb in
          Assert.assert_true (different_props_in_record a b ra rb ka kb) (ka = kb) in
        assert_type_expression_eq (va, vb)
      in
      let%bind _ =
        Assert.assert_list_same_size (different_size_records_tuples a b ra rb) ra' rb' in
      trace (different_types "record type" a b)
      @@ bind_list_iter aux (List.combine ra' rb')

    )
  | T_record _, _ -> fail @@ different_kinds a b
  | T_arrow {type1;type2}, T_arrow {type1=type1';type2=type2'} ->
      let%bind _ = assert_type_expression_eq (type1, type1') in
      let%bind _ = assert_type_expression_eq (type2, type2') in
      ok ()
  | T_arrow _, _ -> fail @@ different_kinds a b
  | T_variable x, T_variable y -> let _ = (x = y) in failwith "TODO : we must check that the two types were bound at the same location (even if they have the same name), i.e. use something like De Bruijn indices or a propper graph encoding"
  | T_variable _, _ -> fail @@ different_kinds a b

(* No information about what made it fail *)
let type_expression_eq ab = Trace.to_bool @@ assert_type_expression_eq ab

let assert_literal_eq (a, b : literal * literal) : (unit, typer_error) result =
  match (a, b) with
  | Literal_int a, Literal_int b when a = b -> ok ()
  | Literal_int _, Literal_int _ -> fail @@ different_literals "different ints" a b
  | Literal_int _, _ -> fail @@ different_literals_because_different_types "int vs non-int" a b
  | Literal_nat a, Literal_nat b when a = b -> ok ()
  | Literal_nat _, Literal_nat _ -> fail @@ different_literals "different nats" a b
  | Literal_nat _, _ -> fail @@ different_literals_because_different_types "nat vs non-nat" a b
  | Literal_timestamp a, Literal_timestamp b when a = b -> ok ()
  | Literal_timestamp _, Literal_timestamp _ -> fail @@ different_literals "different timestamps" a b
  | Literal_timestamp _, _ -> fail @@ different_literals_because_different_types "timestamp vs non-timestamp" a b
  | Literal_mutez a, Literal_mutez b when a = b -> ok ()
  | Literal_mutez _, Literal_mutez _ -> fail @@ different_literals "different tezs" a b
  | Literal_mutez _, _ -> fail @@ different_literals_because_different_types "tez vs non-tez" a b
  | Literal_string a, Literal_string b when a = b -> ok ()
  | Literal_string _, Literal_string _ -> fail @@ different_literals "different strings" a b
  | Literal_string _, _ -> fail @@ different_literals_because_different_types "string vs non-string" a b
  | Literal_bytes a, Literal_bytes b when a = b -> ok ()
  | Literal_bytes _, Literal_bytes _ -> fail @@ different_literals "different bytes" a b
  | Literal_bytes _, _ -> fail @@ different_literals_because_different_types "bytes vs non-bytes" a b
  | Literal_unit, Literal_unit -> ok ()
  | Literal_unit, _ -> fail @@ different_literals_because_different_types "unit vs non-unit" a b
  | Literal_address a, Literal_address b when a = b -> ok ()
  | Literal_address _, Literal_address _ -> fail @@ different_literals "different addresss" a b
  | Literal_address _, _ -> fail @@ different_literals_because_different_types "address vs non-address" a b
  | Literal_signature a, Literal_signature b when a = b -> ok ()
  | Literal_signature _, Literal_signature _ -> fail @@ different_literals "different signature" a b
  | Literal_signature _, _ -> fail @@ different_literals_because_different_types "signature vs non-signature" a b
  | Literal_key a, Literal_key b when a = b -> ok ()
  | Literal_key _, Literal_key _ -> fail @@ different_literals "different key" a b
  | Literal_key _, _ -> fail @@ different_literals_because_different_types "key vs non-key" a b
  | Literal_key_hash a, Literal_key_hash b when a = b -> ok ()
  | Literal_key_hash _, Literal_key_hash _ -> fail @@ different_literals "different key_hash" a b
  | Literal_key_hash _, _ -> fail @@ different_literals_because_different_types "key_hash vs non-key_hash" a b
  | Literal_chain_id a, Literal_chain_id b when a = b -> ok ()
  | Literal_chain_id _, Literal_chain_id _ -> fail @@ different_literals "different chain_id" a b
  | Literal_chain_id _, _ -> fail @@ different_literals_because_different_types "chain_id vs non-chain_id" a b
  | Literal_operation _, Literal_operation _ -> fail @@ error_uncomparable_literals "can't compare operations" a b
  | Literal_operation _, _ -> fail @@ different_literals_because_different_types "operation vs non-operation" a b


let rec assert_value_eq (a, b: (expression*expression)) : (unit, typer_error) result =
  trace compare_tracer @@
  match (a.expression_content, b.expression_content) with
  | E_literal a, E_literal b ->
      assert_literal_eq (a, b)
  | E_constant {cons_name=ca;arguments=lsta}, E_constant {cons_name=cb;arguments=lstb} when ca = cb -> (
      let%bind lst =
        generic_try (corner_case "constants with different number of elements")
          (fun () -> List.combine lsta lstb) in
      let%bind _all = bind_list @@ List.map assert_value_eq lst in
      ok ()
    )
  | E_constant _, E_constant _ ->
      fail @@ different_values "constants" a b
  | E_constant _, _ ->
      fail @@ (corner_case "comparing constant with other stuff")

  | E_constructor {constructor=ca;element=a}, E_constructor {constructor=cb;element=b} when ca = cb -> (
      let%bind _eq = assert_value_eq (a, b) in
      ok ()
    )
  | E_constructor _, E_constructor _ ->
      fail @@ different_values "constructors" a b
  | E_constructor _, _ ->
      fail @@ different_values_because_different_types "constructor vs. non-constructor" a b
  | E_record sma, E_record smb -> (
      let aux (Label k) a b =
        match a, b with
        | Some a, Some b -> Some (assert_value_eq (a, b))
        | _              -> Some (fail @@ missing_key_in_record_value k)
      in
      let%bind _all = Helpers.bind_lmap @@ LMap.merge aux sma smb in
      ok ()
    )
  | E_record _, _ ->
      fail @@ (different_values_because_different_types "record vs. non-record" a b)

  | (E_literal _, _) | (E_variable _, _) | (E_application _, _)
  | (E_lambda _, _) | (E_let_in _, _) | (E_raw_code _, _) | (E_recursive _, _)
  | (E_record_accessor _, _) | (E_record_update _,_)
  | (E_matching _, _)
  -> fail @@ error_uncomparable_values "can't compare sequences nor loops" a b
