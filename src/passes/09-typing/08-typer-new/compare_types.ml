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
        -> fail @@ different_types a b
      | _ -> 
        Assert.assert_true (different_types a b) (ca = cb)
    in
    if List.length lsta <> List.length lstb then
      fail @@ different_types a b
    else
      trace (fun _ -> different_types a b)
      @@ bind_list_iter assert_type_expression_eq (List.combine lsta lstb)
  )
  | T_constant _, _ -> fail @@ different_types a b
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
        Assert.assert_list_same_size (different_types a b)
        sa' sb'
      in
      trace (fun _ -> different_types a b) @@
      bind_list_iter aux (List.combine sa' sb')
    )
  | T_sum _, _ -> fail @@ different_types a b
  | T_record ra, T_record rb
       when Helpers.is_tuple_lmap ra <> Helpers.is_tuple_lmap rb -> (
    fail @@ different_types a b
  )
  | T_record ra, T_record rb -> (
      let sort_lmap r' = List.sort (fun (Label a,_) (Label b,_) -> String.compare a b) r' in
      let ra' = sort_lmap @@ LMap.to_kv_list ra in
      let rb' = sort_lmap @@ LMap.to_kv_list rb in
      let aux ((ka, {associated_type=va;_}), (kb, {associated_type=vb;_})) =
        let%bind _ =
          trace (fun _ -> different_types a b) @@
          let Label ka = ka in
          let Label kb = kb in
          Assert.assert_true (different_types a b) (ka = kb) in
        assert_type_expression_eq (va, vb)
      in
      let%bind _ =
        Assert.assert_list_same_size (different_types a b) ra' rb' in
      trace (fun _ -> different_types a b)
      @@ bind_list_iter aux (List.combine ra' rb')

    )
  | T_record _, _ -> fail @@ different_types a b
  | T_arrow {type1;type2}, T_arrow {type1=type1';type2=type2'} ->
      let%bind _ = assert_type_expression_eq (type1, type1') in
      let%bind _ = assert_type_expression_eq (type2, type2') in
      ok ()
  | T_arrow _, _ -> fail @@ different_types a b
  | T_variable x, T_variable y -> let _ = (x = y) in failwith "TODO : we must check that the two types were bound at the same location (even if they have the same name), i.e. use something like De Bruijn indices or a propper graph encoding"
  | T_variable _, _ -> fail @@ different_types a b

(* No information about what made it fail *)
let type_expression_eq ab = Trace.to_bool @@ assert_type_expression_eq ab

