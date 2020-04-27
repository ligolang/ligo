open Ast_typed
open Trace

let to_sorted_kv_list lmap =
  List.sort (fun (_,{decl_position=a;_}) (_,{decl_position=b;}) -> Int.compare a b) @@
  LMap.to_kv_list lmap

let accessor (record:expression) (path:label) (t:type_expression) =
  { expression_content = E_record_accessor {record; path} ;
    location = Location.generated ;
    type_expression = t ;
    environment = record.environment}

let rec to_left_comb' first prev l conv_map =
  match l with
  | [] -> conv_map 
  | (label_l, {field_type=t_l}) :: (label_r, {field_type=t_r})::tl when first ->
    let exp_l = accessor prev label_l t_l in
    let exp_r = accessor prev label_r t_r in
    let conv_map' = LMap.add_bindings [ (Label "0" , exp_l) ; (Label "1" , exp_r) ] LMap.empty in
    to_left_comb' false prev tl conv_map'
  | (label, {field_type=t})::tl ->
    let conv_map' = LMap.add_bindings [
        (Label "0" , {prev with expression_content = E_record conv_map});
        (Label "1" , accessor prev label t)]
      LMap.empty in
    to_left_comb' first prev tl conv_map'

let to_left_comb = to_left_comb' true

let rec to_right_comb
    (prev:expression)
    (l:(label * field_content) list)
    (conv_map: expression label_map) : expression label_map =
  match l with
  | [] -> conv_map 
  | [ (label_l,{field_type=tl}) ; (label_r,{field_type=tr}) ] ->
    let exp_l = accessor prev label_l tl in
    let exp_r = accessor prev label_r tr in
    LMap.add_bindings [ (Label "0" , exp_l) ; (Label "1" , exp_r) ] conv_map
  | (label,{field_type})::tl ->
    let exp = { expression_content = E_record_accessor {record = prev ; path = label } ;
                location = Location.generated ;
                type_expression = field_type ;
                environment = prev.environment } in
    let conv_map' = LMap.add (Label "0") exp conv_map in
    LMap.add (Label "1") ({exp with expression_content = E_record (to_right_comb prev tl conv_map')}) conv_map'

let rec from_right_comb
    (prev:expression) 
    (src_lmap: field_content label_map)
    (dst_kvl:(label * field_content) list)
    (conv_map:expression label_map) : expression label_map =
  match dst_kvl with
  | (label , {field_type;_}) :: (_::_ as tl) ->
    let intermediary_type = LMap.find (Label "1") src_lmap in
    let src_lmap' = match intermediary_type.field_type.type_content with
      | T_record a -> a
      | _ -> src_lmap in
    let conv_map' = LMap.add label (accessor prev (Label "0") field_type) conv_map in
    let next = accessor prev (Label "1") intermediary_type.field_type in
    from_right_comb next src_lmap' tl conv_map'
  | [(label,_)] -> LMap.add label prev conv_map
  | [] -> conv_map

let rec from_left_comb'
    (prev:expression) 
    (src_lmap: field_content label_map)
    (dst_kvl:(label * field_content) list)
    (conv_map:expression label_map) : expression label_map =
  match dst_kvl with
  | (label , {field_type;_}) :: (_::_ as tl) ->
    let intermediary_type = LMap.find (Label "0") src_lmap in
    let src_lmap' = match intermediary_type.field_type.type_content with
      | T_record a -> a
      | _ -> src_lmap in
    let conv_map' = LMap.add label (accessor prev (Label "1") field_type) conv_map in
    let next = accessor prev (Label "0") intermediary_type.field_type in
    from_left_comb' next src_lmap' tl conv_map'
  | [(label,_)] -> LMap.add label prev conv_map
  | [] -> conv_map

let from_left_comb prev src_lmap dst_kvl conv_map =
  from_left_comb' prev src_lmap (List.rev dst_kvl) conv_map

(**
  converts pair/record of a given layout to record/pair to another
  - foo = (a,(b,(c,d))) -> foo_converted = { a=foo.0 ; b=foo.1.0 ; c=foo.1.1.0 ; d=foo.1.1.1 }
**)
let peephole_expression : expression -> expression result = fun e ->
  let return expression_content = ok { e with expression_content } in
  match e.expression_content with
  | E_constant {cons_name= (C_CONVERT_TO_LEFT_COMB);
                arguments= [ to_convert ] } ->
    let%bind src_lmap = get_t_record to_convert.type_expression in
    let src_kvl = to_sorted_kv_list src_lmap in
    return @@ E_record (to_left_comb to_convert src_kvl LMap.empty)
  | E_constant {cons_name= (C_CONVERT_TO_RIGHT_COMB);
                arguments= [ to_convert ] } ->
    let%bind src_lmap = get_t_record to_convert.type_expression in
    let src_kvl = to_sorted_kv_list src_lmap in
    return @@ E_record (to_right_comb to_convert src_kvl LMap.empty)
  | E_constant {cons_name= (C_CONVERT_FROM_RIGHT_COMB);
                arguments= [ to_convert ] } ->
    let%bind dst_lmap = get_t_record e.type_expression in
    let%bind src_lmap = get_t_record to_convert.type_expression in
    let dst_kvl = to_sorted_kv_list dst_lmap in
    return @@ E_record (from_right_comb to_convert src_lmap dst_kvl LMap.empty)
  | E_constant {cons_name= (C_CONVERT_FROM_LEFT_COMB);
                arguments= [ to_convert ] } ->
    let%bind dst_lmap = get_t_record e.type_expression in
    let%bind src_lmap = get_t_record to_convert.type_expression in
    let dst_kvl = to_sorted_kv_list dst_lmap in
    return @@ E_record (from_left_comb to_convert src_lmap dst_kvl LMap.empty)
  | _ as e -> return e
