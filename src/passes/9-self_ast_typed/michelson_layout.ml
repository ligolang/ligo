open Ast_typed
open Trace

let to_sorted_kv_list_l lmap =
  List.sort (fun (_,{field_decl_pos=a;_}) (_,{field_decl_pos=b;}) -> Int.compare a b) @@
  LMap.to_kv_list lmap

let to_sorted_kv_list_c lmap =
  List.sort (fun (_,{ctor_decl_pos=a;_}) (_,{ctor_decl_pos=b;}) -> Int.compare a b) @@
  CMap.to_kv_list lmap

let accessor (record:expression) (path:label) (t:type_expression) =
  { expression_content = E_record_accessor {record; path} ;
    location = Location.generated ;
    type_expression = t ;
    environment = record.environment }

let constructor (constructor:constructor') (element:expression) (t:type_expression) =
  { expression_content = E_constructor { constructor ; element } ;
    location = Location.generated ;
    type_expression = t ;
    environment = element.environment }

let match_var env (t:type_expression) =
  { expression_content = E_variable (Var.of_name "x") ;
    location = Location.generated ;
    type_expression = t ;
    environment = env }

let rec to_left_comb_record' first prev l conv_map =
  match l with
  | [] -> conv_map 
  | (label_l, {field_type=t_l}) :: (label_r, {field_type=t_r})::tl when first ->
    let exp_l = accessor prev label_l t_l in
    let exp_r = accessor prev label_r t_r in
    let conv_map' = LMap.add_bindings [ (Label "0" , exp_l) ; (Label "1" , exp_r) ] LMap.empty in
    to_left_comb_record' false prev tl conv_map'
  | (label, {field_type=t})::tl ->
    let conv_map' = LMap.add_bindings [
        (Label "0" , {prev with expression_content = E_record conv_map});
        (Label "1" , accessor prev label t)]
      LMap.empty in
    to_left_comb_record' first prev tl conv_map'
let to_left_comb_record = to_left_comb_record' true

let rec to_right_comb_variant' (i:int) (e:expression) (dst_lmap:ctor_content constructor_map) (src_kvl:(constructor' * ctor_content) list) : expression list =
  let rec descend_types lmap i =
    if i > 0 then
      let {ctor_type;_} = CMap.find (Constructor "M_right") lmap in
      match ctor_type.type_content with
        | T_sum a -> ctor_type::(descend_types a (i-1)) 
        | _ -> []
    else [] in
  let intermediary_types i =  if i = 0 then [] else e.type_expression::(descend_types dst_lmap i) in
  let rec comb (ctor_type,outer) l =
    let env' = Environment.add_ez_binder (Var.of_name "x") ctor_type e.environment in
    match l with
    | [] -> constructor outer (match_var env' ctor_type) e.type_expression
    | [t] -> constructor outer (match_var env' ctor_type) t
    | t::tl -> constructor (Constructor "M_right") (comb (ctor_type,outer) tl) t in
  ( match src_kvl with
    | [] -> []
    | (_,{ctor_type;_})::[] ->
      let combs_t = intermediary_types (i-1) in
      [comb (ctor_type,Constructor "M_right") combs_t]
    | (_,{ctor_type;_})::tl ->
      let combs_t = intermediary_types i in
      (comb (ctor_type,Constructor "M_left") combs_t) :: to_right_comb_variant' (i+1) e dst_lmap tl )
let to_right_comb_variant = to_right_comb_variant' 0

let rec to_left_comb_variant' (i:int) (e:expression) (dst_lmap:ctor_content constructor_map) (src_kvl:(constructor' * ctor_content) list) : expression list =
  let rec descend_types lmap i =
    if i > 0 then
      let {ctor_type;_} = CMap.find (Constructor "M_left") lmap in
      match ctor_type.type_content with
        | T_sum a -> ctor_type::(descend_types a (i-1)) 
        | _ -> []
    else [] in
  let intermediary_types i =  if i = 0 then [] else e.type_expression::(descend_types dst_lmap i) in
  let rec comb (ctor_type,outer) l =
    let env' = Environment.add_ez_binder (Var.of_name "x") ctor_type e.environment in
    match l with
    | [] -> constructor outer (match_var env' ctor_type) e.type_expression
    | [t] -> constructor outer (match_var env' ctor_type) t
    | t::tl -> constructor (Constructor "M_left") (comb (ctor_type,outer) tl) t in
  ( match src_kvl with
    | [] -> []
    | (_,{ctor_type;_})::[] ->
      let combs_t = intermediary_types (i-1) in
      [comb (ctor_type,Constructor "M_left") combs_t]
    | (_,{ctor_type;_})::tl ->
      let combs_t = intermediary_types i in
      (comb (ctor_type,Constructor "M_right") combs_t) :: to_left_comb_variant' (i+1) e dst_lmap tl )
let to_left_comb_variant a b c = List.rev @@ to_left_comb_variant' 0 a b (List.rev c)

let rec to_right_comb_record
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
    LMap.add (Label "1") ({exp with expression_content = E_record (to_right_comb_record prev tl conv_map')}) conv_map'

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
  | E_constant {cons_name= (C_CONVERT_TO_LEFT_COMB);arguments= [ to_convert ] } -> (
    match to_convert.type_expression.type_content with
      | T_record src_lmap ->
        let src_kvl = to_sorted_kv_list_l src_lmap in
        return @@ E_record (to_left_comb_record to_convert src_kvl LMap.empty)
      | T_sum src_cmap ->
        let%bind dst_cmap = get_t_sum e.type_expression in
        let src_kvl = to_sorted_kv_list_c src_cmap in
        let bodies = to_left_comb_variant e dst_cmap src_kvl in
        let to_cases ((constructor,{ctor_type=_;_}),body) =
          let pattern = (Var.of_name "x") in
          {constructor ; pattern ; body }
        in
        let cases = Match_variant {
          cases = List.map to_cases @@ (List.combine src_kvl bodies) ;
          tv = to_convert.type_expression }
        in
        return @@ E_matching {matchee = to_convert ; cases}
      | _ -> return e.expression_content
  )
  | E_constant {cons_name= (C_CONVERT_TO_RIGHT_COMB);arguments= [ to_convert ] } -> (
    match to_convert.type_expression.type_content with
      | T_record src_lmap ->
        let src_kvl = to_sorted_kv_list_l src_lmap in
        return @@ E_record (to_right_comb_record to_convert src_kvl LMap.empty)
      | T_sum src_cmap ->
        let%bind dst_cmap = get_t_sum e.type_expression in
        let src_kvl = to_sorted_kv_list_c src_cmap in
        let bodies = to_right_comb_variant e dst_cmap src_kvl in
        let to_cases ((constructor,{ctor_type=_;_}),body) =
          let pattern = (Var.of_name "x") in
          {constructor ; pattern ; body }
        in
        let cases = Match_variant {
          cases = List.map to_cases @@ (List.combine src_kvl bodies) ;
          tv = to_convert.type_expression }
        in
        return @@ E_matching {matchee = to_convert ; cases}
      | _ -> return e.expression_content
  )
  | E_constant {cons_name= (C_CONVERT_FROM_RIGHT_COMB);
                arguments= [ to_convert ] } ->
    let%bind dst_lmap = get_t_record e.type_expression in
    let%bind src_lmap = get_t_record to_convert.type_expression in
    let dst_kvl = to_sorted_kv_list_l dst_lmap in
    return @@ E_record (from_right_comb to_convert src_lmap dst_kvl LMap.empty)
  | E_constant {cons_name= (C_CONVERT_FROM_LEFT_COMB);
                arguments= [ to_convert ] } ->
    let%bind dst_lmap = get_t_record e.type_expression in
    let%bind src_lmap = get_t_record to_convert.type_expression in
    let dst_kvl = to_sorted_kv_list_l dst_lmap in
    return @@ E_record (from_left_comb to_convert src_lmap dst_kvl LMap.empty)
  | _ as e -> return e