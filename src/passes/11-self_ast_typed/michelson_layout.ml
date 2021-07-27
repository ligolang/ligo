open Errors
open Ast_typed
open Trace

let to_sorted_kv_list_l (lmap : row_element label_map) =
  List.sort ~compare:(fun (_,({decl_pos=a;_}:row_element)) (_,({decl_pos=b;}:row_element)) -> Int.compare a b) @@
  LMap.to_kv_list_rev lmap

let accessor (record:expression) (path:label) (t:type_expression) =
  { expression_content = E_record_accessor {record; path} ;
    location = Location.generated ;
    type_expression = t ;
  }

let constructor (constructor:label) (element:expression) (t:type_expression) =
  { expression_content = E_constructor { constructor ; element } ;
    location = Location.generated ;
    type_expression = t ;
  }

let match_var (t:type_expression) =
  { expression_content = E_variable (Location.wrap @@ Var.of_name "x") ;
    location = Location.generated ;
    type_expression = t ;
  }

let matching (e:expression) matchee cases =
  { expression_content = E_matching {matchee ; cases};
    location = Location.generated ;
    type_expression = e.type_expression ;
  }

let rec descend_types s lmap i =
  if i > 0 then
    let {associated_type;_} = LMap.find (Label s) lmap in
    match associated_type.type_content with
      | T_sum {content ; layout=_ } -> associated_type::(descend_types s content (i-1))
      | _ -> []
  else []

let rec to_left_comb_record' first prev l conv_map =
  match l with
  | [] -> conv_map
  | (label_l, {associated_type=t_l}) :: (label_r, {associated_type=t_r})::tl when first ->
    let exp_l = accessor prev label_l t_l in
    let exp_r = accessor prev label_r t_r in
    let conv_map' = LMap.add_bindings [ (Label "0" , exp_l) ; (Label "1" , exp_r) ] LMap.empty in
    to_left_comb_record' false prev tl conv_map'
  | (label, {associated_type=t})::tl ->
    let conv_map' = LMap.add_bindings [
        (Label "0" , {prev with expression_content = E_record conv_map});
        (Label "1" , accessor prev label t)]
      LMap.empty in
    to_left_comb_record' first prev tl conv_map'
let to_left_comb_record = to_left_comb_record' true

let rec right_comb_variant_combination' (i:int) (e:expression) (dst_lmap:_ row_element_mini_c label_map) (src_kvl:(label * ty_expr row_element_mini_c) list) : expression list =
  let intermediary_types i =  if i = 0 then [] else e.type_expression::(descend_types "M_right" dst_lmap i) in
  let rec comb (ctor_type,outer) l =
    match l with
    | [] -> constructor outer (match_var ctor_type) e.type_expression
    | [t] -> constructor outer (match_var ctor_type) t
    | t::tl -> constructor (Label "M_right") (comb (ctor_type,outer) tl) t in
  ( match src_kvl with
    | [] -> []
    | (_,{associated_type;_})::[] ->
      let combs_t = intermediary_types (i-1) in
      [comb (associated_type,Label "M_right") combs_t]
    | (_,{associated_type;_})::tl ->
      let combs_t = intermediary_types i in
      (comb (associated_type,Label "M_left") combs_t) :: right_comb_variant_combination' (i+1) e dst_lmap tl )
let right_comb_variant_combination = right_comb_variant_combination' 0

let rec left_comb_variant_combination' (i:int) (e:expression) (dst_lmap:ty_expr row_element_mini_c label_map) (src_kvl:(label * ty_expr row_element_mini_c) list) : expression list =
  let intermediary_types i =  if i = 0 then [] else e.type_expression::(descend_types "M_left" dst_lmap i) in
  let rec comb (ctor_type,outer) l =
    match l with
    | [] -> constructor outer (match_var ctor_type) e.type_expression
    | [t] -> constructor outer (match_var ctor_type) t
    | t::tl -> constructor (Label "M_left") (comb (ctor_type,outer) tl) t in
  ( match src_kvl with
    | [] -> []
    | (_,{associated_type;_})::[] ->
      let combs_t = intermediary_types (i-1) in
      [comb (associated_type,Label "M_left") combs_t]
    | (_,{associated_type;_})::tl ->
      let combs_t = intermediary_types i in
      (comb (associated_type,Label "M_right") combs_t) :: left_comb_variant_combination' (i+1) e dst_lmap tl )
let left_comb_variant_combination a b c = List.rev @@ left_comb_variant_combination' 0 a b (List.rev c)

let make_row t =
 {
    associated_type = t;
    michelson_annotation = None ;
    decl_pos = 0 ;
}

let rec to_right_comb_record
    (prev:expression)
    (l:(label * row_element) list)
    (conv_map: expression label_map) : expression label_map * type_expression =
  match l with
  | [] -> conv_map, prev.type_expression
  | [ (label_l,tl) ; (label_r,tr) ] ->
    let exp_l = accessor prev label_l tl.associated_type in
    let exp_r = accessor prev label_r tr.associated_type in
    let lmap = LMap.add_bindings [ (Label "0" , exp_l) ; (Label "1" , exp_r) ] conv_map in
    let t = ez_t_record [ (Label "0", tl) ; (Label "1", tr) ] in
    (lmap,t)
  | (label,t)::tl ->
    let exp = make_e (E_record_accessor {record = prev ; path = label }) t.associated_type in
    let conv_map' = LMap.add (Label "0") exp conv_map in
    let (lmap, ty) = to_right_comb_record prev tl conv_map' in
    let int_t = ez_t_record [ (Label "0", t) ; (Label "1", make_row ty) ] in
    LMap.add (Label "1") (make_e (E_record lmap) ty) conv_map', int_t

let rec from_right_comb_record
    (prev:expression)
    (src_lmap:ty_expr row_element_mini_c label_map)
    (dst_kvl:(label * ty_expr row_element_mini_c) list)
    (conv_map:expression label_map) : expression label_map =
  match dst_kvl with
  | (label , {associated_type;_}) :: (_::_ as tl) ->
    let intermediary_type = LMap.find (Label "1") src_lmap in
    let src_lmap' = match intermediary_type.associated_type.type_content with
      | T_record a -> a.content
      | _ -> src_lmap in
    let conv_map' = LMap.add label (accessor prev (Label "0") associated_type) conv_map in
    let next = accessor prev (Label "1") intermediary_type.associated_type in
    from_right_comb_record next src_lmap' tl conv_map'
  | [(label,_)] -> LMap.add label prev conv_map
  | [] -> conv_map

let rec from_left_comb_record
    (prev:expression)
    (src_lmap:ty_expr row_element_mini_c label_map)
    (dst_kvl:(label * ty_expr row_element_mini_c) list)
    (conv_map:expression label_map) : expression label_map =
  match dst_kvl with
  | (label , {associated_type;_}) :: (_::_ as tl) ->
    let intermediary_type = LMap.find (Label "0") src_lmap in
    let src_lmap' = match intermediary_type.associated_type.type_content with
      | T_record a -> a.content
      | _ -> src_lmap in
    let conv_map' = LMap.add label (accessor prev (Label "1") associated_type) conv_map in
    let next = accessor prev (Label "0") intermediary_type.associated_type in
    from_left_comb_record next src_lmap' tl conv_map'
  | [(label,_)] -> LMap.add label prev conv_map
  | [] -> conv_map
let from_left_comb prev src_lmap dst_kvl conv_map =
  from_left_comb_record prev src_lmap (List.rev dst_kvl) conv_map

let rec from_right_comb_or ~raise (to_convert:expression) (e:expression) (matchee_t,bodies) : expression =
  match matchee_t , bodies with
  | [m] , bl::br::[] ->
    let cases = [
      { constructor = Label "M_left" ;
        pattern = Location.wrap @@ Var.of_name "x";
        body = bl } ;
      { constructor = Label "M_right" ;
        pattern = Location.wrap @@ Var.of_name "x";
        body = br } ] in
    matching e m (Match_variant { cases ; tv = to_convert.type_expression })
  | m::mtl , b::btl ->
    let body = from_right_comb_or ~raise to_convert e (mtl,btl) in
    let cases = [
      { constructor = Label "M_left" ;
        pattern = Location.wrap @@ Var.of_name "x";
        body = b } ;
      { constructor = Label "M_right" ;
        pattern = Location.wrap @@ Var.of_name "x";
        body } ] in
    matching e m (Match_variant { cases ; tv = to_convert.type_expression })
  | _ -> raise.raise @@ corner_case "from_right_comb conversion"

let rec from_left_comb_or ~raise (to_convert:expression) (e:expression) (matchee_t,bodies) : expression =
  match matchee_t , bodies with
  | [m] , bl::br::[] ->
    let cases = [
      { constructor = Label "M_right" ;
        pattern = Location.wrap @@ Var.of_name "x";
        body = bl } ;
      { constructor = Label "M_left" ;
        pattern = Location.wrap @@ Var.of_name "x";
        body = br } ] in
    matching e m (Match_variant { cases ; tv = to_convert.type_expression })
  | m::mtl , b::btl ->
    let body = from_left_comb_or ~raise to_convert e (mtl,btl) in
    let cases = [
      { constructor = Label "M_right" ;
        pattern = Location.wrap @@ Var.of_name "x";
        body = b } ;
      { constructor = Label "M_left" ;
        pattern = Location.wrap @@ Var.of_name "x";
        body } ] in
    matching e m (Match_variant { cases ; tv = to_convert.type_expression })
  | _ -> raise.raise @@ corner_case "from_left_comb conversion"

(**
  converts pair/record of a given layout to record/pair to another
  - foo = (a,(b,(c,d))) -> foo_converted = { a=foo.0 ; b=foo.1.0 ; c=foo.1.1.0 ; d=foo.1.1.1 }
  - foo = M_left(a) -> foo_converted = match foo with M_left x -> Foo x | M_right x -> Bar x
**)
let peephole_expression ~raise : expression -> expression = fun e ->
  let return expression_content = { e with expression_content } in
  match e.expression_content with
  | E_constant {cons_name= (C_CONVERT_TO_LEFT_COMB);arguments= [ to_convert ] } -> (
    match to_convert.type_expression.type_content with
      | T_record {content=src_lmap;_} ->
        let src_kvl = to_sorted_kv_list_l src_lmap in
        return @@ E_record (to_left_comb_record to_convert src_kvl LMap.empty)
      | T_sum {content ; _ } ->
        let dst_cmap = trace_option ~raise (corner_case "to_left_comb conversion") @@ get_t_sum e.type_expression in
        let src_kvl = to_sorted_kv_list_l content in
        let bodies = left_comb_variant_combination e dst_cmap.content src_kvl in
        let to_cases ((constructor,{associated_type=_;_}),body) =
          let pattern = Location.wrap @@ Var.of_name "x" in
          {constructor ; pattern ; body }
        in
        let cases = Match_variant {
          cases = List.map ~f:to_cases @@ (List.zip_exn src_kvl bodies) ;
          tv = to_convert.type_expression }
        in
        return @@ E_matching {matchee = to_convert ; cases}
      | _ -> return e.expression_content
  )
  | E_constant {cons_name= (C_CONVERT_TO_RIGHT_COMB);arguments= [ to_convert ] } -> (
    match to_convert.type_expression.type_content with
      | T_record {content=src_lmap;_} ->
        let src_kvl = to_sorted_kv_list_l src_lmap in
        return @@ E_record ( fst @@ to_right_comb_record to_convert src_kvl LMap.empty)
      | T_sum {content ; _ } ->
        let dst_cmap = trace_option ~raise (corner_case "to_right_comb conversion") @@ get_t_sum e.type_expression in
        let src_kvl = to_sorted_kv_list_l content in
        let bodies = right_comb_variant_combination e dst_cmap.content src_kvl in
        let to_cases ((constructor,{associated_type=_;_}),body) =
          let pattern = Location.wrap @@ Var.of_name "x" in
          {constructor ; pattern ; body }
        in
        let cases = Match_variant {
          cases = List.map ~f:to_cases @@ (List.zip_exn src_kvl bodies) ;
          tv = to_convert.type_expression }
        in
        return @@ E_matching {matchee = to_convert ; cases}
      | _ -> return e.expression_content
  )
  | E_constant {cons_name= (C_CONVERT_FROM_RIGHT_COMB); arguments= [ to_convert ] } -> (
    match to_convert.type_expression.type_content with
      | T_record {content=src_lmap;_} ->
        let dst_lmap = trace_option ~raise (corner_case "from_right_comb conversion") @@ get_t_record e.type_expression in
        let dst_kvl = to_sorted_kv_list_l dst_lmap.content in
        return @@ E_record (from_right_comb_record to_convert src_lmap dst_kvl LMap.empty)
      | T_sum src_lmap ->
        let dst_lmap = trace_option ~raise (corner_case "from_right_comb conversion") @@ get_t_sum e.type_expression in
        let dst_kvl = to_sorted_kv_list_l dst_lmap.content in
        let intermediary_types i = descend_types "M_right" src_lmap.content i in
        let matchee = to_convert :: (List.map ~f:(fun t -> match_var t) @@ intermediary_types ((List.length dst_kvl)-2)) in
        let bodies = List.map
          ~f:(fun (ctor , {associated_type;_}) -> constructor ctor (match_var associated_type) e.type_expression)
          dst_kvl in
        let match_expr = from_right_comb_or ~raise to_convert e (matchee,bodies) in
        return match_expr.expression_content
      | _ -> return e.expression_content
  )
  | E_constant {cons_name= (C_CONVERT_FROM_LEFT_COMB); arguments= [ to_convert ] } -> (
    match to_convert.type_expression.type_content with
      | T_record {content=src_lmap;_} ->
        let dst_lmap = trace_option ~raise (corner_case "from_left_comb conversion") @@ get_t_record e.type_expression in
        let dst_kvl = to_sorted_kv_list_l dst_lmap.content in
        return @@ E_record (from_left_comb to_convert src_lmap dst_kvl LMap.empty)
      | T_sum src_lmap ->
        let dst_lmap = trace_option ~raise (corner_case "from_left_comb conversion") @@  get_t_sum e.type_expression in
        let dst_kvl = to_sorted_kv_list_l dst_lmap.content in
        let intermediary_types i = descend_types "M_left" src_lmap.content i in
        let matchee = to_convert :: (List.map ~f:(fun t -> match_var t) @@ intermediary_types ((List.length dst_kvl)-2)) in
        let bodies = List.map
          ~f:(fun (ctor , {associated_type;_}) -> constructor ctor (match_var associated_type) e.type_expression)
          (List.rev dst_kvl) in
        let match_expr = from_left_comb_or ~raise to_convert e (matchee,bodies) in
        return match_expr.expression_content
      | _ -> return e.expression_content
  )
  | _ as e -> return e
