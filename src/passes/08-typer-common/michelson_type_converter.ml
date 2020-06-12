open Errors
open Ast_typed
open Trace

let record_checks kvl loc =
  let%bind () = Assert.assert_true
    (too_small_record loc)
    (List.length kvl >=2) in
  let all_undefined = List.for_all (fun (_,{field_decl_pos;_}) -> field_decl_pos = 0) kvl in
  let%bind () = Assert.assert_true
    (declaration_order_record loc)
    (not all_undefined) in
  ok ()

let variant_checks kvl loc =
  let%bind () = Assert.assert_true
    (too_small_variant loc)
    (List.length kvl >=2) in
  let all_undefined = List.for_all (fun (_,{ctor_decl_pos;_}) -> ctor_decl_pos = 0) kvl in
  let%bind () = Assert.assert_true
    (declaration_order_variant loc)
    (not all_undefined) in
  ok ()

let annotate_field (field:field_content) (ann:string) : field_content =
  {field with michelson_annotation=Some ann}

let annotate_ctor (ctor:ctor_content) (ann:string) : ctor_content =
  {ctor with michelson_annotation=Some ann}

let comb_pair (t:type_content) : field_content =
  let field_type = {
    type_content = t ;
    type_meta = None ;
    location = Location.generated ; } in
  {field_type ; michelson_annotation = Some "" ; field_decl_pos = 0}

let comb_ctor (t:type_content) : ctor_content =
  let ctor_type = {
    type_content = t ;
    type_meta = None ;
    location = Location.generated ; } in
  {ctor_type ; michelson_annotation = Some "" ; ctor_decl_pos = 0}

let rec to_right_comb_pair l new_map =
  match l with
  | [] -> new_map
  | [ (Label ann_l, field_content_l) ; (Label ann_r, field_content_r) ] ->
    LMap.add_bindings [
      (Label "0" , annotate_field field_content_l ann_l) ;
      (Label "1" , annotate_field field_content_r ann_r) ] new_map
  | (Label ann, field)::tl ->
    let new_map' = LMap.add (Label "0") (annotate_field field ann) new_map in
    LMap.add (Label "1") (comb_pair (T_record (to_right_comb_pair tl new_map'))) new_map'

let rec to_right_comb_variant l new_map =
  match l with
  | [] -> new_map
  | [ (Constructor ann_l, field_content_l) ; (Constructor ann_r, field_content_r) ] ->
    CMap.add_bindings [
      (Constructor "M_left" , annotate_ctor field_content_l ann_l) ;
      (Constructor "M_right" , annotate_ctor field_content_r ann_r) ] new_map
  | (Constructor ann, field)::tl ->
    let new_map' = CMap.add (Constructor "M_left") (annotate_ctor field ann) new_map in
    CMap.add (Constructor "M_right") (comb_ctor (T_sum (to_right_comb_variant tl new_map'))) new_map'

let rec to_left_comb_pair' first l new_map =
  match l with
  | [] -> new_map
  | (Label ann_l, field_content_l) :: (Label ann_r, field_content_r) ::tl when first ->
    let new_map' = LMap.add_bindings [
      (Label "0" , annotate_field field_content_l ann_l) ;
      (Label "1" , annotate_field field_content_r ann_r) ] LMap.empty in
    to_left_comb_pair' false tl new_map'
  | (Label ann, field)::tl ->
    let new_map' = LMap.add_bindings [
      (Label "0" , comb_pair (T_record new_map)) ;
      (Label "1" , annotate_field field ann ) ;] LMap.empty in
    to_left_comb_pair' first tl new_map'
let to_left_comb_pair = to_left_comb_pair' true

let rec to_left_comb_variant' first l new_map =
  match l with
  | [] -> new_map
  | (Constructor ann_l, ctor_content_l) :: (Constructor ann_r, ctor_content_r) ::tl when first ->
    let new_map' = CMap.add_bindings [
      (Constructor "M_left" , annotate_ctor ctor_content_l ann_l) ;
      (Constructor "M_right" , annotate_ctor ctor_content_r ann_r) ] CMap.empty in
    to_left_comb_variant' false tl new_map'
  | (Constructor ann, ctor)::tl ->
    let new_map' = CMap.add_bindings [
      (Constructor "M_left" , comb_ctor (T_sum new_map)) ;
      (Constructor "M_right" , annotate_ctor ctor ann ) ;] CMap.empty in
    to_left_comb_variant' first tl new_map'
let to_left_comb_variant = to_left_comb_variant' true

let rec from_right_comb_pair (l:field_content label_map) (size:int) : (field_content list , typer_error) result =
  let l' = List.rev @@ LMap.to_kv_list l in
  match l' , size with
  | [ (_,l) ; (_,r) ] , 2 -> ok [ l ; r ]
  | [ (_,l) ; (_,{field_type=tr;_}) ], _ ->
    let%bind comb_lmap = trace_option (expected_record tr) @@ get_t_record tr in
    let%bind next = from_right_comb_pair comb_lmap (size-1) in
    ok (l :: next)
  | _ -> fail (corner_case "Could not convert michelson_pair_right_comb pair to a record")

let rec from_left_comb_pair (l:field_content label_map) (size:int) : (field_content list , typer_error) result =
  let l' = List.rev @@ LMap.to_kv_list l in
  match l' , size with
  | [ (_,l) ; (_,r) ] , 2 -> ok [ l ; r ]
  | [ (_,{field_type=tl;_}) ; (_,r) ], _ ->
    let%bind comb_lmap = trace_option (expected_record tl) @@ get_t_record tl in
    let%bind next = from_left_comb_pair comb_lmap (size-1) in
    ok (List.append next [r])
  | _ -> fail (corner_case "Could not convert michelson_pair_left_comb pair to a record")

let rec from_right_comb_variant (l:ctor_content constructor_map) (size:int) : (ctor_content list , typer_error) result =
  let l' = List.rev @@ CMap.to_kv_list l in
  match l' , size with
  | [ (_,l) ; (_,r) ] , 2 -> ok [ l ; r ]
  | [ (_,l) ; (_,{ctor_type=tr;_}) ], _ ->
    let%bind comb_cmap = trace_option (expected_variant tr) @@ get_t_sum tr in
    let%bind next = from_right_comb_variant comb_cmap (size-1) in
    ok (l :: next)
  | _ -> fail (corner_case "Could not convert michelson_or right comb to a variant")

let rec from_left_comb_variant (l:ctor_content constructor_map) (size:int) : (ctor_content list , typer_error) result =
  let l' = List.rev @@ CMap.to_kv_list l in
  match l' , size with
  | [ (_,l) ; (_,r) ] , 2 -> ok [ l ; r ]
  | [ (_,{ctor_type=tl;_}) ; (_,r) ], _ ->
    let%bind comb_cmap = trace_option (expected_variant tl) @@ get_t_sum tl in
    let%bind next = from_left_comb_variant comb_cmap (size-1) in
    ok (List.append next [r])
  | _ -> fail (corner_case "Could not convert michelson_or left comb to a record")

let convert_pair_to_right_comb l =
  let l' = List.sort (fun (_,{field_decl_pos=a;_}) (_,{field_decl_pos=b;_}) -> Int.compare a b) l in
  T_record (to_right_comb_pair l' LMap.empty)

let convert_pair_to_left_comb l =
  let l' = List.sort (fun (_,{field_decl_pos=a;_}) (_,{field_decl_pos=b;_}) -> Int.compare a b) l in
  T_record (to_left_comb_pair l' LMap.empty)

let convert_pair_from_right_comb (src: field_content label_map) (dst: field_content label_map) : (type_content , typer_error) result =
  let%bind fields = from_right_comb_pair src (LMap.cardinal dst) in
  let labels = List.map (fun (l,_) -> l) @@
    List.sort (fun (_,{field_decl_pos=a;_}) (_,{field_decl_pos=b;_}) -> Int.compare a b ) @@
    LMap.to_kv_list dst in
  ok @@ (T_record (LMap.of_list @@ List.combine labels fields))

let convert_pair_from_left_comb (src: field_content label_map) (dst: field_content label_map) : (type_content , typer_error) result =
  let%bind fields = from_left_comb_pair src (LMap.cardinal dst) in
  let labels = List.map (fun (l,_) -> l) @@
    List.sort (fun (_,{field_decl_pos=a;_}) (_,{field_decl_pos=b;_}) -> Int.compare a b ) @@
    LMap.to_kv_list dst in
  ok @@ (T_record (LMap.of_list @@ List.combine labels fields))

let convert_variant_to_right_comb l =
  let l' = List.sort (fun (_,{ctor_decl_pos=a;_}) (_,{ctor_decl_pos=b;_}) -> Int.compare a b) l in
  T_sum (to_right_comb_variant l' CMap.empty)

let convert_variant_to_left_comb l =
  let l' = List.sort (fun (_,{ctor_decl_pos=a;_}) (_,{ctor_decl_pos=b;_}) -> Int.compare a b) l in
  T_sum (to_left_comb_variant l' CMap.empty)

let convert_variant_from_right_comb (src: ctor_content constructor_map) (dst: ctor_content constructor_map) : (type_content , typer_error) result =
  let%bind ctors = from_right_comb_variant src (CMap.cardinal dst) in
  let ctors_name = List.map (fun (l,_) -> l) @@
    List.sort (fun (_,{ctor_decl_pos=a;_}) (_,{ctor_decl_pos=b;_}) -> Int.compare a b ) @@
    CMap.to_kv_list dst in
  ok @@ (T_sum (CMap.of_list @@ List.combine ctors_name ctors))

let convert_variant_from_left_comb (src: ctor_content constructor_map) (dst: ctor_content constructor_map) : (type_content , typer_error) result =
  let%bind ctors = from_left_comb_variant src (CMap.cardinal dst) in
  let ctors_name = List.map (fun (l,_) -> l) @@
    List.sort (fun (_,{ctor_decl_pos=a;_}) (_,{ctor_decl_pos=b;_}) -> Int.compare a b ) @@
    CMap.to_kv_list dst in
  ok @@ (T_sum (CMap.of_list @@ List.combine ctors_name ctors))