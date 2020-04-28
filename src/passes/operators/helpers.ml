module Typer = struct

  open Trace
  open Ast_typed

  module Errors = struct
    let wrong_param_number = fun name expected got ->
      let title () = "wrong number of params" in
      let full () = Format.asprintf "constant name: %s\nexpected: %d\ngot: %d\n"
          name expected (List.length got) in
      error title full

    let error_uncomparable_types a b () =
      let title () = "these types are not comparable" in
      let message () = "" in
      let data = [
        ("a" , fun () -> Format.asprintf "%a" PP.type_expression a) ;
        ("b" , fun () -> Format.asprintf "%a" PP.type_expression b )
      ] in
      error ~data title message ()
  end
  open Errors

  type type_result = type_expression
  type typer = type_expression list -> type_expression option -> type_result result

  let typer_0 : string -> (type_expression option -> type_expression result) -> typer = fun s f lst tv_opt ->
    match lst with
    | [] -> (
      let%bind tv' = f tv_opt in
      ok (tv')
    )
    | _ -> fail @@ wrong_param_number s 0 lst

  let typer_1 : string -> (type_expression -> type_expression result) -> typer = fun s f lst _ ->
    match lst with
    | [ a ] -> (
        let%bind tv' = f a in
        ok (tv')
      )
    | _ -> fail @@ wrong_param_number s 1 lst

  let typer_1_opt : string -> (type_expression -> type_expression option -> type_expression result) -> typer = fun s f lst tv_opt ->
    match lst with
    | [ a ] -> (
        let%bind tv' = f a tv_opt in
        ok (tv')
      )
    | _ -> fail @@ wrong_param_number s 1 lst

  let typer_2 : string -> (type_expression -> type_expression -> type_expression result) -> typer = fun s f lst _ ->
    match lst with
    | [ a ; b ] -> (
        let%bind tv' = f a b in
        ok (tv')
      )
    | _ -> fail @@ wrong_param_number s 2 lst

  let typer_2_opt : string -> (type_expression -> type_expression -> type_expression option -> type_expression result) -> typer = fun s f lst tv_opt ->
    match lst with
    | [ a ; b ] -> (
        let%bind tv' = f a b tv_opt in
        ok (tv')
      )
    | _ -> fail @@ wrong_param_number s 2 lst

  let typer_3 : string -> (type_expression -> type_expression -> type_expression -> type_expression result) -> typer = fun s f lst _ ->
    match lst with
    | [ a ; b ; c ] -> (
        let%bind tv' = f a b c in
        ok (tv')
      )
    | _ -> fail @@ wrong_param_number s 3 lst

  let typer_4 : string -> (type_expression -> type_expression -> type_expression -> type_expression -> type_expression result) -> typer = fun s f lst _ ->
    match lst with
    | [ a ; b ; c ; d ] -> (
        let%bind tv' = f a b c d in
        ok (tv')
      )
    | _ -> fail @@ wrong_param_number s 4 lst

  let typer_5 : string -> (type_expression -> type_expression -> type_expression -> type_expression -> type_expression -> type_expression result) -> typer = fun s f lst _ ->
    match lst with
    | [ a ; b ; c ; d ; e ] -> (
        let%bind tv' = f a b c d e in
        ok (tv')
      )
    | _ -> fail @@ wrong_param_number s 5 lst

  let typer_6 : string -> (type_expression -> type_expression -> type_expression -> type_expression -> type_expression -> type_expression -> type_expression result) -> typer = fun s f lst _ ->
    match lst with
    | [ a ; b ; c ; d ; e ; f_ ] -> (
        let%bind tv' = f a b c d e f_ in
        ok (tv')
      )
    | _ -> fail @@ wrong_param_number s 6 lst

  let constant' name cst = typer_0 name (fun _ -> ok cst)

  open Combinators

  let eq_1 a cst = type_expression_eq (a , cst)
  let eq_2 (a , b) cst = type_expression_eq (a , cst) && type_expression_eq (b , cst)

  let assert_eq_1 ?msg a b = Assert.assert_true ?msg (eq_1 a b)

  let comparator : string -> typer = fun s -> typer_2 s @@ fun a b ->
    let%bind () =
      trace_strong (error_uncomparable_types a b) @@
      Assert.assert_true @@
      List.exists (eq_2 (a , b)) [
        t_int () ;
        t_nat () ;
        t_bool () ;
        t_mutez () ;
        t_string () ;
        t_bytes () ;
        t_address () ;
        t_timestamp () ;
        t_key_hash () ;
      ] in
    ok @@ t_bool ()

  let boolean_operator_2 : string -> typer = fun s -> typer_2 s @@ fun a b ->
    let%bind () =
      trace_strong (simple_error "A isn't of type bool") @@
      Assert.assert_true @@
      type_expression_eq (t_bool () , a) in
    let%bind () =
      trace_strong (simple_error "B isn't of type bool") @@
      Assert.assert_true @@
      type_expression_eq (t_bool () , b) in
    ok @@ t_bool ()

  module Converter = struct
    open Ast_typed
    open Trace

    let record_checks kvl =
      let%bind () = Assert.assert_true_err
        (simple_error "converted record must have at least two elements")
        (List.length kvl >=2) in
      let all_undefined = List.for_all (fun (_,{field_decl_pos;_}) -> field_decl_pos = 0) kvl in
      let%bind () = Assert.assert_true_err
        (simple_error "can't retrieve declaration order in the converted record, you need to annotate it")
        (not all_undefined) in
      ok ()

    let annotate_field (field:field_content) (ann:string) : field_content =
      {field with michelson_annotation=Some ann}

    let comb (t:type_content) : field_content =
      let field_type = {
        type_content = t ;
        type_meta = None ;
        location = Location.generated ; } in
      {field_type ; michelson_annotation = Some "" ; field_decl_pos = 0}

    let rec to_right_comb_t l new_map =
      match l with
      | [] -> new_map
      | [ (Label ann_l, field_content_l) ; (Label ann_r, field_content_r) ] ->
        LMap.add_bindings [
          (Label "0" , annotate_field field_content_l ann_l) ;
          (Label "1" , annotate_field field_content_r ann_r) ] new_map
      | (Label ann, field)::tl ->
        let new_map' = LMap.add (Label "0") (annotate_field field ann) new_map in
        LMap.add (Label "1") (comb (T_record (to_right_comb_t tl new_map'))) new_map'

    let rec to_left_comb_t' first l new_map =
      match l with
      | [] -> new_map
      | (Label ann_l, field_content_l) :: (Label ann_r, field_content_r) ::tl when first ->
        let new_map' = LMap.add_bindings [
          (Label "0" , annotate_field field_content_l ann_l) ;
          (Label "1" , annotate_field field_content_r ann_r) ] LMap.empty in
        to_left_comb_t' false tl new_map'
      | (Label ann, field)::tl ->
        let new_map' = LMap.add_bindings [
          (Label "0" , comb (T_record new_map)) ;
          (Label "1" , annotate_field field ann ) ;] LMap.empty in
        to_left_comb_t' first tl new_map'
    let to_left_comb_t = to_left_comb_t' true

    let convert_type_to_right_comb l =
      let l' = List.sort (fun (_,{field_decl_pos=a;_}) (_,{field_decl_pos=b;_}) -> Int.compare a b) l in
      T_record (to_right_comb_t l' LMap.empty)

    let convert_type_to_left_comb l =
      let l' = List.sort (fun (_,{field_decl_pos=a;_}) (_,{field_decl_pos=b;_}) -> Int.compare a b) l in
      T_record (to_left_comb_t l' LMap.empty)

    let rec from_right_comb (l:field_content label_map) (size:int) : (field_content list) result =
      let l' = List.rev @@ LMap.to_kv_list l in
      match l' , size with
      | [ (_,l) ; (_,r) ] , 2 -> ok [ l ; r ]
      | [ (_,l) ; (_,{field_type=tr;_}) ], _ ->
        let%bind comb_lmap = get_t_record tr in
        let%bind next = from_right_comb comb_lmap (size-1) in
        ok (l :: next)
      | _ -> simple_fail "Could not convert michelson_right_comb pair to a record"

    let rec from_left_comb (l:field_content label_map) (size:int) : (field_content list) result =
      let l' = List.rev @@ LMap.to_kv_list l in
      match l' , size with
      | [ (_,l) ; (_,r) ] , 2 -> ok [ l ; r ]
      | [ (_,{field_type=tl;_}) ; (_,r) ], _ ->
        let%bind comb_lmap = get_t_record tl in
        let%bind next = from_left_comb comb_lmap (size-1) in
        ok (List.append next [r])
      | _ -> simple_fail "Could not convert michelson_left_comb pair to a record"
    
    let convert_from_right_comb (src: field_content label_map) (dst: field_content label_map) : type_content result =
      let%bind fields = from_right_comb src (LMap.cardinal dst) in
      let labels = List.map (fun (l,_) -> l) @@
        List.sort (fun (_,{field_decl_pos=a;_}) (_,{field_decl_pos=b;_}) -> Int.compare a b ) @@
        LMap.to_kv_list dst in
      ok @@ (T_record (LMap.of_list @@ List.combine labels fields))

    let convert_from_left_comb (src: field_content label_map) (dst: field_content label_map) : type_content result =
      let%bind fields = from_left_comb src (LMap.cardinal dst) in
      let labels = List.map (fun (l,_) -> l) @@
        List.sort (fun (_,{field_decl_pos=a;_}) (_,{field_decl_pos=b;_}) -> Int.compare a b ) @@
        LMap.to_kv_list dst in
      ok @@ (T_record (LMap.of_list @@ List.combine labels fields))

  end

end

module Compiler = struct

  open Tezos_utils.Michelson

  type predicate =
    | Constant of michelson
    | Unary of michelson
    | Binary of michelson
    | Ternary of michelson
    | Tetrary of michelson
    | Pentary of michelson
    | Hexary of michelson
  let simple_constant c = Constant c
  let simple_unary c = Unary c
  let simple_binary c = Binary c
  let simple_ternary c = Ternary c
  let simple_tetrary c = Tetrary c
  let simple_pentary c = Pentary c
  let simple_hexary c = Hexary c
end
