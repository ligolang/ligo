open Types
open Ligo_prim

let rec assert_list_eq f a b =
  match a, b with
  | [], [] -> Some ()
  | [], _ -> None
  | _, [] -> None
  | hda :: tla, hdb :: tlb ->
    Simple_utils.Option.(
      let* () = f hda hdb in
      assert_list_eq f tla tlb)


let assert_record_eq f sma smb =
  if Record.equal (fun t1 t2 -> Option.is_some @@ f t1 t2) sma smb then Some () else None


let assert_array_eq f a b =
  if Array_repr.equal (fun t1 t2 -> Option.is_some @@ f t1 t2) a b then Some () else None


(* TODO this was supposed to mean equality of _values_; if
   assert_value_eq (a, b) = Some (), then a and b should be values *)
let rec assert_value_eq a b =
  assert_value_eq_content a.expression_content b.expression_content


and assert_value_eq_content a b =
  match a, b with
  | E_literal a, E_literal b -> Literal_value.assert_eq (a, b)
  | E_constant ca, E_constant cb when Caml.( = ) ca.cons_name cb.cons_name ->
    let lst = List.zip_exn ca.arguments cb.arguments in
    let all = List.map ~f:(fun (a, b) -> assert_value_eq a b) lst in
    if List.exists ~f:Option.is_none all then None else Some ()
  | E_constructor ca, E_constructor cb when Label.equal ca.constructor cb.constructor ->
    assert_value_eq ca.element cb.element
  | ( E_module_accessor { module_path = maa; element = a }
    , E_module_accessor { module_path = mab; element = b } ) ->
    let open Simple_utils.Option in
    let* _ = if Value_var.equal a b then Some () else None in
    assert_list_eq (fun a b -> if Module_var.equal a b then Some () else None) maa mab
  | E_record sma, E_record smb -> assert_record_eq assert_value_eq sma smb
  (* NOTE: E_tuple is made compatible with E_record
      to make it backwards compatible *)
  | E_tuple a, b ->
    let sma = Record.record_of_proper_tuple a in
    assert_value_eq_content (E_record sma) b
  | a, E_tuple b ->
    let smb = Record.record_of_proper_tuple b in
    assert_value_eq_content a (E_record smb)
  (* TODO: records / tuples and array are separated here
      this is likely okay due to this being only used for tests  *)
  | E_array a, E_array b -> assert_array_eq assert_value_eq a b
  | E_array_as_list a, E_array_as_list b -> assert_array_eq assert_value_eq a b
  | E_update ura, E_update urb ->
    (match assert_value_eq ura.struct_ urb.struct_ with
    | None -> None
    | Some () ->
      let aux (a, b) = assert (Label.equal a b) in
      let () = aux (ura.path, urb.path) in
      assert_value_eq ura.update urb.update)
  | E_update _, _ -> None
  | E_ascription a, b -> assert_value_eq_content a.anno_expr.expression_content b
  | a, E_ascription b -> assert_value_eq_content a b.anno_expr.expression_content
  | E_variable _, _
  | E_contract _, _
  | E_lambda _, _
  | E_type_abstraction _, _
  | E_application _, _
  | E_let_in _, _
  | E_let_mut_in _, _
  | E_assign _, _
  | E_for _, _
  | E_for_each _, _
  | E_while _, _
  | E_type_in _, _
  | E_mod_in _, _
  | E_raw_code _, _
  | E_recursive _, _
  | E_accessor _, _
  | E_matching _, _
  | E_module_accessor _, _ -> None
  | E_literal _, _
  | E_constant _, E_constant _
  | E_constant _, _
  | E_constructor _, E_constructor _
  | E_record _, _
  | E_array _, _
  | E_array_as_list _, _
  | E_constructor _, _ -> None


let assert_value_eq (a, b) = assert_value_eq a b
