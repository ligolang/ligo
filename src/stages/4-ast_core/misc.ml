open Types
open Ligo_prim

let rec assert_list_eq f = fun a b -> match (a,b) with
  | [], [] -> Some ()
  | [], _  -> None
  | _ , [] -> None
  | hda::tla, hdb::tlb -> Simple_utils.Option.(
    let* () = f hda hdb in
    assert_list_eq f tla tlb
  )


(* TODO this was supposed to mean equality of _values_; if
   assert_value_eq (a, b) = Some (), then a and b should be values *)
let rec assert_value_eq (a, b: (expression * expression )) : unit option =
  match (a.expression_content , b.expression_content) with
  | E_literal a , E_literal b ->
    Literal_value.assert_eq (a, b)
  | E_constant (ca) , E_constant (cb) when Caml.(=) ca.cons_name cb.cons_name -> (
      let lst = List.zip_exn ca.arguments cb.arguments in
      let all = List.map ~f:assert_value_eq lst in
      if List.exists ~f:(Option.is_none) all then None else Some ()
    )
  | E_constructor (ca), E_constructor (cb) when Caml.(=) ca.constructor cb.constructor -> (
      assert_value_eq (ca.element, cb.element)
    )
  | E_module_accessor {module_path=maa;element=a}, E_module_accessor {module_path=mab;element=b} -> (
    let open Simple_utils.Option in
    let* _ = if Value_var.equal a b then Some () else None in
    assert_list_eq (fun a b -> if Module_var.equal a b then Some () else None) maa mab
  )
  | E_record sma, E_record smb -> (
      let aux _ a b =
        match a, b with
        | Some a, Some b -> assert_value_eq (a, b)
        | _ -> None
      in
      let all = Record.LMap.merge aux sma smb in
      if    ((Record.LMap.cardinal all) = (Record.LMap.cardinal sma))
         || ((Record.LMap.cardinal all) = (Record.LMap.cardinal smb)) then
        Some ()
      else None
    )
  | E_update ura, E_update urb -> (
    match assert_value_eq (ura.struct_, urb.struct_) with
    | None -> None
    | Some () ->
      let aux (a, b) =
        assert (Label.equal a b)
      in
      let () = aux (ura.path, urb.path) in
      assert_value_eq (ura.update,urb.update)
  )
  | E_update _, _ -> None
  | (E_ascription a ,  _b') -> assert_value_eq (a.anno_expr , b)
  | (_a' , E_ascription b) -> assert_value_eq (a , b.anno_expr)
  | (E_variable _, _) | (E_lambda _, _) | (E_type_abstraction _, _)

  | (E_application _, _) | (E_let_in _, _) | (E_let_mut_in _, _) | (E_assign _, _)
  | (E_for _, _) | (E_for_each _, _) | (E_while _, _)
  | (E_type_in _, _) | (E_mod_in _, _)
  | (E_raw_code _, _)
  | (E_recursive _,_) | (E_accessor _, _)
  | (E_matching _, _)
  | E_module_accessor _, _
   -> None

  | E_literal _ , _
  | E_constant _ , E_constant _
  | E_constant _ , _
  | E_constructor _, E_constructor _
  | E_record _, _
  | E_constructor _, _ ->
      None
