open Mini_c
open Simple_utils.Trace

(* This function checks that an `E_closure` has no free variables
   which involve `contract, `operation`, `big_map` or
   `sapling_transaction` *)

let capture_expression ~raise : expression -> expression =
 fun e ->
  let rec has_no_bad_capture ~err t =
    let self = has_no_bad_capture ~err in
    match t.type_content with
    | T_big_map _ -> err t
    | T_sapling_transaction _ -> err t
    | T_base TB_operation -> err t
    | T_contract _ -> err t
    | T_tuple ls ->
      let f (_, t) = self t in
      List.iter ~f ls
    | T_or ((_, t1), (_, t2)) ->
      self t1;
      self t2
    | T_list t -> self t
    | T_set t -> self t
    | T_option t -> self t
    | T_map (t1, t2) ->
      self t1;
      self t2
    | T_function (_, _) -> ()
    | T_ticket _ -> ()
    | T_sapling_state _ -> ()
    | T_base _ -> ()
  in
  let f expr =
    match expr.content with
    | E_closure _ ->
      let f (_, t) =
        let err t = raise.error @@ Errors.bad_capture expr.location t in
        has_no_bad_capture ~err t
      in
      let () = List.iter ~f (get_fv [] expr) in
      expr
    | _ -> expr
  in
  Helpers.map_expression f e
