open Trace
open Types

let assert_literal_eq (a, b : literal * literal) : unit result =
  match (a, b) with
  | Literal_bool a, Literal_bool b when a = b -> ok ()
  | Literal_bool _, Literal_bool _ -> simple_fail "different bools"
  | Literal_bool _, _ -> simple_fail "bool vs non-bool"
  | Literal_int a, Literal_int b when a = b -> ok ()
  | Literal_int _, Literal_int _ -> simple_fail "different ints"
  | Literal_int _, _ -> simple_fail "int vs non-int"
  | Literal_nat a, Literal_nat b when a = b -> ok ()
  | Literal_nat _, Literal_nat _ -> simple_fail "different nats"
  | Literal_nat _, _ -> simple_fail "nat vs non-nat"
  | Literal_tez a, Literal_tez b when a = b -> ok ()
  | Literal_tez _, Literal_tez _ -> simple_fail "different tezs"
  | Literal_tez _, _ -> simple_fail "tez vs non-tez"
  | Literal_string a, Literal_string b when a = b -> ok ()
  | Literal_string _, Literal_string _ -> simple_fail "different strings"
  | Literal_string _, _ -> simple_fail "string vs non-string"
  | Literal_bytes a, Literal_bytes b when a = b -> ok ()
  | Literal_bytes _, Literal_bytes _ -> simple_fail "different bytess"
  | Literal_bytes _, _ -> simple_fail "bytes vs non-bytes"
  | Literal_unit, Literal_unit -> ok ()
  | Literal_unit, _ -> simple_fail "unit vs non-unit"
  | Literal_address a, Literal_address b when a = b -> ok ()
  | Literal_address _, Literal_address _ -> simple_fail "different addresss"
  | Literal_address _, _ -> simple_fail "address vs non-address"
  | Literal_operation _, Literal_operation _ -> simple_fail "can't compare operations"
  | Literal_operation _, _ -> simple_fail "operation vs non-operation"


let rec assert_value_eq (a, b: (expression * expression )) : unit result =
  let error_content () =
    Format.asprintf "\n@[<v>- %a@;- %a]" PP.expression a PP.expression b
  in
  trace (fun () -> error (thunk "not equal") error_content ()) @@
  match (Location.unwrap a , Location.unwrap b) with
  | E_literal a , E_literal b ->
      assert_literal_eq (a, b)
  | E_literal _ , _ ->
    simple_fail "comparing a literal with not a literal"
  | E_constant (ca, lsta) , E_constant (cb, lstb) when ca = cb -> (
      let%bind lst =
        generic_try (simple_error "constants with different number of elements")
          (fun () -> List.combine lsta lstb) in
      let%bind _all = bind_list @@ List.map assert_value_eq lst in
      ok ()
    )
  | E_constant _ , E_constant _ ->
      simple_fail "different constants"
  | E_constant _ , _ ->
      let error_content () =
        Format.asprintf "%a vs %a"
          PP.expression a
          PP.expression b
      in
      fail @@ (fun () -> error (thunk "comparing constant with other stuff") error_content ())

  | E_constructor (ca, a), E_constructor (cb, b) when ca = cb -> (
      let%bind _eq = assert_value_eq (a, b) in
      ok ()
    )
  | E_constructor _, E_constructor _ ->
      simple_fail "different constructors"
  | E_constructor _, _ ->
      simple_fail "comparing constructor with other stuff"

  | E_tuple lsta, E_tuple lstb -> (
      let%bind lst =
        generic_try (simple_error "tuples with different number of elements")
          (fun () -> List.combine lsta lstb) in
      let%bind _all = bind_list @@ List.map assert_value_eq lst in
      ok ()
    )
  | E_tuple _, _ ->
      simple_fail "comparing tuple with other stuff"

  | E_record sma, E_record smb -> (
      let aux _ a b =
        match a, b with
        | Some a, Some b -> Some (assert_value_eq (a, b))
        | _ -> Some (simple_fail "different record keys")
      in
      let%bind _all = bind_smap @@ Map.String.merge aux sma smb in
      ok ()
    )
  | E_record _, _ ->
      simple_fail "comparing record with other stuff"

  | E_map lsta, E_map lstb -> (
      let%bind lst = generic_try (simple_error "maps of different lengths")
          (fun () ->
             let lsta' = List.sort compare lsta in
             let lstb' = List.sort compare lstb in
             List.combine lsta' lstb') in
      let aux = fun ((ka, va), (kb, vb)) ->
        let%bind _ = assert_value_eq (ka, kb) in
        let%bind _ = assert_value_eq (va, vb) in
        ok () in
      let%bind _all = bind_map_list aux lst in
      ok ()
    )
  | E_map _, _ ->
      simple_fail "comparing map with other stuff"

  | E_list lsta, E_list lstb -> (
      let%bind lst =
        generic_try (simple_error "list of different lengths")
          (fun () -> List.combine lsta lstb) in
      let%bind _all = bind_map_list assert_value_eq lst in
      ok ()
    )
  | E_list _, _ ->
      simple_fail "comparing list with other stuff"
  | (E_annotation (a , _) ,  _b') -> assert_value_eq (a , b)
  | (_a' , E_annotation (b , _)) -> assert_value_eq (a , b)
  | (E_variable _, _) | (E_lambda _, _)
  | (E_application _, _) | (E_let_in _, _)
  | (E_accessor _, _)
  | (E_look_up _, _) | (E_matching _, _) | (E_failwith _, _) | (E_sequence _, _)
  | (E_loop _, _) | (E_assign _, _) | (E_skip, _) -> simple_fail "comparing not a value"


(* module Rename = struct
 *   open Trace
 *
 *   module Type = struct
 *     (\* Type renaming, not needed. Yet. *\)
 *   end
 * 
 *   module Value = struct
 *     type renaming = string * (string * access_path) (\* src -> dst *\)
 *     type renamings = renaming list
 *     let filter (r:renamings) (s:string) : renamings =
 *       List.filter (fun (x, _) -> not (x = s)) r
 *     let filters (r:renamings) (ss:string list) : renamings =
 *       List.filter (fun (x, _) -> not (List.mem x ss)) r
 * 
 *     let rec rename_instruction (r:renamings) (i:instruction) : instruction result =
 *       match i with
 *       | I_assignment ({name;annotated_expression = e} as a) -> (
 *           match List.assoc_opt name r with
 *           | None ->
 *               let%bind annotated_expression = rename_annotated_expression (filter r name) e in
 *               ok (I_assignment {a with annotated_expression})
 *           | Some (name', lst) -> (
 *               let%bind annotated_expression = rename_annotated_expression r e in
 *               match lst with
 *               | [] -> ok (I_assignment {name = name' ; annotated_expression})
 *               | lst ->
 *                   let (hds, tl) =
 *                     let open List in
 *                     let r = rev lst in
 *                     rev @@ tl r, hd r
 *                   in
 *                   let%bind tl' = match tl with
 *                     | Access_record n -> ok n
 *                     | Access_tuple _ -> simple_fail "no support for renaming into tuples yet" in
 *                   ok (I_record_patch (name', hds, [tl', annotated_expression]))
 *             )
 *         )
 *       | I_skip -> ok I_skip
 *       | I_fail e ->
 *           let%bind e' = rename_annotated_expression r e in
 *           ok (I_fail e')
 *       | I_loop (cond, body) ->
 *           let%bind cond' = rename_annotated_expression r cond in
 *           let%bind body' = rename_block r body in
 *           ok (I_loop (cond', body'))
 *       | I_matching (ae, m) ->
 *           let%bind ae' = rename_annotated_expression r ae in
 *           let%bind m' = rename_matching rename_block r m in
 *           ok (I_matching (ae', m'))
 *       | I_record_patch (v, path, lst) ->
 *           let aux (x, y) =
 *             let%bind y' = rename_annotated_expression (filter r v) y in
 *             ok (x, y') in
 *           let%bind lst' = bind_map_list aux lst in
 *           match List.assoc_opt v r with
 *           | None -> (
 *               ok (I_record_patch (v, path, lst'))
 *             )
 *           | Some (v', path') -> (
 *               ok (I_record_patch (v', path' @ path, lst'))
 *             )
 *     and rename_block (r:renamings) (bl:block) : block result =
 *       bind_map_list (rename_instruction r) bl
 * 
 *     and rename_matching : type a . (renamings -> a -> a result) -> renamings -> a matching -> a matching result =
 *       fun f r m ->
 *       match m with
 *       | Match_bool { match_true = mt ; match_false = mf } ->
 *           let%bind match_true = f r mt in
 *           let%bind match_false = f r mf in
 *           ok (Match_bool {match_true ; match_false})
 *       | Match_option { match_none = mn ; match_some = (some, ms) } ->
 *           let%bind match_none = f r mn in
 *           let%bind ms' = f (filter r some) ms in
 *           ok (Match_option {match_none ; match_some = (some, ms')})
 *       | Match_list { match_nil = mn ; match_cons = (hd, tl, mc) } ->
 *           let%bind match_nil = f r mn in
 *           let%bind mc' = f (filters r [hd;tl]) mc in
 *           ok (Match_list {match_nil ; match_cons = (hd, tl, mc')})
 *       | Match_tuple (lst, body) ->
 *           let%bind body' = f (filters r lst) body in
 *           ok (Match_tuple (lst, body'))
 * 
 *     and rename_matching_instruction = fun x -> rename_matching rename_block x
 * 
 *     and rename_matching_expr = fun x -> rename_matching rename_expression x
 * 
 *     and rename_annotated_expression (r:renamings) (ae:annotated_expression) : annotated_expression result =
 *       let%bind expression = rename_expression r ae.expression in
 *       ok {ae with expression}
 * 
 *     and rename_expression : renamings -> expression -> expression result = fun r e ->
 *       match e with
 *       | E_literal _ as l -> ok l
 *       | E_constant (name, lst) ->
 *           let%bind lst' = bind_map_list (rename_annotated_expression r) lst in
 *           ok (E_constant (name, lst'))
 *       | E_constructor (name, ae) ->
 *           let%bind ae' = rename_annotated_expression r ae in
 *           ok (E_constructor (name, ae'))
 *       | E_variable v -> (
 *           match List.assoc_opt v r with
 *           | None -> ok (E_variable v)
 *           | Some (name, path) -> ok (E_accessor (ae (E_variable (name)), path))
 *         )
 *       | E_lambda ({binder;body;result} as l) ->
 *           let r' = filter r binder in
 *           let%bind body = rename_block r' body in
 *           let%bind result = rename_annotated_expression r' result in
 *           ok (E_lambda {l with body ; result})
 *       | E_application (f, arg) ->
 *           let%bind f' = rename_annotated_expression r f in
 *           let%bind arg' = rename_annotated_expression r arg in
 *           ok (E_application (f', arg'))
 *       | E_tuple lst ->
 *           let%bind lst' = bind_map_list (rename_annotated_expression r) lst in
 *           ok (E_tuple lst')
 *       | E_accessor (ae, p) ->
 *           let%bind ae' = rename_annotated_expression r ae in
 *           ok (E_accessor (ae', p))
 *       | E_record sm ->
 *           let%bind sm' = bind_smap
 *             @@ SMap.map (rename_annotated_expression r) sm in
 *           ok (E_record sm')
 *       | E_map m ->
 *           let%bind m' = bind_map_list
 *             (fun (x, y) -> bind_map_pair (rename_annotated_expression r) (x, y)) m in
 *           ok (E_map m')
 *       | E_list lst ->
 *           let%bind lst' = bind_map_list (rename_annotated_expression r) lst in
 *           ok (E_list lst')
 *       | E_look_up m ->
 *           let%bind m' = bind_map_pair (rename_annotated_expression r) m in
 *           ok (E_look_up m')
 *       | E_matching (ae, m) ->
 *           let%bind ae' = rename_annotated_expression r ae in
 *           let%bind m' = rename_matching rename_annotated_expression r m in
 *           ok (E_matching (ae', m'))
 *   end
 * end *)
