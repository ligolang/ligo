open Trace
open Types

open Stage_common.Helpers
module Errors = struct
  let different_literals_because_different_types name a b () =
    let title () = "literals have different types: " ^ name in
    let message () = "" in
    let data = [
      ("a" , fun () -> Format.asprintf "%a" PP.literal a) ;
      ("b" , fun () -> Format.asprintf "%a" PP.literal b )
    ] in
    error ~data title message ()

  let different_literals name a b () =
    let title () = name ^ " are different" in
    let message () = "" in
    let data = [
      ("a" , fun () -> Format.asprintf "%a" PP.literal a) ;
      ("b" , fun () -> Format.asprintf "%a" PP.literal b )
    ] in
    error ~data title message ()

  let error_uncomparable_literals name a b () =
    let title () = name ^ " are not comparable" in
    let message () = "" in
    let data = [
      ("a" , fun () -> Format.asprintf "%a" PP.literal a) ;
      ("b" , fun () -> Format.asprintf "%a" PP.literal b )
    ] in
    error ~data title message ()
end
open Errors

let assert_literal_eq (a, b : literal * literal) : unit result =
  match (a, b) with
  | Literal_bool a, Literal_bool b when a = b -> ok ()
  | Literal_bool _, Literal_bool _ -> fail @@ different_literals "different bools" a b
  | Literal_bool _, _ -> fail @@ different_literals_because_different_types "bool vs non-bool" a b
  | Literal_int a, Literal_int b when a = b -> ok ()
  | Literal_int _, Literal_int _ -> fail @@ different_literals "different ints" a b
  | Literal_int _, _ -> fail @@ different_literals_because_different_types "int vs non-int" a b
  | Literal_nat a, Literal_nat b when a = b -> ok ()
  | Literal_nat _, Literal_nat _ -> fail @@ different_literals "different nats" a b
  | Literal_nat _, _ -> fail @@ different_literals_because_different_types "nat vs non-nat" a b
  | Literal_timestamp a, Literal_timestamp b when a = b -> ok ()
  | Literal_timestamp _, Literal_timestamp _ -> fail @@ different_literals "different timestamps" a b
  | Literal_timestamp _, _ -> fail @@ different_literals_because_different_types "timestamp vs non-timestamp" a b
  | Literal_mutez a, Literal_mutez b when a = b -> ok ()
  | Literal_mutez _, Literal_mutez _ -> fail @@ different_literals "different tezs" a b
  | Literal_mutez _, _ -> fail @@ different_literals_because_different_types "tez vs non-tez" a b
  | Literal_string a, Literal_string b when a = b -> ok ()
  | Literal_string _, Literal_string _ -> fail @@ different_literals "different strings" a b
  | Literal_string _, _ -> fail @@ different_literals_because_different_types "string vs non-string" a b
  | Literal_bytes a, Literal_bytes b when a = b -> ok ()
  | Literal_bytes _, Literal_bytes _ -> fail @@ different_literals "different bytess" a b
  | Literal_bytes _, _ -> fail @@ different_literals_because_different_types "bytes vs non-bytes" a b
  | Literal_void, Literal_void -> ok ()
  | Literal_void, _ -> fail @@ different_literals_because_different_types "void vs non-void" a b
  | Literal_unit, Literal_unit -> ok ()
  | Literal_unit, _ -> fail @@ different_literals_because_different_types "unit vs non-unit" a b
  | Literal_address a, Literal_address b when a = b -> ok ()
  | Literal_address _, Literal_address _ -> fail @@ different_literals "different addresss" a b
  | Literal_address _, _ -> fail @@ different_literals_because_different_types "address vs non-address" a b
  | Literal_operation _, Literal_operation _ -> fail @@ error_uncomparable_literals "can't compare operations" a b
  | Literal_operation _, _ -> fail @@ different_literals_because_different_types "operation vs non-operation" a b
  | Literal_signature a, Literal_signature b when a = b -> ok ()
  | Literal_signature _, Literal_signature _ -> fail @@ different_literals "different signature" a b
  | Literal_signature _, _ -> fail @@ different_literals_because_different_types "signature vs non-signature" a b
  | Literal_key a, Literal_key b when a = b -> ok ()
  | Literal_key _, Literal_key _ -> fail @@ different_literals "different key" a b
  | Literal_key _, _ -> fail @@ different_literals_because_different_types "key vs non-key" a b
  | Literal_key_hash a, Literal_key_hash b when a = b -> ok ()
  | Literal_key_hash _, Literal_key_hash _ -> fail @@ different_literals "different key_hash" a b
  | Literal_key_hash _, _ -> fail @@ different_literals_because_different_types "key_hash vs non-key_hash" a b
  | Literal_chain_id a, Literal_chain_id b when a = b -> ok ()
  | Literal_chain_id _, Literal_chain_id _ -> fail @@ different_literals "different chain_id" a b
  | Literal_chain_id _, _ -> fail @@ different_literals_because_different_types "chain_id vs non-chain_id" a b

let rec assert_value_eq (a, b: (expression * expression )) : unit result =
  Format.printf "in assert_value_eq %a %a\n%!" PP.expression a PP.expression b;
  let error_content () =
    Format.asprintf "\n@[<v>- %a@;- %a]" PP.expression a PP.expression b
  in
  trace (fun () -> error (thunk "not equal") error_content ()) @@
  match (a.expression_content , b.expression_content) with
  | E_literal a , E_literal b ->
      assert_literal_eq (a, b)
  | E_literal _ , _ ->
    simple_fail "comparing a literal with not a literal"
  | E_constant (ca) , E_constant (cb) when ca.cons_name = cb.cons_name -> (
      let%bind lst =
        generic_try (simple_error "constants with different number of elements")
          (fun () -> List.combine ca.arguments cb.arguments) in
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
      fail @@ (fun () -> error (thunk "comparing constant with other expression") error_content ())

  | E_constructor (ca), E_constructor (cb) when ca.constructor = cb.constructor -> (
      let%bind _eq = assert_value_eq (ca.element, cb.element) in
      ok ()
    )
  | E_constructor _, E_constructor _ ->
      simple_fail "different constructors"
  | E_constructor _, _ ->
      simple_fail "comparing constructor with other expression"


  | E_record sma, E_record smb -> (
      let aux _ a b =
        match a, b with
        | Some a, Some b -> Some (assert_value_eq (a, b))
        | _ -> Some (simple_fail "different record keys")
      in
      let%bind _all = bind_lmap @@ LMap.merge aux sma smb in
      ok ()
    )
  | E_record _, _ ->
      simple_fail "comparing record with other expression"
  
  | E_record_update ura, E_record_update urb ->
    let _ = 
      generic_try (simple_error "Updating different record") @@ 
      fun () -> assert_value_eq (ura.record, urb.record) in
    let aux (Label a,Label b) =
      assert (String.equal a b)
    in
    let () = aux (ura.path, urb.path) in
    let%bind () = assert_value_eq (ura.update,urb.update) in
    ok ()
  | E_record_update _, _ ->
     simple_fail "comparing record update with other expression"

  | (E_map lsta, E_map lstb | E_big_map lsta, E_big_map lstb) -> (
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
  | (E_map _ | E_big_map _), _ ->
      simple_fail "comparing map with other expression"

  | E_list lsta, E_list lstb -> (
      let%bind lst =
        generic_try (simple_error "list of different lengths")
          (fun () -> List.combine lsta lstb) in
      let%bind _all = bind_map_list assert_value_eq lst in
      ok ()
    )
  | E_list _, _ ->
      simple_fail "comparing list with other expression"

  | E_set lsta, E_set lstb -> (
      let lsta' = List.sort (compare) lsta in
      let lstb' = List.sort (compare) lstb in
      let%bind lst =
        generic_try (simple_error "set of different lengths")
          (fun () -> List.combine lsta' lstb') in
      let%bind _all = bind_map_list assert_value_eq lst in
      ok ()
    )
  | E_set _, _ ->
      simple_fail "comparing set with other expression"

  | (E_ascription a ,  _b') -> assert_value_eq (a.anno_expr , b)
  | (_a' , E_ascription b) -> assert_value_eq (a , b.anno_expr)
  | (E_variable _, _) | (E_lambda _, _)
  | (E_application _, _) | (E_let_in _, _)
  | (E_record_accessor _, _)
  | (E_look_up _, _) | (E_matching _, _)
  | (E_skip, _) -> simple_fail "comparing not a value"

let is_value_eq (a , b) = to_bool @@ assert_value_eq (a , b)

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
