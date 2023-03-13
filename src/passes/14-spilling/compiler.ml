(* The compiler is a function that takes as input the Typed AST, and outputs expressions in a language that is basically a Michelson with named variables and first-class-environments.

   For more info, see back-end.md: https://gitlab.com/ligolang/ligo/blob/dev/gitlab-pages/docs/contributors/big-picture/back-end.md *)

module Layout_t = Layout
open Ligo_prim
module Layout = Layout_t
open Simple_utils.Trace
module Pair = Simple_utils.Pair
open Errors

module AST = Ast_expanded
module Append_tree = Tree.Append
open AST.Combinators
open Mini_c

module SMap = Map.Make(String)

let temp_unwrap_loc_list = List.map ~f:Location.unwrap
let remove_empty_annotation (ann : string option) : string option =
  match ann with
  | Some "" -> None
  | Some ann -> Some ann
  | None -> None

let compile_constant' : Constant.constant' -> Constant.constant' = fun x ->
  if Constant.constant'_is_only_interpreter x then
    failwith (Format.asprintf "%a is only available for LIGO interpreter" Constant.pp_constant' x)
  else x

let rec compile_type ~raise (t:AST.type_expression) : type_expression =
  let compile_type = compile_type ~raise in
  let return tc = Expression.make_t ~loc:t.location ?source_type:t.source_type @@ tc in
  let loc = t.location in
  match t.type_content with
  | T_variable (name) -> raise.error @@ no_type_variable @@ name
  | t when (AST.compare_type_content t (t_bool ~loc ()).type_content) = 0-> return (T_base TB_bool)
  | T_constant {language ; injection ; parameters} -> (
    let () = Assert.assert_true ~raise (corner_case ~loc:__LOC__ "unsupported language") @@ String.equal language Backend.Michelson.name in
    match injection , parameters with
    | (Unit,            []) -> return (T_base TB_unit)
    | (Michelson_program,[]) -> return (T_base TB_unit) (* hit when testing framwork need to compile 'failwith "x" : michelson_program' *)
    | (Int,             []) -> return (T_base TB_int)
    | (Nat,             []) -> return (T_base TB_nat)
    | (Timestamp,       []) -> return (T_base TB_timestamp)
    | (Tez,             []) -> return (T_base TB_mutez)
    | (String,          []) -> return (T_base TB_string)
    | (Bytes,           []) -> return (T_base TB_bytes)
    | (Typed_address,   [_;_]) -> return (T_base TB_address)
    | (Address,         []) -> return (T_base TB_address)
    | (Operation,       []) -> return (T_base TB_operation)
    | (Key,             []) -> return (T_base TB_key)
    | (Key_hash,        []) -> return (T_base TB_key_hash)
    | (Chain_id,        []) -> return (T_base TB_chain_id)
    | (Signature,       []) -> return (T_base TB_signature)
    | (Baker_hash,      []) -> return (T_base TB_baker_hash)
    | (Pvss_key,        []) -> return (T_base TB_pvss_key)
    | (Chest,           []) -> return (T_base TB_chest)
    | (Chest_key,       []) -> return (T_base TB_chest_key)
    | (Tx_rollup_l2_address, []) -> return (T_base TB_tx_rollup_l2_address)
    | (Baker_operation, []) -> return (T_base TB_baker_operation)
    | (Bls12_381_g1,    []) -> return (T_base TB_bls12_381_g1)
    | (Bls12_381_g2,    []) -> return (T_base TB_bls12_381_g2)
    | (Bls12_381_fr,    []) -> return (T_base TB_bls12_381_fr)
    | (Never,           []) -> return (T_base TB_never)
    | (Ticket,         [x]) -> let x' = compile_type x in
                               return (T_ticket x')
    | (Sapling_transaction, [x]) -> (
      match x.type_content with
      | AST.T_singleton (Literal_value.Literal_int x') -> return (T_sapling_transaction x')
      | _ -> failwith "wrong sapling_transaction"
    )
    | (Sapling_state, [x]) -> (
      match x.type_content with
      | AST.T_singleton (Literal_int x') -> return (T_sapling_state x')
      | _ -> failwith "wrong sapling_state"
    )
    | (Contract, [x]) ->
      let x' = compile_type x in
      return (T_contract x')
    | (Map, [k;v]) ->
      let kv' = Pair.map ~f:compile_type (k, v) in
      return (T_map kv')
    | (Big_map, [k; v]) ->
      let kv' = Pair.map ~f:compile_type (k, v) in
      return (T_big_map kv')
    | (List, [t]) ->
      let t' = compile_type t in
      return (T_list t')
    | (Set, [t]) ->
      let t' = compile_type t in
      return (T_set t')
    (* External types are allowed (since they're fully resolved) *)
    | (External "int", [ param ]) ->
      (match (compile_type param).type_content with
      | T_base TB_bls12_381_fr | T_base TB_nat -> return (T_base TB_int)
      | _ -> raise.error (corner_case ~loc:__LOC__ "invalid external_int application"))
    | (External ("ediv" | "u_ediv"), [ param1; param2 ]) ->
      let open AST in
      let return t1 t2 = compile_type (t_option ~loc (t_pair ~loc t1 t2)) in
      (match (compile_type param1).type_content, (compile_type param2).type_content with
      | T_base TB_nat, T_base TB_nat -> return (t_nat ~loc ()) (t_nat ~loc ())
      | T_base TB_int, T_base TB_int -> return (t_int ~loc ()) (t_nat ~loc ())
      | T_base TB_nat, T_base TB_int -> return (t_int ~loc ()) (t_nat ~loc ())
      | T_base TB_int, T_base TB_nat -> return (t_int ~loc ()) (t_nat ~loc ())
      | T_base TB_mutez, T_base TB_mutez -> return (t_nat ~loc ()) (t_tez ~loc ())
      | T_base TB_mutez, T_base TB_nat -> return (t_tez ~loc ()) (t_tez ~loc ())
      | _ -> raise.error (corner_case ~loc:__LOC__ "invalid external_(ediv|u_ediv) application"))
    | (External ("and" | "u_and"), [ param1; param2 ]) ->
      (match (compile_type param1).type_content, (compile_type param2).type_content with
      | T_base TB_nat, T_base TB_nat -> return (T_base TB_nat)
      | T_base TB_int, T_base TB_nat -> return (T_base TB_nat)
      | _ -> raise.error (corner_case ~loc:__LOC__ "invalid external_(ediv|u_ediv) application"))
    | ((Michelson_or               | Chest_opening_result | Sapling_transaction |
        Ticket          | Int64    | Sapling_state        | Michelson_contract  |
        Contract        | Map      | Big_map              | Typed_address       |
        Michelson_pair  | Set      | Mutation             | Ast_contract        |
        List            | Gen      | External _           | Views), [])
        -> raise.error @@ corner_case ~loc:__LOC__ "wrong constant"
    | ((Int64      | Unit      | Baker_operation      |
      Nat          | Timestamp | Michelson_or         |
      String       | Gen       | Chest_opening_result |
      Address      | Operation | Bls12_381_fr         |
      Key_hash     | Chain_id  | Sapling_transaction  |
      Baker_hash   | Pvss_key  | Michelson_contract   |
      Chest        | Int       | Bls12_381_g1         |
      Bls12_381_g2 | Key       | Michelson_program    |
      Ticket       | Signature | Sapling_state        |
      Contract     | Map       | Big_map              |
      Set          | Tez       | Michelson_pair       |
      Never        | Chest_key | Ast_contract         |
      Bytes        | Mutation  | Typed_address        |
      External _   | List      | Tx_rollup_l2_address |
      Views        ), _::_) -> raise.error @@ corner_case ~loc:__LOC__ (Format.asprintf "wrong constant\n%a\n" Ast_aggregated.PP.type_expression t)
  )
  | T_sum _ when Option.is_some (AST.get_t_bool t) ->
    return (T_base TB_bool)
  | T_sum _ when Option.is_some (AST.get_t_option t) ->
    let o = trace_option ~raise (corner_case ~loc:__LOC__ ("impossible")) @@ AST.get_t_option t in
    let o' = compile_type o in
    return (T_option o')
  | T_sum { fields ; layout } -> (
      match is_michelson_or fields layout with
      | Some (a, b) -> (
          let aux ( (t,ann): AST.type_expression * string option) =
            let t = compile_type t in
            let annot = remove_empty_annotation ann in
            (annot , t)
          in
          let a' = aux a in
          let b' = aux b in
          return @@ T_or (a' , b')
        )
      | None ->
        let fields = Label.Map.map fields ~f:(compile_type) in
        Layout.t_sum ~raise fields layout
    )
  | T_record { fields ; layout } -> (
      match is_michelson_pair fields layout with
      | Some (a, b) -> (
        let aux ( (t,ann): AST.type_expression * string option) =
          let t = compile_type t in
          let annot = remove_empty_annotation ann in
          (annot , t)
        in
        let a' = aux a in
        let b' = aux b in
        return @@ T_tuple [a' ; b']
      )
      | None ->
        let fields = Label.Map.map fields ~f:(compile_type) in
        Layout.t_record ~raise fields layout
    )
  | T_arrow {type1;type2} -> (
      let param' = compile_type type1 in
      let result' = compile_type type2 in
      return @@ (T_function (param',result'))
  )
  | T_singleton (Literal_int z) ->
    return @@ (T_base (TB_type_int z))
  | T_singleton _ ->
    raise.error @@ corner_case ~loc:__LOC__ "Singleton uncaught"
  | T_for_all _ ->
    raise.error @@ corner_case ~loc:__LOC__ "For all type uncaught"

(* probably should use result monad for conformity? but these errors
   are supposed to be impossible *)
let internal_error loc msg =
  failwith
    (Format.asprintf
       "@[<v>Internal error, please report this as a bug@ %s@ %s@ @]"
       loc msg)

let rec compile_expression ~raise (ae:AST.expression) : expression =
  let tv = compile_type ~raise ae.type_expression in
  let self = compile_expression ~raise in
  let loc = ae.location in
  let return ?(tv = tv) expr =
    Combinators.Expression.make_tpl ~loc (expr, tv) in
  match ae.expression_content with
  | E_type_abstraction _
  | E_type_inst _ ->
    raise.error @@ corner_case ~loc:__LOC__ (Format.asprintf "Type instance: This program should be monomorphised")
  | E_let_in {let_binder; rhs; let_result; attributes = { inline; no_mutation=_; view=_; public=_ ; hidden = _ ; thunk = _; entry = _ } } ->
    let rhs' = self rhs in
    let result' = self let_result in
    return (E_let_in (rhs', inline, ((Binder.get_var let_binder, rhs'.type_expression), result')))
  | E_literal l -> return @@ E_literal l
  | E_variable name -> return @@ E_variable name
  | E_application {lamb; args} ->
    let a = self lamb in
    let b = self args in
    return @@ E_application (a, b)
  | E_constructor {constructor=Label "True";element} when AST.compare_expression_content element.expression_content (AST.e_unit ()) = 0 ->
    return @@ E_constant { cons_name = C_TRUE ; arguments = [] }
  | E_constructor {constructor=Label "False";element} when AST.compare_expression_content element.expression_content (AST.e_unit ()) = 0 ->
    return @@ E_constant { cons_name = C_FALSE ; arguments = [] }
  | E_constructor {constructor=Label "None";_} ->
    return @@ E_constant { cons_name = C_NONE ; arguments = [] }
  | E_constructor {constructor=Label "Some";element} ->
    let e = compile_expression ~raise element in
    return @@ E_constant { cons_name = C_SOME ; arguments = [e] }
  | E_constructor {constructor;element} -> (
      let ty' = compile_type ~raise ae.type_expression in
      let ty_variant =
        trace_option ~raise (corner_case ~loc:__LOC__ "not a record") @@
        get_t_sum_opt (get_type ae) in
      let path = Layout.constructor_to_lr ~layout:ty_variant.layout ty' constructor in
      let aux = fun pred (ty, lr) ->
        let c : Constant.constant' = match lr with
          | `Left  -> C_LEFT
          | `Right -> C_RIGHT
        in
        return ~tv:ty @@ E_constant {cons_name=c;arguments=[pred]}
      in
      let element' = self element in
      let expr = List.fold ~f:aux ~init:element' path in
      expr
    )
  | E_record m -> (
      let (record_t : Ast_aggregated.Types.row) = trace_option ~raise (corner_case ~loc:__LOC__ "record expected") (AST.get_t_record_opt ae.type_expression) in
      (* Note: now returns E_tuple, not pairs, for combs *)
      (* Layout.record_to_pairs ~raise self (fun e1 e2 -> return @@ ec_pair e1 e2) (fun es -> return @@ E_tuple es) record_t m *)
      let m = Record.map ~f:self m in
      Layout.record_to_pairs ~raise (fun ~tv x -> return ~tv x) m record_t.layout
    )
  | E_accessor {struct_; path} -> (
      let ty' = compile_type ~raise (get_type struct_) in
      let record_ty = trace_option ~raise (corner_case ~loc:__LOC__ "not a record") @@
        get_t_record_opt (get_type struct_) in
      let path = Layout.path_to_field record_ty.layout path ty' in
      let struct_' = compile_expression ~raise struct_ in
      let content =
        List.fold_left
          path
          ~f:(fun expr (i, n, ty, _) ->
              let expr = { content = E_proj (expr, i, n) ; type_expression = ty ; location = Location.generated } in
              expr)
          ~init:struct_' in
      return content.content
    )
  | E_update {struct_; path; update} -> (
      (* Compile record update to simple constructors &
         projections. This will be optimized to some degree by eta
         contraction in a later pass. *)
      let ty = get_type struct_ in
      let record_ty =
        trace_option ~raise (corner_case ~loc:__LOC__ "not a record") @@
        get_t_record_opt (ty) in
      let ty' = compile_type ~raise (ty) in
      let path = Layout.path_to_field record_ty.layout path ty' in
      let record_var = Value_var.fresh ~loc () in
      let with_ty content ty =
        { content = content;
          type_expression = ty;
          location = Location.generated} in
      let update_' = self update in
      let struct_' = self struct_ in
      let content =
        List.fold_right
          path
          ~f:(fun (i, n, field_ty, parent_ty) update ->
              E_update (with_ty (E_variable record_var) parent_ty,
                        i,
                        with_ty (E_let_in (with_ty
                                             (E_proj (with_ty (E_variable record_var) parent_ty, i, n))
                                             field_ty,
                                           false,
                                           ((record_var, parent_ty), with_ty update field_ty)))
                          field_ty,
                        n))
          ~init:update_'.content in
      return @@ E_let_in (struct_', false, ((record_var, ty'), with_ty content ty'))
    )
  | E_constant {cons_name=name; arguments=lst} -> (
      let iterator_generator (iterator_name : Constant.constant') =
        let expression_to_iterator_body (f : AST.expression) =
          let Arrow.{ type1 = input ; type2 = output } = trace_option ~raise (corner_case ~loc:__LOC__ "expected function type") @@ AST.get_t_arrow f.type_expression in
          let f' = self f in
          let input' = compile_type ~raise input in
          let output' = compile_type ~raise output in
          let binder = Value_var.fresh ~loc ~name:"iterated" () in
          let application = Mini_c.Combinators.e_application f' output' (Mini_c.Combinators.e_var binder input') in
          ((binder , input'), application)
        in
        fun (lst : AST.expression list) -> match (lst , iterator_name) with
          | [f ; i] , C_ITER | [f ; i] , C_MAP -> (
              let f' = expression_to_iterator_body f in
              let i' = self i in
              return @@ E_iterator (iterator_name , f' , i')
            )
          | [ f ; collection ; initial ] , C_FOLD -> (
              let f' = expression_to_iterator_body f in
              let initial' = self initial in
              let collection' = self collection in
              return @@ E_fold (f' , collection' , initial')
            )
          | [ f ; initial ; collection ], C_FOLD_LEFT -> (
              let f' = expression_to_iterator_body f in
              let initial' = self initial in
              let collection' = self collection in
              return @@ E_fold (f' , collection' , initial')
            )
          | [ f ; collection ; initial ], C_FOLD_RIGHT -> (
              let f' = expression_to_iterator_body f in
              let initial' = self initial in
              let elem_type =
                (trace_option ~raise (corner_case ~loc:__LOC__ "Wrong type : expecting collection")) @@
                get_t_collection @@ compile_type ~raise collection.type_expression in
              let collection' = self collection in
              return @@ E_fold_right (f' , (collection',elem_type) , initial')
            )
          | [ f ; i], C_LOOP_LEFT -> (
              let f' = expression_to_iterator_body f in
              let i' = self i in
              return @@ E_iterator (iterator_name , f' , i')
            )
          | (code :: args), C_CREATE_CONTRACT -> (
              let code' = expression_to_iterator_body code in
              let args' = List.map ~f:self args in
              let code_type = compile_type ~raise code.type_expression in
              let (code_input_type, _) =
                trace_option ~raise (corner_case ~loc:__LOC__ "Wrong type : expecting function for CREATE_CONTRACT script")
                  (get_t_function code_type) in
              let (p, s) =
                trace_option ~raise (corner_case ~loc:__LOC__ "Wrong type : expecting function for CREATE_CONTRACT script")
                  (get_t_pair code_input_type) in
              return @@ E_create_contract (p, s, code', args')
            )
          | _ -> raise.error @@ corner_case ~loc:__LOC__ (Format.asprintf "bad iterator arity: %a" PP.constant iterator_name)
      in
      let iter = iterator_generator C_ITER in
      let map = iterator_generator C_MAP in
      let fold = iterator_generator C_FOLD in
      let fold_left = iterator_generator C_FOLD_LEFT in
      let fold_right = iterator_generator C_FOLD_RIGHT in
      let loop_left = iterator_generator C_LOOP_LEFT in
      (* wait what *)
      let create_contract = iterator_generator C_CREATE_CONTRACT in
      match (name , lst) with
      | (C_SET_ITER , lst) -> iter lst
      | (C_LIST_ITER , lst) -> iter lst
      | (C_MAP_ITER , lst) -> iter lst
      | (C_LIST_MAP , lst) -> map lst
      | (C_MAP_MAP , lst) -> map lst
      | (C_OPTION_MAP , lst) -> map lst
      | (C_LIST_FOLD , lst) -> fold lst
      | (C_SET_FOLD , lst) -> fold lst
      | (C_MAP_FOLD , lst) -> fold lst
      | (C_FOLD, lst) -> fold lst
      | (C_LIST_FOLD_LEFT, lst) -> fold_left lst
      | (C_LIST_FOLD_RIGHT, lst) -> fold_right lst
      | (C_SET_FOLD_DESC , lst) -> fold_right lst
      | (C_LOOP_LEFT, lst) -> loop_left lst
      | (C_CREATE_CONTRACT , lst) -> create_contract lst
      | _ -> (
          let lst' = List.map ~f:(self) lst in
          return @@ E_constant {cons_name=compile_constant' name;arguments=lst'}
        )
    )
  | E_lambda l ->
    return @@ compile_lambda ~raise l
  | E_recursive ({ fun_name ; fun_type = _ ; lambda; force_lambdarec } as r) when not force_lambdarec && Recursion.is_tail_recursive fun_name lambda ->
    return @@ compile_recursive ~raise r
  | E_recursive { fun_name ; fun_type ; lambda; force_lambdarec = _ } ->
    return @@ compile_rec_lambda ~raise lambda fun_name fun_type
  | E_matching {matchee=expr; cases=m} -> (
      let expr' = self expr in
      match m with
      | Match_variant {cases ; tv} -> (
          match expr.type_expression.type_content with
          | T_constant { injection = Literal_types.List ; parameters = [list_ty];language =_ } ->
            let list_ty = compile_type ~raise list_ty in
            let get_c_body (case : _ AST.matching_content_case) = (case.constructor, (case.body, case.pattern)) in
            let c_body_lst = Record.of_list (List.map ~f:get_c_body cases) in
            let get_case c =
              trace_option ~raise
                (corner_case ~loc:__LOC__ ("missing " ^ c ^ " case in match"))
                (Record.find_opt c_body_lst (Label.of_string c)) in
            let match_nil = get_case "Nil" in
            let match_cons = get_case "Cons" in
            let nil = self (fst match_nil) in
            let cons =
              let hd = Value_var.fresh ~loc () in
              let tl = Value_var.fresh ~loc () in
              let proj_t = t_pair (None,list_ty) (None,expr'.type_expression) in
              let proj = Expression.make (ec_pair (e_var  hd list_ty) (e_var tl expr'.type_expression)) proj_t in
              let cons_body = self (fst match_cons) in
              let cons_body' = e_let_in (snd match_cons) proj_t false proj cons_body in
              (((hd,list_ty), (tl,expr'.type_expression)), cons_body')
            in
            return @@ E_if_cons (expr' , nil , cons)
          | T_sum _ when Option.is_some (AST.get_t_option expr.type_expression) ->
            let opt_tv = trace_option ~raise (corner_case ~loc:__LOC__ ("impossible")) @@ AST.get_t_option expr.type_expression in
            let get_c_body (case : _ AST.matching_content_case) = (case.constructor, (case.body, case.pattern)) in
            let c_body_lst = Record.of_list (List.map ~f:get_c_body cases) in
            let get_case c =
              trace_option ~raise
                (corner_case ~loc:__LOC__ ("missing " ^ c ^ " case in match"))
                (Record.find_opt c_body_lst (Label.of_string c)) in
            let match_none = get_case "None" in
            let match_some = get_case "Some" in
            let n = self (fst match_none) in
            let (tv' , s') =
              let tv' = compile_type ~raise opt_tv in
              let s' = self (fst match_some) in
              (tv' , s')
            in
            return @@ E_if_none (expr' , n , ((snd match_some , tv') , s'))
          | T_sum _ when Option.is_some (AST.get_t_bool expr.type_expression) ->
            let ctor_body (case : _ AST.matching_content_case) = (case.constructor, case.body) in
            let cases = Record.of_list (List.map ~f:ctor_body cases) in
            let get_case c =
              trace_option ~raise
                (corner_case ~loc:__LOC__ ("missing " ^ c ^ " case in match"))
                (Record.find_opt cases (Label c)) in
            let match_true  = get_case "True" in
            let match_false = get_case "False" in
            let (t , f) = Pair.map ~f:self (match_true, match_false) in
            return @@ E_if_bool (expr', t, f)
          | _ -> (
              let record_ty = trace_option ~raise (corner_case ~loc:__LOC__ "compile_recursive: getting lr tree") @@
                get_t_sum_opt tv in
              let tree = Layout.match_variant_to_tree ~raise ~layout:record_ty.layout (compile_type ~raise tv) in
              let rec aux top t =
                match t with
                | ((`Leaf (Label.Label constructor_name)) , tv) -> (
                    let ({constructor=_ ; pattern ; body} : _ AST.matching_content_case)=
                      trace_option ~raise (corner_case ~loc:__LOC__ "missing match clause") @@
                      let aux ({constructor = Label c ; pattern=_ ; body=_} : _ AST.matching_content_case) =
                        (String.equal c constructor_name) in
                      List.find ~f:aux cases in
                    let body' = self body in
                    return @@ E_let_in (top, false, ((pattern, tv) , body'))
                  )
                | ((`Node (a , b)) , tv) ->
                  let a' =
                    let a_ty = trace_option ~raise (corner_case ~loc:__LOC__ "wrongtype") @@ get_t_left tv in
                    let left_var = Value_var.fresh ~loc ~name:"left" () in
                    let e = aux (((Expression.make (E_variable left_var) a_ty))) a in
                    ((left_var , a_ty) , e)
                  in
                  let b' =
                    let b_ty = trace_option ~raise (corner_case ~loc:__LOC__ "wrongtype") @@ get_t_right tv in
                    let right_var = Value_var.fresh ~loc ~name:"right" () in
                    let e = aux (((Expression.make (E_variable right_var) b_ty))) b in
                    ((right_var , b_ty) , e)
                  in
                  return @@ E_if_left (top , a' , b')
              in
              trace_strong ~raise (corner_case ~loc:__LOC__ "building constructor") @@
              (fun ~raise:_ -> aux expr' tree)
            )
        )
      | Match_record record ->
        compile_record_matching ~raise ~loc (fun content -> return content) self expr' record  
    )
  | E_raw_code { language; code = { expression_content = m ; _ } } when Option.is_some (get_e_tuple m) ->
    let backend = Backend.Michelson.name in
    let () =
      Assert.assert_true ~raise
        (corner_case ~loc:__LOC__ "Language insert - backend mismatch only provide code insertion in the language you are compiling to")
        (String.equal language backend)
    in
    let tuple = Option.value ~default:[] (AST.get_e_tuple m) in
    let code, args = match tuple with
      | [] -> raise.error (corner_case ~loc:__LOC__ "expected non-empty tuple in %Michelson")
      | hd :: tl -> hd, tl in
    let args = List.map ~f:(fun t -> self t) args in
    let type_anno  = get_type ae in
    let type_anno' = compile_type ~raise type_anno in
    let code = trace_option ~raise (corner_case ~loc:__LOC__ "could not get a string") @@ get_a_string code in
    let open Tezos_micheline in
    let orig_code = code in
    let (code, errs) = Micheline_parser.tokenize code in
    (match errs with
     | _ :: _ -> raise.error (could_not_parse_raw_michelson ae.location orig_code)
     | [] ->
       let (code, errs) = Micheline_parser.parse_expression ~check:false code in
       match errs with
       | _ :: _ -> raise.error (could_not_parse_raw_michelson ae.location orig_code)
       | [] ->
         let code = Micheline.strip_locations code in
         (* hmm *)
         let code = Micheline.inject_locations (fun _ -> Location.generated) code in
         match code with
         | Seq (_, code) ->
           return ~tv:type_anno' @@ E_raw_michelson (code, args)
         | _ ->
           raise.error (raw_michelson_must_be_seq ae.location code)
    )
  | E_raw_code { language; code} ->
    let backend = Backend.Michelson.name in
    let () =
      Assert.assert_true ~raise
        (corner_case ~loc:__LOC__ "Language insert - backend mismatch only provide code insertion in the language you are compiling to")
        (String.equal language backend)
    in
    let type_anno  = get_type code in
    let type_anno' = compile_type ~raise type_anno in
    let code = trace_option ~raise (corner_case ~loc:__LOC__ "could not get a string") @@ get_a_string code in
    let open Tezos_micheline in
    let orig_code = code in
    let (code, errs) = Micheline_parser.tokenize code in
    (match errs with
     | _ :: _ -> raise.error (could_not_parse_raw_michelson ae.location orig_code)
     | [] ->
       let (code, errs) = Micheline_parser.parse_expression ~check:false code in
       match errs with
       | _ :: _ -> raise.error (could_not_parse_raw_michelson ae.location orig_code)
       | [] ->
         let code = Micheline.strip_locations code in
         (* hmm *)
         let code = Micheline.inject_locations (fun _ -> Location.generated) code in
         match code with
         | Seq (_, code) ->
           return ~tv:type_anno' @@ E_raw_michelson (code, [])
         | _ ->
           raise.error (raw_michelson_must_be_seq ae.location code)
    )
  | E_let_mut_in { let_binder; rhs; let_result; attributes = _ } ->
    let binder = compile_binder ~raise let_binder in
    let rhs = self rhs in
    let let_result = self let_result in
    return @@ E_let_mut_in (rhs, (binder, let_result)) 
  | E_deref mut_var -> return @@ E_deref mut_var 
  | E_assign { binder; expression } ->
    let expression = self expression in
    return @@ E_assign (Binder.get_var binder, expression)
  | E_for { start; final; incr; binder; f_body } -> 
    let type_ = compile_type ~raise start.type_expression in
    let start = self start in
    let final = self final in
    let incr = self incr in
    let binder = (binder, type_) in
    let body = self f_body in
    return @@ E_for (start, final, incr, (binder, body))
  | E_for_each { fe_binder = binder1, Some binder2; collection; fe_body; _ } ->
    let type_ = compile_type ~raise collection.type_expression in
    let key_type, val_type = 
      AST.get_t_map_exn collection.type_expression
    in
    let binders = [ binder1, compile_type ~raise key_type; binder2, compile_type ~raise val_type ] in
    let collection = self collection in
    let body = self fe_body in
    return @@ E_for_each (collection, type_, (binders, body))
  | E_for_each { fe_binder = binder1, None; collection; fe_body; _ } ->
    let type_ = compile_type ~raise collection.type_expression in
    let elt_type = 
      let type_ = collection.type_expression in
      if is_t_list type_ then get_t_list_exn type_
      else if is_t_set type_ then get_t_set_exn type_
      else if is_t_map type_ then
        let key_type, val_type = AST.get_t_map_exn type_
        in AST.t_pair ~loc key_type val_type
      else failwith "Expected set, map or list type for for-each loop (should have been caught earlier)"
    in
    let binders = [ binder1, compile_type ~raise elt_type ] in
    let collection = self collection in
    let body = self fe_body in
    return @@ E_for_each (collection, type_, (binders, body))
  | E_while { cond; body } ->
    let cond = self cond in
    let body = self body in
    return @@ E_while (cond, body)

and compile_record_matching ~raise ~loc return k (matchee : expression)
    ({ fields; body; tv } : _ AST.matching_content_record) =
  let row =
    trace_option ~raise
      (corner_case ~loc:__LOC__ "Invariant broken. `tv` isn't a record type")
    @@ get_t_record_opt tv
  in
  let body = k body in
  let row = AST.Row.map (compile_type ~raise) row in
  let ty' = compile_type ~raise tv in
  let bindings = Layout.explode_row ~loc row ty' fields matchee in
  List.fold_right bindings ~init:body ~f:(fun (binding, matchee) body ->
      return  @@ 
      match binding with
      | `Let binding -> E_let_in (matchee, false, (binding, body))
      | `Let_tuple bindings -> E_let_tuple (matchee, (bindings, body)))


and compile_lambda ~raise l =
  let { binder ; output_type ; result } : _ Lambda.t = l in
  let result' = compile_expression ~raise result in
  let param = Param.map (compile_type ~raise) binder in
  let output_type = compile_type ~raise output_type in
  let (binder, body) = make_lambda ~loc:result.location param result' output_type in
  E_closure { binder; body }

and compile_rec_lambda ~raise l fun_name _fun_type =
  let { binder ; output_type ; result } : _ Lambda.t = l in
  let result' = compile_expression ~raise result in
  let param = Param.map (compile_type ~raise) binder in
  let output_type = compile_type ~raise output_type in
  let (binder, body) = make_lambda ~loc:result.location param result' output_type in
  E_rec { func = { binder; body } ; rec_binder = fun_name }

and compile_binder ~raise binder = 
  let ascr = compile_type ~raise (Binder.get_ascr binder) in
  (Binder.get_var binder, ascr)

(* ast_aggregated has mutable lambda parameters, for now, mini_c does
   not. so here we will translate:
       (fun mut x -> e1)
   to:
       (fun x -> let mut x = x in e1) *)
and make_lambda ~loc param body body_type =
  let body =
    if Param.is_mut param
    then
      let x = Param.get_var param in
      let a = Param.get_ascr param in
      Expression.make ~loc (E_let_mut_in (Expression.make ~loc (E_variable x) a, ((x, a), body))) body_type
    else body in
  (Param.get_var param, body)

and compile_recursive ~raise Recursive.{fun_name; fun_type; lambda; force_lambdarec = _} =
  let rec map_lambda : Value_var.t -> type_expression -> AST.expression -> expression = fun fun_name loop_type e ->
    match e.expression_content with
      E_lambda {binder;output_type;result} ->
      let param = binder in
      let body = map_lambda  fun_name loop_type result in
      let body_type = compile_type ~raise output_type in
      let param = Param.map (compile_type ~raise) param in
      let (binder, body) = make_lambda ~loc:e.location param body body_type in
      Expression.make ~loc:e.location (E_closure {binder;body }) loop_type
    | _  ->
      let res = replace_callback ~raise fun_name loop_type false e in
      res

  and replace_callback ~raise : Value_var.t -> type_expression -> bool -> AST.expression -> expression = fun fun_name loop_type shadowed e ->
    match e.expression_content with
    | E_let_in li ->
      let shadowed = shadowed || Binder.apply (Value_var.equal fun_name) li.let_binder in
      let let_result = replace_callback ~raise fun_name loop_type shadowed li.let_result in
      let rhs = compile_expression ~raise li.rhs in
      let ty  = compile_type ~raise li.rhs.type_expression in
      let let_binder = Binder.get_var li.let_binder in
      e_let_in let_binder ty li.attributes.inline rhs let_result
    | E_let_mut_in li ->
      (* Not possible for mut to shadow fun_name *)
      let let_result = replace_callback ~raise fun_name loop_type shadowed li.let_result in
      let rhs = compile_expression ~raise li.rhs in
      let ty  = compile_type ~raise li.rhs.type_expression in
      let let_binder = Binder.get_var li.let_binder in
      e_let_mut_in let_binder ty rhs let_result
    | E_matching m ->
      let ty = compile_type ~raise e.type_expression in
      matching ~raise fun_name loop_type shadowed m ty
    | E_application {lamb;args} -> (
        match lamb.expression_content,shadowed with
        | E_variable name, false when Value_var.equal fun_name name ->
          let expr = compile_expression ~raise args in
          Expression.make (E_constant {cons_name=C_LOOP_CONTINUE;arguments=[expr]}) loop_type
        | _ ->
          let expr = compile_expression ~raise e in
          Expression.make (E_constant {cons_name=C_LOOP_STOP;arguments=[expr]}) loop_type
      )
    | _ ->
      let expr = compile_expression ~raise e in
      Expression.make (E_constant {cons_name=C_LOOP_STOP;arguments=[expr]}) loop_type

  and matching ~raise : Value_var.t -> type_expression -> bool -> AST.matching -> type_expression -> expression = fun fun_name loop_type shadowed m ty ->
    let return ret = Expression.make ret @@ ty in
    let expr' = compile_expression ~raise m.matchee in
    let self = replace_callback ~raise fun_name loop_type shadowed in
    let loc = Value_var.get_location fun_name in
    match m.cases with
    | Match_variant {cases ; tv} -> (
        match m.matchee.type_expression.type_content with
        | T_constant { injection = Literal_types.List ; parameters = [list_ty];language=_ } ->
          let list_ty = compile_type ~raise list_ty in
          let get_c_body (case : _ AST.matching_content_case) = (case.constructor, (case.body, case.pattern)) in
          let c_body_lst = Record.of_list (List.map ~f:get_c_body cases) in
          let get_case c =
            trace_option ~raise
              (corner_case ~loc:__LOC__ ("missing " ^ c ^ " case in match"))
              (Record.find_opt c_body_lst (Label.of_string c)) in
          let match_nil = get_case "Nil" in
          let match_cons = get_case "Cons" in
          let nil = self (fst match_nil) in
          let cons =
            let hd = Value_var.fresh ~loc () in
            let tl = Value_var.fresh ~loc () in
            let proj_t = t_pair (None,list_ty) (None,expr'.type_expression) in
            let proj = Expression.make (ec_pair (e_var  hd list_ty) (e_var tl expr'.type_expression)) proj_t in
            let cons_body = self (fst match_cons) in
            let cons_body' = e_let_in (snd match_cons) proj_t false proj cons_body in
            (((hd,list_ty), (tl,expr'.type_expression)), cons_body')
          in
          return @@ E_if_cons (expr' , nil , cons)
        | T_sum _ when Option.is_some (AST.get_t_option m.matchee.type_expression) ->
          let opt_tv = trace_option ~raise (corner_case ~loc:__LOC__ ("impossible")) @@ AST.get_t_option m.matchee.type_expression in
          let get_c_body (case : _ AST.matching_content_case) = (case.constructor, (case.body, case.pattern)) in
          let c_body_lst = Record.of_list (List.map ~f:get_c_body cases) in
          let get_case c =
            trace_option ~raise
              (corner_case ~loc:__LOC__ ("missing " ^ c ^ " case in match"))
              (Record.find_opt c_body_lst (Label.of_string c)) in
          let match_none = get_case "None" in
          let match_some = get_case "Some" in
          let n = self (fst match_none) in
          let (tv' , s') =
            let tv' = compile_type ~raise opt_tv in
            let s' = self (fst match_some) in
            (tv' , s')
          in
          return @@ E_if_none (expr' , n , ((snd match_some , tv') , s'))
        | T_sum _ when Option.is_some (AST.get_t_bool m.matchee.type_expression) ->
          let ctor_body (case : _ AST.matching_content_case) = (case.constructor, case.body) in
          let cases = Record.of_list (List.map ~f:ctor_body cases) in
          let get_case c =
            trace_option ~raise
              (corner_case ~loc:__LOC__ ("missing " ^ c ^ " case in match"))
              (Record.find_opt cases (Label c)) in
          let match_true  = get_case "True" in
          let match_false = get_case "False" in
          let (t , f) = Pair.map ~f:self (match_true, match_false) in
          return @@ E_if_bool (expr', t, f)
        | _ -> (
            let record_ty = trace_option ~raise (corner_case ~loc:__LOC__ "compile_recursive: getting lr tree") @@
              get_t_sum_opt tv in
            let tree = Layout.match_variant_to_tree ~raise ~layout:record_ty.layout (compile_type ~raise tv) in
            let rec aux top t =
              match t with
              | ((`Leaf (Label.Label constructor_name)) , tv) -> (
                  let ({constructor=_ ; pattern ; body} : _ AST.matching_content_case)=
                    trace_option ~raise (corner_case ~loc:__LOC__ "missing match clause") @@
                    let aux ({constructor = Label c ; pattern=_ ; body=_} : _ AST.matching_content_case) =
                      (String.equal c constructor_name) in
                    List.find ~f:aux cases in
                  let body' = self body in
                  return @@ E_let_in (top, false, ((pattern, tv) , body'))
                )
              | ((`Node (a , b)) , tv) ->
                let a' =
                  let a_ty = trace_option ~raise (corner_case ~loc:__LOC__ "wrongtype") @@ get_t_left tv in
                  let left_var = Value_var.fresh ~loc ~name:"left" () in
                  let e = aux (((Expression.make (E_variable left_var) a_ty))) a in
                  ((left_var , a_ty) , e)
                in
                let b' =
                  let b_ty = trace_option ~raise (corner_case ~loc:__LOC__ "wrongtype") @@ get_t_right tv in
                  let right_var = Value_var.fresh ~loc ~name:"right" () in
                  let e = aux (((Expression.make (E_variable right_var) b_ty))) b in
                  ((right_var , b_ty) , e)
                in
                return @@ E_if_left (top , a' , b')
            in
            trace_strong ~raise (corner_case ~loc:__LOC__ "building constructor") @@
            (fun ~raise:_ -> aux expr' tree)
          )
      )
    | Match_record record ->
      compile_record_matching ~raise ~loc return (replace_callback ~raise fun_name loop_type shadowed) expr' record
  in
  let fun_type = compile_type ~raise fun_type in
  let (input_type,output_type) = trace_option ~raise (corner_case ~loc:__LOC__ "wrongtype") @@ get_t_function fun_type in
  let loop_type = t_union (None, input_type) (None, output_type) in
  let body = map_lambda fun_name loop_type lambda.result in
  let param = Param.map (compile_type ~raise) lambda.binder in
  let (binder, body) = make_lambda ~loc:body.location param body loop_type in
  let expr = Expression.make_tpl (E_variable binder, input_type) in
  let body = Expression.make (E_iterator (C_LOOP_LEFT, ((binder, input_type), body), expr)) output_type in
  E_closure {binder;body}

let compile_program ~raise : AST.expression -> Mini_c.expression = fun p ->
  compile_expression ~raise p
