(* The compiler is a function that takes as input the Typed AST, and outputs expressions in a language that is basically a Michelson with named variables and first-class-environments.

   For more info, see back-end.md: https://gitlab.com/ligolang/ligo/blob/dev/gitlab-pages/docs/contributors/big-picture/back-end.md *)

module Layout_t = Layout
open Ligo_prim
module Layout = Layout_t
open Simple_utils.Trace
module Pair = Simple_utils.Pair
open Errors

module AST = Ast_aggregated
module Append_tree = Tree.Append
open AST.Combinators
open Mini_c

module SMap = Map.Make(String)

let temp_unwrap_loc_list = List.map ~f:Location.unwrap


let compile_constant' : Constant.constant' -> Constant.constant' = fun x ->
  if Constant.ppx_is_only_interpreter x then
    failwith (Format.asprintf "%a is only available for LIGO interpreter" Constant.pp_constant' x)
  else x

let rec compile_type ~raise (t:AST.type_expression) : type_expression =
  let compile_type = compile_type ~raise in
  let return tc = Expression.make_t ~loc:t.location ?source_type:t.source_type @@ tc in
  match t.type_content with
  | T_variable (name) -> raise.error @@ no_type_variable @@ name
  | t when (AST.compare_type_content t (t_bool ()).type_content) = 0-> return (T_base TB_bool)
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
      let return t1 t2 = compile_type (t_option (t_pair t1 t2)) in
      (match (compile_type param1).type_content, (compile_type param2).type_content with
      | T_base TB_nat, T_base TB_nat -> return (t_nat ()) (t_nat ())
      | T_base TB_int, T_base TB_int -> return (t_int ()) (t_nat ())
      | T_base TB_nat, T_base TB_int -> return (t_int ()) (t_nat ())
      | T_base TB_int, T_base TB_nat -> return (t_int ()) (t_nat ())
      | T_base TB_mutez, T_base TB_mutez -> return (t_nat ()) (t_mutez ())
      | T_base TB_mutez, T_base TB_nat -> return (t_mutez ()) (t_mutez ())
      | _ -> raise.error (corner_case ~loc:__LOC__ "invalid external_(ediv|u_ediv) application"))
    | (External ("and" | "u_and"), [ param1; param2 ]) ->
      (match (compile_type param1).type_content, (compile_type param2).type_content with
      | T_base TB_nat, T_base TB_nat -> return (T_base TB_nat)
      | T_base TB_int, T_base TB_nat -> return (T_base TB_nat)
      | _ -> raise.error (corner_case ~loc:__LOC__ "invalid external_(ediv|u_ediv) application"))
    | ((Michelson_or               | Chest_opening_result | Sapling_transaction |
        Ticket                     | Sapling_state        | Michelson_contract  |
        Contract        | Map      | Big_map              | Typed_address       |
        Michelson_pair  | Set      | Mutation             | Ast_contract        |
        List            | External _ | Gen), [])
        -> raise.error @@ corner_case ~loc:__LOC__ "wrong constant"
    | ((             Unit      | Baker_operation      |
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
      List         | External _ | Tx_rollup_l2_address ), _::_) -> raise.error @@ corner_case ~loc:__LOC__ (Format.asprintf "wrong constant\n%a\n" Ast_aggregated.PP.type_expression t)
  )
  | T_sum _ when Option.is_some (AST.get_t_bool t) ->
    return (T_base TB_bool)
  | T_sum _ when Option.is_some (AST.get_t_option t) ->
    let o = trace_option ~raise (corner_case ~loc:__LOC__ ("impossible")) @@ AST.get_t_option t in
    let o' = compile_type o in
    return (T_option o')
  | T_sum { fields = m ; layout } -> (
      let open AST.Helpers in
      match is_michelson_or m with
      | Some (a , b) -> (
          let aux (x : AST.row_element) =
            let t = compile_type x.associated_type in
            let annot = remove_empty_annotation x.michelson_annotation in
            (annot , t)
          in
          let a' = aux a in
          let b' = aux b in
          return @@ T_or (a' , b')
        )
      | None -> Layout.t_sum ~raise ~layout return compile_type m
    )
  | T_record { fields = m ; layout } -> (
      let open AST.Helpers in
      match is_michelson_pair m with
      | Some (a , b) -> (
          let aux (x : AST.row_element) =
            let t = compile_type x.associated_type in
            let annot = remove_empty_annotation x.michelson_annotation in
            (annot , t)
          in
          let a' = aux a in
          let b' = aux b in
          let t = T_tuple [a'; b'] in
          return t
        )
      | None -> Layout.t_record_to_pairs ~layout return compile_type m
    )
  | T_arrow {type1;type2} -> (
      let param' = compile_type type1 in
      let result' = compile_type type2 in
      return @@ (T_function (param',result'))
  )
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

(* todo: refactor handling of recursive functions *)
let compile_record_matching ~raise expr' return k ({ fields; body; tv } : _ AST.matching_content_record) =
  let record =
    trace_option ~raise (corner_case ~loc:__LOC__ "compile_record_matching: getting lr tree") @@
    get_t_record_opt tv in
  match record.layout with
  (* TODO unify with or simplify other case below? *)
  | L_comb ->
    let record_fields = AST.Helpers.kv_list_of_t_record_or_tuple ~layout:L_comb record.fields in
    let fields =
      List.map
        ~f:(fun (l, (row_element : AST.row_element)) ->
          let t = compile_type ~raise row_element.associated_type in
          let x = trace_option ~raise
            (corner_case ~loc:__LOC__ ("missing label in record"))
            Record.(LMap.find_opt l fields)
          in
          (x.var, t)
        )
        record_fields
    in
    let body = k body in
    return (E_let_tuple (expr', (fields, body)))
  | _ ->
    let tree = Layout.record_tree ~layout:record.layout ?source_type:tv.source_type (compile_type ~raise) record.fields in
    let body = k body in
    let rec aux expr (tree : Layout.record_tree) body =
      match tree.content with
      | Field l ->
        let x = trace_option ~raise
          (corner_case ~loc:__LOC__ ("missing label in record"))
          (Record.LMap.find_opt l fields)
        in
        let var = x.var in
        return @@ E_let_in (expr, false, ((var, tree.type_), body))
      | Pair (x, y) ->
        let x_var = Value_var.fresh () in
        let y_var = Value_var.fresh () in
        let x_ty = x.type_ in
        let y_ty = y.type_ in
        let x_var_expr = Combinators.Expression.make_tpl (E_variable x_var, x_ty) in
        let y_var_expr = Combinators.Expression.make_tpl (E_variable y_var, y_ty) in
        let yrec = aux y_var_expr y body in
        let xrec = aux x_var_expr x yrec in
        return @@ E_let_tuple (expr, ([(x_var, x_ty); (y_var, y_ty)], xrec))
    in
    aux expr' tree body

let rec compile_expression ~raise (ae:AST.expression) : expression =
  let tv = compile_type ~raise ae.type_expression in
  let self = compile_expression ~raise in
  let return ?(tv = tv) expr =
    Combinators.Expression.make_tpl ~loc:ae.location (expr, tv) in
  match ae.expression_content with
  | E_type_abstraction _
  | E_type_inst _ ->
    raise.error @@ corner_case ~loc:__LOC__ (Format.asprintf "Type instance: This program should be monomorphised")
  | E_let_in {let_binder; rhs; let_result; attr = { inline; no_mutation=_; view=_; public=_ ; hidden = _ ; thunk = _ } } ->
    let rhs' = self rhs in
    let result' = self let_result in
    return (E_let_in (rhs', inline, ((let_binder.var, rhs'.type_expression), result')))
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
    let path = Layout.constructor_to_lr ~raise ~layout:ty_variant.layout ty' ty_variant.fields constructor in
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
      let record_t = trace_option ~raise (corner_case ~loc:__LOC__ "record expected") (AST.get_t_record_opt ae.type_expression) in
      (* Note: now returns E_tuple, not pairs, for combs *)
      Layout.record_to_pairs ~raise self return record_t m
    )
  | E_accessor {struct_; path} -> (
    let ty' = compile_type ~raise (get_type struct_) in
    let record_ty = trace_option ~raise (corner_case ~loc:__LOC__ "not a record") @@
      get_t_record_opt (get_type struct_) in
    match record_ty.layout with
    | L_comb ->
      let record_fields = AST.Helpers.kv_list_of_t_record_or_tuple ~layout:record_ty.layout record_ty.fields in
      let i = fst @@ Option.value_exn  (List.findi ~f:(fun _ (label, _) -> 0 = Label.compare path label) record_fields) in
      let n = List.length record_fields in
      let struct_ = compile_expression ~raise struct_ in
      return (E_proj (struct_, i, n))
    | _ ->
    let path = Layout.record_access_to_lr ~raise ~layout:record_ty.layout ty' record_ty.fields path in
    let aux = fun pred (ty, lr) ->
      let c : Constant.constant' = match lr with
        | `Left  -> C_CAR
        | `Right -> C_CDR
      in
      return ~tv:ty @@ E_constant {cons_name=c;arguments=[pred]}
    in
    let struct_' = compile_expression ~raise struct_ in
    let expr = List.fold ~f:aux ~init:struct_' path in
    expr
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
      match record_ty.layout with
      | L_comb ->
        let record_fields = AST.Helpers.kv_list_of_t_record_or_tuple ~layout:record_ty.layout record_ty.fields in
        let struct_ = self struct_ in
        let update = self update in
        let i = fst @@ Option.value_exn  (List.findi ~f:(fun _ (label, _) -> 0 = Label.compare path label) record_fields) in
        let n = List.length record_fields in
        return (E_update (struct_, i, update, n))
      | _ ->
      let path =
        trace_strong ~raise (corner_case ~loc:__LOC__ "record access") @@
        (fun ~raise:_ -> Layout.record_access_to_lr ~raise ~layout:record_ty.layout ty' record_ty.fields path) in
      let path = List.map ~f:snd path in
      let update = self update in
      let struct_ = self struct_ in
      let record_var = Value_var.fresh () in
      let car (e : expression) : expression =
        match e.type_expression.type_content with
        | T_tuple [(_, a); _] ->
          { e with
            content = E_constant { cons_name = C_CAR ; arguments = [e] } ;
            type_expression = a }
        | _ -> internal_error __LOC__ "record did not have pair type" in
      let cdr (e : expression) : expression =
        match e.type_expression.type_content with
        | T_tuple [_; (_, b)] ->
          { e with
            content = E_constant { cons_name = C_CDR ; arguments = [e] } ;
            type_expression = b }
        | _ -> internal_error __LOC__ "record did not have pair type" in
      let rec build_record_update record path =
        match path with
        | [] -> update
        | `Left :: path ->
          { record with
            content = E_constant { cons_name = C_PAIR ;
                                   arguments = [ build_record_update (car record) path;
                                                 cdr record ] } }
        | `Right :: path ->
          { record with
            content = E_constant { cons_name = C_PAIR ;
                                   arguments = [ car record;
                                                 build_record_update (cdr record) path ] } } in
      return
        (E_let_in (struct_, false, ((record_var, struct_.type_expression),
                   build_record_update
                     (e_var record_var struct_.type_expression)
                     path)))
  )
  | E_constant {cons_name=name; arguments=lst} -> (
      let iterator_generator (iterator_name : Constant.constant') =
        let expression_to_iterator_body (f : AST.expression) =
          let Arrow.{ type1 = input ; type2 = output } = trace_option ~raise (corner_case ~loc:__LOC__ "expected function type") @@ AST.get_t_arrow f.type_expression in
          let f' = self f in
          let input' = compile_type ~raise input in
          let output' = compile_type ~raise output in
          let binder = Value_var.fresh ~name:"iterated" () in
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
    compile_lambda ~raise l ae.type_expression
  | E_recursive r ->
    compile_recursive ~raise r
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
                (Record.LMap.find_opt (Label c) c_body_lst) in
            let match_nil = get_case "Nil" in
            let match_cons = get_case "Cons" in
            let nil = self (fst match_nil) in
            let cons =
              let hd = Value_var.fresh () in
              let tl = Value_var.fresh () in
              let proj_t = t_pair (None,list_ty) (None,expr'.type_expression) in
              let proj = Expression.make (ec_pair (e_var hd list_ty) (e_var tl expr'.type_expression)) proj_t in
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
                (Record.LMap.find_opt (Label c) c_body_lst) in
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
                (Record.LMap.find_opt (Label c) cases) in
            let match_true  = get_case "True" in
            let match_false = get_case "False" in
            let (t , f) = Pair.map ~f:self (match_true, match_false) in
            return @@ E_if_bool (expr', t, f)
          | _ -> (
              let record_ty = trace_option ~raise (corner_case ~loc:__LOC__ "compile_expression: getting lr tree") @@
                get_t_sum_opt tv in
              let tree = Layout.match_variant_to_tree ~raise ~layout:record_ty.layout ~compile_type:(compile_type ~raise) record_ty.fields in
              let rec aux top t =
                match t with
                | ((`Leaf (Label.Label constructor_name)) , tv) -> (
                    let ({constructor=_ ; pattern ; body} : _ AST.matching_content_case ) =
                      trace_option ~raise (corner_case ~loc:__LOC__ "missing match clause") @@
                      let aux ({constructor = Label c ; pattern=_ ; body=_} : _ AST.matching_content_case) =
                        (String.equal c constructor_name) in
                      List.find ~f:aux cases in
                    let body' = self body in
                    return @@ E_let_in (top, false, ((pattern , tv) , body'))
                  )
                | ((`Node (a , b)) , tv) ->
                  let a' =
                    let a_ty = trace_option ~raise (corner_case ~loc:__LOC__ "wrongtype") @@ get_t_left tv in
                    let left_var = Value_var.fresh ~name:"left" () in
                    let e = aux (((Expression.make (E_variable left_var) a_ty))) a in
                    ((left_var , a_ty) , e)
                  in
                  let b' =
                    let b_ty = trace_option ~raise (corner_case ~loc:__LOC__ "wrongtype") @@ get_t_right tv in
                    let right_var = Value_var.fresh ~name:"right" () in
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
        compile_record_matching ~raise expr' return self record
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
          return ~tv:type_anno' @@ E_raw_michelson code
        | _ ->
          raise.error (raw_michelson_must_be_seq ae.location code)
    )
    | E_assign _ -> failwith "assign should be compiled to let in self-ast-aggregated"

and compile_lambda ~raise l fun_type =
  let { binder ; output_type =_ ; result } : _ Lambda.t = l in
  let result' = compile_expression ~raise result in
  let fun_type = compile_type ~raise fun_type in
  let binder = binder.var in
  let closure = E_closure { binder; body = result'} in
  Combinators.Expression.make_tpl ~loc:result.location (closure , fun_type)

and compile_recursive ~raise {fun_name; fun_type; lambda} =
  let rec map_lambda : Value_var.t -> type_expression -> AST.expression -> expression * Value_var.t list = fun fun_name loop_type e ->
    match e.expression_content with
      E_lambda {binder;output_type=_;result} ->
      let binder   = binder.var in
      let (body,l) = map_lambda  fun_name loop_type result in
      (Expression.make ~loc:e.location (E_closure {binder;body}) loop_type, binder::l)
    | _  ->
      let res = replace_callback ~raise fun_name loop_type false e in
      (res, [])

  and replace_callback ~raise : Value_var.t -> type_expression -> bool -> AST.expression -> expression = fun fun_name loop_type shadowed e ->
    match e.expression_content with
      | E_let_in li ->
        let shadowed = shadowed || Value_var.equal li.let_binder.var fun_name in
        let let_result = replace_callback ~raise fun_name loop_type shadowed li.let_result in
        let rhs = compile_expression ~raise li.rhs in
        let ty  = compile_type ~raise li.rhs.type_expression in
        let let_binder = li.let_binder.var in
        e_let_in let_binder ty li.attr.inline rhs let_result
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
              (Record.LMap.find_opt (Label.of_string c) c_body_lst) in
          let match_nil = get_case "Nil" in
          let match_cons = get_case "Cons" in
          let nil = self (fst match_nil) in
          let cons =
            let hd = Value_var.fresh () in
            let tl = Value_var.fresh () in
            let proj_t = t_pair (None,list_ty) (None,expr'.type_expression) in
            let proj = Expression.make (ec_pair (e_var hd list_ty) (e_var tl expr'.type_expression)) proj_t in
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
              (Record.LMap.find_opt (Label.of_string c) c_body_lst) in
          let match_none = get_case "None" in
          let match_some = get_case "Some" in
          let n = self (fst match_none) in
          let (tv' , s') =
            let tv' = compile_type ~raise opt_tv in
            let s' = self (fst match_some) in
            (tv' , s')
          in
          return @@ E_if_none (expr' , n , (((snd match_some) , tv') , s'))
        | T_sum _ when Option.is_some (AST.get_t_bool m.matchee.type_expression) ->
          let ctor_body (case : _ AST.matching_content_case) = (case.constructor, case.body) in
          let cases = Record.of_list (List.map ~f:ctor_body cases) in
          let get_case c =
            trace_option ~raise
              (corner_case ~loc:__LOC__ ("missing " ^ c ^ " case in match"))
              (Record.LMap.find_opt (Label c) cases) in
          let match_true  = get_case "True" in
          let match_false = get_case "False" in
          let (t , f) = Pair.map ~f:self (match_true, match_false) in
          return @@ E_if_bool (expr', t, f)
        | _ -> (
            let record_ty = trace_option ~raise (corner_case ~loc:__LOC__ "compile_recursive: getting lr tree") @@
              get_t_sum_opt tv in
            let tree = Layout.match_variant_to_tree ~raise ~layout:record_ty.layout ~compile_type:(compile_type ~raise) record_ty.fields in
            let rec aux top t =
              match t with
              | ((`Leaf (Label.Label constructor_name)) , tv) -> (
                  let ({constructor=_ ; pattern ; body} : _ AST.matching_content_case)=
                    trace_option ~raise (corner_case ~loc:__LOC__ "missing match clause") @@
                    let aux ({constructor = Label c ; pattern=_ ; body=_} : _ AST.matching_content_case) =
                      (String.equal c constructor_name) in
                    List.find ~f:aux cases in
                  let body' = self body in
                  return @@ E_let_in (top, false, ((pattern , tv) , body'))
                )
              | ((`Node (a , b)) , tv) ->
                let a' =
                  let a_ty = trace_option ~raise (corner_case ~loc:__LOC__ "wrongtype") @@ get_t_left tv in
                  let left_var = Value_var.fresh ~name:"left" () in
                  let e = aux (((Expression.make (E_variable left_var) a_ty))) a in
                  ((left_var , a_ty) , e)
                in
                let b' =
                  let b_ty = trace_option ~raise (corner_case ~loc:__LOC__ "wrongtype") @@ get_t_right tv in
                  let right_var = Value_var.fresh ~name:"right" () in
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
        compile_record_matching ~raise expr' return (replace_callback ~raise fun_name loop_type shadowed) record
  in
  let fun_type = compile_type ~raise fun_type in
  let (input_type,output_type) = trace_option ~raise (corner_case ~loc:__LOC__ "wrongtype") @@ get_t_function fun_type in
  let loop_type = t_union (None, input_type) (None, output_type) in
  let (body,binder) = map_lambda fun_name loop_type lambda.result in
  let binder = lambda.binder.var :: binder in
  let loc = Value_var.get_location fun_name in
  let binder = match binder with hd::[] -> hd | _ -> raise.error @@ unsupported_recursive_function loc fun_name in
  let expr = Expression.make_tpl (E_variable binder, input_type) in
  let body = Expression.make (E_iterator (C_LOOP_LEFT, ((lambda.binder.var, input_type), body), expr)) output_type in
  Expression.make (E_closure {binder;body}) fun_type

let compile_program ~raise : AST.expression -> Mini_c.expression = fun p ->
  compile_expression ~raise p
