open Trace
open Mini_c
open Michelson
open Memory_proto_alpha.Protocol.Script_ir_translator
open Operators.Compiler

module Errors = struct
  let corner_case ~loc message =
    let title () = "corner case" in
    let content () = "we don't have a good error message for this case. we are
striving find ways to better report them and find the use-cases that generate
them. please report this to the developers." in
    let data = [
      ("location" , fun () -> loc) ;
      ("message" , fun () -> message) ;
    ] in
    error ~data title content

  let contract_entrypoint_must_be_literal ~loc =
    let title () = "contract entrypoint must be literal" in
    let content () = "For get_entrypoint, entrypoint must be given as a literal string" in
    let data =
      [ ("location", fun () -> loc) ;
      ] in
    error ~data title content
end
open Errors

(* This does not makes sense to me *)
let get_operator : constant' -> type_value -> expression list -> predicate result = fun s ty lst ->
  match Operators.Compiler.get_operators s with
  | Ok (x,_) -> ok x
  | Error _ -> (
      match s with
      | C_NONE -> (
          let%bind ty' = Mini_c.get_t_option ty in
          let%bind m_ty = Compiler_type.type_ ty' in
          ok @@ simple_constant @@ prim ~children:[m_ty] I_NONE

        )
      | C_NIL -> (
          let%bind ty' = Mini_c.get_t_list ty in
          let%bind m_ty = Compiler_type.type_ ty' in
          ok @@ simple_unary @@ prim ~children:[m_ty] I_NIL
        )
      | C_SET_EMPTY -> (
          let%bind ty' = Mini_c.get_t_set ty in
          let%bind m_ty = Compiler_type.type_ ty' in
          ok @@ simple_constant @@ prim ~children:[m_ty] I_EMPTY_SET
        )
      | C_BYTES_UNPACK -> (
          let%bind ty' = Mini_c.get_t_option ty in
          let%bind m_ty = Compiler_type.type_ ty' in
          ok @@ simple_unary @@ prim ~children:[m_ty] I_UNPACK
        )
      | C_MAP_REMOVE ->
          let%bind v = match lst with
            | [ _ ; expr ] ->
                let%bind (_, v) = Mini_c.Combinators.(bind_map_or (get_t_map , get_t_big_map) (Expression.get_type expr)) in
                ok v
            | _ -> simple_fail "mini_c . MAP_REMOVE" in
          let%bind v_ty = Compiler_type.type_ v in
          ok @@ simple_binary @@ seq [dip (i_none v_ty) ; prim I_UPDATE ]
      | C_LEFT ->
          let%bind r = match lst with
            | [ _ ] -> get_t_right ty
            | _ -> simple_fail "mini_c . LEFT" in
          let%bind r_ty = Compiler_type.type_ r in
          ok @@ simple_unary @@ prim ~children:[r_ty] I_LEFT
      | C_RIGHT ->
          let%bind l = match lst with
            | [ _ ] -> get_t_left ty
            | _ -> simple_fail "mini_c . RIGHT" in
          let%bind l_ty = Compiler_type.type_ l in
          ok @@ simple_unary @@ prim ~children:[l_ty] I_RIGHT
      | C_CONTRACT ->
          let%bind r = get_t_contract ty in
          let%bind r_ty = Compiler_type.type_ r in
          ok @@ simple_unary @@ seq [
            prim ~children:[r_ty] I_CONTRACT ;
            i_assert_some_msg (i_push_string "bad address for get_contract") ;
          ]
      | C_CONTRACT_OPT -> 
          let%bind tc = get_t_option ty in
          let%bind r = get_t_contract tc in
          let%bind r_ty = Compiler_type.type_ r in
          ok @@ simple_unary @@ prim ~children:[r_ty] I_CONTRACT ;

      | C_CONTRACT_ENTRYPOINT ->
          let%bind r = get_t_contract ty in
          let%bind r_ty = Compiler_type.type_ r in
          let%bind entry = match lst with
            | [ { content = E_literal (D_string entry); type_value = _ } ; _addr ] -> ok entry
            | [ _entry ; _addr ] ->
               fail @@ contract_entrypoint_must_be_literal ~loc:__LOC__
            | _ ->
               fail @@ corner_case ~loc:__LOC__ "mini_c . CONTRACT_ENTRYPOINT" in
          ok @@ simple_binary @@ seq [
            i_drop ; (* drop the entrypoint... *)
            prim ~annot:[entry] ~children:[r_ty] I_CONTRACT ;
            i_assert_some_msg (i_push_string @@ Format.sprintf "bad address for get_entrypoint (%s)" entry) ;
          ]
      | C_CONTRACT_ENTRYPOINT_OPT ->
          let%bind tc = get_t_option ty in
          let%bind r = get_t_contract tc in
          let%bind r_ty = Compiler_type.type_ r in
          let%bind entry = match lst with
            | [ { content = E_literal (D_string entry); type_value = _ } ; _addr ] -> ok entry
            | [ _entry ; _addr ] ->
               fail @@ contract_entrypoint_must_be_literal ~loc:__LOC__
            | _ ->
               fail @@ corner_case ~loc:__LOC__ "mini_c . CONTRACT_ENTRYPOINT" in
          ok @@ simple_binary @@ seq [
            i_drop ; (* drop the entrypoint... *)
            prim ~annot:[entry] ~children:[r_ty] I_CONTRACT ;
          ]
      | x -> simple_fail (Format.asprintf "predicate \"%a\" doesn't exist" PP.constant x)
    )

let rec translate_value (v:value) ty : michelson result = match v with
  | D_bool b -> ok @@ prim (if b then D_True else D_False)
  | D_int n -> ok @@ int (Z.of_int n)
  | D_nat n -> ok @@ int (Z.of_int n)
  | D_timestamp n -> ok @@ int (Z.of_int n)
  | D_mutez n -> ok @@ int (Z.of_int n)
  | D_string s -> ok @@ string s
  | D_bytes s -> ok @@ bytes s
  | D_unit -> ok @@ prim D_Unit
  | D_pair (a, b) -> (
      let%bind (a_ty , b_ty) = get_t_pair ty in
      let%bind a = translate_value a a_ty in
      let%bind b = translate_value b b_ty in
      ok @@ prim ~children:[a;b] D_Pair
    )
  | D_left a -> (
      let%bind (a_ty , _) = get_t_or ty in
      let%bind a' = translate_value a a_ty in
      ok @@ prim ~children:[a'] D_Left
    )
  | D_right b -> (
      let%bind (_ , b_ty) = get_t_or ty in
      let%bind b' = translate_value b b_ty in
      ok @@ prim ~children:[b'] D_Right
    )
  | D_none -> ok @@ prim D_None
  | D_some s ->
      let%bind s' = translate_value s ty in
      ok @@ prim ~children:[s'] D_Some
  | D_map lst -> (
      let%bind (k_ty , v_ty) = get_t_map ty in
      let%bind lst' =
        let aux (k , v) = bind_pair (translate_value k k_ty , translate_value v v_ty) in
        bind_map_list aux lst in
      let sorted = List.sort (fun (x , _) (y , _) -> compare x y) lst' in
      let aux (a, b) = prim ~children:[a;b] D_Elt in
      ok @@ seq @@ List.map aux sorted
    )
  | D_big_map lst -> (
      let%bind (k_ty , v_ty) = get_t_big_map ty in
      let%bind lst' =
        let aux (k , v) = bind_pair (translate_value k k_ty , translate_value v v_ty) in
        bind_map_list aux lst in
      let sorted = List.sort (fun (x , _) (y , _) -> compare x y) lst' in
      let aux (a, b) = prim ~children:[a;b] D_Elt in
      ok @@ seq @@ List.map aux sorted
    )
  | D_list lst -> (
      let%bind e_ty = get_t_list ty in
      let%bind lst' = bind_map_list (fun x -> translate_value x e_ty) lst in
      ok @@ seq lst'
    )
  | D_set lst -> (
      let%bind e_ty = get_t_set ty in
      let%bind lst' = bind_map_list (fun x -> translate_value x e_ty) lst in
      let sorted = List.sort compare lst' in
      ok @@ seq sorted
    )
  | D_operation _ ->
      simple_fail "can't compile an operation"

and translate_expression (expr:expression) (env:environment) : michelson result =
  let (expr' , ty) = Combinators.Expression.(get_content expr , get_type expr) in
  let error_message () =
    Format.asprintf  "\n- expr: %a\n- type: %a\n" PP.expression expr PP.type_variable ty
  in
  let return code = ok code in

  trace (error (thunk "compiling expression") error_message) @@
  match expr' with
  | E_skip -> return @@ i_push_unit
  | E_literal v ->
      let%bind v = translate_value v ty in
      let%bind t = Compiler_type.type_ ty in
      return @@ i_push t v
  | E_closure anon -> (
      match ty with
      | T_function (input_ty , output_ty) ->
        translate_function anon env input_ty output_ty
      | _ -> simple_fail "expected function type"
    )
  | E_application (f , arg) -> (
      trace (simple_error "Compiling quote application") @@
      let%bind f = translate_expression f env in
      let%bind arg = translate_expression arg env in
      return @@ seq [
        arg ;
        dip f ;
        prim I_EXEC ;
      ]
    )
  | E_variable x ->
    let%bind code = Compiler_environment.get env x in
    return code
  | E_sequence (a , b) -> (
    let%bind a' = translate_expression a env in
    let%bind b' = translate_expression b env in
    return @@ seq [
      a' ;
      i_drop ;
      b' ;
    ]
  )
  | E_constant{cons_name=str;arguments= lst} ->
      let module L = Logger.Stateful() in
      let%bind pre_code =
        let aux code expr =
          let%bind expr_code = translate_expression expr env in
          L.log @@ Format.asprintf "\n%a -> %a in %a\n"
            PP.expression expr
            Michelson.pp expr_code
            PP.environment env ;
          ok (seq [ expr_code ; dip code ]) in
        bind_fold_right_list aux (seq []) lst in
      let%bind predicate = get_operator str ty lst in
      let%bind code = match (predicate, List.length lst) with
        | Constant c, 0 -> ok @@ seq [
            pre_code ;
            c ;
          ]
        | Unary f, 1 -> ok @@ seq [
            pre_code ;
            f ;
          ]
        | Binary f, 2 -> ok @@ seq [
            pre_code ;
            f ;
          ]
        | Ternary f, 3 -> ok @@ seq [
            pre_code ;
            f ;
          ]
        | _ -> simple_fail (Format.asprintf "bad arity for %a" PP.constant str)
      in
      let error =
        let title () = "error compiling constant" in
        let content () = L.get () in
        error title content in
      trace error @@
      return code
  | E_make_empty_map sd ->
      let%bind (src, dst) = bind_map_pair Compiler_type.type_ sd in
      return @@ i_empty_map src dst
  | E_make_empty_big_map sd ->
      let%bind (src, dst) = bind_map_pair Compiler_type.type_ sd in
      return @@ i_empty_big_map src dst
  | E_make_empty_list t ->
      let%bind t' = Compiler_type.type_ t in
      return @@ i_nil t'
  | E_make_empty_set t ->
      let%bind t' = Compiler_type.type_ t in
      return @@ i_empty_set t'
  | E_make_none o ->
      let%bind o' = Compiler_type.type_ o in
      return @@ i_none o'
  | E_if_bool (c, a, b) -> (
      let%bind c' = translate_expression c env in
      let%bind a' = translate_expression a env in
      let%bind b' = translate_expression b env in
      let%bind code = ok (seq [
          c' ;
          i_if a' b' ;
        ]) in
      return code
    )
  | E_if_none (c, n, (ntv , s)) -> (
      let%bind c' = translate_expression c env in
      let%bind n' = translate_expression n env in
      let s_env = Environment.add ntv env in
      let%bind s' = translate_expression s s_env in
      let%bind code = ok (seq [
          c' ;
          i_if_none n' (seq [
              s' ;
              dip i_drop ;
            ])
          ;
        ]) in
      return code
    )
  | E_if_cons (cond , nil , ((hd , tl) , cons)) -> (
      let%bind cond' = translate_expression cond env in
      let%bind nil' = translate_expression nil env in
      let s_env =
        Environment.add hd
        @@ Environment.add tl env
      in
      let%bind s' = translate_expression cons s_env in
      let%bind code = ok (seq [
          cond' ;
          i_if_cons (seq [
              s' ;
              dip (seq [ i_drop ; i_drop ]) ;
            ]) nil'
          ;
        ]) in
      return code
    )
  | E_if_left (c, (l_ntv , l), (r_ntv , r)) -> (
      let%bind c' = translate_expression c env in
      let l_env = Environment.add l_ntv env in
      let%bind l' = translate_expression l l_env in
      let r_env = Environment.add r_ntv env in
      let%bind r' = translate_expression r r_env in
      let%bind code = ok (seq [
          c' ;
          i_if_left (seq [
              l' ;
              i_comment "restrict left" ;
              dip i_drop ;
            ]) (seq [
              r' ;
              i_comment "restrict right" ;
              dip i_drop ;
            ])
          ;
        ]) in
      return code
    )
  | E_let_in (v , _, expr , body) -> (
      let%bind expr' = translate_expression expr env in
      let%bind body' = translate_expression body (Environment.add v env) in
      let%bind code = ok (seq [
          expr' ;
          body' ;
          i_comment "restrict let" ;
          dip i_drop ;
        ]) in
      return code
    )
  | E_iterator (name,(v , body) , expr) -> (
      let%bind expr' = translate_expression expr env in
      let%bind body' = translate_expression body (Environment.add v env) in
      match name with
      | C_ITER -> (
          let%bind code = ok (seq [
              expr' ;
              i_iter (seq [body' ; i_drop ; i_drop]) ;
              i_push_unit ;
            ]) in
          return code
        )
      | C_MAP -> (
          let%bind code = ok (seq [
              expr' ;
              i_map (seq [body' ; dip i_drop]) ;
            ]) in
          return code
        )
      | s -> (
          let iter = Format.asprintf "iter %a" PP.constant s in
          let error = error (thunk "bad iterator") (thunk iter) in
          fail error
        )
    )
  | E_fold ((v , body) , collection , initial) -> (
      let%bind collection' = translate_expression collection env in
      let%bind initial' = translate_expression initial env in
      let%bind body' = translate_expression body (Environment.add v env) in
      let code = seq [
          collection' ;
          dip initial' ;
          i_iter (seq [
              i_swap ;
              i_pair ; body' ; dip i_drop ;
            ]) ;
        ] in
      ok code
    )
  | E_record_update (record, path, expr) -> (
    let%bind record' = translate_expression record env in

    let record_var = Var.fresh () in
    let env' = Environment.add (record_var, record.type_value) env in
    let%bind expr' = translate_expression expr env' in
    let modify_code =
      let aux acc step = match step with
        | `Left -> seq [dip i_unpair ; acc ; i_pair]
        | `Right -> seq [dip i_unpiar ; acc ; i_piar]
      in
      let init = dip i_drop in
      List.fold_right' aux init path
    in
    return @@ seq [
      i_comment "r_update: start # env";
      record';
      i_comment "r_update: move the record on top # env";
      expr';
      i_comment "r_updates : compute rhs # rhs:env";
      modify_code;
      i_comment "r_update: modify code # record+rhs : env";
      ]

  )
  | E_while (expr , block) -> (
      let%bind expr' = translate_expression expr env in
      let%bind block' = translate_expression block env in
      return @@ seq [
        expr' ;
        prim ~children:[seq [
            block' ;
            i_drop ;
            expr']] I_LOOP ;
        i_push_unit ;
      ]
    )

and translate_function_body ({body ; binder} : anon_function) lst input : michelson result =
  let pre_env = Environment.of_list lst in
  let env = Environment.(add (binder , input) pre_env) in
  let%bind expr_code = translate_expression body env in
  let%bind unpack_closure_code = Compiler_environment.unpack_closure pre_env in
  let code = seq [
      i_comment "unpack closure env" ;
      unpack_closure_code ;
      i_comment "function result" ;
      expr_code ;
      i_comment "remove env" ;
      dip i_drop ;
      seq (List.map (Function.constant (dip i_drop)) lst) ;
    ] in

  ok code

and translate_function anon env input_ty output_ty : michelson result =
  let fvs = Mini_c.Free_variables.lambda [] anon in
  let small_env = Mini_c.Environment.select fvs env in
  let%bind lambda_ty = Compiler_type.lambda_closure (small_env , input_ty , output_ty) in
  let%bind lambda_body_code = translate_function_body anon small_env input_ty in
  match fvs with
  | [] -> ok @@ seq [ i_push lambda_ty lambda_body_code ]
  | _ :: _ ->
    let selector = List.map fst small_env in
    let%bind closure_pack_code = Compiler_environment.pack_closure env selector in
    ok @@ seq [
      closure_pack_code ;
      i_push lambda_ty lambda_body_code ;
      i_swap ;
      i_apply ;
    ]

type compiled_expression = {
  expr_ty : ex_ty ;
  expr : michelson ;
}
