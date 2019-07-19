open Trace
open Mini_c

open Michelson
module Stack = Meta_michelson.Stack
module Contract_types = Meta_michelson.Types

open Memory_proto_alpha.Script_ir_translator

open Operators.Compiler

open Proto_alpha_utils

let get_predicate : string -> type_value -> expression list -> predicate result = fun s ty lst ->
  match Map.String.find_opt s Operators.Compiler.predicates with
  | Some x -> ok x
  | None -> (
      match s with
      | "NONE" -> (
          let%bind ty' = Mini_c.get_t_option ty in
          let%bind m_ty = Compiler_type.type_ ty' in
          ok @@ simple_unary @@ prim ~children:[m_ty] I_NONE
        )
      | "NIL" -> (
          let%bind ty' = Mini_c.get_t_list ty in
          let%bind m_ty = Compiler_type.type_ ty' in
          ok @@ simple_unary @@ prim ~children:[m_ty] I_NIL
        )
      | "UNPACK" -> (
          let%bind ty' = Mini_c.get_t_option ty in
          let%bind m_ty = Compiler_type.type_ ty' in
          ok @@ simple_unary @@ prim ~children:[m_ty] I_UNPACK
        )
      | "MAP_REMOVE" ->
          let%bind v = match lst with
            | [ _ ; expr ] ->
                let%bind (_, v) = Mini_c.Combinators.(get_t_map (Expression.get_type expr)) in
                ok v
            | _ -> simple_fail "mini_c . MAP_REMOVE" in
          let%bind v_ty = Compiler_type.type_ v in
          ok @@ simple_binary @@ seq [dip (i_none v_ty) ; prim I_UPDATE ]
      | "LEFT" ->
          let%bind r = match lst with
            | [ _ ] -> get_t_right ty
            | _ -> simple_fail "mini_c . LEFT" in
          let%bind r_ty = Compiler_type.type_ r in
          ok @@ simple_unary @@ prim ~children:[r_ty] I_LEFT
      | "RIGHT" ->
          let%bind l = match lst with
            | [ _ ] -> get_t_left ty
            | _ -> simple_fail "mini_c . RIGHT" in
          let%bind l_ty = Compiler_type.type_ l in
          ok @@ simple_unary @@ prim ~children:[l_ty] I_RIGHT
      | "CONTRACT" ->
          let%bind r = match lst with
            | [ _ ] -> get_t_contract ty
            | _ -> simple_fail "mini_c . CONTRACT" in
          let%bind r_ty = Compiler_type.type_ r in
          ok @@ simple_unary @@ seq [
            prim ~children:[r_ty] I_CONTRACT ;
            i_assert_some_msg (i_push_string "bad address for get_contract") ;
          ]
      | x -> simple_fail ("predicate \"" ^ x ^ "\" doesn't exist")
    )

let rec translate_value (v:value) : michelson result = match v with
  | D_bool b -> ok @@ prim (if b then D_True else D_False)
  | D_int n -> ok @@ int (Z.of_int n)
  | D_nat n -> ok @@ int (Z.of_int n)
  | D_timestamp n -> ok @@ int (Z.of_int n)
  | D_tez n -> ok @@ int (Z.of_int n)
  | D_string s -> ok @@ string s
  | D_bytes s -> ok @@ bytes (Tezos_stdlib.MBytes.of_bytes s)
  | D_unit -> ok @@ prim D_Unit
  | D_pair (a, b) -> (
      let%bind a = translate_value a in
      let%bind b = translate_value b in
      ok @@ prim ~children:[a;b] D_Pair
    )
  | D_left a -> translate_value a >>? fun a -> ok @@ prim ~children:[a] D_Left
  | D_right b -> translate_value b >>? fun b -> ok @@ prim ~children:[b] D_Right
  | D_function anon -> translate_function anon
  | D_none -> ok @@ prim D_None
  | D_some s ->
      let%bind s' = translate_value s in
      ok @@ prim ~children:[s'] D_Some
  | D_map lst ->
      let%bind lst' = bind_map_list (bind_map_pair translate_value) lst in
      let aux (a, b) = prim ~children:[a;b] D_Elt in
      ok @@ seq @@ List.map aux lst'
  | D_list lst ->
      let%bind lst' = bind_map_list translate_value lst in
      ok @@ seq lst'
  | D_set lst ->
      let%bind lst' = bind_map_list translate_value lst in
      ok @@ seq lst'
  | D_operation _ ->
      simple_fail "can't compile an operation"

and translate_function (content:anon_function) : michelson result =
  let%bind body = translate_quote_body content in
  ok @@ seq [ body ]

and translate_expression ?push_var_name (expr:expression) (env:environment) : (michelson * environment) result =
  let (expr' , ty) = Combinators.Expression.(get_content expr , get_type expr) in
  let error_message () =
    Format.asprintf  "\n- expr: %a\n- type: %a\n" PP.expression expr PP.type_ ty
  in
  (* let i_skip = i_push_unit in *)

  let return ?prepend_env ?end_env ?(unit_opt = false) code =
    let code =
      if unit_opt && push_var_name <> None
      then seq [code ; i_push_unit]
      else code
    in
    let%bind env' =
      match (prepend_env , end_env , push_var_name) with
      | (Some _ , Some _ , _) ->
         simple_fail ("two args to return at " ^ __LOC__)
      | None , None , None ->
         ok @@ Environment.add ("_tmp_expression" , ty) env
      | None , None , Some push_var_name ->
         ok @@ Environment.add (push_var_name , ty) env
      | Some prepend_env , None , None ->
        ok @@ Environment.add ("_tmp_expression" , ty) prepend_env
      | Some prepend_env , None , Some push_var_name ->
        ok @@ Environment.add (push_var_name , ty) prepend_env
      | None , Some end_env , None ->
         ok end_env
      | None , Some end_env , Some push_var_name -> (
         if unit_opt
         then ok @@ Environment.add (push_var_name , ty) end_env
         else ok end_env
      )
    in
    let%bind (Stack.Ex_stack_ty input_stack_ty) = Compiler_type.Ty.environment env in
    let%bind output_type = Compiler_type.type_ ty in
    let%bind (Stack.Ex_stack_ty output_stack_ty) = Compiler_type.Ty.environment env' in
    let error_message () =
      let%bind schema_michelsons = Compiler_type.environment env in
      ok @@ Format.asprintf
        "expression : %a\ncode : %a\npreenv : %a\npostenv : %a\nschema type : %a\noutput type : %a"
        PP.expression expr
        Michelson.pp code
        PP.environment env
        PP.environment env'
        PP_helpers.(list_sep Michelson.pp (const ".")) schema_michelsons
        Michelson.pp output_type
    in
    let%bind _ =
      Trace.trace_tzresult_lwt_r
        (fun () ->
           let%bind error_message = error_message () in
           ok @@ (fun () -> error (thunk "error parsing expression code")
                                  (fun () -> error_message)
                                  ())) @@
      Memory_proto_alpha.parse_michelson code
        input_stack_ty output_stack_ty
    in
    ok (code , env')
  in

  trace (error (thunk "compiling expression") error_message) @@
  match expr' with
  | E_skip -> return ~end_env:env ~unit_opt:true @@ seq []
  | E_environment_capture c ->
      let%bind code = Compiler_environment.pack_select env c in
      return @@ code
  | E_environment_load (expr , load_env) -> (
      let%bind (expr' , _) = translate_expression ~push_var_name:"env_to_load" expr env in
      let%bind clear = Compiler_environment.select env [] in
      let%bind unpack = Compiler_environment.unpack load_env in
      return ~end_env:load_env @@ seq [
        expr' ;
        dip clear ;
        unpack ;
      ]
  )
  | E_environment_select sub_env ->
      let%bind code = Compiler_environment.select_env env sub_env in
      return ~end_env:sub_env @@ seq [
        code ;
      ]
  | E_environment_return expr -> (
      let%bind (expr' , env) = translate_expression ~push_var_name:"return_clause" expr env in
      let%bind (code , cleared_env) = Compiler_environment.clear env in
      Format.printf "pre env %a\n" PP.environment env ;
      Format.printf "post clean env %a\n" PP.environment cleared_env ;
      return ~end_env:cleared_env @@ seq [
        expr' ;
        code ;
      ]
    )
  | E_literal v ->
      let%bind v = translate_value v in
      let%bind t = Compiler_type.type_ ty in
      return @@ i_push t v
  | E_application(f, arg) -> (
      match Combinators.Expression.get_type f with
      | T_function _ -> (
          trace (simple_error "Compiling quote application") @@
          let%bind (f , env') = translate_expression ~push_var_name:"application_f" f env in
          let%bind (arg , _) = translate_expression ~push_var_name:"application_arg" arg env' in
          return @@ seq [
            i_comment "quote application" ;
            i_comment "get f" ;
            f ;
            i_comment "get arg" ;
            arg ;
            prim I_EXEC ;
          ]
        )
      | T_deep_closure (small_env, input_ty , _) -> (
          trace (simple_error "Compiling deep closure application") @@
          let%bind (arg' , env') = translate_expression ~push_var_name:"closure_arg" arg env in
          let%bind (f' , env'') = translate_expression ~push_var_name:"closure_f" f env' in
          let%bind f_ty = Compiler_type.type_ f.type_value in
          let%bind append_closure = Compiler_environment.add_packed_anon small_env input_ty in
          let error =
            let error_title () = "michelson type-checking closure application" in
            let error_content () =
              Format.asprintf "\nEnv. %a\nEnv'. %a\nEnv''. %a\nclosure. %a ; %a ; %a\narg. %a\n"
                PP.environment env
                PP.environment env'
                PP.environment env''
                PP.expression_with_type f Michelson.pp f_ty Michelson.pp f'
                PP.expression_with_type arg
            in
            error error_title error_content
          in
          trace error @@
          return @@ seq [
            i_comment "closure application" ;
            i_comment "arg" ;
            arg' ;
            i_comment "f'" ;
            f' ; i_unpair ;
            i_comment "append" ;
            dip @@ seq [i_swap ; append_closure] ;
            i_comment "exec" ;
            i_swap ; i_exec ;
          ]
        )
      | _ -> simple_fail "E_applicationing something not appliable"
    )
  | E_variable x ->
    let%bind code = Compiler_environment.get env x in
    return code
  | E_sequence (a , b) -> (
    let%bind (a' , env_a) = translate_expression a env in
    let%bind (b' , env_b) = translate_expression b env_a in
    return ~end_env:env_b @@ seq [
      a' ;
      b' ;
    ]
  )
  | E_constant(str, lst) ->
      let module L = Logger.Stateful() in
      let%bind lst' =
        let aux env expr =
          let%bind (code , env') = translate_expression ~push_var_name:"constant_argx" expr env in
          L.log @@ Format.asprintf "\n%a -> %a in %a\n"
            PP.expression expr
            Michelson.pp code
            PP.environment env ;
          ok (env' , code)
        in
        bind_fold_map_right_list aux env lst in
      let%bind predicate = get_predicate str ty lst in
      let pre_code = seq @@ List.rev lst' in
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
        | _ -> simple_fail "bad arity"
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
      let%bind (c' , env') = translate_expression ~push_var_name:"bool_condition" c env in
      let%bind popped = Compiler_environment.pop env' in
      let%bind (a' , env_a') = translate_expression ~push_var_name:"if_true" a popped in
      let%bind (b' , _env_b') = translate_expression ~push_var_name:"if_false" b popped in
      let%bind code = ok (seq [
          c' ;
          i_if a' b' ;
        ]) in
      return ~end_env:env_a' code
    )
  | E_if_none (c, n, (ntv , s)) -> (
      let%bind (c' , env') = translate_expression ~push_var_name:"if_none_condition" c env in
      let%bind popped = Compiler_environment.pop env' in
      let%bind (n' , _) = translate_expression ~push_var_name:"if_none" n popped in
      let s_env = Environment.add ntv popped in
      let%bind (s' , s_env') = translate_expression ~push_var_name:"if_some" s s_env in
      let%bind popped' = Compiler_environment.pop s_env' in
      let%bind restrict_s = Compiler_environment.select_env popped' popped in
      let%bind code = ok (seq [
          c' ;
          i_if_none n' (seq [
              s' ;
              dip restrict_s ;
            ])
          ;
        ]) in
      return code
    )
  | E_if_left (c, (l_ntv , l), (r_ntv , r)) -> (
      let%bind (c' , _env') = translate_expression ~push_var_name:"if_left_cond" c env in
      let l_env = Environment.add l_ntv env in
      let%bind (l' , _l_env') = translate_expression ~push_var_name:"if_left" l l_env in
      let r_env = Environment.add r_ntv env in
      let%bind (r' , _r_env') = translate_expression ~push_var_name:"if_right" r r_env in
      let%bind restrict_l = Compiler_environment.select_env l_env env in
      let%bind restrict_r = Compiler_environment.select_env r_env env in
      let%bind code = ok (seq [
          c' ;
          i_if_left (seq [
              l' ;
              i_comment "restrict left" ;
              dip restrict_l ;
            ]) (seq [
              r' ;
              i_comment "restrict right" ;
              dip restrict_r ;
            ])
          ;
        ]) in
      return code
    )
  | E_let_in (v , expr , body) -> (
      let%bind (expr' , expr_env) = translate_expression ~push_var_name:"let_expr" expr env in
      let%bind env' =
        let%bind popped = Compiler_environment.pop expr_env in
        ok @@ Environment.add v popped in
      let%bind (body' , body_env) = translate_expression ~push_var_name:"let_body" body env' in
      let%bind restrict =
        let%bind popped = Compiler_environment.pop body_env in
        Compiler_environment.select_env popped env in
      let%bind code = ok (seq [
          expr' ;
          body' ;
          i_comment "restrict let" ;
          dip restrict ;
        ]) in
      return code
    )
  | E_assignment (name , lrs , expr) -> (
      let%bind (expr' , env') = translate_expression ~push_var_name:"assignment_expr" expr env in
      let%bind get_code = Compiler_environment.get env' name in
      let modify_code =
        let aux acc step = match step with
          | `Left -> seq [dip i_unpair ; acc ; i_pair]
          | `Right -> seq [dip i_unpiar ; acc ; i_piar]
        in
        let init = dip i_drop in
        List.fold_right' aux init lrs
      in
      let%bind set_code = Compiler_environment.set env name in
      let error =
        let title () = "michelson type-checking patch" in
        let content () =
          let aux ppf = function
            | `Left -> Format.fprintf ppf "left"
            | `Right -> Format.fprintf ppf "right" in
          Format.asprintf "Sub path: %a\n"
            PP_helpers.(list_sep aux (const " , ")) lrs
        in
        error title content in
      trace error @@
      return ~end_env:env ~unit_opt:true @@ seq [
        i_comment "assign: start # env" ;
        expr' ;
        i_comment "assign: compute rhs # rhs : env" ;
        get_code ;
        i_comment "assign: get name # name : rhs : env" ;
        i_swap ;
        i_comment "assign: swap # rhs : name : env" ;
        modify_code ;
        i_comment "assign: modify code # name+rhs : env" ;
        set_code ;
        i_comment "assign: set new # new_env" ;
      ]
    )
  | E_while (expr , block) -> (
      let%bind (expr' , env') = translate_expression ~push_var_name:"while_expr" expr env in
      let%bind popped = Compiler_environment.pop env' in
      let%bind (block' , env'') = translate_expression block popped in
      let%bind restrict_block = Compiler_environment.select_env env'' popped in
      return ~end_env:env ~unit_opt:true @@ seq [
        expr' ;
        prim ~children:[seq [
            block' ;
            restrict_block ;
            expr']] I_LOOP ;
      ]
    )

and translate_quote_body ({result ; binder ; input} as f:anon_function) : michelson result =
  let env = Environment.(add (binder , input) empty) in
  let%bind (expr , env') = translate_expression result env in
  let code = seq [
      i_comment "function result" ;
      expr ;
    ] in

  let%bind _assert_type =
    let%bind (Ex_ty input_ty) = Compiler_type.Ty.type_ f.input in
    let%bind (Ex_ty output_ty) = Compiler_type.Ty.type_ f.output in
    let input_stack_ty = Stack.(input_ty @: nil) in
    let output_stack_ty = Stack.(output_ty @: nil) in
    let error_message () =
      Format.asprintf
        "\nCode : %a\nMichelson code : %a\ninput : %a\noutput : %a\nstart env : %a\nend env : %a\n"
        PP.expression result
        Michelson.pp code
        PP.type_ f.input
        PP.type_ f.output
        PP.environment env
        PP.environment env'
    in
    let%bind _ =
      Trace.trace_tzresult_lwt (
        error (thunk "error parsing quote code") error_message
      ) @@
      Proto_alpha_utils.Memory_proto_alpha.parse_michelson code
        input_stack_ty output_stack_ty
    in
    ok ()
  in

  ok code

type compiled_program = {
  input : ex_ty ;
  output : ex_ty ;
  body : michelson ;
}

let get_main : program -> string -> anon_function result = fun p entry ->
  let is_main (((name , expr), _):toplevel_statement) =
    match Combinators.Expression.(get_content expr , get_type expr)with
    | (E_literal (D_function content) , T_function _)
      when name = entry ->
        Some content
    | _ ->  None
  in
  let%bind main =
    trace_option (simple_error "no functional entry") @@
    List.find_map is_main p
  in
  ok main

let translate_program (p:program) (entry:string) : compiled_program result =
  let%bind main = get_main p entry in
  let {input;output} : anon_function = main in
  let%bind body = translate_quote_body main in
  let%bind input = Compiler_type.Ty.type_ input in
  let%bind output = Compiler_type.Ty.type_ output in
  ok ({input;output;body}:compiled_program)

let translate_entry (p:anon_function) : compiled_program result =
  let {input;output} : anon_function = p in
  let%bind body =
    trace (simple_error "compile entry body") @@
    translate_quote_body p in
  let%bind input = Compiler_type.Ty.type_ input in
  let%bind output = Compiler_type.Ty.type_ output in
  ok ({input;output;body}:compiled_program)

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
end
open Errors

let translate_contract : anon_function -> michelson result = fun f ->
  let%bind compiled_program =
    trace_strong (corner_case ~loc:__LOC__ "compiling") @@
    translate_entry f in
  let%bind (param_ty , storage_ty) = Combinators.get_t_pair f.input in
  let%bind param_michelson = Compiler_type.type_ param_ty in
  let%bind storage_michelson = Compiler_type.type_ storage_ty in
  let contract = Michelson.contract param_michelson storage_michelson compiled_program.body in
  ok contract
