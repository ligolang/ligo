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
  | D_operation _ ->
      simple_fail "can't compile an operation"

and translate_function (content:anon_function) : michelson result =
  let%bind body = translate_quote_body content in
  ok @@ seq [ body ]

and translate_expression ?(first=false) (expr:expression) (env:environment) : (michelson * environment) result =
  let (expr' , ty) = Combinators.Expression.(get_content expr , get_type expr) in
  let error_message () = Format.asprintf  "%a" PP.expression expr in

  let return ?env' code =
    let env' =
      let default = env in
      Environment.add ("_tmp_expression" , ty) @@ Option.unopt ~default env' in
    let%bind (Stack.Ex_stack_ty input_stack_ty) = Compiler_type.Ty.environment env in
    let%bind output_type = Compiler_type.type_ ty in
    let%bind (Stack.Ex_stack_ty output_stack_ty) = Compiler_type.Ty.environment env' in
    let error_message () =
      let%bind schema_michelsons = Compiler_type.environment env in
      ok @@ Format.asprintf
        "expression : %a\ncode : %a\nschema type : %a\noutput type : %a"
        PP.expression expr
        Michelson.pp code
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
  | E_capture_environment c ->
      let%bind code = Compiler_environment.pack_select env c in
      return @@ code
  | E_literal v ->
      let%bind v = translate_value v in
      let%bind t = Compiler_type.type_ ty in
      return @@ i_push t v
  | E_application(f, arg) -> (
      match Combinators.Expression.get_type f with
      | T_function _ -> (
          trace (simple_error "Compiling quote application") @@
          let%bind (f , env') = translate_expression ~first f env in
          let%bind (arg , _) = translate_expression arg env' in
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
          let%bind (arg' , env') = translate_expression arg env in
          let%bind (f' , env'') = translate_expression f env' in
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
  | E_constant(str, lst) ->
      let module L = Logger.Stateful() in
      let%bind lst' =
        let aux env expr =
          let%bind (code , env') = translate_expression expr env in
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
  | E_empty_map sd ->
      let%bind (src, dst) = bind_map_pair Compiler_type.type_ sd in
      return @@ i_empty_map src dst
  | E_empty_list t ->
      let%bind t' = Compiler_type.type_ t in
      return @@ i_nil t'
  | E_make_none o ->
      let%bind o' = Compiler_type.type_ o in
      return @@ i_none o'
  | E_Cond (c, a, b) -> (
      let%bind (c' , env') = translate_expression c env in
      let%bind (a' , _) = translate_expression a env' in
      let%bind (b' , _) = translate_expression b env' in
      let%bind code = ok (seq [
          c' ;
          i_if a' b' ;
        ]) in
      return code
    )
  | E_if_none (c, n, (_ , s)) -> (
      let%bind (c' , _env') = translate_expression c env in
      let%bind (n' , _) = translate_expression n n.environment in
      let%bind (s' , _) = translate_expression s s.environment in
      let%bind restrict_s = Compiler_environment.select_env s.environment env in
      let%bind code = ok (seq [
          c' ;
          i_if_none n' (seq [
              s' ;
              restrict_s ;
            ])
          ;
        ]) in
      return code
    )
  | E_if_left (c, (_ , l), (_ , r)) -> (
      let%bind (c' , _env') = translate_expression c env in
      let%bind (l' , _) = translate_expression l l.environment in
      let%bind (r' , _) = translate_expression r r.environment in
      let%bind restrict_l = Compiler_environment.select_env l.environment env in
      let%bind restrict_r = Compiler_environment.select_env r.environment env in
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
  | E_let_in (v, expr , body) -> (
      let%bind (expr' , _) = translate_expression expr env in
      let env' = Environment.add v env in
      let%bind (body' , _) = translate_expression body env' in
      let%bind restrict = Compiler_environment.select_env env' env in
      let%bind code = ok (seq [
          expr' ;
          body' ;
          i_comment "restrict let" ;
          dip restrict ;
        ]) in
      return code
    )

and translate_statement ((s', w_env) as s:statement) : michelson result =
  let error_message () = Format.asprintf "%a" PP.statement s in
  let return code =
    let%bind (Stack.Ex_stack_ty input_stack_ty) = Compiler_type.Ty.environment w_env.pre_environment in
    let%bind (Stack.Ex_stack_ty output_stack_ty) = Compiler_type.Ty.environment w_env.post_environment in
    let error_message () =
      let%bind pre_env_michelson = Compiler_type.environment w_env.pre_environment in
      let%bind post_env_michelson = Compiler_type.environment w_env.post_environment in
      ok @@ Format.asprintf
        "statement : %a\ncode : %a\npre type : %a\npost type : %a\n"
        PP.statement s
        Michelson.pp code
        PP_helpers.(list_sep Michelson.pp (const " ; ")) pre_env_michelson
        PP_helpers.(list_sep Michelson.pp (const " ; ")) post_env_michelson
    in
    let%bind _ =
      Trace.trace_tzresult_lwt_r (fun () -> let%bind error_message = error_message () in
                                   ok (fun () -> error (thunk "error parsing statement code")
                                                 (fun () -> error_message)
                                                 ())) @@
      Proto_alpha_utils.Memory_proto_alpha.parse_michelson_fail code
        input_stack_ty output_stack_ty
    in
    ok code
  in

  trace (fun () -> error (thunk "compiling statement") error_message ()) @@ match s' with
  | S_environment_add _ ->
      simple_fail "add not ready yet"
  | S_environment_select sub_env ->
      let%bind code = Compiler_environment.select_env w_env.pre_environment sub_env in
      return code
  | S_environment_load (expr , env) ->
      let%bind (expr' , _) = translate_expression expr w_env.pre_environment in
      let%bind clear = Compiler_environment.select w_env.pre_environment [] in
      let%bind unpack = Compiler_environment.unpack env in
      return @@ seq [
        expr' ;
        dip clear ;
        unpack ;
      ]
  | S_declaration (s, expr) ->
      let tv = Combinators.Expression.get_type expr in
      let%bind (expr , _) = translate_expression expr w_env.pre_environment in
      let%bind add = Compiler_environment.add w_env.pre_environment (s, tv) in
      return @@ seq [
        i_comment "declaration" ;
        seq [
          i_comment "expr" ;
          expr ;
        ] ;
        seq [
          i_comment "env <- env . expr" ;
          add ;
        ];
      ]
  | S_assignment (s, expr) ->
      let%bind (expr , _) = translate_expression expr w_env.pre_environment in
      let%bind set = Compiler_environment.set w_env.pre_environment s in
      return @@ seq [
        i_comment "assignment" ;
        seq [
          i_comment "expr" ;
          expr ;
        ] ;
        seq [
          i_comment "env <- env . expr" ;
          set ;
        ];
      ]
  | S_cond (expr, a, b) ->
      let%bind (expr , _) = translate_expression expr w_env.pre_environment in
      let%bind a' = translate_regular_block a in
      let%bind b' = translate_regular_block b in
      return @@ seq [
        expr ;
        prim ~children:[seq [a'];seq [b']] I_IF ;
      ]
  | S_do expr -> (
      match Combinators.Expression.get_content expr with
      | E_constant ("FAILWITH" , [ fw ] ) -> (
          let%bind (fw' , _) = translate_expression fw w_env.pre_environment in
          return @@ seq [
            fw' ;
            i_failwith ;
          ]
        )
      | _ -> (
          let%bind (expr' , _) = translate_expression expr w_env.pre_environment in
          return @@ seq [
            expr' ;
            i_drop ;
          ]
        )
    )
  | S_if_none (expr, none, ((name, tv), some)) ->
      let%bind (expr , _) = translate_expression expr w_env.pre_environment in
      let%bind none' = translate_regular_block none in
      let%bind some' = translate_regular_block some in
      let%bind add =
        let env' = w_env.pre_environment in
        Compiler_environment.add env' (name, tv) in
      let%bind restrict_s = Compiler_environment.select_env (snd some).post_environment w_env.pre_environment in
      return @@ seq [
        expr ;
        prim ~children:[
          seq [none'] ;
          seq [add ; some' ; restrict_s] ;
        ] I_IF_NONE
      ]
  | S_while (expr, block) ->
      let%bind (expr , _) = translate_expression expr w_env.pre_environment in
      let%bind block' = translate_regular_block block in
      let%bind restrict_block =
        let env_while = (snd block).pre_environment in
        Compiler_environment.select_env (snd block).post_environment env_while in
      return @@ seq [
        expr ;
        prim ~children:[seq [
            block' ;
            restrict_block ;
            expr]] I_LOOP ;
      ]
  | S_patch (name, lrs, expr) ->
      let%bind (expr' , env') = translate_expression expr w_env.pre_environment in
      let%bind get_code = Compiler_environment.get env' name in
      let modify_code =
        let aux acc step = match step with
          | `Left -> seq [dip i_unpair ; acc ; i_pair]
          | `Right -> seq [dip i_unpiar ; acc ; i_piar]
        in
        let init = dip i_drop in
        List.fold_right' aux init lrs
      in
      let%bind set_code = Compiler_environment.set w_env.pre_environment name in
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
      return @@ seq [
        expr' ;
        get_code ;
        i_swap ; modify_code ;
        set_code ;
      ]

and translate_regular_block ((b, env):block) : michelson result =
  let aux prev statement =
    let%bind (lst : michelson list) = prev in
    let%bind instruction = translate_statement statement in
    ok (instruction :: lst)
  in
  let%bind codes =
    let error_message () =
      let%bind schema_michelsons = Compiler_type.environment env.pre_environment in
      ok @@ Format.asprintf "\nblock : %a\nschema : %a\n"
        PP.block (b, env)
        PP_helpers.(list_sep Michelson.pp (const " ; ")) schema_michelsons
    in
    trace_r (fun () ->
        let%bind error_message = error_message () in
        ok (fun () -> error (thunk "compiling regular block")
                      (fun () -> error_message)
                      ())) @@
    List.fold_left aux (ok []) b in
  let code = seq (List.rev codes) in
  ok code

and translate_quote_body ({body;result} as f:anon_function) : michelson result =
  let%bind body' = translate_regular_block body in
  let%bind (expr , _) = translate_expression result (snd body).post_environment in
  let%bind restrict = Compiler_environment.clear (snd body).post_environment in
  let code = seq [
      i_comment "function body" ;
      body' ;
      i_comment "function result" ;
      expr ;
      dip restrict ;
    ] in

  let%bind _assert_type =
    let%bind (Ex_ty input_ty) = Compiler_type.Ty.type_ f.input in
    let%bind (Ex_ty output_ty) = Compiler_type.Ty.type_ f.output in
    let input_stack_ty = Stack.(input_ty @: nil) in
    let output_stack_ty = Stack.(output_ty @: nil) in
    let error_message () =
      Format.asprintf
        "\ncode : %a\ninput : %a\noutput : %a\nenv : %a\n"
        Michelson.pp code
        PP.type_ f.input
        PP.type_ f.output
        PP.environment (snd body).post_environment
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

let translate_contract : anon_function -> michelson result = fun f ->
  let%bind compiled_program = translate_entry f in
  let%bind (param_ty , storage_ty) = Combinators.get_t_pair f.input in
  let%bind param_michelson = Compiler_type.type_ param_ty in
  let%bind storage_michelson = Compiler_type.type_ storage_ty in
  let contract = Michelson.contract param_michelson storage_michelson compiled_program.body in
  ok contract
