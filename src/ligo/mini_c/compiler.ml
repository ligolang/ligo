open Trace
open Types

module Michelson = Micheline.Michelson
open Michelson
module Environment = Compiler_environment
module Stack = Meta_michelson.Stack
module Contract_types = Meta_michelson.Types

open Memory_proto_alpha.Script_ir_translator

type predicate =
  | Constant of michelson
  | Unary of michelson
  | Binary of michelson
  | Ternary of michelson

let simple_constant c = Constant ( seq [
    c ; i_pair ;
  ])

let simple_unary c = Unary ( seq [
    i_unpair ; c ; i_pair ;
  ])

let simple_binary c = Binary ( seq [
    i_unpair ; dip i_unpair ; i_swap ; c ; i_pair ;
  ])

let simple_ternary c = Ternary ( seq [
    i_unpair ; dip i_unpair ; dip (dip i_unpair) ; i_swap ; dip i_swap ; i_swap ; c ; i_pair ;
  ])

let rec get_predicate : string -> expression list -> predicate result = fun s lst ->
  match s with
  | "ADD_INT" -> ok @@ simple_binary @@ prim I_ADD
  | "ADD_NAT" -> ok @@ simple_binary @@ prim I_ADD
  | "TIMES_INT" -> ok @@ simple_binary @@ prim I_MUL
  | "TIMES_NAT" -> ok @@ simple_binary @@ prim I_MUL
  | "NEG" -> ok @@ simple_unary @@ prim I_NEG
  | "OR" -> ok @@ simple_binary @@ prim I_OR
  | "AND" -> ok @@ simple_binary @@ prim I_AND
  | "PAIR" -> ok @@ simple_binary @@ prim I_PAIR
  | "CAR" -> ok @@ simple_unary @@ prim I_CAR
  | "CDR" -> ok @@ simple_unary @@ prim I_CDR
  | "EQ" -> ok @@ simple_binary @@ seq [prim I_COMPARE ; prim I_EQ]
  | "LT" -> ok @@ simple_binary @@ seq [prim I_COMPARE ; prim I_LT]
  | "UPDATE" -> ok @@ simple_ternary @@ prim I_UPDATE
  | "SOME" -> ok @@ simple_unary @@ prim I_SOME
  | "GET_FORCE" -> ok @@ simple_binary @@ seq [prim I_GET ; i_assert_some]
  | "GET" -> ok @@ simple_binary @@ prim I_GET
  | "SIZE" -> ok @@ simple_unary @@ prim I_SIZE
  | "INT" -> ok @@ simple_unary @@ prim I_INT
  | "CONS" -> ok @@ simple_binary @@ prim I_CONS
  (* | "CONS" -> ok @@ simple_binary @@ seq [prim I_SWAP ; prim I_CONS] *)
  | "MAP_REMOVE" ->
      let%bind v = match lst with
        | [ _ ; (_, m, _) ] ->
            let%bind (_, v) = Combinators.get_t_map m in
            ok v
        | _ -> simple_fail "mini_c . MAP_REMOVE" in
      let%bind v_ty = Compiler_type.type_ v in
      ok @@ simple_binary @@ seq [dip (i_none v_ty) ; prim I_UPDATE ]
  | "MAP_UPDATE" ->
      ok @@ simple_ternary @@ seq [dip (i_some) ; prim I_UPDATE ]
  | x -> simple_fail ("predicate \"" ^ x ^ "\" doesn't exist")

and translate_value (v:value) : michelson result = match v with
  | D_bool b -> ok @@ prim (if b then D_True else D_False)
  | D_int n -> ok @@ int (Z.of_int n)
  | D_nat n -> ok @@ int (Z.of_int n)
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
      let aux = fun a -> a in
      (* let aux = fun a -> prim ~children:[a] D_Elt in *)
      ok @@ seq @@ List.map aux lst'

and translate_function ({capture;content}:anon_function) : michelson result =
  let {capture_type } = content in
  match capture, capture_type with
  | _, No_capture ->
      let%bind body = translate_quote_body content in
      ok @@ seq [ body ]
  | Some value, Deep_capture senv -> (
      let senv_type = Compiler_environment.Small.to_mini_c_type senv in
      let%bind body = translate_closure_body content senv_type in
      let%bind capture_m = translate_value value in
      ok @@ d_pair capture_m body
    )
  | Some value, Shallow_capture env ->
      let env_type = Compiler_environment.to_mini_c_type env in
      let%bind body = translate_closure_body content env_type in
      let%bind capture_m = translate_value value in
      ok @@ d_pair capture_m body
  | _ -> simple_fail "compiling closure without capture"

and translate_expression ((expr', ty, env) as expr:expression) : michelson result =
  let error_message () = Format.asprintf  "%a" PP.expression expr in
  let%bind (code : michelson) =
    trace (error (thunk "compiling expression") error_message) @@
    match expr' with
    | E_literal v ->
        let%bind v = translate_value v in
        let%bind t = Compiler_type.type_ ty in
        ok @@ seq [
          prim ~children:[t;v] I_PUSH ;
          prim I_PAIR ;
        ]
    | E_application((_, f_ty, _) as f, arg) -> (
        match f_ty with
        | T_function _ -> (
            trace (simple_error "Compiling quote application") @@
            let%bind f = translate_expression f in
            let%bind arg = translate_expression arg in
            ok @@ seq [
              arg ;
              i_unpair ;
              dip f ;
              dip i_unpair ;
              prim I_EXEC ;
              i_pair ;
            ]
          )
        | T_deep_closure (small_env, _, _) -> (
            let env' = Environment.of_small small_env in
            let%bind add = Environment.to_michelson_anonymous_add env' in
            let%bind f = translate_expression f in
            let%bind arg = translate_expression arg in
            ok @@ seq [
              f ; i_unpair ; (* closure :: expr :: env *)
              dip arg ; dip i_unpair ; (* closure :: arg :: expr :: env *)
              i_unpair ; dip add ; (* fun :: full_arg :: expr :: env *)
              i_swap ; prim I_EXEC ;
              i_pair ; (* expr :: env *)
            ]
          )
        | T_shallow_closure (_, _, _) -> (
            trace (simple_error "Compiling shallow closure application") @@
            let%bind f = translate_expression f in
            let%bind arg = translate_expression arg in
            ok @@ seq [
              i_comment "(* unit :: env *)" ;
              i_comment "compute closure" ;
              f ;
              i_comment "(* (closure * unit) :: env *)" ;
              i_comment "compute arg" ;
              arg ;
              i_comment "(* (arg * closure * unit) :: env *)" ;
              i_comment "separate stuff" ;
              i_unpair ; dip i_unpair ; dip i_unpair ;
              i_comment "(* arg :: capture :: f :: unit :: env *)" ;
              i_piar ;
              i_exec ; (* output :: stack :: env *)
              i_pair ; (* stack :: env *)
            ]
          )
        | _ -> simple_fail "E_applicationing something not appliable"
      )
    | E_variable x ->
        let%bind (get, _) = Environment.to_michelson_get env x in
        ok @@ seq [
          dip (seq [prim I_DUP ; get]) ;
          i_piar ;
        ]
    | E_constant(str, lst) ->
        let%bind lst' = bind_list @@ List.map translate_expression lst in
        let%bind predicate = get_predicate str lst in
        let%bind code = match (predicate, List.length lst) with
          | Constant c, 0 -> ok (seq @@ lst' @ [c])
          | Unary f, 1 -> ok (seq @@ lst' @ [f])
          | Binary f, 2 -> ok (seq @@ lst' @ [f])
          | Ternary f, 3 -> ok (seq @@ lst' @ [f])
          | _ -> simple_fail "bad arity"
        in
        ok code
    | E_empty_map sd ->
        let%bind (src, dst) = bind_map_pair Compiler_type.type_ sd in
        let code = seq [
            prim ~children:[src;dst] I_EMPTY_MAP ;
            i_pair ;
          ] in
        ok code
    | E_empty_list t ->
        let%bind t' = Compiler_type.type_ t in
        let code = seq [
            prim ~children:[t'] I_NIL ;
            i_pair ;
          ] in
        ok code
    | E_make_none o ->
        let%bind o' = Compiler_type.type_ o in
        let code = seq [
            prim ~children:[o'] I_NONE ;
            i_pair ;
          ] in
        ok code
    | E_function anon -> (
        match anon.capture_type with
        | No_capture ->
            let%bind body = translate_quote_body anon in
            let%bind input_type = Compiler_type.type_ anon.input in
            let%bind output_type = Compiler_type.type_ anon.output in
            let code = seq [
                i_lambda input_type output_type body ;
                i_pair ;
              ] in
            ok code
        | Deep_capture small_env ->
            (* Capture the variable bounds, assemble them. On call, append the input. *)
            let senv_type = Compiler_environment.Small.to_mini_c_type small_env in
            let%bind body = translate_closure_body anon senv_type in
            let%bind capture = Environment.Small.to_mini_c_capture env small_env in
            let%bind capture = translate_expression capture in
            let%bind input_type = Compiler_type.type_ anon.input in
            let%bind output_type = Compiler_type.type_ anon.output in
            let code = seq [
                capture ;
                i_unpair ;
                i_lambda input_type output_type body ;
                i_piar ;
                i_pair ;
              ] in
            ok code
        | Shallow_capture env ->
            (* Capture the whole environment. *)
            let env_type = Compiler_environment.to_mini_c_type env in
            let%bind body = translate_closure_body anon env_type in
            let%bind input_type =
              let input_type = Combinators.t_pair anon.input env_type in
              Compiler_type.type_ input_type in
            let%bind output_type = Compiler_type.type_ anon.output in
            let code = seq [ (* stack :: env *)
                i_comment "env on top" ;
                dip i_dup ; i_swap ; (* env :: stack :: env *)
                i_comment "lambda" ;
                i_lambda input_type output_type body ; (* lambda :: env :: stack :: env *)
                i_comment "pair env + lambda" ;
                i_piar ; (* (env * lambda) :: stack :: env *)
                i_comment "new stack" ;
                i_pair ; (* new_stack :: env *)
              ] in
            ok code
      )
    | E_Cond (c, a, b) -> (
        let%bind c' = translate_expression c in
        let%bind a' = translate_expression a in
        let%bind b' = translate_expression b in
        let%bind code = ok (seq [
            c' ; i_unpair ;
            i_if a' b' ;
          ]) in
        ok code
      )
  in

  let%bind () =
    let%bind (Ex_ty schema_ty) = Environment.to_ty env in
    let%bind output_type = Compiler_type.type_ ty in
    let%bind (Ex_ty output_ty) =
      let error_message () = Format.asprintf "%a" Michelson.pp output_type in
      Trace.trace_tzresult_lwt (fun () -> error (thunk "error parsing output ty") error_message ()) @@
      Tezos_utils.Memory_proto_alpha.parse_michelson_ty output_type in
    let input_stack_ty = Stack.(Contract_types.unit @: schema_ty @: nil) in
    let output_stack_ty = Stack.(Contract_types.(pair output_ty unit) @: schema_ty @: nil) in
    let error_message () =
      let%bind schema_michelson = Environment.to_michelson_type env in
      ok @@ Format.asprintf
        "expression : %a\ncode : %a\nschema type : %a\noutput type : %a"
        PP.expression expr
        Michelson.pp code
        Michelson.pp schema_michelson
        Michelson.pp output_type
    in
    let%bind _ =
      Trace.trace_tzresult_lwt_r
        (fun () ->
           let%bind error_message = error_message () in
           ok @@ (fun () -> error (thunk "error parsing expression code")
                                  (fun () -> error_message)
                                  ())) @@
      Tezos_utils.Memory_proto_alpha.parse_michelson code
        input_stack_ty output_stack_ty
    in
    ok ()
  in

  ok code

and translate_statement ((s', w_env) as s:statement) : michelson result =
  let error_message () = Format.asprintf "%a" PP.statement s in
  let%bind (code : michelson) =
    trace (fun () -> error (thunk "compiling statement") error_message ()) @@ match s' with
    | S_environment_extend ->
        ok @@ Environment.to_michelson_extend w_env.pre_environment
    | S_environment_restrict ->
        Environment.to_michelson_restrict w_env.pre_environment
    | S_environment_add _ ->
        simple_fail "not ready yet"
    (* | S_environment_add (name, tv) ->
     *     Environment.to_michelson_add (name, tv) w_env.pre_environment *)
    | S_declaration (s, ((_, tv, _) as expr)) ->
        let%bind expr = translate_expression expr in
        let%bind add = Environment.to_michelson_add (s, tv) w_env.pre_environment in
        ok (seq [
            i_comment "declaration" ;
            seq [
              i_comment "expr" ;
              i_push_unit ; expr ; i_car ;
            ] ;
            seq [
              i_comment "env <- env . expr" ;
              add ;
            ];
          ])
    | S_assignment (s, expr) ->
        let%bind expr = translate_expression expr in
        let%bind set = Environment.to_michelson_set s w_env.pre_environment in
        ok (seq [
            i_comment "assignment" ;
            seq [
              i_comment "expr" ;
              i_push_unit ; expr ; i_car ;
            ] ;
            seq [
              i_comment "env <- env . expr" ;
              set ;
            ];
          ])
    | S_cond (expr, a, b) ->
        let%bind expr = translate_expression expr in
        let%bind a' = translate_regular_block a in
        let%bind b' = translate_regular_block b in
        ok @@ (seq [
            i_push_unit ;
            expr ;
            prim I_CAR ;
            prim ~children:[seq [a'];seq [b']] I_IF ;
          ])
    | S_if_none (expr, none, ((name, tv), some)) ->
        let%bind expr = translate_expression expr in
        let%bind none' = translate_regular_block none in
        let%bind some' = translate_regular_block some in
        let%bind add =
          let env' = Environment.extend w_env.pre_environment in
          Environment.to_michelson_add (name, tv) env' in
        ok @@ (seq [
            i_push_unit ; expr ; i_car ;
            prim ~children:[
              seq [none'] ;
              seq [add ; some'] ;
            ] I_IF_NONE
          ])
    | S_while ((_, _, _) as expr, block) ->
        let%bind expr = translate_expression expr in
        let%bind block' = translate_regular_block block in
        let%bind restrict_block =
          let env_while = (snd block).pre_environment in
          Environment.to_michelson_restrict env_while in
        ok @@ (seq [
            i_push_unit ; expr ; i_car ;
            prim ~children:[seq [
                Environment.to_michelson_extend w_env.pre_environment;
                block' ;
                restrict_block ;
                i_push_unit ; expr ; i_car]] I_LOOP ;
          ])
    | S_patch (name, lrs, expr) ->
        let%bind expr' = translate_expression expr in
        let%bind (name_path, _) = Environment.get_path name w_env.pre_environment in
        let path = name_path @ lrs in
        let set_code = Environment.path_to_michelson_set path in
        ok @@ seq [
          i_push_unit ; expr' ; i_car ;
          set_code ;
        ]
  in

  let%bind () =
    let%bind (Ex_ty pre_ty) = Environment.to_ty w_env.pre_environment in
    let input_stack_ty = Stack.(pre_ty @: nil) in
    let%bind (Ex_ty post_ty) = Environment.to_ty w_env.post_environment in
    let output_stack_ty = Stack.(post_ty @: nil) in
    let error_message () =
      let%bind pre_env_michelson = Environment.to_michelson_type w_env.pre_environment in
      let%bind post_env_michelson = Environment.to_michelson_type w_env.post_environment in
      ok @@ Format.asprintf
        "statement : %a\ncode : %a\npre type : %a\npost type : %a\n"
        PP.statement s
        Michelson.pp code
        Michelson.pp pre_env_michelson
        Michelson.pp post_env_michelson
    in
    let%bind _ =
      Trace.trace_tzresult_lwt_r (fun () -> let%bind error_message = error_message () in
                                   ok (fun () -> error (thunk "error parsing statement code")
                                                 (fun () -> error_message)
                                                 ())) @@
      Tezos_utils.Memory_proto_alpha.parse_michelson code
        input_stack_ty output_stack_ty
    in
    ok ()
  in


  ok code

and translate_regular_block ((b, env):block) : michelson result =
  let aux prev statement =
    let%bind (lst : michelson list) = prev in
    let%bind instruction = translate_statement statement in
    ok (instruction :: lst)
  in
  let%bind codes =
    let error_message () =
      let%bind schema_michelson = Environment.to_michelson_type env.pre_environment in
      ok @@ Format.asprintf "\nblock : %a\nschema : %a\n"
        PP.block (b, env)
        Tezos_utils.Micheline.Michelson.pp schema_michelson
    in
    trace_r (fun () ->
        let%bind error_message = error_message () in
        ok (fun () -> error (thunk "compiling regular block")
                      (fun () -> error_message)
                      ())) @@
    List.fold_left aux (ok []) b in
  let code = seq (List.rev codes) in
  ok code

and translate_quote_body ({body;result} as f:anon_function_content) : michelson result =
  let%bind body = translate_regular_block body in
  let%bind expr = translate_expression result in
  let code = seq [
      i_comment "function body" ;
      body ;
      i_comment "function result" ;
      i_push_unit ; expr ; i_car ;
      dip i_drop ;
    ] in

  let%bind _assert_type =
    let%bind (Ex_ty input_ty) = Compiler_type.Ty.type_ f.input in
    let%bind (Ex_ty output_ty) = Compiler_type.Ty.type_ f.output in
    let input_stack_ty = Stack.(input_ty @: nil) in
    let output_stack_ty = Stack.(output_ty @: nil) in
    let error_message () =
      Format.asprintf
        "\ncode : %a\ninput : %a\noutput : %a\n"
        Tezos_utils.Micheline.Michelson.pp code
        PP.type_ f.input
        PP.type_ f.output
    in
    let%bind _ =
      Trace.trace_tzresult_lwt (
        error (thunk "error parsing quote code") error_message
      ) @@
      Tezos_utils.Memory_proto_alpha.parse_michelson code
        input_stack_ty output_stack_ty
    in
    ok ()
  in

  ok code

and translate_closure_body ({body;result} as f:anon_function_content) (env_type:type_value) : michelson result =
  let%bind body = translate_regular_block body in
  let%bind expr = translate_expression result in
  let code = seq [
      i_comment "function body" ;
      body ;
      i_comment "function result" ;
      i_push_unit ; expr ; i_car ;
      dip i_drop ;
    ] in

  let%bind _assert_type =
    let input = Combinators.t_pair env_type f.input in
    let output = f.output in
    let%bind (Ex_ty input_ty) = Compiler_type.Ty.type_ input in
    let%bind (Ex_ty output_ty) = Compiler_type.Ty.type_ output in
    let input_stack_ty = Stack.(input_ty @: nil) in
    let output_stack_ty = Stack.(output_ty @: nil) in
    let error_message () =
      Format.asprintf
        "\ncode : %a\ninput : %a\noutput : %a\n"
        Tezos_utils.Micheline.Michelson.pp code
        PP.type_ input
        PP.type_ output
    in
    let%bind _ =
      Trace.trace_tzresult_lwt (
        error (thunk "error parsing closure code") error_message
      ) @@
      Tezos_utils.Memory_proto_alpha.parse_michelson code
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

let translate_program (p:program) (entry:string) : compiled_program result =
  let is_main ((s, _):toplevel_statement) =
    match s with
    | name , (E_function f, T_function (_, _), _)
      when f.capture_type = No_capture && name = entry ->
        Some f
    | name , (E_literal (D_function {content ; capture = None}), T_function (_, _), _)
      when name = entry ->
        Some content
    | _ ->  None
  in
  let%bind main =
    trace_option (simple_error "no functional entry") @@
    Tezos_utils.List.find_map is_main p
  in
  let {input;output} : anon_function_content = main in
  let%bind body = translate_quote_body main in
  let%bind input = Compiler_type.Ty.type_ input in
  let%bind output = Compiler_type.Ty.type_ output in
  ok ({input;output;body}:compiled_program)

let translate_entry (p:anon_function) : compiled_program result =
  let {input;output} : anon_function_content = p.content in
  let%bind body =
    trace (simple_error "compile entry body") @@
    translate_quote_body p.content in
  let%bind input = Compiler_type.Ty.type_ input in
  let%bind output = Compiler_type.Ty.type_ output in
  ok ({input;output;body}:compiled_program)
