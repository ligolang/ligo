open Trace
open Errors
open! Stage_common.Types
open! Co_de_bruijn
open Michelson
open Predefined.Stacking

open Co_de_bruijn.Util

(* TODO optimize? *)
let translate_usages (us : usages) : michelson =
  let rec aux n us =
    match us with
    | [] -> seq []
    | Drop :: us -> seq [ i_dig n ; i_drop ; aux n us ]
    | Keep :: us -> seq [ aux (n + 1) us ] in
  aux 0 us

(* TODO optimize? *)
let translate_splitting (s : splitting) : michelson =
  let rec aux n s =
    match s with
    | [] -> seq []
    | Left :: s -> seq [ i_dig (n - 1) ; aux n s ]
    | Right :: s -> aux (n - 1) s
    | Both :: s -> seq [ i_dig (n - 1) ; i_dup ; i_dug n ; aux n s ] in
  aux (List.length s) (List.rev s)

(* Using left combs here because it's easier. This is only used for
   APPLY. If COMB/UNCOMB instructions are added someday, can switch to
   them. Must also switch Compiler_type.environment_closure at that
   time. *)
let comb n = seq (List.repeat n i_pair)
let uncomb n = seq (List.repeat n (seq [i_dup; i_cdr; i_swap; i_car]))

let translate_bind' translate (body : 'a bind) env outer =
  let (ty, used, body) = body in
  match used with
  | Keep -> translate body (ty :: env) (Left :: outer)
  | Drop ->
    let%bind body = translate body env outer in
    ok (seq [ i_drop ; body ])

let rec translate_binds' translate (binds : 'a binds) env outer =
  match binds with
  | Bind_zero x -> translate x env outer
  | Bind_suc bind -> translate_bind' (translate_binds' translate) bind env outer

let rec get_operator : constant' -> type_expression -> expression args -> (predicate , stacking_error) result = fun s ty lst ->
  match Predefined.Stacking.get_operators s with
  | Some x -> ok x
  | None -> (
      match s with
      | C_SELF -> (
          let%bind entrypoint_as_string = match lst with
            | Arg_cons (_, { content = E_literal (Literal_string s); type_expression = _ }, _) -> (
                let s = Ligo_string.extract s in
                match String.split_on_char '%' s with
                | ["" ; s] -> ok @@ String.concat "" ["%" ; (String.uncapitalize_ascii s)]
                | _ -> fail @@ corner_case ~loc:__LOC__ "mini_c . SELF"
            )
            | _ ->
              fail @@ corner_case ~loc:__LOC__ "mini_c . SELF" in
          ok @@ simple_unary @@ seq [
            i_drop ;
            prim ~annot:[entrypoint_as_string] "SELF"
          ]
      )
      | C_NONE -> (
          let%bind ty' = trace_option (corner_case ~loc:__LOC__ "wrongtype") @@ Mini_c.get_t_option ty in
          let%bind m_ty = Compiler_type.type_ ty' in
          ok @@ simple_constant @@ prim ~children:[m_ty] "NONE"
        )
      | C_NIL -> (
          let%bind ty' = trace_option (corner_case ~loc:__LOC__ "wrongtype") @@  Mini_c.get_t_list ty in
          let%bind m_ty = Compiler_type.type_ ty' in
          ok @@ simple_unary @@ prim ~children:[m_ty] "NIL"
        )
      | C_LOOP_CONTINUE -> (
          let%bind (_,ty) = trace_option (corner_case ~loc:__LOC__ "wrongtype") @@  Mini_c.get_t_or ty in
          let%bind m_ty = Compiler_type.type_ ty in
          ok @@ simple_unary @@ prim ~children:[m_ty] "LEFT"
      )
      | C_LOOP_STOP -> (
          let%bind (ty, _) = trace_option (corner_case ~loc:__LOC__ "wrongtype") @@  Mini_c.get_t_or ty in
          let%bind m_ty = Compiler_type.type_ ty in
          ok @@ simple_unary @@ prim ~children:[m_ty] "RIGHT"
      )
      | C_LIST_EMPTY -> (
          let%bind ty' = trace_option (corner_case ~loc:__LOC__ "wrongtype") @@  Mini_c.get_t_list ty in
          let%bind m_ty = Compiler_type.type_ ty' in
          ok @@ simple_constant @@ i_nil m_ty
        )
      | C_SET_EMPTY -> (
          let%bind ty' = trace_option (corner_case ~loc:__LOC__ "wrongtype") @@  Mini_c.get_t_set ty in
          let%bind m_ty = Compiler_type.type_ ty' in
          ok @@ simple_constant @@ i_empty_set m_ty
        )
      | C_MAP_EMPTY -> (
          let%bind sd = trace_option (corner_case ~loc:__LOC__ "wrongtype") @@  Mini_c.get_t_map ty in
          let%bind (src, dst) = bind_map_pair Compiler_type.type_ sd in
          ok @@ simple_constant @@ i_empty_map src dst
        )
      | C_BIG_MAP_EMPTY -> (
          let%bind sd = trace_option (corner_case ~loc:__LOC__ "wrongtype") @@  Mini_c.get_t_big_map ty in
          let%bind (src, dst) = bind_map_pair Compiler_type.type_ sd in
          ok @@ simple_constant @@ i_empty_big_map src dst
        )
      | C_BYTES_UNPACK -> (
          let%bind ty' = trace_option (corner_case ~loc:__LOC__ "wrongtype") @@  Mini_c.get_t_option ty in
          let%bind m_ty = Compiler_type.type_ ty' in
          ok @@ simple_unary @@ prim ~children:[m_ty] "UNPACK"
        )
      | C_MAP_REMOVE ->
          let%bind (_k,v) = trace_option (corner_case ~loc:__LOC__ "wrongtype") @@  match lst with
            | Arg_cons (_, _, Arg_cons (_, expr, _)) ->
              Option.(map_pair_or (Mini_c.get_t_map , Mini_c.get_t_big_map) expr.type_expression)
            | _ -> None in
          let%bind v_ty = Compiler_type.type_ v in
          ok @@ simple_binary @@ seq [dip (i_none v_ty) ; prim "UPDATE" ]
      | C_LEFT ->
          let%bind r = trace_option (corner_case ~loc:__LOC__ "wrongtype") @@  Mini_c.get_t_right ty in
          let%bind r_ty = Compiler_type.type_ r in
          ok @@ simple_unary @@ prim ~children:[r_ty] "LEFT"
      | C_RIGHT ->
          let%bind l = trace_option (corner_case ~loc:__LOC__ "wrongtype") @@  Mini_c.get_t_left ty in
          let%bind l_ty = Compiler_type.type_ l in
          ok @@ simple_unary @@ prim ~children:[l_ty] "RIGHT"
      | C_CONTRACT ->
          let%bind r = trace_option (corner_case ~loc:__LOC__ "wrongtype") @@  Mini_c.get_t_contract ty in
          let%bind r_ty = Compiler_type.type_ r in
          ok @@ simple_unary @@ seq [
            prim ~children:[r_ty] "CONTRACT" ;
            i_assert_some_msg (i_push_string "bad address for get_contract") ;
          ]
      | C_CONTRACT_OPT ->
          let%bind tc = trace_option (corner_case ~loc:__LOC__ "wrongtype") @@  Mini_c.get_t_option ty in
          let%bind r = trace_option (corner_case ~loc:__LOC__ "wrongtype") @@  Mini_c.get_t_contract tc in
          let%bind r_ty = Compiler_type.type_ r in
          ok @@ simple_unary @@ prim ~children:[r_ty] "CONTRACT" ;

      | C_CONTRACT_ENTRYPOINT ->
          let%bind r = trace_option (corner_case ~loc:__LOC__ "wrongtype") @@  Mini_c.get_t_contract ty in
          let%bind r_ty = Compiler_type.type_ r in
          let%bind entry = match lst with
            | Arg_cons (_, { content = E_literal (Literal_string entry); type_expression = _ }, _) -> ok entry
            | _ ->
               fail @@ contract_entrypoint_must_be_literal ~loc:__LOC__ in
          let entry = Ligo_string.extract entry in
          ok @@ simple_binary @@ seq [
            i_drop ; (* drop the entrypoint... *)
            prim ~annot:[entry] ~children:[r_ty] "CONTRACT" ;
            i_assert_some_msg (i_push_string @@ Format.sprintf "bad address for get_entrypoint (%s)" entry) ;
          ]
      | C_CONTRACT_ENTRYPOINT_OPT ->
          let%bind tc = trace_option (corner_case ~loc:__LOC__ "wrongtype") @@  Mini_c.get_t_option ty in
          let%bind r = trace_option (corner_case ~loc:__LOC__ "wrongtype") @@  Mini_c.get_t_contract tc in
          let%bind r_ty = Compiler_type.type_ r in
          let%bind entry = match lst with
            | Arg_cons (_, { content = E_literal (Literal_string entry); type_expression = _ }, _) -> ok entry
            | _ ->
               fail @@ contract_entrypoint_must_be_literal ~loc:__LOC__ in
          let entry = Ligo_string.extract entry in
          ok @@ simple_binary @@ seq [
            i_drop ; (* drop the entrypoint... *)
            prim ~annot:[entry] ~children:[r_ty] "CONTRACT" ;
          ]
      | C_CREATE_CONTRACT ->
        let%bind ch = match lst with
          (* {type_content=T_function ({type_content=T_pair ((_,p),(_,s));_} as tin,_);_} *)
          | Arg_cons (_, { content= E_closure body ; type_expression = closure_ty }, _) ->
            let%bind closure = translate_function_body body [] [] in
            let%bind (input_ty, _) = trace_option (corner_case ~loc:__LOC__ "wrongtype") @@ Mini_c.get_t_function closure_ty in
            let%bind (p, s) = trace_option (corner_case ~loc:__LOC__ "wrongtype") @@ Mini_c.get_t_pair input_ty in
            let%bind (p',s') = bind_map_pair Compiler_type.type_ (p,s) in
            ok @@ contract p' s' closure
          | _ -> fail @@ corner_case ~loc:__LOC__ "mini_c . CREATE_CONTRACT"
        in
        ok @@ simple_tetrary @@ seq [
          i_drop ;
          prim ~children:[ch] "CREATE_CONTRACT" ;
          i_pair ;
        ]
      | x -> fail @@ corner_case ~loc:__LOC__ (Format.asprintf "predicate \"%a\" doesn't exist" Mini_c.PP.constant x)
    )

and translate_literal (v : literal) : michelson =
  match v with
  | Literal_unit -> prim "Unit"
  | Literal_int x -> int x
  | Literal_nat x -> int x
  | Literal_timestamp x -> int x
  | Literal_mutez x -> int x
  | Literal_string x -> string (Ligo_string.extract x)
  | Literal_bytes x -> bytes x
  | Literal_address x -> string x
  | Literal_signature x -> string x
  | Literal_key x -> string x
  | Literal_key_hash x -> string x
  (* these two don't really exist in Michelson: *)
  | Literal_chain_id x -> string x
  | Literal_operation x -> bytes x

and translate_expression (expr : expression) env outer : (michelson , stacking_error) result =
  let expr' = expr.content in
  let ty = expr.type_expression in
  let return code = ok code in

  match expr' with
  | E_literal v ->
      let v = translate_literal v in
      let%bind t = Compiler_type.type_ ty in
      return @@ i_push t v
  | E_closure body -> (
      match ty.type_content with
      | T_function (_ , output_ty) ->
        translate_function body env outer output_ty
      | _ -> fail @@ corner_case ~loc:__LOC__ "expected function type"
    )
  | E_application (inner, f , arg) -> (
      let (env_f, env_arg) = split inner env in
      let inner = flip inner in
      let (outer', inner') = assoc outer inner in
      let%bind arg = translate_expression arg env_arg outer' in
      let%bind f = translate_expression f env_f (Right :: inner') in
      return @@ seq [
        arg ;
        f ;
        (* we could avoid the swap, but, this makes it easier to
           reduce applications of inlined literal michelson
           lambdas. *)
        i_swap ;
        prim "EXEC" ;
      ]
    )
  | E_variable -> ok (translate_splitting outer)
  | E_constant {cons_name; arguments} ->
      let%bind pre_code = translate_args arguments env outer in
      let%bind predicate = get_operator cons_name ty arguments in
      let predicate = Predefined.Stacking.unpredicate predicate in
      return (seq [ pre_code ; predicate ])
  | E_if_bool (inner1, c, (inner2, a, b)) -> (
      let%bind code = translate_conditional "IF" (inner1, c, (inner2, binds0 a, binds0 b)) env outer in
      return code
    )
  | E_if_none (inner1, c, (inner2, n, s)) -> (
      let%bind code = translate_conditional "IF_NONE" (inner1, c, (inner2, binds0 n, binds1 s)) env outer in
      return code
    )
  | E_if_cons (inner1, c, (inner2, cons, nil)) -> (
      let%bind code = translate_conditional "IF_CONS"
          (* TODO get rid of this... *)
          ~extra_left:i_swap
          (inner1, c, (inner2, binds2 cons, binds0 nil)) env outer in
      return code
    )
  | E_if_left (inner1, c, (inner2, l, r)) -> (
      let%bind code = translate_conditional "IF_LEFT" (inner1, c, (inner2, binds1 l, binds1 r)) env outer in
      return code
    )
  | E_let_in (_, (inner, e1, e2)) -> (
      let (env1, env2) = split inner env in
      let (outer', inner') = assoc outer inner in
      let%bind e1 = translate_expression e1 env1 outer' in
      let%bind e2 = translate_bind e2 env2 inner' in
      return (seq [ e1 ; e2 ])
    )
  | E_iterator (name, (inner, body, expr)) -> (
      let (env_body, env_expr) = split inner env in
      let inner = flip inner in
      let (outer, inner) = assoc outer inner in
      let%bind expr' = translate_expression expr env_expr outer in
      let%bind body' = translate_bind body env_body (rights_with (left_usages inner)) in
      match name with
      | C_ITER -> (
          let%bind code = ok (seq [
              expr' ;
              i_iter (seq [body' ; i_drop]) ;
              translate_usages (right_usages inner) ;
              i_push_unit ;
            ]) in
          return code
        )
      | C_MAP -> (
          let%bind code = ok (seq [
              expr' ;
              i_map (seq [body']) ;
              translate_usages (Keep :: right_usages inner) ;
            ]) in
          return code
        )
      | C_LOOP_LEFT -> (
          let (ty, _, _) = body in
          let%bind (_, ty) = trace_option (corner_case ~loc:__LOC__ "wrongtype") @@  Mini_c.get_t_or ty in
          let%bind m_ty = Compiler_type.type_ ty in
          let%bind code = ok (seq [
              expr' ;
              prim ~children:[m_ty] "LEFT";
              i_loop_left body';
              translate_usages (Keep :: right_usages inner) ;
            ]) in
          return code
        )
      | s -> (
          fail (bad_iterator s)
        )
    )
  | E_fold (inner1, initial, (inner2, collection, body)) -> (
      let (env_init, env') = split inner1 env in
      let (env_coll, env_body) = split inner2 env' in
      let (outer, inner1) = assoc outer inner1 in
      let (inner1, inner2) = assoc inner1 inner2 in
      let%bind initial' = translate_expression initial env_init outer in
      let%bind collection' = translate_expression collection env_coll (Right :: inner1) in
      let%bind body' = translate_bind body env_body (rights_with (left_usages inner2)) in
      let code = seq [
          initial' ;
          collection' ;
          i_iter (seq [
              i_swap ;
              i_pair ; body' ;
            ]) ;
          translate_usages (Keep :: right_usages inner2) ;
        ] in
      ok code
    )
  | E_record_update (path, (inner, record, expr)) -> (
    let (env_record, env_expr) = split inner env in
    let (outer', inner') = assoc outer inner in
    let%bind record' = translate_expression record env_record outer' in
    let%bind expr' = translate_expression expr env_expr (Right :: inner') in
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
  | E_raw_michelson code ->
      let orig_code = code in
      let (code, errs1) =
        Tezos_micheline.Micheline_parser.tokenize code in
      match errs1 with
      | _ :: _ -> fail (Errors.could_not_tokenize_michelson orig_code)
      | _ ->
      let (code, errs2) =
        Tezos_micheline.Micheline_parser.parse_expression ~check:false code in
      match errs2 with
      | _ :: _ -> fail (Errors.could_not_parse_michelson orig_code)
      | _ ->
      let code = Tezos_micheline.Micheline.strip_locations code in
      let code = Tezos_micheline.Micheline.root code in
      let%bind ty = Compiler_type.type_ ty in
      return @@ i_push ty code

and translate_bind (body : expression bind) env outer =
  translate_bind' translate_expression body env outer

and translate_binds (body : expression binds) env outer =
  translate_binds' translate_expression body env outer

and translate_args (args : expression args) env outer =
  match args with
  | Arg_nil -> ok (translate_splitting outer)
  | Arg_cons (inner, arg, args) ->
    let args_env = select (right_usages inner) env in
    let arg_env = select (left_usages inner) env in
    let (outer', inner') = assoc outer (flip inner) in
    let%bind args' = translate_args args args_env outer' in
    let inner' = List.repeat (args_count args) Right @ inner' in
    let%bind arg' = translate_expression arg arg_env inner' in
    ok (seq [ args' ; arg' ])

and translate_conditional (if_prim : string) ?extra_left
    (cond : (expression, (expression binds, expression binds) split) split) (env : environment) (outer : splitting) =
  let (inner1, e1, (inner2, e2, e3)) = cond in
  let (env1, env') = split inner1 env in
  let (env2, env3) = split inner2 env' in
  let (outer', inner1') = assoc outer inner1 in
  let (outerL, innerR) = assoc inner1' inner2 in
  let (outerR, innerL) = assoc inner1' (flip inner2) in
  let%bind e1 = translate_expression e1 env1 outer' in
  let p2 = translate_usages (List.repeat (num_binds e2) Keep @ right_usages outerR) in
  let p3 = translate_usages (List.repeat (num_binds e3) Keep @ right_usages outerL) in
  let%bind e2 = translate_binds e2 env2 innerL in
  let%bind e3 = translate_binds e3 env3 innerR in
  (* TODO get rid of this... *)
  let extra_left = match extra_left with
    | None -> seq []
    | Some code -> code in
  ok (seq [ e1 ;
            prim ~children:[seq [extra_left; p2; e2];
                            seq [p3; e3]]
              if_prim])

and translate_function_body (body : expression bind) env outer : (michelson , stacking_error) result =
  let (ty, used, body) = body in
  let%bind expr_code = match used with
    | Keep -> translate_expression body (ty :: env) (Left :: outer)
    | Drop -> translate_expression body env outer in
  let unpack_closure_code = match used with
    | Keep ->
      (match env with
       | [] -> seq []
       | _ :: _ ->
         seq [ i_unpair ;
               uncomb (List.length env - 1) ;
               i_dig (List.length env) ])
    | Drop ->
      (match env with
       | [] -> seq [i_drop]
       | _ :: _ ->
         seq [ i_car ;
               uncomb (List.length env - 1) ]) in
  let code = seq [
      i_comment "unpack closure env" ;
      unpack_closure_code ;
      i_comment "function result" ;
      expr_code ;
    ] in
  ok code

and translate_function body env outer output_ty : (michelson , stacking_error) result =
  let (input_ty, _, _) = body in
  let%bind (_lambda_ty , input_ty' , output_ty') =
    Compiler_type.lambda_closure_with_ty (env , input_ty , output_ty) in
  let%bind lambda_body_code = translate_function_body body env (List.repeat (List.length env) Left) in
  match env with
  | [] -> ok @@ seq [ translate_splitting outer ;
                      i_lambda input_ty' output_ty' lambda_body_code ]
  | _ :: _ ->
    let closure_pack_code = comb (List.length env - 1) in
    ok @@ seq [
      translate_splitting outer ;
      closure_pack_code ;
      i_lambda input_ty' output_ty' lambda_body_code ;
      i_swap ;
      i_apply ;
    ]

type compiled_expression = {
  expr_ty : michelson ;
  expr : michelson ;
}
