open Trace
open Function
module I = Parser.Camligo.Ast
module O = Ast_simplified
open O.Combinators

let unwrap : type a . a Location.wrap -> a = Location.unwrap

let type_constants = Operators.Simplify.type_constants

let type_variable : string -> O.type_expression result = fun str ->
  match List.assoc_opt str type_constants with
  | Some 0 -> ok @@ O.T_constant (str, [])
  | Some _ -> simple_fail "non-nullary type constructor"
  | None -> ok @@ O.T_variable str

let get_param_restricted_pattern : I.param -> I.restricted_pattern Location.wrap result = fun p ->
  match p with
  | I.Param_restricted_pattern c -> ok c
  | _ -> simple_fail "not a restricted param pattern"

let get_unrestricted_pattern : I.restricted_pattern -> I.pattern Location.wrap result = fun rp ->
  match rp with
  | I.Pr_restrict p -> ok p
  | _ -> simple_fail "not an unrestricted pattern"

let get_p_type_annotation : I.pattern -> (I.pattern Location.wrap * I.restricted_type_expression Location.wrap) result = fun p ->
  match p with
  | I.P_type_annotation pta -> ok pta
  | _ -> simple_fail "not a pattern type annotation"

let get_p_variable : I.pattern -> string Location.wrap result = fun p ->
  match p with
  | I.P_variable v -> ok v
  | _ -> simple_fail "not a pattern variable"

let get_p_typed_variable : I.pattern -> (string Location.wrap * I.restricted_type_expression Location.wrap) result = fun p ->
  let%bind (p' , rte) = get_p_type_annotation p in
  let%bind var = get_p_variable (unwrap p') in
  ok (var , rte)

let get_arg : I.param -> _ result = fun arg ->
  let%bind rp =
    get_param_restricted_pattern arg >>?
    Function.compose get_unrestricted_pattern unwrap in
  let%bind (var , rte) = get_p_typed_variable (unwrap rp) in
  ok (var , rte)

let get_type_annotation_ : I.type_annotation_ -> I.type_expression Location.wrap result = fun p ->
    match p with
    | I.Type_annotation_ p -> ok p

let get_e_match_clause : I.e_match_clause -> (I.pattern Location.wrap * I.expression_no_match Location.wrap) result = fun e ->
  match e with
  | E_match_clause c -> ok c

let rec type_expression : I.type_expression -> O.type_expression result = fun te ->
  match te with
  | T_variable tv ->
      let%bind tv' = bind_map_location type_variable tv in
      ok @@ unwrap tv'
  | T_tuple lst ->
      let%bind lst' = bind_map_list (bind_map_location type_expression) lst in
      ok @@ O.T_tuple (List.map unwrap lst')
  | T_paren p ->
      let%bind p' = bind_map_location type_expression p in
      ok @@ unwrap p'
  | T_record r ->
      let aux : I.t_record_element -> _ = fun (T_record_element (s, te)) ->
        let%bind te' = bind_map_location type_expression te in
        ok (s, te')
      in
      let%bind r' = bind_map_list (bind_map_location aux) r in
      let te_map =
        let lst = List.map ((fun (x, y) -> unwrap x, unwrap y) >| unwrap) r' in
        let open Map.String in
        List.fold_left (fun prec (k , v) -> add k v prec) empty lst
      in
      ok @@ O.T_record te_map
  | T_application (arg , f) ->
      let%bind arg' = bind_map_location type_expression arg in
      match unwrap f with
        | I.T_variable v -> (
            match List.assoc_opt v.wrap_content type_constants with
            | Some n -> (
                match arg'.wrap_content with
                | T_tuple lst -> (
                    let%bind () =
                      trace (simple_error "bad arity") @@
                      Assert.assert_list_size lst n in
                    ok @@ O.T_constant (v.wrap_content , lst)
                  )
                | _ -> simple_fail "bad arity"
              )
            | None -> (
                let error =
                  let title () = "unrecognized type-constant" in
                  let content () = Format.asprintf "%s" v.wrap_content in
                  error title content
                in
                fail error
              )
          )
        | _ -> simple_fail "type applying to non-var"

let restricted_type_expression : I.restricted_type_expression -> O.type_expression result = fun rte ->
  match rte with
  | Tr_variable tv ->
      let%bind tv' = bind_map_location type_variable tv in
      ok @@ unwrap tv'
  | Tr_paren te -> type_expression (unwrap te)

let rec expression : I.expression -> O.annotated_expression result = fun e ->
  match e with
  | E_sequence _
  | E_let_in _
  | E_ifthen _
  | E_ifthenelse _
    -> simple_fail "not block expressions in local expressions yet"
  | E_fun _ -> simple_fail "no local functions yet"
  | E_match _ -> simple_fail "no match in expressions yet"
  | E_main m ->
      let%bind m' = bind_map_location expression_main m in
      ok @@ unwrap m'
  | E_record r -> expression_record r

and expression_no_match_block : I.expression_no_match -> O.block result = fun e ->
  match e with
  | I.Em_let_in _|I.Em_fun _|I.Em_record _|I.Em_ifthenelse _|I.Em_ifthen _
  |I.Em_main _ -> simple_fail "lel"

and sequence_block : I.expression Location.wrap list -> O.block result = fun s ->
  let%bind blocks = bind_map_list (bind_map_location expression_block) s in
  let block = List.(concat @@ map unwrap blocks) in
  ok block

and let_in_block : (I.pattern Location.wrap * I.expression Location.wrap * I.expression Location.wrap) -> O.block result =
  fun (var , expr , body) ->
  let%bind (var' , te) = get_p_typed_variable (unwrap var) in
  let%bind expr' =
    let%bind expr' = bind_map_location expression expr in
    bind_map_location O.Combinators.get_untyped_expression expr' in
  let%bind te' = bind_map_location restricted_type_expression te in
  let instruction = O.Combinators.(i_assignment @@ named_typed_expression (unwrap var') (unwrap expr') (unwrap te')) in
  let%bind body' = bind_map_location expression_block body in
  ok @@ instruction :: (unwrap body')

and if_then_else_block : (I.expression Location.wrap * I.expression Location.wrap * I.expression Location.wrap) -> O.block result =
  fun (cond , branch_true , branch_false) ->
  let%bind cond' = bind_map_location expression cond in
  let%bind branch_true' = bind_map_location expression_block branch_true in
  let%bind branch_false' = bind_map_location expression_block branch_false in
  ok [ O.I_matching ((unwrap cond') , Match_bool { match_true = (unwrap branch_true') ; match_false = (unwrap branch_false') }) ]

and if_then_block : (I.expression Location.wrap * I.expression Location.wrap) -> O.block result =
  fun (cond , branch_true) ->
  let%bind cond' = bind_map_location expression cond in
  let%bind branch_true' = bind_map_location expression_block branch_true in
  let branch_false = O.I_skip in
  ok [ O.I_matching ((unwrap cond') , Match_bool { match_true = (unwrap branch_true') ; match_false = [ branch_false ] }) ]

and match_clauses : type a . (I.pattern * a) list -> a O.matching result = fun _clauses ->
  let match_bool _ = simple_fail "" in
  let match_stuff _ = simple_fail "" in
  bind_find_map_list (simple_error "no weird matching yet") (fun f -> f ()) [ match_bool ; match_stuff ]

and match_block : _ -> O.block result = fun (case , clauses) ->
  let%bind case' = bind_map_location expression case in
  let%bind clauses' =
    let u = List.map unwrap clauses in
    let%bind cs = bind_map_list get_e_match_clause u in
    let ucs = List.map (Tuple.map_h_2 unwrap unwrap) cs in
    let%bind ucs' =
      let aux (x , y) =
        let%bind y' = expression_no_match_block y in
        ok (x , y') in
      bind_map_list aux ucs in
    ok ucs' in
  let%bind matching = match_clauses clauses' in
  ok [ O.I_matching ((unwrap case') , matching) ]

and expression_block : I.expression -> O.block result = fun e ->
  match e with
  | I.E_sequence s -> sequence_block s
  | I.E_let_in li -> let_in_block li
  | I.E_ifthenelse ite -> if_then_else_block ite
  | I.E_ifthen it -> if_then_block it
  | I.E_match cc -> match_block cc
  |I.E_fun _|I.E_record _
  |I.E_main _ -> simple_fail "no regular expression in blocks"

and expression_record : _ -> O.annotated_expression result = fun r ->
  let aux : I.e_record_element -> _ = fun re ->
    match re with
    | E_record_element_record_implicit _ -> simple_fail "no implicit record element yet"
    | E_record_element_record_explicit (s, e) ->
        let%bind e' = bind_map_location expression_no_seq e in
        ok (s, e')
  in
  let%bind r' = bind_map_list (bind_map_location aux) r in
  let e_map =
    let lst = List.map ((fun (x, y) -> unwrap x, unwrap y) >| unwrap) r' in
    let open Map.String in
    List.fold_left (fun prec (k , v) -> add k v prec) empty lst
  in
  ok @@ O.(make_e_a @@ E_record e_map)

and expression_main : I.expression_main -> O.annotated_expression result = fun em ->
  let return x = ok @@ make_e_a x in
  let simple_binop name ab =
    let%bind (a' , b') = bind_map_pair (bind_map_location expression_main) ab in
    return @@ E_constant (name, [unwrap a' ; unwrap b']) in
  match em with
  | Eh_tuple lst ->
      let%bind lst' = bind_map_list (bind_map_location expression_main) lst in
      return @@ E_tuple (List.map unwrap lst')
  | Eh_application farg ->
      (* TODO: constructor case *)
      let%bind farg' = bind_map_pair (bind_map_location expression_main) farg in
      return @@ E_application (Tuple.map2 unwrap farg')
  | Eh_type_annotation (e, te) ->
      let%bind e' = bind_map_location expression_main e in
      let%bind e'' = match (unwrap e').type_annotation with
        | None -> ok (unwrap e').expression
        | Some _ -> simple_fail "can't double annotate" in
      let%bind te' = bind_map_location restricted_type_expression te in
      ok @@ make_e_a_full e'' (unwrap te')
  | Eh_lt ab ->
      simple_binop "LT" ab
  | Eh_gt ab ->
      simple_binop "GT" ab
  | Eh_le ab ->
      simple_binop "LE" ab
  | Eh_eq ab ->
      simple_binop "EQ" ab
  | Eh_cons ab ->
      simple_binop "CONS" ab
  | Eh_addition ab ->
      simple_binop "ADD" ab
  | Eh_substraction ab ->
      simple_binop "MINUS" ab
  | Eh_multiplication ab ->
      simple_binop "TIMES" ab
  | Eh_division ab ->
      simple_binop "DIV" ab
  | Eh_int n ->
      return @@ E_literal (Literal_int (unwrap n))
  | Eh_string s ->
      return @@ E_literal (Literal_string (unwrap s))
  | Eh_unit _ ->
      return @@ E_literal Literal_unit
  | Eh_tz _ ->
      simple_fail "tz literals not supported yet"
  | Eh_module_ident _ ->
      simple_fail "modules not supported yet"
  | Eh_variable v ->
      return @@ E_variable (unwrap v)
  | Eh_constructor _ ->
      simple_fail "constructor without parameter"
  | Eh_list _ ->
      simple_fail "list not supported yet"
  | Eh_name _ ->
      simple_fail "named parameter not supported yet"
  | Eh_assign _ ->
      simple_fail "assign not supported yet"
  | Eh_accessor _ ->
      simple_fail "accessor not supported yet"
  | Eh_bottom e ->
      expression (unwrap e)

and expression_no_seq : I.expression_no_seq -> O.annotated_expression result = fun mns ->
  match mns with
  | Es_record r -> expression_record r
  | Es_let_in _
  | Es_ifthen _
  | Es_ifthenelse _
    -> simple_fail "not block expressions in local expressions yet"
  | Es_fun _ -> simple_fail "no local functions yet"
  | Es_match _ -> simple_fail "no match in expressions yet"
  | Es_main e ->
      expression_main (unwrap e)

let let_content : I.let_content -> _ result = fun (Let_content (n, args, ty_opt, e)) ->
  let%bind () =
    trace_strong (simple_error "no sugar-candy for args yet") @@
    Assert.assert_list_empty args in
  let%bind args' = bind_map_list (bind_map_location get_arg) args in
  let%bind ty' =
    let%bind tya =
      trace_option (simple_error "top-level declarations need a type") @@
      ty_opt in
    let%bind ty = get_type_annotation_ (unwrap tya) in
    bind_map_location type_expression ty in
  match args' with
  | [] -> ( (* No arguments. Simplify as regular value. *)
      let%bind e' =
        let%bind e' = bind_map_location expression e in
        bind_map_location O.Combinators.get_untyped_expression e' in
      let ae = make_e_a_full (unwrap e') (unwrap ty') in
      ok @@ O.Declaration_constant {name = (unwrap n) ; annotated_expression = ae}
    )
  | _lst -> ( (* Arguments without fun. *)
      simple_fail "no syntactic sugar for functions yet"
    )

let statement : I.statement -> O.declaration result = fun s ->
  match s with
  | Statement_variable_declaration x -> let_content (unwrap x)
  | Statement_init_declaration x -> let_content (unwrap x)
  | Statement_entry_declaration x -> let_content (unwrap x)
  | Statement_type_declaration (n, te) ->
      let%bind te' = bind_map_location type_expression te in
      ok @@ O.Declaration_type {type_name = unwrap n ; type_expression = unwrap te'}

let program : I.program -> O.program result = fun (Program lst) ->
  bind_map_list (bind_map_location statement) lst

let main : I.entry_point -> O.program Location.wrap result =
  bind_map_location program
