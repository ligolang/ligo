open Trace

module I = Ast_simplified
module O = Ast_typed
open O.Combinators

module Environment = O.Environment

module Solver = Typer_new.Solver

type environment = Environment.t

module Errors = struct
  let unbound_type_variable (e:environment) (tv:I.type_variable) () =
    let name = Var.to_name tv in
    let suggestion = match name with
        | "integer" -> "int"
        | "str" -> "string"
        | "boolean" -> "bool"
        | _ -> "no suggestion" in
    let title = (thunk "unbound type variable") in
    let message () = "" in
    let data = [
      ("variable" , fun () -> Format.asprintf "%a" I.PP.type_variable tv) ;
      (* TODO: types don't have srclocs for now. *)
      (* ("location" , fun () -> Format.asprintf "%a" Location.pp (n.location)) ; *)
      ("in" , fun () -> Format.asprintf "%a" Environment.PP.full_environment e) ;
      ("did_you_mean" , fun () -> suggestion)
    ] in
    error ~data title message ()

  let unbound_variable (e:environment) (n:I.expression_variable) (loc:Location.t) () =
    let name () = Format.asprintf "%a" I.PP.expression_variable n in
    let title = (thunk ("unbound variable "^(name ()))) in
    let message () = "" in
    let data = [
      ("variable" , name) ;
      ("environment" , fun () -> Format.asprintf "%a" Environment.PP.full_environment e) ;
      ("location" , fun () -> Format.asprintf "%a" Location.pp loc)
    ] in
    error ~data title message ()

  let match_empty_variant : I.matching_expr -> Location.t -> unit -> _ =
    fun matching loc () ->
      let title = (thunk "match with no cases") in
      let message () = "" in
      let data = [
        ("variant" , fun () -> Format.asprintf "%a" I.PP.matching_type matching) ;
        ("location" , fun () -> Format.asprintf "%a" Location.pp loc)
      ] in
      error ~data title message ()

  let match_missing_case : I.matching_expr -> Location.t -> unit -> _ =
    fun matching loc () ->
    let title = (thunk "missing case in match") in
    let message () = "" in
    let data = [
      ("variant" , fun () -> Format.asprintf "%a" I.PP.matching_type matching) ;
      ("location" , fun () -> Format.asprintf "%a" Location.pp loc)
    ] in
    error ~data title message ()

  let match_redundant_case : I.matching_expr -> Location.t -> unit -> _ =
    fun matching loc () ->
    let title = (thunk "redundant case in match") in
    let message () = "" in
    let data = [
      ("variant" , fun () -> Format.asprintf "%a" I.PP.matching_type matching) ;
      ("location" , fun () -> Format.asprintf "%a" Location.pp loc)
    ] in
    error ~data title message ()

  let unbound_constructor (e:environment) (c:I.constructor') (loc:Location.t) () =
    let title = (thunk "unbound constructor") in
    let message () = "" in
    let data = [
      ("constructor" , fun () -> Format.asprintf "%a" I.PP.constructor c);
      ("environment" , fun () -> Format.asprintf "%a" Environment.PP.full_environment e) ;
      ("location" , fun () -> Format.asprintf "%a" Location.pp loc)
    ] in
    error ~data title message ()

  let redundant_constructor (e:environment) (c:I.constructor') () =
    let title = (thunk "redundant constructor") in
    let message () = "" in
    let data = [
      ("constructor" , fun () -> Format.asprintf "%a" I.PP.constructor c);
      ("environment" , fun () -> Format.asprintf "%a" Environment.PP.full_environment e) ;
    ] in
    error ~data title message ()

  let wrong_arity (n:string) (expected:int) (actual:int) (loc : Location.t) () =
    let title () = "wrong arity" in
    let message () = "" in
    let data = [
      ("function" , fun () -> Format.asprintf "%s" n) ;
      ("expected" , fun () -> Format.asprintf "%d" expected) ;
      ("actual" , fun () -> Format.asprintf "%d" actual) ;
      ("location" , fun () -> Format.asprintf "%a" Location.pp loc)
    ] in
    error ~data title message ()


  let match_tuple_wrong_arity (expected:'a list) (actual:'b list) (loc:Location.t) () =
    let title () = "matching tuple of different size" in
    let message () = "" in
    let data = [
      ("expected" , fun () -> Format.asprintf "%d" (List.length expected)) ;
      ("actual" , fun () -> Format.asprintf "%d" (List.length actual)) ;
      ("location" , fun () -> Format.asprintf "%a" Location.pp loc)
    ] in
    error ~data title message ()

  (* TODO: this should be a trace_info? *)
  let program_error (p:I.program) () =
    let message () = "" in
    let title = (thunk "typing program") in
    let data = [
      ("program" , fun () -> Format.asprintf "%a" I.PP.program p)
    ] in
    error ~data title message ()

  let constant_declaration_error (name:I.expression_variable) (ae:I.expr) (expected: O.type_expression option) () =
    let title = (thunk "typing constant declaration") in
    let message () = "" in
    let data = [
      ("constant" , fun () -> Format.asprintf "%a" I.PP.expression_variable name) ;
      ("expression" , fun () -> Format.asprintf "%a" I.PP.expression ae) ;
      ("expected" , fun () ->
          match expected with
            None -> "(no annotation for the expected type)"
          | Some expected -> Format.asprintf "%a" O.PP.type_expression expected) ;
      ("location" , fun () -> Format.asprintf "%a" Location.pp ae.location)
    ] in
    error ~data title message ()

  let match_error : ?msg:string -> expected: I.matching_expr -> actual: O.type_expression -> Location.t -> unit -> _ =
    fun ?(msg = "") ~expected ~actual loc () ->
    let title = (thunk "typing match") in
    let message () = msg in
    let data = [
      ("expected" , fun () -> Format.asprintf "%a" I.PP.matching_type expected);
      ("actual" , fun () -> Format.asprintf "%a" O.PP.type_expression actual) ;
      ("location" , fun () -> Format.asprintf "%a" Location.pp loc)
    ] in
    error ~data title message ()

  let needs_annotation (e : I.expression) (case : string) () =
    let title = (thunk "this expression must be annotated with its type") in
    let message () = Format.asprintf "%s needs an annotation" case in
    let data = [
      ("expression" , fun () -> Format.asprintf "%a" I.PP.expression e) ;
      ("location" , fun () -> Format.asprintf "%a" Location.pp e.location)
    ] in
    error ~data title message ()

  let fvs_in_create_contract_lambda (e : I.expression) (fvar : Ast_typed.expression_variable) () =
    let title = (thunk "No free variable allowed in this lambda") in
    let message () = Format.asprintf "variable '%a'" Var.pp fvar in
    let data = [
      ("expression" , fun () -> Format.asprintf "%a" I.PP.expression e) ;
      ("location" , fun () -> Format.asprintf "%a" Location.pp e.location)
    ] in
    error ~data title message ()

  let create_contract_lambda (cst : I.constant') (e : I.expression) () =
    let title () = Format.asprintf "%a first argument must be inlined" I.PP.constant cst in
    let message () = Format.asprintf "contract code can be inlined using a lambda" in
    let data = [
      ("expression" , fun () -> Format.asprintf "%a" I.PP.expression e) ;
      ("location" , fun () -> Format.asprintf "%a" Location.pp e.location)
    ] in
    error ~data title message ()

  let type_error_approximate ?(msg="") ~(expected: string) ~(actual: O.type_expression) ~(expression : I.expression) (loc:Location.t) () =
    let title = (thunk "type error") in
    let message () = msg in
    let data = [
      ("expected"   , fun () -> Format.asprintf "%s" expected);
      ("actual"     , fun () -> Format.asprintf "%a" O.PP.type_expression actual);
      ("expression" , fun () -> Format.asprintf "%a" I.PP.expression expression) ;
      ("location" , fun () -> Format.asprintf "%a" Location.pp loc)
    ] in
    error ~data title message ()

  let type_error ?(msg="") ~(expected: O.type_expression) ~(actual: O.type_expression) ~(expression : I.expression) (loc:Location.t) () =
    let title = (thunk "type error") in
    let message () = msg in
    let data = [
      ("expected"   , fun () -> Format.asprintf "%a" O.PP.type_expression expected);
      ("actual"     , fun () -> Format.asprintf "%a" O.PP.type_expression actual);
      ("expression" , fun () -> Format.asprintf "%a" I.PP.expression expression) ;
      ("location" , fun () -> Format.asprintf "%a" Location.pp loc)
    ] in
    error ~data title message ()

  let bad_record_access (field : I.label) (ae : I.expression) (t : O.type_expression) (loc:Location.t) () =
    let title = (thunk "invalid record field") in
    let message () = "" in
    let data = [
      ("field" , fun () -> Format.asprintf "%a" I.PP.label field) ;
      ("record_value" , fun () -> Format.asprintf "%a" I.PP.expression ae) ;
      ("tuple_type" , fun () -> Format.asprintf "%a" O.PP.type_expression t) ;
      ("location" , fun () -> Format.asprintf "%a" Location.pp loc)
    ] in
    error ~data title message ()

end
open Errors

let rec type_program (p:I.program) : (O.program * Solver.state) result =
  let aux (e, acc:(environment * O.declaration Location.wrap list)) (d:I.declaration Location.wrap) =
    let%bind ed' = (bind_map_location (type_declaration e (Solver.placeholder_for_state_of_new_typer ()))) d in
    let loc : 'a . 'a Location.wrap -> _ -> _ = fun x v -> Location.wrap ~loc:x.location v in
    let (e', _placeholder_for_state_of_new_typer , d') = Location.unwrap ed' in
    match d' with
    | None -> ok (e', acc)
    | Some d' -> ok (e', loc ed' d' :: acc)
  in
  let%bind (_, lst) =
    trace (fun () -> program_error p ()) @@
    bind_fold_list aux (Environment.full_empty, []) p in
  ok @@ (List.rev lst , (Solver.placeholder_for_state_of_new_typer ()))

and type_declaration env (_placeholder_for_state_of_new_typer : Solver.state) : I.declaration -> (environment * Solver.state * O.declaration option) result = function
  | Declaration_type (type_name , type_expression) ->
      let%bind tv = evaluate_type env type_expression in
      let env' = Environment.add_type (type_name) tv env in
      ok (env', (Solver.placeholder_for_state_of_new_typer ()) , None)
  | Declaration_constant (name , tv_opt , inline, expression) -> (
      let%bind tv'_opt = bind_map_option (evaluate_type env) tv_opt in
      let%bind ae' =
        trace (constant_declaration_error name expression tv'_opt) @@
        type_expression' ?tv_opt:tv'_opt env expression in
      let env' = Environment.add_ez_ae name ae' env in
      ok (env', (Solver.placeholder_for_state_of_new_typer ()) , Some (O.Declaration_constant (name,ae', inline, env')))
    )

and type_match : (environment -> I.expression -> O.expression result) -> environment -> O.type_expression -> I.matching_expr -> I.expression -> Location.t -> O.matching_expr result =
  fun f e t i ae loc -> match i with
    | Match_bool {match_true ; match_false} ->
      let%bind _ =
        trace_strong (match_error ~expected:i ~actual:t loc)
        @@ get_t_bool t in
      let%bind match_true = f e match_true in
      let%bind match_false = f e match_false in
      ok (O.Match_bool {match_true ; match_false})
  | Match_option {match_none ; match_some} ->
      let%bind t_opt =
        trace_strong (match_error ~expected:i ~actual:t loc)
        @@ get_t_option t in
      let%bind match_none = f e match_none in
      let (n, b,_) = match_some in
      let e' = Environment.add_ez_binder n t_opt e in
      let%bind b' = f e' b in
      ok (O.Match_option {match_none ; match_some = (n, b', t_opt)})
  | Match_list {match_nil ; match_cons} ->
      let%bind t_elt =
        trace_strong (match_error ~expected:i ~actual:t loc)
        @@ get_t_list t in
      let%bind match_nil = f e match_nil in
      let (hd, tl, b,_) = match_cons in
      let e' = Environment.add_ez_binder hd t_elt e in
      let e' = Environment.add_ez_binder tl t e' in
      let%bind b' = f e' b in
      ok (O.Match_list {match_nil ; match_cons = (hd, tl, b', t_elt)})
  | Match_tuple ((lst, b),_) ->
      let%bind t_tuple =
        trace_strong (match_error ~expected:i ~actual:t loc)
        @@ get_t_tuple t in
      let%bind lst' =
        generic_try (match_tuple_wrong_arity t_tuple lst loc)
        @@ (fun () -> List.combine lst t_tuple) in
      let aux prev (name, tv) = Environment.add_ez_binder name tv prev in
      let e' = List.fold_left aux e lst' in
      let%bind b' = f e' b in
      ok (O.Match_tuple ((lst, b'),t_tuple))
  | Match_variant (lst,_) ->
      let%bind variant_opt =
        let aux acc ((constructor_name , _) , _) =
          let%bind (_ , variant) =
            trace_option (unbound_constructor e constructor_name loc) @@
            Environment.get_constructor constructor_name e in
          let%bind acc = match acc with
            | None -> ok (Some variant)
            | Some variant' -> (
                trace (type_error
                         ~msg:"in match variant"
                         ~expected:variant
                         ~actual:variant'
                         ~expression:ae
                         loc
                      ) @@
                Ast_typed.assert_type_expression_eq (variant , variant') >>? fun () ->
                ok (Some variant)
              ) in
          ok acc in
        trace (simple_info "in match variant") @@
        bind_fold_list aux None lst in
      let%bind variant =
        trace_option (match_empty_variant i loc) @@
        variant_opt in
      let%bind () =
        let%bind variant_cases' =
          trace (match_error ~expected:i ~actual:t loc)
          @@ Ast_typed.Combinators.get_t_sum variant in
        let variant_cases = List.map fst @@ I.CMap.to_kv_list variant_cases' in
        let match_cases = List.map (Function.compose fst fst) lst in
        let test_case = fun c ->
          Assert.assert_true (List.mem c match_cases)
        in
        let%bind () =
          trace_strong (match_missing_case i loc) @@
          bind_iter_list test_case variant_cases in
        let%bind () =
          trace_strong (match_redundant_case i loc) @@
          Assert.assert_true List.(length variant_cases = length match_cases) in
        ok ()
      in
      let%bind lst' =
        let aux ((constructor_name , name) , b) =
          let%bind (constructor , _) =
            trace_option (unbound_constructor e constructor_name loc) @@
            Environment.get_constructor constructor_name e in
          let e' = Environment.add_ez_binder name constructor e in
          let%bind b' = f e' b in
          ok ((constructor_name , name) , b')
        in
        bind_map_list aux lst in
      ok (O.Match_variant (lst' , variant))

and evaluate_type (e:environment) (t:I.type_expression) : O.type_expression result =
  let return tv' = ok (make_t tv' (Some t)) in
  match t.type_content with
  | T_arrow {type1;type2} ->
      let%bind type1 = evaluate_type e type1 in
      let%bind type2 = evaluate_type e type2 in
      return (T_arrow {type1;type2})
  | T_sum m ->
      let aux k v prev =
        let%bind prev' = prev in
        let%bind v' = evaluate_type e v in
        let%bind () = match Environment.get_constructor k e with
          | Some _ -> fail (redundant_constructor e k)
          | None -> ok () in
        ok @@ I.CMap.add k v' prev'
      in
      let%bind m = I.CMap.fold aux m (ok I.CMap.empty) in
      return (T_sum m)
  | T_record m ->
      let aux k v prev =
        let%bind prev' = prev in
        let%bind v' = evaluate_type e v in
        ok @@ I.LMap.add k v' prev'
      in
      let%bind m = I.LMap.fold aux m (ok I.LMap.empty) in
      return (T_record m)
  | T_variable name ->
      let%bind tv =
        trace_option (unbound_type_variable e name)
        @@ Environment.get_type_opt (name) e in
      ok tv
  | T_constant cst ->
      return (T_constant cst)
  | T_operator opt ->
      let%bind opt = match opt with
        | TC_set s -> 
            let%bind s = evaluate_type e s in 
            ok @@ O.TC_set (s) 
        | TC_option o -> 
            let%bind o = evaluate_type e o in 
            ok @@ O.TC_option (o) 
        | TC_list l -> 
            let%bind l = evaluate_type e l in 
            ok @@ O.TC_list (l) 
        | TC_map (k,v) ->
            let%bind k = evaluate_type e k in 
            let%bind v = evaluate_type e v in 
            ok @@ O.TC_map (k,v) 
        | TC_big_map (k,v) ->
            let%bind k = evaluate_type e k in 
            let%bind v = evaluate_type e v in 
            ok @@ O.TC_big_map (k,v) 
        | TC_arrow ( arg , ret ) ->
            let%bind arg' = evaluate_type e arg in
            let%bind ret' = evaluate_type e ret in
            ok @@ O.TC_arrow ( arg' , ret' )
        | TC_contract c ->
            let%bind c = evaluate_type e c in
            ok @@ O.TC_contract c
        in
      return (T_operator (opt))

and type_expression : environment -> Solver.state -> ?tv_opt:O.type_expression -> I.expression -> (O.expression * Solver.state) result
  = fun e _placeholder_for_state_of_new_typer ?tv_opt ae ->
    let%bind res = type_expression' e ?tv_opt ae in
    ok (res, (Solver.placeholder_for_state_of_new_typer ()))
and type_expression' : environment -> ?tv_opt:O.type_expression -> I.expression -> O.expression result = fun e ?tv_opt ae ->
  let module L = Logger.Stateful() in
  let return expr tv =
    let%bind () =
      match tv_opt with
      | None -> ok ()
      | Some tv' -> O.assert_type_expression_eq (tv' , tv) in
    let location = ae.location in
    ok @@ make_a_e ~location expr tv e in
  let main_error =
    let title () = "typing expression" in
    let content () = "" in
    let data = [
      ("expression" , fun () -> Format.asprintf "%a" I.PP.expression ae) ;
      ("location" , fun () -> Format.asprintf "%a" Location.pp ae.location) ;
      ("misc" , fun () -> L.get ()) ;
    ] in
    error ~data title content in
  trace main_error @@
  match ae.expression_content with
  (* Basic *)
  | E_variable name ->
      let%bind tv' =
        trace_option (unbound_variable e name ae.location)
        @@ Environment.get_opt name e in
      return (E_variable name) tv'.type_value
  | E_literal (Literal_bool b) ->
      return (E_literal (Literal_bool b)) (t_bool ())
  | E_literal Literal_unit | E_skip ->
      return (E_literal (Literal_unit)) (t_unit ())
  | E_literal Literal_void -> return (E_literal (Literal_void)) (t_unit ()) (* TODO : IS this really a t_unit ?*)
  | E_literal (Literal_string s) ->
      return (E_literal (Literal_string s)) (t_string ())
  | E_literal (Literal_key s) ->
      return (E_literal (Literal_key s)) (t_key ())
  | E_literal (Literal_key_hash s) ->
      return (E_literal (Literal_key_hash s)) (t_key_hash ())
  | E_literal (Literal_chain_id s) ->
      return (E_literal (Literal_chain_id s)) (t_chain_id ())
  | E_literal (Literal_signature s) ->
      return (E_literal (Literal_signature s)) (t_signature ())
  | E_literal (Literal_bytes s) ->
      return (E_literal (Literal_bytes s)) (t_bytes ())
  | E_literal (Literal_int n) ->
      return (E_literal (Literal_int n)) (t_int ())
  | E_literal (Literal_nat n) ->
      return (E_literal (Literal_nat n)) (t_nat ())
  | E_literal (Literal_timestamp n) ->
      return (E_literal (Literal_timestamp n)) (t_timestamp ())
  | E_literal (Literal_mutez n) ->
      return (E_literal (Literal_mutez n)) (t_mutez ())
  | E_literal (Literal_address s) ->
      return (e_address s) (t_address ())
  | E_literal (Literal_operation op) ->
      return (e_operation op) (t_operation ())
  | E_record_accessor {expr;label} ->
      let%bind e' = type_expression' e expr in
      let aux (prev:O.expression) (a:I.label) : O.expression result =
            let property = a in
            let%bind r_tv = get_t_record prev.type_expression in
            let%bind tv =
              generic_try (bad_record_access property ae prev.type_expression ae.location)
              @@ (fun () -> I.LMap.find property r_tv) in
            let location = ae.location in
            ok @@ make_a_e ~location (E_record_accessor {expr=prev; label=property}) tv e
      in
      let%bind ae =
      trace (simple_info "accessing") @@ aux e' label in
      (* check type annotation of the final accessed element *)
      let%bind () =
        match tv_opt with
        | None -> ok ()
        | Some tv' -> O.assert_type_expression_eq (tv' , ae.type_expression) in
      ok(ae)
  (* Sum *)
  | E_constructor {constructor; element} ->
      let%bind (c_tv, sum_tv) =
        let error =
          let title () = "no such constructor" in
          let content () =
            Format.asprintf "%a in:\n%a\n"
              Stage_common.PP.constructor constructor 
              O.Environment.PP.full_environment e
          in
          error title content in
        trace_option error @@
        Environment.get_constructor constructor e in
      let%bind expr' = type_expression' e element in
      let%bind _assert = O.assert_type_expression_eq (expr'.type_expression, c_tv) in
      return (E_constructor {constructor; element=expr'}) sum_tv
  (* Record *)
  | E_record m ->
      let aux prev k expr =
        let%bind expr' = type_expression' e expr in
        ok (I.LMap.add k expr' prev)
      in
      let%bind m' = Stage_common.Helpers.bind_fold_lmap aux (ok I.LMap.empty) m in
      return (E_record m') (t_record (I.LMap.map get_type_expression m') ())
  | E_record_update {record; path; update} ->

    let%bind record = type_expression' e record in
    let%bind update = type_expression' e update in
    let wrapped = get_type_expression record in
    let%bind tv = 
      match wrapped.type_content with 
      | T_record record -> (
          let field_op = I.LMap.find_opt path record in
          match field_op with
          | Some tv -> ok (tv)
          | None -> failwith @@ Format.asprintf "field %a is not part of record %a" Stage_common.PP.label path O.PP.type_expression wrapped
      )
      | _ -> failwith "Update an expression which is not a record"
    in
    let%bind () = O.assert_type_expression_eq (tv, get_type_expression update) in
    return (E_record_update {record; path; update}) wrapped
  (* Data-structure *)
  | E_list lst ->
      let%bind lst' = bind_map_list (type_expression' e) lst in
      let%bind tv =
        let aux opt c =
          match opt with
          | None -> ok (Some c)
          | Some c' ->
              let%bind _eq = Ast_typed.assert_type_expression_eq (c, c') in
              ok (Some c') in
        let%bind init = match tv_opt with
          | None -> ok None
          | Some ty ->
              let%bind ty' = get_t_list ty in
              ok (Some ty') in
        let%bind ty =
          let%bind opt = bind_fold_list aux init
          @@ List.map get_type_expression lst' in
          trace_option (needs_annotation ae "empty list") opt in
        ok (t_list ty ())
      in
      return (E_list lst') tv
  | E_set lst ->
      let%bind lst' = bind_map_list (type_expression' e) lst in
      let%bind tv =
        let aux opt c =
          match opt with
          | None -> ok (Some c)
          | Some c' ->
              let%bind _eq = Ast_typed.assert_type_expression_eq (c, c') in
              ok (Some c') in
        let%bind init = match tv_opt with
          | None -> ok None
          | Some ty ->
              let%bind ty' = get_t_set ty in
              ok (Some ty') in
        let%bind ty =
          let%bind opt = bind_fold_list aux init
          @@ List.map get_type_expression lst' in
          trace_option (needs_annotation ae "empty set") opt in
        ok (t_set ty ())
      in
      return (E_set lst') tv
  | E_map lst ->
      let%bind lst' = bind_map_list (bind_map_pair (type_expression' e)) lst in
      let%bind tv =
        let aux opt c =
          match opt with
          | None -> ok (Some c)
          | Some c' ->
              let%bind _eq = Ast_typed.assert_type_expression_eq (c, c') in
              ok (Some c') in
        let%bind key_type =
          let%bind sub =
            bind_fold_list aux None
            @@ List.map get_type_expression
            @@ List.map fst lst' in
          let%bind annot = bind_map_option get_t_map_key tv_opt in
          trace (simple_info "empty map expression without a type annotation") @@
          O.merge_annotation annot sub (needs_annotation ae "this map literal")
        in
        let%bind value_type =
          let%bind sub =
            bind_fold_list aux None
            @@ List.map get_type_expression
            @@ List.map snd lst' in
          let%bind annot = bind_map_option get_t_map_value tv_opt in
          trace (simple_info "empty map expression without a type annotation") @@
          O.merge_annotation annot sub (needs_annotation ae "this map literal")
        in
        ok (t_map key_type value_type ())
      in
      return (E_map lst') tv
  | E_big_map lst ->
      let%bind lst' = bind_map_list (bind_map_pair (type_expression' e)) lst in
      let%bind tv =
        let aux opt c =
          match opt with
          | None -> ok (Some c)
          | Some c' ->
              let%bind _eq = Ast_typed.assert_type_expression_eq (c, c') in
              ok (Some c') in
        let%bind key_type =
          let%bind sub =
            bind_fold_list aux None
            @@ List.map get_type_expression
            @@ List.map fst lst' in
          let%bind annot = bind_map_option get_t_big_map_key tv_opt in
          trace (simple_info "empty map expression without a type annotation") @@
          O.merge_annotation annot sub (needs_annotation ae "this map literal")
        in
        let%bind value_type =
          let%bind sub =
            bind_fold_list aux None
            @@ List.map get_type_expression
            @@ List.map snd lst' in
          let%bind annot = bind_map_option get_t_big_map_value tv_opt in
          trace (simple_info "empty map expression without a type annotation") @@
          O.merge_annotation annot sub (needs_annotation ae "this map literal")
        in
        ok (t_big_map key_type value_type ())
      in
      return (E_big_map lst') tv
  | E_lambda {
      binder ;
      input_type ;
      output_type ;
      result ;
    } -> (
      let%bind input_type =
        let%bind input_type =
          (* Hack to take care of let_in introduced by `simplify/cameligo.ml` in ECase's hack *)
          let default_action e () = fail @@ (needs_annotation e "the returned value") in
          match input_type with
          | Some ty -> ok ty
          | None -> (
              match result.expression_content with
              | I.E_let_in li -> (
                  match li.rhs.expression_content with
                  | I.E_variable name when name = (fst binder) -> (
                      match snd li.let_binder with
                      | Some ty -> ok ty
                      | None -> default_action li.rhs ()
                    )
                  | _ -> default_action li.rhs ()
                )
              | _ -> default_action result ()
            )
        in
        evaluate_type e input_type in
      let%bind output_type =
        bind_map_option (evaluate_type e) output_type
      in
      let binder = fst binder in
      let e' = Environment.add_ez_binder binder input_type e in
      let%bind body = type_expression' ?tv_opt:output_type e' result in
      let output_type = body.type_expression in
      return (E_lambda {binder; result=body}) (t_function input_type output_type ())
    )
  | E_constant {cons_name=( C_LIST_FOLD | C_MAP_FOLD | C_SET_FOLD) as opname ;
                arguments=[
                    ( { expression_content = (I.E_lambda { binder = (lname, None) ;
                                                   input_type = None ; 
                                                   output_type = None ; 
                                                   result }) ;
                        location = _ }) as _lambda ;
                    collect ; 
                    init_record ;
                  ]} ->
      (* this special case is here force annotation of the untyped lambda
         generated by pascaligo's for_collect loop *)
      let%bind (v_col , v_initr ) = bind_map_pair (type_expression' e) (collect , init_record ) in
      let tv_col = get_type_expression v_col   in (* this is the type of the collection  *) 
      let tv_out = get_type_expression v_initr in (* this is the output type of the lambda*)
      let%bind input_type = match tv_col.type_content with
        | O.T_operator ( TC_list t | TC_set t) -> ok @@ make_t_ez_record (("0",tv_out)::[("1",t)])
        | O.T_operator ( TC_map (k,v)| TC_big_map (k,v)) -> ok @@ make_t_ez_record (("0",tv_out)::[("1",make_t_ez_record [("0",k);("1",v)])])
        | _ ->
          let wtype = Format.asprintf
            "Loops over collections expect lists, sets or maps, got type %a" O.PP.type_expression tv_col in 
          fail @@ simple_error wtype in 
      let lname = lname in
      let e' = Environment.add_ez_binder lname input_type e in
      let%bind body = type_expression' ?tv_opt:(Some tv_out) e' result in
      let output_type = body.type_expression in
      let lambda' = make_a_e (E_lambda {binder = lname ; result=body}) (t_function input_type output_type ()) e' in
      let lst' = [lambda'; v_col; v_initr] in
      let tv_lst = List.map get_type_expression lst' in
      let%bind (opname', tv) =
        type_constant opname tv_lst tv_opt in
      return (E_constant {cons_name=opname';arguments=lst'}) tv
  | E_constant {cons_name=C_FOLD_WHILE as opname;
                arguments = [
                    ( { expression_content = (I.E_lambda { binder = (lname, None) ;
                                                   input_type = None ; 
                                                   output_type = None ; 
                                                   result }) ;
                        location = _ }) as _lambda ;
                    init_record ;
                ]} -> 
      let%bind v_initr = type_expression' e init_record in
      let tv_out = get_type_expression v_initr in
      let input_type  = tv_out in
      let e' = Environment.add_ez_binder lname input_type e in
      let%bind body = type_expression' e' result in
      let output_type = body.type_expression in
      let lambda' = make_a_e (E_lambda {binder = lname ; result=body}) (t_function input_type output_type ()) e' in
      let lst' = [lambda';v_initr] in
      let tv_lst = List.map get_type_expression lst' in
      let%bind (opname',tv) = type_constant opname tv_lst tv_opt in
      return (E_constant {cons_name=opname';arguments=lst'}) tv
  | E_constant {cons_name=C_CREATE_CONTRACT as cons_name;arguments} ->
      let%bind lst' = bind_list @@ List.map (type_expression' e) arguments in
      let%bind () = match lst' with
        | { expression_content = O.E_lambda l ; _ } :: _ ->
          let open Ast_typed.Misc in
          let fvs = Free_variables.lambda [] l in
          if List.length fvs = 0 then ok ()
          else fail @@ fvs_in_create_contract_lambda ae (List.hd fvs)
        | _ -> fail @@ create_contract_lambda C_CREATE_CONTRACT ae
      in
      let tv_lst = List.map get_type_expression lst' in
      let%bind (name', tv) =
        type_constant cons_name tv_lst tv_opt in
      return (E_constant {cons_name=name';arguments=lst'}) tv
  | E_constant {cons_name;arguments} ->
      let%bind lst' = bind_list @@ List.map (type_expression' e) arguments in
      let tv_lst = List.map get_type_expression lst' in
      let%bind (name', tv) =
        type_constant cons_name tv_lst tv_opt in
      return (E_constant {cons_name=name';arguments=lst'}) tv
  | E_application {expr1;expr2} ->
      let%bind expr1' = type_expression' e expr1 in
      let%bind expr2 = type_expression' e expr2 in
      let%bind tv = match expr1'.type_expression.type_content with
        | T_arrow {type1;type2} ->
            let%bind _ = O.assert_type_expression_eq (type1, expr2.type_expression) in
            ok type2
        | _ ->
          fail @@ type_error_approximate
            ~expected:"should be a function type"
            ~expression:expr1
            ~actual:expr1'.type_expression
            expr1'.location
      in
      return (E_application {expr1=expr1';expr2}) tv
  | E_look_up dsi ->
      let%bind (ds, ind) = bind_map_pair (type_expression' e) dsi in
      let%bind (src, dst) = bind_map_or (get_t_map , get_t_big_map) ds.type_expression in
      let%bind _ = O.assert_type_expression_eq (ind.type_expression, src) in
      return (E_look_up (ds , ind)) (t_option dst ())
  (* Advanced *)
  | E_matching {matchee;cases} -> (
      let%bind ex' = type_expression' e matchee in
      let%bind m' = type_match (type_expression' ?tv_opt:None) e ex'.type_expression cases ae ae.location in
      let tvs =
        let aux (cur:O.matching_expr) =
          match cur with
          | Match_bool { match_true ; match_false } -> [ match_true ; match_false ]
          | Match_list { match_nil ; match_cons = (_ , _ , match_cons, _) } -> [ match_nil ; match_cons ]
          | Match_option { match_none ; match_some = (_ , match_some, _) } -> [ match_none ; match_some ]
          | Match_tuple ((_ , match_tuple), _) -> [ match_tuple ]
          | Match_variant (lst , _) -> List.map snd lst in
        List.map get_type_expression @@ aux m' in
      let aux prec cur =
        let%bind () =
          match prec with
          | None -> ok ()
          | Some cur' -> Ast_typed.assert_type_expression_eq (cur , cur') in
        ok (Some cur) in
      let%bind tv_opt = bind_fold_list aux None tvs in
      let%bind tv =
        trace_option (match_empty_variant cases ae.location) @@
        tv_opt in
      return (O.E_matching {matchee=ex'; cases=m'}) tv
    )
  | E_let_in {let_binder ; rhs ; let_result; inline} ->
    let%bind rhs_tv_opt = bind_map_option (evaluate_type e) (snd let_binder) in
    let%bind rhs = type_expression' ?tv_opt:rhs_tv_opt e rhs in
    let let_binder = fst let_binder in
    let e' = Environment.add_ez_declaration (let_binder) rhs e in
    let%bind let_result = type_expression' e' let_result in
    return (E_let_in {let_binder; rhs; let_result; inline}) let_result.type_expression
  | E_ascription {anno_expr; type_annotation} ->
    let%bind tv = evaluate_type e type_annotation in
    let%bind expr' = type_expression' ~tv_opt:tv e anno_expr in
    let%bind type_annotation =
      O.merge_annotation
        (Some tv)
        (Some expr'.type_expression)
        (internal_assertion_failure "merge_annotations (Some ...) (Some ...) failed") in
    (* check type annotation of the expression as a whole (e.g. let x : t = (v : t') ) *)
    let%bind () =
      match tv_opt with
      | None -> ok ()
      | Some tv' -> O.assert_type_expression_eq (tv' , type_annotation) in
    ok {expr' with type_expression=type_annotation}


and type_constant (name:I.constant') (lst:O.type_expression list) (tv_opt:O.type_expression option) : (O.constant' * O.type_expression) result =
  let%bind typer = Operators.Typer.constant_typers name in
  let%bind tv = typer lst tv_opt in
  ok(name, tv)

let untype_type_expression (t:O.type_expression) : (I.type_expression) result =
  match t.type_meta with
  | Some s -> ok s
  | _ -> fail @@ internal_assertion_failure "trying to untype generated type"

let untype_literal (l:O.literal) : I.literal result =
  let open I in
  match l with
  | Literal_unit -> ok Literal_unit
  | Literal_void -> ok Literal_void
  | Literal_bool b -> ok (Literal_bool b)
  | Literal_nat n -> ok (Literal_nat n)
  | Literal_timestamp n -> ok (Literal_timestamp n)
  | Literal_mutez n -> ok (Literal_mutez n)
  | Literal_int n -> ok (Literal_int n)
  | Literal_string s -> ok (Literal_string s)
  | Literal_signature s -> ok (Literal_signature s)
  | Literal_key s -> ok (Literal_key s)

  | Literal_key_hash s -> ok (Literal_key_hash s)
  | Literal_chain_id s -> ok (Literal_chain_id s)
  | Literal_bytes b -> ok (Literal_bytes b)
  | Literal_address s -> ok (Literal_address s)
  | Literal_operation s -> ok (Literal_operation s)

let rec untype_expression (e:O.expression) : (I.expression) result =
  let open I in
  let return e = ok e in
  match e.expression_content with
  | E_literal l ->
      let%bind l = untype_literal l in
      return (e_literal l)
  | E_constant {cons_name;arguments} ->
      let%bind lst' = bind_map_list untype_expression arguments in
      return (e_constant cons_name lst')
  | E_variable n ->
      return (e_variable (n))
  | E_application {expr1;expr2} ->
      let%bind f' = untype_expression expr1 in
      let%bind arg' = untype_expression expr2 in
      return (e_application f' arg')
  | E_lambda {binder ; result} -> (
      let%bind io = get_t_function e.type_expression in
      let%bind (input_type , output_type) = bind_map_pair untype_type_expression io in
      let%bind result = untype_expression result in
      return (e_lambda (binder) (Some input_type) (Some output_type) result)
    )
  | E_constructor {constructor; element} ->
      let%bind p' = untype_expression element in
      let Constructor n = constructor in
      return (e_constructor n p')
  | E_record r ->
    let aux ( Label k ,v) = (k, v) in
    let r = Map.String.of_list @@ List.map aux (LMap.to_kv_list r) in
    let%bind r' = bind_smap
      @@ Map.String.map untype_expression r in
    return (e_record r')
  | E_record_accessor {expr; label} ->
      let%bind r' = untype_expression expr in
      let Label s = label in
      return (e_accessor r' s)
  | E_record_update {record=r; path=l; update=e} ->
    let%bind r' = untype_expression r in
    let%bind e = untype_expression e in 
    let Label l = l in
    return (e_update r' l e)
  | E_map m ->
      let%bind m' = bind_map_list (bind_map_pair untype_expression) m in
      return (e_map m')
  | E_big_map m ->
      let%bind m' = bind_map_list (bind_map_pair untype_expression) m in
      return (e_big_map m')
  | E_list lst ->
      let%bind lst' = bind_map_list untype_expression lst in
      return (e_list lst')
  | E_set lst ->
      let%bind lst' = bind_map_list untype_expression lst in
      return (e_set lst')
  | E_look_up dsi ->
      let%bind (a , b) = bind_map_pair untype_expression dsi in
      return (e_look_up a b)
  | E_matching {matchee;cases} ->
      let%bind ae' = untype_expression matchee in
      let%bind m' = untype_matching untype_expression cases in
      return (e_matching ae' m')
  | E_let_in {let_binder;rhs;let_result; inline} ->
      let%bind tv = untype_type_expression rhs.type_expression in
      let%bind rhs = untype_expression rhs in
      let%bind result = untype_expression let_result in
      return (I.e_let_in (let_binder , (Some tv)) false inline rhs result)

and untype_matching : (O.expression -> I.expression result) -> O.matching_expr -> I.matching_expr result = fun f m ->
  let open I in
  match m with
  | Match_bool {match_true ; match_false} ->
      let%bind match_true = f match_true in
      let%bind match_false = f match_false in
      ok @@ Match_bool {match_true ; match_false}
  | Match_tuple ((lst, b),_) ->
      let%bind b = f b in
      ok @@ I.Match_tuple ((lst, b),[])
  | Match_option {match_none ; match_some = (v, some,_)} ->
      let%bind match_none = f match_none in
      let%bind some = f some in
      let match_some = v, some, () in
      ok @@ Match_option {match_none ; match_some}
  | Match_list {match_nil ; match_cons = (hd_name, tl_name, cons,_)} ->
      let%bind match_nil = f match_nil in
      let%bind cons = f cons in
      let match_cons = hd_name , tl_name , cons, () in
      ok @@ Match_list {match_nil ; match_cons}
  | Match_variant (lst , _) ->
      let aux ((a,b),c) =
        let%bind c' = f c in
        ok ((a,b),c') in
      let%bind lst' = bind_map_list aux lst in
      ok @@ Match_variant (lst',())
