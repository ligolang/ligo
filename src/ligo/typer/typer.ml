open Trace

module I = Ast_simplified
module O = Ast_typed
open O.Combinators

module SMap = O.SMap

module Environment = O.Environment

type environment = Environment.t

module Errors = struct
  let unbound_type_variable (e:environment) (n:string) () =
    let title = (thunk "unbound type variable") in
    let full () = Format.asprintf "%s in %a" n Environment.PP.full_environment e in
    error title full ()

  let unbound_variable (e:environment) (n:string) () =
    let title = (thunk "unbound variable") in
    let full () = Format.asprintf "%s in %a" n Environment.PP.full_environment e in
    error title full ()

  let unrecognized_constant (n:string) () =
    let title = (thunk "unrecognized constant") in
    let full () = n in
    error title full ()

  let wrong_arity (n:string) (expected:int) (actual:int) () =
    let title () = "wrong arity" in
    let full () =
      Format.asprintf "Wrong number of args passed to [%s]. Expected was %d, received was %d."
        n expected actual
    in
    error title full ()

  let program_error (p:I.program) () =
    let title = (thunk "typing program") in
    let full () = Format.asprintf "%a" I.PP.program p in
    error title full ()

  let constant_declaration_error (name:string) (ae:I.ae) () =
    let title = (thunk "typing constant declaration") in
    let full () =
      Format.asprintf "%s = %a" name
        I.PP.annotated_expression ae
    in
    error title full ()

end
open Errors

let rec type_program (p:I.program) : O.program result =
  let aux (e, acc:(environment * O.declaration Location.wrap list)) (d:I.declaration Location.wrap) =
    let%bind ed' = (bind_map_location (type_declaration e)) d in
    let loc : 'a . 'a Location.wrap -> _ -> _ = fun x v -> Location.wrap ~loc:x.location v in
    let (e', d') = Location.unwrap ed' in
    match d' with
    | None -> ok (e', acc)
    | Some d' -> ok (e', loc ed' d' :: acc)
  in
  let%bind (_, lst) =
    trace (fun () -> program_error p ()) @@
    bind_fold_list aux (Environment.full_empty, []) p in
  ok @@ List.rev lst

and type_declaration env : I.declaration -> (environment * O.declaration option) result = function
  | Declaration_type {type_name;type_expression} ->
      let%bind tv = evaluate_type env type_expression in
      let env' = Environment.add_type type_name tv env in
      ok (env', None)
  | Declaration_constant {name;annotated_expression} ->
      let%bind ae' =
        trace (constant_declaration_error name annotated_expression) @@
        type_annotated_expression env annotated_expression in
      let env' = Environment.add_ez_declaration name (O.get_type_annotation ae') (O.get_expression ae') env in
      ok (env', Some (O.Declaration_constant ((make_n_e name ae') , env')))

and type_block_full (e:environment) (b:I.block) : (O.block * environment) result =
  let aux (e, acc:(environment * O.instruction list)) (i:I.instruction) =
    let%bind (e', i') = type_instruction e i in
    ok (e', i' @ acc)
  in
  let%bind (e', lst) = bind_fold_list aux (e, []) b in
  ok @@ (List.rev lst, e')

and type_block (e:environment) (b:I.block) : O.block result =
  let%bind (block, _) = type_block_full e b in
  ok block

and type_instruction (e:environment) : I.instruction -> (environment * O.instruction list) result = fun i ->
  let return x = ok (e, [x]) in
  match i with
  | I_skip -> return O.I_skip
  | I_do x ->
      let%bind expression = type_annotated_expression e x in
      let%bind () =
        trace_strong (simple_error "do without unit") @@
        Ast_typed.assert_type_value_eq (get_type_annotation expression , t_unit ()) in
      return @@ O.I_do expression
  | I_loop (cond, body) ->
      let%bind cond = type_annotated_expression e cond in
      let%bind _ =
        O.assert_type_value_eq (cond.type_annotation, t_bool ()) in
      let%bind body = type_block e body in
      return @@ O.I_loop (cond, body)
  | I_assignment {name;annotated_expression} -> (
      match annotated_expression.type_annotation, Environment.get_opt name e with
      | None, None -> simple_fail "Initial assignments need type annotation"
      | Some _, None ->
          let%bind annotated_expression = type_annotated_expression e annotated_expression in
          let e' = Environment.add_ez_ae name annotated_expression e in
          ok (e', [O.I_declaration (make_n_e name annotated_expression)])
      | None, Some prev ->
          let%bind annotated_expression = type_annotated_expression e annotated_expression in
          let%bind _ =
            O.assert_type_value_eq (annotated_expression.type_annotation, prev.type_value) in
          ok (e, [O.I_assignment (make_n_e name annotated_expression)])
      | Some _, Some prev ->
          let%bind annotated_expression = type_annotated_expression e annotated_expression in
          let%bind _assert = trace (simple_error "Annotation doesn't match environment")
            @@ O.assert_type_value_eq (annotated_expression.type_annotation, prev.type_value) in
          let e' = Environment.add_ez_ae name annotated_expression e in
          ok (e', [O.I_assignment (make_n_e name annotated_expression)])
    )
  | I_matching (ex, m) -> (
      let%bind ex' = type_annotated_expression e ex in
      match m with
      (* Special case for assert-like failwiths. TODO: CLEAN THIS. *)
      | I.Match_bool { match_false = [] ; match_true = [ I_do { expression = E_failwith fw } ] }
      | I.Match_bool { match_false = [ I_skip ] ; match_true = [ I_do { expression = E_failwith fw } ] } -> (
          let%bind fw' = type_annotated_expression e fw in
          let%bind () =
            trace_strong (simple_error "Matching bool on not-a-bool")
            @@ assert_t_bool (get_type_annotation ex') in
          let assert_ = make_a_e
              (E_constant ("ASSERT" , [ex' ; fw']))
              (t_unit ())
              e
          in
          return (O.I_do assert_)
        )
      | _ -> (
          let%bind m' = type_match type_block e ex'.type_annotation m in
          return @@ O.I_matching (ex', m')
        )
    )
  | I_record_patch (r, path, lst) ->
      let aux (s, ae) =
        let%bind ae' = type_annotated_expression e ae in
        let%bind ty =
          trace_option (simple_error "unbound variable in record_patch") @@
          Environment.get_opt r e in
        let tv = O.{type_name = r ; type_value = ty.type_value} in
        let aux ty access =
          match access with
          | I.Access_record s ->
              let%bind m = O.Combinators.get_t_record ty in
              let%bind ty =
                trace_option (simple_error "unbound record access in record_patch") @@
                Map.String.find_opt s m in
              ok (ty , O.Access_record s)
          | I.Access_tuple i ->
              let%bind t = O.Combinators.get_t_tuple ty in
              let%bind ty =
                generic_try (simple_error "unbound tuple access in record_patch") @@
                (fun () -> List.nth t i) in
              ok (ty , O.Access_tuple i)
          | I.Access_map ind ->
              let%bind (k , v) = O.Combinators.get_t_map ty in
              let%bind ind' = type_annotated_expression e ind in
              let%bind () = Ast_typed.assert_type_value_eq (get_type_annotation ind' ,  k) in
              ok (v , O.Access_map ind')
        in
        let%bind path' = bind_fold_map_list aux ty.type_value (path @ [Access_record s]) in
        ok @@ O.I_patch (tv, path', ae')
      in
      let%bind lst' = bind_map_list aux lst in
      ok (e, lst')
  | I_tuple_patch (r, path, lst) ->
      let aux (s, ae) =
        let%bind ae' = type_annotated_expression e ae in
        let%bind ty =
          trace_option (simple_error "unbound variable in tuple_patch") @@
          Environment.get_opt r e in
        let tv = O.{type_name = r ; type_value = ty.type_value} in
        let aux ty access =
          match access with
          | I.Access_record s ->
              let%bind m = O.Combinators.get_t_record ty in
              let%bind ty =
                trace_option (simple_error "unbound record access in tuple_patch") @@
                Map.String.find_opt s m in
              ok (ty , O.Access_record s)
          | I.Access_tuple i ->
              let%bind t = O.Combinators.get_t_tuple ty in
              let%bind ty =
                generic_try (simple_error "unbound tuple access in tuple_patch") @@
                (fun () -> List.nth t i) in
              ok (ty , O.Access_tuple i)
          | I.Access_map ind ->
              let%bind (k , v) = O.Combinators.get_t_map ty in
              let%bind ind' = type_annotated_expression e ind in
              let%bind () = Ast_typed.assert_type_value_eq (get_type_annotation ind' ,  k) in
              ok (v , O.Access_map ind')
        in
        let%bind path' = bind_fold_map_list aux ty.type_value (path @ [Access_tuple s]) in
        ok @@ O.I_patch (tv, path', ae')
      in
      let%bind lst' = bind_map_list aux lst in
      ok (e, lst')


and type_match : type i o . (environment -> i -> o result) -> environment -> O.type_value -> i I.matching -> o O.matching result =
  fun f e t i -> match i with
  | Match_bool {match_true ; match_false} ->
      let%bind _ =
        trace_strong (simple_error "Matching bool on not-a-bool")
        @@ get_t_bool t in
      let%bind match_true = f e match_true in
      let%bind match_false = f e match_false in
      ok (O.Match_bool {match_true ; match_false})
  | Match_option {match_none ; match_some} ->
      let%bind t_opt =
        trace_strong (simple_error "Matching option on not-an-option")
        @@ get_t_option t in
      let%bind match_none = f e match_none in
      let (n, b) = match_some in
      let n' = n, t_opt in
      let e' = Environment.add_ez_binder n t_opt e in
      let%bind b' = f e' b in
      ok (O.Match_option {match_none ; match_some = (n', b')})
  | Match_list {match_nil ; match_cons} ->
      let%bind t_list =
        trace_strong (simple_error "Matching list on not-an-list")
        @@ get_t_list t in
      let%bind match_nil = f e match_nil in
      let (hd, tl, b) = match_cons in
      let e' = Environment.add_ez_binder hd t_list e in
      let e' = Environment.add_ez_binder tl t e' in
      let%bind b' = f e' b in
      ok (O.Match_list {match_nil ; match_cons = (hd, tl, b')})
  | Match_tuple (lst, b) ->
      let%bind t_tuple =
        trace_strong (simple_error "Matching tuple on not-a-tuple")
        @@ get_t_tuple t in
      let%bind lst' =
        generic_try (simple_error "Matching tuple of different size")
        @@ (fun () -> List.combine lst t_tuple) in
      let aux prev (name, tv) = Environment.add_ez_binder name tv prev in
      let e' = List.fold_left aux e lst' in
      let%bind b' = f e' b in
      ok (O.Match_tuple (lst, b'))
  | Match_variant lst ->
      let%bind variant_opt =
        let aux acc ((constructor_name , _) , _) =
          let%bind (_ , variant) =
            trace_option (simple_error "bad constructor") @@
            Environment.get_constructor constructor_name e in
          let%bind acc = match acc with
            | None -> ok (Some variant)
            | Some variant' -> (
                Ast_typed.assert_type_value_eq (variant , variant') >>? fun () ->
                ok (Some variant)
              ) in
          ok acc in
        trace (simple_error "in match variant") @@
        bind_fold_list aux None lst in
      let%bind variant =
        trace_option (simple_error "empty variant") @@
        variant_opt in
      let%bind  () =
        let%bind variant_cases' = Ast_typed.Combinators.get_t_sum variant in
        let variant_cases = List.map fst @@ Map.String.to_kv_list variant_cases' in
        let match_cases = List.map (Function.compose fst fst) lst in
        let test_case = fun c ->
          Assert.assert_true (List.mem c match_cases)
        in
        let%bind () =
          trace (simple_error "missing case match") @@
          bind_iter_list test_case variant_cases in
        let%bind () =
          trace_strong (simple_error "redundant case match") @@
          Assert.assert_true List.(length variant_cases = length match_cases) in
        ok ()
      in
      let%bind lst' =
        let aux ((constructor_name , name) , b) =
          let%bind (constructor , _) =
            trace_option (simple_error "bad constructor??") @@
            Environment.get_constructor constructor_name e in
          let e' = Environment.add_ez_binder name constructor e in
          let%bind b' = f e' b in
          ok ((constructor_name , name) , b')
        in
        bind_map_list aux lst in
      ok (O.Match_variant (lst' , variant))

and evaluate_type (e:environment) (t:I.type_expression) : O.type_value result =
  let return tv' = ok (make_t tv' (Some t)) in
  match t with
  | T_function (a, b) ->
      let%bind a' = evaluate_type e a in
      let%bind b' = evaluate_type e b in
      return (T_function (a', b'))
  | T_tuple lst ->
      let%bind lst' = bind_list @@ List.map (evaluate_type e) lst in
      return (T_tuple lst')
  | T_sum m ->
      let aux k v prev =
        let%bind prev' = prev in
        let%bind v' = evaluate_type e v in
        ok @@ SMap.add k v' prev'
      in
      let%bind m = SMap.fold aux m (ok SMap.empty) in
      return (T_sum m)
  | T_record m ->
      let aux k v prev =
        let%bind prev' = prev in
        let%bind v' = evaluate_type e v in
        ok @@ SMap.add k v' prev'
      in
      let%bind m = SMap.fold aux m (ok SMap.empty) in
      return (T_record m)
  | T_variable name ->
      let%bind tv =
        trace_option (unbound_type_variable e name)
        @@ Environment.get_type_opt name e in
      ok tv
  | T_constant (cst, lst) ->
      let%bind lst' = bind_list @@ List.map (evaluate_type e) lst in
      return (T_constant(cst, lst'))

and type_annotated_expression : environment -> I.annotated_expression -> O.annotated_expression result = fun e ae ->
  let%bind tv_opt = match ae.type_annotation with
    | None -> ok None
    | Some s -> let%bind r = evaluate_type e s in ok (Some r) in
  let check tv = O.(merge_annotation tv_opt (Some tv)) in
  let return expr tv =
    let%bind type_annotation = check tv in
    ok @@ make_a_e expr type_annotation e in
  let main_error =
    let title () = "typing annotated_expression" in
    let content () = Format.asprintf "%a" I.PP.annotated_expression ae in
    error title content in
  trace main_error @@
  match ae.expression with
  (* Basic *)
  | E_failwith _ -> simple_fail "can't type failwith in isolation"
  | E_variable name ->
      let%bind tv' =
        trace_option (unbound_variable e name)
        @@ Environment.get_opt name e in
      return (E_variable name) tv'.type_value
  | E_literal (Literal_bool b) ->
      return (E_literal (Literal_bool b)) (t_bool ())
  | E_literal Literal_unit ->
      return (E_literal (Literal_unit)) (t_unit ())
  | E_literal (Literal_string s) ->
      return (E_literal (Literal_string s)) (t_string ())
  | E_literal (Literal_bytes s) ->
      return (E_literal (Literal_bytes s)) (t_bytes ())
  | E_literal (Literal_int n) ->
      return (E_literal (Literal_int n)) (t_int ())
  | E_literal (Literal_nat n) ->
      return (E_literal (Literal_nat n)) (t_nat ())
  | E_literal (Literal_tez n) ->
      return (E_literal (Literal_tez n)) (t_tez ())
  | E_literal (Literal_address s) ->
      return (e_address s) (t_address ())
  (* Tuple *)
  | E_tuple lst ->
      let%bind lst' = bind_list @@ List.map (type_annotated_expression e) lst in
      let tv_lst = List.map get_type_annotation lst' in
      return (E_tuple lst') (t_tuple tv_lst ())
  | E_accessor (ae, path) ->
      let%bind e' = type_annotated_expression e ae in
      let aux (prev:O.annotated_expression) (a:I.access) : O.annotated_expression result =
        match a with
        | Access_tuple index -> (
            let%bind tpl_tv = get_t_tuple prev.type_annotation in
            let%bind tv =
              generic_try (simple_error "bad tuple index")
              @@ (fun () -> List.nth tpl_tv index) in
            return (E_tuple_accessor (prev , index)) tv
          )
        | Access_record property -> (
            let%bind r_tv = get_t_record prev.type_annotation in
            let%bind tv =
              generic_try (simple_error "bad record index")
              @@ (fun () -> SMap.find property r_tv) in
            return (E_record_accessor (prev , property)) tv
          )
        | Access_map ae -> (
            let%bind ae' = type_annotated_expression e ae in
            let%bind (k , v) = get_t_map prev.type_annotation in
            let%bind () =
              Ast_typed.assert_type_value_eq (k , get_type_annotation ae') in
            return (E_look_up (prev , ae')) v
          )
      in
      trace (simple_error "accessing") @@
      bind_fold_list aux e' path

  (* Sum *)
  | E_constructor (c, expr) ->
      let%bind (c_tv, sum_tv) =
        let error =
          let title () = "no such constructor" in
          let content () =
            Format.asprintf "%s in:\n%a\n"
              c O.Environment.PP.full_environment e
          in
          error title content in
        trace_option error @@
        Environment.get_constructor c e in
      let%bind expr' = type_annotated_expression e expr in
      let%bind _assert = O.assert_type_value_eq (expr'.type_annotation, c_tv) in
      return (E_constructor (c , expr')) sum_tv
  (* Record *)
  | E_record m ->
      let aux prev k expr =
        let%bind expr' = type_annotated_expression e expr in
        ok (SMap.add k expr' prev)
      in
      let%bind m' = bind_fold_smap aux (ok SMap.empty) m in
      return (E_record m') (t_record (SMap.map get_type_annotation m') ())
  (* Data-structure *)
  | E_list lst ->
      let%bind lst' = bind_map_list (type_annotated_expression e) lst in
      let%bind tv =
        let aux opt c =
          match opt with
          | None -> ok (Some c)
          | Some c' ->
              let%bind _eq = Ast_typed.assert_type_value_eq (c, c') in
              ok (Some c') in
        let%bind init = match tv_opt with
          | None -> ok None
          | Some ty ->
              let%bind ty' = get_t_list ty in
              ok (Some ty') in
        let%bind ty =
          let%bind opt = bind_fold_list aux init
          @@ List.map get_type_annotation lst' in
          trace_option (simple_error "empty list expression without annotation") opt in
        ok (t_list ty ())
      in
      return (E_list lst') tv
  | E_map lst ->
      let%bind lst' = bind_map_list (bind_map_pair (type_annotated_expression e)) lst in
      let%bind tv =
        let aux opt c =
          match opt with
          | None -> ok (Some c)
          | Some c' ->
              let%bind _eq = Ast_typed.assert_type_value_eq (c, c') in
              ok (Some c') in
        let%bind key_type =
          let%bind opt =
            bind_fold_list aux None
            @@ List.map get_type_annotation
            @@ List.map fst lst' in
          trace_option (simple_error "empty map expression") opt
        in
        let%bind value_type =
          let%bind opt =
            bind_fold_list aux None
            @@ List.map get_type_annotation
            @@ List.map snd lst' in
          trace_option (simple_error "empty map expression") opt
        in
        ok (t_map key_type value_type ())
      in
      return (E_map lst') tv
  | E_lambda {
      binder ;
      input_type ;
      output_type ;
      result ;
      body ;
    } ->
      let%bind input_type = evaluate_type e input_type in
      let%bind output_type = evaluate_type e output_type in
      let e' = Environment.add_ez_binder binder input_type e in
      let%bind (body, e'') = type_block_full e' body in
      let%bind result = type_annotated_expression e'' result in
      return (E_lambda {binder;input_type;output_type;result;body}) (t_function input_type output_type ())
  | E_constant (name, lst) ->
      let%bind lst' = bind_list @@ List.map (type_annotated_expression e) lst in
      let tv_lst = List.map get_type_annotation lst' in
      let%bind (name', tv) = type_constant name tv_lst tv_opt in
      return (E_constant (name' , lst')) tv
  | E_application (f, arg) ->
      let%bind f = type_annotated_expression e f in
      let%bind arg = type_annotated_expression e arg in
      let%bind tv = match f.type_annotation.type_value' with
        | T_function (param, result) ->
            let%bind _ = O.assert_type_value_eq (param, arg.type_annotation) in
            ok result
        | _ -> simple_fail "applying to not-a-function"
      in
      return (E_application (f , arg)) tv
  | E_look_up dsi ->
      let%bind (ds, ind) = bind_map_pair (type_annotated_expression e) dsi in
      let%bind (src, dst) = get_t_map ds.type_annotation in
      let%bind _ = O.assert_type_value_eq (ind.type_annotation, src) in
      return (E_look_up (ds , ind)) (t_option dst ())
  (* Advanced *)
  | E_matching (ex, m) -> (
      let%bind ex' = type_annotated_expression e ex in
      match m with
      (* Special case for assert-like failwiths. TODO: CLEAN THIS. *)
      | I.Match_bool { match_false ; match_true = { expression = E_failwith fw } } -> (
          let%bind fw' = type_annotated_expression e fw in
          let%bind mf' = type_annotated_expression e match_false in
          let%bind () =
            trace_strong (simple_error "Matching bool on not-a-bool")
            @@ assert_t_bool (get_type_annotation ex') in
          let%bind () =
            trace_strong (simple_error "Matching not-unit on an assert")
            @@ assert_t_unit (get_type_annotation mf') in
          let mt' = make_a_e
              (E_failwith fw')
              (t_unit ())
              e
          in
          let m' = O.Match_bool { match_true = mt' ; match_false = mf' } in
          return (O.E_matching (ex' , m')) (t_unit ())
        )
      | _ -> (
          let%bind m' = type_match type_annotated_expression e ex'.type_annotation m in
          let tvs =
            let aux (cur:O.value O.matching) =
              match cur with
              | Match_bool { match_true ; match_false } -> [ match_true ; match_false ]
              | Match_list { match_nil ; match_cons = (_ , _ , match_cons) } -> [ match_nil ; match_cons ]
              | Match_option { match_none ; match_some = (_ , match_some) } -> [ match_none ; match_some ]
              | Match_tuple (_ , match_tuple) -> [ match_tuple ]
              | Match_variant (lst , _) -> List.map snd lst in
            List.map get_type_annotation @@ aux m' in
          let aux prec cur =
            let%bind () =
              match prec with
              | None -> ok ()
              | Some cur' -> Ast_typed.assert_type_value_eq (cur , cur') in
            ok (Some cur) in
          let%bind tv_opt = bind_fold_list aux None tvs in
          let%bind tv =
            trace_option (simple_error "empty matching") @@
            tv_opt in
          return (O.E_matching (ex', m')) tv
        )
    )

and type_constant (name:string) (lst:O.type_value list) (tv_opt:O.type_value option) : (string * O.type_value) result =
  (* Constant poorman's polymorphism *)
  let ct = Operators.Typer.constant_typers in
  let%bind v =
    trace_option (unrecognized_constant name) @@
    Map.String.find_opt name ct in
  let (arity, typer) = v in
  let%bind () =
    let l = List.length lst in
    trace_strong (wrong_arity name arity l) @@
    Assert.assert_true (arity = l) in

  let error =
    let title () = "typing: constant predicates all failed" in
    let content () =
      Format.asprintf "%s in %a"
      name
      PP_helpers.(list_sep Ast_typed.PP.type_value (const " , "))
      lst
    in
    error title content in
  let rec aux = fun ts ->
    match ts with
    | [] -> fail error
    | (predicate, typer') :: tl -> (
        match predicate lst with
        | false -> aux tl
        | true -> typer' lst tv_opt
      )
  in
  aux typer

let untype_type_value (t:O.type_value) : (I.type_expression) result =
  match t.simplified with
  | Some s -> ok s
  | _ -> simple_fail "trying to untype generated type"

let untype_literal (l:O.literal) : I.literal result =
  let open I in
  match l with
  | Literal_unit -> ok Literal_unit
  | Literal_bool b -> ok (Literal_bool b)
  | Literal_nat n -> ok (Literal_nat n)
  | Literal_tez n -> ok (Literal_tez n)
  | Literal_int n -> ok (Literal_int n)
  | Literal_string s -> ok (Literal_string s)
  | Literal_bytes b -> ok (Literal_bytes b)
  | Literal_address s -> ok (Literal_address s)

let rec untype_annotated_expression (e:O.annotated_expression) : (I.annotated_expression) result =
  let open I in
  let type_annotation = e.type_annotation.simplified in
  let return e = ok @@ I.Combinators.make_e_a ?type_annotation e in
  match e.expression with
  | E_literal l ->
      let%bind l = untype_literal l in
      return (E_literal l)
  | E_constant (n, lst) ->
      let%bind lst' = bind_list
        @@ List.map untype_annotated_expression lst in
      return (E_constant (n, lst'))
  | E_variable n ->
      return (E_variable n)
  | E_application (f, arg) ->
      let%bind f' = untype_annotated_expression f in
      let%bind arg' = untype_annotated_expression arg in
      return (E_application (f', arg'))
  | E_lambda {binder;input_type;output_type;body;result} ->
      let%bind input_type = untype_type_value input_type in
      let%bind output_type = untype_type_value output_type in
      let%bind result = untype_annotated_expression result in
      let%bind body = untype_block body in
      return (E_lambda {binder;input_type;output_type;body;result})
  | E_tuple lst ->
      let%bind lst' = bind_list
        @@ List.map untype_annotated_expression lst in
      return (E_tuple lst')
  | E_tuple_accessor (tpl, ind)  ->
      let%bind tpl' = untype_annotated_expression tpl in
      return (E_accessor (tpl', [Access_tuple ind]))
  | E_constructor (n, p) ->
      let%bind p' = untype_annotated_expression p in
      return (E_constructor (n, p'))
  | E_record r ->
      let%bind r' = bind_smap
        @@ SMap.map untype_annotated_expression r in
      return (E_record r')
  | E_record_accessor (r, s) ->
      let%bind r' = untype_annotated_expression r in
      return (E_accessor (r', [Access_record s]))
  | E_map m ->
      let%bind m' = bind_map_list (bind_map_pair untype_annotated_expression) m in
      return (E_map m')
  | E_list lst ->
      let%bind lst' = bind_map_list untype_annotated_expression lst in
      return (E_list lst')
  | E_look_up dsi ->
      let%bind dsi' = bind_map_pair untype_annotated_expression dsi in
      return (E_look_up dsi')
  | E_matching (ae, m) ->
      let%bind ae' = untype_annotated_expression ae in
      let%bind m' = untype_matching untype_annotated_expression m in
      return (E_matching (ae', m'))
  | E_failwith ae ->
      let%bind ae' = untype_annotated_expression ae in
      return (E_failwith ae')

and untype_block (b:O.block) : (I.block) result =
  bind_list @@ List.map untype_instruction b

and untype_instruction (i:O.instruction) : (I.instruction) result =
  let open I in
  match i with
  | I_skip -> ok I_skip
  | I_do e ->
      let%bind e' = untype_annotated_expression e in
      ok (I_do e')
  | I_loop (e, b) ->
      let%bind e' = untype_annotated_expression e in
      let%bind b' = untype_block b in
      ok @@ I_loop (e', b')
  | I_declaration a ->
      let%bind annotated_expression = untype_annotated_expression a.annotated_expression in
      ok @@ I_assignment {name = a.name ; annotated_expression}
  | I_assignment a ->
      let%bind annotated_expression = untype_annotated_expression a.annotated_expression in
      ok @@ I_assignment {name = a.name ; annotated_expression}
  | I_matching (e, m) ->
      let%bind e' = untype_annotated_expression e in
      let%bind m' = untype_matching untype_block m in
      ok @@ I_matching (e', m')
  | I_patch (s, p, e) ->
      let%bind e' = untype_annotated_expression e in
      let%bind (hds, tl) =
        trace_option (simple_error "patch without path") @@
        List.rev_uncons_opt p in
      let%bind tl_name = match tl with
        | Access_record n -> ok n
        | Access_tuple _ -> simple_fail "last element of patch is tuple"
        | Access_map _ -> simple_fail "last element of patch is map"
      in
      let%bind hds' = bind_map_list untype_access hds in
      ok @@ I_record_patch (s.type_name, hds', [tl_name, e'])

and untype_access (a:O.access) : I.access result =
  match a with
  | Access_record n -> ok @@ I.Access_record n
  | Access_tuple n -> ok @@ I.Access_tuple n
  | Access_map n ->
      let%bind n' = untype_annotated_expression n in
      ok @@ I.Access_map n'

and untype_matching : type o i . (o -> i result) -> o O.matching -> (i I.matching) result = fun f m ->
  let open I in
  match m with
  | Match_bool {match_true ; match_false} ->
      let%bind match_true = f match_true in
      let%bind match_false = f match_false in
      ok @@ Match_bool {match_true ; match_false}
  | Match_tuple (lst, b) ->
      let%bind b = f b in
      ok @@ Match_tuple (lst, b)
  | Match_option {match_none ; match_some = (v, some)} ->
      let%bind match_none = f match_none in
      let%bind some = f some in
      let match_some = fst v, some in
      ok @@ Match_option {match_none ; match_some}
  | Match_list {match_nil ; match_cons = (hd, tl, cons)} ->
      let%bind match_nil = f match_nil in
      let%bind cons = f cons in
      let match_cons = hd, tl, cons in
      ok @@ Match_list {match_nil ; match_cons}
  | Match_variant (lst , _) ->
      let aux ((a,b),c) =
        let%bind c' = f c in
        ok ((a,b),c') in
      let%bind lst' = bind_map_list aux lst in
      ok @@ Match_variant lst'
