open Trace

module I = Ast_simplified
module O = Ast_typed
open O.Combinators

module SMap = O.SMap

module Environment = struct
  type ele = O.type_value

  type t = {
    environment: (string * ele) list ;
    type_environment: (string * ele) list ;
  }
  let empty : t = {
    (*  TODO: use maps *)
    environment = [] ;
    type_environment = [] ;
  }

  let get (e:t) (s:string) : ele option =
    List.assoc_opt s e.environment
  let get_constructor (e:t) (s:string) : (ele * ele) option =
    let rec aux = function
      | [] -> None
      | (_, (O.{type_value'=(O.T_sum m)} as tv)) :: _ when SMap.mem s m -> Some (SMap.find s m, tv)
      | _ :: tl -> aux tl
    in
    aux e.environment

  let add (e:t) (s:string) (tv:ele) : t =
    {e with environment = (s, tv) :: e.environment}
  let get_type (e:t) (s:string) : ele option =
    List.assoc_opt s e.type_environment
  let add_type (e:t) (s:string) (tv:ele) : t =
    {e with type_environment = (s, tv) :: e.type_environment}

  module PP = struct
    open Format
    open PP_helpers

    let list_sep_d x = list_sep x (const " , ")

    let value ppf (e:t) =
      let pp ppf (s, e) = fprintf ppf "%s -> %a" s O.PP.type_value e in
      fprintf ppf "ValueEnv[%a]" (list_sep_d pp) e.environment

    let type_ ppf e =
      let pp ppf (s, e) = fprintf ppf "%s -> %a" s O.PP.type_value e in
      fprintf ppf "TypeEnv[%a]" (list_sep_d pp) e.type_environment

    let full ppf e =
      fprintf ppf "%a\n%a" value e type_ e
  end

  module Combinators = struct
    let env_sum_type ?(env = empty)
                     ?(name = "a_sum_type")
                     (lst : (string * ele) list) =
      add env name (make_t_ez_sum lst)
  end
end

type environment = Environment.t

module Errors = struct
  let unbound_type_variable (e:environment) (n:string) () =
    let title = (thunk "unbound type variable") in
    let full () = Format.asprintf "%s in %a" n Environment.PP.type_ e in
    error title full ()

  let unbound_variable (e:environment) (n:string) () =
    let title = (thunk "unbound variable") in
    let full () = Format.asprintf "%s in %a" n Environment.PP.value e in
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
    bind_fold_list aux (Environment.empty, []) p in
  ok @@ List.rev lst

and type_declaration env : I.declaration -> (environment * O.declaration option) result = function
  | Declaration_type {type_name;type_expression} ->
      let%bind tv = evaluate_type env type_expression in
      let env' = Environment.add_type env type_name tv in
      ok (env', None)
  | Declaration_constant {name;annotated_expression} ->
      let%bind ae' =
        trace (constant_declaration_error name annotated_expression) @@
        type_annotated_expression env annotated_expression in
      let env' = Environment.add env name ae'.type_annotation in
      ok (env', Some (O.Declaration_constant {name;annotated_expression=ae'}))

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
  | I_fail x ->
      let%bind expression = type_annotated_expression e x in
      return @@ O.I_fail expression
  | I_loop (cond, body) ->
      let%bind cond = type_annotated_expression e cond in
      let%bind _ =
        O.assert_type_value_eq (cond.type_annotation, t_bool ()) in
      let%bind body = type_block e body in
      return @@ O.I_loop (cond, body)
  | I_assignment {name;annotated_expression} -> (
      match annotated_expression.type_annotation, Environment.get e name with
      | None, None -> simple_fail "Initial assignments need type annotation"
      | Some _, None ->
          let%bind annotated_expression = type_annotated_expression e annotated_expression in
          let e' = Environment.add e name annotated_expression.type_annotation in
          ok (e', [O.I_declaration {name;annotated_expression}])
      | None, Some prev ->
          let%bind annotated_expression = type_annotated_expression e annotated_expression in
          let%bind _ =
            O.assert_type_value_eq (annotated_expression.type_annotation, prev) in
          ok (e, [O.I_assignment {name;annotated_expression}])
      | Some _, Some prev ->
          let%bind annotated_expression = type_annotated_expression e annotated_expression in
          let%bind _assert = trace (simple_error "Annotation doesn't match environment")
            @@ O.assert_type_value_eq (annotated_expression.type_annotation, prev) in
          let e' = Environment.add e name annotated_expression.type_annotation in
          ok (e', [O.I_assignment {name;annotated_expression}])
    )
  | I_matching (ex, m) ->
      let%bind ex' = type_annotated_expression e ex in
      let%bind m' = type_match type_block e ex'.type_annotation m in
      return @@ O.I_matching (ex', m')
  | I_record_patch (r, path, lst) ->
      let aux (s, ae) =
        let%bind ae' = type_annotated_expression e ae in
        let%bind ty =
          trace_option (simple_error "unbound variable in record_patch") @@
          Environment.get e r in
        let tv = O.{type_name = r ; type_value = ty} in
        let aux ty access =
          match access with
          | I.Access_record s ->
              let%bind m = O.Combinators.get_t_record ty in
              trace_option (simple_error "unbound record access in record_patch") @@
              Map.String.find_opt s m
          | Access_tuple i ->
              let%bind t = O.Combinators.get_t_tuple ty in
              generic_try (simple_error "unbound tuple access in record_patch") @@
              (fun () -> List.nth t i)
        in
        let%bind _assert = bind_fold_list aux ty (path @ [Access_record s]) in
        ok @@ O.I_patch (tv, path @ [Access_record s], ae')
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
      let e' = Environment.add e n t_opt in
      let%bind b' = f e' b in
      ok (O.Match_option {match_none ; match_some = (n', b')})
  | Match_list {match_nil ; match_cons} ->
      let%bind t_list =
        trace_strong (simple_error "Matching list on not-an-list")
        @@ get_t_list t in
      let%bind match_nil = f e match_nil in
      let (hd, tl, b) = match_cons in
      let e' = Environment.add e hd t_list in
      let e' = Environment.add e' tl t in
      let%bind b' = f e' b in
      ok (O.Match_list {match_nil ; match_cons = (hd, tl, b')})
  | Match_tuple (lst, b) ->
      let%bind t_tuple =
        trace_strong (simple_error "Matching tuple on not-a-tuple")
        @@ get_t_tuple t in
      let%bind lst' =
        generic_try (simple_error "Matching tuple of different size")
        @@ (fun () -> List.combine lst t_tuple) in
      let aux prev (name, tv) = Environment.add prev name tv in
      let e' = List.fold_left aux e lst' in
      let%bind b' = f e' b in
      ok (O.Match_tuple (lst, b'))

and evaluate_type (e:environment) (t:I.type_expression) : O.type_value result =
  let return tv' = ok O.(type_value tv' (Some t)) in
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
        @@ Environment.get_type e name in
      ok tv
  | T_constant (cst, lst) ->
      let%bind lst' = bind_list @@ List.map (evaluate_type e) lst in
      return (T_constant(cst, lst'))

and type_annotated_expression (e:environment) (ae:I.annotated_expression) : O.annotated_expression result =
  let%bind tv_opt = match ae.type_annotation with
    | None -> ok None
    | Some s -> let%bind r = evaluate_type e s in ok (Some r) in
  let check tv = O.(merge_annotation tv_opt (Some tv)) in
  match ae.expression with
  (* Basic *)
  | E_variable name ->
      let%bind tv' =
        trace_option (unbound_variable e name)
        @@ Environment.get e name in
      let%bind type_annotation = check tv' in
      ok O.{expression = E_variable name ; type_annotation}
  | E_literal (Literal_bool b) ->
      let%bind type_annotation = check (t_bool ()) in
      ok O.{expression = E_literal (Literal_bool b) ; type_annotation }
  | E_literal Literal_unit ->
      let%bind type_annotation = check (t_unit ()) in
      ok O.{expression = E_literal (Literal_unit) ; type_annotation }
  | E_literal (Literal_string s) ->
      let%bind type_annotation = check (t_string ()) in
      ok O.{expression = E_literal (Literal_string s) ; type_annotation }
  | E_literal (Literal_bytes s) ->
      let%bind type_annotation = check (t_bytes ()) in
      ok O.{expression = E_literal (Literal_bytes s) ; type_annotation }
  | E_literal (Literal_int n) ->
      let%bind type_annotation = check (t_int ()) in
      ok O.{expression = E_literal (Literal_int n) ; type_annotation }
  | E_literal (Literal_nat n) ->
      let%bind type_annotation = check (t_nat ()) in
      ok O.{expression = E_literal (Literal_nat n) ; type_annotation }
  (* Tuple *)
  | E_tuple lst ->
      let%bind lst' = bind_list @@ List.map (type_annotated_expression e) lst in
      let tv_lst = List.map get_annotation lst' in
      let%bind type_annotation = check (t_tuple tv_lst ()) in
      ok O.{expression = E_tuple lst' ; type_annotation }
  | E_accessor (ae, path) ->
      let%bind e' = type_annotated_expression e ae in
      let aux (prev:O.annotated_expression) (a:I.access) : O.annotated_expression result =
        match a with
        | Access_tuple index -> (
            let%bind tpl_tv = get_t_tuple prev.type_annotation in
            let%bind tv =
              generic_try (simple_error "bad tuple index")
              @@ (fun () -> List.nth tpl_tv index) in
            let%bind type_annotation = check tv in
            let annotated_expression = O.{expression = E_tuple_accessor (prev, index) ; type_annotation} in
            ok annotated_expression
          )
        | Access_record property -> (
            let%bind r_tv = get_t_record prev.type_annotation in
            let%bind tv =
              generic_try (simple_error "bad record index")
              @@ (fun () -> SMap.find property r_tv) in
            let%bind type_annotation = check tv in
            ok O.{expression = O.E_record_accessor (prev, property) ; type_annotation }
          )
      in
      trace (simple_error "accessing") @@
      bind_fold_list aux e' path

  (* Sum *)
  | E_constructor (c, expr) ->
      let%bind (c_tv, sum_tv) =
        trace_option (simple_error "no such constructor")
        @@ Environment.get_constructor e c in
      let%bind expr' = type_annotated_expression e expr in
      let%bind _assert = O.assert_type_value_eq (expr'.type_annotation, c_tv) in
      let%bind type_annotation = check sum_tv in
      ok O.{expression = O.E_constructor(c, expr') ; type_annotation }
  (* Record *)
  | E_record m ->
      let aux prev k expr =
        let%bind expr' = type_annotated_expression e expr in
        ok (SMap.add k expr' prev)
      in
      let%bind m' = bind_fold_smap aux (ok SMap.empty) m in
      let%bind type_annotation = check @@ t_record (SMap.map get_annotation m') () in
      ok O.{expression = O.E_record m' ; type_annotation }
  (* Data-structure *)
  | E_list lst ->
      let%bind lst' = bind_map_list (type_annotated_expression e) lst in
      let%bind type_annotation =
        let aux opt c =
          match opt with
          | None -> ok (Some c)
          | Some c' ->
              let%bind _eq = Ast_typed.assert_type_value_eq (c, c') in
              ok (Some c') in
        let%bind init = match tv_opt with
          | None -> ok None
          | Some ty ->
              let%bind ty' = Ast_typed.Combinators.get_t_list ty in
              ok (Some ty') in
        let%bind ty =
          let%bind opt = bind_fold_list aux init
          @@ List.map Ast_typed.get_type_annotation lst' in
          trace_option (simple_error "empty list expression without annotation") opt in
        check (t_list ty ())
      in
      ok O.{expression = O.E_list lst' ; type_annotation}
  | E_map lst ->
      let%bind lst' = bind_map_list (bind_map_pair (type_annotated_expression e)) lst in
      let%bind type_annotation =
        let aux opt c =
          match opt with
          | None -> ok (Some c)
          | Some c' ->
              let%bind _eq = Ast_typed.assert_type_value_eq (c, c') in
              ok (Some c') in
        let%bind key_type =
          let%bind opt =
            bind_fold_list aux None
            @@ List.map Ast_typed.get_type_annotation
            @@ List.map fst lst' in
          trace_option (simple_error "empty map expression") opt
        in
        let%bind value_type =
          let%bind opt =
            bind_fold_list aux None
            @@ List.map Ast_typed.get_type_annotation
            @@ List.map snd lst' in
          trace_option (simple_error "empty map expression") opt
        in
        check (t_map key_type value_type ())
      in
      ok O.{expression = O.E_map lst' ; type_annotation}
  | E_lambda {
      binder ;
      input_type ;
      output_type ;
      result ;
      body ;
    } ->
      let%bind input_type = evaluate_type e input_type in
      let%bind output_type = evaluate_type e output_type in
      let e' = Environment.add e binder input_type in
      let%bind (body, e'') = type_block_full e' body in
      let%bind result = type_annotated_expression e'' result in
      let%bind type_annotation = check @@ (t_function input_type output_type ()) in
      ok O.{expression = E_lambda {binder;input_type;output_type;result;body} ; type_annotation}
  | E_constant (name, lst) ->
      let%bind lst' = bind_list @@ List.map (type_annotated_expression e) lst in
      let tv_lst = List.map get_annotation lst' in
      let%bind (name', tv) = type_constant name tv_lst tv_opt in
      let%bind type_annotation = check tv in
      ok O.{expression = O.E_constant (name', lst') ; type_annotation}
  | E_application (f, arg) ->
      let%bind f = type_annotated_expression e f in
      let%bind arg = type_annotated_expression e arg in
      let%bind type_annotation = match f.type_annotation.type_value' with
        | T_function (param, result) ->
            let%bind _ = O.assert_type_value_eq (param, arg.type_annotation) in
            ok result
        | _ -> simple_fail "applying to not-a-function"
      in
      ok O.{expression = E_application (f, arg) ; type_annotation}
  | E_look_up dsi ->
      let%bind (ds, ind) = bind_map_pair (type_annotated_expression e) dsi in
      let%bind (src, dst) = get_t_map ds.type_annotation in
      let%bind _ = O.assert_type_value_eq (ind.type_annotation, src) in
      let dst_opt = (t_option dst ()) in
      let%bind type_annotation = check dst_opt in
      ok O.{expression = E_look_up (ds, ind) ; type_annotation}
  (* Advanced *)
  | E_matching (ex, m) -> (
      let%bind ex' = type_annotated_expression e ex in
      let%bind m' = type_match type_annotated_expression e ex'.type_annotation m in
      let%bind type_annotation = match m' with
        | Match_bool {match_true ; match_false} ->
            let%bind _ = O.assert_type_value_eq (match_true.type_annotation, match_false.type_annotation) in
            ok match_true.type_annotation
        | _ -> simple_fail "can only type match_bool expressions yet" in
      ok O.{expression = E_matching (ex', m') ; type_annotation}
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
  | Literal_int n -> ok (Literal_int n)
  | Literal_string s -> ok (Literal_string s)
  | Literal_bytes b -> ok (Literal_bytes b)

let rec untype_annotated_expression (e:O.annotated_expression) : (I.annotated_expression) result =
  let open I in
  let annotation = e.type_annotation.simplified in
  let return e = ok @@ annotated_expression e annotation in
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

and untype_block (b:O.block) : (I.block) result =
  bind_list @@ List.map untype_instruction b

and untype_instruction (i:O.instruction) : (I.instruction) result =
  let open I in
  match i with
  | I_skip -> ok I_skip
  | I_fail e ->
      let%bind e' = untype_annotated_expression e in
      ok (I_fail e')
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
        | Access_tuple _ -> simple_fail "last element of patch is tuple" in
      ok @@ I_record_patch (s.type_name, hds, [tl_name, e'])

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
