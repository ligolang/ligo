open Ligo_helpers.Trace

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
    environment = [] ;
    type_environment = [] ;
  }

  let get (e:t) (s:string) : ele option =
    List.assoc_opt s e.environment
  let get_constructor (e:t) (s:string) : (ele * ele) option =
    let rec aux = function
      | [] -> None
      | (_, (O.{type_value=(O.Type_sum m)} as tv)) :: _ when SMap.mem s m -> Some (SMap.find s m, tv)
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
    open Ligo_helpers.PP

    let value ppf (e:t) =
      let pp ppf (s, e) = fprintf ppf "%s -> %a" s O.PP.type_value e in
      fprintf ppf "ValueEnv[%a]" (list_sep pp) e.environment

    let type_ ppf e =
      let pp ppf (s, e) = fprintf ppf "%s -> %a" s O.PP.type_value e in
      fprintf ppf "TypeEnv[%a]" (list_sep pp) e.type_environment

    let full ppf e =
      fprintf ppf "%a\n%a" value e type_ e
  end
end

type environment = Environment.t

module Errors = struct
  let unbound_type_variable (e:environment) (n:string) =
    let title = "unbound type variable" in
    let full = Format.asprintf "%s in %a" n Environment.PP.type_ e in
    error title full

  let unbound_variable (e:environment) (n:string) =
    let title = "unbound variable" in
    let full = Format.asprintf "%s in %a" n Environment.PP.value e in
    error title full

  let unrecognized_constant (n:string) =
    let title = "unrecognized constant" in
    let full = n in
    error title full

  let program_error (p:I.program) =
    let title = "typing program" in
    let full = Format.asprintf "%a" I.PP.program p in
    error title full

  let constant_declaration_error (name:string) (ae:I.ae) =
    let title = "typing constant declaration" in
    let full =
      Format.asprintf "%s = %a" name
        I.PP.annotated_expression ae
    in
    error title full

end
open Errors


let rec type_program (p:I.program) : O.program result =
  let aux (e, acc:(environment * O.declaration list)) (d:I.declaration) =
    let%bind (e', d') = type_declaration e d in
    match d' with
    | None -> ok (e', acc)
    | Some d' -> ok (e', d' :: acc)
  in
  let%bind (_, lst) =
    trace (program_error p) @@
    bind_fold_list aux (Environment.empty, []) p in
  ok @@ List.rev lst

and type_declaration env : I.declaration -> (environment * O.declaration option) result = function
  | Type_declaration {type_name;type_expression} ->
      let%bind tv = evaluate_type env type_expression in
      let env' = Environment.add_type env type_name tv in
      ok (env', None)
  | Constant_declaration {name;annotated_expression} ->
      let%bind ae' =
        trace (constant_declaration_error name annotated_expression) @@
        type_annotated_expression env annotated_expression in
      let env' = Environment.add env name ae'.type_annotation in
      ok (env', Some (O.Constant_declaration {name;annotated_expression=ae'}))

and type_block_full (e:environment) (b:I.block) : (O.block * environment) result =
  let aux (e, acc:(environment * O.instruction list)) (i:I.instruction) =
    let%bind (e', i') = type_instruction e i in
    ok (e', i' :: acc)
  in
  let%bind (e', lst) = bind_fold_list aux (e, []) b in
  ok @@ (List.rev lst, e')

and type_block (e:environment) (b:I.block) : O.block result =
  let%bind (block, _) = type_block_full e b in
  ok block

and type_instruction (e:environment) : I.instruction -> (environment * O.instruction) result = function
  | Skip -> ok (e, O.Skip)
  | Fail x ->
      let%bind expression = type_annotated_expression e x in
      ok (e, O.Fail expression)
  | Loop (cond, body) ->
      let%bind cond = type_annotated_expression e cond in
      let%bind _ =
        O.assert_type_value_eq (cond.type_annotation, make_t_bool) in
      let%bind body = type_block e body in
      ok (e, O.Loop (cond, body))
  | Assignment {name;annotated_expression} -> (
      match annotated_expression.type_annotation, Environment.get e name with
      | None, None -> simple_fail "Initial assignments need type"
      | Some _, None ->
          let%bind annotated_expression = type_annotated_expression e annotated_expression in
          let e' = Environment.add e name annotated_expression.type_annotation in
          ok (e', O.Assignment {name;annotated_expression})
      | None, Some prev ->
          let%bind annotated_expression = type_annotated_expression e annotated_expression in
          let e' = Environment.add e name annotated_expression.type_annotation in
          let%bind _ =
            O.assert_type_value_eq (annotated_expression.type_annotation, prev) in
          ok (e', O.Assignment {name;annotated_expression})
      | Some _, Some prev ->
          let%bind annotated_expression = type_annotated_expression e annotated_expression in
          let%bind _assert = trace (simple_error "Annotation doesn't match environment")
            @@ O.assert_type_value_eq (annotated_expression.type_annotation, prev) in
          let e' = Environment.add e name annotated_expression.type_annotation in
          ok (e', O.Assignment {name;annotated_expression})
    )
  | Matching (ex, m) ->
      let%bind ex' = type_annotated_expression e ex in
      let%bind m' = type_match e ex'.type_annotation m in
      ok (e, O.Matching (ex', m'))
  | Record_patch _ -> simple_fail "no record_patch yet"

and type_match (e:environment) (t:O.type_value) : I.matching -> O.matching result = function
  | Match_bool {match_true ; match_false} ->
      let%bind _ =
        trace_strong (simple_error "Matching bool on not-a-bool")
        @@ get_t_bool t in
      let%bind match_true = type_block e match_true in
      let%bind match_false = type_block e match_false in
      ok (O.Match_bool {match_true ; match_false})
  | Match_option {match_none ; match_some} ->
      let%bind t_opt =
        trace_strong (simple_error "Matching option on not-an-option")
        @@ get_t_option t in
      let%bind match_none = type_block e match_none in
      let (n, b) = match_some in
      let e' = Environment.add e n t_opt in
      let%bind b' = type_block e' b in
      ok (O.Match_option {match_none ; match_some = (n, b')})
  | Match_list {match_nil ; match_cons} ->
      let%bind t_list =
        trace_strong (simple_error "Matching list on not-an-list")
        @@ get_t_list t in
      let%bind match_nil = type_block e match_nil in
      let (hd, tl, b) = match_cons in
      let e' = Environment.add e hd t_list in
      let e' = Environment.add e' tl t in
      let%bind b' = type_block e' b in
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
      let%bind b' = type_block e' b in
      ok (O.Match_tuple (lst, b'))

and evaluate_type (e:environment) (t:I.type_expression) : O.type_value result =
  let return tv' = ok O.(type_value tv' (Some t)) in
  match t with
  | Type_function (a, b) ->
      let%bind a' = evaluate_type e a in
      let%bind b' = evaluate_type e b in
      return (Type_function (a', b'))
  | Type_tuple lst ->
      let%bind lst' = bind_list @@ List.map (evaluate_type e) lst in
      return (Type_tuple lst')
  | Type_sum m ->
      let aux k v prev =
        let%bind prev' = prev in
        let%bind v' = evaluate_type e v in
        ok @@ SMap.add k v' prev'
      in
      let%bind m = SMap.fold aux m (ok SMap.empty) in
      return (Type_sum m)
  | Type_record m ->
      let aux k v prev =
        let%bind prev' = prev in
        let%bind v' = evaluate_type e v in
        ok @@ SMap.add k v' prev'
      in
      let%bind m = SMap.fold aux m (ok SMap.empty) in
      return (Type_record m)
  | Type_variable name ->
      let%bind tv =
        trace_option (unbound_type_variable e name)
        @@ Environment.get_type e name in
      ok tv
  | Type_constant (cst, lst) ->
      let%bind lst' = bind_list @@ List.map (evaluate_type e) lst in
      return (Type_constant(cst, lst'))

and type_annotated_expression (e:environment) (ae:I.annotated_expression) : O.annotated_expression result =
  let%bind tv_opt = match ae.type_annotation with
    | None -> ok None
    | Some s -> let%bind r = evaluate_type e s in ok (Some r) in
  let check tv = O.(merge_annotation tv_opt (Some tv)) in
  match ae.expression with
  (* Basic *)
  | Variable name ->
      let%bind tv' =
        trace_option (unbound_variable e name)
        @@ Environment.get e name in
      let%bind type_annotation = check tv' in
      ok O.{expression = Variable name ; type_annotation}
  | Literal (Bool b) ->
      let%bind type_annotation = check make_t_bool in
      ok O.{expression = Literal (Bool b) ; type_annotation }
  | Literal Unit ->
      let%bind type_annotation = check make_t_unit in
      ok O.{expression = Literal (Unit) ; type_annotation }
  | Literal (String s) ->
      let%bind type_annotation = check make_t_string in
      ok O.{expression = Literal (String s) ; type_annotation }
  | Literal (Bytes s) ->
      let%bind type_annotation = check make_t_bytes in
      ok O.{expression = Literal (Bytes s) ; type_annotation }
  | Literal (Number n) ->
      let%bind type_annotation = check make_t_int in
      ok O.{expression = Literal (Int n) ; type_annotation }
  (* Tuple *)
  | Tuple lst ->
      let%bind lst' = bind_list @@ List.map (type_annotated_expression e) lst in
      let tv_lst = List.map get_annotation lst' in
      let%bind type_annotation = check (make_t_tuple tv_lst) in
      ok O.{expression = Tuple lst' ; type_annotation }
  | Accessor (ae, path) ->
      let%bind e' = type_annotated_expression e ae in
      let aux (prev:O.annotated_expression) (a:I.access) : O.annotated_expression result =
        match a with
        | Tuple_access index -> (
            let%bind tpl_tv = get_t_tuple prev.type_annotation in
            let%bind tv =
              generic_try (simple_error "bad tuple index")
              @@ (fun () -> List.nth tpl_tv index) in
            let%bind type_annotation = check tv in
            ok O.{expression = O.Tuple_accessor (prev, index) ; type_annotation}
          )
        | Record_access property -> (
            let%bind r_tv = get_t_record prev.type_annotation in
            let%bind tv =
              generic_try (simple_error "bad record index")
              @@ (fun () -> SMap.find property r_tv) in
            let%bind type_annotation = check tv in
            ok O.{expression = O.Record_accessor (prev, property) ; type_annotation }
          )
      in
      bind_fold_list aux e' path

  (* Sum *)
  | Constructor (c, expr) ->
      let%bind (c_tv, sum_tv) =
        trace_option (simple_error "no such constructor")
        @@ Environment.get_constructor e c in
      let%bind expr' = type_annotated_expression e expr in
      let%bind _assert = O.assert_type_value_eq (expr'.type_annotation, c_tv) in
      let%bind type_annotation = check sum_tv in
      ok O.{expression = O.Constructor(c, expr') ; type_annotation }
  (* Record *)
  | Record m ->
      let aux prev k expr =
        let%bind expr' = type_annotated_expression e expr in
        ok (SMap.add k expr' prev)
      in
      let%bind m' = bind_fold_smap aux (ok SMap.empty) m in
      let%bind type_annotation = check @@ make_t_record (SMap.map get_annotation m') in
      ok O.{expression = O.Record m' ; type_annotation }
  | Lambda {
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
      let%bind type_annotation = check @@ make_t_function (input_type, output_type) in
      ok O.{expression = Lambda {binder;input_type;output_type;result;body} ; type_annotation}
  | Constant (name, lst) ->
      let%bind lst' = bind_list @@ List.map (type_annotated_expression e) lst in
      let tv_lst = List.map get_annotation lst' in
      let%bind (name', tv) = type_constant name tv_lst in
      let%bind type_annotation = check tv in
      ok O.{expression = O.Constant (name', lst') ; type_annotation}
  | Application (f, arg) ->
      let%bind f = type_annotated_expression e f in
      let%bind arg = type_annotated_expression e arg in
      let%bind type_annotation = match f.type_annotation.type_value with
        | Type_function (param, result) ->
            let%bind _ = O.assert_type_value_eq (param, arg.type_annotation) in
            ok result
        | _ -> simple_fail "applying to not-a-function"
      in
      ok O.{expression = Application (f, arg) ; type_annotation}

and type_constant (name:string) (lst:O.type_value list) : (string * O.type_value) result =
  (* Constant poorman's polymorphism *)
  let open O in
  match (name, lst) with
  | "ADD", [a ; b] when type_value_eq (a, make_t_int) && type_value_eq (b, make_t_int) -> ok ("ADD_INT", make_t_int)
  | "ADD", [a ; b] when type_value_eq (a, make_t_string) && type_value_eq (b, make_t_string) -> ok ("CONCAT", make_t_string)
  | "ADD", [_ ; _] -> simple_fail "bad types to add"
  | "ADD", _ -> simple_fail "bad number of params to add"
  | "EQ", [a ; b] when type_value_eq (a, make_t_int) && type_value_eq (b, make_t_int) -> ok ("EQ", make_t_bool)
  | "EQ", _ -> simple_fail "EQ only defined over int"
  | "OR", [a ; b] when type_value_eq (a, make_t_bool) && type_value_eq (b, make_t_bool) -> ok ("OR", make_t_bool)
  | "OR", _ -> simple_fail "OR only defined over bool"
  | "AND", [a ; b] when type_value_eq (a, make_t_bool) && type_value_eq (b, make_t_bool) -> ok ("AND", make_t_bool)
  | "AND", _ -> simple_fail "AND only defined over bool"
  | name, _ -> fail @@ unrecognized_constant name

let untype_type_value (t:O.type_value) : (I.type_expression) result =
  match t.simplified with
  | Some s -> ok s
  | _ -> simple_fail "trying to untype generated type"

let untype_literal (l:O.literal) : I.literal result =
  let open I in
  match l with
  | Unit -> ok Unit
  | Bool b -> ok (Bool b)
  | Nat n -> ok (Number n)
  | Int n -> ok (Number n)
  | String s -> ok (String s)
  | Bytes b -> ok (Bytes b)

let rec untype_annotated_expression (e:O.annotated_expression) : (I.annotated_expression) result =
  let open I in
  let annotation = e.type_annotation.simplified in
  let return e = ok @@ annotated_expression e annotation in
  match e.expression with
  | Literal l ->
      let%bind l = untype_literal l in
      return (Literal l)
  | Constant (n, lst) ->
      let%bind lst' = bind_list
        @@ List.map untype_annotated_expression lst in
      return (Constant (n, lst'))
  | Variable n ->
      return (Variable n)
  | Application (f, arg) ->
      let%bind f' = untype_annotated_expression f in
      let%bind arg' = untype_annotated_expression arg in
      return (Application (f', arg'))
  | Lambda {binder;input_type;output_type;body;result} ->
      let%bind input_type = untype_type_value input_type in
      let%bind output_type = untype_type_value output_type in
      let%bind result = untype_annotated_expression result in
      let%bind body = untype_block body in
      return (Lambda {binder;input_type;output_type;body;result})
  | Tuple lst ->
      let%bind lst' = bind_list
        @@ List.map untype_annotated_expression lst in
      return (Tuple lst')
  | Tuple_accessor (tpl, ind)  ->
      let%bind tpl' = untype_annotated_expression tpl in
      return (Accessor (tpl', [Tuple_access ind]))
  | Constructor (n, p) ->
      let%bind p' = untype_annotated_expression p in
      return (Constructor (n, p'))
  | Record r ->
      let%bind r' = bind_smap
        @@ SMap.map untype_annotated_expression r in
      return (Record r')
  | Record_accessor (r, s) ->
      let%bind r' = untype_annotated_expression r in
      return (Accessor (r', [Record_access s]))

and untype_block (b:O.block) : (I.block) result =
  bind_list @@ List.map untype_instruction b

and untype_instruction (i:O.instruction) : (I.instruction) result =
  let open I in
  match i with
  | Skip -> ok Skip
  | Fail e ->
      let%bind e' = untype_annotated_expression e in
      ok (Fail e')
  | Loop (e, b) ->
      let%bind e' = untype_annotated_expression e in
      let%bind b' = untype_block b in
      ok @@ Loop (e', b')
  | Assignment a ->
      let%bind annotated_expression = untype_annotated_expression a.annotated_expression in
      ok @@ Assignment {name = a.name ; annotated_expression}
  | Matching (e, m) ->
      let%bind e' = untype_annotated_expression e in
      let%bind m' = untype_matching m in
      ok @@ Matching (e', m')

and untype_matching (m:O.matching) : (I.matching) result =
  let open I in
  match m with
  | Match_bool {match_true ; match_false} ->
      let%bind match_true = untype_block match_true in
      let%bind match_false = untype_block match_false in
      ok @@ Match_bool {match_true ; match_false}
  | Match_tuple (lst, b) ->
      let%bind b = untype_block b in
      ok @@ Match_tuple (lst, b)
  | Match_option {match_none ; match_some = (v, some)} ->
      let%bind match_none = untype_block match_none in
      let%bind some = untype_block some in
      let match_some = v, some in
      ok @@ Match_option {match_none ; match_some}
  | Match_list {match_nil ; match_cons = (hd, tl, cons)} ->
      let%bind match_nil = untype_block match_nil in
      let%bind cons = untype_block cons in
      let match_cons = hd, tl, cons in
      ok @@ Match_list {match_nil ; match_cons}
