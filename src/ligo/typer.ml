open Ligo_helpers.Trace

module I = Ast_simplified
module O = Ast_typed

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
      | (_, ((O.Type_sum m) as tv)) :: _ when SMap.mem s m -> Some (SMap.find s m, tv)
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

  let unrecognized_constant (n:string) =
    let title = "unrecognized constant" in
    let full = n in
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
  let%bind (_, lst) = bind_fold_list aux (Environment.empty, []) p in
  ok @@ List.rev lst

and type_declaration env : I.declaration -> (environment * O.declaration option) result = function
  | Type_declaration {type_name;type_expression} ->
      let%bind tv = evaluate_type env type_expression in
      let env' = Environment.add_type env type_name tv in
      ok (env', None)
  | Constant_declaration {name;annotated_expression} ->
      let%bind ae' = type_annotated_expression env annotated_expression in
      let env' = Environment.add env name ae'.type_annotation in
      ok (env', Some (O.Constant_declaration {name;annotated_expression=ae'}))

and type_block (e:environment) (b:I.block) : O.block result =
  let aux (e, acc:(environment * O.instruction list)) (i:I.instruction) =
    let%bind (e', i') = type_instruction e i in
    ok (e', i' :: acc)
  in
  let%bind (_, lst) = bind_fold_list aux (e, []) b in
  ok @@ List.rev lst

and type_instruction (e:environment) : I.instruction -> (environment * O.instruction) result = function
  | Skip -> ok (e, O.Skip)
  | Fail x ->
      let%bind expression = type_annotated_expression e x in
      ok (e, O.Fail expression)
  | Loop (cond, body) ->
      let%bind cond = type_annotated_expression e cond in
      let%bind _ =
        O.type_value_eq (cond.type_annotation, (O.Type_constant ("bool", []))) in
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
            O.type_value_eq (annotated_expression.type_annotation, prev) in
          ok (e', O.Assignment {name;annotated_expression})
      | Some _, Some prev ->
          let%bind annotated_expression = type_annotated_expression e annotated_expression in
          let%bind _assert = trace (simple_error "Annotation doesn't match environment")
            @@ O.type_value_eq (annotated_expression.type_annotation, prev) in
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
        @@ O.get_t_bool t in
      let%bind match_true = type_block e match_true in
      let%bind match_false = type_block e match_false in
      ok (O.Match_bool {match_true ; match_false})
  | Match_option {match_none ; match_some} ->
      let%bind t_opt =
        trace_strong (simple_error "Matching option on not-an-option")
        @@ O.get_t_option t in
      let%bind match_none = type_block e match_none in
      let (n, b) = match_some in
      let e' = Environment.add e n t_opt in
      let%bind b' = type_block e' b in
      ok (O.Match_option {match_none ; match_some = (n, b')})
  | Match_list {match_nil ; match_cons} ->
      let%bind t_list =
        trace_strong (simple_error "Matching list on not-an-list")
        @@ O.get_t_list t in
      let%bind match_nil = type_block e match_nil in
      let (hd, tl, b) = match_cons in
      let e' = Environment.add e hd t_list in
      let e' = Environment.add e' tl t in
      let%bind b' = type_block e' b in
      ok (O.Match_list {match_nil ; match_cons = (hd, tl, b')})
  | Match_tuple (lst, b) ->
      let%bind lst =
        trace_strong (simple_error "Matching tuple on not-a-tuple")
        get_tuple
      let aux (x, y) =
        let%bind y = type_block e y in
        ok (x, y) in
      let%bind lst' = bind_list @@ List.map aux lst in
      ok (O.Match_tuple lst')

and evaluate_type (e:environment) : I.type_expression -> O.type_value result = function
  | Type_function (a, b) ->
      let%bind a' = evaluate_type e a in
      let%bind b' = evaluate_type e b in
      ok (O.Type_function (a', b'))
  | Type_tuple lst ->
      let%bind lst' = bind_list @@ List.map (evaluate_type e) lst in
      ok (O.Type_tuple lst')
  | Type_sum m ->
      let aux k v prev =
        let%bind prev' = prev in
        let%bind v' = evaluate_type e v in
        ok @@ SMap.add k v' prev'
      in
      let%bind m = SMap.fold aux m (ok SMap.empty) in
      ok (O.Type_sum m)
  | Type_record m ->
      let aux k v prev =
        let%bind prev' = prev in
        let%bind v' = evaluate_type e v in
        ok @@ SMap.add k v' prev'
      in
      let%bind m = SMap.fold aux m (ok SMap.empty) in
      ok (O.Type_record m)
  | Type_variable name ->
      let%bind tv =
        trace_option (unbound_type_variable e name)
        @@ Environment.get_type e name in
      ok tv
  | Type_constant (cst, lst) ->
      let%bind lst' = bind_list @@ List.map (evaluate_type e) lst in
      ok (O.Type_constant(cst, lst'))

and type_annotated_expression (e:environment) (ae:I.annotated_expression) : O.annotated_expression result =
  let%bind tv_opt = match ae.type_annotation with
    | None -> ok None
    | Some s -> let%bind r = evaluate_type e s in ok (Some r) in
  let check tv = O.merge_annotation (Some tv) tv_opt in
  match ae.expression with
  (* Basic *)
  | Variable name ->
      let%bind tv' =
        trace_option (simple_error "var not in env")
        @@ Environment.get e name in
      let%bind type_annotation = check tv' in
      ok O.{expression = Variable name ; type_annotation}
  | Literal (Bool b) ->
      let%bind type_annotation = check O.t_bool in
      ok O.{expression = Literal (Bool b) ; type_annotation }
  | Literal Unit ->
      let%bind type_annotation = check O.t_unit in
      ok O.{expression = Literal (Unit) ; type_annotation }
  | Literal (String s) ->
      let%bind type_annotation = check O.t_string in
      ok O.{expression = Literal (String s) ; type_annotation }
  | Literal (Bytes s) ->
      let%bind type_annotation = check O.t_bytes in
      ok O.{expression = Literal (Bytes s) ; type_annotation }
  | Literal (Number n) ->
      let%bind type_annotation = check O.t_int in
      ok O.{expression = Literal (Int n) ; type_annotation }
  (* Tuple *)
  | Tuple lst ->
      let%bind lst' = bind_list @@ List.map (type_annotated_expression e) lst in
      let tv_lst = List.map O.get_annotation lst' in
      let%bind type_annotation = check (O.Type_tuple tv_lst) in
      ok O.{expression = Tuple lst' ; type_annotation }
  | Tuple_accessor (tpl, ind) ->
      let%bind tpl' = type_annotated_expression e tpl in
      let%bind tpl_tv = O.get_t_tuple tpl'.type_annotation in
      let%bind tv =
        generic_try (simple_error "bad tuple index")
        @@ (fun () -> List.nth tpl_tv ind) in
      let%bind type_annotation = check tv in
      ok O.{expression = O.Tuple_accessor (tpl', ind) ; type_annotation}
  (* Sum *)
  | Constructor (c, expr) ->
      let%bind (c_tv, sum_tv) =
        trace_option (simple_error "no such constructor")
        @@ Environment.get_constructor e c in
      let%bind expr' = type_annotated_expression e expr in
      let%bind _assert = O.type_value_eq (expr'.type_annotation, c_tv) in
      let%bind type_annotation = check sum_tv in
      ok O.{expression = O.Constructor(c, expr') ; type_annotation }
  (* Record *)
  | Record m ->
      let aux k expr prev =
        let%bind prev' = prev in
        let%bind expr' = type_annotated_expression e expr in
        ok (SMap.add k expr' prev')
      in
      let%bind m' = SMap.fold aux m (ok SMap.empty) in
      let%bind type_annotation = check @@ O.Type_record (SMap.map O.get_annotation m') in
      ok O.{expression = O.Record m' ; type_annotation }
  | Record_accessor (r, ind) ->
      let%bind r' = type_annotated_expression e r in
      let%bind r_tv = O.get_t_record r'.type_annotation in
      let%bind tv =
        generic_try (simple_error "bad record index")
        @@ (fun () -> SMap.find ind r_tv) in
      let%bind type_annotation = check tv in
      ok O.{expression = O.Record_accessor (r', ind) ; type_annotation }
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
      let%bind result = type_annotated_expression e' result in
      let%bind body = type_block e' body in
      let%bind type_annotation = check O.(Type_function (input_type, output_type)) in
      ok O.{expression = Lambda {binder;input_type;output_type;result;body} ; type_annotation}
  | Constant (name, lst) ->
      let%bind lst' = bind_list @@ List.map (type_annotated_expression e) lst in
      let tv_lst = List.map O.get_annotation lst' in
      let%bind (name', tv) = type_constant name tv_lst in
      let%bind type_annotation = check tv in
      ok O.{expression = O.Constant (name', lst') ; type_annotation}
  | Application (f, arg) ->
      let%bind f = type_annotated_expression e f in
      let%bind arg = type_annotated_expression e arg in
      let%bind type_annotation = match f.type_annotation with
        | Type_function (param, result) ->
            let%bind _ = O.type_value_eq (param, arg.type_annotation) in
            ok result
        | _ -> simple_fail "applying to not-a-function"
      in
      ok O.{expression = Application (f, arg) ; type_annotation}

and type_constant (name:string) (lst:O.type_value list) : (string * O.type_value) result =
  (* Constant poorman's polymorphism *)
  let open O in
  match (name, lst) with
  | "ADD", [a ; b] when a = t_int && b = t_int -> ok ("ADD_INT", t_int)
  | "ADD", [a ; b] when a = t_string && b = t_string -> ok ("CONCAT", t_string)
  | "ADD", [_ ; _] -> simple_fail "bad types to add"
  | "ADD", _ -> simple_fail "bad number of params to add"
  | name, _ -> fail @@ unrecognized_constant name
