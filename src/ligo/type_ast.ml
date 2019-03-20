open Ligo_helpers.Trace

module I = Ast_simplified
module O = Ast_typed

module SMap = O.SMap

module Environment = struct
  type t = unit
  let empty : t = ()

  let get (():t) (_s:string) : O.type_value option = None
  let add (():t) (_s:string) (_tv:O.type_value) : t = ()
  let get_type (():t) (_s:string) : O.type_value option = None
  let add_type (():t) (_s:string) (_tv:O.type_value) : t = ()
end

type environment = Environment.t

type environment = unit
let empty : environment = ()

let rec type_program (p:I.program) : O.program result =
  let aux (e, acc:(environment * O.declaration list)) (d:I.declaration) =
    let%bind (e', d') = type_declaration e d in
    match d' with
    | None -> ok (e', acc)
    | Some d' -> ok (e', d' :: acc)
  in
  let%bind (_, lst) = bind_fold_list aux (empty, []) p in
  ok @@ List.rev lst

and type_declaration _env : I.declaration -> (environment * O.declaration option) result = function
  | Type_declaration _ -> simple_fail ""
  | Constant_declaration _ -> simple_fail ""

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
  | Matching m ->
      let%bind m' = type_match e m in
      ok (e, O.Matching m')

and type_match (e:environment) : I.matching -> O.matching result = function
  | Match_bool {match_true ; match_false} ->
      let%bind match_true = type_block e match_true in
      let%bind match_false = type_block e match_false in
      ok (O.Match_bool {match_true ; match_false})
  | Match_option {match_none ; match_some} ->
      let%bind match_none = type_block e match_none in
      let (n, b) = match_some in
      let%bind b' = type_block e b in
      ok (O.Match_option {match_none ; match_some = (n, b')})
  | Match_list {match_nil ; match_cons} ->
      let%bind match_nil = type_block e match_nil in
      let (n, m, b) = match_cons in
      let%bind b' = type_block e b in
      ok (O.Match_list {match_nil ; match_cons = (n, m, b')})
  | Match_tuple lst ->
      let aux (x, y) =
        let%bind y = type_block e y in
        ok (x, y) in
      let%bind lst' = bind_list @@ List.map aux lst in
      ok (O.Match_tuple lst')

and evaluate_type (e:environment) : I.type_expression -> O.type_value result = function
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
        trace_option (simple_error "unbound type variable")
        @@ Environment.get_type e name in
      ok tv
  | Type_constant (cst, lst) ->
      let%bind lst' = bind_list @@ List.map (evaluate_type e) lst in
      ok (O.Type_constant(cst, lst'))

and type_annotated_expression (e:environment) (ae:I.annotated_expression) : O.annotated_expression result =
  match ae.expression with
  | Variable name ->
      let%bind tv' =
        trace_option (simple_error "var not in env")
        @@ Environment.get e name in
      ok O.{expression = Variable name ; type_annotation = tv'}
  | _ -> simple_fail "default"
