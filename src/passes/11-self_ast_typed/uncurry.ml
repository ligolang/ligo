open Errors
(* opening Ast_typed shadows Helpers *)
let map_expression = Helpers.map_expression
open Ast_typed
open Trace

(* Utilities *)

let rec uncurry_lambda (depth : int) (expr : expression) : expression_variable list * expression =
  match expr.expression_content with
  | E_lambda { binder; result } when depth > 0 ->
    let (vars, result) = uncurry_lambda (depth - 1) result in
    (binder :: vars, result)
  | _ -> ([], expr)

let rec uncurry_arrow (depth : int) (type_ : type_expression) :
  type_expression list * type_expression =
  match type_.type_content with
  | T_arrow { type1; type2 } when depth > 0 ->
    let (rest, type2) = uncurry_arrow (depth - 1) type2 in
    (type1 :: rest, type2)
  | _ -> ([], type_)

let rec uncurry_app (expr : expression) : expression * expression list =
  match expr.expression_content with
  | E_application { lamb ; args } ->
    let (lamb, args') = uncurry_app lamb in
    (lamb, args' @ [args])
  | _ -> (expr, [])

let curried_depth_in_lambda (rhs : expression) : int =
  let (vars, _) = uncurry_lambda max_int rhs in
  List.length vars

let eqvar f x : bool =
  Location.equal_content ~equal:Var.equal f x

let isvar f x : bool =
  match x.expression_content with
  | E_variable x -> eqvar f x
  | _ -> false

(* Finding the usage of a function in an expression: we will look for
   functions which are _only_ used in applications with a certain
   fixed number of args. *)
type usage =
  | Application of int (* number of applied args *)
  | Other
  | Unused

let combine_usage (u1 : usage) (u2 : usage) : usage =
  match (u1, u2) with
  | (Application d1, Application d2) ->
    if d1 = d2
    then u1
    else Other
  | (Other, _) -> Other
  | (_, Other) -> Other
  | (Unused, u2) -> u2
  | (u1, Unused) -> u1

let usages = List.fold_left combine_usage Unused

let rec usage_in_expr (f : expression_variable) (expr : expression) : usage =
  let self = usage_in_expr f in
  let self_binder vars e =
    if List.mem ~compare:(Location.compare_content ~compare:Var.compare) f vars
    then Unused
    else usage_in_expr f e in
  match expr.expression_content with
  (* interesting cases: *)
  | E_variable x ->
    if eqvar f x
    (* if f was only used in applications we won't get here *)
    then Other
    else Unused
  | E_application _ ->
    let (g, args) = uncurry_app expr in
    let g =
      if isvar f g
      (* found an application of f *)
      then Application (List.length args)
      (* else g might be something weird which contains a usage of f,
         e.g. if expr is ((if b then f else h) arg) *)
      else self g in
    usages (g :: List.map self args)

  (* everything else is boilerplate... *)
  | E_literal _ ->
    Unused
  | E_constant { cons_name = _; arguments } ->
    usages (List.map self arguments)
  | E_lambda { binder; result } ->
    self_binder [binder] result
  | E_recursive { fun_name; fun_type = _; lambda = { binder; result } } ->
    self_binder [fun_name; binder] result
  | E_let_in { let_binder; rhs; let_result; inline = _ } ->
    usages [self rhs; self_binder [let_binder] let_result]
  | E_type_in { type_binder = _; rhs = _; let_result } ->
    self let_result
  | E_raw_code _ ->
    Unused
  | E_constructor { constructor = _; element } ->
    self element
  | E_matching { matchee;
                 cases = Ast_typed.Match_list { match_nil;
                                                match_cons = { hd; tl; body; tv = _ } } } ->
    usages [self matchee; self match_nil; self_binder [hd; tl] body]
  | E_matching { matchee;
                 cases = Ast_typed.Match_option { match_none;
                                                  match_some = { opt; body; tv = _ } } } ->
    usages [self matchee; self match_none; self_binder [opt] body]
  | E_matching { matchee; cases = Ast_typed.Match_variant { cases; tv = _ } } ->
    usages (self matchee ::
            List.map (fun { constructor = _; pattern; body } -> self_binder [pattern] body) cases)
  | E_matching { matchee; cases = Ast_typed.Match_record { fields; body; tv = _ } } ->
    usages [self matchee; self_binder (List.map fst (LMap.to_list fields)) body]
  | E_record fields ->
    usages (List.map self (LMap.to_list fields))
  | E_record_accessor { record; path = _ } ->
    self record
  | E_record_update { record; path = _; update } ->
    usages [self record; self update]
  | E_module_accessor { module_name = _; element } ->
    (* todo either this is wrong or element should not be an arbitrary
       subexpression *)
    ignore element; Unused
  | E_mod_in { module_binder = _; rhs; let_result } ->
    usages [usage_in_module_fully_typed f rhs; self let_result]
  | E_mod_alias { alias = _; binders = _; result } ->
    self result

and usage_in_module (f : expression_variable) (module_ : module') : usage =
  match module_ with
  | [] -> Unused
  | decl :: module_ ->
    let return us = usages [us; usage_in_module f module_] in
    match Location.unwrap decl with
    | Declaration_constant { binder; expr; inline = _ } ->
      if eqvar f binder
      then Unused
      else return (usage_in_expr f expr)
    | Declaration_type _ ->
      return Unused
    | Declaration_module { module_binder = _; module_ } ->
      return (usage_in_module_fully_typed f module_)
    | Module_alias { alias = _; binders = _ } ->
      return Unused

and usage_in_module_fully_typed (f : expression_variable) (module_ : module_fully_typed) : usage =
  match module_ with
  | Module_Fully_Typed module_ -> usage_in_module f module_

(* Actually doing one instance of uncurrying *)

let uncurried_labels (depth : int) =
  Ast_core.Helpers.label_range 0 depth

let uncurried_rows (depth : int) (args : type_expression list) : rows =
  let labels = uncurried_labels depth in
  let content =
    LMap.of_list
      (List.mapi
         (fun i (label, ty) ->
            (label,
             { associated_type = ty ;
               michelson_annotation = None ;
               decl_pos = i }))
         (List.combine labels args)) in
  { content ; layout = L_comb }

let uncurried_record_type depth args =
  let record_type = uncurried_rows depth args in
  { type_content = T_record record_type ;
    type_meta = None ;
    orig_var = None ;
    location = Location.generated }

let uncurry_rhs (depth : int) (expr : expression) : (expression, self_ast_typed_error) result =
  let (arg_types, ret_type) = uncurry_arrow depth expr.type_expression in

  let (vars, body) = uncurry_lambda depth expr in
  let binder = Location.wrap (Var.fresh ()) in

  let labels = uncurried_labels depth in
  let rows = uncurried_rows depth arg_types in
  let record_type = uncurried_record_type depth arg_types in

  let matchee = { expression_content = E_variable binder ;
                  location = Location.generated ;
                  type_expression = record_type } in
  let fields = LMap.of_list (List.combine labels (List.combine vars arg_types)) in
  let record_tv = { type_content = T_record rows ;
                    type_meta = None ;
                    orig_var = None ;
                    location = Location.generated } in
  let result = { expression_content = E_matching { matchee ;
                                                   cases = Match_record { fields ;
                                                                          body ;
                                                                          tv = record_tv } } ;
                 location = Location.generated ;
                 type_expression = body.type_expression } in
  ok { expr with
       expression_content = E_lambda { binder ; result } ;
       type_expression = { type_content = T_arrow {type1 = record_type ; type2 = ret_type} ;
                           type_meta = None ;
                           orig_var = None ;
                           location = Location.generated } }

let rec uncurry_in_expression
    (f : expression_variable) (depth : int) (expr : expression) :
  (expression, self_ast_typed_error) result =
  let self = uncurry_in_expression f depth in
  let self_binder vars e =
    if List.mem ~compare:(Location.compare_content ~compare:Var.compare) f vars
    then ok e
    else uncurry_in_expression f depth e in
  let return e' = ok { expr with expression_content = e' } in
  let return_id = return expr.expression_content in
  match expr.expression_content with
  | E_application app ->
    let (lamb, args) = uncurry_app expr in
    if isvar f lamb
    then
      (* the interesting part... *)
      let (arg_types, _ret_type) = uncurry_arrow depth lamb.type_expression in
      let args = { expression_content =
                     E_record (LMap.of_list (List.combine (uncurried_labels depth) args)) ;
                   location = Location.generated ;
                   type_expression = uncurried_record_type depth arg_types
                 } in
      let%bind args = self args in
      return (E_application { lamb ; args })
    else
      let { lamb ; args } = app in
      let%bind lamb = self lamb in
      let%bind args = self args in
      return (E_application { lamb ; args })
  | E_literal _ ->
    return_id
  | E_constant { cons_name; arguments } ->
    let%bind arguments = bind_map_list self arguments in
    return (E_constant { cons_name; arguments })
  | E_variable _ ->
    return_id
  | E_lambda { binder; result } ->
    let%bind result = self_binder [binder] result in
    return (E_lambda { binder; result })
  | E_recursive { fun_name; fun_type; lambda = { binder; result } } ->
    let%bind result = self_binder [fun_name; binder] result in
    return (E_recursive { fun_name; fun_type; lambda = { binder; result } })
  | E_let_in { let_binder; rhs; let_result; inline } ->
    let%bind rhs = self rhs in
    let%bind let_result = self_binder [let_binder] let_result in
    return (E_let_in { let_binder; rhs; let_result; inline })
  | E_type_in { type_binder; rhs; let_result } ->
    let%bind let_result = self let_result in
    return (E_type_in { type_binder; rhs; let_result })
  | E_raw_code _ ->
    return_id
  | E_constructor { constructor; element } ->
    let%bind element = self element in
    return (E_constructor { constructor; element })
  | E_matching { matchee; cases = Match_list { match_nil; match_cons = { hd; tl; body; tv } } } ->
    let%bind matchee = self matchee in
    let%bind match_nil = self match_nil in
    let%bind body = self_binder [hd; tl] body in
    return (E_matching { matchee; cases = Match_list { match_nil; match_cons = { hd; tl; body; tv } } })
  | E_matching { matchee; cases = Match_option { match_none; match_some = { opt; body; tv } } } ->
    let%bind matchee = self matchee in
    let%bind match_none = self match_none in
    let%bind body = self_binder [opt] body in
    return (E_matching { matchee; cases = Match_option { match_none; match_some = { opt; body; tv } } })
  | E_matching { matchee; cases = Match_variant { cases; tv } } ->
    let%bind matchee = self matchee in
    let%bind cases =
      bind_map_list
        (fun { constructor; pattern; body } ->
           let%bind body = self_binder [pattern] body in
           ok { constructor; pattern; body })
        cases in
    return (E_matching { matchee; cases = Match_variant { cases; tv } } )
  | E_matching { matchee; cases = Match_record { fields; body; tv } } ->
    let%bind matchee = self matchee in
    let%bind body = self_binder (List.map fst (LMap.to_list fields)) body in
    return (E_matching { matchee; cases = Match_record { fields; body; tv } })
  | E_record fields ->
    let%bind fields =
      bind_map_list
        (fun (k, v) -> let%bind v = self v in ok (k, v))
        (LMap.to_kv_list fields) in
    let fields = LMap.of_list fields in
    return (E_record fields)
  | E_record_accessor { record; path } ->
    let%bind record = self record in
    return (E_record_accessor { record; path })
  | E_record_update { record; path; update } ->
    let%bind record = self record in
    let%bind update = self update in
    return (E_record_update { record; path; update })
  | E_module_accessor { module_name; element } ->
    (* todo either this is wrong or element should not be an arbitrary
       subexpression *)
    return (E_module_accessor { module_name; element })
  | E_mod_in { module_binder; rhs; let_result } ->
    let%bind rhs = uncurry_in_module_fully_typed f depth rhs in
    let%bind let_result = self let_result in
    return (E_mod_in { module_binder; rhs; let_result })
  | E_mod_alias { alias; binders; result } ->
    let%bind result = self result in
    return (E_mod_alias { alias; binders; result })

and uncurry_in_module (f : expression_variable) (depth : int) (module_ : module') : (module', self_ast_typed_error) result =
  match module_ with
  | [] -> ok []
  | decl :: module_ ->
    match Location.unwrap decl with
    | Declaration_type _ ->
      let%bind module_ = uncurry_in_module f depth module_ in
      ok (decl :: module_)
    | Declaration_constant { name; binder; expr; inline } ->
      let%bind expr = uncurry_in_expression f depth expr in
      let decl = {decl with wrap_content = (Declaration_constant { name; binder; expr; inline } : declaration)} in
      let%bind module_ =
        if eqvar f binder
        then ok module_
        else uncurry_in_module f depth module_ in
      ok (decl :: module_)
    | Declaration_module { module_binder; module_ = module_' } ->
      let%bind module_' = uncurry_in_module_fully_typed f depth module_' in
      let decl = {decl with wrap_content = (Declaration_module { module_binder; module_ = module_' } : declaration)} in
      let%bind module_ = uncurry_in_module f depth module_ in
      ok (decl :: module_)
    | Module_alias _ ->
      let%bind module_ = uncurry_in_module f depth module_ in
      ok (decl :: module_)

and uncurry_in_module_fully_typed f depth module_ =
  match module_ with
  | Module_Fully_Typed module_ ->
    let%bind module_ = uncurry_in_module f depth module_ in
    ok (Module_Fully_Typed module_)

(* Uncurrying as much as possible throughout an expression or module *)

let uncurry_expression (expr : expression) : (expression, self_ast_typed_error) result =
  map_expression
    (fun expr ->
       match expr.expression_content with
       | E_let_in { let_binder; rhs; let_result; inline } ->
         let continue rhs let_result =
           ok { expr with expression_content = E_let_in { let_binder; rhs; let_result; inline } } in
         (match usage_in_expr let_binder let_result with
          | Unused | Other ->
            continue rhs let_result
          | Application depth ->
            if curried_depth_in_lambda rhs >= depth && depth > 1
            then
              let%bind rhs = uncurry_rhs depth rhs in
              let%bind let_result = uncurry_in_expression let_binder depth let_result in
              continue rhs let_result
            else
              continue rhs let_result)
       | _ -> ok expr)
    expr

(* NB: This is only appropriate for the top-level module *)
let rec uncurry_module (module_ : module') : (module', self_ast_typed_error) result =
  match module_ with
  | [] -> ok []
  | decl :: module_ ->
    match Location.unwrap decl with
    | Declaration_type _ ->
      let%bind module_ = uncurry_module module_ in
      ok (decl :: module_)
    | Declaration_constant { name; binder; expr; inline } ->
      let%bind expr = uncurry_expression expr in
      let continue expr module_ =
        let%bind module_ = uncurry_module module_ in
        ok ({ decl with wrap_content = (Declaration_constant { name; binder; expr; inline } : declaration) } :: module_) in
      (match usage_in_module binder module_ with
       | Unused | Other ->
         continue expr module_
       | Application depth ->
         if curried_depth_in_lambda expr >= depth && depth > 1
         then
           let%bind expr = uncurry_rhs depth expr in
           let%bind module_ = uncurry_in_module binder depth module_ in
           continue expr module_
         else
           continue expr module_)
    | Declaration_module _ ->
      (* not descending into the sub-module *)
      let%bind module_ = uncurry_module module_ in
      ok (decl :: module_)
    | Module_alias _ -> 
      let%bind module_ = uncurry_module module_ in
      ok (decl :: module_)

(* NB: This is only appropriate for the top-level module *)
let uncurry_module_fully_typed (module_ : module_fully_typed) : (module_fully_typed, self_ast_typed_error) result =
  match module_ with
  | Module_Fully_Typed p ->
    let%bind p = uncurry_module p in
    ok (Module_Fully_Typed p)
