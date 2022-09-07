let map_expression = Helpers.map_expression
open Ligo_prim
open Ast_aggregated

(* Utilities *)

let rec uncurry_lambda (depth : int) (expr : expression) : Value_var.t list * expression =
  match expr.expression_content with
  | E_lambda { binder; result } when depth > 0 ->
    let (vars, result) = uncurry_lambda (depth - 1) result in
    (binder.var :: vars, result)
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
  let (vars, _) = uncurry_lambda Int.max_value rhs in
  List.length vars

let isvar f x : bool =
  match x.expression_content with
  | E_variable x -> Value_var.equal f x
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

let usages = List.fold_left ~f:combine_usage ~init:Unused

let rec usage_in_expr (f : Value_var.t) (expr : expression) : usage =
  let self = usage_in_expr f in
  let self_binder vars e =
    if List.mem ~equal:Value_var.equal vars f
    then Unused
    else usage_in_expr f e in
  match expr.expression_content with
  (* interesting cases: *)
  | E_variable x ->
    if Value_var.equal f x
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
    usages (g :: List.map ~f:self args)

  (* everything else is boilerplate... *)
  | E_literal _ ->
    Unused
  | E_constant { cons_name = _; arguments } ->
    usages (List.map ~f:self arguments)
  | E_lambda { binder; result } ->
    self_binder [binder.var] result
  | E_type_abstraction { type_binder = _; result } ->
    self result
  | E_recursive { fun_name; fun_type = _; lambda = { binder; result } } ->
    self_binder [fun_name; binder.var] result
  | E_let_in { let_binder; rhs; let_result; attr = _ } ->
    usages [self rhs; self_binder [let_binder.var] let_result]
  | E_raw_code _ ->
    Unused
  | E_constructor { constructor = _; element } ->
    self element
  | E_matching { matchee; cases = Ast_aggregated.Match_variant { cases; tv = _ } } ->
    usages (self matchee ::
            List.map ~f:(fun { constructor = _; pattern; body } -> self_binder [pattern] body) cases)
  | E_matching { matchee; cases = Ast_aggregated.Match_record { fields; body; tv = _ } } ->
    usages [self matchee; self_binder (List.map ~f:(fun b -> b.Binder.var) (Record.LMap.to_list fields)) body]
  | E_record fields ->
    usages (List.map ~f:self (Record.LMap.to_list fields))
  | E_accessor { struct_; path = _ } ->
    self struct_
  | E_update { struct_; path = _; update } ->
    usages [self struct_; self update]
  | E_type_inst { forall; type_ = _} ->
    self forall
  | E_assign _ -> failwith "Assignation is purified before" (* Todo: maybe add for commutativity *)

(* Actually doing one instance of uncurrying *)

let uncurried_labels (depth : int) =
  Label.range 0 depth

let uncurried_rows (depth : int) (args : type_expression list) : rows =
  let labels = uncurried_labels depth in
  let fields =
    Record.of_list
      (List.mapi
         ~f:(fun i (label, ty) ->
            (label,
             ({ associated_type = ty ;
               michelson_annotation = None ;
               decl_pos = i }: row_element)))
         (match List.zip labels args with Ok x -> x
         | _ -> failwith @@
          Format.asprintf "uncurried_rows: args length : %i expected %i\n%!"
            (List.length args) depth)) in
  { fields ; layout = L_comb }

let uncurried_record_type depth args =
  let record_type = uncurried_rows depth args in
  { type_content = T_record record_type ;
    orig_var = None ;
    location = Location.generated ;
    source_type = None }

let uncurry_rhs (depth : int) (expr : expression) =
  let (arg_types, ret_type) = uncurry_arrow depth expr.type_expression in

  let (vars, body) = uncurry_lambda depth expr in
  let binder = Value_var.fresh () in

  let labels = uncurried_labels depth in
  let rows = uncurried_rows depth arg_types in
  let record_type = uncurried_record_type depth arg_types in
  let matchee = { expression_content = E_variable binder ;
                  location = Location.generated ;
                  type_expression = record_type } in
  let fields =
    try
      Record.of_list (List.zip_exn labels (List.map2_exn ~f:(fun var ascr : _ Binder.t -> {var;ascr;attributes={const_or_var=None}}) vars arg_types))
    with
      | _ -> failwith @@
        Format.asprintf "Uncurry: mismatching number of arguments, expr: %a, type %a\n%!"
        PP.expression expr PP.type_expression expr.type_expression
    in
  let record_tv = { type_content = T_record rows ;
                    orig_var = None ;
                    location = Location.generated ;
                    source_type = None } in
  let result = { expression_content = E_matching { matchee ;
                                                   cases = Match_record { fields ;
                                                                          body ;
                                                                          tv = record_tv } } ;
                 location = Location.generated ;
                 type_expression = body.type_expression } in
  (binder, result, arg_types, record_type, ret_type)

let rec uncurry_in_expression ~raise
    (f : Value_var.t) (depth : int) (expr : expression) :
  expression =
  let self = uncurry_in_expression ~raise f depth in
  let self_binder vars e =
    if List.mem ~equal:Value_var.equal vars f
    then e
    else uncurry_in_expression ~raise f depth e in
  let return e' = { expr with expression_content = e' } in
  let return_id = return expr.expression_content in
  match expr.expression_content with
  | E_application app ->
    let (lamb, args) = uncurry_app expr in
    if isvar f lamb
    then
      (* the interesting part... *)
      let (arg_types, _ret_type) = uncurry_arrow depth lamb.type_expression in
      let record = E_record (Record.of_list @@
          match List.zip (uncurried_labels depth) args with
            Ok x -> x
          | _ -> failwith @@
            Format.asprintf "Uncurry: mismatching number of arguments, expr: %a, type %a\n%!"
            PP.expression expr PP.type_expression expr.type_expression
       ) in
      let args = { expression_content = record ;
                   location = Location.generated ;
                   type_expression = uncurried_record_type depth arg_types
                 } in
      let args = self args in
      return (E_application { lamb ; args })
    else
      let app = Application.map self app in
      return (E_application app)
  | E_literal _ ->
    return_id
  | E_constant { cons_name; arguments } ->
    let arguments = List.map ~f:self arguments in
    return (E_constant { cons_name; arguments })
  | E_variable _ ->
    return_id
  | E_lambda { binder; output_type; result } ->
    let result = self_binder [binder.var] result in
    return (E_lambda { binder; output_type; result })
  | E_type_abstraction { type_binder; result } ->
    let result = self result in
    return (E_type_abstraction { type_binder; result })
  | E_recursive { fun_name; fun_type; lambda = { binder; output_type; result } } ->
    let result = self_binder [fun_name; binder.var] result in
    return (E_recursive { fun_name; fun_type; lambda = { binder; output_type; result } })
  | E_let_in { let_binder; rhs; let_result; attr } ->
    let rhs = self rhs in
    let let_result = self_binder [let_binder.var] let_result in
    return (E_let_in { let_binder; rhs; let_result; attr })
  | E_raw_code _ ->
    return_id
  | E_constructor { constructor; element } ->
    let element = self element in
    return (E_constructor { constructor; element })
  | E_matching { matchee; cases = Match_variant { cases; tv } } ->
    let matchee = self matchee in
    let cases =
      List.map
        ~f:(fun { constructor; pattern; body } ->
           let body = self_binder [pattern] body in
           { constructor; pattern; body })
        cases in
    return (E_matching { matchee; cases = Match_variant { cases; tv } } )
  | E_matching { matchee; cases = Match_record { fields; body; tv } } ->
    let matchee = self matchee in
    let body = self_binder (List.map ~f:(fun b -> b.Binder.var) (Record.LMap.to_list fields)) body in
    return (E_matching { matchee; cases = Match_record { fields; body; tv } })
  | E_record fields ->
    let fields = Record.map self fields in
    return (E_record fields)
  | E_accessor { struct_; path } ->
    let struct_ = self struct_ in
    return (E_accessor { struct_; path })
  | E_update { struct_; path; update } ->
    let struct_ = self struct_ in
    let update = self update in
    return (E_update { struct_; path; update })
  | E_type_inst {forall;type_} ->
    let forall = self forall in
    return @@ E_type_inst {forall;type_}
  | E_assign _ -> failwith "Assignation is purified before" (* Todo: maybe add for commutativity *)

(* Uncurrying as much as possible throughout an expression *)

let uncurry_expression (expr : expression) : expression =
  (* We transform
       rec (f, x1).fun x2. ... . fun xn . E[x1, x2, ..., xn]
     into
       fun x1' . fun x2'. ... . fun xn' . let f = (rec(f, (x1, x2, ..., xn)).E[x1, x2, ..., xn]) in f(x1', x2', ..., xn') *)
  map_expression
    (fun expr ->
       match expr.expression_content with
       | E_recursive { fun_name ; fun_type ; lambda = { binder = _ ; result } as lambda } ->
         let inner_lambda = { expr with expression_content = E_lambda lambda } in
         (match usage_in_expr fun_name result with
          | Unused | Other ->
            expr
          | Application depth ->
            if curried_depth_in_lambda inner_lambda >= depth && depth > 1
            then
              (* Prepare the lambda *)
              let (var, result, arg_types, record_type, ret_type) = uncurry_rhs depth inner_lambda in
              (* Uncurry calls inside the expression *)
              let result = uncurry_in_expression ~raise fun_name depth result in
              (* Generate binders for each argument: x1', ..., xn' *)
              let binder_types = List.map ~f:(fun t -> (Value_var.fresh (), t)) arg_types in
              (* An variable for each function argument *)
              let args = List.map ~f:(fun (b, t) -> e_a_variable b t) binder_types in
              (* Generate tupled argument (x1', ..., xn') *)
              let record = E_record (Record.of_list @@
                  match List.zip (uncurried_labels depth) args with
                    Ok x -> x
                  | _ -> failwith @@
                    Format.asprintf "Uncurry: mismatching number of arguments, expr: %a, type %a\n%!"
                    PP.expression expr PP.type_expression expr.type_expression
              ) in
              let args = { expression_content = record;
                           location = Location.generated ;
                           type_expression = uncurried_record_type depth arg_types
                         } in
              (* the source type is now wrong... but still useful? *)
              let fun_type = t_arrow ~loc:fun_type.location ?source_type:fun_type.source_type record_type ret_type () in
              (* Generate the rhs for the new let: (rec(f, (x1, x2, ..., xn)).E[x1, x2, ..., xn]) *)
              let rhs = { expr with
                          expression_content = E_recursive { fun_name ; fun_type ; lambda = { binder = {var;ascr=record_type;attributes=Binder.empty_attribute} ; output_type=ret_type; result } } ;
                          type_expression = { type_content = T_arrow {type1 = record_type ; type2 = ret_type} ;
                                              orig_var = None ;
                                              location = Location.generated ;
                                              source_type = None } } in
              (* Apply function to tuple: f(x1', x2', ..., xn') *)
              let result = e_a_application (e_a_variable fun_name fun_type) args ret_type in
              let attr = ValueAttr.{ inline = true ; no_mutation = false ; view = false; public = true ; hidden = false ; thunk = false } in
              (* Construct the let *)
              let result = e_a_let_in {var=fun_name;ascr=rhs.type_expression;attributes=Binder.empty_attribute} rhs result attr in
              let f (var, t) result =
                let binder : _ Binder.t = {var;ascr=t; attributes=Binder.empty_attribute} in
                e_a_lambda { binder ; output_type = result.type_expression; result } t result.type_expression in
              (* Add the external lambdas *)
              let lambda = List.fold_right ~f ~init:result binder_types in
              lambda
            else
              expr)
       | _ -> expr)
    expr
