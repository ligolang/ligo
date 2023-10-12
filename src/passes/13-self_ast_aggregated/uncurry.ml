let map_expression = Ast_aggregated.Helpers.map_expression

open Ligo_prim
open Ast_aggregated

(* Utilities *)

let rec uncurry_lambda (depth : int) (expr : expression) : Value_var.t list * expression =
  match expr.expression_content with
  | E_lambda { binder; result } when depth > 0 ->
    let vars, result = uncurry_lambda (depth - 1) result in
    Param.get_var binder :: vars, result
  | _ -> [], expr


let rec uncurry_arrow (depth : int) (type_ : type_expression)
    : type_expression list * type_expression
  =
  match type_.type_content with
  | T_arrow { type1; type2 } when depth > 0 ->
    let rest, type2 = uncurry_arrow (depth - 1) type2 in
    type1 :: rest, type2
  | _ -> [], type_


let rec uncurry_app (expr : expression) : expression * expression list =
  match expr.expression_content with
  | E_application { lamb; args } ->
    let lamb, args' = uncurry_app lamb in
    lamb, args' @ [ args ]
  | _ -> expr, []


let curried_depth_in_lambda (rhs : expression) : int =
  let vars, _ = uncurry_lambda Int.max_value rhs in
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
  match u1, u2 with
  | Application d1, Application d2 -> if d1 = d2 then u1 else Other
  | Other, _ -> Other
  | _, Other -> Other
  | Unused, u2 -> u2
  | u1, Unused -> u1


let usages = List.fold_left ~f:combine_usage ~init:Unused

let rec usage_in_expr (f : Value_var.t) (expr : expression) : usage =
  let self = usage_in_expr f in
  let self_param param e =
    match Param.get_mut_flag param with
    | Mutable -> usage_in_expr f e
    | Immutable ->
      if Value_var.equal (Param.get_var param) f then Unused else usage_in_expr f e
  in
  let self_binder vars e =
    if List.mem ~equal:Value_var.equal vars f then Unused else usage_in_expr f e
  in
  let self_cases : _ Match_expr.match_case list -> usage list =
   fun cases ->
    List.map cases ~f:(fun { pattern; body } ->
        self_binder (List.map ~f:Binder.get_var (Pattern.binders pattern)) body)
  in
  match expr.expression_content with
  (* interesting cases: *)
  | E_variable x ->
    if Value_var.equal f x (* if f was only used in applications we won't get here *)
    then Other
    else Unused
  | E_application _ ->
    let g, args = uncurry_app expr in
    let g =
      if isvar f g (* found an application of f *)
      then
        Application (List.length args)
        (* else g might be something weird which contains a usage of f,
         e.g. if expr is ((if b then f else h) arg) *)
      else self g
    in
    usages (g :: List.map ~f:self args)
  (* everything else is boilerplate... *)
  | E_literal _ -> Unused
  | E_constant { cons_name = _; arguments } -> usages (List.map ~f:self arguments)
  | E_lambda { binder; result } -> self_param binder result
  | E_type_abstraction { type_binder = _; result } -> self result
  | E_recursive { fun_name; fun_type = _; lambda = { binder; result } } ->
    let binders = fun_name :: Param.(if is_imm binder then [ get_var binder ] else []) in
    self_binder binders result
  | E_let_in { let_binder; rhs; let_result; attributes = _ } ->
    usages
      [ self rhs
      ; self_binder (List.map ~f:Binder.get_var (Pattern.binders let_binder)) let_result
      ]
  | E_raw_code _ -> Unused
  | E_constructor { constructor = _; element } -> self element
  | E_matching { matchee; cases } -> usages (self matchee :: self_cases cases)
  | E_record fields -> usages (List.map ~f:self (Record.values fields))
  | E_accessor { struct_; path = _ } -> self struct_
  | E_update { struct_; path = _; update } -> usages [ self struct_; self update ]
  | E_type_inst { forall; type_ = _ } -> self forall
  | E_assign { expression; _ } -> self expression
  | E_coerce { anno_expr; _ } -> self anno_expr
  | E_let_mut_in { let_binder = _; rhs; let_result; attributes = _ } ->
    usages [ self rhs; self let_result ]
  | E_while { cond; body } -> usages [ self cond; self body ]
  | E_for { binder; start; final; incr; f_body } ->
    usages [ self start; self final; self incr; self_binder [ binder ] f_body ]
  | E_for_each { fe_binder = binder1, binder2; collection; fe_body; _ } ->
    let binders = binder1 :: Option.to_list binder2 in
    usages [ self collection; self_binder binders fe_body ]
  | E_deref _ -> Unused


(* Actually doing one instance of uncurrying *)

let uncurried_labels (depth : int) = Label.range 0 depth

let uncurried_rows (depth : int) (args : type_expression list) : row =
  assert (List.length args = depth);
  Row.create_tuple args


let uncurried_record_type ~loc depth args =
  let record_type = uncurried_rows depth args in
  { type_content = T_record record_type
  ; orig_var = None
  ; location = loc
  ; source_type = None
  }


let uncurry_rhs (depth : int) (expr : expression) =
  let arg_types, ret_type = uncurry_arrow depth expr.type_expression in
  let vars, body = uncurry_lambda depth expr in
  let loc = expr.location in
  let binder = Value_var.fresh ~loc () in
  let labels = uncurried_labels depth in
  let record_type = uncurried_record_type ~loc depth arg_types in
  let matchee =
    { expression_content = E_variable binder
    ; location = loc
    ; type_expression = record_type
    }
  in
  let fields =
    try
      let f : Value_var.t -> type_expression -> _ Pattern.t =
       fun v ty -> Pattern.var ~loc (Binder.make v ty)
      in
      Record.of_list (List.zip_exn labels (List.map2_exn ~f vars arg_types))
    with
    | _ ->
      failwith
      @@ Format.asprintf
           "Uncurry: mismatching number of arguments, expr: %a, type %a\n%!"
           PP.expression
           expr
           PP.type_expression
           expr.type_expression
  in
  let pattern = Location.wrap ~loc (Pattern.P_record fields) in
  let result =
    { expression_content = E_matching { matchee; cases = [ { pattern; body } ] }
    ; location = loc
    ; type_expression = body.type_expression
    }
  in
  binder, result, arg_types, record_type, ret_type


let rec uncurry_in_expression ~raise (f : Value_var.t) (depth : int) (expr : expression)
    : expression
  =
  let self = uncurry_in_expression ~raise f depth in
  let self_param param e =
    match Param.get_mut_flag param with
    | Mutable -> uncurry_in_expression ~raise f depth e
    | Immutable ->
      if Value_var.equal (Param.get_var param) f
      then e
      else uncurry_in_expression ~raise f depth e
  in
  let self_binder vars e =
    if List.mem ~equal:Value_var.equal vars f
    then e
    else uncurry_in_expression ~raise f depth e
  in
  let self_cases
      : ('a, 'b) Match_expr.match_case list -> ('a, 'b) Match_expr.match_case list
    =
   fun cases ->
    let f : _ Match_expr.match_case -> _ Match_expr.match_case =
     fun { pattern; body } ->
      let body =
        self_binder (List.map ~f:Binder.get_var (Pattern.binders pattern)) body
      in
      { pattern; body }
    in
    List.map cases ~f
  in
  let return e' = { expr with expression_content = e' } in
  let return_id = return expr.expression_content in
  let loc = expr.location in
  match expr.expression_content with
  | E_application app ->
    let lamb, args = uncurry_app expr in
    if isvar f lamb
    then (
      (* the interesting part... *)
      let arg_types, _ret_type = uncurry_arrow depth lamb.type_expression in
      let record =
        E_record
          (Record.of_list
          @@
          match List.zip (uncurried_labels depth) args with
          | Ok x -> x
          | _ ->
            failwith
            @@ Format.asprintf
                 "Uncurry: mismatching number of arguments, expr: %a, type %a\n%!"
                 PP.expression
                 expr
                 PP.type_expression
                 expr.type_expression)
      in
      let args =
        { expression_content = record
        ; location = loc
        ; type_expression = uncurried_record_type ~loc depth arg_types
        }
      in
      let args = self args in
      return (E_application { lamb; args }))
    else (
      let app = Application.map self app in
      return (E_application app))
  | E_literal _ -> return_id
  | E_constant { cons_name; arguments } ->
    let arguments = List.map ~f:self arguments in
    return (E_constant { cons_name; arguments })
  | E_variable _ -> return_id
  | E_lambda { binder; output_type; result } ->
    let result = self_param binder result in
    return (E_lambda { binder; output_type; result })
  | E_type_abstraction { type_binder; result } ->
    let result = self result in
    return (E_type_abstraction { type_binder; result })
  | E_recursive
      { fun_name; fun_type; lambda = { binder; output_type; result }; force_lambdarec } ->
    let result = self_binder [ fun_name ] (self_param binder result) in
    return
      (E_recursive
         { fun_name; fun_type; lambda = { binder; output_type; result }; force_lambdarec })
  | E_let_in { let_binder; rhs; let_result; attributes } ->
    let rhs = self rhs in
    let let_result =
      self_binder (List.map ~f:Binder.get_var (Pattern.binders let_binder)) let_result
    in
    return (E_let_in { let_binder; rhs; let_result; attributes })
  | E_raw_code _ -> return_id
  | E_constructor { constructor; element } ->
    let element = self element in
    return (E_constructor { constructor; element })
  | E_matching { matchee; cases } ->
    let matchee = self matchee in
    let cases = self_cases cases in
    return (E_matching { matchee; cases })
  | E_record fields ->
    let fields = Record.map ~f:self fields in
    return (E_record fields)
  | E_accessor { struct_; path } ->
    let struct_ = self struct_ in
    return (E_accessor { struct_; path })
  | E_update { struct_; path; update } ->
    let struct_ = self struct_ in
    let update = self update in
    return (E_update { struct_; path; update })
  | E_type_inst { forall; type_ } ->
    let forall = self forall in
    return @@ E_type_inst { forall; type_ }
  | E_assign { expression; binder } ->
    let expression = self expression in
    return @@ E_assign { binder; expression }
  | E_coerce { anno_expr; type_annotation } ->
    let anno_expr = self anno_expr in
    return @@ E_coerce { anno_expr; type_annotation }
  | E_let_mut_in { let_binder; rhs; let_result; attributes } ->
    let rhs = self rhs in
    let let_result = self let_result in
    return @@ E_let_mut_in { let_binder; rhs; let_result; attributes }
  | E_while { cond; body } ->
    let cond = self cond in
    let body = self body in
    return @@ E_while { cond; body }
  | E_for { binder; start; final; incr; f_body } ->
    let start = self start in
    let final = self final in
    let incr = self incr in
    let f_body = self_binder [ binder ] f_body in
    return @@ E_for { binder; start; final; incr; f_body }
  | E_for_each
      { fe_binder = (binder1, binder2) as fe_binder
      ; collection
      ; fe_body
      ; collection_type
      } ->
    let binders = binder1 :: Option.to_list binder2 in
    let collection = self collection in
    let fe_body = self_binder binders fe_body in
    return @@ E_for_each { fe_binder; collection; fe_body; collection_type }
  | E_deref _ -> return_id


(* Uncurrying as much as possible throughout an expression *)

let uncurry_expression (expr : expression) : expression =
  (* We transform
       rec (f, x1).fun x2. ... . fun xn . E[x1, x2, ..., xn]
     into
       fun x1' . fun x2'. ... . fun xn' . let f = (rec(f, (x1, x2, ..., xn)).E[x1, x2, ..., xn]) in f(x1', x2', ..., xn') *)
  map_expression
    (fun expr ->
      let loc = expr.location in
      match expr.expression_content with
      | E_recursive
          { fun_name
          ; fun_type
          ; lambda = { binder = _; result } as lambda
          ; force_lambdarec
          } ->
        let inner_lambda = { expr with expression_content = E_lambda lambda } in
        (match usage_in_expr fun_name result with
        | Unused | Other -> expr
        | Application depth ->
          if curried_depth_in_lambda inner_lambda >= depth && depth > 1
          then (
            (* Prepare the lambda *)
            let var, result, arg_types, record_type, ret_type =
              uncurry_rhs depth inner_lambda
            in
            (* Uncurry calls inside the expression *)
            let result = uncurry_in_expression ~raise fun_name depth result in
            (* Generate binders for each argument: x1', ..., xn' *)
            let binder_types =
              List.map ~f:(fun t -> Value_var.fresh ~loc (), t) arg_types
            in
            (* An variable for each function argument *)
            let args = List.map ~f:(fun (b, t) -> e_a_variable ~loc b t) binder_types in
            (* Generate tupled argument (x1', ..., xn') *)
            let record =
              E_record
                (Record.of_list
                @@
                match List.zip (uncurried_labels depth) args with
                | Ok x -> x
                | _ ->
                  failwith
                  @@ Format.asprintf
                       "Uncurry: mismatching number of arguments, expr: %a, type %a\n%!"
                       PP.expression
                       expr
                       PP.type_expression
                       expr.type_expression)
            in
            let args =
              { expression_content = record
              ; location = loc
              ; type_expression = uncurried_record_type ~loc depth arg_types
              }
            in
            (* the source type is now wrong... but still useful? *)
            let fun_type =
              t_arrow
                ~loc:fun_type.location
                ?source_type:fun_type.source_type
                record_type
                ret_type
                ()
            in
            (* Generate the rhs for the new let: (rec(f, (x1, x2, ..., xn)).E[x1, x2, ..., xn]) *)
            let rhs =
              { expr with
                expression_content =
                  E_recursive
                    { fun_name
                    ; fun_type
                    ; lambda =
                        { binder = Param.make var record_type
                        ; output_type = ret_type
                        ; result
                        }
                    ; force_lambdarec
                    }
              ; type_expression =
                  { type_content = T_arrow { type1 = record_type; type2 = ret_type }
                  ; orig_var = None
                  ; location = loc
                  ; source_type = None
                  }
              }
            in
            (* Apply function to tuple: f(x1', x2', ..., xn') *)
            let result =
              e_a_application ~loc (e_a_variable ~loc fun_name fun_type) args ret_type
            in
            let attr =
              ValueAttr.
                { inline = true
                ; no_mutation = false
                ; view = false
                ; public = true
                ; hidden = false
                ; thunk = false
                ; entry = false
                ; dyn_entry = false
                ; deprecated = None
                }
            in
            (* Construct the let *)
            let result =
              e_a_let_in
                ~loc
                (Pattern.var ~loc (Binder.make fun_name rhs.type_expression))
                rhs
                result
                attr
            in
            let f (var, t) result =
              let binder = Param.make var t in
              e_a_lambda
                ~loc
                { binder; output_type = result.type_expression; result }
                t
                result.type_expression
            in
            (* Add the external lambdas *)
            let lambda = List.fold_right ~f ~init:result binder_types in
            lambda)
          else expr)
      | _ -> expr)
    expr
