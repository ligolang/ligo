include Fuzz_shared.Monad
open Cst.Jsligo

module Fold_helpers (M : Monad) = struct
  open Monad_context (M)

  type 'a monad = 'a t

  let ok x = return x

  let bind_map_npseq f (hd, tl) =
    let* hd = f hd in
    let* tl =
      bind_map_list
        (fun (a, b) ->
          let* b = f b in
          ok (a, b))
        tl
    in
    ok (hd, tl)


  let bind_map_nseq f (hd, tl) =
    let* hd = f hd in
    let* tl =
      bind_map_list
        (fun a ->
          let* a = f a in
          ok a)
        tl
    in
    ok (hd, tl)


  type 'a folder =
    { e : 'a -> expr -> 'a monad
    ; t : 'a -> type_expr -> 'a monad
    ; d : 'a -> statement -> 'a monad
    }

  type mapper =
    { e : expr -> (bool * expr) monad
    ; t : type_expr -> type_expr monad
    ; d : statement -> statement monad
    }

  let rec map_type_expression : mapper -> type_expr -> 'b monad =
   fun f t ->
    let self = map_type_expression f in
    let self_variant = map_variant f in
    let* t = f.t t in
    let return = ok in
    match t with
    | TProd { inside = { value; region }; attributes } ->
      let* inside = bind_map_npseq self value.inside in
      let value = { value with inside } in
      return @@ TProd { inside = { value; region }; attributes }
    | TSum { value; region } ->
      let* variants = bind_map_npseq self_variant value.variants.value in
      let value = { value with variants = { value.variants with value = variants } } in
      return @@ TSum { value; region }
    | TObject { value; region } ->
      let aux (element : _ reg) =
        let* field_type = self element.value.field_type in
        let value = { element.value with field_type } in
        ok @@ { element with value }
      in
      let* ne_elements = bind_map_npseq aux value.ne_elements in
      let value = { value with ne_elements } in
      return @@ TObject { value; region }
    | TApp { value; region } ->
      let const, tuple = value in
      let* inside = bind_map_npseq self tuple.value.inside in
      let tuple = { tuple with value = { tuple.value with inside } } in
      let value = const, tuple in
      return @@ TApp { value; region }
    | TFun { value; region } ->
      let map_fun_type_arg (f : fun_type_arg) =
        let* type_expr = self f.type_expr in
        ok { f with type_expr }
      in
      let ty1, wild, ty2 = value in
      let* ty1 =
        let* inside = bind_map_npseq map_fun_type_arg ty1.inside in
        ok { lpar = ty1.lpar; inside; rpar = ty1.rpar }
      in
      let* ty2 = self ty2 in
      let value = ty1, wild, ty2 in
      return @@ TFun { value; region }
    | TPar { value; region } ->
      let* inside = self value.inside in
      let value = { value with inside } in
      return @@ TPar { value; region }
    | TDisc t ->
      let field (element : field_decl reg) =
        let* field_type = self element.value.field_type in
        let value = { element.value with field_type } in
        ok @@ { element with value }
      in
      let obj (element : field_decl reg ne_injection reg) =
        let* ne_elements = bind_map_npseq field element.value.ne_elements in
        let element = { element with value = { element.value with ne_elements } } in
        (* let* value = bind_map_npseq field element.value in  *)
        ok @@ element
      in
      let* t = bind_map_npseq obj t in
      return @@ TDisc t
    | TParameter { value; region } -> return @@ TParameter { value; region }
    | (TVar _ | TModA _ | TInt _ | TString _) as e -> ok e


  and map_variant : mapper -> variant reg -> variant reg monad =
   fun f (v : variant reg) : variant reg monad ->
    let self_type = map_type_expression f in
    let value = v.value in
    let tuple = value.tuple in
    let tuple_value = tuple.value in
    let inside = tuple_value.inside in
    let params = v.value.tuple.value.inside.params in
    let* params =
      match params with
      | Some (comma, params) ->
        let* params = bind_map_npseq self_type params in
        ok @@ Some (comma, params)
      | None -> ok params
    in
    let inside = { inside with params } in
    let tuple_value = { tuple_value with inside } in
    let tuple = { tuple with value = tuple_value } in
    let value = { value with tuple } in
    let variant_reg = { v with value } in
    ok variant_reg


  let rec map_expression : mapper -> expr -> expr monad =
   fun f e ->
    let self_type = map_type_expression f in
    let self_statement = map_statement f in
    let return = ok in
    let* b, e = f.e e in
    let self = if b then map_expression f else ok in
    let bin_op value =
      let { op; arg1; arg2 } = value in
      let* arg1 = self arg1 in
      let* arg2 = self arg2 in
      ok { op; arg1; arg2 }
    in
    match e with
    | EFun { value; region } ->
      let map_fun_expr_body = function
        | FunctionBody { value; region } ->
          let* inside = bind_map_npseq self_statement value.inside in
          ok @@ FunctionBody { value = { value with inside }; region }
        | ExpressionBody e ->
          let* e = self e in
          ok @@ ExpressionBody e
      in
      let* parameters = self value.parameters in
      let map_lhs_type (c, t) =
        let* t = self_type t in
        ok (c, t)
      in
      let* lhs_type = bind_map_option map_lhs_type value.lhs_type in
      let* body = map_fun_expr_body value.body in
      let value =
        { parameters
        ; lhs_type
        ; arrow = value.arrow
        ; body
        ; type_params = value.type_params
        }
      in
      return @@ EFun { value; region }
    | EPar { value; region } ->
      let* inside = self value.inside in
      let value = { value with inside } in
      return @@ EPar { value; region }
    | ESeq { value; region } ->
      let* value = bind_map_npseq self value in
      return @@ ESeq { value; region }
    | EVar v -> return @@ EVar v
    | EModA a -> return @@ EModA a
    | ELogic (BoolExpr (Or { value; region })) ->
      let* value = bin_op value in
      return @@ ELogic (BoolExpr (Or { value; region }))
    | ELogic (BoolExpr (And { value; region })) ->
      let* value = bin_op value in
      return @@ ELogic (BoolExpr (And { value; region }))
    | ELogic (BoolExpr (Not { value; region })) ->
      let* arg = self value.arg in
      let value = { value with arg } in
      return @@ ELogic (BoolExpr (Not { value; region }))
    | ELogic (CompExpr (Lt { value; region })) ->
      let* value = bin_op value in
      return @@ ELogic (CompExpr (Lt { value; region }))
    | ELogic (CompExpr (Leq { value; region })) ->
      let* value = bin_op value in
      return @@ ELogic (CompExpr (Leq { value; region }))
    | ELogic (CompExpr (Gt { value; region })) ->
      let* value = bin_op value in
      return @@ ELogic (CompExpr (Gt { value; region }))
    | ELogic (CompExpr (Geq { value; region })) ->
      let* value = bin_op value in
      return @@ ELogic (CompExpr (Geq { value; region }))
    | ELogic (CompExpr (Equal { value; region })) ->
      let* value = bin_op value in
      return @@ ELogic (CompExpr (Equal { value; region }))
    | ELogic (CompExpr (Neq { value; region })) ->
      let* value = bin_op value in
      return @@ ELogic (CompExpr (Neq { value; region }))
    | EArith (Add { value; region }) ->
      let* value = bin_op value in
      return @@ EArith (Add { value; region })
    | EArith (Sub { value; region }) ->
      let* value = bin_op value in
      return @@ EArith (Sub { value; region })
    | EArith (Mult { value; region }) ->
      let* value = bin_op value in
      return @@ EArith (Mult { value; region })
    | EArith (Div { value; region }) ->
      let* value = bin_op value in
      return @@ EArith (Div { value; region })
    | EArith (Mod { value; region }) ->
      let* value = bin_op value in
      return @@ EArith (Mod { value; region })
    | EArith (Neg { value; region }) ->
      let* arg = self value.arg in
      let value = { value with arg } in
      return @@ EArith (Neg { value; region })
    | EArith (Int _) -> return @@ e
    (* | EArith Nat   _ *)
    (* | EArith Mutez _ as e  *)
    | ECall { value = e, a; region } ->
      let map_arguments = function
        | Multiple { value; region } ->
          let* inside = bind_map_npseq self value.inside in
          let value = { value with inside } in
          ok @@ Multiple { value; region }
        | Unit _ as u -> ok u
      in
      let* e = self e in
      let* a = map_arguments a in
      let value = e, a in
      return @@ ECall { value; region }
    | EBytes _ as e -> return @@ e
    | EArray { value; region } ->
      let map_array_item = function
        | Expr_entry e ->
          let* e = self e in
          ok @@ Expr_entry e
        | Rest_entry { value; region } ->
          let* expr = self value.expr in
          ok @@ Rest_entry { value = { value with expr }; region }
      in
      let* inside =
        match value.inside with
        | Some inside ->
          let* inside = bind_map_npseq map_array_item inside in
          ok @@ Some inside
        | None -> ok None
      in
      let value = { value with inside } in
      return @@ EArray { value; region }
    | EObject { value; region } ->
      let map_property = function
        | Punned_property { value; region } ->
          let* value = self value in
          ok @@ Punned_property { value; region }
        | Property { value; region } ->
          let* name = self value.name in
          let* value2 = self value.value in
          ok @@ Property { value = { value with name; value = value2 }; region }
        | Property_rest { value; region } ->
          let* expr = self value.expr in
          ok @@ Property_rest { value = { value with expr }; region }
      in
      let* inside = bind_map_npseq map_property value.inside in
      let value = { value with inside } in
      return @@ EObject { value; region }
    | EString _ as e -> return @@ e
    | EProj { value; region } ->
      let map_selection = function
        | FieldName _ as f -> ok f
        | Component { value; region } ->
          let* inside = self value.inside in
          ok @@ Component { value = { value with inside }; region }
      in
      let* expr = self value.expr in
      let* selection = map_selection value.selection in
      let value = { expr; selection } in
      return @@ EProj { value; region }
    | EAssign (a, e, b) ->
      let* a = self a in
      let* b = self b in
      return @@ EAssign (a, e, b)
    | EAnnot { value; region } ->
      let a, e, b = value in
      let* a = self a in
      let* b = self_type b in
      return @@ EAnnot { value = a, e, b; region }
    | EUnit _ as u -> return @@ u
    | ECodeInj { value; region } ->
      let* code = self value.code in
      let value = { value with code } in
      return @@ ECodeInj { value; region }
    | EConstr { value; region } ->
      let const, expr = value in
      let* expr = bind_map_option self expr in
      let value = const, expr in
      return @@ EConstr { value; region }
    | ETernary { value; region } ->
      let* condition = self value.condition in
      let* truthy = self value.truthy in
      let* falsy = self value.falsy in
      return @@ ETernary { value = { value with condition; truthy; falsy }; region }
    | EContract { value; region } -> return @@ EContract { value; region }


  and map_statement : mapper -> statement -> statement monad =
   fun f s ->
    let self = map_statement f in
    let self_expr = map_expression f in
    let self_type = map_type_expression f in
    let return = ok in
    match s with
    | SBlock { value; region } ->
      let* inside = bind_map_npseq self value.inside in
      return @@ SBlock { value = { value with inside }; region }
    | SExpr (attributes, e) ->
      let* e = self_expr e in
      return @@ SExpr (attributes, e)
    | SCond { value; region } ->
      let { attributes; kwd_if; test = { inside; lpar; rpar }; ifso; ifnot } = value in
      let* inside = self_expr inside in
      let* ifso = self ifso in
      let map_ifnot (else_, statement) =
        let* statement = self statement in
        ok (else_, statement)
      in
      let* ifnot = bind_map_option map_ifnot ifnot in
      let value = { attributes; kwd_if; test = { inside; lpar; rpar }; ifso; ifnot } in
      return @@ SCond { value; region }
    | SReturn { value = { kwd_return; expr }; region } ->
      let* expr = bind_map_option self_expr expr in
      return @@ SReturn { value = { kwd_return; expr }; region }
    | SLet { value; region } ->
      let map_lhs_type (c, t) =
        let* t = self_type t in
        ok (c, t)
      in
      let map_binding ({ value; region } : val_binding Region.reg) =
        let* lhs_type = bind_map_option map_lhs_type value.lhs_type in
        let* expr = self_expr value.expr in
        ok ({ value = { value with lhs_type; expr }; region } : val_binding Region.reg)
      in
      let* bindings = bind_map_npseq map_binding value.bindings in
      return @@ SLet { value = { value with bindings }; region }
    | SConst { value; region } ->
      let map_lhs_type (c, t) =
        let* t = self_type t in
        ok (c, t)
      in
      let map_binding ({ value; region } : val_binding Region.reg) =
        let* lhs_type = bind_map_option map_lhs_type value.lhs_type in
        let* expr = self_expr value.expr in
        ok ({ value = { value with lhs_type; expr }; region } : val_binding Region.reg)
      in
      let* bindings = bind_map_npseq map_binding value.bindings in
      return @@ SConst { value = { value with bindings }; region }
    | SType { value; region } ->
      let* type_expr = self_type value.type_expr in
      return @@ SType { value = { value with type_expr }; region }
    | SSwitch { value; region } ->
      let map_case = function
        | Switch_case c ->
          let* expr = self_expr c.expr in
          let* statements = bind_map_option (bind_map_npseq self) c.statements in
          ok @@ Switch_case { c with expr; statements }
        | Switch_default_case d ->
          let* statements = bind_map_option (bind_map_npseq self) d.statements in
          ok @@ Switch_default_case { d with statements }
      in
      let* expr = self_expr value.expr in
      let* cases = bind_map_ne_list map_case value.cases in
      return @@ SSwitch { value = { value with expr; cases }; region }
    | SBreak b -> return @@ SBreak b
    | SNamespace { value; region } ->
      let kwd_namespace, name, statements, attributes = value in
      let ({ value = statements_value; region = statements_region }
            : statements braces reg)
        =
        statements
      in
      let* inside = bind_map_npseq self statements_value.inside in
      let statements : statements braces reg =
        { value = { statements_value with inside }; region = statements_region }
      in
      let value = kwd_namespace, name, statements, attributes in
      return @@ SNamespace { value; region }
    | SExport { value; region } ->
      let kwd_export, statement = value in
      let* statement = self statement in
      return @@ SExport { value = kwd_export, statement; region }
    | SImport i -> return @@ SImport i
    | SForOf { value; region } ->
      let* expr = self_expr value.expr in
      let* statement = self value.statement in
      return @@ SForOf { value = { value with expr; statement }; region }
    | SWhile { value; region } ->
      let* expr = self_expr value.expr in
      let* statement = self value.statement in
      return @@ SWhile { value = { value with expr; statement }; region }


  and map_toplevel_statement f = function
    | TopLevel (statement, terminator) ->
      let stmt = map_statement f statement in
      map (fun stmt -> TopLevel (stmt, terminator)) stmt
    | Directive _ as d -> ok d


  and map_module : mapper -> Cst.Jsligo.t -> Cst.Jsligo.t monad =
   fun f { statements; eof } ->
    let self = map_toplevel_statement f in
    map (fun statements -> { statements; eof }) @@ bind_map_nseq self statements
end
