module CST = Cst.Jsligo
module AST = Ast_unified
module Helpers = Unification_shared.Helpers
open Simple_utils
open Lexing_jsligo.Token

let rec folder =
  let todo _ = failwith ("TODO" ^ __LOC__) in
  AST.Catamorphism.
    { expr
    ; ty_expr
    ; pattern = todo
    ; statement = todo
    ; block = todo
    ; mod_expr = todo
    ; instruction = todo
    ; declaration = todo
    ; program_entry = todo
    ; program = todo
    ; sig_expr = todo
    ; sig_entry = todo
    }


and decompile_program p = AST.Catamorphism.cata_program ~f:folder p
and decompile_expression e = AST.Catamorphism.cata_expr ~f:folder e
and decompile_type_expression e = AST.Catamorphism.cata_ty_expr ~f:folder e
and decompile_pattern p = AST.Catamorphism.cata_pattern ~f:folder p

(* Helpers *)

and decompile_attr : AST.Attribute.t -> CST.attribute =
 fun { key; value } -> ghost_attr key (Option.map ~f:(fun x -> Attr.String x) value)
(* ^ XXX Attr.String or Attr.Ident? *)


and decompile_mod_path
    : type a.
      (AST.Mod_variable.t Simple_utils.List.Ne.t, a) AST.Mod_access.t
      -> a CST.module_access
  =
 fun { module_path; field; field_as_open = _ } ->
  let f = Format.asprintf "%a" AST.Mod_variable.pp in
  let module_name =
    (* FIXME should create nested `CST.module_access` instead, this is a workaround
       created because `CST.module_access` will accept module path soon.
       It works correctly for pretty printers, but the CST itself is incorrect,
       because it contains one module name with dots instead of nested module applications *)
    ghost_ident @@ String.concat ~sep:"." @@ List.map ~f @@ Utils.nseq_to_list module_path
  in
  (* XXX: What is [field_as_open]?? Do we expect module path with more than 1 element here? *)
  CST.{ module_name; selector = ghost_dot; field }


(* Decompilers: expect that all Ast nodes are initial, i.e.
  that backwards nanopasses were applied to Ast_unified before decompiler *)

and expr : (CST.expr, CST.type_expr, CST.pattern, unit, unit) AST.expression_ -> CST.expr =
 fun e ->
  let w = Region.wrap_ghost in
  match Location.unwrap e with
  | E_attr (_attr, e) -> e (* FIXME should EAttr be added to JsLIGO CST?? *)
  | E_variable v -> EVar (ghost_ident (Format.asprintf "%a" AST.Variable.pp v))
  | E_binary_op { operator; left; right } ->
    let binop op : 'a CST.wrap CST.bin_op CST.reg =
      w @@ CST.{ op; arg1 = left; arg2 = right }
    in
    (match Location.unwrap operator with
    | PLUS -> EArith (Add (binop ghost_plus))
    | MINUS -> EArith (Sub (binop ghost_minus))
    | STAR -> EArith (Mult (binop ghost_times))
    | SLASH -> EArith (Div (binop ghost_slash))
    | PRCENT -> EArith (Mod (binop ghost_rem))
    | DAMPERSAND -> ELogic (BoolExpr (And (binop ghost_bool_and)))
    | LT -> ELogic (CompExpr (Lt (binop ghost_lt)))
    | GT -> ELogic (CompExpr (Gt (binop ghost_gt)))
    | GE -> ELogic (CompExpr (Geq (binop ghost_ge)))
    | LE -> ELogic (CompExpr (Leq (binop ghost_le)))
    | SEQ -> ELogic (CompExpr (Equal (binop ghost_eq)))
    | LTGT -> ELogic (CompExpr (Neq (binop ghost_ne)))
    | DPIPE -> ELogic (BoolExpr (Or (binop ghost_bool_or)))
    | DEQ -> ELogic (CompExpr (Equal (binop ghost_eq2)))
    | EQ_SLASH_EQ -> ELogic (CompExpr (Neq (binop ghost_ne)))
    | DCOLON
    | WORD_LSL
    | WORD_LSR
    | WORD_LOR
    | WORD_LAND
    | WORD_LXOR
    | EX_MARK
    | WORD_XOR
    | CONTAINS
    | SHARP
    | WORD_OR
    | WORD_MOD
    | WORD_NOT
    | WORD_AND
    | CARET -> failwith "Impossible")
  | E_unary_op { operator; arg } ->
    let unop op : 'a CST.wrap CST.un_op CST.reg = w @@ CST.{ op; arg } in
    (match Location.unwrap operator with
    | MINUS -> EArith (Neg (unop ghost_minus))
    | WORD_NOT -> ELogic (BoolExpr (Not (unop ghost_bool_not)))
    | _ -> failwith "Impossible")
  | E_literal Literal_unit -> CST.EUnit (w (ghost_lpar, ghost_rpar))
  | E_literal (Literal_int x) -> CST.EArith (Int (ghost_int x))
  | E_literal (Literal_nat x) ->
    CST.EAnnot
      (w @@ (CST.EArith (Int (ghost_int x)), ghost_as, CST.TVar (ghost_ident "nat")))
  | E_literal (Literal_string x) ->
    CST.EString
      (match x with
      | Standard s -> String (ghost_string s)
      | Verbatim v -> Verbatim (ghost_verbatim v))
  | E_literal (Literal_mutez x) ->
    CST.EAnnot
      (w @@ (CST.EArith (Int (ghost_int x)), ghost_as, CST.TVar (ghost_ident "tez")))
  | E_module_open_in m -> EModA (w @@ decompile_mod_path m)
  | E_application { lamb; args } ->
    CST.ECall
      (w
      @@ ( lamb
         , CST.Multiple
             (w @@ CST.{ lpar = ghost_lpar; rpar = ghost_rpar; inside = args, [] }) ))
  | expr when AST.expr_is_not_initial expr ->
    Helpers.failwith_not_initial_node_decompiler @@ `Expr e
  | _ ->
    failwith
      (Format.asprintf
         "Can't decompile this node : \n%a"
         Sexp.pp_hum
         (AST.sexp_of_expr_
            (fun _ -> Sexp.Atom "xx")
            (fun _ -> Sexp.Atom "xx")
            (fun _ -> Sexp.Atom "xx")
            (fun _ -> Sexp.Atom "xx")
            (fun _ -> Sexp.Atom "xx")
            e))


and ty_expr : CST.type_expr AST.ty_expr_ -> CST.type_expr =
 fun te ->
  let w = Region.wrap_ghost in
  (* ^ XXX we should split generated names on '#'? Can we just extract the name somehow?
        Why [t.name] is not working?? *)
  let decompile_tvar : AST.Ty_variable.t -> CST.type_expr =
   fun t -> TVar (ghost_ident @@ Format.asprintf "%a" Ligo_prim.Type_var.pp t)
   (* XXX we should split generated names on '#'? Can we just extract the name somehow?
         Why [t.name] is not working?? *)
  and decompile_variant
      : AST.Label.t -> CST.type_expr option -> AST.Attribute.t list -> CST.variant
    =
   fun (AST.Label.Label constr_name) t attributes ->
    let params =
      (* Looks like it's the correct way to decompile a constr with multiple params,
    we should add tests for this scenario *)
      match t with
      | None -> None
      | Some (TProd types) -> Some (ghost_comma, types.inside.value.inside)
      | Some t -> Some (ghost_comma, (t, []))
      (* ^ XXX leading comma? *)
    in
    let inside = CST.{ constr = ghost_ident constr_name; params } in
    { tuple = w CST.{ lbracket = ghost_lbracket; inside; rbracket = ghost_rbracket }
    ; attributes = List.map ~f:decompile_attr attributes
    }
  and decompile_field
      : AST.Label.t -> CST.type_expr -> AST.Attribute.t list -> CST.field_decl
    =
   fun (AST.Label.Label field_name) t attributes ->
    { field_name = ghost_ident field_name
    ; colon = ghost_colon
    ; field_type = t
    ; attributes = List.map ~f:decompile_attr attributes
    }
  and mk_object : (CST.field_decl CST.reg, CST.semi) Utils.nsepseq -> CST.obj_type =
   fun nsepseq ->
    w
    @@ CST.
         { compound = Some braces_compound
         ; ne_elements = nsepseq
         ; terminator = None
         ; attributes = []
         }
  in
  match Location.unwrap te with
  | T_attr (_attr, t) -> t (* FIXME should TAttr be added to JsLIGO CST?? *)
  | T_var v -> decompile_tvar v
  | T_int (_, z) -> TInt (ghost_int z)
  | T_string s -> TString (ghost_string s)
  | T_module_open_in { module_path; field; field_as_open } ->
    TModA (w @@ decompile_mod_path { module_path = module_path, []; field; field_as_open })
  | T_module_access { module_path; field; field_as_open } ->
    TModA
      (w
      @@ decompile_mod_path { module_path; field = decompile_tvar field; field_as_open })
  | T_arg s -> TVar (ghost_ident s)
  (* ^ XXX is this correct? CameLIGO has separate T_Arg in CST *)
  | T_app { constr; type_args } ->
    let params_nsepseq = Utils.nsepseq_of_nseq type_args ~sep:ghost_comma in
    let params =
      CST.{ lchevron = ghost_lt; inside = params_nsepseq; rchevron = ghost_gt }
    in
    let constr =
      match constr with
      | TVar v -> v
      | _ -> failwith "Decompiler: T_app 's first argument decompiled not to a TVar"
      (* XXX CST constr shpould be a ty_expr, not just a string, cause it can be a module access *)
    in
    TApp (w (constr, w params))
  | T_prod types ->
    TProd
      { attributes = []
      ; inside =
          w
            CST.
              { lbracket = ghost_lbracket
              ; inside = Utils.nsepseq_of_nseq types ~sep:ghost_comma
              ; rbracket = ghost_rbracket
              }
      }
  | T_named_fun (args, f) ->
    let decompile_arg : CST.type_expr AST.Named_fun.fun_type_arg -> CST.fun_type_arg =
     fun { name; type_expr } ->
      { name = ghost_ident @@ name; colon = ghost_colon; type_expr }
    in
    let args =
      match Utils.list_to_nsepseq_opt (List.map ~f:decompile_arg args) ghost_comma with
      | Some nsepseq -> CST.{ rpar = ghost_rpar; inside = nsepseq; lpar = ghost_lpar }
      | None -> failwith "Decompiler: got a T_named_fun with no args"
    in
    TFun (w (args, ghost_arrow, f))
  | T_record_raw fields ->
    let f : CST.type_expr option AST.Non_linear_rows.row -> CST.field_decl CST.reg =
     fun (field_name, { associated_type; attributes; _ }) ->
      match associated_type with
      | None -> failwith "Decompiler: got a field with no associated type in T_record_raw"
      | Some t -> w @@ decompile_field field_name t attributes
    in
    (match Utils.list_to_nsepseq_opt (List.map ~f fields) ghost_semi with
    | None -> failwith "Decompiler: got a T_record_raw with no fields"
    | Some nsepseq -> TObject (mk_object nsepseq))
  | T_sum_raw variants ->
    let f : CST.type_expr option AST.Non_linear_rows.row -> CST.variant CST.reg =
     fun (constr, { associated_type; attributes; _ }) ->
      w @@ decompile_variant constr associated_type attributes
    in
    (match Utils.list_to_nsepseq_opt (List.map ~f variants) ghost_vbar with
    | None -> failwith "Decompiler: got a T_sum_raw with no fields"
    | Some nsepseq ->
      TSum
        (w
        @@ CST.{ leading_vbar = Some ghost_vbar; variants = w nsepseq; attributes = [] }))
  | T_disc_union objects ->
    let f : CST.type_expr AST.Non_linear_disc_rows.row -> CST.obj_type =
     fun (_empty_label, obj) ->
      match obj.associated_type with
      | TObject obj -> obj
      | _ -> failwith "Decompiler: field of T_disc_union should be decompiled to TObject"
    in
    (match Utils.list_to_nsepseq_opt (List.map ~f objects) ghost_vbar with
    | None -> failwith "Decompiler: got a T_disc_union with no fields"
    | Some nsepseq -> TDisc nsepseq)
  | T_sum { fields; layout = _ } ->
    (* XXX those are not initial, but backwards nanopass T_sum -> T_sum_row and
         T_record -> T_record_raw is not implemented, so we need to handle those here*)
    let f : AST.Label.t * CST.type_expr -> CST.variant CST.reg =
     fun (constr, t) -> w @@ decompile_variant constr (Some t) []
    in
    let pairs =
      match Utils.list_to_sepseq (AST.Label.Map.to_alist fields) ghost_vbar with
      | None -> failwith "Decompiler: got a T_sum with no elements"
      | Some nsepseq -> Utils.nsepseq_map f nsepseq
    in
    TSum (w @@ CST.{ leading_vbar = Some ghost_vbar; variants = w pairs; attributes = [] })
  | T_record { fields; layout = _ } ->
    let f : AST.Label.t * CST.type_expr -> CST.field_decl CST.reg =
     fun (field_name, t) -> w @@ decompile_field field_name t []
    in
    (match Utils.list_to_sepseq (AST.Label.Map.to_alist fields) ghost_semi with
    | None -> failwith "Decompiler: got a T_record with no elements"
    | Some nsepseq -> TObject (mk_object @@ Utils.nsepseq_map f nsepseq))
  | T_fun _ ->
    failwith
      "Decompiler: T_fun is not initial for JsLIGO, should be transformed to T_named_fun \
       via backwards nanopass"
  (* This node is not initial,
  i.e. types like [∀ a : * . option (a) -> bool] can not exist at Ast_unified level,
  so type declaration that contains expression with abstraction should be transformed to
  D_type_abstraction by type_abstraction_declaration nanopass, so this case looks impossible,
  but in some cases (e.g. LSP hovers) we just want to transform type expression to pretty string,
  so we'll just drop the quantifiers here *)
  | T_abstraction Ligo_prim.Abstraction.{ ty_binder = _; kind = _; type_ }
  | T_for_all Ligo_prim.Abstraction.{ ty_binder = _; kind = _; type_ } -> type_
  | T_module_app _ | T_constant _ ->
    Helpers.failwith_not_initial_node_decompiler @@ `Ty_expr te


and bracket_compound = CST.Brackets (ghost_lbracket, ghost_rbracket)
and braces_compound = CST.Braces (ghost_lbrace, ghost_rbrace)
