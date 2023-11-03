module CST = Cst.Jsligo
module AST = Ast_unified
module Helpers = Unification_shared.Helpers
open Simple_utils
open Lexing_jsligo.Token
module Value_escaped_var = Nano_prim.Value_escaped_var
module Ty_escaped_var = Nano_prim.Ty_escaped_var

let ghost_var v = ghost_ident (Format.asprintf "%a" AST.Variable.pp v)

let decompile_var_esc = function
  | Value_escaped_var.Raw v -> CST.Var (ghost_var v)
  | Esc v -> Esc (ghost_var v)


let ghost_tvar v = ghost_ident (Format.asprintf "%a" AST.Ty_variable.pp v)

let decompile_tvar_esc = function
  | Ty_escaped_var.Raw v -> CST.T_Var (Var (ghost_tvar v))
  | Esc v -> T_Var (Esc (ghost_tvar v))


let decompile_tvar : AST.Ty_variable.t -> CST.type_expr =
 fun t -> T_Var (Var (ghost_tvar t))


let decompile_tvar_into_var v = CST.Var (ghost_tvar v)

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
    ; sig_expr
    ; sig_entry
    }


and decompile_program p = AST.Catamorphism.cata_program ~f:folder p
and decompile_expression e = AST.Catamorphism.cata_expr ~f:folder e
and decompile_type_expression e = AST.Catamorphism.cata_ty_expr ~f:folder e
and decompile_pattern p = AST.Catamorphism.cata_pattern ~f:folder p
and decompile_sig_expr p = AST.Catamorphism.cata_sig_expr ~f:folder p

(* Helpers *)
and decompile_mvar x = ghost_ident @@ Format.asprintf "%a" AST.Mod_variable.pp x
and decompile_var v = CST.Var (ghost_ident @@ Format.asprintf "%a" AST.Variable.pp v)
(* and decompile_tvar v = ghost_ident @@ Format.asprintf "%a" AST.Ty_variable.pp v *)


and decompile_attr : AST.Attribute.t -> CST.attribute =
 fun { key; value } -> ghost_attr key (Option.map ~f:(fun x -> Attr.String x) value)


(* ^ XXX Attr.String or Attr.Ident? *)

and sig_expr
    : (CST.intf_expr, CST.intf_entry, CST.type_expr) AST.sig_expr_ -> CST.intf_expr
  =
  let w = Region.wrap_ghost in
  function
  | { wrap_content = AST.S_body inside; location } ->
    let inside = Utils.sep_or_term_of_list ~sep:ghost_semi ~sep_or_term:`Sep inside in
    CST.(I_Body (w { lbrace = ghost_lbrace; inside; rbrace = ghost_rbrace }))
  | { wrap_content = AST.S_path lst; location } ->
    let lst = List.Ne.map decompile_mvar lst in
    let module_path_opt, property =
      let last, lst = List.Ne.rev lst in
      ( (match List.Ne.of_list_opt lst with
        | None -> None
        | Some lst -> Some (List.Ne.rev lst))
      , last )
    in
    (match module_path_opt with
    | Some namespace_path ->
      let namespace_path = Utils.nsepseq_of_nseq ~sep:ghost_dot lst in
      let xx : _ CST.namespace_path =
        CST.{ namespace_path; selector = ghost_dot; property }
      in
      let x : CST.namespace_selection = CST.M_Path (w xx) in
      CST.(I_Path x)
    | None -> CST.(I_Path (M_Alias property)))


and sig_entry
    : (CST.intf_expr, CST.intf_entry, CST.type_expr) AST.sig_entry_ -> CST.intf_entry
  =
 fun se ->
  let w = Region.wrap_ghost in
  match Location.unwrap se with
  | AST.S_value (v, ty, optional) ->
    CST.(
      I_Const
        (w
           { kwd_const = ghost_const
           ; const_name = decompile_var v
           ; const_type = ghost_colon, ty
           ; const_optional = (if optional then Some ghost_qmark else None)
           }))
  | AST.S_type (v, ty) ->
    CST.(
      I_Type
        (w
           { kwd_type = ghost_type
           ; type_name = decompile_tvar_into_var v
           ; type_rhs = Some (ghost_eq, ty)
           }))
  | AST.S_type_var v ->
    CST.(
      I_Type
        (w
           { kwd_type = ghost_type
           ; type_name = decompile_tvar_into_var v
           ; type_rhs = None
           }))
  | AST.S_attr (attr, se) -> CST.(I_Attr (decompile_attr attr, se))
  | AST.S_include _ -> failwith "Decompile: got S_include in JsLIGO"


and decompile_to_namespace_path
    : type a. AST.Mod_variable.t Simple_utils.List.Ne.t -> a -> a CST.namespace_path
  =
 fun module_path field ->
  let f v = ghost_ident @@ Format.asprintf "%a" AST.Mod_variable.pp v in
  let module_path = List.Ne.map f module_path in
  let namespace_path = Utils.nsepseq_of_nseq ~sep:ghost_dot module_path in
  (* XXX: What is [field_as_open]?? Do we expect module path with more than 1 element here? *)
  CST.{ namespace_path; selector = ghost_dot; property = field }


and decompile_to_namespace_selection
    : AST.Mod_variable.t Simple_utils.List.Ne.t -> CST.namespace_selection
  =
 fun module_path ->
  let f v = ghost_ident @@ Format.asprintf "%a" AST.Mod_variable.pp v in
  let module_path = List.Ne.map f module_path in
  let namespace_path = Utils.nsepseq_of_nseq ~sep:ghost_dot module_path in
  (* XXX: What is [field_as_open]?? Do we expect module path with more than 1 element here? *)
  match namespace_path with
  | m, [] -> M_Alias m
  | _ ->
    let last, rev_init = Utils.nsepseq_rev namespace_path in
    let rev_init =
      match rev_init with
      | [] -> assert false
      | (_, hd) :: tl -> hd, tl
    in
    let init = Utils.nsepseq_rev rev_init in
    M_Path
      (Region.wrap_ghost
         CST.{ namespace_path = init; selector = ghost_dot; property = last })


and decompile_namespace_path
    : type a.
      (AST.Mod_variable.t Simple_utils.List.Ne.t, a) AST.Mod_access.t
      -> a CST.namespace_path
  =
 fun { module_path; field; field_as_open = _ } ->
  let module_path = List.Ne.map decompile_mvar module_path in
  let namespace_path = Utils.nsepseq_of_nseq ~sep:ghost_dot module_path in
  (* XXX: What is [field_as_open]?? Do we expect module path with more than 1 element here? *)
  CST.{ namespace_path; selector = ghost_dot; property = field }


(* Decompilers: expect that all Ast nodes are initial, i.e. that
    backwards nanopasses were applied to Ast_unified before
    decompiler *)

(* TODO: The E_variable case can be removed once
   Nanopasses.Espaced_variables.decompile is implemented. *)

and expr : (CST.expr, CST.type_expr, CST.pattern, unit, unit) AST.expression_ -> CST.expr =
 fun e ->
  let w = Region.wrap_ghost in
  match Location.unwrap e with
  | E_attr (attr, e) -> E_Attr (decompile_attr attr, e)
  | E_variable v -> E_Var (Var (ghost_var v))
  | E_variable_esc v -> E_Var (decompile_var_esc v)
  | E_binary_op { operator; left; right } ->
    let binop op : 'a CST.wrap CST.bin_op CST.reg =
      w @@ CST.{ op; arg1 = left; arg2 = right }
    in
    (match Location.unwrap operator with
    | PLUS -> E_Add (binop ghost_plus)
    | MINUS -> E_Sub (binop ghost_minus)
    | STAR -> E_Mult (binop ghost_times)
    | SLASH -> E_Div (binop ghost_slash)
    | PRCENT -> E_Rem (binop ghost_rem)
    | DAMPERSAND -> E_And (binop ghost_and)
    | LT -> E_Lt (binop ghost_lt)
    | GT -> E_Gt (binop ghost_gt)
    | GE -> E_Geq (binop ghost_ge)
    | LE -> E_Leq (binop ghost_le)
    | SEQ -> E_Equal (binop ghost_eq) (* TODO: or it should be E_Assign? *)
    | LTGT -> E_Neq (binop ghost_ne)
    | DPIPE -> E_Or (binop ghost_or)
    | DEQ -> E_Equal (binop ghost_eq2)
    | EQ_SLASH_EQ -> E_Neq (binop ghost_ne)
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
    | MINUS -> E_Neg (unop ghost_minus)
    | WORD_NOT -> E_Not (unop ghost_not)
    | _ -> failwith "Impossible")
  | E_literal Literal_unit ->
    CST.E_Array
      (w CST.{ lbracket = ghost_lbracket; inside = None; rbracket = ghost_rbracket })
  | E_literal (Literal_int x) -> CST.E_Int (ghost_int x)
  | E_literal (Literal_nat x) -> CST.E_Nat (ghost_nat x)
  | E_literal (Literal_string (Standard s)) -> CST.E_String (ghost_string s)
  | E_literal (Literal_string (Verbatim v)) -> CST.E_Verbatim (ghost_verbatim v)
  | E_literal (Literal_mutez x) -> CST.E_Mutez (ghost_mutez @@ Z.to_int64 x)
  | E_module_open_in m -> E_NamePath (w @@ decompile_namespace_path m)
  | E_application { lamb; args } ->
    CST.E_App
      (w
      @@ ( lamb
         , w @@ CST.{ lpar = ghost_lpar; rpar = ghost_rpar; inside = Some (args, []) } ))
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
  (* XXX we should split generated names on '#'? Can we just extract the name somehow?
         Why [t.name] is not working?? *)
  let decompile_variant
      :  AST.Label.t -> CST.type_expr option -> AST.Attribute.t list
      -> CST.type_expr CST.legacy_variant
    =
   fun (AST.Label.Label constr_name) t attributes ->
    let ctor_params : (CST.comma * (CST.type_expr, CST.comma) Utils.nsep_or_term) option =
      Option.map
        ~f:(fun t ->
          match t with
          | T_Array { value; _ } ->
            let (CST.{ inside; _ } : (_, _) Utils.nsep_or_term CST.brackets') = value in
            ghost_comma, inside
          | t -> ghost_comma, `Sep (t, []))
        t
    in
    let ctor = ghost_string constr_name in
    let args : (CST.comma * CST.type_expr) list =
      match ctor_params with
      | None -> []
      | Some (comma, args) ->
        List.map ~f:(fun arg -> comma, arg) @@ Utils.nsep_or_term_to_list args
    in
    let inside : CST.type_expr CST.legacy_variant_args = { ctor; args } in
    let attributes = List.map ~f:decompile_attr attributes in
    let tuple = w CST.{ lbracket = ghost_lbracket; inside; rbracket = ghost_rbracket } in
    { attributes; tuple }
  and decompile_field
      : AST.Label.t -> CST.type_expr -> AST.Attribute.t list -> _ CST.property
    =
   fun (AST.Label.Label field_name) t attributes ->
    { property_id = F_Name (CST.Var (ghost_string field_name))
    ; property_rhs = Some (ghost_colon, t)
    ; attributes = List.map ~f:decompile_attr attributes
    }
  and mk_object
      :  (CST.type_expr CST.property Region.reg, CST.semi) Utils.nsepseq
      -> CST.type_expr Cst_jsligo.CST._object
    =
   fun nsepseq ->
    let inside : (CST.type_expr CST.property CST.reg, CST.property_sep) Utils.sep_or_term =
      Some (`Sep nsepseq)
    in
    w @@ CST.{ lbrace = ghost_lbrace; rbrace = ghost_rbrace; inside }
  in
  match Location.unwrap te with
  | T_attr (_attr, t) -> t (* FIXME should TAttr be added to JsLIGO CST?? *)
  | T_var t -> decompile_tvar t
  | T_var_esc t -> decompile_tvar_esc t
  | T_int (_, z) -> T_Int (ghost_int z)
  | T_string s -> T_String (ghost_string s)
  | T_module_open_in { module_path; field; field_as_open } ->
    let module_path = module_path, [] in
    let v : CST.type_expr CST.namespace_path =
      decompile_to_namespace_path module_path field
    in
    T_NamePath (w v)
  | T_module_access { module_path; field; field_as_open } ->
    let field : CST.type_expr = decompile_tvar field in
    let v : CST.type_expr CST.namespace_path =
      decompile_to_namespace_path module_path field
    in
    T_NamePath (w v)
  | T_arg s -> T_Var (Var (ghost_ident s))
  (* ^ XXX is this correct? CameLIGO has separate T_Arg in CST *)
  | T_app { constr; type_args } ->
    let params_nsepseq = Utils.nsepseq_of_nseq type_args ~sep:ghost_comma in
    let params =
      CST.{ lchevron = ghost_lt; inside = `Sep params_nsepseq; rchevron = ghost_gt }
    in
    T_App (w (constr, w params))
  | T_prod types ->
    let inside = Utils.nsepseq_of_nseq types ~sep:ghost_comma in
    let inside : (CST.type_expr, CST.comma) Utils.nsep_or_term = `Sep inside in
    let v : (CST.type_expr, CST.comma) Utils.nsep_or_term CST.brackets' =
      CST.{ lbracket = ghost_lbracket; rbracket = ghost_rbracket; inside }
    in
    T_Array (w v)
  | T_named_fun (args, t) ->
    let decompile_arg
        : CST.type_expr AST.Named_fun.fun_type_arg -> CST.fun_type_param CST.reg
      =
     fun { name; type_expr } ->
      w @@ (CST.P_Var (Var (ghost_ident name)), (ghost_colon, type_expr))
    in
    let args : CST.fun_type_params =
      match Utils.list_to_sepseq (List.map ~f:decompile_arg args) ghost_comma with
      | Some nsepseq ->
        let inside : (CST.fun_type_param CST.reg, CST.comma) Utils.sep_or_term =
          Some (`Sep nsepseq)
        in
        w @@ CST.{ rpar = ghost_rpar; inside; lpar = ghost_lpar }
      | None -> failwith "Decompiler: got a T_named_fun with no args"
    in
    T_Fun (w (args, ghost_arrow, t))
  | T_record_raw fields ->
    let f : CST.type_expr option AST.Non_linear_rows.row -> _ CST.property CST.reg =
     fun (field_name, { associated_type; attributes; _ }) ->
      match associated_type with
      | None -> failwith "Decompiler: got a field with no associated type in T_record_raw"
      | Some t -> w @@ decompile_field field_name t attributes
    in
    (match Utils.list_to_sepseq (List.map ~f fields) ghost_semi with
    | None -> failwith "Decompiler: got a T_record_raw with no fields"
    | Some nsepseq -> T_Object (mk_object nsepseq))
  | T_sum_raw variants ->
    let f : CST.type_expr option AST.Non_linear_rows.row -> CST.type_expr CST.variant_kind
      =
     fun (constr, { associated_type; attributes; _ }) ->
      Legacy (w @@ decompile_variant constr associated_type attributes)
    in
    (match Utils.list_to_sepseq (List.map ~f variants) ghost_vbar with
    | None -> failwith "Decompiler: got a T_sum_raw with no fields"
    | Some nsepseq ->
      let variant : (CST.type_expr CST.variant_kind, CST.vbar) Utils.nsep_or_pref =
        `Sep nsepseq
      in
      T_Variant (w variant))
  | T_disc_union objects ->
    let f : CST.type_expr AST.Non_linear_disc_rows.row -> CST.type_expr CST._object =
     fun (_empty_label, obj) ->
      match obj.associated_type with
      | T_Object obj -> obj
      | _ -> failwith "Decompiler: field of T_disc_union should be decompiled to TObject"
    in
    (match Utils.list_to_sepseq (List.map ~f objects) ghost_vbar with
    | None -> failwith "Decompiler: got a T_disc_union with no fields"
    | Some nsepseq ->
      let variant : (CST.type_expr Cst_jsligo.CST._object, CST.vbar) Utils.nsep_or_pref =
        `Sep nsepseq
      in
      T_Union (w variant))
  | T_sum { fields; layout = _ } ->
    (* XXX those are not initial, but backwards nanopass T_sum -> T_sum_row and
         T_record -> T_record_raw is not implemented, so we need to handle those here*)
    let f : AST.Label.t * CST.type_expr -> CST.type_expr CST.variant_kind =
     fun (constr, t) -> Legacy (w @@ decompile_variant constr (Some t) [])
    in
    let pairs =
      match Utils.list_to_sepseq (AST.Label.Map.to_alist fields) ghost_vbar with
      | None -> failwith "Decompiler: got a T_sum with no elements"
      | Some nsepseq -> Utils.nsepseq_map f nsepseq
    in
    let variant : (CST.type_expr CST.variant_kind, CST.vbar) Utils.nsep_or_pref =
      `Sep pairs
    in
    T_Variant (w variant)
  | T_record { fields; layout = _ } ->
    let f : AST.Label.t * CST.type_expr -> CST.type_expr CST.property CST.reg =
     fun (field_name, t) -> w @@ decompile_field field_name t []
    in
    (match Utils.list_to_sepseq (AST.Label.Map.to_alist fields) ghost_semi with
    | None -> failwith "Decompiler: got a T_record with no elements"
    | Some nsepseq -> T_Object (mk_object @@ Utils.nsepseq_map f nsepseq))
  | T_fun _ ->
    failwith
      "Decompiler: T_fun is not initial for JsLIGO, should be transformed to T_named_fun \
       via backwards nanopass"
  | T_contract_parameter x ->
    let namespace_path = decompile_to_namespace_selection x in
    T_ParameterOf (w CST.{ kwd_parameter_of = ghost_parameter_of; namespace_path })
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
