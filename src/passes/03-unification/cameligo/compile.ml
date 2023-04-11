open Simple_utils.Utils

(* open Simple_utils.Trace *)
open Unification_shared.Helpers
module CST = Cst.Cameligo
module AST = Ast_unified
module Option = Simple_utils.Option
open AST (* Brings types and combinators functions *)

module TODO_do_in_parsing = struct
  let _r_split = r_split (* could compute Location directly in Parser *)
  let var ~loc (var : string) = Ligo_prim.Value_var.of_input_var ~loc var
  let tvar ~loc (var : string) = Ligo_prim.Type_var.of_input_var ~loc var
  let mvar ~loc (var : string) = Ligo_prim.Module_var.of_input_var ~loc var
  let labelize x = Label.of_string x
  let quoted_tvar (x : CST.type_var) = tvar ~loc:(r_snd x.name) ("'" ^ x.name.value)

  let type_vars_to_lst (p : CST.type_vars) : AST.Ty_variable.t List.Ne.t =
    match p with
    | QParam x -> List.Ne.singleton (quoted_tvar (r_fst x))
    | QParamTuple x ->
      nseq_map (fun x -> quoted_tvar (r_fst x)) (nsepseq_to_nseq x.value.inside)


  let translate_selection (sel : CST.selection) : _ AST.Selection.t =
    (* could directly be a name for FieldName ?*)
    match sel with
    | FieldName name ->
      let name, _ = r_split name in
      FieldName (Label.of_string name)
    | Component comp ->
      let index, _ = r_split comp in
      Component_num index


  let flatten_moda ({ module_name; selector = _; field } : CST.expr CST.module_access) =
    let rec aux
        : CST.module_name List.Ne.t -> CST.expr -> CST.module_name List.Ne.t * CST.expr
      =
     fun acc expr ->
      match expr with
      | EModA { value = { module_name; field; _ }; _ } ->
        aux (List.Ne.append acc (List.Ne.singleton module_name)) field
      | _ -> acc, expr
    in
    let path, field = aux (List.Ne.singleton module_name) field in
    let is_open =
      match field with
      | CST.EPar _ -> true
      | _ -> false
    in
    (path, field), is_open


  let field_as_open_t field =
    match field with
    | CST.TPar _ -> true
    | _ -> false


  let weird_attributes _ =
    (* I don't know what to do with those attributes *)
    ()


  let empty_sequence () = failwith "should a sequence be allowed to be empty ?"
end

module TODO_unify_in_cst = struct
  let conv_attr (attr : CST.attribute list) : (AST.Attribute.t * Location.t) list =
    List.map attr ~f:(fun attr_reg ->
        let (key, value_opt), loc = w_split attr_reg in
        let value : string option =
          Option.map
            ~f:(function
              | String x -> x
              | Ident x -> x)
            value_opt
        in
        Nano_prim.Attribute.{ key; value }, loc)


  let attach_attr (attr : CST.attribute list) (e : AST.expr) : AST.expr =
    List.fold (conv_attr attr) ~init:e ~f:(fun e (attr, loc) -> e_attr ~loc (attr, e))


  let d_attach_attr (attr : CST.attribute list) (e : AST.declaration) : AST.declaration =
    List.fold (conv_attr attr) ~init:e ~f:(fun e (attr, loc) -> d_attr ~loc (attr, e))


  let t_attach_attr (attr : CST.attribute list) (e : AST.ty_expr) : AST.ty_expr =
    List.fold (conv_attr attr) ~init:e ~f:(fun e (attr, loc) -> t_attr ~loc attr e)


  let compile_rows = Non_linear_rows.make
  let _compile_disc_rows = Non_linear_disc_rows.make

  let type_operator ~loc v =
    (* could be a type expr ? or we could emit a type variable expression ? *)
    t_var ~loc (TODO_do_in_parsing.tvar ~loc v)


  let module_alias ~loc alias binders =
    let loc_path =
      List.fold (nseq_to_list binders) ~init:Location.generated ~f:(fun acc x ->
          Location.cover acc (Ligo_prim.Module_var.get_location x))
    in
    d_module { name = alias; mod_expr = m_path ~loc:loc_path binders } ~loc


  let e_string ~loc s =
    e_literal ~loc (Literal_string (Simple_utils.Ligo_string.Standard s))


  let e_verbatim ~loc s =
    e_literal ~loc (Literal_string (Simple_utils.Ligo_string.Verbatim s))


  let nested_proj : CST.projection CST.reg -> AST.expr =
   fun x ->
    let CST.{ struct_name; selector = _; field_path }, loc = r_split x in
    (* projections could be nested ? as in jsligo *)
    let init =
      let name, loc = r_split struct_name in
      e_variable (TODO_do_in_parsing.var ~loc name) ~loc
    in
    List.fold (nsepseq_to_list field_path) ~init ~f:(fun acc -> function
      | FieldName name ->
        let name, _loc = r_split name in
        e_proj ~loc { struct_ = acc; path = FieldName (Label.of_string name) }
      | Component comp ->
        let index, _loc = r_split comp in
        e_proj ~loc { struct_ = acc; path = Component_num index })


  let update_lhs : CST.path -> AST.expr = function
    (* one day, update lhs should be an expression *)
    | Name name ->
      let loc = r_snd name in
      e_variable ~loc (TODO_do_in_parsing.var ~loc (r_fst name))
    | Path v -> nested_proj v


  let update_rhs
      :  (CST.expr -> AST.expr)
      -> CST.field_path_assignment Region.reg CST.ne_injection Region.reg
      -> AST.expr AST.Update.field list
    =
   fun self
       { region = _; value = { compound = _; ne_elements; terminator = _; attributes } } ->
    let attributes = List.map (conv_attr attributes) ~f:fst in
    TODO_do_in_parsing.weird_attributes attributes;
    let x = nsepseq_to_list ne_elements in
    let f : CST.field_path_assignment Region.reg -> AST.expr AST.Update.field =
     fun fpa ->
      let fpa, _loc = r_split fpa in
      match fpa with
      | Path_property { field_path; assignment = _; field_expr } ->
        let field_rhs = self field_expr in
        let field_lhs =
          match field_path with
          | Name v -> [ Selection.FieldName (Label.of_string (r_fst v)) ]
          | Path { region = _; value = { struct_name; selector = _; field_path } } ->
            Selection.FieldName (Label.of_string (r_fst struct_name))
            :: List.map
                 (nsepseq_to_list field_path)
                 ~f:TODO_do_in_parsing.translate_selection
        in
        Full_field { field_lhs; field_lens = Lens_Id; field_rhs }
      | Path_punned_property pun ->
        let label = Label.of_string (r_fst pun) in
        Pun (Location.wrap ~loc:(r_snd pun) label)
    in
    List.map x ~f


  let fun_binder : AST.pattern list -> pattern AST.Param.t list =
   fun ps ->
    (* use the same type as in other CST's *)
    List.map ~f:(fun pattern -> AST.Param.{ param_kind = `Const; pattern }) ps


  let mod_in_as_mod_expr binders =
    (* having a module expression as mod_in rhs would be cool *)
    m_path
      ~loc:Location.generated
      (nseq_map
         (fun x -> TODO_do_in_parsing.mvar ~loc:(r_snd x) (r_fst x))
         (nsepseq_to_nseq binders))


  let nested_ctor_application ~loc (constr : CST.constr) arg_opt =
    let constructor = Label.of_string constr.value in
    let element = Option.map ~f:List.Ne.singleton arg_opt in
    e_ctor_app ~loc (e_constr ~loc:(Location.lift constr.region) constructor, element)


  let declarations_as_program decls =
    make_prg (List.Ne.to_list @@ nseq_map pe_declaration decls)
end

let rec compile_type_expression : CST.type_expr -> AST.ty_expr =
 fun te ->
  let self = compile_type_expression in
  match te with
  | TProd t ->
    let t, loc = r_split t in
    let t = List.Ne.map self @@ nsepseq_to_nseq t in
    t_prod t ~loc
  | TSum t ->
    let CST.{ variants; attributes; _ }, loc = r_split t in
    let variants =
      let compile_variant CST.{ constr; arg; attributes } =
        ( TODO_do_in_parsing.labelize (r_fst constr)
        , Option.map ~f:(self <@ snd) arg
        , List.map (TODO_unify_in_cst.conv_attr attributes) ~f:fst )
      in
      let lst = List.map (nsepseq_to_list variants) ~f:(compile_variant <@ r_fst) in
      TODO_unify_in_cst.compile_rows lst
    in
    TODO_unify_in_cst.t_attach_attr attributes (t_sum_raw variants ~loc)
  | TRecord t ->
    let CST.{ attributes; ne_elements; compound = _; terminator = _ }, loc = r_split t in
    let fields =
      let field_decls : CST.field_decl nseq =
        nseq_map r_fst @@ nsepseq_to_nseq ne_elements
      in
      (* let open Ligo_prim in *)
      let compile_field_decl
          : int -> CST.field_decl -> AST.ty_expr option Non_linear_rows.row
        =
       fun i { field_name; field_type; attributes; _ } ->
        let l = TODO_do_in_parsing.labelize (r_fst field_name) in
        let rows =
          Non_linear_rows.
            { decl_pos = i
            ; associated_type = Some (self field_type)
            ; attributes = List.map (TODO_unify_in_cst.conv_attr attributes) ~f:fst
            }
        in
        l, rows
      in
      List.mapi ~f:compile_field_decl (nseq_to_list field_decls)
    in
    TODO_unify_in_cst.t_attach_attr attributes (t_record_raw ~loc fields)
  | TApp t ->
    let t, loc = r_split t in
    let constr, args = t in
    let constr = TODO_unify_in_cst.type_operator ~loc:(r_snd constr) (r_fst constr) in
    let type_args : ty_expr nseq =
      match args with
      | CST.CArg te -> List.Ne.singleton (self te)
      | CST.CArgTuple tes -> List.Ne.map self @@ nsepseq_to_nseq (r_fst tes).inside
    in
    t_app { constr; type_args } ~loc
  | TFun t ->
    let (te1, _, te2), loc = r_split t in
    let te1 = self te1 in
    let te2 = self te2 in
    t_fun ~loc (te1, te2)
  | TPar t -> self (r_fst t).inside
  | TVar t ->
    let t, loc = r_split t in
    t_var (TODO_do_in_parsing.tvar ~loc t) ~loc
  | TString t ->
    let t, loc = r_split t in
    t_string t ~loc
  | TInt t ->
    let (s, z), loc = r_split t in
    t_int s z ~loc
  | TModA t ->
    let (t : CST.type_expr CST.module_access), loc = r_split t in
    let module_path =
      let x, loc = r_split t.module_name in
      TODO_do_in_parsing.mvar ~loc x
    in
    let field_as_open = TODO_do_in_parsing.field_as_open_t t.field in
    let field = self t.field in
    t_module_open_in { module_path; field; field_as_open } ~loc
  | TArg t ->
    let t, loc = r_split t in
    let t = r_fst t.name in
    t_arg t ~loc
  | TParameter { value; region } ->
    let loc = Location.lift region in
    t_module_access
      ~loc
      { module_path =
          (nseq_map (fun x ->
               let x, loc = r_split x in
               TODO_do_in_parsing.mvar ~loc x)
          <@ nsepseq_to_nseq)
            value
      ; field = Ligo_prim.Type_var.of_input_var ~loc "$parameter"
      ; field_as_open = false
      }


(* ========================== PATTERNS ===================================== *)

let rec compile_pattern : CST.pattern -> AST.pattern =
 fun p ->
  let self = compile_pattern in
  match p with
  | PConstr p ->
    let (ctor, p_opt), loc = r_split p in
    let ctor = TODO_do_in_parsing.labelize ctor.value in
    let p_opt = Option.map ~f:self p_opt in
    p_variant ~loc ctor p_opt
  | PUnit p ->
    let _, loc = r_split p in
    p_unit ~loc
  | PVar p ->
    let p, loc = r_split p in
    let v = r_fst p.variable in
    p_var ~loc (TODO_do_in_parsing.var ~loc v)
  | PInt p ->
    let (_s, z), loc = r_split p in
    p_literal ~loc (Literal_int z)
  | PNat p ->
    let (_s, z), loc = r_split p in
    p_literal ~loc (Literal_nat z)
  | PBytes p ->
    let (_s, hex), loc = r_split p in
    let bytes_ = Hex.to_bytes hex in
    p_literal ~loc (Literal_bytes bytes_)
  | PString p ->
    let s, loc = r_split p in
    p_literal ~loc (Literal_string (Simple_utils.Ligo_string.standard s))
  | PVerbatim p ->
    let s, loc = r_split p in
    p_literal ~loc (Literal_string (Simple_utils.Ligo_string.verbatim s))
  | PList p ->
    let p, loc =
      match p with
      | CST.PListComp p ->
        let p, loc = r_split p in
        let ps = List.map ~f:self (sepseq_to_list p.elements) in
        List ps, loc
      | CST.PCons p ->
        let (p1, _, p2), loc = r_split p in
        let p1 = self p1 in
        let p2 = self p2 in
        Cons (p1, p2), loc
    in
    p_list ~loc p
  | PTuple p ->
    let p, loc = r_split p in
    let p = List.map ~f:self (nsepseq_to_list p) in
    p_tuple ~loc p
  | PPar p -> self (r_fst p).inside
  | PRecord p ->
    let p, loc = r_split p in
    let p =
      let compile_field_pattern : CST.field_pattern -> (Label.t, AST.pattern) Field.t =
       fun fp ->
        Complete (TODO_do_in_parsing.labelize (r_fst fp.field_name), self fp.pattern)
      in
      List.map ~f:(compile_field_pattern <@ r_fst) (nsepseq_to_list p.ne_elements)
    in
    p_pun_record ~loc p
  | PTyped p ->
    let p, loc = r_split p in
    let ty = compile_type_expression p.type_expr in
    let p = self p.pattern in
    p_typed ~loc ty p


(* ========================== EXPRESSIONS ================================== *)

let rec compile_expression : CST.expr -> AST.expr =
 fun e ->
  let self = compile_expression in
  let return e = e in
  let compile_bin_op (sign : AST.Operators.op) (op : _ CST.bin_op CST.reg) =
    let CST.{ op; arg1; arg2 }, loc = r_split op in
    let _, _loc = w_split op in
    e_binary_op
      ~loc
      AST.{ operator = Location.wrap ~loc sign; left = self arg1; right = self arg2 }
  in
  let compile_unary_op (sign : AST.Operators.op)
      : string CST.wrap CST.un_op CST.reg -> AST.expr
    =
   fun op ->
    let CST.{ op; arg }, loc = r_split op in
    let _, _loc = w_split op in
    e_unary_op ~loc AST.{ operator = Location.wrap ~loc sign; arg = self arg }
  in
  let translate_field_assign (fa : CST.field_assign) : (_, AST.expr) AST.Field.t =
    match fa with
    | CST.Property fap ->
      let s = TODO_do_in_parsing.labelize (r_fst fap.field_name) in
      let e = self fap.field_expr in
      AST.Field.Complete (s, e)
    | Punned_property fn ->
      let s = TODO_do_in_parsing.labelize (r_fst fn) in
      AST.Field.Punned (Location.wrap ~loc:(r_snd fn) s)
  in
  let compile_type_params : CST.type_params CST.par CST.reg -> Ligo_prim.Type_var.t nseq =
   fun tp ->
    let lst =
      nseq_map
        (fun (x : CST.variable) ->
          TODO_do_in_parsing.tvar ~loc:(Location.lift x.region) x.value)
        (r_fst tp).inside.type_vars
    in
    lst
  in
  let compile_rhs_type : CST.colon * CST.type_expr -> AST.ty_expr =
   fun (_, t) -> compile_type_expression t
  in
  return
  @@
  match e with
  | EVar var ->
    let name, loc = r_split var in
    e_variable (TODO_do_in_parsing.var ~loc name) ~loc
  (* we keep parenthesis so that the backward pass which add parenthesis is done only once for all syntaxes (?) *)
  | EPar par ->
    let par, _loc = r_split par in
    self par.inside
  | EUnit unit_ ->
    let _, loc = r_split unit_ in
    e_unit ~loc
  | EBytes bytes_ ->
    let bytes_, loc = r_split bytes_ in
    let _s, b = bytes_ in
    e_bytes_hex b ~loc
  | EString str ->
    (match str with
    | Cat c -> compile_bin_op CARET c
    | String str ->
      let str, loc = r_split str in
      TODO_unify_in_cst.e_string str ~loc
    | Verbatim str ->
      let str, loc = r_split str in
      TODO_unify_in_cst.e_verbatim str ~loc)
  | EArith arth ->
    (match arth with
    | Add plus -> compile_bin_op PLUS plus
    | Sub minus -> compile_bin_op MINUS minus
    | Mult times -> compile_bin_op STAR times
    | Div slash -> compile_bin_op SLASH slash
    | Mod mod_ -> compile_bin_op PRCENT mod_
    | Land land_ -> compile_bin_op WORD_LAND land_
    | Lor lor_ -> compile_bin_op WORD_LOR lor_
    | Lxor lxor_ -> compile_bin_op WORD_LXOR lxor_
    | Lsl lsl_ -> compile_bin_op WORD_LSL lsl_
    | Lsr lsr_ -> compile_bin_op WORD_LSR lsr_
    | Neg minus -> compile_unary_op MINUS minus
    | Int i ->
      let (_, i), loc = r_split i in
      e_int_z ~loc i
    | Nat n ->
      let (_, n), loc = r_split n in
      e_nat_z ~loc n
    | Mutez m ->
      let (_, m), loc = r_split m in
      e_mutez_z ~loc (Z.of_int64 m))
  | ELogic logic ->
    (match logic with
    | BoolExpr be ->
      (match be with
      | Or or_ -> compile_bin_op DPIPE or_
      | And and_ -> compile_bin_op DAMPERSAND and_
      | Not not_ -> compile_unary_op WORD_NOT not_)
    | CompExpr ce ->
      (match ce with
      | Lt lt -> compile_bin_op LT lt
      | Leq le -> compile_bin_op LE le
      | Gt gt -> compile_bin_op GT gt
      | Geq ge -> compile_bin_op GE ge
      | Equal eq -> compile_bin_op SEQ eq
      | Neq ne -> compile_bin_op LTGT ne))
  | ERevApp ra ->
    let ra, loc = r_split ra in
    let x = self ra.arg1 in
    let f = self ra.arg2 in
    e_rev_app { x; f } ~loc
  | ECall call ->
    let (func, args), loc = r_split call in
    let func = self func in
    let args = nseq_map self args in
    e_call func (Location.wrap ~loc:Location.dummy @@ List.Ne.to_list args) ~loc
  | ETuple lst ->
    let npseq, loc = r_split lst in
    let nseq = nseq_map self (nsepseq_to_nseq npseq) in
    e_tuple nseq ~loc
  | ERecord record ->
    let record, loc = r_split record in
    let fields =
      record.ne_elements |> nsepseq_to_nseq |> nseq_map (translate_field_assign <@ r_fst)
    in
    e_record_pun (nseq_to_list fields) ~loc
  | EProj proj -> TODO_unify_in_cst.nested_proj proj
  | EModA ma ->
    let ma, loc = r_split ma in
    let (module_path, field), field_as_open = TODO_do_in_parsing.flatten_moda ma in
    let module_path =
      List.Ne.map (fun x -> TODO_do_in_parsing.mvar ~loc:(r_snd x) (r_fst x)) module_path
    in
    let field = self field in
    e_module_open_in { module_path; field; field_as_open } ~loc
  | EUpdate up ->
    let up, loc = r_split up in
    let structure = TODO_unify_in_cst.update_lhs up.record in
    let update = TODO_unify_in_cst.update_rhs self up.updates in
    e_update { structure; update } ~loc
  | EFun f ->
    let f, loc = r_split f in
    let type_params = Option.map ~f:compile_type_params f.type_params in
    let binders = List.map ~f:compile_pattern (nseq_to_list f.binders) in
    let ret_type = Option.map ~f:compile_rhs_type f.rhs_type in
    let body = self f.body in
    e_poly_fun
      { type_params; parameters = TODO_unify_in_cst.fun_binder binders; ret_type; body }
      ~loc
  | EConstr constr ->
    let (name, expr), loc = r_split constr in
    let element = Option.map ~f:self expr in
    TODO_unify_in_cst.nested_ctor_application ~loc name element
  | ECase case ->
    let CST.{ expr; cases; _ }, loc = r_split case in
    let cases, loc2 = r_split cases in
    let loc = Location.cover loc loc2 in
    let expr = self expr in
    let cases : (pattern, expr) AST.Case.clause nseq =
      let compile_case_clause : CST.expr CST.case_clause -> (pattern, expr) Case.clause =
       fun c -> { pattern = compile_pattern c.pattern; rhs = self c.rhs }
      in
      nseq_map (compile_case_clause <@ r_fst) @@ nsepseq_to_nseq cases
    in
    e_match { expr; cases } ~loc
  | EAnnot annot ->
    let annot, loc = r_split annot in
    let e, _, te = annot.inside in
    let e = self e in
    let te = compile_type_expression te in
    e_annot (e, te) ~loc
  | ECond cond ->
    let cond, loc = r_split cond in
    let test = self cond.test in
    let ifso = self cond.ifso in
    let ifnot = Option.map ~f:(self <@ snd) cond.ifnot in
    e_cond { test; ifso; ifnot } ~loc
  | EList list ->
    (match list with
    | ECons cons -> compile_bin_op DCOLON cons
    | EListComp listcomp ->
      let list, loc = r_split listcomp in
      let elements = List.map ~f:self @@ sepseq_to_list list.elements in
      e_list elements ~loc)
  | ELetIn li ->
    let li, loc = r_split li in
    let ({ kwd_let = _; kwd_rec; binding; kwd_in = _; body; attributes } : CST.let_in) =
      li
    in
    let ({ type_params; binders; rhs_type; eq = _; let_rhs } : CST.let_binding) =
      binding
    in
    let is_rec =
      match kwd_rec with
      | Some _ -> true
      | None -> false
    in
    let type_params = Option.map ~f:compile_type_params type_params in
    let lhs = nseq_map compile_pattern binders in
    let rhs_type = Option.map ~f:compile_rhs_type rhs_type in
    let rhs = self let_rhs in
    let body = self body in
    TODO_unify_in_cst.attach_attr
      attributes
      (e_let_in { is_rec; type_params; lhs; rhs_type; rhs; body } ~loc)
  | ETypeIn ti ->
    let CST.{ type_decl = { name; type_expr; params; _ }; kwd_in = _; body }, loc =
      r_split ti
    in
    let name = TODO_do_in_parsing.tvar ~loc:(r_snd name) (r_fst name) in
    let type_expr = compile_type_expression type_expr in
    let params = Option.map params ~f:(fun p -> TODO_do_in_parsing.type_vars_to_lst p) in
    let body = self body in
    e_type_in { type_decl = { name; params; type_expr }; body } ~loc
  | EModIn mi ->
    let mi, loc = r_split mi in
    let ({ mod_decl = { name; module_; _ }; kwd_in = _; body } : CST.mod_in) = mi in
    let module_name = TODO_do_in_parsing.mvar ~loc:(r_snd name) (r_fst name) in
    let rhs = compile_module module_ in
    let body = self body in
    e_mod_in { module_name; rhs; body } ~loc
  | EModAlias ma ->
    let ma, loc = r_split ma in
    let ({ mod_alias = { alias; binders; _ }; kwd_in = _; body } : CST.mod_alias) = ma in
    let module_name = TODO_do_in_parsing.mvar ~loc:(r_snd alias) (r_fst alias) in
    let rhs = TODO_unify_in_cst.mod_in_as_mod_expr binders in
    let body = self body in
    e_mod_in { module_name; rhs; body } ~loc
  | ECodeInj ci ->
    let ci, loc = r_split ci in
    let language = r_fst @@ w_fst ci.language in
    let code = self ci.code in
    e_raw_code { language; code } ~loc
  | ESeq seq ->
    let seq, loc = r_split seq in
    (match seq.elements with
    | None -> TODO_do_in_parsing.empty_sequence ()
    | Some nelst ->
      let seq = nseq_map self (nsepseq_to_nseq nelst) in
      e_sequence ~loc (List.Ne.to_list seq))
  | EContract c ->
    let lst, loc = r_split c in
    let lst =
      List.Ne.map
        (fun x -> TODO_do_in_parsing.mvar ~loc:(r_snd x) (r_fst x))
        (nsepseq_to_nseq lst)
    in
    e_contract ~loc lst


and compile_seq_expr : (CST.expr, _) nsepseq option -> AST.expr =
 fun x ->
  let lst =
    Option.value_map ~default:[] ~f:(List.map ~f:compile_expression <@ nsepseq_to_list) x
  in
  e_sequence
    ~loc:
      (List.fold ~init:Location.generated ~f:Location.cover (List.map ~f:get_e_loc lst))
    lst


and compile_declaration : CST.declaration -> AST.declaration =
 fun decl ->
  match decl with
  | Directive d ->
    let loc = Simple_utils.Location.lift (Preprocessor.Directive.to_region d) in
    d_directive (ignore d) ~loc
  | Let e ->
    let (_kwd_let, kwd_rec, e, attributes), loc = r_split e in
    let is_rec =
      match kwd_rec with
      | None -> false
      | Some _ -> true
    in
    let type_params =
      let compile_type_params : CST.type_params CST.par CST.reg -> AST.Ty_variable.t nseq =
       fun tp ->
        nseq_map
          (fun x -> TODO_do_in_parsing.tvar ~loc:(r_snd x) (r_fst x))
          (r_fst tp).inside.type_vars
      in
      Option.map ~f:compile_type_params e.type_params
    in
    let pattern = nseq_map compile_pattern e.binders in
    let rhs_type = Option.map ~f:(compile_type_expression <@ snd) e.rhs_type in
    let let_rhs = compile_expression e.let_rhs in
    TODO_unify_in_cst.d_attach_attr
      attributes
      (d_let ~loc { is_rec; type_params; pattern; rhs_type; let_rhs })
  | TypeDecl d ->
    let d, loc = r_split d in
    let name = TODO_do_in_parsing.tvar ~loc:(r_snd d.name) (r_fst d.name) in
    let params = Option.map ~f:TODO_do_in_parsing.type_vars_to_lst d.params in
    let type_expr = compile_type_expression d.type_expr in
    d_type_abstraction { name; params; type_expr } ~loc
  | ModuleDecl d ->
    let d, loc = r_split d in
    let name = TODO_do_in_parsing.mvar ~loc:(r_snd d.name) (r_fst d.name) in
    let mod_expr = compile_module d.module_ in
    d_module { name; mod_expr } ~loc
  | ModuleAlias d ->
    let d, loc = r_split d in
    let alias = TODO_do_in_parsing.mvar ~loc:(r_snd d.alias) (r_fst d.alias) in
    let binders =
      nseq_map
        (fun x -> TODO_do_in_parsing.mvar ~loc:(r_snd x) (r_fst x))
        (nsepseq_to_nseq d.binders)
    in
    TODO_unify_in_cst.module_alias alias binders ~loc


and compile_module : CST.t -> AST.mod_expr =
 fun m ->
  let ds : AST.declaration nseq = nseq_map compile_declaration m.decl in
  let loc =
    (* The region of the module is the union of all its declarations' regions *)
    let locations = nseq_map get_d_loc ds in
    List.Ne.fold_left locations ~init:Location.dummy ~f:Location.cover
  in
  m_body (TODO_unify_in_cst.declarations_as_program ds) ~loc


let compile_program : CST.ast -> AST.program =
 fun t ->
  nseq_to_list t.decl
  |> List.map ~f:(fun a -> compile_declaration a)
  |> List.map ~f:(fun x -> make_pe (PE_declaration x))
  |> make_prg
