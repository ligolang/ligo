open Simple_utils.Utils
module List = Simple_utils.List
open Unification_shared.Helpers
module Option = Simple_utils.Option
module O = Ast_unified
module I = Cst.Cameligo

module Eq = struct
  type expr = I.expr
  type ty_expr = I.type_expr
  type pattern = I.pattern
  type statement = unit
  type block = unit

  type mod_expr =
    [ `Prg of I.t
    | `Path of (I.module_name, I.dot) nsepseq
    ]

  type instruction = unit
  type declaration = I.declaration
  type program_entry = I.declaration
  type program = I.t

  let mod_expr_of_path x = `Path x
  let mod_expr_of_prg x = `Prg x
  let not_part_of_the_language _ = assert false
end

module Folding = Folding (Eq)

module TODO_do_in_parsing = struct
  let conv_attr : I.attribute -> AST.Attribute.t * Location.t =
   fun attr_reg ->
    let (key, value_opt), loc = w_split attr_reg in
    let value : string option =
      Option.map
        ~f:(function
          | String x -> x
          | Ident x -> x)
        value_opt
    in
    Nano_prim.Attribute.{ key; value }, loc


  let conv_attrs = List.map ~f:conv_attr

  let weird_attributes _ =
    (* I don't know what to do with those attributes *)
    ()


  let mvar x = Ligo_prim.Module_var.of_input_var ~loc:(r_snd x) (r_fst x)
  let var x = Ligo_prim.Value_var.of_input_var ~loc:(r_snd x) (r_fst x)
  let tvar x = Ligo_prim.Type_var.of_input_var ~loc:(r_snd x) (r_fst x)
  let labelize x = Ligo_prim.Label.of_string (r_fst x)

  let nest_projection compile_selection value region hd tl =
    let field_path =
      match List.rev tl with
      | (_, el) :: tl -> el, tl
      | _ -> assert false
    in
    O.E_proj
      { struct_ = I.EProj { value = { value with field_path }; region }
      ; path = compile_selection hd
      }


  (* flatten module_name in access
  Note: is_open indicates if parenthesis were used, which would correspond to a module opening
  *)
  let flatten_moda ({ module_name; selector = _; field } : I.expr I.module_access) =
    let rec aux acc expr =
      match expr with
      | I.EModA { value = { module_name; field; _ }; _ } ->
        aux (List.Ne.append acc (List.Ne.singleton module_name)) field
      | _ -> acc, expr
    in
    let path, field = aux (List.Ne.singleton module_name) field in
    let is_open =
      match field with
      | I.EPar _ -> true
      | _ -> false
    in
    (path, field), is_open


  (* one day, update lhs should be an expression *)
  let update_lhs_as_expression : I.path -> I.expr = function
    | Name name -> I.EVar name
    | Path v -> I.EProj v


  (* the I.field_path_assignment type is slightly different from other syntaxes:
     - no lenses ; labels are string ; the Path case *)
  let update_rhs
      :  _ -> I.field_path_assignment Region.reg I.ne_injection Region.reg
      -> I.expr AST.Update.field list
    =
   fun compile_selection
       { region = _; value = { compound = _; ne_elements; terminator = _; attributes } } ->
    weird_attributes attributes;
    let x = nsepseq_to_list ne_elements in
    let f : I.field_path_assignment Region.reg -> I.expr AST.Update.field =
     fun fpa ->
      let fpa, _loc = r_split fpa in
      match fpa with
      | Path_property { field_path; assignment = _; field_expr } ->
        let field_rhs = field_expr in
        let field_lhs =
          match field_path with
          | Name v -> [ O.Selection.FieldName (labelize v) ]
          | Path { region = _; value = { struct_name; selector = _; field_path } } ->
            O.Selection.FieldName (labelize struct_name)
            :: List.map (nsepseq_to_list field_path) ~f:compile_selection
        in
        Full_field { field_lhs; field_lens = Lens_Id; field_rhs }
      | Path_punned_property pun ->
        let label = labelize pun in
        Pun (Location.wrap ~loc:(r_snd pun) label)
    in
    List.map x ~f


  (* use the same type as in other CST's *)
  let fun_binder : I.pattern nseq -> I.pattern AST.Param.t list =
   fun ps ->
    List.map
      ~f:(fun pattern -> AST.Param.{ param_kind = `Const; pattern })
      (nseq_to_list ps)


  (* constructor application as in jsligo *)
  let fake_constructor_app el_opt ctor =
    match el_opt with
    | Some element ->
      O.(E_constructor { constructor = ctor; element })
    | None ->
      let element =
        let open Lexing_cameligo.Token in
        I.EUnit { value = ghost_lpar, ghost_rpar; region = Region.ghost }
      in
      O.(E_constructor { constructor = ctor; element })


  (* just to match other syntaxes .. *)
  let quoted_tvar (x : I.type_var) = tvar { x.name with value = "'" ^ x.name.value }

  (* couldn't it be a list directly ?*)
  let type_vars_to_lst (p : I.type_vars) : O.Ty_variable.t List.Ne.t =
    match p with
    | QParam x -> List.Ne.singleton (quoted_tvar (r_fst x))
    | QParamTuple x ->
      nseq_map (fun x -> quoted_tvar (r_fst x)) (nsepseq_to_nseq x.value.inside)


  let empty_sequence () = failwith "should a sequence be allowed to be empty ?"

  (* could be a type expr ? or we could emit a type variable expression ? *)
  let type_operator v = I.TVar v

  (* Not sure what we should do here , maybe TPar should be in unified ?*)
  let field_as_open_t field =
    match field with
    | I.TPar _ -> true
    | _ -> false


  (* would be cool to use the same type if possible *)
  let compile_rows = O.Non_linear_rows.make
end

let block = Eq.not_part_of_the_language
let instruction = Eq.not_part_of_the_language
let statement = Eq.not_part_of_the_language

let rec expr : Eq.expr -> Folding.expr =
 fun e ->
  let loc = Location.lift (I.expr_to_region e) in
  let ret = Location.wrap ~loc in
  let compile_bin_op (sign : O.Operators.op) (op : _ I.bin_op Region.reg) =
    let I.{ op = _; arg1; arg2 } = r_fst op in
    O.E_binary_op { operator = Location.wrap ~loc sign; left = arg1; right = arg2 }
  in
  let compile_unary_op (sign : AST.Operators.op) op =
    let I.{ op = _; arg } = r_fst op in
    O.E_unary_op { operator = Location.wrap ~loc sign; arg }
  in
  let compile_selection =
    let open Nano_prim.Selection in
    function
    | I.FieldName name -> FieldName (TODO_do_in_parsing.labelize name)
    | I.Component c -> Component_num c.value
  in
  let compile_type_params : I.type_params I.par I.reg -> Ligo_prim.Type_var.t nseq =
   fun tp -> nseq_map TODO_do_in_parsing.tvar (r_fst tp).inside.type_vars
  in
  match e with
  | EVar var -> ret @@ O.E_variable (TODO_do_in_parsing.var var)
  (* we keep parenthesis so that the backward pass which add parenthesis is done only once for all syntaxes (?) *)
  | EPar par -> expr par.value.inside
  | EUnit _ -> ret @@ E_literal Literal_unit
  | EBytes { value = _, b; _ } -> ret @@ E_literal (Literal_bytes (Hex.to_bytes b))
  | EString str ->
    (match str with
    | Cat c -> ret @@ compile_bin_op CARET c
    | String str ->
      ret @@ E_literal (Literal_string (Simple_utils.Ligo_string.Standard str.value))
    | Verbatim str ->
      ret @@ E_literal (Literal_string (Simple_utils.Ligo_string.Verbatim str.value)))
  | EArith arth ->
    ret
    @@
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
    | Int i -> E_literal (Literal_int (snd i.value))
    | Nat n -> E_literal (Literal_nat (snd n.value))
    | Mutez m -> E_literal (Literal_mutez (Z.of_int64 (snd m.value))))
  | ELogic logic ->
    ret
    @@
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
  | ERevApp ra -> ret @@ E_rev_app { x = ra.value.arg1; f = ra.value.arg2 }
  | ECall { value = func, args; _ } ->
    let args = List.Ne.to_list args in
    let arg_loc =
      List.fold
        ~init:Location.dummy
        ~f:Location.cover
        (List.map ~f:(Location.lift <@ I.expr_to_region) args)
    in
    ret @@ E_call (func, Location.wrap ~loc:arg_loc args)
  | ETuple { value; _ } -> ret @@ E_tuple (nsepseq_to_nseq value)
  | ERecord { value = record; _ } ->
    let translate_field_assign (fa : I.field_assign) : (_, _) AST.Field.t =
      match fa with
      | Property { field_expr; field_name; _ } ->
        let s = TODO_do_in_parsing.labelize field_name in
        O.Field.Complete (s, field_expr)
      | Punned_property fn ->
        let s = TODO_do_in_parsing.labelize fn in
        O.Field.Punned (Location.wrap ~loc:(r_snd fn) s)
    in
    let fields =
      record.ne_elements |> nsepseq_to_nseq |> nseq_map (translate_field_assign <@ r_fst)
    in
    ret @@ E_record_pun (nseq_to_list fields)
  | EProj { value = { struct_name; field_path; _ } as value; region } ->
    (match nsepseq_rev field_path with
    | one, [] ->
      ret @@ E_proj { struct_ = EVar struct_name; path = compile_selection one }
    | hd, tl ->
      ret @@ TODO_do_in_parsing.nest_projection compile_selection value region hd tl)
  | EModA ma ->
    let (module_path, field), field_as_open = TODO_do_in_parsing.flatten_moda ma.value in
    let module_path = List.Ne.map TODO_do_in_parsing.mvar module_path in
    ret @@ E_module_open_in { module_path; field; field_as_open }
  | EUpdate { value = { record; updates; _ }; _ } ->
    let structure = TODO_do_in_parsing.update_lhs_as_expression record in
    let update = TODO_do_in_parsing.update_rhs compile_selection updates in
    ret @@ E_update { structure; update }
  | EFun { value = { type_params; binders; rhs_type; body; _ }; _ } ->
    let type_params = Option.map ~f:compile_type_params type_params in
    ret
    @@ E_poly_fun
         { type_params
         ; parameters = TODO_do_in_parsing.fun_binder binders
         ; ret_type = Option.map ~f:snd rhs_type
         ; body
         }
  | EConstr { value = name, el_opt; _ } ->
    let ctor = TODO_do_in_parsing.labelize name in
    ret (TODO_do_in_parsing.fake_constructor_app el_opt ctor)
  | ECase { value = { expr; cases; _ }; region } ->
    let cases, loc2 = r_split cases in
    let loc = Location.cover (Location.lift region) loc2 in
    let cases =
      let compile_case_clause I.{ pattern; rhs; _ } = O.Case.{ pattern; rhs } in
      nseq_map (compile_case_clause <@ r_fst) @@ nsepseq_to_nseq cases
    in
    Location.wrap ~loc @@ O.E_match { expr; cases }
  | EAnnot a ->
    let e, _, te = a.value.inside in
    ret @@ E_annot (e, te)
  | ECond { value = { test; ifso; ifnot; _ }; _ } ->
    let ifnot = Option.map ~f:snd ifnot in
    ret @@ E_cond { test; ifso; ifnot }
  | EList list ->
    (match list with
    | ECons cons -> ret @@ compile_bin_op DCOLON cons
    | EListComp listcomp ->
      let elements = sepseq_to_list listcomp.value.elements in
      ret @@ E_list elements)
  | ELetIn { value; region } ->
    let I.{ kwd_rec; binding; body; attributes; _ } = value in
    let I.{ type_params; binders; rhs_type; eq = _; let_rhs } = binding in
    let is_rec =
      match kwd_rec with
      | Some _ -> true
      | None -> false
    in
    let type_params = Option.map ~f:compile_type_params type_params in
    let rhs_type = Option.map ~f:snd rhs_type in
    (match attributes with
    | [] ->
      ret
      @@ E_let_in { is_rec; type_params; lhs = binders; rhs_type; rhs = let_rhs; body }
    | hd :: tl ->
      let rm_attr = I.ELetIn { value = { value with attributes = tl }; region } in
      ret @@ E_attr (fst @@ TODO_do_in_parsing.conv_attr hd, rm_attr))
  | ETypeIn { value = { type_decl = { name; type_expr; params; _ }; body; _ }; _ } ->
    let name = TODO_do_in_parsing.tvar name in
    let params = Option.map params ~f:(fun p -> TODO_do_in_parsing.type_vars_to_lst p) in
    ret @@ E_type_in { type_decl = { name; params; type_expr }; body }
  | EModIn { value = { mod_decl = { name; module_; _ }; body; _ }; _ } ->
    let module_name = TODO_do_in_parsing.mvar name in
    let rhs = Eq.mod_expr_of_prg module_ in
    ret @@ E_mod_in { module_name; rhs; body }
  | EModAlias { value = { mod_alias = { alias; binders; _ }; kwd_in = _; body }; _ } ->
    let module_name = TODO_do_in_parsing.mvar alias in
    let rhs = Eq.mod_expr_of_path binders in
    ret @@ E_mod_in { module_name; rhs; body }
  | ECodeInj { value = { language; code; _ }; _ } ->
    let language = r_fst @@ w_fst language in
    ret @@ E_raw_code { language; code }
  | ESeq { value = { elements; _ }; _ } ->
    (match elements with
    | None -> TODO_do_in_parsing.empty_sequence ()
    | Some nelst ->
      let seq = nsepseq_to_list nelst in
      ret @@ E_sequence seq)
  | EContract c ->
    ret @@ E_contract (List.Ne.map TODO_do_in_parsing.mvar (nsepseq_to_nseq c.value))


let rec ty_expr : Eq.ty_expr -> Folding.ty_expr =
 fun te ->
  let loc = Location.lift (I.type_expr_to_region te) in
  let ret = Location.wrap ~loc in
  let ret_attr attributes ~attr ~no_attr =
    match attributes with
    | [] -> ret no_attr
    | hd :: tl -> ret @@ O.T_attr (fst @@ TODO_do_in_parsing.conv_attr hd, attr tl)
  in
  match te with
  | TProd t -> ret @@ O.T_prod (nsepseq_to_nseq t.value)
  | TSum { value = { variants; lead_vbar; attributes }; region } ->
    let sum =
      let compile_variant I.{ constr; arg; attributes } =
        ( TODO_do_in_parsing.labelize constr
        , Option.map ~f:snd arg
        , List.map (TODO_do_in_parsing.conv_attrs attributes) ~f:fst )
      in
      let lst = List.map (nsepseq_to_list variants) ~f:(compile_variant <@ r_fst) in
      TODO_do_in_parsing.compile_rows lst
    in
    ret_attr attributes ~no_attr:(T_sum_raw sum) ~attr:(fun attributes ->
        TSum { value = { variants; lead_vbar; attributes }; region })
  | TRecord { value = { attributes; ne_elements; compound; terminator }; region } ->
    let fields =
      let field_decls = nseq_map r_fst @@ nsepseq_to_nseq ne_elements in
      (* let open Ligo_prim in *)
      let compile_field_decl
          : int -> I.field_decl -> I.type_expr option O.Non_linear_rows.row
        =
       fun i { field_name; field_type; attributes; _ } ->
        let l = TODO_do_in_parsing.labelize field_name in
        let rows =
          O.Non_linear_rows.
            { decl_pos = i
            ; associated_type = Some field_type
            ; attributes = List.map (TODO_do_in_parsing.conv_attrs attributes) ~f:fst
            }
        in
        l, rows
      in
      List.mapi ~f:compile_field_decl (nseq_to_list field_decls)
    in
    ret_attr attributes ~no_attr:(T_record_raw fields) ~attr:(fun attributes ->
        TRecord { value = { attributes; ne_elements; compound; terminator }; region })
  | TApp { value = constr, args; _ } ->
    let constr = TODO_do_in_parsing.type_operator constr in
    let type_args =
      match args with
      | I.CArg te -> List.Ne.singleton te
      | I.CArgTuple tes -> nsepseq_to_nseq (r_fst tes).inside
    in
    ret @@ T_app { constr; type_args }
  | TFun { value = te1, _, te2; _ } -> ret @@ T_fun (te1, te2)
  | TPar t -> ty_expr (r_fst t).inside
  | TVar t -> ret @@ T_var (TODO_do_in_parsing.tvar t)
  | TString { value; _ } -> ret @@ T_string value
  | TInt { value = s, z; _ } -> ret @@ T_int (s, z)
  | TModA { value = { module_name; field; _ }; _ } ->
    let module_path = TODO_do_in_parsing.mvar module_name in
    let field_as_open = TODO_do_in_parsing.field_as_open_t field in
    ret @@ T_module_open_in { module_path; field; field_as_open }
  | TArg { value = { name; _ }; _ } -> ret @@ T_arg name.value
  | TParameter { value; _ } ->
    ret
    @@ T_module_access
         { module_path = (nseq_map TODO_do_in_parsing.mvar <@ nsepseq_to_nseq) value
         ; field = Ligo_prim.Type_var.of_input_var ~loc "$parameter"
         ; field_as_open = false
         }


let rec pattern : Eq.pattern -> Folding.pattern =
 fun p ->
  let loc = Location.lift (I.pattern_to_region p) in
  let ret = Location.wrap ~loc in
  match p with
  | PConstr { value = ctor, p_opt; _ } ->
    let ctor = TODO_do_in_parsing.labelize ctor in
    ret @@ O.P_variant (ctor, p_opt)
  | PUnit _ -> ret @@ P_unit
  | PVar p -> ret @@ P_var (TODO_do_in_parsing.var p.value.variable)
  | PInt { value = _s, z; _ } -> ret @@ P_literal (Literal_int z)
  | PNat { value = _s, z; _ } -> ret @@ P_literal (Literal_nat z)
  | PBytes { value = _s, hex; _ } -> ret @@ P_literal (Literal_bytes (Hex.to_bytes hex))
  | PString { value = s; _ } ->
    ret @@ P_literal (Literal_string (Simple_utils.Ligo_string.standard s))
  | PVerbatim { value = s; _ } ->
    ret @@ P_literal (Literal_string (Simple_utils.Ligo_string.verbatim s))
  | PList p ->
    let p =
      match p with
      | I.PListComp { value = { elements; _ }; _ } ->
        let ps = sepseq_to_list elements in
        O.List ps
      | I.PCons { value = p1, _, p2; _ } -> Cons (p1, p2)
    in
    ret @@ P_list p
  | PTuple { value = p; _ } -> ret @@ P_tuple (nsepseq_to_list p)
  | PPar p -> pattern (r_fst p).inside
  | PRecord { value = p; _ } ->
    let p =
      let compile_field_pattern I.{ field_name; pattern; _ } =
        O.Field.Complete (TODO_do_in_parsing.labelize field_name, pattern)
      in
      List.map ~f:(compile_field_pattern <@ r_fst) (nsepseq_to_list p.ne_elements)
    in
    ret @@ P_pun_record p
  | PTyped { value = { type_expr; pattern; _ }; _ } -> ret @@ P_typed (type_expr, pattern)


let declaration : Eq.declaration -> Folding.declaration =
 fun decl ->
  let loc = Location.lift (I.declaration_to_region decl) in
  let ret = Location.wrap ~loc in
  let ret_attr attributes ~attr ~no_attr =
    match attributes with
    | [] -> ret no_attr
    | hd :: tl -> ret @@ O.D_attr (fst @@ TODO_do_in_parsing.conv_attr hd, attr tl)
  in
  match decl with
  | Directive d ->
    let loc = Simple_utils.Location.lift (Preprocessor.Directive.to_region d) in
    Location.wrap ~loc @@ O.D_directive (ignore d)
  | Let { value = kwd_let, kwd_rec, e, attributes; region } ->
    let is_rec =
      match kwd_rec with
      | None -> false
      | Some _ -> true
    in
    let type_params =
      let compile_type_params : I.type_params I.par I.reg -> AST.Ty_variable.t nseq =
       fun tp -> nseq_map TODO_do_in_parsing.tvar (r_fst tp).inside.type_vars
      in
      Option.map ~f:compile_type_params e.type_params
    in
    let pattern = e.binders in
    let rhs_type = Option.map ~f:snd e.rhs_type in
    let let_rhs = e.let_rhs in
    ret_attr
      attributes
      ~no_attr:(D_let { is_rec; type_params; pattern; rhs_type; let_rhs })
      ~attr:(fun attributes -> I.Let { value = kwd_let, kwd_rec, e, attributes; region })
  | TypeDecl { value = { name; params; type_expr; _ }; _ } ->
    let name = TODO_do_in_parsing.tvar name in
    let params = Option.map ~f:TODO_do_in_parsing.type_vars_to_lst params in
    ret @@ D_type_abstraction { name; params; type_expr }
  | ModuleDecl { value = { name; module_; _ }; _ } ->
    let name = TODO_do_in_parsing.mvar name in
    ret @@ D_module { name; mod_expr = Eq.mod_expr_of_prg module_ }
  | ModuleAlias { value = { alias; binders; _ }; _ } ->
    let name = TODO_do_in_parsing.mvar alias in
    ret @@ D_module { name; mod_expr = Eq.mod_expr_of_path binders }


let mod_expr : Eq.mod_expr -> Folding.mod_expr = function
  | `Path x ->
    let path = nseq_map TODO_do_in_parsing.mvar (nsepseq_to_nseq x) in
    let loc =
      Location.(nseq_foldl cover dummy (nseq_map Ligo_prim.Module_var.get_location path))
    in
    Location.wrap ~loc @@ O.M_path (nseq_map TODO_do_in_parsing.mvar (nsepseq_to_nseq x))
  | `Prg prg ->
    let loc =
      let locations = nseq_map (Location.lift <@ I.declaration_to_region) prg.decl in
      List.Ne.fold_left locations ~init:Location.dummy ~f:Location.cover
    in
    Location.wrap ~loc @@ O.M_body prg


let program_entry : Eq.program_entry -> Folding.program_entry = fun x -> PE_declaration x
let program : Eq.program -> Folding.program = fun { decl; _ } -> List.Ne.to_list decl
