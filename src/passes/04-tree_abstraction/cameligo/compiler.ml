open Errors
open Simple_utils.Trace
open Simple_utils.Function
module CST = Cst.Cameligo
module AST = Ast_imperative
module Utils = Simple_utils.Utils
open Ligo_prim
open AST

let nseq_to_list (hd, tl) = hd :: tl
let npseq_to_list (hd, tl) = hd :: List.map ~f:snd tl

let npseq_to_ne_list (hd, tl) = hd, List.map ~f:snd tl

and type_vars_to_list : CST.type_vars -> CST.type_var Region.reg list = function
  | QParam x -> [ x ]
  | QParamTuple x -> Utils.nsepseq_to_list x.value.inside


let pseq_to_list = function
  | None -> []
  | Some lst -> npseq_to_list lst


let r_split = Location.r_split
let quote_var var = "'" ^ var

let compile_variable var =
  let var, loc = r_split var in
  Value_var.of_input_var ~loc var


let compile_type_var var =
  let var, loc = r_split var in
  Type_var.of_input_var ~loc var


let compile_mod_var var =
  let var, loc = r_split var in
  Module_var.of_input_var ~loc var


let compile_attributes : CST.attributes -> string list =
 fun attr ->
  let f : CST.attribute Region.reg -> string =
   fun x ->
    let (k, v_opt), _loc = r_split x in
    match v_opt with
    | Some (String v) -> String.concat ~sep:":" [ k; v ]
    | None -> k
  in
  List.map ~f attr


let rec compile_type_expression ~raise : CST.type_expr -> AST.type_expression =
 fun te ->
  let self = compile_type_expression ~raise in
  let return te = te in
  match te with
  | TSum sum ->
    let sum_type, loc = r_split sum in
    let ({ variants; attributes; _ } : CST.sum_type) = sum_type in
    let lst = npseq_to_list variants in
    let attr = compile_attributes attributes in
    let aux (variant : CST.variant CST.reg) =
      let v, _ = r_split variant in
      let type_expr = Option.map ~f:(self <@ snd) v.arg in
      let type_expr = Option.value ~default:(t_unit ()) type_expr in
      let variant_attr = compile_attributes v.attributes in
      v.constr.value, type_expr, variant_attr
    in
    let sum = List.map ~f:aux lst in
    return @@ t_sum_ez_attr ~loc ~attr sum
  | TRecord record ->
    let injection, loc = r_split record in
    let attr = compile_attributes injection.attributes in
    let lst = npseq_to_list injection.ne_elements in
    let aux (field : CST.field_decl CST.reg) =
      let f, _ = r_split field in
      let type_expr = self f.field_type in
      let field_attr = compile_attributes f.attributes in
      return @@ (f.field_name.value, type_expr, field_attr)
    in
    let fields = List.map ~f:aux lst in
    return @@ t_record_ez_attr ~loc ~attr fields
  | TProd prod ->
    let nsepseq, loc = r_split prod in
    let lst = npseq_to_list nsepseq in
    let lst = List.map ~f:self lst in
    return @@ t_tuple ~loc lst
  | TApp app ->
    let get_t_string_singleton_opt = function
      | CST.TString s -> Some s.value
      | _ -> None
    in
    let get_t_int_singleton_opt = function
      | CST.TInt x ->
        let _, z = x.value in
        Some z
      | _ -> None
    in
    let get_t_var_singleton_opt = function
      | CST.TVar var ->
        let name, loc = r_split var in
        let v = Type_var.of_input_var name in
        Some (t_variable ~loc v)
      | _ -> None
    in
    let (operator, args), loc = r_split app in
    let args =
      match args with
      | CArg x -> [ x ]
      | CArgTuple args -> npseq_to_list args.value.inside
    in
    (* this is a bad design, michelson_or and pair should be an operator
       see AnnotType *)
    (match operator.value with
    | "michelson_or" ->
      (match args with
      | [ a; b; c; d ] ->
        let b' =
          trace_option ~raise (michelson_type_wrong te operator.value)
          @@ get_t_string_singleton_opt b
        in
        let d' =
          trace_option ~raise (michelson_type_wrong te operator.value)
          @@ get_t_string_singleton_opt d
        in
        let a' = self a in
        let c' = self c in
        return @@ t_michelson_or ~loc a' b' c' d'
      | _ -> raise.error @@ michelson_type_wrong_arity loc operator.value)
    | "michelson_pair" ->
      (match args with
      | [ a; b; c; d ] ->
        let b' =
          trace_option ~raise (michelson_type_wrong te operator.value)
          @@ get_t_string_singleton_opt b
        in
        let d' =
          trace_option ~raise (michelson_type_wrong te operator.value)
          @@ get_t_string_singleton_opt d
        in
        let a' = self a in
        let c' = self c in
        return @@ t_michelson_pair ~loc a' b' c' d'
      | _ -> raise.error @@ michelson_type_wrong_arity loc operator.value)
    | "sapling_state" ->
      (match args with
      | [ (a : CST.type_expr) ] ->
        let sloc = Location.lift @@ Raw.type_expr_to_region a in
        (match get_t_int_singleton_opt a with
        | Some a' ->
          let singleton = t_singleton ~loc:sloc (Literal_int a') in
          return @@ t_sapling_state ~loc singleton
        | None ->
          (match get_t_var_singleton_opt a with
          | Some v -> return @@ t_sapling_state ~loc v
          | None -> raise.error (michelson_type_wrong te operator.value)))
      | _ -> raise.error @@ michelson_type_wrong_arity loc operator.value)
    | "sapling_transaction" ->
      (match args with
      | [ (a : CST.type_expr) ] ->
        let sloc = Location.lift @@ Raw.type_expr_to_region a in
        (match get_t_int_singleton_opt a with
        | Some a' ->
          let singleton = t_singleton ~loc:sloc (Literal_int a') in
          return @@ t_sapling_transaction ~loc singleton
        | None ->
          (match get_t_var_singleton_opt a with
          | Some v -> return @@ t_sapling_transaction ~loc v
          | None -> raise.error (michelson_type_wrong te operator.value)))
      | _ -> raise.error @@ michelson_type_wrong_arity loc operator.value)
    | _ ->
      let operator = Type_var.of_input_var operator.value in
      let lst = List.map ~f:self args in
      return @@ t_app ~loc operator lst)
  | TFun func ->
    let (input_type, _, output_type), loc = r_split func in
    let input_type = self input_type in
    let output_type = self output_type in
    return @@ t_arrow ~loc input_type output_type
  | TPar par ->
    let par, _ = r_split par in
    let type_expr = par.inside in
    self type_expr
  | TVar var ->
    let name, loc = r_split var in
    let v = Type_var.of_input_var name in
    return @@ t_variable ~loc v
  | TString _s -> raise.error @@ unsupported_string_singleton te
  | TInt _s -> raise.error @@ unsupported_string_singleton te
  | TArg var ->
    let quoted_var, loc = r_split var in
    let v = Type_var.of_input_var (quote_var quoted_var.name.value) in
    return @@ t_variable ~loc v
  | TModA ma ->
    let ma, loc = r_split ma in
    let module_name = compile_mod_var ma.module_name in
    let rec aux : Module_var.t list -> CST.type_expr -> AST.type_expression =
     fun acc exp ->
      match exp with
      | TVar v ->
        let accessed_el = compile_type_var v in
        return @@ t_module_accessor ~loc acc accessed_el
      | TModA ma ->
        aux (acc @ [ Module_var.of_input_var ma.value.module_name.value ]) ma.value.field
      | _ -> raise.error (expected_access_to_variable (CST.type_expr_to_region ma.field))
    in
    aux [ module_name ] ma.field


let compile_selection (selection : CST.selection) =
  match selection with
  | FieldName name ->
    let name, loc = r_split name in
    Access_path.Access_record name, loc
  | Component comp ->
    let (_, index), loc = r_split comp in
    Access_tuple index, loc


let rec compile_expression ~raise : CST.expr -> AST.expr =
 fun e ->
  let self = compile_expression ~raise in
  let return e = e in
  let compile_tuple_expression ?loc tuple_expr =
    let lst = List.map ~f:self @@ nseq_to_list tuple_expr in
    match lst with
    | [ hd ] -> return hd
    | lst -> return @@ e_tuple ?loc lst
  in
  let compile_path (path : CST.path) =
    match path with
    | Name var ->
      let var, loc = r_split var in
      return @@ e_variable_ez ~loc var
    | Path proj ->
      let proj, loc = r_split proj in
      let var, _loc_var = r_split proj.struct_name in
      let var = e_variable_ez ~loc var in
      let sels, _ =
        List.unzip @@ List.map ~f:compile_selection @@ npseq_to_list proj.field_path
      in
      return @@ e_accessor var sels
  in
  let compile_bin_op (op_type : Constant.constant') (op : _ CST.bin_op CST.reg) =
    let op, loc = r_split op in
    let a = self op.arg1 in
    let b = self op.arg2 in
    return @@ e_constant ~loc (Const op_type) [ a; b ]
  in
  let compile_un_op (op_type : Constant.constant') (op : _ CST.un_op CST.reg) =
    let op, loc = r_split op in
    let arg = self op.arg in
    return @@ e_constant ~loc (Const op_type) [ arg ]
  in
  match e with
  | EVar var ->
    let var, loc = r_split var in
    return @@ e_variable_ez ~loc var
  | EPar par -> self par.value.inside
  | EUnit the_unit ->
    let loc = Location.lift the_unit.region in
    return @@ e_unit ~loc ()
  | EBytes bytes ->
    let bytes, loc = r_split bytes in
    let _s, b = bytes in
    return @@ e_bytes_hex ~loc b
  | EString str ->
    (match str with
    | Cat c ->
      let op, loc = r_split c in
      let a = self op.arg1 in
      let b = self op.arg2 in
      return @@ e_constant ~loc (Const C_CONCAT) [ a; b ]
    | String str ->
      let str, loc = r_split str in
      return @@ e_string ~loc str
    | Verbatim str ->
      let str, loc = r_split str in
      return @@ e_verbatim ~loc str)
  | EArith arth ->
    (match arth with
    | Add plus -> compile_bin_op C_ADD plus
    | Sub minus -> compile_bin_op C_POLYMORPHIC_SUB minus
    | Mult times -> compile_bin_op C_MUL times
    | Div slash -> compile_bin_op C_DIV slash
    | Mod mod_ -> compile_bin_op C_MOD mod_
    | Land land_ -> compile_bin_op C_AND land_
    | Lor lor_ -> compile_bin_op C_OR lor_
    | Lxor lxor_ -> compile_bin_op C_XOR lxor_
    | Lsl lsl_ -> compile_bin_op C_LSL lsl_
    | Lsr lsr_ -> compile_bin_op C_LSR lsr_
    | Neg minus -> compile_un_op C_NEG minus
    | Int i ->
      let (_, i), loc = r_split i in
      return @@ e_int_z ~loc i
    | Nat n ->
      let (_, n), loc = r_split n in
      return @@ e_nat_z ~loc n
    | Mutez mtez ->
      let (_, mtez), loc = r_split mtez in
      return @@ e_mutez_z ~loc (Z.of_int64 mtez))
  | ELogic logic ->
    (match logic with
    | BoolExpr be ->
      (match be with
      | Or or_ -> compile_bin_op C_OR or_
      | And and_ -> compile_bin_op C_AND and_
      | Not not_ -> compile_un_op C_NOT not_)
    | CompExpr ce ->
      (match ce with
      | Lt lt -> compile_bin_op C_LT lt
      | Leq le -> compile_bin_op C_LE le
      | Gt gt -> compile_bin_op C_GT gt
      | Geq ge -> compile_bin_op C_GE ge
      | Equal eq -> compile_bin_op C_EQ eq
      | Neq ne -> compile_bin_op C_NEQ ne))
  | ERevApp { value; region } ->
    let loc = Location.lift region in
    let x = self value.arg1 in
    let f = self value.arg2 in
    return @@ e_application ~loc f x
  (* This case is due to a bad besign of our constant it as to change
    with the new typer so LIGO-684 on Jira *)
  | ECall { value = EVar var, args; region } ->
    let loc = Location.lift region in
    let var, loc_var = r_split var in
    let func = e_variable_ez ~loc:loc_var var in
    let args = List.map ~f:self @@ nseq_to_list args in
    return @@ List.fold_left ~f:(e_application ~loc) ~init:func @@ args
  | ECall call ->
    let (func, args), loc = r_split call in
    let func = self func in
    let args = List.map ~f:self @@ nseq_to_list args in
    return @@ List.fold_left ~f:(e_application ~loc) ~init:func @@ args
  | ETuple lst ->
    let lst, loc = r_split lst in
    let lst = npseq_to_ne_list lst in
    compile_tuple_expression ~loc lst
  | ERecord record ->
    let record, loc = r_split record in
    let aux (fa : CST.field_assign CST.reg) =
      let fa, _ = r_split fa in
      match fa with
      | Property fa ->
        let name, _ = r_split fa.field_name in
        let expr = self fa.field_expr in
        return (name, expr)
      | Punned_property name ->
        let expr = self (CST.EVar name) in
        return (name.value, expr)
    in
    let record = List.map ~f:aux @@ npseq_to_list record.ne_elements in
    return @@ e_record_ez ~loc record
  | EProj proj ->
    let proj, loc = r_split proj in
    let var, loc_var = r_split proj.struct_name in
    let var = e_variable_ez ~loc:loc_var var in
    let sels, _ =
      List.unzip @@ List.map ~f:compile_selection @@ npseq_to_list proj.field_path
    in
    return @@ e_accessor ~loc var sels
  | EModA ma ->
    let ma, loc = r_split ma in
    let rec aux : Module_var.t list -> CST.expr -> AST.expression =
     fun acc exp ->
      match exp with
      | EVar v ->
        let accessed_el = compile_variable v in
        return @@ e_module_accessor ~loc acc accessed_el
      | EProj proj ->
        let proj, _ = r_split proj in
        let var, _ = r_split proj.struct_name in
        let moda = e_module_accessor ~loc acc (Value_var.of_input_var var) in
        let sels, _ =
          List.unzip @@ List.map ~f:compile_selection @@ npseq_to_list proj.field_path
        in
        return @@ e_accessor ~loc moda sels
      | EModA ma -> aux (acc @ [ compile_mod_var ma.value.module_name ]) ma.value.field
      | _ -> raise.error (expected_access_to_variable (CST.expr_to_region ma.field))
    in
    aux [ compile_mod_var ma.module_name ] ma.field
  | EUpdate update ->
    let update, _loc = r_split update in
    let record = compile_path update.record in
    let updates, _loc = r_split update.updates in
    let aux (up : CST.field_path_assignment CST.reg) =
      let up, loc = r_split up in
      match up with
      | Path_property up ->
        let path = up.field_path in
        let expr = self up.field_expr in
        let path =
          match path with
          | Name var -> [ Access_path.Access_record var.value ]
          | Path proj ->
            let proj, _ = r_split proj in
            let path, _ =
              List.unzip @@ List.map ~f:compile_selection @@ npseq_to_list proj.field_path
            in
            Access_path.Access_record proj.struct_name.value :: path
        in
        return (path, expr, loc)
      | Path_punned_property up ->
        let path = [ Access_path.Access_record up.value ] in
        let expr = self (EVar up) in
        return (path, expr, loc)
    in
    let updates = List.map ~f:aux @@ npseq_to_list updates.ne_elements in
    let aux e (path, update, loc) = e_update ~loc e path update in
    return @@ List.fold_left ~f:aux ~init:record updates
  | EFun func ->
    (* todo : make it in common with let function *)
    let func, loc = r_split func in
    let ({ binders
         ; rhs_type
         ; body
         ; kwd_fun = _
         ; type_params = _
         ; arrow = _
         ; attributes = _
         }
          : CST.fun_expr)
      =
      func
    in
    let rhs_type = Option.map ~f:(compile_type_expression ~raise <@ snd) rhs_type in
    let (binder, fun_), lst = List.Ne.map (compile_parameter ~raise) binders in
    let body = self body in
    let rec aux lst =
      match lst with
      | [] -> body, rhs_type
      | (binder, fun_) :: lst ->
        let expr, rhs_type = aux lst in
        let expr = fun_ expr in
        ( e_lambda ~loc binder rhs_type expr
        , Option.map ~f:(Utils.uncurry @@ t_arrow ~loc)
          @@ Option.bind_pair (Param.get_ascr binder, rhs_type) )
    in
    let expr, rhs_type = aux lst in
    let expr = fun_ expr in
    return @@ e_lambda ~loc binder rhs_type expr
  | EConstr constr ->
    let (constr, args_o), loc = r_split constr in
    let args_o = Option.map ~f:(compile_tuple_expression <@ List.Ne.singleton) args_o in
    let args =
      Option.value ~default:(e_unit ~loc:(Location.lift constr.region) ()) args_o
    in
    return @@ e_constructor ~loc constr.value args
  | ECase case ->
    let case, loc1 = r_split case in
    let matchee = self case.expr in
    let cases, loc2 = r_split case.cases in
    let loc = Location.cover loc1 loc2 in
    (* TODO: locations are weird here *)
    let cases = compile_matching_expr ~raise @@ npseq_to_ne_list cases in
    return @@ e_matching ~loc matchee cases
  | EAnnot annot ->
    let annot, loc = r_split annot in
    let expr, _, ty = annot.inside in
    let expr = self expr in
    let ty = compile_type_expression ~raise ty in
    return @@ e_annotation ~loc expr ty
  | ECond cond ->
    let cond, loc = r_split cond in
    let test = self cond.test in
    let then_clause = self cond.ifso in
    let else_clause = Option.map ~f:(self <@ snd) cond.ifnot in
    return
    @@ e_cond ~loc test then_clause
    @@ Option.value ~default:(e_unit ~loc ()) else_clause
  | EList lst ->
    (match lst with
    | ECons cons ->
      let cons, loc = r_split cons in
      let a = self cons.arg1 in
      let b = self cons.arg2 in
      return @@ e_constant ~loc (Const C_CONS) [ a; b ]
    | EListComp lc ->
      let lc, loc = r_split lc in
      let lst = Option.value ~default:[] @@ Option.map ~f:npseq_to_list lc.elements in
      let lst = List.map ~f:self lst in
      return @@ e_list ~loc lst)
  | ELetIn li ->
    let li, loc = r_split li in
    let ({ kwd_let = _; kwd_rec; binding; kwd_in = _; body; attributes } : CST.let_in) =
      li
    in
    let let_attr = compile_attributes attributes in
    let body = self body in
    let ({ type_params; binders; rhs_type; eq = _; let_rhs } : CST.let_binding) =
      binding
    in
    let let_rhs = compile_expression ~raise let_rhs in
    let rhs_type = Option.map ~f:(compile_type_expression ~raise <@ snd) rhs_type in
    let pattern, args = binders in
    (match args, type_params, kwd_rec with
    | [], None, None ->
      let matchee =
        match rhs_type with
        | Some t -> e_annotation ~loc:let_rhs.location let_rhs t
        | None ->
          (match unepar pattern with
          | CST.PUnit _ -> e_annotation ~loc:let_rhs.location let_rhs (AST.t_unit ())
          | _ -> let_rhs)
      in
      let pattern = compile_pattern ~raise pattern in
      let pattern =
        Location.map
          (function
            | Pattern.P_var binder ->
              let binder = Binder.set_ascr binder rhs_type in
              AST.Pattern.P_var binder
            | x -> x)
          pattern
      in
      e_let_in ~loc pattern let_attr matchee body
    | _, _, _ ->
      (* function *)
      let let_binder, fun_ = compile_binder ~raise pattern in
      let binders = List.map ~f:(compile_parameter ~raise) args in
      (* collect type annotation for let function declaration *)
      let let_rhs, rhs_type =
        List.fold_right
          ~init:(let_rhs, rhs_type)
          ~f:(fun (b, fun_) (e, a) ->
            ( e_lambda ~loc:(Value_var.get_location @@ Param.get_var b) b a @@ fun_ e
            , Option.map2 ~f:t_arrow (Param.get_ascr b) a ))
          binders
      in
      (* Add polymorphic binder to ascription *)
      let rhs_type =
        Option.map rhs_type ~f:(fun rhs_type ->
            Option.value_map type_params ~default:rhs_type ~f:(fun tp ->
                let tp, loc = r_split tp in
                let tp = tp.inside in
                let type_vars = List.Ne.map compile_type_var tp.type_vars in
                List.Ne.fold_right
                  ~f:(fun tvar t -> t_for_all ~loc tvar Type t)
                  ~init:rhs_type
                  type_vars))
      in
      let let_binder = Binder.map (Fn.const rhs_type) let_binder in
      (* This handle the recursion *)
      let let_rhs =
        match kwd_rec with
        | Some reg ->
          (* Hack for type params in recursive position. Remove them! Better fix would be to relax form of lambda *)
          let fun_type =
            trace_option ~raise (untyped_recursive_fun reg#region) @@ rhs_type
          in
          let _, fun_type = destruct_for_alls fun_type in
          let rec get_first_non_annotation e =
            Option.value_map ~default:e ~f:(fun e ->
                get_first_non_annotation e.Ascription.anno_expr)
            @@ get_e_annotation e
          in
          let lambda =
            trace_option ~raise (recursion_on_non_function loc)
            @@ get_e_lambda
            @@ (get_first_non_annotation let_rhs).expression_content
          in
          let Arrow.{ type1 = arg_type; type2 = ret_type } = get_t_arrow_exn fun_type in
          let lambda =
            Lambda.
              { binder = Param.map (Fn.const arg_type) lambda.binder
              ; result = lambda.result
              ; output_type = ret_type
              }
          in
          e_recursive
            ~loc:(Location.lift reg#region)
            (Binder.get_var let_binder)
            fun_type
            lambda
        | None -> let_rhs
      in
      (* This handle polymorphic annotation (slight hack). Better way would be to relax form of let_rhs. *)
      let let_rhs =
        Option.value_map
          ~default:let_rhs
          ~f:(fun tp ->
            let tp, loc = r_split tp in
            let tp : CST.type_params = tp.inside in
            let type_vars = List.Ne.map compile_type_var tp.type_vars in
            List.Ne.fold_right ~f:(fun t e -> e_type_abs ~loc t e) ~init:let_rhs type_vars)
          type_params
      in
      let pattern =
        Pattern.var ~loc:(Location.lift (CST.pattern_to_region pattern)) let_binder
      in
      return @@ e_let_in ~loc pattern let_attr let_rhs @@ fun_ body)
  | ETypeIn ti ->
    let ti, loc = r_split ti in
    let ({ type_decl = { name; type_expr; _ }; kwd_in = _; body } : CST.type_in) = ti in
    let type_binder = compile_type_var name in
    let rhs = compile_type_expression ~raise type_expr in
    let body = self body in
    return @@ e_type_in ~loc type_binder rhs body
  | EModIn mi ->
    let mi, loc = r_split mi in
    let ({ mod_decl = { kwd_module; name; module_; kwd_end; _ }; kwd_in = _; body }
          : CST.mod_in)
      =
      mi
    in
    let module_binder = compile_mod_var name in
    let rhs =
      let loc = Location.lift @@ CST.Region.cover kwd_module#region kwd_end#region in
      let decls = compile_module ~raise module_ in
      m_struct ~loc decls
    in
    let body = self body in
    return @@ e_mod_in ~loc module_binder rhs body
  | EModAlias ma ->
    (* This node is a bit too generic, should be moved to a dedicated module language *)
    let ma, loc = r_split ma in
    let ({ mod_alias = { alias; binders; _ }; kwd_in = _; body } : CST.mod_alias) = ma in
    let alias = compile_mod_var alias in
    let rhs =
      let path = List.Ne.map compile_mod_var @@ npseq_to_ne_list binders in
      m_path ~loc:Location.generated path
    in
    let body = self body in
    return @@ e_mod_in ~loc alias rhs body
  | ECodeInj ci ->
    let ci, loc = r_split ci in
    let language, _ = r_split ci.language in
    let language, _ = r_split language in
    let code = self ci.code in
    return @@ e_raw_code ~loc language code
  | ESeq seq ->
    let seq, loc = r_split seq in
    let seq = List.map ~f:self @@ pseq_to_list seq.elements in
    (match seq with
    | [] -> return @@ e_unit ~loc ()
    | hd :: tl ->
      let rec aux prev = function
        | [] -> return @@ prev
        | hd :: tl -> (return <@ e_sequence ~loc prev) @@ aux hd tl
      in
      aux hd @@ tl)


and compile_pattern ~raise : CST.pattern -> AST.ty_expr option Pattern.t =
 fun p ->
  let open Pattern in
  match unepar p with
  | CST.PTyped { region; value = { pattern = CST.PVar x; type_expr; _ } } ->
    let b =
      let var = compile_variable { x.value.variable with region } in
      Binder.make var (Some (compile_type_expression ~raise type_expr))
    in
    let loc = Location.lift region in
    Location.wrap ~loc @@ P_var b
  | CST.PVar { region = _; value = { variable; attributes = _todo } } ->
    let b =
      let var = compile_variable variable in
      Binder.make var None
    in
    Location.wrap ~loc:(Binder.get_loc b) @@ P_var b
  | CST.PTuple tuple ->
    let tuple, loc = r_split tuple in
    let lst = npseq_to_ne_list tuple in
    let patterns = List.Ne.to_list lst in
    let nested = List.map ~f:(compile_pattern ~raise) patterns in
    Location.wrap ~loc @@ P_tuple nested
  | CST.PRecord record ->
    let inj, loc = r_split record in
    let aux : CST.field_pattern CST.reg -> Label.t * AST.ty_expr option Pattern.t =
     fun field ->
      let ({ field_name; eq = _; pattern } : CST.field_pattern) = field.value in
      let pattern = compile_pattern ~raise pattern in
      Label field_name.value, pattern
    in
    let lst = List.Ne.map aux @@ npseq_to_ne_list inj.ne_elements in
    let lst = List.Ne.to_list lst in
    Location.wrap ~loc @@ P_record lst
  | CST.PConstr pattern ->
    let (constr, p_opt), loc = r_split pattern in
    let l, ploc = r_split constr in
    let pv_opt =
      match p_opt with
      | Some p -> compile_pattern ~raise p
      | None -> Location.wrap ~loc:ploc P_unit
    in
    Location.wrap ~loc @@ P_variant (Label l, pv_opt)
  | CST.PList list_pattern ->
    let repr =
      match list_pattern with
      | PListComp p_inj ->
        let loc = Location.lift p_inj.region in
        (match p_inj.value.elements with
        | None -> Location.wrap ~loc @@ P_list (List [])
        | Some lst ->
          let lst = Utils.nsepseq_to_list lst in
          let aux
              :  CST.pattern -> AST.type_expression option Pattern.t
              -> AST.type_expression option Pattern.t
            =
           fun p acc ->
            let p' = compile_pattern ~raise p in
            Location.wrap (P_list (Cons (p', acc)))
          in
          let conscomb =
            List.fold_right ~f:aux ~init:(Location.wrap ~loc (P_list (List []))) lst
          in
          conscomb)
      | PCons p ->
        let loc = Location.lift p.region in
        let hd, _, tl = p.value in
        let hd = compile_pattern ~raise hd in
        let tl = compile_pattern ~raise tl in
        Location.wrap ~loc @@ P_list (Cons (hd, tl))
    in
    repr
  | CST.PUnit u ->
    let loc = Location.lift u.region in
    Location.wrap ~loc @@ P_unit
  | _ -> raise.error @@ unsupported_pattern_type [ p ]


and compile_matching_expr ~raise
    :  'a CST.case_clause CST.reg List.Ne.t
    -> (AST.expression, AST.ty_expr option) Match_expr.match_case list
  =
 fun cases ->
  let aux (case : 'a CST.case_clause CST.reg) =
    let case, _loc = r_split case in
    let expr = compile_expression ~raise case.rhs in
    case.pattern, expr
  in
  let cases = List.Ne.map aux cases in
  let cases : (CST.pattern * AST.expression) list = List.Ne.to_list cases in
  let aux
      :  CST.pattern * AST.expression
      -> (AST.expression, AST.ty_expr option) Match_expr.match_case
    =
   fun (raw_pattern, body) ->
    let pattern = compile_pattern ~raise raw_pattern in
    { pattern; body }
  in
  List.map ~f:aux cases


and unepar = function
  | CST.PPar { value = { inside; _ }; _ } -> unepar inside
  | _ as v -> v


and untpar = function
  | CST.TPar { value = { inside; _ }; _ } -> untpar inside
  | _ as v -> v


and check_annotation ~raise = function
  | CST.PVar var -> raise.error (missing_funarg_annotation var.value.variable)
  | CST.PPar { value = { inside; _ }; _ } -> check_annotation ~raise inside
  | CST.PTuple { value; _ } ->
    let l = Utils.nsepseq_to_list value in
    List.iter ~f:(check_annotation ~raise) l
  | CST.PTyped { value = { pattern; type_expr; _ }; _ } ->
    let (pattern : CST.pattern) = unepar pattern in
    let (type_expr : CST.type_expr) = untpar type_expr in
    (match pattern, type_expr with
    | PTuple { value = pval; region }, TProd { value = tval; _ } ->
      let no_of_tuple_components = List.length (Utils.nsepseq_to_list pval) in
      let no_of_tuple_type_components = List.length (Utils.nsepseq_to_list tval) in
      if no_of_tuple_components <> no_of_tuple_type_components
      then raise.error (funarg_tuple_type_mismatch region pattern type_expr)
      else ()
    | _ -> ())
  | _ -> ()


and compile_binder ~raise : CST.pattern -> _ Binder.t * (_ -> _) =
 fun pattern ->
  let return ?ascr fun_ var = Binder.make var ascr, fun_ in
  let return_1 ?ascr var = return ?ascr (fun e -> e) var in
  match pattern with
  | PConstr _ -> raise.error @@ unsupported_pattern_type [ pattern ]
  | PUnit the_unit ->
    let loc = Location.lift the_unit.region in
    return_1 ~ascr:(t_unit ~loc ()) @@ Value_var.fresh ~loc ()
  | PVar pvar ->
    let pvar, _loc = r_split pvar in
    (* TODO: shouldn't _loc be used somewhere bellow ?*)
    let ({ variable; _ } : CST.var_pattern) = pvar in
    return_1 @@ compile_variable variable
  | PTuple tuple ->
    let tuple, loc = r_split tuple in
    let var = Value_var.fresh ~loc () in
    let aux pattern (binder_lst, fun_) =
      let binder, fun_' = compile_binder ~raise pattern in
      binder :: binder_lst, fun_' <@ fun_
    in
    let binder_lst, fun_ =
      List.fold_right ~f:aux ~init:([], fun e -> e) @@ npseq_to_list tuple
    in
    let expr expr = e_matching_tuple ~loc (e_variable ~loc var) binder_lst @@ fun_ expr in
    let ascr = Option.all @@ List.map ~f:Binder.get_ascr binder_lst in
    let ascr = Option.map ~f:t_tuple ascr in
    return ?ascr expr var
  | PPar par ->
    let par, _loc = r_split par in
    let binder, expr = compile_binder ~raise par.inside in
    binder, expr
  | PRecord record ->
    let record, loc = r_split record in
    let var = Value_var.fresh ~loc () in
    let aux
        ({ value = { field_name; eq = _; pattern }; _ } : CST.field_pattern CST.reg)
        (binder_lst, fun_')
      =
      let field_name = field_name.value in
      let binder, fun_ = compile_binder ~raise pattern in
      (field_name, binder) :: binder_lst, fun_ <@ fun_'
    in
    let binder_lst, fun_ =
      List.fold_right ~f:aux ~init:([], fun e -> e) @@ npseq_to_list record.ne_elements
    in
    let expr expr =
      e_matching_record ~loc (e_variable ~loc:(Value_var.get_location var) var) binder_lst
      @@ fun_ expr
    in
    let ascr = Option.all @@ List.map ~f:(Fn.compose Binder.get_ascr snd) binder_lst in
    let ascr = Option.map ~f:t_tuple ascr in
    return ?ascr expr var
  | PTyped tp ->
    let tp, _loc = r_split tp in
    let ({ pattern; type_expr; colon = _ } : CST.typed_pattern) = tp in
    let ascr = compile_type_expression ~raise type_expr in
    let (b : _ Binder.t), exprs = compile_binder ~raise pattern in
    Binder.map (Fn.const (Some ascr)) b, exprs
  | _ -> raise.error @@ unsupported_pattern_type [ pattern ]


and compile_parameter ~raise : CST.pattern -> _ Param.t * (_ -> _) =
 fun pattern ->
  let return ?ascr fun_ var = Param.make var ascr, fun_ in
  let return_1 ?ascr var = return ?ascr (fun e -> e) var in
  match pattern with
  | PConstr _ -> raise.error @@ unsupported_pattern_type [ pattern ]
  | PUnit the_unit ->
    let loc = Location.lift the_unit.region in
    return_1 ~ascr:(t_unit ~loc ()) @@ Value_var.fresh ~loc ()
  | PVar pvar ->
    let pvar, _loc = r_split pvar in
    (* TODO: shouldn't _loc be used somewhere bellow ?*)
    let ({ variable; _ } : CST.var_pattern) = pvar in
    return_1 @@ compile_variable variable
  | PTuple tuple ->
    let tuple, loc = r_split tuple in
    let var = Value_var.fresh ~loc () in
    let aux pattern (binder_lst, fun_) =
      let binder, fun_' = compile_parameter ~raise pattern in
      binder :: binder_lst, fun_' <@ fun_
    in
    let binder_lst, fun_ =
      List.fold_right ~f:aux ~init:([], fun e -> e) @@ npseq_to_list tuple
    in
    let expr expr =
      e_param_matching_tuple ~loc (e_variable ~loc var) binder_lst @@ fun_ expr
    in
    let ascr = Option.all @@ List.map ~f:Param.get_ascr binder_lst in
    let ascr = Option.map ~f:t_tuple ascr in
    return ?ascr expr var
  | PPar par ->
    let par, _loc = r_split par in
    let binder, expr = compile_parameter ~raise par.inside in
    binder, expr
  | PRecord record ->
    let record, loc = r_split record in
    let var = Value_var.fresh ~loc () in
    let aux
        ({ value = { field_name; eq = _; pattern }; _ } : CST.field_pattern CST.reg)
        (binder_lst, fun_')
      =
      let field_name = field_name.value in
      let binder, fun_ = compile_parameter ~raise pattern in
      (field_name, binder) :: binder_lst, fun_ <@ fun_'
    in
    let binder_lst, fun_ =
      List.fold_right ~f:aux ~init:([], fun e -> e) @@ npseq_to_list record.ne_elements
    in
    let expr expr =
      e_param_matching_record ~loc (e_variable var) binder_lst @@ fun_ expr
    in
    let ascr = Option.all @@ List.map ~f:(Fn.compose Param.get_ascr snd) binder_lst in
    let ascr = Option.map ~f:t_tuple ascr in
    return ?ascr expr var
  | PTyped tp ->
    let tp, _loc = r_split tp in
    let ({ pattern; type_expr; colon = _ } : CST.typed_pattern) = tp in
    let ascr = compile_type_expression ~raise type_expr in
    let (b : _ Param.t), exprs = compile_parameter ~raise pattern in
    Param.map (Fn.const (Some ascr)) b, exprs
  | _ -> raise.error @@ unsupported_pattern_type [ pattern ]


and compile_declaration ~raise : CST.declaration -> AST.declaration option =
 fun decl ->
  let return reg decl = Some (Location.wrap ~loc:(Location.lift reg) decl) in
  let skip = None in
  match decl with
  | Directive _ -> skip (* Directives are not propagated to the AST *)
  | TypeDecl { value = { name; type_expr; params; kwd_type = _; eq = _ }; region } ->
    let name, loc = r_split name in
    let type_expr =
      let rhs = compile_type_expression ~raise type_expr in
      match params with
      | None -> rhs
      | Some x ->
        let lst = type_vars_to_list x in
        let aux : CST.type_var Region.reg -> AST.type_expression -> AST.type_expression =
         fun param type_ ->
          let param, ploc = r_split param in
          let ty_binder = Type_var.of_input_var ~loc:ploc (quote_var param.name.value) in
          t_abstraction ~loc:(Location.lift region) ty_binder Type type_
        in
        List.fold_right ~f:aux ~init:rhs lst
    in
    return
      region
      (D_type { type_binder = Type_var.of_input_var ~loc name; type_expr; type_attr = [] })
  | ModuleDecl { value = { kwd_module; name; module_; kwd_end; _ }; region } ->
    let module_binder = compile_mod_var name in
    let module_ =
      let loc = Location.lift @@ CST.Region.cover kwd_module#region kwd_end#region in
      let decls = compile_module ~raise module_ in
      m_struct ~loc decls
    in
    let ast = D_module { module_binder; module_; module_attr = [] } in
    return region ast
  | ModuleAlias { value = { alias; binders; _ }; region } ->
    let module_binder = compile_mod_var alias in
    let module_ =
      let path = List.Ne.map compile_mod_var @@ npseq_to_ne_list binders in
      m_path ~loc:Location.generated path (* wrong location *)
    in
    return region (D_module { module_binder; module_; module_attr = [] })
  | Let { value = _kwd_let, kwd_rec, let_binding, attributes; region } ->
    let attr = compile_attributes attributes in
    let ({ type_params; binders; rhs_type; eq = _; let_rhs } : CST.let_binding) =
      let_binding
    in
    let type_params =
      match type_params with
      | Some _ -> type_params
      | None ->
        (match let_rhs with
        | EFun { value = { type_params; _ }; _ } -> type_params
        | _ -> None)
    in
    let pattern, args = binders in
    let let_rhs = compile_expression ~raise let_rhs in
    let rhs_type = Option.map ~f:(compile_type_expression ~raise <@ snd) rhs_type in
    (*
    function :
      let (type a b) x = ..
      let x foo bar = ..
      let (type a b) foo bar = ..
    not function:
      let x = ..
      let (x,y) = ..   
    *)
    (match args, type_params with
    | [], None ->
      (* not function *)
      (*
        let x : ty = body
        let x = (body : ty)
      *)
      let attr = compile_attributes attributes in
      let pattern = compile_pattern ~raise pattern in
      (match Location.unwrap pattern with
      | P_var binder ->
        let loc = pattern.location in
        let binder = Binder.set_ascr binder rhs_type in
        let pattern = Location.wrap ~loc (AST.Pattern.P_var binder) in
        (* For patterns, we annotate the rhs : let <pattern> : <type> = <expr> |-> let <pattern> = <expr> : <type> *)
        let let_rhs =
          Option.value_map rhs_type ~default:let_rhs ~f:(fun ty ->
              e_annotation ~loc:let_rhs.location let_rhs ty)
        in
        return region (D_irrefutable_match { pattern; attr; expr = let_rhs })
      | _ ->
        (* For patterns, we annotate the rhs : let <pattern> : <type> = <expr> |-> let <pattern> = <expr> : <type> *)
        let let_rhs =
          Option.value_map rhs_type ~default:let_rhs ~f:(fun ty ->
              e_annotation ~loc:let_rhs.location let_rhs ty)
        in
        return region (D_irrefutable_match { pattern; attr; expr = let_rhs }))
    | _, _ ->
      (* function *)
      let binder, _fun_ = compile_binder ~raise pattern in
      let params = List.map ~f:(compile_parameter ~raise) args in
      (* collect type annotation for let function declaration *)
      let let_rhs, rhs_type =
        List.fold_right
          ~init:(let_rhs, rhs_type)
          ~f:(fun (b, fun_) (e, a) ->
            ( e_lambda ~loc:(Value_var.get_location @@ Param.get_var b) b a @@ fun_ e
            , Option.map2 ~f:t_arrow (Param.get_ascr b) a ))
          params
      in
      (* This handle polymorphic annotation *)
      let rhs_type : type_expression option =
        Option.map rhs_type ~f:(fun rhs_type ->
            Option.value_map type_params ~default:rhs_type ~f:(fun tp ->
                let tp, loc = r_split tp in
                let tp = tp.inside in
                let type_vars = List.Ne.map compile_type_var tp.type_vars in
                List.Ne.fold_right
                  ~f:(fun tvar t -> t_for_all ~loc tvar Type t)
                  ~init:rhs_type
                  type_vars))
      in
      let binder = Binder.map (Fn.const rhs_type) binder in
      (* This handle the recursion *)
      let let_rhs =
        match kwd_rec with
        | Some reg ->
          let fun_type =
            trace_option ~raise (untyped_recursive_fun reg#region) @@ rhs_type
          in
          let _, fun_type = destruct_for_alls fun_type in
          let rec get_first_non_annotation e =
            Option.value_map ~default:e ~f:(fun e ->
                get_first_non_annotation e.Ascription.anno_expr)
            @@ get_e_annotation e
          in
          let lambda =
            trace_option ~raise (recursion_on_non_function @@ Location.lift region)
            @@ get_e_lambda
            @@ (get_first_non_annotation let_rhs).expression_content
          in
          let Arrow.{ type1; type2 } = get_t_arrow_exn fun_type in
          let lambda =
            Lambda.
              { lambda with
                binder = Param.map (Fn.const type1) lambda.binder
              ; output_type = type2
              }
          in
          e_recursive
            ~loc:(Location.lift reg#region)
            (Binder.get_var binder)
            fun_type
            lambda
        | None -> let_rhs
      in
      (* This handle polymorphic functions (type abstraction) *)
      let let_rhs =
        Option.value_map
          ~default:let_rhs
          ~f:(fun tp ->
            let tp, loc = r_split tp in
            let tp : CST.type_params = tp.inside in
            let type_vars = List.Ne.map compile_type_var tp.type_vars in
            List.Ne.fold_right ~f:(fun t e -> e_type_abs ~loc t e) ~init:let_rhs type_vars)
          type_params
      in
      return region @@ D_value { binder; attr; expr = let_rhs })


and compile_module ~raise : CST.ast -> AST.module_ =
 fun t -> List.filter_map ~f:(compile_declaration ~raise) @@ nseq_to_list t.decl


let compile_program ~raise : CST.ast -> AST.program =
 fun t ->
  nseq_to_list t.decl
  |> List.map ~f:(fun a ~raise -> compile_declaration ~raise a)
  |> Simple_utils.Trace.collect ~raise
  |> List.filter_map ~f:Fun.id
