open Errors

open Simple_utils.Trace
open Simple_utils.Function

module Utils = Simple_utils.Utils

module CST = Cst.Pascaligo
module AST = Ast_imperative
module Attr = Lexing_shared.Attr
module Location = Simple_utils.Location

open Ligo_prim
open AST

let nseq_to_list (hd, tl) = hd :: tl
let npseq_to_list (hd, tl) = hd :: (List.map ~f:snd tl)
let npseq_to_ne_list (hd, tl) = (hd, List.map ~f:snd tl)

let pseq_to_list = function
  None -> []
| Some seq -> npseq_to_list seq

let built_ins = ["Operator";"Tezos";"List";"Set";"Map";"Big_map";"Bitwise";"Option"]

open Predefined.Tree_abstraction

let check_no_attributes ~(raise:(Errors.abs_error,Main_warnings.all) Simple_utils.Trace.raise) (loc: Location.t) lst =
  (* TODO: should be done in a dedicated pass ?*)
  if not (List.is_empty lst) then raise.error (ignored_attribute loc)

let r_split = Location.r_split

let w_split (x: 'a CST.Wrap.t) : 'a * Location.t =
  (x#payload, Location.lift x#region)

let compile_variable var = let (var,loc) = w_split var in Value_var.of_input_var ~loc var
let compile_type_var var = let (var,loc) = w_split var in Type_var.of_input_var ~loc var
let compile_mod_var var = let (var,loc) = w_split var in Module_var.of_input_var ~loc var

let rec get_var : CST.expr -> (string * Location.t) option =
  function
  | E_Par x -> get_var x.value.inside
  | E_Var v -> Some (w_split v)
  | _ -> None
let compile_var_opt : CST.expr -> Value_var.t option = fun expr ->
  Option.map (get_var expr) ~f:(fun (v,loc) -> Value_var.of_input_var ~loc v)

let compile_attributes : CST.attribute list -> AST.attributes = fun attr ->
  let f : CST.attribute -> string =
    fun x ->
      let ((k,v_opt),_loc) = r_split x in
      match v_opt with
      | Some (String v) -> String.concat ~sep:":" [k;v]
      | None -> k
  in List.map ~f attr

let compile_selection : CST.selection -> 'a Access_path.access * Location.t = function
  | FieldName name ->
    let name, loc = w_split name in
    Access_record name, loc
  | Component comp ->
    let (_, index), loc = w_split comp in
    Access_tuple  index, loc

let rec compile_type_expression ~(raise :(Errors.abs_error,Main_warnings.all) Simple_utils.Trace.raise) : ?attr:CST.attribute list -> CST.type_expr -> AST.type_expression =
  fun ?(attr = []) te ->
    let self = compile_type_expression ~raise in
    match te with
    | T_Attr (x,te) -> compile_type_expression ~attr:(x::attr) ~raise te
    | T_Var v ->
      let (v,loc) = w_split v in
      t_variable_ez ~loc v

    | T_Cart {region; value = (fst, _, rest)} ->
      let loc = Location.lift region in
      let lst = List.map ~f:self (fst::npseq_to_list rest) in
      t_tuple ~loc lst

    | T_Sum { region ; value } ->
      let loc = Location.lift region in
      let attr = compile_attributes attr in
      let cases = Utils.nsepseq_to_list value.variants in
      let f : CST.variant CST.reg -> string * AST.type_expression * string list =
        fun { value = {ctor ; ctor_args ; attributes} ; region = _ } ->
          let t = Option.value ~default:(t_unit ()) (Option.map ~f:(self <@ snd) ctor_args) in
          let case_attr = compile_attributes attributes in
          (ctor#payload, t, case_attr)
      in
      t_sum_ez_attr ~loc ~attr (List.map ~f cases)

    | T_App { region ; value = (type_constant, args) } -> (
      let loc = Location.lift region in
      let get_t_string_singleton_opt =
        function
        | CST.T_String s -> Some s#payload
        | _ -> None
      in
      let get_t_int_singleton_opt = function
        | CST.T_Int x ->
          let (_,z) = x#payload in
          Some z
        | _ -> None
      in
      match type_constant with
      | T_Var v when String.equal v#payload "michelson_or" -> (
        let lst = npseq_to_list args.value.inside in
        match lst with
        | [a ; b ; c ; d ] -> (
          let b' = trace_option ~raise (michelson_type_wrong te v#payload) @@ get_t_string_singleton_opt b in
          let d' = trace_option ~raise (michelson_type_wrong te v#payload) @@ get_t_string_singleton_opt d in
          let a' = self a in
          let c' = self c in
          t_michelson_or ~loc a' b' c' d'
          )
        | _ -> raise.error @@ michelson_type_wrong_arity loc v#payload
      )
      | T_Var v when String.equal v#payload "michelson_pair" -> (
        let lst = npseq_to_list args.value.inside in
        match lst with
        | [a ; b ; c ; d ] -> (
          let b' = trace_option ~raise (michelson_type_wrong te v#payload) @@ get_t_string_singleton_opt b in
          let d' = trace_option ~raise (michelson_type_wrong te v#payload) @@ get_t_string_singleton_opt d in
          let a' = self a  in
          let c' = self c  in
          t_michelson_pair ~loc a' b' c' d'
        )
        | _ -> raise.error @@ michelson_type_wrong_arity loc v#payload
      )
      | T_Var v when String.equal v#payload "sapling_state" -> (
        let lst = npseq_to_list args.value.inside in
        match lst with
        | [(a : CST.type_expr)] -> (
          let sloc = Location.lift @@ Raw.type_expr_to_region a in
          let a' = trace_option ~raise (michelson_type_wrong te v#payload) @@ get_t_int_singleton_opt a in
          let singleton = t_singleton ~loc:sloc (Literal_int a') in
          t_sapling_state ~loc singleton
        )
        | _ -> raise.error @@ michelson_type_wrong_arity loc v#payload
      )
      | T_Var v when String.equal v#payload "sapling_transaction" -> (
        let lst = npseq_to_list args.value.inside in
        match lst with
        | [(a : CST.type_expr)] -> (
          let sloc = Location.lift @@ CST.type_expr_to_region a in
          let a' = trace_option ~raise (michelson_type_wrong te v#payload) @@ get_t_int_singleton_opt a in
          let singleton = t_singleton ~loc:sloc (Literal_int a') in
          t_sapling_transaction ~loc singleton
          )
        | _ -> raise.error @@ michelson_type_wrong_arity loc v#payload
      )
      | T_Var type_var -> (
        let operator = compile_type_var type_var in
        let lst = npseq_to_list args.value.inside in
        let lst = List.map ~f:self lst in
        t_app ~loc operator lst
      )
      | T_ModPath x ->
        raise.error (expected_variable (Location.lift @@ CST.type_expr_to_region x.value.field))
      | _ -> raise.error (expected_variable (Location.lift @@ CST.type_expr_to_region type_constant))
    )
    | T_Fun { region ; value = (lhs , _ , rhs) } -> (
      let loc = Location.lift region in
      t_arrow ~loc (self lhs) (self rhs)
    )
    | T_ModPath { region ; value = { module_path ; selector = _ ; field } } -> (
      let loc = Location.lift region in
      let module_path = List.map ~f:compile_mod_var (npseq_to_list module_path) in
      match field with
      | T_Var v ->
        t_module_accessor ~loc module_path (compile_type_var v)
      | _ -> raise.error (expected_variable (Location.lift @@ CST.type_expr_to_region field))
    )
    | T_Par { region ; value = { lpar = _ ; inside ; rpar = _} } -> (
      let loc = Location.lift region in
      let inside = self inside in
      { inside with location = loc }
    )
    | T_Record {region; value = {kind = _loc; opening =_;
                                 elements; closing=_; terminator=_}} -> (
      let elements = Utils.sepseq_to_list elements in
      let f : CST.field_decl CST.reg -> string * AST.type_expression * AST.attributes = fun decl ->
        let (({field_name;field_type;attributes} : CST.field_decl), _loc) = r_split decl in
        let t =
          match field_type with
          | None -> (* type punning: { x } -> { x : x } *)
            let (v , loc) = w_split field_name in
            t_variable_ez ~loc v
          | Some (_colon , x) -> self x
        in
        let attributes = compile_attributes attributes in
        (field_name#payload , t ,attributes)
      in
      let lst = List.map ~f elements in
      t_record_ez_attr ~loc:(Location.lift region) ~attr:(compile_attributes attr) lst
    )
    | T_Int _ | T_String _ -> raise.error @@ unsupported_string_singleton te

let rec compile_expression ~(raise :(Errors.abs_error,Main_warnings.all) Simple_utils.Trace.raise) : ?attr:CST.attribute list -> CST.expr -> AST.expr = fun ?(attr = []) e ->
  let self = compile_expression ~raise
  in
  let compile_arguments
    : (CST.expr, CST.comma) Utils.sepseq CST.par Region.reg -> AST.expression =
    fun args ->
      let par, loc = r_split args in
      let lst = List.map ~f:self (Utils.sepseq_to_list par.inside)
      in match lst with
           [] -> e_unit ~loc ()
         | [expr] -> expr
         | args -> e_tuple ~loc args
  in
  let compile_tuple_expression : CST.expr CST.tuple -> AST.expression = fun tuple_expr ->
    let (lst, loc) = r_split tuple_expr in
    let (hd,tl) = lst.inside in
    let tl = List.map ~f:snd tl in
    match tl with
    | [] -> self hd
    | _ ->
      let lst = List.map ~f:self (hd::tl) in
      e_tuple ~loc lst
  in
  let compile_bin_op : Constant.constant' ->  _ CST.bin_op CST.reg -> AST.expression = fun op_type op ->
    let (op, loc) = r_split op in
    let a = self op.arg1 in
    let b = self op.arg2 in
    e_constant ~loc (Const op_type) [a; b]
  in
  let compile_un_op : Constant.constant' -> _ CST.un_op CST.reg -> AST.expression = fun op_type op ->
    let (op, loc) = r_split op in
    let arg = self op.arg in
    e_constant ~loc (Const op_type) [arg]
  in
  let rec compile_pseudomodule_access : CST.expr -> string = fun field ->
    match field with
    | E_Var v -> v#payload
    | E_ModPath { value = { module_path ; field ; selector = _ } ; region = _ } ->
       Utils.nsepseq_foldr (fun module_name b -> module_name#payload ^ "." ^ b) module_path (compile_pseudomodule_access field)
    | _ -> failwith "Corner case : This couldn't be produce by the parser"
  in
  match e with
  | E_Var var -> (
    let (var, loc) = w_split var in
    match constants var with
    | Some const -> e_constant ~loc const []
    | None -> e_variable_ez ~loc var
  )
  | E_Par par -> self par.value.inside
  | E_Bytes bytes_ ->
    let (bytes_, loc) = w_split bytes_ in
    let (_s,b) = bytes_ in
    e_bytes_hex ~loc b
  | E_String str ->
    let (str, loc) = w_split str in
    e_string ~loc str
  | E_Cat c ->
    let (op,loc) = r_split c in
    let a = self op.arg1 in
    let b = self op.arg2 in
    e_constant ~loc (Const C_CONCAT) [a;b]
  | E_Verbatim str ->
    let (str, loc) = w_split str in
    e_verbatim ~loc str
  | E_Add plus   -> compile_bin_op C_ADD plus
  | E_Sub minus  -> compile_bin_op C_POLYMORPHIC_SUB minus
  | E_Mult times -> compile_bin_op C_MUL times
  | E_Div slash  -> compile_bin_op C_DIV slash
  | E_Mod mod_   -> compile_bin_op C_MOD mod_
  | E_Neg minus  -> compile_un_op C_NEG minus
  | E_Int i ->
    let ((_,i), loc) = w_split i in
    e_int_z ~loc i
  | E_Nat n ->
    let ((_,n), loc) = w_split n in
    e_nat_z ~loc n
  | E_Mutez mtez ->
    let ((_,mtez), loc) = w_split mtez in
    e_mutez_z ~loc (Z.of_int64 mtez)
  | E_Or or_   -> compile_bin_op C_OR  or_
  | E_And and_ -> compile_bin_op C_AND and_
  | E_Not not_ -> compile_un_op  C_NOT not_
  | E_Lt lt    -> compile_bin_op C_LT  lt
  | E_Leq le   -> compile_bin_op C_LE  le
  | E_Gt gt    -> compile_bin_op C_GT  gt
  | E_Geq ge   -> compile_bin_op C_GE  ge
  | E_Equal eq -> compile_bin_op C_EQ  eq
  | E_Neq ne   -> compile_bin_op C_NEQ ne
  | E_Call {value=(E_Var var,args);region} -> (
    let loc = Location.lift region in
    let (var, loc_var) = w_split var in
    match constants var with
      Some const ->
      let (args, _) = r_split args in
      let args = List.map ~f:self @@ pseq_to_list args.inside in
      e_constant ~loc const args
    | None ->
      let func = e_variable_ez ~loc:loc_var var in
      let args = compile_arguments args in
      e_application ~loc func args
  )
  (*TODO: move to proper module*)
  | E_Call ({ value=( E_ModPath { value={ module_path = (module_name,[]) ; _ }; region=_ } as value, args ); region } as call)
      when List.mem ~equal:String.equal built_ins module_name#payload -> (
    let loc = Location.lift region in
    let var = compile_pseudomodule_access value in
    match constants var with
    | Some const ->
       let (args, _) = r_split args in
       let args = List.map ~f:self @@ pseq_to_list args.inside in
       e_constant ~loc const args
    | None ->
       let ((func, args), loc) = r_split call in
       let func = self func in
       let args = compile_arguments args in
       e_application ~loc func args
  )
  | E_Call call ->
    let ((func, args), loc) = r_split call in
    let func = self func in
    let args = compile_arguments args in
    e_application ~loc func args
  | E_Tuple lst ->
    compile_tuple_expression lst
  | E_Record record ->
    let (record, loc) = r_split record in
    let aux : (CST.expr, CST.expr) CST.field CST.reg -> string * AST.expression =
      fun field ->
      let (fa, loc_field) = r_split field in
      match fa with
      | CST.Punned { pun ; attributes } ->
        check_no_attributes ~raise loc_field attributes ;
        let (label,_) = trace_option ~raise (expected_field_name @@ CST.expr_to_region pun) @@ get_var pun in
        if String.equal label "_" then raise.error (unexpected_wildcard @@ CST.expr_to_region pun) ;
        (label, self pun)
      | CST.Complete {field_lhs ; field_lens ; field_rhs ; attributes } -> (
        check_no_attributes ~raise loc_field attributes ;
        match field_lens with
        | Lens_Id _ ->
          let (label,_) = trace_option ~raise (expected_field_name @@ CST.expr_to_region field_lhs) @@ get_var field_lhs in
          (label, self field_rhs)
        | _ -> raise.error (wrong_functional_lens @@ CST.field_lens_to_region field_lens)
      )
    in
    let record = List.map ~f:aux @@ Utils.sepseq_to_list record.elements in
    e_record_ez ~loc record
  | E_Proj proj ->
    let (proj, loc) = r_split proj in
    let expr = self proj.record_or_tuple in
    let (sels, _) = List.unzip @@ List.map ~f:compile_selection @@ Utils.nsepseq_to_list proj.field_path in
    e_accessor ~loc expr sels
  | E_ModPath ma -> (
    let (ma, loc) = r_split ma in
    match ma.module_path with
    | (module_name,[]) when List.mem ~equal:String.(=) built_ins module_name#payload -> (
      (*TODO: move to proper module*)
      let var = compile_pseudomodule_access e in
      match constants var with
      | Some const -> e_constant ~loc const []
      | None -> (
        let module_path = List.map ~f:compile_mod_var (npseq_to_list ma.module_path) in
        match ma.field with
        | E_Var v ->
           e_module_accessor ~loc module_path (compile_variable v)
        | E_Proj proj ->
           let (proj, loc) = r_split proj in
           (* let expr = self proj.record_or_tuple in *)
           let var =
             let (x,loc) = trace_option ~raise (expected_variable (Location.lift @@ CST.expr_to_region proj.record_or_tuple)) @@ get_var proj.record_or_tuple in
             Value_var.of_input_var ~loc x
           in
           let (sels, _) = List.unzip @@ List.map ~f:compile_selection @@ Utils.nsepseq_to_list proj.field_path in
           e_accessor ~loc (e_module_accessor ~loc module_path var) sels
        | _ -> raise.error (expected_variable (Location.lift @@ CST.expr_to_region ma.field))
      )
    )
    | _ -> (
      let module_path = List.map ~f:compile_mod_var (npseq_to_list ma.module_path) in
      match ma.field with
      | E_Var v ->
        e_module_accessor ~loc module_path (compile_variable v)
      | E_Proj proj ->
        let (proj, loc) = r_split proj in
        (* let expr = self proj.record_or_tuple in *)
        let var =
          let (x,loc) = trace_option ~raise (expected_variable (Location.lift @@ CST.expr_to_region proj.record_or_tuple)) @@ get_var proj.record_or_tuple in
          Value_var.of_input_var ~loc x
        in
        let (sels, _) = List.unzip @@ List.map ~f:compile_selection @@ Utils.nsepseq_to_list proj.field_path in
        e_accessor ~loc (e_module_accessor ~loc module_path var) sels
      | _ -> raise.error (expected_variable (Location.lift @@ CST.expr_to_region ma.field))
    )
  )
  | E_Update { value = { structure ; kwd_with=_ ; update } ; region } -> (
    let loc = Location.lift region in
    let structure = self structure in
    match update with
    | E_Record record_lhs -> (
      let f : AST.expression -> (CST.expr, CST.expr) CST.field CST.reg -> AST.expression = fun acc x ->
        let field_loc = Location.lift x.region in
        match x.value with
        | CST.Complete {field_lhs ; field_lens ; field_rhs ; attributes} -> (
          check_no_attributes ~raise field_loc attributes;
          let field_rhs = self field_rhs in
          let func_update self_accessor =
            match field_lens with
            | Lens_Id _ -> field_rhs
            | Lens_Add _ -> e_add ~loc:field_loc self_accessor field_rhs
            | Lens_Sub _ -> e_sub ~loc:field_loc self_accessor field_rhs
            | Lens_Mult _ -> e_mult ~loc:field_loc self_accessor field_rhs
            | Lens_Div _ -> e_div ~loc:field_loc self_accessor field_rhs
            | Lens_Fun _ -> e_application ~loc:field_loc self_accessor field_rhs
          in
          match field_lhs with
          | CST.E_Var x -> (
            let label = fst @@ w_split x in
            let self_accessor = e_accessor ~loc structure [Access_record label] in
            e_update ~loc acc [Access_record label] (func_update self_accessor)
          )
          | CST.E_Proj {region ; value = {record_or_tuple ; selector = _ ; field_path }} -> (
            let (label,_) = trace_option ~raise (expected_variable (Location.lift @@ CST.expr_to_region record_or_tuple)) @@ get_var record_or_tuple in
            let path =
              let path = List.map (Utils.nsepseq_to_list field_path)
                ~f:(function FieldName x -> Access_path.Access_record x#payload | Component x -> Access_tuple (snd @@ fst @@ w_split x))
              in
              (Access_path.Access_record label::path)
            in
            let self_accessor = e_accessor ~loc:(Location.lift region) structure path in
            e_update ~loc acc path (func_update self_accessor)
          )
          | x -> raise.error (expected_field_or_access @@ CST.expr_to_region x)
        )
        | CST.Punned {pun ; attributes} -> (
          check_no_attributes ~raise field_loc attributes;
          let label,v =
            let (label,loc) = trace_option ~raise (expected_variable (Location.lift @@ CST.expr_to_region pun)) @@ get_var pun in
            if String.equal label "_" then raise.error (unexpected_wildcard @@ CST.expr_to_region pun) ;
            (label, Value_var.of_input_var ~loc label)
          in
          e_update ~loc acc [Access_record label] (e_variable v)
        )
      in
      List.fold_left (Utils.sepseq_to_list record_lhs.value.elements) ~f ~init:structure
    )
    | x -> raise.error (wrong_functional_updator @@ CST.expr_to_region x)
  )
  | E_Fun { value = { parameters; ret_type ; return ; _ } ; region} -> (
    check_no_attributes ~raise (Location.lift region) attr ;
    let compile_param : CST.param_decl CST.reg -> _ Binder.t  = fun { value = { param_kind ; pattern ; param_type } ; region = _ } ->
      (* TODO: feels wrong, binders do not have loc in AST *)
      let var =
        match pattern with
        | P_Var x -> compile_variable x
        | x -> raise.error (unsuported_pattern_in_function @@ CST.pattern_to_region x)
      in
      let ascr = Option.map ~f:(compile_type_expression ~raise <@ snd) param_type in
      let attributes =
        match param_kind with
        | `Const _kwd -> Binder.const_attribute
        | `Var _kwd -> Binder.var_attribute
      in
      { var ; ascr ; attributes }
    in
    let loc = Location.lift region in
    let (lambda, fun_type) =
      let params, loc_par = r_split parameters in
      let params = Utils.sepseq_map compile_param params.inside in
      let body = self return in
      let ret_ty = Option.map ~f:(compile_type_expression ~raise <@ snd ) ret_type in
      let params = Utils.sepseq_to_list params in
      match params with
        [] ->
          let ty_opt = Option.map ~f:(fun (a,b) -> t_arrow ~loc a b)
                                  (Option.bind_pair (None, ret_ty))
          in (e_unit ~loc (), ty_opt)
      | [param] ->
        let expr = e_lambda ~loc param ret_ty body in
        let ty_opt = Option.map ~f:(fun (a,b) -> t_arrow ~loc a b)
                                (Option.bind_pair (param.ascr,ret_ty))
        in (expr, ty_opt)
      | params ->
        let input_tuple_ty =
          (* TODOpoly: polymorphism should give some leeway (using Option.all feels wrong) *)
          let in_tys_opt = Option.all @@ List.map ~f:(fun b -> b.ascr) params in
          Option.map ~f:t_tuple in_tys_opt
        in
        let binder = Value_var.fresh ~loc ~name:"parameter" () in
        let expr =
          let body = e_matching_tuple ~loc:loc_par (e_variable binder) params body in
          e_lambda_ez ~loc binder ?ascr:input_tuple_ty ret_ty body
        in
        let ty_opt = Option.map ~f:(fun (a,b) -> t_arrow a b) (Option.bind_pair (input_tuple_ty,ret_ty)) in
        (expr, ty_opt)
    in
    Option.value_map ~default:lambda ~f:(e_annotation ~loc lambda) fun_type
  )
  | E_Ctor constr -> (
    let (v,loc) = w_split constr in
    match v with
    | "Unit" -> e_unit ~loc ()
    | _ -> e_constructor ~loc v (e_unit ())
  )
  | E_App x -> (
    let ((expr, args_opt), loc) = r_split x in
    match expr , args_opt with
    | CST.E_Ctor x , None when String.equal (fst (w_split x)) "Unit" ->
      e_unit ~loc ()
    | CST.E_Ctor x , _ ->
      let (ctor_name,_loc) = w_split x in
      let args_o = Option.map ~f:compile_tuple_expression args_opt in
      let args = Option.value ~default:(e_unit ()) args_o in
      e_constructor ~loc ctor_name args
    | _ -> failwith "impossible"
  )
  | E_Case case -> (
    let (CST.{cases ; expr ; _}, loc) = r_split case in
    let matchee = self expr in
    let cases = compile_matching_expr ~raise self @@ npseq_to_ne_list cases in
    e_matching ~loc matchee cases
  )
  | E_Typed annot -> (
    let (annot, loc) = r_split annot in
    let (expr, (_,ty)) = annot.inside in
    let expr = self expr in
    let ty   = compile_type_expression ~raise ty  in
    e_annotation ~loc expr ty
  )
  | E_Cond cond -> (
    let (cond, loc) = r_split cond in
    let test        = self cond.test in
    let then_clause = self cond.if_so in
    let else_clause =
      match cond.if_not with
      | Some (_else, ifnot) -> self ifnot
      | None -> e_unit ~loc ()
    in
    e_cond ~loc test then_clause else_clause
  )
  | E_List lc -> (
    let (lc,loc) = r_split lc in
    let lst =
      Option.value ~default:[] @@
      Option.map ~f:npseq_to_list lc.elements
    in
    let lst = List.map ~f:self lst in
    e_list ~loc lst
  )
  | E_Cons cons -> (
    let (cons, loc) = r_split cons in
    let a  = self cons.arg1 in
    let b  = self cons.arg2 in
    e_constant ~loc (Const C_CONS) [a; b]
  )
  | E_Set set -> (
    let (si, loc) = r_split set in
    let set =
      Option.value ~default:[] @@
      Option.map ~f:npseq_to_list si.elements
    in
    let set = List.map ~f:self set in
    e_set ~loc set
  )
  | E_SetMem sm -> (
    let (sm, loc) = r_split sm in
    let set  = self sm.set in
    let elem = self sm.element in
    e_constant ~loc (Const C_SET_MEM) [elem;set]
  )
  | E_MapLookup mlu -> (
    let (mlu, loc) = r_split mlu in
    let expr  = self mlu.map in
    let keys = List.map ~f:(fun x -> Access_path.Access_map (self x.value.inside)) (nseq_to_list mlu.keys) in
    e_accessor ~loc expr keys
  )
  | E_Map map -> (
      let (mij, loc) = r_split map in
      let lst = Option.value ~default:[] @@
        Option.map ~f:npseq_to_list mij.elements in
      let aux (binding : CST.binding CST.reg) =
        let (binding, _) = r_split binding in
        let key   = self binding.key in
        let value = self binding.value in
        (key,value)
      in
      let map = List.map ~f:aux lst in
      e_map ~loc map
  )
  | E_BigMap mij -> (
    let (mij, loc) = r_split mij in
    let lst = Option.value ~default:[] @@
      Option.map ~f:npseq_to_list mij.elements in
    let aux (binding : CST.binding CST.reg) =
      let (binding, _) = r_split binding in
      let key   = self binding.key in
      let value = self binding.value in
      (key,value)
    in
    let map = List.map ~f:aux lst in
    e_big_map ~loc map
  )
  | E_CodeInj ci ->
    let (ci, loc) = r_split ci in
    let (language, _) = r_split ci.language in
    let (language, _) = r_split language in
    let code = self ci.code in
    e_raw_code ~loc language code
  | E_Block be ->
    let be, _ = r_split be in
    let next = self be.expr in
    compile_block ~raise ~next be.block
  | E_Nil nil -> (
    let (_,loc) = w_split nil in
    e_list ~loc []
  )
  | E_Attr (a,x) -> compile_expression ~raise ~attr:(a::attr) x

and conv ~raise : ?const:bool -> CST.pattern -> AST.ty_expr option Pattern.t =
  fun ?(const = false) p ->
    let open Pattern in
    let self = conv ~raise ~const in
    match p with
    | P_Verbatim _ | P_Attr _ -> raise.error (unsupported_pattern_type p)
    | P_Var var -> (
      let var = compile_variable var in
      let attributes = if const then Binder.const_attribute else Binder.var_attribute in
      let b : _ Binder.t = { var ; ascr = None ; attributes } in
      Location.wrap ~loc:(Value_var.get_location var) (Pattern.P_var b)
    )
    | P_Tuple tuple -> (
      let (tuple, loc) = r_split tuple in
      let lst = npseq_to_ne_list tuple.inside in
      let patterns = List.Ne.to_list lst in
      let nested = List.map ~f:self patterns in
      match nested with (* (x) == x *)
      | [x] -> x
      | _ -> Location.wrap ~loc @@ Pattern.P_tuple nested
    )
    | P_App constr_pattern -> (
      let ((constr,p_opt), loc) = r_split constr_pattern in
      let rec get_ctor : CST.pattern -> string option = function
        | P_Par x -> get_ctor x.value.inside
        | P_Ctor x -> Some x#payload
        | _ -> None
      in
      match get_ctor constr with
      | Some "Unit" -> Location.wrap ~loc @@ P_unit
      | Some label ->
        let carg = match p_opt with
          | Some p -> self (CST.P_Tuple p)
          | None -> Location.wrap ~loc P_unit
        in
        Location.wrap ~loc @@ P_variant (Label label, carg)
      | None -> raise.error (unsupported_pattern_type p)
    )
    | P_List { region ; value = { elements ; _ } } -> (
      (* let () = check_no_attributes attr in *)
      let loc = Location.lift region in
      let elements = Utils.sepseq_to_list elements in
      let f : CST.pattern -> AST.type_expression option Pattern.t -> AST.type_expression option Pattern.t =
        fun x prev ->
          let p = self x in
          Location.wrap (P_list (Cons (p, prev)))
      in
      List.fold_right ~f ~init:(Location.wrap ~loc (P_list (List []))) elements
    )
    | P_Cons { region ; value = (hd,_,tl) } -> (
      let loc = Location.lift region in
      let hd = self hd in
      let tl = self tl in
      Location.wrap ~loc (P_list (Cons (hd,tl)))
    )
    | P_Nil (x: _ CST.wrap) -> (
      let loc = Location.lift x#region in
      Location.wrap ~loc (P_list (List []))
    )
    | P_Par { region = _ ; value } -> (
      self value.inside
    )
    | P_Record { region ; value = { elements ; _ }} -> (
      let loc = Location.lift region in
      let lst = Utils.sepseq_to_list elements in
      let aux : CST.field_pattern CST.reg -> Label.t * AST.ty_expr option Pattern.t =
        fun x ->
          let (field, field_loc) = r_split x in
          match field with
          | Punned { pun ; attributes } ->
            check_no_attributes ~raise field_loc attributes ;
            let (label,loc) = match pun with
              | P_Var pun -> w_split pun
              | x -> raise.error (expected_field_name @@ CST.pattern_to_region x)
            in
            if String.equal label "_" then raise.error (unexpected_wildcard @@ CST.pattern_to_region pun) ;
            let attributes = if const then Binder.const_attribute else Binder.var_attribute in
            let binder : _ Binder.t = { var = Value_var.of_input_var ~loc label ; ascr = None ; attributes } in
            (Label label , Location.wrap ~loc (P_var binder))
          | Complete { field_lhs; field_rhs ; attributes ; _} ->
            check_no_attributes ~raise field_loc attributes ;
            let (lhs,_loc) = match field_lhs with
              | P_Var x -> w_split x
              | x -> raise.error (expected_field_or_access @@ CST.pattern_to_region x)
            in
            (Label lhs, self field_rhs)
      in
      let lst' = List.map ~f:aux lst in
      let (labels,patterns) = List.unzip lst' in
      Location.wrap ~loc (P_record (labels,patterns))
    )
    | P_Typed {region ; value = { pattern ; type_annot = (_,ty_expr) }} -> (
      let loc = Location.lift region in
      let p = self pattern in
      let ty_expr = compile_type_expression ~raise ty_expr in
      match p.wrap_content with
      | P_var x -> Location.wrap ~loc (P_var {x with ascr = Some ty_expr})
      | _ -> raise.error (unsupported_type_ann_on_patterns @@ CST.pattern_to_region pattern)
    )
    | P_Ctor x -> (
      let (c,loc) = w_split x in
      match x#payload with
      | "Unit" -> Location.wrap ~loc P_unit
      | _ -> Location.wrap ~loc (P_variant (Label c, Location.wrap P_unit))
    )
    | P_ModPath _ | P_Mutez _ | P_Bytes _ | P_Int _ | P_Nat _ | P_String _ -> raise.error @@ unsupported_pattern_type p

and compile_matching_expr : type a . raise:('b,'w) raise -> (a-> AST.expression) -> a CST.case_clause CST.reg List.Ne.t -> (AST.expression, AST.ty_expr option) Match_expr.match_case list =
  fun ~raise compiler cases ->
    let aux (case : a CST.case_clause CST.reg) =
      let (case, _loc) = r_split case in
      let expr    = compiler case.rhs in
      (case.pattern, expr)
    in
    let cases = List.Ne.map aux cases in
    let cases : (CST.pattern * AST.expression) list = List.Ne.to_list cases in
    let aux : (CST.pattern * AST.expression) -> (AST.expression , AST.ty_expr option) Match_expr.match_case =
      fun (raw_pattern, body) ->
        let pattern = conv ~raise ~const:true raw_pattern in
        { pattern ; body }
    in
    List.map ~f:aux cases

and compile_parameters ~raise : CST.parameters -> (AST.type_expression option Binder.t) list = fun params ->
  let aux : CST.param_decl CST.reg -> AST.type_expression option Binder.t = fun param ->
    let (param, _loc) = r_split param in
    let var = match param.pattern with
      | P_Var param -> compile_variable param
      | x -> raise.error (unsuported_pattern_in_function @@ CST.pattern_to_region x)
    in
    match param.param_kind with
    | `Var _ ->
      let ascr = Option.map ~f:(compile_type_expression ~raise <@ snd) param.param_type in
      { var ; ascr ; attributes = Binder.var_attribute }
    | `Const _ ->
      let ascr = Option.map ~f:(compile_type_expression ~raise  <@ snd) param.param_type in
      { var ; ascr ; attributes = Binder.const_attribute }
  in
  let (params, _loc) = r_split params in
  let params = pseq_to_list params.inside in
  List.map ~f:aux params

and compile_path : (CST.selection, CST.dot) Utils.nsepseq -> AST.expression Access_path.t =
  fun x ->
    let f : CST.selection -> AST.expression Access_path.access = function
      | FieldName name -> Access_record name#payload
      | Component v -> Access_tuple (snd v#payload)
    in
    List.map (Utils.nsepseq_to_list x) ~f

(*
  `path_of_lvalue [lvalue]` extracts the path and the left-end-side out of an expression to be used in an assignment.
  The path is extracted as an access list, and can be used to construct the right side of the assigment
    - ((r.x).y).z         |-> (r, [x;y;z])
    - (m.["foo"]).["bar"] |-> (m, ["foo";"bar"])

  Restrictions on [lvalue]:
    - the left-most accessed element must be a variable (e.g. `{ x = 1 ; y = 2}.x = 2` is rejected)
    - module access are forbidden (we do not support effects on module declarations)
    - any expression that is not a record/map/variable access is rejected
*)
and path_of_lvalue ~raise : CST.expr -> Value_var.t * AST.expression Access_path.t =
  fun expr ->
  let rec aux = fun (lhs:CST.expr) (cpath:AST.expression Access_path.t) ->
    match lhs with
    | E_Par x -> aux x.value.inside cpath
    | E_Var v -> (
      let v = compile_variable v in
      (v, cpath)
    )
    | E_Proj { value = { record_or_tuple ; field_path ; _ } ; _} -> (
      let path = compile_path field_path @ cpath in
      match compile_var_opt record_or_tuple with
      | Some v -> (v, path)
      | None -> aux record_or_tuple path
    )
    | E_MapLookup { value = { map ; keys } ; _ } -> (
      let keys = List.map ~f:(fun x -> Access_path.Access_map (compile_expression ~raise x.value.inside)) (nseq_to_list keys) in
      let path = keys @ cpath in
      match compile_var_opt map with
      | Some v -> (v,path)
      | None -> aux map path
    )
    | E_ModPath _ -> raise.error (expected_field_or_access @@ CST.expr_to_region lhs)
    | _ -> raise.error (expected_field_or_access @@ CST.expr_to_region lhs)
  in
  aux expr []

(*
  `compile_assignment [~loc] [~last_proj_update] [~lhs] [~path] [~default_rhs]` build the assignment of [lhs] accessed by [path].

  This function is used in case of patches (`patch <X> with <Y>`) ; assignments (`<X> := <Y>`) or removals (`remove <X> from <Y>`).

  The return assignment will only update [lhs] if all the accessed map element in [path] are already present, i.e. we produce
  matching expression of the form  `match Map.find_opt .. with | None -> [lhs] -> Some -> ...` if not it will fail (`failwith ..`)
  with a default message.

  [default_rhs] is used as a default assigned value when path is empty, if the path isn't empty [last_proj_update] will be used as follow:
    - Internally, this function produces a "context" (expression -> expression) building the whole acess expression on the right-end side
    of the assigment and the last accessed element, e.g:
    ```
      v := match MAP_FIND_OPT ("x",v) with | Some <last_accessed_element> -> <last_proj_update <last_accessed_element>> | None -> failwith "DEFAULT"`
    ```
*)
and compile_assignment : loc:Location.t -> last_proj_update:(expression -> expression) ->
                  lhs:Value_var.t -> path:AST.expression Access_path.t -> default_rhs:AST.expression -> AST.expression =
  fun ~loc ~last_proj_update ~lhs ~path ~default_rhs ->
    let rec aux : AST.expr * (AST.expr -> AST.expr) -> AST.expression Access_path.t -> AST.expression =
      fun (last_proj, updator) lst ->
        (* [last_proj] is an accessor to the projection in [path] (i.e. [lhs].path(0).path(1)...path(n) *)
        match lst with
        | [] -> updator (last_proj_update last_proj)
        | access::tl -> (
          match access with
          | Access_tuple _ | Access_record _ -> (
            let updator = fun hole -> updator (e_update ~loc last_proj [access] hole) in
            let prev_access = e_accessor ~loc last_proj [access] in
            aux (prev_access,updator) tl
          )
          | Access_map k -> (
            let matchee = e_map_find_opt ~loc k last_proj in
            let none_body = e_variable ~loc lhs in
            let some_proj = Value_var.fresh () in
            let some_body =
              let updator = fun hole -> updator (e_map_add ~loc k hole last_proj) in
              let last_proj' = e_variable ~loc some_proj in
              aux (last_proj', updator) tl
            in
            e_unopt ~loc matchee none_body (some_proj,some_body)
          )
        )
    in
    match path with
    | [] -> e_assign ~loc {var=lhs;ascr=None;attributes={const_or_var=Some `Var}} default_rhs
    | _ ->
      let init = e_variable ~loc lhs in
      e_assign ~loc {var=lhs;ascr=None;attributes={const_or_var=Some `Var}} (aux (init,Fun.id) path)

and compile_instruction ~raise : ?next: AST.expression -> CST.instruction -> AST.expression  = fun ?next instruction ->
  let return expr = Option.value_map next ~default:expr ~f:(e_sequence expr) in
  let compile_if_clause : ?next:AST.expression -> CST.test_clause -> AST.expression =
    fun ?next if_clause ->
      match if_clause with
      | ClauseInstr i -> compile_instruction ~raise ?next i
      | ClauseBlock block -> compile_block ~raise ?next block
  in
  match instruction with
  | I_Cond { region ; value = { test ; if_so ; if_not ; _ } } -> (
    let loc = Location.lift region in
    let test = compile_expression ~raise test in
    let ifso = compile_if_clause if_so in
    let ifnot = Option.value_map if_not ~default:(e_skip ()) ~f:(fun x -> compile_if_clause (snd x)) in
    return @@ e_cond ~loc test ifso ifnot
  )
  | I_Case { region ; value = { expr ; cases ; _ } } -> (
    let loc = Location.lift region in
    let matchee = compile_expression ~raise expr in
    let cases = compile_matching_expr ~raise compile_if_clause (npseq_to_ne_list cases) in
    return @@ e_matching ~loc matchee cases
  )
  | I_Assign {region ; value = { lhs ; rhs ; _ }} -> (
    let loc = Location.lift region in
    let (var,path) = path_of_lvalue ~raise lhs in
    match List.rev path with
    | [] ->
      let rhs = compile_expression ~raise rhs in
      return @@ e_assign ~loc {var;ascr=None;attributes={const_or_var=Some `Var}} rhs
    | last_access::path -> (
      let path = List.rev path in
      match last_access with
      | Access_map k ->
        let default_rhs = e_map_add ~loc k (compile_expression ~raise rhs) (e_variable var) in
        let last_proj_update = fun last_proj -> e_map_add ~loc k (compile_expression ~raise rhs) last_proj in
        return @@ compile_assignment ~loc ~last_proj_update ~lhs:var ~path ~default_rhs
      | Access_record _ | Access_tuple _ ->
        let rhs = compile_expression ~raise rhs in
        let default_rhs = e_update ~loc (e_variable var) [last_access] rhs in
        let last_proj_update = fun last_proj -> e_update ~loc last_proj [last_access] rhs in
        return @@ compile_assignment ~loc ~last_proj_update ~lhs:var ~path ~default_rhs
    )
  )
  | I_While { region ; value = { cond ; block ; _ } } -> (
    let loc = Location.lift region in
    let cond = compile_expression ~raise cond in
    let body = compile_block ~raise block in
    return @@ e_while ~loc cond body
  )
  | I_For { value = { index ; init ; bound ; step ; block ; _ } ; region } -> (
    let loc = Location.lift region in
    let index = compile_variable index in
    let start = compile_expression ~raise init in
    let bound = compile_expression ~raise bound in
    let increment = Option.value_map step ~default:(e_int_z Z.one) ~f:(compile_expression ~raise <@ snd) in
    let body  = compile_block ~raise block in
    return @@ e_for ~loc index start bound increment body
  )
  | I_ForIn (ForMap { region ; value = { binding = (key,_,value) ; collection ; block ; _ } }) -> (
    let loc = Location.lift region in
    let binder =
      let key' = compile_variable key in
      let value' = compile_variable value in
      (key', Some value')
    in
    let collection = compile_expression ~raise collection in
    let body = compile_block ~raise block in
    return @@ e_for_each ~loc binder collection Map body
  )
  | I_ForIn (ForSetOrList { region ; value = { var ; for_kind ; collection ; block ; _ } }) -> (
    let loc = Location.lift region in
    let binder = (compile_variable var, None) in
    let collection = compile_expression ~raise collection in
    let body = compile_block ~raise block in
    return @@ e_for_each ~loc binder collection (match for_kind with `Set _ -> Set | `List _ -> List) body
  )
  | I_Skip s -> (
    let loc = Location.lift s#region in
    return @@ e_skip ~loc ()
  )
  | I_Patch { region ; value = { collection ; patch ; patch_kind ; _ } } -> (
    let loc = Location.lift region in
    let (v,path) = path_of_lvalue ~raise collection in
    let patch = compile_expression ~raise patch in
    let last_proj_update, default_rhs =
      match patch.expression_content, patch_kind with
      | E_map kvl , `Map _ ->
        let f acc (k,v) = e_map_add ~loc k v acc in
        (fun last_proj -> List.fold kvl ~f ~init:last_proj), List.fold kvl ~f ~init:(e_variable v)
      | E_record kl , `Record _ ->
        let f acc (label,expr) = e_update ~loc acc [Access_record (Label.to_string label)] expr in
        (fun last_proj -> List.fold kl ~f ~init:last_proj), List.fold kl ~f ~init:(e_variable v)
      | E_set lst , `Set _ ->
        let f acc v = e_set_add ~loc v acc in
        (fun last_proj -> List.fold lst ~f ~init:last_proj), List.fold lst ~f ~init:(e_variable v)
      | _ -> failwith "impossible patch rhs"
    in
    return @@ compile_assignment ~loc ~last_proj_update ~lhs:v ~path ~default_rhs
  )
  | I_Remove { region ; value = { item; collection ; remove_kind ; _ }} -> (
    let loc = Location.lift region in
    let (v,path) = path_of_lvalue ~raise collection in
    let item = compile_expression ~raise item in
    let remove_func = match remove_kind with
      | `Set _ -> e_set_remove ~loc
      | `Map _ -> e_map_remove ~loc
    in
    let default_rhs = remove_func item (e_variable v) in
    let last_proj_update = fun prev_proj -> remove_func item prev_proj in
    return @@ compile_assignment ~loc ~last_proj_update ~lhs:v ~path ~default_rhs
  )
  | I_Call { region ; value = (f,args) } -> (
    return @@ compile_expression ~raise (E_Call { region ; value = (f,args)})
  )

and compile_let_destructuring ~raise :
  ?const:bool -> Location.t -> CST.expr -> CST.pattern -> AST.expression -> AST.type_expression option -> AST.expression =
    fun ?(const = false) loc value pattern body ty_opt ->
      let init = compile_expression ~raise value in
      let pattern = conv ~raise ~const pattern in
      let match_case = Match_expr.{ pattern ; body } in
      let match_ = e_matching ~loc init [match_case] in
      Option.value_map ty_opt ~default:match_ ~f:(e_annotation ~loc match_)

and compile_data_declaration ~raise : ?attr:CST.attribute list -> next:AST.expression -> CST.declaration -> AST.expression =
  fun ?(attr = []) ~next data_decl ->
  let return loc var ascr var_attr attr init =
    e_let_in ~loc {var;ascr;attributes=var_attr} attr init next
  in
  match data_decl with
  | D_Attr (a,x) -> compile_data_declaration ~raise ~attr:(a::attr) ~next x
  | D_Const const_decl -> (
    let cd, loc = r_split const_decl in
    let type_ = Option.map ~f:(compile_type_expression ~raise <@ snd) cd.const_type in
    match cd.pattern with
    | P_Var name -> (
      let init = compile_expression ~raise cd.init in
      let p = compile_variable name in
      let attr = compile_attributes attr in
      return loc p type_ Binder.const_attribute attr init
    )
    | pattern ->
      (* not sure what to do with  attributes in that case *)
      compile_let_destructuring ~raise ~const:true loc cd.init pattern next type_
  )
  | D_Directive _ -> next
  | D_Fun fun_decl -> (
    let fun_decl, loc = r_split fun_decl in
    let attr = compile_attributes [] in
    let fun_var, fun_type, lambda = compile_fun_decl loc ~raise fun_decl in
    return loc fun_var fun_type Binder.empty_attribute attr lambda
  )
  | D_Type type_decl -> (
    let td,loc = r_split type_decl in
    let rhs = compile_type_expression ~raise td.type_expr in
    let name = compile_type_var td.name in
    e_type_in ~loc name rhs next
  )
  | D_Module {value; region} -> (
    let loc = Location.lift region in
    let CST.{name; module_expr; _} = value in
    let module_ = compile_module_expression ~raise module_expr in
    let module_binder = compile_mod_var name in
    e_mod_in ~loc module_binder module_ next
  )

and compile_statement ~raise : ?next:AST.expression -> CST.statement -> AST.expression option =
  fun ?next statement ->
  match statement with
  | S_Attr (_,statement) -> compile_statement ~raise ?next statement
  | S_Instr i ->
    let i = compile_instruction ~raise ?next i in
    Some i
  | S_Decl dd ->
    let next = Option.value ~default:(e_skip ()) next in
    let dd = compile_data_declaration ~raise ~next dd in
    (Some dd)
  | S_VarDecl var_decl -> (
    let vd, loc = r_split var_decl in
    let type_ = Option.map ~f:(compile_type_expression ~raise <@ snd) vd.var_type in
    match vd.pattern with
    | P_Var name -> (
      let init = compile_expression ~raise vd.init in
      let var = compile_variable name in
      match next with
      | Some next ->
        Some (e_let_in ~loc {var;ascr=type_;attributes=Binder.var_attribute} [] init next)
      | None -> None
    )
    | pattern ->
      (* not sure what to do with  attributes in that case *)
      match next with
      | Some next ->
        let x = compile_let_destructuring ~raise ~const:false loc vd.init pattern next type_ in
        Some x
      | None -> None
  )

and compile_block ~raise : ?next:AST.expression -> CST.block CST.reg -> AST.expression =
  fun ?next block ->
    let (block', _loc) = r_split block in
    let statements = npseq_to_list block'.statements in
    let aux statement next = compile_statement ~raise ?next statement in
    let block' = List.fold_right ~f:aux ~init:next statements in
    match block' with
    | Some block -> block
    | None -> raise.error @@ block_start_with_attribute block

and compile_fun_decl loc ~raise : CST.fun_decl -> Value_var.t * type_expression option * expression =
  fun ({kwd_recursive; kwd_function=_; fun_name; type_params; parameters; ret_type; kwd_is=_; return=r; terminator=_ }: CST.fun_decl) ->
  let fun_binder = compile_variable fun_name in
  let ret_type = Option.map ~f:(compile_type_expression ~raise <@ snd) ret_type in
  let param = compile_parameters ~raise parameters in
  let result = compile_expression ~raise r in
  let (lambda, fun_type) =
    match param with
    | binder::[] ->
      let lambda : (_,_ option) Lambda.t = { binder; output_type = ret_type; result } in
      (lambda , Option.map ~f:(fun (a,b) -> t_arrow a b) @@ Option.bind_pair (binder.ascr,ret_type))
    | lst ->
      let lst = Option.all @@ List.map ~f:(fun e -> e.ascr) lst in
      let input_type = Option.map ~f:t_tuple lst in
      let var = Value_var.fresh ~name:"parameters" () in
      let binder : _ Binder.t = { var;ascr=input_type;attributes=Binder.const_attribute} in
      let result = e_matching_tuple (e_variable var) param result in
      let lambda : _ Lambda.t = { binder ; output_type = ret_type ; result } in
      (lambda, Option.map ~f:(fun (a,b) -> t_arrow a b) @@ Option.bind_pair (input_type,ret_type))
  in
  (* This handle polymorphic annotation *)
  let fun_type = 
    Option.map fun_type ~f:(fun rhs_type -> 
      Option.value_map type_params ~default:rhs_type ~f:(fun tp ->
      let (tp, loc) = r_split tp in
      let type_vars = List.Ne.map compile_type_var @@ npseq_to_ne_list tp.inside in
      List.Ne.fold_right ~f:(fun tvar t -> t_for_all ~loc tvar Type t) ~init:rhs_type type_vars
    ))
  in
  let func =
    match kwd_recursive with
    | Some reg ->
      let fun_type = trace_option ~raise (untyped_recursive_fun loc) @@ fun_type in
      let _, fun_type = destruct_for_alls fun_type in 
      let Arrow.{type1; type2} = get_t_arrow_exn fun_type in 
      let lambda = Lambda.{lambda with binder = {lambda.binder with ascr = type1}; output_type = type2} in
      e_recursive ~loc:(Location.lift reg#region) fun_binder fun_type lambda
    | None -> make_e ~loc @@ E_lambda lambda
  in
  let func = Option.value_map ~default:func ~f:(fun tp ->
    let (tp,loc) = r_split tp in
    let tp : CST.type_params = tp.inside in
    let type_vars = List.Ne.map compile_type_var @@ npseq_to_ne_list tp in
    List.Ne.fold_right ~f:(fun t e -> e_type_abs ~loc t e) ~init:func type_vars
  ) type_params in
  (fun_binder, fun_type, func)

and compile_declaration ~raise : ?attr:CST.attribute list -> CST.declaration -> AST.declaration list =
  fun ?(attr=[]) decl ->
  let return reg decl = [Location.wrap ~loc:(Location.lift reg) decl] in
  match decl with
  | D_Attr (a,x) -> compile_declaration ~attr:(a::attr) ~raise x
  | D_Type { value = { name ; params ; type_expr; _ } ; region } ->
      let type_expr =
        let rhs = compile_type_expression ~raise type_expr in
        match params with
        | None -> rhs
        | Some x ->
          let lst = Utils.nsepseq_to_list x.value.inside in
          let aux : CST.variable -> AST.type_expression -> AST.type_expression =
            fun param type_ ->
              let ty_binder = compile_type_var param in
              t_abstraction ~loc:(Location.lift region) ty_binder Type type_
          in
          List.fold_right ~f:aux ~init:rhs lst in
      let ast =
        D_type {type_binder= compile_type_var name;
                              type_expr; type_attr=[]}
      in return region ast

  | D_Const {value={pattern; const_type; init; _}; region} -> (
    let attr = compile_attributes attr in
    match pattern with
    | P_Var name ->
      let var = compile_variable name in
      let ascr = Option.map ~f:(compile_type_expression ~raise <@ snd) const_type in
      let expr = compile_expression ~raise init in
      let attributes = Binder.const_attribute in
      let binder : _ Binder.t = {var; ascr; attributes} in
      let ast = D_value {binder; attr; expr} in
      return region ast
    | _ ->
        raise.error (unsupported_top_level_destructuring region)
  )

  | D_Fun {value; region} ->
    let var, ascr, expr = compile_fun_decl (Location.lift region) ~raise value in
    let attributes = Binder.empty_attribute in
    let binder : _ Binder.t = {var; ascr; attributes} in
    let ast = D_value {binder; attr = compile_attributes attr; expr} in
    return region ast

  | D_Module {value; region} ->
    let CST.{name; module_expr; _} = value in
    let module_ = compile_module_expression ~raise module_expr in
    let module_binder = compile_mod_var name in
    let ast = D_module {module_binder; module_; module_attr=[]}
    in return region ast

  | D_Directive _ -> []

and compile_module_expression ~raise : CST.module_expr ->  AST.module_expr = function
  | CST.M_Body { region ; value = { enclosing = _ ; declarations } } -> (
    let decls = compile_declarations ~raise declarations in
    Location.wrap ~loc:(Location.lift region) (Module_expr.M_struct decls)
  )
  | CST.M_Path { region ; value = { module_path ; selector = _ ; field } } -> (
    let path =
      let module_path = List.Ne.map compile_mod_var (npseq_to_ne_list module_path) in
      let field = compile_mod_var field in
      List.Ne.append module_path (field,[])
    in
    Location.wrap ~loc:(Location.lift region) (Module_expr.M_module_path path)
  )
  | CST.M_Var mod_name -> (
    let v : Module_var.t = compile_mod_var mod_name in
    Location.wrap ~loc:(Module_var.get_location v) (Module_expr.M_variable v)
  )

and compile_declarations ~raise : CST.declaration Utils.nseq -> AST.module_ =
  fun decl ->
    let lst = List.map ~f:(compile_declaration ~raise) @@ nseq_to_list decl
    in List.concat lst

let compile_program ~raise : CST.declaration Utils.nseq -> AST.program = fun t ->
  nseq_to_list t
  |> List.map ~f:(fun a ~raise -> compile_declaration ~raise a)
  |> Simple_utils.Trace.collect ~raise
  |> List.concat
