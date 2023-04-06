open Simple_utils.Utils
open Unification_shared.Helpers
module CST = Cst.Pascaligo
module AST = Ast_unified
module Option = Simple_utils.Option
module Region = Simple_utils.Region
open AST (* Brings types and combinators functions *)

let translate_attr_pascaligo : CST.Attr.t -> AST.Attribute.t =
 fun attr ->
  let key, value = attr in
  let value : string option =
    Option.map
      ~f:(function
        | String s -> s
        | Ident s -> s (* not sure ?*))
      value
  in
  { key; value }


module TODO_do_in_parsing = struct
  let r_split = r_split (* could compute Location directly in Parser *)
  let _lift = Location.lift
  let var ~loc var = Ligo_prim.Value_var.of_input_var ~loc var
  let tvar ~loc var = Ligo_prim.Type_var.of_input_var ~loc var
  let mvar ~loc var = Ligo_prim.Module_var.of_input_var ~loc var
  let need_rework _ y = (*most probably a node that we should avoid, maybe ?*) failwith y
  let six_to_z x = Z.of_int64 x (* not sure who's right ? *)

  let rec compile_pattern_record_lhs (p : CST.pattern) : CST.variable =
    match p with
    | CST.P_Par x -> compile_pattern_record_lhs x.value.inside
    | CST.P_Var x -> x
    | _ -> failwith "CST.field_pattern is wrong ?  should be '(string, pattern) field' ?"


  let weird_attributes _ =
    (* I don't know what to do with those attributes *)
    ()


  let extract_arg_from_app self p =
    (* other syntax do not have the App thing ?
       also have an optional 'carg' ?
    *)
    let (constr, p_opt), loc = r_split p in
    let rec get_ctor : CST.pattern -> string option = function
      | P_Par x -> get_ctor x.value.inside
      | P_Ctor x -> Some x#payload
      | _ -> None
    in
    match get_ctor constr with
    | Some "Unit" -> p_unit ~loc
    | Some label ->
      let carg =
        match p_opt with
        | Some (parg : CST.pattern CST.tuple) ->
          let p, _ = r_split parg in
          let p = List.map ~f:self (nsepseq_to_list p.inside) in
          (match p with
          | [ x ] -> x
          | _ -> self (CST.P_Tuple parg))
        | None -> p_unit ~loc
      in
      p_variant ~loc (Label label) (Some carg)
    | None -> failwith "impossible ?"


  let label_as_var (e : CST.expr) : Label.t Location.wrap =
    match e with
    | E_Var x -> Location.wrap ~loc:(w_snd x) @@ Label.of_string (w_fst x)
    | _ -> failwith "would not make sense ? tofix"


  (* here, param_type could be parsed within the pattern ?*)
  let compile_param_decl compile_pattern compile_type_expression
      : CST.param_decl -> pattern AST.Param.t
    =
   fun p ->
    let param_kind =
      match p.param_kind with
      | `Var _ -> `Var
      | `Const _ -> `Const
    in
    let pattern = compile_pattern p.pattern in
    let pattern =
      Option.value_map
        ~default:pattern
        ~f:(fun (_, ty) ->
          let ty = compile_type_expression ty in
          p_typed ~loc:(get_p_loc pattern) ty pattern)
        p.param_type
    in
    { param_kind; pattern }


  let field_as_open (ma : CST.expr CST.module_path) =
    (* here, we should use module expressions, maybe ? *)
    match ma.field with
    | CST.E_Par _ -> true
    | _ -> false


  let field_as_open_t (ma : CST.type_expr CST.module_path) =
    (* here, we should use module expressions, maybe ? *)
    match ma.field with
    | CST.T_Par _ -> true
    | _ -> false
end

module TODO_unify_in_cst = struct
  let compile_rows = Non_linear_rows.make

  let vardecl_as_decl ~loc x =
    (* https://tezos-dev.slack.com/archives/GMHV0U3Q9/p1669146559008189 *)
    s_decl ~loc (d_var ~loc x)


  let for_in compile_for_map compile_for_set_or_list i =
    (* could be done directyu in Parser.mly *)
    let open AST.For_collection in
    match i with
    | CST.ForMap m ->
      let m, loc = r_split m in
      ForMap (compile_for_map m), loc
    | CST.ForSetOrList s ->
      let s, loc = r_split s in
      ForSetOrList (compile_for_set_or_list s), loc


  (* let e_cat ~loc a b =
    e_binary_op ~loc AST.{ operator = Location.wrap ~loc "^"; left = a; right = b } *)

  let e_string ~loc s =
    e_literal ~loc (Literal_string (Simple_utils.Ligo_string.Standard s))


  let e_verbatim ~loc s =
    e_literal ~loc (Literal_string (Simple_utils.Ligo_string.Verbatim s))


  let better_as_binop_sharp ~loc kwd (left, right) =
    e_binary_op
      ~loc
      { operator = Location.wrap ~loc:(w_snd kwd) Operators.SHARP; left; right }


  let better_as_binop_contains ~loc kwd (left, right) =
    e_binary_op
      ~loc
      { operator = Location.wrap ~loc:(w_snd kwd) Operators.CONTAINS; left; right }


  let nested_proj : AST.expr -> (CST.selection, CST.dot) nsepseq -> AST.expr =
   fun init field_path ->
    (* projections could be nested ? *)
    List.fold (nsepseq_to_list field_path) ~init ~f:(fun acc -> function
      | FieldName name ->
        let name, loc = w_split name in
        e_proj ~loc { struct_ = acc; path = FieldName (Label.of_string name) }
      | Component comp ->
        let index, loc = w_split comp in
        e_proj ~loc { struct_ = acc; path = Component_num index })


  let update_rhs : (CST.expr -> AST.expr) -> CST.expr -> AST.expr AST.Update.field list =
   fun self rhs ->
    (* here, having expressions as update rhs seems a little bit too much *)
    match rhs with
    | E_Record record_lhs ->
      let f : (CST.expr, CST.expr) CST.field CST.reg -> AST.expr AST.Update.field =
       fun x ->
        match x.value with
        | CST.Complete { field_lhs; field_lens; field_rhs; attributes } ->
          let attributes =
            TODO_do_in_parsing.weird_attributes attributes;
            List.map attributes ~f:(translate_attr_pascaligo <@ w_fst)
          in
          let field_lens =
            let open AST.Update in
            match field_lens with
            | Lens_Id _ -> Lens_Id
            | Lens_Add _ -> Lens_Add
            | Lens_Sub _ -> Lens_Sub
            | Lens_Mult _ -> Lens_Mult
            | Lens_Div _ -> Lens_Div
            | Lens_Fun _ -> Lens_Fun
          in
          let field_lhs : AST.expr AST.Selection.t list =
            match field_lhs with
            | CST.E_Var x ->
              let label = w_fst x in
              [ FieldName (Label.of_string label) ]
            | CST.E_Proj
                { region = _
                ; value = { record_or_tuple = CST.E_Var label; selector = _; field_path }
                } ->
              let r = Selection.FieldName (Label.of_string @@ w_fst label) in
              r
              :: List.map (nsepseq_to_list field_path) ~f:(function
                     | FieldName name ->
                       Selection.FieldName (Label.of_string (w_fst name))
                     | Component x -> Component_num (w_fst x))
            | _x ->
              failwith "raise.error (expected_field_or_access @@ CST.expr_to_region x)"
          in
          let field_rhs = self field_rhs in
          TODO_do_in_parsing.weird_attributes attributes;
          Full_field { field_lhs; field_lens; field_rhs }
        | CST.Punned { pun; attributes } ->
          TODO_do_in_parsing.weird_attributes attributes;
          (match pun with
          | CST.E_Var v ->
            let label = Label.of_string (w_fst v) in
            Pun (Location.wrap ~loc:(w_snd v) label)
          | _ -> failwith "pun should be a string/label directly ?")
      in
      List.map (Utils.sepseq_to_list record_lhs.value.elements) ~f
    | _ -> failwith "raise.error (wrong_functional_updator @@ CST.expr_to_region x)"


  let tnested_mod_access init lst =
    fst
    @@ List.fold_right
         ~init
         ~f:(fun x (field, field_as_open) ->
           ( t_module_open_in
               ~loc:(get_t_loc field)
               { module_path = x; field; field_as_open }
           , false ))
         (List.Ne.to_list lst)


  let declarations_as_program decls =
    make_prg (List.Ne.to_list @@ nseq_map pe_declaration decls)
end

let extract_type_params
    : CST.type_params CST.chevrons CST.reg -> Ligo_prim.Type_var.t nseq
  =
 fun tp ->
  nseq_map
    (fun x ->
      let x, loc = w_split x in
      TODO_do_in_parsing.tvar ~loc x)
    (nsepseq_to_nseq @@ (r_fst tp).inside)


(* ========================== TYPES ======================================== *)

let rec compile_type_expression : CST.type_expr -> AST.ty_expr =
 fun te ->
  let self = compile_type_expression in
  match te with
  | T_App t ->
    let (te, ttuple), loc = r_split t in
    let constr = self te in
    let type_args : ty_expr nseq =
      List.Ne.map self @@ nsepseq_to_nseq (r_fst ttuple).inside
    in
    t_app { constr; type_args } ~loc
  | T_Attr t ->
    let attr, te = t in
    let attr, loc = w_split attr in
    let attr = translate_attr_pascaligo attr in
    let te = self te in
    t_attr attr te ~loc
  | T_Cart t ->
    let (te, _, tes), loc = r_split t in
    let te = self te in
    let hd, tl = List.Ne.map self @@ nsepseq_to_nseq tes in
    t_prod (te, hd :: tl) ~loc
  | T_Fun t ->
    let (te1, _, te2), loc = r_split t in
    let te1 = self te1 in
    let te2 = self te2 in
    t_fun ~loc (te1, te2)
  | T_Int t ->
    let (s, z), loc = w_split t in
    t_int s z ~loc
  | T_ModPath t ->
    let t, _loc = r_split t in
    let module_path =
      List.Ne.map
        (fun t ->
          let x, loc = w_split t in
          TODO_do_in_parsing.mvar ~loc x)
        (nsepseq_to_nseq t.module_path)
    in
    let field = self t.field in
    let field_as_open = TODO_do_in_parsing.field_as_open_t t in
    TODO_unify_in_cst.tnested_mod_access (field, field_as_open) module_path
  | T_Par t -> self (r_fst t).inside
  | T_Record t ->
    let t, loc = r_split t in
    let fields =
      let destruct CST.{ field_type; field_name; attributes } =
        ( Label.of_string (w_fst field_name)
        , Option.map ~f:(self <@ snd) field_type
        , List.map attributes ~f:(translate_attr_pascaligo <@ w_fst) )
      in
      let lst = List.map ~f:(destruct <@ r_fst) @@ sepseq_to_list t.elements in
      TODO_unify_in_cst.compile_rows lst
    in
    t_record_raw fields ~loc
  | T_String t ->
    let t, loc = w_split t in
    t_string t ~loc
  | T_Sum t ->
    let t, loc = r_split t in
    let variants =
      let destruct CST.{ ctor; ctor_args; attributes } =
        ( Label.of_string (w_fst ctor)
        , Option.map ~f:(self <@ snd) ctor_args
        , List.map attributes ~f:(translate_attr_pascaligo <@ w_fst) )
      in
      let lst = List.map ~f:(destruct <@ r_fst) (nsepseq_to_list t.variants) in
      TODO_unify_in_cst.compile_rows lst
    in
    t_sum_raw variants ~loc
  | T_Var t ->
    let t, loc = w_split t in
    t_var (TODO_do_in_parsing.tvar ~loc t) ~loc


(* ========================== PATTERNS ===================================== *)

and compile_pattern : CST.pattern -> AST.pattern =
 fun p ->
  let self = compile_pattern in
  match p with
  | P_Ctor p -> TODO_do_in_parsing.need_rework p "never emited alone"
  | P_App p -> TODO_do_in_parsing.extract_arg_from_app self p
  | P_Attr p ->
    let attr, ptrn = p in
    let attr, loc = w_split attr in
    let attr = translate_attr_pascaligo attr in
    let ptrn = self ptrn in
    p_attr ~loc attr ptrn
  | P_Bytes p ->
    let (_s, hex), loc = w_split p in
    let b = Hex.to_bytes hex in
    p_literal ~loc (Literal_bytes b)
  | P_Cons p ->
    let (p1, _, p2), loc = r_split p in
    let p1 = self p1 in
    let p2 = self p2 in
    p_list ~loc (Cons (p1, p2))
  | P_Int p ->
    let (_s, z), loc = w_split p in
    p_literal ~loc (Literal_int z)
  | P_List p ->
    let p, loc = r_split p in
    let ps = List.map ~f:self (sepseq_to_list p.elements) in
    p_list ~loc (List ps)
  | P_ModPath p ->
    let p, loc = r_split p in
    let module_path =
      List.Ne.map
        (fun x ->
          let x, loc = w_split x in
          TODO_do_in_parsing.mvar ~loc x)
        (nsepseq_to_nseq p.module_path)
    in
    let field = self p.field in
    p_mod_access ~loc Mod_access.{ module_path; field; field_as_open = true }
  | P_Mutez p ->
    let (_s, z), loc = w_split p in
    p_literal ~loc (Literal_mutez (TODO_do_in_parsing.six_to_z z))
  | P_Nat p ->
    let (_s, z), loc = w_split p in
    p_literal ~loc (Literal_nat z)
  | P_Nil p ->
    let _, loc = w_split p in
    p_list ~loc (List [])
  | P_Par p -> self (r_fst p).inside
  | P_Record p ->
    let p, loc = r_split p in
    let fields =
      let translate_field_assign : CST.field_pattern -> (Label.t, AST.pattern) AST.Field.t
        = function
        | Punned { pun; attributes } ->
          TODO_do_in_parsing.weird_attributes attributes;
          let loc = Location.lift @@ CST.pattern_to_region pun in
          let pun, _ = w_split (TODO_do_in_parsing.compile_pattern_record_lhs pun) in
          Punned (Location.wrap ~loc (Label.of_string pun))
        | Complete { field_lhs; field_lens = _; field_rhs; attributes } ->
          TODO_do_in_parsing.weird_attributes attributes;
          let lhs, _ =
            w_split (TODO_do_in_parsing.compile_pattern_record_lhs field_lhs)
          in
          Complete (Label.of_string lhs, self field_rhs)
      in
      List.map ~f:(translate_field_assign <@ r_fst) @@ sepseq_to_list p.elements
    in
    p_pun_record ~loc fields
  | P_String p ->
    let s, loc = w_split p in
    p_literal ~loc (Literal_string (Standard s))
  | P_Tuple p ->
    let p, loc = r_split p in
    let p = List.map ~f:self (nsepseq_to_list p.inside) in
    p_tuple ~loc p
  | P_Typed p ->
    let p, loc = r_split p in
    let ptrn = self p.pattern in
    let ty = compile_type_expression @@ snd p.type_annot in
    p_typed ~loc ty ptrn
  | P_Var p ->
    let s, loc = w_split p in
    p_var ~loc (TODO_do_in_parsing.var ~loc s)
  | P_Verbatim p ->
    let s, loc = w_split p in
    p_literal ~loc (Literal_string (Standard s))


(* ========================== INSTRUCTIONS ================================= *)

and compile_block : CST.block -> AST.block =
 fun b ->
  let stmts = List.Ne.map compile_statement @@ nsepseq_to_nseq b.statements in
  block_of_statements stmts


and compile_test_clause : CST.test_clause -> (instruction, block) AST.Test_clause.t =
 fun c ->
  match c with
  | CST.ClauseInstr i -> ClauseInstr (compile_instruction i)
  | CST.ClauseBlock b -> ClauseBlock (compile_block @@ r_fst b)


and compile_case_clause
    : type a b. (a -> b) -> a CST.case_clause -> (_, b) AST.Case.clause
  =
 fun f c ->
  let pattern = compile_pattern c.pattern in
  let rhs = f c.rhs in
  { pattern; rhs }


and compile_case : type a b. (a -> b) -> a CST.case -> (_, _, b) AST.Case.t =
 fun f c ->
  let expr = compile_expression c.expr in
  let cases = List.Ne.map (compile_case_clause f <@ r_fst) @@ nsepseq_to_nseq c.cases in
  { expr; cases }


and compile_cond : 'a 'b. ('a -> 'b) -> 'a CST.conditional -> (_, 'b) AST.Cond.t =
 fun f c ->
  let test = compile_expression c.test in
  let ifso = f c.if_so in
  let ifnot = Option.map ~f:(f <@ snd) c.if_not in
  { test; ifso; ifnot }


and compile_for_map : CST.for_map -> (_, _) AST.For_collection.for_map =
 fun m ->
  let binding =
    let k, _, v = m.binding in
    ( TODO_do_in_parsing.var ~loc:(w_snd k) (w_fst k)
    , TODO_do_in_parsing.var ~loc:(w_snd v) (w_fst v) )
  in
  let collection = compile_expression m.collection in
  let block = compile_block @@ r_fst m.block in
  { binding; collection; block }


and compile_for_set_or_list
    : CST.for_set_or_list -> (_, _) AST.For_collection.for_set_or_list
  =
 fun s ->
  let var = TODO_do_in_parsing.var ~loc:(w_snd s.var) (w_fst s.var) in
  let for_kind =
    match s.for_kind with
    | `Set _ -> `Set
    | `List _ -> `List
  in
  let collection = compile_expression s.collection in
  let block = compile_block (r_fst s.block) in
  { var; for_kind; collection; block }


and compile_instruction : CST.instruction -> AST.instruction =
 fun i ->
  let compile_expr = compile_expression in
  match i with
  | I_Assign i ->
    let i, loc = r_split i in
    let lhs_expr = compile_expr i.lhs in
    let rhs_expr = compile_expr i.rhs in
    i_struct_assign { lhs_expr; rhs_expr } ~loc
  | I_Call i ->
    let i, loc = r_split i in
    let f, args = i in
    let f = compile_expr f in
    let args =
      let x, loc = r_split args in
      Location.wrap ~loc @@ List.map ~f:compile_expr @@ sepseq_to_list x.inside
    in
    i_call ~loc (f, args)
  | I_Case i ->
    let i, loc = r_split i in
    let i = compile_case compile_test_clause i in
    i_case i ~loc
  | I_Cond i ->
    let i, loc = r_split i in
    let i = compile_cond compile_test_clause i in
    i_cond i ~loc
  | I_For i ->
    let i, loc = r_split i in
    let index = TODO_do_in_parsing.var ~loc:(w_snd i.index) (w_fst i.index) in
    let init = compile_expr i.init in
    let bound = compile_expr i.bound in
    let step = Option.map ~f:(compile_expr <@ snd) i.step in
    let block = compile_block @@ r_fst i.block in
    i_for ~loc { index; init; bound; step; block }
  | I_ForIn i ->
    let i, loc = TODO_unify_in_cst.for_in compile_for_map compile_for_set_or_list i in
    i_for_in i ~loc
  | I_Patch i ->
    let i, loc = r_split i in
    let collection = compile_expr i.collection in
    let patch_kind =
      match i.patch_kind with
      | `Map _ -> `Map
      | `Record _ -> `Record
      | `Set _ -> `Set
    in
    let patch = compile_expr i.patch in
    i_patch { collection; patch_kind; patch } ~loc
  | I_Remove i ->
    let i, loc = r_split i in
    let item_expr = compile_expr i.item in
    let remove_kind =
      match i.remove_kind with
      | `Set _ -> `Set
      | `Map _ -> `Map
    in
    let collection = compile_expr i.collection in
    i_remove { item_expr; remove_kind; collection } ~loc
  | I_Skip i ->
    let _, loc = w_split i in
    i_skip ~loc
  | I_While i ->
    let i, loc = r_split i in
    let cond = compile_expr i.cond in
    let block = compile_block @@ r_fst i.block in
    i_while { cond; block } ~loc


(* ========================== STATEMENTS ================================= *)

and compile_statement : CST.statement -> AST.statement =
 fun s ->
  let self = compile_statement in
  match s with
  | S_Attr (attr, stmt) ->
    let attr, loc = w_split attr in
    let attr = translate_attr_pascaligo attr in
    let stmt = self stmt in
    s_attr ~loc (attr, stmt)
  | S_Decl s ->
    let s = compile_declaration s in
    let loc = s.fp.location in
    s_decl s ~loc
  | S_Instr s ->
    let s = compile_instruction s in
    let loc = s.fp.location in
    s_instr s ~loc
  | S_VarDecl s ->
    let s, loc = r_split s in
    let pattern = compile_pattern s.pattern in
    let type_params = Option.map ~f:extract_type_params s.type_params in
    let rhs_type = Option.map ~f:(compile_type_expression <@ snd) s.var_type in
    let let_rhs = compile_expression s.init in
    TODO_unify_in_cst.vardecl_as_decl ~loc { pattern; type_params; rhs_type; let_rhs }


(* ========================== EXPRESSIONS ================================== *)

and extract_tuple : 'a. ('a, CST.comma) sepseq CST.par CST.reg -> 'a nseq option =
 fun t -> Option.map t.value.inside ~f:(fun x -> nsepseq_to_nseq x)


and extract_key : 'a. 'a CST.brackets CST.reg -> 'a = fun k -> (r_fst k).inside

and compile_expression : CST.expr -> AST.expr =
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
  let translate_projection : CST.projection -> AST.expr =
   fun proj ->
    let expr = self proj.record_or_tuple in
    TODO_unify_in_cst.nested_proj expr proj.field_path
  in
  return
  @@
  match e with
  | E_Var var ->
    let var, loc = w_split var in
    e_variable (TODO_do_in_parsing.var ~loc var) ~loc
  | E_Par par ->
    let par, _loc = r_split par in
    self par.inside
  | E_Bytes bytes_ ->
    let bytes_, loc = w_split bytes_ in
    let _s, b = bytes_ in
    e_bytes_hex b ~loc
  | E_String str ->
    let str, loc = w_split str in
    TODO_unify_in_cst.e_string str ~loc
  | E_Verbatim str ->
    let str, loc = w_split str in
    TODO_unify_in_cst.e_verbatim str ~loc
  | E_Cat cat -> compile_bin_op CARET cat
  | E_Add plus -> compile_bin_op PLUS plus
  | E_Sub minus -> compile_bin_op MINUS minus
  | E_Mult times -> compile_bin_op STAR times
  | E_Div slash -> compile_bin_op SLASH slash
  | E_Mod mod_ -> compile_bin_op WORD_MOD mod_
  | E_Neg minus -> compile_unary_op MINUS minus
  | E_Int i ->
    let (_, i), loc = w_split i in
    e_int_z ~loc i
  | E_Nat n ->
    let (_, n), loc = w_split n in
    e_nat_z ~loc n
  | E_Mutez m ->
    let (_, m), loc = w_split m in
    e_mutez_z ~loc (Z.of_int64 m)
  | E_Or or_ -> compile_bin_op WORD_OR or_
  | E_And and_ -> compile_bin_op WORD_AND and_
  | E_Not not_ -> compile_unary_op WORD_NOT not_
  | E_Lt lt -> compile_bin_op LT lt
  | E_Leq le -> compile_bin_op LE le
  | E_Gt gt -> compile_bin_op GT gt
  | E_Geq ge -> compile_bin_op GE ge
  | E_Equal eq -> compile_bin_op SEQ eq
  | E_Neq ne -> compile_bin_op EQ_SLASH_EQ ne
  | E_Tuple lst ->
    let npseq, loc = r_split lst in
    let nseq = nseq_map self (nsepseq_to_nseq npseq.inside) in
    e_tuple nseq ~loc
  | E_Record record ->
    let record, loc = r_split record in
    let fields =
      let translate_field_assign : (CST.expr, CST.expr) CST.field -> (_, expr) AST.Field.t
        = function
        | Punned p -> Punned (TODO_do_in_parsing.label_as_var p.pun)
        | Complete c ->
          let label = TODO_do_in_parsing.label_as_var c.field_lhs in
          Complete (label.wrap_content, self c.field_rhs)
      in
      List.map ~f:(translate_field_assign <@ r_fst) @@ sepseq_to_list record.elements
    in
    e_record_pun fields ~loc
  | E_Proj proj ->
    let proj, _loc = r_split proj in
    translate_projection proj
  | E_ModPath ma ->
    let ma, loc = r_split ma in
    let module_path =
      nseq_map (fun x -> TODO_do_in_parsing.mvar ~loc:(w_snd x) (w_fst x))
      @@ nsepseq_to_nseq ma.module_path
    in
    let field = self ma.field in
    let field_as_open = TODO_do_in_parsing.field_as_open ma in
    e_module_open_in ~loc { module_path; field; field_as_open }
  | E_Update up ->
    let up, loc = r_split up in
    let structure = self up.structure in
    let update = TODO_unify_in_cst.update_rhs self up.update in
    e_update { structure; update } ~loc
  | E_Fun f ->
    let f, loc = r_split f in
    let type_params = Option.map ~f:extract_type_params f.type_params in
    let parameters =
      List.map
        ~f:
          (TODO_do_in_parsing.compile_param_decl compile_pattern compile_type_expression
          <@ r_fst)
      @@ sepseq_to_list
      @@ (r_fst f.parameters).inside
    in
    let ret_type = Option.map ~f:(compile_type_expression <@ snd) f.ret_type in
    let body = self f.return in
    e_poly_fun { type_params; parameters; ret_type; body } ~loc
  | E_Ctor ctor ->
    let ctor, loc = w_split ctor in
    let ctor = e_constr (Label.of_string ctor) ~loc in
    e_ctor_app (ctor, None) ~loc
  | E_App app ->
    let (func, args), loc = r_split app in
    (match func with
    | E_Ctor ctor ->
      let ctor =
        let ctor, loc = w_split ctor in
        e_constr (Label.of_string ctor) ~loc
      in
      let args =
        Option.map args.value.inside ~f:(fun x -> nseq_map self @@ nsepseq_to_nseq x)
      in
      e_ctor_app (ctor, args) ~loc
    | _ ->
      let func = self func in
      let args =
        let args, loc = r_split args in
        Location.wrap ~loc @@ List.map ~f:self (sepseq_to_list args.inside)
      in
      e_call func args ~loc)
  | E_Case case ->
    let case, loc = r_split case in
    let cases = compile_case self case in
    e_match cases ~loc
  | E_Typed annot ->
    let annot, loc = r_split annot in
    let e, (_, te) = annot.inside in
    let e = self e in
    let te = compile_type_expression te in
    e_annot (e, te) ~loc
  | E_Cond cond ->
    let cond, loc = r_split cond in
    let test = self cond.test in
    let ifso = self cond.if_so in
    let ifnot = Option.map ~f:(self <@ snd) cond.if_not in
    e_cond { test; ifso; ifnot } ~loc
  | E_List list ->
    let list, loc = r_split list in
    let elements = List.map ~f:self @@ sepseq_to_list list.elements in
    e_list elements ~loc
  | E_Cons cons ->
    let CST.{ op; arg1; arg2 }, loc = r_split cons in
    let left = self arg1 in
    let right = self arg2 in
    TODO_unify_in_cst.better_as_binop_sharp ~loc op (left, right)
  | E_Set set ->
    let set, loc = r_split set in
    let elements = List.map ~f:self @@ sepseq_to_list set.elements in
    e_set elements ~loc
  | E_SetMem sm ->
    let sm, loc = r_split sm in
    let structure = self sm.set in
    let element = self sm.element in
    TODO_unify_in_cst.better_as_binop_contains ~loc sm.kwd_contains (element, structure)
  | E_MapLookup mlu ->
    let mlu, loc = r_split mlu in
    let map = self mlu.map in
    let keys = nseq_map (self <@ extract_key) mlu.keys in
    e_map_lookup { map; keys } ~loc
  | E_Map m ->
    let m, loc = r_split m in
    let elements : (expr * expr) list =
      let compile_binding (b : CST.binding) = self b.key, self b.value in
      List.map ~f:(compile_binding <@ r_fst) @@ sepseq_to_list m.elements
    in
    e_map elements ~loc
  | E_BigMap m ->
    let m, loc = r_split m in
    let elements : (expr * expr) list =
      let compile_binding (b : CST.binding) = self b.key, self b.value in
      List.map ~f:(compile_binding <@ r_fst) @@ sepseq_to_list m.elements
    in
    e_big_map elements ~loc
  | E_CodeInj ci ->
    let ci, loc = r_split ci in
    let language = r_fst @@ w_fst ci.language in
    let code = self ci.code in
    e_raw_code { language; code } ~loc
  | E_Block be ->
    let CST.{ block; expr; _ }, loc = r_split be in
    let block =
      let b, _ = r_split block in
      let block = nseq_map compile_statement @@ nsepseq_to_nseq b.statements in
      make_b ~loc block
    in
    e_block_with { block; expr = self expr } ~loc
  | E_Nil nil ->
    let _, loc = w_split nil in
    e_list [] ~loc
  | E_Attr (attr, expr) ->
    let attr, loc = w_split attr in
    let attr = translate_attr_pascaligo attr in
    let expr = self expr in
    e_attr (attr, expr) ~loc


(* ========================== DECLARATIONS ================================= *)

and compile_declaration : CST.declaration -> AST.declaration =
 fun decl ->
  let self = compile_declaration in
  let compile_type_params
      : CST.type_params CST.chevrons Region.reg -> AST.Ty_variable.t nseq
    =
   fun tp ->
    nseq_map
      (fun x -> TODO_do_in_parsing.tvar ~loc:(w_snd x) (w_fst x))
      (nsepseq_to_nseq (r_fst tp).inside)
  in
  match decl with
  | D_Directive d ->
    let loc = Simple_utils.Location.lift (Preprocessor.Directive.to_region d) in
    d_directive () ~loc
  | D_Type d ->
    let d, loc = r_split d in
    let name = TODO_do_in_parsing.tvar ~loc:(w_snd d.name) (w_fst d.name) in
    let params =
      Option.map
        ~f:(fun (tp : _ CST.par CST.reg) ->
          List.Ne.map
            (fun x -> TODO_do_in_parsing.tvar ~loc:(w_snd x) (w_fst x))
            (nsepseq_to_nseq (r_fst tp).inside))
        d.params
    in
    let type_expr = compile_type_expression d.type_expr in
    d_type_abstraction { name; params; type_expr } ~loc
  | D_Const d ->
    let d, loc = r_split d in
    let type_params = Option.map ~f:compile_type_params d.type_params in
    let pattern = compile_pattern d.pattern in
    let rhs_type = Option.map ~f:(compile_type_expression <@ snd) d.const_type in
    let let_rhs = compile_expression d.init in
    d_const ~loc { pattern; type_params; rhs_type; let_rhs }
  | D_Attr d ->
    let (attr, decl), loc = r_split d in
    let attr = translate_attr_pascaligo (w_fst attr) in
    let decl = self decl in
    d_attr (attr, decl) ~loc
  | D_Fun d ->
    let d, loc = r_split d in
    let is_rec =
      match d.kwd_recursive with
      | Some _ -> true
      | None -> false
    in
    let fun_name = TODO_do_in_parsing.var ~loc:(w_snd d.fun_name) (w_fst d.fun_name) in
    let type_params = Option.map ~f:compile_type_params d.type_params in
    let parameters =
      let compile_param_decl =
        TODO_do_in_parsing.compile_param_decl compile_pattern compile_type_expression
      in
      List.map ~f:(compile_param_decl <@ r_fst)
      @@ sepseq_to_list (r_fst d.parameters).inside
    in
    let ret_type = Option.map ~f:(compile_type_expression <@ snd) d.ret_type in
    let return = compile_expression d.return in
    d_fun { is_rec; fun_name; type_params; parameters; ret_type; return } ~loc
  | D_Module d ->
    let d, loc = r_split d in
    let name = TODO_do_in_parsing.mvar ~loc:(w_snd d.name) (w_fst d.name) in
    let mod_expr = compile_module d.module_expr in
    d_module { name; mod_expr } ~loc


(* ========================== MODULES ===================================== *)

and compile_module : CST.module_expr -> AST.mod_expr =
 fun m ->
  match m with
  | M_Body m ->
    let m, loc = r_split m in
    let ds = nseq_map compile_declaration m.declarations in
    m_body (TODO_unify_in_cst.declarations_as_program ds) ~loc
  | M_Path m ->
    let m, loc = r_split m in
    let module_path =
      List.Ne.map
        (fun t -> TODO_do_in_parsing.mvar ~loc:(w_snd t) (w_fst t))
        (nsepseq_to_nseq m.module_path)
    in
    let field = TODO_do_in_parsing.mvar ~loc:(w_snd m.field) (w_fst m.field) in
    m_path (List.Ne.append module_path (field, [])) ~loc
  | M_Var m ->
    let s, loc = w_split m in
    let v = TODO_do_in_parsing.mvar ~loc s in
    m_var v ~loc


(* ========================== PROGRAM ===================================== *)
let compile_program : CST.t -> AST.program =
 fun t ->
  nseq_to_list t.decl
  |> List.map ~f:(fun a -> compile_declaration a)
  |> List.map ~f:(fun x -> make_pe @@ PE_declaration x)
  |> make_prg
