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
  type mod_expr = I.module_expr
  type instruction = unit
  type declaration = I.declaration
  type program_entry = I.declaration
  type program = I.t
  type sig_expr = I.signature_expr
  type sig_entry = I.sig_item

  let not_part_of_the_language _ = assert false
end

module Folding = Folding (Eq)

module TODO_do_in_parsing = struct
  let should_be_nseq (x : ('a, _) sepseq) = Option.value_exn x

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

  (* I don't know what to do with those lens/attributes *)

  let weird_attributes _ = ()
  let weird_lens _ = ()

  (* Variables *)

  let mvar x = Ligo_prim.Module_var.of_input_var ~loc:(w_snd x) (w_fst x)
  let var x = Ligo_prim.Value_var.of_input_var ~loc:(w_snd x) (w_fst x)
  let tvar x = Ligo_prim.Type_var.of_input_var ~loc:(w_snd x) (w_fst x)
  let labelize x = Ligo_prim.Label.of_string (w_fst x)

  (* (Potentially) Escaped variables *)

  let get_var = function
    | I.Var v | I.Esc v -> v


  let esc_var = var <@ get_var
  let esc_tvar = tvar <@ get_var
  let labelize_esc = labelize <@ get_var

  let mk_E_Variable_esc = function
    | I.Var v -> O.E_variable_esc (Raw (var v))
    | I.Esc v -> O.E_variable_esc (Esc (var v))


  let mk_T_var_esc = function
    | I.Var v -> O.T_var_esc (Raw (tvar v))
    | I.Esc v -> O.T_var_esc (Esc (tvar v))


  let mk_P_var_esc = function
    | I.Var v -> O.P_var_esc (Raw (var v))
    | I.Esc v -> O.P_var_esc (Esc (var v))


  (* Constructors *)

  let ctor_arg (lst : I.expr nseq) : I.expr =
    let region =
      let lst = List.Ne.to_list lst in
      List.fold ~init:Region.ghost ~f:Region.cover (List.map ~f:I.expr_to_region lst)
    in
    match lst with
    | e, [] -> e (* When there's only one arg, no need to wrap it in E_Tuple *)
    | l ->
      I.E_Tuple
        { region; value = nsepseq_of_nseq ~sep:Lexing_cameligo.Token.ghost_comma l }


  (* the I.field_path_assignment type is slightly different from other syntaxes:
     - no lenses ; labels are string ; the Path case *)
  let update_rhs (compile_lens, compile_selection) updates =
    let lst = nsepseq_to_list updates in
    let f : (I.path, _, I.expr) I.field -> I.expr O.Update.field = function
      | Punned { region; value = { attributes; pun } } ->
        weird_attributes attributes;
        let loc = Location.lift region in
        (match pun with
        | Name x -> O.Update.Pun (Location.wrap ~loc @@ labelize_esc x)
        | _ -> assert false (* never emited by parser *))
      | Complete { region = _; value = { attributes; field_lhs; field_lens; field_rhs } }
        ->
        weird_attributes attributes;
        let field_lhs =
          match field_lhs with
          | Name v -> [ AST.Selection.FieldName (labelize_esc v) ]
          | Path { region = _; value = { record_or_tuple; selector = _; field_path } } ->
            let struct_name =
              match record_or_tuple with
              | I.E_Var x -> x
              | _ -> assert false (* never emited by parser *)
            in
            AST.Selection.FieldName (labelize_esc struct_name)
            :: List.map (nsepseq_to_list field_path) ~f:compile_selection
        in
        let field_lens = compile_lens field_lens in
        (* let field_rhs = self field_rhs in *)
        O.Update.(Full_field { field_lhs; field_lens; field_rhs })
    in
    List.map ~f lst


  (* use the same type as in other CST's *)
  let fun_binder : I.pattern nseq -> I.pattern AST.Param.t list =
   fun ps ->
    List.map
      ~f:(fun pattern -> AST.Param.{ param_kind = `Const; pattern })
      (nseq_to_list ps)


  let quoted_tvar (x : I.type_var) =
    let var = snd @@ r_fst x in
    let var =
      match var with
      | I.Var v | I.Esc v -> v
    in
    tvar @@ I.Wrap.make ("'" ^ var#payload) var#region


  (* couldn't it be a list directly ?*)
  let type_vars_to_lst (p : I.type_vars) : AST.Ty_variable.t List.Ne.t =
    match p with
    | TV_Single x -> List.Ne.singleton (quoted_tvar x)
    | TV_Tuple x -> nseq_map (fun x -> quoted_tvar x) (nsepseq_to_nseq (r_fst x).inside)


  let empty_sequence () = failwith "should a sequence be allowed to be empty ?"

  (* Not sure what we should do here , maybe TPar should be in unified ?*)
  let field_as_open_t field =
    match field with
    | I.T_Par _ -> true
    | _ -> false


  (* would be cool to use the same type if possible *)
  let compile_rows = O.Non_linear_rows.make

  let make_block ~region (seq_expr : (I.expr, I.semi) nsepseq option) : I.expr =
    match seq_expr with
    | None -> E_Unit { value = Lexing_cameligo.Token.(ghost_lpar, ghost_rpar); region }
    | Some _ ->
      let seq : _ Region.reg =
        { region; value = I.{ elements = seq_expr; compound = None } }
      in
      E_Seq seq


  let make_int i : I.expr = E_Int Lexing_cameligo.Token.(ghost_int (Z.of_int i))
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
    ret @@ O.E_binary_op { operator = Location.wrap ~loc sign; left = arg1; right = arg2 }
  in
  let compile_unary_op (sign : AST.Operators.op) op =
    let I.{ op = _; arg } = r_fst op in
    ret @@ O.E_unary_op { operator = Location.wrap ~loc sign; arg }
  in
  let compile_selection =
    let open Nano_prim.Selection in
    function
    | I.FieldName name -> FieldName (TODO_do_in_parsing.labelize_esc name)
    | I.Component c -> Component_num c#payload
  in
  let compile_type_params : I.type_params I.par -> Ligo_prim.Type_var.t nseq =
   fun tp -> nseq_map TODO_do_in_parsing.esc_tvar (snd (r_fst tp).inside)
  in
  let compile_lens : I.lens -> AST.Update.field_lens = function
    | I.Lens_Id _ -> Lens_Id
    | I.Lens_Add _ -> Lens_Add
    | I.Lens_Sub _ -> Lens_Sub
    | I.Lens_Mult _ -> Lens_Mult
    | I.Lens_Div _ -> Lens_Div
    | I.Lens_Fun _ -> Lens_Fun
  in
  match e with
  | E_Attr (x, y) -> ret @@ E_attr (fst @@ TODO_do_in_parsing.conv_attr x, y)
  | E_Var v -> ret @@ TODO_do_in_parsing.mk_E_Variable_esc v
  (* we keep parenthesis so that the backward pass which add parenthesis is done only once for all syntaxes (?) *)
  | E_Par par -> expr par.value.inside
  | E_Unit _ -> ret @@ E_literal Literal_unit
  | E_False _ -> ret @@ E_constr (Ligo_prim.Label.of_string "False")
  | E_True _ -> ret @@ E_constr (Ligo_prim.Label.of_string "True")
  | E_Bytes x -> ret @@ E_literal (Literal_bytes (Hex.to_bytes (snd x#payload)))
  | E_Cat c -> compile_bin_op CARET c
  | E_String str ->
    ret @@ E_literal (Literal_string (Simple_utils.Ligo_string.Standard str#payload))
  | E_Verbatim str ->
    ret @@ E_literal (Literal_string (Simple_utils.Ligo_string.Verbatim str#payload))
  | E_Add plus -> compile_bin_op PLUS plus
  | E_Sub minus -> compile_bin_op MINUS minus
  | E_Mult times -> compile_bin_op STAR times
  | E_Div slash -> compile_bin_op SLASH slash
  | E_Mod mod_ -> compile_bin_op PRCENT mod_
  | E_Land land_ -> compile_bin_op WORD_LAND land_
  | E_Lor lor_ -> compile_bin_op WORD_LOR lor_
  | E_Lxor lxor_ -> compile_bin_op WORD_LXOR lxor_
  | E_Lsl lsl_ -> compile_bin_op WORD_LSL lsl_
  | E_Lsr lsr_ -> compile_bin_op WORD_LSR lsr_
  | E_Neg minus -> compile_unary_op MINUS minus
  | E_Int i -> ret @@ E_literal (Literal_int (snd i#payload))
  | E_Nat n -> ret @@ E_literal (Literal_nat (snd n#payload))
  | E_Mutez m -> ret @@ E_literal (Literal_mutez (Z.of_int64 (snd m#payload)))
  | E_Or or_ -> compile_bin_op DPIPE or_
  | E_And and_ -> compile_bin_op DAMPERSAND and_
  | E_Not not_ -> compile_unary_op WORD_NOT not_
  | E_Lt lt -> compile_bin_op LT lt
  | E_Leq le -> compile_bin_op LE le
  | E_Gt gt -> compile_bin_op GT gt
  | E_Geq ge -> compile_bin_op GE ge
  | E_Equal eq -> compile_bin_op SEQ eq
  | E_Neq ne -> compile_bin_op LTGT ne
  | E_Cons cons -> compile_bin_op DCOLON cons
  | E_RevApp ra -> ret @@ E_rev_app { x = ra.value.arg1; f = ra.value.arg2 }
  | E_Tuple { value; _ } -> ret @@ E_tuple (nsepseq_to_nseq value)
  | E_Record { value = record; _ } ->
    let compile_field
        : (I.field_name, I.equal, I.expr) I.field -> (O.Label.t, I.expr) O.Field.t
      = function
      | Punned p ->
        let I.{ attributes; pun }, loc = r_split p in
        TODO_do_in_parsing.weird_attributes attributes;
        let s = TODO_do_in_parsing.labelize_esc pun in
        AST.Field.Punned (Location.wrap ~loc s)
      | Complete c ->
        let I.{ attributes; field_lhs; field_lens; field_rhs } = r_fst c in
        TODO_do_in_parsing.weird_lens field_lens (* TODO : add weird_lens *);
        TODO_do_in_parsing.weird_attributes attributes;
        let label = TODO_do_in_parsing.labelize_esc field_lhs in
        AST.Field.Complete (label, field_rhs)
    in
    let record = sepseq_to_list record.inside in
    let fields = List.map ~f:compile_field record in
    ret @@ E_record_pun fields
  | E_Proj { value = { record_or_tuple; field_path; _ }; region } ->
    let field_path = nsepseq_map compile_selection field_path in
    ret @@ E_proj (record_or_tuple, nsepseq_to_list @@ field_path)
  | E_ModPath { value; _ } ->
    let I.{ module_path; selector = _; field } = value in
    let module_path =
      List.Ne.map TODO_do_in_parsing.mvar @@ nsepseq_to_nseq module_path
    in
    let field_as_open =
      match field with
      | E_Par _ -> true
      | _ -> false
    in
    ret @@ E_module_open_in { module_path; field; field_as_open }
  | E_Update { value; _ } ->
    let I.{ record; updates; _ } = value.inside in
    let structure = record in
    let update =
      TODO_do_in_parsing.update_rhs (compile_lens, compile_selection) updates
    in
    ret @@ E_update { structure; update }
  | E_Fun { value = { type_params; binders; rhs_type; body; _ }; _ } ->
    let type_params = Option.map ~f:compile_type_params type_params in
    ret
    @@ E_poly_fun
         { type_params
         ; parameters = TODO_do_in_parsing.fun_binder binders
         ; ret_type = Option.map ~f:snd rhs_type
         ; body
         }
  | E_App { value = E_Ctor name, args; _ } ->
    (* TODO: matching between E_Ctor in TODO_do_in_parsing *)
    let constructor = TODO_do_in_parsing.labelize name in
    let element = TODO_do_in_parsing.ctor_arg args in
    ret @@ O.(E_applied_constructor { constructor; element })
  | E_App { value = func, args; _ } ->
    let args = List.Ne.to_list args in
    let arg_loc =
      List.fold
        ~init:Location.dummy
        ~f:Location.cover
        (List.map ~f:(Location.lift <@ I.expr_to_region) args)
    in
    ret @@ E_call (func, Location.wrap ~loc:arg_loc args)
  | E_Ctor value -> ret @@ E_constr (TODO_do_in_parsing.labelize value)
  | E_Match { value = { subject; clauses; _ }; region } ->
    let cases, loc2 = r_split clauses in
    let loc = Location.cover (Location.lift region) loc2 in
    let cases =
      let compile_case_clause I.{ pattern; rhs; _ } =
        O.Case.{ pattern = Some pattern; rhs }
      in
      nseq_map (compile_case_clause <@ r_fst) @@ nsepseq_to_nseq cases
    in
    Location.wrap ~loc @@ O.E_match { expr = subject; cases }
  | E_Typed a ->
    let e, (_, te) = a.value.inside in
    ret @@ E_annot (e, te)
  | E_Cond { value = { test; if_so = ifso; if_not; _ }; _ } ->
    let ifnot = Option.map ~f:snd if_not in
    ret @@ E_cond { test; ifso; ifnot }
  | E_List listcomp ->
    let elements = sepseq_to_list listcomp.value.inside in
    ret @@ E_list elements
  | E_LetIn { value = { kwd_rec; binding; body; _ }; _ } ->
    let I.{ type_params; binders; rhs_type; eq = _; let_rhs } = binding.value in
    let is_rec =
      match kwd_rec with
      | Some _ -> true
      | None -> false
    in
    let type_params = Option.map ~f:compile_type_params type_params in
    let rhs_type = Option.map ~f:snd rhs_type in
    ret @@ E_let_in { is_rec; type_params; lhs = binders; rhs_type; rhs = let_rhs; body }
  | E_TypeIn
      { value = { type_decl = { value = { name; type_expr; params; _ }; _ }; body; _ }
      ; _
      } ->
    let name = TODO_do_in_parsing.esc_tvar name in
    let params = Option.map params ~f:(fun p -> TODO_do_in_parsing.type_vars_to_lst p) in
    ret @@ E_type_in { type_decl = { name; params; type_expr }; body }
  | E_ModIn { value = { mod_decl = { value = { name; module_expr; _ }; _ }; body; _ }; _ }
    ->
    let module_name = TODO_do_in_parsing.mvar name in
    ret @@ E_mod_in { module_name; rhs = module_expr; body }
  | E_CodeInj { value = { language; code; _ }; _ } ->
    let language = r_fst @@ w_fst language in
    ret @@ E_raw_code { language; code }
  | E_Seq { value = { elements; _ }; _ } ->
    (match elements with
    | None -> TODO_do_in_parsing.empty_sequence ()
    | Some nelst ->
      let seq = nsepseq_to_list nelst in
      ret @@ E_sequence seq)
  | E_ContractOf c ->
    ret @@ E_contract (List.Ne.map TODO_do_in_parsing.mvar (nsepseq_to_nseq c.value))
  | E_Assign { value = { binder; expr = expression; _ }; _ } ->
    let binder = Ligo_prim.Binder.make (TODO_do_in_parsing.esc_var binder) None in
    ret @@ E_assign_unitary { binder; expression }
  | E_LetMutIn
      { value =
          { binding = { value = { binders; type_params; rhs_type; let_rhs; _ }; _ }
          ; body
          ; _
          }
      ; _
      } ->
    let type_params = Option.map ~f:compile_type_params type_params in
    let rhs_type = Option.map ~f:snd rhs_type in
    ret
    @@ E_let_mut_in
         { is_rec = false; type_params; lhs = binders; rhs_type; rhs = let_rhs; body }
  | E_While { value = { cond; body = { value = { seq_expr; _ }; region }; _ }; _ } ->
    let block = TODO_do_in_parsing.make_block ~region seq_expr in
    ret @@ E_while { cond; block }
  | E_For
      { value =
          { index; bound1; direction; bound2; body = { value = { seq_expr; _ }; _ }; _ }
      ; region
      } ->
    let index = TODO_do_in_parsing.esc_var index in
    let init = bound1 in
    let bound = bound2 in
    let step =
      match direction with
      | Upto _ -> None
      | Downto _ -> Some TODO_do_in_parsing.(make_int (-1))
    in
    let block = TODO_do_in_parsing.make_block ~region seq_expr in
    ret @@ E_for { index; init; bound; step; block }
  | E_ForIn
      { value = { pattern; collection; body = { value = { seq_expr; _ }; region }; _ }
      ; _
      } ->
    let block = TODO_do_in_parsing.make_block ~region seq_expr in
    ret @@ E_for_in (ForAny { pattern; collection; block })


let rec ty_expr : Eq.ty_expr -> Folding.ty_expr =
 fun te ->
  let loc = Location.lift (I.type_expr_to_region te) in
  let ret = Location.wrap ~loc in
  match te with
  | T_Var v -> ret @@ TODO_do_in_parsing.mk_T_var_esc v
  | T_Arg v -> ret @@ T_var_esc (Raw (TODO_do_in_parsing.quoted_tvar v))
  | T_Attr (x, y) -> ret @@ T_attr (fst @@ TODO_do_in_parsing.conv_attr x, y)
  | T_Cart { value = hd, _, tl; _ } -> ret @@ T_prod (hd, nsepseq_to_list tl)
  | T_Variant { value = { variants; _ }; region = _ } ->
    let sum =
      let compile_variant I.{ ctor; ctor_args; attributes } =
        ( TODO_do_in_parsing.labelize ctor
        , Option.map ~f:snd ctor_args
        , List.map (TODO_do_in_parsing.conv_attrs attributes) ~f:fst )
      in
      let lst = List.map (nsepseq_to_list variants) ~f:(compile_variant <@ r_fst) in
      TODO_do_in_parsing.compile_rows lst
    in
    ret @@ T_sum_raw sum
  | T_Record { value; region = _ } ->
    let elements = TODO_do_in_parsing.should_be_nseq value.inside in
    let fields =
      let field_decls = nseq_map r_fst @@ nsepseq_to_nseq elements in
      (* let open Ligo_prim in *)
      let compile_field_decl
          : int -> I.field_decl -> I.type_expr option O.Non_linear_rows.row
        =
       fun i { field_name; field_type; attributes; _ } ->
        let l = TODO_do_in_parsing.labelize_esc field_name in
        let rows =
          O.Non_linear_rows.
            { decl_pos = i
            ; associated_type = Option.map ~f:snd field_type
            ; attributes = List.map (TODO_do_in_parsing.conv_attrs attributes) ~f:fst
            }
        in
        l, rows
      in
      List.mapi ~f:compile_field_decl (nseq_to_list field_decls)
    in
    ret @@ T_record_raw fields
  | T_App { value = constr, args; _ } ->
    let constr = constr in
    let type_args =
      match args with
      | I.TC_Single te -> List.Ne.singleton te
      | I.TC_Tuple tes -> nsepseq_to_nseq (r_fst tes).inside
    in
    ret @@ T_app { constr; type_args }
  | T_Fun { value = te1, _, te2; _ } -> ret @@ T_fun (te1, te2)
  | T_Par t -> ty_expr (r_fst t).inside
  | T_String v -> ret @@ T_string (w_fst v)
  | T_Int v ->
    let a, b = w_fst v in
    ret @@ T_int (a, b)
  | T_ModPath { value = { module_path = hd, tl; field; selector }; region } ->
    let field_as_open = TODO_do_in_parsing.field_as_open_t field in
    (* TODO: change AST_unified *)
    (match List.Ne.of_list_opt tl with
    | None ->
      ret
      @@ T_module_open_in
           { module_path = TODO_do_in_parsing.mvar hd; field; field_as_open }
    | Some ((_, rhd), rtl) ->
      let field =
        I.T_ModPath { value = { module_path = rhd, rtl; field; selector }; region }
      in
      ret
      @@ T_module_open_in
           { module_path = TODO_do_in_parsing.mvar hd; field; field_as_open })
  | T_ParameterOf { value; _ } ->
    let path = nsepseq_map TODO_do_in_parsing.mvar value in
    ret @@ T_contract_parameter (nsepseq_to_nseq path)


let rec pattern : Eq.pattern -> Folding.pattern =
 fun p ->
  let loc = Location.lift (I.pattern_to_region p) in
  let ret = Location.wrap ~loc in
  match p with
  | P_Ctor v -> ret @@ O.P_ctor (TODO_do_in_parsing.labelize v)
  | P_Unit _ -> ret @@ P_unit
  | P_False _ -> ret @@ P_ctor (Ligo_prim.Label.of_string "False")
  | P_True _ -> ret @@ P_ctor (Ligo_prim.Label.of_string "True")
  | P_Var v -> ret @@ TODO_do_in_parsing.mk_P_var_esc v
  | P_Int v -> ret @@ P_literal (Literal_int (snd (w_fst v)))
  | P_Nat v -> ret @@ P_literal (Literal_nat (snd (w_fst v)))
  | P_Mutez v -> ret @@ P_literal (Literal_mutez (Z.of_int64 (snd (w_fst v))))
  | P_Bytes v -> ret @@ P_literal (Literal_bytes (Hex.to_bytes (snd (w_fst v))))
  | P_String v ->
    ret @@ P_literal (Literal_string (Simple_utils.Ligo_string.standard (w_fst v)))
  | P_Verbatim v ->
    ret @@ P_literal (Literal_string (Simple_utils.Ligo_string.verbatim (w_fst v)))
  | P_List { value; _ } ->
    let ps = sepseq_to_list value.inside in
    ret @@ P_list (List ps)
  | P_Cons { value = p1, _, p2; _ } -> ret @@ P_list (Cons (p1, p2))
  | P_Tuple { value = p; _ } -> ret @@ P_tuple (nsepseq_to_list p)
  | P_Par p -> pattern (r_fst p).inside
  | P_Record { value = { inside = p; _ }; _ } ->
    let p =
      let compile_field_pattern = function
        | I.Punned { value = { attributes = _; pun }; region } ->
          O.Field.Punned
            Location.(wrap ~loc:(lift region) (TODO_do_in_parsing.labelize_esc pun))
        | I.Complete { value = { attributes; field_lhs; field_rhs; _ }; region = _ } ->
          TODO_do_in_parsing.weird_attributes attributes;
          O.Field.Complete (TODO_do_in_parsing.labelize_esc field_lhs, field_rhs)
      in
      List.map ~f:compile_field_pattern (sepseq_to_list p)
    in
    ret @@ P_pun_record p
  | P_Typed { value = pattern, (_, ty); _ } -> ret @@ P_typed (ty, pattern)
  | P_App { value = p, p_arg; _ } -> ret @@ P_app (p, p_arg)
  | P_Attr (attr, p) -> ret @@ P_attr (fst @@ TODO_do_in_parsing.conv_attr attr, p)
  | P_ModPath { value = { module_path; field; _ }; _ } ->
    let field_as_open =
      match field with
      | P_Par _ -> true
      | _ -> false
    in
    let module_path = nseq_map TODO_do_in_parsing.mvar (nsepseq_to_nseq module_path) in
    ret @@ P_mod_access { module_path; field; field_as_open }


let declaration : Eq.declaration -> Folding.declaration =
 fun decl ->
  let loc = Location.lift (I.declaration_to_region decl) in
  let ret = Location.wrap ~loc in
  match decl with
  | D_Attr { value = attr, d; _ } ->
    ret @@ O.D_attr (fst @@ TODO_do_in_parsing.conv_attr attr, d)
  | D_Directive d ->
    let loc = Simple_utils.Location.lift (Preprocessor.Directive.to_region d) in
    Location.wrap ~loc @@ O.D_directive (ignore d)
  | D_Let
      { value = _, kwd_rec, { binders; type_params; rhs_type; let_rhs; _ }; region = _ }
    ->
    let is_rec =
      match kwd_rec with
      | None -> false
      | Some _ -> true
    in
    let type_params =
      let compile_type_params : I.type_params I.par -> AST.Ty_variable.t nseq =
       fun tp -> nseq_map TODO_do_in_parsing.esc_tvar (snd tp.value.inside)
      in
      Option.map ~f:compile_type_params type_params
    in
    let pattern = binders in
    let rhs_type = Option.map ~f:snd rhs_type in
    ret (D_let { is_rec; type_params; pattern; rhs_type; let_rhs })
  | D_Type { value = { name; params; type_expr; _ }; _ } ->
    let name = TODO_do_in_parsing.esc_tvar name in
    let params = Option.map ~f:TODO_do_in_parsing.type_vars_to_lst params in
    ret @@ D_type_abstraction { name; params; type_expr }
  | D_Module { value = { name; module_expr; annotation; _ }; _ } ->
    let name = TODO_do_in_parsing.mvar name in
    let annotation =
      O.Mod_decl.
        { signatures = Option.to_list @@ Option.map annotation ~f:snd; filter = true }
    in
    ret @@ D_module { name; mod_expr = module_expr; annotation }
  | D_Signature { value = { name; signature_expr; _ }; _ } ->
    let name = TODO_do_in_parsing.mvar name in
    ret @@ D_signature { name; sig_expr = signature_expr; extends = [] }
  | D_Include { value = { module_expr; _ }; _ } -> ret @@ D_module_include module_expr


let mod_expr : Eq.mod_expr -> Folding.mod_expr =
 fun me ->
  let loc = Location.lift (I.module_expr_to_region me) in
  let ret = Location.wrap ~loc in
  match me with
  | M_Body { value; _ } ->
    let decl = List.Ne.of_list value.declarations in
    ret @@ O.M_body I.{ decl; eof = Lexing_cameligo.Token.ghost_eof }
  | M_Path m ->
    let m, _loc = r_split m in
    let module_path =
      List.Ne.map TODO_do_in_parsing.mvar (nsepseq_to_nseq m.module_path)
    in
    let field = TODO_do_in_parsing.mvar m.field in
    ret @@ M_path (List.Ne.append module_path (field, []))
  | M_Var m -> ret @@ M_var (TODO_do_in_parsing.mvar m)


let program_entry : Eq.program_entry -> Folding.program_entry = fun x -> PE_declaration x
let program : Eq.program -> Folding.program = fun { decl; _ } -> List.Ne.to_list decl

let sig_expr : Eq.sig_expr -> Folding.sig_expr = function
  | S_Sig { value = { sig_items; kwd_sig = _; kwd_end = _ }; region } ->
    let loc = Location.lift region in
    Location.wrap ~loc @@ O.S_body sig_items
  | S_Path { value = m; region } ->
    let loc = Location.lift region in
    let module_path =
      List.Ne.map TODO_do_in_parsing.mvar (nsepseq_to_nseq m.module_path)
    in
    let field = TODO_do_in_parsing.mvar m.field in
    Location.wrap ~loc @@ O.S_path (List.Ne.append module_path (field, []))
  | S_Var name ->
    let loc = Location.generated in
    let name = TODO_do_in_parsing.mvar name in
    Location.wrap ~loc @@ O.S_path (name, [])


let sig_entry : Eq.sig_entry -> Folding.sig_entry = function
  | S_Value { region; value = _, v, _, ty } ->
    let loc = Location.lift region in
    Location.wrap ~loc (O.S_value (TODO_do_in_parsing.esc_var v, ty, false))
  | S_Type { region; value = _, v, _, ty } ->
    let loc = Location.lift region in
    Location.wrap ~loc (O.S_type (TODO_do_in_parsing.esc_tvar v, ty))
  | S_TypeVar { region; value = _, v } ->
    let loc = Location.lift region in
    Location.wrap ~loc (O.S_type_var (TODO_do_in_parsing.esc_tvar v))
  | S_Attr { region; value = attr, si } ->
    let loc = Location.lift region in
    Location.wrap
      ~loc
      (O.S_attr (fst @@ TODO_do_in_parsing.conv_attr attr, si) : _ O.sig_entry_content_)
  | S_Include { region; value = _, sig_expr } ->
    let loc = Location.lift region in
    Location.wrap ~loc (O.S_include sig_expr)
