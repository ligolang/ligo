module CST = Cst.Cameligo
module AST = Ast_unified
module Helpers = Unification_shared.Helpers
open Simple_utils
module Label = Ligo_prim.Label
open Lexing_cameligo.Token
module Value_escaped_var = Nano_prim.Value_escaped_var
module Ty_escaped_var = Nano_prim.Ty_escaped_var

let ghost_var v = ghost_ident (Format.asprintf "%a" AST.Variable.pp v)
let decompile_var v = CST.Var (ghost_var v)

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
    ; pattern
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
and decompile_pattern p = AST.Catamorphism.cata_pattern ~f:folder p
and decompile_type_expression t = AST.Catamorphism.cata_ty_expr ~f:folder t
and decompile_sig_expr t = AST.Catamorphism.cata_sig_expr ~f:folder t
and decompile_mvar x = ghost_ident @@ Format.asprintf "%a" AST.Mod_variable.pp x

(* Helpers *)

and decompile_attr : AST.Attribute.t -> CST.attribute =
 fun { key; value } -> ghost_attr key (Option.map ~f:(fun x -> Attr.String x) value)


(* ^ XXX Attr.String or Attr.Ident? *)

and decompile_mod_path
    : type a.
      (AST.Mod_variable.t Simple_utils.List.Ne.t, a) AST.Mod_access.t -> a CST.module_path
  =
 fun { module_path; field; field_as_open = _ } ->
  let module_path =
    Utils.nsepseq_of_nseq ~sep:ghost_dot @@ Utils.nseq_map decompile_mvar module_path
  in
  (* XXX: What is [field_as_open]??
     answer: it basically means you need to add a parenthesis after module_path
     i.e. `A.B.x` is an access `A.B.(x)` is an module opening
  *)
  CST.{ module_path; selector = ghost_dot; field }


(* Decompilers: expect that all Ast nodes are initial, i.e.
   that backwards nanopasses were applied to Ast_unified before decompiler *)

and sig_expr
    :  (CST.signature_expr, CST.sig_item, CST.type_expr) AST.sig_expr_
    -> CST.signature_expr
  =
  let w = Region.wrap_ghost in
  function
  | { wrap_content = AST.S_body sig_items; location } ->
    CST.(S_Sig (w { kwd_sig = ghost_sig; sig_items; kwd_end = ghost_end }))
  | { wrap_content = AST.S_path lst; location } ->
    let lst = List.Ne.map decompile_mvar lst in
    let module_path_opt, field =
      let last, lst = List.Ne.rev lst in
      ( (match List.Ne.of_list_opt lst with
        | None -> None
        | Some lst -> Some (List.Ne.rev lst))
      , last )
    in
    (match module_path_opt with
    | Some module_path ->
      let module_path = Utils.nsepseq_of_nseq ~sep:ghost_dot lst in
      CST.(S_Path (w { module_path; selector = ghost_dot; field }))
    | None -> CST.(S_Var field))


and sig_entry
    : (CST.signature_expr, CST.sig_item, CST.type_expr) AST.sig_entry_ -> CST.sig_item
  =
 fun se ->
  let w = Region.wrap_ghost in
  match Location.unwrap se with
  | AST.S_value (v, ty, _) ->
    CST.(S_Value (w (ghost_val, decompile_var v, ghost_colon, ty)))
  | AST.S_type (v, ty) ->
    CST.(S_Type (w (ghost_type, decompile_tvar_into_var v, ghost_eq, ty)))
  | AST.S_type_var v -> CST.(S_TypeVar (w (ghost_type, decompile_tvar_into_var v)))
  | AST.S_attr (attr, se) -> CST.(S_Attr (w (decompile_attr attr, se)))
  | AST.S_include se -> CST.(S_Include (w (ghost_include, se)))


(* TODO: The E_variable case can be removed once
   Nanopasses.Espaced_variables.decompile is implemented. *)

and expr : (CST.expr, CST.type_expr, CST.pattern, unit, unit) AST.expression_ -> CST.expr =
 fun e ->
  let w = Region.wrap_ghost in
  match Location.unwrap e with
  | E_attr (attr, expr) -> E_Attr (decompile_attr attr, expr)
  | E_variable v -> E_Var (Var (ghost_var v))
  | E_variable_esc v -> E_Var (decompile_var_esc v)
  | E_binary_op { operator; left; right } ->
    let binop op : 'a CST.wrap CST.bin_op CST.reg =
      w @@ CST.{ op; arg1 = left; arg2 = right }
    in
    (match Location.unwrap operator with
    | CARET -> E_Cat (binop ghost_caret)
    | PLUS -> E_Add (binop ghost_plus)
    | MINUS -> E_Sub (binop ghost_minus)
    | STAR -> E_Mult (binop ghost_times)
    | SLASH -> E_Div (binop ghost_slash)
    | PRCENT -> E_Mod (binop ghost_mod)
    | DAMPERSAND -> E_And (binop ghost_bool_and)
    | LT -> E_Lt (binop ghost_lt)
    | GT -> E_Gt (binop ghost_gt)
    | GE -> E_Geq (binop ghost_ge)
    | LE -> E_Leq (binop ghost_le)
    | SEQ -> E_Equal (binop ghost_eq)
    | LTGT -> E_Neq (binop ghost_ne)
    | DCOLON -> E_Cons (binop ghost_cons)
    | WORD_LSL -> E_Lsl (binop ghost_lsl)
    | WORD_LSR -> E_Lsr (binop ghost_lsr)
    | WORD_LOR -> E_Lor (binop ghost_lor)
    | WORD_LAND -> E_Land (binop ghost_land)
    | WORD_LXOR -> E_Lxor (binop ghost_lxor)
    | DPIPE -> E_Or (binop ghost_or)
    | EX_MARK
    | WORD_XOR
    | CONTAINS
    | SHARP
    | WORD_OR
    | WORD_MOD
    | WORD_NOT
    | WORD_AND
    | DEQ
    | EQ_SLASH_EQ -> failwith "Decompiler: EQ_SLASH_EQ: Impossible")
  | E_unary_op { operator; arg } ->
    let unop op : 'a CST.wrap CST.un_op CST.reg = w @@ CST.{ op; arg } in
    (match Location.unwrap operator with
    | MINUS -> E_Neg (unop ghost_minus)
    | WORD_NOT -> E_Not (unop ghost_not)
    | _ -> failwith "Decompiler: unop: Impossible")
  | E_literal Literal_unit -> CST.E_Unit (w (ghost_lpar, ghost_rpar))
  | E_literal (Literal_int x) -> CST.E_Int (ghost_int x)
  | E_literal (Literal_nat x) -> CST.E_Nat (ghost_nat x)
  | E_literal (Literal_mutez x) -> CST.E_Mutez (ghost_mutez @@ Z.to_int64 x)
  | E_module_open_in m -> E_ModPath (w @@ decompile_mod_path m)
  | E_application { lamb; args } -> CST.E_App (w @@ (lamb, (args, [])))
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


and pattern : (CST.pattern, CST.type_expr) AST.pattern_ -> CST.pattern =
 fun p ->
  let w = Region.wrap_ghost in
  match Location.unwrap p with
  | P_attr (attr, p) -> P_Attr (decompile_attr attr, p)
  | P_unit -> P_Unit (w (ghost_lpar, ghost_rpar))
  | P_typed (type_expr, pattern) ->
    (* should be this:
      PTyped (w CST.{ pattern; colon = ghost_colon; type_expr })
    but since pattern are only used for errors, we don't want them
    *)
    ignore type_expr;
    pattern
  | P_var v -> P_Var (Var (ghost_var v))
  | P_var_esc v -> P_Var (decompile_var_esc v)
  | P_list (List lst) ->
    let elements = Utils.list_to_sepseq lst ghost_semi in
    P_List
      (w CST.{ lbracket = ghost_lbracket; inside = elements; rbracket = ghost_rbracket })
  | P_list (Cons (l, r)) -> P_Cons (w (l, ghost_cons, r))
  | P_variant (l, p_opt) ->
    let constr = CST.P_Ctor (ghost_ident @@ AST.Label.to_string l) in
    (match p_opt with
    | None -> constr
    | Some pat -> P_App (w (constr, Some pat)))
  | P_tuple lst ->
    let lst = Utils.list_to_sepseq lst ghost_comma in
    let lst =
      match lst with
      | None -> failwith "Decompiler: empty P_tuple"
      | Some lst -> lst
    in
    P_Par (w CST.{ lpar = ghost_lpar; inside = P_Tuple (w lst); rpar = ghost_rpar })
  | P_pun_record lst ->
    let ne_elements =
      let lst : (CST.field_name, CST.equal, CST.pattern) CST.field list =
        List.map lst ~f:(function
            | Complete (l, p) ->
              CST.Complete
                (w
                @@ CST.
                     { field_lhs = Var (ghost_ident (AST.Label.to_string l))
                     ; field_lens = ghost_eq
                     ; field_rhs = p
                     ; attributes = []
                     })
            | Punned { wrap_content = l; _ } ->
              (* XXX do we need to extract attributes here?
                              Why this CST node has a separate record for attributes at all?*)
              CST.Punned
                (w
                @@ CST.
                     { attributes = []; pun = Var (ghost_ident (AST.Label.to_string l)) }
                ))
      in
      Utils.list_to_sepseq lst ghost_semi
    in
    P_Record
      (w CST.{ lbrace = ghost_lbrace; inside = ne_elements; rbrace = ghost_rbrace })
  | P_ctor c -> P_Ctor (CST.Wrap.ghost @@ AST.Label.to_string c)
  | P_app (c, arg) -> P_App (w (c, arg))
  | P_mod_access m -> P_ModPath (w @@ decompile_mod_path m)
  | pat when AST.pattern_is_not_initial pat ->
    Helpers.failwith_not_initial_node_decompiler @@ `Pattern p
  | _ ->
    failwith
      (Format.asprintf
         "Can't decompile this node : \n%a"
         Sexp.pp_hum
         (AST.sexp_of_pattern_ (fun _ -> Sexp.Atom "xx") (fun _ -> Sexp.Atom "xx") p))


and ty_expr : CST.type_expr AST.ty_expr_ -> CST.type_expr =
 fun te ->
  let w = Region.wrap_ghost in
  let needs_parens : CST.type_expr -> bool = function
    (* When we apply type constr to some type, sometimes we need to add parens
      e.g. [A of int] -> [(A of int) option] vs [{a : int}] -> [{ a : int }] option. *)
    | T_Fun _ | T_Cart _ | T_Variant _ | T_Attr _ | T_ParameterOf _ -> true
    | T_Par _
    | T_App _
    | T_Var _
    | T_Arg _
    | T_Record _
    | T_ModPath _
    | T_Int _
    | T_String _ -> false
  in
  let parens : CST.type_expr -> CST.type_expr =
   fun t -> T_Par (w CST.{ lpar = ghost_lpar; inside = t; rpar = ghost_rpar })
  in
  (* Since [a -> (b -> c)] is identical to [a -> (b -> c)], if a [T_Fun] is an rhs of another [T_Fun],
     we don't need parens. So we call `p ~arrow_rhs:true` for the rhs of a T_Fun *)
  let p : ?arrow_rhs:bool -> CST.type_expr -> CST.type_expr =
   fun ?(arrow_rhs = false) t ->
    if needs_parens t
    then
      if arrow_rhs
      then (
        match t with
        | T_Fun _ -> t
        | _ -> parens t)
      else parens t
    else t
  in
  (* ^ XXX we should split generated names on '#'? Can we just extract the name somehow?
      Why [t.name] is not working?? *)
  match Location.unwrap te with
  | T_attr (attr, t) -> T_Attr (decompile_attr attr, t)
  | T_int (_s, z) -> T_Int (ghost_int z)
  | T_string s -> T_String (ghost_string s)
  | T_arg s -> T_Arg (w (Some ghost_quote, CST.Var (ghost_ident s)))
  | T_var t -> decompile_tvar t
  | T_var_esc t -> decompile_tvar_esc t
  | T_fun (t1, t2) -> T_Fun (w (p t1, ghost_arrow, p ~arrow_rhs:true t2))
  | T_prod (first, rest) ->
    (match Utils.list_to_sepseq rest ghost_times with
    | Some nsepseq -> T_Cart (w (first, ghost_times, nsepseq))
    | None -> failwith "Decompiler: got a T_prod with only one element")
  | T_sum { fields; layout = _ } ->
    (* XXX those are not initial, but backwards nanopass T_sum -> T_sum_row and
       T_record -> T_record_raw is not implemented, so we need to handle those here*)
    (* TODO #1758 support nullary constructors *)
    let pairs =
      match Utils.list_to_sepseq (Label.Map.to_alist fields) ghost_vbar with
      | None -> failwith "Decompiler: got a T_sum with no elements"
      | Some nsepseq ->
        Utils.nsepseq_map
          (fun (Label.Label constr, t) ->
            w
              CST.
                { ctor = ghost_ident constr
                ; ctor_args = Some (ghost_of, p t)
                ; attributes = []
                })
          nsepseq
    in
    T_Variant (w CST.{ lead_vbar = Some ghost_vbar; variants = pairs })
  | T_record { fields; layout = _ } ->
    (* XXX again not-initial node workaround *)
    let pairs =
      Utils.sepseq_map (fun (Label.Label field, t) ->
          w
            CST.
              { field_name = Var (ghost_ident field)
              ; field_type = Some (ghost_colon, t)
              ; attributes = []
              })
      @@ Utils.list_to_sepseq (Label.Map.to_alist fields) ghost_semi
    in
    T_Record (w CST.{ lbrace = ghost_lbrace; inside = pairs; rbrace = ghost_rbrace })
  | T_app { constr; type_args } ->
    let constr_arg =
      match type_args with
      | t, [] -> CST.TC_Single (p t)
      | _ ->
        CST.TC_Tuple
          (w
             CST.
               { lpar = ghost_lpar
               ; inside = Utils.nsepseq_of_nseq ~sep:ghost_comma type_args
               ; rpar = ghost_rpar
               })
    in
    T_App (w (constr, constr_arg))
  | T_record_raw row ->
    let pairs =
      Utils.list_to_sepseq
        (List.map
           ~f:(fun (Label.Label field, { associated_type; attributes; decl_pos = _ }) ->
             w
               CST.
                 { field_name = Var (ghost_ident field)
                 ; field_type = Option.map ~f:(fun t -> ghost_colon, t) associated_type
                 ; attributes = List.map ~f:decompile_attr attributes
                 })
           row)
        ghost_semi
    in
    T_Record (w CST.{ lbrace = ghost_lbrace; inside = pairs; rbrace = ghost_rbrace })
  | T_sum_raw row ->
    let pairs =
      Utils.list_to_sepseq
        (List.map
           ~f:(fun (Label.Label constr, { associated_type; attributes; decl_pos = _ }) ->
             w
               CST.
                 { ctor = ghost_ident constr
                 ; ctor_args = Option.map ~f:(fun t -> ghost_of, p t) associated_type
                 ; attributes = List.map ~f:decompile_attr attributes
                 })
           row)
        ghost_vbar
    in
    T_Variant
      (w
         CST.
           { lead_vbar = Some ghost_vbar
           ; variants =
               (match pairs with
               | None -> failwith "Decompiler: got a T_sum_raw with no elements"
               | Some nsepseq -> nsepseq)
           })
  | T_module_open_in { module_path; field; field_as_open } ->
    T_ModPath
      (w
      @@ decompile_mod_path
           { module_path = module_path, []; field = parens field; field_as_open })
    (* ^ XXX Should not module path be `Ne` from the beginning here? *)
  | T_module_access { module_path; field; field_as_open } ->
    T_ModPath
      (w
      @@ decompile_mod_path { module_path; field = decompile_tvar field; field_as_open })
  | T_disc_union _ -> failwith "Decompiler: disc unions should appear only in JsLIGO"
  | T_named_fun _ -> failwith "Decompiler: Named arguments should appear only in JsLIGO"
  | T_contract_parameter x ->
    let lst = Utils.nsepseq_of_nseq (List.Ne.map decompile_mvar x) ~sep:ghost_dot in
    T_ParameterOf (w lst)
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
