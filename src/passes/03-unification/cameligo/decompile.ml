module CST = Cst.Cameligo
module AST = Ast_unified
module Helpers = Unification_shared.Helpers
open Simple_utils
module Label = Ligo_prim.Label
open Lexing_cameligo.Token

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
    }


and decompile_program p = AST.Catamorphism.cata_program ~f:folder p
and decompile_expression e = AST.Catamorphism.cata_expr ~f:folder e
and decompile_pattern p = AST.Catamorphism.cata_pattern ~f:folder p
and decompile_type_expression t = AST.Catamorphism.cata_ty_expr ~f:folder t

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
    Utils.nsepseq_of_nseq ~sep:ghost_dot
    @@ Utils.nseq_map
         Function.(ghost_ident <@ Format.asprintf "%a" Ligo_prim.Module_var.pp)
         module_path
  in
  (* XXX: What is [field_as_open]?? *)
  CST.{ module_path; selector = ghost_dot; field }


(* Decompilers: expect that all Ast nodes are initial, i.e.
   that backwards nanopasses were applied to Ast_unified before decompiler *)

and expr : (CST.expr, CST.type_expr, CST.pattern, unit, unit) AST.expression_ -> CST.expr =
 fun e ->
  let w = Region.wrap_ghost in
  match Location.unwrap e with
  | E_attr (attr, expr) -> E_Attr (decompile_attr attr, expr)
  | E_variable v -> E_Var (ghost_ident (Format.asprintf "%a" AST.Variable.pp v))
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
  | P_var v -> P_Var (ghost_ident @@ Format.asprintf "%a" AST.Variable.pp v)
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
    let lst = Utils.list_to_nsepseq_opt lst ghost_comma in
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
                     { field_lhs = ghost_ident (AST.Label.to_string l)
                     ; field_lens = ghost_eq
                     ; field_rhs = p
                     ; attributes = []
                     })
            | Punned { wrap_content = l; _ } ->
              (* XXX do we need to extract attributes here?
                              Why this CST node has a separate record for attributes at all?*)
              CST.Punned
                (w @@ CST.{ attributes = []; pun = ghost_ident (AST.Label.to_string l) }))
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
      e.g. A of int -> (A of int) option vs {a : int} -> { a : int } option.
      TODO: Currently we're using the same function for type applications and
      lhs/rhs of operators like "->", but it's better to consider operator precendance 
      / associativity. *)
    | T_Fun _ | T_Cart _ | T_Variant _ | T_Attr _ | T_Parameter _ -> true
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
  let p : CST.type_expr -> CST.type_expr =
   fun t -> if needs_parens t then parens t else t
  in
  let decompile_tvar : AST.Ty_variable.t -> CST.type_expr =
   fun t -> T_Var (ghost_ident @@ Format.asprintf "%a" Ligo_prim.Type_var.pp t)
   (* ^ XXX we should split generated names on '#'? Can we just extract the name somehow? 
      Why [t.name] is not working?? *)
  in
  match Location.unwrap te with
  | T_attr (attr, t) -> T_Attr (decompile_attr attr, t)
  | T_int (_s, z) -> T_Int (ghost_int z)
  | T_string s -> T_String (ghost_string s)
  | T_arg s -> T_Arg (w (Some ghost_quote, ghost_ident s))
  | T_var t -> decompile_tvar t
  | T_fun (t1, t2) -> T_Fun (w (p t1, ghost_arrow, p t2))
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
              { field_name = ghost_ident field
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
                 { field_name = ghost_ident field
                 ; field_type = Option.map ~f:(fun t -> ghost_colon, t) associated_type
                 ; attributes = List.map ~f:decompile_attr attributes
                 })
           row)
        ghost_semi
    in
    T_Record (w CST.{ lbrace = ghost_lbrace; inside = pairs; rbrace = ghost_rbrace })
  | T_sum_raw row ->
    let pairs =
      Utils.list_to_nsepseq_opt
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
  | T_module_app _ | T_abstraction _ | T_constant _ | T_for_all _ ->
    Helpers.failwith_not_initial_node_decompiler @@ `Ty_expr te
