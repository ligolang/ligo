open Core
open Cst_shared.Fold
open CST
open Region

type 'a fold_control = 'a Cst_shared.Fold.fold_control

type _ sing =
    S_annot_expr : annot_expr sing
  | S_arguments : arguments sing
  | S_arith_expr : arith_expr sing
  | S_array_item : array_item sing
  | S_array_item_rest : array_item_rest sing
  | S_array_pattern : array_pattern sing
  | S_arrow : arrow sing
  | S_assign_pattern : assign_pattern sing
  | S_assignment_operator : assignment_operator sing
  | S_attr : Attr.t sing
  | S_attribute : attribute sing
  | S_bin_op : 'a sing -> 'a bin_op sing
  | S_body : body sing
  | S_bool_and : bool_and sing
  | S_bool_expr : bool_expr sing
  | S_bool_or : bool_or sing
  | S_braces : 'a sing -> 'a braces sing
  | S_brackets : 'a sing -> 'a brackets sing
  | S_cartesian : cartesian sing
  | S_case : case sing
  | S_chevrons : 'a sing -> 'a chevrons sing
  | S_code_inj : code_inj sing
  | S_colon : colon sing
  | S_comma : comma sing
  | S_comp_expr : comp_expr sing
  | S_compound : compound sing
  | S_cond_statement : cond_statement sing
  | S_const_decl : const_decl sing
  | S_constr : constr sing
  | S_cst : CST.t sing
  | S_decrement : decrement sing
  | S_destruct : destruct sing
  | S_directive : Directive.t sing
  | S_dot : dot sing
  | S_ellipsis : ellipsis sing
  | S_eof : eof sing
  | S_equal : equal sing
  | S_equal_cmp : equal_cmp sing
  | S_expr : expr sing
  | S_field_decl : field_decl sing
  | S_field_name : field_name sing
  | S_for_of : for_of sing
  | S_for_stmt : for_stmt sing
  | S_fun_expr : fun_expr sing
  | S_fun_name : fun_name sing
  | S_fun_type_arg : fun_type_arg sing
  | S_fun_type_args : fun_type_args sing
  | S_geq : geq sing
  | S_gt : gt sing
  | S_hex : Hex.t sing
  | S_import : import sing
  | S_increment : increment sing
  | S_index_kind : index_kind sing
  | S_interface_annotation : interface_annotation sing
  | S_interface_expr : interface_expr sing
  | S_interface_body : interface_body sing
  | S_kwd_as : kwd_as sing
  | S_kwd_break : kwd_break sing
  | S_kwd_case : kwd_case sing
  | S_kwd_const : kwd_const sing
  | S_kwd_default : kwd_default sing
  | S_kwd_else : kwd_else sing
  | S_kwd_export : kwd_export sing
  | S_kwd_for : kwd_for sing
  | S_kwd_from : kwd_from sing
  | S_kwd_if : kwd_if sing
  | S_kwd_import : kwd_import sing
  | S_kwd_let : kwd_let sing
  | S_kwd_implements : kwd_implements sing
  | S_kwd_interface : kwd_interface sing
  | S_kwd_namespace : kwd_namespace sing
  | S_kwd_of : kwd_of sing
  | S_kwd_return : kwd_return sing
  | S_kwd_switch : kwd_switch sing
  | S_kwd_type : kwd_type sing
  | S_kwd_while : kwd_while sing
  | S_language : language sing
  | S_lbrace : lbrace sing
  | S_lbracket : lbracket sing
  | S_leq : leq sing
  | S_let_decl : let_decl sing
  | S_lexeme : lexeme sing
  | S_list : 'a sing -> 'a list sing
  | S_logic_expr : logic_expr sing
  | S_lpar : lpar sing
  | S_lt : lt sing
  | S_minus : minus sing
  | S_module_access : 'a sing -> 'a module_access sing
  | S_module_name : module_name sing
  | S_modulo : modulo sing
  | S_interface_statement : interface_statement sing
  | S_namespace_statement : namespace_statement sing
  | S_ne_injection : 'a sing -> 'a ne_injection sing
  | S_negate : negate sing
  | S_neq : neq sing
  | S_nsepseq : 'a sing * 'b sing -> ('a, 'b) Utils.nsepseq sing
  | S_nseq : 'a sing -> 'a Utils.nseq sing
  | S_obj_type : obj_type sing
  | S_object_expr : object_expr sing
  | S_object_pattern : object_pattern sing
  | S_operator : operator sing
  | S_option : 'a sing -> 'a option sing
  | S_par : 'a sing -> 'a par sing
  | S_pattern : pattern sing
  | S_plus : plus sing
  | S_prefix_postfix_op : prefix_postfix_op sing
  | S_projection : projection sing
  | S_property : property sing
  | S_property_rest : property_rest sing
  | S_property2 : property2 sing
  | S_qmark : qmark sing
  | S_rbrace : rbrace sing
  | S_rbracket : rbracket sing
  | S_reg : 'a sing -> 'a reg sing
  | S_region : region sing
  | S_rest_pattern : rest_pattern sing
  | S_return : return sing
  | S_rpar : rpar sing
  | S_selection : selection sing
  | S_selection_field_name : selection_field_name sing
  | S_semi : semi sing
  | S_sepseq : 'a sing * 'b sing -> ('a, 'b) Utils.sepseq sing
  | S_slash : slash sing
  | S_statement : statement sing
  | S_statements : statements sing
  | S_interface_entry : interface_entry sing
  | S_interface_entries : interface_entries sing
  | S_string : string sing
  | S_string_expr : string_expr sing
  | S_sum_type : sum_type sing
  | S_switch : switch sing
  | S_switch_case : switch_case sing
  | S_switch_default_case : switch_default_case sing
  | S_ternary : ternary sing
  | S_the_unit : the_unit sing
  | S_times : times sing
  | S_toplevel_statement : toplevel_statement sing
  | S_toplevel_statements : toplevel_statements sing
  | S_tuple_2 : 'a sing * 'b sing -> ('a * 'b) sing
  | S_tuple_3 : 'a sing * 'b sing * 'c sing -> ('a * 'b * 'c) sing
  | S_tuple_4 :
    'a sing * 'b sing * 'c sing * 'd sing -> ('a * 'b * 'c * 'd) sing
  | S_tuple_5 :
    'a sing * 'b sing * 'c sing * 'd sing * 'e sing -> ('a * 'b * 'c * 'd * 'e) sing
  | S_type_constr : type_constr sing
  | S_type_decl : type_decl sing
  | S_type_expr : type_expr sing
  | S_type_generics : type_generics sing
  | S_type_name : type_name sing
  | S_type_params : type_params sing
  | S_type_var : type_var sing
  | S_type_vars : type_vars sing
  | S_update_type : update_type sing
  | S_un_op : 'a sing -> 'a un_op sing
  | S_val_binding : val_binding sing
  | S_var_pattern : var_pattern sing
  | S_variable : variable sing
  | S_variant : variant sing
  | S_variant_comp : variant_comp sing
  | S_vbar : vbar sing
  | S_while_stmt : while_stmt sing
  | S_wild : wild sing
  | S_wrap : 'a sing -> 'a wrap sing
  | S_z : Z.t sing

type some_node = Some_node : 'b * 'b sing -> some_node
let (-|) a b = Some_node (a, b)

let fold
    (type a b)
    (init : b)
    (f : b -> a -> b)
    (instruction : some_node -> a fold_control)
    (cst : CST.t) : b =
  let acc = ref init in
  let rec process : some_node -> unit =
    fun some_node ->
      match instruction some_node with
        Stop -> ()
      | Skip -> fold some_node
      | Continue x -> acc := f !acc x; fold some_node
      | Last x -> acc := f !acc x

  and process_list : some_node list -> unit =
    fun l -> List.iter l ~f:process

  and fold : some_node -> unit =
  function (Some_node (node, sing)) -> match sing with
    S_annot_expr ->
    process @@ node -| S_tuple_3 (S_expr, S_kwd_as, S_type_expr)
  | S_arguments -> process
    (match node with
      Multiple node -> node -| S_reg (S_par (S_nsepseq (S_expr, S_comma)))
    | Unit node -> node -| S_reg S_the_unit)
  | S_arith_expr -> process
    (match node with
      Add node -> node -| S_reg (S_bin_op S_plus)
    | Sub node -> node -| S_reg (S_bin_op S_minus)
    | Mult node -> node -| S_reg (S_bin_op S_times)
    | Div node -> node -| S_reg (S_bin_op S_slash)
    | Mod node -> node -| S_reg (S_bin_op S_modulo)
    | Neg node -> node -| S_reg (S_un_op S_minus)
    | Int node -> node -| S_wrap (S_tuple_2 (S_lexeme, S_z)))
  | S_array_item -> process
    (match node with
      Expr_entry node -> node -| S_expr
    | Rest_entry node -> node -| S_reg S_array_item_rest)
  | S_array_item_rest ->
    let { ellipsis; expr } : array_item_rest = node in
    process_list
    [ ellipsis -| S_ellipsis
    ; expr -| S_expr ]
  | S_array_pattern ->
    process @@ node -| S_reg (S_brackets (S_nsepseq (S_pattern, S_comma)))
  | S_arrow -> process @@ node -| S_wrap S_lexeme
  | S_assign_pattern ->
    let { property; eq; value } = node in
    process_list
    [ property -| S_variable
    ; eq -| S_equal
    ; value -| S_expr ]
  | S_assignment_operator ->
    (match node with
      Times_eq -> () (* Leaf *)
    | Div_eq -> () (* Leaf *)
    | Min_eq -> () (* Leaf *)
    | Plus_eq -> ()(* Leaf *)
    | Mod_eq -> ()) (* Leaf *)
  | S_attr -> () (* Leaf *)
  | S_attribute -> process @@ node -| S_wrap S_attr
  | S_bin_op sing ->
    let { op; arg1; arg2 } = node in
    process_list
    [ op -| sing
    ; arg1 -| S_expr
    ; arg2 -| S_expr ]
  | S_body -> process
    (match node with
      FunctionBody node -> node -| S_reg (S_braces S_statements)
    | ExpressionBody node -> node -| S_expr)
  | S_bool_and -> process @@ node -| S_wrap S_lexeme
  | S_bool_expr -> process
    (match node with
      Or node -> node -| S_reg (S_bin_op S_bool_or)
    | And node -> node -| S_reg (S_bin_op S_bool_and)
    | Not node -> node -| S_reg (S_un_op S_negate))
  | S_bool_or -> process @@ node -| S_wrap S_lexeme
  | S_braces sing ->
    let { lbrace; inside; rbrace } = node in
    process_list
    [ lbrace -| S_lbrace
    ; inside -| sing
    ; rbrace -| S_rbrace ]
  | S_brackets sing ->
    let { lbracket; inside; rbracket } = node in
    process_list
    [ lbracket -| S_lbracket
    ; inside -| sing
    ; rbracket -| S_rbracket ]
  | S_cartesian ->
    let { inside; attributes } = node in
    process_list
    [ inside -| S_reg (S_brackets (S_nsepseq (S_type_expr, S_comma)))
    ; attributes -| S_list S_attribute ]
  | S_case -> process
    (match node with
      Switch_case node -> node -| S_switch_case
    | Switch_default_case node -> node -| S_switch_default_case)
  | S_chevrons sing ->
    let { lchevron; inside; rchevron } = node in
    process_list
    [ lchevron -| S_lt
    ; inside -| sing
    ; rchevron -| S_gt ]
  | S_code_inj ->
    let { language; code } = node in
    process_list
    [ language -| S_language
    ; code -| S_expr ]
  | S_colon -> process @@ node -| S_wrap S_lexeme
  | S_comma -> process @@ node -| S_wrap S_lexeme
  | S_comp_expr -> process
    (match node with
      Lt node -> node -| S_reg (S_bin_op S_lt)
    | Leq node -> node -| S_reg (S_bin_op S_leq)
    | Gt node -> node -| S_reg (S_bin_op S_gt)
    | Geq node -> node -| S_reg (S_bin_op S_geq)
    | Equal node -> node -| S_reg (S_bin_op S_equal_cmp)
    | Neq node -> node -| S_reg (S_bin_op S_neq))
  | S_compound -> process_list
    (match node with
      Braces (node_1, node_2) ->
      [ node_1 -| S_lbrace
      ; node_2 -| S_rbrace ]
    | Brackets (node_1, node_2) ->
      [ node_1 -| S_lbracket
      ; node_2 -| S_rbracket ])
  | S_cond_statement ->
    let { attributes; kwd_if; test; ifso; ifnot } = node in
    process_list
    [ attributes -| S_list S_attribute
    ; kwd_if -| S_kwd_if
    ; test -| S_par S_expr
    ; ifso -| S_statement
    ; ifnot -| S_option (S_tuple_2 (S_kwd_else, S_statement)) ]
  | S_const_decl ->
    let { attributes; kwd_const; bindings } = node in
    process_list
    [ attributes -| S_list S_attribute
    ; kwd_const -| S_kwd_const
    ; bindings -| S_nsepseq (S_reg S_val_binding, S_comma) ]
  | S_constr -> process @@ node -| S_wrap S_lexeme
  | S_cst ->
    let { statements; eof } = node in
    process_list
    [ statements -| S_toplevel_statements
    ; eof -| S_eof ]
  | S_destruct ->
    let { property; colon; target } = node in
    process_list
    [ property -| S_variable
    ; colon -| S_colon
    ; target -| S_reg S_val_binding ]
  | S_decrement -> process @@ node -| S_wrap S_lexeme
  | S_directive -> () (* Leaf *)
  | S_dot -> process @@ node -| S_wrap S_lexeme
  | S_ellipsis -> process @@ node -| S_wrap S_lexeme
  | S_eof -> process @@ node -| S_wrap S_lexeme
  | S_equal -> process @@ node -| S_wrap S_lexeme
  | S_equal_cmp -> process @@ node -| S_wrap S_lexeme
  | S_expr -> process
    (match node with
      EFun node -> node -| S_reg S_fun_expr
    | EPar node -> node -| S_reg (S_par S_expr)
    | ESeq node -> node -| S_reg (S_nsepseq (S_expr, S_comma))
    | EVar node -> node -| S_variable
    | EModA node -> node -| S_reg (S_module_access S_expr)
    | ELogic node -> node -| S_logic_expr
    | EArith node -> node -| S_arith_expr
    | ECall node -> node -| S_reg (S_tuple_2 (S_expr, S_arguments))
    | EBytes node -> node -| S_wrap (S_tuple_2 (S_lexeme, S_hex))
    | EArray node ->
      node -| S_reg (S_brackets (S_sepseq (S_array_item, S_comma)))
    | EObject node -> node -| S_object_expr
    | EString node -> node -| S_string_expr
    | EProj node -> node -| S_reg S_projection
    | EAssign node -> node -| S_tuple_3 (S_expr, S_reg S_operator, S_expr)
    | EConstr node -> node -| S_reg (S_tuple_2 (S_constr, S_option S_expr))
    | EAnnot node -> node -| S_reg S_annot_expr
    | EUnit node -> node -| S_reg S_the_unit
    | ECodeInj node -> node -| S_reg S_code_inj
    | ETernary node -> node -| S_reg S_ternary
    | EContract node -> node -| S_reg (S_nsepseq (S_module_name, S_dot))
    | EPrefix node -> node -| S_reg S_prefix_postfix_op
    | EPostfix node -> node -| S_reg S_prefix_postfix_op)
  | S_field_decl ->
    let { field_name; colon; field_type; attributes } = node in
    process_list
    [ field_name -| S_field_name
    ; colon -| S_colon
    ; field_type -| S_type_expr
    ; attributes -| S_list S_attribute ]
  | S_field_name -> process @@ node -| S_wrap S_lexeme
  | S_for_of ->
    let { attributes
        ; kwd_for
        ; lpar; index
        ; index_kind
        ; kwd_of
        ; expr
        ; rpar
        ; statement } = node in
    process_list
    [ attributes -| S_list S_attribute
    ; kwd_for -| S_kwd_for
    ; lpar -| S_lpar
    ; index -| S_variable
    ; index_kind -| S_index_kind
    ; kwd_of -| S_kwd_of
    ; expr -| S_expr
    ; rpar -| S_rpar
    ; statement -| S_statement ]
  | S_for_stmt ->
    let { attributes   ;
    kwd_for      ;
    lpar         ;
    initialiser ;
    semi1        ;
    condition    ;
    semi2        ;
    afterthought ;
    rpar         ;
    statement     } = node in
    process_list
    [ attributes -| S_list S_attribute
    ; kwd_for -| S_kwd_for
    ; lpar -| S_lpar
    ; initialiser -| S_option S_statement
    ; semi1 -| S_semi
    ; condition -| S_option S_expr
    ; semi2 -| S_semi
    ; afterthought -| S_option (S_nsepseq (S_expr, S_comma))
    ; rpar -| S_rpar
    ; statement -| S_option S_statement ]
  | S_fun_expr ->
    let { type_params; parameters; lhs_type; arrow; body } = node in
    process_list
    [ type_params -| S_option S_type_generics
    ; parameters -| S_expr
    ; lhs_type -| S_option (S_tuple_2 (S_colon, S_type_expr))
    ; arrow -| S_arrow
    ; body -| S_body ]
  | S_fun_name -> process @@ node -| S_wrap S_lexeme
  | S_fun_type_arg ->
    let { name; colon; type_expr } = node in
    process_list
    [ name -| S_variable
    ; colon -| S_colon
    ; type_expr -| S_type_expr ]
  | S_fun_type_args
    -> process @@ node -| S_par (S_nsepseq (S_fun_type_arg, S_comma))
  | S_geq -> process @@ node -| S_wrap S_lexeme
  | S_gt -> process @@ node -| S_wrap S_lexeme
  | S_hex -> ()  (* Leaf *)
  | S_import -> process_list
    (match node with
      Import_rename { kwd_import; alias; equal; module_path } ->
      [ kwd_import -| S_kwd_import
      ; alias -| S_module_name
      ; equal -| S_equal
      ; module_path -| S_nsepseq (S_module_name, S_dot) ]
    | Import_all_as
      { kwd_import; times; kwd_as; alias; kwd_from; module_path } ->
      [ kwd_import -| S_kwd_import
      ; times -| S_times
      ; kwd_as -| S_kwd_as
      ; alias -| S_module_name
      ; kwd_from -| S_kwd_from
      ; module_path -| S_wrap S_string ]
    | Import_selected { kwd_import; imported; kwd_from; module_path } ->
      [ kwd_import -| S_kwd_import
      ; imported -| S_reg (S_braces (S_nsepseq (S_field_name, S_comma)))
      ; kwd_from -| S_kwd_from
      ; module_path -| S_wrap S_string ])
  | S_increment -> process @@ node -| S_wrap S_lexeme
  | S_index_kind -> process
    (match node with
      `Let node -> node -| S_kwd_let
    | `Const node -> node -| S_kwd_const)
  | S_kwd_as -> process @@ node -| S_wrap S_lexeme
  | S_kwd_break -> process @@ node -| S_wrap S_lexeme
  | S_kwd_case -> process @@ node -| S_wrap S_lexeme
  | S_kwd_const -> process @@ node -| S_wrap S_lexeme
  | S_kwd_default -> process @@ node -| S_wrap S_lexeme
  | S_kwd_else -> process @@ node -| S_wrap S_lexeme
  | S_kwd_export -> process @@ node -| S_wrap S_lexeme
  | S_kwd_for -> process @@ node -| S_wrap S_lexeme
  | S_kwd_from -> process @@ node -| S_wrap S_lexeme
  | S_kwd_if -> process @@ node -| S_wrap S_lexeme
  | S_kwd_import -> process @@ node -| S_wrap S_lexeme
  | S_kwd_let -> process @@ node -| S_wrap S_lexeme
  | S_kwd_implements -> process @@ node -| S_wrap S_lexeme
  | S_kwd_interface -> process @@ node -| S_wrap S_lexeme
  | S_kwd_namespace -> process @@ node -| S_wrap S_lexeme
  | S_kwd_of -> process @@ node -| S_wrap S_lexeme
  | S_kwd_return -> process @@ node -| S_wrap S_lexeme
  | S_kwd_switch -> process @@ node -| S_wrap S_lexeme
  | S_kwd_type -> process @@ node -| S_wrap S_lexeme
  | S_kwd_while -> process @@ node -| S_wrap S_lexeme
  | S_language -> process @@ node -| S_wrap S_lexeme
  | S_lbrace -> process @@ node -| S_wrap S_lexeme
  | S_lbracket -> process @@ node -| S_wrap S_lexeme
  | S_leq -> process @@ node -| S_wrap S_lexeme
  | S_let_decl ->
    let { attributes; kwd_let; bindings } = node in
    process_list
    [ attributes -| S_list S_attribute
    ; kwd_let -| S_kwd_let
    ; bindings -| S_nsepseq (S_reg S_val_binding, S_comma) ]
  | S_lexeme -> () (* Leaf *)
  | S_list sing -> process_list @@ List.map ~f:(fun x -> x -| sing) node
  | S_logic_expr -> process
    (match node with
      BoolExpr node -> node -| S_bool_expr
    | CompExpr node -> node -| S_comp_expr)
  | S_lpar -> process @@ node -| S_wrap S_lexeme
  | S_lt -> process @@ node -| S_wrap S_lexeme
  | S_minus -> process @@ node -| S_wrap S_lexeme
  | S_module_access sing ->
    let { module_name; selector; field } = node in
    process_list
    [ module_name -| S_module_name
    ; selector -| S_dot
    ; field -| sing ]
  | S_module_name -> process @@ node -| S_wrap S_lexeme
  | S_modulo -> process @@ node -| S_wrap S_lexeme
  | S_interface_statement ->
    process @@ node -| S_tuple_4 (S_kwd_interface
                                 , S_module_name
                                 , S_interface_body
                                 , S_list S_attribute)
  | S_interface_annotation -> process @@ node -| S_reg (S_tuple_2 (S_kwd_implements
                                                                  , S_interface_expr))
  | S_interface_expr ->
    process @@ (match node with
      IInterface node -> node -| S_interface_body
        | IPath node -> node -| S_reg (S_nsepseq (S_module_name, S_dot)))
  | S_interface_body -> process @@ node -| S_reg (S_braces S_interface_entries)
  | S_namespace_statement ->
    process @@ node -| S_tuple_5 (S_kwd_namespace
                                 , S_module_name
                                 , S_option S_interface_annotation
                                 , S_reg (S_braces S_statements)
                                 , S_list S_attribute)
  | S_ne_injection sing ->
    let { compound; ne_elements; terminator; attributes } = node in
    process_list
    [ compound -| S_option S_compound
    ; ne_elements -| S_nsepseq (sing, S_semi)
    ; terminator -| S_option S_semi
    ; attributes -| S_list S_attribute ]
  | S_negate -> process @@ node -| S_wrap S_lexeme
  | S_neq -> process @@ node -| S_wrap S_lexeme
  | S_nsepseq (sing_1, sing_2) ->
    process @@ node -| S_tuple_2 (sing_1, S_list (S_tuple_2 (sing_2, sing_1)))
  | S_nseq sing -> process @@ node -| S_tuple_2 (sing, S_list sing)
  | S_obj_type
    -> process @@ node -| S_reg (S_ne_injection (S_reg S_field_decl))
  | S_object_expr ->
    process @@ node -| S_reg (S_braces (S_nsepseq (S_property, S_comma)))
  | S_object_pattern ->
    process @@ node -| S_reg (S_braces (S_nsepseq (S_pattern, S_comma)))
  | S_operator ->
    (match node with
      Eq -> () (* Leaf *)
    | Assignment_operator node -> process @@ node -| S_assignment_operator)
  | S_option sing ->
    (match node with
      None -> () (* Leaf *)
    | Some node -> process @@ node -| sing)
  | S_par sing ->
    let { lpar; inside; rpar } = node in
    process_list
    [ lpar -| S_lpar
    ; inside -| sing
    ; rpar -| S_rpar ]
  | S_pattern -> process
    (match node with
      PRest node -> node -| S_reg S_rest_pattern
    | PAssign node -> node -| S_reg S_assign_pattern
    | PVar node -> node -| S_reg S_var_pattern
    | PConstr node -> node -| S_variable
    | PDestruct node -> node -| S_reg S_destruct
    | PObject node -> node -| S_object_pattern
    | PArray node -> node -| S_array_pattern)
  | S_plus -> process @@ node -| S_wrap S_lexeme
  | S_prefix_postfix_op ->
    let { update_type; variable } = node in
    process_list
    [ update_type -| S_update_type
    ; variable -| S_variable]
  | S_projection ->
    let { expr; selection } = node in
    process_list
    [ expr -| S_expr
    ; selection -| S_selection ]
  | S_property -> process
    (match node with
      Punned_property node -> node -| S_reg S_expr
    | Property node -> node -| S_reg S_property2
    | Property_rest node -> node -| S_reg S_property_rest)
  | S_property_rest ->
    let { ellipsis; expr } : property_rest = node in
    process_list
    [ ellipsis -| S_ellipsis
    ; expr -| S_expr ]
  | S_property2 ->
    let { name; colon; value; attributes } = node in
    process_list
    [ name -| S_expr
    ; colon -| S_colon
    ; value -| S_expr
    ; attributes -| S_list S_attribute ]
  | S_qmark -> process @@ node -| S_wrap S_lexeme
  | S_rbrace -> process @@ node -| S_wrap S_lexeme
  | S_rbracket -> process @@ node -| S_wrap S_lexeme
  | S_reg sing ->
    let { region; value } = node in
    process_list
    [ region -| S_region
    ; value -| sing ]
  | S_region -> () (* Leaf *)
  | S_rest_pattern ->
    let { ellipsis; rest } = node in
    process_list
    [ ellipsis -| S_ellipsis
    ; rest -| S_variable ]
  | S_return ->
    let { kwd_return; expr } = node in
    process_list
    [ kwd_return -| S_kwd_return
    ; expr -| S_option S_expr ]
  | S_rpar -> process @@ node -| S_wrap S_lexeme
  | S_selection -> process
    (match node with
      FieldName node -> node -| S_reg S_selection_field_name
    | Component node -> node -| S_reg (S_brackets S_expr))
  | S_selection_field_name ->
    let { dot; value } = node in
    process_list
    [ dot -| S_dot
    ; value -| S_variable ]
  | S_semi -> process @@ node -| S_wrap S_lexeme
  | S_sepseq (sing_1, sing_2) ->
    process @@ node -| S_option (S_nsepseq (sing_1, sing_2))
  | S_slash -> process @@ node -| S_wrap S_lexeme
  | S_statement -> process
    (match node with
      SBlock node -> node -| S_reg (S_braces S_statements)
    | SExpr node -> node -| S_tuple_2 (S_list S_attribute, S_expr)
    | SCond node -> node -| S_reg S_cond_statement
    | SReturn node -> node -| S_reg S_return
    | SLet node -> node -| S_reg S_let_decl
    | SConst node -> node -| S_reg S_const_decl
    | SType node -> node -| S_reg S_type_decl
    | SSwitch node -> node -| S_reg S_switch
    | SBreak node -> node -| S_kwd_break
    | SInterface node -> node -| S_reg S_interface_statement
    | SNamespace node -> node -| S_reg S_namespace_statement
    | SExport node -> node -| S_reg (S_tuple_2 (S_kwd_export, S_statement))
    | SImport node -> node -| S_reg S_import
    | SWhile node -> node -| S_reg S_while_stmt
    | SForOf node -> node -| S_reg S_for_of
    | SFor node -> node -| S_reg S_for_stmt)
  | S_statements -> process @@ node -| S_nsepseq (S_statement, S_semi)
  | S_interface_entry -> process
    (match node with
        IType node -> node -| S_reg (S_tuple_5 (S_list S_attribute, S_kwd_type, S_variable, S_equal, S_type_expr))
      | IType_var node -> node -| S_reg (S_tuple_3 (S_list S_attribute, S_kwd_type, S_variable))
      | IConst node -> node -| S_reg (S_tuple_5 (S_list S_attribute, S_kwd_const, S_variable, S_colon, S_type_expr)))
  | S_interface_entries -> process @@ node -| S_nsepseq (S_interface_entry, S_semi)
  | S_string -> () (* Leaf *)
  | S_string_expr -> process
    (match node with
      String node -> node -| S_wrap S_lexeme
    | Verbatim node -> node -| S_wrap S_lexeme)
  | S_sum_type ->
    let { leading_vbar; variants; attributes } = node in
    process_list
    [ leading_vbar -| S_option S_vbar
    ; variants -| S_reg (S_nsepseq (S_reg S_variant, S_vbar))
    ; attributes -| S_list S_attribute ]
  | S_switch ->
    let { kwd_switch; lpar; expr; rpar; lbrace; cases; rbrace } = node in
    process_list
    [ kwd_switch -| S_kwd_switch
    ; lpar -| S_lpar
    ; expr -| S_expr
    ; rpar -| S_rpar
    ; lbrace -| S_lbrace
    ; cases -| S_nseq S_case
    ; rbrace -| S_rbrace ]
  | S_switch_case ->
    let { kwd_case; expr; colon; statements } = node in
    process_list
    [ kwd_case -| S_kwd_case
    ; expr -| S_expr
    ; colon  -| S_colon
    ; statements -| S_option S_statements ]
  | S_switch_default_case ->
    let { kwd_default; colon; statements } = node in
    process_list
    [ kwd_default -| S_kwd_default
    ; colon  -| S_colon
    ; statements -| S_option S_statements ]
  | S_ternary ->
    let { condition; qmark; truthy; colon; falsy } = node in
    process_list
    [ condition -| S_expr
    ; qmark -| S_qmark
    ; truthy -| S_expr
    ; colon -| S_colon
    ; falsy -| S_expr ]
  | S_the_unit -> process @@ node -| S_tuple_2 (S_lpar, S_rpar)
  | S_times -> process @@ node -| S_wrap S_lexeme
  | S_toplevel_statement -> process_list
    (match node with
      TopLevel (node_1, node_2) ->
      [node_1 -| S_statement; node_2 -| S_option S_semi ]
    | Directive node -> [node -| S_directive ])
  | S_toplevel_statements -> process @@ node -| S_nseq S_toplevel_statement
  | S_tuple_2 (sing_1, sing_2) ->
    (match node with
      (node_1, node_2) -> process_list
      [ node_1 -| sing_1
      ; node_2 -| sing_2 ])
  | S_tuple_3 (sing_1, sing_2, sing_3) ->
    (match node with
      (node_1, node_2, node_3) -> process_list
      [ node_1 -| sing_1
      ; node_2 -| sing_2
      ; node_3 -| sing_3 ])
  | S_tuple_4 (sing_1, sing_2, sing_3, sing_4) ->
    (match node with
      (node_1, node_2, node_3, node_4) -> process_list
      [ node_1 -| sing_1
      ; node_2 -| sing_2
      ; node_3 -| sing_3
      ; node_4 -| sing_4 ])
  | S_tuple_5 (sing_1, sing_2, sing_3, sing_4, sing_5) ->
    (match node with
      (node_1, node_2, node_3, node_4, node_5) -> process_list
      [ node_1 -| sing_1
      ; node_2 -| sing_2
      ; node_3 -| sing_3
      ; node_4 -| sing_4
      ; node_5 -| sing_5 ])
  | S_type_constr -> process @@ node -| S_wrap S_lexeme
  | S_type_decl ->
    let { attributes; kwd_type; name; params; eq; type_expr } = node in
    process_list
    [ attributes -| S_list S_attribute
    ; kwd_type -| S_kwd_type
    ; name -| S_type_name
    ; params -| S_option S_type_vars
    ; eq -| S_equal
    ; type_expr -| S_type_expr ]
  | S_type_expr -> process
    (match node with
      TProd node -> node -| S_cartesian
    | TSum node -> node -| S_reg S_sum_type
    | TObject node -> node -| S_obj_type
    | TApp node -> node -| S_reg (S_tuple_2 (S_type_constr, S_type_params))
    | TFun node ->
      node -| S_reg (S_tuple_3 (S_fun_type_args, S_arrow, S_type_expr))
    | TPar node -> node -| S_reg (S_par S_type_expr)
    | TVar node -> node -| S_variable
    | TString node -> node -| S_wrap S_lexeme
    | TInt node -> node -| S_wrap (S_tuple_2 (S_lexeme, S_z))
    | TModA node -> node -| S_reg (S_module_access S_type_expr)
    | TDisc node -> node -| S_nsepseq (S_obj_type, S_vbar)
    | TParameter node -> node -| S_reg (S_nsepseq (S_module_name, S_dot)))
  | S_type_generics
    -> process @@ node -| S_reg (S_chevrons (S_nsepseq (S_variable, S_comma)))
  | S_type_name -> process @@ node -| S_wrap S_lexeme
  | S_type_params ->
    process @@ node -| S_reg (S_chevrons (S_nsepseq (S_type_expr, S_comma)))
  | S_type_var -> process @@ node -| S_wrap S_lexeme
  | S_type_vars
    -> process @@ node -| S_reg (S_chevrons (S_nsepseq (S_type_var, S_comma)))
  | S_un_op sing ->
    let { op; arg } = node in
    process_list
    [ op -| sing
    ; arg -| S_expr ]
  | S_update_type -> process
    (match node with
      Increment node -> node -| S_increment
    | Decrement node -> node -| S_decrement)
  | S_val_binding ->
    let { binders; type_params; lhs_type; eq; expr } = node in
    process_list
    [ binders -| S_pattern
    ; type_params -| S_option S_type_generics
    ; lhs_type -| S_option (S_tuple_2 (S_colon, S_type_expr))
    ; eq -| S_equal
    ;expr -| S_expr ]
  | S_var_pattern ->
    let { variable; attributes } = node in
    process_list
    [ variable -| S_variable
    ; attributes -| S_list S_attribute ]
  | S_variable -> process @@ node -| S_wrap S_lexeme
  | S_variant ->
    let { tuple; attributes } = node in
    process_list
    [ tuple -| S_reg (S_brackets S_variant_comp)
    ; attributes -| S_list S_attribute ]
  | S_variant_comp ->
    let { constr; params } = node in
    process_list
    [ constr -| S_constr
    ; params -|
      S_option (S_tuple_2 (S_comma, S_nsepseq (S_type_expr, S_comma))) ]
  | S_vbar -> process @@ node -| S_wrap S_lexeme
  | S_while_stmt ->
    let { kwd_while; lpar; expr; rpar; statement } = node in
    process_list
    [ kwd_while -| S_kwd_while
    ; lpar -| S_lpar
    ; expr -| S_expr
    ; rpar -| S_rpar
    ; statement -| S_statement ]
  | S_wild -> process @@ node -| S_wrap S_lexeme
  | S_wrap sing -> process_list
    [ node#payload -| sing
    ; node#attributes -| S_list (S_reg S_attr)
    ; node#region -| S_region
    ; node#directives -| S_list S_directive ]
  | S_z -> () (* Leaf *)
  in
  process @@ cst -| S_cst;
  !acc

let fold_map
  (type a)
  (m : a monoid)
  (f : some_node -> a fold_control)
  (cst : CST.t) : a = fold m.empty m.append f cst
