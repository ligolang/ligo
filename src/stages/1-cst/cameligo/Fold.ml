open Core
open Cst_shared.Fold
open CST
open Region

type 'a fold_control = 'a Cst_shared.Fold.fold_control

type _ sing =
    S_append : append sing
  | S_arrow : arrow sing
  | S_ass : ass sing
  | S_assign : assign sing
  | S_attr : Attr.t sing
  | S_attribute : attribute sing
  | S_bin_op : 'a sing -> 'a bin_op sing
  | S_bool_and : bool_and sing
  | S_bool_or : bool_or sing
  | S_braces : 'a sing -> 'a braces sing
  | S_braces' : 'a sing -> 'a braces' sing
  | S_brackets : 'a sing -> 'a brackets sing
  | S_brackets' : 'a sing -> 'a brackets' sing
  | S_caret : caret sing
  | S_cartesian : cartesian sing
  | S_code_inj : code_inj sing
  | S_colon : colon sing
  | S_comma : comma sing
  | S_compound : compound sing
  | S_cond_expr : cond_expr sing
  | S_cons : cons sing
  | S_cst : cst sing
  | S_ctor : ctor sing
  | S_declaration : declaration sing
  | S_direction : direction sing
  | S_directive : Directive.t sing
  | S_dot : dot sing
  | S_eof : eof sing
  | S_equal : equal sing
  | S_expr : expr sing
  | S_field : 'a sing * 'b sing * 'c sing -> ('a, 'b, 'c) field sing
  | S_field_decl : field_decl sing
  | S_field_name : field_name sing
  | S_for_in_loop : for_in_loop sing
  | S_for_all : for_all sing
  | S_for_loop : for_loop sing
  | S_full_field : 'a sing * 'b sing * 'c sing -> ('a, 'b, 'c) full_field sing
  | S_fun_expr : fun_expr sing
  | S_fun_type : fun_type sing
  | S_geq : geq sing
  | S_gt : gt sing
  | S_hex : Hex.t sing
  | S_int64 : Int64.t sing
  | S_kwd_begin : kwd_begin sing
  | S_kwd_do : kwd_do sing
  | S_kwd_done : kwd_done sing
  | S_kwd_downto : kwd_downto sing
  | S_kwd_else : kwd_else sing
  | S_kwd_end : kwd_end sing
  | S_kwd_false : kwd_false sing
  | S_kwd_for : kwd_for sing
  | S_kwd_fun : kwd_fun sing
  | S_kwd_if : kwd_if sing
  | S_kwd_in : kwd_in sing
  | S_kwd_include : kwd_include sing
  | S_kwd_land : kwd_land sing
  | S_kwd_let : kwd_let sing
  | S_kwd_lor : kwd_lor sing
  | S_kwd_lsl : kwd_lsl sing
  | S_kwd_lsr : kwd_lsr sing
  | S_kwd_lxor : kwd_lxor sing
  | S_kwd_match : kwd_match sing
  | S_kwd_mod : kwd_mod sing
  | S_kwd_module : kwd_module sing
  | S_kwd_mut : kwd_mut sing
  | S_kwd_not : kwd_not sing
  | S_kwd_of : kwd_of sing
  | S_kwd_or : kwd_or sing
  | S_kwd_rec : kwd_rec sing
  | S_kwd_struct : kwd_struct sing
  | S_kwd_then : kwd_then sing
  | S_kwd_true : kwd_true sing
  | S_kwd_type : kwd_type sing
  | S_kwd_upto : kwd_upto sing
  | S_kwd_val  : kwd_val sing
  | S_kwd_while : kwd_while sing
  | S_kwd_with : kwd_with sing
  | S_language : language sing
  | S_lbrace : lbrace sing
  | S_lbracket : lbracket sing
  | S_lens : lens sing
  | S_leq : leq sing
  | S_let_binding : let_binding sing
  | S_let_decl : let_decl sing
  | S_let_in : let_in sing
  | S_let_mut_in : let_mut_in sing
  | S_lexeme : lexeme sing
  | S_list : 'a sing -> 'a list sing
  | S_list_ : 'a sing -> 'a list_ sing
  | S_loop_body : loop_body sing
  | S_lpar : lpar sing
  | S_lt : lt sing
  | S_match_clause : match_clause sing
  | S_match_expr : match_expr sing
  | S_minus : minus sing
  | S_minus_eq : minus_eq sing
  | S_module_body : module_body sing
  | S_module_decl : module_decl sing
  | S_module_include : module_include sing
  | S_module_expr : module_expr sing
  | S_module_in : module_in sing
  | S_module_name : module_name sing
  | S_module_path : 'a sing -> 'a module_path sing
  | S_neq : neq sing
  | S_nsepseq : 'a sing * 'b sing -> ('a, 'b) Utils.nsepseq sing
  | S_nseq : 'a sing -> 'a Utils.nseq sing
  | S_option : 'a sing -> 'a option sing
  | S_par : 'a sing -> 'a par sing
  | S_par' : 'a sing -> 'a par' sing
  | S_path : path sing
  | S_pattern : pattern sing
  | S_plus : plus sing
  | S_plus_eq : plus_eq sing
  | S_projection : projection sing
  | S_punned : 'a sing -> 'a punned sing
  | S_quote : quote sing
  | S_rbrace : rbrace sing
  | S_rbracket : rbracket sing
  | S_record : 'a sing -> 'a record sing
  | S_record_expr : record_expr sing
  | S_record_pattern : record_pattern sing
  | S_reg : 'a sing -> 'a reg sing
  | S_region : region sing
  | S_rev_app : rev_app sing
  | S_rpar : rpar sing
  | S_selection : selection sing
  | S_semi : semi sing
  | S_sepseq : 'a sing * 'b sing -> ('a, 'b) Utils.sepseq sing
  | S_sequence_expr : sequence_expr sing
  | S_sig_item : sig_item sing
  | S_sig_attr : sig_attr sing
  | S_sig_include : sig_include sing
  | S_sig_type : sig_type sing
  | S_sig_value : sig_value sing
  | S_signature_body : signature_body sing
  | S_signature_decl : signature_decl sing
  | S_signature_expr : signature_expr sing
  | S_slash : slash sing
  | S_slash_eq : slash_eq sing
  | S_string_literal : string_literal sing
  | S_the_unit : the_unit sing
  | S_times : times sing
  | S_times_eq : times_eq sing
  | S_tuple : 'a sing -> 'a tuple sing
  | S_tuple_2 : 'a sing * 'b sing -> ('a * 'b) sing
  | S_tuple_3 : 'a sing * 'b sing * 'c sing -> ('a * 'b * 'c) sing
  | S_tuple_4 : 'a sing * 'b sing * 'c sing * 'd sing -> ('a * 'b * 'c * 'd) sing
  | S_type_annotation : type_annotation sing
  | S_type_ctor_arg : type_ctor_arg sing
  | S_type_decl : type_decl sing
  | S_type_expr : type_expr sing
  | S_type_in : type_in sing
  | S_type_name : type_name sing
  | S_type_params : type_params sing
  | S_type_var : type_var sing
  | S_type_variable : type_variable sing
  | S_type_vars : type_vars sing
  | S_typed_expr : typed_expr sing
  | S_typed_pattern : typed_pattern sing
  | S_un_op : 'a sing -> 'a un_op sing
  | S_update_expr : update_expr sing
  | S_variable : variable sing
  | S_variant : variant sing
  | S_variant_type : variant_type sing
  | S_vbar : vbar sing
  | S_vbar_eq : vbar_eq sing
  | S_verbatim_literal : verbatim_literal sing
  | S_while_loop : while_loop sing
  | S_wrap : 'a sing -> 'a wrap sing
  | S_z : Z.t sing

type some_node = Some_node : 'b * 'b sing -> some_node
let (-|) a b = Some_node (a, b)

let fold'
    (type acc)
    (init : acc)
    (instruction : acc -> some_node -> acc fold_control)
    (node : some_node) : acc =
  let acc = ref init in
  let rec process : some_node -> unit =
    fun some_node ->
      match instruction !acc some_node with
        Stop -> ()
      | Skip -> fold some_node
      | Continue x -> acc := x; fold some_node
      | Last x -> acc := x

  and process_list : some_node list -> unit =
    fun l -> List.iter l ~f:process

  and fold : some_node -> unit =
  function (Some_node (node, sing)) -> match sing with
    S_append -> process @@ node -| S_wrap S_lexeme
  | S_arrow -> process @@ node -| S_wrap S_lexeme
  | S_ass -> process @@ node -| S_wrap S_lexeme
  | S_assign ->
    let { binder; ass; expr } = node in
    process_list
    [ binder -| S_variable
    ; ass -| S_ass
    ; expr -| S_expr ]
  | S_attr -> () (* Leaf *)
  | S_attribute -> process @@ node -| S_wrap S_attr
  | S_bin_op sing ->
    let { op; arg1; arg2 } = node in
    process_list
    [ arg1 -| S_expr
    ; op -| sing
    ; arg2 -| S_expr ]
  | S_bool_and -> process @@ node -| S_wrap S_lexeme
  | S_bool_or -> process @@ node -| S_wrap S_lexeme
  | S_braces sing -> process @@ node -| S_reg (S_braces' sing)
  | S_braces' sing ->
    let { lbrace; inside; rbrace } = node in
    process_list
    [ lbrace -| S_lbrace
    ; inside -| sing
    ; rbrace -| S_rbrace ]
  | S_brackets sing -> process @@ node -| S_reg (S_brackets' sing)
  | S_brackets' sing ->
    let { lbracket; inside; rbracket } = node in
    process_list
    [ lbracket -| S_lbracket
    ; inside -| sing
    ; rbracket -| S_rbracket ]
  | S_caret ->  process @@ node -| S_wrap S_lexeme
  | S_cartesian -> process @@ node -|
    S_tuple_3 ( S_type_expr, S_times, S_nsepseq (S_type_expr, S_times))
  | S_code_inj ->
    let { language; code; rbracket } = node in
    process_list
    [ language -| S_language
    ; code -| S_expr
    ; rbracket -| S_rbracket ]
  | S_colon -> process @@ node -| S_wrap S_lexeme
  | S_comma -> process @@ node -| S_wrap S_lexeme
  | S_compound -> process_list
    (match node with
      BeginEnd (node_1, node_2) ->
      [ node_1 -| S_kwd_begin
      ; node_2 -| S_kwd_end ]
    | Parens (node_1, node_2) ->
      [ node_1 -| S_lpar
      ; node_2 -| S_rpar ])
  | S_cond_expr ->
    let { kwd_if; test; kwd_then; if_so; if_not } = node in
    process_list
    [ kwd_if -| S_kwd_if
    ; test -| S_expr
    ; kwd_then -| S_kwd_then
    ; if_so -| S_expr
    ; if_not -| S_option (S_tuple_2 (S_kwd_else, S_expr)) ]
  | S_cons -> process @@ node -| S_wrap S_lexeme
  | S_cst ->
      let { decl; eof } = node in
      process_list
      [ decl -| S_nseq S_declaration
      ; eof -| S_eof ]
  | S_ctor -> process @@ node -| S_wrap S_lexeme
  | S_declaration -> process
    (match node with
      D_Attr node -> node -| S_reg (S_tuple_2 (S_attribute, S_declaration))
    | D_Let node -> node -| S_reg S_let_decl
    | D_Type node -> node -| S_reg S_type_decl
    | D_Module node -> node -| S_reg S_module_decl
    | D_Directive node -> node -| S_directive
    | D_Include node -> node -| S_reg S_module_include
    | D_Signature node -> node -| S_reg S_signature_decl)
  | S_direction -> process
    (match node with
      Upto node -> node -| S_kwd_upto
    | Downto node -> node -| S_kwd_downto)
  | S_directive -> () (* Leaf *)
  | S_dot -> process @@ node -| S_wrap S_lexeme
  | S_eof -> process @@ node -| S_wrap S_lexeme
  | S_equal -> process @@ node -| S_wrap S_lexeme
  | S_expr -> process
    (match node with
      E_Add node -> node -| S_reg (S_bin_op S_plus)
    | E_And node -> node -| S_reg (S_bin_op S_bool_and)
    | E_App node -> node -| S_reg (S_tuple_2 (S_expr, S_nseq S_expr))
    | E_Assign node -> node -| S_reg S_assign
    | E_Attr node -> node -| S_tuple_2 (S_attribute, S_expr)
    | E_Bytes node -> node -| S_wrap (S_tuple_2 (S_lexeme, S_hex))
    | E_Cat node -> node -| S_reg (S_bin_op S_caret)
    | E_CodeInj node -> node -| S_reg S_code_inj
    | E_Cond node -> node -| S_reg S_cond_expr
    | E_Cons node -> node -| S_reg (S_bin_op S_cons)
    | E_ContractOf node -> node -| S_reg (S_nsepseq (S_module_name, S_dot))
    | E_Ctor node -> node -| S_ctor
    | E_Div node -> node -| S_reg (S_bin_op S_slash)
    | E_Equal node -> node -| S_reg (S_bin_op S_equal)
    | E_False node -> node -| S_kwd_false
    | E_For node -> node -| S_reg S_for_loop
    | E_ForIn node -> node -| S_reg S_for_in_loop
    | E_Fun node -> node -| S_reg S_fun_expr
    | E_Geq node -> node -| S_reg (S_bin_op S_geq)
    | E_Gt node -> node -| S_reg (S_bin_op S_gt)
    | E_Int node -> node -| S_wrap (S_tuple_2 (S_lexeme, S_z))
    | E_Land node -> node -| S_reg (S_bin_op S_kwd_land)
    | E_Leq node -> node -| S_reg (S_bin_op S_leq)
    | E_LetIn node -> node -| S_reg S_let_in
    | E_LetMutIn node -> node -| S_reg S_let_mut_in
    | E_List node -> node -| S_list_ S_expr
    | E_Lor node -> node -| S_reg (S_bin_op S_kwd_lor)
    | E_Lsl node -> node -| S_reg (S_bin_op S_kwd_lsl)
    | E_Lsr node -> node -| S_reg (S_bin_op S_kwd_lsr)
    | E_Lt node -> node -| S_reg (S_bin_op S_lt)
    | E_Lxor node -> node -| S_reg (S_bin_op S_kwd_lxor)
    | E_Match node -> node -| S_reg S_match_expr
    | E_Mod node -> node -| S_reg (S_bin_op S_kwd_mod)
    | E_ModIn node -> node -| S_reg S_module_in
    | E_ModPath node -> node -| S_reg (S_module_path S_expr)
    | E_Mult node -> node -| S_reg (S_bin_op S_times)
    | E_Mutez node -> node -| S_wrap (S_tuple_2 (S_lexeme, S_int64))
    | E_Nat node -> node -| S_wrap (S_tuple_2 (S_lexeme, S_z))
    | E_Neg node -> node -| S_reg (S_un_op S_minus)
    | E_Neq node -> node -| S_reg (S_bin_op S_neq)
    | E_Not node -> node -| S_reg (S_un_op S_kwd_not)
    | E_Or node -> node -| S_reg (S_bin_op S_kwd_or)
    | E_Par node -> node -| S_par S_expr
    | E_Proj node -> node -| S_reg S_projection
    | E_Record node -> node -| S_record_expr
    | E_String node -> node -| S_string_literal
    | E_Sub node -> node -| S_reg (S_bin_op S_minus)
    | E_True node -> node -| S_kwd_true
    | E_Tuple node -> node -| S_reg (S_tuple S_expr)
    | E_Typed node -> node -| S_par S_typed_expr
    | E_TypeIn node -> node -| S_reg S_type_in
    | E_Unit node -> node -| S_reg S_the_unit
    | E_Update node -> node -| S_braces S_update_expr
    | E_Var node -> node -| S_variable
    | E_Verbatim node -> node -| S_verbatim_literal
    | E_Seq node -> node -| S_reg S_sequence_expr
    | E_RevApp node -> node -| S_reg (S_bin_op S_rev_app)
    | E_While node -> node -| S_reg S_while_loop)
  | S_field (sing_1, sing_2, sing_3) -> process
    (match node with
      Punned node -> node -| S_reg (S_punned sing_1)
    | Complete node -> node -| S_reg (S_full_field (sing_1, sing_2, sing_3)))
  | S_full_field (sing_1, sing_2, sing_3) ->
    let { attributes; field_lhs; field_lens; field_rhs } = node in
    process_list
    [ attributes -| S_list S_attribute
    ; field_lhs -| sing_1
    ; field_lens -| sing_2
    ; field_rhs -| sing_3 ]
  | S_field_decl ->
    let { attributes; field_name; field_type } = node in
    process_list
    [ attributes -| S_list S_attribute
    ; field_name -| S_field_name
    ; field_type -| S_option S_type_annotation ]
  | S_field_name -> process @@ node -| S_variable
  | S_for_all ->
    process @@ node -| S_tuple_3 (S_nseq S_type_var, S_dot, S_type_expr)
  | S_for_in_loop ->
    let { kwd_for; pattern; kwd_in; collection; body } = node in
    process_list
    [ kwd_for -| S_kwd_for
    ; pattern -| S_pattern
    ; kwd_in -| S_kwd_in
    ; collection -| S_expr
    ; body -| S_reg S_loop_body ]
  | S_for_loop ->
    let { kwd_for; index; equal; bound1; direction; bound2; body } = node in
    process_list
    [ kwd_for -| S_kwd_for
    ; index -| S_variable
    ; equal -| S_equal
    ; bound1 -| S_expr
    ; direction -| S_direction
    ; bound2 -| S_expr
    ; body -| S_reg S_loop_body ]
  | S_fun_expr ->
    let { kwd_fun; type_params; binders; rhs_type; arrow; body} = node in
    process_list
    [ kwd_fun -| S_kwd_fun
    ; type_params -| S_option (S_par S_type_params)
    ; binders -| S_nseq S_pattern
    ; rhs_type -| S_option (S_tuple_2 (S_colon, S_type_expr))
    ; arrow -| S_arrow
    ; body -| S_expr ]
  | S_fun_type -> process @@ node -| S_tuple_3 (S_type_expr, S_arrow, S_type_expr)
  | S_geq -> process @@ node -| S_wrap S_lexeme
  | S_gt -> process @@ node -| S_wrap S_lexeme
  | S_hex -> () (* Leaf *)
  | S_int64 -> () (* Leaf *)
  | S_kwd_begin -> process @@ node -| S_wrap S_lexeme
  | S_kwd_do -> process @@ node -| S_wrap S_lexeme
  | S_kwd_done -> process @@ node -| S_wrap S_lexeme
  | S_kwd_downto -> process @@ node -| S_wrap S_lexeme
  | S_kwd_else -> process @@ node -| S_wrap S_lexeme
  | S_kwd_end -> process @@ node -| S_wrap S_lexeme
  | S_kwd_false -> process @@ node -| S_wrap S_lexeme
  | S_kwd_for -> process @@ node -| S_wrap S_lexeme
  | S_kwd_fun -> process @@ node -| S_wrap S_lexeme
  | S_kwd_if -> process @@ node -| S_wrap S_lexeme
  | S_kwd_in -> process @@ node -| S_wrap S_lexeme
  | S_kwd_include -> process @@ node -| S_wrap S_lexeme
  | S_kwd_land -> process @@ node -| S_wrap S_lexeme
  | S_kwd_let -> process @@ node -| S_wrap S_lexeme
  | S_kwd_lor -> process @@ node -| S_wrap S_lexeme
  | S_kwd_lsl -> process @@ node -| S_wrap S_lexeme
  | S_kwd_lsr -> process @@ node -| S_wrap S_lexeme
  | S_kwd_lxor -> process @@ node -| S_wrap S_lexeme
  | S_kwd_match -> process @@ node -| S_wrap S_lexeme
  | S_kwd_mod -> process @@ node -| S_wrap S_lexeme
  | S_kwd_module -> process @@ node -| S_wrap S_lexeme
  | S_kwd_mut -> process @@ node -| S_wrap S_lexeme
  | S_kwd_not -> process @@ node -| S_wrap S_lexeme
  | S_kwd_of -> process @@ node -| S_wrap S_lexeme
  | S_kwd_or -> process @@ node -| S_wrap S_lexeme
  | S_kwd_rec -> process @@ node -| S_wrap S_lexeme
  | S_kwd_struct -> process @@ node -| S_wrap S_lexeme
  | S_kwd_then -> process @@ node -| S_wrap S_lexeme
  | S_kwd_true -> process @@ node -| S_wrap S_lexeme
  | S_kwd_type -> process @@ node -| S_wrap S_lexeme
  | S_kwd_upto -> process @@ node -| S_wrap S_lexeme
  | S_kwd_val -> process @@ node -| S_wrap S_lexeme
  | S_kwd_while -> process @@ node -| S_wrap S_lexeme
  | S_kwd_with -> process @@ node -| S_wrap S_lexeme
  | S_language -> process @@ node -| S_wrap (S_reg S_lexeme)
  | S_lbrace -> process @@ node -| S_wrap S_lexeme
  | S_lbracket -> process @@ node -| S_wrap S_lexeme
  | S_lens -> process
    (match node with
      Lens_Id node -> node -| S_equal
    | Lens_Add node -> node -| S_plus_eq
    | Lens_Sub node -> node -| S_minus_eq
    | Lens_Mult node -> node -| S_times_eq
    | Lens_Div node -> node -| S_slash_eq
    | Lens_Fun node -> node -| S_vbar_eq)
  | S_leq -> process @@ node -| S_wrap S_lexeme
  | S_let_binding ->
    let { type_params; binders; rhs_type; eq; let_rhs } = node in
    process_list
    [ type_params -| S_option (S_par S_type_params)
    ; binders -| S_nseq S_pattern
    ; rhs_type -| S_option (S_tuple_2 (S_colon, S_type_expr))
    ; eq -| S_equal
    ; let_rhs -| S_expr ]
  | S_let_decl ->
    process @@ node -| S_tuple_3 (S_kwd_let, S_option S_kwd_rec, S_let_binding)
  | S_let_in ->
    let { kwd_let; kwd_rec; binding; kwd_in; body } = node in
    process_list
    [ kwd_let -| S_kwd_let
    ; kwd_rec -| S_option S_kwd_rec
    ; binding -| S_reg S_let_binding
    ; kwd_in -| S_kwd_in
    ; body -| S_expr ]
  | S_let_mut_in ->
    let { kwd_let; kwd_mut; binding; kwd_in; body } = node in
    process_list
    [ kwd_let -| S_kwd_let
    ; kwd_mut -| S_kwd_mut
    ; binding -| S_reg S_let_binding
    ; kwd_in -| S_kwd_in
    ; body -| S_expr ]
  | S_lexeme -> () (* Leaf *)
  | S_list sing -> process_list @@ List.map ~f:(fun x -> x -| sing) node
  | S_list_ sing -> process @@ node -| S_brackets (S_sepseq (sing, S_semi))
  | S_loop_body ->
    let { kwd_do; seq_expr; kwd_done } = node in
    process_list
    [ kwd_do -| S_kwd_do
    ; seq_expr -| S_option (S_nsepseq (S_expr, S_semi))
    ; kwd_done -| S_kwd_done ]
  | S_lpar -> process @@ node -| S_wrap S_lexeme
  | S_lt -> process @@ node -| S_wrap S_lexeme
  | S_match_clause ->
    let { pattern; arrow; rhs } = node in
    process_list
    [ pattern -| S_pattern
    ; arrow -| S_arrow
    ; rhs -| S_expr ]
  | S_match_expr ->
    let { kwd_match; subject; kwd_with; lead_vbar; clauses } = node in
    process_list
    [ kwd_match -| S_kwd_match
    ; subject -| S_expr
    ; kwd_with -| S_kwd_with
    ; lead_vbar -| S_option S_vbar
    ; clauses -| S_reg (S_nsepseq (S_reg S_match_clause, S_vbar)) ]
  | S_minus -> process @@ node -| S_wrap S_lexeme
  | S_minus_eq -> process @@ node -| S_wrap S_lexeme
  | S_module_body ->
    let { kwd_struct; declarations; kwd_end } = node in
    process_list
    [ kwd_struct -| S_kwd_module
    ; declarations -| S_list S_declaration
    ; kwd_end -| S_kwd_end ]
  | S_module_decl ->
    let { kwd_module; name; annotation; eq; module_expr} = node in
    process_list
    [ kwd_module -| S_kwd_module
    ; name -| S_module_name
    ; annotation -| S_option (S_tuple_2 (S_colon, S_signature_expr))
    ; eq -| S_equal
    ; module_expr -| S_module_expr ]
  | S_module_include ->
    let {kwd_include; module_expr} = node in
    process_list
    [ kwd_include -| S_kwd_include
    ; module_expr -| S_module_expr
    ]
  | S_module_expr -> process
    (match node with
      M_Body node -> node -| S_reg S_module_body
    | M_Path node -> node -| S_reg (S_module_path S_module_name)
    | M_Var node -> node -| S_module_name)
  | S_module_in ->
    let { mod_decl; kwd_in; body } = node in
    process_list
    [ mod_decl -| S_reg S_module_decl
    ; kwd_in -| S_kwd_in
    ; body -| S_expr ]
  | S_module_name -> process @@ node -| S_wrap S_lexeme
  | S_module_path sing ->
    let { module_path; selector; field } = node in
    process_list
    [ module_path -| S_nsepseq (S_module_name, S_dot)
    ; selector -| S_dot
    ; field -| sing ]
  | S_neq -> process @@ node -| S_wrap S_lexeme
  | S_nsepseq (sing_1, sing_2) ->
    process @@ node -| S_tuple_2 (sing_1, S_list (S_tuple_2 (sing_2, sing_1)))
  | S_nseq sing -> process @@ node -| S_tuple_2 (sing, S_list sing)
  | S_option sing ->
    (match node with
      None -> () (* Leaf *)
    | Some node -> process @@ node -| sing)
  | S_par sing -> process @@ node -| S_reg (S_par' sing)
  | S_par' sing ->
    let { lpar; inside; rpar } = node in
    process_list
    [ lpar -| S_lpar
    ; inside -| sing
    ; rpar -| S_rpar ]
  | S_path -> process
    (match node with
      Name node -> node -| S_variable
    | Path node -> node -| S_reg S_projection)
  | S_pattern -> process
    (match node with
      P_App node -> node -| S_reg (S_tuple_2 (S_pattern, S_option S_pattern))
    | P_Attr node -> node -| S_tuple_2 (S_attribute, S_pattern)
    | P_Bytes node -> node -| S_wrap (S_tuple_2 (S_lexeme, S_hex))
    | P_Cons node -> node -| S_reg (S_tuple_3 (S_pattern, S_cons, S_pattern))
    | P_Ctor node -> node -| S_ctor
    | P_False node -> node -| S_kwd_false
    | P_Int node -> node -| S_wrap (S_tuple_2 (S_lexeme, S_z))
    | P_List node -> node -| S_list_ S_pattern
    | P_ModPath node -> node -| S_reg (S_module_path S_pattern)
    | P_Mutez node -> node -| S_wrap (S_tuple_2 (S_lexeme, S_int64))
    | P_Nat node -> node -| S_wrap (S_tuple_2 (S_lexeme, S_z))
    | P_Par node -> node -| S_par S_pattern
    | P_Record node -> node -| S_record_pattern
    | P_String node -> node -| S_string_literal
    | P_True node -> node -| S_kwd_true
    | P_Tuple node -> node -| S_reg (S_tuple S_pattern)
    | P_Typed node -> node -| S_reg S_typed_pattern
    | P_Var node -> node -| S_variable
    | P_Verbatim node -> node -| S_verbatim_literal
    | P_Unit node -> node -| S_reg S_the_unit)
  | S_plus -> process @@ node -| S_wrap S_lexeme
  | S_plus_eq -> process @@ node -| S_wrap S_lexeme
  | S_projection ->
    let { record_or_tuple; selector; field_path } = node in
    process_list
    [ record_or_tuple -| S_expr
    ; selector -| S_dot
    ; field_path -| S_nsepseq (S_selection, S_dot) ]
  | S_punned sing ->
    let { attributes; pun } = node in
    process_list
    [ attributes -| S_list S_attribute
    ; pun -| sing]
  | S_quote -> process @@ node -| S_wrap S_lexeme
  | S_rbrace -> process @@ node -| S_wrap S_lexeme
  | S_rbracket -> process @@ node -| S_wrap S_lexeme
  | S_record sing -> process @@ node -| S_braces (S_sepseq (sing, S_semi))
  | S_record_expr ->
    process @@ node -| S_record (S_field (S_field_name, S_equal, S_expr))
  | S_record_pattern ->
    process @@ node -| S_record (S_field (S_field_name, S_equal, S_pattern))
  | S_reg sing ->
    let { region; value } = node in
    process_list
    [ region -| S_region
    ; value -| sing ]
  | S_region -> () (* Leaf *)
  | S_rev_app -> process @@ node -| S_wrap S_lexeme
  | S_rpar -> process @@ node -| S_wrap S_lexeme
  | S_sig_attr -> process @@ node -| S_tuple_2 (S_attribute, S_sig_item)
  | S_sig_include -> process @@ node -| S_tuple_2 (S_kwd_include, S_signature_expr)
  | S_sig_type ->
    let { kwd_type; type_vars; type_name; type_rhs } = node in
    process_list
      [ kwd_type -| S_kwd_type
      ; type_vars -| S_option S_type_vars
      ; type_name -| S_type_name
      ; type_rhs -| S_option (S_tuple_2 (S_equal, S_type_expr)) ]
  | S_sig_value ->
    let { kwd_val; var; colon; val_type} = node in
    process_list
      [ kwd_val -| S_kwd_val
      ; var -| S_variable
      ; colon -| S_colon
      ; val_type -| S_type_expr]
  | S_selection -> process
    (match node with
      FieldName node -> node -| S_variable
    | Component node -> node -| S_wrap (S_tuple_2 (S_lexeme, S_z)))
  | S_semi -> process @@ node -| S_wrap S_lexeme
  | S_sequence_expr ->
    let { compound; elements } = node in
    process_list
    [ compound -| S_option S_compound
    ; elements -| S_sepseq (S_expr, S_semi) ]
  | S_sepseq (sing_1, sing_2) ->
    process @@ node -| S_option (S_nsepseq (sing_1, sing_2))
  | S_signature_body ->
    let { kwd_sig; sig_items; kwd_end } : signature_body = node in
    process_list
    [ kwd_sig -| S_kwd_module
    ; sig_items -| S_list S_sig_item
    ; kwd_end -| S_kwd_end ]
  | S_signature_decl ->
    let { kwd_module; kwd_type; name; eq; signature_expr} : signature_decl = node in
    process_list
    [ kwd_module -| S_kwd_module
    ; kwd_type -| S_kwd_type
    ; name -| S_module_name
    ; eq -| S_equal
    ; signature_expr -| S_signature_expr ]
  | S_signature_expr -> process
    (match node with
      S_Sig node -> node -| S_reg S_signature_body
    | S_Path node -> node -| S_reg (S_module_path S_module_name)
    | S_Var node -> node -| S_module_name)
  | S_sig_item -> process
    (match node with
      S_Attr node -> node -| S_reg S_sig_attr
    | S_Value node -> node -| S_reg S_sig_value
    | S_Type node -> node -| S_reg S_sig_type
    | S_Include node -> node -| S_reg S_sig_include)
  | S_slash -> process @@ node -| S_wrap S_lexeme
  | S_slash_eq -> process @@ node -| S_wrap S_lexeme
  | S_string_literal -> process @@ node -| S_wrap S_lexeme
  | S_the_unit -> process @@ node -| S_tuple_2 (S_lpar, S_rpar)
  | S_times -> process @@ node -| S_wrap S_lexeme
  | S_times_eq -> process @@ node -| S_wrap S_lexeme
  | S_tuple sing -> process @@ node -| S_nsepseq (sing, S_comma)
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
  | S_type_annotation -> process @@ node -| S_tuple_2 (S_colon, S_type_expr)
  | S_type_ctor_arg -> process
    (match node with
      TC_Single node -> node -| S_type_expr
    | TC_Tuple node -> node -| S_par (S_tuple S_type_expr))
  | S_type_decl ->
    let { kwd_type; params; name; eq; type_expr } = node in
    process_list
    [ kwd_type -| S_kwd_type
    ; params -| S_option S_type_vars
    ; name -| S_type_name
    ; eq -| S_equal
    ; type_expr -| S_type_expr ]
  | S_type_expr -> process
    (match node with
      T_App node -> node -| S_reg (S_tuple_2 (S_type_expr, S_type_ctor_arg))
    | T_Arg node -> node -| S_type_var
    | T_Attr node -> node -| S_tuple_2 (S_attribute, S_type_expr)
    | T_Cart node -> node -| S_reg S_cartesian
    | T_ForAll node -> node -| S_reg S_for_all
    | T_Fun node -> node -| S_reg S_fun_type
    | T_Int node -> node -| S_wrap (S_tuple_2 (S_lexeme, S_z))
    | T_ModPath node -> node -| S_reg (S_module_path S_type_expr)
    | T_Par node -> node -| S_par S_type_expr
    | T_Record node -> node -| S_record (S_reg S_field_decl)
    | T_String node -> node -| S_string_literal
    | T_Variant node -> node -| S_reg S_variant_type
    | T_Var node -> node -| S_type_variable
    | T_ParameterOf node -> node -| S_reg (S_nsepseq (S_module_name, S_dot)))
  | S_type_in ->
    let { type_decl; kwd_in; body } = node in
    process_list
    [ type_decl -| S_reg S_type_decl
    ; kwd_in -| S_kwd_in
    ; body -| S_expr ]
  | S_type_name -> process @@ node -| S_variable
  | S_type_params ->
    process @@ node -| S_tuple_2 (S_kwd_type, S_nseq S_type_variable)
  | S_type_var ->
    process @@ node -| S_reg (S_tuple_2 (S_option S_quote, S_type_variable))
  | S_type_variable -> process @@ node -| S_variable
  | S_type_vars -> process
    (match node with
      TV_Single node -> node -| S_type_var
    | TV_Tuple node -> node -| S_par (S_tuple S_type_var))
  | S_typed_expr -> process @@ node -| S_tuple_2 (S_expr, S_type_annotation)
  | S_typed_pattern ->
    process @@ node -| S_tuple_2 (S_pattern, S_type_annotation)
  | S_un_op sing ->
    let { op; arg } = node in
    process_list
    [ op -| sing
    ; arg -| S_expr ]
  | S_update_expr ->
    let { record; kwd_with; updates } = node in
    process_list
    [ record -| S_expr
    ; kwd_with -| S_kwd_with
    ; updates -| S_nsepseq (S_field (S_path, S_lens, S_expr), S_semi)]
  | S_variable -> process
    ( match node with
        Var node | Esc node -> node -| S_wrap S_lexeme)
  | S_variant ->
    let { attributes; ctor; ctor_args } = node in
    process_list
    [ attributes -| S_list S_attribute
    ; ctor -| S_ctor
    ; ctor_args -| S_option (S_tuple_2 (S_kwd_of, S_type_expr))]
  | S_variant_type ->
    let { lead_vbar; variants } = node in
    process_list
    [ lead_vbar -| S_option S_vbar
    ; variants -| S_nsepseq (S_reg S_variant, S_vbar)]
  | S_vbar -> process @@ node -| S_wrap S_lexeme
  | S_vbar_eq -> process @@ node -| S_wrap S_lexeme
  | S_verbatim_literal -> process @@ node -| S_wrap S_lexeme
  | S_while_loop ->
    let { kwd_while; cond; body } = node in
    process_list
    [ kwd_while -| S_kwd_while
    ; cond -| S_expr
    ; body -| S_reg S_loop_body]
  | S_wrap sing -> process_list
    [ node#payload -| sing
    ; node#attributes -| S_list (S_reg S_attr)
    ; node#region -| S_region
    ; node#directives -| S_list S_directive ]
  | S_z -> () (* Leaf *)
  in
  process node;
  !acc

let fold
    (type a b)
    (init : b)
    (f : b -> a -> b)
    (instruction : some_node -> a fold_control)
    (node : some_node) : b =
  fold' init (fun acc n -> map_fold_control (instruction n) ~f:(f acc)) node

let fold_cst'
    (type acc)
    (init : acc)
    (instruction : acc -> some_node -> acc fold_control)
    (cst : CST.t) : acc =
  fold' init instruction (cst -| S_cst)

let fold_cst
    (type a b)
    (init : b)
    (f : b -> a -> b)
    (instruction : some_node -> a fold_control)
    (cst : CST.t) : b =
  fold init f instruction (cst -| S_cst)

let fold_map_cst
    (type a)
    (m : a monoid)
    (f : some_node -> a fold_control)
    (cst : CST.t) : a =
  fold_cst m.empty m.append f cst

let fold_map
    (type a)
    (m : a monoid)
    (f : some_node -> a fold_control)
    (node : some_node) : a =
  fold m.empty m.append f node
