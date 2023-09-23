open Core
open Cst_shared.Fold
open CST
open Region

type 'a fold_control = 'a Cst_shared.Fold.fold_control

type _ sing =
    S_arrow : arrow sing
  | S_assign : assign sing
  | S_assignment : assignment sing
  | S_attr : Attr.t sing
  | S_attribute : attribute sing
  | S_bin_op : 'a sing -> 'a bin_op sing
  | S_binding : binding sing
  | S_block : block sing
  | S_block_enclosing : block_enclosing sing
  | S_block_with : block_with sing
  | S_brackets : 'a sing -> 'a brackets sing
  | S_call : call sing
  | S_call_args : call_args sing
  | S_caret : caret sing
  | S_cartesian : cartesian sing
  | S_case : 'a sing -> 'a case sing
  | S_case_clause : 'a sing -> 'a case_clause sing
  | S_chevrons : 'a sing -> 'a chevrons sing
  | S_code_inj : code_inj sing
  | S_collection : collection sing
  | S_colon : colon sing
  | S_comma : comma sing
  | S_compound : 'a sing -> 'a compound sing
  | S_conditional : 'a sing -> 'a conditional sing
  | S_const_decl : const_decl sing
  | S_cst : CST.t sing
  | S_ctor : ctor sing
  | S_declaration : declaration sing
  | S_declarations : declarations sing
  | S_directive : Directive.t sing
  | S_dot : dot sing
  | S_eof : eof sing
  | S_equal : equal sing
  | S_expr : expr sing
  | S_field : 'a sing * 'b sing -> ('a, 'b) field sing
  | S_field_decl : field_decl sing
  | S_field_lens : field_lens sing
  | S_field_name : field_name sing
  | S_field_pattern : field_pattern sing
  | S_for_in : for_in sing
  | S_for_int : for_int sing
  | S_for_map : for_map sing
  | S_for_set_or_list : for_set_or_list sing
  | S_full_field : 'a sing * 'b sing -> ('a, 'b) full_field sing
  | S_fun_decl : fun_decl sing
  | S_fun_expr : fun_expr sing
  | S_geq : geq sing
  | S_gt : gt sing
  | S_hex : Hex.t sing
  | S_instruction : instruction sing
  | S_int64 : Int64.t sing
  | S_kwd_and : kwd_and sing
  | S_kwd_begin : kwd_begin sing
  | S_kwd_big_map : kwd_big_map sing
  | S_kwd_block : kwd_block sing
  | S_kwd_case : kwd_case sing
  | S_kwd_const : kwd_const sing
  | S_kwd_contains : kwd_contains sing
  | S_kwd_down : kwd_down sing
  | S_kwd_else : kwd_else sing
  | S_kwd_end : kwd_end sing
  | S_kwd_for : kwd_for sing
  | S_kwd_from : kwd_from sing
  | S_kwd_function : kwd_function sing
  | S_kwd_if : kwd_if sing
  | S_kwd_in : kwd_in sing
  | S_kwd_is : kwd_is sing
  | S_kwd_list : kwd_list sing
  | S_kwd_map : kwd_map sing
  | S_kwd_mod : kwd_mod sing
  | S_kwd_module : kwd_module sing
  | S_kwd_nil : kwd_nil sing
  | S_kwd_not : kwd_not sing
  | S_kwd_of : kwd_of sing
  | S_kwd_or : kwd_or sing
  | S_kwd_patch : kwd_patch sing
  | S_kwd_record : kwd_record sing
  | S_kwd_recursive : kwd_recursive sing
  | S_kwd_remove : kwd_remove sing
  | S_kwd_set : kwd_set sing
  | S_kwd_skip : kwd_skip sing
  | S_kwd_step : kwd_step sing
  | S_kwd_then : kwd_then sing
  | S_kwd_to : kwd_to sing
  | S_kwd_type : kwd_type sing
  | S_kwd_var : kwd_var sing
  | S_kwd_while : kwd_while sing
  | S_kwd_with : kwd_with sing
  | S_language : language sing
  | S_lbrace : lbrace sing
  | S_lbracket : lbracket sing
  | S_leq : leq sing
  | S_lexeme : lexeme sing
  | S_list : 'a sing -> 'a list sing
  | S_lpar : lpar sing
  | S_lt : lt sing
  | S_map_lookup : map_lookup sing
  | S_minus : minus sing
  | S_minus_eq : minus_eq sing
  | S_module_body : module_body sing
  | S_module_decl : module_decl sing
  | S_module_expr : module_expr sing
  | S_module_name : module_name sing
  | S_module_path : 'a sing -> 'a module_path sing
  | S_neq : neq sing
  | S_nsepseq : 'a sing * 'b sing -> ('a, 'b) Utils.nsepseq sing
  | S_nseq : 'a sing -> 'a Utils.nseq sing
  | S_option : 'a sing -> 'a option sing
  | S_par : 'a sing -> 'a par sing
  | S_param_decl : param_decl sing
  | S_parameters : parameters sing
  | S_patch : patch sing
  | S_patch_kind : patch_kind sing
  | S_pattern : pattern sing
  | S_plus : plus sing
  | S_plus_eq : plus_eq sing
  | S_projection : projection sing
  | S_punned : 'a sing -> 'a punned sing
  | S_rbrace : rbrace sing
  | S_rbracket : rbracket sing
  | S_record_expr : record_expr sing
  | S_record_pattern : record_pattern sing
  | S_reg : 'a sing -> 'a reg sing
  | S_region : region sing
  | S_removal : removal sing
  | S_rpar : rpar sing
  | S_selection : selection sing
  | S_semi : semi sing
  | S_sepseq : 'a sing * 'b sing -> ('a, 'b) Utils.sepseq sing
  | S_set_membership : set_membership sing
  | S_sharp : sharp sing
  | S_slash : slash sing
  | S_slash_eq : slash_eq sing
  | S_statement : statement sing
  | S_statements : statements sing
  | S_string : string_ sing
  | S_sum_type : sum_type sing
  | S_test_clause : test_clause sing
  | S_times : times sing
  | S_times_eq : times_eq sing
  | S_tuple : 'a sing -> 'a tuple sing
  | S_tuple_2 : 'a sing * 'b sing -> ('a * 'b) sing
  | S_tuple_3 : 'a sing * 'b sing * 'c sing -> ('a * 'b * 'c) sing
  | S_tuple_4 : 'a sing * 'b sing * 'c sing * 'd sing -> ('a * 'b * 'c * 'd) sing
  | S_type_annotation : type_annotation sing
  | S_type_decl : type_decl sing
  | S_type_expr : type_expr sing
  | S_type_name : type_name sing
  | S_type_params : type_params sing
  | S_type_tuple : type_tuple sing
  | S_typed_expr : typed_expr sing
  | S_typed_pattern : typed_pattern sing
  | S_un_op : 'a sing -> 'a un_op sing
  | S_update : update sing
  | S_var_decl : var_decl sing
  | S_variable : variable sing
  | S_variant : variant sing
  | S_vbar : vbar sing
  | S_vbar_eq : vbar_eq sing
  | S_verbatim : verbatim sing
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
    S_arrow -> process @@ node -| S_wrap S_lexeme
  | S_assign -> process @@ node -| S_wrap S_lexeme
  | S_assignment ->
    let { lhs; assign; rhs } = node in
    process_list
    [ lhs -| S_expr
    ; assign -| S_assign
    ; rhs -| S_expr ]
  | S_attr -> () (* Leaf *)
  | S_attribute -> process @@ node -| S_wrap S_attr
  | S_bin_op sing ->
    let { op; arg1; arg2 } = node in
    process_list
    [ arg1 -| S_expr
    ; op -| sing
    ; arg2 -| S_expr ]
  | S_binding ->
    let { key; arrow; value } = node in
    process_list
    [ key -| S_expr
    ; arrow -| S_arrow
    ; value -| S_expr ]
  | S_block ->
    let { enclosing; statements; terminator } = node in
    process_list
    [ enclosing -| S_block_enclosing
    ; statements -| S_statements
    ; terminator -| S_option S_semi ]
  | S_block_enclosing -> process_list
    (match node with
      Braces (node_1, node_2, node_3) ->
      [ node_1 -| S_option S_kwd_block
      ; node_2 -| S_lbrace
      ; node_3 -| S_rbrace ]
    | BeginEnd (node_1, node_2) ->
      [ node_1 -| S_kwd_begin
      ; node_2 -| S_kwd_end ])
  | S_block_with ->
    let { block; kwd_with; expr } = node in
    process_list
    [ block -| S_reg S_block
    ; kwd_with -| S_kwd_with
    ; expr -| S_expr ]
  | S_brackets sing ->
    let { lbracket; inside; rbracket } = node in
    process_list
    [ lbracket -| S_lbracket
    ; inside -| sing
    ; rbracket -| S_rbracket ]
  | S_call -> process @@ node -| S_reg (S_tuple_2 (S_expr, S_call_args))
  | S_call_args -> process @@ node -| S_reg (S_par (S_sepseq (S_expr, S_comma)))
  | S_caret -> process @@ node -| S_wrap S_lexeme
  | S_cartesian ->
    process @@ node -|
      S_reg (S_tuple_3 (S_type_expr, S_times, S_nsepseq (S_type_expr, S_times)))
  | S_case sing ->
    let { kwd_case; expr; kwd_of; opening; lead_vbar; cases; closing } = node in
    process_list
    [ kwd_case -| S_kwd_case
    ; expr -| S_expr
    ; kwd_of -| S_kwd_of
    ; opening -| S_lbracket
    ; lead_vbar -| S_option S_vbar
    ; cases -| S_nsepseq (S_reg (S_case_clause sing), S_vbar)
    ; closing -| S_rbracket ]
  | S_case_clause sing ->
    let { pattern; arrow; rhs } = node in
    process_list
    [ pattern -| S_pattern
    ; arrow -| S_arrow
    ; rhs -| sing ]
  | S_chevrons sing ->
    let { lchevron; inside; rchevron } = node in
    process_list
    [ lchevron -| S_lt
    ; inside -| sing
    ; rchevron -| S_gt ]
  | S_code_inj ->
    let { language; code; rbracket } = node in
    process_list
    [ language -| S_language
    ; code -| S_expr
    ; rbracket -| S_rbracket ]
  | S_collection -> process @@ node -| S_wrap S_lexeme
  | S_colon -> process @@ node -| S_wrap S_lexeme
  | S_comma -> process @@ node -| S_wrap S_lexeme
  | S_compound sing ->
    let { kind; opening; elements; terminator; closing } = node in
    process_list
    [ kind -| S_collection
    ; opening -| S_lbracket
    ; elements -| S_sepseq (sing, S_semi)
    ; terminator -| S_option S_semi
    ; closing -| S_rbracket ]
  | S_conditional sing ->
    let { kwd_if; test; kwd_then; if_so; if_not } = node in
    process_list
    [ kwd_if -| S_kwd_if
    ; test -| S_expr
    ; kwd_then -| S_kwd_then
    ; if_so -| sing
    ; if_not -| S_option (S_tuple_2  (S_kwd_else, sing)) ]
  | S_const_decl ->
    let { kwd_const
      ; pattern
      ; type_params
      ; const_type
      ; equal
      ; init
      ; terminator } = node in
    process_list
    [ kwd_const -| S_kwd_const
    ; pattern -| S_pattern
    ; type_params -| S_option (S_reg (S_chevrons S_type_params))
    ; const_type -| S_option S_type_annotation
    ; equal -| S_equal
    ; init -| S_expr
    ; terminator -| S_option S_semi ]
  | S_cst ->
    let { decl; eof } = node in
    process_list
    [ decl -| S_declarations
    ; eof -| S_eof ]
  | S_ctor -> process @@ node -| S_wrap S_lexeme
  | S_declaration -> process
    (match node with
      D_Attr node -> node -| S_reg (S_tuple_2 (S_attribute, S_declaration))
    | D_Const node -> node -| S_reg S_const_decl
    | D_Directive node -> node -| S_directive
    | D_Fun node -> node -| S_reg S_fun_decl
    | D_Module node -> node -| S_reg S_module_decl
    | D_Type node -> node -| S_reg S_type_decl)
  | S_declarations -> process @@ node -| S_nseq S_declaration
  | S_directive -> () (* Leaf *)
  | S_dot -> process @@ node -| S_wrap S_lexeme
  | S_eof -> process @@ node -| S_wrap S_lexeme
  | S_equal -> process @@ node -| S_wrap S_lexeme
  | S_expr -> process
    (match node with
      E_Add node -> node -| S_reg (S_bin_op S_plus)
    | E_And node -> node -| S_reg (S_bin_op  S_kwd_and)
    | E_App node -> node -| S_call
    | E_Attr node -> node -| S_tuple_2 (S_attribute, S_expr)
    | E_BigMap node -> node -| S_reg (S_compound (S_reg S_binding))
    | E_Block node -> node -| S_reg S_block_with
    | E_Bytes node -> node -| S_wrap (S_tuple_2 (S_lexeme, S_hex))
    | E_Case node -> node -| S_reg (S_case S_expr)
    | E_Cat node -> node -| S_reg (S_bin_op S_caret)
    | E_CodeInj node -> node -| S_reg S_code_inj
    | E_Ctor node -> node -| S_ctor
    | E_Cond node -> node -| S_reg (S_conditional S_expr)
    | E_Cons node -> node -| S_reg (S_bin_op  S_sharp)
    | E_Div node -> node -| S_reg (S_bin_op  S_slash)
    | E_Equal node -> node -| S_reg (S_bin_op  S_equal)
    | E_Fun node -> node -| S_reg S_fun_expr
    | E_Geq node -> node -| S_reg (S_bin_op  S_geq)
    | E_Gt node -> node -| S_reg (S_bin_op  S_gt)
    | E_Int node -> node -| S_wrap (S_tuple_2 (S_lexeme, S_z))
    | E_Leq node -> node -| S_reg (S_bin_op  S_leq)
    | E_List node -> node -| S_reg (S_compound S_expr)
    | E_Lt node -> node -| S_reg (S_bin_op  S_lt)
    | E_Map node -> node -| S_reg (S_compound (S_reg S_binding))
    | E_MapLookup node -> node -| S_reg S_map_lookup
    | E_Mod node -> node -| S_reg (S_bin_op  S_kwd_mod)
    | E_ModPath node -> node -| S_reg (S_module_path S_expr)
    | E_Mult node -> node -| S_reg (S_bin_op  S_times)
    | E_Mutez node -> node -| S_wrap (S_tuple_2 (S_lexeme, S_int64))
    | E_Nat node -> node -| S_wrap (S_tuple_2 (S_lexeme, S_z))
    | E_Neg node -> node -| S_reg (S_un_op S_minus)
    | E_Neq node -> node -| S_reg (S_bin_op  S_neq)
    | E_Nil node -> node -| S_kwd_nil
    | E_Not node -> node -| S_reg (S_un_op S_kwd_not)
    | E_Or node -> node -| S_reg (S_bin_op  S_kwd_or)
    | E_Par node -> node -| S_reg (S_par S_expr)
    | E_Proj node -> node -| S_reg S_projection
    | E_Record node -> node -| S_record_expr
    | E_Set node -> node -| S_reg (S_compound S_expr)
    | E_SetMem node -> node -| S_reg S_set_membership
    | E_String node -> node -| S_string
    | E_Sub node -> node -| S_reg (S_bin_op  S_minus)
    | E_Tuple node -> node -| S_tuple S_expr
    | E_Typed node -> node -| S_reg (S_par S_typed_expr)
    | E_Update node -> node -| S_reg S_update
    | E_Var node -> node -| S_variable
    | E_Verbatim node -> node -| S_verbatim)
  | S_field (sing_1, sing_2) -> process
    (match node with
      Punned node -> node -| S_punned sing_1
    | Complete node -> node -| S_full_field (sing_1, sing_2))
  | S_field_decl ->
    let { attributes; field_name; field_type } = node in
    process_list
    [ attributes -| S_list S_attribute
    ; field_name -| S_field_name
    ; field_type -| S_option S_type_annotation ]
  | S_field_lens -> process
    (match node with
      Lens_Id node -> node -| S_assign
    | Lens_Add node -> node -| S_plus_eq
    | Lens_Sub node -> node -| S_minus_eq
    | Lens_Mult node -> node -| S_times_eq
    | Lens_Div node -> node -| S_slash_eq
    | Lens_Fun node -> node -| S_vbar_eq)
  | S_field_name -> process @@ node -| S_wrap S_lexeme
  | S_field_pattern -> process @@ node -| S_field (S_pattern, S_pattern)
  | S_for_in -> process
    (match node with
      ForMap node -> node -| S_reg S_for_map
    | ForSetOrList node -> node -| S_reg S_for_set_or_list)
  | S_for_int ->
    let { kwd_for; index; assign; init; kwd_to; bound; step; block } = node in
    process_list
    [ kwd_for -| S_kwd_for
    ; index -| S_variable
    ; assign -| S_assign
    ; init -| S_expr
    ; kwd_to -| S_kwd_to
    ; bound -| S_expr
    ; step -| S_option (S_tuple_2 (S_kwd_step, S_expr))
    ; block -| S_reg S_block ]
  | S_for_map ->
    let { kwd_for; binding; kwd_in; kwd_map; collection; block } = node in
    process_list
    [ kwd_for -| S_kwd_for
    ; binding -| S_tuple_3 (S_variable, S_arrow, S_variable)
    ; kwd_in -| S_kwd_in
    ; kwd_map -| S_kwd_map
    ; collection -| S_expr
    ; block -| S_reg S_block ]
  | S_for_set_or_list ->
    let { kwd_for; var; kwd_in; for_kind; collection; block } = node in
    process_list
    [ kwd_for -| S_kwd_for
    ; var -| S_variable
    ; kwd_in -| S_kwd_in
    ; (match for_kind with
        `Set node -> node -| S_kwd_set
      | `List node -> node -| S_kwd_list)
    ; collection -| S_expr
    ; block -| S_reg S_block ]
  | S_full_field (sing_1, sing_2) ->
    let { attributes; field_lhs; field_lens; field_rhs } = node in
    process_list
    [ attributes -| S_list S_attribute
    ; field_lhs -| sing_1
    ; field_lens -| S_field_lens
    ; field_rhs -| sing_2 ]
  | S_fun_decl ->
    let { kwd_recursive
      ; kwd_function
      ; fun_name
      ; type_params
      ; parameters
      ; ret_type
      ; kwd_is
      ; return
      ; terminator } = node in
    process_list
    [ kwd_recursive -| S_option S_kwd_recursive
    ; kwd_function -| S_kwd_function
    ; fun_name -| S_variable
    ; type_params -| S_option (S_reg (S_chevrons S_type_params))
    ; parameters -| S_parameters
    ; ret_type -| S_option S_type_annotation
    ; kwd_is -| S_kwd_is
    ; return -| S_expr
    ; terminator -| S_option S_semi ]
  | S_fun_expr ->
    let { kwd_function
      ; type_params
      ; parameters
      ; ret_type
      ; kwd_is
      ; return } : fun_expr = node in
    process_list
    [ kwd_function -| S_kwd_function
    ; type_params -| S_option (S_reg (S_chevrons S_type_params))
    ; parameters -| S_parameters
    ; ret_type -| S_option S_type_annotation
    ; kwd_is -| S_kwd_is
    ; return -| S_expr ]
  | S_geq -> process @@ node -| S_wrap S_lexeme
  | S_gt -> process @@ node -| S_wrap S_lexeme
  | S_hex -> () (* Leaf *)
  | S_instruction -> process
    (match node with
      I_Assign node -> node -| S_reg S_assignment
    | I_Call node -> node -| S_call
    | I_Case node -> node -| S_reg (S_case S_test_clause)
    | I_Cond node -> node -| S_reg (S_conditional S_test_clause)
    | I_For node -> node -| S_reg S_for_int
    | I_ForIn node -> node -| S_for_in
    | I_Patch node -> node -| S_reg S_patch
    | I_Remove node -> node -| S_reg S_removal
    | I_Skip node -> node -| S_kwd_skip
    | I_While node -> node -| S_reg S_while_loop)
  | S_int64 -> () (* Leaf *)
  | S_kwd_and -> process @@ node -| S_wrap S_lexeme
  | S_kwd_begin -> process @@ node -| S_wrap S_lexeme
  | S_kwd_big_map -> process @@ node -| S_wrap S_lexeme
  | S_kwd_block -> process @@ node -| S_wrap S_lexeme
  | S_kwd_case -> process @@ node -| S_wrap S_lexeme
  | S_kwd_const -> process @@ node -| S_wrap S_lexeme
  | S_kwd_contains -> process @@ node -| S_wrap S_lexeme
  | S_kwd_down -> process @@ node -| S_wrap S_lexeme
  | S_kwd_else -> process @@ node -| S_wrap S_lexeme
  | S_kwd_end -> process @@ node -| S_wrap S_lexeme
  | S_kwd_for -> process @@ node -| S_wrap S_lexeme
  | S_kwd_from -> process @@ node -| S_wrap S_lexeme
  | S_kwd_function -> process @@ node -| S_wrap S_lexeme
  | S_kwd_if -> process @@ node -| S_wrap S_lexeme
  | S_kwd_in -> process @@ node -| S_wrap S_lexeme
  | S_kwd_is -> process @@ node -| S_wrap S_lexeme
  | S_kwd_list -> process @@ node -| S_wrap S_lexeme
  | S_kwd_map -> process @@ node -| S_wrap S_lexeme
  | S_kwd_mod -> process @@ node -| S_wrap S_lexeme
  | S_kwd_module -> process @@ node -| S_wrap S_lexeme
  | S_kwd_nil -> process @@ node -| S_wrap S_lexeme
  | S_kwd_not -> process @@ node -| S_wrap S_lexeme
  | S_kwd_of -> process @@ node -| S_wrap S_lexeme
  | S_kwd_or -> process @@ node -| S_wrap S_lexeme
  | S_kwd_patch -> process @@ node -| S_wrap S_lexeme
  | S_kwd_record -> process @@ node -| S_wrap S_lexeme
  | S_kwd_recursive -> process @@ node -| S_wrap S_lexeme
  | S_kwd_remove -> process @@ node -| S_wrap S_lexeme
  | S_kwd_set -> process @@ node -| S_wrap S_lexeme
  | S_kwd_skip -> process @@ node -| S_wrap S_lexeme
  | S_kwd_step -> process @@ node -| S_wrap S_lexeme
  | S_kwd_then -> process @@ node -| S_wrap S_lexeme
  | S_kwd_to -> process @@ node -| S_wrap S_lexeme
  | S_kwd_type -> process @@ node -| S_wrap S_lexeme
  | S_kwd_var -> process @@ node -| S_wrap S_lexeme
  | S_kwd_while -> process @@ node -| S_wrap S_lexeme
  | S_kwd_with -> process @@ node -| S_wrap S_lexeme
  | S_language -> process @@ node -| S_wrap (S_reg S_lexeme)
  | S_lbrace -> process @@ node -| S_wrap S_lexeme
  | S_lbracket -> process @@ node -| S_wrap S_lexeme
  | S_leq -> process @@ node -| S_wrap S_lexeme
  | S_lexeme -> () (* Leaf *)
  | S_list sing -> process_list @@ List.map ~f:(fun x -> x -| sing) node
  | S_lpar -> process @@ node -| S_wrap S_lexeme
  | S_lt -> process @@ node -| S_wrap S_lexeme
  | S_map_lookup ->
    let { map; keys } = node in
    process_list
    [ map -| S_expr
    ; keys -| S_nseq (S_reg (S_brackets S_expr)) ]
  | S_minus -> process @@ node -| S_wrap S_lexeme
  | S_minus_eq -> process @@ node -| S_wrap S_lexeme
  | S_module_body ->
    let { enclosing; declarations } = node in
    process_list
    [ enclosing -| S_block_enclosing
    ; declarations -| S_declarations ]
  | S_module_decl ->
    let { kwd_module; name; kwd_is; module_expr; terminator } = node in
    process_list
    [ kwd_module -| S_kwd_module
    ; name -| S_module_name
    ; kwd_is -| S_kwd_is
    ; module_expr -| S_module_expr
    ; terminator -| S_option S_semi ]
  | S_module_expr -> process
    (match node with
      M_Body node -> node -| S_reg S_module_body
    | M_Path node -> node -| S_reg (S_module_path S_module_name)
    | M_Var node -> node -| S_module_name)
  | S_module_name -> process @@ node -| S_wrap S_lexeme
  | S_module_path sing ->
    let { module_path; selector; field } = node in
    process_list
    [ module_path -| S_nsepseq (S_module_name, S_dot)
    ; selector -| S_dot
    ; field -| sing ]
  | S_neq -> process @@ node -| S_wrap S_lexeme
  | S_nsepseq (sing_1, sing_2) -> process @@ node -|
    S_tuple_2 (sing_1, S_list (S_tuple_2 (sing_2, sing_1)))
  | S_nseq sing -> process @@ node -| S_tuple_2 (sing, S_list sing)
  | S_option sing ->
    (match node with
      None -> ()
    | Some node -> process @@ node -| sing)
  | S_par sing ->
    let { lpar; inside; rpar } = node in
    process_list
    [ lpar -| S_lpar
    ; inside -| sing
    ; rpar -| S_rpar ]
  | S_param_decl ->
    let { param_kind; pattern; param_type } = node in
    process_list
    [ (match param_kind with
        `Var node -> node -| S_kwd_var
      | `Const node -> node -| S_kwd_const)
    ; pattern -| S_pattern
    ; param_type -| S_option S_type_annotation ]
  | S_parameters ->
    process @@ node -| S_reg (S_par (S_sepseq (S_reg S_param_decl , S_comma)))
  | S_patch ->
    let { kwd_patch; collection; kwd_with; patch_kind; patch } = node in
    process_list
    [ kwd_patch -| S_kwd_patch
    ; collection -| S_expr
    ; kwd_with -| S_kwd_with
    ; patch_kind -| S_patch_kind
    ; patch -| S_expr ]
  | S_patch_kind -> process
    (match node with
      `Map node -> node -| S_kwd_map
    | `Record node -> node -| S_kwd_record
    | `Set node -> node -| S_kwd_set)
  | S_pattern -> process
    (match node with
      P_App node ->
      node -| S_reg (S_tuple_2 (S_pattern, S_option (S_tuple S_pattern)))
    | P_Attr node -> node -| S_tuple_2 (S_attribute, S_pattern)
    | P_Bytes node -> node -| S_wrap (S_tuple_2 (S_lexeme, S_hex))
    | P_Cons node -> node -| S_reg (S_tuple_3 (S_pattern, S_sharp, S_pattern))
    | P_Ctor node -> node -| S_ctor
    | P_Int node -> node -| S_wrap (S_tuple_2 (S_lexeme, S_z))
    | P_List node -> node -| S_reg (S_compound S_pattern)
    | P_ModPath node -> node -| S_reg (S_module_path S_pattern)
    | P_Mutez node -> node -| S_wrap (S_tuple_2 (S_lexeme, S_int64))
    | P_Nat node -> node -| S_wrap (S_tuple_2 (S_lexeme, S_z))
    | P_Nil node -> node -| S_kwd_nil
    | P_Par node -> node -| S_reg (S_par S_pattern)
    | P_Record node -> node -| S_record_pattern
    | P_String node -> node -| S_string
    | P_Tuple node -> node -| S_tuple S_pattern
    | P_Typed node -> node -| S_reg S_typed_pattern
    | P_Var node -> node -| S_variable
    | P_Verbatim node -> node -| S_verbatim)
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
    ; pun -| sing ]
  | S_rbrace -> process @@ node -| S_wrap S_lexeme
  | S_rbracket -> process @@ node -| S_wrap S_lexeme
  | S_record_expr ->
    process @@ node -| S_reg (S_compound (S_reg (S_field (S_expr, S_expr))))
  | S_record_pattern ->
    process @@ node -| S_reg (S_compound (S_reg S_field_pattern))
  | S_reg sing ->
    let { region; value } = node in
    process_list
    [ region -| S_region
    ; value -| sing ]
  | S_region -> () (* Leaf *)
  | S_removal ->
    let { kwd_remove; item; kwd_from; remove_kind; collection } = node in
    process_list
    [ kwd_remove -| S_kwd_remove
    ; item -| S_expr
    ; kwd_from -| S_kwd_from
    ; (match remove_kind with
        `Set node -> node -| S_kwd_set
      | `Map node -> node -| S_kwd_map)
    ; collection -| S_expr ]
  | S_rpar -> process @@ node -| S_wrap S_lexeme
  | S_selection -> process
    (match node with
      FieldName node -> node -| S_field_name
    | Component node -> node -| S_wrap (S_tuple_2 (S_lexeme, S_z)))
  | S_semi -> process @@ node -| S_wrap S_lexeme
  | S_sepseq (sing_1, sing_2) -> process @@ node -|
    S_option (S_nsepseq (sing_1, sing_2))
  | S_set_membership ->
    let { set; kwd_contains; element } = node in
    process_list
    [ set -| S_expr
    ; kwd_contains -| S_kwd_contains
    ; element -| S_expr ]
  | S_sharp -> process @@ node -| S_wrap S_lexeme
  | S_slash -> process @@ node -| S_wrap S_lexeme
  | S_slash_eq -> process @@ node -| S_wrap S_lexeme
  | S_statement -> process
    (match node with
      S_Attr node -> node -| S_tuple_2 (S_attribute, S_statement)
    | S_Decl node -> node -| S_declaration
    | S_Instr node -> node -| S_instruction
    | S_VarDecl node -> node -| S_reg S_var_decl)
  | S_statements -> process @@ node -| S_nsepseq (S_statement, S_semi)
  | S_string -> process @@ node -| S_wrap S_lexeme
  | S_sum_type ->
    let { lead_vbar; variants } = node in
    process_list
    [ lead_vbar -| S_option S_vbar
    ; variants -| S_nsepseq (S_reg S_variant, S_vbar) ]
  | S_test_clause -> process
    (match node with
      ClauseInstr node -> node -| S_instruction
    | ClauseBlock node -> node -| S_reg S_block)
  | S_times -> process @@ node -| S_wrap S_lexeme
  | S_times_eq -> process @@ node -| S_wrap S_lexeme
  | S_tuple sing -> process @@ node -| S_reg (S_par (S_nsepseq (sing, S_comma)))
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
  | S_type_decl ->
    let { kwd_type; name; params; kwd_is; type_expr; terminator } = node in
    process_list
    [ kwd_type -| S_kwd_type
    ; name -| S_type_name
    ; params -| S_option (S_tuple S_type_name)
    ; kwd_is -| S_kwd_is
    ; type_expr -| S_type_expr
    ; terminator -| S_option S_semi ]
  | S_type_expr -> process
    (match node with
      T_App node -> node -| S_reg (S_tuple_2 (S_type_expr, S_type_tuple))
    | T_Attr node -> node -| S_tuple_2 (S_attribute, S_type_expr)
    | T_Cart node -> node -| S_cartesian
    | T_Fun node ->
      node -| S_reg (S_tuple_3 (S_type_expr, S_arrow, S_type_expr))
    | T_Int node -> node -| S_wrap (S_tuple_2 (S_lexeme, S_z))
    | T_ModPath node -> node -| S_reg (S_module_path S_type_expr)
    | T_Par node -> node -| S_reg (S_par S_type_expr)
    | T_Record node -> node -| S_reg (S_compound (S_reg S_field_decl))
    | T_String node -> node -| S_string
    | T_Sum node -> node -| S_reg S_sum_type
    | T_Var node -> node -| S_type_name)
  | S_type_name -> process @@ node -| S_wrap S_lexeme
  | S_type_params -> process @@ node -| S_nsepseq (S_type_name, S_comma)
  | S_type_tuple -> process @@ node -| S_tuple S_type_expr
  | S_typed_expr -> process @@ node -| S_tuple_2 (S_expr, S_type_annotation)
  | S_typed_pattern ->
    let { pattern; type_annot } = node in
    process_list
    [ pattern -| S_pattern
    ; type_annot -| S_type_annotation ]
  | S_un_op sing ->
    let { op; arg } = node in
    process_list
    [ op -| sing
    ; arg -| S_expr ]
  | S_update ->
    let { structure; kwd_with; update } = node in
    process_list
    [ structure -| S_expr
    ; kwd_with -| S_kwd_with
    ; update -| S_expr ]
  | S_var_decl ->
    let { kwd_var
      ; pattern
      ; type_params
      ; var_type
      ; assign
      ; init
      ; terminator } = node in
    process_list
    [ kwd_var -| S_kwd_var
    ; pattern -| S_pattern
    ; type_params -| S_option (S_reg (S_chevrons S_type_params))
    ; var_type -| S_option S_type_annotation
    ; assign -| S_assign
    ; init -| S_expr
    ; terminator -| S_option S_semi ]
  | S_variable -> process @@ node -| S_wrap S_lexeme
  | S_variant ->
    let { attributes; ctor; ctor_args } = node in
    process_list
    [ attributes -| S_list S_attribute
    ; ctor -| S_ctor
    ; ctor_args -| S_option (S_tuple_2 (S_kwd_of, S_type_expr)) ]
  | S_vbar -> process @@ node -| S_wrap S_lexeme
  | S_vbar_eq -> process @@ node -| S_wrap S_lexeme
  | S_verbatim -> process @@ node -| S_wrap S_lexeme
  | S_while_loop ->
    let { kwd_while; cond; block } = node in
    process_list
    [ kwd_while -| S_kwd_while
    ; cond -| S_expr
    ; block -| S_reg S_block ]
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
