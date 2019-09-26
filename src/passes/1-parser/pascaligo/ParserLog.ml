[@@@warning "-42"]

open Utils
open AST
open! Region

(* Printing the tokens with their source regions *)

let printf = Printf.printf

let offsets = ref true

let mode = ref `Point

let compact (region: Region.t) =
  region#compact ~offsets:!offsets !mode

let print_nsepseq :
  string -> ('a -> unit) -> ('a, Region.t) nsepseq -> unit =
  fun sep visit (head, tail) ->
    let print_aux (sep_reg, item) =
      printf "%s: %s\n" (compact sep_reg) sep;
      visit item
    in visit head; List.iter print_aux tail

let print_sepseq :
  string -> ('a -> unit) -> ('a, Region.t) sepseq -> unit =
  fun sep visit -> function
        None -> ()
  | Some seq -> print_nsepseq sep visit seq

let print_token region lexeme =
  printf "%s: %s\n"(compact region) lexeme

let print_var {region; value=lexeme} =
  printf "%s: Ident \"%s\"\n" (compact region) lexeme

let print_constr {region; value=lexeme} =
  printf "%s: Constr \"%s\"\n"
         (compact region) lexeme

let print_string {region; value=lexeme} =
  printf "%s: String %s\n"
         (compact region) lexeme

let print_bytes {region; value = lexeme, abstract} =
  printf "%s: Bytes (\"%s\", \"0x%s\")\n"
         (compact region) lexeme
         (Hex.to_string abstract)

let print_int {region; value = lexeme, abstract} =
  printf "%s: Int (\"%s\", %s)\n"
         (compact region) lexeme
         (Z.to_string abstract)

(* Main printing function *)

let rec print_tokens ast =
  let {decl; eof} = ast in
  Utils.nseq_iter print_decl decl;
  print_token eof "EOF"

and print_decl = function
  TypeDecl   decl -> print_type_decl       decl
| ConstDecl  decl -> print_const_decl      decl
| LambdaDecl decl -> print_lambda_decl     decl

and print_const_decl {value; _} =
  let {kwd_const; name; colon; const_type;
       equal; init; terminator} = value in
  print_token      kwd_const "const";
  print_var        name;
  print_token      colon ":";
  print_type_expr  const_type;
  print_token      equal "=";
  print_expr       init;
  print_terminator terminator

and print_type_decl {value; _} =
  let {kwd_type; name; kwd_is;
       type_expr; terminator} = value in
  print_token      kwd_type "type";
  print_var        name;
  print_token      kwd_is "is";
  print_type_expr  type_expr;
  print_terminator terminator

and print_type_expr = function
  TProd   cartesian   -> print_cartesian   cartesian
| TSum    sum_type    -> print_sum_type    sum_type
| TRecord record_type -> print_record_type record_type
| TApp    type_app    -> print_type_app    type_app
| TFun    type_fun    -> print_type_fun    type_fun
| TPar    par_type    -> print_par_type    par_type
| TAlias  type_alias  -> print_var         type_alias

and print_cartesian {value; _} =
  print_nsepseq "*" print_type_expr value

and print_variant {value; _} =
  let {constr; args} = value in
  print_constr    constr;
  match args with
    None -> ()
  | Some (kwd_of, product) ->
      print_token     kwd_of "of";
      print_cartesian product

and print_sum_type {value; _} =
  print_nsepseq "|" print_variant value

and print_record_type record_type =
  print_injection "record" print_field_decl record_type

and print_type_app {value; _} =
  let type_name, type_tuple = value in
  print_var        type_name;
  print_type_tuple type_tuple

and print_type_fun {value; _} =
  let type_expr_a, arrow, type_expr_b = value in
  print_type_expr  type_expr_a;
  print_token      arrow "->";
  print_type_expr  type_expr_b

and print_par_type {value; _} =
  let {lpar; inside; rpar} = value in
  print_token     lpar "(";
  print_type_expr inside;
  print_token     rpar ")"

and print_field_decl {value; _} =
  let {field_name; colon; field_type} = value in
  print_var       field_name;
  print_token     colon ":";
  print_type_expr field_type

and print_type_tuple {value; _} =
  let {lpar; inside; rpar} = value in
  print_token lpar "(";
  print_nsepseq "," print_type_expr inside;
  print_token rpar ")"

and print_lambda_decl = function
  FunDecl     fun_decl -> print_fun_decl   fun_decl
| ProcDecl   proc_decl -> print_proc_decl  proc_decl

and print_fun_decl {value; _} =
  let {kwd_function; name; param; colon;
       ret_type; kwd_is; local_decls;
       block; kwd_with; return; terminator} = value in
  print_token       kwd_function "function";
  print_var         name;
  print_parameters  param;
  print_token       colon ":";
  print_type_expr   ret_type;
  print_token       kwd_is "is";
  print_local_decls local_decls;
  print_block       block;
  print_token       kwd_with "with";
  print_expr        return;
  print_terminator  terminator

and print_proc_decl {value; _} =
  let {kwd_procedure; name; param; kwd_is;
       local_decls; block; terminator} = value in
  print_token       kwd_procedure "procedure";
  print_var         name;
  print_parameters  param;
  print_token       kwd_is "is";
  print_local_decls local_decls;
  print_block       block;
  print_terminator  terminator

and print_parameters {value; _} =
  let {lpar; inside; rpar} = value in
  print_token lpar "(";
  print_nsepseq ";" print_param_decl inside;
  print_token rpar ")"

and print_param_decl = function
  ParamConst param_const -> print_param_const param_const
| ParamVar   param_var   -> print_param_var   param_var

and print_param_const {value; _} =
  let {kwd_const; var; colon; param_type} = value in
  print_token     kwd_const "const";
  print_var       var;
  print_token     colon ":";
  print_type_expr param_type

and print_param_var {value; _} =
  let {kwd_var; var; colon; param_type} = value in
  print_token     kwd_var "var";
  print_var       var;
  print_token     colon ":";
  print_type_expr param_type

and print_block {value; _} =
  let {opening; statements; terminator; closing} = value in
  print_block_opening opening;
  print_statements    statements;
  print_terminator    terminator;
  print_block_closing closing

and print_block_opening = function
  Block (kwd_block, lbrace) -> print_token kwd_block "block";
                              print_token lbrace    "{"
| Begin kwd_begin           -> print_token kwd_begin "begin"

and print_block_closing = function
  Block rbrace -> print_token rbrace "}"
| End kwd_end  -> print_token kwd_end "end"

and print_local_decls sequence =
  List.iter print_local_decl sequence

and print_local_decl = function
  LocalFun  decl -> print_fun_decl  decl
| LocalProc decl -> print_proc_decl decl
| LocalData decl -> print_data_decl decl

and print_data_decl = function
  LocalConst decl -> print_const_decl decl
| LocalVar   decl -> print_var_decl   decl

and print_var_decl {value; _} =
  let {kwd_var; name; colon; var_type;
       assign; init; terminator} = value in
  print_token      kwd_var "var";
  print_var        name;
  print_token      colon ":";
  print_type_expr  var_type;
  print_token      assign ":=";
  print_expr       init;
  print_terminator terminator

and print_statements sequence =
  print_nsepseq ";" print_statement sequence

and print_statement = function
  Instr instr -> print_instruction instr
| Data data -> print_data_decl data

and print_instruction = function
  Single instr -> print_single_instr instr
| Block block -> print_block block

and print_single_instr = function
  Cond        {value; _} -> print_conditional value
| CaseInstr   {value; _} -> print_case_instr value
| Assign      assign     -> print_assignment assign
| Loop        loop       -> print_loop loop
| ProcCall    fun_call   -> print_fun_call fun_call
| Fail        {value; _} -> print_fail value
| Skip        kwd_skip   -> print_token kwd_skip "skip"
| RecordPatch {value; _} -> print_record_patch value
| MapPatch    {value; _} -> print_map_patch value
| SetPatch    {value; _} -> print_set_patch value
| MapRemove   {value; _} -> print_map_remove value
| SetRemove   {value; _} -> print_set_remove value

and print_fail {kwd_fail; fail_expr} =
  print_token kwd_fail "fail";
  print_expr fail_expr

and print_conditional node =
  let {kwd_if; test; kwd_then; ifso; terminator;
       kwd_else; ifnot} = node in
  print_token      kwd_if "if";
  print_expr       test;
  print_token      kwd_then "then";
  print_if_clause  ifso;
  print_terminator terminator;
  print_token      kwd_else "else";
  print_if_clause  ifnot

and print_if_clause = function
  ClauseInstr instr -> print_instruction instr
| ClauseBlock {value; _} ->
    let {lbrace; inside; rbrace} = value in
    let statements, terminator = inside in
    print_token lbrace "{";
    print_statements statements;
    print_terminator terminator;
    print_token rbrace "}"

and print_case_instr (node : instruction case) =
  let {kwd_case; expr; opening;
       lead_vbar; cases; closing} = node in
  print_token kwd_case "case";
  print_expr  expr;
  print_opening "of" opening;
  print_token_opt lead_vbar "|";
  print_cases_instr cases;
  print_closing closing

and print_token_opt = function
         None -> fun _ -> ()
| Some region -> print_token region

and print_cases_instr {value; _} =
  print_nsepseq "|" print_case_clause_instr value

and print_case_clause_instr {value; _} =
  let {pattern; arrow; rhs} = value in
  print_pattern pattern;
  print_token arrow "->";
  print_instruction rhs

and print_assignment {value; _} =
  let {lhs; assign; rhs} = value in
  print_lhs lhs;
  print_token assign ":=";
  print_rhs rhs

and print_rhs = function
      Expr e -> print_expr e
| NoneExpr r -> print_token r "None"

and print_lhs = function
  Path path -> print_path path
| MapPath {value; _} -> print_map_lookup value

and print_loop = function
  While {value; _} -> print_while_loop value
| For     for_loop -> print_for_loop for_loop

and print_while_loop value =
  let {kwd_while; cond; block} = value in
  print_token kwd_while "while";
  print_expr cond;
  print_block block

and print_for_loop = function
  ForInt     for_int     -> print_for_int for_int
| ForCollect for_collect -> print_for_collect for_collect

and print_for_int ({value; _} : for_int reg) =
  let {kwd_for; assign; down; kwd_to;
       bound; step; block} = value in
  print_token      kwd_for "for";
  print_var_assign assign;
  print_down       down;
  print_token      kwd_to "to";
  print_expr       bound;
  print_step       step;
  print_block      block

and print_var_assign {value; _} =
  let {name; assign; expr} = value in
  print_var name;
  print_token assign ":=";
  print_expr expr

and print_down = function
  Some kwd_down -> print_token kwd_down "down"
| None -> ()

and print_step = function
  Some (kwd_step, expr) ->
    print_token kwd_step "step";
    print_expr expr
| None -> ()

and print_for_collect ({value; _} : for_collect reg) =
  let {kwd_for; var; bind_to; kwd_in; expr; block} = value in
  print_token   kwd_for "for";
  print_var     var;
  print_bind_to bind_to;
  print_token   kwd_in "in";
  print_expr    expr;
  print_block   block

and print_bind_to = function
  Some (arrow, variable) ->
    print_token arrow "->";
    print_var   variable
| None -> ()

and print_expr = function
  ECase   {value;_} -> print_case_expr value
| EAnnot  {value;_} -> print_annot_expr value
| ELogic  e -> print_logic_expr e
| EArith  e -> print_arith_expr e
| EString e -> print_string_expr e
| EList   e -> print_list_expr e
| ESet    e -> print_set_expr e
| EConstr e -> print_constr_expr e
| ERecord e -> print_record_expr e
| EProj   e -> print_projection e
| EMap    e -> print_map_expr e
| EVar    v -> print_var v
| ECall   e -> print_fun_call e
| EBytes  b -> print_bytes b
| EUnit   r -> print_token r "Unit"
| ETuple  e -> print_tuple_expr e
| EPar    e -> print_par_expr e

and print_annot_expr (expr , type_expr) =
  print_expr expr ;
  print_type_expr type_expr

and print_case_expr (node : expr case) =
  let {kwd_case; expr; opening;
       lead_vbar; cases; closing} = node in
  print_token kwd_case "case";
  print_expr  expr;
  print_opening "of" opening;
  print_token_opt lead_vbar "|";
  print_cases_expr cases;
  print_closing closing

and print_cases_expr {value; _} =
  print_nsepseq "|" print_case_clause_expr value

and print_case_clause_expr {value; _} =
  let {pattern; arrow; rhs} = value in
  print_pattern pattern;
  print_token arrow "->";
  print_expr rhs

and print_map_expr = function
  MapLookUp {value; _} -> print_map_lookup value
| MapInj inj           -> print_injection "map" print_binding inj

and print_set_expr = function
  SetInj inj -> print_injection "set" print_expr inj
| SetMem mem -> print_set_membership mem

and print_set_membership {value; _} =
  let {set; kwd_contains; element} = value in
  print_expr set;
  print_token kwd_contains "contains";
  print_expr element

and print_map_lookup {path; index} =
  let {lbracket; inside; rbracket} = index.value in
  print_path  path;
  print_token lbracket "[";
  print_expr  inside;
  print_token rbracket "]"

and print_path = function
  Name var  -> print_var var
| Path path -> print_projection path

and print_logic_expr = function
  BoolExpr e -> print_bool_expr e
| CompExpr e -> print_comp_expr e

and print_bool_expr = function
  Or {value = {arg1; op; arg2}; _} ->
    print_expr arg1; print_token op "||"; print_expr arg2
| And {value = {arg1; op; arg2}; _} ->
    print_expr arg1; print_token op "&&"; print_expr arg2
| Not {value = {op; arg}; _} ->
    print_token op "not"; print_expr arg
| False region -> print_token region "False"
| True region  -> print_token region "True"

and print_comp_expr = function
  Lt {value = {arg1; op; arg2}; _} ->
    print_expr arg1; print_token op "<"; print_expr arg2
| Leq {value = {arg1; op; arg2}; _} ->
    print_expr arg1; print_token op "<="; print_expr arg2
| Gt {value = {arg1; op; arg2}; _} ->
    print_expr arg1; print_token op ">"; print_expr arg2
| Geq {value = {arg1; op; arg2}; _} ->
    print_expr arg1; print_token op ">="; print_expr arg2
| Equal {value = {arg1; op; arg2}; _} ->
    print_expr arg1; print_token op "="; print_expr arg2
| Neq {value = {arg1; op; arg2}; _} ->
    print_expr arg1; print_token op "=/="; print_expr arg2

and print_arith_expr = function
  Add {value = {arg1; op; arg2}; _} ->
    print_expr arg1; print_token op "+"; print_expr arg2
| Sub {value = {arg1; op; arg2}; _} ->
    print_expr arg1; print_token op "-"; print_expr arg2
| Mult {value = {arg1; op; arg2}; _} ->
    print_expr arg1; print_token op "*"; print_expr arg2
| Div {value = {arg1; op; arg2}; _} ->
    print_expr arg1; print_token op "/"; print_expr arg2
| Mod {value = {arg1; op; arg2}; _} ->
    print_expr arg1; print_token op "mod"; print_expr arg2
| Neg {value = {op; arg}; _} ->
    print_token op "-"; print_expr arg
| Int i
| Nat i
| Mtz i -> print_int i

and print_string_expr = function
  Cat {value = {arg1; op; arg2}; _} ->
    print_expr arg1; print_token op "^"; print_expr arg2
| String s -> print_string s

and print_list_expr = function
  Cons {value = {arg1; op; arg2}; _} ->
    print_expr arg1; print_token op "#"; print_expr arg2
| List e -> print_injection "list" print_expr e
| Nil  e -> print_nil e

and print_constr_expr = function
  SomeApp e   -> print_some_app e
| NoneExpr e  -> print_none_expr e
| ConstrApp e -> print_constr_app e

and print_record_expr e =
  print_injection "record" print_field_assign e

and print_field_assign {value; _} =
  let {field_name; equal; field_expr} = value in
  print_var field_name;
  print_token equal "=";
  print_expr field_expr

and print_projection {value; _} =
  let {struct_name; selector; field_path} = value in
  print_var struct_name;
  print_token selector ".";
  print_field_path field_path

and print_field_path sequence =
  print_nsepseq "." print_selection sequence

and print_selection = function
  FieldName name -> print_var name
| Component int  -> print_int int

and print_record_patch node =
  let {kwd_patch; path; kwd_with; record_inj} = node in
  print_token kwd_patch "patch";
  print_path  path;
  print_token kwd_with "with";
  print_record_expr record_inj

and print_set_patch node =
  let {kwd_patch; path; kwd_with; set_inj} = node in
  print_token kwd_patch "patch";
  print_path path;
  print_token kwd_with "with";
  print_injection "set" print_expr set_inj

and print_map_patch node =
  let {kwd_patch; path; kwd_with; map_inj} = node in
  print_token kwd_patch "patch";
  print_path path;
  print_token kwd_with "with";
  print_injection "map" print_binding map_inj

and print_map_remove node =
  let {kwd_remove; key; kwd_from; kwd_map; map} = node in
  print_token kwd_remove "remove";
  print_expr  key;
  print_token kwd_from "from";
  print_token kwd_map "map";
  print_path  map

and print_set_remove node =
  let {kwd_remove; element; kwd_from; kwd_set; set} = node in
  print_token kwd_remove "remove";
  print_expr  element;
  print_token kwd_from "from";
  print_token kwd_set "set";
  print_path  set

and print_injection :
  'a.string -> ('a -> unit) -> 'a injection reg -> unit =
  fun kwd print {value; _} ->
    let {opening; elements; terminator; closing} = value in
    print_opening kwd opening;
    print_sepseq ";" print elements;
    print_terminator terminator;
    print_closing closing

and print_opening lexeme = function
  Kwd kwd -> print_token kwd lexeme
| KwdBracket (kwd, lbracket) ->
    print_token kwd lexeme;
    print_token lbracket "{"

and print_closing = function
  RBracket rbracket -> print_token rbracket "}"
| End kwd_end       -> print_token kwd_end "end"

and print_binding {value; _} =
  let {source; arrow; image} = value in
  print_expr source;
  print_token arrow "->";
  print_expr image

and print_tuple_expr = function
  TupleInj inj -> print_tuple_inj inj

and print_tuple_inj {value; _} =
  let {lpar; inside; rpar} = value in
  print_token lpar "(";
  print_nsepseq "," print_expr inside;
  print_token rpar ")"

and print_nil value =
  print_token     value "nil";

and print_none_expr value =
  print_token     value "None";

and print_fun_call {value; _} =
  let fun_name, arguments = value in
  print_var       fun_name;
  print_tuple_inj arguments

and print_constr_app {value; _} =
  let constr, arguments = value in
  print_constr    constr;
  match arguments with
    None -> ()
  | Some args -> print_tuple_inj args

and print_some_app {value; _} =
  let c_Some, arguments = value in
  print_token c_Some "Some";
  print_tuple_inj arguments

and print_par_expr {value; _} =
  let {lpar; inside; rpar} = value in
  print_token lpar "(";
  print_expr  inside;
  print_token rpar ")"

and print_pattern = function
  PCons {value; _} -> print_nsepseq "#" print_pattern value
| PVar var         -> print_var var
| PWild wild       -> print_token wild "_"
| PInt i           -> print_int i
| PBytes b         -> print_bytes b
| PString s        -> print_string s
| PUnit region     -> print_token region "Unit"
| PFalse region    -> print_token region "False"
| PTrue region     -> print_token region "True"
| PNone region     -> print_token region "None"
| PSome psome      -> print_psome psome
| PList pattern    -> print_list_pattern pattern
| PTuple ptuple    -> print_ptuple ptuple
| PConstr pattern  -> print_constr_pattern pattern

and print_constr_pattern {value; _} =
  let (constr, args) = value in
  print_constr constr;
  match args with
    None -> ()
  | Some tuple -> print_ptuple tuple

and print_psome {value; _} =
  let c_Some, patterns = value in
  print_token    c_Some "Some";
  print_patterns patterns

and print_patterns {value; _} =
  let {lpar; inside; rpar} = value in
  print_token lpar "(";
  print_pattern inside;
  print_token rpar ")"

and print_list_pattern = function
  Sugar  sugar -> print_injection "list" print_pattern sugar
| PNil kwd_nil -> print_token kwd_nil "nil"
| Raw      raw -> print_raw raw

and print_raw {value; _} =
  let {lpar; inside; rpar} = value in
  let head, cons, tail = inside in
  print_token   lpar "(";
  print_pattern head;
  print_token   cons "#";
  print_pattern tail;
  print_token   rpar ")"

and print_ptuple {value; _} =
  let {lpar; inside; rpar} = value in
  print_token lpar "(";
  print_nsepseq "," print_pattern inside;
  print_token rpar ")"

and print_terminator = function
  Some semi -> print_token semi ";"
| None -> ()
