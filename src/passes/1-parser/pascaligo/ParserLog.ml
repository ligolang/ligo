[@@@warning "-42"]

open Utils
open AST
open! Region

(* Printing the tokens with their source regions *)

let sprintf = Printf.sprintf

let offsets = ref true
let mode    = ref `Point

let compact (region: Region.t) =
  region#compact ~offsets:!offsets !mode

let print_nsepseq :
  Buffer.t -> string -> (Buffer.t -> 'a -> unit) ->
  ('a, Region.t) nsepseq -> unit =
  fun buffer sep print (head, tail) ->
    let print_aux (sep_reg, item) =
      let sep_line = sprintf "%s: %s\n" (compact sep_reg) sep in
      Buffer.add_string buffer sep_line;
      print buffer item
    in print buffer head; List.iter print_aux tail

let print_sepseq :
  Buffer.t -> string -> (Buffer.t -> 'a -> unit) ->
  ('a, Region.t) sepseq -> unit =
  fun buffer sep print -> function
        None -> ()
  | Some seq -> print_nsepseq buffer sep print seq

let print_token buffer region lexeme =
  let line = sprintf "%s: %s\n"(compact region) lexeme
  in Buffer.add_string buffer line

let print_var buffer {region; value=lexeme} =
  let line = sprintf "%s: Ident \"%s\"\n"
               (compact region) lexeme
  in Buffer.add_string buffer line

let print_constr buffer {region; value=lexeme} =
  let line = sprintf "%s: Constr \"%s\"\n"
               (compact region) lexeme
  in Buffer.add_string buffer line

let print_string buffer {region; value=lexeme} =
  let line = sprintf "%s: String %s\n"
               (compact region) lexeme
  in Buffer.add_string buffer line

let print_bytes buffer {region; value = lexeme, abstract} =
  let line = sprintf "%s: Bytes (\"%s\", \"0x%s\")\n"
               (compact region) lexeme
               (Hex.to_string abstract)
  in Buffer.add_string buffer line

let print_int buffer {region; value = lexeme, abstract} =
  let line = sprintf "%s: Int (\"%s\", %s)\n"
               (compact region) lexeme
               (Z.to_string abstract)
  in Buffer.add_string buffer line


(* Main printing function *)

let rec print_tokens buffer ast =
  let {decl; eof} = ast in
  Utils.nseq_iter (print_decl buffer) decl;
  print_token buffer eof "EOF"

and print_decl buffer = function
  TypeDecl   decl -> print_type_decl   buffer decl
| ConstDecl  decl -> print_const_decl  buffer decl
| LambdaDecl decl -> print_lambda_decl buffer decl

and print_const_decl buffer {value; _} =
  let {kwd_const; name; colon; const_type;
       equal; init; terminator} = value in
  print_token      buffer kwd_const "const";
  print_var        buffer name;
  print_token      buffer colon ":";
  print_type_expr  buffer const_type;
  print_token      buffer equal "=";
  print_expr       buffer init;
  print_terminator buffer terminator

and print_type_decl buffer {value; _} =
  let {kwd_type; name; kwd_is;
       type_expr; terminator} = value in
  print_token      buffer kwd_type "type";
  print_var        buffer name;
  print_token      buffer kwd_is "is";
  print_type_expr  buffer type_expr;
  print_terminator buffer terminator

and print_type_expr buffer = function
  TProd   cartesian   -> print_cartesian   buffer cartesian
| TSum    sum_type    -> print_sum_type    buffer sum_type
| TRecord record_type -> print_record_type buffer record_type
| TApp    type_app    -> print_type_app    buffer type_app
| TFun    type_fun    -> print_type_fun    buffer type_fun
| TPar    par_type    -> print_par_type    buffer par_type
| TAlias  type_alias  -> print_var         buffer type_alias

and print_cartesian buffer {value; _} =
  print_nsepseq buffer "*" print_type_expr value

and print_variant buffer {value; _} =
  let {constr; args} = value in
  print_constr buffer constr;
  match args with
    None -> ()
  | Some (kwd_of, product) ->
      print_token     buffer kwd_of "of";
      print_cartesian buffer product

and print_sum_type buffer {value; _} =
  print_nsepseq buffer "|" print_variant value

and print_record_type buffer record_type =
  print_injection buffer "record" print_field_decl record_type

and print_type_app buffer {value; _} =
  let type_name, type_tuple = value in
  print_var        buffer type_name;
  print_type_tuple buffer type_tuple

and print_type_fun buffer {value; _} =
  let type_expr_a, arrow, type_expr_b = value in
  print_type_expr  buffer type_expr_a;
  print_token      buffer arrow "->";
  print_type_expr  buffer type_expr_b

and print_par_type buffer {value; _} =
  let {lpar; inside; rpar} = value in
  print_token     buffer lpar "(";
  print_type_expr buffer inside;
  print_token     buffer rpar ")"

and print_field_decl buffer {value; _} =
  let {field_name; colon; field_type} = value in
  print_var       buffer field_name;
  print_token     buffer colon ":";
  print_type_expr buffer field_type

and print_type_tuple buffer {value; _} =
  let {lpar; inside; rpar} = value in
  print_token buffer lpar "(";
  print_nsepseq buffer "," print_type_expr inside;
  print_token buffer rpar ")"

and print_lambda_decl buffer = function
  FunDecl     fun_decl -> print_fun_decl   buffer fun_decl
| ProcDecl   proc_decl -> print_proc_decl  buffer proc_decl

and print_fun_decl buffer {value; _} =
  let {kwd_function; name; param; colon;
       ret_type; kwd_is; local_decls;
       block; kwd_with; return; terminator} = value in
  print_token       buffer kwd_function "function";
  print_var         buffer name;
  print_parameters  buffer param;
  print_token       buffer colon ":";
  print_type_expr   buffer ret_type;
  print_token       buffer kwd_is "is";
  print_local_decls buffer local_decls;
  print_block       buffer block;
  print_token       buffer kwd_with "with";
  print_expr        buffer return;
  print_terminator  buffer terminator

and print_proc_decl buffer {value; _} =
  let {kwd_procedure; name; param; kwd_is;
       local_decls; block; terminator} = value in
  print_token       buffer kwd_procedure "procedure";
  print_var         buffer name;
  print_parameters  buffer param;
  print_token       buffer kwd_is "is";
  print_local_decls buffer local_decls;
  print_block       buffer block;
  print_terminator  buffer terminator

and print_parameters buffer {value; _} =
  let {lpar; inside; rpar} = value in
  print_token buffer lpar "(";
  print_nsepseq buffer ";" print_param_decl inside;
  print_token buffer rpar ")"

and print_param_decl buffer = function
  ParamConst param_const -> print_param_const buffer param_const
| ParamVar   param_var   -> print_param_var   buffer param_var

and print_param_const buffer {value; _} =
  let {kwd_const; var; colon; param_type} = value in
  print_token     buffer kwd_const "const";
  print_var       buffer var;
  print_token     buffer colon ":";
  print_type_expr buffer param_type

and print_param_var buffer {value; _} =
  let {kwd_var; var; colon; param_type} = value in
  print_token     buffer kwd_var "var";
  print_var       buffer var;
  print_token     buffer colon ":";
  print_type_expr buffer param_type

and print_block buffer {value; _} =
  let {opening; statements; terminator; closing} = value in
  print_block_opening buffer opening;
  print_statements    buffer statements;
  print_terminator    buffer terminator;
  print_block_closing buffer closing

and print_block_opening buffer = function
  Block (kwd_block, lbrace) ->
    print_token buffer kwd_block "block";
    print_token buffer lbrace    "{"
| Begin kwd_begin ->
    print_token buffer kwd_begin "begin"

and print_block_closing buffer = function
  Block rbrace -> print_token buffer rbrace  "}"
| End kwd_end  -> print_token buffer kwd_end "end"

and print_local_decls buffer sequence =
  List.iter (print_local_decl buffer) sequence

and print_local_decl buffer = function
  LocalFun  decl -> print_fun_decl  buffer decl
| LocalProc decl -> print_proc_decl buffer decl
| LocalData decl -> print_data_decl buffer decl

and print_data_decl buffer = function
  LocalConst decl -> print_const_decl buffer decl
| LocalVar   decl -> print_var_decl   buffer decl

and print_var_decl buffer {value; _} =
  let {kwd_var; name; colon; var_type;
       assign; init; terminator} = value in
  print_token      buffer kwd_var "var";
  print_var        buffer name;
  print_token      buffer colon ":";
  print_type_expr  buffer var_type;
  print_token      buffer assign ":=";
  print_expr       buffer init;
  print_terminator buffer terminator

and print_statements buffer sequence =
  print_nsepseq buffer ";" print_statement sequence

and print_statement buffer = function
  Instr instr -> print_instruction buffer instr
| Data  data  -> print_data_decl   buffer data

and print_instruction buffer = function
  Single instr -> print_single_instr buffer instr
| Block  block -> print_block        buffer block

and print_single_instr buffer = function
  Cond        {value; _} -> print_conditional buffer value
| CaseInstr   {value; _} -> print_case_instr buffer value
| Assign      assign     -> print_assignment buffer assign
| Loop        loop       -> print_loop buffer loop
| ProcCall    fun_call   -> print_fun_call buffer fun_call
| Skip        kwd_skip   -> print_token buffer kwd_skip "skip"
| RecordPatch {value; _} -> print_record_patch buffer value
| MapPatch    {value; _} -> print_map_patch buffer value
| SetPatch    {value; _} -> print_set_patch buffer value
| MapRemove   {value; _} -> print_map_remove buffer value
| SetRemove   {value; _} -> print_set_remove buffer value

and print_conditional buffer node =
  let {kwd_if; test; kwd_then; ifso; terminator;
       kwd_else; ifnot} = node in
  print_token      buffer kwd_if "if";
  print_expr       buffer test;
  print_token      buffer kwd_then "then";
  print_if_clause  buffer ifso;
  print_terminator buffer terminator;
  print_token      buffer kwd_else "else";
  print_if_clause  buffer ifnot

and print_if_clause buffer = function
  ClauseInstr instr -> print_instruction buffer instr
| ClauseBlock {value; _} ->
    let {lbrace; inside; rbrace} = value in
    let statements, terminator = inside in
    print_token      buffer lbrace "{";
    print_statements buffer statements;
    print_terminator buffer terminator;
    print_token      buffer rbrace "}"

and print_case_instr buffer (node : instruction case) =
  let {kwd_case; expr; opening;
       lead_vbar; cases; closing} = node in
  print_token       buffer kwd_case "case";
  print_expr        buffer expr;
  print_opening     buffer "of" opening;
  print_token_opt   buffer lead_vbar "|";
  print_cases_instr buffer cases;
  print_closing     buffer closing

and print_token_opt buffer = function
         None -> fun _ -> ()
| Some region -> print_token buffer region

and print_cases_instr buffer {value; _} =
  print_nsepseq buffer "|" print_case_clause_instr value

and print_case_clause_instr buffer {value; _} =
  let {pattern; arrow; rhs} = value in
  print_pattern     buffer pattern;
  print_token       buffer arrow "->";
  print_instruction buffer rhs

and print_assignment buffer {value; _} =
  let {lhs; assign; rhs} = value in
  print_lhs   buffer lhs;
  print_token buffer assign ":=";
  print_rhs   buffer rhs

and print_rhs buffer e = print_expr buffer e

and print_lhs buffer = function
  Path path          -> print_path buffer path
| MapPath {value; _} -> print_map_lookup buffer value

and print_loop buffer = function
  While {value; _} -> print_while_loop buffer value
| For     for_loop -> print_for_loop   buffer for_loop

and print_while_loop buffer value =
  let {kwd_while; cond; block} = value in
  print_token buffer kwd_while "while";
  print_expr  buffer cond;
  print_block buffer block

and print_for_loop buffer = function
  ForInt     for_int     -> print_for_int     buffer for_int
| ForCollect for_collect -> print_for_collect buffer for_collect

and print_for_int buffer ({value; _} : for_int reg) =
  let {kwd_for; assign; down; kwd_to;
       bound; step; block} = value in
  print_token      buffer kwd_for "for";
  print_var_assign buffer assign;
  print_down       buffer down;
  print_token      buffer kwd_to "to";
  print_expr       buffer bound;
  print_step       buffer step;
  print_block      buffer block

and print_var_assign buffer {value; _} =
  let {name; assign; expr} = value in
  print_var   buffer name;
  print_token buffer assign ":=";
  print_expr  buffer expr

and print_down buffer = function
  Some kwd_down -> print_token buffer kwd_down "down"
| None          -> ()

and print_step buffer = function
  Some (kwd_step, expr) ->
    print_token buffer kwd_step "step";
    print_expr  buffer expr
| None -> ()

and print_for_collect buffer ({value; _} : for_collect reg) =
  let {kwd_for; var; bind_to; kwd_in; expr; block} = value in
  print_token   buffer kwd_for "for";
  print_var     buffer var;
  print_bind_to buffer bind_to;
  print_token   buffer kwd_in "in";
  print_expr    buffer expr;
  print_block   buffer block

and print_bind_to buffer = function
  Some (arrow, variable) ->
    print_token buffer arrow "->";
    print_var   buffer variable
| None -> ()

and print_expr buffer = function
  ECase   {value;_} -> print_case_expr buffer value
| EAnnot  {value;_} -> print_annot_expr buffer value
| ELogic  e -> print_logic_expr buffer e
| EArith  e -> print_arith_expr buffer e
| EString e -> print_string_expr buffer e
| EList   e -> print_list_expr buffer e
| ESet    e -> print_set_expr buffer e
| EConstr e -> print_constr_expr buffer e
| ERecord e -> print_record_expr buffer e
| EProj   e -> print_projection buffer e
| EMap    e -> print_map_expr buffer e
| EVar    v -> print_var buffer v
| ECall   e -> print_fun_call buffer e
| EBytes  b -> print_bytes buffer b
| EUnit   r -> print_token buffer r "Unit"
| ETuple  e -> print_tuple_expr buffer e
| EPar    e -> print_par_expr buffer e

and print_annot_expr buffer (expr , type_expr) =
  print_expr buffer expr;
  print_type_expr buffer type_expr

and print_case_expr buffer (node : expr case) =
  let {kwd_case; expr; opening;
       lead_vbar; cases; closing} = node in
  print_token      buffer kwd_case "case";
  print_expr       buffer expr;
  print_opening    buffer "of" opening;
  print_token_opt  buffer lead_vbar "|";
  print_cases_expr buffer cases;
  print_closing    buffer closing

and print_cases_expr buffer {value; _} =
  print_nsepseq buffer "|" print_case_clause_expr value

and print_case_clause_expr buffer {value; _} =
  let {pattern; arrow; rhs} = value in
  print_pattern buffer pattern;
  print_token   buffer arrow "->";
  print_expr    buffer rhs

and print_map_expr buffer = function
  MapLookUp {value; _} -> print_map_lookup buffer value
| MapInj inj           -> print_injection  buffer "map" print_binding inj

and print_set_expr buffer = function
  SetInj inj -> print_injection buffer "set" print_expr inj
| SetMem mem -> print_set_membership buffer mem

and print_set_membership buffer {value; _} =
  let {set; kwd_contains; element} = value in
  print_expr  buffer set;
  print_token buffer kwd_contains "contains";
  print_expr  buffer element

and print_map_lookup buffer {path; index} =
  let {lbracket; inside; rbracket} = index.value in
  print_path  buffer path;
  print_token buffer lbracket "[";
  print_expr  buffer inside;
  print_token buffer rbracket "]"

and print_path buffer = function
  Name var  -> print_var        buffer var
| Path path -> print_projection buffer path

and print_logic_expr buffer = function
  BoolExpr e -> print_bool_expr buffer e
| CompExpr e -> print_comp_expr buffer e

and print_bool_expr buffer = function
  Or {value = {arg1; op; arg2}; _} ->
    print_expr  buffer arg1;
    print_token buffer op "||";
    print_expr  buffer arg2
| And {value = {arg1; op; arg2}; _} ->
    print_expr  buffer arg1;
    print_token buffer op "&&";
    print_expr  buffer arg2
| Not {value = {op; arg}; _} ->
    print_token buffer op "not";
    print_expr  buffer arg
| False region ->
    print_token buffer region "False"
| True region ->
    print_token buffer region "True"

and print_comp_expr buffer = function
  Lt {value = {arg1; op; arg2}; _} ->
    print_expr  buffer arg1;
    print_token buffer op "<";
    print_expr  buffer arg2
| Leq {value = {arg1; op; arg2}; _} ->
    print_expr  buffer arg1;
    print_token buffer op "<=";
    print_expr  buffer arg2
| Gt {value = {arg1; op; arg2}; _} ->
    print_expr  buffer arg1;
    print_token buffer op ">";
    print_expr  buffer arg2
| Geq {value = {arg1; op; arg2}; _} ->
    print_expr  buffer arg1;
    print_token buffer op ">=";
    print_expr  buffer arg2
| Equal {value = {arg1; op; arg2}; _} ->
    print_expr  buffer arg1;
    print_token buffer op "=";
    print_expr  buffer arg2
| Neq {value = {arg1; op; arg2}; _} ->
    print_expr  buffer arg1;
    print_token buffer op "=/=";
    print_expr  buffer arg2

and print_arith_expr buffer = function
  Add {value = {arg1; op; arg2}; _} ->
    print_expr  buffer arg1;
    print_token buffer op "+";
    print_expr  buffer arg2
| Sub {value = {arg1; op; arg2}; _} ->
    print_expr  buffer arg1;
    print_token buffer op "-";
    print_expr  buffer arg2
| Mult {value = {arg1; op; arg2}; _} ->
    print_expr  buffer arg1;
    print_token buffer op "*";
    print_expr  buffer arg2
| Div {value = {arg1; op; arg2}; _} ->
    print_expr  buffer arg1;
    print_token buffer op "/";
    print_expr  buffer arg2
| Mod {value = {arg1; op; arg2}; _} ->
    print_expr  buffer arg1;
    print_token buffer op "mod";
    print_expr  buffer arg2
| Neg {value = {op; arg}; _} ->
    print_token buffer op "-";
    print_expr  buffer arg
| Int i
| Nat i
| Mtz i -> print_int buffer i

and print_string_expr buffer = function
  Cat {value = {arg1; op; arg2}; _} ->
    print_expr  buffer arg1;
    print_token buffer op "^";
    print_expr  buffer arg2
| String s ->
    print_string buffer s

and print_list_expr buffer = function
  Cons {value = {arg1; op; arg2}; _} ->
    print_expr  buffer arg1;
    print_token buffer op "#";
    print_expr  buffer arg2
| List e -> print_injection buffer "list" print_expr e
| Nil  e -> print_nil buffer e

and print_constr_expr buffer = function
  SomeApp e   -> print_some_app   buffer e
| NoneExpr e  -> print_none_expr  buffer e
| ConstrApp e -> print_constr_app buffer e

and print_record_expr buffer e =
  print_injection buffer "record" print_field_assign e

and print_field_assign buffer {value; _} =
  let {field_name; equal; field_expr} = value in
  print_var   buffer field_name;
  print_token buffer equal "=";
  print_expr  buffer field_expr

and print_projection buffer {value; _} =
  let {struct_name; selector; field_path} = value in
  print_var        buffer struct_name;
  print_token      buffer selector ".";
  print_field_path buffer field_path

and print_field_path buffer sequence =
  print_nsepseq buffer "." print_selection sequence

and print_selection buffer = function
  FieldName name -> print_var buffer name
| Component int  -> print_int buffer int

and print_record_patch buffer node =
  let {kwd_patch; path; kwd_with; record_inj} = node in
  print_token       buffer kwd_patch "patch";
  print_path        buffer path;
  print_token       buffer kwd_with "with";
  print_record_expr buffer record_inj

and print_set_patch buffer node =
  let {kwd_patch; path; kwd_with; set_inj} = node in
  print_token     buffer kwd_patch "patch";
  print_path      buffer path;
  print_token     buffer kwd_with "with";
  print_injection buffer "set" print_expr set_inj

and print_map_patch buffer node =
  let {kwd_patch; path; kwd_with; map_inj} = node in
  print_token      buffer kwd_patch "patch";
  print_path       buffer path;
  print_token      buffer kwd_with "with";
  print_injection  buffer "map" print_binding map_inj

and print_map_remove buffer node =
  let {kwd_remove; key; kwd_from; kwd_map; map} = node in
  print_token buffer kwd_remove "remove";
  print_expr  buffer key;
  print_token buffer kwd_from "from";
  print_token buffer kwd_map "map";
  print_path  buffer map

and print_set_remove buffer node =
  let {kwd_remove; element; kwd_from; kwd_set; set} = node in
  print_token buffer kwd_remove "remove";
  print_expr  buffer element;
  print_token buffer kwd_from "from";
  print_token buffer kwd_set "set";
  print_path  buffer set

and print_injection :
  'a.Buffer.t -> string -> (Buffer.t -> 'a -> unit) ->
  'a injection reg -> unit =
  fun buffer kwd print {value; _} ->
    let {opening; elements; terminator; closing} = value in
    print_opening    buffer kwd opening;
    print_sepseq     buffer ";" print elements;
    print_terminator buffer terminator;
    print_closing    buffer closing

and print_opening buffer lexeme = function
  Kwd kwd ->
    print_token buffer kwd lexeme
| KwdBracket (kwd, lbracket) ->
    print_token buffer kwd lexeme;
    print_token buffer lbracket "{"

and print_closing buffer = function
  RBracket rbracket -> print_token buffer rbracket "}"
| End kwd_end       -> print_token buffer kwd_end "end"

and print_binding buffer {value; _} =
  let {source; arrow; image} = value in
  print_expr  buffer source;
  print_token buffer arrow "->";
  print_expr  buffer image

and print_tuple_expr buffer = function
  TupleInj inj -> print_tuple_inj buffer inj

and print_tuple_inj buffer {value; _} =
  let {lpar; inside; rpar} = value in
  print_token   buffer lpar "(";
  print_nsepseq buffer "," print_expr inside;
  print_token   buffer rpar ")"

and print_nil buffer value = print_token buffer value "nil"

and print_none_expr buffer value = print_token buffer value "None"

and print_fun_call buffer {value; _} =
  let fun_name, arguments = value in
  print_var       buffer fun_name;
  print_tuple_inj buffer arguments

and print_constr_app buffer {value; _} =
  let constr, arguments = value in
  print_constr buffer constr;
  match arguments with
    None -> ()
  | Some args -> print_tuple_inj buffer args

and print_some_app buffer {value; _} =
  let c_Some, arguments = value in
  print_token     buffer c_Some "Some";
  print_tuple_inj buffer arguments

and print_par_expr buffer {value; _} =
  let {lpar; inside; rpar} = value in
  print_token buffer lpar "(";
  print_expr  buffer inside;
  print_token buffer rpar ")"

and print_pattern buffer = function
  PCons {value; _} -> print_nsepseq buffer "#" print_pattern value
| PVar var         -> print_var buffer var
| PWild wild       -> print_token buffer wild "_"
| PInt i           -> print_int buffer i
| PBytes b         -> print_bytes buffer b
| PString s        -> print_string buffer s
| PUnit region     -> print_token buffer region "Unit"
| PFalse region    -> print_token buffer region "False"
| PTrue region     -> print_token buffer region "True"
| PNone region     -> print_token buffer region "None"
| PSome psome      -> print_psome buffer psome
| PList pattern    -> print_list_pattern buffer pattern
| PTuple ptuple    -> print_ptuple buffer ptuple
| PConstr pattern  -> print_constr_pattern buffer pattern

and print_constr_pattern buffer {value; _} =
  let (constr, args) = value in
  print_constr buffer constr;
  match args with
    None -> ()
  | Some tuple -> print_ptuple buffer tuple

and print_psome buffer {value; _} =
  let c_Some, patterns = value in
  print_token    buffer c_Some "Some";
  print_patterns buffer patterns

and print_patterns buffer {value; _} =
  let {lpar; inside; rpar} = value in
  print_token   buffer lpar "(";
  print_pattern buffer inside;
  print_token   buffer rpar ")"

and print_list_pattern buffer = function
  Sugar  sugar ->
   print_injection buffer "list" print_pattern sugar
| PNil kwd_nil ->
    print_token buffer kwd_nil "nil"
| Raw raw ->
    print_raw buffer raw

and print_raw buffer {value; _} =
  let {lpar; inside; rpar} = value in
  let head, cons, tail = inside in
  print_token   buffer lpar "(";
  print_pattern buffer head;
  print_token   buffer cons "#";
  print_pattern buffer tail;
  print_token   buffer rpar ")"

and print_ptuple buffer {value; _} =
  let {lpar; inside; rpar} = value in
  print_token   buffer lpar "(";
  print_nsepseq buffer "," print_pattern inside;
  print_token   buffer rpar ")"

and print_terminator buffer = function
  Some semi -> print_token buffer semi ";"
| None -> ()

(* Conversion to string *)

let to_string printer node =
  let buffer = Buffer.create 131 in
  let () = printer buffer node
  in Buffer.contents buffer

let tokens_to_string      = to_string print_tokens
let path_to_string        = to_string print_path
let pattern_to_string     = to_string print_pattern
let instruction_to_string = to_string print_instruction
