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

let print_nat buffer {region; value = lexeme, abstract} =
  let line = sprintf "%s: Nat (\"%s\", %s)\n"
               (compact region) lexeme
               (Z.to_string abstract)
  in Buffer.add_string buffer line

(* Main printing function *)

let rec print_tokens buffer ast =
  let {decl; eof} = ast in
  Utils.nseq_iter (print_decl buffer) decl;
  print_token buffer eof "EOF"

and print_decl buffer = function
  TypeDecl  decl -> print_type_decl  buffer decl
| ConstDecl decl -> print_const_decl buffer decl
| FunDecl   decl -> print_fun_decl   buffer decl

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
| TVar    type_var    -> print_var         buffer type_var

and print_cartesian buffer {value; _} =
  print_nsepseq buffer "*" print_type_expr value

and print_variant buffer ({value; _}: variant reg) =
  let {constr; arg} = value in
  print_constr buffer constr;
  match arg with
    None -> ()
  | Some (kwd_of, t_expr) ->
      print_token     buffer kwd_of "of";
      print_type_expr buffer t_expr

and print_sum_type buffer {value; _} =
  print_nsepseq buffer "|" print_variant value

and print_record_type buffer record_type =
  print_ne_injection buffer "record" print_field_decl record_type

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
  match kwd_with with
  | Some kwd_with ->
    print_token       buffer kwd_with "with";
  | None          ->  ();
  print_expr        buffer return;
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

and print_block buffer reg =
  match reg with
  | Some reg ->
    let value = reg.value in
    let {opening; statements; terminator; closing} = value in
    print_block_opening buffer opening;
    print_statements    buffer statements;
    print_terminator    buffer terminator;
    print_block_closing buffer closing
  | None -> ()

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

and print_cond_expr buffer (node: cond_expr) =
  print_token      buffer node.kwd_if "if";
  print_expr       buffer node.test;
  print_token      buffer node.kwd_then "then";
  print_expr       buffer node.ifso;
  print_terminator buffer node.terminator;
  print_token      buffer node.kwd_else "else";
  print_expr       buffer node.ifnot

and print_conditional buffer (node: conditional) =
  print_token      buffer node.kwd_if "if";
  print_expr       buffer node.test;
  print_token      buffer node.kwd_then "then";
  print_if_clause  buffer node.ifso;
  print_terminator buffer node.terminator;
  print_token      buffer node.kwd_else "else";
  print_if_clause  buffer node.ifnot

and print_if_clause buffer = function
  ClauseInstr instr -> print_instruction buffer instr
| ClauseBlock block -> print_clause_block buffer block

and print_clause_block buffer = function
    LongBlock block ->  print_block buffer (Some block)
  | ShortBlock {value; _} ->
    let {lbrace; inside; rbrace} = value in
    let statements, terminator = inside in
    print_token      buffer lbrace "{";
    print_statements buffer statements;
    print_terminator buffer terminator;
    print_token      buffer rbrace "}"

and print_case_instr buffer (node : if_clause case) =
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
  print_pattern   buffer pattern;
  print_token     buffer arrow "->";
  print_if_clause buffer rhs

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
  print_block buffer (Some block)

and print_for_loop buffer = function
  ForInt     for_int     -> print_for_int     buffer for_int
| ForCollect for_collect -> print_for_collect buffer for_collect

and print_for_int buffer ({value; _} : for_int reg) =
  let {kwd_for; assign; kwd_to; bound; block} = value in
  print_token      buffer kwd_for "for";
  print_var_assign buffer assign;
  print_token      buffer kwd_to "to";
  print_expr       buffer bound;
  print_block      buffer (Some block)

and print_var_assign buffer {value; _} =
  let {name; assign; expr} = value in
  print_var   buffer name;
  print_token buffer assign ":=";
  print_expr  buffer expr

and print_for_collect buffer ({value; _} : for_collect reg) =
  let {kwd_for; var; bind_to;
       kwd_in; collection; expr; block} = value in
  print_token      buffer kwd_for "for";
  print_var        buffer var;
  print_bind_to    buffer bind_to;
  print_token      buffer kwd_in "in";
  print_collection buffer collection;
  print_expr       buffer expr;
  print_block      buffer (Some block)

and print_collection buffer = function
  Map kwd_map ->
    print_token buffer kwd_map "map"
| Set kwd_set ->
    print_token buffer kwd_set "set"
| List kwd_list ->
    print_token buffer kwd_list "list"

and print_bind_to buffer = function
  Some (arrow, variable) ->
    print_token buffer arrow "->";
    print_var   buffer variable
| None -> ()

and print_expr buffer = function
  ECase   {value;_} -> print_case_expr buffer value
| ECond   {value;_} -> print_cond_expr buffer value
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
| BigMapInj inj        -> print_injection  buffer "big_map" print_binding inj

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
| Mutez i -> print_int buffer i

and print_string_expr buffer = function
  Cat {value = {arg1; op; arg2}; _} ->
    print_expr  buffer arg1;
    print_token buffer op "^";
    print_expr  buffer arg2
| String s ->
    print_string buffer s

and print_list_expr buffer = function
  ECons {value = {arg1; op; arg2}; _} ->
    print_expr  buffer arg1;
    print_token buffer op "#";
    print_expr  buffer arg2
| EListComp e -> print_injection buffer "list" print_expr e
| ENil e -> print_nil buffer e

and print_constr_expr buffer = function
  SomeApp e   -> print_some_app   buffer e
| NoneExpr e  -> print_none_expr  buffer e
| ConstrApp e -> print_constr_app buffer e

and print_record_expr buffer e =
  print_ne_injection buffer "record" print_field_assign e

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
  print_token        buffer kwd_patch "patch";
  print_path         buffer path;
  print_token        buffer kwd_with "with";
  print_ne_injection buffer "record" print_field_assign record_inj

and print_set_patch buffer node =
  let {kwd_patch; path; kwd_with; set_inj} = node in
  print_token        buffer kwd_patch "patch";
  print_path         buffer path;
  print_token        buffer kwd_with "with";
  print_ne_injection buffer "set" print_expr set_inj

and print_map_patch buffer node =
  let {kwd_patch; path; kwd_with; map_inj} = node in
  print_token        buffer kwd_patch "patch";
  print_path         buffer path;
  print_token        buffer kwd_with "with";
  print_ne_injection buffer "map" print_binding map_inj

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

and print_ne_injection :
  'a.Buffer.t -> string -> (Buffer.t -> 'a -> unit) ->
  'a ne_injection reg -> unit =
  fun buffer kwd print {value; _} ->
    let {opening; ne_elements; terminator; closing} = value in
    print_opening    buffer kwd opening;
    print_nsepseq    buffer ";" print ne_elements;
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

and print_tuple_expr buffer {value; _} =
  let {lpar; inside; rpar} = value in
  print_token   buffer lpar "(";
  print_nsepseq buffer "," print_expr inside;
  print_token   buffer rpar ")"

and print_nil buffer value = print_token buffer value "nil"

and print_none_expr buffer value = print_token buffer value "None"

and print_fun_call buffer {value; _} =
  let fun_name, arguments = value in
  print_var        buffer fun_name;
  print_tuple_expr buffer arguments

and print_constr_app buffer {value; _} =
  let constr, arguments = value in
  print_constr buffer constr;
  match arguments with
    None -> ()
  | Some arg -> print_tuple_expr buffer arg

and print_some_app buffer {value; _} =
  let c_Some, arguments = value in
  print_token      buffer c_Some "Some";
  print_tuple_expr buffer arguments

and print_par_expr buffer {value; _} =
  let {lpar; inside; rpar} = value in
  print_token buffer lpar "(";
  print_expr  buffer inside;
  print_token buffer rpar ")"

and print_pattern buffer = function
  PVar var         -> print_var buffer var
| PWild wild       -> print_token buffer wild "_"
| PInt i           -> print_int buffer i
| PNat n           -> print_nat buffer n
| PBytes b         -> print_bytes buffer b
| PString s        -> print_string buffer s
| PList pattern    -> print_list_pattern buffer pattern
| PTuple ptuple    -> print_ptuple buffer ptuple
| PConstr pattern  -> print_constr_pattern buffer pattern

and print_constr_pattern buffer = function
  PUnit region     -> print_token buffer region "Unit"
| PFalse region    -> print_token buffer region "False"
| PTrue region     -> print_token buffer region "True"
| PNone region     -> print_token buffer region "None"
| PSomeApp psome   -> print_psome buffer psome
| PConstrApp {value; _} ->
    let constr, arg = value in
    print_constr buffer constr;
    match arg with
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
  PListComp comp ->
   print_injection buffer "list" print_pattern comp
| PNil kwd_nil ->
    print_token buffer kwd_nil "nil"
| PParCons cons ->
    print_par_cons buffer cons
| PCons {value; _} ->
   print_nsepseq buffer "#" print_pattern value

and print_par_cons buffer {value; _} =
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

(* Pretty-printing the AST *)

(* The function [mk_pad] updates the current padding, which is
   comprised of two components: the padding to reach the new node
   (space before reaching a subtree, then a vertical bar for it) and
   the padding for the new node itself (Is it the last child of its
   parent?). *)
let mk_pad len rank pc =
  pc ^ (if rank = len-1 then "`-- " else "|-- "),
  pc ^ (if rank = len-1 then "    " else "|   ")

let pp_ident buffer ~pad:(pd,_) Region.{value=name; region} =
  let node = sprintf "%s%s (%s)\n" pd name (region#compact `Byte)
  in Buffer.add_string buffer node

let pp_node buffer ~pad:(pd,_) name =
  let node = sprintf "%s%s\n" pd name
  in Buffer.add_string buffer node

let pp_string buffer = pp_ident buffer

let pp_loc_node buffer ~pad name region =
  pp_ident buffer ~pad Region.{value=name; region}

let rec pp_ast buffer ~pad:(_,pc as pad) {decl; _} =
  let apply len rank =
    let pad = mk_pad len rank pc in
    pp_declaration buffer ~pad in
  let decls = Utils.nseq_to_list decl in
  pp_node buffer ~pad "<ast>";
  List.iteri (List.length decls |> apply) decls

and pp_declaration buffer ~pad:(_,pc as pad) = function
  TypeDecl {value; region} ->
    pp_loc_node buffer ~pad "TypeDecl" region;
    pp_ident buffer ~pad:(mk_pad 2 0 pc) value.name;
    pp_type_expr buffer ~pad:(mk_pad 2 1 pc) value.type_expr
| ConstDecl {value; region} ->
    pp_loc_node buffer ~pad "ConstDecl" region;
    pp_const_decl buffer ~pad value
| FunDecl {value; region} ->
    pp_loc_node buffer ~pad "FunDecl" region;
    pp_fun_decl buffer ~pad value

and pp_const_decl buffer ~pad:(_,pc) decl =
  pp_ident buffer ~pad:(mk_pad 3 0 pc) decl.name;
  pp_type_expr buffer ~pad:(mk_pad 3 1 pc) decl.const_type;
  pp_expr buffer ~pad:(mk_pad 3 2 pc) decl.init

and pp_type_expr buffer ~pad:(_,pc as pad) = function
  TProd cartesian ->
    pp_loc_node buffer ~pad "TProd" cartesian.region;
    pp_cartesian buffer ~pad cartesian
| TVar v ->
    pp_node buffer ~pad "TVar";
    pp_ident buffer ~pad:(mk_pad 1 0 pc) v
| TPar {value; region} ->
    pp_loc_node buffer ~pad "TPar" region;
    pp_type_expr buffer ~pad:(mk_pad 1 0 pc) value.inside
| TApp {value=name,tuple; region} ->
    pp_loc_node buffer ~pad "TApp" region;
    pp_ident buffer ~pad:(mk_pad 1 0 pc) name;
    pp_type_tuple buffer ~pad:(mk_pad 2 1 pc) tuple
| TFun {value; region} ->
    pp_loc_node buffer ~pad "TFun" region;
    let apply len rank =
      let pad = mk_pad len rank pc in
      pp_type_expr buffer ~pad in
    let domain, _, range = value in
    List.iteri (apply 2) [domain; range]
| TSum {value; region} ->
    pp_loc_node buffer ~pad "TSum" region;
    let apply len rank variant =
      let pad = mk_pad len rank pc in
      pp_variant buffer ~pad variant.value in
    let variants = Utils.nsepseq_to_list value in
    List.iteri (List.length variants |> apply) variants
| TRecord {value; region} ->
    pp_loc_node buffer ~pad "TRecord" region;
    let apply len rank field_decl =
      pp_field_decl buffer ~pad:(mk_pad len rank pc)
                    field_decl.value in
    let fields = Utils.nsepseq_to_list value.ne_elements in
    List.iteri (List.length fields |> apply) fields

and pp_cartesian buffer ~pad:(_,pc) {value; _} =
  let apply len rank =
    pp_type_expr buffer ~pad:(mk_pad len rank pc) in
  let components = Utils.nsepseq_to_list value
  in List.iteri (List.length components |> apply) components

and pp_variant buffer ~pad:(_,pc as pad) {constr; arg} =
  pp_ident buffer ~pad constr;
  match arg with
          None -> ()
  | Some (_,c) ->
      pp_type_expr buffer ~pad:(mk_pad 1 0 pc) c

and pp_field_decl buffer ~pad:(_,pc as pad) decl =
  pp_ident buffer ~pad decl.field_name;
  pp_type_expr buffer ~pad:(mk_pad 1 0 pc) decl.field_type

and pp_type_tuple buffer ~pad:(_,pc) {value; _} =
  let components = Utils.nsepseq_to_list value.inside in
  let apply len rank =
    pp_type_expr buffer ~pad:(mk_pad len rank pc)
  in List.iteri (List.length components |> apply) components

and pp_fun_decl buffer ~pad:(_,pc) decl =
  let fields =
    if decl.local_decls = [] then 5 else 6 in
  let () =
    let pad = mk_pad fields 0 pc in
    pp_ident buffer ~pad decl.name in
  let () =
    let pad = mk_pad fields 1 pc in
    pp_node buffer ~pad "<parameters>";
    pp_parameters buffer ~pad decl.param in
  let () =
    let _, pc as pad = mk_pad fields 2 pc in
    pp_node buffer ~pad "<return type>";
    pp_type_expr buffer ~pad:(mk_pad 1 0 pc) decl.ret_type in
  let () =
    if fields = 6 then
      let pad = mk_pad fields 3 pc in
      pp_node buffer ~pad "<local declarations>";
      pp_local_decls buffer ~pad decl.local_decls in
  let () =
    let pad = mk_pad fields (fields - 2) pc in
    pp_node buffer ~pad "<block>";
    let statements =
      match decl.block with
        Some block -> block.value.statements
      | None -> Instr (Skip Region.ghost), [] in
    pp_statements buffer ~pad statements in
  let () =
    let _, pc as pad = mk_pad fields (fields - 1) pc in
    pp_node buffer ~pad "<return>";
    pp_expr buffer ~pad:(mk_pad 1 0 pc) decl.return
  in ()

and pp_parameters buffer ~pad:(_,pc) {value; _} =
  let params = Utils.nsepseq_to_list value.inside in
  let arity  = List.length params in
  let apply len rank =
    pp_param_decl buffer ~pad:(mk_pad len rank pc)
  in List.iteri (apply arity) params

and pp_param_decl buffer ~pad:(_,pc as pad) = function
  ParamConst {value; region} ->
    pp_loc_node buffer ~pad "ParamConst" region;
    pp_ident buffer ~pad:(mk_pad 2 0 pc) value.var;
    pp_type_expr buffer ~pad:(mk_pad 2 1 pc) value.param_type
| ParamVar {value; region} ->
    pp_loc_node buffer ~pad "ParamVar" region;
    pp_ident buffer ~pad:(mk_pad 2 0 pc) value.var;
    pp_type_expr buffer ~pad:(mk_pad 2 1 pc) value.param_type

and pp_statements buffer ~pad:(_,pc) statements =
  let statements = Utils.nsepseq_to_list statements in
  let length     = List.length statements in
  let apply len rank =
    pp_statement buffer ~pad:(mk_pad len rank pc)
  in List.iteri (apply length) statements

and pp_statement buffer ~pad:(_,pc as pad) = function
  Instr instr ->
    pp_node buffer ~pad "Instr";
    pp_instruction buffer ~pad:(mk_pad 1 0 pc) instr
| Data data_decl ->
    pp_node buffer ~pad "Data";
    pp_data_decl buffer ~pad:(mk_pad 1 0 pc) data_decl

and pp_instruction buffer ~pad:(_,pc as pad) = function
  Cond {value; region} ->
    pp_loc_node buffer ~pad "Cond" region;
    pp_conditional buffer ~pad value
| CaseInstr {value; region} ->
    pp_loc_node buffer ~pad "CaseInstr" region;
    pp_case pp_if_clause buffer ~pad value
| Assign {value; region} ->
    pp_loc_node buffer ~pad "Assign" region;
    pp_assignment buffer ~pad value
| Loop loop ->
    pp_node buffer ~pad "Loop";
    pp_loop buffer ~pad:(mk_pad 1 0 pc) loop
| ProcCall {value; region} ->
    pp_loc_node buffer ~pad "ProcCall" region;
    pp_fun_call buffer ~pad value
| Skip region ->
    pp_loc_node buffer ~pad "Skip" region
| RecordPatch {value; region} ->
    pp_loc_node buffer ~pad "RecordPatch" region;
    pp_record_patch buffer ~pad value
| MapPatch {value; region} ->
    pp_loc_node buffer ~pad "MapPatch" region;
    pp_map_patch buffer ~pad value
| SetPatch {value; region} ->
    pp_loc_node buffer ~pad "SetPatch" region;
    pp_set_patch buffer ~pad value
| MapRemove {value; region} ->
    pp_loc_node buffer ~pad "MapRemove" region;
    pp_map_remove buffer ~pad value
| SetRemove {value; region} ->
    pp_loc_node buffer ~pad "SetRemove" region;
    pp_set_remove buffer ~pad value

and pp_cond_expr buffer ~pad:(_,pc) (cond: cond_expr) =
  let () =
    let _, pc as pad = mk_pad 3 0 pc in
    pp_node buffer ~pad "<condition>";
    pp_expr buffer ~pad:(mk_pad 1 0 pc) cond.test in
  let () =
    let _, pc as pad = mk_pad 3 1 pc in
    pp_node buffer ~pad "<true>";
    pp_expr buffer ~pad:(mk_pad 1 0 pc) cond.ifso in
  let () =
    let _, pc as pad = mk_pad 3 2 pc in
    pp_node buffer ~pad "<false>";
    pp_expr buffer ~pad:(mk_pad 1 0 pc) cond.ifnot
  in ()

and pp_conditional buffer ~pad:(_,pc) (cond: conditional) =
  let () =
    let _, pc as pad = mk_pad 3 0 pc in
    pp_node buffer ~pad "<condition>";
    pp_expr buffer ~pad:(mk_pad 1 0 pc) cond.test in
  let () =
    let _, pc as pad = mk_pad 3 1 pc in
    pp_node buffer ~pad "<true>";
    pp_if_clause buffer ~pad:(mk_pad 1 0 pc) cond.ifso in
  let () =
    let _, pc as pad = mk_pad 3 2 pc in
    pp_node buffer ~pad "<false>";
    pp_if_clause buffer ~pad:(mk_pad 1 0 pc) cond.ifnot
  in ()

and pp_if_clause buffer ~pad:(_,pc as pad) = function
  ClauseInstr instr ->
    pp_node buffer ~pad "ClauseInstr";
    pp_instruction buffer ~pad:(mk_pad 1 0 pc) instr
| ClauseBlock block ->
    pp_node buffer ~pad "ClauseBlock";
    pp_clause_block buffer ~pad:(mk_pad 1 0 pc) block

and pp_clause_block buffer ~pad = function
  LongBlock {value; region} ->
    pp_loc_node buffer ~pad "LongBlock" region;
    pp_statements buffer ~pad value.statements
| ShortBlock {value; region} ->
    pp_loc_node buffer ~pad "ShortBlock" region;
    pp_statements buffer ~pad (fst value.inside)

and pp_case :
  'a.(Buffer.t -> pad:(string*string) -> 'a -> unit)
  -> Buffer.t -> pad:(string*string) -> 'a case -> unit =
  fun printer buffer ~pad:(_,pc) case ->
    let clauses = Utils.nsepseq_to_list case.cases.value in
    let clauses = List.map (fun {value; _} -> value) clauses in
    let length  = List.length clauses + 1 in
    let apply len rank =
      pp_case_clause printer buffer ~pad:(mk_pad len (rank+1) pc)
    in pp_expr buffer ~pad:(mk_pad length 0 pc) case.expr;
    List.iteri (apply length) clauses

and pp_case_clause :
  'a.(Buffer.t -> pad:(string*string) -> 'a -> unit)
  -> Buffer.t -> pad:(string*string) -> 'a case_clause -> unit =
  fun printer buffer ~pad:(_,pc as pad) clause ->
    pp_node buffer ~pad "<clause>";
    pp_pattern buffer ~pad:(mk_pad 2 0 pc) clause.pattern;
    printer buffer ~pad:(mk_pad 2 1 pc) clause.rhs

and pp_pattern buffer ~pad:(_,pc as pad) = function
  PWild region ->
    pp_loc_node buffer ~pad "PWild" region
| PConstr pattern ->
    pp_node buffer ~pad "PConstr";
    pp_constr_pattern buffer ~pad:(mk_pad 1 0 pc) pattern
| PVar v ->
    pp_node buffer ~pad "PVar";
    pp_ident buffer ~pad:(mk_pad 1 0 pc) v
| PInt n ->
    pp_node buffer ~pad "PInt";
    pp_int buffer ~pad n
| PNat n ->
    pp_node buffer ~pad "PNat";
    pp_int buffer ~pad n
| PBytes b ->
    pp_node buffer ~pad "PBytes";
    pp_bytes buffer ~pad b
| PString s ->
    pp_node buffer ~pad "PString";
    pp_ident buffer ~pad:(mk_pad 1 0 pc) s
| PList plist ->
    pp_node buffer ~pad "PList";
    pp_list_pattern buffer ~pad:(mk_pad 1 0 pc) plist
| PTuple {value; region} ->
    pp_loc_node buffer ~pad "PTuple" region;
    pp_tuple_pattern buffer ~pad:(mk_pad 1 0 pc) value

and pp_bytes buffer ~pad:(_,pc) {value=lexeme,hex; region} =
  pp_loc_node buffer ~pad:(mk_pad 2 0 pc) lexeme region;
  pp_node     buffer ~pad:(mk_pad 2 1 pc) (Hex.to_string hex)

and pp_int buffer ~pad:(_,pc) {value=lexeme,z; region} =
  pp_loc_node buffer ~pad:(mk_pad 2 0 pc) lexeme region;
  pp_node     buffer ~pad:(mk_pad 2 1 pc) (Z.to_string z)

and pp_constr_pattern buffer ~pad:(_,pc as pad) = function
  PNone region ->
    pp_loc_node buffer ~pad "PNone" region
| PSomeApp {value=_,{value=par; _}; region} ->
    pp_loc_node buffer ~pad "PSomeApp" region;
    pp_pattern buffer ~pad:(mk_pad 1 0 pc) par.inside
| PUnit region ->
    pp_loc_node buffer ~pad "PUnit" region
| PFalse region ->
    pp_loc_node buffer ~pad "PFalse" region
| PTrue region ->
    pp_loc_node buffer ~pad "PTrue" region
| PConstrApp {value; region} ->
    pp_loc_node buffer ~pad "PConstrApp" region;
    pp_constr_app_pattern buffer ~pad:(mk_pad 1 0 pc) value

and pp_constr_app_pattern buffer ~pad (constr, pat_opt) =
  pp_ident buffer ~pad constr;
  match pat_opt with
               None -> ()
  | Some {value; _} -> pp_tuple_pattern buffer ~pad value

and pp_list_pattern buffer ~pad:(_,pc as pad) = function
  PListComp {value; region} ->
    pp_loc_node buffer ~pad "PListComp" region;
    pp_injection pp_pattern buffer ~pad:(mk_pad 1 0 pc) value
| PNil region ->
    pp_loc_node buffer ~pad "PNil" region
| PParCons {value; region} ->
    pp_loc_node buffer ~pad "PParCons" region;
    pp_bin_cons buffer ~pad:(mk_pad 1 0 pc) value.inside
| PCons {value; region} ->
    let patterns = Utils.nsepseq_to_list value in
    let length   = List.length patterns in
    let apply len rank =
      pp_pattern buffer ~pad:(mk_pad len rank pc) in
    pp_loc_node buffer ~pad "PCons" region;
    List.iteri (apply length) patterns

and pp_bin_cons buffer ~pad:(_,pc) (head, _, tail) =
  pp_pattern buffer ~pad:(mk_pad 2 0 pc) head;
  pp_pattern buffer ~pad:(mk_pad 2 1 pc) tail

and pp_injection :
  'a.(Buffer.t -> pad:(string*string) -> 'a -> unit)
  -> Buffer.t -> pad:(string*string) -> 'a injection -> unit =
  fun printer buffer ~pad:(_,pc) inj ->
  let elements = Utils.sepseq_to_list inj.elements in
  let length   = List.length elements in
  let apply len rank = printer buffer ~pad:(mk_pad len rank pc)
  in List.iteri (apply length) elements

and pp_ne_injection :
  'a.(Buffer.t -> pad:(string*string) -> 'a -> unit)
  -> Buffer.t -> pad:(string*string) -> 'a ne_injection -> unit =
  fun printer buffer ~pad:(_,pc) inj ->
  let ne_elements = Utils.nsepseq_to_list inj.ne_elements in
  let length      = List.length ne_elements in
  let apply len rank = printer buffer ~pad:(mk_pad len rank pc)
  in List.iteri (apply length) ne_elements

and pp_tuple_pattern buffer ~pad:(_,pc) tuple =
  let patterns = Utils.nsepseq_to_list tuple.inside in
  let length   = List.length patterns in
  let apply len rank =
    pp_pattern buffer ~pad:(mk_pad len rank pc)
  in List.iteri (apply length) patterns

and pp_assignment buffer ~pad:(_,pc) asgn =
  pp_lhs  buffer ~pad:(mk_pad 2 0 pc) asgn.lhs;
  pp_expr buffer ~pad:(mk_pad 2 1 pc) asgn.rhs

and pp_lhs buffer ~pad:(_,pc as pad) = function
  Path path ->
    pp_node buffer ~pad "Path";
    pp_path buffer ~pad:(mk_pad 1 0 pc) path
| MapPath {value; region} ->
    pp_loc_node buffer ~pad "MapPath" region;
    pp_map_lookup buffer ~pad value

and pp_path buffer ~pad:(_,pc as pad) = function
  Name name ->
    pp_node buffer ~pad "Name";
    pp_ident buffer ~pad:(mk_pad 1 0 pc) name
| Path {value; region} ->
    pp_loc_node buffer ~pad "Path" region;
    pp_projection buffer ~pad value

and pp_projection buffer ~pad:(_,pc) proj =
  let selections = Utils.nsepseq_to_list proj.field_path in
  let len = List.length selections in
  let apply len rank =
    pp_selection buffer ~pad:(mk_pad len rank pc) in
  pp_ident buffer ~pad:(mk_pad (1+len) 0 pc) proj.struct_name;
  List.iteri (apply len) selections

and pp_selection buffer ~pad:(_,pc as pad) = function
  FieldName name ->
    pp_node buffer ~pad "FieldName";
    pp_ident buffer ~pad:(mk_pad 1 0 pc) name
| Component comp ->
    pp_node buffer ~pad "Component";
    pp_int buffer ~pad comp

and pp_map_lookup buffer ~pad:(_,pc) lookup =
  pp_path buffer ~pad:(mk_pad 2 0 pc) lookup.path;
  pp_expr buffer ~pad:(mk_pad 2 1 pc) lookup.index.value.inside

and pp_loop buffer ~pad:(_,pc as pad) = function
  While {value; _} ->
    pp_node buffer ~pad "<while>";
    let () =
      let _, pc as pad = mk_pad 2 0 pc in
      pp_node buffer ~pad "<condition>";
      pp_expr buffer ~pad:(mk_pad 1 0 pc) value.cond in
    let () =
      let pad = mk_pad 2 1 pc in
      let statements = value.block.value.statements in
      pp_node buffer ~pad "<statements>";
      pp_statements buffer ~pad statements
    in ()
| For for_loop ->
    pp_node buffer ~pad "<for>";
    pp_for_loop buffer ~pad:(mk_pad 1 0 pc) for_loop

and pp_for_loop buffer ~pad = function
  ForInt {value; region} ->
    pp_loc_node buffer ~pad "ForInt" region;
    pp_for_int buffer ~pad value
| ForCollect {value; region} ->
    pp_loc_node buffer ~pad "ForCollect" region;
    pp_for_collect buffer ~pad value

and pp_for_int buffer ~pad:(_,pc) for_int =
  let () =
    let pad = mk_pad 3 0 pc in
    pp_node buffer ~pad "<init>";
    pp_var_assign buffer ~pad for_int.assign.value in
  let () =
    let _, pc as pad = mk_pad 3 1 pc in
    pp_node buffer ~pad "<bound>";
    pp_expr buffer ~pad:(mk_pad 1 0 pc) for_int.bound in
  let () =
    let pad = mk_pad 3 2 pc in
    let statements = for_int.block.value.statements in
    pp_node buffer ~pad "<statements>";
    pp_statements buffer ~pad statements
  in ()

and pp_var_assign buffer ~pad:(_,pc) asgn =
  let pad = mk_pad 2 0 pc in
  pp_ident buffer ~pad asgn.name;
  let pad = mk_pad 2 1 pc in
  pp_expr buffer ~pad asgn.expr

and pp_for_collect buffer ~pad:(_,pc) collect =
  let () =
    let pad = mk_pad 3 0 pc in
    match collect.bind_to with
      None ->
        pp_ident buffer ~pad collect.var
    | Some (_, var) ->
        pp_var_binding buffer ~pad (collect.var, var) in
  let () =
    let _, pc  as pad = mk_pad 3 1 pc in
    pp_node buffer ~pad "<collection>";
    pp_collection buffer ~pad:(mk_pad 2 0 pc) collect.collection;
    pp_expr buffer ~pad:(mk_pad 1 0 pc) collect.expr in
  let () =
      let pad = mk_pad 3 2 pc in
      let statements = collect.block.value.statements in
      pp_node buffer ~pad "<statements>";
      pp_statements buffer ~pad statements
  in ()

and pp_collection buffer ~pad = function
  Map  region -> pp_loc_node buffer ~pad "map"  region
| Set  region -> pp_loc_node buffer ~pad "set"  region
| List region -> pp_loc_node buffer ~pad "list" region

and pp_var_binding buffer ~pad:(_,pc as pad) (source, image) =
  pp_node buffer ~pad "<binding>";
  pp_ident buffer ~pad:(mk_pad 2 0 pc) source;
  pp_ident buffer ~pad:(mk_pad 2 1 pc) image

and pp_fun_call buffer ~pad:(_,pc) (name, args) =
  let args  = Utils.nsepseq_to_list args.value.inside in
  let arity = List.length args in
  let apply len rank =
    pp_expr buffer ~pad:(mk_pad len rank pc)
  in pp_ident buffer ~pad:(mk_pad (1+arity) 0 pc) name;
     List.iteri (apply arity) args

and pp_record_patch buffer ~pad:(_,pc as pad) patch =
  pp_path buffer ~pad:(mk_pad 2 0 pc) patch.path;
  pp_ne_injection pp_field_assign buffer
    ~pad patch.record_inj.value

and pp_field_assign buffer ~pad:(_,pc as pad) {value; _} =
  pp_node buffer ~pad "<field assignment>";
  pp_ident buffer ~pad:(mk_pad 2 0 pc) value.field_name;
  pp_expr  buffer ~pad:(mk_pad 2 1 pc) value.field_expr

and pp_map_patch buffer ~pad:(_,pc as pad) patch =
  pp_path buffer ~pad:(mk_pad 2 0 pc) patch.path;
  pp_ne_injection pp_binding buffer
    ~pad patch.map_inj.value

and pp_binding buffer ~pad:(_,pc as pad) {value; _} =
  let source, image = value.source, value.image in
  pp_node buffer ~pad "<binding>";
  pp_expr buffer ~pad:(mk_pad 2 0 pc) source;
  pp_expr buffer ~pad:(mk_pad 2 1 pc) image

and pp_set_patch buffer ~pad:(_,pc as pad) patch =
  pp_path buffer ~pad:(mk_pad 2 0 pc) patch.path;
  pp_ne_injection pp_expr buffer ~pad patch.set_inj.value

and pp_map_remove buffer ~pad:(_,pc) rem =
  pp_expr buffer ~pad:(mk_pad 2 0 pc) rem.key;
  pp_path buffer ~pad:(mk_pad 2 1 pc) rem.map

and pp_set_remove buffer ~pad:(_,pc) rem =
  pp_expr buffer ~pad:(mk_pad 2 0 pc) rem.element;
  pp_path buffer ~pad:(mk_pad 2 1 pc) rem.set

and pp_local_decls buffer ~pad:(_,pc) decls =
  let apply len rank =
    pp_local_decl buffer ~pad:(mk_pad len rank pc)
  in List.iteri (List.length decls |> apply) decls

and pp_local_decl buffer ~pad:(_,pc as pad) = function
  LocalFun {value; region} ->
    pp_loc_node buffer ~pad "LocalFun" region;
    pp_fun_decl buffer ~pad value
| LocalData data ->
    pp_node buffer ~pad "LocalData";
    pp_data_decl buffer ~pad:(mk_pad 1 0 pc) data

and pp_data_decl buffer ~pad = function
  LocalConst {value; region} ->
    pp_loc_node buffer ~pad "LocalConst" region;
    pp_const_decl buffer ~pad value
| LocalVar {value; region} ->
    pp_loc_node buffer ~pad "LocalVar" region;
    pp_var_decl buffer ~pad value

and pp_var_decl buffer ~pad:(_,pc) decl =
  pp_ident     buffer ~pad:(mk_pad 3 0 pc) decl.name;
  pp_type_expr buffer ~pad:(mk_pad 3 1 pc) decl.var_type;
  pp_expr      buffer ~pad:(mk_pad 3 2 pc) decl.init

and pp_expr buffer ~pad:(_,pc as pad) = function
  ECase {value; region} ->
    pp_loc_node buffer ~pad "ECase" region;
    pp_case pp_expr buffer ~pad value
| ECond {value; region} ->
    pp_loc_node buffer ~pad "ECond" region;
    pp_cond_expr buffer ~pad value
| EAnnot {value; region} ->
    pp_loc_node buffer ~pad "EAnnot" region;
    pp_annotated buffer ~pad value
| ELogic e_logic ->
    pp_node buffer ~pad "ELogic";
    pp_e_logic buffer ~pad:(mk_pad 1 0 pc) e_logic
| EArith e_arith ->
    pp_node buffer ~pad "EArith";
    pp_arith_expr buffer ~pad:(mk_pad 1 0 pc) e_arith
| EString e_string ->
    pp_node buffer ~pad "EString";
    pp_string_expr buffer ~pad:(mk_pad 1 0 pc) e_string
| EList e_list ->
    pp_node buffer ~pad "EList";
    pp_list_expr buffer ~pad:(mk_pad 1 0 pc) e_list
| ESet e_set ->
    pp_node buffer ~pad "ESet";
    pp_set_expr buffer ~pad:(mk_pad 1 0 pc) e_set
| EConstr e_constr ->
    pp_node buffer ~pad "EConstr";
    pp_constr_expr buffer ~pad:(mk_pad 1 0 pc) e_constr
| ERecord {value; region} ->
    pp_loc_node buffer ~pad "ERecord" region;
    pp_ne_injection pp_field_assign buffer ~pad value
| EProj {value; region} ->
    pp_loc_node buffer ~pad "EProj" region;
    pp_projection buffer ~pad value
| EMap e_map ->
    pp_node buffer ~pad "EMap";
    pp_map_expr buffer ~pad:(mk_pad 1 0 pc) e_map
| EVar v ->
    pp_node buffer ~pad "EVar";
    pp_ident buffer ~pad:(mk_pad 1 0 pc) v
| ECall {value; region} ->
    pp_loc_node buffer ~pad "ECall" region;
    pp_fun_call buffer ~pad value
| EBytes b ->
    pp_node buffer ~pad "EBytes";
    pp_bytes buffer ~pad b
| EUnit region ->
    pp_loc_node buffer ~pad "EUnit" region
| ETuple e_tuple ->
    pp_node buffer ~pad "ETuple";
    pp_tuple_expr buffer ~pad e_tuple
| EPar {value; region} ->
    pp_loc_node buffer ~pad "EPar" region;
    pp_expr buffer ~pad:(mk_pad 1 0 pc) value.inside

and pp_list_expr buffer ~pad:(_,pc as pad) = function
  ECons {value; region} ->
    pp_loc_node buffer ~pad "ECons" region;
    pp_expr buffer ~pad:(mk_pad 2 0 pc) value.arg1;
    pp_expr buffer ~pad:(mk_pad 2 1 pc) value.arg2
| ENil region ->
    pp_loc_node buffer ~pad "ENil" region
| EListComp {value; region} ->
    pp_loc_node buffer ~pad "EListComp" region;
    if value.elements = None then
      pp_node buffer ~pad:(mk_pad 1 0 pc) "[]"
    else
      pp_injection pp_expr buffer ~pad value

and pp_arith_expr buffer ~pad:(_,pc as pad) = function
  Add {value; region} ->
    pp_bin_op "Add" region buffer ~pad value
| Sub {value; region} ->
    pp_bin_op "Sub" region buffer ~pad value
| Mult {value; region} ->
    pp_bin_op "Mult" region buffer ~pad value
| Div {value; region} ->
    pp_bin_op "Div" region buffer ~pad value
| Mod {value; region} ->
    pp_bin_op "Mod" region buffer ~pad value
| Neg {value; region} ->
    pp_loc_node buffer ~pad "Neg" region;
    pp_expr buffer ~pad:(mk_pad 1 0 pc) value.arg;
| Int i ->
    pp_node buffer ~pad "Int";
    pp_int  buffer ~pad i
| Nat n ->
    pp_node buffer ~pad "Nat";
    pp_int  buffer ~pad n
| Mutez m ->
    pp_node buffer ~pad "Mutez";
    pp_int  buffer ~pad m

and pp_set_expr buffer ~pad:(_,pc as pad) = function
  SetInj {value; region} ->
    pp_loc_node buffer ~pad "SetInj" region;
    pp_injection pp_expr buffer ~pad value
| SetMem {value; region} ->
    pp_loc_node buffer ~pad "SetMem" region;
    pp_expr buffer ~pad:(mk_pad 2 0 pc) value.set;
    pp_expr buffer ~pad:(mk_pad 2 1 pc) value.element

and pp_e_logic buffer ~pad:(_, pc as pad) = function
  BoolExpr e ->
    pp_node buffer ~pad "BoolExpr";
    pp_bool_expr buffer ~pad:(mk_pad 1 0 pc) e
| CompExpr e ->
    pp_node buffer ~pad "CompExpr";
    pp_comp_expr buffer ~pad:(mk_pad 1 0 pc) e

and pp_bool_expr buffer ~pad:(_,pc as pad) = function
  Or {value; region} ->
    pp_bin_op "Or" region buffer ~pad value
| And {value; region} ->
    pp_bin_op "And" region buffer ~pad value
| Not {value; region} ->
    pp_loc_node buffer ~pad "Not" region;
    pp_expr buffer ~pad:(mk_pad 1 0 pc) value.arg
| False region ->
    pp_loc_node buffer ~pad "False" region
| True region ->
    pp_loc_node buffer ~pad "True" region

and pp_comp_expr buffer ~pad = function
  Lt {value; region} ->
    pp_bin_op "Lt" region buffer ~pad value
| Leq {value; region} ->
    pp_bin_op "Leq" region buffer ~pad value
| Gt {value; region} ->
    pp_bin_op "Gt" region buffer ~pad value
| Geq {value; region} ->
    pp_bin_op "Geq" region buffer ~pad value
| Equal {value; region} ->
    pp_bin_op "Equal" region buffer ~pad value
| Neq {value; region} ->
    pp_bin_op "Neq" region buffer ~pad value

and pp_constr_expr buffer ~pad:(_, pc as pad) = function
  NoneExpr region ->
    pp_loc_node buffer ~pad "NoneExpr" region
| SomeApp {value=_,args; region} ->
    pp_loc_node buffer ~pad "SomeApp" region;
    pp_tuple_expr buffer ~pad args
| ConstrApp {value; region} ->
    pp_loc_node buffer ~pad "ConstrApp" region;
    pp_constr_app buffer ~pad:(mk_pad 1 0 pc) value

and pp_constr_app buffer ~pad (constr, args_opt) =
  pp_ident buffer ~pad constr;
  match args_opt with
         None -> ()
  | Some args -> pp_tuple_expr buffer ~pad args

and pp_map_expr buffer ~pad = function
  MapLookUp {value; region} ->
    pp_loc_node buffer ~pad "MapLookUp" region;
    pp_map_lookup buffer ~pad value
| MapInj {value; region} | BigMapInj {value; region} ->
    pp_loc_node buffer ~pad "MapInj" region;
    pp_injection pp_binding buffer ~pad value

and pp_tuple_expr buffer ~pad:(_,pc) {value; _} =
  let exprs  = Utils.nsepseq_to_list value.inside in
  let length = List.length exprs in
  let apply len rank =
    pp_expr buffer ~pad:(mk_pad len rank pc)
  in List.iteri (apply length) exprs

and pp_string_expr buffer ~pad:(_,pc as pad) = function
  Cat {value; region} ->
    pp_loc_node buffer ~pad "Cat" region;
    pp_expr buffer ~pad:(mk_pad 2 0 pc) value.arg1;
    pp_expr buffer ~pad:(mk_pad 2 1 pc) value.arg2;
| String s ->
    pp_node buffer ~pad "String";
    pp_string buffer ~pad:(mk_pad 1 0 pc) s

and pp_annotated buffer ~pad:(_,pc) (expr, t_expr) =
  pp_expr buffer ~pad:(mk_pad 2 0 pc) expr;
  pp_type_expr buffer ~pad:(mk_pad 2 1 pc) t_expr

and pp_bin_op node region buffer ~pad:(_,pc as pad) op =
  pp_loc_node buffer ~pad node region;
  pp_expr buffer ~pad:(mk_pad 2 0 pc) op.arg1;
  pp_expr buffer ~pad:(mk_pad 2 1 pc) op.arg2

let pp_ast buffer = pp_ast buffer ~pad:("","")
