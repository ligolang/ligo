open AST
open Utils
open Region

let printf = Printf.printf

let compact (region: Region.t) =
  region#compact ~offsets:EvalOpt.offsets EvalOpt.mode

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

and print_token _visitor region lexeme =
  printf "%s: %s\n"(compact region) lexeme

and print_var _visitor {region; value=lexeme} =
  printf "%s: Ident \"%s\"\n" (compact region) lexeme

and print_constr _visitor {region; value=lexeme} =
  printf "%s: Constr \"%s\"\n"
         (compact region) lexeme

and print_string _visitor {region; value=lexeme} =
  printf "%s: String \"%s\"\n"
         (compact region) lexeme

and print_bytes _visitor {region; value = lexeme, abstract} =
  printf "%s: Bytes (\"%s\", \"0x%s\")\n"
         (compact region) lexeme
         (MBytes.to_hex abstract |> Hex.to_string)

and print_int _visitor {region; value = lexeme, abstract} =
  printf "%s: Int (\"%s\", %s)\n"
         (compact region) lexeme
         (Z.to_string abstract)

(* Main printing function *)

and print_tokens (v: 'x visitor) ast =
  List.iter v.type_decl   ast.types;
  v.parameter_decl        ast.parameter;
  v.storage_decl          ast.storage;
  v.operations_decl       ast.operations;
  List.iter v.lambda_decl ast.lambdas;
  v.block                 ast.block;
  v.token                 ast.eof "EOF"

and print_parameter_decl (v: 'x visitor) {value=node; _} =
  v.token      node.kwd_parameter "parameter";
  v.var        node.name;
  v.token      node.colon ":";
  v.type_expr  node.param_type;
  v.terminator node.terminator

and print_storage_decl (v: 'x visitor) {value=node; _} =
  v.token      node.kwd_storage "storage";
  v.type_expr  node.store_type;
  v.terminator node.terminator

and print_operations_decl (v: 'x visitor) {value=node; _} =
  v.token      node.kwd_operations "operations";
  v.type_expr  node.op_type;
  v.terminator node.terminator

and print_type_decl (v: 'x visitor) {value=node; _} =
  v.token      node.kwd_type "type";
  v.var        node.name;
  v.token      node.kwd_is "is";
  v.type_expr  node.type_expr;
  v.terminator node.terminator

and print_type_expr (v: 'x visitor) = function
  Prod    cartesian   -> v.cartesian   cartesian
| Sum     sum_type    -> v.sum_type    sum_type
| Record  record_type -> v.record_type record_type
| TypeApp type_app    -> v.type_app    type_app
| ParType par_type    -> v.par_type    par_type
| TAlias  type_alias  -> v.var         type_alias

and print_cartesian (v: 'x visitor) {value=sequence; _} =
  v.nsepseq "*" v.type_expr sequence

and print_variant (v: 'x visitor) {value=node; _} =
  let constr, kwd_of, cartesian = node in
  v.constr    constr;
  v.token     kwd_of "of";
  v.cartesian cartesian

and print_sum_type (v: 'x visitor) {value=sequence; _} =
  v.nsepseq "|" v.variant sequence

and print_record_type (v: 'x visitor) {value=node; _} =
  let kwd_record, field_decls, kwd_end = node in
  v.token       kwd_record "record";
  v.field_decls field_decls;
  v.token       kwd_end "end"

and print_type_app (v: 'x visitor) {value=node; _} =
  let type_name, type_tuple = node in
  v.var        type_name;
  v.type_tuple type_tuple

and print_par_type (v: 'x visitor) {value=node; _} =
  let lpar, type_expr, rpar = node in
  v.token     lpar "(";
  v.type_expr type_expr;
  v.token     rpar ")"

and print_field_decls (v: 'x visitor) sequence =
  v.nsepseq ";" v.field_decl sequence

and print_field_decl (v: 'x visitor) {value=node; _} =
  let var, colon, type_expr = node in
  v.var       var;
  v.token     colon ":";
  v.type_expr type_expr

and print_type_tuple (v: 'x visitor) {value=node; _} =
  let lpar, sequence, rpar = node in
  v.token lpar "(";
  v.nsepseq "," v.var sequence;
  v.token rpar ")"

and print_lambda_decl (v: 'x visitor) = function
  FunDecl   fun_decl -> v.fun_decl fun_decl
| ProcDecl proc_decl -> v.proc_decl proc_decl

and print_fun_decl (v: 'x visitor) {value=node; _} =
  v.token       node.kwd_function "function";
  v.var         node.name;
  v.parameters  node.param;
  v.token       node.colon ":";
  v.type_expr   node.ret_type;
  v.token       node.kwd_is "is";
  v.local_decls node.local_decls;
  v.block       node.block;
  v.token       node.kwd_with "with";
  v.expr        node.return;
  v.terminator  node.terminator

and print_proc_decl (v: 'x visitor) {value=node; _} =
  v.token       node.kwd_procedure "procedure";
  v.var         node.name;
  v.parameters  node.param;
  v.token       node.kwd_is "is";
  v.local_decls node.local_decls;
  v.block       node.block;
  v.terminator  node.terminator

and print_parameters (v: 'x visitor) {value=node; _} =
  let lpar, sequence, rpar = node in
  v.token lpar "(";
  v.nsepseq ";" v.param_decl sequence;
  v.token rpar ")"

and print_param_decl (v: 'x visitor) = function
  ParamConst param_const -> v.param_const param_const
| ParamVar   param_var   -> v.param_var   param_var

and print_param_const (v: 'x visitor) {value=node; _} =
  let kwd_const, variable, colon, type_expr = node in
  v.token     kwd_const "const";
  v.var       variable;
  v.token     colon ":";
  v.type_expr type_expr

and print_param_var (v: 'x visitor) {value=node; _} =
  let kwd_var, variable, colon, type_expr = node in
  v.token     kwd_var "var";
  v.var       variable;
  v.token     colon ":";
  v.type_expr type_expr

and print_block (v: 'x visitor) {value=node; _} =
  v.token        node.opening "begin";
  v.instructions node.instr;
  v.terminator   node.terminator;
  v.token        node.close "end"

and print_local_decls (v: 'x visitor) sequence =
  List.iter v.local_decl sequence

and print_local_decl (v: 'x visitor) = function
  LocalLam   decl -> v.lambda_decl decl
| LocalConst decl -> v.const_decl  decl
| LocalVar   decl -> v.var_decl    decl

and print_const_decl (v: 'x visitor) {value=node; _} =
  v.token      node.kwd_const "const";
  v.var        node.name;
  v.token      node.colon ":";
  v.type_expr  node.vtype;
  v.token      node.equal "=";
  v.expr       node.init;
  v.terminator node.terminator

and print_var_decl (v: 'x visitor) {value=node; _} =
  v.token      node.kwd_var "var";
  v.var        node.name;
  v.token      node.colon ":";
  v.type_expr  node.vtype;
  v.token      node.ass ":=";
  v.expr       node.init;
  v.terminator node.terminator

and print_instructions (v: 'x visitor) {value=sequence; _} =
  v.nsepseq ";" v.instruction sequence

and print_instruction (v: 'x visitor) = function
  Single instr -> v.single_instr instr
|  Block block -> v.block block

and print_single_instr (v: 'x visitor) = function
  Cond     {value; _} -> v.conditional value
| Match    {value; _} -> v.match_instr value
| Ass      instr      -> v.ass_instr instr
| Loop     loop       -> v.loop loop
| ProcCall fun_call   -> v.fun_call fun_call
| Null     kwd_null   -> v.token kwd_null "null"
| Fail     {value; _} -> v.fail value

and print_fail (v: 'x visitor) (kwd_fail, expr) =
  v.token kwd_fail "fail";
  v.expr expr

and print_conditional (v: 'x visitor) node =
  v.token       node.kwd_if "if";
  v.expr        node.test;
  v.token       node.kwd_then "then";
  v.instruction node.ifso;
  v.token       node.kwd_else "else";
  v.instruction node.ifnot

and print_match_instr (v: 'x visitor) node =
  v.token node.kwd_match "match";
  v.expr  node.expr;
  v.token node.kwd_with "with";
  v.cases node.cases;
  v.token node.kwd_end "end"

and print_cases (v: 'x visitor) {value=sequence; _} =
  v.nsepseq "|" v.case sequence

and print_case (v: 'x visitor) {value=node; _} =
  let pattern, arrow, instruction = node in
  v.pattern pattern;
  v.token arrow "->";
  v.instruction instruction

and print_ass_instr (v: 'x visitor) {value=node; _} =
  let variable, ass, expr = node in
  v.var variable;
  v.token ass ":=";
  v.expr expr

and print_loop (v: 'x visitor) = function
  While while_loop -> v.while_loop while_loop
| For     for_loop -> v.for_loop for_loop

and print_while_loop (v: 'x visitor) {value=node; _} =
  let kwd_while, expr, block = node in
  v.token kwd_while "while";
  v.expr expr;
  v.block block

and print_for_loop (v: 'x visitor) = function
  ForInt     for_int     -> v.for_int for_int
| ForCollect for_collect -> v.for_collect for_collect

and print_for_int (v: 'x visitor) ({value=node; _} : 'x for_int reg) =
  v.token     node.kwd_for "for";
  v.ass_instr node.ass;
  v.down      node.down;
  v.token     node.kwd_to "to";
  v.expr      node.bound;
  v.step      node.step;
  v.block     node.block

and print_down (v: 'x visitor) = function
  Some kwd_down -> v.token kwd_down "down"
| None -> ()

and print_step (v: 'x visitor) = function
  Some (kwd_step, expr) ->
    v.token kwd_step "step";
    v.expr expr
| None -> ()

and print_for_collect (v: 'x visitor) ({value=node; _} : 'x for_collect reg) =
  v.token   node.kwd_for "for";
  v.var     node.var;
  v.bind_to node.bind_to;
  v.token   node.kwd_in "in";
  v.expr    node.expr;
  v.block   node.block

and print_bind_to (v: 'x visitor) = function
  Some (arrow, variable) ->
    v.token arrow "->";
    v.var   variable
| None -> ()

and print_expr (v: 'x visitor) = function
  Or {value = expr1, bool_or, expr2; _} ->
    v.expr expr1; v.token bool_or "||"; v.expr expr2
| And {value = expr1, bool_and, expr2; _} ->
    v.expr expr1; v.token bool_and "&&"; v.expr expr2
| Lt {value = expr1, lt, expr2; _} ->
    v.expr expr1; v.token lt "<"; v.expr expr2
| Leq {value = expr1, leq, expr2; _} ->
    v.expr expr1; v.token leq "<="; v.expr expr2
| Gt {value = expr1, gt, expr2; _} ->
    v.expr expr1; v.token gt ">"; v.expr expr2
| Geq {value = expr1, geq, expr2; _} ->
    v.expr expr1; v.token geq ">="; v.expr expr2
| Equal {value = expr1, equal, expr2; _} ->
    v.expr expr1; v.token equal "="; v.expr expr2
| Neq {value = expr1, neq, expr2; _} ->
    v.expr expr1; v.token neq "=/="; v.expr expr2
| Cat {value = expr1, cat, expr2; _} ->
    v.expr expr1; v.token cat "^"; v.expr expr2
| Cons {value = expr1, cons, expr2; _} ->
    v.expr expr1; v.token cons "<:"; v.expr expr2
| Add {value = expr1, add, expr2; _} ->
    v.expr expr1; v.token add "+"; v.expr expr2
| Sub {value = expr1, sub, expr2; _} ->
    v.expr expr1; v.token sub "-"; v.expr expr2
| Mult {value = expr1, mult, expr2; _} ->
    v.expr expr1; v.token mult "*"; v.expr expr2
| Div {value = expr1, div, expr2; _} ->
    v.expr expr1; v.token div "/"; v.expr expr2
| Mod {value = expr1, kwd_mod, expr2; _} ->
    v.expr expr1; v.token kwd_mod "mod"; v.expr expr2
| Neg {value = minus, expr; _} ->
    v.token minus "-"; v.expr expr
| Not {value = kwd_not, expr; _} ->
    v.token kwd_not "not"; v.expr expr
| Int i            -> v.int i
| Var var          -> v.var var
| String s         -> v.string s
| Bytes b          -> v.bytes b
| False region     -> v.token region "False"
| True region      -> v.token region "True"
| Unit region      -> v.token region "Unit"
| Tuple tuple      -> v.tuple tuple
| List list        -> v.list list
| EmptyList elist  -> v.empty_list elist
| Set set          -> v.set set
| EmptySet eset    -> v.empty_set eset
| NoneExpr nexpr   -> v.none_expr nexpr
| FunCall fun_call -> v.fun_call fun_call
| ConstrApp capp   -> v.constr_app capp
| SomeApp sapp     -> v.some_app sapp
| MapLookUp lookup -> v.map_lookup lookup
| ParExpr pexpr    -> v.par_expr pexpr

and print_tuple (v: 'x visitor) {value=node; _} =
  let lpar, sequence, rpar = node in
  v.token lpar "(";
  v.nsepseq "," v.expr sequence;
  v.token rpar ")"

and print_list (v: 'x visitor) {value=node; _} =
  let lbra, sequence, rbra = node in
  v.token lbra "[";
  v.nsepseq "," v.expr sequence;
  v.token rbra "]"

and print_empty_list (v: 'x visitor) {value=node; _} =
  let lpar, (lbracket, rbracket, colon, type_expr), rpar = node in
  v.token     lpar "(";
  v.token     lbracket "[";
  v.token     rbracket "]";
  v.token     colon ":";
  v.type_expr type_expr;
  v.token     rpar ")"

and print_set (v: 'x visitor) {value=node; _} =
  let lbrace, sequence, rbrace = node in
  v.token lbrace "{";
  v.nsepseq "," v.expr sequence;
  v.token rbrace "}"

and print_empty_set (v: 'x visitor) {value=node; _} =
  let lpar, (lbrace, rbrace, colon, type_expr), rpar = node in
  v.token     lpar "(";
  v.token     lbrace "{";
  v.token     rbrace "}";
  v.token     colon ":";
  v.type_expr type_expr;
  v.token     rpar ")"

and print_none_expr (v: 'x visitor) {value=node; _} =
  let lpar, (c_None, colon, type_expr), rpar = node in
  v.token     lpar "(";
  v.token     c_None "None";
  v.token     colon ":";
  v.type_expr type_expr;
  v.token     rpar ")"

and print_fun_call (v: 'x visitor) {value=node; _} =
  let fun_name, arguments = node in
  v.var   fun_name;
  v.tuple arguments

and print_constr_app (v: 'x visitor) {value=node; _} =
  let constr, arguments = node in
  v.constr constr;
  v.tuple  arguments

and print_some_app (v: 'x visitor) {value=node; _} =
  let c_Some, arguments = node in
  v.token c_Some "Some";
  v.tuple arguments

and print_map_lookup (v: 'x visitor) {value=node; _} =
  let {value = lbracket, expr, rbracket; _} = node.index in
  v.var   node.map_name;
  v.token node.selector ".";
  v.token lbracket "[";
  v.expr  expr;
  v.token rbracket "]"

and print_par_expr (v: 'x visitor) {value=node; _} =
  let lpar, expr, rpar = node in
  v.token lpar "(";
  v.expr  expr;
  v.token rpar ")"

and print_pattern (v: 'x visitor) {value=sequence; _} =
  v.nsepseq "<:" v.core_pattern sequence

and print_core_pattern (v: 'x visitor) = function
  PVar var      -> v.var var
| PWild wild    -> v.token wild "_"
| PInt i        -> v.int i
| PBytes b      -> v.bytes b
| PString s     -> v.string s
| PUnit region  -> v.token region "Unit"
| PFalse region -> v.token region "False"
| PTrue region  -> v.token region "True"
| PNone region  -> v.token region "None"
| PSome psome   -> v.psome psome
| PList pattern -> v.list_pattern pattern
| PTuple ptuple -> v.ptuple ptuple

and print_psome (v: 'x visitor) {value=node; _} =
  let c_Some, patterns = node in
  v.token    c_Some "Some";
  v.patterns patterns

and print_patterns (v: 'x visitor) {value=node; _} =
  let lpar, core_pattern, rpar = node in
  v.token lpar "(";
  v.core_pattern core_pattern;
  v.token rpar ")"

and print_list_pattern (v: 'x visitor) = function
  Sugar sugar -> v.sugar sugar
| Raw     raw -> v.raw raw

and print_sugar (v: 'x visitor) {value=node; _} =
  let lbracket, sequence, rbracket = node in
  v.token lbracket "[";
  v.sepseq "," v.core_pattern sequence;
  v.token rbracket "]"

and print_raw (v: 'x visitor) {value=node; _} =
  let lpar, (core_pattern, cons, pattern), rpar = node in
  v.token        lpar "(";
  v.core_pattern core_pattern;
  v.token        cons "<:";
  v.pattern      pattern;
  v.token        rpar ")"

and print_ptuple (v: 'x visitor) {value=node; _} =
  let lpar, sequence, rpar = node in
  v.token lpar "(";
  v.nsepseq "," v.core_pattern sequence;
  v.token rpar ")"

and print_terminator (v: 'x visitor) = function
  Some semi -> v.token semi ";"
| None -> ()

let rec visitor () : 'x visitor = {
  nsepseq         = print_nsepseq;
  sepseq          = print_sepseq;
  token           = print_token           (visitor ());
  var             = print_var             (visitor ());
  constr          = print_constr          (visitor ());
  string          = print_string          (visitor ());
  bytes           = print_bytes           (visitor ());
  int             = print_int             (visitor ());

  local_decl      = print_local_decl      (visitor ());
  fail            = print_fail            (visitor ());
  param_var       = print_param_var       (visitor ());
  param_const     = print_param_const     (visitor ());
  const_decl      = print_const_decl      (visitor ());
  parameter_decl  = print_parameter_decl  (visitor ());
  storage_decl    = print_storage_decl    (visitor ());
  operations_decl = print_operations_decl (visitor ());
  type_decl       = print_type_decl       (visitor ());
  type_expr       = print_type_expr       (visitor ());
  cartesian       = print_cartesian       (visitor ());
  variant         = print_variant         (visitor ());
  sum_type        = print_sum_type        (visitor ());
  record_type     = print_record_type     (visitor ());
  type_app        = print_type_app        (visitor ());
  par_type        = print_par_type        (visitor ());
  field_decls     = print_field_decls     (visitor ());
  field_decl      = print_field_decl      (visitor ());
  type_tuple      = print_type_tuple      (visitor ());
  lambda_decl     = print_lambda_decl     (visitor ());
  fun_decl        = print_fun_decl        (visitor ());
  proc_decl       = print_proc_decl       (visitor ());
  parameters      = print_parameters      (visitor ());
  param_decl      = print_param_decl      (visitor ());
  block           = print_block           (visitor ());
  local_decls     = print_local_decls     (visitor ());
  var_decl        = print_var_decl        (visitor ());
  instructions    = print_instructions    (visitor ());
  instruction     = print_instruction     (visitor ());
  single_instr    = print_single_instr    (visitor ());
  conditional     = print_conditional     (visitor ());
  match_instr     = print_match_instr     (visitor ());
  cases           = print_cases           (visitor ());
  case            = print_case            (visitor ());
  ass_instr       = print_ass_instr       (visitor ());
  loop            = print_loop            (visitor ());
  while_loop      = print_while_loop      (visitor ());
  for_loop        = print_for_loop        (visitor ());
  for_int         = print_for_int         (visitor ());
  down            = print_down            (visitor ());
  step            = print_step            (visitor ());
  for_collect     = print_for_collect     (visitor ());
  bind_to         = print_bind_to         (visitor ());
  expr            = print_expr            (visitor ());
  tuple           = print_tuple           (visitor ());
  list            = print_list            (visitor ());
  empty_list      = print_empty_list      (visitor ());
  set             = print_set             (visitor ());
  empty_set       = print_empty_set       (visitor ());
  none_expr       = print_none_expr       (visitor ());
  fun_call        = print_fun_call        (visitor ());
  constr_app      = print_constr_app      (visitor ());
  some_app        = print_some_app        (visitor ());
  map_lookup      = print_map_lookup      (visitor ());
  par_expr        = print_par_expr        (visitor ());
  pattern         = print_pattern         (visitor ());
  core_pattern    = print_core_pattern    (visitor ());
  psome           = print_psome           (visitor ());
  patterns        = print_patterns        (visitor ());
  list_pattern    = print_list_pattern    (visitor ());
  sugar           = print_sugar           (visitor ());
  raw             = print_raw             (visitor ());
  ptuple          = print_ptuple          (visitor ());
  terminator      = print_terminator      (visitor ())
}

let print_tokens = print_tokens (visitor ())
