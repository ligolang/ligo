%{
(* START HEADER *)

[@@@warning "-42"]
[@@@warning "-33"] (* TODO: Remove *)
[@@@warning "-32"] (* TODO: Remove *)

open Simple_utils.Region
module CST = Cst_jsligo.CST
open! CST

(* Utilities *)

let mk_wild region =
  let variable = {value="_"; region} in
  let value = {variable; attributes=[]}
  in {region; value}

let list_of_option = function
       None -> []
| Some list -> list

(* END HEADER *)
%}

(* See [ParToken.mly] for the definition of tokens. *)

(* Entry points *)

%start contract interactive_expr
%type <CST.t> contract
%type <CST.expr> interactive_expr

%%

(* RULES *)

(* Compound constructs *)

par(X):
  "(" X ")" {
    let region = cover $1 $3
    and value  = {lpar=$1; inside=$2; rpar=$3}
    in {region; value} }

chevrons(X):
  "<" X ">" ioption(ZWSP) {
    let region = cover $1 $3
    and value  = {lchevron=$1; inside=$2; rchevron=$3}
    in {region; value} }

gt:
  ">" ioption(ZWSP) { $1 }

brackets(X):
  "[" X "]" {
    let region = cover $1 $3
    and value  = {lbracket=$1; inside=$2; rbracket=$3}
    in {region; value}
  }

braces(X):
  "{" X "}" {
    let region = cover $1 $3
    and value  = {lbrace=$1; inside=$2; rbrace=$3}
    in {region; value} }

(* Sequences

   Series of instances of the same syntactical category have often to
   be parsed, like lists of expressions, patterns etc. The simplest of
   all is the possibly empty sequence (series), parsed below by
   [seq]. The non-empty sequence is parsed by [nseq]. Note that the
   latter returns a pair made of the first parsed item (the parameter
   [X]) and the rest of the sequence (possibly empty). This way, the
   OCaml typechecker can keep track of this information along the
   static control-flow graph. The rule [sepseq] parses possibly empty
   sequences of items separated by some token (e.g., a comma), and
   rule [nsepseq] is for non-empty such sequences. See module [Utils]
   for the types corresponding to the semantic actions of those
   rules. *)

(* Possibly empty sequence of items *)

seq(item):
  (**)           {     [] }
| item seq(item) { $1::$2 }

(* Non-empty sequence of items *)

nseq(item):
  item seq(item) { $1,$2 }

(* Non-empty separated sequence of items *)

nsepseq(item,sep):
  item                       {                        $1, [] }
| item sep nsepseq(item,sep) { let h,t = $3 in $1, ($2,h)::t }

(* The rule [sep_or_term(item,sep)] ("separated or terminated list")
   parses a non-empty list of items separated by [sep], and optionally
   terminated by [sep]. *)

sep_or_term_list(item,sep):
  nsepseq(item,sep) {
    $1, None
  }
| nseq(item sep {$1,$2}) {
    let (first,sep), tail = $1 in
    let rec trans (seq, prev_sep as acc) = function
      [] -> acc
    | (item,next_sep)::others ->
        trans ((prev_sep,item)::seq, next_sep) others in
    let list, term = trans ([],sep) tail
    in (first, List.rev list), Some term }

(* Helpers *)

  (*%inline variable    : "<ident>"  { $1 }*)
%inline type_var    : "<ident>"  { $1 }
%inline type_name   : "<ident>"  { $1 }
%inline field_name  : "<ident>"  { $1 }
%inline module_name : "<uident>" { $1 }
%inline ctor        : "<uident>" { $1 }

(* Non-empty comma-separated values (at least two values) *)

(*
tuple(item):
  item "," nsepseq(item,",") { let h,t = $3 in $1, ($2,h)::t }
 *)

(* Entry point *)

interactive_expr:
  expr EOF { $1 }

(* Entry point *)

contract:
  toplevel_stmts EOF { {statements=$1; eof=$2} : CST.t }

toplevel_stmts:
  stmt_opt_namespace ";" toplevel_stmts {
    Utils.nseq_cons (TopLevel ($1, Some $2)) $3
  }
| "<directive>" toplevel_stmts {
    Utils.nseq_cons (Directive $1) $2
  }
| stmt_opt_namespace ";"? {
    TopLevel ($1,$2), [] }

stmt_opt_namespace:
  statement | namespace_stmt { $1 }

statement:
  base_stmt(statement) | if_stmt(statement) { $1 }

base_stmt(right_stmt):
  expr_stmt                  { SExpr   $1 }
| return_stmt                { SReturn $1 }
| block_stmt                 { SBlock  $1 }
| switch_stmt                { SSwitch $1 }
| import_stmt                { SImport $1 }
| export_decl                { SExport $1 }
| declaration
| if_else_stmt(right_stmt)
| iteration_stmt(right_stmt) { $1 }

closed_stmt:
  base_stmt(closed_stmt) { $1 }

expr_stmt:
  fun_expr
| disj_expr_level               { $1 }
| disj_expr_level "=" expr_stmt { EAssign ($1, $2, $3) }

export_decl:
  "export" declaration {
    let region = cover $1 (statement_to_region $2)
    in {region; value=$1,$2} }

namespace_stmt:
  export_stmt | namespace_raw { $1 }

export_stmt:
  "export" namespace_raw {
    let region = cover $1 (statement_to_region $2)
    in SExport {region; value=$1,$2} }

namespace_raw:
 "namespace" module_name braces(stmts_opt_namespace) {
    let region = cover $1 $3.region
    in SNamespace {region; value=$1,$2,$3} }

expr:
  expr_stmt | object_literal { $1 }

block_stmt:
  braces(statements) { $1 : (statement, semi) Utils.nsepseq braces reg}

return_stmt:
  "return" {
    let value = {kwd_return=$1; expr=None}
    in {region=$1; value}
  }
| "return" expr {
    let region = cover $1 (expr_to_region $2)
    and value  = {kwd_return=$1; expr = Some $2}
    in {region; value} }

if_stmt(right_stmt):
  "if" par(expr) right_stmt {
    let region = cover $1 (statement_to_region $3) in
    let value  = {kwd_if=$1; test=$2.value; ifso=$3; ifnot=None}
    in SCond {region; value} }

if_else_stmt(right_stmt):
  "if" par(expr) closed_stmt "else" right_stmt {
    let region = cover $1 (statement_to_region $5)
    and value  = {kwd_if=$1; test=$2.value; ifso=$3; ifnot = Some ($4,$5)}
    in SCond {region; value} }

cell_pattern:
  /* empty  */          { PWild Region.ghost }
| var_pattern           { PVar $1 }
| "_"                   { PVar (mk_wild $1) }
| array_pattern
| rest                  { $1 }

array_pattern:
  brackets(nsepseq(cell_pattern,",")) { PArray $1 }

rest: (* TODO: Find a descriptive name *)
  "..." "<ident>" {
    let region = cover $1 $2.region
    and value = {ellipsis=$1; rest=$2}
    in PRest {region; value} }

type_annotation:
  ":" type_expr { $1, $2 }

(* Attributes *)

%inline attributes:
  ioption(nseq("[@attr]") { Utils.nseq_to_list $1 }) { list_of_option $1 }

(* Declarations *)

declaration:
  let_decl | const_decl | type_decl { $1 }

let_decl:
  attributes "let" binding_list {
    let stop   = nsepseq_to_region (fun e -> e.region) $3 in
    let region = cover $2 stop
    and value  = {kwd_let=$2; bindings=$3; attributes=$1}
    in SLet {region; value} }

const_decl:
  attributes "const" binding_list {
    let stop   = nsepseq_to_region (fun e -> e.region) $3 in
    let region = cover $2 stop
    and value  = {kwd_const=$2; bindings=$3; attributes=$1}
    in SConst {region; value} }

binding_list:
  nsepseq(binding_initializer,",") { $1 }

binding_initializer:
  binding_pattern ioption(type_annotation) "=" expr {
    let start    = pattern_to_region $1
    and stop     = expr_to_region $4 in
    let region   = cover start stop in
    let value    = {binders=$1; lhs_type=$2; eq=$3; expr=$4}
    in {region; value} }

(* Patterns *)

binding_pattern:
  var_pattern    { PVar $1 }
| "_"            { PVar (mk_wild $1) }
| object_pattern
| array_pattern  { $1 }

var_pattern:
  attributes "<ident>" {
    let value = {variable=$2; attributes=$1}
    in {$2 with value} }

object_pattern:
  braces(object_pattern_items) { PObject $1 }

object_pattern_items:
  object_binding_property "," object_pattern_items? {
    match $3 with
    | Some s -> Utils.nsepseq_cons $1 $2 s
    | None -> ($1, [])
  }
| object_binding_property { ($1, []) }
| rest                    { ($1, []) }

object_binding_property:
  "<ident>" "=" expr {
    let region = cover $1.region (expr_to_region $3) in
    let value  = {property=$1; eq=$2; value=$3}
    in PAssign {region; value}
  }
| "<ident>" ":" binding_initializer {
    let region = cover $1.region $3.region
    and value  = {property=$1; colon=$2; target=$3}
    in PDestruct {region; value}
  }
| var_pattern { PVar $1 }

(* Type expressions *)

type_expr:
  fun_type | sum_type | core_type { $1 }

fun_type:
  par(nsepseq(fun_param,",")) "=>" type_expr {
    let stop   = type_expr_to_region $3 in
    let region = cover $1.region stop
    and value  = $1.value, $2, $3
    in TFun {region; value} }

sum_type:
  core_type "|" nsepseq(core_type,"|") {
    let variants = Utils.nsepseq_cons $1 $2 $3 in
    let region   = nsepseq_to_region type_expr_to_region variants
    and value    = {lead_vbar=None; variants; attributes=[]}
    in TSum {region; value}
  }
| attributes "|" nsepseq(core_type,"|") {
   let region = nsepseq_to_region type_expr_to_region $3
   and value  = {lead_vbar = Some $2; variants=$3; attributes=$1}
   in TSum {region; value} }

fun_param:
  "<ident>" ":" type_expr { {name=$1; colon=$2; type_expr=$3} }

core_type:
  "<string>"      { TString $1 }
| "<int>"         { TInt    $1 }
| "_"             { TVar    {value="_"; region=$1} }
| type_name       { TVar    $1 }
| module_access_t { TModA   $1 }
| object_type     { TObject $1 }
| type_ctor_app   { TApp    $1 }
| attributes type_tuple { TProd {inside = $2; attributes = $1} }
| par(type_expr)  { TPar    $1 }

type_tuple:
  brackets(nsepseq(type_expr,",")) { $1 }

type_ctor_app:
  type_name chevrons(nsepseq(type_expr,",")) { (* TODO *)
    let region = cover $1.region $2.region
    in {region; value = $1,$2} }

module_access_t:
  "<uident>" "." module_var_t {
    let start  = $1.region in
    let stop   = type_expr_to_region $3 in
    let region = cover start stop in
    let value  = {module_name=$1; selector=$2; field=$3}
    in {region; value} }

module_var_t:
  module_access_t  { TModA $1 }
| "<ident>"        { TVar  $1 }

object_type:
  attributes "{" sep_or_term_list(field_decl,",") "}" {
    let fields, terminator = $3 in
    let region = cover $2 $4
    and value = {
      compound = Some (Braces ($2,$4));
      ne_elements = fields;
      terminator;
      attributes=$1}
    in {region; value} }

field_decl:
  attributes field_name {
    let value = {
      field_name=$2;
      colon=ghost;  (* TODO: Create a new CST node *)
      field_type = TVar $2;
      attributes=$1}
    in {$2 with value}
  }
| attributes field_name ":" type_expr {
    let stop   = type_expr_to_region $4 in
    let region = cover $2.region stop in
    let value : field_decl = { (* TODO: Use type_annotation *)
      field_name=$2; colon=$3; field_type=$4; attributes= $1}
    in {region; value} }

type_decl:
  "type" type_ident type_params? "=" type_expr {
    let region = cover $1 (type_expr_to_region $5) in
    let value  = {kwd_type=$1; name=$2; params=$3; eq=$4; type_expr=$5}
    in SType {region; value} }

type_params:
  chevrons(nsepseq(type_var,",")) { $1 }

type_ident:
  "<ident>" | "<uident>" { $1 }

switch_stmt:
  "switch" "(" expr ")" "{" nseq(case) "}" {
    let region = cover $1 $7 in
    let value = {kwd_switch=$1; lpar=$2; expr=$3; rpar=$4;
                 lbrace=$5; cases=$6; rbrace=$7}
    in {region; value} }

case:
  "case" expr ":" ioption(statements) {
    Switch_case {kwd_case=$1; expr=$2; colon=$3; statements=$4}
  }
| "default" ":" ioption(statements) {
    Switch_default_case {kwd_default=$1; colon=$2; statements=$3} }

iteration_stmt(right_stmt):
  for_of_stmt(right_stmt) | while_stmt(right_stmt) { $1 }

for_of_stmt(right_stmt):
  "for" "(" index_kind "<ident>" "of" expr_stmt ")" right_stmt {
    let stop   = statement_to_region $8 in
    let region = cover $1 stop
    and value  = {kwd_for=$1; lpar=$2; index_kind=$3; index=$4;
                  kwd_of=$5; expr=$6; rpar=$7; statement=$8}
    in SForOf {region; value} }

index_kind:
  "const" { `Const $1 }
| "let"   { `Let   $1 }

while_stmt(right_stmt):
  "while" "(" expr ")" right_stmt {
    let region = cover $1 (statement_to_region $5)
    and value = {kwd_while=$1; lpar=$2; expr=$3; rpar=$4; statement=$5}
    in SWhile {region; value} }

import_stmt:
  "import" module_name "=" nsepseq(module_name,".") {
    let region = cover $1 (nsepseq_to_region (fun a -> a.region) $4)
    and value = {kwd_import=$1; alias=$2; equal=$3; module_path=$4}
    in {region; value} }

(* TODO: Keep terminator *)
statements:
  sep_or_term_list(statement,";") {
    fst $1 : (statement, semi) Utils.nsepseq }

(* TODO: Keep terminator *)
stmts_opt_namespace:
  sep_or_term_list(stmt_opt_namespace,";") { fst $1 }

(* Expressions *)

fun_expr:
  par(parameters) ioption(type_annotation) "=>" body {
    let region = cover $1.region (body_to_region $4) in
    let value  = {parameters = EPar $1; lhs_type=$2; arrow=$3; body=$4}
    in EFun {region; value}
  }
| "(" ")" ioption(type_annotation) "=>" body {
    let region     = cover $1 $2 in
    let parameters = EUnit {region; value = ($1,$2)} in
    let region     = cover $1 (body_to_region $5) in
    let value      = {parameters; lhs_type=$3; arrow=$4; body=$5}
    in EFun {region; value}
  }
| "<ident>" "=>" body {
    let region     = cover $1.region (body_to_region $3)
    and parameters = EVar $1 in
    let value = {parameters; lhs_type=None; arrow=$2; body=$3}
    in EFun {region; value} }

parameters:
  nsepseq(colon_annot_expr,",") {
    let region = nsepseq_to_region expr_to_region $1
    in ESeq {region; value=$1} }

colon_annot_expr:
  expr type_annotation {
    let colon, type_expr = $2 in
    let start  = expr_to_region $1
    and stop   = type_expr_to_region type_expr in
    let region = cover start stop
    and value  = $1, colon, type_expr
    in EAnnot {region; value} }

body:
  braces(statements) { FunctionBody   $1 }
| expr_stmt          { ExpressionBody $1 }

disj_expr_level:
  bin_op(disj_expr_level, "||", conj_expr_level) {
    ELogic (BoolExpr (Or $1)) }
| conj_expr_level { $1 }

bin_op(arg1,op,arg2):
  arg1 op arg2 {
    let start  = expr_to_region $1 in
    let stop   = expr_to_region $3 in
    let region = cover start stop
    and value  = {arg1=$1; op=$2; arg2=$3}
    in {region; value} }

conj_expr_level:
  conj_expr
| as_annot_expr  (* TODO: Why not at the root of [expr]? *)
| comp_expr_level { $1 }

conj_expr:
  bin_op(conj_expr_level, "&&", comp_expr_level) { ELogic (BoolExpr (And $1)) }

as_annot_expr:
  call_expr_level "as" type_expr {
    let start  = expr_to_region $1
    and stop   = type_expr_to_region $3 in
    let region = cover start stop in
    let value  = $1, $2, $3
    in EAnnot {region; value} }

comp_expr_level:
  bin_op(comp_expr_level, "<", add_expr_level) {
    ELogic (CompExpr (Lt $1)) }
| bin_op(comp_expr_level, "<=", add_expr_level) {
    ELogic (CompExpr (Leq $1)) }
| bin_op(comp_expr_level, gt, add_expr_level) {
    ELogic (CompExpr (Gt $1)) }
| bin_op(comp_expr_level, ">=", add_expr_level) {
    ELogic (CompExpr (Geq $1)) }
| bin_op(comp_expr_level, "==", add_expr_level) {
    ELogic (CompExpr (Equal $1)) }
| bin_op(comp_expr_level, "!=", add_expr_level) {
    ELogic (CompExpr (Neq $1)) }
| add_expr_level { $1 }

add_expr_level:
  bin_op(add_expr_level, "+", mult_expr_level)     {  EArith (Add $1) }
| bin_op(add_expr_level, "-", mult_expr_level)     {  EArith (Sub $1) }
| mult_expr_level                                  {               $1 }

mult_expr_level:
  bin_op(mult_expr_level, "*", unary_expr_level)   { EArith (Mult $1) }
| bin_op(mult_expr_level, "/", unary_expr_level)   {  EArith (Div $1) }
| bin_op(mult_expr_level, "%", unary_expr_level)   {  EArith (Mod $1) }
| unary_expr_level                                 {               $1 }

unary_expr_level:
  "-" call_expr_level {
    let start = $1 in
    let stop = expr_to_region $2 in
    let region = cover start stop
    and value  = {op=$1; arg=$2}
    in EArith (Neg {region; value})
  }
| "!" call_expr_level {
    let start = $1 in
    let stop = expr_to_region $2 in
    let region = cover start stop
    and value  = {op=$1; arg=$2} in
    ELogic (BoolExpr (Not ({region; value})))
  }
| call_expr_level { $1 }

call_expr_level:
  call_expr | new_expr { $1 }

array_item:
  /* */      { Empty_entry Region.ghost }
| expr       { Expr_entry $1 }
| "..." expr {
  let region = cover $1 (expr_to_region $2) in
  let value: array_item_rest = {ellipsis=$1; expr =$2}
  in Rest_entry {region; value} }

array_literal:
  brackets(nsepseq(array_item,",")) { EArray $1 }

property_name:
  "<int>"    {       EArith (Int $1) }
| ctor
| field_name {               EVar $1 }
| "<string>" {   EString (String $1) }

property:
  field_name {
    let region = $1.region
    and value  = EVar $1 in
    Punned_property {region; value}
  }
| property_name ":" expr {
    let region = cover (expr_to_region $1) (expr_to_region $3)
    and value = {name=$1; colon=$2; value=$3}
    in Property {region; value}
  }
| "..." expr_stmt {
    let region = cover $1 (expr_to_region $2)
    and value : CST.property_rest = {ellipsis=$1; expr=$2}
    in Property_rest {region; value} }

object_literal: (* TODO: keep the terminator *)
  braces(sep_or_term_list(property,",") { fst $1 }) { EObject $1 }

member_expr:
  "<ident>"                  {                               EVar $1 }
| "_"                        {       EVar {value = "_"; region = $1} }
| "<int>"                    {                       EArith (Int $1) }
| "<bytes>"                  {                             EBytes $1 }
| "<string>"                 {                   EString (String $1) }
| "<ident>" "<verbatim>"
| "<uident>" "<verbatim>"    {
    let region = cover $1.region $2.region
    and value  = {language = $1; code = EString (Verbatim $2)}
    in ECodeInj {region; value}
  }
| member_expr brackets(expr) {
    let region = cover (expr_to_region $1) $2.region in
    let value  = {expr = $1; selection = Component $2 }
    in EProj {region; value }
  }
| member_expr "." field_name {
    let selection =
      FieldName {region = cover $2 $3.region;
                 value = {dot=$2; value=$3}} in
    let region = cover (expr_to_region $1) $3.region
    and value = {expr=$1; selection}
    in EProj {region; value }
  }
| module_access_e
| array_literal       { $1 }
| par(object_literal)
| par(expr_sequence) { EPar $1 }

expr_sequence:
  nsepseq(expr_stmt,",") {
    let region = nsepseq_to_region expr_to_region $1
    in ESeq {region; value=$1} }

module_access_e :
  module_name "." module_var_e {
    let start       = $1.region in
    let stop        = expr_to_region $3 in
    let region      = cover start stop in
    let value       = {module_name=$1; selector=$2; field=$3}
    in EModA {region; value} }
| ctor "(" ")" {
    EConstr {$1 with value = ($1, None)}
  }
| ctor "(" expr_sequence ")" {
    let region = cover $1.region $4 in
    EConstr {region; value = ($1, Some $3)} }

module_var_e:
  module_access_e   { $1 }
| field_name        { EVar  $1 }
(*
 | "or"              { EVar {value="or"; region=$1} }
 | projection        { EProj $1 }*)

call_expr:
  member_expr par(nsepseq(expr, ","))
| call_expr par(nsepseq(expr, ",")) {
    let start  = expr_to_region $1 in
    let stop   = $2.region in
    let region = cover start stop in
    ECall {region; value = ($1, Multiple $2)}
  }
| call_expr "(" ")"
| member_expr "(" ")" {
    let start  = expr_to_region $1 in
    let stop   = $3 in
    let region = cover start stop
    and value  = $1, Unit {region = cover $2 $3; value = ($2,$3)}
    in ECall {region; value} }

new_expr:
  "new" new_expr {
    let region = cover $1 (expr_to_region $2)
    in ENew {region; value=$1,$2}
  }
| member_expr { $1 }
