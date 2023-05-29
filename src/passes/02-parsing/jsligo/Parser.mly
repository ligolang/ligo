%{
(* START HEADER *)

[@@@warning "-42"]

(* Vendors dependencies *)

open Simple_utils.Region

(* LIGO dependencies *)

module CST = Cst_jsligo.CST
open! CST
module Wrap = Lexing_shared.Wrap

(* Utilities *)

let ghost = Wrap.ghost

let mk_wild_pattern variable =
  let region   = variable#region in
  let variable = Wrap.make "_" region in
  let value    = {variable; attributes=[]}
  in {region; value}

let private_attribute = Wrap.ghost ("private", None)

(* END HEADER *)
%}

%attribute nsepseq(case_statement,SEMI) [@recover.cost 1004]
%attribute nsepseq(statement,SEMI)      [@recover.cost 1004]

(* Reductions on error *)

%on_error_reduce gt
%on_error_reduce nseq(Attr)
%on_error_reduce bin_op(add_expr_level,PLUS,mult_expr_level)
%on_error_reduce bin_op(add_expr_level,MINUS,mult_expr_level)
%on_error_reduce call_expr_level
%on_error_reduce bin_op(disj_expr_level,BOOL_OR,conj_expr_level)
%on_error_reduce type_expr
%on_error_reduce core_type
%on_error_reduce chevrons(type_ctor_args)
%on_error_reduce disj_expr_level
%on_error_reduce member_expr
%on_error_reduce add_expr_level
%on_error_reduce nsepseq(binding_initializer,COMMA)
%on_error_reduce nsepseq(module_name,DOT)
%on_error_reduce base_stmt(statement)
%on_error_reduce unary_expr_level
%on_error_reduce bin_op(comp_expr_level,NE,add_expr_level)
%on_error_reduce bin_op(comp_expr_level,LT,add_expr_level)
%on_error_reduce bin_op(comp_expr_level,LE,add_expr_level)
%on_error_reduce bin_op(comp_expr_level,gt,add_expr_level)
%on_error_reduce bin_op(comp_expr_level,ge,add_expr_level)
%on_error_reduce bin_op(comp_expr_level,EQ2,add_expr_level)
%on_error_reduce expr_stmt
%on_error_reduce expr
%on_error_reduce comp_expr_level
%on_error_reduce conj_expr_level
%on_error_reduce bin_op(conj_expr_level,BOOL_AND,comp_expr_level)
%on_error_reduce return_stmt
%on_error_reduce nsepseq(statement,SEMI)
%on_error_reduce nsepseq(variant,VBAR)
%on_error_reduce nsepseq(object_type,VBAR)
%on_error_reduce nsepseq(field_name,COMMA)
%on_error_reduce module_var_t
%on_error_reduce for_stmt(statement)
%on_error_reduce chevrons(nsepseq(type_param,COMMA))

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
    let region = cover $1#region $3#region
    and value  = {lpar=$1; inside=$2; rpar=$3}
    in {region; value} }

chevrons(X):
  "<" X ">" ioption(ZWSP) {
    let region = cover $1#region $3#region
    and value  = {lchevron=$1; inside=$2; rchevron=$3}
    in {region; value} }

gt:
  ">" ioption(ZWSP) { $1 }

ge:
  ">" ZWSP "=" { Wrap.wrap ">=" (cover $1#region $3#region) }

brackets(X):
  "[" X "]" {
    let lbracket = $1 in
    let rbracket = $3 in
    let region = cover lbracket#region rbracket#region
    and value  = {lbracket; inside=$2; rbracket}
    in {region; value} }

braces(X):
  "{" X "}" {
    let region = cover $1#region $3#region
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
   static control-flow graph. See module [Utils] for the types
   corresponding to the semantic actions of those rules. *)

(* Non-empty sequence of items *)

nseq(X):
  X         { $1, [] }
| X nseq(X) { let hd,tl = $2 in $1, hd::tl }

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
    | (item, next_sep) :: others ->
        trans ((prev_sep,item)::seq, next_sep) others in
    let list, term = trans ([],sep) tail
    in (first, List.rev list), Some term }

(* Helpers *)

%inline type_param  : "<ident>" | "<uident>" { $1 }
%inline field_name  : "<ident>"  { $1 }
%inline module_name : "<uident>" { $1 }
%inline ctor        : "<uident>" { $1 }
%inline type_name   : "<ident>" | "<uident>" { $1 }
%inline module_path : "<string>" { $1 }

(* NOTES *)

(* The reason for rules [if_cond], [while_cond] and [switch_cond],
   instead of the obvious [par(expr)], is meant to identify the
   syntactic construct for error messages. The only [par(expr)] as a
   left-hand side in an LR item corresponds to
   [member_expr: ... | par(expr)]
   so the context is clear: a general expression between parentheses. *)

(* Entry point *)

interactive_expr: expr EOF { $1 }

(* Entry point *)

contract:
  toplevel_stmts EOF { {statements=$1; eof=$2} : CST.t }

(* TOP-LEVEL STATEMENTS *)

toplevel_stmts:
  stmt_or_namespace ";" toplevel_stmts {
    Utils.nseq_cons (TopLevel ($1, Some $2)) $3
  }
| stmt_or_namespace ";"? {
    TopLevel ($1, $2), []
  }
| "<directive>" {
    Directive $1, []
  }
| "<directive>" toplevel_stmts {
    Utils.nseq_cons (Directive $1) $2 }

stmt_or_namespace:
  statement | namespace_stmt { $1 }

(* Attributes *)

%inline
attributes:
  ioption(nseq("[@attr]") { Utils.nseq_to_list $1 }) {
    Option.value ~default:[] $1 }

(* Namespace Statement *)

namespace_stmt:
  attributes ioption("export") namespace {
    let namespace = $3 $1 in
    match $2 with
      Some kwd_export ->
        let region = cover kwd_export#region (statement_to_region namespace)
        in SExport {region; value = (kwd_export, namespace)}
    | None -> namespace }

namespace:
  "namespace" module_name braces(stmts_or_namespace) {
    let region = cover $1#region $3.region
    in fun attrs ->
       SNamespace {region; value=($1,$2,$3, private_attribute::attrs)} }

stmts_or_namespace: (* TODO: Keep terminator *)
  sep_or_term_list(stmt_or_namespace,";") { fst $1 }

(* STATEMENTS *)

statement:
  base_stmt(statement) | if_stmt { $1 }

base_stmt(right_stmt):
  attributes expr_stmt     { $2 $1      }
| return_stmt              { SReturn $1 }
| block_stmt               { SBlock  $1 }
| switch_stmt              { SSwitch $1 }
| import_stmt              { SImport $1 }
| export_decl              { SExport $1 }
| if_else_stmt(right_stmt)
| for_of_stmt(right_stmt)
| for_stmt(right_stmt)
| while_stmt(right_stmt)   { $1 }

closed_stmt:
  base_stmt(closed_stmt) { $1 }

(* Bounded loops *)

for_of_stmt(right_stmt):
  attributes "for" "(" index_kind "<ident>" "of" expr ")" right_stmt {
    let region = cover $2#region (statement_to_region $9)
    and value  = {attributes=$1; kwd_for=$2; lpar=$3; index_kind=$4;
                  index=$5; kwd_of=$6; expr=$7; rpar=$8; statement=$9}
    in SForOf {region; value} }

%inline
index_kind:
  "const" { `Const $1 }
| "let"   { `Let   $1 }

(* Unbounded loops *)

while_stmt(right_stmt):
  "while" par(while_cond) right_stmt {
    let cond : expr par reg = $2 in
    let {lpar; inside=expr; rpar} : expr par = cond.value in
    let region = cover $1#region (statement_to_region $3)
    and value = {kwd_while=$1; lpar; expr; rpar; statement=$3}
    in SWhile {region; value} }

while_cond:
  expr { $1 }

for_initialiser:
  expr_stmt { $1 }

for_stmt(right_stmt):
  attributes "for" "("
    ioption(for_initialiser) ";"
    ioption(expr) ";"
    ioption(nsepseq(closed_non_decl_expr_stmt, ","))
  ")" ioption(right_stmt) {
    let initialiser = Core.Option.map $4 ~f:(fun f -> f [])
    and condition    = $6
    and afterthought = $8
    and statement    = $10 in
    let region_end   =
      match $10 with
        Some s -> statement_to_region s
      | None   -> $9#region
    in
    let region = cover $2#region region_end
    and value = {
      attributes=$1;
      kwd_for=$2;
      lpar=$3;
      initialiser;
      semi1=$5;
      condition;
      semi2=$7;
      afterthought;
      rpar=$9;
      statement;
    }
    in SFor {region;value}
  }

(* Expressions as Statements *)

expr_stmt:
  declaration                   { $1 }
| non_decl_expr_stmt(expr_stmt) { fun attrs -> SExpr (attrs, $1) }

non_decl_expr_stmt(right_stmt):
  assign_stmt                                  { EAssign $1 }
| increment_decrement_operators
| call_expr
| as_expr
| ternary_expr(non_decl_expr_stmt(right_stmt)) { $1 }

closed_non_decl_expr_stmt:
  non_decl_expr_stmt(closed_non_decl_expr_stmt) { $1 }

assign_lhs:
  projection  { EProj $1 }
| "<ident>"   { EVar  $1 }

assign_stmt:
  assign_lhs "="  expr {
    $1, {value = Eq; region = $2#region}, $3 }
| assign_lhs "*=" expr {
    $1, {value = Assignment_operator Times_eq; region=$2#region}, $3 }
| assign_lhs "/=" expr {
    $1, {value = Assignment_operator Div_eq; region=$2#region}, $3 }
| assign_lhs "%=" expr {
    $1, {value = Assignment_operator Mod_eq; region=$2#region}, $3 }
| assign_lhs "+=" expr {
    $1, {value = Assignment_operator Plus_eq; region = $2#region}, $3 }
| assign_lhs "-=" expr {
    $1, {value = Assignment_operator Min_eq; region = $2#region}, $3 }

ternary_expr(expr):
  disj_expr_level "?" expr ":" expr {
    let start  = expr_to_region $1
    and stop   = expr_to_region $5 in
    let region = cover start stop in
    let value  = {condition=$1; qmark=$2; truthy=$3; colon=$4; falsy=$5}
    in ETernary {value; region}}

increment_decrement_operators:
  "++" "<ident>" {
    let region = cover $1#region $2#region
    and update_type = Increment $1 in
    let value = {update_type; variable=$2}
    in EPrefix {region; value}
  }
| "--" "<ident>" {
    let region = cover $1#region $2#region
    and update_type = Decrement $1 in
    let value = {update_type; variable=$2}
    in EPrefix {region; value}
  }
| "<ident>" "++" {
    let region = cover $1#region $2#region
    and update_type = Increment $2 in
    let value = {update_type; variable=$1}
    in EPostfix {region; value}
  }
| "<ident>" "--" {
    let region = cover $1#region $2#region
    and update_type = Decrement $2 in
    let value  = {update_type; variable=$1}
    in EPostfix {region; value}
  }

(* Expressions *)

expr:
  fun_expr           { EFun $1 }
| ternary_expr(expr)
| as_expr
| disj_expr_level    { $1 }

as_expr:
  call_expr_level "as" type_expr {
    let stop   = type_expr_to_region $3 in
    let region = cover (expr_to_region $1) stop
    in EAnnot {region; value = $1,$2,$3} }

disj_expr_level:
  bin_op(disj_expr_level, "||", conj_expr_level) {
    ELogic (BoolExpr (Or $1)) }
| conj_expr_level { $1 }

bin_op(arg1,op,arg2):
  arg1 op arg2 {
    let region = cover (expr_to_region $1) (expr_to_region $3)
    and value  = {arg1=$1; op=$2; arg2=$3}
    in {region; value} }

conj_expr_level:
  bin_op(conj_expr_level, "&&", comp_expr_level) {
    ELogic (BoolExpr (And $1)) }
| comp_expr_level { $1 }

comp_expr_level:
  bin_op(comp_expr_level, "<", add_expr_level)  {
    ELogic (CompExpr (Lt $1)) }
| bin_op(comp_expr_level, "<=", add_expr_level) {
    ELogic (CompExpr (Leq $1)) }
| bin_op(comp_expr_level, gt, add_expr_level)   {
    ELogic (CompExpr (Gt $1)) }
| bin_op(comp_expr_level, ge, add_expr_level) {
    ELogic (CompExpr (Geq $1)) }
| bin_op(comp_expr_level, "==", add_expr_level) {
    ELogic (CompExpr (Equal $1)) }
| bin_op(comp_expr_level, "!=", add_expr_level) {
    ELogic (CompExpr (Neq $1)) }
| add_expr_level { $1 }

add_expr_level:
  bin_op(add_expr_level, "+", mult_expr_level)   {  EArith (Add $1) }
| bin_op(add_expr_level, "-", mult_expr_level)   {  EArith (Sub $1) }
| mult_expr_level                                {               $1 }

mult_expr_level:
  bin_op(mult_expr_level, "*", unary_expr_level) { EArith (Mult $1) }
| bin_op(mult_expr_level, "/", unary_expr_level) {  EArith (Div $1) }
| bin_op(mult_expr_level, "%", unary_expr_level) {  EArith (Mod $1) }
| unary_expr_level                               {               $1 }

unary_expr_level:
  "-" call_expr_level {
    let region = cover $1#region (expr_to_region $2)
    and value  = {op=$1; arg=$2}
    in EArith (Neg {region; value})
  }
| "!" call_expr_level {
    let region = cover $1#region (expr_to_region $2)
    and value  = {op=$1; arg=$2} in
    ELogic (BoolExpr (Not ({region; value})))
  }
| increment_decrement_operators
| call_expr_level { $1 }

call_expr_level:
  call_expr | member_expr { $1 }

(* Function calls *)

call_expr:
  "contract_of" "(" nsepseq(module_name,".") ")" {
    let region = cover $1#region $4#region
    in EContract {region; value=$3 }
  }
| lambda par(ioption(nsepseq(fun_arg,","))) {
    let par    = $2.value in
    let region = cover (expr_to_region $1) $2.region in
    let args   =
      match par.inside with
        None ->
          Unit {region=$2.region; value = (par.lpar, par.rpar)}
      | Some args ->
          Multiple {$2 with value = {par with inside=args}}
    in ECall {region; value = ($1, args)} }

lambda:
  call_expr
| member_expr { $1 } (* TODO: specialise *)

fun_arg:
  expr { $1 }

(* General expressions *)

member_expr:
  "_" | "<ident>" { EVar     $1            }
| "<int>"         { EArith   (Int $1)      }
| "<bytes>"       { EBytes   $1            }
| "<string>"      { EString  (String $1)   }
| "<verbatim>"    { EString  (Verbatim $1) }
| ctor_expr       { EConstr  $1            }
| projection      { EProj    $1            }
| code_inj        { ECodeInj $1            }
| par(expr)       { EPar     $1            }
| module_access_e { EModA    $1            }
| array_literal   { EArray   $1            }
| object_literal  { EObject  $1            }

(* Qualified values *)

module_access_e:
  module_name "." module_var_e {
    let region = cover $1#region (expr_to_region $3)
    and value  = {module_name=$1; selector=$2; field=$3}
    in {region; value} }

module_var_e:
  module_access_e { EModA $1 }
| field_name      { EVar  $1 }
(*| projection      { EProj $1 } TODO *)

(* Code injection *)

code_inj:
  "<ident>" "<verbatim>"
| "<uident>" "<verbatim>" {
    let region = cover $1#region $2#region
    and value  = {language=$1; code = EString (Verbatim $2)}
    in {region; value} }

(* Tuple projection *)

projection:
  member_expr brackets(expr) {
    let region = cover (expr_to_region $1) $2.region in
    let value  = {expr=$1; selection = Component $2 }
    in {region; value}
  }
| member_expr "." field_name {
    let selection =
      FieldName {region = cover $2#region $3#region;
                 value  = {dot=$2; value=$3}} in
    let region = cover (expr_to_region $1) $3#region
    and value  = {expr=$1; selection}
    in {region; value} }

(* Constructor applications *)

ctor_expr:
  ctor "(" ctor_args? ")" {
    let region = cover $1#region $4#region
    in {region; value = ($1,$3)} }

ctor_args:
  nsepseq(ctor_arg,",") {
    let region = nsepseq_to_region expr_to_region $1
    in ESeq {region; value=$1} }

ctor_arg:
  expr { $1 }

(* Export Declaration *)

export_decl:
  attributes "export" attributes declaration {
    let declaration = $4 ($1 @ $3)  in
    let region = cover $2#region (statement_to_region declaration)
    in {region; value = ($2, declaration)} }

(* Block of Statements *)

block_stmt:
  braces(statements) { $1 : (statement, semi) Utils.nsepseq braces reg}

(* Switch Statement *)

switch_stmt:
  "switch" par(switch_cond) braces(cases) {
    let {lpar; inside=expr; rpar} = ($2 : expr par reg).value
    and {lbrace; inside=cases; rbrace} = ($3 : _ braces reg).value in
    let region = cover $1#region $3.region
    and value = {kwd_switch=$1; lpar; expr; rpar; lbrace; cases; rbrace}
    in {region; value} }

switch_cond:
  expr { $1 }

cases:
  nseq(case) ioption(default_case) {
    match $2 with
      None -> $1
    | Some default ->
       Utils.(nseq_rev $1 |> nseq_cons default |> nseq_rev)
  }
| default_case { $1,[] }

case:
  "case" expr ":" ioption(case_statements) {
    Switch_case {kwd_case=$1; expr=$2; colon=$3; statements=$4} }

default_case:
  "default" ":" ioption(case_statements) {
    Switch_default_case {kwd_default=$1; colon=$2; statements=$3} }

case_statements:
  sep_or_term_list(case_statement,";") {
    fst $1 : (statement, semi) Utils.nsepseq }

case_statement:
  statement { $1 }
| "break"   { SBreak $1 }

(* Return Statements *)

return_stmt:
  "return" {
    let value = {kwd_return=$1; expr=None}
    in {region=$1#region; value}
  }
| "return" expr {
    let region = cover $1#region (expr_to_region $2)
    and value  = {kwd_return=$1; expr = Some $2}
    in {region; value} }

(* Conditional Statements *)

if_stmt:
  attributes "if" par(if_cond) statement {
    let region = cover $2#region (statement_to_region $4) in
    let value  = {attributes=$1; kwd_if=$2; test=$3.value;
                  ifso=$4; ifnot=None}
    in SCond {region; value} }

if_else_stmt(right_stmt):
  "if" par(if_cond) closed_stmt "else" right_stmt {
    let region = cover $1#region (statement_to_region $5)
    and value  = {attributes=[]; kwd_if=$1; test=$2.value;
                  ifso=$3; ifnot = Some ($4,$5)}
    in SCond {region; value} }

if_cond:
  expr { $1 }

(* Array Patterns *)

array_pattern:
  brackets(array_item_patterns) { PArray $1 }

array_item_patterns:
  array_item_pattern {
    $1, []
  }
| array_item_patterns "," array_item_pattern {
    Utils.(nsepseq_rev $1 |> nsepseq_cons $3 ($2) |> nsepseq_rev)
  }
| array_item_patterns "," array_rest_pattern  {
    Utils.(nsepseq_rev $1 |> nsepseq_cons $3 ($2) |> nsepseq_rev) }

array_item_pattern:
  "_"           { PVar (mk_wild_pattern $1) }
| var_pattern   { PVar $1 }
| array_pattern { $1 }

array_rest_pattern:
  "..." "<ident>" {
    let region = cover $1#region $2#region
    and value  = {ellipsis=$1; rest=$2}
    in PRest {region; value} }

type_annotation:
  ":" type_expr { $1, $2 }

(* DECLARATIONS *)

declaration:
  let_decl | const_decl | type_decl { $1 }

let_decl:
  "let" binding_list {
    let stop   = nsepseq_to_region (fun e -> e.region) $2 in
    let region = cover $1#region stop
    and mk_value attr =
      {kwd_let=$1; bindings=$2; attributes=private_attribute::attr}
    in fun attr -> SLet {region; value = mk_value attr} }

const_decl:
  "const" binding_list {
    let stop   = nsepseq_to_region (fun e -> e.region) $2 in
    let region = cover $1#region stop
    and mk_value attributes = {kwd_const=$1; bindings=$2; attributes}
    in fun attr -> SConst {region; value = mk_value attr} }

(* PATTERNS *)

binding_list:
  nsepseq(binding_initializer,",") { $1 }

binding_initializer:
  binding_pattern ioption(binding_type) "=" expr {
    let lhs_type, type_params =
      match $2 with
        None -> None, None
      | Some (a,b) -> Some a, b in
    let start  = pattern_to_region $1
    and stop   = expr_to_region $4 in
    let region = cover start stop
    and value  = {binders=$1; type_params; lhs_type; eq=$3; expr=$4}
    in {region; value} }

binding_type:
  ":" ioption(type_parameters) type_expr { ($1,$3), $2 }

type_parameters:
  chevrons(nsepseq(type_param,",")) { $1 }

binding_pattern:
  "_"            { PVar (mk_wild_pattern $1) }
| var_pattern    { PVar $1 }
| object_pattern
| array_pattern  { $1 }

var_pattern:
  attributes "<ident>" {
    let value = {variable=$2; attributes=$1}
    in {region=$2#region; value} }

(* Record patterns (a.k.a. "object patterns" in JS) *)

object_pattern:
  braces(property_patterns) { PObject $1 }

object_sep:
  ";" | "," { $1 }

property_patterns:
  property_pattern {
    $1, []
  }
| property_patterns object_sep property_pattern {
    Utils.(nsepseq_rev $1 |> nsepseq_cons $3 ($2) |> nsepseq_rev)
  }
| property_patterns object_sep object_rest_pattern {
    Utils.(nsepseq_rev $1 |> nsepseq_cons $3 $2 |> nsepseq_rev) }

property_pattern:
  "<ident>" "=" expr {
    let region = cover $1#region (expr_to_region $3)
    and value  = {property=$1; eq=$2; value=$3}
    in PAssign {region; value}
  }
| "<ident>" ":" binding_initializer {
    let region = cover $1#region $3.region
    and value  = {property=$1; colon=$2; target=$3}
    in PDestruct {region; value}
  }
| var_pattern { PVar $1 }

object_rest_pattern:
  "..." "<ident>" {
    let region = cover $1#region $2#region
    and value  = {ellipsis=$1; rest=$2}
    in PRest {region; value} }

(* Type declarations *)

type_decl:
  "type" type_name ioption(type_params) "=" type_expr {
    let region = cover $1#region (type_expr_to_region $5) in
    let mk_value attr =
      {kwd_type=$1; name=$2; params=$3; eq=$4; type_expr=$5;
       attributes=private_attribute::attr}
    in fun attr -> SType {region; value = mk_value attr} }

type_params:
  chevrons(nsepseq(type_param,",")) { $1 }

(* TYPE EXPRESSIONS *)

type_expr:
  fun_type | sum_type | core_type { $1 }

(* Functional types *)

fun_type:
  ES6FUN par(nsepseq(fun_param,",")) "=>" type_expr {
    let stop   = type_expr_to_region $4 in
    let region = cover $2.region stop
    and value  = $2.value, $3, $4
    in TFun {region; value} }

fun_param:
  "<ident>" type_annotation {
    let colon, type_expr = $2
    in {name=$1; colon; type_expr} }

(* Sum types *)

sum_type:
  attributes "|" nsepseq(variant, "|") {
    let stop     = nsepseq_to_region (fun x -> x.region) $3 in
    let region   = cover $2#region stop in
    let variants = {region; value=$3} in
    let value    = {attributes=$1; leading_vbar = Some $2; variants}
    in TSum {region; value}
  }
| nsepseq(variant, "|") {
    let region   = nsepseq_to_region (fun x -> x.region) $1 in
    let variants = {region; value=$1} in
    let value    = {attributes=[]; leading_vbar=None; variants}
    in TSum {region; value} }

variant:
  attributes brackets(variant_comp) {
    let region = $2.region
    and value  = {attributes=$1; tuple=$2}
    in {region; value} }

%inline
variant_comp:
  "<string>"                 { {constr=$1; params = None}         }
| "<string>" "," ctor_params { {constr=$1; params = Some ($2,$3)} }

ctor_params:
  nsepseq(ctor_param,",") { $1 }

ctor_param:
  type_expr { $1 }

(* Core types *)

core_type:
  "<string>"            { TString $1 }
| core_type_no_string   { $1}

%inline
core_type_no_string:
  "<int>"               { TInt    $1 }
| "_" | type_name       { TVar    $1 }
| parameter_of_type     {         $1 }
| module_access_t       { TModA   $1 }
| union_type            {         $1 }
| type_ctor_app         { TApp    $1 }
| attributes type_tuple { TProd   {inside=$2; attributes=$1} }
| par(type_expr)        { TPar    $1 }

(* Union type (see sum type) *)

union_type:
  ioption("|" { $1 }) nsepseq(object_type, "|") {
    match $2 with
      obj, [] -> TObject obj
    | _       -> TDisc $2 }

(* Tuples of types *)

(* The production [core_type_no_string] is here to avoid a conflict
   with a variant for a constant contructor, e.g. [["C"]], which could
   be interpreted otherwise as an type tuple (array) of the type
   ["C"]. *)

type_tuple:
  brackets(type_components) { $1 }

type_components:
  type_component_no_string { $1,[] }
| type_component_no_string "," nsepseq(type_component,",") {
    Utils.nsepseq_cons $1 $2 $3 }

type_component_no_string:
  fun_type | sum_type | core_type_no_string { $1 }

type_component:
  type_expr { $1 }

(* Parameter of contract *)

parameter_of_type:
  "parameter_of" nsepseq(module_name,".") {
    let stop   = nsepseq_to_region (fun x -> x#region) $2 in
    let region = cover $1#region stop
    in TParameter {region; value=$2} }

(* Application of type arguments to type constructors *)

type_ctor_app:
  type_name chevrons(type_ctor_args) {
    let region = cover $1#region $2.region
    in {region; value = ($1,$2)} }

type_ctor_args:
  nsepseq(type_ctor_arg,",") { $1 }

type_ctor_arg:
  type_expr { $1 }

(* Selection of types in modules (a.k.a. qualified type name) *)

module_access_t:
  module_name "." module_var_t {
    let stop   = type_expr_to_region $3 in
    let region = cover $1#region stop
    and value  = {module_name=$1; selector=$2; field=$3}
    in {region; value} }

module_var_t:
  module_access_t { TModA $1 }
| "<ident>"       { TVar  $1 }
| type_ctor_app   { TApp  $1 }

(* Record types (a.k.a. "object types" in JS) *)

object_type:
  attributes "{" sep_or_term_list(field_decl,object_sep) "}" {
    let lbrace = $2 in
    let rbrace = $4 in
    let fields, terminator = $3 in
    let region = cover lbrace#region rbrace#region
    and value = {
      compound = Some (Braces (lbrace,rbrace));
      ne_elements = fields;
      terminator;
      attributes=$1}
    in {region; value} }

field_decl:
  attributes field_name {
    let value = {
      field_name=$2;
      colon = ghost ":";  (* TODO: Create a "new" CST node *)
      field_type = TVar (ghost $2#payload); (* TODO *)
      attributes=$1}
    in {value; region = $2#region}
  }
| attributes field_name type_annotation {
    let colon, field_type = $3 in
    let stop   = type_expr_to_region field_type in
    let region = cover $2#region stop in
    let value : field_decl = {
      field_name=$2; colon; field_type; attributes= $1}
    in {region; value} }

(* Import statement *)

import_stmt:
  "import" module_name "=" nsepseq(module_name,".") {
    let region =
      cover $1#region (nsepseq_to_region (fun a -> a#region) $4)
    and value =
      Import_rename {kwd_import=$1; alias=$2; equal=$3; module_path=$4}
    in {region; value}
  }
| "import" "*" "as" module_name "from" module_path {
    let region = cover $1#region $6#region in
    let value =
      Import_all_as {kwd_import=$1; times=$2; kwd_as=$3; alias=$4;
                     kwd_from=$5; module_path=$6}
    in {region; value}
  }
| "import" braces(nsepseq(field_name, ",")) "from" module_path {
    let region = cover $1#region $4#region in
    let value =
      Import_selected {kwd_import=$1; imported=$2; kwd_from=$3;
                       module_path=$4}
    in {region; value} }

(* Statements *)

(* TODO: Keep terminator *)
statements:
  sep_or_term_list(statement,";") {
    fst $1 : (statement, semi) Utils.nsepseq }

(* Expressions *)

fun_expr:
  ioption(type_parameters) ES6FUN par(parameters)
  ioption(type_annotation) "=>" body {
    let region = cover $3.region (body_to_region $6) in
    let value  = {type_params=$1; parameters = EPar $3;
                  lhs_type=$4; arrow=$5; body=$6}
    in {region; value}
  }
| ioption(type_parameters) ES6FUN "(" ")" ioption(type_annotation) "=>" body {
    let region     = cover $3#region $4#region in
    let parameters = EUnit {region; value = ($3,$4)} in
    let region     = cover $3#region (body_to_region $7) in
    let value      = {type_params=$1; parameters; lhs_type=$5; arrow=$6; body=$7}
    in {region; value}
 }
| ES6FUN "<ident>" "=>" body
| ES6FUN "_" "=>" body {
    let region     = cover $2#region (body_to_region $4)
    and parameters = EVar $2 in
    let value = {type_params=None; parameters; lhs_type=None; arrow=$3; body=$4}
    in {region; value} }

parameters:
  nsepseq(parameter,",") {
    let region = nsepseq_to_region expr_to_region $1
    in ESeq {region; value=$1} }

(* Note: we use [expr] to avoid an LR conflict, and obtain instead
   the item
   ## par(expr) -> LPAR expr . RPAR [ ... ]
   ## parameter -> expr . type_annotation [ RPAR COMMA ]
*)

parameter:
  expr ioption(type_annotation) {
    match $2 with
      Some (colon, type_expr) ->
        let start = expr_to_region $1 in
        let stop = type_expr_to_region type_expr in
        let region = cover start stop in
        EAnnot { region; value = $1, colon, type_expr }
    | None -> $1 }

body:
  braces(statements) { FunctionBody   $1 }
| expr               { ExpressionBody $1 }

(* Tuples (a.k.a "arrays" is JS) *)

array_item:
  expr       { Expr_entry $1 }
| "..." expr {
    let region = cover $1#region (expr_to_region $2) in
    let value : array_item_rest = {ellipsis=$1; expr=$2}
    in Rest_entry {region; value} }

array_literal:
  brackets(ioption(nsepseq(array_item,","))) { $1 }

(* Records (a.k.a. "objects" in JS) *)

object_literal: (* TODO: keep the terminator *)
  braces(sep_or_term_list(property,object_sep) { fst $1 }) { $1 }

property:
  field_name {
    let region = $1#region in
    Punned_property {region; value = EVar $1}
  }
| attributes property_name ":" expr {
    let region = cover (expr_to_region $2) (expr_to_region $4)
    and value  = {attributes=$1; name=$2; colon=$3; value=$4}
    in Property {region; value}
  }
| "..." expr {
    let region = cover $1#region (expr_to_region $2)
    and value : property_rest = {ellipsis=$1; expr=$2}
    in Property_rest {region; value} }

property_name:
  "<int>"    { EArith  (Int $1)    }
| "<string>" { EString (String $1) }
| ctor
| field_name { EVar $1 }
