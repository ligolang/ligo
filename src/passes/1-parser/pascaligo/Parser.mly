%{
(* START HEADER *)

[@@@warning "-42"]

open Region
open AST

(* END HEADER *)
%}

(* See [ParToken.mly] for the definition of tokens. *)

(* Entry points *)

%start contract interactive_expr
%type <AST.t> contract
%type <AST.expr> interactive_expr

%%

(* RULES *)

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

(* Compound constructs *)

par(X):
  "(" X ")" {
    let region = cover $1 $3
    and value  = {
      lpar   = $1;
      inside = $2;
      rpar   = $3}
    in {region; value} }

brackets(X):
  "[" X "]" {
    let region = cover $1 $3
    and value  = {
      lbracket = $1;
      inside   = $2;
      rbracket = $3}
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
   for the types corresponding to the semantic actions of those rules.
 *)

(* Possibly empty sequence of items *)

seq(X):
  (**)     {     [] }
| X seq(X) { $1::$2 }

(* Non-empty sequence of items *)

nseq(X):
  X seq(X) { $1,$2 }

(* Non-empty separated sequence of items *)

nsepseq(X,Sep):
  X                    {                 $1,        [] }
| X Sep nsepseq(X,Sep) { let h,t = $3 in $1, ($2,h)::t }

(* Possibly empy separated sequence of items *)

sepseq(X,Sep):
  (**)           {    None }
| nsepseq(X,Sep) { Some $1 }

(* Inlines *)

%inline var         : "<ident>" { $1 }
%inline type_name   : "<ident>" { $1 }
%inline fun_name    : "<ident>" { $1 }
%inline field_name  : "<ident>" { $1 }
%inline struct_name : "<ident>" { $1 }

(* Main *)

contract:
  nseq(declaration) EOF { {decl=$1; eof=$2} }

declaration:
  type_decl  {  TypeDecl $1 }
| const_decl { ConstDecl $1 }
| fun_decl   {   FunDecl $1 }
| attr_decl  {  AttrDecl $1 }

(* Attribute declarations *)

attr_decl:
  open_attr_decl ";"? { $1 }

open_attr_decl:
  ne_injection("attributes","<string>") { $1 }

(* Type declarations *)

type_decl:
  "type" type_name "is" type_expr ";"? {
    Scoping.check_reserved_name $2;
    let stop =
      match $5 with
        Some region -> region
      |        None -> type_expr_to_region $4 in
    let region = cover $1 stop in
    let value  = {kwd_type   = $1;
                  name       = $2;
                  kwd_is     = $3;
                  type_expr  = $4;
                  terminator = $5}
    in {region; value} }

type_expr:
  fun_type | sum_type | record_type { $1 }

fun_type:
  cartesian { $1 }
| cartesian "->" fun_type {
    let start  = type_expr_to_region $1
    and stop   = type_expr_to_region $3 in
    let region = cover start stop in
    TFun {region; value = $1,$2,$3} }

cartesian:
  core_type { $1 }
| core_type "*" nsepseq(core_type,"*") {
    let value  = Utils.nsepseq_cons $1 $2 $3 in
    let region = nsepseq_to_region type_expr_to_region value
    in TProd {region; value} }

core_type:
  type_name      { TVar $1 }
| par(type_expr) { TPar $1 }
| type_name type_tuple {
    let region = cover $1.region $2.region
    in TApp {region; value = $1,$2}
  }
| "map" type_tuple {
    let region = cover $1 $2.region in
    let type_constr = {value="map"; region=$1}
    in TApp {region; value = type_constr, $2}
  }
| "big_map" type_tuple {
    let region = cover $1 $2.region in
    let type_constr = {value="big_map"; region=$1}
    in TApp {region; value = type_constr, $2}
  }
| "set" par(type_expr) {
    let total = cover $1 $2.region in
    let type_constr = {value="set"; region=$1} in
    let {region; value = {lpar; inside; rpar}} = $2 in
    let tuple = {region; value={lpar; inside=inside,[]; rpar}}
    in TApp {region=total; value = type_constr, tuple}
  }
| "list" par(type_expr) {
    let total = cover $1 $2.region in
    let type_constr = {value="list"; region=$1} in
    let {region; value = {lpar; inside; rpar}} = $2 in
    let tuple = {region; value={lpar; inside=inside,[]; rpar}}
    in TApp {region=total; value = type_constr, tuple}
  }

type_tuple:
  par(nsepseq(type_expr,",")) { $1 }

sum_type:
  "|"? nsepseq(variant,"|") {
    Scoping.check_variants (Utils.nsepseq_to_list $2);
    let region = nsepseq_to_region (fun x -> x.region) $2
    in TSum {region; value=$2} }

variant:
  "<constr>" { {$1 with value = {constr=$1; arg=None}} }
| "<constr>" "of" fun_type {
    let region = cover $1.region (type_expr_to_region $3)
    and value  = {constr=$1; arg = Some ($2,$3)}
    in {region; value} }

record_type:
  "record" sep_or_term_list(field_decl,";") "end" {
    let ne_elements, terminator = $2 in
    let () = Utils.nsepseq_to_list ne_elements
             |> Scoping.check_fields in
    let region = cover $1 $3
    and value  = {opening = Kwd $1;
                  ne_elements;
                  terminator;
                  closing = End $3}
    in TRecord {region; value}
  }
| "record" "[" sep_or_term_list(field_decl,";") "]" {
   let ne_elements, terminator = $3 in
   let region = cover $1 $4
   and value  = {opening = KwdBracket ($1,$2);
                 ne_elements;
                 terminator;
                 closing = RBracket $4}
   in TRecord {region; value} }

field_decl:
  field_name ":" type_expr {
    let stop   = type_expr_to_region $3 in
    let region = cover $1.region stop
    and value  = {field_name=$1; colon=$2; field_type=$3}
    in {region; value} }


fun_expr:
  "function" parameters ":" type_expr "is" expr {
    let stop   = expr_to_region $6 in
    let region = cover $1 stop
    and value  = {kwd_function = $1;
                  param        = $2;
                  colon        = $3;
                  ret_type     = $4;
                  kwd_is       = $5;
                  return       = $6}
    in {region; value} }

(* Function declarations *)

open_fun_decl:
  "function" fun_name parameters ":" type_expr "is"
  block "with" expr {
    Scoping.check_reserved_name $2;
    let stop   = expr_to_region $9 in
    let region = cover $1 stop
    and value  = {kwd_function = $1;
                  fun_name     = $2;
                  param        = $3;
                  colon        = $4;
                  ret_type     = $5;
                  kwd_is       = $6;
                  block_with   = Some ($7, $8);
                  return       = $9;
                  terminator   = None;
                  attributes   = None}
    in {region; value}
  }
| "function" fun_name parameters ":" type_expr "is" expr {
    Scoping.check_reserved_name $2;
    let stop   = expr_to_region $7 in
    let region = cover $1 stop
    and value  = {kwd_function = $1;
                  fun_name     = $2;
                  param        = $3;
                  colon        = $4;
                  ret_type     = $5;
                  kwd_is       = $6;
                  block_with   = None;
                  return       = $7;
                  terminator   = None;
                  attributes   = None}
    in {region; value} }

fun_decl:
  open_fun_decl ";"? {
    {$1 with value = {$1.value with terminator=$2}} }

parameters:
  par(nsepseq(param_decl,";")) {
    let params =
      Utils.nsepseq_to_list ($1.value: _ par).inside
    in Scoping.check_parameters params; $1 }

param_decl:
  "var" var ":" param_type {
    Scoping.check_reserved_name $2;
    let stop   = type_expr_to_region $4 in
    let region = cover $1 stop
    and value  = {kwd_var    = $1;
                  var        = $2;
                  colon      = $3;
                  param_type = $4}
    in ParamVar {region; value}
  }
| "const" var ":" param_type {
    Scoping.check_reserved_name $2;
    let stop   = type_expr_to_region $4 in
    let region = cover $1 stop
    and value  = {kwd_const  = $1;
                  var        = $2;
                  colon      = $3;
                  param_type = $4}
    in ParamConst {region; value} }

param_type:
  fun_type { $1 }

block:
  "begin" sep_or_term_list(statement,";") "end" {
     let statements, terminator = $2 in
     let region = cover $1 $3
     and value  = {opening = Begin $1;
                   statements;
                   terminator;
                   closing = End $3}
     in {region; value}
  }
| "block" "{" sep_or_term_list(statement,";") "}" {
     let statements, terminator = $3 in
     let region = cover $1 $4
     and value  = {opening = Block ($1,$2);
                   statements;
                   terminator;
                   closing = Block $4}
     in {region; value} }

statement:
  instruction     { Instr $1 }
| open_data_decl  { Data  $1 }
| open_attr_decl  { Attr  $1 }

open_data_decl:
  open_const_decl { LocalConst $1 }
| open_var_decl   { LocalVar   $1 }
| open_fun_decl   { LocalFun   $1 }

open_const_decl:
  "const" unqualified_decl("=") {
    let name, colon, const_type, equal, init, stop = $2 in
    let region = cover $1 stop
    and value  = {kwd_const = $1;
                  name;
                  colon;
                  const_type;
                  equal;
                  init;
                  terminator = None;
                  attributes = None}
    in {region; value} }

open_var_decl:
  "var" unqualified_decl(":=") {
    let name, colon, var_type, assign, init, stop = $2 in
    let region = cover $1 stop
    and value  = {kwd_var = $1;
                  name;
                  colon;
                  var_type;
                  assign;
                  init;
                  terminator=None}
    in {region; value} }

unqualified_decl(OP):
  var ":" type_expr OP expr {
    Scoping.check_reserved_name $1;
    let region = expr_to_region $5
    in $1, $2, $3, $4, $5, region }

const_decl:
  open_const_decl ";"? {
    {$1 with value = {$1.value with terminator=$2}} }

instruction:
  conditional  {        Cond $1 }
| case_instr   {   CaseInstr $1 }
| assignment   {      Assign $1 }
| loop         {        Loop $1 }
| proc_call    {    ProcCall $1 }
| "skip"       {        Skip $1 }
| record_patch { RecordPatch $1 }
| map_patch    {    MapPatch $1 }
| set_patch    {    SetPatch $1 }
| map_remove   {   MapRemove $1 }
| set_remove   {   SetRemove $1 }

set_remove:
  "remove" expr "from" "set" path {
    let region = cover $1 (path_to_region $5) in
    let value  = {
      kwd_remove = $1;
      element    = $2;
      kwd_from   = $3;
      kwd_set    = $4;
      set        = $5}
    in {region; value} }

map_remove:
  "remove" expr "from" "map" path {
    let region = cover $1 (path_to_region $5) in
    let value  = {
      kwd_remove = $1;
      key        = $2;
      kwd_from   = $3;
      kwd_map    = $4;
      map        = $5}
    in {region; value} }

set_patch:
  "patch" path "with" ne_injection("set",expr) {
    let region = cover $1 $4.region in
    let value  = {
      kwd_patch = $1;
      path      = $2;
      kwd_with  = $3;
      set_inj   = $4}
    in {region; value} }

map_patch:
  "patch" path "with" ne_injection("map",binding) {
    let region = cover $1 $4.region in
    let value  = {
      kwd_patch = $1;
      path      = $2;
      kwd_with  = $3;
      map_inj   = $4}
    in {region; value} }

injection(Kind,element):
  Kind sep_or_term_list(element,";") "end" {
    let elements, terminator = $2 in
    let region = cover $1 $3
    and value = {
      opening  = Kwd $1;
      elements = Some elements;
      terminator;
      closing = End $3}
    in {region; value}
  }
| Kind "end" {
    let region = cover $1 $2
    and value = {
      opening    = Kwd $1;
      elements   = None;
      terminator = None;
      closing    = End $2}
    in {region; value}
  }
| Kind "[" sep_or_term_list(element,";") "]" {
    let elements, terminator = $3 in
    let region = cover $1 $4
    and value = {
      opening  = KwdBracket ($1,$2);
      elements = Some elements;
      terminator;
      closing = RBracket $4}
    in {region; value}
  }
| Kind "[" "]" {
    let region = cover $1 $3
    and value = {
      opening    = KwdBracket ($1,$2);
      elements   = None;
      terminator = None;
      closing    = RBracket $3}
    in {region; value} }

ne_injection(Kind,element):
  Kind sep_or_term_list(element,";") "end" {
    let ne_elements, terminator = $2 in
    let region = cover $1 $3
    and value = {
      opening  = Kwd $1;
      ne_elements;
      terminator;
      closing = End $3}
    in {region; value}
  }
| Kind "[" sep_or_term_list(element,";") "]" {
    let ne_elements, terminator = $3 in
    let region = cover $1 $4
    and value = {
      opening  = KwdBracket ($1,$2);
      ne_elements;
      terminator;
      closing = RBracket $4}
    in {region; value} }

binding:
  expr "->" expr {
    let start  = expr_to_region $1
    and stop   = expr_to_region $3 in
    let region = cover start stop
    and value  = {
      source = $1;
      arrow  = $2;
      image  = $3}
    in {region; value} }

record_patch:
  "patch" path "with" ne_injection("record",field_assignment) {
    let region = cover $1 $4.region in
    let value  = {
      kwd_patch  = $1;
      path       = $2;
      kwd_with   = $3;
      record_inj = $4}
    in {region; value} }

proc_call:
  fun_call { $1 }

conditional:
  "if" expr "then" if_clause ";"? "else" if_clause {
    let region = cover $1 (if_clause_to_region $7) in
    let value : AST.conditional = {
      kwd_if     = $1;
      test       = $2;
      kwd_then   = $3;
      ifso       = $4;
      terminator = $5;
      kwd_else   = $6;
      ifnot      = $7}
    in {region; value} }

if_clause:
  instruction  { ClauseInstr $1 }
| clause_block { ClauseBlock $1 }

clause_block:
  block { LongBlock $1 }
| "{" sep_or_term_list(statement,";") "}" {
    let statements, terminator = $2 in
    let region = cover $1 $3 in
    let value  = {lbrace = $1;
                  inside = statements, terminator;
                  rbrace = $3} in
    ShortBlock {value; region} }

case_instr:
  case(if_clause) { $1 if_clause_to_region }

case(rhs):
  "case" expr "of" "|"? cases(rhs) "end" {
    fun rhs_to_region ->
      let region = cover $1 $6 in
      let value  = {kwd_case  = $1;
                    expr      = $2;
                    opening   = Kwd $3;
                    lead_vbar = $4;
                    cases     = $5 rhs_to_region;
                    closing   = End $6}
      in {region; value}
  }
| "case" expr "of" "[" "|"? cases(rhs) "]" {
    fun rhs_to_region ->
      let region = cover $1 $7 in
      let value  = {kwd_case  = $1;
                    expr      = $2;
                    opening   = KwdBracket ($3,$4);
                    lead_vbar = $5;
                    cases     = $6 rhs_to_region;
                    closing   = RBracket $7}
      in {region; value} }

cases(rhs):
  nsepseq(case_clause(rhs),"|") {
    fun rhs_to_region ->
      let mk_clause pre_clause = pre_clause rhs_to_region in
      let value  = Utils.nsepseq_map mk_clause $1 in
      let region = nsepseq_to_region (fun x -> x.region) value
      in {region; value} }

case_clause(rhs):
  pattern "->" rhs {
    Scoping.check_pattern $1;
    fun rhs_to_region ->
      let start  = pattern_to_region $1 in
      let region = cover start (rhs_to_region $3)
      and value  = {pattern=$1; arrow=$2; rhs=$3}
      in {region; value} }

assignment:
  lhs ":=" rhs {
    let stop   = rhs_to_region $3 in
    let region = cover (lhs_to_region $1) stop
    and value  = {lhs = $1; assign = $2; rhs = $3}
    in {region; value} }

rhs:
  expr { $1 }

lhs:
  path       {    Path $1 }
| map_lookup { MapPath $1 }

loop:
  while_loop { $1 }
| for_loop   { $1 }

while_loop:
  "while" expr block {
    let region = cover $1 $3.region
    and value  = {kwd_while=$1; cond=$2; block=$3}
    in While {region; value} }

for_loop:
  "for" var_assign "to" expr block {
    let region = cover $1 $5.region in
    let value  = {kwd_for = $1;
                  assign  = $2;
                  kwd_to  = $3;
                  bound   = $4;
                  block   = $5}
    in For (ForInt {region; value})
  }
| "for" var arrow_clause? "in" collection expr block {
    Scoping.check_reserved_name $2;
    let region = cover $1 $7.region in
    let value  = {kwd_for    = $1;
                  var        = $2;
                  bind_to    = $3;
                  kwd_in     = $4;
                  collection = $5;
                  expr       = $6;
                  block      = $7}
    in For (ForCollect {region; value}) }

collection:
  "map"  { Map  $1 }
| "set"  { Set  $1 }
| "list" { List $1 }

var_assign:
  var ":=" expr {
    Scoping.check_reserved_name $1;
    let region = cover $1.region (expr_to_region $3)
    and value  = {name=$1; assign=$2; expr=$3}
    in {region; value} }

arrow_clause:
  "->" var { Scoping.check_reserved_name $2; ($1,$2) }

(* Expressions *)

interactive_expr:
  expr EOF { $1 }

expr:
  case(expr) { ECase ($1 expr_to_region) }
| cond_expr  { $1                        }
| disj_expr  { $1                        }
| fun_expr   { EFun $1                   }

cond_expr:
  "if" expr "then" expr ";"? "else" expr {
    let region = cover $1 (expr_to_region $7) in
    let value : AST.cond_expr = {
      kwd_if     = $1;
      test       = $2;
      kwd_then   = $3;
      ifso       = $4;
      terminator = $5;
      kwd_else   = $6;
      ifnot      = $7}
    in ECond {region; value} }

disj_expr:
  conj_expr { $1 }
| disj_expr "or" conj_expr {
    let start  = expr_to_region $1
    and stop   = expr_to_region $3 in
    let region = cover start stop
    and value  = {arg1=$1; op=$2; arg2=$3} in
    ELogic (BoolExpr (Or {region; value})) }

conj_expr:
  set_membership { $1 }
| conj_expr "and" set_membership {
    let start  = expr_to_region $1
    and stop   = expr_to_region $3 in
    let region = cover start stop
    and value  = {arg1=$1; op=$2; arg2=$3}
    in ELogic (BoolExpr (And {region; value})) }

set_membership:
  comp_expr { $1 }
| core_expr "contains" set_membership {
    let start  = expr_to_region $1
    and stop   = expr_to_region $3 in
    let region = cover start stop in
    let value  = {set=$1; kwd_contains=$2; element=$3}
    in ESet (SetMem {region; value}) }

comp_expr:
  comp_expr "<" cat_expr {
    let start  = expr_to_region $1
    and stop   = expr_to_region $3 in
    let region = cover start stop
    and value  = {arg1 = $1; op = $2; arg2 = $3}
    in ELogic (CompExpr (Lt {region; value}))
  }
| comp_expr "<=" cat_expr {
    let start  = expr_to_region $1
    and stop   = expr_to_region $3 in
    let region = cover start stop
    and value  = {arg1 = $1; op = $2; arg2 = $3}
    in ELogic (CompExpr (Leq {region; value}))
  }
| comp_expr ">" cat_expr {
    let start  = expr_to_region $1
    and stop   = expr_to_region $3 in
    let region = cover start stop
    and value  = {arg1 = $1; op = $2; arg2 = $3}
    in ELogic (CompExpr (Gt {region; value}))
  }
| comp_expr ">=" cat_expr {
    let start  = expr_to_region $1
    and stop   = expr_to_region $3 in
    let region = cover start stop
    and value  = {arg1 = $1; op = $2; arg2 = $3}
    in ELogic (CompExpr (Geq {region; value}))
  }
| comp_expr "=" cat_expr {
    let start  = expr_to_region $1
    and stop   = expr_to_region $3 in
    let region = cover start stop
    and value  = {arg1 = $1; op = $2; arg2 = $3}
    in ELogic (CompExpr (Equal {region; value}))
  }
| comp_expr "=/=" cat_expr {
    let start  = expr_to_region $1
    and stop   = expr_to_region $3 in
    let region = cover start stop
    and value  = {arg1 = $1; op = $2; arg2 = $3}
    in ELogic (CompExpr (Neq {region; value}))
  }
| cat_expr { $1 }

cat_expr:
  cons_expr "^" cat_expr {
    let start  = expr_to_region $1
    and stop   = expr_to_region $3 in
    let region = cover start stop
    and value  = {arg1 = $1; op = $2; arg2 = $3}
    in EString (Cat {region; value})
  }
| cons_expr { $1 }

cons_expr:
  add_expr "#" cons_expr {
    let start  = expr_to_region $1
    and stop   = expr_to_region $3 in
    let region = cover start stop
    and value  = {arg1 = $1; op = $2; arg2 = $3}
    in EList (ECons {region; value})
  }
| add_expr { $1 }

add_expr:
  add_expr "+" mult_expr {
    let start  = expr_to_region $1
    and stop   = expr_to_region $3 in
    let region = cover start stop
    and value  = {arg1 = $1; op = $2; arg2 = $3}
    in EArith (Add {region; value})
  }
| add_expr "-" mult_expr {
    let start  = expr_to_region $1
    and stop   = expr_to_region $3 in
    let region = cover start stop
    and value  = {arg1 = $1; op = $2; arg2 = $3}
    in EArith (Sub {region; value})
  }
| mult_expr { $1 }

mult_expr:
  mult_expr "*" unary_expr {
    let start  = expr_to_region $1
    and stop   = expr_to_region $3 in
    let region = cover start stop
    and value  = {arg1 = $1; op = $2; arg2 = $3}
    in EArith (Mult {region; value})
  }
| mult_expr "/" unary_expr {
    let start  = expr_to_region $1
    and stop   = expr_to_region $3 in
    let region = cover start stop
    and value  = {arg1 = $1; op = $2; arg2 = $3}
    in EArith (Div {region; value})
  }
| mult_expr "mod" unary_expr {
    let start  = expr_to_region $1
    and stop   = expr_to_region $3 in
    let region = cover start stop
    and value  = {arg1 = $1; op = $2; arg2 = $3}
    in EArith (Mod {region; value})
  }
| unary_expr { $1 }

unary_expr:
  "-" core_expr {
    let stop   = expr_to_region $2 in
    let region = cover $1 stop
    and value  = {op = $1; arg = $2}
    in EArith (Neg {region; value})
  }
| "not" core_expr {
    let stop   = expr_to_region $2 in
    let region = cover $1 stop
    and value  = {op = $1; arg = $2} in
    ELogic (BoolExpr (Not {region; value}))
  }
| core_expr { $1 }

core_expr:
  "<int>"                       { EArith (Int $1)              }
| "<nat>"                       { EArith (Nat $1)              }
| "<mutez>"                     { EArith (Mutez $1)            }
| var                           { EVar $1                      }
| "<string>"                    { EString (String $1)          }
| "<bytes>"                     { EBytes $1                    }
| "False"                       { ELogic (BoolExpr (False $1)) }
| "True"                        { ELogic (BoolExpr (True  $1)) }
| "Unit"                        { EUnit $1                     }
| annot_expr                    { EAnnot $1                    }
| tuple_expr                    { ETuple $1                    }
| list_expr                     { EList $1                     }
| "None"                        { EConstr (NoneExpr $1)        }
| fun_call_or_par_or_projection { $1                           }
| map_expr                      { EMap $1                      }
| set_expr                      { ESet $1                      }
| record_expr                   { ERecord $1                   }
| update_record                 { EUpdate $1                   }
| "<constr>" arguments {
    let region = cover $1.region $2.region in
    EConstr (ConstrApp {region; value = $1, Some $2})
  }
| "<constr>" {
    EConstr (ConstrApp {region=$1.region; value = $1,None})
  }
| "Some" arguments {
    let region = cover $1 $2.region in
    EConstr (SomeApp {region; value = $1,$2}) }

fun_call_or_par_or_projection:
  par(expr) arguments? {
    let parenthesized = EPar $1 in
    match $2 with
      None -> parenthesized
    | Some args ->
        let region_1 = $1.region in
        let region = cover region_1 args.region in
        ECall {region; value = parenthesized,args}
  }
| projection arguments? {
    let project = EProj $1 in
    match $2 with
      None -> project
    | Some args ->
        let region_1 = $1.region in
        let region = cover region_1 args.region
        in ECall {region; value = project,args}
  }
| fun_call { ECall $1 }

annot_expr:
  "(" disj_expr ":" type_expr ")" {
    let start  = expr_to_region $2
    and stop   = type_expr_to_region $4 in
    let region = cover start stop
    and value  = $2, $4
    in {region; value} }

set_expr:
  injection("set",expr) { SetInj $1 }

map_expr:
  map_lookup                   { MapLookUp $1 }
| injection("map",binding)     {    MapInj $1 }
| injection("big_map",binding) { BigMapInj $1 }

map_lookup:
  path brackets(expr) {
    let region = cover (path_to_region $1) $2.region in
    let value  = {path=$1; index=$2}
    in {region; value} }

path:
  var        { Name $1 }
| projection { Path $1 }

projection:
  struct_name "." nsepseq(selection,".") {
    let stop   = nsepseq_to_region selection_to_region $3 in
    let region = cover $1.region stop
    and value  = {struct_name = $1;
                  selector    = $2;
                  field_path  = $3}
    in {region; value} }

selection:
  field_name { FieldName $1 }
| "<int>"    { Component $1 }

record_expr:
  "record" sep_or_term_list(field_assignment,";") "end" {
    let ne_elements, terminator = $2 in
    let region = cover $1 $3
    and value : field_assign AST.reg ne_injection = {
      opening = Kwd $1;
      ne_elements;
      terminator;
      closing = End $3}
    in {region; value}
  }
| "record" "[" sep_or_term_list(field_assignment,";") "]" {
   let ne_elements, terminator = $3 in
   let region = cover $1 $4
   and value : field_assign AST.reg ne_injection = {
     opening = KwdBracket ($1,$2);
     ne_elements;
     terminator;
     closing = RBracket $4}
   in {region; value} }

update_record:
  path "with" ne_injection("record",field_assignment){
    let region = cover (path_to_region $1) $3.region in
    let value = {
      record = $1;
      kwd_with = $2;
      updates = $3}
    in {region; value} }


field_assignment:
  field_name "=" expr {
    let region = cover $1.region (expr_to_region $3)
    and value  = {field_name = $1;
                  equal      = $2;
                  field_expr = $3}
    in {region; value} }

fun_call:
  fun_name arguments {
    let region = cover $1.region $2.region
    in {region; value = (EVar $1),$2} }

tuple_expr:
  par(tuple_comp) { $1 }

tuple_comp:
  expr "," nsepseq(expr,",") { Utils.nsepseq_cons $1 $2 $3 }

arguments:
  par(nsepseq(expr,",")) { $1 }

list_expr:
  injection("list",expr) { EListComp $1 }
| "nil"                  {      ENil $1 }

(* Patterns *)

pattern:
  core_pattern { $1 }
| core_pattern "#" nsepseq(core_pattern,"#") {
    let value = Utils.nsepseq_cons $1 $2 $3 in
    let region = nsepseq_to_region pattern_to_region value
    in PList (PCons {region; value}) }

core_pattern:
  var                      {    PVar $1 }
| "_"                      {   PWild $1 }
| "<int>"                  {    PInt $1 }
| "<nat>"                  {    PNat $1 }
| "<bytes>"                {  PBytes $1 }
| "<string>"               { PString $1 }
| list_pattern             {   PList $1 }
| tuple_pattern            {  PTuple $1 }
| constr_pattern           { PConstr $1 }

list_pattern:
  injection("list",core_pattern) { PListComp $1 }
| "nil"                          {      PNil $1 }
| par(cons_pattern)              {  PParCons $1 }

cons_pattern:
  core_pattern "#" pattern { $1,$2,$3 }

tuple_pattern:
  par(nsepseq(core_pattern,",")) { $1 }

constr_pattern:
  "Unit"                   {   PUnit $1 }
| "False"                  {  PFalse $1 }
| "True"                   {   PTrue $1 }
| "None"                   {   PNone $1 }
| "Some" par(core_pattern) {
    let region = cover $1 $2.region
    in PSomeApp {region; value = $1,$2}
  }
| "<constr>" tuple_pattern {
    let region = cover $1.region $2.region in
    PConstrApp {region; value = $1, Some $2}
  }
| "<constr>" {
    PConstrApp {region=$1.region; value=$1,None} }
