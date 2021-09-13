%{
(* START HEADER *)

[@@@warning "-42"]

open Simple_utils.Region
module CST = Cst_cameligo.CST
open CST

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

(* Reductions on error *)

%on_error_reduce seq_expr
%on_error_reduce nsepseq(selection,DOT)
%on_error_reduce call_expr_level
%on_error_reduce add_expr_level
%on_error_reduce cons_expr_level
%on_error_reduce cat_expr_level
%on_error_reduce disj_expr_level
%on_error_reduce conj_expr_level
%on_error_reduce shift_expr_level
%on_error_reduce bin_op(conj_expr_level,BOOL_AND,comp_expr_level)
%on_error_reduce bin_op(disj_expr_level,Or,conj_expr_level)
%on_error_reduce bin_op(disj_expr_level,BOOL_OR,conj_expr_level)
%on_error_reduce base_expr(expr)
%on_error_reduce base_expr(base_cond)
%on_error_reduce base_expr(closed_expr)
%on_error_reduce module_var_e
%on_error_reduce module_var_t
%on_error_reduce nsepseq(module_name,DOT)
%on_error_reduce core_expr
%on_error_reduce match_expr(base_cond)
%on_error_reduce constr_expr
%on_error_reduce nsepseq(disj_expr_level,COMMA)
%on_error_reduce constant_constr_expr
%on_error_reduce constant_constr_pattern
%on_error_reduce arguments
%on_error_reduce bin_op(add_expr_level,MINUS,mult_expr_level)
%on_error_reduce bin_op(add_expr_level,PLUS,mult_expr_level)
%on_error_reduce seq(Attr)
%on_error_reduce constr_pattern
%on_error_reduce cons_pattern_level
%on_error_reduce nsepseq(cons_pattern_level,COMMA)
%on_error_reduce pattern
%on_error_reduce nsepseq(core_irrefutable,COMMA)
%on_error_reduce variant(fun_type_level)
%on_error_reduce variant(prod_type_level)
%on_error_reduce nsepseq(variant(fun_type_level),VBAR)
%on_error_reduce nsepseq(variant(prod_type_level),VBAR)
%on_error_reduce nsepseq(core_type,TIMES)
%on_error_reduce fun_type_level
%on_error_reduce prod_type_level

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

(* Sequences

   Series of instances of the same syntactical category have often to
   be parsed, like lists of expressions, patterns etc. The simplest of
   all is the possibly empty sequence (series), parsed below by
   [seq]. The non-empty sequence is parsed by [nseq]. Note that the
   latter returns a pair made of the first parsed item (the parameter
   [X]) and the rest of the sequence (possibly empty). This way, the
   OCaml typechecker can keep track of this information along the
   static control-flow graph. See module [Utils] for the types
   corresponding to the semantic actions of those rules.
 *)

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

%inline variable    : "<ident>"  { $1 }
%inline type_name   : "<ident>"  { $1 }
%inline field_name  : "<ident>"  { $1 }
%inline struct_name : "<ident>"  { $1 }
%inline module_name : "<uident>" { $1 }

(* Non-empty comma-separated values (at least two values) *)

tuple(item):
  item "," nsepseq(item,",") { let h,t = $3 in $1, ($2,h)::t }

(* Possibly empty semicolon-separated values between brackets *)

list_of(item):
  "[" sep_or_term_list(item,";")? "]" {
    let compound = Some (Brackets ($1,$3))
    and region = cover $1 $3 in
    let elements, terminator =
      match $2 with
        None -> None, None
      | Some (elements, terminator) ->
          Some elements, terminator in
    let value = {compound; elements; terminator}
    in {region; value} }

(* Main *)

contract:
  module_ EOF { {$1 with eof=$2} }

module_:
  nseq(declaration) { {decl=$1; eof=Region.ghost} }

declaration:
  type_decl       {    TypeDecl $1 }
| let_declaration {         Let $1 }
| module_decl     {  ModuleDecl $1 }
| module_alias    { ModuleAlias $1 }
| "<directive>"   {   Directive $1 }

(* Type declarations *)

type_decl:
  "type" ioption(quoted_type_params) type_name "=" type_expr {
    let region = cover $1 (type_expr_to_region $5) in
    let value  = {kwd_type  = $1;
                  params    = $2;
                  name      = $3;
                  eq        = $4;
                  type_expr = $5}
    in {region; value} }

quoted_type_params:
  type_var             { QParam      $1 }
| par(tuple(type_var)) { QParamTuple $1 }

type_var:
  "'" variable {
    let region = cover $1 $2.region
    and value = {quote=$1; name=$2}
    in {region; value} }

module_decl:
  "module" module_name "=" "struct" module_ "end" {
    let region = cover $1 $6 in
    let value  = {kwd_module  = $1;
                  name        = $2;
                  eq          = $3;
                  kwd_struct  = $4;
                  module_     = $5;
                  kwd_end     = $6}
    in {region; value} }

module_alias:
  "module" module_name "=" nsepseq(module_name,".") {
    let stop   = nsepseq_to_region (fun x -> x.region) $4 in
    let region = cover $1 stop in
    let value  = {kwd_module  = $1;
                  alias       = $2;
                  eq          = $3;
                  binders     = $4}
    in {region; value} }

type_expr:
  fun_type_level | sum_type(fun_type_level) { $1 }

fun_type_level:
  prod_type_level "->" fun_type_level {
    let start  = type_expr_to_region $1
    and stop   = type_expr_to_region $3 in
    let region = cover start stop in
    TFun {region; value=$1,$2,$3} }
| prod_type_level { $1 }

prod_type_level:
  core_type "*" nsepseq(core_type,"*") {
    let value  = Utils.nsepseq_cons $1 $2 $3 in
    let region = nsepseq_to_region type_expr_to_region value
    in TProd {region; value} }
| core_type { $1 }

core_type:
  "<string>"       { TString $1 }
| "<int>"          {    TInt $1 }
| "_"              { TVar {value="_"; region=$1} }
| type_name        {    TVar $1 }
| module_access_t  {   TModA $1 }
| type_constr_app  {    TApp $1 }
| record_type      { TRecord $1 }
| type_var         {    TArg $1 }
| par(type_expr)   {    TPar $1 }

type_constr_app:
  type_constr_arg type_name {
    let start  = type_constr_arg_to_region $1
    and stop   = $2.region in
    let region = cover start stop
    and value  = $2, $1
    in {region; value} }

type_constr_arg:
  core_type             { CArg      $1 }
| par(tuple(type_expr)) { CArgTuple $1 }

(* Sum types. We parameterise the variants by the kind of type
   expression that may occur at the rightmost side of a sentence. This
   enables to use [sum_type] in contexts that allow different types to
   avoid LR conflicts. For example, if the return type of a lambda is
   a functional type, parentheses are mandatory. *)

sum_type(right_type_expr):
  nsepseq(variant(right_type_expr),"|") {
    let region = nsepseq_to_region (fun x -> x.region) $1 in
    let value  = {variants=$1; attributes=[]; lead_vbar=None}
    in TSum {region; value}
  }
| attributes "|" nsepseq(variant(right_type_expr),"|") {
    let region = nsepseq_to_region (fun x -> x.region) $3 in
    let value  = {variants=$3; attributes=$1; lead_vbar = Some $2}
    in TSum {region; value} }

(* Always use [ioption] at the end of a rule *)

variant(right_type_expr):
  attributes "<uident>" ioption(of_type(right_type_expr)) {
    let stop   = match $3 with
                   None -> $2.region
                | Some (_, t) -> type_expr_to_region t in
    let region = cover $2.region stop
    and value  = {constr=$2; arg=$3; attributes=$1}
    in {region; value} }

of_type(right_type_expr):
  "of" right_type_expr { $1, $2 }

record_type:
  attributes "{" sep_or_term_list(field_decl,";") "}" {
    let fields, terminator = $3 in
    let region = cover $2 $4
    and value = {
      compound = Some (Braces ($2,$4));
      ne_elements = fields;
      terminator;
      attributes=$1}
    in {region; value} }

module_access_t :
  module_name "." module_var_t {
    let start       = $1.region in
    let stop        = type_expr_to_region $3 in
    let region      = cover start stop in
    let value       = {module_name=$1; selector=$2; field=$3}
    in {region; value} }

module_var_t:
  module_access_t  { TModA $1 }
| field_name       { TVar  $1 }

field_decl:
  attributes field_name ":" type_expr {
    let stop   = type_expr_to_region $4 in
    let region = cover $2.region stop
    and value  = {field_name=$2; colon=$3; field_type=$4; attributes=$1}
    in {region; value} }

(* Top-level definitions

   Note how we did not write:

   let_declaration:
     seq("[@attr]") "let" ioption("rec") let_binding { ... }

   because this leads to an error state with an LR item of the form

   let_declaration -> seq("[@attr]") . "let" ioption("rec") let_binding [ ... ]

   with a spurious reduction on [seq("[@attr]")]. As a consequence the
   message about the past becomes awkward, along the lines of "if the
   attributes (if any) are complete". To avoid that phrasing, we use
   the following trick: use [ioption] and [nseq] instead of [seq] in
   the rule [attributes]. *)

let_declaration:
  attributes "let" ioption("rec") let_binding {
    let attributes = $1
    and kwd_let    = $2
    and kwd_rec    = $3
    and binding    = $4 in
    let value      = kwd_let, kwd_rec, binding, attributes in
    let stop       = expr_to_region binding.let_rhs in
    let region     = cover $2 stop
    in {region; value} }

%inline attributes:
  ioption(nseq("[@attr]") { Utils.nseq_to_list $1 }) { list_of_option $1 }

let_binding:
  var_pattern type_parameters parameters let_lhs_type "=" expr {
    let binders = Utils.nseq_cons (PVar $1) $3 in
    {binders; type_params=$2; lhs_type=$4; eq=$5; let_rhs=$6}
  }
| irrefutable type_parameters let_lhs_type "=" expr {
    {binders=$1,[]; type_params=$2; lhs_type=$3; eq=$4; let_rhs=$5} }

%inline let_lhs_type:
  ioption(type_annotation(type_expr)) { $1 }

(* The %inline solves a shift/reduce conflict: *)

%inline type_parameters:
  ioption(par(type_param_list)) { $1 }

type_param_list:
  "type" nseq(variable) { {kwd_type=$1; type_vars=$2} }

parameters:
  nseq(core_irrefutable) { $1 }

type_annotation(right_type_expr):
  ":" right_type_expr { $1,$2 }

(* PATTERNS *)

(* Irrefutable Patterns *)

%inline
irrefutable:
  tuple(core_irrefutable) {
    let region = nsepseq_to_region pattern_to_region $1
    in PTuple {region; value=$1}
  }
| core_irrefutable { $1 }

%inline
core_irrefutable:
  "_"                         { PVar    (mk_wild $1) }
| var_pattern                 { PVar    $1 }
| unit                        { PUnit   $1 }
| record_pattern(irrefutable) { PRecord $1 }
| par(typed_irrefutable)
| par(irrefutable)            { PPar    $1 }
| constr_irrefutable          { $1 }

var_pattern:
  attributes "<ident>" {
    let value = {variable=$2; attributes=$1}
    in {$2 with value} }

typed_irrefutable:
  irrefutable type_annotation(type_expr) {
    let colon, type_expr = $2 in
    let start  = pattern_to_region $1 in
    let stop   = type_expr_to_region type_expr in
    let region = cover start stop in
    let value  = {pattern=$1; colon; type_expr}
    in PTyped {region; value} }

constr_irrefutable:
  par(non_const_constr_irrefutable) {    PPar $1 }
| constant_constr_pattern           { PConstr $1 }

constant_constr_pattern:
  "<uident>" { {$1 with value = $1,None} }

non_const_constr_irrefutable:
  "<uident>" core_irrefutable {
    let stop   = pattern_to_region $2 in
    let region = cover $1.region stop
    and value  = $1, Some $2 in
    PConstr {region; value} }

(* General Patterns *)

pattern:
  tuple(cons_pattern_level) {
    let region = nsepseq_to_region pattern_to_region $1
    in PTuple {region; value=$1} }
| cons_pattern_level { $1 }

cons_pattern_level:
  core_pattern "::" cons_pattern_level {
    let start  = pattern_to_region $1 in
    let stop   = pattern_to_region $3 in
    let region = cover start stop in
    PList (PCons {region; value=$1,$2,$3}) }
| core_pattern { $1 }

core_pattern:
  var_pattern                     { PVar $1 }
| "_"                             { PVar (mk_wild $1) }
| "<int>"                         {                           PInt $1 }
| "<nat>"                         {                           PNat $1 }
| "<bytes>"                       {                         PBytes $1 }
| "<string>"                      {                        PString $1 }
| "<verbatim>"                    {                      PVerbatim $1 }
| unit                            {                          PUnit $1 }
| list_of(cons_pattern_level)     {              PList (PListComp $1) }
| constr_pattern                  {                        PConstr $1 }
| record_pattern(core_pattern)    {                        PRecord $1 }
| par(pattern)
| par(typed_pattern)              { PPar $1}

typed_pattern:
  pattern type_annotation(type_expr) {
    let colon, type_expr = $2 in
    let start  = pattern_to_region $1 in
    let stop   = type_expr_to_region type_expr in
    let region = cover start stop in
    let value  = {pattern=$1; colon; type_expr}
    in PTyped {region; value} }

record_pattern(rhs_pattern):
  "{" sep_or_term_list(field_pattern(rhs_pattern),";") "}" {
    let ne_elements, terminator = $2 in
    let region = cover $1 $3 in
    let value  = {
      compound = Some (Braces ($1,$3));
      ne_elements;
      terminator;
      attributes=[]}
    in {region; value} }

field_pattern(rhs_pattern):
  field_name {
    let region  = $1.region in
    let pattern = PVar {region; value = {variable=$1; attributes=[]}} in
    let value   = {field_name=$1; eq=Region.ghost; pattern}
    in {region; value}
  }
| field_name "=" rhs_pattern {
    let start  = $1.region
    and stop   = pattern_to_region $3 in
    let region = cover start stop
    and value  = {field_name=$1; eq=$2; pattern=$3}
    in {region; value} }

constr_pattern:
  "<uident>" ioption(core_pattern) {
    let region =
      match $2 with
        None -> $1.region
      | Some stop -> cover $1.region (pattern_to_region stop)
    in {region; value = ($1,$2)} }

unit:
  "(" ")" { {region = cover $1 $2; value = $1,$2} }

(* Expressions *)

interactive_expr:
  expr EOF { $1 }

expr:
  base_cond__open(expr) | match_expr(base_cond) { $1 }

base_cond__open(x):
  base_expr(x) | conditional(x) { $1 }

base_cond:
  base_cond__open(base_cond) { $1 }

base_expr(right_expr):
  tuple_expr
| let_in_expr(right_expr)
| local_type_decl(right_expr)
| local_module_decl(right_expr)
| local_module_alias(right_expr)
| fun_expr(right_expr)
| disj_expr_level { $1 }

tuple_expr:
  tuple(disj_expr_level) {
    let region = nsepseq_to_region expr_to_region $1
    in ETuple {region; value=$1} }

conditional(right_expr):
  if_then_else(right_expr) | if_then(right_expr) { $1 }

if_then_else(right_expr):
  "if" expr "then" closed_expr "else" right_expr {
    let region = cover $1 (expr_to_region $6)
    and value  = {kwd_if   = $1;
                  test     = $2;
                  kwd_then = $3;
                  ifso     = $4;
                  ifnot    = Some($5,$6)}
    in ECond {region; value} }

if_then(right_expr):
  "if" expr "then" right_expr {
    let stop     = expr_to_region $4 in
    let region   = cover $1 stop in
    let value    = {kwd_if   = $1;
                    test     = $2;
                    kwd_then = $3;
                    ifso     = $4;
                    ifnot    = None}
    in ECond {region; value} }

base_if_then_else__open(x):
  base_expr(x) | if_then_else(x) { $1 }

base_if_then_else:
  base_if_then_else__open(base_if_then_else) { $1 }

closed_expr:
  base_if_then_else__open(closed_expr)
| match_expr(base_if_then_else) { $1 }

match_expr(right_expr):
  "match" expr "with" "|"? cases(right_expr) {
    let cases = {
      value  = Utils.nsepseq_rev $5;
      region = nsepseq_to_region (fun x -> x.region) $5}
    and stop =
      match $5 with
        {region; _}, [] -> region
      |           _, tl -> last fst tl in
    let region = cover $1 stop
    and value  = {kwd_match = $1;
                  expr      = $2;
                  kwd_with  = $3;
                  lead_vbar = $4;
                  cases}
    in ECase {region; value} }

cases(right_expr):
  case_clause(right_expr) {
    let start  = pattern_to_region $1.pattern
    and stop   = expr_to_region $1.rhs in
    let region = cover start stop
    in {region; value=$1}, []
  }
| cases(base_cond) "|" case_clause(right_expr) {
    let start =
      match $1 with
         only_case, [] -> only_case.region
      | _, other_cases -> last fst other_cases
    and stop             = expr_to_region $3.rhs in
    let region           = cover start stop in
    let fst_case         = {region; value=$3}
    and snd_case, others = $1
    in fst_case, ($2,snd_case)::others }

case_clause(right_expr):
  pattern "->" right_expr {
    {pattern=$1; arrow=$2; rhs=$3} }

let_in_expr(right_expr):
   attributes "let" ioption("rec") let_binding "in" right_expr  {
    let stop   = expr_to_region $6 in
    let region = cover $2 stop
    and value  = {attributes = $1;
                  kwd_let    = $2;
                  kwd_rec    = $3;
                  binding    = $4;
                  kwd_in     = $5;
                  body       = $6}
    in ELetIn {region; value} }

local_type_decl(right_expr):
  type_decl "in" right_expr  {
    let stop   = expr_to_region $3 in
    let region = cover $1.region stop
    and value  = {type_decl  = $1.value;
                  kwd_in     = $2;
                  body       = $3}
    in ETypeIn {region; value} }

local_module_decl(right_expr):
  module_decl "in" right_expr  {
    let stop   = expr_to_region $3 in
    let region = cover $1.region stop
    and value  = {mod_decl  = $1.value;
                  kwd_in    = $2;
                  body      = $3}
    in EModIn {region; value} }

local_module_alias(right_expr):
  module_alias "in" right_expr  {
    let stop   = expr_to_region $3 in
    let region = cover $1.region stop
    and value  = {mod_alias = $1.value;
                  kwd_in    = $2;
                  body      = $3}
    in EModAlias {region; value} }

fun_expr(right_expr):
  attributes "fun" type_parameters nseq(core_irrefutable) ret_type "->" right_expr {
    let stop   = expr_to_region $7 in
    let region = cover $2 stop in
    let value  = {kwd_fun=$2; type_params=$3; binders=$4;
                  lhs_type=$5; arrow=$6; body=$7; attributes=$1}
    in EFun {region; value} }

%inline ret_type:
  ioption(type_annotation(lambda_app_type)) { $1 }

lambda_app_type:
  prod_type_level | sum_type(prod_type_level) { $1 }

disj_expr_level:
  bin_op(disj_expr_level, "||", conj_expr_level)
| bin_op(disj_expr_level, "or", conj_expr_level) {
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
  bin_op(conj_expr_level, "&&", comp_expr_level) {
    ELogic (BoolExpr (And $1)) }
| comp_expr_level { $1 }

comp_expr_level:
  bin_op(comp_expr_level, "<", cat_expr_level) {
    ELogic (CompExpr (Lt $1)) }
| bin_op(comp_expr_level, "<=", cat_expr_level) {
    ELogic (CompExpr (Leq $1)) }
| bin_op(comp_expr_level, ">", cat_expr_level) {
    ELogic (CompExpr (Gt $1)) }
| bin_op(comp_expr_level, ">=", cat_expr_level) {
    ELogic (CompExpr (Geq $1)) }
| bin_op(comp_expr_level, "=", cat_expr_level) {
    ELogic (CompExpr (Equal $1)) }
| bin_op(comp_expr_level, "<>", cat_expr_level) {
    ELogic (CompExpr (Neq $1)) }
| cat_expr_level { $1 }

cat_expr_level:
  bin_op(cons_expr_level, "^", cat_expr_level)     { EString (Cat $1) }
| cons_expr_level                                  {               $1 }

cons_expr_level:
  bin_op(add_expr_level, "::", cons_expr_level)    { EList (ECons $1) }
| add_expr_level                                   {               $1 }

add_expr_level:
  bin_op(add_expr_level, "+", mult_expr_level)     {  EArith (Add $1) }
| bin_op(add_expr_level, "-", mult_expr_level)     {  EArith (Sub $1) }
| mult_expr_level                                  {               $1 }

mult_expr_level:
  bin_op(mult_expr_level, "*", shift_expr_level)    {  EArith (Mult $1) }
| bin_op(mult_expr_level, "/", shift_expr_level)    {   EArith (Div $1) }
| bin_op(mult_expr_level, "mod", shift_expr_level)  {   EArith (Mod $1) }
| bin_op(mult_expr_level, "land", shift_expr_level) {  EArith (Land $1) }
| bin_op(mult_expr_level, "lor", shift_expr_level)  {   EArith (Lor $1) }
| bin_op(mult_expr_level, "lxor", shift_expr_level) {  EArith (Lxor $1) }
| shift_expr_level                                  {                $1 }

shift_expr_level:
  bin_op(unary_expr_level, "lsl", shift_expr_level) { EArith (Lsl $1) }
| bin_op(unary_expr_level, "lsr", shift_expr_level) { EArith (Lsr $1) }
| unary_expr_level                                  { $1 }

unary_expr_level:
  "-" call_expr_level {
    let start = $1 in
    let stop = expr_to_region $2 in
    let region = cover start stop
    and value  = {op=$1; arg=$2}
    in EArith (Neg {region; value})
  }
| "not" call_expr_level {
    let start = $1 in
    let stop = expr_to_region $2 in
    let region = cover start stop
    and value  = {op=$1; arg=$2} in
    ELogic (BoolExpr (Not {region; value}))
  }
| call_expr_level { $1 }

call_expr_level:
  call_expr
| core_expr
| constr_expr { $1 }

constr_expr:
  "<uident>" argument {
    let region = cover $1.region (expr_to_region $2)
    in EConstr {region; value = ($1, Some $2)}
  }
| constant_constr_expr { $1 }

constant_constr_expr:
  "<uident>" { EConstr {$1 with value=($1,None)} }

arguments:
  argument           { $1,[]                      }
| argument arguments { let h,t = $2 in ($1, h::t) }

argument:
  core_expr
| constant_constr_expr { $1 }

call_expr:
  core_expr arguments {
    let start  = expr_to_region $1 in
    let stop   = match $2 with
                   e, [] -> expr_to_region e
                 | _,  l -> last expr_to_region l in
    let region = cover start stop in
    ECall {region; value=$1,$2} }

core_expr:
  "<int>"                             {               EArith (Int $1) }
| "<mutez>"                           {             EArith (Mutez $1) }
| "<nat>"                             {               EArith (Nat $1) }
| "<bytes>"                           {                     EBytes $1 }
| "<ident>"                           {                       EVar $1 }
| projection                          {                      EProj $1 }
| module_access_e                     {                      EModA $1 }
| "<string>"                          {           EString (String $1) }
| "<verbatim>"                        {         EString (Verbatim $1) }
| unit                                {                      EUnit $1 }
| list_of(expr)                       {          EList (EListComp $1) }
| sequence                            {                       ESeq $1 }
| record_expr                         {                    ERecord $1 }
| update_record                       {                    EUpdate $1 }
| code_inj                            {                   ECodeInj $1 }
| par(expr)                           {                       EPar $1 }
| par(typed_expr)                     {                     EAnnot $1 }

code_inj:
  "[%lang" expr "]" {
    let region = cover $1.region $3
    and value  = {language=$1; code=$2; rbracket=$3}
    in {region; value} }

typed_expr:
  expr ":" type_expr { $1,$2,$3 }

projection:
  struct_name "." nsepseq(selection,".") {
    let start  = $1.region in
    let stop   = nsepseq_to_region selection_to_region $3 in
    let region = cover start stop in
    let value  = {struct_name=$1; selector=$2; field_path=$3}
    in {region; value}
  }

module_access_e:
  module_name "." module_var_e {
    let start       = $1.region in
    let stop        = expr_to_region $3 in
    let region      = cover start stop in
    let value       = {module_name=$1; selector=$2; field=$3}
    in {region; value} }

module_var_e:
  module_access_e   { EModA $1 }
| "or"              { EVar {value="or"; region=$1} }
| field_name        { EVar  $1 }
| projection        { EProj $1 }

selection:
  field_name { FieldName $1 }
| "<int>"    { Component $1 }

record_expr:
  "{" sep_or_term_list(field_assignment,";") "}" {
    let ne_elements, terminator = $2 in
    let region = cover $1 $3 in
    let value  = {compound = Some (Braces ($1,$3));
                  ne_elements;
                  terminator;
                  attributes=[]}
    in {region; value} }

update_record:
  "{" path "with" sep_or_term_list(field_path_assignment,";") "}" {
    let region = cover $1 $5 in
    let ne_elements, terminator = $4 in
    let value = {
      lbrace   = $1;
      record   = $2;
      kwd_with = $3;
      updates  = {value = {compound = None;
                           ne_elements;
                           terminator;
                           attributes=[]};
                  region = cover $3 $5};
      rbrace   = $5}
    in {region; value} }

field_path_assignment:
  path "=" expr {
    let region = cover (path_to_region $1) (expr_to_region $3)
    and value  = {field_path=$1; assignment=$2; field_expr=$3}
    in {region; value} }

field_assignment:
  field_name "=" expr {
    let region = cover $1.region (expr_to_region $3)
    and value  = {field_name=$1; assignment=$2; field_expr=$3}
    in {region; value} }

path:
 "<ident>"   { Name $1 }
| projection { Path $1 }

(* Sequences *)

sequence:
  "begin" ioption(series) "end" {
    let region   = cover $1 $3
    and compound = Some (BeginEnd ($1,$3)) in
    let elements = $2 in
    let value    = {compound; elements; terminator=None}
    in {region; value} }

series:
  seq_expr ";" series { Utils.nsepseq_cons $1 $2 $3 }
| last_expr           { $1,[] }

last_expr:
  seq_expr
| fun_expr(last_expr)
| match_expr(last_expr)
| let_in_sequence
| tuple_expr { $1 }

let_in_sequence:
  attributes "let" ioption("rec") let_binding "in" series  {
    let seq      = $6 in
    let stop     = nsepseq_to_region expr_to_region seq in
    let region   = cover $2 stop in
    let compound = None in
    let elements = Some seq in
    let value    = {compound; elements; terminator=None} in
    let body     = ESeq {region; value} in
    let value    = {attributes = $1;
                    kwd_let    = $2;
                    kwd_rec    = $3;
                    binding    = $4;
                    kwd_in     = $5;
                    body}
    in ELetIn {region; value} }

seq_expr:
  disj_expr_level | if_then_else (seq_expr) { $1 }
