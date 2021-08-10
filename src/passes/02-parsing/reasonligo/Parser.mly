%{
(* START HEADER *)

[@@@warning "-42"]

open Simple_utils.Region
module CST = Cst.Reasonligo
open CST

(* Utilities *)


let first_region = function
  [] -> None
| x::_ -> Some x.Region.region

(* END HEADER *)
%}

(* Reductions on error *)

%on_error_reduce selection
%on_error_reduce call_expr_level
%on_error_reduce add_expr_level
%on_error_reduce cat_expr_level
%on_error_reduce disj_expr_level
%on_error_reduce conj_expr_level
%on_error_reduce bin_op(conj_expr_level,BOOL_AND,comp_expr_level)
%on_error_reduce bin_op(disj_expr_level,Or,conj_expr_level)
%on_error_reduce bin_op(disj_expr_level,BOOL_OR,conj_expr_level)
%on_error_reduce base_expr(expr)
%on_error_reduce base_expr(base_cond)
%on_error_reduce core_expr
%on_error_reduce constr_expr
%on_error_reduce bin_op(add_expr_level,MINUS,mult_expr_level)
%on_error_reduce bin_op(add_expr_level,PLUS,mult_expr_level)
%on_error_reduce seq(Attr)
%on_error_reduce constr_pattern
%on_error_reduce if_then(expr)
%on_error_reduce irrefutable
%on_error_reduce variant
%on_error_reduce nsepseq(variant,VBAR)
%on_error_reduce nsepseq(sub_irrefutable,COMMA)
%on_error_reduce nsepseq(fun_arg,COMMA)
%on_error_reduce fun_type
%on_error_reduce core_type
%on_error_reduce module_var_e
%on_error_reduce module_var_t
%on_error_reduce nsepseq(module_name,DOT)

(* See [ParToken.mly] for the definition of tokens. *)

(* Entry points *)

%start contract interactive_expr
%type <Cst.Reasonligo.t> contract
%type <Cst.Reasonligo.expr> interactive_expr

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
   static control-flow graph. The rule [sepseq] parses possibly empty
   sequences of items separated by some token (e.g., a comma), and
   rule [nsepseq] is for non-empty such sequences. See module [Utils]
   for the types corresponding to the semantic actions of those
   rules.
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

%inline type_name        : "<ident>"  { $1 }
%inline field_name       : "<ident>"  { $1 }
%inline struct_name      : "<ident>"  { $1 }
%inline module_name      : "<constr>" { $1 }

(* Non-empty comma-separated values (at least two values) *)

tuple(item):
  item "," nsepseq(item,",") { let h,t = $3 in $1, ($2,h)::t }

(* Possibly empty semicolon-separated values between brackets *)

list__(item):
  "[" sep_or_term_list(item,",")? "]" {
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
  nseq(declaration) EOF { {decl=$1; eof=$2} }

declaration:
  type_decl ";"?        { TypeDecl    $1 }
| let_declaration ";"?  { ConstDecl   $1 }
| module_decl ";"?      { ModuleDecl  $1 }
| module_alias ";"?     { ModuleAlias $1 }
| "<directive>"         { Directive   $1 }

module_:
  nseq(declaration) { {decl=$1; eof=Region.ghost} }

(* Type declarations *)

type_decl:
  "type" type_name "=" type_expr {
    let region = cover $1 (type_expr_to_region $4) in
    let value  = {kwd_type  = $1;
                  name      = $2;
                  eq        = $3;
                  type_expr = $4}
    in {region; value} }

module_decl:
  "module" module_name "=" "{" module_ "}" {
    let region = cover $1 $6 in
    let value  = {kwd_module = $1;
                  name       = $2;
                  eq         = $3;
                  lbrace     = $4;
                  module_    = $5;
                  rbrace     = $6}
    in {region; value} }

module_alias:
  "module" module_name "=" nsepseq (module_name,".") {
    let stop   = nsepseq_to_region (fun x -> x.region) $4 in
    let region = cover $1 stop in
    let value  = {kwd_module = $1;
                  alias      = $2;
                  eq         = $3;
                  binders    = $4}
    in {region; value} }

type_expr:
  fun_type | sum_type | record_type { $1 }

fun_type:
  cartesian { $1 }
| ES6FUN cartesian "=>" fun_type {
    let start  = type_expr_to_region $2
    and stop   = type_expr_to_region $4 in
    let region = cover start stop in
    TFun {region; value=$2,$3,$4} }

cartesian:
  core_type { $1 }
| par(tuple (cartesian)) { TProd $1 }

type_args:
  tuple(fun_type) { $1 }
| fun_type        { $1, [] }

core_type:
  type_name       {    TVar $1 }
| "_"             {   TWild $1 }
| par(type_expr)  {    TPar $1 }
| "<string>"      { TString $1 }
| "<int>"         { TInt    $1 }
| module_access_t {   TModA $1 }
| type_name par(type_args) {
   let region = cover $1.region $2.region
   in TApp {region; value = $1,$2} }

sum_type:
  nsepseq(variant,"|") {
    let region = nsepseq_to_region (fun x -> x.region) $1 in
    let value  = {variants=$1; attributes=[]; lead_vbar=None}
    in TSum {region; value}
  }
| seq("[@attr]") "|" nsepseq(variant,"|") {
    let region = nsepseq_to_region (fun x -> x.region) $3 in
    let value  = {variants=$3; attributes=$1; lead_vbar = Some $2}
    in TSum {region; value} }

variant:
  nseq("[@attr]") "<constr>" {
    let attr   = Utils.nseq_to_list $1 in
    let region = cover (fst $1).region $2.region
    and value  = {constr=$2; arg=None; attributes=attr}
    in {region; value}
  }
| "<constr>" {
    {$1 with value = {constr=$1; arg=None; attributes=[]}}
  }
| "<constr>" "(" fun_type ")" {
    let region = cover $1.region $4
    and value  = {constr=$1; arg = Some (ghost,$3); attributes=[]}
    in {region; value}
  }
| nseq("[@attr]") "<constr>" "(" fun_type ")" {
    let attr   = Utils.nseq_to_list $1 in
    let region = cover (fst $1).region $5
    and value  = {constr=$2; arg = Some (ghost,$4); attributes=attr}
    in {region; value} }

record_type:
  seq("[@attr]") "{" sep_or_term_list(field_decl,",") "}" {
    let fields, terminator = $3 in
    let region =
      match first_region $1 with
        None -> cover $2 $4
      | Some start -> cover start $4
    and value = {
      compound = Some (Braces ($2,$4));
      ne_elements = fields;
      terminator;
      attributes=$1}
    in TRecord {region; value} }

module_access_t :
  module_name "." module_var_t {
    let start       = $1.region in
    let stop        = type_expr_to_region $3 in
    let region      = cover start stop in
    let value       = {module_name=$1; selector=$2; field=$3}
    in {region; value} }

module_var_t:
  module_access_t   { TModA $1 }
| field_name        { TVar  $1 }

field_decl:
  seq("[@attr]") field_name {
    let value = {
      field_name=$2;
      colon=ghost;
      field_type = TVar $2;
      attributes=$1}
    in {$2 with value}
  }
| seq("[@attr]") field_name ":" type_expr {
    let stop   = type_expr_to_region $4 in
    let region = match first_region $1 with
                   None -> cover $2.region stop
                 | Some start -> cover start stop
    and value  = {field_name=$2; colon=$3; field_type=$4; attributes=$1}
    in {region; value} }

(* Top-level definitions *)

let_declaration:
  seq("[@attr]") "let" ioption("rec") let_binding {
    let attributes = $1 in
    let kwd_let    = $2 in
    let kwd_rec    = $3 in
    let binding    = $4 in
    let value      = kwd_let, kwd_rec, binding, attributes in
    let stop       = expr_to_region binding.let_rhs in
    let region     = match first_region $1 with
                       None -> cover $2 stop
                     | Some start -> cover start stop
    in {region; value} }

let_binding:
  irrefutable type_annotation? "=" expr {
    {binders = $1; lhs_type=$2; eq=$3; let_rhs=$4}
  }

type_annotation:
  ":" type_expr { $1,$2 }

(* Patterns *)

irrefutable:
  sub_irrefutable        { $1 }
| tuple(sub_irrefutable) {
    let region = nsepseq_to_region pattern_to_region $1
    in PTuple {region; value=$1} }

sub_irrefutable:
  var_pattern                   {                                     PVar $1 }
| "_"                           { PVar { var = { value = "_"; region = $1 } ;
                                         attributes = [] }                    }
| unit                          {                                    PUnit $1 }
| record_pattern                {                                  PRecord $1 }
| par(closed_irrefutable)       {                                     PPar $1 }

closed_irrefutable:
  irrefutable                                            {         $1 }
| constr_pattern                                         { PConstr $1 }
| typed_pattern                                          {  PTyped $1 }

var_pattern:
  "<ident>" seq("[@attr]")        {  {var=$1; attributes=$2} }

typed_pattern:
  irrefutable ":" type_expr  {
    let start  = pattern_to_region $1 in
    let stop   = type_expr_to_region $3 in
    let region = cover start stop in
    let value  = {pattern=$1; colon=$2; type_expr=$3}
    in {region; value} }

pattern:
  core_pattern { $1 }
| "[" sub_pattern "," "..." sub_pattern "]" {
    let start  = pattern_to_region $2 in
    let stop   = pattern_to_region $5 in
    let region = cover start stop in
    let value  =
      { lbracket = $1;
        lpattern = $2;
        comma    = $3;
        ellipsis = $4;
        rpattern = $5;
        rbracket = $6}
    in PList (PCons {value;region})
  }
| tuple(sub_pattern) {
    let region = nsepseq_to_region pattern_to_region $1
    in PTuple {region; value=$1} }

sub_pattern:
  par(sub_pattern) { PPar $1 }
| core_pattern     {      $1 }

core_pattern:
  var_pattern                     {                                     PVar $1 }
| "_"                             { PVar { var = { value = "_"; region = $1 } ;
                                             attributes = [] }                  }
| "<int>"                         {                                     PInt $1 }
| "<nat>"                         {                                     PNat $1 }
| "<bytes>"                       {                                   PBytes $1 }
| "<string>"                      {                                  PString $1 }
| "<verbatim>"                    {                                PVerbatim $1 }
| unit                            {                                    PUnit $1 }
| par(ptuple)                     {                                     PPar $1 }
| list__(sub_pattern)             {                        PList (PListComp $1) }
| constr_pattern                  {                                  PConstr $1 }
| record_pattern                  {                                  PRecord $1 }

record_pattern:
  "{" sep_or_term_list(field_pattern,",") "}" {
    let ne_elements, terminator = $2 in
    let region = cover $1 $3 in
    let value  = {
      compound = Some (Braces ($1,$3));
      ne_elements;
      terminator;
      attributes=[]}
    in {region; value} }

field_pattern:
  field_name {
    let region  = $1.region
    and value  = {field_name=$1; eq=Region.ghost; pattern=PVar { var = $1 ;
							         attributes = [] } }
    in {region; value} 
  }
| field_name ":" sub_pattern {
    let start  = $1.region
    and stop   = pattern_to_region $3 in
    let region = cover start stop
    and value  = {field_name=$1; eq=$2; pattern=$3}
    in {region; value} }

constr_pattern:
  "None" { PNone $1 }
| "Some" sub_pattern {
    let stop   = pattern_to_region $2 in
    let region = cover $1 stop
    and value  = $1,$2
    in PSomeApp {region; value}
  }
| "false" { PFalse $1 }
| "true"  {  PTrue $1 }
| "<constr>" {
    PConstrApp {$1 with value=$1,None}
  }
| "<constr>" sub_pattern {
    let region = cover $1.region (pattern_to_region $2)
    in PConstrApp {region; value = $1, Some $2} }

ptuple:
  tuple(sub_pattern) {
    let region = nsepseq_to_region pattern_to_region $1
    in PTuple {region; value=$1} }

unit:
  "(" ")" { {region = cover $1 $2; value = $1, $2} }

(* Expressions *)

interactive_expr:
  expr EOF { $1 }

base_cond_opt_semi:
  base_cond ";"? {
    $1
  }

expr:
  base_cond__open(expr) | switch_expr(base_cond_opt_semi) { $1 }

base_cond__open(x):
  base_expr(x) | conditional(x) { $1 }

base_cond:
  base_cond__open(base_cond) { $1 }

%inline fun_args2:
  var_pattern ":" type_expr {
    let start  = $1.var.region in
    let stop   = type_expr_to_region $3 in
    let region = cover start stop in
    let value  = {pattern=PVar $1; colon=$2; type_expr=$3}
    in
    PTyped {region; value}
  }
| var_pattern { PVar $1 }

base_expr(right_expr):
  let_expr(right_expr)
| fun_expr(right_expr)
| local_type_decl(right_expr)
| local_module_decl(right_expr)
| local_module_alias(right_expr)
| disj_expr_level { $1 }

conditional(right_expr):
  if_then_else(right_expr) | if_then(right_expr) { $1 }

if_then_else(right_expr):
  "if" parenthesized_expr "{" closed_if ";"?  "}"
  "else" "{" right_expr ";"? "}" {
    let region = cover $1 $11 in
    let value  = {kwd_if   = $1;
                  test     = $2;
                  ifso     = {lbrace=$3; inside=($4,$5); rbrace=$6};
                  ifnot    = Some ($7,{lbrace=$8; inside=($9,$10); rbrace=$11})}
    in ECond {region; value} }

if_then(right_expr):
  "if" parenthesized_expr "{" closed_if ";"? "}" {
    let region   = cover $1 $6 in
    let value    = {kwd_if   = $1;
                    test     = $2;
                    ifso     = {lbrace=$3; inside=($4,$5); rbrace=$6};
                    ifnot    = None}
    in ECond {region; value} }

base_if_then_else__open(x):
  base_expr(x) | conditional(x) { $1 }

base_if_then_else:
  base_if_then_else__open(base_if_then_else) { $1 }


parenthesized_expr:
  "{" expr "}" | "(" expr ")" { $2 }

base_if_then_else_opt_semi:
  base_if_then_else ";"? { $1 }

closed_if:
  base_if_then_else__open(closed_if)
| switch_expr(base_if_then_else_opt_semi) { $1 }

switch_expr(right_expr):
  "switch" core_expr "{" cases(right_expr) "}" {
    let start = $1
    and stop = $5 in
    let region = cover start stop
    and cases = $4 in
    let value = {
      kwd_switch = $1;
      expr      = $2;
      lbrace    = $3;
      cases;
      rbrace  = $5 }
    in ECase {region; value} }

cases(right_expr):
  nseq(case_clause(right_expr)) {
    let hd, tl = $1 in
    let nseq = snd hd, tl in {
      region = nsepseq_to_region (fun x -> x.region) nseq;
      value  = nseq} }

case_clause(right_expr):
  "|" pattern "=>" right_expr {
    let start  = pattern_to_region $2
    and stop   = expr_to_region $4 in
    let region = cover start stop
    and value  = {pattern=$2; arrow=$3; rhs=$4; terminator=None }
    in $1,{region; value} }

let_expr(right_expr):
  seq("[@attr]") "let" ioption("rec") let_binding ";" right_expr {
    let attributes = $1 in
    let kwd_let = $2 in
    let kwd_rec = $3 in
    let binding = $4 in
    let semi    = $5 in
    let body    = $6 in
    let stop    = expr_to_region $6 in
    let region  = cover $2 stop
    and value   = {kwd_let; kwd_rec; binding; semi; body; attributes}
    in ELetIn {region; value} }

local_type_decl(right_expr):
  type_decl ";" right_expr {
    let type_decl = $1.value in
    let semi      = $2 in
    let body      = $3 in
    let stop      = expr_to_region $3 in
    let region    = cover $1.region stop
    and value     = {type_decl; semi; body}
    in ETypeIn {region; value} }

local_module_decl(right_expr):
  module_decl ";" right_expr {
    let mod_decl  = $1.value in
    let semi      = $2 in
    let body      = $3 in
    let stop      = expr_to_region $3 in
    let region    = cover $1.region stop
    and value     = {mod_decl; semi; body}
    in EModIn {region; value} }

local_module_alias(right_expr):
  module_alias ";" right_expr {
    let mod_alias = $1.value in
    let semi      = $2 in
    let body      = $3 in
    let stop      = expr_to_region $3 in
    let region    = cover $1.region stop
    and value     = {mod_alias; semi; body}
    in EModAlias {region; value} }

fun_arg:
  sub_irrefutable type_annotation? {
    match $2 with
    | Some (colon, type_expr) ->
      let region = cover (pattern_to_region $1) (type_expr_to_region type_expr) in
      PTyped { value = {pattern=$1; colon; type_expr}; region }
    | None -> $1
  }

fun_expr(right_expr):
  ES6FUN fun_args2 "=>" right_expr {
    let region = cover (pattern_to_region $2) (expr_to_region $4) in
    let value = {
      binders = $2;
      lhs_type = None;
      arrow = $3;
      body = $4
    }
    in
    EFun {region; value}
  }
| ES6FUN "(" nsepseq(fun_arg, ",") ")" type_annotation? "=>" right_expr {
    let region = cover $1 (expr_to_region $7) in
    let ptuple_region = nsepseq_to_region pattern_to_region $3 in
    let (hd, tl) = $3 in
    let value = {
      binders = PPar {
        region = cover $2 $4;
        value = {
          lpar = $2;
          inside = (match tl with 
          | [] -> hd
          | _ -> PTuple {
            value = $3;
            region = ptuple_region;
          });
          rpar = $4;
        }
      };
      lhs_type = $5;
      arrow = $6;
      body = $7
    }
    in
    EFun {region; value}
}

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
| bin_op(comp_expr_level, "==", cat_expr_level) {
    ELogic (CompExpr (Equal $1)) }
| bin_op(comp_expr_level, "!=", cat_expr_level) {
    ELogic (CompExpr (Neq $1)) }
| cat_expr_level { $1 }

cat_expr_level:
  bin_op(add_expr_level, "++", cat_expr_level)    {  EString (Cat $1) }
| add_expr_level                                  {                $1 }

add_expr_level:
  bin_op(add_expr_level, "+", mult_expr_level)     {  EArith (Add $1) }
| bin_op(add_expr_level, "-", mult_expr_level)     {  EArith (Sub $1) }
| mult_expr_level                                  {               $1 }

mult_expr_level:
  bin_op(mult_expr_level, "*", shift_expr_level)    { EArith (Mult $1) }
| bin_op(mult_expr_level, "/", shift_expr_level)    {  EArith (Div $1) }
| bin_op(mult_expr_level, "mod", shift_expr_level)  {  EArith (Mod $1) }
| bin_op(mult_expr_level, "land", shift_expr_level) { EArith (Land $1) }
| bin_op(mult_expr_level, "lor", shift_expr_level)  {  EArith (Lor $1) }
| bin_op(mult_expr_level, "lxor", shift_expr_level) { EArith (Lxor $1) }
| shift_expr_level                                  {               $1 }

shift_expr_level:
  bin_op(unary_expr_level, "lsl", shift_expr_level) { EArith (Lsl $1) }
| bin_op(unary_expr_level, "lsr", shift_expr_level) { EArith (Lsr $1) }
| unary_expr_level                                  {              $1 }

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
    ELogic (BoolExpr (Not {region; value}))
  }
| call_expr_level { $1 }

call_expr_level:
  call_expr
| core_expr   { $1 }
| constr_expr { EConstr $1 }

constr_expr:
  "Some" argument {
    let region = cover $1 (expr_to_region $2)
    in ESomeApp {region; value=$1,$2}
  }
| "<constr>" argument {
    let region = cover $1.region (expr_to_region $2) in
    EConstrApp {region; value = ($1, Some $2)}
  }
| constant_constr { $1 }

argument:
  constant_constr { EConstr $1 }
| core_expr       {         $1 }

constant_constr:
  "None"     { ENone $1                             }
| "<constr>" { EConstrApp {$1 with value=($1,None)} }

call_expr:
  core_expr par(nsepseq(annot_expr, ",")) {
    let start  = expr_to_region $1 in
    let stop   = $2 in
    let region = cover start stop.region in
    ECall {region; value = $1,Multiple $2}
  }
| core_expr unit {
    let start  = expr_to_region $1 in
    let stop   = $2.region in
    let region = cover start stop
    and value  = $1, Unit $2
    in ECall {region; value} }

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
| "false"                             {  ELogic (BoolExpr (False $1)) }
| "true"                              {  ELogic (BoolExpr (True  $1)) }
| list__(expr)                        {          EList (EListComp $1) }
| spread                              {                            $1 }
| sequence                            {                       ESeq $1 }
| record_expr                         {                    ERecord $1 }
| update_record                       {                    EUpdate $1 }
| code_inj                            {                     ECodeInj $1 }
| par(annot_expr)                     {                         EPar $1 }
| par(tuple(annot_expr))    {
    let region  = $1.region in
    let inside = ETuple {value = $1.value.inside; region} in
    EPar {value = { $1.value with inside }; region = $1.region}
 }

code_inj:
  "[%lang" expr "]" {
    let region = cover $1.region $3
    and value  = {language=$1; code=$2; rbracket=$3}
    in {region; value} }

spread:
"[" expr "," "..." expr "]" {
    let region = cover $1 $6
    and value : cons_expr = {lbracket=$1; lexpr=$2; comma=$3; ellipsis=$4; rexpr=$5; rbracket=$6}
    in EList (ECons {region; value})
  }


annot_expr:
  expr type_annotation? {
    match $2 with
    | Some (colon, annot) ->
      let region = cover (expr_to_region $1) (type_expr_to_region annot) in
      EAnnot { value = $1, colon, annot; region }
    | None -> $1
  }

projection:
  struct_name selection {
    let start  = $1.region in
    let stop   = nsepseq_to_region selection_to_region (snd $2) in
    let region = cover start stop
    and value  = {struct_name = $1;
                  selector    = fst $2;
                  field_path  = snd $2}
    in {region; value}
  }

selection:
  "[" "<int>" "]" selection {
    let r, (hd, tl) = $4 in
    let result: (selection, dot) Utils.nsepseq =
      Component $2, (ghost, hd) :: tl
    in r, result
  }
| "." field_name selection {
    let r, (hd, tl) = $3 in
    let result: (selection, dot) Utils.nsepseq =
      FieldName $2, ($1, hd) :: tl
    in r, result
  }
| "." field_name  {    $1, (FieldName $2, []) }
| "[" "<int>" "]" { ghost, (Component $2, []) }

module_access_e :
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

record_expr:
  "{" field_assignment more_field_assignments? "}" {
    let compound = Some (Braces ($1,$4)) in
    let region   = cover $1 $4 in
    match $3 with
    | Some (comma, elts) ->
        let ne_elements = Utils.nsepseq_cons $2 comma elts in
        let value = {compound;
                     ne_elements;
                     terminator=None;
                     attributes=[]}
        in {value; region}
    | None ->
       let value = {compound;
                    ne_elements = ($2,[]);
                    terminator=None;
                    attributes=[]}
       in {value; region}
  }
| "{" field_name more_field_assignments "}" {
    let value = {
      field_name = $2;
      assignment = ghost;
      field_expr = EVar $2 } in
    let field_name = {$2 with value} in
    let comma, elts = $3 in
    let ne_elements = Utils.nsepseq_cons field_name comma elts in
    let compound = Some (Braces ($1,$4)) in
    let region   = cover $1 $4 in
    let value = {compound;
                 ne_elements;
                 terminator=None;
                 attributes=[]}
    in {value; region} }


update_record:
  "{" "..." path "," sep_or_term_list(field_path_assignment,",") "}" {
    let region = cover $1 $6 in
    let ne_elements, terminator = $5 in
    let value = {
      lbrace   = $1;
      ellipsis = $2;
      record   = $3;
      comma    = $4;
      updates  = {value = {compound = None;
                          ne_elements;
                          terminator;
                          attributes=[]};
                 region = cover $4 $6};
      rbrace   = $6}
    in {region; value} }

more_field_assignments:
  "," sep_or_term_list(field_assignment_punning,",") {
    let elts, _region = $2
    in $1, elts }

path:
 "<ident>"   { Name $1 }
| projection { Path $1 }

(* Sequences *)

sequence:
  "{" series "}" {
    let region   = cover $1 $3
    and compound = Some (Braces ($1,$3)) in
    let elements = Some $2 in
    let value    = {compound; elements; terminator=None}
    in {region; value} }

series:
  seq_expr ";" series? {
    match $3 with
    | Some s ->  Utils.nsepseq_cons $1 $2 s
    | None -> ($1, [])
  }
| last_expr     { $1,[] }

%inline last_expr_inner:
  fun_expr(last_expr_opt_semi)
| let_in_sequence
| local_type_decl(last_expr_opt_semi)
| switch_expr(last_expr_opt_semi) ";"? { $1 }

last_expr_opt_semi:
  last_expr_inner
| seq_expr ";"? { $1 }

last_expr:
  last_expr_inner
| seq_expr { $1 }

let_in_sequence:
  seq("[@attr]") "let" ioption("rec") let_binding ";" series  {
    let seq      = $6 in
    let stop     = nsepseq_to_region expr_to_region seq in
    let region   = match first_region $1 with
                     None -> cover $2 stop
                   | Some start -> cover start stop in
    let compound = None in
    let elements = Some seq in
    let value    = {compound; elements; terminator=None} in
    let body     = ESeq {region; value} in
    let value    = {attributes = $1;
                    kwd_let    = $2;
                    kwd_rec    = $3;
                    binding    = $4;
                    semi     = $5;
                    body}
    in ELetIn {region; value} }

seq_expr:
  disj_expr_level | conditional (closed_if) { $1 }

field_assignment_punning:
  (* This can only happen with multiple fields -
     one item punning does NOT work in ReasonML *)
  field_name {
    let value = {field_name = $1;
                 assignment = ghost;
                 field_expr = EVar $1}
    in {$1 with value}
  }
| field_assignment { $1 }

field_assignment:
  field_name ":" expr {
    let region = cover $1.region (expr_to_region $3)
    and value  = {field_name = $1;
                  assignment = $2;
                  field_expr = $3}
    in {region; value} }

field_path_assignment:
  path ":" expr {
    let region = cover (path_to_region $1) (expr_to_region $3)
    and value  = {field_path=$1; assignment=$2; field_expr=$3}
    in {region; value} }
