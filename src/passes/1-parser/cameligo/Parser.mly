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
  LPAR X RPAR {
    let region = cover $1 $3
    and value  = {
      lpar   = $1;
      inside = $2;
      rpar   = $3}
    in {region; value}
  }

brackets(X):
  LBRACKET X RBRACKET {
    let region = cover $1 $3
    and value  = {
      lbracket = $1;
      inside   = $2;
      rbracket = $3}
    in {region; value}
  }

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

(* Possibly empy separated sequence of items *)

sepseq(item,sep):
  (**)              {    None }
| nsepseq(item,sep) { Some $1 }

(* Helpers *)

%inline type_name   : Ident  { $1 }
%inline field_name  : Ident  { $1 }
%inline module_name : Constr { $1 }
%inline struct_name : Ident  { $1 }

(* Non-empty comma-separated values (at least two values) *)

tuple(item):
  item COMMA nsepseq(item,COMMA) {
    let h,t = $3 in $1,($2,h)::t
  }

(* Possibly empty semicolon-separated values between brackets *)

list(item):
  LBRACKET sep_or_term_list(item,SEMI) RBRACKET {
    let elements, terminator = $2 in
    let value = {
      compound = Brackets ($1,$3);
      elements = Some elements;
      terminator} in
    let region = cover $1 $3
    in {value; region}
  }
| LBRACKET RBRACKET {
    let value = {
      compound   = Brackets ($1,$2);
      elements   = None;
      terminator = None} in
    let region = cover $1 $2
    in {value; region}}

(* Main *)

contract:
  declarations EOF {
    {decl=$1; eof=$2} }

declarations:
  declaration              { $1,[] : AST.declaration Utils.nseq }
| declaration declarations { Utils.nseq_cons $1 $2              }

declaration:
  type_decl       { TypeDecl $1 }
| let_declaration {      Let $1 }

(* Type declarations *)

type_decl:
  Type type_name EQ type_expr {
    let region = cover $1 (type_expr_to_region $4) in
    let value = {
      kwd_type   = $1;
      name       = $2;
      eq         = $3;
      type_expr  = $4}
    in {region; value} }

type_expr:
  cartesian                                              {         $1 }
| sum_type                                               {    TSum $1 }
| record_type                                            { TRecord $1 }

cartesian:
  fun_type TIMES nsepseq(fun_type,TIMES) {
    let value  = Utils.nsepseq_cons $1 $2 $3 in
    let region = nsepseq_to_region type_expr_to_region value
    in TProd {region; value}
  }
| fun_type { ($1 : type_expr) }

fun_type:
  core_type {
    $1
  }
| core_type ARROW fun_type {
    let start  = type_expr_to_region $1
    and stop   = type_expr_to_region $3 in
    let region = cover start stop in
    TFun {region; value=$1,$2,$3} }

core_type:
   type_name {
    TVar $1
  }
| module_name DOT type_name {
    let module_name = $1.value in
    let type_name   = $3.value in
    let value       = module_name ^ "." ^ type_name in
    let region      = cover $1.region $3.region
    in TVar {region; value}
  }
| arg=core_type constr=type_constr {
    let start      = type_expr_to_region arg in
    let stop       = constr.region in
    let region     = cover start stop in
    let lpar, rpar = ghost, ghost in
    let value      = {lpar; inside=arg,[]; rpar} in
    let arg        = {value; region = start} in
    TApp Region.{value = (constr,arg); region}
  }
| type_tuple type_constr {
    let region = cover $1.region $2.region
    in TApp {region; value = $2,$1}
  }
| par(type_expr) {
    TPar $1 }

type_constr:
  type_name { $1 }

type_tuple:
  par(tuple(type_expr)) { $1 }

sum_type:
  ioption(VBAR) nsepseq(variant,VBAR) {
    let region = nsepseq_to_region (fun x -> x.region) $2
    in {region; value=$2} }

variant:
  Constr Of cartesian {
    let region = cover $1.region (type_expr_to_region $3)
    and value = {constr=$1; arg = Some ($2, $3)}
    in {region; value}
  }
| Constr {
    {region=$1.region; value={constr=$1; arg=None}} }

record_type:
  LBRACE sep_or_term_list(field_decl,SEMI) RBRACE {
    let ne_elements, terminator = $2 in
    let region = cover $1 $3
    and value  = {
      compound = Braces ($1,$3);
      ne_elements;
      terminator}
   in {region; value} }

field_decl:
  field_name COLON type_expr {
    let stop   = type_expr_to_region $3 in
    let region = cover $1.region stop
    and value  = {field_name = $1; colon = $2; field_type = $3}
    in {region; value} }

(* Top-level non-recursive definitions *)

let_declaration:
  Let let_binding {
    let kwd_let = $1 in
    let binding = $2 in
    let value   = kwd_let, binding in
    let stop    = expr_to_region binding.let_rhs in
    let region  = cover $1 stop
    in {value; region} }

let_binding:
  Ident nseq(sub_irrefutable) type_annotation? EQ expr {
    let binders = Utils.nseq_cons (PVar $1) $2 in
    {binders; lhs_type=$3; eq=$4; let_rhs=$5}
  }
| irrefutable type_annotation? EQ expr {
    let binders = $1,[] in
    {binders; lhs_type=$2; eq=$3; let_rhs=$4} }

type_annotation:
  COLON type_expr { $1,$2 }

(* Patterns *)

irrefutable:
  tuple(sub_irrefutable) {
    let hd, tl = $1 in
    let start  = pattern_to_region hd in
    let stop   = last fst tl in
    let region = cover start stop
    in PTuple {value=$1; region}
  }
| sub_irrefutable { $1 }

sub_irrefutable:
  Ident                                                  {    PVar $1 }
| WILD                                                   {   PWild $1 }
| unit                                                   {   PUnit $1 }
| record_pattern                                         { PRecord $1 }
| par(closed_irrefutable)                                {    PPar $1 }
| Constr {
    let value  = $1, None
    and region = $1.region in    PConstr (PConstrApp {value; region}) }

closed_irrefutable:
  irrefutable {
    $1 }
| Constr sub_pattern {
    let stop   = pattern_to_region $2 in
    let region = cover $1.region stop
    and value  = $1, Some $2
    in PConstr (PConstrApp {value; region}) }
| typed_pattern {
    PTyped $1 }

typed_pattern:
  irrefutable COLON type_expr  {
    let start  = pattern_to_region $1 in
    let stop   = type_expr_to_region $3 in
    let region = cover start stop in
    let value  = {
      pattern   = $1;
      colon     = $2;
      type_expr = $3}
    in {value; region} }

pattern:
  sub_pattern CONS tail {
    let start = pattern_to_region $1 in
    let stop = pattern_to_region $3 in
    let region = cover start stop
    and value  = $1, $2, $3 in
    PList (PCons {region; value})
  }
| tuple(sub_pattern) {
    let start  = pattern_to_region (fst $1) in
    let stop   = last fst (snd $1) in
    let region = cover start stop
    in PTuple {value=$1; region}
  }
| core_pattern { $1 }

sub_pattern:
  par(tail)    { PPar $1 }
| core_pattern {      $1 }

core_pattern:
  Ident                                            {          PVar $1 }
| WILD                                             {         PWild $1 }
| Int                                              {          PInt $1 }
| Nat                                              {          PNat $1 }
| Bytes                                            {        PBytes $1 }
| String                                           {       PString $1 }
| unit                                             {         PUnit $1 }
| False                                            {        PFalse $1 }
| True                                             {         PTrue $1 }
| par(ptuple)                                      {          PPar $1 }
| list(tail)                                   { PList (PListComp $1) }
| constr_pattern                                   {       PConstr $1 }
| record_pattern                                   {       PRecord $1 }

record_pattern:
  LBRACE sep_or_term_list(field_pattern,SEMI) RBRACE {
    let ne_elements, terminator = $2 in
    let region = cover $1 $3 in
    let value = {
      compound = Braces ($1,$3);
      ne_elements;
      terminator}
    in {region; value} }

field_pattern:
  field_name EQ sub_pattern {
    let start  = $1.region
    and stop   = pattern_to_region $3 in
    let region = cover start stop
    and value  = {field_name=$1; eq=$2; pattern=$3}
    in {value; region} }

constr_pattern:
  C_None { PNone $1 }
| C_Some sub_pattern {
    let stop   = pattern_to_region $2 in
    let region = cover $1 stop
    and value  = $1, $2
    in PSomeApp {value; region}
  }
| Constr sub_pattern? {
    let start  = $1.region in
    let stop   =
      match $2 with
        Some p -> pattern_to_region p
      |   None -> start in
    let region = cover start stop
    and value  = $1,$2
    in PConstrApp {value; region} }

ptuple:
  tuple(tail) {
    let h, t = $1 in
    let start = pattern_to_region h in
    let stop = last fst t in
    let region = cover start stop in
    PTuple {value = $1; region} }

unit:
  LPAR RPAR {
    let value = ghost, ghost in
    let region = cover $1 $2
    in {value; region} }

tail:
  sub_pattern CONS tail {
    let start = pattern_to_region $1 in
    let stop = pattern_to_region $3 in
    let region = cover start stop in
    PList (PCons {value = ($1, $2, $3); region} )
  }
| sub_pattern {
    $1 }

(* Expressions *)

interactive_expr:
  expr EOF { $1 }

expr:
  base_cond__open(expr) {       $1 }
| match_expr(base_cond) { ECase $1 }

base_cond__open(x):
  base_expr(x)
| conditional(x) { $1 }

base_cond:
  base_cond__open(base_cond) { $1 }

base_expr(right_expr):
  tuple(disj_expr_level) {
    let start  = expr_to_region (fst $1) in
    let stop   = last fst (snd $1) in
    let region = cover start stop
    in ETuple {value=$1; region}
  }
| let_expr(right_expr)
| fun_expr(right_expr)
| disj_expr_level {
    $1 }

conditional(right_expr):
  if_then_else(right_expr)
| if_then(right_expr)      { ECond $1 }

if_then(right_expr):
  If expr Then right_expr {
    let the_unit = ghost, ghost in
    let ifnot    = EUnit {region=ghost; value=the_unit} in
    let stop     = expr_to_region $4 in
    let region   = cover $1 stop in
    let value = {
      kwd_if   = $1;
      test     = $2;
      kwd_then = $3;
      ifso     = $4;
      kwd_else = ghost;
      ifnot}
    in {value; region} }

if_then_else(right_expr):
  If expr Then closed_if Else right_expr {
    let region = cover $1 (expr_to_region $6)
    and value = {
      kwd_if   = $1;
      test     = $2;
      kwd_then = $3;
      ifso     = $4;
      kwd_else = $5;
      ifnot    = $6}
    in {value; region} }

base_if_then_else__open(x):
  base_expr(x)                                             {       $1 }
| if_then_else(x)                                          { ECond $1 }

base_if_then_else:
  base_if_then_else__open(base_if_then_else)               {       $1 }

closed_if:
  base_if_then_else__open(closed_if)                       {       $1 }
| match_expr(base_if_then_else)                            { ECase $1 }

match_expr(right_expr):
  Match expr With VBAR? cases(right_expr) {
    let cases = {
      value  = Utils.nsepseq_rev $5;
      region = nsepseq_to_region (fun x -> x.region) $5}
    and stop =
      match $5 with
        {region; _}, [] -> region
      |           _, tl -> last fst tl in
    let region = cover $1 stop
    and value = {
      kwd_match = $1;
      expr      = $2;
      kwd_with  = $3;
      lead_vbar = $4;
      cases}
    in {value; region} }

cases(right_expr):
  case_clause(right_expr) {
    let start  = pattern_to_region $1.pattern
    and stop   = expr_to_region $1.rhs in
    let region = cover start stop
    in {value=$1; region}, []
  }
| cases(base_cond) VBAR case_clause(right_expr) {
    let start =
      match $1 with
         only_case, [] -> only_case.region
      | _, other_cases -> last fst other_cases
    and stop = expr_to_region $3.rhs in
    let region = cover start stop in
    let fst_case = {value=$3; region}
    and snd_case, others = $1
    in fst_case, ($2,snd_case)::others }

case_clause(right_expr):
  pattern ARROW right_expr {
    {pattern=$1; arrow=$2; rhs=$3} }

let_expr(right_expr):
  Let let_binding In right_expr {
    let kwd_let = $1 in
    let binding = $2 in
    let kwd_in = $3 in
    let body = $4 in
    let stop = expr_to_region $4 in
    let region = cover $1 stop in
    let let_in = {kwd_let; binding; kwd_in; body}
    in ELetIn {region; value=let_in} }

fun_expr(right_expr):
  Fun nseq(irrefutable) ARROW right_expr {
    let stop   = expr_to_region $4 in
    let region = cover $1 stop in
    let f      = {
      kwd_fun  = $1;
      binders  = $2;
      lhs_type = None;
      arrow    = $3;
      body     = $4}
    in EFun {region; value=f} }

disj_expr_level:
  disj_expr                               { ELogic (BoolExpr (Or $1)) }
| conj_expr_level                                                { $1 }

bin_op(arg1,op,arg2):
  arg1 op arg2 {
    let start  = expr_to_region $1 in
    let stop   = expr_to_region $3 in
    let region = cover start stop in
    {value={arg1=$1; op=$2; arg2=$3}; region}
  }

disj_expr:
  bin_op(disj_expr_level, BOOL_OR, conj_expr_level)
| bin_op(disj_expr_level, Or,      conj_expr_level)              { $1 }

conj_expr_level:
  conj_expr                              { ELogic (BoolExpr (And $1)) }
| comp_expr_level                        {                         $1 }

conj_expr:
  bin_op(conj_expr_level, BOOL_AND, comp_expr_level)        { $1 }

comp_expr_level:
  lt_expr                              { ELogic (CompExpr (Lt    $1)) }
| le_expr                              { ELogic (CompExpr (Leq   $1)) }
| gt_expr                              { ELogic (CompExpr (Gt    $1)) }
| ge_expr                              { ELogic (CompExpr (Geq   $1)) }
| eq_expr                              { ELogic (CompExpr (Equal $1)) }
| ne_expr                              { ELogic (CompExpr (Neq   $1)) }
| cat_expr_level                       {                           $1 }

lt_expr:
  bin_op(comp_expr_level, LT, cat_expr_level)                    { $1 }

le_expr:
  bin_op(comp_expr_level, LE, cat_expr_level)                    { $1 }

gt_expr:
  bin_op(comp_expr_level, GT, cat_expr_level)                    { $1 }

ge_expr:
  bin_op(comp_expr_level, GE, cat_expr_level)                    { $1 }

eq_expr:
  bin_op(comp_expr_level, EQ, cat_expr_level)                    { $1 }

ne_expr:
  bin_op(comp_expr_level, NE, cat_expr_level)                    { $1 }

cat_expr_level:
  cat_expr                                        {  EString (Cat $1) }
(*| reg(append_expr)                           { EList (Append $1) } *)
| cons_expr_level                                 {                $1 }

cat_expr:
  bin_op(cons_expr_level, CAT, cat_expr_level)              { $1 }

(*
append_expr:
  cons_expr_level sym(APPEND) cat_expr_level               { $1,$2,$3 }
 *)

cons_expr_level:
  cons_expr                                         { EList (ECons $1) }
| add_expr_level                                    {               $1 }

cons_expr:
  bin_op(add_expr_level, CONS, cons_expr_level)                  { $1 }

add_expr_level:
  plus_expr                                         { EArith (Add $1) }
| minus_expr                                        { EArith (Sub $1) }
| mult_expr_level                                   {              $1 }

plus_expr:
  bin_op(add_expr_level, PLUS, mult_expr_level)                  { $1 }

minus_expr:
  bin_op(add_expr_level, MINUS, mult_expr_level)                 { $1 }

mult_expr_level:
  times_expr                                      {  EArith (Mult $1) }
| div_expr                                        {   EArith (Div $1) }
| mod_expr                                        {   EArith (Mod $1) }
| unary_expr_level                                {                $1 }

times_expr:
  bin_op(mult_expr_level, TIMES, unary_expr_level)               { $1 }

div_expr:
  bin_op(mult_expr_level, SLASH, unary_expr_level)               { $1 }

mod_expr:
  bin_op(mult_expr_level, Mod, unary_expr_level)                 { $1 }

unary_expr_level:
   MINUS call_expr_level {
    let start = $1 in
    let stop = expr_to_region $2 in
    let region = cover start stop
    and value  = {op = $1; arg = $2}
    in EArith (Neg {region; value}) }
| Not call_expr_level {
    let start = $1 in
    let stop = expr_to_region $2 in
    let region = cover start stop
    and value  = {op = $1; arg = $2} in
    ELogic (BoolExpr (Not ({region; value}))) }
| call_expr_level {
    $1 }

call_expr_level:
  call_expr                                              {   ECall $1 }
| constr_expr                                            { EConstr $1 }
| core_expr                                              {         $1 }

constr_expr:
  C_None {
    ENone $1
  }
| C_Some core_expr {
    let region = cover $1 (expr_to_region $2)
    in ESomeApp {value = $1,$2; region}
  }
| Constr core_expr? {
    let start = $1.region in
    let stop =
      match $2 with
        Some c -> expr_to_region c
      |   None -> start in
    let region = cover start stop
    in EConstrApp {value=$1,$2; region} }

call_expr:
  core_expr nseq(core_expr) {
    let start = expr_to_region $1 in
    let stop = match $2 with
      e, [] -> expr_to_region e
    | _,  l -> last expr_to_region l in
    let region = cover start stop in
    {value = $1,$2; region} }

core_expr:
  Int                                 {               EArith (Int $1) }
| Mutez                               {             EArith (Mutez $1) }
| Nat                                 {               EArith (Nat $1) }
| Ident | module_field                {                       EVar $1 }
| projection                          {                      EProj $1 }
| String                              {           EString (StrLit $1) }
| unit                                {                      EUnit $1 }
| False                               {  ELogic (BoolExpr (False $1)) }
| True                                {  ELogic (BoolExpr (True  $1)) }
| list(expr)                          {          EList (EListComp $1) }
| par(expr)                           {                       EPar $1 }
| sequence                            {                       ESeq $1 }
| record_expr                         {                    ERecord $1 }
| par(expr COLON type_expr {$1,$3}) {
    EAnnot {$1 with value=$1.value.inside} }

module_field:
  module_name DOT field_name {
    let region = cover $1.region $3.region in
    {value = $1.value ^ "." ^ $3.value; region} }

projection:
  struct_name DOT nsepseq(selection,DOT) {
    let start  = $1.region in
    let stop   = nsepseq_to_region selection_to_region $3 in
    let region = cover start stop in
    let value  = {
      struct_name = $1;
      selector    = $2;
      field_path  = $3}
    in {value; region}
  }
| module_name DOT field_name DOT nsepseq(selection,DOT) {
    let value  = $1.value ^ "." ^ $3.value in
    let struct_name = {$1 with value} in
    let start  = $1.region in
    let stop   = nsepseq_to_region selection_to_region $5 in
    let region = cover start stop in
    let value  = {
      struct_name;
      selector   = $4;
      field_path = $5}
    in {value; region} }

selection:
  field_name { FieldName $1 }
| Int        { Component $1 }

record_expr:
  LBRACE sep_or_term_list(field_assignment,SEMI) RBRACE {
    let ne_elements, terminator = $2 in
    let region = cover $1 $3 in
    let value = {
      compound = Braces ($1,$3);
      ne_elements;
      terminator}
    in {value; region} }

field_assignment:
  field_name EQ expr {
    let start  = $1.region in
    let stop   = expr_to_region $3 in
    let region = cover start stop in
    let value  = {
      field_name = $1;
      assignment = $2;
      field_expr = $3}
    in {value; region} }

sequence:
  Begin sep_or_term_list(expr,SEMI) End {
    let ne_elements, terminator = $2 in
    let value  = {
      compound = BeginEnd ($1,$3);
      elements = Some ne_elements;
      terminator} in
    let region = cover $1 $3
    in {value; region}
  }
| Begin End {
    let value  = {
      compound   = BeginEnd ($1,$2);
      elements   = None;
      terminator = None} in
    let region = cover $1 $2
    in {value; region} }
