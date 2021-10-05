(* Menhir specification of the parsing of PascaLIGO *)

%{
(* START HEADER *)

[@@@warning "-42"]

(* Dependencies *)

open Simple_utils.Region
module CST = Cst_pascaligo.CST
open CST

(* Utilities *)

let mk_wild region =
  let variable = {value="_"; region} in
  let value = {variable; attributes=[]}
  in {region; value}

let list_of_option = function
       None -> []
| Some list -> list

let mk_comp f arg1 op arg2 =
  let start  = expr_to_region arg1
  and stop   = expr_to_region arg2 in
  let region = cover start stop
  and value  = {arg1; op; arg2}
  in ELogic (CompExpr (f Region.{value; region}))

let mk_arith f arg1 op arg2 =
  let start  = expr_to_region arg1
  and stop   = expr_to_region arg2 in
  let region = cover start stop
  and value  = {arg1; op; arg2}
  in EArith (f Region.{value; region})

(* END HEADER *)
%}

(* See [ParToken.mly] for the definition of tokens. *)

(* Entry points *)

%start contract interactive_expr
%type <CST.t> contract
%type <CST.expr> interactive_expr

%on_error_reduce nseq(__anonymous_0(field_decl,SEMI))
%on_error_reduce nseq(__anonymous_0(field_path_assignment,SEMI))
%on_error_reduce nseq(__anonymous_0(binding,SEMI))
%on_error_reduce nseq(__anonymous_0(field_assignment,SEMI))
%on_error_reduce nseq(__anonymous_0(core_pattern,SEMI))
%on_error_reduce nseq(__anonymous_0(expr,SEMI))
%on_error_reduce nsepseq(field_assignment,SEMI)
%on_error_reduce nseq(__anonymous_0(statement,SEMI))
%on_error_reduce nsepseq(case_clause(expr),VBAR)
%on_error_reduce nsepseq(core_pattern,SEMI)
%on_error_reduce pattern
%on_error_reduce nsepseq(core_pattern,CONS)
%on_error_reduce nsepseq(case_clause(if_clause),VBAR)
%on_error_reduce lhs
%on_error_reduce map_lookup
%on_error_reduce nsepseq(statement,SEMI)
%on_error_reduce nsepseq(core_pattern,COMMA)
%on_error_reduce constr_pattern
%on_error_reduce core_expr
%on_error_reduce nsepseq(param_decl,SEMI)
%on_error_reduce nsepseq(selection,DOT)
%on_error_reduce nsepseq(field_path_assignment,SEMI)
%on_error_reduce nsepseq(binding,SEMI)
%on_error_reduce nsepseq(expr,SEMI)
%on_error_reduce add_expr
%on_error_reduce unary_expr
%on_error_reduce const_decl
%on_error_reduce open_const_decl
%on_error_reduce fun_decl
%on_error_reduce variant(fun_type_level)
%on_error_reduce nsepseq(variant(fun_type_level),VBAR)
%on_error_reduce core_type
%on_error_reduce nsepseq(field_decl,SEMI)
%on_error_reduce nsepseq(core_type,TIMES)
%on_error_reduce type_decl
%on_error_reduce prod_type_level
%on_error_reduce fun_type_level
%on_error_reduce cons_expr
%on_error_reduce cat_expr
%on_error_reduce set_membership
%on_error_reduce disj_expr
%on_error_reduce core_pattern
%on_error_reduce nsepseq(type_expr,COMMA)
%on_error_reduce expr
%on_error_reduce nsepseq(expr,COMMA)
%on_error_reduce option(SEMI)
%on_error_reduce option(VBAR)
%on_error_reduce projection
%on_error_reduce module_access_e
%on_error_reduce module_access_t
%on_error_reduce nsepseq(module_name,DOT)
%on_error_reduce nseq(declaration)
%on_error_reduce option(arguments)
%on_error_reduce path
%on_error_reduce nseq(Attr)
%on_error_reduce module_var_e
%on_error_reduce module_var_t

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
   static control-flow graph. The rule [nsepseq] is for non-empty such
   sequences. See module [Utils] for the types corresponding to the
   semantic actions of those rules.  *)

(* Possibly empty sequence of items *)

%inline seq(X):
  (**)    { [] }
| nseq(X) { let hd,tl = $1 in hd::tl }

(* Non-empty sequence of items *)

nseq(X):
  X         { $1, [] }
| X nseq(X) { let hd,tl = $2 in $1, hd::tl }

(* Non-empty separated sequence of items *)

nsepseq(X,Sep):
  X                    {                 $1,        [] }
| X Sep nsepseq(X,Sep) { let h,t = $3 in $1, ($2,h)::t }

(* Inlines *)

%inline variable    : "<ident>"  { $1 }
%inline type_var    : "<ident>"  { $1 }
%inline type_name   : "<ident>"  { $1 }
%inline fun_name    : "<ident>"  { $1 }
%inline field_name  : "<ident>"  { $1 }
%inline struct_name : "<ident>"  { $1 }
%inline module_name : "<uident>" { $1 }

(* Main *)

contract:
  module_ EOF { {$1 with eof=$2} }

module_:
  nseq(declaration) { {decl=$1; eof=Region.ghost} }

declaration:
  type_decl     {    TypeDecl $1 }
| const_decl    {   ConstDecl $1 }
| fun_decl      {     FunDecl $1 }
| module_decl   {  ModuleDecl $1 }
| module_alias  { ModuleAlias $1 }
| "<directive>" {   Directive $1 }

(* Attributes *)

%inline attributes:
  ioption(nseq("[@attr]") { Utils.nseq_to_list $1 }) { list_of_option $1 }

(* Type declarations *)

open_type_decl:
  "type" type_name type_params? "is" type_expr {
    let stop   = type_expr_to_region $5 in
    let region = cover $1 stop in
    let value  = {kwd_type   = $1;
                  name       = $2;
                  params     = $3;
                  kwd_is     = $4;
                  type_expr  = $5;
                  terminator = None}
    in {region; value} }

type_params:
  par(nsepseq(type_var,",")) { $1 }

type_decl:
  open_type_decl ";"? {
    let type_decl : CST.type_decl = $1.value in
    let type_decl = {type_decl with terminator=$2} in
    {$1 with value = type_decl} }

open_module_decl:
  "module" module_name "is" "{" module_ "}" {
    let region = cover $1 $6 in
    let value  = {kwd_module = $1;
                  name       = $2;
                  kwd_is     = $3;
                  enclosing  = Brace ($4,$6);
                  module_    = $5;
                  terminator = None}
    in {region; value} }

| "module" module_name "is" "begin" module_ "end" {
    let region = cover $1 $6 in
    let value  = {kwd_module = $1;
                  name       = $2;
                  kwd_is     = $3;
                  enclosing  = BeginEnd ($4,$6);
                  module_    = $5;
                  terminator = None}
    in {region; value} }

module_decl:
  open_module_decl ";"? {
    let mod_decl : CST.module_decl = $1.value in
    let mod_decl = {mod_decl with terminator=$2} in
    {$1 with value = mod_decl} }


open_module_alias:
  "module" module_name "is" nsepseq(module_name,".") {
    let stop   = nsepseq_to_region (fun x -> x.region) $4 in
    let region = cover $1 stop in
    let value  = {kwd_module = $1;
                  alias      = $2;
                  kwd_is     = $3;
                  binders    = $4;
                  terminator = None}
    in {region; value} }

module_alias:
  open_module_alias ";"? {
    let mod_alias : CST.module_alias = $1.value in
    let mod_alias = {mod_alias with terminator=$2} in
    {$1 with value = mod_alias} }

type_annotation:
  ":" type_expr { $1,$2 }

type_expr:
  fun_type_level | sum_type(fun_type_level) { $1 }

fun_type_level:
  prod_type_level { $1 }
| prod_type_level "->" fun_type_level {
    let start  = type_expr_to_region $1
    and stop   = type_expr_to_region $3 in
    let region = cover start stop in
    TFun {region; value = $1,$2,$3} }

prod_type_level:
  core_type { $1 }
| core_type "*" nsepseq(core_type,"*") {
    let value  = Utils.nsepseq_cons $1 $2 $3 in
    let region = nsepseq_to_region type_expr_to_region value
    in TProd {region; value} }

core_type:
  "<string>"      { TString $1 }
| "<int>"         { TInt    $1 }
| "_"             { TVar    {value="_"; region=$1} }
| type_name       { TVar    $1 }
| module_access_t { TModA   $1 }
| type_constr_app { TApp    $1 }
| record_type     { TRecord $1 }
| par(type_expr)  { TPar    $1 }

type_constr_app:
  type_name type_tuple {
    let region = cover $1.region $2.region
    in {region; value = $1,$2}
  }
| "map" type_tuple {
    let region = cover $1 $2.region in
    let type_constr = {value="map"; region=$1}
    in {region; value = type_constr, $2}
  }
| "big_map" type_tuple {
    let region = cover $1 $2.region in
    let type_constr = {value="big_map"; region=$1}
    in {region; value = type_constr, $2}
  }
| "set" par(type_expr) {
    let total = cover $1 $2.region in
    let type_constr = {value="set"; region=$1} in
    let {region; value = {lpar; inside; rpar}} = $2 in
    let tuple = {region; value={lpar; inside=inside,[]; rpar}}
    in {region=total; value = type_constr, tuple}
  }
| "list" par(type_expr) {
    let total = cover $1 $2.region in
    let type_constr = {value="list"; region=$1} in
    let {region; value = {lpar; inside; rpar}} = $2 in
    let tuple = {region; value={lpar; inside=inside,[]; rpar}}
    in {region=total; value = type_constr, tuple} }

type_tuple:
  par(nsepseq(type_expr,",")) { $1 }

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
  attributes "record" sep_or_term_list(field_decl,";") "end" {
    let fields, terminator = $3 in
    let region = cover $2 $4
    and value  = {kind        = NEInjRecord $2;
                  enclosing   = End $4;
                  ne_elements = fields;
                  terminator;
                 attributes=$1}
    in {region; value}
  }
| attributes "record" "[" sep_or_term_list(field_decl,";") "]" {
    let fields, terminator = $4 in
    let region = cover $2 $5
    and value  = {kind      = NEInjRecord $2;
                  enclosing = Brackets ($3,$5);
                  ne_elements = fields;
                  terminator;
                  attributes=$1}
    in {region; value} }

module_access_t:
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
  attributes field_name ":" type_expr {
    let stop   = type_expr_to_region $4 in
    let region = cover $2.region stop
    and value  = {attributes=$1; field_name=$2; colon=$3; field_type=$4}
    in {region; value} }

fun_expr:
  attributes "function" parameters ioption(type_annotation) "is" expr {
    let stop   = expr_to_region $6 in
    let region = cover $2 stop
    and value  = {kwd_function = $2;
                  param        = $3;
                  ret_type     = $4;
                  kwd_is       = $5;
                  return       = $6;
                  attributes   = $1}
    in {region; value} }

(* Function declarations *)

open_fun_decl:
  attributes ioption("recursive") "function" fun_name parameters
  ioption(type_annotation) "is" expr {
    let start  = match $2 with Some start -> start | None -> $3 in
    let stop   = expr_to_region $8 in
    let region = cover start stop
    and value  = {attributes   = $1;
                  kwd_recursive= $2;
                  kwd_function = $3;
                  fun_name     = $4;
                  param        = $5;
                  ret_type     = $6;
                  kwd_is       = $7;
                  return       = $8;
                  terminator   = None}
    in {region; value} }

fun_decl:
  open_fun_decl ";"? {
    {$1 with value = {$1.value with terminator=$2}} }

parameters:
  par(nsepseq(param_decl,";")) {$1}

param_decl:
  "var" var_pattern param_type? {
    let stop   = match $3 with
                   None -> $2.region
                 | Some (_,t) -> type_expr_to_region t in
    let region = cover $1 stop
    and value  = {kwd_var    = $1;
                  var        = $2;
                  param_type = $3}
    in ParamVar {region; value}
  }
| "var" "_" param_type? {
    let stop   = match $3 with
                   None -> $2
                 | Some (_,t) -> type_expr_to_region t in
    let region = cover $1 stop
    and value  = {kwd_var    = $1;
                  var        = mk_wild $2;
                  param_type = $3}
    in ParamVar {region; value}
  }
| "const" var_pattern param_type? {
    let stop   = match $3 with
                   None -> $2.region
                 | Some (_,t) -> type_expr_to_region t in
    let region = cover $1 stop
    and value  = {kwd_const  = $1;
                  var        = $2;
                  param_type = $3}
    in ParamConst {region; value}
  }
| "const" "_" param_type? {
    let stop   = match $3 with
                   None -> $2
                 | Some (_,t) -> type_expr_to_region t in
    let region = cover $1 stop
    and value  = {kwd_const  = $1;
                  var        = mk_wild $2;
                  param_type = $3}
    in ParamConst {region; value} }

param_type:
  ":" fun_type_level { $1,$2 }

block:
  "begin" sep_or_term_list(statement,";") "end" {
     let statements, terminator = $2 in
     let region = cover $1 $3
     and value  = {enclosing = BeginEnd ($1,$3);
                   statements;
                   terminator}
     in {region; value}
  }
| "block" "{" sep_or_term_list(statement,";") "}" {
     let statements, terminator = $3 in
     let region = cover $1 $4
     and value  = {enclosing = Block ($1,$2,$4);
                   statements;
                   terminator}
     in {region; value} }

statement:
  instruction     { Instr $1 }
| open_data_decl  { Data  $1 }

open_data_decl:
  open_const_decl   { LocalConst       $1 }
| open_var_decl     { LocalVar         $1 }
| open_fun_decl     { LocalFun         $1 }
| open_type_decl    { LocalType        $1 }
| open_module_decl  { LocalModule      $1 }
| open_module_alias { LocalModuleAlias $1 }

open_const_decl:
  attributes "const" unqualified_decl("=") {
    let pattern, const_type, equal, init, stop = $3 in
    let region = cover $2 stop
    and value  = {attributes=$1;
                  kwd_const=$2;
                  pattern;
                  const_type;
                  equal;
                  init;
                  terminator=None}
    in {region; value} }

open_var_decl:
  attributes "var" unqualified_decl(":=") {
    let pattern, var_type, assign, init, stop = $3 in
    let region = cover $2 stop
    and value  = {attributes=$1;
                  kwd_var=$2;
                  pattern;
                  var_type;
                  assign;
                  init;
                  terminator=None}
    in {region; value} }

unqualified_decl(OP):
  core_pattern ioption(type_annotation) OP expr {
    let region = expr_to_region $4
    in $1, $2, $3, $4, region }

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
    let value  = {kwd_remove = $1;
                  element    = $2;
                  kwd_from   = $3;
                  kwd_set    = $4;
                  set        = $5}
    in {region; value} }

map_remove:
  "remove" expr "from" "map" path {
    let region = cover $1 (path_to_region $5) in
    let value  = {kwd_remove = $1;
                  key        = $2;
                  kwd_from   = $3;
                  kwd_map    = $4;
                  map        = $5}
    in {region; value} }

set_patch:
  "patch" path "with" ne_injection("set",expr) {
    let set_inj = $4 (fun region -> NEInjSet region) in
    let region = cover $1 set_inj.region in
    let value  = {kwd_patch = $1;
                  path      = $2;
                  kwd_with  = $3;
                  set_inj}
    in {region; value} }

map_patch:
  "patch" path "with" ne_injection("map",binding) {
    let map_inj = $4 (fun region -> NEInjMap region) in
    let region  = cover $1 map_inj.region in
    let value   = {kwd_patch = $1;
                   path      = $2;
                   kwd_with  = $3;
                   map_inj}
    in {region; value} }

injection(Kind,element):
  Kind sep_or_term_list(element,";") "end" {
    fun mk_kwd ->
      let elements, terminator = $2 in
      let region = cover $1 $3
      and value  = {
        kind      = mk_kwd $1;
        enclosing = End $3;
        elements  = Some elements;
        terminator}
      in {region; value}
  }
| Kind "end" {
    fun mk_kwd ->
      let region = cover $1 $2
      and value  = {kind       = mk_kwd $1;
                    enclosing  = End $2;
                    elements   = None;
                    terminator = None}
      in {region; value}
  }
| Kind "[" sep_or_term_list(element,";") "]" {
    fun mk_kwd ->
      let elements, terminator = $3 in
      let region = cover $1 $4
      and value  = {kind      = mk_kwd $1;
                    enclosing = Brackets ($2,$4);
                    elements  = Some elements;
                    terminator}
      in {region; value}
  }
| Kind "[" "]" {
    fun mk_kwd ->
      let region = cover $1 $3
      and value  = {kind       = mk_kwd $1;
                    enclosing  = Brackets ($2,$3);
                    elements   = None;
                    terminator = None}
      in {region; value} }

ne_injection(Kind,element):
  Kind sep_or_term_list(element,";") "end" {
    fun mk_kwd ->
      let ne_elements, terminator = $2 in
      let region = cover $1 $3
      and value  = {kind      = mk_kwd $1;
                    enclosing = End $3;
                    ne_elements;
                    terminator;
                    attributes = []}
      in {region; value}
  }
| Kind "[" sep_or_term_list(element,";") "]" {
    fun mk_kwd ->
      let ne_elements, terminator = $3 in
      let region = cover $1 $4
      and value = {kind      = mk_kwd $1;
                   enclosing = Brackets ($2,$4);
                   ne_elements;
                   terminator;
                   attributes = []}
      in {region; value} }

binding:
  expr "->" expr {
    let start  = expr_to_region $1
    and stop   = expr_to_region $3 in
    let region = cover start stop
    and value  = {source = $1;
                  arrow  = $2;
                  image  = $3}
    in {region; value} }

record_patch:
  "patch" path "with" record_expr {
    let region = cover $1 $4.region in
    let value  = {kwd_patch  = $1;
                  path       = $2;
                  kwd_with   = $3;
                  record_inj = $4}
    in {region; value} }

proc_call:
  fun_call { $1 }

(* Conditionals instructions *)

conditional:
  "if" expr "then" if_clause ";"? "else" if_clause {
    let region = cover $1 (if_clause_to_region $7) in
    let value : CST.conditional = {
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
    let region = cover $1 $3 in
    let value  = {lbrace=$1; inside=$2; rbrace=$3}
    in ShortBlock {value; region} }

(* Case instructions and expressions *)

case_instr:
  case(if_clause) { $1 if_clause_to_region }

case(rhs):
  "case" expr "of" "|"? cases(rhs) "end" {
    fun rhs_to_region ->
      let region = cover $1 $6 in
      let value  = {kwd_case  = $1;
                    expr      = $2;
                    kwd_of    = $3;
                    enclosing = End $6;
                    lead_vbar = $4;
                    cases     = $5 rhs_to_region}
      in {region; value}
  }
| "case" expr "of" "[" "|"? cases(rhs) "]" {
    fun rhs_to_region ->
      let region = cover $1 $7 in
      let value  = {kwd_case  = $1;
                    expr      = $2;
                    kwd_of    = $3;
                    enclosing = Brackets ($4,$7);
                    lead_vbar = $5;
                    cases     = $6 rhs_to_region}
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
    fun rhs_to_region ->
      let start  = pattern_to_region $1 in
      let region = cover start (rhs_to_region $3)
      and value  = {pattern=$1; arrow=$2; rhs=$3}
      in {region; value} }

assignment:
  lhs ":=" rhs {
    let stop   = expr_to_region $3 in
    let region = cover (lhs_to_region $1) stop
    and value  = {lhs = $1; assign = $2; rhs = $3}
    in {region; value} }

rhs:
  expr { $1 }

lhs:
  path       {    Path $1 }
| map_lookup { MapPath $1 }

(* Loops *)

loop:
  while_loop { $1 }
| for_loop   { $1 }

while_loop:
  "while" expr block {
    let region = cover $1 $3.region
    and value  = {kwd_while=$1; cond=$2; block=$3}
    in While {region; value} }

for_loop:
  "for" variable "->" variable "in" "map" expr block {
    let region = cover $1 $8.region in
    let value  = {kwd_for    = $1;
                  var        = $2;
                  bind_to    = Some ($3,$4);
                  kwd_in     = $5;
                  collection = Map $6;
                  expr       = $7;
                  block      = $8}
    in For (ForCollect {region; value})
  }
| "for" variable ":=" expr "to" expr ioption(step_clause) block {
    let region = cover $1 $8.region in
    let value  = {kwd_for = $1;
                  binder  = $2;
                  assign  = $3;
                  init    = $4;
                  kwd_to  = $5;
                  bound   = $6;
                  step    = $7;
                  block   = $8}
    in For (ForInt {region; value})
  }
| "for" variable "in" collection expr block {
    let region = cover $1 $6.region in
    let value  = {kwd_for    = $1;
                  var        = $2;
                  bind_to    = None;
                  kwd_in     = $3;
                  collection = $4;
                  expr       = $5;
                  block      = $6}
    in For (ForCollect {region; value}) }

step_clause:
  "step" expr { $1,$2 }

collection:
  "set"  { Set  $1 }
| "list" { List $1 }

(* Expressions *)

interactive_expr:
  expr EOF { $1 }

expr:
  case(expr) { ECase ($1 expr_to_region) }
| fun_expr   { EFun $1                   }
| block_with { EBlock $1                 }
| cond_expr
| disj_expr  { $1 }

block_with:
  block "with" expr {
    let start  = $2
    and stop   = expr_to_region $3 in
    let region = cover start stop in
    let value  = {block=$1; kwd_with=$2; expr=$3}
    in {region; value} }

cond_expr:
  "if" expr "then" expr ";"? "else" expr {
    let region = cover $1 (expr_to_region $7) in
    let value : CST.cond_expr = {
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
  comp_expr "<"   cat_expr { mk_comp (fun reg -> Lt reg)    $1 $2 $3 }
| comp_expr "<="  cat_expr { mk_comp (fun reg -> Leq reg)   $1 $2 $3 }
| comp_expr ">"   cat_expr { mk_comp (fun reg -> Gt reg)    $1 $2 $3 }
| comp_expr ">="  cat_expr { mk_comp (fun reg -> Geq reg)   $1 $2 $3 }
| comp_expr "="   cat_expr { mk_comp (fun reg -> Equal reg) $1 $2 $3 }
| comp_expr "=/=" cat_expr { mk_comp (fun reg -> Neq reg)   $1 $2 $3 }
| cat_expr                 { $1 }

cat_expr:
  cons_expr "^" cat_expr {
    let start  = expr_to_region $1
    and stop   = expr_to_region $3 in
    let region = cover start stop
    and value  = {arg1=$1; op=$2; arg2=$3}
    in EString (Cat {region; value})
  }
| cons_expr { $1 }

cons_expr:
  add_expr "#" cons_expr {
    let start  = expr_to_region $1
    and stop   = expr_to_region $3 in
    let region = cover start stop
    and value  = {arg1=$1; op=$2; arg2=$3}
    in EList (ECons {region; value})
  }
| add_expr { $1 }

add_expr:
  add_expr "+" mult_expr { mk_arith (fun reg -> Add reg) $1 $2 $3 }
| add_expr "-" mult_expr { mk_arith (fun reg -> Sub reg) $1 $2 $3 }
| mult_expr              { $1 }

mult_expr:
  mult_expr "*"   unary_expr { mk_arith (fun reg -> Mult reg) $1 $2 $3 }
| mult_expr "/"   unary_expr { mk_arith (fun reg -> Div reg)  $1 $2 $3 }
| mult_expr "mod" unary_expr { mk_arith (fun reg -> Mod reg)  $1 $2 $3 }
| unary_expr                 { $1 }

unary_expr:
  "-" core_expr {
    let region = cover $1 (expr_to_region $2)
    and value  = {op=$1; arg=$2}
    in EArith (Neg {region; value})
  }
| "not" core_expr {
    let region = cover $1 (expr_to_region $2)
    and value  = {op=$1; arg=$2} in
    ELogic (BoolExpr (Not {region; value}))
  }
| core_expr { $1 }

core_expr:
  "<int>"                       { EArith (Int $1)              }
| "<nat>"                       { EArith (Nat $1)              }
| "<mutez>"                     { EArith (Mutez $1)            }
| "<ident>"                     { EVar $1                      }
| "<string>"                    { EString (String $1)          }
| "<verbatim>"                  { EString (Verbatim $1)        }
| "<bytes>"                     { EBytes $1                    }
| par(annot_expr)               { EAnnot $1                    }
| tuple_expr                    { ETuple $1                    }
| list_expr                     { EList $1                     }
| fun_call_or_par_or_projection { $1                           }
| module_access_e               { EModA $1                     }
| map_expr                      { EMap $1                      }
| set_expr                      { ESet $1                      }
| record_expr                   { ERecord $1                   }
| record_update                 { EUpdate $1                   }
| code_inj                      { ECodeInj $1                  }
| "<uident>"                    { EConstr {$1 with value=$1,None} }
| "<uident>" arguments {
    let region = cover $1.region $2.region in
    EConstr {region; value = $1, Some $2} }

fun_call_or_par_or_projection:
  par(expr) arguments? {
    let parenthesized = EPar $1 in
    match $2 with
      None -> parenthesized
    | Some args ->
        let region = cover $1.region args.region in
        ECall {region; value = parenthesized,args}
  }
| projection arguments? {
    let project = EProj $1 in
    match $2 with
      None -> project
    | Some args ->
        let region = cover $1.region args.region
        in ECall {region; value = project,args}
  }
| fun_call { ECall $1 }

annot_expr:
  disj_expr ":" type_expr { $1,$2,$3 }

set_expr:
  injection("set",expr) { SetInj ($1 (fun region -> InjSet region)) }

map_expr:
  map_lookup {
    MapLookUp $1
  }
| injection("map",binding) {
    MapInj ($1 (fun region -> InjMap region))
  }
| injection("big_map",binding) {
    BigMapInj ($1 (fun region -> InjBigMap region)) }

map_lookup:
  path brackets(expr) {
    let region = cover (path_to_region $1) $2.region in
    let value  = {path=$1; index=$2}
    in {region; value} }

path:
  variable   { Name $1 }
| projection { Path $1 }

projection:
  struct_name "." nsepseq(selection,".") {
    let stop   = nsepseq_to_region selection_to_region $3 in
    let region = cover $1.region stop
    and value  = {struct_name=$1; selector=$2; field_path=$3}
    in {region; value} }

module_access_e:
  module_name "." module_var_e {
    let start       = $1.region in
    let stop        = expr_to_region $3 in
    let region      = cover start stop in
    let value       = {module_name=$1; selector=$2; field=$3}
    in {region; value} }

module_var_e:
  module_access_e { EModA $1                         }
| field_name      { EVar $1                          }
| "map"           { EVar {value="map";    region=$1} }
| "or"            { EVar {value="or";     region=$1} }
| "and"           { EVar {value="and";    region=$1} }
| "remove"        { EVar {value="remove"; region=$1} }
| projection      { EProj $1                         }

selection:
  field_name { FieldName $1 }
| "<int>"    { Component $1 }

record_expr:
  ne_injection("record",field_assignment) {
    $1 (fun region -> NEInjRecord region) }

field_assignment:
  field_name "=" expr {
    let region = cover $1.region (expr_to_region $3)
    and value  = {field_name=$1; assignment=$2; field_expr=$3}
    in {region; value} }

record_update:
  path "with" ne_injection("record",field_path_assignment) {
    let updates = $3 (fun region -> NEInjRecord region) in
    let region  = cover (path_to_region $1) updates.region in
    let value   = {record=$1; kwd_with=$2; updates}
    in {region; value} }

field_path_assignment:
  path "=" expr {
    let region = cover (path_to_region $1) (expr_to_region $3)
    and value  = {field_path=$1; assignment=$2; field_expr=$3}
    in {region; value} }

code_inj:
  "[%lang" expr "]" {
    let region   = cover $1.region $3
    and value    = {language=$1; code=$2; rbracket=$3}
    in {region; value} }

fun_call:
  fun_name arguments {
    let region = cover $1.region $2.region
    in {region; value = (EVar $1), $2}
  }
| module_access_e arguments {
    let region = cover $1.region $2.region
    in {region; value = (EModA $1), $2} }

tuple_expr:
  par(tuple_comp) { $1 }

tuple_comp:
  expr "," nsepseq(expr,",") { Utils.nsepseq_cons $1 $2 $3 }

arguments:
  par(nsepseq(expr,",")) { $1 }

list_expr:
  injection("list",expr) { EListComp ($1 (fun region -> InjList region)) }
| "nil"                  { ENil $1                                     }

(* Patterns *)

pattern:
  core_pattern { $1 }
| core_pattern "#" nsepseq(core_pattern,"#") {
    let value = Utils.nsepseq_cons $1 $2 $3 in
    let region = nsepseq_to_region pattern_to_region value
    in PList (PCons {region; value}) }

core_pattern:
  "<int>"        { PInt    $1 }
| "<nat>"        { PNat    $1 }
| "<bytes>"      { PBytes  $1 }
| "<string>"     { PString $1 }
| "_"            { PVar    (mk_wild $1) }
| var_pattern    { PVar    $1 }
| list_pattern   { PList   $1 }
| tuple_pattern  { PTuple  $1 }
| constr_pattern { PConstr $1 }
| record_pattern { PRecord $1 }

var_pattern:
  attributes "<ident>" {
    let value = {variable=$2; attributes=$1}
    in {$2 with value} }

field_pattern:
  field_name {
    let region = $1.region in
    let pattern = PVar {region; value = {variable=$1; attributes=[]}} in
    let value   = {field_name=$1; eq=Region.ghost; pattern}
    in {region; value}
  }
| field_name "=" core_pattern {
    let start  = $1.region
    and stop   = pattern_to_region $3 in
    let region = cover start stop
    and value  = {field_name=$1; eq=$2; pattern=$3}
    in {region; value} }

record_pattern:
  injection("record", field_pattern) {
    $1 (fun region -> InjRecord region) }

list_pattern:
  "nil"                          {      PNil $1 }
| par(cons_pattern)              {  PParCons $1 }
| injection("list",core_pattern) {
    PListComp ($1 (fun region -> InjList region)) }

cons_pattern:
  core_pattern "#" pattern { $1,$2,$3 }

tuple_pattern:
  par(nsepseq(core_pattern,",")) { $1 }

constr_pattern:
  "<uident>" ioption(tuple_pattern) {
    let region = match $2 with
                   None -> $1.region
                 | Some stop -> cover $1.region stop.region
    in {region; value = ($1,$2)} }
