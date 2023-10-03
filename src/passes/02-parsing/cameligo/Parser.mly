(* Menhir specification of the parser of CameLIGO *)

%{
(* START HEADER *)

[@@@warning "-42"]

(* Vendors dependencies *)

open Simple_utils.Region

(* LIGO dependencies *)

module CST = Cst_cameligo.CST
open! CST
module Wrap = Lexing_shared.Wrap

(* UTILITIES

   The following functions help build CST nodes. When they are
   complicated, like [mk_mod_path], it is because the grammar rule had
   to be written in a certain way to remain LR, and that way did not
   make it easy in the semantic action to collate the information into
   CST nodes. *)

let mk_reg region value = Region.{region; value}

let mk_mod_path :
  (module_name * dot) Utils.nseq * 'a ->
  ('a -> Region.t) ->
  'a CST.module_path Region.reg =
  fun (nseq, field) to_region ->
    let (first, sep), tail = nseq in
    let rec trans (seq, prev_sep as acc) = function
      [] -> acc
    | (item, next_sep) :: others ->
        trans ((prev_sep, item) :: seq, next_sep) others in
    let list, last_dot = trans ([], sep) tail in
    let module_path = first, List.rev list in
    let region = CST.nseq_to_region (fun (x,_) -> x#region) nseq in
    let region = Region.cover region (to_region field)
    and value = {module_path; selector=last_dot; field}
    in {value; region}

(* Hooking attributes, if any *)

let rec hook mk_attr attrs node =
  match attrs with
    []            -> node
  | attr :: attrs -> mk_attr attr @@ hook mk_attr attrs node

let hook_E_Attr = hook @@ fun a e -> E_Attr (a,e)
let hook_T_Attr = hook @@ fun a t -> T_Attr (a,t)

(* END HEADER *)
%}

(* Reductions on error *)

%on_error_reduce pattern
%on_error_reduce nsepseq(cons_pattern_level,COMMA)
%on_error_reduce variable
%on_error_reduce ctor
%on_error_reduce base_expr(expr)
%on_error_reduce app_expr_level
%on_error_reduce shift_expr_level
%on_error_reduce bin_op(add_expr_level,MINUS,mult_expr_level)
%on_error_reduce bin_op(add_expr_level,PLUS,mult_expr_level)
%on_error_reduce cons_expr_level
%on_error_reduce cat_expr_level
%on_error_reduce bin_op(conj_expr_level,BOOL_AND,comp_expr_level)
%on_error_reduce bin_op(disj_expr_level,REV_APP,conj_expr_level)
%on_error_reduce base_expr(base_cond)
%on_error_reduce match_expr(base_cond)
%on_error_reduce bin_op(disj_expr_level,BOOL_OR,conj_expr_level)
%on_error_reduce nsepseq(disj_expr_level,COMMA)
%on_error_reduce bin_op(disj_expr_level,Or,conj_expr_level)
%on_error_reduce ass_expr_level
%on_error_reduce tuple_expr_level
%on_error_reduce disj_expr_level
%on_error_reduce conj_expr_level
%on_error_reduce base_expr(closed_expr)
%on_error_reduce add_expr_level
%on_error_reduce seq_expr
%on_error_reduce core_type
%on_error_reduce nsepseq(core_type,TIMES)
%on_error_reduce fun_type_level
%on_error_reduce variant_type(fun_type_level)
%on_error_reduce cartesian_level
%on_error_reduce short_variant(fun_type_level)
%on_error_reduce nsepseq(core_irrefutable,COMMA)
%on_error_reduce cons_pattern_level
%on_error_reduce module_expr
%on_error_reduce module_path(module_name)
%on_error_reduce no_attr_expr
%on_error_reduce selected
%on_error_reduce field_path
%on_error_reduce nsepseq(selection,DOT)
%on_error_reduce ctor_app_pattern
%on_error_reduce module_path(__anonymous_9)
%on_error_reduce nseq(long_variant(fun_type_level))
%on_error_reduce nseq(no_attr_expr)
%on_error_reduce nsepseq(module_name,DOT)

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

brackets(X):
  "[" X "]" {
    let region = cover $1#region $3#region
    and value  = {lbracket=$1; inside=$2; rbracket=$3}
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

nsepseq(X,Sep):
  X                    {                 $1,        [] }
| X Sep nsepseq(X,Sep) { let h,t = $3 in $1, ($2,h)::t }

(* The rule [sep_or_term_list(item,sep)] ("separated or terminated
   list") parses a non-empty list of items separated by [sep], and
   optionally terminated by [sep]. The following rules were inspired
   by the following blog post by Pottier:

   http://gallium.inria.fr/blog/lr-lists/
*)

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

(* Non-empty comma-separated values (at least two values) *)

tuple(item):
  item "," nsepseq(item,",") { let h,t = $3 in $1, ($2,h)::t }

(* Lists *)

list_of(item):
  brackets(option(sep_or_term_list(item,";") { fst $1 })) { $1 }

(* Records *)

record(item):
  braces(option(sep_or_term_list(item,";") { fst $1 })) { $1 }

(* Aliasing and inlining some tokens *)

variable        : "<ident>"  { $1 }
record_name     : "<ident>"  { $1 }
module_name     : "<uident>" { $1 }
field_name      : "<ident>"  { $1 }
type_name       : "<ident>"  { $1 }
type_ctor       : "<ident>"  { $1 }
ctor            : "<uident>" { $1 }
record_or_tuple : "<ident>"  { $1 }

(* Unary operators *)

unary_op(op,arg):
  op arg {
    let region = cover $1#region (expr_to_region $2)
    and value  = {op=$1; arg=$2}
    in {region; value} }

(* Binary operators *)

bin_op(arg1,op,arg2):
  arg1 op arg2 {
    let start  = expr_to_region $1
    and stop   = expr_to_region $3 in
    let region = cover start stop
    and value  = {arg1=$1; op=$2; arg2=$3}
    in {region; value} }

(* Attributes *)

%inline
attributes:
  ioption(nseq("[@attr]") { Utils.nseq_to_list $1 }) {
    match $1 with None -> [] | Some list -> list }

(* ENTRY *)

contract:
  nseq(top_declaration) EOF { {decl=$1; eof=$2} }

top_declaration:
  declaration   { $1 }
| "<directive>" { D_Directive $1 } (* Only at top-level *)

declaration:
  type_decl      { D_Type      $1 }
| let_decl       { D_Let       $1 }
| module_decl    { D_Module    $1 }
| attr_decl      { D_Attr      $1 }
| module_include { D_Include   $1 }
| signature_decl { D_Signature $1 }

(* Attributed declarations *)

attr_decl:
 "[@attr]" declaration {
    let stop   = declaration_to_region $2 in
    let region = cover $1#region stop
    in {region; value = ($1,$2)} }

(* Top-level type declarations *)

type_decl:
  "type" ioption(type_vars) type_name "=" type_expr {
    let region = cover $1#region (type_expr_to_region $5)
    and value  = {kwd_type=$1; params=$2; name=$3; eq=$4; type_expr=$5}
    in {region; value} }

type_vars:
  type_var             { TV_Single $1 }
| par(tuple(type_var)) { TV_Tuple  $1 }

type_var:
  quoted_type_var { $1 }
| "_"             { {region=$1#region; value = (None, $1)} }

quoted_type_var:
  "'" variable {
    let region = cover $1#region $2#region
    and value  = Some $1, $2
    in {region; value} }

(* Type expressions *)

type_expr:
  fun_type_level | variant_type(fun_type_level) { $1 }

(* The following subgrammar is _stratified_ in the usual manner to
   build in the grammar the different priorities between the syntactic
   categories. Associativity is implemented by making a rule
   left-recursive or right-recursive. This is the same technique often
   used to handle arithmetic and Boolean expressions, for instance,
   without resorting to Menhir annotations. *)

(* Functional type expressions *)

(* The recursivity to the right of the rule [fun_type_level] enforces
   the right-associativity of the arrow type constructor. So "a -> b
   -> c" is parsed as "a -> (b -> c)". *)

fun_type_level:
  cartesian_level "->" fun_type_level {
    let start  = type_expr_to_region $1
    and stop   = type_expr_to_region $3 in
    let region = cover start stop in
    T_Fun {region; value = $1,$2,$3}
  }
| cartesian_level { $1 }

(* Cartesian products *)

cartesian_level:
  core_type "*" nsepseq(core_type,"*") {
    let start  = type_expr_to_region $1
    and stop   = nsepseq_to_region type_expr_to_region $3 in
    let region = cover start stop in
    T_Cart {region; value = $1,$2,$3}
  }
| core_type { $1 }

(* Core types *)

core_type:
  "[@attr]" core_type { T_Attr ($1,$2) }
| no_attr_type        { $1 }

no_attr_type:
  "<string>"        { T_String $1 }
| "<int>"           { T_Int    $1 }
| type_name         { T_Var    $1 } (* Removed "_" *)
| type_ctor_app     { T_App    $1 }
| record_type       { T_Record $1 }
| par(type_expr)    { T_Par    $1 }
| quoted_type_var   { T_Arg    $1 }
| qualified_type
| parameter_of_type {          $1 }

(* Parameter of contract *)

parameter_of_type:
  nsepseq(module_name,".") "parameter_of" {
    let start  = nsepseq_to_region (fun x -> x#region) $1 in
    let region = cover start $2#region
    in T_ParameterOf {region; value=$1} }

(* Variant types.

   We parameterise the variants by the kind of type expression that
   may occur at the rightmost side of a sentence. This enables to use
   [variant_type] in contexts that allow different types to avoid LR
   conflicts. For example, if the return type of a lambda is a
   functional type, parentheses are mandatory. *)

variant_type(right_type_expr):
  short_variant(right_type_expr)
  ioption(long_variants(right_type_expr)) {
    let region, tail =
      match $2 with
        None -> $1.region, []
      | Some long ->
          let stop = CST.nseq_to_region (fun (_,v) -> v.region) long
          in cover $1.region stop, Utils.nseq_to_list long in
    let value = {lead_vbar = None; variants = ($1, tail)}
    in T_Variant {region; value}
   }
| attributes long_variants(right_type_expr) {
    (* Attributes of the variant type *)
    let region    = CST.nseq_to_region (fun (_,v) -> v.region) $2 in
    let (lead_vbar, short_variant), list = $2 in
    let variants  = short_variant, list in
    let value     = {lead_vbar = Some lead_vbar; variants} in
    let type_expr = T_Variant {region; value}
    in hook_T_Attr $1 type_expr }

long_variants(right_type_expr):
  nseq(long_variant(right_type_expr)) { $1 }

long_variant(right_type_expr):
  "|" short_variant(right_type_expr) { $1,$2 }

short_variant(right_type_expr):
  "[@attr]" short_variant(right_type_expr) {
    let {region; value} = $2 in
    let value = {value with attributes = $1 :: value.attributes}
    in {region; value }
  }
| ctor ioption(of_type(right_type_expr)) {
    let stop   = match $2 with
                   None -> $1#region
                 | Some (_, t) -> type_expr_to_region t in
    let region = cover $1#region stop
    and value  = {ctor=$1; ctor_args=$2; attributes=[]}
    in {region; value} }

of_type(right_type_expr):
  "of" right_type_expr { $1,$2 }

(* Type constructor applications *)

type_ctor_app:
  type_ctor_arg type_ctor {
    let region = type_ctor_arg_to_region $1
    in mk_reg region (T_Var $2, $1) }

type_ctor_arg:
  no_attr_type          { TC_Single $1 }
| par(tuple(type_expr)) { TC_Tuple  $1 }

(* Record types *)

record_type:
  record(field_decl) { $1 }

(* When the type annotation is missing in a field declaration, the
   type of the field is the name of the field. *)

field_decl:
  attributes field_name ioption(type_annotation(type_expr)) {
    let stop = match $3 with
                        None -> $2#region
               | Some (_, t) -> type_expr_to_region t in
    let region = cover $2#region stop
    and value = {attributes=$1; field_name=$2; field_type=$3}
    in {region; value} }

(* Type qualifications

   The rule [module_path] is parameterised by what is derived after a
   series of selections of modules inside modules (nested modules),
   like [A.B.C.D]. For example, here, we want to qualify ("select") a
   type in a module, so the parameter is [type_name], because only
   types defined at top-level are in the scope (that is, any type
   declaration inside blocks is not). Then we can derive
   [A.B.C.D.t]. Notice that, in the semantic action of
   [type_in_module] we call the function [mk_mod_path] to reorganise
   the steps of the path and thus fit our CST. That complicated step
   is necessary because we need an LR(1) grammar. Indeed, rule
   [module_path] is right-recursive, yielding the reverse order of
   selection: "A.(B.(C))" instead of the expected "((A).B).C": the
   function [mk_mod_path] the semantic action of [type_in_module]
   reverses that path. We could have chosen to leave the associativity
   unspecified, like so:

     type_in_module(type_expr):
       nsepseq(module_name,".") "." type_expr { ... }

   Unfortunately, this creates a shift/reduce conflict (on "."),
   whence our more involved solution. *)

qualified_type:
  type_ctor_arg type_in_module(type_ctor { T_Var $1 }) {
    let start  = type_ctor_arg_to_region $1
    and stop   = type_expr_to_region $2 in
    let region = cover start stop
    in T_App {region; value=$2,$1}
  }
| type_in_module(type_name      { T_Var $1 })
| type_in_module(par(type_expr) { T_Par $1 }) { $1 }

type_in_module(type_expr):
  module_path(type_expr) {
    T_ModPath (mk_mod_path $1 type_expr_to_region) }

module_path(selected):
  module_name "." module_path(selected) {
    let (head, tail), selected = $3 in
    (($1,$2), head::tail), selected
  }
| module_name "." selected { (($1,$2), []), $3 }

(* Top-level value declarations *)

let_decl:
  "let" ioption("rec") let_binding {
    let stop   = expr_to_region $3.value.let_rhs in
    let region = cover $1#region stop
    in mk_reg region ($1,$2,$3.value) }

let_binding:
  fun_decl | non_fun_decl { $1 }

fun_decl:
  var_pattern type_params parameters rhs_type "=" expr {
    let binders = Utils.nseq_cons $1 $3 in
    let start = pattern_to_region $1 in
    let stop = expr_to_region $6 in
    let region = cover start stop in
    mk_reg region {binders; type_params=$2; rhs_type=$4; eq=$5; let_rhs=$6} }

non_fun_decl:
  irrefutable type_params rhs_type "=" expr {
    let start = pattern_to_region $1 in
    let stop = expr_to_region $5 in
    let region = cover start stop in
    mk_reg region {binders=($1,[]); type_params=$2; rhs_type=$3; eq=$4; let_rhs=$5} }

%inline
rhs_type:
  ioption(type_annotation(type_expr)) { $1 }

type_annotation(right_type_expr):
  ":" right_type_expr { $1,$2 }

%inline (* This %inline solves a shift/reduce conflict. *)
type_params:
  ioption(par("type" nseq(variable) { $1,$2 })) { $1 }

parameters:
  nseq(core_irrefutable) { $1 }

(* Top-level directives *)

module_include:
  "include" module_expr {
    let stop   = module_expr_to_region $2 in
    let region = cover $1#region stop
    and value  = {kwd_include=$1; module_expr=$2}
    in {region; value} }

(* Top-level module declaration *)

module_decl:
  "module" module_name ioption(module_constraint) "=" module_expr {
    let region = cover $1#region (module_expr_to_region $5)
    and value  = {kwd_module=$1; name=$2; eq=$4; module_expr=$5; annotation=$3}
    in {region; value} }

module_expr:
  structure                { M_Body $1 }
| module_name              { M_Var  $1 }
| module_path(module_name) { M_Path (mk_mod_path $1 (fun x -> x#region)) }

structure:
  "struct" ioption(nseq(declaration)) "end" {
    let region = cover $1#region $3#region
    and declarations =
      match $2 with
        None -> []
      | Some nseq -> Utils.nseq_to_list nseq in
    let value  = {kwd_struct=$1; declarations; kwd_end=$3}
    in {region; value} }

module_constraint:
  ":" signature_expr { $1,$2 }

signature_decl:
  "module" "type" module_name "=" signature_expr {
    let region = cover $1#region $4#region
    and value  = {kwd_module=$1; kwd_type=$2; name=$3; eq=$4;
                  signature_expr=$5}
    in {region; value} }

signature_sig:
  "sig" ioption(nseq(sig_item)) "end" {
    let sig_items =
      match $2 with
        None -> []
      | Some nseq -> Utils.nseq_to_list nseq in
    let region = cover $1#region $3#region in
    let value : signature_body = {kwd_sig=$1; kwd_end=$3; sig_items}
    in {region;value} }

sig_item:
  "val" variable type_annotation(type_expr) {
    let colon, t_expr = $3 in
    let value  = $1, $2, colon, t_expr
    and stop   = type_expr_to_region t_expr in
    let region = cover $1#region stop
    in S_Value {region; value}
  }
| "type" type_name "=" type_expr {
    let value  = $1, $2, $3, $4
    and stop   = type_expr_to_region $4 in
    let region = cover $1#region stop
    in S_Type {region; value}
  }
| "type" type_name {
    let region = cover $1#region $2#region
    in S_TypeVar {region; value=$1,$2}
  }
| "[@attr]" sig_item {
    let region = cover $1#region (sig_item_to_region $2)
    in S_Attr {region; value = $1,$2} }

signature_expr:
  module_path(module_name) { S_Path (mk_mod_path $1 (fun x -> x#region)) }
| module_name { S_Var $1 }
| signature_sig { S_Sig $1 }

(* PATTERNS *)

(* Irrefutable Patterns *)

%inline
irrefutable:
  tuple(core_irrefutable) {
    let region = nsepseq_to_region pattern_to_region $1
    in P_Tuple {region; value=$1}
  }
| core_irrefutable { $1 }

%inline
core_irrefutable:
  unit                        { P_Unit   $1 }
| record_pattern(irrefutable) { P_Record $1 }
| par(in_core_irrefutable)    { P_Par    $1 }
| var_pattern
| ctor_irrefutable            { $1 }

var_pattern:
  "_" | variable        { P_Var       $1 }
| "[@attr]" var_pattern { P_Attr ($1,$2) }

in_core_irrefutable:
  typed_irrefutable | irrefutable { $1 }

typed_irrefutable:
  irrefutable type_annotation(type_expr) {
    let start  = pattern_to_region $1
    and stop   = type_expr_to_region (snd $2) in
    let region = cover start stop in
    P_Typed {region; value = ($1,$2)} }

ctor_irrefutable:
  par(non_const_ctor_irrefutable) { P_Par  $1 }
| const_ctor_pattern              { P_Ctor $1 }

const_ctor_pattern:
  ctor { $1 }

non_const_ctor_irrefutable:
  ctor core_irrefutable {
    let region = cover $1#region (pattern_to_region $2)
    and value  = P_Ctor $1, Some $2
    in P_App {region; value} }

(* General Patterns *)

pattern:
  tuple(cons_pattern_level) {
    let region = nsepseq_to_region pattern_to_region $1
    in P_Tuple {region; value=$1} }
| cons_pattern_level { $1 }

cons_pattern_level:
  core_pattern "::" cons_pattern_level {
    let start  = pattern_to_region $1 in
    let colons = $2 in
    let stop   = pattern_to_region $3 in
    let region = cover start stop in
    P_Cons {region; value=$1,colons,$3} }
| core_pattern { $1 }

core_pattern:
  "_" | variable               { P_Var      $1 }
| unit                         { P_Unit     $1 }
| list_pattern                 { P_List     $1 }
| ctor_app_pattern             { P_App      $1 }
| record_pattern(core_pattern) { P_Record   $1 }
| attr_pattern                 { P_Attr     $1 }
| literal_pattern
| in_pattern
| qualified_pattern            { $1            }

%inline
literal_pattern:
  "<int>"      { P_Int      $1 }
| "<nat>"      { P_Nat      $1 }
| "<bytes>"    { P_Bytes    $1 }
| "<string>"   { P_String   $1 }
| "<verbatim>" { P_Verbatim $1 }
| "<mutez>"    { P_Mutez    $1 }
| "true"       { P_True     $1 }
| "false"      { P_False    $1 }

(* Parenthesised patterns *)

in_pattern:
  par(pattern | typed_pattern { $1 }) { P_Par $1 }

(* Attributed patterns *)

attr_pattern:
  "[@attr]" core_pattern { $1,$2 }

(* Qualified patterns (patterns modulo module paths) *)

qualified_pattern:
  pattern_in_module(ctor { P_Ctor $1 }) core_pattern {
    let start  = pattern_to_region $1
    and stop   = pattern_to_region $2 in
    let region = cover start stop
    in P_App {region; value = ($1, Some $2)}
  }
| pattern_in_module(variable   { P_Var $1 })
| pattern_in_module(in_pattern { $1 }) { $1 }

pattern_in_module(pattern):
  module_path(pattern) {
    P_ModPath (mk_mod_path $1 pattern_to_region) }

(* List patterns *)

list_pattern:
  list_of(cons_pattern_level) { $1 }

(* Constructed patterns *)

ctor_app_pattern:
  ctor core_pattern {
    let region = cover $1#region (pattern_to_region $2)
    in mk_reg region (P_Ctor $1, Some $2)
  }
| ctor { {region=$1#region; value = (P_Ctor $1, None)} }

(* Typed patterns *)

typed_pattern:
  pattern type_annotation(type_expr) {
    let start  = pattern_to_region $1
    and stop   = type_expr_to_region (snd $2) in
    let region = cover start stop
    in P_Typed {region; value=($1,$2)} }

(* Record patterns *)

record_pattern(rhs_pattern):
  record(field_pattern(rhs_pattern)) { $1 }

field_pattern(rhs_pattern):
  nseq("[@attr]") field_name {
    let attributes = Utils.nseq_to_list $1 in
    Punned {region=$2#region; value = {attributes; pun=$2}}
  }
| field_name | "_" {
    Punned {region=$1#region; value = {attributes=[]; pun=$1}}
  }
| attributes field_name "=" rhs_pattern {
    let stop   = pattern_to_region $4 in
    let region = cover $2#region stop
    and value  = {attributes=$1; field_lhs=$2; field_lens=$3;
                  field_rhs=$4}
    in Complete {region; value} }

(* Unit (value and pattern) *)

unit:
  "(" ")" { {region = cover $1#region $2#region; value = ($1,$2)} }

(* EXPRESSIONS *)

interactive_expr:
  expr EOF { $1 }

expr:
  attributes match_expr(base_cond) { hook_E_Attr $1 $2 }
| base_cond__open(expr)            { $1 } (* Next stratum *)

base_cond__open(right_expr):
  attributes conditional(right_expr) { hook_E_Attr $1 $2 }
| base_expr(right_expr)              { $1 } (* Next stratum *)

base_cond:
  base_cond__open(base_cond) { $1 } (* Tying the knot *)

base_expr(right_expr):
  attributes right_opened_expr(right_expr) { hook_E_Attr $1 $2 }
| ass_expr_level { $1 }
| while_expr { $1 }
| for_expr { $1 }

right_opened_expr(right_expr):
  let_in_expr(right_expr)
| let_mut_in_expr(right_expr)
| local_type_decl(right_expr)
| local_module_decl(right_expr)
| fun_expr(right_expr) { $1 }

(* Mutable value assignment *)

ass_expr_level:
  variable ":=" ass_expr_level {
    let start  = $1#region in
    let stop   = expr_to_region $3 in
    let region = cover start stop
    and value  = {binder=$1; ass=$2; expr=$3}
    in E_Assign {region; value} }
| tuple_expr_level { $1 }

(* Tuple expression *)

tuple_expr_level:
  tuple(disj_expr_level) {
    let region = nsepseq_to_region expr_to_region $1
    in E_Tuple {region; value=$1} }
| disj_expr_level { $1 }

(* Conditional expression *)

conditional(right_expr):
  if_then_else(right_expr) | if_then(right_expr) { $1 }

if_then_else(right_expr):
  "if" expr "then" closed_expr "else" right_expr {
    let region = cover $1#region (expr_to_region $6)
    and value  = {kwd_if=$1; test=$2; kwd_then=$3;
                  if_so=$4; if_not = Some ($5,$6)}
    in E_Cond {region; value} }

if_then(right_expr):
  "if" expr "then" right_expr {
    let region = cover $1#region (expr_to_region $4)
    and value  = {kwd_if=$1; test=$2; kwd_then=$3; if_so=$4; if_not=None}
    in E_Cond {region; value} }

base_if_then_else__open(right_expr):
  base_expr(right_expr) | if_then_else(right_expr) { $1 }

base_if_then_else:
  base_if_then_else__open(base_if_then_else) { $1 }

closed_expr:
  base_if_then_else__open(closed_expr)
| match_expr(base_if_then_else) { $1 }

(* Pattern matching *)

match_expr(right_expr):
  "match" expr "with" ioption("|") cases(right_expr) {
    let clauses : ( match_clause reg, vbar) Utils.nsepseq reg = {
      value  = Utils.nsepseq_rev $5;
      region = nsepseq_to_region (fun x -> x.region) $5}
    and stop = (fst $5).region in
    let region = cover $1#region stop
    and value  = {kwd_match=$1; subject=$2; kwd_with=$3;
                  lead_vbar=$4; clauses}
    in E_Match {region; value} }

cases(right_expr):
  match_clause(right_expr) {
    let start  = pattern_to_region $1.pattern
    and stop   = expr_to_region $1.rhs in
    let region = cover start stop
    in {region; value=$1}, []
  }
| cases(base_cond) "|" match_clause(right_expr) {
    let start =
      match $1 with
         only_case, [] -> only_case.region
      | _, other_cases -> last (fun f -> (fst f)#region) other_cases
    and stop             = expr_to_region $3.rhs in
    let region           = cover start stop in
    let fst_case         = {region; value=$3}
    and snd_case, others = $1
    in fst_case, ($2,snd_case)::others }

match_clause(right_expr):
  pattern "->" right_expr { {pattern=$1; arrow=$2; rhs=$3} }

(* Local value declaration *)

let_in_expr(right_expr):
  "let" ioption("rec") let_binding "in" right_expr {
    let region = cover $1#region (expr_to_region $5)
    and value  = {kwd_let=$1; kwd_rec=$2; binding=$3; kwd_in=$4; body=$5}
    in E_LetIn {region; value} }

(* Mutable value declaration *)

let_mut_in_expr(right_expr):
  "let" "mut" let_binding "in" right_expr {
    let region = cover $1#region (expr_to_region $5)
    and value  = {kwd_let=$1; kwd_mut=$2; binding=$3; kwd_in=$4; body=$5}
    in E_LetMutIn {region; value} }

(* Local type declaration *)

local_type_decl(right_expr):
  type_decl "in" right_expr {
    let region = cover $1.region (expr_to_region $3)
    and value  = {type_decl=$1; kwd_in=$2; body=$3}
    in E_TypeIn {region; value} }

(* Local module declaration *)

local_module_decl(right_expr):
  module_decl "in" right_expr {
    let region = cover $1.region (expr_to_region $3)
    and value  = {mod_decl=$1; kwd_in=$2; body=$3}
    in E_ModIn {region; value} }

(* Functional expression (a.k.a. lambda) *)

fun_expr(right_expr):
  "fun" type_params nseq(core_irrefutable) ret_type "->" right_expr {
    let region = cover $1#region (expr_to_region $6)
    and value  = {kwd_fun=$1; type_params=$2; binders=$3;
                  rhs_type=$4; arrow=$5; body=$6}
    in E_Fun {region; value} }

(* For loops *)

for_expr:
  for_int_expr
| for_in_expr { $1 }

for_int_expr:
  "for" index "=" expr direction expr block {
    let region = cover $1#region $7.region in
    let value  = {kwd_for=$1; index=$2; equal=$3; bound1=$4;
                  direction=$5; bound2=$6; body=$7}
    in E_For {region; value} }

index:
  variable { $1 }

direction:
  "upto"   { Upto   $1 }
| "downto" { Downto $1 }

block:
  "do" ioption(series) "done" {
    let region = cover $1#region $3#region
    and value  = {kwd_do=$1; seq_expr=$2; kwd_done=$3}
    in {region; value} }

for_in_expr:
  "for" pattern "in" expr block {
    let region = cover $1#region $5.region in
    let value  = {kwd_for=$1; pattern=$2; kwd_in=$3; collection=$4; body=$5}
    in E_ForIn {region; value} }

while_expr:
  "while" expr block {
    let region = cover $1#region $3.region
    and value  = {kwd_while=$1; cond=$2; body=$3}
    in E_While {region; value} }

%inline
ret_type:
  ioption(type_annotation(lambda_app_type)) { $1 }

lambda_app_type:
  cartesian_level | variant_type(cartesian_level) { $1 }

(* Resuming stratification of [base_expr] *)

disj_expr_level:
  bin_op(disj_expr_level, "||", conj_expr_level)
| bin_op(disj_expr_level, "or", conj_expr_level) { E_Or     $1 }
| bin_op(disj_expr_level, "|>", conj_expr_level) { E_RevApp $1 }
| conj_expr_level                                { $1          }

conj_expr_level:
  bin_op(conj_expr_level, "&&", comp_expr_level) { E_And $1 }
| comp_expr_level                                { $1       }

comp_expr_level:
  bin_op(comp_expr_level, "<",  cat_expr_level) { E_Lt    $1 }
| bin_op(comp_expr_level, "<=", cat_expr_level) { E_Leq   $1 }
| bin_op(comp_expr_level, ">",  cat_expr_level) { E_Gt    $1 }
| bin_op(comp_expr_level, ge,   cat_expr_level) { E_Geq   $1 }
| bin_op(comp_expr_level, "=",  cat_expr_level) { E_Equal $1 }
| bin_op(comp_expr_level, "<>", cat_expr_level) { E_Neq   $1 }
| cat_expr_level                                { $1         }

ge:
  ">" ZWSP "=" { Wrap.wrap ">=" (cover $1#region $3#region) }

cat_expr_level:
  bin_op(cons_expr_level, "^", cat_expr_level)     { E_Cat $1 }
| cons_expr_level                                  { $1       }

cons_expr_level:
  bin_op(add_expr_level, "::", cons_expr_level)    { E_Cons $1 }
| add_expr_level                                   { $1        }

add_expr_level:
  bin_op(add_expr_level, "+", mult_expr_level)     { E_Add $1 }
| bin_op(add_expr_level, "-", mult_expr_level)     { E_Sub $1 }
| mult_expr_level                                  { $1       }

mult_expr_level:
  bin_op(mult_expr_level, "*",    shift_expr_level) { E_Mult $1 }
| bin_op(mult_expr_level, "/",    shift_expr_level) { E_Div  $1 }
| bin_op(mult_expr_level, "mod",  shift_expr_level) { E_Mod  $1 }
| bin_op(mult_expr_level, "land", shift_expr_level) { E_Land $1 }
| bin_op(mult_expr_level, "lor",  shift_expr_level) { E_Lor  $1 }
| bin_op(mult_expr_level, "lxor", shift_expr_level) { E_Lxor $1 }
| shift_expr_level                                  { $1        }

shift_expr_level:
  bin_op(unary_expr_level, "lsl", shift_expr_level) { E_Lsl $1 }
| bin_op(unary_expr_level, "lsr", shift_expr_level) { E_Lsr $1 }
| unary_expr_level                                  { $1       }

unary_expr_level:
  unary_op("-",   app_expr_level) { E_Neg $1 }
| unary_op("not", app_expr_level) { E_Not $1 }
| app_expr_level                  { $1       }

app_expr_level:
  app_expr | core_expr { $1 }

(* Applications *)

app_expr:
  core_expr arguments {
    let start  = expr_to_region $1 in
    let stop   = match $2 with
                   e, [] -> expr_to_region e
                 | _, tl -> last expr_to_region tl in
    let region = cover start stop
    in E_App {region; value=($1,$2)}
  }
| "contract_of" nsepseq(module_name,".") {
    let stop   = nsepseq_to_region (fun x -> x#region) $2 in
    let region = cover $1#region stop
    in E_ContractOf {region; value=$2} }

arguments:
  nseq(no_attr_expr) { $1 }

(* Core expressions *)

core_expr:
  "[@attr]" core_expr { E_Attr ($1,$2) }
| no_attr_expr        { $1 }

no_attr_expr:
  unit            { E_Unit     $1 }
| list_of(expr)   { E_List     $1 }
| record_expr     { E_Record   $1 }
| code_inj        { E_CodeInj  $1 }
| par(typed_expr) { E_Typed    $1 }
| sequence_expr   { E_Seq      $1 }
| record_update   { E_Update   $1 }
| ctor            { E_Ctor     $1 }
| literal_expr
| path_expr       { $1 }

(* Literal expressions *)

%inline
literal_expr:
  "<int>"      { E_Int      $1 }
| "<nat>"      { E_Nat      $1 }
| "<mutez>"    { E_Mutez    $1 }
| "<string>"   { E_String   $1 }
| "<verbatim>" { E_Verbatim $1 }
| "<bytes>"    { E_Bytes    $1 }
| "true"       { E_True     $1 }
| "false"      { E_False    $1 }

(* Code injection *)

code_inj:
  "[%lang" expr "]" {
    let region = cover $1#region $3#region
    and value  = {language=$1; code=$2; rbracket=$3}
    in {region; value} }

(* Typed expression *)

typed_expr:
  expr type_annotation(type_expr) { $1,$2 }

(* Path expressions

   A path expression is a construct that qualifies unambiguously a
   value or type. When maintaining this subgrammar, be wary of not
   introducing a regression. Here are the currently possible cases:

      * a single variable: "a" or "@0" or "@let" etc.
      * a single variable in a nested module: "A.B.a"
      * nested fields and compoments from a variable: "a.0.1.b"
      * same within a nested module: "A.B.a.0.1.b"
      * nested fields and components from an expression: "(e).a.0.1.b" *)

path_expr:
  module_path(selected) { E_ModPath (mk_mod_path $1 expr_to_region) }
| local_path            { $1 }

selected:
  field_path { $1        }
| ctor       { E_Ctor $1 }
| "or"       { E_Var (Wrap.make "or" $1#region) }

field_path:
  record_or_tuple "." nsepseq(selection,".") {
    let stop   = nsepseq_to_region selection_to_region $3 in
    let region = cover $1#region stop
    and value  = {record_or_tuple=(E_Var $1); selector=$2; field_path=$3}
    in E_Proj {region; value}
  }
| par(expr) { E_Par $1 }
| variable  { E_Var $1 }

selection:
  field_name { FieldName $1 } (* Can be a component, e.g. "@1" *)
| "<int>"    { Component $1 }

local_path:
  par(expr) "." nsepseq(selection,".") {
    let stop   = nsepseq_to_region selection_to_region $3 in
    let region = cover $1.region stop
    and value  = {record_or_tuple=(E_Par $1); selector=$2; field_path=$3}
    in E_Proj {region; value}
  }
| field_path { $1 }

(* Record expression *)

record_expr:
  record(field_assignment) { $1 }

field_assignment:
  attributes field_name "=" expr {
    let region = cover $2#region (expr_to_region $4)
    and value  = {attributes=$1; field_lhs=$2; field_lens=$3;
                  field_rhs=$4}
    in Complete {region; value}
  }
| attributes field_name {
    let value = {attributes=$1; pun=$2}
    in Punned {region=$2#region; value} }

(* Functional updates of records *)

record_update:
  braces(update_expr) { $1 }

update_expr:
  no_attr_expr "with" sep_or_term_list(field_path_assignment,";") {
    {record=$1; kwd_with=$2; updates = fst $3} }

field_path_assignment:
  attributes field_name {
    let value = {attributes=$1; pun = Name $2}
    in Punned {region = $2#region; value} }
| attributes path field_lens expr {
    let region = cover (path_to_region $2) (expr_to_region $4)
    and value = {attributes=$1; field_lhs=$2; field_lens=$3;
                 field_rhs=$4}
    in Complete {region; value} }

field_lens:
  "="  { Lens_Id   $1 }
| "+=" { Lens_Add  $1 }
| "-=" { Lens_Sub  $1 }
| "*=" { Lens_Mult $1 }
| "/=" { Lens_Div  $1 }
| "|=" { Lens_Fun  $1 }

path:
  record_name { Name $1 }
| projection  { Path $1 }

projection:
  record_or_tuple "." nsepseq(selection,".") {
    let stop   = nsepseq_to_region selection_to_region $3 in
    let region = cover $1#region stop in
    let value  = {record_or_tuple=(E_Var $1); selector=$2; field_path=$3}
    in {region; value} }

(* Sequence expression *)

(* TODO Add support for production ["(" series ")"]
   (LR conflicts to solve). See CST *)

sequence_expr:
  "begin" ioption(series) "end" {
    let region   = cover $1#region $3#region
    and compound = Some (BeginEnd ($1,$3)) in
    let value    = {compound; elements=$2}
    in {region; value} }

series:
  seq_expr ";" series { Utils.nsepseq_cons $1 $2 $3 }
| last_expr           { $1,[] }

last_expr:
  seq_expr
| fun_expr(last_expr)
| match_expr(last_expr)
| let_in_sequence
| let_mut_sequence { $1 }

let_in_sequence:
  attributes "let" ioption("rec") let_binding "in" series {
    let stop   = nsepseq_to_region expr_to_region $6 in
    let region = cover $2#region stop in
    let value  = {compound=None; elements = Some $6} in
    let body   = E_Seq {region; value} in
    let value  = {kwd_let=$2; kwd_rec=$3; binding=$4; kwd_in=$5; body} in
    let let_in = E_LetIn {region; value}
    in hook_E_Attr $1 let_in }

let_mut_sequence:
  attributes "let" "mut" let_binding "in" series {
    let stop    = nsepseq_to_region expr_to_region $6 in
    let region  = cover $2#region stop in
    let value   = {compound=None; elements = Some $6} in
    let body    = E_Seq {region; value} in
    let value   = {kwd_let=$2; kwd_mut=$3; binding=$4; kwd_in=$5; body} in
    let let_mut = E_LetMutIn {region; value}
    in hook_E_Attr $1 let_mut }

seq_expr:
  ass_expr_level
| for_expr
| while_expr
| if_then_else(seq_expr) { $1 }
