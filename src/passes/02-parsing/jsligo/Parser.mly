%{
(* START HEADER *)

[@@@warning "-42"]
[@@@warning "-33"]
[@@@warning "-32"]

open Simple_utils.Region
module CST = Cst.Jsligo
open! CST

(* Utilities *)


let first_region = function
  [] -> None
| x::_ -> Some x.Region.region

(* END HEADER *)
%}

(* To solve the dangling else problem. *)
%nonassoc below_ELSE
%nonassoc Else

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

%inline chevrons(X):
  "<" X ">" {
    let region = cover $1 $3
    and value  = {lchevron=$1; inside=$2; rchevron=$3}
    in {region; value} }

brackets(X):
  "[" X "]" {
    let region = cover $1 $3
    and value  = {lbracket=$1; inside=$2; rbracket=$3}
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

%inline type_name   : "<ident>" { $1 }
%inline field_name  : "<ident>" { $1 }
%inline module_name : "<uident>" { $1 }
%inline constr      : "<uident>" { $1 }

%inline 

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

block_statement:
  "{" statements "}" {
    let region = cover $1 $3 in
    let value = {
      lbrace = $1;
      inside = $2;
      rbrace = $3;
    }
    in SBlock {region; value}
  }

return_statement:
  "return" expr? {
    let region = match $2 with
      Some s -> cover $1 (expr_to_region s)
    | None -> $1
    in
    let value = {
      kwd_return  = $1;
      expr        = $2;
    }
    in
    SReturn {region; value}
  }

if_else_statement:
  "if" par(expr) statement "else" statement {
    let region = cover $1 (statement_to_region $5) in
    let value = {
      kwd_if = $1;
      test   = $2.value;
      ifso   = $3;
      ifnot  = Some ($4, $5);
    }
    in
    SCond {region; value}
}
| "if" par(expr) statement %prec below_ELSE {
    let region = cover $1 (statement_to_region $3) in
    let value = {
      kwd_if = $1;
      test   = $2.value;
      ifso   = $3;
      ifnot  = None;
    }
    in
    SCond {region; value}
  }

initializer_expr:
  expr             { $1 }

initializer_:
  "=" initializer_expr {
    ($1, $2)
  }

rest:
  "..." "<ident>" {
    let region = cover $1 $2.region in
    let value = {
      ellipsis = $1;
      rest     = $2;
    } in
    PRest {
      region;
      value
    }
  }

object_binding_property:
  "<ident>" initializer_? {
    match $2 with
    | Some (eq, expr) ->
      let region = cover $1.region (expr_to_region expr) in
      let value = {
         property = $1;
         eq;
         value = expr;
      } in
      PAssign {
        region;
        value
      }
    | None ->
      PVar $1
  }
| "<ident>" ":" binding_initializer {
    let region = cover $1.region $3.region in
    let value = {
      property = $1;
      colon    = $2;
      target   = $3;
    } in
    PDestruct {
      region;
      value
    }
}

object_binding_pattern_items:
  object_binding_property "," object_binding_pattern_items? {
    match $3 with
    | Some s -> Utils.nsepseq_cons $1 $2 s
    | None -> ($1, [])
  }
| object_binding_property { ($1, []) }
| rest                    { ($1, []) }

object_binding_pattern:
  "{" object_binding_pattern_items "}" {
    let region = cover $1 $3 in
    let value = {
      lbrace = $1;
      inside = $2;
      rbrace = $3;
    } in
    PObject { region; value };
  }

array_binding_pattern_item:
  /* empty  */          { PWild Region.ghost }
| rest                  { $1 }
| "<ident>"            { PVar $1}
| "_"                   { PWild $1 }
| array_binding_pattern { $1 }

array_binding_pattern_items:
  array_binding_pattern_item "," array_binding_pattern_items { Utils.nsepseq_cons $1 $2 $3 }
| array_binding_pattern_item                                 { ($1, []) }

array_binding_pattern:
  "[" array_binding_pattern_items "]"  {
    let region = cover $1 $3 in
    let value = {
      lbracket = $1;
      inside = $2;
      rbracket = $3
    } in
    PArray { region; value }
  }

binding_pattern:
  "<ident>"              { PVar $1 }
| object_binding_pattern  { $1 }
| array_binding_pattern   { $1 }
| "_"                     { PWild $1 }

%inline type_annot_opt:
  /* */         { None }
| ":" type_expr { Some ($1, $2)}

%inline type_annot:
  ":" type_expr { $1, $2 }

binding_initializer:
  seq("[@attr]") binding_pattern type_annot_opt initializer_ {
    let region = cover (pattern_to_region $2) (expr_to_region (snd $4))
    in
    let value = {
      binders    = $2;
      lhs_type   = $3;
      eq         = fst $4;
      expr       = snd $4;
      attributes = $1;
    } in
    {
      region; value
    }
   }

binding_list:
  nsepseq(binding_initializer, ",") {
    $1
  }

declaration:
  seq("[@attr]") "let" binding_list {
    let region = cover $2 (nsepseq_to_region (fun e -> e.region) $3) in
    let bindings = Utils.nsepseq_map 
      (fun ({value; region}: let_binding Region.reg) ->
      ({value = {value with attributes = ($1 @ value.attributes)}; region }: let_binding Region.reg)) 
      $3 
    in
    let value = {
      kwd_let  = $2;
      bindings
    } in
    SLet { region; value }
  }
| seq("[@attr]") "const" binding_list {
    let region = cover $2 (nsepseq_to_region (fun e -> e.region) $3) in
    let bindings = Utils.nsepseq_map 
      (fun ({value; region}: let_binding Region.reg) ->
      ({value = {value with attributes = ($1 @ value.attributes)}; region }: let_binding Region.reg)) 
      $3 
    in
    let value = {
      kwd_const  = $2;
      bindings
    }
    in
    SConst { region; value }
  }
| type_decl { $1 }


type_expr:
  fun_type | sum_type | record_type { $1 }

fun_type_arg:
  "<ident>" ":" type_expr {
    {name      = $1;
     colon     = $2;
     type_expr = $3; }
  }

fun_type2:
  cartesian | fun_type { $1 }

fun_type:
  "(" nsepseq(fun_type_arg, ",") ")" "=>" fun_type2 {
    let start = $1
    and stop   = type_expr_to_region $5 in
    let region = cover start stop in
    let args = {
      lpar = $1;
      inside = $2;
      rpar = $3;
    } in
    TFun {region; value=args,$4,$5} }

cartesian:
  core_type { $1 }
| seq("[@attr]") brackets(nsepseq(type_expr, ",")) {  TProd {
  inside = $2;
  attributes = $1} 
}

module_access_t:
  "<uident>" "." module_var_t {
  let start       = $1.region in
    let stop        = type_expr_to_region $3 in
    let region      = cover start stop in
    let value       = {module_name=$1; selector=$2; field=$3}
    in {region; value} 
  }

module_var_t: 
  module_access_t   { TModA $1 }
| "<ident>"             { TVar  $1 }

core_type:
  type_name            {       TVar $1 }
| "<string>"           {    TString $1 }
| "<int>"              {       TInt $1 }
|  "_"                 {      TWild $1 }
| par(type_expr)       {       TPar $1 }
| module_access_t      {      TModA $1 }
| type_name chevrons(nsepseq(type_expr, ",")) {
   let region = cover $1.region $2.region
   in TApp {region; value = $1,$2} }

sum_type:
  nsepseq(cartesian,"|") {
    let region = nsepseq_to_region type_expr_to_region $1 in
    (match $1 with 
      (hd, []) -> hd
    | _ ->
      TSum {
        region;
        value = {
          lead_vbar  = None;
          variants   = $1;
          attributes = []
        }
      })
  }
| seq("[@attr]") "|" nsepseq(cartesian,"|") {
    let region = cover $2 (nsepseq_to_region type_expr_to_region $3) in
    TSum {
      region;
      value = {
        lead_vbar  = Some $2;
        variants   = $3;
        attributes = $1
      }
    }
  }

record_type:
  seq("[@attr]") "{" sep_or_term_list(field_decl,",") "}" {
    let fields, terminator = $3 in
    let region = cover $2 $4
    and value = {
      compound = Some (Braces ($2,$4));
      ne_elements = fields;
      terminator;
      attributes=$1}
    in TObject {region; value} }

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
    let region = cover $2.region stop in
    let field_name = $2 in
    let value: field_decl = {
      field_name = field_name;
      colon=$3;
      field_type=$4;
      attributes= $1}
    in {region; value}
  }

type_decl:
  "type" type_name "=" type_expr {
    let region = cover $1 (type_expr_to_region $4) in
    let value  = {kwd_type  = $1;
                  name      = $2;
                  eq        = $3;
                  type_expr = $4}
    in SType {region; value}
  }

switch_statement:
  "switch" "(" expr ")" "{" nseq(case_block) "}" {
    let region = cover $1 $7 in
    let value = {
      kwd_switch  = $1;
      lpar        = $2;
      expr        = $3;
      rpar        = $4;
      lbrace      = $5;
      cases       = $6;
      rbrace      = $7;
    } in
    SSwitch {
      region;
      value
    }
  }

case_block:
  "case" expr ":" statements? {
    Switch_case {
      kwd_case   = $1;
      expr       = $2;
      colon      = $3;
      statements = $4;
    }
  }
| "default" ":" statements? {
  Switch_default_case {
    kwd_default = $1;
    colon       = $2;
    statements  = $3;
  }
}

statement:
  expr_statement { SExpr $1 }
| block_statement
| if_else_statement
| switch_statement
| import_statement
| iteration_statement
| return_statement { $1 }
| "export"? declaration
  { 
    match $1 with 
      Some s -> SExport {value = (s, $2); region = cover s (statement_to_region $2)}
    | None -> $2 
  }

iteration_statement:
  for_of_statement
| while_statement  { $1 }

for_of_statement:
  "for" "(" "let" "<ident>" "of" assignment_expr ")" statement {
    let region = cover $1 (statement_to_region $8) in
    SForOf {
      value = {
        kwd_for   = $1;
        lpar      = $2;
        const     = false;
        name      = $4;
        kwd_of    = $5;
        expr      = $6;
        rpar      = $7;
        statement = $8;
      };
      region
    }
  }
| "for" "(" "const" "<ident>" "of" assignment_expr ")" statement {
    let region = cover $1 (statement_to_region $8) in
    SForOf {
      value = {
        kwd_for   = $1;
        lpar      = $2;
        const     = true;
        name      = $4;
        kwd_of    = $5;
        expr      = $6;
        rpar      = $7;
        statement = $8;
      };
      region
    }
  }

while_statement: 
  "while" "(" expr ")" statement {
    let region = cover $1 (statement_to_region $5) in
    SWhile {
      value = {
        kwd_while = $1;
        lpar      = $2;
        expr      = $3;
        rpar      = $4;
        statement = $5;
      };
      region
    }
  }

import_statement: 
  "import" module_name "=" nsepseq(module_name, ".") { 
    let region = cover $1 (nsepseq_to_region (fun a -> a.region) $4) in 
    SImport {
      value = {
        kwd_import  = $1;
        alias       = $2;
        equal       = $3;
        module_path = $4;
      };
      region 
    }
   } 


namespace_statement: 
  "export"? "namespace" module_name "{" statements_with_namespace "}" { 
    let region = cover (match $1 with Some s -> s | _ -> $2) $6 in 
    let value = ($2, $3, {
      value = {
        lbrace = $4;
        inside = $5;
        rbrace = $6
      };
      region = cover $4 $6
    }) in
    SNamespace {value; region}
  }

statement_with_namespace:
  statement { $1 }
| namespace_statement { $1 }

statements:
  statement ";" statements? {
    match $3 with
    | Some s ->  Utils.nsepseq_cons $1 $2 s
    | None -> ($1, [])
  }
| statement { $1, [] }

statements_with_namespace:
  statement_with_namespace ";" statements_with_namespace? {
    match $3 with
    | Some s ->  Utils.nsepseq_cons $1 $2 s
    | None -> ($1, [])
  }
| statement_with_namespace { $1, [] }


contract:
  toplevel_statements EOF { {statements=$1; eof=$2} : CST.t }

toplevel_statements:
  statement_with_namespace ";" toplevel_statements {
    Utils.nseq_cons (TopLevel ($1, Some $2)) $3
  }
| "<directive>" toplevel_statements {
    Utils.nseq_cons (Directive $1) $2
  }
| statement_with_namespace ";"? { TopLevel ($1, $2), [] }

(* Expressions *)

expr_sequence:
  expr_statement "," expr_sequence {
    let region = cover (expr_to_region $1) $3.region in
    {
      value = Utils.nsepseq_cons $1 $2 $3.value;
      region
    }
  }
| expr_statement {
    {
      value = ($1, []);
      region = expr_to_region $1;
    }
}

arrow_function_body:
  "{" statements "}" {
    let region = cover $1 $3 in
    FunctionBody {
      region;
      value = {
        lbrace   = $1;
        inside = $2;
        rbrace   = $3;
      }
    }
  }
| expr_statement { ExpressionBody $1 }

expr_annot_sequence:
  expr type_annot "," expr_annot_sequence {
    let region = cover (expr_to_region $1) $4.region in
    let annot = EAnnot {
      region = cover (expr_to_region $1) (type_expr_to_region (snd $2));
      value = $1, fst $2, snd $2
    }
    in
    {
      value = Utils.nsepseq_cons annot $3 $4.value;
      region
    }
  }
| expr type_annot {
    let annot = EAnnot {
      region = cover (expr_to_region $1) (type_expr_to_region (snd $2));
      value = $1, fst $2, snd $2
    }
    in
    {
      value = (annot, []);
      region = expr_to_region $1;
    }
}

arrow_function:
  "(" ")" type_annot_opt "=>" arrow_function_body {
    let region = cover $1 (arrow_function_body_to_region $5) in
    let value = {
      parameters = EUnit {value = ($1,$2); region = cover $1 $2};
      lhs_type = $3;
      arrow    = $4;
      body     = $5;
    }
    in
    EFun {
      region;
      value;
    }
  }
| "(" expr_annot_sequence ")" type_annot_opt "=>" arrow_function_body {
    let region = cover $1 (arrow_function_body_to_region $6) in
    let value = {
      parameters = EPar {
        region = cover $1 $3;
        value = {
          lpar = $1;
          inside = ESeq $2;
          rpar = $3;
        }
      };
      lhs_type = $4;
      arrow    = $5;
      body     = $6;
    }
    in
    EFun {
      region;
      value;
    }
 }
| "<ident>" "=>" arrow_function_body {
    let region = cover $1.region (arrow_function_body_to_region $3) in
    let value = {
      parameters = EVar $1;
      lhs_type = None; (* TODO *)
      arrow = $2;
      body = $3
    } in
    EFun {
      region;
      value
    }
  }

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
  bin_op(conj_expr_level, "&&", comp_expr_level) {
    ELogic (BoolExpr (And $1)) }
| comp_expr_level { $1 }
| call_expr_level "as" type_expr { 
   EAnnot {
      region = cover (expr_to_region $1) (type_expr_to_region $3);
      value = $1, $2, $3
    }
}

comp_expr_level:
  bin_op(comp_expr_level, "<", add_expr_level) {
    ELogic (CompExpr (Lt $1)) }
| bin_op(comp_expr_level, "<=", add_expr_level) {
    ELogic (CompExpr (Leq $1)) }
| bin_op(comp_expr_level, ">", add_expr_level) {
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
  call_expr { $1 }
| new_expr  { $1 }

array_item:
  /* */      { Empty_entry Region.ghost }
| expr       { Expr_entry $1 }
| "..." expr {
  let region = cover $1 (expr_to_region $2) in
  let value: array_item_rest = {
    ellipsis = $1;
    expr     = $2;
  } in
  Rest_entry {
    region;
    value
  }
 }

array_items:
  array_item "," array_items {  Utils.nsepseq_cons $1 $2 $3 }
| array_item                 { ($1, []) }

array_literal:
  "[" array_items "]" {
    let region = cover $1 $3 in
    let value = {
      lbracket = $1;
      inside   = $2;
      rbracket = $3
    } in
    EArray {
      region;
      value
    }
  }

property_name:
  "<int>"    {       EArith (Int $1) }
| constr
| field_name {               EVar $1 }
| "<string>" {   EString (String $1) }

property:
  field_name {
    let region = $1.region in
    let value = EVar $1 in
    Punned_property {
      region;
      value
    }
  }
| property_name ":" expr {
  let region = cover (expr_to_region $1) (expr_to_region $3) in
  let value = {
    name  = $1;
    colon = $2;
    value = $3;
  } in
  Property {
    region;
    value
  }
 }
| "..." assignment_expr             {
  let region = cover $1 (expr_to_region $2) in
  let value : CST.property_rest = {
    ellipsis = $1;
    expr     = $2;
  } in
  Property_rest {
    region;
    value
  }
 }

object_literal:
  "{" sep_or_term_list(property, ",") "}" {
    let region = cover $1 $3 in
    let value = {
      lbrace = $1;
      inside = fst $2;
      rbrace = $3
    } in
    EObject {
      region;
      value;
    }
  }

member_expr:
  "<ident>"                 {                               EVar $1 }
| "_"                        {        EVar {value = "_"; region = $1}}
| "<int>"                    {                       EArith (Int $1) }
| "<bytes>"                  {                             EBytes $1 }
| "<string>"                 {                   EString (String $1) }
| "<uident>" "<verbatim>"    {                           
  let value = {
    language = $1;
    code = EString (Verbatim $2);
  }
  in
  ECodeInj {value; region = cover $1.region $2.region } }
| "false"                    {          ELogic (BoolExpr (False $1)) }
| "true"                     {           ELogic (BoolExpr (True $1)) }
| member_expr "[" expr "]"   {
  let region = cover (expr_to_region $1) $4 in
  let value = {
    expr = $1;
    selection = Component {
      region = cover $2 $4;
      value = {
        lbracket = $2;
        inside   = $3;
        rbracket = $4;
      }
    }
  } in
  EProj {
    region;
    value
  }
}
| member_expr "." field_name {
  let region = cover (expr_to_region $1) $3.region in
  let value = {
    expr = $1;
    selection = FieldName {
      region = cover $2 $3.region;
      value = {
        dot   = $2;
        value = $3;
      }
    }
  } in
  EProj {
    region;
    value
  }
 }
| module_access_e         { $1}
| array_literal           { $1 }
| "(" object_literal ")"  {
    let region = cover $1 $3 in
    let value = {
      lpar    = $1;
      inside  = $2;
      rpar    = $3;
    } in
    EPar { region; value }
  }
| "(" expr_sequence ")"  {
    let region = cover $1 $3 in
    let value = {
      lpar   = $1;
      inside = ESeq $2;
      rpar   = $3;
    } in
    EPar { region; value }
  }

module_access_e :
  module_name "." module_var_e {
    let start       = $1.region in
    let stop        = expr_to_region $3 in
    let region      = cover start stop in
    let value       = {module_name=$1; selector=$2; field=$3}
    in 
    EModA {region; value} }
| "Some" "(" expr_sequence ")" {
    let region = cover $1 $4 in
    EConstr (ESomeApp {region; value = ($1, ESeq $3)})
}
| "None" "(" ")" { 
    EConstr (ENone (cover $1 $3))  
}
| constr "(" ")" {
    EConstr (EConstrApp {$1 with value = ($1, None)})
}
| constr "(" expr_sequence ")" {
    let region = cover $1.region $4 in
    EConstr (EConstrApp {region; value = ($1, Some (ESeq $3))})
}

module_var_e:
  module_access_e   { $1 }
// | "or"              { EVar {value="or"; region=$1} }
| field_name        { EVar  $1 }
// | projection        { EProj $1 }


call_expr:
  member_expr par(nsepseq(expr, ","))
| call_expr par(nsepseq(expr, ",")) {
    let start  = expr_to_region $1 in
    let stop   = $2 in
    let region = cover start stop.region in
    ECall {region; value = $1,Multiple $2}
  }
| call_expr "(" ")"
| member_expr "(" ")" {
    let start  = expr_to_region $1 in
    let stop   = $3 in
    let region = cover start stop
    and value  = $1, (Unit {region = cover $2 $3; value = $2, $3})
    in ECall {region; value} }

new_expr:
  member_expr    { $1 }
| "new" new_expr {
    let region = cover $1 (expr_to_region $2) in
    let value = $1,$2 in
    ENew {region; value}
}

expr_statement:
  arrow_function                        { $1 }
| disj_expr_level                       { $1 }
| disj_expr_level "=" assignment_expr   { EAssign ($1, $2, $3) }

assignment_expr:
  expr_statement                        { $1 }

expr:
  assignment_expr                       { $1 }
| object_literal                        { $1 }

interactive_expr:
  expr EOF                              { $1 }
