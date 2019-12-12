%{
(* START HEADER *)

[@@@warning "-42"]

open Region
module AST = Parser_cameligo.AST
open AST


type 'a sequence_elements = {
  s_elts : ('a, semi) Utils.nsepseq;
  s_terminator  : semi option
}

type 'a record_elements = {
  r_elts : (field_assign reg, semi) Utils.nsepseq;
  r_terminator  : semi option
}

type 'a sequence_or_record =
  PaSequence of 'a sequence_elements
| PaRecord of 'a record_elements
| PaSingleExpr of expr

(* END HEADER *)
%}

(* See [ParToken.mly] for the definition of tokens. *)

(* Entry points *)

%start contract interactive_expr
%type <AST.t> contract
%type <AST.expr> interactive_expr


%nonassoc Ident
%nonassoc COLON (* Solves a shift/reduce problem that happens with record 
                   and sequences. To elaborate: 
                   - sequence_or_record_in can be reduced to 
                     expr -> Ident, but also to 
                     field_assignment -> Ident.                     
                    *)
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

braces(X):
  LBRACE X RBRACE {
    let region = cover $1 $3
    and value  = {
      lpar   = $1;
      inside = $2;
      rpar   = $3}
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
  LBRACKET sep_or_term_list(item, COMMA) RBRACKET {
    let elements, terminator = $2 in 
    { value =
      {
        compound = Brackets ($1,$3);
        elements   = Some elements;
        terminator;
      };
      region = cover $1 $3
    } 
  }
| LBRACKET RBRACKET {
    let value = {
      compound   = Brackets ($1,$2);
      elements   = None;
      terminator = None} in
    let region = cover $1 $2
    in {value; region}
  }
  
(* Main *)

contract:
  declarations EOF               { 
    {decl = $1; eof=$2} }

declarations:
  declaration              { $1,[] : AST.declaration Utils.nseq }
| declaration declarations { Utils.nseq_cons $1 $2              }

declaration:
| type_decl SEMI                                            { TypeDecl $1 }
| let_declaration SEMI                                      { Let      $1 }

(* Type declarations *)

type_decl:
  Type type_name EQ type_expr {  
    let region = cover $1 (type_expr_to_region $4) in
    let value = {
      kwd_type   = $1;
      name       = $2;
      eq         = $3;
      type_expr  = $4;
    }
    in {region; value}
  }

type_expr:
  cartesian                                                      { $1 }
| sum_type                                                  { TSum $1 }
| record_type                                            { TRecord $1 }

cartesian:
  fun_type COMMA nsepseq(fun_type,COMMA) {
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
    let region = cover (type_expr_to_region $1) (type_expr_to_region $3) in 
    TFun {region; value = ($1, $2, $3)}
}

core_type:
  type_name {
    TVar $1
  }
| module_name DOT type_name {
    let module_name = $1.value in
    let type_name = $3.value in
    let value = module_name ^ "." ^ type_name in 
    let region = cover $1.region $3.region
    in 
    TVar {region; value}
  }
| type_constr LPAR nsepseq(core_type, COMMA) RPAR {       
    let arg_val = $3 in
    let constr = $1 in
    let start = $1.region in 
    let stop = $4 in 
    let region = cover start stop in
    let lpar, rpar = $2, $4 in
    TApp Region.{value = constr, {
      value = {
        lpar; 
        rpar;
        inside = arg_val
      };
      region = cover lpar rpar;
    }; region}
  }
| par (type_expr) { 
  TPar $1
}

type_constr:
  type_name { $1                               }

sum_type:
  VBAR nsepseq(variant,VBAR) { 
    let region = nsepseq_to_region (fun x -> x.region) $2
    in {region; value = $2}
  }

variant:
  Constr LPAR cartesian RPAR {
    let region = cover $1.region $4
    and value = {constr = $1; arg = Some ($2, $3)}
    in {region; value}
  }
| Constr {  
    {region=$1.region; value= {constr=$1; arg=None}} }

record_type:
  LBRACE sep_or_term_list(field_decl,COMMA) RBRACE {    
    let ne_elements, terminator = $2 in
    let region = cover $1 $3
    and value  = {
     compound = Braces ($1,$3);
     ne_elements;
     terminator;
     }
   in {region; value}  
  }

type_expr_field:
  core_type                                             {   $1 }
| sum_type                                               {    TSum $1 }
| record_type                                            { TRecord $1 }

field_decl:
  field_name {
    let value  = {field_name = $1; colon = Region.ghost; field_type = TVar $1}
    in {region = $1.region; value} 
  }
  | field_name COLON type_expr_field {
    let stop   = type_expr_to_region $3 in
    let region = cover $1.region stop
    and value  = {field_name = $1; colon = $2; field_type = $3}
    in {region; value} 
  }

(* Top-level non-recursive definitions *)

let_declaration:
  Let let_binding {
    let kwd_let = $1 in 
    let binding, (region: Region.region) = $2 in
    {value = kwd_let, binding; region}
  }
   
es6_func:
  ARROW expr {
   $1, $2
  }

let_binding:
 | Ident type_annotation? EQ expr {
   let pattern = PVar $1 in
   let start = pattern_to_region pattern in
   let stop = expr_to_region $4 in
   let region = cover start stop in
  ({binders = pattern, []; lhs_type=$2; eq=$3; let_rhs=$4}, region)
 }
| tuple(sub_irrefutable) type_annotation? EQ expr {  
    let h, t = $1 in    
    let start = pattern_to_region h in
    let stop = last (fun (region, _) -> region) t in
    let region = cover start stop in   
    let pattern = PTuple { value = $1; region } in
    let start = region in
    let stop = expr_to_region $4 in  
    let region = cover start stop in
    ({binders = pattern, []; lhs_type=$2; eq=$3; let_rhs=$4}, region)
}
| WILD type_annotation? EQ expr {         
    let pattern = PWild $1 in
    let start = pattern_to_region pattern in
    let stop = expr_to_region $4 in  
    let region = cover start stop in
    ({binders = pattern, []; lhs_type=$2; eq=$3; let_rhs=$4}, region)
  }
| unit type_annotation? EQ expr {         
    let pattern = PUnit $1 in
    let start = pattern_to_region pattern in
    let stop = expr_to_region $4 in  
    let region = cover start stop in
    ({binders = pattern, []; lhs_type=$2; eq=$3; let_rhs=$4}, region)
  }
| record_pattern type_annotation? EQ expr {         
    let pattern = PRecord $1 in
    let start = pattern_to_region pattern in
    let stop = expr_to_region $4 in  
    let region = cover start stop in
    ({binders = pattern, []; lhs_type=$2; eq=$3; let_rhs=$4}, region)
  }
| par(closed_irrefutable) type_annotation? EQ expr {         
    let pattern = PPar $1 in
    let start = pattern_to_region pattern in
    let stop = expr_to_region $4 in  
    let region = cover start stop in
    ({binders = pattern, []; lhs_type=$2; eq=$3; let_rhs=$4}, region)
  }

type_annotation:
  COLON type_expr { $1,$2 }

(* Patterns *)

irrefutable:
  tuple(sub_irrefutable) {  
    let h, t = $1 in    
    let start = pattern_to_region h in
    let stop = last (fun (region, _) -> region) t in
    let region = cover start stop in    
    PTuple { value = $1; region }
  }
| sub_irrefutable                                        {         $1 }

sub_irrefutable:
  Ident                                                  {    PVar $1 }
| WILD                                                   {   PWild $1 }
| unit                                                   {   PUnit $1 }
| record_pattern                                         { PRecord $1 }
| par(closed_irrefutable)                                {    PPar $1 }

closed_irrefutable:
  irrefutable                                            {         $1 }
| constr_pattern                                         { PConstr $1 }
| typed_pattern                                          {  PTyped $1 }

typed_pattern:
  irrefutable COLON type_expr  { 
    let start = pattern_to_region $1 in 
    let stop = type_expr_to_region $3 in
    let region = cover start stop in
    {
      value = {
        pattern = $1; 
        colon = $2; 
        type_expr = $3
      };
      region
    }
  }

pattern:
  LBRACKET sub_pattern COMMA DOTDOTDOT sub_pattern RBRACKET { 
    let start = pattern_to_region $2 in
    let stop = pattern_to_region $5 in 
    let region = cover start stop in
    let val_ = {value = $2, $3, $5; region} in
    PList (PCons val_) 
  }
| tuple(sub_pattern) { 
    let h, t = $1 in    
    let start = pattern_to_region h in
    let stop = last (fun (region, _) -> region) t in
    let region = cover start stop in    
    PTuple { value = $1; region }
  }
| core_pattern                                            {        $1 }

sub_pattern:
  par(sub_pattern)                                              {    PPar $1 }
| core_pattern                                           {         $1 }

core_pattern:
  Ident                                                  {    PVar $1 }
| WILD                                                   {   PWild $1 }
| unit                                                   {   PUnit $1 }
| Int                                                    {    PInt $1 }
| True                                                   {   PTrue $1 }
| False                                                  {  PFalse $1 }
| Str                                                    { PString $1 }
| par(ptuple)                                            {    PPar $1 }
| list(sub_pattern)                                      { PList (PListComp $1) } 
| constr_pattern                                         { PConstr $1 }
| record_pattern                                         { PRecord $1 }

record_pattern:
  LBRACE sep_or_term_list(field_pattern,COMMA) RBRACE {
    let ne_elements, terminator = $2 in
    let region = cover $1 $3 in
    let value = {
      compound = Braces ($1,$3);
      ne_elements;
      terminator;
      }
    in
    {region; value}  
  }

field_pattern:
  field_name EQ sub_pattern {
    let start = $1.region in
    let stop = pattern_to_region $3 in
    let region = cover start stop in
    { value = {field_name=$1; eq=$2; pattern=$3}; region }
  }

constr_pattern:
  C_None { PNone $1 }
| C_Some sub_pattern {
    let stop   = pattern_to_region $2 in
    let region = cover $1 stop
    and value  = $1, $2
    in PSomeApp {value; region}
  }
|  Constr {       
     PConstrApp { value = $1, None; region = $1.region }     }
|  Constr sub_pattern {  
    let region = cover $1.region (pattern_to_region $2) in
    PConstrApp { value = $1, Some $2; region } 
  }


ptuple:
  tuple(sub_pattern) {  
    let h, t = $1 in    
    let start = pattern_to_region h in
    let stop = last (fun (region, _) -> region) t in
    let region = cover start stop in    
    PTuple { value = $1; region } 
  }

unit:
  LPAR RPAR { 
    let the_unit = ghost, ghost in
    let region = cover $1 $2 in
    { value = the_unit; region }
  }

(* Expressions *)

interactive_expr:
  expr EOF                                                       { $1 }

expr:
  base_cond__open(expr)                                   {       $1 }
| switch_expr(base_cond)                                  { ECase $1 }


base_cond__open(x):
  base_expr(x)
| conditional(x)                                                 { $1 }

base_cond:
  base_cond__open(base_cond)                                     { $1 }

type_expr_simple_args:
  LPAR nsepseq(type_expr_simple, COMMA) RPAR { 
    $1, $2, $3
   } 

type_expr_simple: 
  core_expr_2 type_expr_simple_args? { 
    let args = $2 in
    let constr = match $1 with 
    | EVar i -> i
    | EProj {value = {struct_name; field_path; _}; region} -> 
        let path = 
          (Utils.nsepseq_foldl 
            (fun a e -> 
              match e with 
              | FieldName v -> a ^ "." ^ v.value
              | Component {value = c, _; _} -> a ^ "." ^ c
            ) 
            struct_name.value
            field_path
          ) 
        in
        {value = path; region }
    | EArith (Mutez {value = s, _; region })
    | EArith (Int {value = s, _; region })
    | EArith (Nat {value = s, _; region }) -> { value = s; region }
    | EString (StrLit {value = s; region}) -> { value = s; region }
    | ELogic (BoolExpr (True t)) -> { value = "true"; region = t }
    | ELogic (BoolExpr (False f)) -> { value = "false"; region = f }
    | _ -> failwith "Not supported"
    in
    match args with 
      Some (lpar, args, rpar) -> (
        let start = expr_to_region $1 in
        let stop = rpar in
        let region = cover start stop in        
        TApp {
          value = constr, {
            value = {
              inside = args; 
              lpar; 
              rpar
            }; 
          region}; 
        region}
      )
    | None -> TVar constr
  }
  | LPAR nsepseq(type_expr_simple, COMMA) RPAR {  
    TProd {value = $2; region = cover $1 $3}
  }
  | LPAR type_expr_simple ARROW type_expr_simple RPAR { 
    TFun {value = $2, $3, $4; region = cover $1 $5}
   }

type_annotation_simple:
  COLON type_expr_simple { $2 }

fun_expr:
  disj_expr_level es6_func {
    let arrow, body = $2 in
    let kwd_fun = Region.ghost in
    let start = expr_to_region $1 in
    let stop = expr_to_region body in 
    let region = cover start stop in
    let rec arg_to_pattern = (function
      | EVar val_ -> PVar val_  
      | EAnnot {value = (EVar v, typ); region} ->
        PTyped {value = {
          pattern = PVar v; 
          colon = Region.ghost;
          type_expr = typ;
        } ; region}
      | EPar {value = {inside; lpar; rpar}; region} -> 
        PPar {value = {inside = arg_to_pattern inside; lpar; rpar}; region}
      | EUnit u -> PUnit u
      | _ -> failwith "Not supported"
    )
    in 
    let fun_args_to_pattern = (function
      | EAnnot {value = (ETuple {value = fun_args; _}, _); _} -> (*  ((foo:x, bar) : type)  *)
        let bindings = List.map (fun arg -> arg_to_pattern (snd arg)) (snd fun_args) in
        (arg_to_pattern (fst fun_args), bindings)
      | EAnnot {value = (EPar {value = {inside = fun_arg ; _}; _}, _); _} -> (* ((foo:x, bar) : type) *)
        (arg_to_pattern fun_arg, [])
      | EPar {value = {inside = fun_arg; _ }; _} -> 
        (arg_to_pattern fun_arg, [])
      | EAnnot e -> (arg_to_pattern (EAnnot e), [])
      | ETuple {value = fun_args; _} -> 
        let bindings = List.map (fun arg -> arg_to_pattern (snd arg)) (snd fun_args) in
        (arg_to_pattern (fst fun_args), bindings)
      | EUnit e ->
        (arg_to_pattern (EUnit e), [])
      | _ -> failwith "Not supported"
      )
      in
    let binders = fun_args_to_pattern $1 in
    let f = {
      kwd_fun ;
      binders ;
      lhs_type = None;
      arrow ;
      body ;
    } in
    EFun { region; value=f }
  }

base_expr(right_expr):
 let_expr(right_expr)
| disj_expr_level                                        {        $1 }
| fun_expr                                               {        $1 }

conditional(right_expr):
    if_then_else(right_expr)
  | if_then(right_expr)                                    { ECond $1 }

parenthesized_expr:
    braces (expr)                                          {    $1.value.inside }
  | par (expr)                                             {    $1.value.inside }

if_then(right_expr):
  If parenthesized_expr LBRACE closed_if RBRACE {
    let the_unit = ghost, ghost in
    let ifnot    = EUnit {region=ghost; value=the_unit} in
    let region = cover $1 $5 in
    { 
      value = {
        kwd_if = $1; 
        test = $2;  
        kwd_then = $3; 
        ifso = $4;
        kwd_else = Region.ghost; 
        ifnot;
      };
      region
    }
  }

if_then_else(right_expr):
  If parenthesized_expr LBRACE closed_if SEMI  RBRACE Else LBRACE right_expr SEMI RBRACE {
    let region = cover $1 $11 in
    { 
      value = {
        kwd_if = $1; 
        test = $2;  
        kwd_then = $3; 
        ifso = $4;
        kwd_else = $6; 
        ifnot = $9
      };
      region
    }
  }

base_if_then_else__open(x):
  base_expr(x)                                             {       $1 }
| if_then_else(x)                                          { ECond $1 }

base_if_then_else:
  base_if_then_else__open(base_if_then_else)               {       $1 }

closed_if:
  base_if_then_else__open(closed_if)                       {       $1 }
| switch_expr(base_if_then_else)                           { ECase $1 }

switch_expr(right_expr):
  Switch switch_expr_ LBRACE cases(right_expr) RBRACE {
    let cases = $4 in
    let start = $1 in
    let stop = $5 in
    let region = cover start stop in
    { value = {
        kwd_match = $1; 
        expr = $2; 
        lead_vbar = None; 
        kwd_with  = Region.ghost;
        cases = {
          value = cases;
          region = nsepseq_to_region (fun {region; _} -> region) $4
        };
      }; 
      region 
    }
  }

switch_expr_: 
  | par(expr) {
    $1.value.inside
  }
  | core_expr_2 {
    $1
  }

cases(right_expr):
  nseq(case_clause(right_expr)) { 
    let (hd, tl) = $1 in
    hd, (List.map (fun f -> expr_to_region f.value.rhs, f) tl)
  }

case_clause(right_expr):
  VBAR pattern ARROW right_expr SEMI? {
    let region = cover (pattern_to_region $2) (expr_to_region $4) in
    {value =   
      {
        pattern = $2; 
        arrow = $3; 
        rhs=$4   
      };
      region
    }
  }

let_expr(right_expr):
  Let let_binding SEMI right_expr {
    let kwd_let = $1 in 
    let (binding: let_binding), _ = $2 in        
    let kwd_in = $3 in
    let body = $4 in
    let stop = expr_to_region $4 in    
    let region = cover $1 stop in
    let let_in = {kwd_let; binding; kwd_in; body}
    in ELetIn {region; value=let_in} }

disj_expr_level:
  disj_expr                                                    { ELogic (BoolExpr (Or $1)) }
| conj_expr_level                                                {                      $1 }
| par(tuple(disj_expr_level)) type_annotation_simple? {
    let region = $1.region in    
    let tuple = ETuple {value=$1.value.inside; region} in
    let region = match $2 with 
    | Some s -> cover $1.region (type_expr_to_region s)
    | None -> region
    in    
    match $2 with 
    | Some typ -> EAnnot({value = tuple, typ; region})
    | None -> tuple
  }

bin_op(arg1,op,arg2):
  arg1 op arg2                            { 
    let start  = expr_to_region $1 in
    let stop   = expr_to_region $3 in
    let region = cover start stop in
    { value = { arg1=$1; op=$2; arg2=$3}; region }
  }

disj_expr:
  bin_op(disj_expr_level, BOOL_OR, conj_expr_level)
| bin_op(disj_expr_level, Or,      conj_expr_level)         { $1 }

conj_expr_level:
  conj_expr                              { ELogic (BoolExpr (And $1)) }
| comp_expr_level                        {                         $1 }

conj_expr:
  bin_op(conj_expr_level, BOOL_AND, comp_expr_level)        { $1 }

comp_expr_level:
  lt_expr                              { ELogic (CompExpr (Lt $1))    }
| le_expr                              { ELogic (CompExpr (Leq $1))   }
| gt_expr                              { ELogic (CompExpr (Gt $1))    }
| ge_expr                              { ELogic (CompExpr (Geq $1))   }
| eq_expr                              { ELogic (CompExpr (Equal $1)) }
| ne_expr                              { ELogic (CompExpr (Neq $1))   }
| cat_expr_level                       {                           $1 }

lt_expr:
  bin_op(comp_expr_level, LT, cat_expr_level)  { $1 }

le_expr:
  bin_op(comp_expr_level, LE, cat_expr_level)  { $1 }

gt_expr:
  bin_op(comp_expr_level, GT, cat_expr_level)  { $1 }

ge_expr:
  bin_op(comp_expr_level, GE, cat_expr_level)  { $1 }

eq_expr:
  bin_op(comp_expr_level, EQEQ, cat_expr_level)  { $1 }

ne_expr:
  bin_op(comp_expr_level, NE, cat_expr_level) { $1 }

cat_expr_level:
  cat_expr                                        {  EString (Cat $1) }
| add_expr_level                                  {                $1 }

cat_expr:
  bin_op(add_expr_level, CAT, add_expr_level)              { $1 }

add_expr_level:
  plus_expr                                         { EArith (Add $1) }
| minus_expr                                        { EArith (Sub $1) }
| mult_expr_level                                   {              $1 }

plus_expr:
  bin_op(add_expr_level, PLUS, mult_expr_level)             { $1 }

minus_expr:
  bin_op(add_expr_level, MINUS, mult_expr_level)            { $1 }

mult_expr_level:
  times_expr                                      {  EArith (Mult $1) }
| div_expr                                        {   EArith (Div $1) }
| mod_expr                                        {   EArith (Mod $1) }
| unary_expr_level                                {                $1 }

times_expr:
  bin_op(mult_expr_level, TIMES, unary_expr_level)          { $1 }

div_expr:
  bin_op(mult_expr_level, SLASH, unary_expr_level)          { $1 }

mod_expr:
  bin_op(mult_expr_level, Mod, unary_expr_level)            { $1 }

unary_expr_level:
   MINUS call_expr_level {
    let start = $1 in
    let end_ = expr_to_region $2 in
    let region = cover start end_
    and value  = {op = $1; arg = $2} 
    in EArith (Neg {region; value})      
}
| NOT call_expr_level {
    let start = $1 in
    let end_ = expr_to_region $2 in
    let region = cover start end_
    and value  = {op = $1; arg = $2} in 
    ELogic (BoolExpr (Not ({region; value})))
}     
| call_expr_level {
    $1 
  }

call_expr_level: 
  call_expr_level_in type_annotation_simple? {
    let region = match $2 with 
    | Some s -> cover (expr_to_region $1) (type_expr_to_region s)
    | None -> expr_to_region $1
    in        
    match $2 with
    | Some t -> 
      EAnnot { value = $1, t; region }     
    | None -> $1
  }

call_expr_level_in:
  call_expr                                                     {  $1 }
| constr_expr                                                   {  $1 }
| core_expr                                                      { $1 }

constr_expr:
  C_None {
    EConstr (ENone $1)
  }
  | C_Some core_expr {
    let region = cover $1 (expr_to_region $2)
    in EConstr (ESomeApp {value = $1,$2; region})
  }  
  | Constr core_expr? { 
    let start = $1.region in
    let stop = match $2 with 
    | Some c -> expr_to_region c
    | None -> start 
    in
    let region = cover start stop in
    EConstr (EConstrApp { value = $1,$2; region})
  }

call_expr:
  core_expr LPAR nsepseq(expr, COMMA) RPAR {
    let start = expr_to_region $1 in
    let stop = $4 in
    let region = cover start stop in
    let hd, tl = $3 in
    let tl = (List.map (fun (_, a) -> a) tl) in
    ECall { value = $1, (hd, tl); region }
  }
  | core_expr unit {
    let start = expr_to_region $1 in
    let stop = $2.region in
    let region = cover start stop in
    ECall { value = $1, (EUnit $2, []); region }
  }

core_expr_2:
  Int                                               { EArith (Int $1) }
| Mtz                                             { EArith (Mutez $1) }
| Nat                                               { EArith (Nat $1) }
| Ident | module_field                                      { EVar $1 }
| projection                                               { EProj $1 }
| Str                                           { EString (StrLit $1) }
| unit                                                     { EUnit $1 }
| False                               {  ELogic (BoolExpr (False $1)) }
| True                                {   ELogic (BoolExpr (True $1)) }
| list(expr)                                   { EList (EListComp $1) }

list_or_spread:
  LBRACKET expr COMMA sep_or_term_list(expr, COMMA) RBRACKET {
    let (e, terminator) = $4 in
    let e = Utils.nsepseq_cons $2 $3 e in
    EList (EListComp ({ value =
      {
        compound = Brackets ($1,$5);
        elements   = Some e;
        terminator;
      };
      region = cover $1 $5
    }))
  }
  | LBRACKET expr COMMA DOTDOTDOT expr RBRACKET {
    let region = cover $1 $6 in
    EList (ECons {value={arg1=$2; op=$4; arg2=$5}; region})
  }
  | LBRACKET expr RBRACKET {
    EList (EListComp ({ value =
      {
        compound = Brackets ($1,$3);
        elements   = Some ($2, []);
        terminator = None;
      };
      region = cover $1 $3
    }))
  }
  | LBRACKET RBRACKET {
     let value = {
      compound   = Brackets ($1,$2);
      elements   = None;
      terminator = None} in
    let region = cover $1 $2
    in EList (EListComp ( {value; region}))
  }

core_expr:
  Int                                               { EArith (Int $1) }
| Mtz                                             { EArith (Mutez $1) }
| Nat                                               { EArith (Nat $1) }
| Ident | module_field                                      { EVar $1 }
| projection                                               { EProj $1 }
| Str                                           { EString (StrLit $1) }
| unit                                                     { EUnit $1 }
| False                               {  ELogic (BoolExpr (False $1)) }
| True                                {  ELogic (BoolExpr (True $1))  }
| list_or_spread                                                 { $1 }
| par(expr)                                                 { EPar $1 }
| sequence_or_record                                          {    $1 }

module_field:
  module_name DOT field_name { 
    let region = cover $1.region $3.region in
    { value = $1.value ^ "." ^ $3.value; region } 
  }

selection:
  | LBRACKET Int RBRACKET selection {
    let r, (h, t) = $4 in
    let result:((selection, dot) Utils.nsepseq) = (Component $2), (Region.ghost, h) :: t in
    r, result
  }
  | DOT field_name selection {
    let r, (h, t) = $3 in
    let result:((selection, dot) Utils.nsepseq) = (FieldName $2), ($1, h) :: t  in 
    r, result
  }
  | DOT field_name {
    $1, ((FieldName $2), [])
  }
  | LBRACKET Int RBRACKET {
    Region.ghost, ((Component $2), [])
  }

projection:
  struct_name selection {
    let start = $1.region in     
    let stop = nsepseq_to_region (function 
    | FieldName f -> f.region 
    | Component c -> c.region) (snd $2)
    in
    let region = cover start stop in
    { value = 
      {
        struct_name = $1; 
        selector = fst $2;
        field_path = snd $2
      };
      region
    }
  }
| module_name DOT field_name selection {
    let module_name = $1 in
    let field_name = $3 in
    let value = module_name.value ^ "." ^ field_name.value in
    let struct_name = {$1 with value} in
    let start = $1.region in     
    let stop = nsepseq_to_region (function 
    | FieldName f -> f.region 
    | Component c -> c.region) (snd $4)
    in
    let region = cover start stop in
    { value = 
      {
        struct_name; 
        selector = fst $4; 
        field_path = snd $4
      };
      region
    }  
  }

sequence_or_record_in:  
  expr SEMI sep_or_term_list(expr,SEMI) {
    let (e, _region) = $3 in
    let e = Utils.nsepseq_cons $1 $2 e in
    PaSequence { s_elts = e; s_terminator = None}
  }
| field_assignment COMMA sep_or_term_list(field_assignment,COMMA)  {  
    let (e, _region) = $3 in
    let e = Utils.nsepseq_cons $1 $2 e in
    PaRecord { r_elts = e; r_terminator = None}
  } 
  | expr SEMI? {
    PaSingleExpr $1
  }

sequence_or_record:
  LBRACE sequence_or_record_in RBRACE {
    let compound = Braces($1, $3) in
    let region = cover $1 $3 in
    match $2 with 
    | PaSequence s -> (
      let value: expr injection = {
        compound;
        elements = Some s.s_elts;
        terminator = s.s_terminator;
      }
      in
      ESeq {value; region}
    )
    | PaRecord r -> (
      let value: field_assign reg ne_injection = {
        compound;
        ne_elements = r.r_elts;
        terminator = r.r_terminator;
      }
      in 
      ERecord {value; region}
    )
    | PaSingleExpr e -> e    
  }

field_assignment:
  field_name {    
    { value = 
      {
        field_name = $1; 
        assignment = Region.ghost; 
        field_expr = EVar $1
      };
      region = $1.region
    }
  }
  | field_name COLON expr {
    let start = $1.region in 
    let stop = expr_to_region $3 in 
    let region = cover start stop in
    { value = 
      {
        field_name = $1; 
        assignment = $2; 
        field_expr = $3
      };
      region
    } 
  }
