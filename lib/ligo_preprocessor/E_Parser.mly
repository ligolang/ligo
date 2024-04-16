(* Grammar for boolean expressions in preprocessing directives #if and
   #elif *)

%{
(* START OF HEADER *)

(* Vendors dependencies *)

open Simple_utils.Region

(* END OF HEADER *)
%}

(* Tokens *)

%token <State.t * string Simple_utils.Region.reg> Ident "<ident>"
%token <State.t * string Simple_utils.Region.reg> True  "true"
%token <State.t * string Simple_utils.Region.reg> False "false"
%token <State.t * string Simple_utils.Region.reg> OR    "||"
%token <State.t * string Simple_utils.Region.reg> AND   "&&"
%token <State.t * string Simple_utils.Region.reg> EQ    "=="
%token <State.t * string Simple_utils.Region.reg> NEQ   "!="
%token <State.t * string Simple_utils.Region.reg> NOT   "!"
%token <State.t * string Simple_utils.Region.reg> LPAR  "("
%token <State.t * string Simple_utils.Region.reg> RPAR  ")"
%token <State.t * string Simple_utils.Region.reg> COMMENT
%token <State.t * string Simple_utils.Region.reg> EOL (* End of line *)
%token <State.t * string Simple_utils.Region.reg> EOF (* End of file *)

(* Entries *)

%start expr
%type <  State.t
       * E_AST.t
       * string Simple_utils.Region.reg option
       * [ `EOL of State.t * string Simple_utils.Region.reg
         | `EOF of State.t * Simple_utils.Region.t]> expr

%%

(* Grammar *)

expr:
  or_expr_level COMMENT? ending {
    let state, tree = $1 in
    match $2 with
      None                  -> state, tree, None,         $3
    | Some (state, comment) -> state, tree, Some comment, $3 }

ending:
  EOL { `EOL $1 }
| EOF { let state, lexeme = $1
        in `EOF (state, lexeme.Simple_utils.Region.region) }

or_expr_level:
  or_expr_level "||" and_expr_level {
    let     _, node1 = $1
    and state, node2 = $3 in
    let start        = E_AST.to_region node1
    and stop         = E_AST.to_region node2 in
    let region       = cover start stop
    in state, Or {value = (node1, node2); region}
  }
| and_expr_level { $1 }

and_expr_level:
  and_expr_level "&&" unary_expr_level {
    let     _, node1 = $1
    and state, node2 = $3 in
    let start        = E_AST.to_region node1
    and stop         = E_AST.to_region node2 in
    let region       = cover start stop
    in state, And {value = (node1, node2); region}
  }
| eq_expr_level { $1 }

eq_expr_level:
  eq_expr_level "==" unary_expr_level  {
    let     _, node1 = $1
    and state, node2 = $3 in
    let start        = E_AST.to_region node1
    and stop         = E_AST.to_region node2 in
    let region       = cover start stop
    in state, Eq {value = (node1, node2); region}
  }
| eq_expr_level "!=" unary_expr_level  {
    let     _, node1 = $1
    and state, node2 = $3 in
    let start        = E_AST.to_region node1
    and stop         = E_AST.to_region node2 in
    let region       = cover start stop
    in state, Neq {value = (node1, node2); region}
  }
| unary_expr_level { $1 }

unary_expr_level:
  "!" unary_expr_level {
    let state, node = $2 in
    let start       = (snd $1).region
    and stop        = E_AST.to_region node in
    let region      = cover start stop
    in state, Not {value=node; region}
  }
| core_expr { $1 }

core_expr:
  "true"    { let state, reg = $1 in state, True  reg.region }
| "false"   { let state, reg = $1 in state, False reg.region }
| "<ident>" { let state, reg = $1 in state, Ident reg }
| "(" or_expr_level ")" {
    let start  = (snd $1).region
    and stop   = (snd $3).region in
    let region = cover start stop
    in fst $3, Parens {value = snd $2; region} }
