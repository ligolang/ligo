%{
    module Location = Ligo_helpers.Location
    open Main.Untyped
    open Value
%}

%token EOF
%token <int> INT
//%token <float> FLOAT
%token <string> STRING
%token <string> VAR_NAME
%token <string> FUNCTION_NAME
%token <string> TYPE_NAME
//%token PLUS MINUS TIMES DIV
%token COLON SEMICOLON /* DOUBLE_SEMICOLON */ COMMA AT EQUAL DOT
%token OR AND
%token LPAREN RPAREN
%token LBRACKET RBRACKET
%token IF ELSEIF ELSE // THEN
%token FOREACH OF WHILE
%token LET TYPE FUNCTION


// toto.tata @ 3 + 4 = 2 ; printf (lel)
//%left COLON
%left COMMA
%left AT
%left OR
%left AND
//%left EQUAL
//%left PLUS MINUS        /* lowest precedence */
//%left TIMES DIV         /* medium precedence */
%left DOT

%start <Main.Untyped.Value.program> main

%%

main:
  | sts = assignment+ EOF
    {
      let loc = Location.make $startpos $endpos in
      program ~loc sts
    }

assignment:
  | LET v = VAR_NAME EQUAL e = expr SEMICOLON
    {
      let loc = Location.make $startpos $endpos in
      let_ ~loc (v, e)
    }
  | FUNCTION f = VAR_NAME COLON t = type_expr EQUAL b = block SEMICOLON
    {
      let loc = Location.make $startpos $endpos in
      fun_ ~loc (f, t, b)
    }
  | TYPE n = TYPE_NAME EQUAL t = type_expr SEMICOLON
    {
      let loc = Location.make $startpos $endpos in
      type_ ~loc (n, t)
    }

statement:
  | ass = assignment
    {
      let loc = Location.make $startpos $endpos in
      assignment ~loc ass
    }
  | FOREACH var = VAR_NAME OF iterator = expr body = block
    {
      let loc = Location.make $startpos $endpos in
      foreach ~loc (var, iterator, body)
    }
  | WHILE cond = expr body = block
    {
      let loc = Location.make $startpos $endpos in
      while_ ~loc (cond, body)
    }
  | IF e = expr b = block eis = else_if* eo = else_?
    {
      let loc = Location.make $startpos $endpos in
      if_ ~loc (e, b, eis, eo)
    }

else_if:
  | ELSEIF LPAREN cond = expr RPAREN body = block
    {
      elseif (cond, body)
    }

else_:
  | ELSE body = block
    {
      else_ body      
    }

block:
  |  LBRACKET sts = statement+ RBRACKET
    {
      let loc = Location.make $startpos $endpos in
      block ~loc sts
    }

expr:
  | i = INT
    {
      let loc = Location.make $startpos $endpos in
      Value.int ~loc i
    }
(*
  | f = FLOAT
    {
      let loc = Location.make $startpos $endpos in
      make ~loc @@ literal @@ Float f
    }
  | s = STRING
    {
      let loc = Location.make $startpos $endpos in
      make ~loc @@ literal @@ String s
    }
*)
  | v = VAR_NAME
    {
      let loc = Location.make $startpos $endpos in
      variable ~loc v
    }
  | LPAREN e = expr RPAREN
    {
      let loc = Location.make $startpos $endpos in
      WrapLocation.update_location ~loc e
    }
  | e1 = expr COMMA e2 = expr
    {
      let loc = Location.make $startpos $endpos in
      Value.pair ~loc (e1, e2)
    }
  | e1 = expr AT e2 = expr
    {
      let loc = Location.make $startpos $endpos in
      application ~loc (e1, e2)
    }
  | e1 = expr DOT e2 = expr
    {
      let loc = Location.make $startpos $endpos in
      application ~loc (e2, e1)
    }
(*
  | e = expr COLON t = type_expr
    {
      let loc = Location.make $startpos $endpos in
      make ~loc @@ cast e t
    }
  | e1 = expr PLUS e2 = expr
    {
      let loc = Location.make $startpos $endpos in
      make ~loc @@ primitive Plus [e1 ; e2]
    }
  | e1 = expr MINUS e2 = expr
    {
      let loc = Location.make $startpos $endpos in
      make ~loc @@ primitive Minus [e1 ; e2]
    }
  | e1 = expr TIMES e2 = expr
    {
      let loc = Location.make $startpos $endpos in
      make ~loc @@ primitive Times [e1 ; e2]
    }
  | e1 = expr DIV e2 = expr
    {
      let loc = Location.make $startpos $endpos in
      make ~loc @@ primitive Div [e1 ; e2]
    }
  | e1 = expr EQUAL e2 = expr
    {
      let loc = Location.make $startpos $endpos in
      make ~loc @@ primitive Equal [e1 ; e2]
    }
  | e = expr DOT v = VAR_NAME
    {
      let loc = Location.make $startpos $endpos in
      make ~loc @@ dot e v
    }
*)

type_expr:
  | t = TYPE_NAME
    {
      let loc = Location.make $startpos $endpos in
      Type.(name ~loc t)
    }
  | t1 = type_expr AND t2 = type_expr
    {
      let loc = Location.make $startpos $endpos in
      Type.(pair ~loc t1 t2)
    }
  | t1 = type_expr OR t2 = type_expr
    {
      let loc = Location.make $startpos $endpos in
      Type.(union ~loc t1 t2)
    }
