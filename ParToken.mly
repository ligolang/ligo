%{
%}

(* Tokens (mirroring thise defined in module LexToken) *)

  (* Literals *)

%token <LexToken.lexeme Region.reg> String
%token <(LexToken.lexeme * MBytes.t) Region.reg> Bytes
%token <(LexToken.lexeme * Z.t) Region.reg> Int
%token <LexToken.lexeme Region.reg> Ident
%token <LexToken.lexeme Region.reg> Constr

  (* Symbols *)

%token <Region.t> SEMI
%token <Region.t> COMMA
%token <Region.t> LPAR
%token <Region.t> RPAR
%token <Region.t> LBRACE
%token <Region.t> RBRACE
%token <Region.t> LBRACKET
%token <Region.t> RBRACKET
%token <Region.t> CONS
%token <Region.t> VBAR
%token <Region.t> ARROW
%token <Region.t> ASGNMNT
%token <Region.t> EQUAL
%token <Region.t> COLON
%token <Region.t> OR
%token <Region.t> AND
%token <Region.t> LT
%token <Region.t> LEQ
%token <Region.t> GT
%token <Region.t> GEQ
%token <Region.t> NEQ
%token <Region.t> PLUS
%token <Region.t> MINUS
%token <Region.t> SLASH
%token <Region.t> TIMES
%token <Region.t> DOT
%token <Region.t> WILD
%token <Region.t> CAT

  (* Keywords *)

%token <Region.t> Begin
%token <Region.t> Const
%token <Region.t> Down
%token <Region.t> If
%token <Region.t> In
%token <Region.t> Is
%token <Region.t> For
%token <Region.t> Function
%token <Region.t> Parameter
%token <Region.t> Storage
%token <Region.t> Type
%token <Region.t> Of
%token <Region.t> Operations
%token <Region.t> Var
%token <Region.t> End
%token <Region.t> Then
%token <Region.t> Else
%token <Region.t> Match
%token <Region.t> Null
%token <Region.t> Procedure
%token <Region.t> Record
%token <Region.t> Step
%token <Region.t> To
%token <Region.t> Mod
%token <Region.t> Not
%token <Region.t> While
%token <Region.t> With

  (* Data constructors *)

%token <Region.t> C_False
%token <Region.t> C_None
%token <Region.t> C_Some
%token <Region.t> C_True
%token <Region.t> C_Unit

  (* Virtual tokens *)

%token <Region.t> EOF

%%
