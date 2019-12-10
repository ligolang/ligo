%{
%}

(* Tokens (mirroring thise defined in module LexToken) *)

  (* Literals *)

%token <string Region.reg> Ident
%token <string Region.reg> Constr
%token <string Region.reg> Str
%token <(string * Z.t) Region.reg> Int
%token <(string * Z.t) Region.reg> Nat
%token <(string * Z.t) Region.reg> Mtz

  (* Symbols *)

%token <Region.t> MINUS
%token <Region.t> PLUS
%token <Region.t> SLASH
%token <Region.t> TIMES

%token <Region.t> LPAR
%token <Region.t> RPAR
%token <Region.t> LBRACKET
%token <Region.t> RBRACKET
%token <Region.t> LBRACE
%token <Region.t> RBRACE

%token <Region.t> CAT
%token <Region.t> DOT
%token <Region.t> DOTDOTDOT

%token <Region.t> COMMA
%token <Region.t> SEMI
%token <Region.t> COLON
%token <Region.t> VBAR

%token <Region.t> WILD

%token <Region.t> EQ
%token <Region.t> EQEQ
%token <Region.t> NE
%token <Region.t> LT
%token <Region.t> GT
%token <Region.t> LE
%token <Region.t> GE
%token <Region.t> ARROW

%token <Region.t> NOT

%token <Region.t> BOOL_OR
%token <Region.t> BOOL_AND


  (* Keywords *)

%token <Region.t> Else
%token <Region.t> False
%token <Region.t> If
%token <Region.t> Let
%token <Region.t> Switch
%token <Region.t> Mod
%token <Region.t> Or
%token <Region.t> True
%token <Region.t> Type

  (* Data constructors *)

%token <Region.t> C_None      (* "None" *)
%token <Region.t> C_Some      (* "Some" *)

  (* Virtual tokens *)
  
%token <Region.t> EOF

%%
