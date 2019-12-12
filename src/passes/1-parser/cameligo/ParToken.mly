%{
%}

(* Tokens (mirroring thise defined in module LexToken) *)

  (* Literals *)

%token                    <string Region.reg> String
%token <(LexToken.lexeme * Hex.t) Region.reg> Bytes
%token            <(string * Z.t) Region.reg> Int
%token            <(string * Z.t) Region.reg> Nat
%token            <(string * Z.t) Region.reg> Mutez
%token                    <string Region.reg> Ident
%token                    <string Region.reg> Constr

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

%token <Region.t> ARROW
%token <Region.t> CONS
%token <Region.t> CAT
(*%token APPEND*)
%token <Region.t> DOT

%token <Region.t> COMMA
%token <Region.t> SEMI
%token <Region.t> COLON
%token <Region.t> VBAR

%token <Region.t> WILD

%token <Region.t> EQ
%token <Region.t> NE
%token <Region.t> LT
%token <Region.t> GT
%token <Region.t> LE
%token <Region.t> GE

%token <Region.t> BOOL_OR
%token <Region.t> BOOL_AND

 (* Keywords *)

(*%token And*)
%token <Region.t> Begin
%token <Region.t> Else
%token <Region.t> End
%token <Region.t> False
%token <Region.t> Fun
%token <Region.t> If
%token <Region.t> In
%token <Region.t> Let
%token <Region.t> Match
%token <Region.t> Mod
%token <Region.t> Not
%token <Region.t> Of
%token <Region.t> Or
%token <Region.t> Then
%token <Region.t> True
%token <Region.t> Type
%token <Region.t> With

  (* Data constructors *)

%token <Region.t> C_None      (* "None" *)
%token <Region.t> C_Some      (* "Some" *)

  (* Virtual tokens *)

%token <Region.t> EOF

%%
