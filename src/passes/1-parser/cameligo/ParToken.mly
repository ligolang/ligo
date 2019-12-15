%{
%}

(* Tokens (mirroring thise defined in module LexToken) *)

  (* Literals *)

%token                    <string Region.reg> String "<string>"
%token <(LexToken.lexeme * Hex.t) Region.reg> Bytes  "<bytes>"
%token            <(string * Z.t) Region.reg> Int    "<int>"
%token            <(string * Z.t) Region.reg> Nat    "<nat>"
%token            <(string * Z.t) Region.reg> Mutez  "<mutez>"
%token                    <string Region.reg> Ident  "<ident>"
%token                    <string Region.reg> Constr "<constr>"

  (* Symbols *)

%token <Region.t> MINUS "-"
%token <Region.t> PLUS  "+"
%token <Region.t> SLASH "/"
%token <Region.t> TIMES "*"

%token <Region.t> LPAR     "("
%token <Region.t> RPAR     ")"
%token <Region.t> LBRACKET "["
%token <Region.t> RBRACKET "]"
%token <Region.t> LBRACE   "{"
%token <Region.t> RBRACE   "}"

%token <Region.t> ARROW "->"
%token <Region.t> CONS  "::"
%token <Region.t> CAT   "^"
(*%token <Region.t> APPEND "@" *)
%token <Region.t> DOT   "."

%token <Region.t> COMMA ","
%token <Region.t> SEMI  ";"
%token <Region.t> COLON ":"
%token <Region.t> VBAR  "|"

%token <Region.t> WILD  "_"

%token <Region.t> EQ "="
%token <Region.t> NE "<>"
%token <Region.t> LT "<"
%token <Region.t> GT ">"
%token <Region.t> LE "<="
%token <Region.t> GE ">="

%token <Region.t> BOOL_OR  "||"
%token <Region.t> BOOL_AND "&&"

 (* Keywords *)

(*%token And*)
%token <Region.t> Begin "begin"
%token <Region.t> Else  "else"
%token <Region.t> End   "end"
%token <Region.t> False "false"
%token <Region.t> Fun   "fun"
%token <Region.t> If    "if"
%token <Region.t> In    "in"
%token <Region.t> Let   "let"
%token <Region.t> Match "match"
%token <Region.t> Mod   "mod"
%token <Region.t> Not   "not"
%token <Region.t> Of    "of"
%token <Region.t> Or    "or"
%token <Region.t> Then  "then"
%token <Region.t> True  "true"
%token <Region.t> Type  "type"
%token <Region.t> With  "with"

  (* Data constructors *)

%token <Region.t> C_None "None"
%token <Region.t> C_Some "Some"

  (* Virtual tokens *)

%token <Region.t> EOF

%%
