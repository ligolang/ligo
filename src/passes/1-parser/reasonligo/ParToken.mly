%{
%}

(* Tokens (mirroring those defined in module LexToken) *)

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

%token <Region.t> CAT       "++"
%token <Region.t> DOT       "."
%token <Region.t> ELLIPSIS  "..."

%token <Region.t> COMMA ","
%token <Region.t> SEMI  ";"
%token <Region.t> COLON ":"
%token <Region.t> VBAR  "|"

%token <Region.t> WILD "_"

%token <Region.t> EQ    "="
%token <Region.t> EQEQ  "=="
%token <Region.t> NE    "!="
%token <Region.t> LT    "<"
%token <Region.t> GT    ">"
%token <Region.t> LE    "<="
%token <Region.t> GE    ">="
%token <Region.t> ARROW "=>"

%token <Region.t> NOT      "!"
%token <Region.t> BOOL_OR  "||"
%token <Region.t> BOOL_AND "&&"


  (* Keywords *)

%token <Region.t> Else   "else"
%token <Region.t> False  "false"
%token <Region.t> If     "if"
%token <Region.t> Let    "let"
%token <Region.t> Switch "switch"
%token <Region.t> Mod    "mod"
%token <Region.t> Or     "or"
%token <Region.t> True   "true"
%token <Region.t> Type   "type"

  (* Data constructors *)

%token <Region.t> C_None "None"
%token <Region.t> C_Some "Some"

  (* Virtual tokens *)

%token <Region.t> EOF

%%
