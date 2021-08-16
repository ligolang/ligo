%{
module Token = Lexing_cameligo.Token
%}

(* Tokens (mirroring thise defined in module Token) *)

  (* Literals *)

%token               <LexerLib.Directive.t> Directive "<directive>"
%token                  <string Region.reg> String    "<string>"
%token                  <string Region.reg> Verbatim  "<verbatim>"
%token  <(Token.lexeme * Hex.t) Region.reg> Bytes     "<bytes>"
%token          <(string * Z.t) Region.reg> Int       "<int>"
%token          <(string * Z.t) Region.reg> Nat       "<nat>"
%token          <(string * Z.t) Region.reg> Mutez     "<mutez>"
%token                  <string Region.reg> Ident     "<ident>"
%token                  <string Region.reg> UIdent    "<uident>"
%token                  <string Region.reg> Attr      "[@attr]"
%token <Token.lexeme Region.reg Region.reg> Lang      "[%lang"

  (* Symbols *)

%token <Region.t> MINUS   "-"
%token <Region.t> PLUS    "+"
%token <Region.t> SLASH   "/"
%token <Region.t> TIMES   "*"

%token <Region.t> LPAR     "("
%token <Region.t> RPAR     ")"
%token <Region.t> LBRACKET "["
%token <Region.t> RBRACKET "]"
%token <Region.t> LBRACE   "{"
%token <Region.t> RBRACE   "}"

%token <Region.t> ARROW "->"
%token <Region.t> CONS  "::"
%token <Region.t> CARET "^"
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
%token <Region.t> QUOTE    "'"

 (* Keywords *)

(*%token And*)
%token <Region.t> Begin  "begin"
%token <Region.t> Else   "else"
%token <Region.t> End    "end"
%token <Region.t> Fun    "fun"
%token <Region.t> Rec    "rec"
%token <Region.t> If     "if"
%token <Region.t> In     "in"
%token <Region.t> Let    "let"
%token <Region.t> Match  "match"
%token <Region.t> Mod    "mod"
%token <Region.t> Land   "land"
%token <Region.t> Lor    "lor"
%token <Region.t> Lxor   "lxor"
%token <Region.t> Lsl    "lsl"
%token <Region.t> Lsr    "lsr"
%token <Region.t> Not    "not"
%token <Region.t> Of     "of"
%token <Region.t> Or     "or"
%token <Region.t> Then   "then"
%token <Region.t> Type   "type"
%token <Region.t> With   "with"
%token <Region.t> Module "module"
%token <Region.t> Struct "struct"

  (* Virtual tokens *)

%token <Region.t> EOF

%%
