%{
module Token = Lexing_reasonligo.Token
%}

(* Tokens (mirroring those defined in module Token) *)

  (* Literals *)

%token               <LexerLib.Directive.t> Directive "<directive>"
%token                  <string Token.wrap> String    "<string>"
%token                  <string Token.wrap> Verbatim  "<verbatim>"
%token  <(Token.lexeme * Hex.t) Token.wrap> Bytes     "<bytes>"
%token          <(string * Z.t) Token.wrap> Int       "<int>"
%token          <(string * Z.t) Token.wrap> Nat       "<nat>"
%token          <(string * Z.t) Token.wrap> Mutez     "<mutez>"
%token                  <string Token.wrap> Ident     "<ident>"
%token                  <string Token.wrap> UIdent    "<uident>"
%token                  <string Token.wrap> Attr      "[@attr]"
%token <Token.lexeme Region.reg Region.reg> Lang      "[%lang"

  (* Symbols *)

%token <Token.lexeme Token.wrap> MINUS   "-"
%token <Token.lexeme Token.wrap> PLUS    "+"
%token <Token.lexeme Token.wrap> SLASH   "/"
%token <Token.lexeme Token.wrap> TIMES   "*"

%token <Token.lexeme Token.wrap> LPAR     "("
%token <Token.lexeme Token.wrap> RPAR     ")"
%token <Token.lexeme Token.wrap> LBRACKET "["
%token <Token.lexeme Token.wrap> RBRACKET "]"
%token <Token.lexeme Token.wrap> LBRACE   "{"
%token <Token.lexeme Token.wrap> RBRACE   "}"

%token <Token.lexeme Token.wrap> PLUS2     "++"
%token <Token.lexeme Token.wrap> DOT       "."
%token <Token.lexeme Token.wrap> ELLIPSIS  "..."

%token <Token.lexeme Token.wrap> COMMA ","
%token <Token.lexeme Token.wrap> SEMI  ";"
%token <Token.lexeme Token.wrap> COLON ":"
%token <Token.lexeme Token.wrap> VBAR  "|"

%token <Token.lexeme Token.wrap> WILD "_"

%token <Token.lexeme Token.wrap> EQ    "="
%token <Token.lexeme Token.wrap> EQ2   "=="
%token <Token.lexeme Token.wrap> NE    "!="
%token <Token.lexeme Token.wrap> LT    "<"
%token <Token.lexeme Token.wrap> GT    ">"
%token <Token.lexeme Token.wrap> LE    "<="
%token <Token.lexeme Token.wrap> GE    ">="
%token <Token.lexeme Token.wrap> ARROW "=>"

%token <Token.lexeme Token.wrap> NOT      "!"
%token <Token.lexeme Token.wrap> BOOL_OR  "||"
%token <Token.lexeme Token.wrap> BOOL_AND "&&"
%token <Token.lexeme Token.wrap> QUOTE    "'"

  (* Keywords *)

%token <Token.lexeme Token.wrap> Else   "else"
%token <Token.lexeme Token.wrap> If     "if"
%token <Token.lexeme Token.wrap> Let    "let"
%token <Token.lexeme Token.wrap> Rec    "rec"
%token <Token.lexeme Token.wrap> Switch "switch"
%token <Token.lexeme Token.wrap> Mod    "mod"
%token <Token.lexeme Token.wrap> Land   "land"
%token <Token.lexeme Token.wrap> Lor    "lor"
%token <Token.lexeme Token.wrap> Lxor   "lxor"
%token <Token.lexeme Token.wrap> Lsl    "lsl"
%token <Token.lexeme Token.wrap> Lsr    "lsr"
%token <Token.lexeme Token.wrap> Or     "or"
%token <Token.lexeme Token.wrap> Type   "type"
%token <Token.lexeme Token.wrap> Module "module"

  (* Virtual tokens *)

%token <Token.lexeme Token.wrap> EOF
%token <Token.lexeme Token.wrap> ES6FUN

%%
