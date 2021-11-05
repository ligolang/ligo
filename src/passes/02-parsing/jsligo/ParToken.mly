%{
module Token = Lexing_jsligo.Token
%}

(* Tokens (mirroring those defined in module Token) *)

  (* Literals *)

%token               <LexerLib.Directive.t> Directive "<directive>"
%token                  <string Token.wrap> BlockCom "<block_comment>"
%token                  <string Token.wrap> LineCom  "<line_comment>"
%token                  <string Token.wrap> String   "<string>"
%token                  <string Token.wrap> Verbatim "<verbatim>"
%token  <(Token.lexeme * Hex.t) Token.wrap> Bytes    "<bytes>"
%token          <(string * Z.t) Token.wrap> Int      "<int>"
(* %token          <(string * Z.t) Token.wrap> Nat      "<nat>"*)
(* %token          <(string * Z.t) Token.wrap> Mutez    "<mutez>"*)
%token                  <string Token.wrap> Ident    "<ident>"
%token                  <string Token.wrap> UIdent   "<uident>"
%token                  <string Token.wrap> Attr     "[@attr]"
// %token <Token.lexeme Region.reg Token.wrap> Lang     "[%lang"

  (* Symbols *)

%token <Token.lexeme Token.wrap> MINUS   "-"
%token <Token.lexeme Token.wrap> PLUS    "+"
%token <Token.lexeme Token.wrap> SLASH   "/"
%token <Token.lexeme Token.wrap> TIMES   "*"
%token <Token.lexeme Token.wrap> REM     "%"
(* %token <Token.lexeme Token.wrap> PLUS2   "++"*)
(* %token <Token.lexeme Token.wrap> MINUS2  "--"*)

%token <Token.lexeme Token.wrap> LPAR     "("
%token <Token.lexeme Token.wrap> RPAR     ")"
%token <Token.lexeme Token.wrap> LBRACKET "["
%token <Token.lexeme Token.wrap> RBRACKET "]"
%token <Token.lexeme Token.wrap> LBRACE   "{"
%token <Token.lexeme Token.wrap> RBRACE   "}"

%token <Token.lexeme Token.wrap> COMMA     ","
%token <Token.lexeme Token.wrap> SEMI      ";"
%token <Token.lexeme Token.wrap> COLON     ":"
%token <Token.lexeme Token.wrap> DOT       "."
%token <Token.lexeme Token.wrap> ELLIPSIS  "..."

%token <Token.lexeme Token.wrap> BOOL_OR  "||"
%token <Token.lexeme Token.wrap> BOOL_AND "&&"
%token <Token.lexeme Token.wrap> BOOL_NOT "!"

// %token <Token.lexeme Token.wrap> BIT_AND  "&"
// %token <Token.lexeme Token.wrap> BIT_NOT  "~"
// %token <Token.lexeme Token.wrap> BIT_XOR  "^"
// %token <Token.lexeme Token.wrap> SHIFT_L  "<<<"
// %token <Token.lexeme Token.wrap> SHIFT_R  ">>>"

%token <Token.lexeme Token.wrap> EQ    "="
%token <Token.lexeme Token.wrap> EQ2   "=="
%token <Token.lexeme Token.wrap> NE    "!="

%token <Token.lexeme Token.wrap> LT    "<"
%token <Token.lexeme Token.wrap> GT    ">"
%token <Token.lexeme Token.wrap> LE    "<="
%token <Token.lexeme Token.wrap> GE    ">="

%token <Token.lexeme Token.wrap> PLUS_EQ  "+="
%token <Token.lexeme Token.wrap> MINUS_EQ "-="
%token <Token.lexeme Token.wrap> MULT_EQ  "*="
%token <Token.lexeme Token.wrap> REM_EQ   "%="
%token <Token.lexeme Token.wrap> DIV_EQ   "/="
// %token <Token.lexeme Token.wrap> SL_EQ    "<<<="
// %token <Token.lexeme Token.wrap> SR_EQ    ">>>="
// %token <Token.lexeme Token.wrap> AND_EQ   "&="
// %token <Token.lexeme Token.wrap> OR_EQ    "|="
// %token <Token.lexeme Token.wrap> XOR_EQ   "^="

%token <Token.lexeme Token.wrap> VBAR   "|"
%token <Token.lexeme Token.wrap> ARROW  "=>"
%token <Token.lexeme Token.wrap> WILD   "_"


(* JavaScript Keywords *)

%token <Token.lexeme Token.wrap> Case     "case"
%token <Token.lexeme Token.wrap> Const    "const"
%token <Token.lexeme Token.wrap> Default  "default"
%token <Token.lexeme Token.wrap> Else     "else"
%token <Token.lexeme Token.wrap> Export   "export"
%token <Token.lexeme Token.wrap> For      "for"
%token <Token.lexeme Token.wrap> If       "if"
%token <Token.lexeme Token.wrap> Import   "import"
%token <Token.lexeme Token.wrap> Let      "let"
%token <Token.lexeme Token.wrap> Of       "of"
%token <Token.lexeme Token.wrap> Return   "return"
%token <Token.lexeme Token.wrap> Break    "break"
%token <Token.lexeme Token.wrap> Switch   "switch"
%token <Token.lexeme Token.wrap> While    "while"

(* TypeScript keywords *)

%token <Token.lexeme Token.wrap> As        "as"
%token <Token.lexeme Token.wrap> Namespace "namespace"
%token <Token.lexeme Token.wrap> Type      "type"

(* Virtual tokens *)

%token <Token.lexeme Token.wrap> ZWSP

(* End of File *)

%token <Token.lexeme Token.wrap> EOF

%%
