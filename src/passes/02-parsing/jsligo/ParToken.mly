%{
module Token = Lexing_jsligo.Token
%}

(* Tokens (mirroring those defined in module Token) *)

  (* Literals *)

%token               <LexerLib.Directive.t> Directive "<directive>"
%token                  <string Region.reg> BlockCom "<block_comment>"
%token                  <string Region.reg> LineCom  "<line_comment>"
%token                  <string Region.reg> String   "<string>"
%token                  <string Region.reg> Verbatim "<verbatim>"
%token  <(Token.lexeme * Hex.t) Region.reg> Bytes    "<bytes>"
%token          <(string * Z.t) Region.reg> Int      "<int>"
(* %token          <(string * Z.t) Region.reg> Nat      "<nat>"*)
(* %token          <(string * Z.t) Region.reg> Mutez    "<mutez>"*)
%token                  <string Region.reg> Ident    "<ident>"
%token                  <string Region.reg> UIdent   "<uident>"
%token                  <string Region.reg> Attr     "[@attr]"
// %token <Token.lexeme Region.reg Region.reg> Lang     "[%lang"

  (* Symbols *)

%token <Region.t> MINUS   "-"
%token <Region.t> PLUS    "+"
%token <Region.t> SLASH   "/"
%token <Region.t> TIMES   "*"
%token <Region.t> REM     "%"
(* %token <Region.t> PLUS2   "++"*)
(* %token <Region.t> MINUS2  "--"*)

%token <Region.t> LPAR     "("
%token <Region.t> RPAR     ")"
%token <Region.t> LBRACKET "["
%token <Region.t> RBRACKET "]"
%token <Region.t> LBRACE   "{"
%token <Region.t> RBRACE   "}"

%token <Region.t> COMMA     ","
%token <Region.t> SEMI      ";"
%token <Region.t> COLON     ":"
%token <Region.t> DOT       "."
%token <Region.t> ELLIPSIS  "..."

%token <Region.t> BOOL_OR  "||"
%token <Region.t> BOOL_AND "&&"
%token <Region.t> BOOL_NOT "!"

// %token <Region.t> BIT_AND  "&"
// %token <Region.t> BIT_NOT  "~"
// %token <Region.t> BIT_XOR  "^"
// %token <Region.t> SHIFT_L  "<<<"
// %token <Region.t> SHIFT_R  ">>>"

%token <Region.t> EQ    "="
%token <Region.t> EQ2   "=="
%token <Region.t> NE    "!="

%token <Region.t> LT    "<"
%token <Region.t> GT    ">"
%token <Region.t> LE    "<="
%token <Region.t> GE    ">="

// %token <Region.t> PLUS_EQ  "+="
// %token <Region.t> MINUS_EQ "-="
// %token <Region.t> MULT_EQ  "*="
// %token <Region.t> REM_EQ   "%="
// %token <Region.t> DIV_EQ   "/="
// %token <Region.t> SL_EQ    "<<<="
// %token <Region.t> SR_EQ    ">>>="
// %token <Region.t> AND_EQ   "&="
// %token <Region.t> OR_EQ    "|="
// %token <Region.t> XOR_EQ   "^="

%token <Region.t> VBAR   "|"
%token <Region.t> ARROW  "=>"
%token <Region.t> WILD   "_"


(* JavaScript Keywords *)

(* %token <Region.t> Break    "break"*)
%token <Region.t> Case     "case"
(* %token <Region.t> Class    "class"*)
%token <Region.t> Const    "const"
%token <Region.t> Default  "default"
%token <Region.t> Else     "else"
%token <Region.t> Export   "export"
%token <Region.t> For      "for"
%token <Region.t> If       "if"
%token <Region.t> Import   "import"
%token <Region.t> Let      "let"
%token <Region.t> Of       "of"
%token <Region.t> Return   "return"
%token <Region.t> Switch   "switch"
(* %token <Region.t> This     "this"*)
(* %token <Region.t> Void     "void"*)
%token <Region.t> While    "while"
(* %token <Region.t> With     "with"*)


(* TypeScript keywords *)

%token <Region.t> As          "as"
%token <Region.t> Namespace   "namespace"
%token <Region.t> Type        "type"

(* Virtual tokens *)

%token <Region.t> ZWSP

(* End of File *)

%token <Region.t> EOF

%%
