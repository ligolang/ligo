%{
module Token = Lexing_pascaligo.Token
%}

(* Tokens (mirroring thise defined in module Token) *)

  (* Literals *)

%token               <LexerLib.Directive.t> Directive "<directive>"
%token            <Token.lexeme Token.wrap> String    "<string>"
%token            <Token.lexeme Token.wrap> Verbatim  "<verbatim>"
%token  <(Token.lexeme * Hex.t) Token.wrap> Bytes     "<bytes>"
%token    <(Token.lexeme * Z.t) Token.wrap> Int       "<int>"
%token    <(Token.lexeme * Z.t) Token.wrap> Nat       "<nat>"
%token    <(Token.lexeme * Z.t) Token.wrap> Mutez     "<mutez>"
%token            <Token.lexeme Token.wrap> Ident     "<ident>"
%token            <Token.lexeme Token.wrap> UIdent    "<uident>"
%token                  <string Token.wrap> Attr      "[@attr]"
%token <Token.lexeme Region.reg Region.reg> Lang      "[%lang"

  (* Symbols *)

%token <lexeme Token.wrap> SEMI        ";"
%token <lexeme Token.wrap> COMMA       ","
%token <lexeme Token.wrap> LPAR        "("
%token <lexeme Token.wrap> RPAR        ")"
%token <lexeme Token.wrap> LBRACE      "{"
%token <lexeme Token.wrap> RBRACE      "}"
%token <lexeme Token.wrap> LBRACKET    "["
%token <lexeme Token.wrap> RBRACKET    "]"
%token <lexeme Token.wrap> CONS        "#"
%token <lexeme Token.wrap> VBAR        "|"
%token <lexeme Token.wrap> ARROW       "->"
%token <lexeme Token.wrap> ASS         ":="
%token <lexeme Token.wrap> EQ          "="
%token <lexeme Token.wrap> COLON       ":"
%token <lexeme Token.wrap> LT          "<"
%token <lexeme Token.wrap> LE          "<="
%token <lexeme Token.wrap> GT          ">"
%token <lexeme Token.wrap> GE          ">="
%token <lexeme Token.wrap> NE          "=/="
%token <lexeme Token.wrap> PLUS        "+"
%token <lexeme Token.wrap> MINUS       "-"
%token <lexeme Token.wrap> SLASH       "/"
%token <lexeme Token.wrap> TIMES       "*"
%token <lexeme Token.wrap> DOT         "."
%token <lexeme Token.wrap> WILD        "_"
%token <lexeme Token.wrap> CARET       "^"

  (* Keywords *)

%token <lexeme Token.wrap> And         "and"
%token <lexeme Token.wrap> Begin       "begin"
%token <lexeme Token.wrap> BigMap      "big_map"
%token <lexeme Token.wrap> Block       "block"
%token <lexeme Token.wrap> Case        "case"
%token <lexeme Token.wrap> Const       "const"
%token <lexeme Token.wrap> Contains    "contains"
%token <lexeme Token.wrap> Else        "else"
%token <lexeme Token.wrap> End         "end"
%token <lexeme Token.wrap> For         "for"
%token <lexeme Token.wrap> Function    "function"
%token <lexeme Token.wrap> Recursive   "recursive"
%token <lexeme Token.wrap> From        "from"
%token <lexeme Token.wrap> If          "if"
%token <lexeme Token.wrap> In          "in"
%token <lexeme Token.wrap> Is          "is"
%token <lexeme Token.wrap> List        "list"
%token <lexeme Token.wrap> Map         "map"
%token <lexeme Token.wrap> Mod         "mod"
%token <lexeme Token.wrap> Nil         "nil"
%token <lexeme Token.wrap> Not         "not"
%token <lexeme Token.wrap> Of          "of"
%token <lexeme Token.wrap> Or          "or"
%token <lexeme Token.wrap> Patch       "patch"
%token <lexeme Token.wrap> Record      "record"
%token <lexeme Token.wrap> Remove      "remove"
%token <lexeme Token.wrap> Set         "set"
%token <lexeme Token.wrap> Skip        "skip"
%token <lexeme Token.wrap> Step        "step"
%token <lexeme Token.wrap> Then        "then"
%token <lexeme Token.wrap> To          "to"
%token <lexeme Token.wrap> Type        "type"
%token <lexeme Token.wrap> Var         "var"
%token <lexeme Token.wrap> While       "while"
%token <lexeme Token.wrap> With        "with"
%token <lexeme Token.wrap> Module      "module"

  (* Virtual tokens *)

%token <lexeme Token.wrap> EOF

%%
