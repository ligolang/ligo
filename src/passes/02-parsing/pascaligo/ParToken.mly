%{
module Token = Lexing_pascaligo.Token
%}

(* Tokens (mirroring thise defined in module Token) *)

  (* Literals *)

%token               <LexerLib.Directive.t> Directive "<directive>"
%token            <Token.lexeme Region.reg> String    "<string>"
%token            <Token.lexeme Region.reg> Verbatim  "<verbatim>"
%token  <(Token.lexeme * Hex.t) Region.reg> Bytes     "<bytes>"
%token    <(Token.lexeme * Z.t) Region.reg> Int       "<int>"
%token    <(Token.lexeme * Z.t) Region.reg> Nat       "<nat>"
%token    <(Token.lexeme * Z.t) Region.reg> Mutez     "<mutez>"
%token            <Token.lexeme Region.reg> Ident     "<ident>"
%token            <Token.lexeme Region.reg> UIdent    "<uident>"
%token                  <string Region.reg> Attr      "[@attr]"
%token <Token.lexeme Region.reg Region.reg> Lang      "[%lang"

  (* Symbols *)

%token <Region.t> SEMI        ";"
%token <Region.t> COMMA       ","
%token <Region.t> LPAR        "("
%token <Region.t> RPAR        ")"
%token <Region.t> LBRACE      "{"
%token <Region.t> RBRACE      "}"
%token <Region.t> LBRACKET    "["
%token <Region.t> RBRACKET    "]"
%token <Region.t> CONS        "#"
%token <Region.t> VBAR        "|"
%token <Region.t> ARROW       "->"
%token <Region.t> ASS         ":="
%token <Region.t> EQ          "="
%token <Region.t> COLON       ":"
%token <Region.t> LT          "<"
%token <Region.t> LE          "<="
%token <Region.t> GT          ">"
%token <Region.t> GE          ">="
%token <Region.t> NE          "=/="
%token <Region.t> PLUS        "+"
%token <Region.t> MINUS       "-"
%token <Region.t> SLASH       "/"
%token <Region.t> TIMES       "*"
%token <Region.t> DOT         "."
%token <Region.t> WILD        "_"
%token <Region.t> CARET       "^"

  (* Keywords *)

%token <Region.t> And         "and"
%token <Region.t> Begin       "begin"
%token <Region.t> BigMap      "big_map"
%token <Region.t> Block       "block"
%token <Region.t> Case        "case"
%token <Region.t> Const       "const"
%token <Region.t> Contains    "contains"
%token <Region.t> Else        "else"
%token <Region.t> End         "end"
%token <Region.t> For         "for"
%token <Region.t> Function    "function"
%token <Region.t> Recursive   "recursive"
%token <Region.t> From        "from"
%token <Region.t> If          "if"
%token <Region.t> In          "in"
%token <Region.t> Is          "is"
%token <Region.t> List        "list"
%token <Region.t> Map         "map"
%token <Region.t> Mod         "mod"
%token <Region.t> Nil         "nil"
%token <Region.t> Not         "not"
%token <Region.t> Of          "of"
%token <Region.t> Or          "or"
%token <Region.t> Patch       "patch"
%token <Region.t> Record      "record"
%token <Region.t> Remove      "remove"
%token <Region.t> Set         "set"
%token <Region.t> Skip        "skip"
%token <Region.t> Step        "step"
%token <Region.t> Then        "then"
%token <Region.t> To          "to"
%token <Region.t> Type        "type"
%token <Region.t> Var         "var"
%token <Region.t> While       "while"
%token <Region.t> With        "with"
%token <Region.t> Module      "module"

  (* Virtual tokens *)

%token <Region.t> EOF

%%
