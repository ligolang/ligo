(* Note: All external symbols here should be unqualified because this file is used
         by [menhir] that does not always insert the [%{..%}] header. So we work
         around it by the [-open Module] option in [dune] but symbols should be
         unqualified.

         Also, keep in mind that [ParToken.mly] and [Parser.mly] are merging into
         one file and the header of [Parser.mly] affects this code.
         For example: [lexeme] type comes from [open CST] *)

%[@recover.prelude
  open Lexing_shared.Wrap
  module Region = Simple_utils.Region

  let mk str loc = wrap str loc

  let mkDirective loc = LexerLib.Directive.Linemarker Region.{value = (0, "<invalid-path>", None);
                                                             region = loc}
  let mkLang loc = Region.{value = Region.{value = "<invalid-lang-literal>";
                                          region = loc};
                          region = loc}
  ]
(* Tokens (mirroring thise defined in module Token) *)

  (* Literals *)

%token               <LexerLib.Directive.t> Directive "<directive>" [@recover.expr mkDirective $loc]
%token                  <lexeme Wrap.wrap> String    "<string>"     [@recover.expr mk "<invalid-string-literal>" $loc]
%token                  <lexeme Wrap.wrap> Verbatim  "<verbatim>"   [@recover.expr mk "<invalid-verbatim-literal>" $loc]
%token        <(lexeme * Hex.t) Wrap.wrap> Bytes     "<bytes>"      [@recover.expr mk ("<invalid-bytes-literal>", `Hex "") $loc]
%token          <(lexeme * Z.t) Wrap.wrap> Int       "<int>"        [@recover.expr mk ("<invalid-int-literal>", Z.zero) $loc]
%token          <(lexeme * Z.t) Wrap.wrap> Nat       "<nat>"        [@recover.expr mk ("<invalid-nat-literal>", Z.zero) $loc]
%token          <(lexeme * Z.t) Wrap.wrap> Mutez     "<mutez>"      [@recover.expr mk ("<invalid-mutz-literal>", Z.zero) $loc]
%token                  <lexeme Wrap.wrap> Ident     "<ident>"      [@recover.expr mk "<invalid-ident>" $loc]
%token                  <lexeme Wrap.wrap> UIdent    "<uident>"     [@recover.expr mk "<invalid-uident>" $loc]
%token                  <string Wrap.wrap> Attr      "[@attr]"      [@recover.expr mk "<invalid-attr-literal>" $loc]
%token      <lexeme Region.reg Region.reg> Lang      "[%lang"       [@recover.expr mkLang $loc]

  (* Symbols *)

%token <lexeme Wrap.wrap> SEMI        ";"   [@recover.expr mk ";" $loc]
%token <lexeme Wrap.wrap> COMMA       ","   [@recover.expr mk "," $loc]
%token <lexeme Wrap.wrap> LPAR        "("   [@recover.expr mk "(" $loc]
%token <lexeme Wrap.wrap> RPAR        ")"   [@recover.expr mk ")" $loc]
%token <lexeme Wrap.wrap> LBRACE      "{"   [@recover.expr mk "{" $loc]
%token <lexeme Wrap.wrap> RBRACE      "}"   [@recover.expr mk "}" $loc]
%token <lexeme Wrap.wrap> LBRACKET    "["   [@recover.expr mk "[" $loc]
%token <lexeme Wrap.wrap> RBRACKET    "]"   [@recover.expr mk "]" $loc]
%token <lexeme Wrap.wrap> CONS        "#"   [@recover.expr mk "#" $loc]
%token <lexeme Wrap.wrap> VBAR        "|"   [@recover.expr mk "|" $loc]
%token <lexeme Wrap.wrap> ARROW       "->"  [@recover.expr mk "->" $loc]
%token <lexeme Wrap.wrap> ASS         ":="  [@recover.expr mk ":=" $loc]
%token <lexeme Wrap.wrap> EQ          "="   [@recover.expr mk "=" $loc]
%token <lexeme Wrap.wrap> COLON       ":"   [@recover.expr mk ":" $loc]
%token <lexeme Wrap.wrap> LT          "<"   [@recover.expr mk "<" $loc]
%token <lexeme Wrap.wrap> LE          "<="  [@recover.expr mk "<=" $loc]
%token <lexeme Wrap.wrap> GT          ">"   [@recover.expr mk ">" $loc]
%token <lexeme Wrap.wrap> GE          ">="  [@recover.expr mk ">=" $loc]
%token <lexeme Wrap.wrap> NE          "=/=" [@recover.expr mk "=/=" $loc]
%token <lexeme Wrap.wrap> PLUS        "+"   [@recover.expr mk "+" $loc]
%token <lexeme Wrap.wrap> MINUS       "-"   [@recover.expr mk "-" $loc]
%token <lexeme Wrap.wrap> SLASH       "/"   [@recover.expr mk "/" $loc]
%token <lexeme Wrap.wrap> TIMES       "*"   [@recover.expr mk "*" $loc]
%token <lexeme Wrap.wrap> DOT         "."   [@recover.expr mk "." $loc]
%token <lexeme Wrap.wrap> WILD        "_"   [@recover.expr mk "_" $loc]
%token <lexeme Wrap.wrap> CARET       "^"   [@recover.expr mk "^" $loc]

  (* Keywords *)

%token <lexeme Wrap.wrap> And         "and"       [@recover.expr mk "and" $loc]
%token <lexeme Wrap.wrap> Begin       "begin"     [@recover.expr mk "begin" $loc]
%token <lexeme Wrap.wrap> BigMap      "big_map"   [@recover.expr mk "big_map" $loc]
%token <lexeme Wrap.wrap> Block       "block"     [@recover.expr mk "block" $loc]
%token <lexeme Wrap.wrap> Case        "case"      [@recover.expr mk "case" $loc]
%token <lexeme Wrap.wrap> Const       "const"     [@recover.expr mk "const" $loc]
%token <lexeme Wrap.wrap> Contains    "contains"  [@recover.expr mk "contains" $loc]
%token <lexeme Wrap.wrap> Else        "else"      [@recover.expr mk "else" $loc]
%token <lexeme Wrap.wrap> End         "end"       [@recover.expr mk "end" $loc]
%token <lexeme Wrap.wrap> For         "for"       [@recover.expr mk "for" $loc]
%token <lexeme Wrap.wrap> Function    "function"  [@recover.expr mk "function" $loc]
%token <lexeme Wrap.wrap> Recursive   "recursive" [@recover.expr mk "recursive" $loc]
%token <lexeme Wrap.wrap> From        "from"      [@recover.expr mk "from" $loc]
%token <lexeme Wrap.wrap> If          "if"        [@recover.expr mk "if" $loc]
%token <lexeme Wrap.wrap> In          "in"        [@recover.expr mk "in" $loc]
%token <lexeme Wrap.wrap> Is          "is"        [@recover.expr mk "is" $loc]
%token <lexeme Wrap.wrap> List        "list"      [@recover.expr mk "list" $loc]
%token <lexeme Wrap.wrap> Map         "map"       [@recover.expr mk "map" $loc]
%token <lexeme Wrap.wrap> Mod         "mod"       [@recover.expr mk "mod" $loc]
%token <lexeme Wrap.wrap> Nil         "nil"       [@recover.expr mk "nil" $loc]
%token <lexeme Wrap.wrap> Not         "not"       [@recover.expr mk "not" $loc]
%token <lexeme Wrap.wrap> Of          "of"        [@recover.expr mk "of" $loc]
%token <lexeme Wrap.wrap> Or          "or"        [@recover.expr mk "or" $loc]
%token <lexeme Wrap.wrap> Patch       "patch"     [@recover.expr mk "patch" $loc]
%token <lexeme Wrap.wrap> Record      "record"    [@recover.expr mk "record" $loc]
%token <lexeme Wrap.wrap> Remove      "remove"    [@recover.expr mk "remove" $loc]
%token <lexeme Wrap.wrap> Set         "set"       [@recover.expr mk "set" $loc]
%token <lexeme Wrap.wrap> Skip        "skip"      [@recover.expr mk "skip" $loc]
%token <lexeme Wrap.wrap> Step        "step"      [@recover.expr mk "step" $loc]
%token <lexeme Wrap.wrap> Then        "then"      [@recover.expr mk "then" $loc]
%token <lexeme Wrap.wrap> To          "to"        [@recover.expr mk "to" $loc]
%token <lexeme Wrap.wrap> Type        "type"      [@recover.expr mk "type" $loc]
%token <lexeme Wrap.wrap> Var         "var"       [@recover.expr mk "var" $loc]
%token <lexeme Wrap.wrap> While       "while"     [@recover.expr mk "while" $loc]
%token <lexeme Wrap.wrap> With        "with"      [@recover.expr mk "with" $loc]
%token <lexeme Wrap.wrap> Module      "module"    [@recover.expr mk "module" $loc]

  (* Virtual tokens *)

%token <lexeme Wrap.wrap> EOF [@recover.expr mk "" $loc]

%%
