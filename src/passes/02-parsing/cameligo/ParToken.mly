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

  let mkDirective loc  = LexerLib.Directive.Linemarker Region.{value = (0, "<invalid-path>", None);
                                                               region = loc}
  let mkLang loc = Region.{value = Region.{value = "<invalid-lang-literal>";
                                           region = loc};
                           region = loc}
 ]
(* Tokens (mirroring thise defined in module Token) *)

  (* Literals *)

%token              <LexerLib.Directive.t> Directive "<directive>"  [@recover.expr mkDirective $loc]
%token                  <string Wrap.wrap> String    "<string>"     [@recover.expr mk "<invalid-string-literal>" $loc]
%token                  <string Wrap.wrap> Verbatim  "<verbatim>"   [@recover.expr mk "<invalid-verbatim-literal>" $loc]
%token        <(lexeme * Hex.t) Wrap.wrap> Bytes     "<bytes>"      [@recover.expr mk ("<invalid-bytes-literal>", `Hex "") $loc]
%token          <(string * Z.t) Wrap.wrap> Int       "<int>"        [@recover.expr mk ("<invalid-int-literal>", Z.zero) $loc]
%token          <(string * Z.t) Wrap.wrap> Nat       "<nat>"        [@recover.expr mk ("<invalid-nat-literal>", Z.zero) $loc]
%token          <(string * Z.t) Wrap.wrap> Mutez     "<mutez>"      [@recover.expr mk ("<invalid-mutez-literal>", Z.zero) $loc]
%token                  <string Wrap.wrap> Ident     "<ident>"      [@recover.expr mk "<invalid-ident>" $loc]
%token                  <string Wrap.wrap> UIdent    "<uident>"     [@recover.expr mk "<invalid-uident>" $loc]
%token                  <string Wrap.wrap> Attr      "[@attr]"      [@recover.expr mk "<invalid-attr-literal>" $loc]
%token      <lexeme Region.reg Region.reg> Lang      "[%lang"       [@recover.expr mkLang $loc]

  (* Symbols *)

%token <lexeme Wrap.wrap> MINUS   "-" [@recover.expr mk "-" $loc]
%token <lexeme Wrap.wrap> PLUS    "+" [@recover.expr mk "+" $loc]
%token <lexeme Wrap.wrap> SLASH   "/" [@recover.expr mk "/" $loc]
%token <lexeme Wrap.wrap> TIMES   "*" [@recover.expr mk "*" $loc]

%token <lexeme Wrap.wrap> LPAR     "(" [@recover.expr mk "(" $loc]
%token <lexeme Wrap.wrap> RPAR     ")" [@recover.expr mk ")" $loc]
%token <lexeme Wrap.wrap> LBRACKET "[" [@recover.expr mk "[" $loc]
%token <lexeme Wrap.wrap> RBRACKET "]" [@recover.expr mk "]" $loc]
%token <lexeme Wrap.wrap> LBRACE   "{" [@recover.expr mk "{" $loc]
%token <lexeme Wrap.wrap> RBRACE   "}" [@recover.expr mk "}" $loc]

%token <lexeme Wrap.wrap> ARROW "->" [@recover.expr mk "->" $loc]
%token <lexeme Wrap.wrap> CONS  "::" [@recover.expr mk "::" $loc]
%token <lexeme Wrap.wrap> CARET "^"  [@recover.expr mk "^" $loc]
(*%token <lexeme Wrap.wrap> APPEND "@" *)
%token <lexeme Wrap.wrap> DOT   "." [@recover.expr mk "." $loc]

%token <lexeme Wrap.wrap> COMMA "," [@recover.expr mk "," $loc]
%token <lexeme Wrap.wrap> SEMI  ";" [@recover.expr mk ";" $loc]
%token <lexeme Wrap.wrap> COLON ":" [@recover.expr mk ":" $loc]
%token <lexeme Wrap.wrap> VBAR  "|" [@recover.expr mk "|" $loc]

%token <lexeme Wrap.wrap> WILD  "_" [@recover.expr mk "_" $loc]

%token <lexeme Wrap.wrap> EQ "="  [@recover.expr mk "=" $loc]
%token <lexeme Wrap.wrap> NE "<>" [@recover.expr mk "<>" $loc]
%token <lexeme Wrap.wrap> LT "<"  [@recover.expr mk "<" $loc]
%token <lexeme Wrap.wrap> GT ">"  [@recover.expr mk ">" $loc]
%token <lexeme Wrap.wrap> LE "<=" [@recover.expr mk "<=" $loc]
%token <lexeme Wrap.wrap> GE ">=" [@recover.expr mk ">=" $loc]

%token <lexeme Wrap.wrap> BOOL_OR  "||" [@recover.expr mk "||" $loc]
%token <lexeme Wrap.wrap> BOOL_AND "&&" [@recover.expr mk "&&" $loc]
%token <lexeme Wrap.wrap> QUOTE    "'"  [@recover.expr mk "'" $loc]

 (* Keywords *)

(*%token And*)
%token <lexeme Wrap.wrap> Begin  "begin"  [@recover.expr mk "begin" $loc]
%token <lexeme Wrap.wrap> Else   "else"   [@recover.expr mk "else" $loc]
%token <lexeme Wrap.wrap> End    "end"    [@recover.expr mk "end" $loc]
%token <lexeme Wrap.wrap> Fun    "fun"    [@recover.expr mk "fun" $loc]
%token <lexeme Wrap.wrap> Rec    "rec"    [@recover.expr mk "rec" $loc]
%token <lexeme Wrap.wrap> If     "if"     [@recover.expr mk "if" $loc]
%token <lexeme Wrap.wrap> In     "in"     [@recover.expr mk "in" $loc]
%token <lexeme Wrap.wrap> Let    "let"    [@recover.expr mk "let" $loc]
%token <lexeme Wrap.wrap> Match  "match"  [@recover.expr mk "match" $loc]
%token <lexeme Wrap.wrap> Mod    "mod"    [@recover.expr mk "mod" $loc]
%token <lexeme Wrap.wrap> Land   "land"   [@recover.expr mk "land" $loc]
%token <lexeme Wrap.wrap> Lor    "lor"    [@recover.expr mk "lor" $loc]
%token <lexeme Wrap.wrap> Lxor   "lxor"   [@recover.expr mk "lxor" $loc]
%token <lexeme Wrap.wrap> Lsl    "lsl"    [@recover.expr mk "lsl" $loc]
%token <lexeme Wrap.wrap> Lsr    "lsr"    [@recover.expr mk "lsr" $loc]
%token <lexeme Wrap.wrap> Not    "not"    [@recover.expr mk "not" $loc]
%token <lexeme Wrap.wrap> Of     "of"     [@recover.expr mk "of" $loc]
%token <lexeme Wrap.wrap> Or     "or"     [@recover.expr mk "or" $loc]
%token <lexeme Wrap.wrap> Then   "then"   [@recover.expr mk "then" $loc]
%token <lexeme Wrap.wrap> Type   "type"   [@recover.expr mk "type" $loc]
%token <lexeme Wrap.wrap> With   "with"   [@recover.expr mk "with" $loc]
%token <lexeme Wrap.wrap> Module "module" [@recover.expr mk "module" $loc]
%token <lexeme Wrap.wrap> Struct "struct" [@recover.expr mk "struct" $loc]

  (* Virtual tokens *)

%token <lexeme Wrap.wrap> EOF [@recover.expr mk "" $loc]

%%
