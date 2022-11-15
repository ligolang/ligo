(* Tokens (mirroring these defined in module Token) *)

(* All external symbols here should be unqualified because this file
   is used by [menhir] that does not always insert the [%{..%}]
   header. So we work around it by the [-open Module] option in [dune]
   but symbols should be unqualified.

   Also, keep in mind that [ParToken.mly] and [Parser.mly] are merged
   into one file, and the header of [Parser.mly] affects this code. *)

%[@recover.prelude
  (* See [dune] file for [-open] flags for modules used in the
     semantic value of tokens, like [Wrap]. *)

  module Directive = Preprocessor.Directive
  module Region    = Simple_utils.Region
  module Token     = Lexing_reasonligo.Token

  let mk_Directive region =
    let linenum   = Region.wrap_ghost 1
    and filename  = Region.wrap_ghost "_none_"
    and flag      = None in
    let open Directive in
    PP_Linemarker (new mk_line_directive region linenum filename flag)

  let mk_lang region =
    Region.{value = {value="Ghost_lang"; region}; region}

  (* Ghost semantic values for inserted tokens *)

  let mk_string   = Token.wrap_string   "ghost string"
  let mk_verbatim = Token.wrap_verbatim "ghost verbatim"
  let mk_bytes    = Token.wrap_bytes    (Hex.of_string "Ghost bytes")
  let mk_int      = Token.wrap_int      Z.zero
  let mk_nat      = Token.wrap_nat      Z.zero
  let mk_mutez    = Token.wrap_mutez    Int64.zero
  let mk_ident    = Token.wrap_ident    "ghost_ident"
  let mk_uident   = Token.wrap_uident   "Ghost_uident"
  let mk_attr     = Token.wrap_attr     "ghost_attr" None
]

(* Make the recovery pay more attention to the number of synthesized
   tokens than production reducing because the latter often means only
   precedence level *)

%[@recover.default_cost_of_symbol     1000]
%[@recover.default_cost_of_production 1]


(* Literals *)

%token     <Preprocessor.Directive.t> Directive "<directive>" [@recover.expr mk_Directive $loc]
%token                <string Wrap.t> String    "<string>"    [@recover.expr mk_string    $loc]
%token                <string Wrap.t> Verbatim  "<verbatim>"  [@recover.expr mk_verbatim  $loc]
%token      <(string * Hex.t) Wrap.t> Bytes     "<bytes>"     [@recover.expr mk_bytes     $loc]
%token        <(string * Z.t) Wrap.t> Int       "<int>"       [@recover.expr mk_int       $loc]
%token        <(string * Z.t) Wrap.t> Nat       "<nat>"       [@recover.expr mk_nat       $loc]
%token    <(string * Int64.t) Wrap.t> Mutez     "<mutez>"     [@recover.expr mk_mutez     $loc]
%token                <string Wrap.t> Ident     "<ident>"     [@recover.expr mk_ident     $loc]
[@recover.cost 900]
%token                <string Wrap.t> UIdent    "<uident>"    [@recover.expr mk_uident    $loc]
%token            <Attr.t Region.reg> Attr      "[@attr]"     [@recover.expr mk_attr      $loc]
%token <string Region.reg Region.reg> Lang      "[%lang"      [@recover.expr mk_lang      $loc]

(* Symbols *)

%token <string Wrap.t> MINUS    "-"   [@recover.expr Token.wrap_minus    $loc]
%token <string Wrap.t> PLUS     "+"   [@recover.expr Token.wrap_plus     $loc]
%token <string Wrap.t> SLASH    "/"   [@recover.expr Token.wrap_slash    $loc]
%token <string Wrap.t> TIMES    "*"   [@recover.expr Token.wrap_times    $loc]
%token <string Wrap.t> LPAR     "("   [@recover.expr Token.wrap_lpar     $loc]
%token <string Wrap.t> RPAR     ")"   [@recover.expr Token.wrap_rpar     $loc]
%token <string Wrap.t> LBRACKET "["   [@recover.expr Token.wrap_lbracket $loc]
%token <string Wrap.t> RBRACKET "]"   [@recover.expr Token.wrap_rbracket $loc]
%token <string Wrap.t> LBRACE   "{"   [@recover.expr Token.wrap_lbrace   $loc]
%token <string Wrap.t> RBRACE   "}"   [@recover.expr Token.wrap_rbrace   $loc]
%token <string Wrap.t> PLUS2    "++"  [@recover.expr Token.wrap_plus2    $loc]
%token <string Wrap.t> DOT      "."   [@recover.expr Token.wrap_dot      $loc]
%token <string Wrap.t> ELLIPSIS "..." [@recover.expr Token.wrap_ellipsis $loc]
%token <string Wrap.t> COMMA    ","   [@recover.expr Token.wrap_comma    $loc]
%token <string Wrap.t> SEMI     ";"   [@recover.expr Token.wrap_semi     $loc]
%token <string Wrap.t> COLON    ":"   [@recover.expr Token.wrap_colon    $loc]
%token <string Wrap.t> VBAR     "|"   [@recover.expr Token.wrap_vbar     $loc]
%token <string Wrap.t> WILD     "_"   [@recover.expr Token.wrap_wild     $loc] [@recover.cost 700]
%token <string Wrap.t> EQ       "="   [@recover.expr Token.wrap_eq       $loc]
%token <string Wrap.t> EQ2      "=="  [@recover.expr Token.wrap_eq2      $loc]
%token <string Wrap.t> NE       "!="  [@recover.expr Token.wrap_ne       $loc]
%token <string Wrap.t> LT       "<"   [@recover.expr Token.wrap_lt       $loc]
%token <string Wrap.t> GT       ">"   [@recover.expr Token.wrap_gt       $loc]
%token <string Wrap.t> LE       "<="  [@recover.expr Token.wrap_le       $loc]
%token <string Wrap.t> GE       ">="  [@recover.expr Token.wrap_ge       $loc]
%token <string Wrap.t> ARROW    "=>"  [@recover.expr Token.wrap_arrow    $loc]
%token <string Wrap.t> NOT      "!"   [@recover.expr Token.wrap_not      $loc]
%token <string Wrap.t> BOOL_OR  "||"  [@recover.expr Token.wrap_bool_or  $loc]
%token <string Wrap.t> BOOL_AND "&&"  [@recover.expr Token.wrap_bool_and $loc]
%token <string Wrap.t> QUOTE    "'"   [@recover.expr Token.wrap_quote    $loc]

(* Keywords *)

%token <string Wrap.t> Else   "else"   [@recover.expr Token.wrap_else   $loc]
%token <string Wrap.t> If     "if"     [@recover.expr Token.wrap_if     $loc]
%token <string Wrap.t> Let    "let"    [@recover.expr Token.wrap_let    $loc]
%token <string Wrap.t> Rec    "rec"    [@recover.expr Token.wrap_rec    $loc]
%token <string Wrap.t> Switch "switch" [@recover.expr Token.wrap_switch $loc]
%token <string Wrap.t> Mod    "mod"    [@recover.expr Token.wrap_mod    $loc]
%token <string Wrap.t> Land   "land"   [@recover.expr Token.wrap_land   $loc]
%token <string Wrap.t> Lor    "lor"    [@recover.expr Token.wrap_lor    $loc]
%token <string Wrap.t> Lxor   "lxor"   [@recover.expr Token.wrap_lxor   $loc]
%token <string Wrap.t> Lsl    "lsl"    [@recover.expr Token.wrap_lsl    $loc]
%token <string Wrap.t> Lsr    "lsr"    [@recover.expr Token.wrap_lsr    $loc]
%token <string Wrap.t> Or     "or"     [@recover.expr Token.wrap_or     $loc]
%token <string Wrap.t> Type   "type"   [@recover.expr Token.wrap_type   $loc]
%token <string Wrap.t> Module "module" [@recover.expr Token.wrap_module $loc]

(* Virtual tokens *)

%token <string Wrap.t> ES6FUN [@recover.expr Token.wrap_es6fun $loc]

(* End of File *)

%token <string Wrap.t> EOF [@recover.expr Token.wrap_eof $loc]

%%
