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
  module Token     = Lexing_pyligo.Token
  module ErrorPrefix = Parsing_shared.Errors.ErrorPrefix

  let mk_Directive region =
    let linenum   = Region.wrap_ghost 1
    and filename  = Region.wrap_ghost "_none_"
    and flag      = None in
    let open Directive in
    PP_Linemarker (mk_line_directive region linenum filename flag)

  let mk_lang region =
    Region.{value = {value = (ErrorPrefix.add "Ghost_lang"); region}; region}

  (* Ghost semantic values for inserted tokens *)

  let mk_string   = Token.wrap_string   @@ ErrorPrefix.add "ghost string"
  let mk_verbatim = Token.wrap_verbatim @@ ErrorPrefix.add "ghost verbatim"
  let mk_bytes    = Token.wrap_bytes    (Hex.of_string "Ghost bytes")
  let mk_int      = Token.wrap_int      Z.zero
  let mk_nat      = Token.wrap_nat      Z.zero
  let mk_mutez    = Token.wrap_mutez    Int64.zero
  let mk_ident    = Token.wrap_ident    @@ ErrorPrefix.add "ghost_ident"
  let mk_uident   = Token.wrap_uident   @@ ErrorPrefix.add "Ghost_uident"
  let mk_eident   = Token.wrap_eident   @@ ErrorPrefix.add "@ghost_eident"
  let mk_attr     = Token.wrap_attr     (ErrorPrefix.add "ghost_attr") None
]

(* Make the recovery pay more attention to the number of synthesized
   tokens than production reducing because the latter often means only
   precedence level *)

%[@recover.default_cost_of_symbol     1000]
%[@recover.default_cost_of_production 1]

(* Tokens (mirroring thise defined in module Token) *)

(* Literals *)

%token     <Preprocessor.Directive.t> Directive "<directive>"
%token                <string Wrap.t> String    "<string>"    [@recover.expr mk_string    $loc]
%token                <string Wrap.t> Verbatim  "<verbatim>"  [@recover.expr mk_verbatim  $loc]
%token      <(string * Hex.t) Wrap.t> Bytes     "<bytes>"     [@recover.expr mk_bytes     $loc]
%token        <(string * Z.t) Wrap.t> Int       "<int>"       [@recover.expr mk_int       $loc]
%token        <(string * Z.t) Wrap.t> Nat       "<nat>"       [@recover.expr mk_nat       $loc]
%token    <(string * Int64.t) Wrap.t> Mutez     "<mutez>"     [@recover.expr mk_mutez     $loc]
%token                <string Wrap.t> Ident     "<ident>"     [@recover.expr mk_ident     $loc] [@recover.cost 900]
%token                <string Wrap.t> UIdent    "<uident>"    [@recover.expr mk_uident    $loc]
%token                <string Wrap.t> EIdent    "<eident>"    [@recover.expr mk_eident   $loc]
%token            <Attr.t Region.reg> Attr      "[@attr]"     [@recover.expr mk_attr      $loc]
%token <string Region.reg Region.reg> Lang      "[%lang"      [@recover.expr mk_lang      $loc]

(* Symbols *)

%token <string Wrap.t> ARROW      "->"  [@recover.expr Token.wrap_arrow      $loc]
%token <string Wrap.t> XOR        "^"   [@recover.expr Token.wrap_xor        $loc]
%token <string Wrap.t> MINUS      "-"   [@recover.expr Token.wrap_minus      $loc]
%token <string Wrap.t> PLUS       "+"   [@recover.expr Token.wrap_plus       $loc]
%token <string Wrap.t> TIMES      "*"   [@recover.expr Token.wrap_times      $loc]
%token <string Wrap.t> EXP        "**"  [@recover.expr Token.wrap_exp        $loc]
%token <string Wrap.t> SLASH2     "//"  [@recover.expr Token.wrap_slash2     $loc]
%token <string Wrap.t> PERCENT    "%"   [@recover.expr Token.wrap_percent    $loc]
%token <string Wrap.t> LPAR       "("   [@recover.expr Token.wrap_lpar       $loc]
%token <string Wrap.t> RPAR       ")"   [@recover.expr Token.wrap_rpar       $loc]
%token <string Wrap.t> LBRACKET   "["   [@recover.expr Token.wrap_lbracket   $loc]
%token <string Wrap.t> RBRACKET   "]"   [@recover.expr Token.wrap_rbracket   $loc]
%token <string Wrap.t> LBRACE     "{"   [@recover.expr Token.wrap_lbrace     $loc]
%token <string Wrap.t> RBRACE     "}"   [@recover.expr Token.wrap_rbrace     $loc]
%token <string Wrap.t> COMMA      ","   [@recover.expr Token.wrap_comma      $loc]
%token <string Wrap.t> SEMI       ";"   [@recover.expr Token.wrap_semi       $loc]
%token <string Wrap.t> VBAR       "|"   [@recover.expr Token.wrap_vbar       $loc]
%token <string Wrap.t> AMPERSAND  "&"   [@recover.expr Token.wrap_ampersand  $loc]
%token <string Wrap.t> COLON      ":"   [@recover.expr Token.wrap_colon      $loc]
%token <string Wrap.t> DOT        "."   [@recover.expr Token.wrap_dot        $loc]
%token <string Wrap.t> TILDE      "~"   [@recover.expr Token.wrap_tilde      $loc]
%token <string Wrap.t> LSHIFT     "<<"  [@recover.expr Token.wrap_lshift     $loc]
%token <string Wrap.t> RSHIFT     ">>"  [@recover.expr Token.wrap_rshift     $loc]
%token <string Wrap.t> EQ         "="   [@recover.expr Token.wrap_eq         $loc]
%token <string Wrap.t> EQ2        "=="  [@recover.expr Token.wrap_eq2        $loc]
%token <string Wrap.t> NE         "!="  [@recover.expr Token.wrap_ne         $loc]
%token <string Wrap.t> LT         "<"   [@recover.expr Token.wrap_lt         $loc]
%token <string Wrap.t> GT         ">"   [@recover.expr Token.wrap_gt         $loc]
%token <string Wrap.t> LE         "<="  [@recover.expr Token.wrap_le         $loc]
%token <string Wrap.t> GE         ">="  [@recover.expr Token.wrap_ge         $loc]
%token <string Wrap.t> PLUS_EQ    "+="  [@recover.expr Token.wrap_plus_eq    $loc]
%token <string Wrap.t> MINUS_EQ   "-="  [@recover.expr Token.wrap_minus_eq   $loc]
%token <string Wrap.t> TIMES_EQ   "*="  [@recover.expr Token.wrap_times_eq   $loc]
%token <string Wrap.t> SLASH2_EQ  "//=" [@recover.expr Token.wrap_slash2_eq  $loc]
%token <string Wrap.t> PERCENT_EQ "%="  [@recover.expr Token.wrap_percent_eq $loc]
%token <string Wrap.t> AND_EQ     "&="  [@recover.expr Token.wrap_and_eq     $loc]
%token <string Wrap.t> OR_EQ      "|="  [@recover.expr Token.wrap_or_eq      $loc]
%token <string Wrap.t> XOR_EQ     "^="  [@recover.expr Token.wrap_xor_eq     $loc]
%token <string Wrap.t> RSHIFT_EQ  ">>=" [@recover.expr Token.wrap_rshift_eq  $loc]
%token <string Wrap.t> LSHIFT_EQ  "<<=" [@recover.expr Token.wrap_lshift_eq  $loc]
%token <string Wrap.t> EXP_EQ     "**=" [@recover.expr Token.wrap_exp_eq     $loc]
%token <string Wrap.t> WILD       "_"   [@recover.expr Token.wrap_wild $loc]
%token <string Wrap.t> GRAVE      "`"   [@recover.expr Token.wrap_grave      $loc]
%token <string Wrap.t> BACKSLASH  "\\"  [@recover.expr Token.wrap_backslash  $loc]

(* Keywords *)

%token <string Wrap.t> And      "and"      [@recover.expr Token.wrap_and      $loc]
(*
%token <string Wrap.t> As       "as"       [@recover.expr Token.wrap_as       $loc] *)
%token <string Wrap.t> Assert   "assert"   [@recover.expr Token.wrap_assert   $loc]
(*
%token <string Wrap.t> Async    "async"    [@recover.expr Token.wrap_async    $loc]
%token <string Wrap.t> Await    "await"    [@recover.expr Token.wrap_await    $loc]
%token <string Wrap.t> Break    "break"    [@recover.expr Token.wrap_break    $loc] *)
%token <string Wrap.t> Case     "case"     [@recover.expr Token.wrap_case     $loc]
%token <string Wrap.t> Class    "class"    [@recover.expr Token.wrap_class    $loc]
%token <string Wrap.t> Const    "const"    [@recover.expr Token.wrap_const    $loc]
(*
%token <string Wrap.t> Continue "continue" [@recover.expr Token.wrap_continue $loc] *)
%token <string Wrap.t> Def      "def"      [@recover.expr Token.wrap_def      $loc]
(*
%token <string Wrap.t> Del      "del"      [@recover.expr Token.wrap_del      $loc] *)
%token <string Wrap.t> Elif     "elif"     [@recover.expr Token.wrap_elif     $loc]
%token <string Wrap.t> Else     "else"     [@recover.expr Token.wrap_else     $loc]
(*
%token <string Wrap.t> Except   "except"   [@recover.expr Token.wrap_except   $loc]
%token <string Wrap.t> Finally  "finally"  [@recover.expr Token.wrap_finally  $loc] *)
%token <string Wrap.t> For      "for"      [@recover.expr Token.wrap_for      $loc]
(*
%token <string Wrap.t> From     "from"     [@recover.expr Token.wrap_from     $loc]
%token <string Wrap.t> Global   "global"   [@recover.expr Token.wrap_global   $loc] *)
%token <string Wrap.t> If       "if"       [@recover.expr Token.wrap_if       $loc]
%token <string Wrap.t> In       "in"       [@recover.expr Token.wrap_in       $loc]
(*
%token <string Wrap.t> Is       "is"       [@recover.expr Token.wrap_is       $loc]
%token <string Wrap.t> Import   "import"   [@recover.expr Token.wrap_import   $loc] *)
%token <string Wrap.t> Lambda   "lambda"   [@recover.expr Token.wrap_lambda   $loc]
%token <string Wrap.t> Match    "match"    [@recover.expr Token.wrap_match    $loc]
%token <string Wrap.t> Module   "module"   [@recover.expr Token.wrap_module   $loc]
(*
%token <string Wrap.t> Nonlocal "nonlocal" [@recover.expr Token.wrap_nonlocal $loc] *)
%token <string Wrap.t> Not      "not"      [@recover.expr Token.wrap_not      $loc]
%token <string Wrap.t> Or       "or"       [@recover.expr Token.wrap_or       $loc]
%token <string Wrap.t> Pass     "pass"     [@recover.expr Token.wrap_pass     $loc]
(*
%token <string Wrap.t> Raise    "raise"    [@recover.expr Token.wrap_raise    $loc] *)
%token <string Wrap.t> Return   "return"   [@recover.expr Token.wrap_return   $loc]
(*
%token <string Wrap.t> Try      "try"      [@recover.expr Token.wrap_try      $loc] *)
%token <string Wrap.t> Type     "type"     [@recover.expr Token.wrap_type     $loc]
%token <string Wrap.t> Var      "var"      [@recover.expr Token.wrap_var      $loc]
%token <string Wrap.t> While    "while"    [@recover.expr Token.wrap_while    $loc]
(*
%token <string Wrap.t> With     "with"     [@recover.expr Token.wrap_with     $loc] *)
(*
%token <string Wrap.t> Yield    "yield"    [@recover.expr Token.wrap_yield    $loc] *)

(* Virtual tokens *)

%token <string Wrap.t> BEGIN     [@recover.expr Token.wrap_begin     $loc]
%token <string Wrap.t> END       [@recover.expr Token.wrap_end       $loc]
%token <int    Wrap.t> INDENT    [@recover.expr Token.wrap_indent    $loc]
%token   <unit Wrap.t> TOP_LEVEL [@recover.expr Token.wrap_top_level $loc]
%token   <unit Wrap.t> ZWSP      [@recover.expr Token.wrap_zwsp      $loc]

(* End of File *)

%token <string Wrap.t> EOF [@recover.expr Token.wrap_eof $loc]

%%
