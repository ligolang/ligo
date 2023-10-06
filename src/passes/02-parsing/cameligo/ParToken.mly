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
  module Token     = Lexing_cameligo.Token

  let mk_Directive region =
    let linenum   = Region.wrap_ghost 1
    and filename  = Region.wrap_ghost "_none_"
    and flag      = None in
    let open Directive in
    PP_Linemarker (mk_line_directive region linenum filename flag)

  let mk_lang region = Token.wrap_lang "Ghost_lang" region

  (* Ghost semantic values for inserted tokens *)

  let mk_string    = Token.wrap_string    "ghost string"
  let mk_verbatim  = Token.wrap_verbatim  "ghost verbatim"
  let mk_bytes     = Token.wrap_bytes     (Hex.of_string "Ghost bytes")
  let mk_int       = Token.wrap_int       Z.zero
  let mk_nat       = Token.wrap_nat       Z.zero
  let mk_mutez     = Token.wrap_mutez     Int64.zero
  let mk_ident     = Token.wrap_ident     "ghost_ident"
  let mk_uident    = Token.wrap_uident    "Ghost_uident"
  let mk_eident    = Token.wrap_eident    "@ghost_eident"
  let mk_attr      = Token.wrap_attr      "ghost_attr" None
  let mk_block_com = Token.wrap_block_com "(* comment *)"
  let mk_line_com  = Token.wrap_line_com  "// comment"
]

(* Make the recovery pay more attention to the number of synthesized tokens than
   production reducing because the latter often means only precedence level *)

%[@recover.default_cost_of_symbol     1000]
%[@recover.default_cost_of_production    1]

(* Tokens (mirroring those defined in module Token) *)

(* Literals *)

%token         <string Wrap.t> BlockCom  "<block_comment>" [@recover.expr mk_block_com $loc]
%token         <string Wrap.t> LineCom   "<line_comment>"  [@recover.expr mk_line_com  $loc]

%token  <Preprocessor.Directive.t> Directive "<directive>"
%token             <string Wrap.t> String    "<string>"    [@recover.expr mk_string   $loc]
%token             <string Wrap.t> Verbatim  "<verbatim>"  [@recover.expr mk_verbatim $loc]
%token   <(string * Hex.t) Wrap.t> Bytes     "<bytes>"     [@recover.expr mk_bytes    $loc]
%token     <(string * Z.t) Wrap.t> Int       "<int>"       [@recover.expr mk_int      $loc]
%token     <(string * Z.t) Wrap.t> Nat       "<nat>"       [@recover.expr mk_nat      $loc]
%token <(string * Int64.t) Wrap.t> Mutez     "<mutez>"     [@recover.expr mk_mutez    $loc]
%token             <string Wrap.t> Ident     "<ident>"     [@recover.expr mk_ident    $loc] [@recover.cost 900]
%token             <string Wrap.t> UIdent    "<uident>"    [@recover.expr mk_uident   $loc]
%token             <string Wrap.t> EIdent    "<eident>"    [@recover.expr mk_eident   $loc]
%token             <Attr.t Wrap.t> Attr      "[@attr]"     [@recover.expr mk_attr     $loc]
%token  <string Region.reg Wrap.t> Lang      "[%lang"      [@recover.expr mk_lang     $loc]

(* Symbols *)

%token <string Wrap.t> MINUS    "-"  [@recover.expr Token.wrap_minus     $loc]
%token <string Wrap.t> PLUS     "+"  [@recover.expr Token.wrap_plus      $loc]
%token <string Wrap.t> SLASH    "/"  [@recover.expr Token.wrap_slash     $loc]
%token <string Wrap.t> TIMES    "*"  [@recover.expr Token.wrap_times     $loc]
%token <string Wrap.t> LPAR     "("  [@recover.expr Token.wrap_lpar      $loc]
%token <string Wrap.t> RPAR     ")"  [@recover.expr Token.wrap_rpar      $loc]
%token <string Wrap.t> LBRACKET "["  [@recover.expr Token.wrap_lbracket  $loc]
%token <string Wrap.t> RBRACKET "]"  [@recover.expr Token.wrap_rbracket  $loc]
%token <string Wrap.t> LBRACE   "{"  [@recover.expr Token.wrap_lbrace    $loc]
%token <string Wrap.t> RBRACE   "}"  [@recover.expr Token.wrap_rbrace    $loc]
%token <string Wrap.t> ARROW    "->" [@recover.expr Token.wrap_arrow     $loc]
%token <string Wrap.t> ASS      ":=" [@recover.expr Token.wrap_ass       $loc]
%token <string Wrap.t> CONS     "::" [@recover.expr Token.wrap_cons      $loc]
%token <string Wrap.t> CARET    "^"  [@recover.expr Token.wrap_caret     $loc]
%token <string Wrap.t> DOT      "."  [@recover.expr Token.wrap_dot       $loc]
%token <string Wrap.t> COMMA    ","  [@recover.expr Token.wrap_comma     $loc]
%token <string Wrap.t> SEMI     ";"  [@recover.expr Token.wrap_semi      $loc]
%token <string Wrap.t> COLON    ":"  [@recover.expr Token.wrap_colon     $loc]
%token <string Wrap.t> VBAR     "|"  [@recover.expr Token.wrap_vbar      $loc]
%token <string Wrap.t> WILD     "_"  [@recover.expr Token.wrap_wild      $loc]
%token <string Wrap.t> EQ       "="  [@recover.expr Token.wrap_eq        $loc]
%token <string Wrap.t> NE       "<>" [@recover.expr Token.wrap_ne        $loc]
%token <string Wrap.t> LT       "<"  [@recover.expr Token.wrap_lt        $loc]
%token <string Wrap.t> GT       ">"  [@recover.expr Token.wrap_gt        $loc]
%token <string Wrap.t> LE       "<=" [@recover.expr Token.wrap_le        $loc]
%token <string Wrap.t> BOOL_OR  "||" [@recover.expr Token.wrap_bool_or   $loc]
%token <string Wrap.t> BOOL_AND "&&" [@recover.expr Token.wrap_bool_and  $loc]
%token <string Wrap.t> QUOTE    "'"  [@recover.expr Token.wrap_quote     $loc]

%token <string Wrap.t> REV_APP  "|>"  [@recover.expr Token.wrap_rev_app  $loc]
%token <string Wrap.t> PLUS_EQ  "+="  [@recover.expr Token.wrap_plus_eq  $loc]
%token <string Wrap.t> MINUS_EQ "-="  [@recover.expr Token.wrap_minus_eq $loc]
%token <string Wrap.t> TIMES_EQ "*="  [@recover.expr Token.wrap_times_eq $loc]
%token <string Wrap.t> SLASH_EQ "/="  [@recover.expr Token.wrap_slash_eq $loc]
%token <string Wrap.t> VBAR_EQ  "|="  [@recover.expr Token.wrap_vbar_eq  $loc]

(* OCaml keywords *)

%token <string Wrap.t> Begin   "begin"   [@recover.expr Token.wrap_begin   $loc]
%token <string Wrap.t> Do      "do"      [@recover.expr Token.wrap_do      $loc]
%token <string Wrap.t> Done    "done"    [@recover.expr Token.wrap_done    $loc]
%token <string Wrap.t> Downto  "downto"  [@recover.expr Token.wrap_downto  $loc]
%token <string Wrap.t> Else    "else"    [@recover.expr Token.wrap_else    $loc]
%token <string Wrap.t> End     "end"     [@recover.expr Token.wrap_end     $loc]
%token <string Wrap.t> False   "false"   [@recover.expr Token.wrap_false   $loc]
%token <string Wrap.t> For     "for"     [@recover.expr Token.wrap_for     $loc]
%token <string Wrap.t> Fun     "fun"     [@recover.expr Token.wrap_fun     $loc]
%token <string Wrap.t> If      "if"      [@recover.expr Token.wrap_if      $loc]
%token <string Wrap.t> In      "in"      [@recover.expr Token.wrap_in      $loc]
%token <string Wrap.t> Include "include" [@recover.expr Token.wrap_include $loc]
%token <string Wrap.t> Land    "land"    [@recover.expr Token.wrap_land    $loc]
%token <string Wrap.t> Let     "let"     [@recover.expr Token.wrap_let     $loc]
%token <string Wrap.t> Lor     "lor"     [@recover.expr Token.wrap_lor     $loc]
%token <string Wrap.t> Lsl     "lsl"     [@recover.expr Token.wrap_lsl     $loc]
%token <string Wrap.t> Lsr     "lsr"     [@recover.expr Token.wrap_lsr     $loc]
%token <string Wrap.t> Lxor    "lxor"    [@recover.expr Token.wrap_lxor    $loc]
%token <string Wrap.t> Match   "match"   [@recover.expr Token.wrap_match   $loc]
%token <string Wrap.t> Mod     "mod"     [@recover.expr Token.wrap_mod     $loc]
%token <string Wrap.t> Module  "module"  [@recover.expr Token.wrap_module  $loc]
%token <string Wrap.t> Mut     "mut"     [@recover.expr Token.wrap_mut     $loc]
%token <string Wrap.t> Not     "not"     [@recover.expr Token.wrap_not     $loc]
%token <string Wrap.t> Of      "of"      [@recover.expr Token.wrap_of      $loc]
%token <string Wrap.t> Or      "or"      [@recover.expr Token.wrap_or      $loc]
%token <string Wrap.t> Rec     "rec"     [@recover.expr Token.wrap_rec     $loc]
%token <string Wrap.t> Sig     "sig"     [@recover.expr Token.wrap_sig     $loc]
%token <string Wrap.t> Struct  "struct"  [@recover.expr Token.wrap_struct  $loc]
%token <string Wrap.t> Then    "then"    [@recover.expr Token.wrap_then    $loc]
%token <string Wrap.t> True    "true"    [@recover.expr Token.wrap_true    $loc]
%token <string Wrap.t> Type    "type"    [@recover.expr Token.wrap_type    $loc]
%token <string Wrap.t> Val     "val"     [@recover.expr Token.wrap_val     $loc]
%token <string Wrap.t> While   "while"   [@recover.expr Token.wrap_while   $loc]
%token <string Wrap.t> With    "with"    [@recover.expr Token.wrap_with    $loc]

(* CameLIGO-specific keywords *)

%token <string Wrap.t> Upto        "upto"         [@recover.expr Token.wrap_upto         $loc]
%token <string Wrap.t> ContractOf  "contract_of"  [@recover.expr Token.wrap_contract_of  $loc]
%token <string Wrap.t> ParameterOf "parameter_of" [@recover.expr Token.wrap_parameter_of $loc]


(* Virtual tokens *)

%token <string Wrap.t> ZWSP [@recover.expr Token.wrap_zwsp $loc]

(* End of File *)

%token <string Wrap.t> EOF [@recover.expr Token.wrap_eof $loc]

%%
