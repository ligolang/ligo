{
(* START OF HEADER *)

type lexeme = string

let sprintf = Printf.sprintf

module Region = Simple_utils.Region
module Pos    = Simple_utils.Pos
module SMap   = Utils.String.Map
module SSet   = Utils.String.Set

(* TOKENS *)

type t =
  (* Symbols *)

  CAT of Region.t       (* "++"  *)

  (* Arithmetics *)

| MINUS of Region.t     (* "-" *)
| PLUS of Region.t      (* "+" *)
| SLASH of Region.t     (* "/" *)
| TIMES of Region.t     (* "*" *)

  (* Compounds *)

| LPAR of Region.t      (* "(" *)
| RPAR of Region.t      (* ")" *)
| LBRACKET of Region.t  (* "[" *)
| RBRACKET of Region.t  (* "]" *)
| LBRACE of Region.t    (* "{" *)
| RBRACE of Region.t    (* "}" *)

  (* Separators *)

| COMMA of Region.t     (* "," *)
| SEMI  of Region.t     (* ";" *)
| VBAR of Region.t      (* "|" *)
| COLON of Region.t     (* ":" *)
| DOT of Region.t       (* "." *)
| ELLIPSIS of Region.t (* "..." *)

  (* Wildcard *)

| WILD of Region.t      (* "_" *)

  (* Comparisons *)

| EQ of Region.t        (* "="  *)
| EQEQ of Region.t      (* "=="  *)
| NE of Region.t        (* "!=" *)
| LT of Region.t        (* "<"  *)
| GT of Region.t        (* ">"  *)
| LE of Region.t        (* "<=" *)
| GE of Region.t        (* ">=" *)
| ARROW of Region.t     (* "=>" *)

| BOOL_OR of Region.t   (* "||" *)
| BOOL_AND of Region.t  (* "&&" *)

| NOT of Region.t (* ! *)

  (* Identifiers, labels, numbers and strings *)

| Ident  of string Region.reg
| Constr of string Region.reg
| Int    of (string * Z.t) Region.reg
| Nat    of (string * Z.t) Region.reg
| Mutez  of (string * Z.t) Region.reg
| String of string Region.reg
| Bytes  of (string * Hex.t) Region.reg

  (* Keywords *)

(*| And*)
| Else of Region.t
| False of Region.t
| If of Region.t
| Let of Region.t
| Switch of Region.t
| Mod of Region.t
| Or of Region.t
| True of Region.t
| Type of Region.t
  (* Data constructors *)

| C_None  of Region.t  (* "None"  *)
| C_Some  of Region.t  (* "Some"  *)

(* Virtual tokens *)

| EOF of Region.t (* End of file *)


type token = t

let proj_token = function
  CAT region -> region, "CAT"
| MINUS region -> region, "MINUS"
| PLUS region -> region, "PLUS"
| SLASH region -> region, "SLASH"
| TIMES region -> region, "TIMES"
| LPAR region -> region, "LPAR"
| RPAR region -> region, "RPAR"
| LBRACKET region -> region, "LBRACKET"
| RBRACKET region -> region, "RBRACKET"
| LBRACE region -> region, "LBRACE"
| RBRACE region -> region, "RBRACE"
| COMMA region -> region, "COMMA"
| SEMI region -> region, "SEMI"
| VBAR region -> region, "VBAR"
| COLON region -> region, "COLON"
| DOT region -> region, "DOT"
| ELLIPSIS region -> region, "ELLIPSIS"
| WILD region -> region, "WILD"
| EQ region -> region, "EQ"
| EQEQ region -> region, "EQEQ"
| NE region -> region, "NE"
| LT region -> region, "LT"
| GT region -> region, "GT"
| LE region -> region, "LE"
| GE region -> region, "GE"
| ARROW region -> region, "ARROW"
| BOOL_OR region -> region, "BOOL_OR"
| BOOL_AND region -> region, "BOOL_AND"
| Ident Region.{region; value} ->
    region, sprintf "Ident %s" value
| Constr Region.{region; value} ->
    region, sprintf "Constr %s" value
| Int Region.{region; value = s,n} ->
    region, sprintf "Int (\"%s\", %s)" s (Z.to_string n)
| Nat Region.{region; value = s,n} ->
    region, sprintf "Nat (\"%s\", %s)" s (Z.to_string n)
| Mutez Region.{region; value = s,n} ->
    region, sprintf "Mutez (\"%s\", %s)" s (Z.to_string n)
| String Region.{region; value} ->
    region, sprintf "String %s" value
| Bytes Region.{region; value = s,b} ->
    region,
    sprintf "Bytes (\"%s\", \"0x%s\")"
      s (Hex.to_string b)
| Else region -> region, "Else"
| False region -> region, "False"
| If region -> region, "If"
| Let region -> region, "Let"
| Switch region -> region, "Switch"
| Mod region -> region, "Mod"
| NOT region -> region, "!"
| Or region -> region, "Or"
| True region -> region, "True"
| Type region -> region, "Type"
| C_None  region -> region, "C_None"
| C_Some  region -> region, "C_Some"
| EOF region -> region, "EOF"

let to_lexeme = function
  CAT _ -> "++"
| MINUS _ -> "-"
| PLUS _ -> "+"
| SLASH _ -> "/"
| TIMES _ -> "*"
| LPAR _ -> "("
| RPAR _ -> ")"
| LBRACKET _ -> "["
| RBRACKET _ -> "]"
| LBRACE _ -> "{"
| RBRACE _ -> "}"
| COMMA _ -> ","
| SEMI _ -> ";"
| VBAR _ -> "|"
| COLON _ -> ":"
| DOT _ -> "."
| ELLIPSIS _ -> "..."
| WILD _ -> "_"
| EQ _ -> "="
| EQEQ _ -> "=="
| NE _ -> "!="
| LT _ -> "<"
| GT _ -> ">"
| LE _ -> "<="
| GE _ -> ">="
| ARROW _ -> "=>"
| BOOL_OR _ -> "||"
| BOOL_AND _ -> "&&"
| Ident id -> id.Region.value
| Constr id -> id.Region.value
| Int i
| Nat i
| Mutez i -> fst i.Region.value
| String s -> s.Region.value
| Bytes b -> fst b.Region.value
| Else _ -> "else"
| False _ -> "false"
| If _ -> "if"
| Let _ -> "let"
| Mod _ -> "mod"
| NOT _ -> "!"
| Or _ -> "or"
| Switch _ -> "switch"
| True _ -> "true"
| Type _ -> "type"
| C_None  _ -> "None"
| C_Some  _ -> "Some"
| EOF _ -> ""

let to_string token ?(offsets=true) mode =
  let region, val_str = proj_token token in
  let reg_str = region#compact ~offsets mode
  in sprintf "%s: %s" reg_str val_str

let to_region token = proj_token token |> fst

(* Injections *)

type   int_err = Non_canonical_zero
type ident_err = Reserved_name
type   nat_err = Invalid_natural
               | Non_canonical_zero_nat
type   sym_err = Invalid_symbol
type   kwd_err = Invalid_keyword

(* LEXIS *)

let keywords = [
  (fun reg -> Else   reg);
  (fun reg -> False  reg);
  (fun reg -> If     reg);
  (fun reg -> Let    reg);
  (fun reg -> Switch reg);
  (fun reg -> Mod    reg);
  (fun reg -> Or     reg);
  (fun reg -> True   reg);
  (fun reg -> Type   reg)]

(* See: http://caml.inria.fr/pub/docs/manual-ocaml/lex.html#sec86 and
   https://github.com/facebook/reason/blob/master/src/reason-parser/reason_parser.mly *)
let reserved =
  let open SSet in
  empty
    |> add "and"
    |> add "as"
    |> add "asr"
    |> add "begin"
    |> add "class"
    |> add "constraint"
    |> add "do"
    |> add "done"
    |> add "downto"
    |> add "end"
    |> add "exception"
    |> add "external"
    |> add "for"
    |> add "function"
    |> add "functor"
    |> add "inherit"
    |> add "initializer"
    (* |> add "land"  - see https://ligo.atlassian.net/browse/LIGO-263 *)
    |> add "lazy"
    (* |> add "lor"  - see https://ligo.atlassian.net/browse/LIGO-263 *)
    |> add "lsl"
    |> add "lsr"
    (* |> add "lxor"  - see https://ligo.atlassian.net/browse/LIGO-263 *)
    |> add "match"
    |> add "method"
    |> add "module"
    |> add "mutable"
    |> add "new"
    |> add "nonrec"
    |> add "object"
    |> add "of"
    |> add "open"
    |> add "private"
    |> add "rec"
    |> add "sig"
    |> add "struct"
    |> add "then"
    |> add "to"
    |> add "try"
    |> add "val"
    |> add "virtual"
    |> add "when"
    |> add "while"
    |> add "pri"
    |> add "pub"

let constructors = [
  (fun reg -> C_None reg);
  (fun reg -> C_Some reg);
]

let add map (key, value) = SMap.add key value map

let mk_map mk_key list =
  let apply map value = add map (mk_key value, value)
  in List.fold_left apply SMap.empty list

type lexis = {
  kwd  : (Region.t -> token) SMap.t;
  cstr : (Region.t -> token) SMap.t;
  res  : SSet.t
}

let lexicon : lexis =
  let build list = mk_map (fun f -> to_lexeme (f Region.ghost)) list
  in {kwd  = build keywords;
      cstr = build constructors;
      res  = reserved}

(* Keywords *)

let mk_kwd ident region =
  match SMap.find_opt ident lexicon.kwd with
    Some mk_kwd -> Ok (mk_kwd region)
  |        None -> Error Invalid_keyword

(* END OF HEADER *)
}

(* START LEXER DEFINITION *)

(* Named regular expressions *)

let small   = ['a'-'z']
let capital = ['A'-'Z']
let letter  = small | capital
let digit   = ['0'-'9']
let ident   = small (letter | '_' | digit)*
let constr  = capital (letter | '_' | digit)*

(* Rules *)

rule scan_ident region lexicon = parse
  (ident as value) eof {
    if   SSet.mem value lexicon.res
    then Error Reserved_name
    else Ok (match SMap.find_opt value lexicon.kwd with
               Some mk_kwd -> mk_kwd region
             |        None -> Ident Region.{region; value}) }

and scan_constr region lexicon = parse
  (constr as value) eof {
    match SMap.find_opt value lexicon.cstr with
      Some mk_cstr -> mk_cstr region
    |         None -> Constr Region.{region; value} }

(* END LEXER DEFINITION *)

{
(* START TRAILER *)

(* comments *)
let block_comment_start lexeme = lexeme = "/*"
let block_comment_end lexeme = lexeme = "*/"
let line_comment_start lexeme = lexeme = "//"

(* Smart constructors (injections) *)

let mk_string lexeme region = String Region.{region; value=lexeme}

let mk_bytes lexeme region =
  let norm = Str.(global_replace (regexp "_") "" lexeme) in
  let value = lexeme, Hex.of_string norm
  in Bytes Region.{region; value}

let mk_int lexeme region =
  let z = Str.(global_replace (regexp "_") "" lexeme)
          |> Z.of_string in
  if   Z.equal z Z.zero && lexeme <> "0"
  then Error Non_canonical_zero
  else Ok (Int Region.{region; value = lexeme, z})

let mk_nat lexeme region =
  let z =
    Str.(global_replace (regexp "_") "" lexeme) |>
    Str.(global_replace (regexp "n") "") |>
    Z.of_string in
  if Z.equal z Z.zero && lexeme <> "0n"
  then Error Non_canonical_zero_nat
  else Ok (Nat Region.{region; value = lexeme, z})

let mk_mutez lexeme region =
  let z =
    Str.(global_replace (regexp "_") "" lexeme) |>
    Str.(global_replace (regexp "mutez") "") |>
    Z.of_string in
  if Z.equal z Z.zero && lexeme <> "0mutez"
  then Error Non_canonical_zero
  else Ok (Mutez Region.{region; value = lexeme, z})

let eof region = EOF region

(* Making symbols *)

let mk_sym lexeme region =
  match lexeme with
    "-"  -> Ok (MINUS     region)
  | "+"  -> Ok (PLUS      region)
  | "/"  -> Ok (SLASH     region)
  | "*"  -> Ok (TIMES     region)
  | "["  -> Ok (LBRACKET  region)
  | "]"  -> Ok (RBRACKET  region)
  | "{"  -> Ok (LBRACE    region)
  | "}"  -> Ok (RBRACE    region)
  | ","  -> Ok (COMMA     region)
  | ";"  -> Ok (SEMI      region)
  | "|"  -> Ok (VBAR      region)
  | ":"  -> Ok (COLON     region)
  | "."  -> Ok (DOT       region)
  | "_"  -> Ok (WILD      region)
  | "="  -> Ok (EQ        region)
  | "!=" -> Ok (NE        region)
  | "<"  -> Ok (LT        region)
  | ">"  -> Ok (GT        region)
  | "<=" -> Ok (LE        region)
  | ">=" -> Ok (GE        region)
  | "||" -> Ok (BOOL_OR   region)
  | "&&" -> Ok (BOOL_AND  region)
  | "("  -> Ok (LPAR      region)
  | ")"  -> Ok (RPAR      region)

  (* Symbols specific to ReasonLIGO *)

  | "..." ->  Ok (ELLIPSIS  region)
  | "=>"  ->  Ok (ARROW     region)
  | "=="  ->  Ok (EQEQ      region)
  | "!"   ->  Ok (NOT       region)
  | "++"  ->  Ok (CAT       region)

  (* Invalid symbols *)

  |     _ ->  Error Invalid_symbol


(* Identifiers *)

let mk_ident' lexeme region lexicon =
  Lexing.from_string lexeme |> scan_ident region lexicon

let mk_ident lexeme region = mk_ident' lexeme region lexicon

(* Constructors *)

let mk_constr' lexeme region lexicon =
  Lexing.from_string lexeme |> scan_constr region lexicon

let mk_constr lexeme region = mk_constr' lexeme region lexicon

(* Predicates *)

let is_string = function
  String _ -> true
|        _ -> false

let is_bytes = function
  Bytes _ -> true
|       _ -> false

let is_int = function
  Int _ -> true
|     _ -> false

let is_ident = function
  Ident _ -> true
|       _ -> false

let is_kwd = function
  Else _
| False _
| If _
| Let _
| Switch _
| Mod _
| Or _
| True _
| Type _ -> true
| _ -> false

let is_constr = function
  Constr _
| Ident _
| False _
| True _ -> true
| _ -> false

let is_sym = function
  CAT _
| MINUS _
| PLUS _
| SLASH _
| TIMES _
| LPAR _
| RPAR _
| LBRACKET _
| RBRACKET _
| LBRACE _
| RBRACE _
| COMMA _
| SEMI  _
| VBAR _
| COLON _
| DOT _
| ELLIPSIS _
| WILD _
| EQ _
| EQEQ _
| NE _
| LT _
| GT _
| LE _
| GE _
| ARROW _
| BOOL_OR _
| NOT _
| BOOL_AND _ -> true
|          _ -> false

let is_eof = function EOF _ -> true | _ -> false

(* END TRAILER *)
}
