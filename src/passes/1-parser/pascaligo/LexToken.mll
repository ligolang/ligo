(* Lexer specification for LIGO, to be processed by [ocamllex] *)

{
(* START HEADER *)

(* Shorthands *)

type lexeme = string

let sprintf = Printf.sprintf

module Region = Simple_utils.Region
module Pos    = Simple_utils.Pos
module SMap   = Utils.String.Map
module SSet   = Utils.String.Set

(* Hack to roll back one lexeme in the current semantic action *)
(*
let rollback buffer =
  let open Lexing in
  let len = String.length (lexeme buffer) in
  let pos_cnum = buffer.lex_curr_p.pos_cnum - len in
  buffer.lex_curr_pos <- buffer.lex_curr_pos - len;
  buffer.lex_curr_p <- {buffer.lex_curr_p with pos_cnum}
*)

(* TOKENS *)

type t =
  (* Literals *)

  String of lexeme Region.reg
| Bytes  of (lexeme * Hex.t) Region.reg
| Int    of (lexeme * Z.t) Region.reg
| Nat    of (lexeme * Z.t) Region.reg
| Mutez    of (lexeme * Z.t) Region.reg
| Ident  of lexeme Region.reg
| Constr of lexeme Region.reg

  (* Symbols *)

| SEMI     of Region.t
| COMMA    of Region.t
| LPAR     of Region.t
| RPAR     of Region.t
| LBRACE   of Region.t
| RBRACE   of Region.t
| LBRACKET of Region.t
| RBRACKET of Region.t
| CONS     of Region.t
| VBAR     of Region.t
| ARROW    of Region.t
| ASS      of Region.t
| EQ       of Region.t
| COLON    of Region.t
| LT       of Region.t
| LE       of Region.t
| GT       of Region.t
| GE       of Region.t
| NE       of Region.t
| PLUS     of Region.t
| MINUS    of Region.t
| SLASH    of Region.t
| TIMES    of Region.t
| DOT      of Region.t
| WILD     of Region.t
| CAT      of Region.t

  (* Keywords *)

| And        of Region.t  (* "and"      *)
| Begin      of Region.t  (* "begin"    *)
| BigMap     of Region.t  (* "big_map"  *)
| Block      of Region.t  (* "block"    *)
| Case       of Region.t  (* "case"     *)
| Const      of Region.t  (* "const"    *)
| Contains   of Region.t  (* "contains" *)
| Else       of Region.t  (* "else"     *)
| End        of Region.t  (* "end"      *)
| False      of Region.t  (* "False"    *)
| For        of Region.t  (* "for"      *)
| From       of Region.t  (* "from"     *)
| Function   of Region.t  (* "function" *)
| If         of Region.t  (* "if"       *)
| In         of Region.t  (* "in"       *)
| Is         of Region.t  (* "is"       *)
| List       of Region.t  (* "list"     *)
| Map        of Region.t  (* "map"      *)
| Mod        of Region.t  (* "mod"      *)
| Nil        of Region.t  (* "nil"      *)
| Not        of Region.t  (* "not"      *)
| Of         of Region.t  (* "of"       *)
| Or         of Region.t  (* "or"       *)
| Patch      of Region.t  (* "patch"    *)
| Record     of Region.t  (* "record"   *)
| Remove     of Region.t  (* "remove"   *)
| Set        of Region.t  (* "set"      *)
| Skip       of Region.t  (* "skip"     *)
| Then       of Region.t  (* "then"     *)
| To         of Region.t  (* "to"       *)
| True       of Region.t  (* "True"     *)
| Type       of Region.t  (* "type"     *)
| Unit       of Region.t  (* "Unit"     *)
| Var        of Region.t  (* "var"      *)
| While      of Region.t  (* "while"    *)
| With       of Region.t  (* "with"     *)

  (* Data constructors *)

| C_None  of Region.t  (* "None"  *)
| C_Some  of Region.t  (* "Some"  *)

  (* Virtual tokens *)

| EOF of Region.t


type token = t

let proj_token = function
  (* Literals *)

  String Region.{region; value} ->
    region, sprintf "String %s" value

| Bytes Region.{region; value = s,b} ->
    region,
    sprintf "Bytes (\"%s\", \"0x%s\")"
      s (Hex.show b)

| Int Region.{region; value = s,n} ->
    region, sprintf "Int (\"%s\", %s)" s (Z.to_string n)

| Nat Region.{region; value = s,n} ->
    region, sprintf "Nat (\"%s\", %s)" s (Z.to_string n)

| Mutez Region.{region; value = s,n} ->
    region, sprintf "Mutez (\"%s\", %s)" s (Z.to_string n)

| Ident Region.{region; value} ->
    region, sprintf "Ident \"%s\"" value

| Constr Region.{region; value} ->
    region, sprintf "Constr \"%s\"" value

  (* Symbols *)

| SEMI     region -> region, "SEMI"
| COMMA    region -> region, "COMMA"
| LPAR     region -> region, "LPAR"
| RPAR     region -> region, "RPAR"
| LBRACE   region -> region, "LBRACE"
| RBRACE   region -> region, "RBRACE"
| LBRACKET region -> region, "LBRACKET"
| RBRACKET region -> region, "RBRACKET"
| CONS     region -> region, "CONS"
| VBAR     region -> region, "VBAR"
| ARROW    region -> region, "ARROW"
| ASS      region -> region, "ASS"
| EQ       region -> region, "EQ"
| COLON    region -> region, "COLON"
| LT       region -> region, "LT"
| LE       region -> region, "LE"
| GT       region -> region, "GT"
| GE       region -> region, "GE"
| NE       region -> region, "NE"
| PLUS     region -> region, "PLUS"
| MINUS    region -> region, "MINUS"
| SLASH    region -> region, "SLASH"
| TIMES    region -> region, "TIMES"
| DOT      region -> region, "DOT"
| WILD     region -> region, "WILD"
| CAT      region -> region, "CAT"

  (* Keywords *)

| And        region -> region, "And"
| Begin      region -> region, "Begin"
| BigMap     region -> region, "BigMap"
| Block      region -> region, "Block"
| Case       region -> region, "Case"
| Const      region -> region, "Const"
| Contains   region -> region, "Contains"
| Else       region -> region, "Else"
| End        region -> region, "End"
| False      region -> region, "False"
| For        region -> region, "For"
| From       region -> region, "From"
| Function   region -> region, "Function"
| If         region -> region, "If"
| In         region -> region, "In"
| Is         region -> region, "Is"
| List       region -> region, "List"
| Map        region -> region, "Map"
| Mod        region -> region, "Mod"
| Nil        region -> region, "Nil"
| Not        region -> region, "Not"
| Of         region -> region, "Of"
| Or         region -> region, "Or"
| Patch      region -> region, "Patch"
| Record     region -> region, "Record"
| Remove     region -> region, "Remove"
| Set        region -> region, "Set"
| Skip       region -> region, "Skip"
| Then       region -> region, "Then"
| To         region -> region, "To"
| True       region -> region, "True"
| Type       region -> region, "Type"
| Unit       region -> region, "Unit"
| Var        region -> region, "Var"
| While      region -> region, "While"
| With       region -> region, "With"

  (* Data *)

| C_None  region -> region, "C_None"
| C_Some  region -> region, "C_Some"

  (* Virtual tokens *)

| EOF region -> region, "EOF"


let to_lexeme = function
  (* Literals *)

  String s  -> String.escaped s.Region.value
| Bytes b   -> fst b.Region.value
| Int i
| Nat i
| Mutez i     -> fst i.Region.value
| Ident id
| Constr id -> id.Region.value

  (* Symbols *)

| SEMI     _ -> ";"
| COMMA    _ -> ","
| LPAR     _ -> "("
| RPAR     _ -> ")"
| LBRACE   _ -> "{"
| RBRACE   _ -> "}"
| LBRACKET _ -> "["
| RBRACKET _ -> "]"
| CONS     _ -> "#"
| VBAR     _ -> "|"
| ARROW    _ -> "->"
| ASS      _ -> ":="
| EQ       _ -> "="
| COLON    _ -> ":"
| LT       _ -> "<"
| LE       _ -> "<="
| GT       _ -> ">"
| GE       _ -> ">="
| NE       _ -> "=/="
| PLUS     _ -> "+"
| MINUS    _ -> "-"
| SLASH    _ -> "/"
| TIMES    _ -> "*"
| DOT      _ -> "."
| WILD     _ -> "_"
| CAT      _ -> "^"

  (* Keywords *)

| And        _ -> "and"
| Begin      _ -> "begin"
| BigMap     _ -> "big_map"
| Block      _ -> "block"
| Case       _ -> "case"
| Const      _ -> "const"
| Contains   _ -> "contains"
| Else       _ -> "else"
| End        _ -> "end"
| False      _ -> "False"
| For        _ -> "for"
| From       _ -> "from"
| Function   _ -> "function"
| If         _ -> "if"
| In         _ -> "in"
| Is         _ -> "is"
| List       _ -> "list"
| Map        _ -> "map"
| Mod        _ -> "mod"
| Nil        _ -> "nil"
| Not        _ -> "not"
| Of         _ -> "of"
| Or         _ -> "or"
| Patch      _ -> "patch"
| Record     _ -> "record"
| Remove     _ -> "remove"
| Set        _ -> "set"
| Skip       _ -> "skip"
| Then       _ -> "then"
| To         _ -> "to"
| True       _ -> "True"
| Type       _ -> "type"
| Unit       _ -> "Unit"
| Var        _ -> "var"
| While      _ -> "while"
| With       _ -> "with"

  (* Data constructors *)

| C_None  _ -> "None"
| C_Some  _ -> "Some"

  (* Virtual tokens *)

| EOF _ -> ""


let to_string token ?(offsets=true) mode =
  let region, val_str = proj_token token in
  let reg_str = region#compact ~offsets mode
  in sprintf "%s: %s" reg_str val_str

let to_region token = proj_token token |> fst

(* LEXIS *)

let keywords = [
  (fun reg -> And        reg);
  (fun reg -> Begin      reg);
  (fun reg -> BigMap     reg);
  (fun reg -> Block      reg);
  (fun reg -> Case       reg);
  (fun reg -> Const      reg);
  (fun reg -> Contains   reg);
  (fun reg -> Else       reg);
  (fun reg -> End        reg);
  (fun reg -> For        reg);
  (fun reg -> From       reg);
  (fun reg -> Function   reg);
  (fun reg -> False      reg);
  (fun reg -> If         reg);
  (fun reg -> In         reg);
  (fun reg -> Is         reg);
  (fun reg -> List       reg);
  (fun reg -> Map        reg);
  (fun reg -> Mod        reg);
  (fun reg -> Nil        reg);
  (fun reg -> Not        reg);
  (fun reg -> C_None     reg);
  (fun reg -> Of         reg);
  (fun reg -> Or         reg);
  (fun reg -> Patch      reg);
  (fun reg -> Record     reg);
  (fun reg -> Remove     reg);
  (fun reg -> Set        reg);
  (fun reg -> Skip       reg);
  (fun reg -> Then       reg);
  (fun reg -> To         reg);
  (fun reg -> True       reg);
  (fun reg -> Type       reg);
  (fun reg -> Unit       reg);
  (fun reg -> Var        reg);
  (fun reg -> While      reg);
  (fun reg -> With       reg)
]

let reserved =
  let open SSet in
  empty |> add "args"

let constructors = [
  (fun reg -> False  reg);
  (fun reg -> True   reg);
  (fun reg -> Unit   reg);
  (fun reg -> C_None reg);
  (fun reg -> C_Some reg)
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

type kwd_err = Invalid_keyword

let mk_kwd ident region =
  match SMap.find_opt ident lexicon.kwd with
    Some mk_kwd -> Ok (mk_kwd region)
  |        None -> Error Invalid_keyword

(* Identifiers *)

type ident_err = Reserved_name

(* END HEADER *)
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

(* Smart constructors (injections) *)

let mk_string lexeme region = String Region.{region; value=lexeme}

let mk_bytes lexeme region =
  let norm = Str.(global_replace (regexp "_") "" lexeme) in
  let value = lexeme, `Hex norm
  in Bytes Region.{region; value}

type int_err = Non_canonical_zero

let mk_int lexeme region =
  let z =
    Str.(global_replace (regexp "_") "" lexeme) |> Z.of_string
  in if   Z.equal z Z.zero && lexeme <> "0"
     then Error Non_canonical_zero
     else Ok (Int Region.{region; value = lexeme,z})

type nat_err =
  Invalid_natural
| Non_canonical_zero_nat

let mk_nat lexeme region =
  match (String.index_opt lexeme 'n') with
  | None -> Error Invalid_natural
  | Some _ -> (
    let z =
      Str.(global_replace (regexp "_") "" lexeme) |>
      Str.(global_replace (regexp "n") "") |>
      Z.of_string in
    if Z.equal z Z.zero && lexeme <> "0n"
    then Error Non_canonical_zero_nat
    else Ok (Nat Region.{region; value = lexeme,z})
  )

let mk_mutez lexeme region =
  let z =
    Str.(global_replace (regexp "_") "" lexeme) |>
    Str.(global_replace (regexp "mutez") "") |>
    Z.of_string in
  if Z.equal z Z.zero && lexeme <> "0mutez"
  then Error Non_canonical_zero
  else Ok (Mutez Region.{region; value = lexeme, z})

let eof region = EOF region

type sym_err = Invalid_symbol

let mk_sym lexeme region =
  match lexeme with
  (* Lexemes in common with all concrete syntaxes *)
    ";"   -> Ok (SEMI     region)
  | ","   -> Ok (COMMA    region)
  | "("   -> Ok (LPAR     region)
  | ")"   -> Ok (RPAR     region)
  | "["   -> Ok (LBRACKET region)
  | "]"   -> Ok (RBRACKET region)
  | "{"   -> Ok (LBRACE   region)
  | "}"   -> Ok (RBRACE   region)
  | "="   -> Ok (EQ       region)
  | ":"   -> Ok (COLON    region)
  | "|"   -> Ok (VBAR     region)
  | "->"  -> Ok (ARROW    region)
  | "."   -> Ok (DOT      region)
  | "_"   -> Ok (WILD     region)
  | "^"   -> Ok (CAT      region)
  | "+"   -> Ok (PLUS     region)
  | "-"   -> Ok (MINUS    region)
  | "*"   -> Ok (TIMES    region)
  | "/"   -> Ok (SLASH    region)
  | "<"   -> Ok (LT       region)
  | "<="  -> Ok (LE       region)
  | ">"   -> Ok (GT       region)
  | ">="  -> Ok (GE       region)

  (* Lexemes specific to PascaLIGO *)
  | "=/=" -> Ok (NE       region)
  | "#"   -> Ok (CONS     region)
  | ":="  -> Ok (ASS      region)

  (* Invalid lexemes *)
  |     _ -> Error Invalid_symbol


(* Identifiers *)

let mk_ident lexeme region =
  Lexing.from_string lexeme |> scan_ident region lexicon

(* Constructors *)

let mk_constr lexeme region =
  Lexing.from_string lexeme |> scan_constr region lexicon

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
  And        _
| Begin      _
| BigMap     _
| Block      _
| Case       _
| Const      _
| Contains   _
| Else       _
| End        _
| False      _
| For        _
| From       _
| Function   _
| If         _
| In         _
| Is         _
| List       _
| Map        _
| Mod        _
| Nil        _
| Not        _
| Of         _
| Or         _
| Patch      _
| Record     _
| Remove     _
| Set        _
| Skip       _
| Then       _
| To         _
| True       _
| Type       _
| Unit       _
| Var        _
| While      _
| With       _ -> true
|            _ -> false

let is_constr = function
  Constr  _
| C_None  _
| C_Some  _ -> true
|         _ -> false

let is_sym = function
  SEMI     _
| COMMA    _
| LPAR     _
| RPAR     _
| LBRACE   _
| RBRACE   _
| LBRACKET _
| RBRACKET _
| CONS     _
| VBAR     _
| ARROW    _
| ASS      _
| EQ       _
| COLON    _
| LT       _
| LE       _
| GT       _
| GE       _
| NE       _
| PLUS     _
| MINUS    _
| SLASH    _
| TIMES    _
| DOT      _
| WILD     _
| CAT      _ -> true
|          _ -> false

let is_eof = function EOF _ -> true | _ -> false

(* END TRAILER *)
}
