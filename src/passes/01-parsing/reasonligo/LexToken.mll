(* ocamlex specification for ReasonLIGO *)
{
(* START OF HEADER *)

(* Dependencies *)

module Region = Simple_utils.Region
module Pos    = Simple_utils.Pos
module Markup = Lexer_shared.Markup
module SMap   = Map.Make (String)
module SSet   = Set.Make (String)

(* TOKENS *)

type lexeme = string

type t =
  (* Identifiers, labels, numbers and strings *)

| Ident    of string Region.reg
| Constr   of string Region.reg
| Int      of (string * Z.t) Region.reg
| Nat      of (string * Z.t) Region.reg
| Mutez    of (string * Z.t) Region.reg
| String   of string Region.reg
| Verbatim of string Region.reg
| Bytes    of (string * Hex.t) Region.reg
| Attr     of string Region.reg
| Lang     of lexeme Region.reg Region.reg

  (* Symbols *)

| CAT of Region.t (* "++"  *)

  (* Arithmetics *)

| MINUS   of Region.t (* "-" *)
| PLUS    of Region.t (* "+" *)
| SLASH   of Region.t (* "/" *)
| TIMES   of Region.t (* "*" *)

  (* Compounds *)

| LPAR     of Region.t (* "(" *)
| RPAR     of Region.t (* ")" *)
| LBRACKET of Region.t (* "[" *)
| RBRACKET of Region.t (* "]" *)
| LBRACE   of Region.t (* "{" *)
| RBRACE   of Region.t (* "}" *)

  (* Separators *)

| COMMA    of Region.t (* ","   *)
| SEMI     of Region.t (* ";"   *)
| VBAR     of Region.t (* "|"   *)
| COLON    of Region.t (* ":"   *)
| DOT      of Region.t (* "."   *)
| ELLIPSIS of Region.t (* "..." *)
| ARROW    of Region.t (* "=>"  *)

  (* Wildcard *)

| WILD of Region.t     (* "_" *)

  (* Comparisons *)

| EQ   of Region.t (* "="  *)
| EQEQ of Region.t (* "==" *)
| NE   of Region.t (* "!=" *)
| LT   of Region.t (* "<"  *)
| GT   of Region.t (* ">"  *)
| LE   of Region.t (* "<=" *)
| GE   of Region.t (* ">=" *)

  (* Logic *)

| BOOL_OR  of Region.t (* "||" *)
| BOOL_AND of Region.t (* "&&" *)
| NOT      of Region.t (* ! *)

  (* Keywords *)

| Else   of Region.t
| False  of Region.t
| If     of Region.t
| Let    of Region.t
| Mod    of Region.t
| Or     of Region.t
| Rec    of Region.t
| Switch of Region.t
| True   of Region.t
| Type   of Region.t

  (* Data constructors *)

| C_None  of Region.t  (* "None"  *)
| C_Some  of Region.t  (* "Some"  *)

(* Virtual tokens *)

| EOF of Region.t (* End of file *)


(* Projections *)

let sprintf = Printf.sprintf

type token = t

let proj_token = function
  (* Literals *)

  String Region.{region; value} ->
    region, sprintf "String %S" value
| Verbatim Region.{region; value} ->
    region, sprintf "Verbatim %S" value
| Bytes Region.{region; value = s,b} ->
    region,
    sprintf "Bytes (%S, \"0x%s\")" s (Hex.show b)
| Int Region.{region; value = s,n} ->
    region, sprintf "Int (%S, %s)" s (Z.to_string n)
| Nat Region.{region; value = s,n} ->
    region, sprintf "Nat (%S, %s)" s (Z.to_string n)
| Mutez Region.{region; value = s,n} ->
    region, sprintf "Mutez (%S, %s)" s (Z.to_string n)
| Ident Region.{region; value} ->
    region, sprintf "Ident %S" value
| Constr Region.{region; value} ->
    region, sprintf "Constr %S" value
| Attr Region.{region; value} ->
   region, sprintf "Attr %S" value
| Lang Region.{region; value} ->
   region, sprintf "Lang %S" (value.Region.value)

  (* Symbols *)

| CAT      region -> region, "CAT"
| MINUS    region -> region, "MINUS"
| PLUS     region -> region, "PLUS"
| SLASH    region -> region, "SLASH"
| TIMES    region -> region, "TIMES"
| LPAR     region -> region, "LPAR"
| RPAR     region -> region, "RPAR"
| LBRACKET region -> region, "LBRACKET"
| RBRACKET region -> region, "RBRACKET"
| LBRACE   region -> region, "LBRACE"
| RBRACE   region -> region, "RBRACE"
| COMMA    region -> region, "COMMA"
| SEMI     region -> region, "SEMI"
| VBAR     region -> region, "VBAR"
| COLON    region -> region, "COLON"
| DOT      region -> region, "DOT"
| ELLIPSIS region -> region, "ELLIPSIS"
| WILD     region -> region, "WILD"
| EQ       region -> region, "EQ"
| EQEQ     region -> region, "EQEQ"
| NE       region -> region, "NE"
| LT       region -> region, "LT"
| GT       region -> region, "GT"
| LE       region -> region, "LE"
| GE       region -> region, "GE"
| ARROW    region -> region, "ARROW"
| NOT      region -> region, "NOT"
| BOOL_OR  region -> region, "BOOL_OR"
| BOOL_AND region -> region, "BOOL_AND"
| Else     region -> region, "Else"
| False    region -> region, "False"
| If       region -> region, "If"
| Let      region -> region, "Let"
| Rec      region -> region, "Rec"
| Switch   region -> region, "Switch"
| Mod      region -> region, "Mod"
| Or       region -> region, "Or"
| True     region -> region, "True"
| Type     region -> region, "Type"
| C_None   region -> region, "C_None"
| C_Some   region -> region, "C_Some"
| EOF      region -> region, "EOF"

let to_lexeme = function
  (* Literals *)

  String s   -> s.Region.value
| Verbatim v -> String.escaped v.Region.value
| Bytes b    -> fst b.Region.value
| Int i
| Nat i
| Mutez i    -> fst i.Region.value
| Ident  id  -> id.Region.value
| Constr id  -> id.Region.value
| Attr a     -> a.Region.value
| Lang lang  -> Region.(lang.value.value)

  (* Symbols *)

| CAT      _ -> "++"
| MINUS    _ -> "-"
| PLUS     _ -> "+"
| SLASH    _ -> "/"
| TIMES    _ -> "*"
| LPAR     _ -> "("
| RPAR     _ -> ")"
| LBRACKET _ -> "["
| RBRACKET _ -> "]"
| LBRACE   _ -> "{"
| RBRACE   _ -> "}"
| COMMA    _ -> ","
| SEMI     _ -> ";"
| VBAR     _ -> "|"
| COLON    _ -> ":"
| DOT      _ -> "."
| ELLIPSIS _ -> "..."
| WILD     _ -> "_"
| EQ       _ -> "="
| EQEQ     _ -> "=="
| NE       _ -> "!="
| LT       _ -> "<"
| GT       _ -> ">"
| LE       _ -> "<="
| GE       _ -> ">="
| ARROW    _ -> "=>"
| BOOL_OR  _ -> "||"
| BOOL_AND _ -> "&&"
| NOT      _ -> "!"

  (* Keywords *)

| Else    _ -> "else"
| False   _ -> "false"
| If      _ -> "if"
| Let     _ -> "let"
| Mod     _ -> "mod"
| Or      _ -> "or"
| Rec     _ -> "rec"
| Switch  _ -> "switch"
| True    _ -> "true"
| Type    _ -> "type"

(* Data constructors *)

| C_None  _ -> "None"
| C_Some  _ -> "Some"

(* Virtual tokens *)

| EOF _ -> ""

(* CONVERSIONS *)

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
type  attr_err = Invalid_attribute
type   kwd_err = Invalid_keyword

(* LEXIS *)

let keywords = [
  (fun reg -> Else   reg);
  (fun reg -> False  reg);
  (fun reg -> If     reg);
  (fun reg -> Let    reg);
  (fun reg -> Rec    reg);
  (fun reg -> Switch reg);
  (fun reg -> Mod    reg);
  (fun reg -> Or     reg);
  (fun reg -> True   reg);
  (fun reg -> Type   reg)]

let reserved =
  let open SSet in
  empty
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
    |> add "lazy"
    |> add "lsl"
    |> add "lsr"
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

let mk_verbatim lexeme region = Verbatim Region.{region; value=lexeme}

let mk_bytes lexeme region =
  let norm = Str.(global_replace (regexp "_") "" lexeme) in
  let value = lexeme, `Hex norm
  in Bytes Region.{region; value}

let mk_int lexeme region =
  let z = Str.(global_replace (regexp "_") "" lexeme)
          |> Z.of_string in
  if   Z.equal z Z.zero && lexeme <> "0"
  then Error Non_canonical_zero
  else Ok (Int Region.{region; value = lexeme, z})

let mk_nat lexeme region =
  let z = Str.(global_replace (regexp "_") "" lexeme) |>
            Str.(global_replace (regexp "n") "") |>
            Z.of_string in
  if   Z.equal z Z.zero && lexeme <> "0n"
  then Error Non_canonical_zero_nat
  else Ok (Nat Region.{region; value = lexeme, z})

let mk_mutez lexeme region =
  let z = Str.(global_replace (regexp "_") "" lexeme) |>
            Str.(global_replace (regexp "mutez") "") |>
            Z.of_string in
  if   Z.equal z Z.zero && lexeme <> "0mutez"
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

  | "..." ->  Ok (ELLIPSIS region)
  | "=>"  ->  Ok (ARROW    region)
  | "=="  ->  Ok (EQEQ     region)
  | "!"   ->  Ok (NOT      region)
  | "++"  ->  Ok (CAT      region)

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

(* Attributes *)

let mk_attr header lexeme region =
  if header = "[@" then
    Ok (Attr Region.{value=lexeme; region})
  else Error Invalid_attribute

(* Language injection *)

let mk_lang lang region = Lang Region.{value=lang; region}

(* Predicates *)

let is_string = function String _ -> true | _ -> false
let is_bytes  = function Bytes  _ -> true | _ -> false
let is_int    = function Int    _ -> true | _ -> false
let is_ident  = function Ident  _ -> true | _ -> false
let is_eof    = function EOF    _ -> true | _ -> false
let is_minus  = function MINUS  _ -> true | _ -> false

(* Errors *)

type error =
  Odd_lengthed_bytes
| Missing_break
| Negative_byte_sequence

let error_to_string = function
  Odd_lengthed_bytes ->
    "The length of the byte sequence is an odd number.\n\
     Hint: Add or remove a digit."
| Missing_break ->
    "Missing break.\n\
     Hint: Insert some space."
| Negative_byte_sequence ->
   "Negative byte sequence.\n\
    Hint: Remove the leading minus sign."

exception Error of error Region.reg

let format_error ?(offsets=true) mode Region.{region; value} ~file =
  let msg = error_to_string value
  and reg = region#to_string ~file ~offsets mode in
  let value = sprintf "Lexical error %s:\n%s\n" reg msg
  in Region.{value; region}

let fail region value = raise (Error Region.{region; value})

let check_right_context token next_token buffer : unit =
  let pos    = (to_region token)#stop in
  let region = Region.make ~start:pos ~stop:pos in
  match next_token buffer with
    None -> ()
  | Some (markup, next) ->
      if   is_minus token && is_bytes next
      then let region =
             Region.cover (to_region token) (to_region next)
           in fail region Negative_byte_sequence
      else
        match markup with
          [] ->
            if   is_int token
            then if   is_string next || is_ident next
                 then fail region Missing_break
                 else ()
            else
              if   is_string token
              then if   is_int next || is_bytes next || is_ident next
                   then fail region Missing_break
                   else ()
              else
                if   is_bytes token
                then if   is_string next || is_ident next
                     then fail region Missing_break
                     else if   is_int next
                          then fail region Odd_lengthed_bytes
                          else ()
                else ()
        | _::_ -> ()

(* END TRAILER *)
}
