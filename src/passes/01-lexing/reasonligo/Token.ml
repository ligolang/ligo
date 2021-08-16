(* Token specification for CameLIGO *)

(* Vendor dependencies *)

module Region    = Simple_utils.Region
module Markup    = LexerLib.Markup
module Directive = LexerLib.Directive

(* Utility modules *)

module SMap = Map.Make (String)
module SSet = Set.Make (String)

(* TOKENS *)

type lexeme = string

module T =
  struct
    type t =
      (* Preprocessing directives *)

      Directive of Directive.t

      (* Literals *)

    | String   of lexeme Region.reg
    | Verbatim of lexeme Region.reg
    | Bytes    of (lexeme * Hex.t) Region.reg
    | Int      of (lexeme * Z.t) Region.reg
    | Nat      of (lexeme * Z.t) Region.reg
    | Mutez    of (lexeme * Z.t) Region.reg
    | Ident    of lexeme Region.reg
    | UIdent   of lexeme Region.reg
    | Lang     of lexeme Region.reg Region.reg
    | Attr     of string Region.reg

    (* Symbols *)

    | PLUS2    of Region.t (* "++"  *)
    | MINUS    of Region.t (* "-"   *)
    | PLUS     of Region.t (* "+"   *)
    | SLASH    of Region.t (* "/"   *)
    | TIMES    of Region.t (* "*"   *)
    | LPAR     of Region.t (* "("   *)
    | RPAR     of Region.t (* ")"   *)
    | LBRACKET of Region.t (* "["   *)
    | RBRACKET of Region.t (* "]"   *)
    | LBRACE   of Region.t (* "{"   *)
    | RBRACE   of Region.t (* "}"   *)
    | COMMA    of Region.t (* ","   *)
    | SEMI     of Region.t (* ";"   *)
    | VBAR     of Region.t (* "|"   *)
    | COLON    of Region.t (* ":"   *)
    | DOT      of Region.t (* "."   *)
    | ELLIPSIS of Region.t (* "..." *)
    | ARROW    of Region.t (* "=>"  *)
    | WILD     of Region.t (* "_"   *)
    | EQ       of Region.t (* "="   *)
    | EQ2      of Region.t (* "=="  *)
    | NE       of Region.t (* "!="  *)
    | LT       of Region.t (* "<"   *)
    | GT       of Region.t (* ">"   *)
    | LE       of Region.t (* "<="  *)
    | GE       of Region.t (* ">="  *)
    | BOOL_OR  of Region.t (* "||"  *)
    | BOOL_AND of Region.t (* "&&"  *)
    | NOT      of Region.t (* "!"   *)
    | QUOTE    of Region.t (* "'"   *)

    (* Keywords *)

    | Else   of Region.t  (* else   *)
    | If     of Region.t  (* if     *)
    | Let    of Region.t  (* let    *)
    | Mod    of Region.t  (* mod    *)
    | Land   of Region.t  (* land   *)
    | Lor    of Region.t  (* lor    *)
    | Lxor   of Region.t  (* lxor   *)
    | Lsl    of Region.t  (* lsl    *)
    | Lsr    of Region.t  (* lsr    *)
    | Or     of Region.t  (* or     *)
    | Rec    of Region.t  (* rec    *)
    | Switch of Region.t  (* switch *)
    | Type   of Region.t  (* type   *)
    | Module of Region.t  (* module *)

    (* Virtual tokens *)

    | ES6FUN of Region.t

    (* End-Of-File *)

    | EOF of Region.t


    (* Unlexing the tokens *)

    let gen_sym prefix =
      let count = ref 0 in
      fun () -> incr count;
             prefix ^ string_of_int !count

    let id_sym   = gen_sym "id"
    and ctor_sym = gen_sym "C"

    let concrete = function
        (* Identifiers, labels, numbers and strings *)

      "Ident"   -> id_sym ()
    | "UIdent"  -> ctor_sym ()
    | "Int"      -> "1"
    | "Nat"      -> "1n"
    | "Mutez"    -> "1mutez"
    | "String"   -> "\"a string\""
    | "Verbatim" -> "{|verbatim|}"
    | "Bytes"    -> "0xAA"
    | "Attr"     -> "[@attr]"
    | "Lang"     -> "[%Michelson"

    (* Symbols *)

    | "PLUS2" -> "++"

    (* Arithmetics *)

    | "MINUS"   -> "-"
    | "PLUS"    -> "+"
    | "SLASH"   -> "/"
    | "TIMES"   -> "*"

    (* Compounds *)

    | "LPAR"     -> "("
    | "RPAR"     -> ")"
    | "LBRACKET" -> "["
    | "RBRACKET" -> "]"
    | "LBRACE"   -> "{"
    | "RBRACE"   -> "}"

    (* Separators *)

    | "COMMA"    -> ","
    | "SEMI"     -> ";"
    | "VBAR"     -> "|"
    | "COLON"    -> ":"
    | "DOT"      -> "."
    | "ELLIPSIS" -> "..."
    | "ARROW"    -> "=>"

    (* Wildcard *)

    | "WILD" ->     "_"

    (* Comparisons *)

    | "EQ"   -> "="
    | "EQ2"  -> "=="
    | "NE"   -> "!="
    | "LT"   -> "<"
    | "GT"   -> ">"
    | "LE"   -> "<="
    | "GE"   -> ">="

    (* Logic *)

    | "BOOL_OR"  -> "||"
    | "BOOL_AND" -> "&&"
    | "NOT"      -> "!"

    (* Keywords *)

    | "Else"    -> "else"
    | "If"      -> "if"
    | "Let"     -> "let"
    | "Mod"     -> "mod"
    | "Land"    -> "land"
    | "Lor"     -> "lor"
    | "Lxor"    -> "lxor"
    | "Lsl"     -> "lsl"
    | "Lsr"     -> "lsr"
    | "Or"      -> "or"
    | "Rec"     -> "rec"
    | "Switch"  -> "switch"
    | "Type"    -> "type"
    | "Module"  -> "module"

    | "QUOTE"   -> "'"

    (* End-Of-File *)

    | "EOF" -> ""

    (* This case should not happen! *)

    | _  -> "\\Unknown" (* Backslash meant to trigger an error *)

    (* Projections *)

    let sprintf = Printf.sprintf

    type token = t

    let proj_token = function
        (* Preprocessing directives *)

      Directive d ->
        Directive.project d

      (* Literals *)

    | String Region.{region; value} ->
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
    | UIdent Region.{region; value} ->
        region, sprintf "UIdent %S" value
    | Lang Region.{region; value} ->
        region, sprintf "Lang %S" (value.Region.value)
    | Attr Region.{region; value} ->
        region, sprintf "Attr %S" value

    (* Symbols *)

    | PLUS2    region -> region, "PLUS2"
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
    | EQ2      region -> region, "EQ2"
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
    | If       region -> region, "If"
    | Let      region -> region, "Let"
    | Rec      region -> region, "Rec"
    | Switch   region -> region, "Switch"
    | Mod      region -> region, "Mod"
    | Land     region -> region, "Land"
    | Lor      region -> region, "Lor"
    | Lxor     region -> region, "Lxor"
    | Lsl      region -> region, "Lsl"
    | Lsr      region -> region, "Lsr"
    | Or       region -> region, "Or"
    | Type     region -> region, "Type"
    | Module   region -> region, "Module"
    | QUOTE    region -> region, "QUOTE"

    (* Virtual tokens *)

    | ES6FUN region -> region, "ES6FUN"

    (* End-Of-File *)

    | EOF region -> region, "EOF"


    let to_lexeme = function
      (* Directives *)

      Directive d -> Directive.to_lexeme d

      (* Literals *)

    | String s   -> sprintf "%S" (String.escaped s.Region.value)
    | Verbatim v -> String.escaped v.Region.value
    | Bytes b    -> fst b.Region.value
    | Int i
    | Nat i
    | Mutez i    -> fst i.Region.value
    | Ident id   -> id.Region.value
    | UIdent id  -> id.Region.value
    | Attr a     -> sprintf "[@%s]" a.Region.value
    | Lang lang  -> Region.(lang.value.value)

    (* Symbols *)

    | PLUS2    _ -> "++"
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
    | EQ2      _ -> "=="
    | NE       _ -> "!="
    | LT       _ -> "<"
    | GT       _ -> ">"
    | LE       _ -> "<="
    | GE       _ -> ">="
    | ARROW    _ -> "=>"
    | BOOL_OR  _ -> "||"
    | BOOL_AND _ -> "&&"
    | NOT      _ -> "!"
    | QUOTE    _ -> "'"

    (* Keywords *)

    | Else    _ -> "else"
    | If      _ -> "if"
    | Let     _ -> "let"
    | Mod     _ -> "mod"
    | Land    _ -> "land"
    | Lor     _ -> "lor"
    | Lxor    _ -> "lxor"
    | Lsl     _ -> "lsl"
    | Lsr     _ -> "lsr"
    | Or      _ -> "or"
    | Rec     _ -> "rec"
    | Switch  _ -> "switch"
    | Type    _ -> "type"
    | Module  _ -> "module"

    (* Virtual tokens *)

    | ES6FUN _ -> ""

    (* End-Of-File *)

    | EOF _ -> ""


    (* CONVERSIONS *)

    let to_string ~offsets mode token =
      let region, val_str = proj_token token in
      let reg_str = region#compact ~offsets mode
      in sprintf "%s: %s" reg_str val_str

    let to_region token = proj_token token |> fst

    (* SMART CONSTRUCTORS *)

    (* Keywords *)

    let keywords = [
      (fun reg -> Else   reg);
      (fun reg -> If     reg);
      (fun reg -> Let    reg);
      (fun reg -> Rec    reg);
      (fun reg -> Switch reg);
      (fun reg -> Mod    reg);
      (fun reg -> Land   reg);
      (fun reg -> Lor    reg);
      (fun reg -> Lxor   reg);
      (fun reg -> Lsl    reg);
      (fun reg -> Lsr    reg);
      (fun reg -> Or     reg);
      (fun reg -> Type   reg);
      (fun reg -> Module reg)
    ]

    let keywords =
      let add map (key, value) = SMap.add key value map in
      let apply map mk_kwd =
        add map (to_lexeme (mk_kwd Region.ghost), mk_kwd)
      in List.fold_left apply SMap.empty keywords

    type kwd_err = Invalid_keyword

    let mk_kwd ident region =
      match SMap.find_opt ident keywords with
        Some mk_kwd -> Ok (mk_kwd region)
      |        None -> Error Invalid_keyword

    (* Strings *)

    let mk_string lexeme region =
      String Region.{region; value=lexeme}

    let mk_verbatim lexeme region =
      Verbatim Region.{region; value=lexeme}

    (* Bytes *)

    let mk_bytes lexeme region =
      let norm = Str.(global_replace (regexp "_") "" lexeme) in
      let value = lexeme, `Hex norm
      in Bytes Region.{region; value}

    (* Numerical values *)

    type int_err = Non_canonical_zero

    let mk_int lexeme region =
      let z =
        Str.(global_replace (regexp "_") "" lexeme) |> Z.of_string
      in if   Z.equal z Z.zero && lexeme <> "0"
         then Error Non_canonical_zero
         else Ok (Int Region.{region; value = lexeme,z})

    type nat_err =
      Invalid_natural
    | Unsupported_nat_syntax
    | Non_canonical_zero_nat

    let mk_nat lexeme region =
      match String.index_opt lexeme 'n' with
        None -> Error Invalid_natural
      | Some _ ->
          let z =
            Str.(global_replace (regexp "_") "" lexeme) |>
              Str.(global_replace (regexp "n") "") |>
              Z.of_string in
          if   Z.equal z Z.zero && lexeme <> "0n"
          then Error Non_canonical_zero_nat
          else Ok (Nat Region.{region; value = lexeme,z})

    type mutez_err =
        Unsupported_mutez_syntax
      | Non_canonical_zero_tez

    let mk_mutez lexeme region =
      let z = Str.(global_replace (regexp "_") "" lexeme) |>
                Str.(global_replace (regexp "mutez") "") |>
                Z.of_string in
      if   Z.equal z Z.zero && lexeme <> "0mutez"
      then Error Non_canonical_zero_tez
      else Ok (Mutez Region.{region; value = lexeme, z})

    (* End-Of-File *)

    let mk_eof region = EOF region

    (* Symbols *)

    type sym_err = Invalid_symbol of string

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
      | "."   -> Ok (DOT      region)
      | "_"   -> Ok (WILD     region)
      | "+"   -> Ok (PLUS     region)
      | "-"   -> Ok (MINUS    region)
      | "*"   -> Ok (TIMES    region)
      | "/"   -> Ok (SLASH    region)
      | "<"   -> Ok (LT       region)
      | "<="  -> Ok (LE       region)
      | ">"   -> Ok (GT       region)
      | ">="  -> Ok (GE       region)

      (* Symbols specific to ReasonLIGO *)

      | "!="   -> Ok (NE       region)
      | "||"   -> Ok (BOOL_OR  region)
      | "&&"   -> Ok (BOOL_AND region)
      | "..." ->  Ok (ELLIPSIS region)
      | "=>"  ->  Ok (ARROW    region)
      | "=="  ->  Ok (EQ2      region)
      | "!"   ->  Ok (NOT      region)
      | "++"  ->  Ok (PLUS2    region)

      | "'"   -> Ok (QUOTE    region)

      (* Invalid symbols *)

      | s ->  Error (Invalid_symbol s)

    (* Identifiers *)

    let mk_ident value region =
      match SMap.find_opt value keywords with
        Some mk_kwd -> mk_kwd region
      |        None -> Ident Region.{region; value}

    (* Constructors/Modules *)

    let mk_uident value region = UIdent Region.{region; value}

    (* Attributes *)

    let mk_attr lexeme region = Attr Region.{value=lexeme; region}

    (* Code injection *)

    type lang_err = Unsupported_lang_syntax

    let mk_lang lang region = Ok (Lang Region.{value=lang; region})

    (* PREDICATES *)

    let is_eof = function EOF _ -> true | _ -> false

    let support_string_delimiter c =
      c = '"'

    let verbatim_delimiters = ("{|", "|}")

  end

include T

module type S = module type of T
