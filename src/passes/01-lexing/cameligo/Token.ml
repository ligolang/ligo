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

    | ARROW    of Region.t  (* "->" *)
    | CONS     of Region.t  (* "::" *)
    | CARET    of Region.t  (* "^"  *)
    | MINUS    of Region.t  (* "-"  *)
    | PLUS     of Region.t  (* "+"  *)
    | SLASH    of Region.t  (* "/"  *)
    | TIMES    of Region.t  (* "*"  *)
    | LPAR     of Region.t  (* "("  *)
    | RPAR     of Region.t  (* ")"  *)
    | LBRACKET of Region.t  (* "["  *)
    | RBRACKET of Region.t  (* "]"  *)
    | LBRACE   of Region.t  (* "{"  *)
    | RBRACE   of Region.t  (* "}"  *)
    | COMMA    of Region.t  (* ","  *)
    | SEMI     of Region.t  (* ";"  *)
    | VBAR     of Region.t  (* "|"  *)
    | COLON    of Region.t  (* ":"  *)
    | DOT      of Region.t  (* "."  *)
    | WILD     of Region.t  (*  "_" *)
    | EQ       of Region.t  (* "="  *)
    | NE       of Region.t  (* "<>" *)
    | LT       of Region.t  (* "<"  *)
    | GT       of Region.t  (* ">"  *)
    | LE       of Region.t  (* "<=" *)
    | GE       of Region.t  (* ">=" *)
    | BOOL_OR  of Region.t  (* "||" *)
    | BOOL_AND of Region.t  (* "&&" *)
    | QUOTE    of Region.t  (* "'"  *)

    (* Keywords *)

    | Begin     of Region.t  (* begin *)
    | Else      of Region.t  (* else  *)
    | End       of Region.t  (* end   *)
    | Fun       of Region.t  (* fun   *)
    | Rec       of Region.t  (* rec   *)
    | If        of Region.t  (* if    *)
    | In        of Region.t  (* in    *)
    | Let       of Region.t  (* let   *)
    | Match     of Region.t  (* match *)
    | Mod       of Region.t  (* mod   *)
    | Land      of Region.t  (* land  *)
    | Lor       of Region.t  (* lor   *)
    | Lxor      of Region.t  (* lxor  *)
    | Lsl       of Region.t  (* lsl   *)
    | Lsr       of Region.t  (* lsr   *)
    | Not       of Region.t  (* not   *)
    | Of        of Region.t  (* of    *)
    | Or        of Region.t  (* or    *)
    | Then      of Region.t  (* then  *)
    | Type      of Region.t  (* type  *)
    | With      of Region.t  (* with  *)
    | Module    of Region.t  (* module *)
    | Struct    of Region.t  (* struct *)

    (* End-Of-File *)

    | EOF of Region.t


    (* Unlexing the tokens *)

    let gen_sym prefix =
      let count = ref 0 in
      fun () -> incr count; prefix ^ string_of_int !count

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

    | "ARROW" ->   "->"
    | "CONS"  ->   "::"
    | "CARET" ->   "^"

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

    | "COMMA" -> ","
    | "SEMI"  -> ";"
    | "VBAR"  -> "|"
    | "COLON" -> ":"
    | "DOT"   -> "."

    (* Wildcard *)

    | "WILD" -> "_"

    (* Comparisons *)

    | "EQ" -> "="
    | "NE" -> "<>"
    | "LT" -> "<"
    | "GT" -> ">"
    | "LE" -> "<="
    | "GE" -> ">="

    | "BOOL_OR"  -> "||"
    | "BOOL_AND" -> "&&"

    | "QUOTE" -> "'"

    (* Keywords *)

    | "Begin" -> "begin"
    | "Else"  -> "else"
    | "End"   -> "end"
    | "Fun"   -> "fun"
    | "Rec"   -> "rec"
    | "If"    -> "if"
    | "In"    -> "in"
    | "Let"   -> "let"
    | "Match" -> "match"
    | "Mod"   -> "mod"
    | "Land"  -> "land"
    | "Lor"   -> "lor"
    | "Lxor"  -> "lxor"
    | "Lsl"   -> "lsl"
    | "Lsr"   -> "lsr"
    | "Not"   -> "not"
    | "Of"    -> "of"
    | "Or"    -> "or"
    | "Then"  -> "then"
    | "Type"  -> "type"
    | "With"  -> "with"
    | "Module"-> "module"
    | "Struct"-> "struct"

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

    | ARROW    region -> region, "ARROW"
    | CONS     region -> region, "CONS"
    | CARET    region -> region, "CARET"
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
    | WILD     region -> region, "WILD"
    | EQ       region -> region, "EQ"
    | NE       region -> region, "NE"
    | LT       region -> region, "LT"
    | GT       region -> region, "GT"
    | LE       region -> region, "LE"
    | GE       region -> region, "GE"
    | BOOL_OR  region -> region, "BOOL_OR"
    | BOOL_AND region -> region, "BOOL_AND"
    | QUOTE    region -> region, "QUOTE"

    (* Keywords *)

    | Begin  region -> region, "Begin"
    | Else   region -> region, "Else"
    | End    region -> region, "End"
    | Fun    region -> region, "Fun"
    | Rec    region -> region, "Rec"
    | If     region -> region, "If"
    | In     region -> region, "In"
    | Let    region -> region, "Let"
    | Match  region -> region, "Match"
    | Mod    region -> region, "Mod"
    | Land   region -> region, "Land"
    | Lor    region -> region, "Lor"
    | Lxor   region -> region, "Lxor"
    | Lsl    region -> region, "Lsl"
    | Lsr    region -> region, "Lsr"
    | Not    region -> region, "Not"
    | Of     region -> region, "Of"
    | Or     region -> region, "Or"
    | Then   region -> region, "Then"
    | Type   region -> region, "Type"
    | With   region -> region, "With"
    | Module region -> region, "Module"
    | Struct region -> region, "Struct"

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

    | ARROW    _ -> "->"
    | CONS     _ -> "::"
    | CARET    _ -> "^"
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
    | WILD     _ -> "_"
    | EQ       _ -> "="
    | NE       _ -> "<>"
    | LT       _ -> "<"
    | GT       _ -> ">"
    | LE       _ -> "<="
    | GE       _ -> ">="
    | BOOL_OR  _ -> "||"
    | BOOL_AND _ -> "&&"
    | QUOTE    _ -> "'"

    (* Keywords *)

    | Begin  _ -> "begin"
    | Else   _ -> "else"
    | End    _ -> "end"
    | Fun    _ -> "fun"
    | Rec    _ -> "rec"
    | If     _ -> "if"
    | In     _ -> "in"
    | Let    _ -> "let"
    | Match  _ -> "match"
    | Mod    _ -> "mod"
    | Land   _ -> "land"
    | Lor    _ -> "lor"
    | Lxor   _ -> "lxor"
    | Lsl    _ -> "lsl"
    | Lsr    _ -> "lsr"
    | Not    _ -> "not"
    | Of     _ -> "of"
    | Or     _ -> "or"
    | Type   _ -> "type"
    | Then   _ -> "then"
    | With   _ -> "with"
    | Module _ -> "module"
    | Struct _ -> "struct"

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
      (fun reg -> Begin     reg);
      (fun reg -> Else      reg);
      (fun reg -> End       reg);
      (fun reg -> Fun       reg);
      (fun reg -> Rec       reg);
      (fun reg -> If        reg);
      (fun reg -> In        reg);
      (fun reg -> Let       reg);
      (fun reg -> Match     reg);
      (fun reg -> Mod       reg);
      (fun reg -> Land      reg);
      (fun reg -> Lor       reg);
      (fun reg -> Lxor      reg);
      (fun reg -> Lsl       reg);
      (fun reg -> Lsr       reg);
      (fun reg -> Not       reg);
      (fun reg -> Of        reg);
      (fun reg -> Or        reg);
      (fun reg -> Then      reg);
      (fun reg -> Type      reg);
      (fun reg -> With      reg);
      (fun reg -> Module    reg);
      (fun reg -> Struct    reg)
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

      (* Lexemes specific to CameLIGO *)

      | "^"   -> Ok (CARET    region)
      | "->"  -> Ok (ARROW    region)
      | "<>"  -> Ok (NE       region)
      | "::"  -> Ok (CONS     region)
      | "||"  -> Ok (BOOL_OR  region)
      | "&&"  -> Ok (BOOL_AND region)
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
