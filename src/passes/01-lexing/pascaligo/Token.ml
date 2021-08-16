(* Token specification for PascaLIGO *)

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

    | SEMI     of Region.t  (* ";"   *)
    | COMMA    of Region.t  (* ","   *)
    | LPAR     of Region.t  (* "("   *)
    | RPAR     of Region.t  (* ")"   *)
    | LBRACE   of Region.t  (* "{"   *)
    | RBRACE   of Region.t  (* "}"   *)
    | LBRACKET of Region.t  (* "["   *)
    | RBRACKET of Region.t  (* "]"   *)
    | CONS     of Region.t  (* "#"   *)
    | VBAR     of Region.t  (* "|"   *)
    | ARROW    of Region.t  (* "->"  *)
    | ASS      of Region.t  (* ":="  *)
    | EQ       of Region.t  (* "="   *)
    | COLON    of Region.t  (* ":"   *)
    | LT       of Region.t  (* "<"   *)
    | LE       of Region.t  (* "<="  *)
    | GT       of Region.t  (* ">"   *)
    | GE       of Region.t  (* ">="  *)
    | NE       of Region.t  (* "=/=" *)
    | PLUS     of Region.t  (* "+"   *)
    | MINUS    of Region.t  (* "-"   *)
    | SLASH    of Region.t  (* "/"   *)
    | TIMES    of Region.t  (* "*"   *)
    | DOT      of Region.t  (* "."   *)
    | WILD     of Region.t  (* "_"   *)
    | CARET    of Region.t  (* "^"   *)

    (* Keywords *)

    | And        of Region.t  (* and        *)
    | Begin      of Region.t  (* begin      *)
    | BigMap     of Region.t  (* big_map    *)
    | Block      of Region.t  (* block      *)
    | Case       of Region.t  (* case       *)
    | Const      of Region.t  (* const      *)
    | Contains   of Region.t  (* contains   *)
    | Else       of Region.t  (* else       *)
    | End        of Region.t  (* end        *)
    | For        of Region.t  (* for        *)
    | From       of Region.t  (* from       *)
    | Function   of Region.t  (* function   *)
    | Recursive  of Region.t  (* recursive  *)
    | If         of Region.t  (* if         *)
    | In         of Region.t  (* in         *)
    | Is         of Region.t  (* is         *)
    | List       of Region.t  (* list       *)
    | Map        of Region.t  (* map        *)
    | Mod        of Region.t  (* mod        *)
    | Nil        of Region.t  (* nil        *)
    | Not        of Region.t  (* not        *)
    | Of         of Region.t  (* of         *)
    | Or         of Region.t  (* or         *)
    | Patch      of Region.t  (* patch      *)
    | Record     of Region.t  (* record     *)
    | Remove     of Region.t  (* remove     *)
    | Set        of Region.t  (* set        *)
    | Skip       of Region.t  (* skip       *)
    | Step       of Region.t  (* step       *)
    | Then       of Region.t  (* then       *)
    | To         of Region.t  (* to         *)
    | Type       of Region.t  (* type       *)
    | Var        of Region.t  (* var        *)
    | While      of Region.t  (* while      *)
    | With       of Region.t  (* with       *)
    | Module     of Region.t  (* module     *)

    (* End-Of-File *)

    | EOF of Region.t


    (* Unlexing the tokens *)

    let gen_sym prefix =
      let count = ref 0 in
      fun () -> incr count; prefix ^ string_of_int !count

    let id_sym   = gen_sym "id"
    and ctor_sym = gen_sym "C"

    let concrete = function
      (* Literals *)

      "Ident"    -> id_sym ()
    | "UIdent"   -> ctor_sym ()
    | "Int"      -> "1"
    | "Nat"      -> "1n"
    | "Mutez"    -> "1mutez"
    | "String"   -> "\"a string\""
    | "Verbatim" -> "{|verbatim|}"
    | "Bytes"    -> "0xAA"
    | "Attr"     -> "[@attr]"
    | "Lang"     -> "[%Michelson"

    (* Symbols *)

    | "SEMI"     -> ";"
    | "COMMA"    -> ","
    | "LPAR"     -> "("
    | "RPAR"     -> ")"
    | "LBRACE"   -> "{"
    | "RBRACE"   -> "}"
    | "LBRACKET" -> "["
    | "RBRACKET" -> "]"
    | "CONS"     -> "#"
    | "VBAR"     -> "|"
    | "ARROW"    -> "->"
    | "ASS"      -> ":="
    | "EQ"       -> "="
    | "COLON"    -> ":"
    | "LT"       -> "<"
    | "LE"       -> "<="
    | "GT"       -> ">"
    | "GE"       -> ">="
    | "NE"       -> "=/="
    | "PLUS"     -> "+"
    | "MINUS"    -> "-"
    | "SLASH"    -> "/"
    | "TIMES"    -> "*"
    | "DOT"      -> "."
    | "WILD"     -> "_"
    | "CARET"    -> "^"

    (* Keywords *)

    | "And"       -> "and"
    | "Begin"     -> "begin"
    | "BigMap"    -> "big_map"
    | "Block"     ->  "block"
    | "Case"      -> "case"
    | "Const"     -> "const"
    | "Contains"  -> "contains"
    | "Else"      -> "else"
    | "End"       -> "end"
    | "For"       -> "for"
    | "From"      -> "from"
    | "Function"  -> "function"
    | "Recursive" -> "recursive"
    | "If"        -> "if"
    | "In"        -> "in"
    | "Is"        -> "is"
    | "List"      -> "list"
    | "Map"       -> "map"
    | "Mod"       -> "mod"
    | "Nil"       -> "nil"
    | "Not"       -> "not"
    | "Of"        -> "of"
    | "Or"        -> "or"
    | "Patch"     -> "patch"
    | "Record"    -> "record"
    | "Remove"    -> "remove"
    | "Set"       -> "set"
    | "Skip"      -> "skip"
    | "Step"      -> "step"
    | "Then"      -> "then"
    | "To"        -> "to"
    | "Type"      -> "type"
    | "Var"       -> "var"
    | "While"     -> "while"
    | "With"      -> "with"
    | "Module"    -> "module"

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
    | Attr Region.{region; value} ->
        region, sprintf "Attr %S" value
    | Lang Region.{region; value} ->
        region, sprintf "Lang %S" (value.Region.value)

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
    | CARET    region -> region, "CARET"

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
    | For        region -> region, "For"
    | From       region -> region, "From"
    | Function   region -> region, "Function"
    | Recursive  region -> region, "Recursive"
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
    | Step       region -> region, "Step"
    | Then       region -> region, "Then"
    | To         region -> region, "To"
    | Type       region -> region, "Type"
    | Var        region -> region, "Var"
    | While      region -> region, "While"
    | With       region -> region, "With"
    | Module     region -> region, "Module"

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
    | Ident id
    | UIdent id  -> id.Region.value
    | Attr a     -> a.Region.value
    | Lang lang  -> Region.(lang.value.value)

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
    | CARET    _ -> "^"

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
    | For        _ -> "for"
    | From       _ -> "from"
    | Function   _ -> "function"
    | Recursive  _ -> "recursive"
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
    | Step       _ -> "step"
    | Then       _ -> "then"
    | To         _ -> "to"
    | Type       _ -> "type"
    | Var        _ -> "var"
    | While      _ -> "while"
    | With       _ -> "with"
    | Module     _ -> "module"

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
      (fun reg -> If         reg);
      (fun reg -> In         reg);
      (fun reg -> Is         reg);
      (fun reg -> List       reg);
      (fun reg -> Map        reg);
      (fun reg -> Mod        reg);
      (fun reg -> Nil        reg);
      (fun reg -> Not        reg);
      (fun reg -> Of         reg);
      (fun reg -> Or         reg);
      (fun reg -> Patch      reg);
      (fun reg -> Record     reg);
      (fun reg -> Recursive  reg);
      (fun reg -> Remove     reg);
      (fun reg -> Set        reg);
      (fun reg -> Skip       reg);
      (fun reg -> Step       reg);
      (fun reg -> Then       reg);
      (fun reg -> To         reg);
      (fun reg -> Type       reg);
      (fun reg -> Var        reg);
      (fun reg -> While      reg);
      (fun reg -> With       reg);
      (fun reg -> Module     reg)
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

      (* Lexemes specific to PascaLIGO *)

      | "^"   -> Ok (CARET    region)
      | "->"  -> Ok (ARROW    region)
      | "=/=" -> Ok (NE       region)
      | "#"   -> Ok (CONS     region)
      | ":="  -> Ok (ASS      region)

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
