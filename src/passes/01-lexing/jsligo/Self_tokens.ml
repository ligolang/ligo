(* This module implements a filter on the lexical units of JsLIGO
   and produces tokens to be consumed by the parser. *)

(* Vendor dependencies *)

module Core   = LexerLib.Core
module Region = Simple_utils.Region
module Utils  = Simple_utils.Utils

(* Signature *)

module type S =
  sig
    type token
    type lex_unit = token Core.lex_unit

    type message = string Region.reg

    val filter :
      (lex_unit list, message) result -> (token list, message) result
  end

(* Filters *)

let ok x = Stdlib.Ok x

type message = string Region.reg

type token = Token.t

type lex_unit = token Core.lex_unit

(* Filtering out the markup *)

let tokens_of = function
  Stdlib.Ok lex_units ->
    let apply tokens = function
      Core.Token token -> token::tokens
    | Core.Markup (BlockCom c) -> Token.BlockCom c :: tokens
    | Core.Markup (LineCom c) -> Token.LineCom c :: tokens
    | Core.Markup _ -> tokens
    | Core.Directive d -> Token.Directive d :: tokens
    in List.fold_left apply [] lex_units |> List.rev |> ok
| Error _ as err -> err


let automatic_semicolon_insertion tokens = 
  let open Token in
  let rec inner result = function
    (Directive _ as t) :: rest ->
    inner (t :: result) rest
  | (LineCom _ as t) :: rest ->
    inner (t :: result) rest
  | (BlockCom _ as t) :: rest ->
    inner (t :: result) rest
  | (_ as semi) :: (LineCom _ as t) :: rest
  | (_ as semi) :: (BlockCom _ as t) :: rest
  | (SEMI _ as semi) :: (Let _ as t)  :: rest
  | (SEMI _ as semi) :: (Const _ as t)  :: rest
  | (SEMI _ as semi) :: (Type _ as t)  :: rest
  | (SEMI _ as semi) :: (Return _ as t)  :: rest
  | (LBRACE _ as semi) :: (Let _ as t)  :: rest
  | (LBRACE _ as semi) :: (Const _ as t)  :: rest
  | (LBRACE _ as semi) :: (Type _ as t)  :: rest
  | (LBRACE _ as semi) :: (Return _ as t)  :: rest -> 
    inner (t:: semi :: result) rest
  | token :: (Const _ as t) :: rest
  | token :: (Type _ as t) :: rest
  | token :: (Return _ as t) :: rest
  | token :: (Let _ as t) :: rest ->
    let (r, _) = Token.proj_token token in
    let (r2, _) = Token.proj_token t in
    if r#stop#line < r2#start#line  then (
      inner (t :: SEMI Region.ghost :: token :: result) rest 
    )
    else (
      match token with 
        RBRACE _ as t -> 
        inner (t :: SEMI Region.ghost :: token :: result) rest 
      | _ ->
        inner (t :: token :: result) rest 
    )
  | hd :: tl -> inner (hd :: result) tl
  | [] -> List.rev result
  in 
  inner [] tokens

let automatic_semicolon_insertion = function
  Stdlib.Ok tokens -> automatic_semicolon_insertion tokens |> ok
| Error _ as err -> err

let attribute_regexp = Str.regexp "@\\([a-zA-Z:0-9_]+\\)"

let collect_attributes str =
  let x = Str.full_split attribute_regexp str in 
  List.rev (List.fold_left (fun all x ->
    match x with 
      Str.Text _ -> all
    | Delim s -> s :: all
  ) [] x)

let attributes tokens = 
  let open Token in
  let rec inner result = function
    LineCom c :: tl
  | BlockCom c :: tl ->
      let attributes = collect_attributes c.value in
      let attributes = List.map (fun e -> 
        Attr {value = e; region = c.region}) attributes in
      inner (attributes @ result) tl
  | hd :: tl -> inner (hd :: result) tl
  | [] -> List.rev result
  in 
  inner [] tokens

let attributes = function
  Stdlib.Ok tokens -> attributes tokens |> ok
| Error _ as err -> err

(* Exported *)

let filter = Utils.(attributes <@ automatic_semicolon_insertion <@ tokens_of <@ Style.check)

