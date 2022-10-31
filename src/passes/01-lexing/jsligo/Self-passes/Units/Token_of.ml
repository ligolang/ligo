(* Vendor dependencies *)
module Region    = Simple_utils.Region
module Std       = Simple_utils.Std
module Core      = LexerLib.Core
module Markup    = LexerLib.Markup
module Directive = Preprocessor.Directive
module Unit      = LexerLib.Unit

(* Local dependencies *)

module Token = Lexing_jsligo_self_tokens.Token

let ok x = Stdlib.Ok x

let filter (units : Token.t Unit.lex_unit list) : Token.t Unit.t list = 
  let open! Token in
  let rec aux acc = function
  | (`Token token) :: remaining -> 
      aux ((`Token token)::acc) remaining
  | (`Markup (Markup.BlockCom {value; region})) :: remaining ->
      aux (`Token (mk_BlockCom value region) :: acc) remaining
  | (`Markup (Markup.LineCom {value; region}))  :: remaining ->
      aux (`Token (mk_LineCom value region) :: acc) remaining
  | (`Markup _)  :: remaining -> 
    aux acc remaining
  | (`Directive d)  :: remaining -> 
      aux (`Token (mk_directive d) :: acc) remaining
  | [] -> 
    List.rev acc
  in 
  aux [] units


type item = Token.t Unit.t

type units = item list

type message = string Region.reg

type result = (units, units * message) Stdlib.result

(* Exported *)
let filter ?print_passes ~add_warning:_ units : result =
  let () =
    match print_passes with
      Some std ->
        Std.(add_line std.out
                      "Running JsLIGO unit  self-pass: \
                      Collect attributes from comments.")
    | None -> ()
  in Ok (filter units)  