(* Handling input spanning multiple lines *)

(* Vendor dependencies *)

module Region    = Simple_utils.Region
module Std       = Simple_utils.Std
module Core      = LexerLib.Core
module Markup    = LexerLib.Markup
module Directive = Preprocessor.Directive
module Unit      = LexerLib.Unit

(* Local dependencies *)

module Token = Lexing_pyligo_self_tokens.Token

(* Result type *)

type item    = Token.t Unit.t
type units   = item list
type message = string Region.reg
type result  = (units, units * message) Stdlib.result

(* Filter *)

let filter (units : units) : result =
  let open Markup
  in
  let rec aux acc = function
    (`Token (Token.BACKSLASH _)) ::
    (`Markup (Newline _)) ::
    (`Markup (Space _ | Tabs _)) :: more
  | (`Token (Token.BACKSLASH _)) ::
    (`Markup (Newline _)) :: more ->
       aux acc more
  | unit :: more ->
       aux (unit :: acc) more
  | [] -> List.rev acc
  in Ok (aux [] units)

(* Exported *)

let filter ?print_passes ~add_warning:_ units : result =
  let () =
    match print_passes with
      Some std ->
        Std.(add_line std.out
                      "Running PyLIGO unit self-pass: \
                       Handling multilines.")
    | None -> ()
  in filter units
