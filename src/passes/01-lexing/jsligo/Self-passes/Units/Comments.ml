(* Transforming comments into tokens so that a self-pass on them can
   try to extract attributes *)

(* Vendor dependencies *)

module Region    = Simple_utils.Region
module Std       = Simple_utils.Std
module Markup    = LexerLib.Markup
module Directive = Preprocessor.Directive
module LexUnit   = LexerLib.LexUnit

(* Local dependencies *)

module Token = Lx_js_self_tokens.Token

let filter (units : Token.t LexUnit.t list) : Token.t LexUnit.t list =
  let open! Token in
  let rec aux acc = function
    `Markup (Markup.BlockCom {value; region}) :: remaining ->
      aux (`Token (mk_BlockCom value region) :: acc) remaining
  | `Markup (Markup.LineCom {value; region}) :: remaining ->
      aux (`Token (mk_LineCom value region) :: acc) remaining
  | other :: remaining ->
      aux (other :: acc) remaining
  | [] -> List.rev acc
  in aux [] units

(* Exported *)

type item = Token.t LexUnit.t

type units = item list

type message = string Region.reg

type nonrec result = (units, units * message) result

let filter ?print_passes ~add_warning:_ units : result =
  let () =
    match print_passes with
      Some std ->
        Std.(add_line std.out
                      "Running JsLIGO unit  self-pass: \
                       Collect attributes from comments.")
    | None -> ()
  in Ok (filter units)
