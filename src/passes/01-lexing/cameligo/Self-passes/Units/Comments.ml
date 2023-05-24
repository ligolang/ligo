(* Transforming comments into tokens so that a self-pass on them can
   try to extract attributes *)

(* Vendor dependencies *)

module Region    = Simple_utils.Region
module Std       = Simple_utils.Std
module Core      = LexerLib.Core
module Markup    = LexerLib.Markup
module Directive = Preprocessor.Directive
module Unit      = LexerLib.Unit

(* Local dependencies *)

module Token = Lx_ml_self_tokens.Token

(* Filter *)

let filter (units : Token.t Unit.t list) : Token.t Unit.t list =
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

type item = Token.t Unit.t

type units = item list

type message = string Region.reg

type result = (units, units * message) Stdlib.result

let filter ?print_passes ~add_warning:_ units : result =
  let () =
    match print_passes with
      Some std ->
        Std.(add_line std.out
                      "Running CameLIGO unit  self-pass: \
                       Collect attributes from comments.")
    | None -> ()
  in Ok (filter units)
