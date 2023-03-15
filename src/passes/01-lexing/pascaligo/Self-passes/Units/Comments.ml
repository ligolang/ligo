(* Tokenising all comments *)

(* Vendor dependencies *)

module Region = Simple_utils.Region
module Std    = Simple_utils.Std
module Unit   = LexerLib.Unit
module Markup = LexerLib.Markup

(* Local dependencies *)

module Token = Lx_psc_self_tokens.Token

(* Utilities *)

type units = Token.t Unit.t list

(* Filter *)

let filter (units: units) =
  let open! Token
  in
  let rec aux acc = function
    `Markup Markup.BlockCom {region; value} :: units ->
       let token = `Token (mk_BlockCom value region)
       in aux (token :: acc) units
  | `Markup Markup.LineCom {region; value} :: units ->
       let token = `Token (mk_LineCom value region)
       in aux (token :: acc) units
  | unit :: units ->
      aux (unit :: acc) units

  | [] -> List.rev acc
  in
  aux [] units

type message = string Region.reg

type result = (units, units * message) Stdlib.result

let filter ?print_passes ~add_warning:_ units : result =
  let () =
    match print_passes with
      Some std ->
        Std.(add_line std.out
                      "Running PascaLIGO unit self-pass: \
                       All comments are tokenised.")
    | None -> ()
  in Ok (filter units)
