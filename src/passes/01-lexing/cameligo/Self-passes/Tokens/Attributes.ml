(* Making attributes from some comments *)

(* Vendor dependencies *)

module Region = Simple_utils.Region
module Std    = Simple_utils.Std
module Utils  = Simple_utils.Utils

(* Local dependencies *)

module Directive = Preprocessor.Directive
module Attr      = Lexing_shared.Attr
module Wrap      = Lexing_shared.Wrap

(* Utilities *)

let (<@) = Utils.(<@)

type tokens = Token.t list

let mk_comment_attr (comment : Wrap.comment) =
  let region = Wrap.comment_to_region comment
  and value = Attr.String (Wrap.comment_to_string comment) in
  Token.mk_attr ~key:"comment" ~value region

let comments_to_attr token acc =
  let init = token :: acc in
  let open! Token in
  match token with
    Type _ | Let _ | Module _ | Attr _ | Include _ | Sig _ | Val _ ->
      let comments = Token.to_comments token
      and f = List.cons <@ mk_comment_attr
      in List.fold_right ~f ~init comments
  | _ -> init

let filter (tokens : tokens) =
  let rec aux acc = function
    t :: tokens -> aux (comments_to_attr t acc) tokens
  | [] -> acc (* Restore original order *)
  in
  aux [] (List.rev tokens)

(* Exported *)

type message = string Region.reg

let filter :
      ?print_passes:Std.t ->
      add_warning:(Main_warnings.all -> unit) ->
      tokens ->
      (tokens, tokens * message) result =
  fun ?print_passes ~add_warning:_ tokens -> (* No warning registered *)
  let () =
    match print_passes with
      Some std ->
        Std.(add_line std.out
             "Running CameLIGO token self-pass: \
              Some comments into attributes.")
    | None -> ()
  in Ok (filter tokens)
