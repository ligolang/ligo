(* Transforming ALL comments into attributes or embedding them in the
   next preprocessing directive as JsLIGO comments *)

(* Vendor dependencies *)

module Region  = Simple_utils.Region
module Std     = Simple_utils.Std
module Utils   = Simple_utils.Utils
module Snippet = Simple_utils.Snippet

(* Local dependencies *)

module Directive = Preprocessor.Directive
module Attr      = Lexing_shared.Attr
module Wrap      = Lexing_shared.Wrap

(* Utilities *)

type tokens = Token.t list

(* Filter (right to left) *)

let rec comments_to_attr acc = function
  Token.(BlockCom w | LineCom w) :: tokens ->
    let value = Attr.String w#payload in
    let attr  = Token.mk_attr ~key:"comment" ~value w#region
    in comments_to_attr (attr :: acc) tokens
| tokens -> acc, tokens

let rec hook_comments_to_dir directive acc = function
  Token.(BlockCom w | LineCom w) :: tokens ->
    let value     = sprintf "/*%s*/" w#payload in
    let comment   = Region.{value; region = w#region} in
    let directive = Directive.add_comment comment directive
    in hook_comments_to_dir directive acc tokens
| tokens -> Token.Directive directive :: acc, tokens

let filter (tokens : tokens) =
  let open! Token
  in
  let rec aux acc = function
    Directive t :: tokens ->
      Utils.uncurry aux @@ hook_comments_to_dir t acc tokens
  | t :: tokens ->
      Utils.uncurry aux @@ comments_to_attr (t :: acc) tokens
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
             "Running PascaLIGO token self-pass: \
              Comments into attributes or embedding.")
    | None -> ()
  in Ok (filter tokens)
