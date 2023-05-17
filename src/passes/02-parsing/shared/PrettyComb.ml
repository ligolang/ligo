(* Useful combinators based on PPrint *)

module Wrap = Lexing_shared.Wrap
module Region = Simple_utils.Region

open PPrint

type leading_bar =
  Always
| Only_on_new_line
| Avoid

type state = <
  indent       : int;
  leading_vbar : leading_bar
>

(* [PPrint.prefix] has a problem: if [x == empty] then we will produce
   an extraneous space, which is undesirable. This is a fix which
   shadows [prefix]. *)

let prefix n b x y =
  group (x ^^
         nest n ((if Core.phys_equal x empty then empty else break b) ^^ y))

(* The same as [( ^/^ )], doesn't output an extra space if the first
   operand is [empty]. *)

let (^/^) x y = if Core.phys_equal x empty then y else x ^/^ y

(* Enclosed structures *)
(*
let pp_enclosed_document
    state ?(force_hardline : bool option) (thread : document)
    break_size left right =
  let left  = token left
  and right = token right in
  group (
    match force_hardline with
      None | Some false ->
        nest state.indent (left ^^ break break_size ^^ thread)
        ^^ break break_size ^^ right
    | Some true ->
        nest state.indent (left ^^ hardline ^^ thread)
        ^^ hardline ^^ right)
*)
