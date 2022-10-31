(* Hooking virtual VBAR tokens to the next (right) token *)

(* Vendor dependencies *)

module Std    = Simple_utils.Std
module Region = Simple_utils.Std

(* Hook *)

let leftward_hook (tokens : Token.t list) : Token.t list =
  let f token (acc : Token.t list) =
    match token, acc with
      Token.VBAR w, right_token::_ when w#region#is_ghost ->
        Token.(mk_VBAR (to_region right_token)) :: acc
    | _ -> token :: acc
  in List.fold_right ~f tokens ~init:[]

let hook tokens = Ok (leftward_hook tokens)

(* Exported *)

let filter
    : ?print_passes:Std.t ->
      add_warning:(Main_warnings.all -> unit) ->
      Token.t list ->
      _ result =
  fun ?print_passes ~add_warning:_ tokens -> (* No warning registered *)
  let () =
    match print_passes with
      Some std ->
        Std.(add_line std.out
             "Running JsLIGO token self-pass: \
              Hooking injected VBAR tokens.")
    | None -> ()
  in hook tokens
