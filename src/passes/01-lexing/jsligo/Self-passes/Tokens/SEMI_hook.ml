(* Hooking virtual SEMI (semicolon) tokens to the previous (left)
   token *)

(* Vendor dependencies *)

module Std    = Simple_utils.Std
module Region = Simple_utils.Std

(* Hook *)

let rightward_hook (tokens : Token.t list) : Token.t list =
  let f (acc : Token.t list) token =
    match acc, token with
      left_token::_, Token.SEMI w when w#region#is_ghost ->
        Token.(mk_SEMI (to_region left_token)) :: acc
    | _ -> token :: acc
  in List.fold_left ~f tokens ~init:[] |> List.rev

let hook tokens = Ok (rightward_hook tokens)

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
              Hooking injected SEMI tokens.")
    | None -> ()
  in hook tokens
