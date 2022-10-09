(* We try attach all the ES6FUN's region to the region of the next
   (right) token. This fails if the ES6FUN is last, which should never
   happen. *)

(* Vendor dependencies *)

module Std    = Simple_utils.Std
module Region = Simple_utils.Std

(* Hooking *)

let leftward_hook (tokens : Token.t list) : Token.t list =
  let f token (acc : Token.t list) =
    match token, acc with
      Token.ES6FUN _, right_token::_ ->
        Token.(mk_ES6FUN (to_region right_token)) :: acc
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
              Hooking ES6FUN virtual tokens.")
    | None -> ()
  in hook tokens
