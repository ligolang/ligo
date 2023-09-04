(* Injecting the virtual token PARAMS before ") =>" *)

(* Vendor dependencies *)

module Region  = Simple_utils.Region
module Std     = Simple_utils.Std

(* Utilities *)

type tokens = Token.t list

(* Filter with Injection *)

let inject (tokens : tokens) : tokens =
  let open Token in
  let rec aux acc = function
    [] -> List.rev acc
  | RPAR _ as rpar :: (COLON _ | ARROW _ as next) :: tokens ->
      let params = mk_PARAMS (to_region rpar) in
      aux (next :: rpar :: params :: acc) tokens
  | token :: tokens -> aux (token :: acc) tokens
  in aux [] tokens

let inject tokens = Ok (inject tokens)

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
              Injecting PARAMS tokens.")
    | None -> ()
  in inject tokens
