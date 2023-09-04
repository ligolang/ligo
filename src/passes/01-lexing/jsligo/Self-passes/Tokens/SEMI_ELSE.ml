(* Replacing "; else" by the virtual token SEMI_ELSE to help the parser *)

(* Vendor dependencies *)

module Region = Simple_utils.Region
module Std    = Simple_utils.Std

(* Utilities *)

type tokens = Token.t list

(* Filter with injection *)

let inject (tokens : tokens) : tokens =
  let open Token in
  let rec aux acc = function
    [] -> List.rev acc
  | SEMI semi :: Else else_ :: tokens ->
      let semi_else = SEMI_ELSE (semi, else_)
      in aux (semi_else :: acc) tokens
  | token :: tokens -> aux (token :: acc) tokens
  in aux [] tokens

let inject tokens = Ok (inject tokens)

(* Exported *)

let filter :
  ?print_passes:Std.t ->
  add_warning:(Main_warnings.all -> unit) ->
  Token.t list ->
  _ result =
  fun ?print_passes ~add_warning:_ tokens -> (* No warning registered *)
  let () =
    match print_passes with
      Some std ->
        Std.(add_line std.out
             "Running JsLIGO token self-pass: \
              Injecting ES6FUN tokens.")
    | None -> ()
  in inject tokens
