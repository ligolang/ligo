(* Injecting the virtual token ES6FUN before a '(' that looks like an
   arrow function (a.k.a. a lambda) *)

(* Vendor dependencies *)

module Region = Simple_utils.Region
module Std    = Simple_utils.Std
module Utils  = Simple_utils.Utils

(* Utilities *)

type tokens = Token.t list

type state = {
  prefix : tokens;             (* Tokens up to "(" excluded.     *)
  params : Token.t Utils.nseq; (* Tokens from "(" included.      *)
  depth  : int                 (* Nesting of arrays and objects. *)
}

let commit state =
  let previous, rest = state.params
  in previous, rest @ state.prefix

let return state =
  List.rev (Utils.nseq_to_list state.params @ state.prefix)

let mk_fun state =
  let {prefix; params; _} = state in
  let previous, rest = params in
  let es6fun = Token.(mk_ES6FUN (to_region previous))
  in previous, rest @ es6fun :: prefix

let init_state acc previous current = {
  prefix = previous :: acc;
  params = current, [];
  depth  = 0
}

let pop  state = {state with depth = max 0 (state.depth - 1)}
let push state = {state with depth = state.depth + 1}

let shift token state =
  {state with params = Utils.nseq_cons token state.params}

let rec scan (previous, acc) current tokens =
  let open Token in
  match previous, current, tokens with
    (* Likely a function *)
    (LBRACKET _ | LPAR _ | EQ _ | COMMA _ | COLON _ | GT _ | ARROW _),
    LPAR _,
    (RPAR _ as next) :: tokens -> (* "= ()" *)
      let es6fun = mk_ES6FUN (to_region previous) in
      scan (current, es6fun :: previous :: acc) next tokens

  | (LBRACKET _ | LPAR _ | EQ _ | COMMA _ | COLON _ | GT _ | ARROW _),
    Ident _,
    (ARROW _ as next) :: tokens ->
    (* Could be wrong if "type u = t =>" *)
      let es6fun = mk_ES6FUN (to_region current) in
      scan (current, es6fun :: previous :: acc) next tokens

    (* Maybe a function: trying harder by scanning parameters *)
  | (LBRACKET _ | LPAR _ | EQ _ | COMMA _ | COLON _ | GT _ | ARROW _),
    LPAR _,
    ((LBRACE _ | LBRACKET _ | Ident _ | WILD _) :: _ as tokens) ->
      (* "= ({" or "= ([" or "= (x" or "= (_" *)
      scan_parameters (init_state acc previous current) tokens

    (* Likely not a function *)
  | _, _, next :: tokens -> (* Sliding left the 3-token window *)
      scan (current, previous :: acc) next tokens

    (* No more tokens *)
  | _, _, [] -> List.rev (current :: previous :: acc)

and scan_parameters state = function
  (* Likely a function: insert ES6FUN and return to [scan]. *)
  (RPAR  _ as current) :: ((COLON _ | ARROW _) :: _ as tokens)
| (COLON _ as current) :: tokens
| (COMMA _ as current) :: tokens when state.depth = 0 ->
    scan (mk_fun state) current tokens

  (* Undetermined: push or pop a token, and try again. *)

| (LBRACE _ | LBRACKET _ as current) :: tokens ->
    scan_parameters (push @@ shift current state) tokens
| (RBRACE _ | RBRACKET _ as current) :: tokens ->
    scan_parameters (pop @@ shift current state) tokens
| (COLON _ | ELLIPSIS _ | SEMI _ | COMMA _
  | Ident _ | WILD _ | Verbatim _ | Bytes _
  | Int _ | Nat _ | Mutez _ as current)
  :: tokens ->
    scan_parameters (shift current state) tokens

  (* Likely not a function: return to [scan]. *)
| current :: tokens -> scan (commit state) current tokens

  (* No more tokens *)
| [] -> return state

let inject (tokens : tokens) : tokens =
  match tokens with
    fst :: snd :: tokens -> scan (fst, []) snd tokens
  | _ -> tokens

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
