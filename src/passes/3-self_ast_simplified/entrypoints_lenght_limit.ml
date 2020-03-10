open Ast_simplified
open Trace
open Stage_common.Helpers

module Errors = struct
  let bad_string_timestamp name () =
    let title = thunk @@ Format.asprintf ("Too long constructor '%s'") name in
    let message () = "names length is limited to 32 (tezos limitation)" in
    error title message ()
end
open Errors

let peephole_type_expression : type_expression -> type_expression result = fun e ->
  let return type_content = ok { e with type_content } in
  match e.type_content with
  | T_sum cmap ->
    let%bind _uu = bind_map_cmapi
      (fun k _ ->
        let (Constructor name) = k in
        if (String.length name  >= 32) then fail @@ bad_string_timestamp name
        else ok ()
      )
      cmap in
    ok e
  | e -> return e
