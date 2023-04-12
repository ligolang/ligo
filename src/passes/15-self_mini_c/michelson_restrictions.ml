open Errors
open Mini_c
open Simple_utils.Trace

let self_in_lambdas ~raise : expression -> expression =
  fun e ->
    match e.content with
    | E_closure {binder=_ ; body } | E_rec { func = { binder = _ ; body } ; rec_binder = _ } ->
      let f = fun ~raise e -> match e.content with
        | E_raw_michelson (code, _) ->
          let code = Tezos_utils.Michelson.lseq Location.generated code in
          let code = Tezos_micheline.Micheline.(map_node (fun _ -> ()) (fun x -> x) code) in
          if Tezos_utils.Michelson.has_prim "SELF" code then
            raise.error bad_self_address
          else
            e
	| _ -> e in
      let _self_in_lambdas : expression = Helpers.map_expression
        (f ~raise)
        body in
      e
    | _ -> e

let rec check_comparable ~raise (error : type_expression -> _) : type_expression -> unit =
  fun t ->
  let self = check_comparable ~raise error in
  match t.type_content with
  | T_base TB_unit | T_base TB_never | T_base TB_bool | T_base TB_int
  | T_base TB_nat | T_base TB_string | T_base TB_timestamp | T_base TB_mutez
  | T_base TB_chain_id | T_base TB_bytes | T_base TB_key_hash | T_base TB_address
  | T_base TB_key | T_base TB_signature -> ()
  | T_tuple ts ->
    let () = List.iter ~f:(fun t -> self (snd t)) ts in
    ()
  | T_option t ->
    let () = self t in
    ()
  | T_or (t1, t2) ->
    let () = self (snd t1) in
    let () = self (snd t2) in
    ()
  | _ -> raise.error @@ error t

let not_comparable ~raise : expression -> expression =
  fun e ->
  let f t = match t.type_content with
    | T_set u ->
      let () = check_comparable ~raise (not_comparable "set" t) u in
      t
    | T_ticket u ->
      let () = check_comparable ~raise (not_comparable "ticket" t) u in
      t
    | _ -> t in
  let _ = Helpers.map_type_expression f e.type_expression in
  e