open Ast_unified
open Pass_type
module Location = Simple_utils.Location
module Ligo_string = Simple_utils.Ligo_string

(* TODO: for decompilation, might be good to build a map Timestamp <-> (fun _ -> e_timestamp _) *)

include Flag.No_arg ()

let compile ~raise =
  let expr : _ expr_ -> expr =
   fun e ->
    let loc = Location.get_location e in
    match Location.unwrap e with
    | E_raw_code { language; code } as e when String.equal language "bytes" ->
      (match get_e code with
      | E_literal (Literal_string lit) ->
        let str = Ligo_string.extract lit in
        e_bytes_string ~loc str
      | E_annot (ae, ty) ->
        (match get_e ae with
        | E_literal (Literal_string lit) ->
          let str = Ligo_string.extract lit in
          e_annot ~loc (e_bytes_string ~loc:(get_e_loc ae) str, ty)
        | _ -> make_e ~loc e)
      | _ -> make_e ~loc e)
    | e -> make_e ~loc e
  in
  Fold { idle_fold with expr }


let name = __MODULE__
let decompile ~raise:_ = Nothing
let reduction ~raise:_ = Iter.defaults
