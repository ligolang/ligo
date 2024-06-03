open Ast_unified
open Pass_type
module Ligo_Constant = Ligo_prim.Constant
module Location = Simple_utils.Location
module Ligo_string = Simple_utils.Ligo_string
include Flag.No_arg ()

let destruct_args (str :: args : expr Nonempty_list.t) : (string * expr list) option =
  match get_e_literal str with
  | Some (Literal_string str) -> Some (Ligo_string.extract str, args)
  | _ -> None


let compile ~raise:_ =
  let expr : _ expr_ -> expr =
   fun e ->
    let loc = Location.get_location e in
    match Location.unwrap e with
    | ( E_raw_code { language = "External"; code }
      | E_raw_code { language = "external"; code } ) as e ->
      (match get_e code with
      | E_tuple m ->
        (match destruct_args m with
        | Some (code, arguments) ->
          (match Ligo_Constant.read_constant' code with
          | None -> failwith @@ "Constant cannot be externalized: " ^ code
          | Some cons_name -> e_constant ~loc { cons_name; arguments })
        | _ -> make_e ~loc e)
      | E_literal (Literal_string code) ->
        let code = Ligo_string.extract code in
        (match Ligo_Constant.read_constant' code with
        | None -> failwith @@ "Constant cannot be externalized: " ^ code
        | Some cons_name -> e_constant ~loc { cons_name; arguments = [] })
      | _ -> make_e ~loc e)
    | e -> make_e ~loc e
  in
  Fold { idle_fold with expr }


let name = __MODULE__
let decompile ~raise:_ = Nothing (* for now ? *)
let reduction ~raise:_ = Iter.defaults
