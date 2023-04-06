open Ast_unified
open Pass_type
open Simple_utils
open Ligo_prim.Constant
module Location = Simple_utils.Location

let destruct_args ((str, args) : expr List.Ne.t) : (string * expr list) option =
  match get_e_literal str with
  | Some (Literal_string str) -> Some (Ligo_string.extract str, args)
  | _ -> None


let compile =
  let expr : _ expr_ -> expr =
   fun e ->
    let loc = Location.get_location e in
    match Location.unwrap e with
    | E_raw_code { language = "external"; code } as e ->
      (match get_e code with
      | E_tuple m ->
        (match destruct_args m with
        | Some (code, arguments) ->
          (match read_constant' code with
          | None -> failwith @@ "Constant cannot be externalized: " ^ code
          | Some cons_name -> e_constant ~loc { cons_name; arguments })
        | _ -> make_e ~loc e)
      | E_literal (Literal_string code) ->
        let code = Simple_utils.Ligo_string.extract code in
        (match Ligo_prim.Constant.read_constant' code with
        | None -> failwith @@ "Constant cannot be externalized: " ^ code
        | Some cons_name -> e_constant ~loc { cons_name; arguments = [] })
      | _ -> make_e ~loc e)
    | e -> make_e ~loc e
  in
  `Cata { idle_cata_pass with expr }


let pass =
  morph
    ~name:__MODULE__
    ~compile
    ~decompile:`None (* for now ? *)
    ~reduction_check:Iter.defaults
