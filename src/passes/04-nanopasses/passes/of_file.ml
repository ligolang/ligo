open Ast_unified
open Pass_type
open Simple_utils
module Location = Simple_utils.Location
module ModRes = Preprocessor.ModRes

include Flag.With_arg (struct
  type flag = ModRes.t option
end)

(* [resolve_contract_file ~mod_res ~source_file ~contract_file] tries to resolve
   [contract_file] w.r.t. to process directory
   if that fails it tries to resolve it as a relative path w.r.t. directory of [source_file]
   if that fails it tries to resolve it as a package path using [mod_res] *)
let resolve_contract_file ~mod_res ~source_file ~contract_file =
  match Caml.Sys.file_exists contract_file with
  | true -> contract_file
  | false ->
    (match source_file with
    | Some source_file ->
      let d = Filename.dirname source_file in
      let s = Filename.concat d contract_file in
      (match Caml.Sys.file_exists s with
      | true -> s
      | false -> ModRes.Helpers.resolve ~file:contract_file mod_res)
    | None -> ModRes.Helpers.resolve ~file:contract_file mod_res)


let get_file_from_location loc =
  let open Option in
  let* reg = Location.get_file loc in
  let file = reg#file in
  if String.(file = "") then None else Some file


let compile ~raise:_ =
  let mod_res = get_flag () in
  let expr : _ expr_ -> expr =
   fun e ->
    let loc = Location.get_location e in
    match Location.unwrap e with
    | E_raw_code { language = "of_file"; code } as e ->
      (match get_e code with
      | E_literal (Literal_string path) ->
        let source_file = get_file_from_location loc in
        let path = Ligo_string.extract path in
        let source_file =
          resolve_contract_file ~mod_res ~source_file ~contract_file:path
        in
        let s = In_channel.(with_file source_file ~f:input_all) in
        make_e ~loc (E_literal (Literal_string (Ligo_string.verbatim s)))
      | _ -> make_e ~loc e)
    | e -> make_e ~loc e
  in
  Fold { idle_fold with expr }

let name = __MODULE__
let decompile ~raise:_ = Nothing
let reduction ~raise:_ = Iter.defaults
