open Ligo_prim
open Simple_utils.Display
module List = Simple_utils.List

let declarations_ppformat ~display_format f (source_file,decls) =
  match display_format with
  | Human_readable | Dev ->
    Format.fprintf f "%s declarations:\n" source_file ;
    List.iter ~f: (fun decl -> Format.fprintf f "%a\n" ValueVar.pp decl) decls

let declarations_jsonformat (source_file,decls) : json =
  (* Use to_name instead of to_yojson for compality with IDE *)
  let json_decl = List.map ~f:(fun decl -> `String (ValueVar.to_name_exn decl)) decls in
  `Assoc [ ("source_file", `String source_file) ; ("declarations", `List json_decl) ]

let declarations_format : 'a format = {
  pp = declarations_ppformat;
  to_json = declarations_jsonformat;
}

let changelog_ppformat ~display_format f changelog =
  match display_format with
  | Human_readable | Dev ->
    Format.fprintf f "%s" changelog

let changelog_jsonformat changelog : json =
  `String changelog

let changelog_format : 'a format = {
  pp = changelog_ppformat;
  to_json = changelog_jsonformat;
}

let contract_size_ppformat ~display_format f contract_size =
  match display_format with
  | Human_readable | Dev ->
    Format.fprintf f "%d bytes" contract_size

let contract_size_jsonformat contract_size : json =
  `Int contract_size

let contract_size_format : 'a format = {
  pp = contract_size_ppformat;
  to_json = contract_size_jsonformat;
}

let list_ppformat ~display_format f (lst) =
  match display_format with
  | Human_readable | Dev ->
    Format.fprintf f "list of projects:\n" ;
    List.iter ~f: (fun str -> Format.fprintf f "%s\n" str) lst

let list_jsonformat (_lst : string list) : json =
  `Null

let list_format : 'a format = {
  pp = list_ppformat;
  to_json = list_jsonformat;
}

let new_project_ppformat  ~display_format f (lst) =
  match display_format with
  | Human_readable | Dev ->
    Format.fprintf f "Folder created: " ;
    List.iter ~f: (fun str -> Format.fprintf f "%s\n" str) lst

let new_project_jsonformat (_lst : string list) : json =
  `Null

let new_project_format : 'a format = {
  pp = new_project_ppformat;
  to_json = new_project_jsonformat;
}

module Michelson_formatter = struct
  open Tezos_utils.Michelson
  open Simple_utils

  let pp_hex ppf michelson =
    let hex = Proto_alpha_utils.Memory_proto_alpha.to_hex michelson in
    Format.fprintf ppf "%a" Hex.pp hex

  type michelson_format = [
    | `Text
    | `Json
    | `Hex
  ]


  type michelson_comments =
    { location : bool;
      source : bool;
      env : bool }

  (* for JSON format with environment (env) data, we will "shrink" the
     result by replacing source_type info with indices into a separate
     "types" array: *)

  type shrunk_variable_meta =
    { location : Location.t;
      name : string option;
      source_type : int option;
    }

  type shrunk_meta =
    { location : Location.t;
      env : shrunk_variable_meta option list }

  type shrunk_result =
    { types : Ast_typed.type_expression list;
      michelson : (shrunk_meta, string) Tezos_micheline.Micheline.node
    }

  let comment michelson_comments =
    match michelson_comments with
    | {location = show_location; source = _; env = show_env} ->
      fun ({location; env} : shrunk_meta) ->
        let loc =
          if show_location && not (Simple_utils.Location.is_virtual location)
          then Format.asprintf "%a" Location.pp location
          else "" in
        let env =
          if show_env && not (List.is_empty env)
          then
            Format.asprintf "%a"
              (Format.pp_print_list
                 ~pp_sep:(fun ppf () -> Format.pp_print_string ppf ", ")
                 Format.pp_print_string)
              (List.map
                 env
                 ~f:(function
                     | None -> "_"
                     | Some meta ->
                       match meta.name with
                       | None -> "_"
                       | Some name -> name))
          else "" in
        let comment = loc ^ env in
        if String.length comment = 0
        then None
        else Some comment

  let rec yojson_to_json (x : Yojson.Safe.t) : Data_encoding.Json.t =
    match x with
    | `Tuple xs -> `A (List.map ~f:yojson_to_json xs)
    | `Bool b -> `Bool b
    | `Intlit n -> `String n
    | `Null -> `Null
    | `Variant (tag, arg) ->
      `A (`String tag :: List.map ~f:yojson_to_json (Option.to_list arg))
    | `Assoc kvs ->
      `O (List.map ~f:(fun (k, v) -> (k, yojson_to_json v)) kvs)
    | `List vs -> `A (List.map ~f:yojson_to_json vs)
    | `Float f -> `Float f
    | `String s -> `String s
    | `Int n -> `String (string_of_int n)

  let location_to_json (location : Location.t) : Data_encoding.Json.t option =
    if Location.is_virtual location
    then None
    else Some (yojson_to_json (Location.to_human_yojson location))

  let source_type_to_json (source_type : Ast_typed.type_expression) : Data_encoding.Json.t =
    yojson_to_json (Ast_typed.type_expression_to_yojson source_type)

  let json_object (vals : (string * Data_encoding.Json.t option) list) : Data_encoding.Json.t =
    `O (List.concat (List.map
                       vals
                       ~f:(fun (k, v) ->
                           match v with
                           | None -> []
                           | Some v -> [(k, v)])))

  module TypeOrd = struct
    type t = Ast_typed.type_expression
    let compare = Ast_typed.compare_type_expression
  end
  module TypeSet = Stdlib.Set.Make(TypeOrd)

  open Tezos_micheline

  let rec fold_micheline (node : _ Micheline.node) ~(f : ('acc -> _ Micheline.node -> 'acc)) ~(init : 'acc) : 'acc =
    let init = f init node in
    match node with
    | Prim (_, _, args, _) ->
      List.fold_left args ~init ~f:(fun init arg -> fold_micheline arg ~f ~init:init)
    | Seq (_, args) ->
      List.fold_left args ~init ~f:(fun init arg -> fold_micheline arg ~f ~init:init)
    | Int _ -> init
    | String _ -> init
    | Bytes _ -> init

  let shrink (node : (Mini_c.meta, 'p) Tezos_micheline.Micheline.node) : shrunk_result =
    (* first collect all source types *)
    let type_set : TypeSet.t =
      fold_micheline node
        ~f:(fun init node ->
            let Mini_c.{ location = _; env; binder = _ } = Tezos_micheline.Micheline.location node in
            List.fold_left
              env ~init
              ~f:(fun init binder_meta ->
                  match binder_meta with
                  | None -> init
                  | Some { location = _; name = _; source_type = None } -> init
                  | Some { location = _; name = _; source_type = Some source_type } -> TypeSet.add source_type init))
        ~init:TypeSet.empty in
    (* hmm *)
    let type_index t =
      match List.findi (TypeSet.elements type_set) ~f:(fun _ x -> 0 = TypeOrd.compare x t) with
      | Some (i, _) -> i
      (* impossible... *)
      | None -> 0 in
    (* now substitute all source types for their indices *)
    let node =
      Micheline.map_node
        (fun Mini_c.{ location; env; binder = _ } ->
           let env =
             List.map
               ~f:(Option.map
                     ~f:(fun Mini_c.{ location; name; source_type } -> { location; name; source_type = Option.map ~f:type_index source_type }))
               env in
           { location; env = env })
        (fun prim -> prim)
        node in
    { types = TypeSet.elements type_set;
      michelson = node }

  let variable_meta_to_json : shrunk_variable_meta -> Data_encoding.Json.t =
    function { location; name; source_type } ->
      json_object [("location", location_to_json location);
                   ("name", (match name with
                             | None -> None
                             | Some name -> Some (`String name)));
                   ("source_type", Option.map ~f:(fun n -> `String (string_of_int n)) source_type)]

  let meta_encoding : shrunk_meta Data_encoding.t =
    Data_encoding.(conv
                     (fun { location; env } ->
                        json_object [("location", location_to_json location);
                                     ("environment",
                                      if List.is_empty env
                                      then None
                                      else Some (`A (List.map
                                                       env
                                                       ~f:(function
                                                           | None -> `Null
                                                           | Some binder_meta -> variable_meta_to_json binder_meta))))])
                     (fun _s -> failwith ("internal error: not implemented @ " ^ __LOC__))
                     Data_encoding.json)

  let comment_encoding michelson_comments =
    match michelson_comments with
    | {location; source = _; env} ->
      if location || env
      then Some meta_encoding
      else None

  let pp_result_json michelson_comments ppf result =
    let json = get_json ?comment:(comment_encoding michelson_comments) result.michelson in
    let json =
      if michelson_comments.env
      then `O [("types", `A (List.map ~f:source_type_to_json result.types));
               ("michelson", json)]
      else json in
    Format.fprintf ppf "%a" Data_encoding.Json.pp json

  let result_ppformat michelson_format michelson_comments ~display_format f (a : shrunk_result) =
    let mich_pp = fun michelson_format michelson_comments ->  match michelson_format with
      | `Text -> fun ppf result -> pp_comment ~comment:(comment michelson_comments) ppf result.michelson
      | `Json -> pp_result_json michelson_comments
      | `Hex -> fun ppf result -> pp_hex ppf result.michelson in
    match display_format with
    | Human_readable | Dev -> (
       let m = Format.asprintf "%a\n" (mich_pp michelson_format michelson_comments) a in
       Format.pp_print_string f m
    )

  (* is this even used anywhere??? *)
  let result_jsonformat michelson_format michelson_comments (a : shrunk_result) : json = match michelson_format with
    | `Text ->
      let code_as_str = Format.asprintf "%a" (pp_comment ~comment:(comment michelson_comments)) a.michelson in
      `Assoc [("text_code" , `String code_as_str)]
    | `Hex ->
      let code_as_hex = Format.asprintf "%a" pp_hex a.michelson in
      `Assoc [("hex_code" , `String code_as_hex)]
    | `Json ->
      let code_as_str = Format.asprintf "%a" (pp_result_json michelson_comments) a in
      `Assoc [("json_code" , `String code_as_str)]

  let convert_michelson_comments : [> `All | `Env | `Location | `Source ] list -> michelson_comments =
    let none = {location = false; source = false; env = false} in
    let all = {location = true; source = true; env = true} in
    List.fold_left
      ~init:none
      ~f:(fun config option ->
          let config2 =
            match option with
            | `All -> all
            | `Env -> { none with env = true }
            | `Location -> { none with location = true }
            | `Source -> { none with source = true } in
          { location = config.location || config2.location ;
            source = config.source || config2.source ;
            env = config.env || config2.env })

  let shrunk_result_format : michelson_format -> _ -> shrunk_result format =
    fun mf michelson_comments ->
    { pp = result_ppformat mf (convert_michelson_comments michelson_comments);
      to_json = result_jsonformat mf (convert_michelson_comments michelson_comments) }

  let michelson_format : michelson_format -> _ -> (Mini_c.meta, string) Micheline.node format =
    fun michelson_format michelson_comments ->
    Display.map ~f:shrink (shrunk_result_format michelson_format michelson_comments)

  let michelson_constant_jsonformat michelson_format a : json = match michelson_format with
    | `Text ->
      let code_as_str = Format.asprintf "%a" (pp_comment ~comment:(fun _ -> None)) a in
      `Assoc [("text_code" , `String code_as_str)]
    | `Hex ->
      let code_as_hex = Format.asprintf "%a" pp_hex a in
      `Assoc [("hex_code" , `String code_as_hex)]
    | `Json ->
      let code_as_str = Format.asprintf "%a" (pp_json ?comment:None) a in
      `Assoc [("json_code" , `String code_as_str)]

  let michelson_constant_ppformat ~display_format f (hash, a) =
    match display_format with
    | Human_readable | Dev -> (
       let code =
         `String (Format.asprintf "%a" (pp_comment ~comment:(fun _ -> None)) a) |>
           Yojson.Safe.pretty_to_string in
       let code_no_newlines = Str.global_replace (Str.regexp_string "\\n") "\n" code in
       let hash = Format.asprintf "%a" Memory_proto_alpha.Protocol.Script_expr_hash.pp hash in
       Format.fprintf f "Michelson constant as JSON string:@.%s@.This string can be passed in `--constants` argument when compiling a contract.@.@.Remember to register it in the network, e.g.:@.> tezos-client register global constant %s from bootstrap1@.@.Constant hash:@.%s" code code_no_newlines hash
    )

  let michelson_constant_format : (Proto_alpha_utils.Memory_proto_alpha.Protocol.Script_expr_hash.t *
                                   (int, string) Micheline.node) format =
    {
      pp = michelson_constant_ppformat;
      to_json = fun (_, a) -> michelson_constant_jsonformat `Text a;
    }
end
