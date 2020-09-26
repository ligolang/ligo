open Display

let declarations_ppformat ~display_format ((source_file,decls),_) =
  match display_format with
  | Human_readable | Dev ->
    let buffer = Buffer.create 100 in
    let formatter = Format.formatter_of_buffer buffer in  
    Format.fprintf formatter "%s declarations:\n" source_file ;
    List.iter (fun decl -> Format.fprintf formatter "%s\n" decl) decls;
    Format.pp_print_flush formatter ();
    (Location.dummy, Buffer.contents buffer)

let declarations_jsonformat ((source_file,decls),_) : json =
  let json_decl = List.map (fun decl -> `String decl) decls in
  `Assoc [ ("source_file", `String source_file) ; ("declarations", `List json_decl) ]

let declarations_format : 'a format = {
  pp = declarations_ppformat;
  to_json = declarations_jsonformat;
}

let changelog_ppformat ~display_format changelog =
  match display_format with
  | Human_readable | Dev ->
    (Location.dummy, changelog)

let changelog_jsonformat changelog : json =
  `String changelog

let changelog_format : 'a format = {
  pp = changelog_ppformat;
  to_json = changelog_jsonformat;
}

let contract_size_ppformat ~display_format (contract_size,_) =
  match display_format with
  | Human_readable | Dev ->
    (Location.dummy, Format.asprintf "%d bytes" contract_size)

let contract_size_jsonformat (contract_size,_) : json =
  `Int contract_size

let contract_size_format : 'a format = {
  pp = contract_size_ppformat;
  to_json = contract_size_jsonformat;
}

module Michelson_formatter = struct
  open Tezos_utils.Michelson

  let pp_hex ppf (michelson : michelson) =
    let hex = Proto_alpha_utils.Memory_proto_alpha.to_hex michelson in
    Format.fprintf ppf "%a" Hex.pp hex

  type michelson_format = [
    | `Text
    | `Json
    | `Hex
  ]

  let michelson_ppformat michelson_format ~display_format (a,_) =
    let mich_pp = fun michelson_format ->  match michelson_format with
      | `Text -> pp
      | `Json -> pp_json
      | `Hex -> pp_hex in
    match display_format with
    | Display.Human_readable | Dev -> (
       let m = Format.asprintf "%a\n" (mich_pp michelson_format) a in
       (Location.dummy, m)
    )

  let michelson_jsonformat michelson_format (a,_) : Display.json = match michelson_format with
    | `Text ->
      let code_as_str = Format.asprintf "%a" pp a in
      `Assoc [("text_code" , `String code_as_str)]
    | `Hex -> 
      let code_as_hex = Format.asprintf "%a" pp_hex a in
      `Assoc [("hex_code" , `String code_as_hex)]
    | `Json ->
      (* Ideally , would like to do that :
      Michelson.get_json a *)
      let code_as_str = Format.asprintf "%a" pp_json a in
      `Assoc [("json_code" , `String code_as_str)]

  let michelson_format : michelson_format -> 'a Display.format = fun mf -> {
    pp = michelson_ppformat mf;
    to_json = michelson_jsonformat mf;
  }
end
