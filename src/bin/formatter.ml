open Display

let declarations_ppformat ~display_format f ((source_file,decls),_) =
  match display_format with
  | Human_readable | Dev ->
    Format.fprintf f "%s declarations:\n" source_file ;
    List.iter (fun decl -> Format.fprintf f "%s\n" decl) decls

let declarations_jsonformat ((source_file,decls),_) : json =
  let json_decl = List.map (fun decl -> `String decl) decls in
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

let contract_size_ppformat ~display_format f (contract_size,_) =
  match display_format with
  | Human_readable | Dev ->
    Format.fprintf f "%d bytes" contract_size

let contract_size_jsonformat (contract_size,_) : json =
  `Int contract_size

let contract_size_format : 'a format = {
  pp = contract_size_ppformat;
  to_json = contract_size_jsonformat;
}