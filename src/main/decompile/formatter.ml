open Display
open Simple_utils.Runned_result

let failwith_to_string (f:failwith) : string =
  let str = match f with
  | Failwith_int i -> string_of_int i
  | Failwith_string s -> Format.asprintf "\"%s\"" (String.escaped s)
  | Failwith_bytes b ->
    Format.asprintf "0X%a" Hex.pp (Hex.of_bytes b) in
  Format.asprintf "failwith(%s)" str

let expression_ppformat ~display_format (runned_result,_) =
  match display_format with
  | Display.Human_readable | Dev -> (
    match runned_result with
    | Fail fail_res ->
      (Location.dummy, failwith_to_string fail_res)
    | Success typed ->
      let buffer = Buffer.create 100 in
      let formatter = Format.formatter_of_buffer buffer in
      Ast_core.PP.expression formatter typed;
      Format.pp_print_flush formatter ();
      (Location.dummy, Buffer.contents buffer)      
  )

let expression_jsonformat (runned_result,_) : Display.json =
  match runned_result with
  | Fail fail_res ->
    let failstring = failwith_to_string fail_res in
    `Assoc [("value", `Null) ; ("failure", `String failstring)]
  | Success typed ->
    `Assoc [("value", Ast_core.Yojson.expression typed) ; ("failure", `Null)]

let expression_format : 'a Display.format = {
  pp = expression_ppformat ;
  to_json = expression_jsonformat ;
}
