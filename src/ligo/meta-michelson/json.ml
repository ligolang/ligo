let force_record ~msg json = match json with
  | `O json -> json
  | _ -> raise @@ Failure ("not json record : " ^ msg)

let force_string ~msg json = match json with
  | `String str -> str
  | _ -> raise @@ Failure ("not json str : " ^ msg)
