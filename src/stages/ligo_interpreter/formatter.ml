open Simple_utils.Display

let tests_ppformat ~display_format ~no_colour f (print_values, toplevel_env) =
  let pp_result ppf (n, v) =
    Format.fprintf ppf "- %s exited with value %a." n (PP.pp_value ~no_colour) v
  in
  let pp_toplevel_env ppf lst =
    Format.fprintf
      ppf
      "@[<v>%a@]"
      (Simple_utils.PP_helpers.list_sep pp_result (Simple_utils.PP_helpers.tag "@."))
      lst
  in
  match display_format with
  | (Dev | Human_readable) when print_values ->
    Format.fprintf
      f
      "@[<v>Everything at the top-level was executed.@.%a@]"
      pp_toplevel_env
      toplevel_env
  | _ -> ()


let tests_jsonformat (_, toplevel_env) : json = Types.toplevel_env_to_yojson toplevel_env
let tests_format : 'a format = { pp = tests_ppformat; to_json = tests_jsonformat }
