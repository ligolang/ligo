open Api_helpers

(* open Simple_utils.Display *)
module OS = Bos.OS
module Cmd = Bos.Cmd
module Trace = Simple_utils.Trace
module Constants = Cli_helpers.Constants
module T = Core

type project_entity =
  [ `LIBRARY
  | `CONTRACT
  ]

let idregex = Str.regexp "[0-9]+\\.[0-9]+\\.[0-9]+"

let contract_template_url_map =
  Map.of_alist_exn
    (module String)
    [ "NFT-factory-cameligo", "https://github.com/ligolang/NFT-factory-cameligo"
    ; "NFT-factory-jsligo", "https://github.com/ligolang/NFT-factory-jsligo"
    ; "randomness-cameligo", "https://github.com/ligolang/randomness-cameligo"
    ; "randomness-jsligo", "https://github.com/ligolang/randomness-jsligo"
    ; "shifumi-cameligo", "https://github.com/ligolang/shifumi-cameligo"
    ; "shifumi-jsligo", "https://github.com/ligolang/shifumi-jsligo"
    ; "multisig-cameligo", "https://github.com/ligolang/multisig-cameligo"
    ; "multisig-jsligo", "https://github.com/ligolang/multisig-jsligo"
    ; "advisor-cameligo", "https://github.com/ligolang/advisor-cameligo"
    ; "advisor-jsligo", "https://github.com/ligolang/advisor-jsligo"
    ; ( "predictive-market-cameligo"
      , "https://github.com/ligolang/predictive-market-cameligo" )
    ; "predictive-market-jsligo", "https://github.com/ligolang/predictive-market-jsligo"
    ; "permit-cameligo", "https://github.com/ligolang/permit-cameligo"
    ; "permit-jsligo", "https://github.com/ligolang/permit-jsligo"
    ; "dao-cameligo", "https://github.com/ligolang/dao-cameligo"
    ; "dao-jsligo", "https://github.com/ligolang/dao-jsligo"
    ]


let library_template_url_map =
  Map.of_alist_exn
    (module String)
    [ "ligo-bigarray", "https://github.com/ligolang/bigarray-cameligo"
    ; "ligo-math-lib", "https://github.com/ligolang/math-lib-cameligo"
    ; "ligo-fa", "https://github.com/ligolang/contract-catalogue"
    ; "ligo-permit", "https://github.com/ligolang/permit-cameligo"
    ; "ligo-breathalyzer", "https://github.com/marigold-dev/breathalyzer"
    ; "ligo-extendable-fa2", "https://github.com/smart-chain-fr/ligoExtendableFA2"
    ]


let determine_map ~kind =
  match kind with
  | `CONTRACT -> contract_template_url_map
  | `LIBRARY -> library_template_url_map


let list' ~kind = List.sort ~compare:String.compare @@ Map.keys (determine_map ~kind)

let list ~kind ~display_format () =
  format_result ~display_format Formatter.list_format @@ fun ~raise:_ -> list' ~kind


let new_project'
    ~(raise : (Main_errors.all, Main_warnings.all) Trace.raise)
    ~kind
    ~template
    ~project_name
    ~version
  =
  let project_url =
    match Map.find (determine_map ~kind) template with
    | Some e -> e
    | None -> raise.error (`Ligo_init_unrecognized_template (list' ~kind))
  in
  let _ = Cli_helpers.run_command (Constants.git_clone ~project_url ~project_name) in
  let is_release = Str.string_match idregex version 0 in
  let _ =
    if is_release
    then (
      let _ =
        Cli_helpers.run_command
          (Constants.git_checkout ~ref:version ~dir_path:project_name)
      in
      ())
    else ()
  in
  let _ = OS.Dir.delete ~recurse:true @@ Fpath.(v "." / project_name / ".git") in
  [ project_name ]


let new_project ~version ~kind ~project_name_opt ~template ~display_format () =
  let project_name =
    match project_name_opt with
    | Some e -> e
    | None -> template
  in
  format_result ~display_format Formatter.new_project_format
  @@ fun ~raise -> new_project' ~raise ~kind ~template ~project_name ~version
