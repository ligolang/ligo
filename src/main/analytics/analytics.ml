module OS = Bos.OS
open Prometheus_push
open Prometheus
open Core
open Compiler_options.Raw_options

(* Types *)
type environment =
  | Ci
  | Tty
  | Docker_non_interactive
  | Docker_interactive

type metric_group =
  | Counter_cli_execution of { command : string }
  | Counter_cli_execution_by_syntax_and_protocol of
      { command : string
      ; syntax : string
      ; protocol : string
      }
  | Counter_cli_transpile of
      { command : string
      ; old_syntax : string
      ; new_syntax : string
      }
  | Counter_cli_init of
      { command : string
      ; template : string
      }
  | Gauge_compilation_size of
      { contract_discriminant : string
      ; syntax : string
      ; protocol : string
      }

type analytics_input =
  { group : metric_group
  ; metric_value : float
  }

type analytics_inputs = analytics_input list

(* Utils *)
let is_skip_analytics_through_env_var =
  match Sys.getenv "LIGO_SKIP_ANALYTICS" with
  | Some _v -> true
  | None -> false


let get_home =
  match Sys.getenv "HOME" with
  | Some v -> v
  | None ->
    (match Sys.getenv "USERPROFILE" with
    | Some v -> v
    | None -> "")


let current_process_is_in_docker =
  match Sys.getenv "DOCKER_EXECUTION" with
  | Some _value -> true
  | None -> false


let is_tty = UnixLabels.isatty UnixLabels.stdin && UnixLabels.isatty UnixLabels.stdout

let env : environment =
  match current_process_is_in_docker, is_tty with
  | false, false -> Ci (* = neither docker not tty *)
  | false, true -> Tty
  | true, false -> Docker_non_interactive
  | true, true -> Docker_interactive


let is_in_ci =
  match env with
  | Ci -> true
  | _ -> false


(* Collector registry *)
let agg_registry =
  PushableCollectorRegistry.create "https://agg.push.analytics.ligolang.org/metrics"


let registry =
  PushableCollectorRegistry.create
    "https://push.analytics.ligolang.org/metrics/job/analytics"


let term_acceptance_filepath =
  if current_process_is_in_docker
  then ".ligo/term_acceptance"
  else get_home ^ "/.ligo/term_acceptance"


let line_separator = if String.equal Sys.os_type "Win32" then "\r\n" else "\n"

let acceptance_condition_common =
  "Ligo uses analytics to have a better understanding compiler community usage."
  ^ line_separator
  ^ line_separator
  ^ "Ligo only collects data necessary to improve its services and stores it in \
     anonymized manner. Collected data can be shared through our communication \
     channel.Data collected can be stored for up to 5 years. If you want to delete any \
     collected \n\
    \     data, please contact us through one of our channel, such as \
     https://discord.com/invite/9rhYaEt."


let acceptance_condition =
  acceptance_condition_common
  ^ line_separator
  ^ line_separator
  ^
  match env with
  | Docker_non_interactive ->
    "When running Ligo through Docker, which is non-interactive, the terms will be \
     automatically accepted. However, it is still possible to use 'ligo analytics \
     refuse' to refuse the terms after the execution. The result is stored in ' "
    ^ term_acceptance_filepath
    ^ "'. If you share this file with another person using Docker, you will also be \
       sharing the policy. To avoid this, add '.ligo/term_acceptance' to your \
       '.gitignore' file."
  | _ ->
    "Your response will be stored in "
    ^ term_acceptance_filepath
    ^ line_separator
    ^ " To avoid seeing this message during CI, use the --skip-analytics flag. \
       Alternatively, set the LIGO_SKIP_ANALYTICS environment variable to true. If your \
       CI uses a Docker image, we recommend using the ligolang/ligo_ci image"
    ^ line_separator
    ^ " . If your change your mind, use the command `ligo analytics accept` or `ligo \
       analytics deny`."
    ^ line_separator
    ^ line_separator
    ^ "Do you agree to transmit anonymous data to Ligo ? (y/n)"
    ^ line_separator


(* Metrics *)
let counter_cli_execution_group =
  Counter.v_labels
    ~label_names:[ "user"; "repository"; "version"; "command" ]
    ~registry:agg_registry.collectorRegistry
    ~help:"ligo execution"
    ~namespace:"ligo"
    ~subsystem:"cli"
    "command"


let counter_cli_transpilation_group =
  Counter.v_labels
    ~label_names:
      [ "user"; "repository"; "version"; "command"; "old_syntax"; "new_syntax" ]
    ~registry:agg_registry.collectorRegistry
    ~help:"ligo compile execution"
    ~namespace:"ligo"
    ~subsystem:"cli"
    "transpile"


let counter_cli_execution_by_syntax_and_protocol =
  Counter.v_labels
    ~label_names:[ "user"; "repository"; "version"; "command"; "syntax"; "protocol" ]
    ~registry:agg_registry.collectorRegistry
    ~help:"ligo run execution"
    ~namespace:"ligo"
    ~subsystem:"cli"
    "execution_by_syntax_and_protocol"


let counter_cli_init_group =
  Counter.v_labels
    ~label_names:[ "user"; "repository"; "version"; "command"; "template" ]
    ~registry:agg_registry.collectorRegistry
    ~help:"ligo init execution"
    ~namespace:"ligo"
    ~subsystem:"cli"
    "init"


let gauge_compilation_size_group =
  Gauge.v_labels
    ~label_names:[ "repository"; "version"; "project"; "syntax"; "protocol" ]
    ~registry:registry.collectorRegistry
    ~help:"ligo compilation size"
    ~namespace:"ligo"
    ~subsystem:"compile"
    "compilation_size"


let create_id () =
  Format.asprintf "%a" Uuid.pp (Uuid.create_random Core.Random.State.default)


let read_file path =
  match OS.File.read Fpath.(v path) with
  | Ok file_content -> file_content
  | Error _msg -> ""


let check_id_presence id_filepath =
  match OS.File.exists @@ Fpath.(v id_filepath) with
  | Ok exist -> exist
  | Error _message -> false


let store str filepath =
  let _ =
    match OS.Dir.create Fpath.(v (Filename.dirname filepath)) with
    | Error msg -> Rresult.R.pp_msg Format.err_formatter msg
    | _ -> ()
  in
  match OS.File.write Fpath.(v filepath) str with
  | Error msg -> Rresult.R.pp_msg Format.err_formatter msg
  | _ -> ()


let get_or_create_id id_store_filepath =
  if check_id_presence id_store_filepath
  then read_file id_store_filepath
  else (
    let id = create_id () in
    store id id_store_filepath;
    id)


(* Term *)
let update_term_acceptance value =
  store value term_acceptance_filepath;
  Ok ("", "")


let is_term_already_proposed () =
  match OS.File.exists @@ Fpath.(v term_acceptance_filepath) with
  | Ok exist -> exist
  | Error _ -> false


let is_term_accepted () =
  let term_acceptance = read_file term_acceptance_filepath in
  if String.equal term_acceptance "accepted" then true else false


let is_in_docker =
  match env with
  | Docker_non_interactive -> true
  | Docker_interactive -> true
  | _ -> false


let is_dev_version =
  (String.is_prefix Version.version ~prefix:"Rolling release"
  || String.is_empty Version.version)
  && not is_in_docker


let rec accept () =
  match In_channel.input_line In_channel.stdin with
  | Some v ->
    (match v with
    | "y" -> "accepted"
    | "n" -> "denied"
    | _ -> accept ())
  | None ->
    (match env with
    | Docker_non_interactive -> "accepted"
    | _ -> "denied")


let propose_term_acceptation ~skip_analytics =
  if skip_analytics
     || is_skip_analytics_through_env_var
     || is_term_already_proposed ()
     || is_in_ci
     || is_dev_version
  then ()
  else (
    Format.eprintf "%s\n%!" acceptance_condition;
    let user_answer = accept () in
    store user_answer term_acceptance_filepath)


(* User id *)
let get_user_id () =
  let user_id =
    if current_process_is_in_docker
    then "docker"
    else get_or_create_id (get_home ^ "/.ligo/user_id")
  in
  user_id


(* Repository id *)
let get_repository_id () = get_or_create_id ".ligo/repository_id"

(* Analytics *)
let set ~gauge_group ~labels ~value =
  let repository_id = get_repository_id () in
  let gauge = Gauge.labels gauge_group (repository_id :: Version.version :: labels) in
  Gauge.set gauge value;
  ()


(* Analytics *)
let inc ~counter_group ~labels ~value =
  let user_id = get_user_id () in
  let repository_id = get_repository_id () in
  let counter =
    Counter.labels counter_group (user_id :: repository_id :: Version.version :: labels)
  in
  Counter.inc counter value;
  ()


let push_collected_metrics ~skip_analytics =
  if skip_analytics
     || (not (is_term_accepted ()))
     || is_in_ci
     || is_skip_analytics_through_env_var
     || is_dev_version
  then ()
  else (
    let p_1 () =
      let _ = PushableCollectorRegistry.push agg_registry in
      Lwt.return_unit
    in
    let p_2 () =
      let _ = PushableCollectorRegistry.push registry in
      Lwt.return_unit
    in
    let p_3 = Lwt.join [ p_1 (); p_2 () ] in
    Lwt_main.run p_3;
    ())


let determine_syntax_label_from_source source : string =
  let ext = Caml.Filename.extension source in
  match ext with
  | ".mligo" -> "CameLIGO"
  | ".jsligo" -> "JsLIGO"
  | _ -> "invalid"


let determine_syntax_label syntax source : string =
  match syntax, source with
  | "auto", source -> determine_syntax_label_from_source source
  | ("cameligo" | "CameLIGO"), _ -> "CameLIGO"
  | ("jsligo" | "JsLIGO"), _ -> "JsLIGO"
  | _ -> "invalid"


let get_protocol_label ~raw_options =
  match raw_options.protocol_version with
  | "current" -> Environment.Protocols.variant_to_string Environment.Protocols.current
  | _ -> raw_options.protocol_version


let generate_cli_metric ~command =
  { group = Counter_cli_execution { command }; metric_value = 1.0 }


let generate_cli_metrics_with_syntax_and_protocol ~command ~raw_options ?source_file () =
  let source =
    match source_file with
    | Some filepath -> filepath
    | None -> ""
  in
  let syntax = determine_syntax_label raw_options.syntax source in
  let protocol_version = get_protocol_label ~raw_options in
  let execution_metric = generate_cli_metric ~command in
  let run_metric =
    { group =
        Counter_cli_execution_by_syntax_and_protocol
          { command; syntax; protocol = protocol_version }
    ; metric_value = 1.0
    }
  in
  [ execution_metric; run_metric ]


let get_family_by_group
    : metric_group -> [ `Ctr of Counter.family | `Gauge of Gauge.family ]
  = function
  | Counter_cli_execution _ -> `Ctr counter_cli_execution_group
  | Counter_cli_execution_by_syntax_and_protocol _ ->
    `Ctr counter_cli_execution_by_syntax_and_protocol
  | Counter_cli_transpile _ -> `Ctr counter_cli_transpilation_group
  | Counter_cli_init _ -> `Ctr counter_cli_init_group
  | Gauge_compilation_size _ -> `Gauge gauge_compilation_size_group


let get_labels_from_group : metric_group -> string list = function
  | Counter_cli_execution { command } -> [ command ]
  | Counter_cli_execution_by_syntax_and_protocol { command; syntax; protocol } ->
    [ command; syntax; protocol ]
  | Counter_cli_transpile { command; old_syntax; new_syntax } ->
    [ command; old_syntax; new_syntax ]
  | Counter_cli_init { command; template } -> [ command; template ]
  | Gauge_compilation_size { contract_discriminant; syntax; protocol } ->
    [ contract_discriminant; syntax; protocol ]


let edit_metric_value : analytics_input -> unit =
 fun { group; metric_value } ->
  let counter_group = get_family_by_group group in
  let labels = get_labels_from_group group in
  match counter_group with
  | `Ctr counter_group -> inc ~counter_group ~labels ~value:metric_value
  | `Gauge gauge_group -> set ~gauge_group ~labels ~value:metric_value


let edit_metrics_values : analytics_inputs -> unit = List.iter ~f:edit_metric_value
