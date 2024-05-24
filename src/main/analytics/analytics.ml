module OS = Bos.OS
open Prometheus_push
open Prometheus
open Core
open Compiler_options.Raw_options

(* dirty, but simple *)
let project_root : string option ref = ref None
let set_project_root : string option -> unit = fun s -> project_root := s
let is_running_lsp_ref = ref false
let set_is_running_lsp is_running = is_running_lsp_ref := is_running

let dot_ligo rest use_home_folder =
  let home =
    match Sys.getenv "HOME" with
    | Some v -> v
    | None ->
      (match Sys.getenv "USERPROFILE" with
      | Some v -> v
      | None -> "")
  in
  (match use_home_folder with
  | true -> home ^ "/.ligo"
  | false ->
    (match !project_root with
    | None -> "./.ligo"
    | Some x -> x ^ "/.ligo"))
  ^/ rest


(* Types *)
type environment =
  | Ci
  | Tty
  | Docker_non_interactive
  | Docker_interactive
  | Lsp

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
  | Counter_lsp_initialize of
      { syntax : string
      ; ide : string
      ; ide_version : string
      ; agg_id : Uuid.t
      }
  | Counter_lsp_restart
  | Counter_lsp_number_of_crashes of
      { user : string
      ; agg_id : Uuid.t
      }
  | Gauge_lsp_method_time of
      { user : string
      ; agg_id : Uuid.t
      ; name : string
      ; index : int
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


let current_process_is_in_docker =
  match Sys.getenv "DOCKER_EXECUTION" with
  | Some _value -> true
  | None -> false


let is_running_lsp () = !is_running_lsp_ref
let is_tty = UnixLabels.isatty UnixLabels.stdin && UnixLabels.isatty UnixLabels.stdout

let env () : environment =
  if is_running_lsp ()
  then Lsp
  else (
    match current_process_is_in_docker, is_tty with
    | false, false -> Ci (* = neither docker not tty *)
    | false, true -> Tty
    | true, false -> Docker_non_interactive
    | true, true -> Docker_interactive)


let is_in_ci () =
  match env () with
  | Ci -> true
  | _ -> false


(* Collector registry *)
let agg_registry =
  (* If you change this string, then also change the one in the debugger in
     [tools/debugger/ligo-debugger/src/Language/LIGO/Analytics.hs]. Look for the
     [aggRegistry] variable. *)
  PushableCollectorRegistry.create "https://agg.push.analytics.ligolang.org/metrics"


let registry =
  PushableCollectorRegistry.create
    "https://push.analytics.ligolang.org/metrics/job/analytics"


let term_acceptance_filepath =
  if current_process_is_in_docker
  then ".ligo/term_acceptance"
  else dot_ligo "term_acceptance" true


let line_separator = "\n"

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


let acceptance_condition () =
  acceptance_condition_common
  ^ line_separator
  ^ line_separator
  ^
  match env () with
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


let counter_lsp_initialize =
  Counter.v_labels
    ~label_names:
      [ "user"; "repository"; "version"; "syntax"; "ide"; "ide_version"; "agg_id" ]
    ~registry:agg_registry.collectorRegistry
    ~help:"ligo lsp initialize request"
    ~namespace:"ligo"
    ~subsystem:"tooling"
    "lsp_initialize"


let counter_lsp_restart =
  Counter.v_labels
    ~label_names:[ "user"; "repository"; "version" ]
    ~registry:agg_registry.collectorRegistry
    ~help:"ligo lsp restarts counter"
    ~namespace:"ligo"
    ~subsystem:"tooling"
    "lsp_restart"


let counter_lsp_number_of_crashes =
  Counter.v_labels
    ~label_names:[ "repository"; "version"; "user"; "agg_id" ]
    ~registry:agg_registry.collectorRegistry
    ~help:"ligo lsp number of crashes on keystrokes"
    ~namespace:"ligo"
    ~subsystem:"tooling"
    "lsp_number_of_crashes"


(* HACK: [Gauge.v_labels] can't be called more than once, otherwise it will fail since the
   metric group will already be registered. Hence we cache it for use by
   [gauge_lsp_method_time] while its metrics are not pushed and its cache not cleared.

   Moreover, the [prometheus] library offers no way to clear pushed metrics, so they'd get
   pushed every time to the server. The [prometheus_push] library tries to get around this
   by "resetting" the collector registry (in this case, [registry.collectorRegistry] with
   [Prometheus.CollectorRegistry.clear ()]), which also ends up erasing its labels.
   Therefore, we need to clear this table in [push_collected_metrics_scheduled] to
   recreate the gauges... *)
let method_tbl = Hashtbl.create (module String)

let gauge_lsp_method_time method_name =
  Hashtbl.find_or_add method_tbl method_name ~default:(fun () ->
      Gauge.v_labels
        ~label_names:[ "repository"; "version"; "user"; "agg_id"; "index" ]
        ~registry:registry.collectorRegistry
        ~help:"ligo lsp method execution time in miliseconds with index of metric"
        ~namespace:"ligo"
        ~subsystem:"tooling"
        (* LSP methods have names like "textDocument/semanticTokens/range", we want to
           replace these slashes with another character since Prometheus doesn't allow
           slashes in names. More specifically, they must match the following regex:
           ^[a-zA-Z_:][a-zA-Z0-9_:]*$ *)
        (Format.sprintf
           "lsp_method_execution_time:%s"
           (String.substr_replace_all ~pattern:"/" ~with_:"_" method_name)))


let create_id () = Format.asprintf "%a" Uuid.pp (Uuid.create_random Random.State.default)

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


let accepted = "accepted"
let denied = "denied"

let is_term_accepted () =
  let term_acceptance = read_file term_acceptance_filepath in
  String.equal term_acceptance accepted


let is_in_docker () =
  match env () with
  | Docker_non_interactive -> true
  | Docker_interactive -> true
  | _ -> false


let is_dev_version () =
  (String.is_prefix Version.version ~prefix:"Rolling release"
  || String.is_empty Version.version)
  && not (is_in_docker ())


let rec get_user_answer () =
  match In_channel.input_line In_channel.stdin with
  | Some v ->
    (match v with
    | "y" -> accepted
    | "n" -> denied
    | _ -> get_user_answer ())
  | None ->
    (match env () with
    | Docker_non_interactive -> accepted
    | _ -> denied)


let accept () = store accepted term_acceptance_filepath
let deny () = store denied term_acceptance_filepath

let should_propose_analytics ~skip_analytics =
  not
    (skip_analytics
    || is_skip_analytics_through_env_var
    || is_term_already_proposed ()
    || is_in_ci ()
    || is_dev_version ())


let propose_term_acceptation ~skip_analytics =
  if not (should_propose_analytics ~skip_analytics)
  then ()
  else (
    Format.eprintf "%s\n%!" (acceptance_condition ());
    let user_answer = get_user_answer () in
    store user_answer term_acceptance_filepath)


(* User id *)
let get_user_id () =
  let user_id =
    if current_process_is_in_docker
    then "docker"
    else get_or_create_id (dot_ligo "user_id" true)
  in
  user_id


(* Repository id *)
let get_repository_id () = get_or_create_id (dot_ligo "repository_id" false)

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


let should_push_metrics ~skip_analytics =
  not
    (skip_analytics
    || (not (is_term_accepted ()))
    || is_in_ci ()
    || is_skip_analytics_through_env_var
    || is_dev_version ())


let push_collected_metrics ~skip_analytics =
  if should_push_metrics ~skip_analytics
  then (
    let p_1 =
      let%lwt _ = PushableCollectorRegistry.push agg_registry in
      Lwt.return_unit
    in
    let p_2 =
      let%lwt _ = PushableCollectorRegistry.push registry in
      Lwt.return_unit
    in
    Lwt.join [ p_1; p_2 ])
  else Lwt.return_unit


let determine_syntax_label_from_source source : string =
  match Filename.split_extension source with
  | _, Some "mligo" -> "CameLIGO"
  | _, Some "jsligo" -> "JsLIGO"
  | _ -> "invalid"


let determine_syntax_label syntax source : string =
  match syntax, source with
  | "auto", source -> determine_syntax_label_from_source source
  | ("cameligo" | "CameLIGO"), _ -> "CameLIGO"
  | ("jsligo" | "JsLIGO"), _ -> "JsLIGO"
  | _ -> "invalid"


let generate_cli_metric ~command =
  { group = Counter_cli_execution { command }; metric_value = 1.0 }


let generate_cli_metrics_with_syntax_and_protocol ~command ~raw_options ?source_file () =
  let source =
    match source_file with
    | Some filepath -> filepath
    | None -> ""
  in
  let syntax = determine_syntax_label raw_options.syntax source in
  let execution_metric = generate_cli_metric ~command in
  let run_metric =
    { group =
        Counter_cli_execution_by_syntax_and_protocol
          { command; syntax; protocol = Memory_proto_alpha.protocol_str }
    ; metric_value = 1.0
    }
  in
  [ execution_metric; run_metric ]


let generate_lsp_initialize_metrics ~syntax ~ide ~ide_version ~session_id () =
  let run_metric =
    { group = Counter_lsp_initialize { syntax; ide; ide_version; agg_id = session_id }
    ; metric_value = 1.0
    }
  in
  [ run_metric ]


let generate_lsp_restart_metrics () =
  let run_metric = { group = Counter_lsp_restart; metric_value = 1.0 } in
  [ run_metric ]


let generate_lsp_number_of_crashes ~session_id ~number_of_crashes_on_keystrokes () =
  let run_metric =
    { group = Counter_lsp_number_of_crashes { user = get_user_id (); agg_id = session_id }
    ; metric_value = float_of_int number_of_crashes_on_keystrokes
    }
  in
  [ run_metric ]


let generate_lsp_method_time ~session_id ~name ~times () =
  let user = get_user_id () in
  let make_run_metric index time =
    { group = Gauge_lsp_method_time { user; agg_id = session_id; name; index }
    ; metric_value = Time_float.Span.to_ms time
    }
  in
  List.mapi ~f:make_run_metric times


let get_family_by_group
    : metric_group -> [ `Ctr of Counter.family | `Gauge of Gauge.family ]
  = function
  | Counter_cli_execution _ -> `Ctr counter_cli_execution_group
  | Counter_cli_execution_by_syntax_and_protocol _ ->
    `Ctr counter_cli_execution_by_syntax_and_protocol
  | Counter_cli_transpile _ -> `Ctr counter_cli_transpilation_group
  | Counter_cli_init _ -> `Ctr counter_cli_init_group
  | Gauge_compilation_size _ -> `Gauge gauge_compilation_size_group
  | Counter_lsp_initialize _ -> `Ctr counter_lsp_initialize
  | Counter_lsp_restart -> `Ctr counter_lsp_restart
  | Counter_lsp_number_of_crashes _ -> `Ctr counter_lsp_number_of_crashes
  | Gauge_lsp_method_time { name; _ } -> `Gauge (gauge_lsp_method_time name)


let get_labels_from_group : metric_group -> string list = function
  | Counter_cli_execution { command } -> [ command ]
  | Counter_cli_execution_by_syntax_and_protocol { command; syntax; protocol } ->
    [ command; syntax; protocol ]
  | Counter_cli_transpile { command; old_syntax; new_syntax } ->
    [ command; old_syntax; new_syntax ]
  | Counter_cli_init { command; template } -> [ command; template ]
  | Gauge_compilation_size { contract_discriminant; syntax; protocol } ->
    [ contract_discriminant; syntax; protocol ]
  | Counter_lsp_initialize { syntax; ide; ide_version; agg_id } ->
    [ syntax; ide; ide_version; Uuid.to_string agg_id ]
  | Counter_lsp_restart -> []
  | Counter_lsp_number_of_crashes { user; agg_id } -> [ user; Uuid.to_string agg_id ]
  | Gauge_lsp_method_time { user; agg_id; name = _; index } ->
    [ user; Uuid.to_string agg_id; Int.to_string index ]


let edit_metric_value : analytics_input -> unit =
 fun { group; metric_value } ->
  let counter_group = get_family_by_group group in
  let labels = get_labels_from_group group in
  match counter_group with
  | `Ctr counter_group -> inc ~counter_group ~labels ~value:metric_value
  | `Gauge gauge_group -> set ~gauge_group ~labels ~value:metric_value


let edit_metrics_values : analytics_inputs -> unit = List.iter ~f:edit_metric_value

let push_collected_metrics_scheduled
    ~skip_analytics
    ~time_between_pushes
    ~should_stop
    ~collect_metrics
  =
  while%lwt should_push_metrics ~skip_analytics && not (should_stop ()) do
    let should_stop_task () =
      while%lwt not (should_stop ()) do
        (* Wait for a second to not get 100% CPU usage. *)
        let%lwt () = Lwt_unix.sleep 1. in
        Lwt.pause ()
      done
    in
    let%lwt () =
      Lwt.pick
        [ should_stop_task ()
        ; Lwt_unix.sleep (Time_float.Span.to_sec time_between_pushes)
        ]
    in
    let () = collect_metrics () in
    let%lwt () =
      try%lwt
        let%lwt _ = PushableCollectorRegistry.push registry in
        Lwt.return_unit
      with
      | _ -> Lwt.return_unit
    in
    (* See the HACK session in [method_tbl] for the reason this is needed. *)
    Hashtbl.clear method_tbl;
    Lwt.return_unit
  done
