open Compiler_options
open Prometheus_push

val set_is_running_lsp : bool -> unit

(* Registry *)

val agg_registry : PushableCollectorRegistry.t
val registry : PushableCollectorRegistry.t

(* Types *)

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

(* Analytics *)
val acceptance_condition : unit -> string
val update_term_acceptance : string -> (string * string, string * string) result
val determine_syntax_label : string -> string -> string
val generate_cli_metric : command:string -> analytics_input
val accept : unit -> unit
val deny : unit -> unit
val should_propose_analytics : skip_analytics:bool -> bool

val generate_cli_metrics_with_syntax_and_protocol
  :  command:string
  -> raw_options:Raw_options.t
  -> ?source_file:string
  -> unit
  -> analytics_inputs

val generate_lsp_initialize_metrics
  :  syntax:string
  -> ide:string
  -> ide_version:string
  -> session_id:Uuid.t
  -> unit
  -> analytics_inputs

val generate_lsp_restart_metrics : unit -> analytics_inputs

val generate_lsp_number_of_crashes
  :  session_id:Uuid.t
  -> number_of_crashes_on_keystrokes:int
  -> unit
  -> analytics_inputs

val generate_lsp_method_time
  :  session_id:Uuid.t
  -> name:string
  -> times:Core_private.Span_float.t list
  -> unit
  -> analytics_inputs

val push_collected_metrics_scheduled
  :  skip_analytics:bool
  -> time_between_pushes:Time_float.Span.t
  -> should_stop:(unit -> bool)
  -> collect_metrics:(unit -> unit)
  -> unit Lwt.t

val push_collected_metrics : skip_analytics:bool -> unit Lwt.t
val propose_term_acceptation : skip_analytics:bool -> unit
val edit_metrics_values : analytics_inputs -> unit
val set_project_root : string option -> unit
