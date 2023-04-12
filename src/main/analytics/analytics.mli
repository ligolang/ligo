open Compiler_options
open Prometheus_push

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

type analytics_input =
  { group : metric_group
  ; metric_value : float
  }

type analytics_inputs = analytics_input list

(* Analytics *)
val update_term_acceptance : string -> (string * string, string * string) result
val determine_syntax_label : string -> string -> string
val generate_cli_metric : command:string -> analytics_input

val generate_cli_metrics_with_syntax_and_protocol
  :  command:string
  -> raw_options:Raw_options.t
  -> ?source_file:string
  -> unit
  -> analytics_input list

val push_collected_metrics : skip_analytics:bool -> unit
val propose_term_acceptation : skip_analytics:bool -> unit
val edit_metrics_values : analytics_inputs -> unit
