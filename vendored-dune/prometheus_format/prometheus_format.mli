(** Format a snapshot in Prometheus's text format, version 0.0.4. *)
module TextFormat_0_0_4 : sig
  val output : Prometheus.CollectorRegistry.snapshot Fmt.t
end
