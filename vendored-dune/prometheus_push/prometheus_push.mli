module PushableCollectorRegistry :
  sig
    type t = {
      url : Uri.t;
      mutable collectorRegistry : Prometheus.CollectorRegistry.t;
    }
    val set_default : string -> unit
    val get_default : unit -> t
    val create : string -> t
    val push : t -> (string * string, string * string) result Lwt.t
  end
