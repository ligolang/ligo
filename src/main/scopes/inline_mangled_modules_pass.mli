(** Mapping from mangled UID to its resolved file name. *)
module UidMap : Core.Map.S with type Key.t = Types.Uid.t

type t = string UidMap.t

(** Preprocessing produces mangled modules.
    The following snippet:

    {[
     #import "file.mligo" "A"
    ]}

    Translates into:

    {[
     module A = Mangled_bla_bla
    ]}

    Thus we get a [Mangled_bla_bla] definition and [A] module alias.

    The idea here is to strip mangled definitions and inline them into each mangled alias.
    The motivation is simple: we don't care about mangled modules. We should think about
    them each time we're working with definitions (e.g. they may appear in completions).
    It would be better to inline and forget about them. *)
val patch : t -> Types.def list -> Types.def list
