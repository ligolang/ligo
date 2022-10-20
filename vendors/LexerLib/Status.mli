(* Status after parsing the CLI *)

type t = [
  Preprocessor.Status.t          (* Preprocessor CLI options      *)
| `Conflict of string * string   (* Conflicting various [command] *)
]

type status = t

val status : t
