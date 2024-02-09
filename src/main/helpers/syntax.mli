module Trace = Simple_utils.Trace

val of_string_opt
  :  raise:(Main_errors.all, Main_warnings.all) Trace.raise
  -> Syntax_types.s_syntax
  -> string option
  -> Syntax_types.t

val to_string : Syntax_types.t -> string
val to_ext : Syntax_types.t -> string
val of_ext_opt : string option -> Syntax_types.t option

(** Tests whether the given string has a valid CameLIGO suffix. *)
val is_cameligo : string -> bool

(** Tests whether the given string has a valid JsLIGO suffix. *)
val is_jsligo : string -> bool

(** Tests whether the given string has a valid LIGO suffix. *)
val is_ligo : string -> bool

(** A glob pattern for CameLIGO files. *)
val cameligo_glob : string

(** A glob pattern for JsLIGO files. *)
val jsligo_glob : string
