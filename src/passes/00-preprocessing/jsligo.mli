(* Interfacing the preprocessor *)

(* Directories and files *)

type file_path = string
type dirs = file_path list (* #include and #import *)

(* Results *)

module Errors = Preprocessing_shared.Errors

type success = Preprocessor.API.success
type nonrec result  = (success, Errors.t) result

(* Preprocessing various sources *)

val from_file    : ?project_root:file_path -> dirs -> file_path  -> result
val from_string  : ?project_root:file_path -> dirs -> string     -> result
val from_channel : ?project_root:file_path -> dirs -> In_channel.t -> result

(* Aliases *)

val preprocess_file    : ?project_root:file_path -> dirs -> file_path  -> result
val preprocess_string  : ?project_root:file_path -> dirs -> string     -> result
val preprocess_channel : ?project_root:file_path -> dirs -> In_channel.t -> result
