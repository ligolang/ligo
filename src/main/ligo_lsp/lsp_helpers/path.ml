open Imports

type t = UnsafePath of string [@@unboxed] [@@deriving eq, ord, sexp, hash]

let to_string (UnsafePath a) = a

let normalise_backslashes : string -> string =
  Str.global_replace (Str.regexp "[\\|/]+") "/"


let normalise_case : string -> string = Caml.String.lowercase_ascii

let normalise : string -> string =
 fun path ->
  if Sys.unix
  then if Caml.Sys.file_exists path then Filename_unix.realpath path else path
  else normalise_case @@ normalise_backslashes path


(** Like [to_string] but capitalizes the drive letter on Windows,
  so drive is e.g. "C:" instead of "c:")
    *)
let to_string_with_canonical_drive_letter : t -> string =
  if Sys.unix then to_string else String.capitalize <@ to_string


let from_absolute : string -> t = fun p -> UnsafePath (normalise p)

let from_relative : string -> t =
 fun p ->
  let abs_path = Filename.concat (Caml.Sys.getcwd ()) p in
  from_absolute abs_path


(** Create a filename which is relative to the base filename.
  The resulting type is [string] since it's expected for [Path.t] to be always absolute. *)
let make_relative : t -> t -> string =
 fun base p -> FilePath.make_relative (to_string base) (to_string p)


let dirname : t -> t = fun p -> from_absolute @@ Filename.dirname @@ to_string p

let concat : t -> string -> t =
 fun p s -> from_absolute @@ Filename.concat (to_string p) s


let get_extension : t -> string option = snd <@ Filename.split_extension <@ to_string
let get_syntax = Syntax.of_ext_opt <@ get_extension

let rec find_file_in_dir_and_parents dir file =
  let potential_file = concat dir file in
  if Caml.Sys.file_exists (to_string potential_file)
  then Some potential_file
  else (
    let parent = dirname dir in
    (* e.g.: [dirname "/" = "/"] *)
    if equal parent dir then None else find_file_in_dir_and_parents parent file)


let pp (ppf : Format.formatter) : t -> unit =
  Format.fprintf ppf "%s" <@ to_string_with_canonical_drive_letter


let testable : t Alcotest.testable = Alcotest.testable pp equal
