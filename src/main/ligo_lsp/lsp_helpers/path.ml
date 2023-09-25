open Imports

(** [Path.t] is expected to be absolute and normalized
  (normalization is currently nontrivial only for Windows), [UnsafePath]
  allows to create a Path form string, make sure those points are satisfied when using it *)
type t = UnsafePath of string [@@unboxed] [@@deriving eq, ord, sexp, hash]
(* We don't need to normalize before equality check since we expect `Path.t` to contain
  a path that's already normalized*)

let to_string (UnsafePath a) = a

let normalise_backslashes : string -> string =
  Str.global_replace (Str.regexp "[\\|/]+") "/"


let normalise_case : string -> string = Caml.String.lowercase_ascii

let normalise : string -> string =
  if Sys.unix then Fun.id else normalise_case <@ normalise_backslashes


(** Like [to_string] but capitalizes the drive letter on Windows,
  so drive is e.g. "C:" instead of "c:")
    *)
let to_string_with_canonical_drive_letter : t -> string =
  if Sys.unix then to_string else String.capitalize <@ to_string


(** Create [Path.t] from a string containing absolute file path *)
let from_absolute : string -> t = fun p -> UnsafePath (normalise p)

(** Create [Path.t] from a string containing file path relative to current dir.
    Made for creating absolute paths in tests *)
let from_relative : string -> t =
 fun p ->
  let abs_path = Filename.concat (Caml.Sys.getcwd ()) p in
  from_absolute abs_path


let dirname : t -> t = fun p -> from_absolute @@ Filename.dirname @@ to_string p

(** Concat absolute path to dir and relative path inside this dir *)
let concat : t -> string -> t =
 fun p s -> from_absolute @@ Filename.concat (to_string p) s


let get_extension : t -> string option = snd <@ Filename.split_extension <@ to_string
let get_syntax = Syntax.of_ext_opt <@ get_extension

let pp (ppf : Format.formatter) : t -> unit =
  Format.fprintf ppf "%s" <@ to_string_with_canonical_drive_letter


let testable : t Alcotest.testable = Alcotest.testable pp equal
