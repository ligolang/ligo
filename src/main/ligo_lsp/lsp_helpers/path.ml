open Imports

type t = UnsafePath of string [@@unboxed] [@@deriving eq, ord, sexp, hash]
type normalization = string -> t

let to_string (UnsafePath a) = a

let normalise : string -> string =
 fun path -> if Caml.Sys.file_exists path then Filename_unix.realpath path else path


let from_absolute : normalization = fun p -> UnsafePath (normalise p)

let from_relative : normalization =
 fun p ->
  let abs_path = Filename.concat (Caml.Sys.getcwd ()) p in
  from_absolute abs_path


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


let pp (ppf : Format.formatter) : t -> unit = Format.fprintf ppf "%s" <@ to_string
let testable : t Alcotest.testable = Alcotest.testable pp equal
