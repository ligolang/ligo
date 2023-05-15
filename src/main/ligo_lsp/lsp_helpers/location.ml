open Imports
include Lsp.Types.Location

let eq = Caml.( = )
let pp = Helpers_pretty.pp_with_yojson yojson_of_t
let testable : t Alcotest.testable = Alcotest.testable pp eq

let of_region : Region.t -> t =
 fun region ->
  let uri =
    if Sys.unix
    then Lsp.Types.DocumentUri.of_path region#file
    else
      region#file
      |> Path.normalise_backslashes
      |> Str.global_replace (Str.regexp "//") "/" (* Sometimes double fwd slashes were seen in the output *)
      |> Caml.String.lowercase_ascii
      |> Lsp.Types.DocumentUri.of_path
  in
  create ~uri ~range:(Range.of_region region)
