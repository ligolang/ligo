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
      |> Path.normalise
      |> Lsp.Types.DocumentUri.of_path
  in
  create ~uri ~range:(Range.of_region region)
