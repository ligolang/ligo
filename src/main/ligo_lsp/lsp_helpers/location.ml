open Imports
include Lsp.Types.Location

let eq = Caml.( = )
let pp = Helpers_pretty.pp_with_yojson yojson_of_t
let testable : t Alcotest.testable = Alcotest.testable pp eq

let of_region : Region.t -> t =
 fun region ->
 let file = Path.normalise region#file in
  let uri = Lsp.Types.DocumentUri.of_path file in
  create ~uri ~range:(Range.of_region region)
