open Imports

include Lsp.Types.Location

let of_region : Region.t -> t =
  fun region ->
   create
     ~uri:(Lsp.Types.DocumentUri.of_path region#file)
     ~range:(Range.of_region region)
