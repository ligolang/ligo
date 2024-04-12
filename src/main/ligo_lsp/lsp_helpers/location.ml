include Lsp.Types.Location
module DocumentUri = Document_uri (* Required for deriving *)
module Range = Range (* Required for fixating the sequence of build *)

(** A location represents a range paired with the URI that contains that range. *)
type t = [%import: Lsp.Types.Location.t] [@@deriving eq, ord, sexp]

let pp = Helpers_pretty.pp_with_yojson yojson_of_t
let testable : t Alcotest.testable = Alcotest.testable pp equal
