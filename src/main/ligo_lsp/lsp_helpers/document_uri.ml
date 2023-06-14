open Imports
include Lsp.Types.DocumentUri

type t = [%import: Lsp.Types.DocumentUri.t] [@@deriving eq, ord]

(* Uri type repr is not exported from LSP, so we need some dirty tricks *)
type internal_replication =
  { scheme : string
  ; authority : string
  ; path : string
  }
[@@deriving sexp]

let t_of_sexp : Sexp.t -> t = Obj.magic internal_replication_of_sexp
let sexp_of_t : t -> Sexp.t = Obj.magic sexp_of_internal_replication
let to_path : t -> Path.t = Path.from_absolute <@ to_path
let of_path : Path.t -> t = of_path <@ Path.to_string
let pp (ppf : Format.formatter) : t -> unit = Format.fprintf ppf "%s" <@ to_string

let%expect_test "uri to sexp" =
  let uri : t = Lsp.Uri.of_path "test/aaa" in
  print_s (sexp_of_t uri);
  [%expect {| ((scheme file) (authority "") (path /test/aaa)) |}]

let%test "uri from sexp" =
  let uri : t = Lsp.Uri.of_path "test/aaa" in
  let sexp = Sexp.of_string {| ((scheme file) (authority "") (path /test/aaa)) |} in
  equal (t_of_sexp sexp) uri
