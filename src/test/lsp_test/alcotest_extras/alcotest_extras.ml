include Pretty_check
include Should_match_list

(* Like Aloctest.string, but without escaping *)
let raw_string : string Alcotest.testable =
  let pp_string ppf x = Fmt.pf ppf "%s" x in
  Alcotest.testable pp_string Caml.( = )
