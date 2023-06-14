include Pretty_check
include Should_match_list

(* Like Aloctest.string, but without escaping *)
let raw_string : string Alcotest.testable =
  let pp_string ppf x = Fmt.pf ppf "%s" x in
  Alcotest.testable pp_string Caml.( = )


(** Like [should_match_list], but as a [Alcotest.testable]. Might not give
    friendly error messages. *)
let unordered_list (testable_a : 'a Alcotest.testable) : 'a list Alcotest.testable =
  let pp ppf =
    Format.fprintf ppf "(* unordered list *) %a" (Fmt.Dump.list (Alcotest.pp testable_a))
  in
  let eq actual expected =
    match match_list ~actual ~expected ~eq:(Alcotest.equal testable_a) with
    | [], [] -> true
    | _, _ -> false
  in
  Alcotest.testable pp eq
