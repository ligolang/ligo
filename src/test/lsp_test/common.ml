open Lsp.Types

(* Range *)

let pp_range ppf x = Fmt.pf ppf "%S" (Utils.range_to_string x)
let eq_range = Caml.( = )
let range : Range.t Alcotest.testable = Alcotest.testable pp_range eq_range

let diagnostic_severity_to_string = function
  | DiagnosticSeverity.Error -> "error"
  | DiagnosticSeverity.Warning -> "warning"
  | DiagnosticSeverity.Information -> "information"
  | DiagnosticSeverity.Hint -> "hint"


(* DiagnosticSeverity *)

let pp_diagnostic_severity ppf x = Fmt.pf ppf "%S" (diagnostic_severity_to_string x)
let eq_diagnostic_severity = Caml.( = )

let diagnostic_severity : DiagnosticSeverity.t Alcotest.testable =
  Alcotest.testable pp_diagnostic_severity eq_diagnostic_severity


let rec remove_by (eq : 'a -> 'a -> bool) (y : 'a) (xs : 'a list) : 'a list =
  match xs with
  | [] -> []
  | x :: xs -> if eq x y then xs else x :: remove_by eq y xs


let diff_by (eq : 'a -> 'a -> bool) (xs : 'a list) (ys : 'a list) : 'a list =
  List.fold_left xs ~init:ys ~f:(Fun.flip (remove_by eq))


let match_list ~(actual : 'a list) ~(expected : 'a list) ~(eq : 'a -> 'a -> bool)
    : 'a list * 'a list
  =
  let extra = diff_by eq actual expected in
  let missing = diff_by eq expected actual in
  extra, missing


(* FIXME: In case of failure, the printed format is pretty ugly and gets repeated twice. *)
let should_match_list
    (testable_a : 'a Alcotest.testable)
    ~(actual : 'a list)
    ~(expected : 'a list)
    : unit
  =
  match match_list ~actual ~expected ~eq:(Alcotest.equal testable_a) with
  | [], [] -> ()
  | extra, missing ->
    let format_list = Fmt.Dump.list (Alcotest.pp testable_a) in
    Alcotest.failf
      ("Lists do not match."
      ^^ "\n* Expected: %a"
      ^^ "\n* Actual:   %a"
      ^^ "\n* Extra:    %a"
      ^^ "\n* Missing:  %a")
      format_list
      expected
      format_list
      actual
      format_list
      extra
      format_list
      missing
