open Lsp.Types

let pp_with_yojson (f : 'a -> Yojson.Safe.t) : 'a Fmt.t =
 fun formatter link -> Yojson.Safe.pretty_print formatter @@ f link


(* Range *)

let pp_range = pp_with_yojson Range.yojson_of_t
let eq_range = Caml.( = )
let testable_range : Range.t Alcotest.testable = Alcotest.testable pp_range eq_range

(* Position *)
let pp_position = pp_with_yojson Position.yojson_of_t
let eq_position = Caml.( = )

let testable_position : Position.t Alcotest.testable =
  Alcotest.testable pp_position eq_position


(* Locations *)
let pp_locations = pp_with_yojson Locations.yojson_of_t
let eq_locations = Caml.( = )

let testable_locations : Locations.t Alcotest.testable =
  Alcotest.testable pp_locations eq_locations


(* should_match_list *)
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
    ?(msg : string option)
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
      ("%s"
      ^^ "\n* Expected: %a"
      ^^ "\n* Actual:   %a"
      ^^ "\n* Extra:    %a"
      ^^ "\n* Missing:  %a")
      (Option.value ~default:"Lists do not match." msg)
      format_list
      expected
      format_list
      actual
      format_list
      extra
      format_list
      missing
