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


(* Things with prettier output then Alcotest built-ins, mainly copy-pasted alcotest/engine/test.ml *)

(* Unsafe use of the alcotest internals *)
let check_err msg = raise (Alcotest_engine__Core.Check_error msg)

let pp_location
    : ?here:Lexing.position -> ?pos:string * int * int * int -> Format.formatter -> unit
  =
  let open Alcotest in
  let pp =
    Fmt.styled `Bold (fun ppf (f, l, c) ->
        Fmt.pf ppf "File \"%s\", line %d, character %d:@," f l c)
  in
  fun ?here ?pos ppf ->
    match here, pos with
    | Some (here : Source_code_position.here), _ ->
      pp ppf (here.pos_fname, here.pos_lnum, here.pos_cnum - here.pos_bol)
    | _, Some (fname, lnum, cnum, _) -> pp ppf (fname, lnum, cnum)
    | None, None -> ()


(** Like [Alcotest.check], but without printing the assertion (so we avoid printing same msg twice)*)
let check (type a) ?here ?pos (t : a Alcotest.testable) msg (expected : a) (actual : a) =
  let open Alcotest in
  if not (equal t expected actual)
  then
    let open Fmt in
    let s = const string in
    let pp_error =
      match msg with
      | "" -> nop
      | _ -> const Alcotest_engine.Private.Pp.tag `Fail ++ s (" " ^ msg) ++ cut
    and pp_expected ppf () =
      Fmt.pf ppf "   Expected: `%a'" (styled `Green (pp t)) expected;
      Format.pp_print_if_newline ppf ();
      Fmt.cut ppf ();
      ()
    and pp_actual ppf () = Fmt.pf ppf "   Received: `%a'" (styled `Red (pp t)) actual in
    check_err
      Fmt.(
        vbox
          ((fun ppf () -> pp_location ?here ?pos ppf)
          ++ pp_error
          ++ cut
          ++ pp_expected
          ++ cut
          ++ pp_actual)
        ++ cut)


(** Like [Alcotest.fail], but without printing the assertion (so we avoid printing same msg twice)*)
let fail ?here ?pos msg =
  check_err (fun ppf () ->
      Fmt.pf
        ppf
        "%t%a %s"
        (pp_location ?here ?pos)
        Alcotest_engine.Private.Pp.tag
        `Fail
        msg)


(** Like [Alcotest.failf], but without printing the assertion (so we avoid printing same msg twice)*)
let failf ?here ?pos fmt = Fmt.kstr (fun msg -> fail ?here ?pos msg) fmt

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
    (* For some reason, using [format_list] for a leads to problems with identation,
       so we print each [a] separately *)
    let to_string = Format.asprintf "%a" (Alcotest.pp testable_a) in
    let format_list = Fmt.Dump.list Fmt.string in
    failf
      ("%s"
      ^^ "\n* Expected: %a"
      ^^ "\n\n* Actual:   %a"
      ^^ "\n\n* Extra:    %a"
      ^^ "\n\n* Missing:  %a")
      (Option.value ~default:"Lists do not match." msg)
      format_list
      (List.map ~f:to_string expected)
      format_list
      (List.map ~f:to_string actual)
      format_list
      (List.map ~f:to_string extra)
      format_list
      (List.map ~f:to_string missing)
