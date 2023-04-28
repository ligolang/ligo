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
