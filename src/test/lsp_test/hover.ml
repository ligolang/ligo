open Lsp_test_helpers.Handlers
open Lsp_test_helpers.Common
open Lsp_helpers
module Requests = Ligo_lsp.Server.Requests

type hover_test =
  { file : string
  ; hover_positions : Position.t list
        (* Each element of a list is a position where hover message should appear
           when mouse is on that position.
           This was introduced to make test output for multiple cases for one file more compact. *)
  }

let get_hover_test ({ file; hover_positions } : hover_test) : unit =
  let test_hover_for_position position =
    let path = normalize_path file in
    let actual_hover, diagnostics =
      test_run_session
      @@
      let open Handler.Let_syntax in
      let%bind uri = open_file path in
      Requests.on_req_hover position uri
    in
    let test_info =
      Format.asprintf
        "Hover request for: %s, %a.\nDiagnostics for this test: %a"
        file
        Position.pp
        position
        Fmt.Dump.(list (pair Path.pp (list Diagnostic.pp)))
        (Hashtbl.to_alist diagnostics)
    in
    match actual_hover with
    | None -> failwith @@ "Expected a hover message, got none.\n" ^ test_info
    | Some hover ->
      Format.printf "%a" (Helpers_pretty.pp_with_yojson Hover.yojson_of_t) hover
  in
  run_multiple_tests hover_positions ~test_runner:test_hover_for_position


(* TODO after resolving some issues new hovers tests should be added:
   - #1959 add tests for typer error recovery (introduced in !2713)
   - #1676 add tests for hovers on constructors and record fields
   - #1965 add tests for e.g. `compose_endo` from `hovers.mligo`
*)
(* TODO JsLIGO tests *)

let pos = Position.create

let%expect_test "simple.mligo" =
  get_hover_test
    { file = "contracts/lsp/simple.mligo"
    ; hover_positions =
        [ pos ~line:0 ~character:4
        ; pos ~line:0 ~character:5
        ; pos ~line:1 ~character:8
        ; pos ~line:1 ~character:9
        ; pos ~line:1 ~character:4
        ; pos ~line:1 ~character:5
        ]
    };
  [%expect
    {|
    ((stack ((Ident _#610))) "3=before tuple")
    ((stack ((Ident _#610))) "2=before tuple")
    ((stack (Value (Ident _#610))) "2=after tuple")
    ((stack (Value (Ident _#610))) "1=before tuple")
    ((stack (Value (Ident _#610)))
      "0=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#610)))
      "0=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#610))) "1=after tuple")
    ((stack (Value (Ident _#610))) "3=after tuple")
    ((stack ((Ident _#998))) "7=before tuple")
    ((stack ((Ident _#998))) "6=before tuple")
    ((stack (Value (Ident _#998))) "6=after tuple")
    ((stack (Value (Ident _#998))) "5=before tuple")
    ((stack (Value (Ident _#998)))
      "4=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#998)))
      "4=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#998))) "5=after tuple")
    ((stack (Value (Ident _#998))) "7=after tuple")
    [{ "contents": [ { "value": "x : int", "language": "cameligo" } ] };
     { "contents": [ { "value": "x : int", "language": "cameligo" } ] };((stack ((Ident _#1386))) "11=before tuple")
    ((stack ((Ident _#1386))) "10=before tuple")
    ((stack (Value (Ident _#1386))) "10=after tuple")
    ((stack (Value (Ident _#1386))) "9=before tuple")
    ((stack (Value (Ident _#1386)))
      "8=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#1386)))
      "8=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#1386))) "9=after tuple")
    ((stack (Value (Ident _#1386))) "11=after tuple")

     { "contents": [ { "value": "x : int", "language": "cameligo" } ] };((stack ((Ident _#1774))) "15=before tuple")
    ((stack ((Ident _#1774))) "14=before tuple")
    ((stack (Value (Ident _#1774))) "14=after tuple")
    ((stack (Value (Ident _#1774))) "13=before tuple")
    ((stack (Value (Ident _#1774)))
      "12=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#1774)))
      "12=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#1774))) "13=after tuple")
    ((stack (Value (Ident _#1774))) "15=after tuple")

     { "contents": [ { "value": "x : int", "language": "cameligo" } ] };((stack ((Ident _#2162))) "19=before tuple")
    ((stack ((Ident _#2162))) "18=before tuple")
    ((stack (Value (Ident _#2162))) "18=after tuple")
    ((stack (Value (Ident _#2162))) "17=before tuple")
    ((stack (Value (Ident _#2162)))
      "16=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#2162)))
      "16=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#2162))) "17=after tuple")
    ((stack (Value (Ident _#2162))) "19=after tuple")

     { "contents": [ { "value": "y : int", "language": "cameligo" } ] };((stack ((Ident _#2550))) "23=before tuple")
    ((stack ((Ident _#2550))) "22=before tuple")
    ((stack (Value (Ident _#2550))) "22=after tuple")
    ((stack (Value (Ident _#2550))) "21=before tuple")
    ((stack (Value (Ident _#2550)))
      "20=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#2550)))
      "20=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#2550))) "21=after tuple")
    ((stack (Value (Ident _#2550))) "23=after tuple")

     { "contents": [ { "value": "y : int", "language": "cameligo" } ] }] |}]

let%expect_test "registry.jsligo" =
  get_hover_test
    { file = "contracts/lsp/registry.jsligo"
    ; hover_positions =
        [ pos ~line:11 ~character:19
        ; pos ~line:26 ~character:10
        ; pos ~line:28 ~character:31
        ; pos ~line:39 ~character:28
        ; pos ~line:40 ~character:50
        ; pos ~line:39 ~character:40
        ; pos ~line:39 ~character:55
        ]
    };
  [%expect
    {|
    ((stack ((Ident _#3009))) "27=before tuple")
    ((stack ((Ident _#3009))) "26=before tuple")
    ((stack (Value (Ident _#3009))) "26=after tuple")
    ((stack (Value (Ident _#3009))) "25=before tuple")
    ((stack (Value (Ident _#3009)))
      "24=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#3009)))
      "24=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#3009))) "25=after tuple")
    ((stack (Value (Ident _#3009))) "27=after tuple")
    [{
       "contents": [
         {
           "value": "get_exn : <a>(_: list<a>) => (_: int) => a",
           "language": "jsligo"
         }
       ]
     };((stack ((Ident _#3468))) "31=before tuple")
    ((stack ((Ident _#3468))) "30=before tuple")
    ((stack (Value (Ident _#3468))) "30=after tuple")
    ((stack (Value (Ident _#3468))) "29=before tuple")
    ((stack (Value (Ident _#3468)))
      "28=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#3468)))
      "28=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#3468))) "29=after tuple")
    ((stack (Value (Ident _#3468))) "31=after tuple")

     {
       "contents": [
         {
           "value": "map :\n  <src, dst>(_: (_: src) => dst) => (_: list<src>) => list<\n    dst\n  >",
           "language": "jsligo"
         },
         "The call `List.map(f, list([a1; ...; an]))` applies the function `f` to\n    `a1`, ..., `an` (from left to right), and builds the list\n    `list([f(a1); ...; f(an)])` with the results returned by `f`."
       ]
     };((stack ((Ident _#3927))) "35=before tuple")
    ((stack ((Ident _#3927))) "34=before tuple")
    ((stack (Value (Ident _#3927))) "34=after tuple")
    ((stack (Value (Ident _#3927))) "33=before tuple")
    ((stack (Value (Ident _#3927)))
      "32=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#3927)))
      "32=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#3927))) "33=after tuple")
    ((stack (Value (Ident _#3927))) "35=after tuple")

     { "contents": [ { "value": "primes : list<int>", "language": "jsligo" } ] };((stack ((Ident _#4386))) "39=before tuple")
    ((stack ((Ident _#4386))) "38=before tuple")
    ((stack (Value (Ident _#4386))) "38=after tuple")
    ((stack (Value (Ident _#4386))) "37=before tuple")
    ((stack (Value (Ident _#4386)))
      "36=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#4386)))
      "36=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#4386))) "37=after tuple")
    ((stack (Value (Ident _#4386))) "39=after tuple")

     { "contents": [ { "value": "store : storage", "language": "jsligo" } ] };((stack ((Ident _#4845))) "43=before tuple")
    ((stack ((Ident _#4845))) "42=before tuple")
    ((stack (Value (Ident _#4845))) "42=after tuple")
    ((stack (Value (Ident _#4845))) "41=before tuple")
    ((stack (Value (Ident _#4845)))
      "40=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#4845)))
      "40=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#4845))) "41=after tuple")
    ((stack (Value (Ident _#4845))) "43=after tuple")

     { "contents": [ { "value": "store : storage", "language": "jsligo" } ] };((stack ((Ident _#5304))) "47=before tuple")
    ((stack ((Ident _#5304))) "46=before tuple")
    ((stack (Value (Ident _#5304))) "46=after tuple")
    ((stack (Value (Ident _#5304))) "45=before tuple")
    ((stack (Value (Ident _#5304)))
      "44=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#5304)))
      "44=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#5304))) "45=after tuple")
    ((stack (Value (Ident _#5304))) "47=after tuple")

     {
       "contents": [
         { "value": "type storage = list<int>", "language": "jsligo" }
       ]
     };((stack ((Ident _#5763))) "51=before tuple")
    ((stack ((Ident _#5763))) "50=before tuple")
    ((stack (Value (Ident _#5763))) "50=after tuple")
    ((stack (Value (Ident _#5763))) "49=before tuple")
    ((stack (Value (Ident _#5763)))
      "48=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#5763)))
      "48=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#5763))) "49=after tuple")
    ((stack (Value (Ident _#5763))) "51=after tuple")

     {
       "contents": [
         {
           "value": "type return_ = [list<operation>, list<int>]",
           "language": "jsligo"
         }
       ]
     }] |}]

let%expect_test "hovers.mligo" =
  get_hover_test
    { file = "contracts/lsp/hovers.mligo"
    ; hover_positions =
        [ pos ~line:0 ~character:8
        ; pos ~line:4 ~character:61
        ; pos ~line:4 ~character:20
        ; pos ~line:5 ~character:12
        ; pos ~line:5 ~character:37
        ; pos ~line:5 ~character:47
        ; pos ~line:9 ~character:31
        ; pos ~line:9 ~character:70
        ; pos ~line:21 ~character:17
        ; pos ~line:40 ~character:18
        ; pos ~line:56 ~character:12
        ; pos ~line:58 ~character:28
        ; pos ~line:59 ~character:15
        ; pos ~line:65 ~character:41
        ; pos ~line:70 ~character:5
        ; pos ~line:72 ~character:23
        ; pos ~line:75 ~character:4
        ; pos ~line:75 ~character:35
        ; pos ~line:75 ~character:27
        ; pos ~line:77 ~character:28
        ; pos ~line:79 ~character:8
        ; pos ~line:83 ~character:20
        ; pos ~line:17 ~character:61
        ; pos ~line:11 ~character:15
        ; pos ~line:46 ~character:55
        ]
    };
  [%expect
    {|
    ((stack ((Ident _#6163))) "55=before tuple")
    ((stack ((Ident _#6163))) "54=before tuple")
    ((stack (Value (Ident _#6163))) "54=after tuple")
    ((stack (Value (Ident _#6163))) "53=before tuple")
    ((stack (Value (Ident _#6163)))
      "52=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#6163)))
      "52=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#6163))) "53=after tuple")
    ((stack (Value (Ident _#6163))) "55=after tuple")
    [{
       "contents": [
         { "value": "type 'a endo = Endo of ('a -> 'a)", "language": "cameligo" }
       ]
     };((stack ((Ident _#6563))) "59=before tuple")
    ((stack ((Ident _#6563))) "58=before tuple")
    ((stack (Value (Ident _#6563))) "58=after tuple")
    ((stack (Value (Ident _#6563))) "57=before tuple")
    ((stack (Value (Ident _#6563)))
      "56=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#6563)))
      "56=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#6563))) "57=after tuple")
    ((stack (Value (Ident _#6563))) "59=after tuple")

     {
       "contents": [
         { "value": "type 'a endo = Endo of ('a -> 'a)", "language": "cameligo" }
       ]
     };((stack ((Ident _#6963))) "63=before tuple")
    ((stack ((Ident _#6963))) "62=before tuple")
    ((stack (Value (Ident _#6963))) "62=after tuple")
    ((stack (Value (Ident _#6963))) "61=before tuple")
    ((stack (Value (Ident _#6963)))
      "60=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#6963)))
      "60=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#6963))) "61=after tuple")
    ((stack (Value (Ident _#6963))) "63=after tuple")

     {
       "contents": [
         {
           "value": "compose_endo_with_type_annotation :\n  'a.'a endo -> 'a endo -> 'a endo",
           "language": "cameligo"
         }
       ]
     };((stack ((Ident _#7363))) "67=before tuple")
    ((stack ((Ident _#7363))) "66=before tuple")
    ((stack (Value (Ident _#7363))) "66=after tuple")
    ((stack (Value (Ident _#7363))) "65=before tuple")
    ((stack (Value (Ident _#7363)))
      "64=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#7363)))
      "64=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#7363))) "65=after tuple")
    ((stack (Value (Ident _#7363))) "67=after tuple")
    ((stack ((Ident _#7763))) "71=before tuple")
    ((stack ((Ident _#7763))) "70=before tuple")
    ((stack (Value (Ident _#7763))) "70=after tuple")
    ((stack (Value (Ident _#7763))) "69=before tuple")
    ((stack (Value (Ident _#7763)))
      "68=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#7763)))
      "68=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#7763))) "69=after tuple")
    ((stack (Value (Ident _#7763))) "71=after tuple")
     { "contents": [ { "value": "f : a -> a", "language": "cameligo" } ] };
     { "contents": [ { "value": "x : a", "language": "cameligo" } ] };((stack ((Ident _#8163))) "75=before tuple")
    ((stack ((Ident _#8163))) "74=before tuple")
    ((stack (Value (Ident _#8163))) "74=after tuple")
    ((stack (Value (Ident _#8163))) "73=before tuple")
    ((stack (Value (Ident _#8163)))
      "72=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#8163)))
      "72=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#8163))) "73=after tuple")
    ((stack (Value (Ident _#8163))) "75=after tuple")

     { "contents": [ { "value": "x : a", "language": "cameligo" } ] };((stack ((Ident _#8563))) "79=before tuple")
    ((stack ((Ident _#8563))) "78=before tuple")
    ((stack (Value (Ident _#8563))) "78=after tuple")
    ((stack (Value (Ident _#8563))) "77=before tuple")
    ((stack (Value (Ident _#8563)))
      "76=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#8563)))
      "76=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#8563))) "77=after tuple")
    ((stack (Value (Ident _#8563))) "79=after tuple")

     { "contents": [ { "value": "f : int -> int", "language": "cameligo" } ] };((stack ((Ident _#8963))) "83=before tuple")
    ((stack ((Ident _#8963))) "82=before tuple")
    ((stack (Value (Ident _#8963))) "82=after tuple")
    ((stack (Value (Ident _#8963))) "81=before tuple")
    ((stack (Value (Ident _#8963)))
      "80=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#8963)))
      "80=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#8963))) "81=after tuple")
    ((stack (Value (Ident _#8963))) "83=after tuple")

     { "contents": [ { "value": "x : int", "language": "cameligo" } ] };((stack ((Ident _#9363))) "87=before tuple")
    ((stack ((Ident _#9363))) "86=before tuple")
    ((stack (Value (Ident _#9363))) "86=after tuple")
    ((stack (Value (Ident _#9363))) "85=before tuple")
    ((stack (Value (Ident _#9363)))
      "84=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#9363)))
      "84=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#9363))) "85=after tuple")
    ((stack (Value (Ident _#9363))) "87=after tuple")

     { "contents": [ { "value": "f1 : a -> b", "language": "cameligo" } ] };((stack ((Ident _#9763))) "91=before tuple")
    ((stack ((Ident _#9763))) "90=before tuple")
    ((stack (Value (Ident _#9763))) "90=after tuple")
    ((stack (Value (Ident _#9763))) "89=before tuple")
    ((stack (Value (Ident _#9763)))
      "88=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#9763)))
      "88=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#9763))) "89=after tuple")
    ((stack (Value (Ident _#9763))) "91=after tuple")

     { "contents": [ { "value": "f : int -> int", "language": "cameligo" } ] };((stack ((Ident _#10163))) "95=before tuple")
    ((stack ((Ident _#10163))) "94=before tuple")
    ((stack (Value (Ident _#10163))) "94=after tuple")
    ((stack (Value (Ident _#10163))) "93=before tuple")
    ((stack (Value (Ident _#10163)))
      "92=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#10163)))
      "92=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#10163))) "93=after tuple")
    ((stack (Value (Ident _#10163))) "95=after tuple")

     { "contents": [ { "value": "x : t", "language": "cameligo" } ] };((stack ((Ident _#10563))) "99=before tuple")
    ((stack ((Ident _#10563))) "98=before tuple")
    ((stack (Value (Ident _#10563))) "98=after tuple")
    ((stack (Value (Ident _#10563))) "97=before tuple")
    ((stack (Value (Ident _#10563)))
      "96=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#10563)))
      "96=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#10563))) "97=after tuple")
    ((stack (Value (Ident _#10563))) "99=after tuple")

     { "contents": [ { "value": "f : a -> a", "language": "cameligo" } ] };((stack ((Ident _#10963))) "103=before tuple")
    ((stack ((Ident _#10963))) "102=before tuple")
    ((stack (Value (Ident _#10963))) "102=after tuple")
    ((stack (Value (Ident _#10963))) "101=before tuple")
    ((stack (Value (Ident _#10963)))
      "100=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#10963)))
      "100=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#10963))) "101=after tuple")
    ((stack (Value (Ident _#10963))) "103=after tuple")

     {
       "contents": [
         {
           "value": "map : 'src 'dst.('src -> 'dst) -> 'src list -> 'dst list",
           "language": "cameligo"
         },
         "The call `List.map f [a1; ...; an]` applies the function `f` to `a1`,\n    ..., `an` (from left to right), and builds the list\n    `[f a1; ...; f an]` with the results returned by `f`."
       ]
     };((stack ((Ident _#11363))) "107=before tuple")
    ((stack ((Ident _#11363))) "106=before tuple")
    ((stack (Value (Ident _#11363))) "106=after tuple")
    ((stack (Value (Ident _#11363))) "105=before tuple")
    ((stack (Value (Ident _#11363)))
      "104=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#11363)))
      "104=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#11363))) "105=after tuple")
    ((stack (Value (Ident _#11363))) "107=after tuple")

     {
       "contents": [
         { "value": "type 'a list2 = 'a list list", "language": "cameligo" }
       ]
     };((stack ((Ident _#11763))) "111=before tuple")
    ((stack ((Ident _#11763))) "110=before tuple")
    ((stack (Value (Ident _#11763))) "110=after tuple")
    ((stack (Value (Ident _#11763))) "109=before tuple")
    ((stack (Value (Ident _#11763)))
      "108=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#11763)))
      "108=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#11763))) "109=after tuple")
    ((stack (Value (Ident _#11763))) "111=after tuple")

     {
       "contents": [
         {
           "value": "x1 : int list list -> int list list2",
           "language": "cameligo"
         }
       ]
     };((stack ((Ident _#12163))) "115=before tuple")
    ((stack ((Ident _#12163))) "114=before tuple")
    ((stack (Value (Ident _#12163))) "114=after tuple")
    ((stack (Value (Ident _#12163))) "113=before tuple")
    ((stack (Value (Ident _#12163)))
      "112=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#12163)))
      "112=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#12163))) "113=after tuple")
    ((stack (Value (Ident _#12163))) "115=after tuple")

     {
       "contents": [
         {
           "value": "endo_list2 : 'a.'a endo -> 'a list2 endo",
           "language": "cameligo"
         }
       ]
     };((stack ((Ident _#12563))) "119=before tuple")
    ((stack ((Ident _#12563))) "118=before tuple")
    ((stack (Value (Ident _#12563))) "118=after tuple")
    ((stack (Value (Ident _#12563))) "117=before tuple")
    ((stack (Value (Ident _#12563)))
      "116=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#12563)))
      "116=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#12563))) "117=after tuple")
    ((stack (Value (Ident _#12563))) "119=after tuple")

     {
       "contents": [
         {
           "value": "z : key_hash option -> tez -> int -> (operation * address)",
           "language": "cameligo"
         }
       ]
     };((stack ((Ident _#12963))) "123=before tuple")
    ((stack ((Ident _#12963))) "122=before tuple")
    ((stack (Value (Ident _#12963))) "122=after tuple")
    ((stack (Value (Ident _#12963))) "121=before tuple")
    ((stack (Value (Ident _#12963)))
      "120=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#12963)))
      "120=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#12963))) "121=after tuple")
    ((stack (Value (Ident _#12963))) "123=after tuple")
    ((stack ((Ident _#13363))) "127=before tuple")
    ((stack ((Ident _#13363))) "126=before tuple")
    ((stack (Value (Ident _#13363))) "126=after tuple")
    ((stack (Value (Ident _#13363))) "125=before tuple")
    ((stack (Value (Ident _#13363)))
      "124=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#13363)))
      "124=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#13363))) "125=after tuple")
    ((stack (Value (Ident _#13363))) "127=after tuple")
     { "contents": [ { "value": "x : int", "language": "cameligo" } ] };
     {
       "contents": [
         {
           "value": "create_contract :\n  'param\n  'storage.('param, 'storage) entrypoint ->\n  key_hash option ->\n  tez -> 'storage -> (operation * address)",
           "language": "cameligo"
         },
         "The call `Tezos.create_contract e d a s` returns a contract creation\n    operation (origination) for the entrypoint `e` (as a function)\n    with optional delegate `d`, initial amount `a` and initial\n    storage `s`, together with the address of the created\n    contract. Note that the created contract cannot be called\n    immediately afterwards (that is, `Tezos.get_contract_opt` on that\n    address would return `None`), as the origination must be\n    performed successfully first, for example by calling a proxy\n    contract or itself."
       ]
     };((stack ((Ident _#13763))) "131=before tuple")
    ((stack ((Ident _#13763))) "130=before tuple")
    ((stack (Value (Ident _#13763))) "130=after tuple")
    ((stack (Value (Ident _#13763))) "129=before tuple")
    ((stack (Value (Ident _#13763)))
      "128=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#13763)))
      "128=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#13763))) "129=after tuple")
    ((stack (Value (Ident _#13763))) "131=after tuple")

     {
       "contents": [
         {
           "value": "type 'v proxy_address =\n  ('v * nat * address, unit) typed_address",
           "language": "cameligo"
         }
       ]
     };((stack ((Ident _#14163))) "135=before tuple")
    ((stack ((Ident _#14163))) "134=before tuple")
    ((stack (Value (Ident _#14163))) "134=after tuple")
    ((stack (Value (Ident _#14163))) "133=before tuple")
    ((stack (Value (Ident _#14163)))
      "132=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#14163)))
      "132=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#14163))) "133=after tuple")
    ((stack (Value (Ident _#14163))) "135=after tuple")

     {
       "contents": [
         {
           "value": "type 'v p = 'v Test.Proxy_ticket.proxy_address",
           "language": "cameligo"
         }
       ]
     };((stack ((Ident _#14563))) "139=before tuple")
    ((stack ((Ident _#14563))) "138=before tuple")
    ((stack (Value (Ident _#14563))) "138=after tuple")
    ((stack (Value (Ident _#14563))) "137=before tuple")
    ((stack (Value (Ident _#14563)))
      "136=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#14563)))
      "136=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#14563))) "137=after tuple")
    ((stack (Value (Ident _#14563))) "139=after tuple")

     {
       "contents": [
         {
           "value": "type int_endo = IntEndo of (int -> int)",
           "language": "cameligo"
         }
       ]
     };((stack ((Ident _#14963))) "143=before tuple")
    ((stack ((Ident _#14963))) "142=before tuple")
    ((stack (Value (Ident _#14963))) "142=after tuple")
    ((stack (Value (Ident _#14963))) "141=before tuple")
    ((stack (Value (Ident _#14963)))
      "140=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#14963)))
      "140=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#14963))) "141=after tuple")
    ((stack (Value (Ident _#14963))) "143=after tuple")

     {
       "contents": [
         {
           "value": "type ('a, 'b) iso =\n  {\n   from : 'a -> 'b;\n   to : 'b -> 'a\n  }",
           "language": "cameligo"
         }
       ]
     };((stack ((Ident _#15363))) "147=before tuple")
    ((stack ((Ident _#15363))) "146=before tuple")
    ((stack (Value (Ident _#15363))) "146=after tuple")
    ((stack (Value (Ident _#15363))) "145=before tuple")
    ((stack (Value (Ident _#15363)))
      "144=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#15363)))
      "144=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#15363))) "145=after tuple")
    ((stack (Value (Ident _#15363))) "147=after tuple")

     {
       "contents": [
         {
           "value": "type ('a, 'b) iso =\n  {\n   from : 'a -> 'b;\n   to : 'b -> 'a\n  }",
           "language": "cameligo"
         }
       ]
     };((stack ((Ident _#15763))) "151=before tuple")
    ((stack ((Ident _#15763))) "150=before tuple")
    ((stack (Value (Ident _#15763))) "150=after tuple")
    ((stack (Value (Ident _#15763))) "149=before tuple")
    ((stack (Value (Ident _#15763)))
      "148=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#15763)))
      "148=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#15763))) "149=after tuple")
    ((stack (Value (Ident _#15763))) "151=after tuple")

     {
       "contents": [
         {
           "value": "type ('a, 'b) iso =\n  {\n   from : 'a -> 'b;\n   to : 'b -> 'a\n  }",
           "language": "cameligo"
         }
       ]
     }] |}]

let%expect_test "C.mligo" =
  get_hover_test
    { file = "contracts/lsp/hover/imports/C.mligo"
    ; hover_positions =
        [ pos ~line:3 ~character:11
        ; pos ~line:3 ~character:13
        ; pos ~line:5 ~character:11
        ]
    };
  [%expect
    {|
    ((stack ((Ident _#16161))) "155=before tuple")
    ((stack ((Ident _#16161))) "154=before tuple")
    ((stack (Value (Ident _#16161))) "154=after tuple")
    ((stack (Value (Ident _#16161))) "153=before tuple")
    ((stack (Value (Ident _#16161)))
      "152=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#16161)))
      "152=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#16161))) "153=after tuple")
    ((stack (Value (Ident _#16161))) "155=after tuple")
    [{
       "contents": [
         { "value": "#import \"B.mligo\" \"M\"", "language": "cameligo" }
       ]
     };((stack ((Ident _#16559))) "159=before tuple")
    ((stack ((Ident _#16559))) "158=before tuple")
    ((stack (Value (Ident _#16559))) "158=after tuple")
    ((stack (Value (Ident _#16559))) "157=before tuple")
    ((stack (Value (Ident _#16559)))
      "156=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#16559)))
      "156=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#16559))) "157=after tuple")
    ((stack (Value (Ident _#16559))) "159=after tuple")

     {
       "contents": [
         { "value": "#import \"A.mligo\" \"C\"", "language": "cameligo" }
       ]
     };((stack ((Ident _#16957))) "163=before tuple")
    ((stack ((Ident _#16957))) "162=before tuple")
    ((stack (Value (Ident _#16957))) "162=after tuple")
    ((stack (Value (Ident _#16957))) "161=before tuple")
    ((stack (Value (Ident _#16957)))
      "160=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#16957)))
      "160=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#16957))) "161=after tuple")
    ((stack (Value (Ident _#16957))) "163=after tuple")

     {
       "contents": [
         { "value": "#import \"A.mligo\" \"K\"", "language": "cameligo" }
       ]
     }] |}]

let%expect_test "outer.mligo" =
  get_hover_test
    { file = "contracts/lsp/hover/imports/outer.mligo"
    ; hover_positions =
        [ pos ~line:2 ~character:12
        ; pos ~line:2 ~character:20
        ; pos ~line:2 ~character:23
        ]
    };
  [%expect
    {|
    ((stack ((Ident _#17365))) "167=before tuple")
    ((stack ((Ident _#17365))) "166=before tuple")
    ((stack (Value (Ident _#17365))) "166=after tuple")
    ((stack (Value (Ident _#17365))) "165=before tuple")
    ((stack (Value (Ident _#17365)))
      "164=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#17365)))
      "164=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#17365))) "165=after tuple")
    ((stack (Value (Ident _#17365))) "167=after tuple")
    [{
       "contents": [
         {
           "value": "#import \"inner/inner.mligo\" \"Inner\"",
           "language": "cameligo"
         }
       ]
     };((stack ((Ident _#17773))) "171=before tuple")
    ((stack ((Ident _#17773))) "170=before tuple")
    ((stack (Value (Ident _#17773))) "170=after tuple")
    ((stack (Value (Ident _#17773))) "169=before tuple")
    ((stack (Value (Ident _#17773)))
      "168=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#17773)))
      "168=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#17773))) "169=after tuple")
    ((stack (Value (Ident _#17773))) "171=after tuple")

     {
       "contents": [
         { "value": "#import \"C.mligo\" \"Outer\"", "language": "cameligo" }
       ]
     };((stack ((Ident _#18181))) "175=before tuple")
    ((stack ((Ident _#18181))) "174=before tuple")
    ((stack (Value (Ident _#18181))) "174=after tuple")
    ((stack (Value (Ident _#18181))) "173=before tuple")
    ((stack (Value (Ident _#18181)))
      "172=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#18181)))
      "172=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#18181))) "173=after tuple")
    ((stack (Value (Ident _#18181))) "175=after tuple")

     {
       "contents": [
         { "value": "#import \"A.mligo\" \"K\"", "language": "cameligo" }
       ]
     }] |}]

let%expect_test "inner.mligo" =
  get_hover_test
    { file = "contracts/lsp/hover/imports/inner/inner.mligo"
    ; hover_positions = [ pos ~line:2 ~character:12; pos ~line:2 ~character:17 ]
    };
  [%expect
    {|
    ((stack ((Ident _#18584))) "179=before tuple")
    ((stack ((Ident _#18584))) "178=before tuple")
    ((stack (Value (Ident _#18584))) "178=after tuple")
    ((stack (Value (Ident _#18584))) "177=before tuple")
    ((stack (Value (Ident _#18584)))
      "176=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#18584)))
      "176=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#18584))) "177=after tuple")
    ((stack (Value (Ident _#18584))) "179=after tuple")
    [{
       "contents": [
         { "value": "#import \"../C.mligo\" \"Outer\"", "language": "cameligo" }
       ]
     };((stack ((Ident _#18987))) "183=before tuple")
    ((stack ((Ident _#18987))) "182=before tuple")
    ((stack (Value (Ident _#18987))) "182=after tuple")
    ((stack (Value (Ident _#18987))) "181=before tuple")
    ((stack (Value (Ident _#18987)))
      "180=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#18987)))
      "180=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#18987))) "181=after tuple")
    ((stack (Value (Ident _#18987))) "183=after tuple")

     {
       "contents": [
         { "value": "#import \"../A.mligo\" \"K\"", "language": "cameligo" }
       ]
     }] |}]

let%expect_test "hover_module.mligo" =
  get_hover_test
    { file = "contracts/lsp/hover/hover_module.mligo"
    ; hover_positions =
        [ pos ~line:0 ~character:7
        ; pos ~line:10 ~character:7
        ; pos ~line:17 ~character:7
        ; pos ~line:33 ~character:24
        ; pos ~line:43 ~character:13
        ; pos ~line:48 ~character:12
        ; pos ~line:54 ~character:10
        ; pos ~line:70 ~character:20
        ; pos ~line:77 ~character:14
        ; pos ~line:25 ~character:9
        ; pos ~line:41 ~character:21
        ; pos ~line:28 ~character:11
        ; pos ~line:41 ~character:25
        ; pos ~line:35 ~character:8
        ; pos ~line:39 ~character:35
        ; pos ~line:62 ~character:9
        ; pos ~line:64 ~character:9
        ; pos ~line:5 ~character:12
        ; pos ~line:10 ~character:11
        ; pos ~line:48 ~character:26
        ; pos ~line:71 ~character:10
        ; pos ~line:66 ~character:12
        ; pos ~line:72 ~character:10
        ]
    };
  [%expect
    {|
    [{
       "contents": [
         {
           "value": "module A : sig\n  val foo : int\n\n  val bar : int\n  end",
           "language": "cameligo"
         }
       ]
     };
     {
       "contents": [
         {
           "value": "module B : sig\n  type t =  nat\n\n  type int =  string\n  end",
           "language": "cameligo"
         }
       ]
     };
     {
       "contents": [
         {
           "value": "module C : sig\n  val another : int\n\n  val foo : tez\n  end",
           "language": "cameligo"
         }
       ]
     };
     {
       "contents": [
         {
           "value": "module Bytes : sig\n  val length : bytes -> nat\n\n  val size : bytes -> nat\n\n  val concat : bytes -> bytes -> bytes\n\n  val concats : bytes list -> bytes\n\n  val sub : nat -> nat -> bytes -> bytes\n\n  val slice : nat -> nat -> bytes -> bytes\n\n  val pack : 'a.'a -> bytes\n\n  val unpack : 'a.bytes -> 'a option\n  end",
           "language": "cameligo"
         },
         "Sequences of bytes\n\n    Bytes are used for serializing data, in order to check signatures\n    and compute hashes on them. They can also be used to read untyped\n    data from outside of the contract."
       ]
     };
     {
       "contents": [
         {
           "value": "module Mangled : sig\n  val where : ^a\n  end",
           "language": "cameligo"
         }
       ]
     };
     {
       "contents": [
         {
           "value": "module Mangled_with_sig : sig\n  type t\n\n  type int\n  end",
           "language": "cameligo"
         }
       ]
     };
     {
       "contents": [
         {
           "value": "module Mangled_with_inlined_sig : sig\n  val foo : int\n  end",
           "language": "cameligo"
         }
       ]
     };
     {
       "contents": [
         {
           "value": "module type With_included = sig\n  type t\n\n  type int =  string\n\n  val b : bool\n\n  val z : string\n  end",
           "language": "cameligo"
         }
       ]
     };
     {
       "contents": [
         {
           "value": "module With_included : sig\n  type t =  int\n\n  type int =  string\n\n  val b : bool\n  end",
           "language": "cameligo"
         }
       ]
     };
     {
       "contents": [
         {
           "value": "module Outer : sig\n  val outer_foo : int -> int -> int\n  end",
           "language": "cameligo"
         }
       ]
     };
     {
       "contents": [
         {
           "value": "module Outer : sig\n  val outer_foo : int -> int -> int\n  end",
           "language": "cameligo"
         }
       ]
     };
     {
       "contents": [
         {
           "value": "module Inner : sig\n  val inner_foo : int -> int -> int\n  end",
           "language": "cameligo"
         }
       ]
     };
     {
       "contents": [
         {
           "value": "module Inner : sig\n  val inner_foo : int -> int -> int\n  end",
           "language": "cameligo"
         }
       ]
     };
     {
       "contents": [
         {
           "value": "module Bytes : sig\n  val overwritten : string\n  end",
           "language": "cameligo"
         }
       ]
     };
     {
       "contents": [
         {
           "value": "module Bytes : sig\n  val overwritten : string\n  end",
           "language": "cameligo"
         }
       ]
     };
     {
       "contents": [
         {
           "value": "module M : sig\n  val v : int\n  end",
           "language": "cameligo"
         }
       ]
     };
     {
       "contents": [
         {
           "value": "module M : sig\n  val v : int\n  end",
           "language": "cameligo"
         }
       ]
     };
     {
       "contents": [
         {
           "value": "module type T = sig\n  type t\n\n  type int =  string\n  end",
           "language": "cameligo"
         }
       ]
     };
     {
       "contents": [
         {
           "value": "module type T = sig\n  type t\n\n  type int =  string\n  end",
           "language": "cameligo"
         }
       ]
     };
     {
       "contents": [
         {
           "value": "module type T = sig\n  type t\n\n  type int =  string\n  end",
           "language": "cameligo"
         }
       ]
     };
     {
       "contents": [
         {
           "value": "module type T = sig\n  type t\n\n  type int =  string\n  end",
           "language": "cameligo"
         }
       ]
     };
     {
       "contents": [
         {
           "value": "module type I = sig\n  val b : bool\n  end",
           "language": "cameligo"
         }
       ]
     };
     {
       "contents": [
         {
           "value": "module type I = sig\n  val b : bool\n  end",
           "language": "cameligo"
         }
       ]
     }] |}]

let%expect_test "hover_module.jsligo" =
  get_hover_test
    { file = "contracts/lsp/hover/hover_module.jsligo"
    ; hover_positions =
        [ pos ~line:0 ~character:10
        ; pos ~line:10 ~character:10
        ; pos ~line:17 ~character:10
        ; pos ~line:33 ~character:27
        ; pos ~line:43 ~character:11
        ; pos ~line:48 ~character:13
        ; pos ~line:54 ~character:26
        ; pos ~line:72 ~character:17
        ; pos ~line:76 ~character:22
        ; pos ~line:25 ~character:11
        ; pos ~line:41 ~character:21
        ; pos ~line:28 ~character:20
        ; pos ~line:41 ~character:26
        ; pos ~line:35 ~character:13
        ; pos ~line:39 ~character:35
        ; pos ~line:62 ~character:12
        ; pos ~line:65 ~character:9
        ; pos ~line:5 ~character:10
        ; pos ~line:10 ~character:23
        ; pos ~line:48 ~character:38
        ; pos ~line:68 ~character:10
        ; pos ~line:72 ~character:35
        ; pos ~line:76 ~character:38
        ]
    };
  [%expect
    {|
    [{
       "contents": [
         {
           "value": "namespace A implements {\n  const foo: int;\n  const bar: int\n}",
           "language": "jsligo"
         }
       ]
     };
     {
       "contents": [
         {
           "value": "namespace B implements {\n  type t = nat;\n  type int = string;\n  const b: t\n}",
           "language": "jsligo"
         }
       ]
     };
     {
       "contents": [
         {
           "value": "namespace C implements {\n  const foo: tez;\n  const another: int\n}",
           "language": "jsligo"
         }
       ]
     };
     {
       "contents": [
         {
           "value": "namespace Bytes implements {\n  const length: (_: bytes) => nat;\n  const size: (_: bytes) => nat;\n  const concat: (_: bytes) => (_: bytes) => bytes;\n  const concats: (_: list<bytes>) => bytes;\n  const sub: (_: nat) => (_: nat) => (_: bytes) => bytes;\n  const slice: (_: nat) => (_: nat) => (_: bytes) => bytes;\n  const pack: <a>(_: a) => bytes;\n  const unpack: <a>(_: bytes) => option<a>\n}",
           "language": "jsligo"
         },
         "Sequences of bytes\n\n    Bytes are used for serializing data, in order to check signatures\n    and compute hashes on them. They can also be used to read untyped\n    data from outside of the contract."
       ]
     };
     {
       "contents": [
         {
           "value": "namespace Mangled implements {\n  const where: ^a;\n  const v: int\n}",
           "language": "jsligo"
         }
       ]
     };
     {
       "contents": [
         {
           "value": "namespace Mangled_with_sig implements {\n  const where: ^a;\n  type t = string;\n  type int = string\n}",
           "language": "jsligo"
         }
       ]
     };
     {
       "contents": [
         {
           "value": "namespace Mangled_with_inlined_sig implements {\n  const where: ^a;\n  const foo: int\n}",
           "language": "jsligo"
         }
       ]
     };
     {
       "contents": [
         {
           "value": "interface With_included {\n  type t;\n  type int = string;\n  const b: bool;\n  const z: string\n}",
           "language": "jsligo"
         }
       ]
     };
     {
       "contents": [
         {
           "value": "namespace With_included implements {\n  type t = int;\n  type int = string;\n  const b: bool\n}",
           "language": "jsligo"
         }
       ]
     };
     {
       "contents": [
         {
           "value": "namespace Outer implements {\n  const outer_foo: (a: int, b: int) => int\n}",
           "language": "jsligo"
         }
       ]
     };
     {
       "contents": [
         {
           "value": "namespace Outer implements {\n  const outer_foo: (a: int, b: int) => int\n}",
           "language": "jsligo"
         }
       ]
     };
     {
       "contents": [
         {
           "value": "namespace Inner implements {\n  const inner_foo: (a: int, b: int) => int\n}",
           "language": "jsligo"
         }
       ]
     };
     {
       "contents": [
         {
           "value": "namespace Inner implements {\n  const inner_foo: (a: int, b: int) => int\n}",
           "language": "jsligo"
         }
       ]
     };
     {
       "contents": [
         {
           "value": "namespace Bytes implements {\n  const overwritten: string\n}",
           "language": "jsligo"
         }
       ]
     };
     {
       "contents": [
         {
           "value": "namespace Bytes implements {\n  const overwritten: string\n}",
           "language": "jsligo"
         }
       ]
     };
     {
       "contents": [
         {
           "value": "namespace M implements {\n  const v: int\n}",
           "language": "jsligo"
         }
       ]
     };
     {
       "contents": [
         {
           "value": "namespace M implements {\n  const v: int\n}",
           "language": "jsligo"
         }
       ]
     };
     {
       "contents": [
         {
           "value": "interface T {\n  type t;\n  type int = string\n}",
           "language": "jsligo"
         }
       ]
     };
     {
       "contents": [
         {
           "value": "interface T {\n  type t;\n  type int = string\n}",
           "language": "jsligo"
         }
       ]
     };
     {
       "contents": [
         {
           "value": "interface T {\n  type t;\n  type int = string\n}",
           "language": "jsligo"
         }
       ]
     };
     {
       "contents": [
         { "value": "interface I {\n  const b: bool\n}", "language": "jsligo" }
       ]
     };
     {
       "contents": [
         { "value": "interface I {\n  const b: bool\n}", "language": "jsligo" }
       ]
     };
     {
       "contents": [
         { "value": "interface I {\n  const b: bool\n}", "language": "jsligo" }
       ]
     }] |}]

let%expect_test "doc_comments.mligo" =
  get_hover_test
    { file = "contracts/lsp/hover/doc_comments.mligo"
    ; hover_positions =
        [ pos ~line:1 ~character:12
        ; pos ~line:15 ~character:11
        ; pos ~line:5 ~character:6
        ; pos ~line:8 ~character:7
        ; pos ~line:11 ~character:6
        ; pos ~line:15 ~character:7
        ; pos ~line:19 ~character:6
        ; pos ~line:22 ~character:7
        ; pos ~line:25 ~character:6
        ; pos ~line:29 ~character:8
        ; pos ~line:38 ~character:4
        ; pos ~line:40 ~character:8
        ; pos ~line:43 ~character:7
        ; pos ~line:49 ~character:6
        ; pos ~line:52 ~character:9
        ; pos ~line:54 ~character:8
        ; pos ~line:59 ~character:4
        ]
    };
  [%expect
    {|
    ((stack ((Ident _#37800))) "187=before tuple")
    ((stack ((Ident _#37800))) "186=before tuple")
    ((stack (Value (Ident _#37800))) "186=after tuple")
    ((stack (Value (Ident _#37800))) "185=before tuple")
    ((stack (Value (Ident _#37800)))
      "184=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#37800)))
      "184=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#37800))) "185=after tuple")
    ((stack (Value (Ident _#37800))) "187=after tuple")
    [{
       "contents": [
         {
           "value": "module type X = sig\n  [@view]\n  val y : int -> int\n\n  type t\n\n  val p : t option\n  end",
           "language": "cameligo"
         },
         "MODULE SIG"
       ]
     };((stack ((Ident _#38228))) "191=before tuple")
    ((stack ((Ident _#38228))) "190=before tuple")
    ((stack (Value (Ident _#38228))) "190=after tuple")
    ((stack (Value (Ident _#38228))) "189=before tuple")
    ((stack (Value (Ident _#38228)))
      "188=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#38228)))
      "188=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#38228))) "189=after tuple")
    ((stack (Value (Ident _#38228))) "191=after tuple")

     {
       "contents": [
         {
           "value": "module type X = sig\n  [@view]\n  val y : int -> int\n\n  type t\n\n  val p : t option\n  end",
           "language": "cameligo"
         },
         "MODULE SIG"
       ]
     };((stack ((Ident _#38656))) "195=before tuple")
    ((stack ((Ident _#38656))) "194=before tuple")
    ((stack (Value (Ident _#38656))) "194=after tuple")
    ((stack (Value (Ident _#38656))) "193=before tuple")
    ((stack (Value (Ident _#38656)))
      "192=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#38656)))
      "192=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#38656))) "193=after tuple")
    ((stack (Value (Ident _#38656))) "195=after tuple")

     {
       "contents": [
         { "value": "y : int -> int", "language": "cameligo" }, "SIG ITEM"
       ]
     };((stack ((Ident _#39084))) "199=before tuple")
    ((stack ((Ident _#39084))) "198=before tuple")
    ((stack (Value (Ident _#39084))) "198=after tuple")
    ((stack (Value (Ident _#39084))) "197=before tuple")
    ((stack (Value (Ident _#39084)))
      "196=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#39084)))
      "196=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#39084))) "197=after tuple")
    ((stack (Value (Ident _#39084))) "199=after tuple")

     {
       "contents": [ { "value": "type t", "language": "cameligo" }, "SIG TYPE" ]
     };((stack ((Ident _#39512))) "203=before tuple")
    ((stack ((Ident _#39512))) "202=before tuple")
    ((stack (Value (Ident _#39512))) "202=after tuple")
    ((stack (Value (Ident _#39512))) "201=before tuple")
    ((stack (Value (Ident _#39512)))
      "200=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#39512)))
      "200=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#39512))) "201=after tuple")
    ((stack (Value (Ident _#39512))) "203=after tuple")

     {
       "contents": [
         { "value": "p : t option", "language": "cameligo" }, "SIG ITEM"
       ]
     };((stack ((Ident _#39940))) "207=before tuple")
    ((stack ((Ident _#39940))) "206=before tuple")
    ((stack (Value (Ident _#39940))) "206=after tuple")
    ((stack (Value (Ident _#39940))) "205=before tuple")
    ((stack (Value (Ident _#39940)))
      "204=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#39940)))
      "204=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#39940))) "205=after tuple")
    ((stack (Value (Ident _#39940))) "207=after tuple")

     {
       "contents": [
         {
           "value": "module M : sig\n  type t =  {foo : nat}\n\n  val p : t option\n\n  [@view]\n  val y : int -> int\n  end",
           "language": "cameligo"
         },
         "MODULE"
       ]
     };((stack ((Ident _#40368))) "211=before tuple")
    ((stack ((Ident _#40368))) "210=before tuple")
    ((stack (Value (Ident _#40368))) "210=after tuple")
    ((stack (Value (Ident _#40368))) "209=before tuple")
    ((stack (Value (Ident _#40368)))
      "208=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#40368)))
      "208=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#40368))) "209=after tuple")
    ((stack (Value (Ident _#40368))) "211=after tuple")

     {
       "contents": [
         { "value": "y : int -> int", "language": "cameligo" }, "TERM IN MODULE"
       ]
     };((stack ((Ident _#40796))) "215=before tuple")
    ((stack ((Ident _#40796))) "214=before tuple")
    ((stack (Value (Ident _#40796))) "214=after tuple")
    ((stack (Value (Ident _#40796))) "213=before tuple")
    ((stack (Value (Ident _#40796)))
      "212=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#40796)))
      "212=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#40796))) "213=after tuple")
    ((stack (Value (Ident _#40796))) "215=after tuple")

     {
       "contents": [
         { "value": "type t = {foo : nat}", "language": "cameligo" },
         "TYPE IN MODULE"
       ]
     };((stack ((Ident _#41224))) "219=before tuple")
    ((stack ((Ident _#41224))) "218=before tuple")
    ((stack (Value (Ident _#41224))) "218=after tuple")
    ((stack (Value (Ident _#41224))) "217=before tuple")
    ((stack (Value (Ident _#41224)))
      "216=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#41224)))
      "216=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#41224))) "217=after tuple")
    ((stack (Value (Ident _#41224))) "219=after tuple")

     {
       "contents": [
         { "value": "p : M.t option", "language": "cameligo" }, "TERM IN MODULE"
       ]
     };((stack ((Ident _#41652))) "223=before tuple")
    ((stack ((Ident _#41652))) "222=before tuple")
    ((stack (Value (Ident _#41652))) "222=after tuple")
    ((stack (Value (Ident _#41652))) "221=before tuple")
    ((stack (Value (Ident _#41652)))
      "220=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#41652)))
      "220=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#41652))) "221=after tuple")
    ((stack (Value (Ident _#41652))) "223=after tuple")

     {
       "contents": [
         { "value": "type 'a t = 'a list", "language": "cameligo" },
         "JUST A TYPE"
       ]
     };((stack ((Ident _#42080))) "227=before tuple")
    ((stack ((Ident _#42080))) "226=before tuple")
    ((stack (Value (Ident _#42080))) "226=after tuple")
    ((stack (Value (Ident _#42080))) "225=before tuple")
    ((stack (Value (Ident _#42080)))
      "224=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#42080)))
      "224=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#42080))) "225=after tuple")
    ((stack (Value (Ident _#42080))) "227=after tuple")

     {
       "contents": [
         { "value": "x : int t", "language": "cameligo" },
         "JUST A TERM\n\n  with some doc\n  in **several** lines\n\n  one ~~more~~ `line`"
       ]
     };((stack ((Ident _#42508))) "231=before tuple")
    ((stack ((Ident _#42508))) "230=before tuple")
    ((stack (Value (Ident _#42508))) "230=after tuple")
    ((stack (Value (Ident _#42508))) "229=before tuple")
    ((stack (Value (Ident _#42508)))
      "228=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#42508)))
      "228=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#42508))) "229=after tuple")
    ((stack (Value (Ident _#42508))) "231=after tuple")

     {
       "contents": [
         { "value": "x : int t", "language": "cameligo" },
         "JUST A TERM\n\n  with some doc\n  in **several** lines\n\n  one ~~more~~ `line`"
       ]
     };((stack ((Ident _#42936))) "235=before tuple")
    ((stack ((Ident _#42936))) "234=before tuple")
    ((stack (Value (Ident _#42936))) "234=after tuple")
    ((stack (Value (Ident _#42936))) "233=before tuple")
    ((stack (Value (Ident _#42936)))
      "232=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#42936)))
      "232=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#42936))) "233=after tuple")
    ((stack (Value (Ident _#42936))) "235=after tuple")

     {
       "contents": [
         {
           "value": "module M1 : sig\n  [@entry]\n  val y : int -> int -> (operation list * int)\n  end",
           "language": "cameligo"
         },
         "MODULE WITH ENTRY POINT"
       ]
     };((stack ((Ident _#43364))) "239=before tuple")
    ((stack ((Ident _#43364))) "238=before tuple")
    ((stack (Value (Ident _#43364))) "238=after tuple")
    ((stack (Value (Ident _#43364))) "237=before tuple")
    ((stack (Value (Ident _#43364)))
      "236=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#43364)))
      "236=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#43364))) "237=after tuple")
    ((stack (Value (Ident _#43364))) "239=after tuple")

     {
       "contents": [
         {
           "value": "y : int -> int -> (operation list * int)",
           "language": "cameligo"
         },
         "BEFORE DECORATOR",
         "AFTER DECORATOR",
         "ENTRY POINT TERM"
       ]
     };((stack ((Ident _#43792))) "243=before tuple")
    ((stack ((Ident _#43792))) "242=before tuple")
    ((stack (Value (Ident _#43792))) "242=after tuple")
    ((stack (Value (Ident _#43792))) "241=before tuple")
    ((stack (Value (Ident _#43792)))
      "240=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#43792)))
      "240=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#43792))) "241=after tuple")
    ((stack (Value (Ident _#43792))) "243=after tuple")

     {
       "contents": [
         {
           "value": "module C : sig\n  val f : int -> int\n  end",
           "language": "cameligo"
         },
         "NESTED MODULE"
       ]
     };((stack ((Ident _#44220))) "247=before tuple")
    ((stack ((Ident _#44220))) "246=before tuple")
    ((stack (Value (Ident _#44220))) "246=after tuple")
    ((stack (Value (Ident _#44220))) "245=before tuple")
    ((stack (Value (Ident _#44220)))
      "244=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#44220)))
      "244=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#44220))) "245=after tuple")
    ((stack (Value (Ident _#44220))) "247=after tuple")

     {
       "contents": [
         { "value": "f : int -> int", "language": "cameligo" },
         "NESTED MODULE TERM"
       ]
     };((stack ((Ident _#44648))) "251=before tuple")
    ((stack ((Ident _#44648))) "250=before tuple")
    ((stack (Value (Ident _#44648))) "250=after tuple")
    ((stack (Value (Ident _#44648))) "249=before tuple")
    ((stack (Value (Ident _#44648)))
      "248=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#44648)))
      "248=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#44648))) "249=after tuple")
    ((stack (Value (Ident _#44648))) "251=after tuple")

     {
       "contents": [
         { "value": "t : int t", "language": "cameligo" },
         "Has type with comment inside"
       ]
     }] |}]

let%expect_test "doc_comments.jsligo" =
  get_hover_test
    { file = "contracts/lsp/hover/doc_comments.jsligo"
    ; hover_positions =
        [ pos ~line:1 ~character:10
        ; pos ~line:14 ~character:23
        ; pos ~line:5 ~character:8
        ; pos ~line:8 ~character:7
        ; pos ~line:10 ~character:8
        ; pos ~line:14 ~character:10
        ; pos ~line:18 ~character:15
        ; pos ~line:20 ~character:14
        ; pos ~line:22 ~character:15
        ; pos ~line:26 ~character:5
        ; pos ~line:35 ~character:6
        ; pos ~line:37 ~character:10
        ; pos ~line:40 ~character:11
        ; pos ~line:46 ~character:15
        ; pos ~line:49 ~character:19
        ; pos ~line:51 ~character:17
        ; pos ~line:56 ~character:6
        ]
    };
  [%expect
    {|
    ((stack ((Ident _#45077))) "255=before tuple")
    ((stack ((Ident _#45077))) "254=before tuple")
    ((stack (Value (Ident _#45077))) "254=after tuple")
    ((stack (Value (Ident _#45077))) "253=before tuple")
    ((stack (Value (Ident _#45077)))
      "252=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#45077)))
      "252=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#45077))) "253=after tuple")
    ((stack (Value (Ident _#45077))) "255=after tuple")
    [{
       "contents": [
         {
           "value": "interface X {\n  @view\n  const y: (_: int) => int;\n  type t;\n  const p: option<t>\n}",
           "language": "jsligo"
         },
         "INTERFACE"
       ]
     };((stack ((Ident _#45506))) "259=before tuple")
    ((stack ((Ident _#45506))) "258=before tuple")
    ((stack (Value (Ident _#45506))) "258=after tuple")
    ((stack (Value (Ident _#45506))) "257=before tuple")
    ((stack (Value (Ident _#45506)))
      "256=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#45506)))
      "256=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#45506))) "257=after tuple")
    ((stack (Value (Ident _#45506))) "259=after tuple")

     {
       "contents": [
         {
           "value": "interface X {\n  @view\n  const y: (_: int) => int;\n  type t;\n  const p: option<t>\n}",
           "language": "jsligo"
         },
         "INTERFACE"
       ]
     };((stack ((Ident _#45935))) "263=before tuple")
    ((stack ((Ident _#45935))) "262=before tuple")
    ((stack (Value (Ident _#45935))) "262=after tuple")
    ((stack (Value (Ident _#45935))) "261=before tuple")
    ((stack (Value (Ident _#45935)))
      "260=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#45935)))
      "260=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#45935))) "261=after tuple")
    ((stack (Value (Ident _#45935))) "263=after tuple")

     {
       "contents": [
         { "value": "y : (_: int) => int", "language": "jsligo" },
         "INTERFACE ITEM"
       ]
     };((stack ((Ident _#46364))) "267=before tuple")
    ((stack ((Ident _#46364))) "266=before tuple")
    ((stack (Value (Ident _#46364))) "266=after tuple")
    ((stack (Value (Ident _#46364))) "265=before tuple")
    ((stack (Value (Ident _#46364)))
      "264=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#46364)))
      "264=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#46364))) "265=after tuple")
    ((stack (Value (Ident _#46364))) "267=after tuple")

     {
       "contents": [
         { "value": "type t", "language": "jsligo" }, "INTERFACE TYPE"
       ]
     };((stack ((Ident _#46793))) "271=before tuple")
    ((stack ((Ident _#46793))) "270=before tuple")
    ((stack (Value (Ident _#46793))) "270=after tuple")
    ((stack (Value (Ident _#46793))) "269=before tuple")
    ((stack (Value (Ident _#46793)))
      "268=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#46793)))
      "268=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#46793))) "269=after tuple")
    ((stack (Value (Ident _#46793))) "271=after tuple")

     {
       "contents": [
         { "value": "p : option<t>", "language": "jsligo" }, "INTERFACE ITEM"
       ]
     };((stack ((Ident _#47222))) "275=before tuple")
    ((stack ((Ident _#47222))) "274=before tuple")
    ((stack (Value (Ident _#47222))) "274=after tuple")
    ((stack (Value (Ident _#47222))) "273=before tuple")
    ((stack (Value (Ident _#47222)))
      "272=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#47222)))
      "272=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#47222))) "273=after tuple")
    ((stack (Value (Ident _#47222))) "275=after tuple")

     {
       "contents": [
         {
           "value": "namespace M implements {\n  @view\n  const y: (x: int) => int;\n  type t = { foo: nat };\n  const p: option<t>\n}",
           "language": "jsligo"
         },
         "NAMESPACE"
       ]
     };((stack ((Ident _#47651))) "279=before tuple")
    ((stack ((Ident _#47651))) "278=before tuple")
    ((stack (Value (Ident _#47651))) "278=after tuple")
    ((stack (Value (Ident _#47651))) "277=before tuple")
    ((stack (Value (Ident _#47651)))
      "276=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#47651)))
      "276=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#47651))) "277=after tuple")
    ((stack (Value (Ident _#47651))) "279=after tuple")

     {
       "contents": [
         { "value": "y : (x: int) => int", "language": "jsligo" },
         "TERM IN NAMESPACE"
       ]
     };((stack ((Ident _#48080))) "283=before tuple")
    ((stack ((Ident _#48080))) "282=before tuple")
    ((stack (Value (Ident _#48080))) "282=after tuple")
    ((stack (Value (Ident _#48080))) "281=before tuple")
    ((stack (Value (Ident _#48080)))
      "280=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#48080)))
      "280=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#48080))) "281=after tuple")
    ((stack (Value (Ident _#48080))) "283=after tuple")

     {
       "contents": [
         { "value": "type t = { foo: nat }", "language": "jsligo" },
         "TYPE IN NAMESPACE"
       ]
     };((stack ((Ident _#48509))) "287=before tuple")
    ((stack ((Ident _#48509))) "286=before tuple")
    ((stack (Value (Ident _#48509))) "286=after tuple")
    ((stack (Value (Ident _#48509))) "285=before tuple")
    ((stack (Value (Ident _#48509)))
      "284=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#48509)))
      "284=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#48509))) "285=after tuple")
    ((stack (Value (Ident _#48509))) "287=after tuple")

     {
       "contents": [
         { "value": "p : option<M.t>", "language": "jsligo" },
         "TERM IN NAMESPACE"
       ]
     };((stack ((Ident _#48938))) "291=before tuple")
    ((stack ((Ident _#48938))) "290=before tuple")
    ((stack (Value (Ident _#48938))) "290=after tuple")
    ((stack (Value (Ident _#48938))) "289=before tuple")
    ((stack (Value (Ident _#48938)))
      "288=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#48938)))
      "288=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#48938))) "289=after tuple")
    ((stack (Value (Ident _#48938))) "291=after tuple")

     {
       "contents": [
         { "value": "type t<a> = list<a>", "language": "jsligo" }, "JUST A TYPE"
       ]
     };((stack ((Ident _#49367))) "295=before tuple")
    ((stack ((Ident _#49367))) "294=before tuple")
    ((stack (Value (Ident _#49367))) "294=after tuple")
    ((stack (Value (Ident _#49367))) "293=before tuple")
    ((stack (Value (Ident _#49367)))
      "292=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#49367)))
      "292=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#49367))) "293=after tuple")
    ((stack (Value (Ident _#49367))) "295=after tuple")

     {
       "contents": [
         { "value": "x : t<int>", "language": "jsligo" },
         "JUST A TERM\nwith some doc\nin **several** lines\n\none ~~more~~ `line`"
       ]
     };((stack ((Ident _#49796))) "299=before tuple")
    ((stack ((Ident _#49796))) "298=before tuple")
    ((stack (Value (Ident _#49796))) "298=after tuple")
    ((stack (Value (Ident _#49796))) "297=before tuple")
    ((stack (Value (Ident _#49796)))
      "296=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#49796)))
      "296=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#49796))) "297=after tuple")
    ((stack (Value (Ident _#49796))) "299=after tuple")

     {
       "contents": [
         { "value": "x : t<int>", "language": "jsligo" },
         "JUST A TERM\nwith some doc\nin **several** lines\n\none ~~more~~ `line`"
       ]
     };((stack ((Ident _#50225))) "303=before tuple")
    ((stack ((Ident _#50225))) "302=before tuple")
    ((stack (Value (Ident _#50225))) "302=after tuple")
    ((stack (Value (Ident _#50225))) "301=before tuple")
    ((stack (Value (Ident _#50225)))
      "300=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#50225)))
      "300=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#50225))) "301=after tuple")
    ((stack (Value (Ident _#50225))) "303=after tuple")

     {
       "contents": [
         {
           "value": "namespace M1 implements {\n  @entry\n  const y: (x: int, _: int) => [list<operation>, int]\n}",
           "language": "jsligo"
         },
         "NAMESPACE WITH ENTRY POINT"
       ]
     };((stack ((Ident _#50654))) "307=before tuple")
    ((stack ((Ident _#50654))) "306=before tuple")
    ((stack (Value (Ident _#50654))) "306=after tuple")
    ((stack (Value (Ident _#50654))) "305=before tuple")
    ((stack (Value (Ident _#50654)))
      "304=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#50654)))
      "304=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#50654))) "305=after tuple")
    ((stack (Value (Ident _#50654))) "307=after tuple")

     {
       "contents": [
         {
           "value": "y : (x: int, _: int) => [list<operation>, int]",
           "language": "jsligo"
         },
         "BEFORE DECORATOR",
         "AFTER DECORATOR",
         "ENTRY POINT TERM"
       ]
     };((stack ((Ident _#51083))) "311=before tuple")
    ((stack ((Ident _#51083))) "310=before tuple")
    ((stack (Value (Ident _#51083))) "310=after tuple")
    ((stack (Value (Ident _#51083))) "309=before tuple")
    ((stack (Value (Ident _#51083)))
      "308=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#51083)))
      "308=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#51083))) "309=after tuple")
    ((stack (Value (Ident _#51083))) "311=after tuple")

     {
       "contents": [
         {
           "value": "namespace C implements {\n  const f: (t: int) => int\n}",
           "language": "jsligo"
         },
         "NESTED NAMESPACE"
       ]
     };((stack ((Ident _#51512))) "315=before tuple")
    ((stack ((Ident _#51512))) "314=before tuple")
    ((stack (Value (Ident _#51512))) "314=after tuple")
    ((stack (Value (Ident _#51512))) "313=before tuple")
    ((stack (Value (Ident _#51512)))
      "312=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#51512)))
      "312=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#51512))) "313=after tuple")
    ((stack (Value (Ident _#51512))) "315=after tuple")

     {
       "contents": [
         { "value": "f : (t: int) => int", "language": "jsligo" },
         "NESTED NAMESPACE TERM"
       ]
     };((stack ((Ident _#51941))) "319=before tuple")
    ((stack ((Ident _#51941))) "318=before tuple")
    ((stack (Value (Ident _#51941))) "318=after tuple")
    ((stack (Value (Ident _#51941))) "317=before tuple")
    ((stack (Value (Ident _#51941)))
      "316=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#51941)))
      "316=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#51941))) "317=after tuple")
    ((stack (Value (Ident _#51941))) "319=after tuple")

     {
       "contents": [
         { "value": "t : (_: unit) => t<int>", "language": "jsligo" },
         "Has type with comment inside"
       ]
     }] |}]

let%expect_test "dynamic_entrypoints.mligo" =
  get_hover_test
    { file = "contracts/dynamic_entrypoints.mligo"
    ; hover_positions =
        [ pos ~line:7 ~character:5
        ; pos ~line:15 ~character:33
        ; pos ~line:32 ~character:30
        ; pos ~line:10 ~character:7
        ; pos ~line:23 ~character:35
        ]
    };
  [%expect
    {|
    ((stack ()) "324=before let in one")
    ((stack ((Ident _#52442))) "323=before tuple")
    ((stack ((Ident _#52442))) "322=before tuple")
    ((stack (Value (Ident _#52442))) "322=after tuple")
    ((stack (Value (Ident _#52442))) "321=before tuple")
    ((stack (Value (Ident _#52442)))
      "320=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#52442)))
      "320=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#52442))) "321=after tuple")
    ((stack (Value (Ident _#52442))) "323=after tuple")
    ((stack (Value)) "324=after let in one")
    [{
       "contents": [
         {
           "value": "one : (unit, int) dynamic_entrypoint",
           "language": "cameligo"
         }
       ]
     };((stack ()) "329=before let in one")
    ((stack ((Ident _#53074))) "328=before tuple")
    ((stack ((Ident _#53074))) "327=before tuple")
    ((stack (Value (Ident _#53074))) "327=after tuple")
    ((stack (Value (Ident _#53074))) "326=before tuple")
    ((stack (Value (Ident _#53074)))
      "325=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#53074)))
      "325=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#53074))) "326=after tuple")
    ((stack (Value (Ident _#53074))) "328=after tuple")
    ((stack (Value)) "329=after let in one")

     {
       "contents": [
         {
           "value": "one : (unit, int) dynamic_entrypoint",
           "language": "cameligo"
         }
       ]
     };((stack ()) "334=before let in one")
    ((stack ((Ident _#53706))) "333=before tuple")
    ((stack ((Ident _#53706))) "332=before tuple")
    ((stack (Value (Ident _#53706))) "332=after tuple")
    ((stack (Value (Ident _#53706))) "331=before tuple")
    ((stack (Value (Ident _#53706)))
      "330=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#53706)))
      "330=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#53706))) "331=after tuple")
    ((stack (Value (Ident _#53706))) "333=after tuple")
    ((stack (Value)) "334=after let in one")

     {
       "contents": [
         {
           "value": "one : (unit, int) dynamic_entrypoint",
           "language": "cameligo"
         }
       ]
     };((stack ()) "339=before let in one")
    ((stack ((Ident _#54338))) "338=before tuple")
    ((stack ((Ident _#54338))) "337=before tuple")
    ((stack (Value (Ident _#54338))) "337=after tuple")
    ((stack (Value (Ident _#54338))) "336=before tuple")
    ((stack (Value (Ident _#54338)))
      "335=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#54338)))
      "335=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#54338))) "336=after tuple")
    ((stack (Value (Ident _#54338))) "338=after tuple")
    ((stack (Value)) "339=after let in one")

     {
       "contents": [
         {
           "value": "tick : (int ticket, int * int) dynamic_entrypoint",
           "language": "cameligo"
         }
       ]
     };((stack ()) "344=before let in one")
    ((stack ((Ident _#54970))) "343=before tuple")
    ((stack ((Ident _#54970))) "342=before tuple")
    ((stack (Value (Ident _#54970))) "342=after tuple")
    ((stack (Value (Ident _#54970))) "341=before tuple")
    ((stack (Value (Ident _#54970)))
      "340=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#54970)))
      "340=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#54970))) "341=after tuple")
    ((stack (Value (Ident _#54970))) "343=after tuple")
    ((stack (Value)) "344=after let in one")

     {
       "contents": [
         {
           "value": "tick : (int ticket, int * int) dynamic_entrypoint",
           "language": "cameligo"
         }
       ]
     }] |}]

let%expect_test "dynamic_entrypoints.jsligo" =
  get_hover_test
    { file = "contracts/dynamic_entrypoints.jsligo"
    ; hover_positions =
        [ pos ~line:7 ~character:9
        ; pos ~line:15 ~character:35
        ; pos ~line:36 ~character:29
        ; pos ~line:11 ~character:9
        ; pos ~line:25 ~character:36
        ]
    };
  [%expect
    {|
    ((stack ()) "349=before let in one")
    ((stack ((Ident _#55598))) "348=before tuple")
    ((stack ((Ident _#55598))) "347=before tuple")
    ((stack (Value (Ident _#55598))) "347=after tuple")
    ((stack (Value (Ident _#55598))) "346=before tuple")
    ((stack (Value (Ident _#55598)))
      "345=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#55598)))
      "345=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#55598))) "346=after tuple")
    ((stack (Value (Ident _#55598))) "348=after tuple")
    ((stack (Value)) "349=after let in one")
    [{
       "contents": [
         { "value": "one : dynamic_entrypoint<unit, int>", "language": "jsligo" }
       ]
     };((stack ()) "354=before let in one")
    ((stack ((Ident _#56227))) "353=before tuple")
    ((stack ((Ident _#56227))) "352=before tuple")
    ((stack (Value (Ident _#56227))) "352=after tuple")
    ((stack (Value (Ident _#56227))) "351=before tuple")
    ((stack (Value (Ident _#56227)))
      "350=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#56227)))
      "350=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#56227))) "351=after tuple")
    ((stack (Value (Ident _#56227))) "353=after tuple")
    ((stack (Value)) "354=after let in one")

     {
       "contents": [
         { "value": "one : dynamic_entrypoint<unit, int>", "language": "jsligo" }
       ]
     };((stack ()) "359=before let in one")
    ((stack ((Ident _#56856))) "358=before tuple")
    ((stack ((Ident _#56856))) "357=before tuple")
    ((stack (Value (Ident _#56856))) "357=after tuple")
    ((stack (Value (Ident _#56856))) "356=before tuple")
    ((stack (Value (Ident _#56856)))
      "355=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#56856)))
      "355=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#56856))) "356=after tuple")
    ((stack (Value (Ident _#56856))) "358=after tuple")
    ((stack (Value)) "359=after let in one")

     {
       "contents": [
         { "value": "one : dynamic_entrypoint<unit, int>", "language": "jsligo" }
       ]
     };((stack ()) "364=before let in one")
    ((stack ((Ident _#57485))) "363=before tuple")
    ((stack ((Ident _#57485))) "362=before tuple")
    ((stack (Value (Ident _#57485))) "362=after tuple")
    ((stack (Value (Ident _#57485))) "361=before tuple")
    ((stack (Value (Ident _#57485)))
      "360=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#57485)))
      "360=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#57485))) "361=after tuple")
    ((stack (Value (Ident _#57485))) "363=after tuple")
    ((stack (Value)) "364=after let in one")

     {
       "contents": [
         {
           "value": "tick : dynamic_entrypoint<ticket<int>, [int, int]>",
           "language": "jsligo"
         }
       ]
     };((stack ()) "369=before let in one")
    ((stack ((Ident _#58114))) "368=before tuple")
    ((stack ((Ident _#58114))) "367=before tuple")
    ((stack (Value (Ident _#58114))) "367=after tuple")
    ((stack (Value (Ident _#58114))) "366=before tuple")
    ((stack (Value (Ident _#58114)))
      "365=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#58114)))
      "365=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#58114))) "366=after tuple")
    ((stack (Value (Ident _#58114))) "368=after tuple")
    ((stack (Value)) "369=after let in one")

     {
       "contents": [
         {
           "value": "tick : dynamic_entrypoint<ticket<int>, [int, int]>",
           "language": "jsligo"
         }
       ]
     }] |}]

(* TODO: If we get a ghost identifier, then the module type is unresolved. *)
let%expect_test "missing_module.mligo" =
  get_hover_test
    { file = "contracts/lsp/hover/missing_module.mligo"
    ; hover_positions = [ pos ~line:0 ~character:7 ]
    };
  [%expect
    {|
    [{
       "contents": [
         { "value": "module A = (* Unresolved *)", "language": "cameligo" }
       ]
     }] |}]

(* TODO: If we get a ghost identifier, then the type annotation is unresolved. *)
let%expect_test "missing_type_annot.mligo" =
  get_hover_test
    { file = "contracts/lsp/hover/missing_type_annot.mligo"
    ; hover_positions = [ pos ~line:0 ~character:4 ]
    };
  [%expect
    {|
    ((stack ((Ident _#59035))) "373=before tuple")
    ((stack ((Ident _#59035))) "372=before tuple")
    ((stack (Value (Ident _#59035))) "372=after tuple")
    ((stack (Value (Ident _#59035))) "371=before tuple")
    ((stack (Value (Ident _#59035)))
      "370=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#59035)))
      "370=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#59035))) "371=after tuple")
    ((stack (Value (Ident _#59035))) "373=after tuple")
    [{
       "contents": [
         { "value": "a : (* Unresolved *)", "language": "cameligo" }
       ]
     }] |}]

(* TODO: If we get a ghost identifier, then the type is unresolved. *)
let%expect_test "missing_type.mligo" =
  get_hover_test
    { file = "contracts/lsp/hover/missing_type.mligo"
    ; hover_positions = [ pos ~line:0 ~character:5 ]
    };
  [%expect
    {|
    ((stack ((Ident _#59423))) "377=before tuple")
    ((stack ((Ident _#59423))) "376=before tuple")
    ((stack (Value (Ident _#59423))) "376=after tuple")
    ((stack (Value (Ident _#59423))) "375=before tuple")
    ((stack (Value (Ident _#59423)))
      "374=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#59423)))
      "374=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#59423))) "375=after tuple")
    ((stack (Value (Ident _#59423))) "377=after tuple")
    [{
       "contents": [
         { "value": "type a = (* Unresolved *)", "language": "cameligo" }
       ]
     }] |}]

let%expect_test "Hovers for variables in complex patterns show the correct types" =
  get_hover_test
    { file = "contracts/get_scope_tests/complex_patterns.mligo"
    ; hover_positions =
        [ pos ~line:0 ~character:4
        ; pos ~line:0 ~character:7
        ; pos ~line:3 ~character:6
        ; pos ~line:3 ~character:9
        ; pos ~line:6 ~character:9
        ; pos ~line:9 ~character:10
        ; pos ~line:9 ~character:13
        ; pos ~line:12 ~character:10
        ; pos ~line:12 ~character:13
        ]
    };
  [%expect
    {|
    ((stack ((Ident _#59811))) "381=before tuple")
    ((stack ((Ident _#59811))) "380=before tuple")
    ((stack (Value (Ident _#59811))) "380=after tuple")
    ((stack (Value (Ident _#59811))) "379=before tuple")
    ((stack (Value (Ident _#59811)))
      "378=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#59811)))
      "378=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#59811))) "379=after tuple")
    ((stack (Value (Ident _#59811))) "381=after tuple")
    ((stack ((Ident _#60199))) "385=before tuple")
    ((stack ((Ident _#60199))) "384=before tuple")
    ((stack (Value (Ident _#60199))) "384=after tuple")
    ((stack (Value (Ident _#60199))) "383=before tuple")
    ((stack (Value (Ident _#60199)))
      "382=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#60199)))
      "382=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#60199))) "383=after tuple")
    ((stack (Value (Ident _#60199))) "385=after tuple")
    [{ "contents": [ { "value": "a : int", "language": "cameligo" } ] };
     { "contents": [ { "value": "b : string", "language": "cameligo" } ] };((stack ((Ident _#60587))) "389=before tuple")
    ((stack ((Ident _#60587))) "388=before tuple")
    ((stack (Value (Ident _#60587))) "388=after tuple")
    ((stack (Value (Ident _#60587))) "387=before tuple")
    ((stack (Value (Ident _#60587)))
      "386=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#60587)))
      "386=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#60587))) "387=after tuple")
    ((stack (Value (Ident _#60587))) "389=after tuple")

     { "contents": [ { "value": "a : int", "language": "cameligo" } ] };((stack ((Ident _#60975))) "393=before tuple")
    ((stack ((Ident _#60975))) "392=before tuple")
    ((stack (Value (Ident _#60975))) "392=after tuple")
    ((stack (Value (Ident _#60975))) "391=before tuple")
    ((stack (Value (Ident _#60975)))
      "390=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#60975)))
      "390=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#60975))) "391=after tuple")
    ((stack (Value (Ident _#60975))) "393=after tuple")

     { "contents": [ { "value": "b : string", "language": "cameligo" } ] };((stack ((Ident _#61363))) "397=before tuple")
    ((stack ((Ident _#61363))) "396=before tuple")
    ((stack (Value (Ident _#61363))) "396=after tuple")
    ((stack (Value (Ident _#61363))) "395=before tuple")
    ((stack (Value (Ident _#61363)))
      "394=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#61363)))
      "394=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#61363))) "395=after tuple")
    ((stack (Value (Ident _#61363))) "397=after tuple")

     { "contents": [ { "value": "x : int", "language": "cameligo" } ] };((stack ((Ident _#61751))) "401=before tuple")
    ((stack ((Ident _#61751))) "400=before tuple")
    ((stack (Value (Ident _#61751))) "400=after tuple")
    ((stack (Value (Ident _#61751))) "399=before tuple")
    ((stack (Value (Ident _#61751)))
      "398=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#61751)))
      "398=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#61751))) "399=after tuple")
    ((stack (Value (Ident _#61751))) "401=after tuple")

     { "contents": [ { "value": "i : int", "language": "cameligo" } ] };((stack ((Ident _#62139))) "405=before tuple")
    ((stack ((Ident _#62139))) "404=before tuple")
    ((stack (Value (Ident _#62139))) "404=after tuple")
    ((stack (Value (Ident _#62139))) "403=before tuple")
    ((stack (Value (Ident _#62139)))
      "402=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#62139)))
      "402=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#62139))) "403=after tuple")
    ((stack (Value (Ident _#62139))) "405=after tuple")

     { "contents": [ { "value": "j : string", "language": "cameligo" } ] };((stack ((Ident _#62527))) "409=before tuple")
    ((stack ((Ident _#62527))) "408=before tuple")
    ((stack (Value (Ident _#62527))) "408=after tuple")
    ((stack (Value (Ident _#62527))) "407=before tuple")
    ((stack (Value (Ident _#62527)))
      "406=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#62527)))
      "406=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#62527))) "407=after tuple")
    ((stack (Value (Ident _#62527))) "409=after tuple")

     { "contents": [ { "value": "a : int", "language": "cameligo" } ] };((stack ((Ident _#62915))) "413=before tuple")
    ((stack ((Ident _#62915))) "412=before tuple")
    ((stack (Value (Ident _#62915))) "412=after tuple")
    ((stack (Value (Ident _#62915))) "411=before tuple")
    ((stack (Value (Ident _#62915)))
      "410=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#62915)))
      "410=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#62915))) "411=after tuple")
    ((stack (Value (Ident _#62915))) "413=after tuple")

     { "contents": [ { "value": "b : string", "language": "cameligo" } ] }] |}]

let%expect_test "The original var preserves the module path" =
  get_hover_test
    { file = "contracts/lsp/hover/module_in_type.mligo"
    ; hover_positions =
        [ pos ~line:6 ~character:4
        ; pos ~line:7 ~character:4
        ; pos ~line:9 ~character:4
        ; pos ~line:10 ~character:7
        ; pos ~line:12 ~character:4
        ; pos ~line:13 ~character:7
        ; pos ~line:14 ~character:7
        ; pos ~line:20 ~character:4
        ; pos ~line:21 ~character:7
        ; pos ~line:22 ~character:7
        ; pos ~line:29 ~character:4
        ; pos ~line:30 ~character:7
        ]
    };
  [%expect
    {|
    ((stack ((Ident _#63303))) "417=before tuple")
    ((stack ((Ident _#63303))) "416=before tuple")
    ((stack (Value (Ident _#63303))) "416=after tuple")
    ((stack (Value (Ident _#63303))) "415=before tuple")
    ((stack (Value (Ident _#63303)))
      "414=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#63303)))
      "414=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#63303))) "415=after tuple")
    ((stack (Value (Ident _#63303))) "417=after tuple")
    ((stack ((Ident _#63691))) "421=before tuple")
    ((stack ((Ident _#63691))) "420=before tuple")
    ((stack (Value (Ident _#63691))) "420=after tuple")
    ((stack (Value (Ident _#63691))) "419=before tuple")
    ((stack (Value (Ident _#63691)))
      "418=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#63691)))
      "418=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#63691))) "419=after tuple")
    ((stack (Value (Ident _#63691))) "421=after tuple")
    [{ "contents": [ { "value": "x1 : M.t", "language": "cameligo" } ] };
     { "contents": [ { "value": "y1 : M.t", "language": "cameligo" } ] };((stack ((Ident _#64079))) "425=before tuple")
    ((stack ((Ident _#64079))) "424=before tuple")
    ((stack (Value (Ident _#64079))) "424=after tuple")
    ((stack (Value (Ident _#64079))) "423=before tuple")
    ((stack (Value (Ident _#64079)))
      "422=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#64079)))
      "422=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#64079))) "423=after tuple")
    ((stack (Value (Ident _#64079))) "425=after tuple")

     { "contents": [ { "value": "x2 : M.u", "language": "cameligo" } ] };((stack ((Ident _#64467))) "429=before tuple")
    ((stack ((Ident _#64467))) "428=before tuple")
    ((stack (Value (Ident _#64467))) "428=after tuple")
    ((stack (Value (Ident _#64467))) "427=before tuple")
    ((stack (Value (Ident _#64467)))
      "426=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#64467)))
      "426=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#64467))) "427=after tuple")
    ((stack (Value (Ident _#64467))) "429=after tuple")

     { "contents": [ { "value": "y2 : unit", "language": "cameligo" } ] };((stack ((Ident _#64855))) "433=before tuple")
    ((stack ((Ident _#64855))) "432=before tuple")
    ((stack (Value (Ident _#64855))) "432=after tuple")
    ((stack (Value (Ident _#64855))) "431=before tuple")
    ((stack (Value (Ident _#64855)))
      "430=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#64855)))
      "430=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#64855))) "431=after tuple")
    ((stack (Value (Ident _#64855))) "433=after tuple")

     { "contents": [ { "value": "x3 : M.v", "language": "cameligo" } ] };((stack ((Ident _#65243))) "437=before tuple")
    ((stack ((Ident _#65243))) "436=before tuple")
    ((stack (Value (Ident _#65243))) "436=after tuple")
    ((stack (Value (Ident _#65243))) "435=before tuple")
    ((stack (Value (Ident _#65243)))
      "434=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#65243)))
      "434=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#65243))) "435=after tuple")
    ((stack (Value (Ident _#65243))) "437=after tuple")

     { "contents": [ { "value": "y3 : M.u", "language": "cameligo" } ] };((stack ((Ident _#65631))) "441=before tuple")
    ((stack ((Ident _#65631))) "440=before tuple")
    ((stack (Value (Ident _#65631))) "440=after tuple")
    ((stack (Value (Ident _#65631))) "439=before tuple")
    ((stack (Value (Ident _#65631)))
      "438=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#65631)))
      "438=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#65631))) "439=after tuple")
    ((stack (Value (Ident _#65631))) "441=after tuple")

     { "contents": [ { "value": "z3 : unit", "language": "cameligo" } ] };((stack ((Ident _#66019))) "445=before tuple")
    ((stack ((Ident _#66019))) "444=before tuple")
    ((stack (Value (Ident _#66019))) "444=after tuple")
    ((stack (Value (Ident _#66019))) "443=before tuple")
    ((stack (Value (Ident _#66019)))
      "442=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#66019)))
      "442=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#66019))) "443=after tuple")
    ((stack (Value (Ident _#66019))) "445=after tuple")

     { "contents": [ { "value": "x4 : N.t", "language": "cameligo" } ] };((stack ((Ident _#66407))) "449=before tuple")
    ((stack ((Ident _#66407))) "448=before tuple")
    ((stack (Value (Ident _#66407))) "448=after tuple")
    ((stack (Value (Ident _#66407))) "447=before tuple")
    ((stack (Value (Ident _#66407)))
      "446=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#66407)))
      "446=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#66407))) "447=after tuple")
    ((stack (Value (Ident _#66407))) "449=after tuple")

     { "contents": [ { "value": "y4 : M.t", "language": "cameligo" } ] };((stack ((Ident _#66795))) "453=before tuple")
    ((stack ((Ident _#66795))) "452=before tuple")
    ((stack (Value (Ident _#66795))) "452=after tuple")
    ((stack (Value (Ident _#66795))) "451=before tuple")
    ((stack (Value (Ident _#66795)))
      "450=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#66795)))
      "450=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#66795))) "451=after tuple")
    ((stack (Value (Ident _#66795))) "453=after tuple")

     { "contents": [ { "value": "z4 : unit", "language": "cameligo" } ] };((stack ((Ident _#67183))) "457=before tuple")
    ((stack ((Ident _#67183))) "456=before tuple")
    ((stack (Value (Ident _#67183))) "456=after tuple")
    ((stack (Value (Ident _#67183))) "455=before tuple")
    ((stack (Value (Ident _#67183)))
      "454=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#67183)))
      "454=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#67183))) "455=after tuple")
    ((stack (Value (Ident _#67183))) "457=after tuple")

     { "contents": [ { "value": "x5 : O.u", "language": "cameligo" } ] };((stack ((Ident _#67571))) "461=before tuple")
    ((stack ((Ident _#67571))) "460=before tuple")
    ((stack (Value (Ident _#67571))) "460=after tuple")
    ((stack (Value (Ident _#67571))) "459=before tuple")
    ((stack (Value (Ident _#67571)))
      "458=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#67571)))
      "458=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#67571))) "459=after tuple")
    ((stack (Value (Ident _#67571))) "461=after tuple")

     { "contents": [ { "value": "y5 : N.t", "language": "cameligo" } ] }] |}]

let%expect_test "Preserves module path in failwith" =
  get_hover_test
    { file = "contracts/lsp/hover/failwith_module_path.mligo"
    ; hover_positions = [ pos ~line:6 ~character:4; pos ~line:7 ~character:4 ]
    };
  [%expect
    {|
    ((stack ((Ident _#67959))) "465=before tuple")
    ((stack ((Ident _#67959))) "464=before tuple")
    ((stack (Value (Ident _#67959))) "464=after tuple")
    ((stack (Value (Ident _#67959))) "463=before tuple")
    ((stack (Value (Ident _#67959)))
      "462=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#67959)))
      "462=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#67959))) "463=after tuple")
    ((stack (Value (Ident _#67959))) "465=after tuple")
    ((stack ((Ident _#68347))) "469=before tuple")
    ((stack ((Ident _#68347))) "468=before tuple")
    ((stack (Value (Ident _#68347))) "468=after tuple")
    ((stack (Value (Ident _#68347))) "467=before tuple")
    ((stack (Value (Ident _#68347)))
      "466=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#68347)))
      "466=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#68347))) "467=after tuple")
    ((stack (Value (Ident _#68347))) "469=after tuple")
    [{ "contents": [ { "value": "x : A.B.t", "language": "cameligo" } ] };
     { "contents": [ { "value": "y : A.B.t", "language": "cameligo" } ] }] |}]

let%expect_test "Preserves module path inside option type" =
  get_hover_test
    { file = "contracts/lsp/hover/option_module_path.mligo"
    ; hover_positions = [ pos ~line:4 ~character:4 ]
    };
  [%expect
    {|
      ((stack ((Ident _#68735))) "473=before tuple")
      ((stack ((Ident _#68735))) "472=before tuple")
      ((stack (Value (Ident _#68735))) "472=after tuple")
      ((stack (Value (Ident _#68735))) "471=before tuple")
      ((stack (Value (Ident _#68735)))
        "470=before (Nil\
       \n ((desc Operation)\
       \n  (range\
       \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
      ((stack (Value Value (Ident _#68735)))
        "470=after (Nil\
       \n ((desc Operation)\
       \n  (range\
       \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
      ((stack (Value Value (Ident _#68735))) "471=after tuple")
      ((stack (Value (Ident _#68735))) "473=after tuple")
      [{ "contents": [ { "value": "x : A.t option", "language": "cameligo" } ] }] |}]

let%expect_test "Preserves module path of an imported module" =
  get_hover_test
    { file = "contracts/lsp/hover/imported_module.mligo"
    ; hover_positions = [ pos ~line:2 ~character:4; pos ~line:3 ~character:4 ]
    };
  [%expect
    {|
    ((stack ((Ident _#69127))) "477=before tuple")
    ((stack ((Ident _#69127))) "476=before tuple")
    ((stack (Value (Ident _#69127))) "476=after tuple")
    ((stack (Value (Ident _#69127))) "475=before tuple")
    ((stack (Value (Ident _#69127)))
      "474=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#69127)))
      "474=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#69127))) "475=after tuple")
    ((stack (Value (Ident _#69127))) "477=after tuple")
    ((stack ((Ident _#69519))) "481=before tuple")
    ((stack ((Ident _#69519))) "480=before tuple")
    ((stack (Value (Ident _#69519))) "480=after tuple")
    ((stack (Value (Ident _#69519))) "479=before tuple")
    ((stack (Value (Ident _#69519)))
      "478=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#69519)))
      "478=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#69519))) "479=after tuple")
    ((stack (Value (Ident _#69519))) "481=after tuple")
    [{ "contents": [ { "value": "x : M.t", "language": "cameligo" } ] };
     { "contents": [ { "value": "y : M.t option", "language": "cameligo" } ] }] |}]

let%expect_test "Shows the correct path relative to the current env" =
  get_hover_test
    { file = "contracts/lsp/hover/module_access.mligo"
    ; hover_positions =
        [ pos ~line:3 ~character:6; pos ~line:6 ~character:4; pos ~line:6 ~character:10 ]
    };
  [%expect
    {|
    ((stack ((Ident _#69908))) "485=before tuple")
    ((stack ((Ident _#69908))) "484=before tuple")
    ((stack (Value (Ident _#69908))) "484=after tuple")
    ((stack (Value (Ident _#69908))) "483=before tuple")
    ((stack (Value (Ident _#69908)))
      "482=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#69908)))
      "482=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#69908))) "483=after tuple")
    ((stack (Value (Ident _#69908))) "485=after tuple")
    ((stack ((Ident _#70297))) "489=before tuple")
    ((stack ((Ident _#70297))) "488=before tuple")
    ((stack (Value (Ident _#70297))) "488=after tuple")
    ((stack (Value (Ident _#70297))) "487=before tuple")
    ((stack (Value (Ident _#70297)))
      "486=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#70297)))
      "486=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#70297))) "487=after tuple")
    ((stack (Value (Ident _#70297))) "489=after tuple")
    [{ "contents": [ { "value": "x : t", "language": "cameligo" } ] };
     { "contents": [ { "value": "y : M.t", "language": "cameligo" } ] };((stack ((Ident _#70686))) "493=before tuple")
    ((stack ((Ident _#70686))) "492=before tuple")
    ((stack (Value (Ident _#70686))) "492=after tuple")
    ((stack (Value (Ident _#70686))) "491=before tuple")
    ((stack (Value (Ident _#70686)))
      "490=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#70686)))
      "490=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#70686))) "491=after tuple")
    ((stack (Value (Ident _#70686))) "493=after tuple")

     { "contents": [ { "value": "x : M.t", "language": "cameligo" } ] }] |}]

let%expect_test "Shows the correct path relative to the current envs" =
  get_hover_test
    { file = "contracts/lsp/hover/module_accesses.mligo"
    ; hover_positions =
        [ pos ~line:14 ~character:10
        ; (* TODO: perhaps should yield the same type as above? *)
          pos ~line:14 ~character:4
        ; (* TODO: type M.export? *)
          pos ~line:16 ~character:11
        ; pos ~line:16 ~character:5
        ]
    };
  [%expect
    {|
    ((stack ((Ident _#71081))) "497=before tuple")
    ((stack ((Ident _#71081))) "496=before tuple")
    ((stack (Value (Ident _#71081))) "496=after tuple")
    ((stack (Value (Ident _#71081))) "495=before tuple")
    ((stack (Value (Ident _#71081)))
      "494=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#71081)))
      "494=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#71081))) "495=after tuple")
    ((stack (Value (Ident _#71081))) "497=after tuple")
    [{
       "contents": [
         { "value": "x : (M.u * O.t * O.u) option", "language": "cameligo" }
       ]
     };((stack ((Ident _#71476))) "501=before tuple")
    ((stack ((Ident _#71476))) "500=before tuple")
    ((stack (Value (Ident _#71476))) "500=after tuple")
    ((stack (Value (Ident _#71476))) "499=before tuple")
    ((stack (Value (Ident _#71476)))
      "498=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#71476)))
      "498=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#71476))) "499=after tuple")
    ((stack (Value (Ident _#71476))) "501=after tuple")

     {
       "contents": [
         {
           "value": "y : (Import.M.u * N.t * N.u) option",
           "language": "cameligo"
         }
       ]
     };((stack ((Ident _#71871))) "505=before tuple")
    ((stack ((Ident _#71871))) "504=before tuple")
    ((stack (Value (Ident _#71871))) "504=after tuple")
    ((stack (Value (Ident _#71871))) "503=before tuple")
    ((stack (Value (Ident _#71871)))
      "502=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#71871)))
      "502=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#71871))) "503=after tuple")
    ((stack (Value (Ident _#71871))) "505=after tuple")

     {
       "contents": [
         { "value": "type export = M.u * O.t * O.u", "language": "cameligo" }
       ]
     };((stack ((Ident _#72266))) "509=before tuple")
    ((stack ((Ident _#72266))) "508=before tuple")
    ((stack (Value (Ident _#72266))) "508=after tuple")
    ((stack (Value (Ident _#72266))) "507=before tuple")
    ((stack (Value (Ident _#72266)))
      "506=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#72266)))
      "506=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#72266))) "507=after tuple")
    ((stack (Value (Ident _#72266))) "509=after tuple")

     { "contents": [ { "value": "type t = O.export", "language": "cameligo" } ] }] |}]

let%expect_test "Constructors and record fields hovers (CameLIGO)" =
  get_hover_test
    { file = "contracts/lsp/hover/ctors_and_fields.mligo"
    ; hover_positions =
        [ pos ~line:1 ~character:5
        ; pos ~line:2 ~character:4
        ; pos ~line:3 ~character:4
        ; pos ~line:4 ~character:6
        ; pos ~line:4 ~character:13
        ; pos ~line:4 ~character:22
        ]
    };
  [%expect
    {|
    ((stack ((Ident _#72654))) "513=before tuple")
    ((stack ((Ident _#72654))) "512=before tuple")
    ((stack (Value (Ident _#72654))) "512=after tuple")
    ((stack (Value (Ident _#72654))) "511=before tuple")
    ((stack (Value (Ident _#72654)))
      "510=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#72654)))
      "510=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#72654))) "511=after tuple")
    ((stack (Value (Ident _#72654))) "513=after tuple")
    ((stack ((Ident _#73042))) "517=before tuple")
    ((stack ((Ident _#73042))) "516=before tuple")
    ((stack (Value (Ident _#73042))) "516=after tuple")
    ((stack (Value (Ident _#73042))) "515=before tuple")
    ((stack (Value (Ident _#73042)))
      "514=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#73042)))
      "514=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#73042))) "515=after tuple")
    ((stack (Value (Ident _#73042))) "517=after tuple")
    [{ "contents": [ { "value": "Foo", "language": "cameligo" } ] };
     { "contents": [ { "value": "Bar of int", "language": "cameligo" } ] };((stack ((Ident _#73430))) "521=before tuple")
    ((stack ((Ident _#73430))) "520=before tuple")
    ((stack (Value (Ident _#73430))) "520=after tuple")
    ((stack (Value (Ident _#73430))) "519=before tuple")
    ((stack (Value (Ident _#73430)))
      "518=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#73430)))
      "518=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#73430))) "519=after tuple")
    ((stack (Value (Ident _#73430))) "521=after tuple")

     { "contents": [ { "value": "Baz of unit", "language": "cameligo" } ] };((stack ((Ident _#73818))) "525=before tuple")
    ((stack ((Ident _#73818))) "524=before tuple")
    ((stack (Value (Ident _#73818))) "524=after tuple")
    ((stack (Value (Ident _#73818))) "523=before tuple")
    ((stack (Value (Ident _#73818)))
      "522=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#73818)))
      "522=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#73818))) "523=after tuple")
    ((stack (Value (Ident _#73818))) "525=after tuple")

     {
       "contents": [
         { "value": "Aaa of {\n a : int;\n b : bool\n}", "language": "cameligo" }
       ]
     };((stack ((Ident _#74206))) "529=before tuple")
    ((stack ((Ident _#74206))) "528=before tuple")
    ((stack (Value (Ident _#74206))) "528=after tuple")
    ((stack (Value (Ident _#74206))) "527=before tuple")
    ((stack (Value (Ident _#74206)))
      "526=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#74206)))
      "526=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#74206))) "527=after tuple")
    ((stack (Value (Ident _#74206))) "529=after tuple")
    ((stack ((Ident _#74594))) "533=before tuple")
    ((stack ((Ident _#74594))) "532=before tuple")
    ((stack (Value (Ident _#74594))) "532=after tuple")
    ((stack (Value (Ident _#74594))) "531=before tuple")
    ((stack (Value (Ident _#74594)))
      "530=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#74594)))
      "530=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#74594))) "531=after tuple")
    ((stack (Value (Ident _#74594))) "533=after tuple")
     { "contents": [ { "value": "a : int", "language": "cameligo" } ] };
     { "contents": [ { "value": "b : bool", "language": "cameligo" } ] }] |}]

let%expect_test "Constructors and record fields hovers (JsLIGO)" =
  get_hover_test
    { file = "contracts/lsp/hover/ctors_and_fields.jsligo"
    ; hover_positions =
        [ pos ~line:1 ~character:7
        ; pos ~line:2 ~character:8
        ; pos ~line:3 ~character:8
        ; pos ~line:4 ~character:9
        ; pos ~line:4 ~character:14
        ; pos ~line:4 ~character:23
        ; pos ~line:5 ~character:8
        ]
    };
  [%expect
    {|
    ((stack ((Ident _#74982))) "537=before tuple")
    ((stack ((Ident _#74982))) "536=before tuple")
    ((stack (Value (Ident _#74982))) "536=after tuple")
    ((stack (Value (Ident _#74982))) "535=before tuple")
    ((stack (Value (Ident _#74982)))
      "534=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#74982)))
      "534=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#74982))) "535=after tuple")
    ((stack (Value (Ident _#74982))) "537=after tuple")
    ((stack ((Ident _#75370))) "541=before tuple")
    ((stack ((Ident _#75370))) "540=before tuple")
    ((stack (Value (Ident _#75370))) "540=after tuple")
    ((stack (Value (Ident _#75370))) "539=before tuple")
    ((stack (Value (Ident _#75370)))
      "538=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#75370)))
      "538=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#75370))) "539=after tuple")
    ((stack (Value (Ident _#75370))) "541=after tuple")
    [{ "contents": [ { "value": "[\"Foo\"]", "language": "jsligo" } ] };
     { "contents": [ { "value": "[\"Bar\", int]", "language": "jsligo" } ] };((stack ((Ident _#75758))) "545=before tuple")
    ((stack ((Ident _#75758))) "544=before tuple")
    ((stack (Value (Ident _#75758))) "544=after tuple")
    ((stack (Value (Ident _#75758))) "543=before tuple")
    ((stack (Value (Ident _#75758)))
      "542=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#75758)))
      "542=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#75758))) "543=after tuple")
    ((stack (Value (Ident _#75758))) "545=after tuple")

     { "contents": [ { "value": "[\"Baz\", unit]", "language": "jsligo" } ] };((stack ((Ident _#76146))) "549=before tuple")
    ((stack ((Ident _#76146))) "548=before tuple")
    ((stack (Value (Ident _#76146))) "548=after tuple")
    ((stack (Value (Ident _#76146))) "547=before tuple")
    ((stack (Value (Ident _#76146)))
      "546=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#76146)))
      "546=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#76146))) "547=after tuple")
    ((stack (Value (Ident _#76146))) "549=after tuple")

     {
       "contents": [
         { "value": "[\"Aaa\", { a: int; b: bool }]", "language": "jsligo" }
       ]
     };((stack ((Ident _#76534))) "553=before tuple")
    ((stack ((Ident _#76534))) "552=before tuple")
    ((stack (Value (Ident _#76534))) "552=after tuple")
    ((stack (Value (Ident _#76534))) "551=before tuple")
    ((stack (Value (Ident _#76534)))
      "550=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#76534)))
      "550=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#76534))) "551=after tuple")
    ((stack (Value (Ident _#76534))) "553=after tuple")
    ((stack ((Ident _#76922))) "557=before tuple")
    ((stack ((Ident _#76922))) "556=before tuple")
    ((stack (Value (Ident _#76922))) "556=after tuple")
    ((stack (Value (Ident _#76922))) "555=before tuple")
    ((stack (Value (Ident _#76922)))
      "554=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#76922)))
      "554=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#76922))) "555=after tuple")
    ((stack (Value (Ident _#76922))) "557=after tuple")
     { "contents": [ { "value": "a : int", "language": "jsligo" } ] };
     { "contents": [ { "value": "b : bool", "language": "jsligo" } ] };((stack ((Ident _#77310))) "561=before tuple")
    ((stack ((Ident _#77310))) "560=before tuple")
    ((stack (Value (Ident _#77310))) "560=after tuple")
    ((stack (Value (Ident _#77310))) "559=before tuple")
    ((stack (Value (Ident _#77310)))
      "558=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#77310)))
      "558=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#77310))) "559=after tuple")
    ((stack (Value (Ident _#77310))) "561=after tuple")

     { "contents": [ { "value": "[\"Bbb\", bool]", "language": "jsligo" } ] }] |}]

let%expect_test "Disc union fields" =
  get_hover_test
    { file = "contracts/lsp/go_to_implementations/disc_union_fields.jsligo"
    ; hover_positions =
        [ pos ~line:0 ~character:11
        ; pos ~line:0 ~character:38
        ; pos ~line:3 ~character:12
        ; pos ~line:0 ~character:24
        ; pos ~line:4 ~character:24
        ; pos ~line:0 ~character:52
        ; pos ~line:5 ~character:22
        ]
    };
  [%expect
    {|
    ((stack ((Ident _#77724))) "565=before tuple")
    ((stack ((Ident _#77724))) "564=before tuple")
    ((stack (Value (Ident _#77724))) "564=after tuple")
    ((stack (Value (Ident _#77724))) "563=before tuple")
    ((stack (Value (Ident _#77724)))
      "562=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#77724)))
      "562=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#77724))) "563=after tuple")
    ((stack (Value (Ident _#77724))) "565=after tuple")
    ((stack ((Ident _#78138))) "569=before tuple")
    ((stack ((Ident _#78138))) "568=before tuple")
    ((stack (Value (Ident _#78138))) "568=after tuple")
    ((stack (Value (Ident _#78138))) "567=before tuple")
    ((stack (Value (Ident _#78138)))
      "566=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#78138)))
      "566=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#78138))) "567=after tuple")
    ((stack (Value (Ident _#78138))) "569=after tuple")
    [{ "contents": [ { "value": "kind : \"42\"", "language": "jsligo" } ] };
     { "contents": [ { "value": "kind : \"aaa\"", "language": "jsligo" } ] };((stack ((Ident _#78552))) "573=before tuple")
    ((stack ((Ident _#78552))) "572=before tuple")
    ((stack (Value (Ident _#78552))) "572=after tuple")
    ((stack (Value (Ident _#78552))) "571=before tuple")
    ((stack (Value (Ident _#78552)))
      "570=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#78552)))
      "570=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#78552))) "571=after tuple")
    ((stack (Value (Ident _#78552))) "573=after tuple")

     { "contents": [ { "value": "kind : \"aaa\"", "language": "jsligo" } ] };((stack ((Ident _#78966))) "577=before tuple")
    ((stack ((Ident _#78966))) "576=before tuple")
    ((stack (Value (Ident _#78966))) "576=after tuple")
    ((stack (Value (Ident _#78966))) "575=before tuple")
    ((stack (Value (Ident _#78966)))
      "574=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#78966)))
      "574=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#78966))) "575=after tuple")
    ((stack (Value (Ident _#78966))) "577=after tuple")

     { "contents": [ { "value": "a : int", "language": "jsligo" } ] };((stack ((Ident _#79380))) "581=before tuple")
    ((stack ((Ident _#79380))) "580=before tuple")
    ((stack (Value (Ident _#79380))) "580=after tuple")
    ((stack (Value (Ident _#79380))) "579=before tuple")
    ((stack (Value (Ident _#79380)))
      "578=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#79380)))
      "578=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#79380))) "579=after tuple")
    ((stack (Value (Ident _#79380))) "581=after tuple")

     { "contents": [ { "value": "a : int", "language": "jsligo" } ] };((stack ((Ident _#79794))) "585=before tuple")
    ((stack ((Ident _#79794))) "584=before tuple")
    ((stack (Value (Ident _#79794))) "584=after tuple")
    ((stack (Value (Ident _#79794))) "583=before tuple")
    ((stack (Value (Ident _#79794)))
      "582=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#79794)))
      "582=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#79794))) "583=after tuple")
    ((stack (Value (Ident _#79794))) "585=after tuple")

     { "contents": [ { "value": "b : bool", "language": "jsligo" } ] };((stack ((Ident _#80208))) "589=before tuple")
    ((stack ((Ident _#80208))) "588=before tuple")
    ((stack (Value (Ident _#80208))) "588=after tuple")
    ((stack (Value (Ident _#80208))) "587=before tuple")
    ((stack (Value (Ident _#80208)))
      "586=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#80208)))
      "586=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#80208))) "587=after tuple")
    ((stack (Value (Ident _#80208))) "589=after tuple")

     { "contents": [ { "value": "b : bool", "language": "jsligo" } ] }] |}]

let%expect_test "Polymorphic types (CameLIGO)" =
  get_hover_test
    { file = "contracts/lsp/hover/poly_types.mligo"
    ; hover_positions =
        [ pos ~line:0 ~character:15
        ; pos ~line:2 ~character:5
        ; pos ~line:4 ~character:4
        ; pos ~line:4 ~character:21
        ; pos ~line:4 ~character:24
        ; pos ~line:4 ~character:28
        ; pos ~line:4 ~character:30
        ; pos ~line:6 ~character:4
        ; pos ~line:6 ~character:15
        ; pos ~line:6 ~character:17
        ; pos ~line:7 ~character:8
        ; pos ~line:8 ~character:9
        ; pos ~line:8 ~character:14
        ; pos ~line:8 ~character:16
        ; pos ~line:10 ~character:4
        ; pos ~line:10 ~character:33
        ; pos ~line:10 ~character:56
        ; pos ~line:16 ~character:4
        ; pos ~line:16 ~character:16
        ; pos ~line:18 ~character:4
        ; pos ~line:20 ~character:4
        ; pos ~line:22 ~character:4
        ; pos ~line:24 ~character:4
        ; pos ~line:30 ~character:4
        ; pos ~line:34 ~character:4
        ; pos ~line:38 ~character:4
        ; pos ~line:40 ~character:4
        ; pos ~line:47 ~character:10
        ; pos ~line:50 ~character:5
        ; pos ~line:52 ~character:4
        ]
    };
  [%expect
    {|
    ((stack ((Ident _#80605))) "593=before tuple")
    ((stack ((Ident _#80605))) "592=before tuple")
    ((stack (Value (Ident _#80605))) "592=after tuple")
    ((stack (Value (Ident _#80605))) "591=before tuple")
    ((stack (Value (Ident _#80605)))
      "590=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#80605)))
      "590=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#80605))) "591=after tuple")
    ((stack (Value (Ident _#80605))) "593=after tuple")
    [{
       "contents": [
         {
           "value": "type ('a, 'b) func = Func of ('a -> 'b)",
           "language": "cameligo"
         }
       ]
     };((stack ((Ident _#81002))) "597=before tuple")
    ((stack ((Ident _#81002))) "596=before tuple")
    ((stack (Value (Ident _#81002))) "596=after tuple")
    ((stack (Value (Ident _#81002))) "595=before tuple")
    ((stack (Value (Ident _#81002)))
      "594=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#81002)))
      "594=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#81002))) "595=after tuple")
    ((stack (Value (Ident _#81002))) "597=after tuple")

     {
       "contents": [
         { "value": "incr : (int, int) func", "language": "cameligo" }
       ]
     };((stack ((Ident _#81399))) "601=before tuple")
    ((stack ((Ident _#81399))) "600=before tuple")
    ((stack (Value (Ident _#81399))) "600=after tuple")
    ((stack (Value (Ident _#81399))) "599=before tuple")
    ((stack (Value (Ident _#81399)))
      "598=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#81399)))
      "598=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#81399))) "599=after tuple")
    ((stack (Value (Ident _#81399))) "601=after tuple")

     {
       "contents": [
         {
           "value": "apply_func : 'a 'b.('a, 'b) func -> 'a -> 'b",
           "language": "cameligo"
         }
       ]
     };((stack ((Ident _#81796))) "605=before tuple")
    ((stack ((Ident _#81796))) "604=before tuple")
    ((stack (Value (Ident _#81796))) "604=after tuple")
    ((stack (Value (Ident _#81796))) "603=before tuple")
    ((stack (Value (Ident _#81796)))
      "602=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#81796)))
      "602=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#81796))) "603=after tuple")
    ((stack (Value (Ident _#81796))) "605=after tuple")
    ((stack ((Ident _#82193))) "609=before tuple")
    ((stack ((Ident _#82193))) "608=before tuple")
    ((stack (Value (Ident _#82193))) "608=after tuple")
    ((stack (Value (Ident _#82193))) "607=before tuple")
    ((stack (Value (Ident _#82193)))
      "606=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#82193)))
      "606=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#82193))) "607=after tuple")
    ((stack (Value (Ident _#82193))) "609=after tuple")
     { "contents": [ { "value": "f : a -> b", "language": "cameligo" } ] };
     { "contents": [ { "value": "x : a", "language": "cameligo" } ] };((stack ((Ident _#82590))) "613=before tuple")
    ((stack ((Ident _#82590))) "612=before tuple")
    ((stack (Value (Ident _#82590))) "612=after tuple")
    ((stack (Value (Ident _#82590))) "611=before tuple")
    ((stack (Value (Ident _#82590)))
      "610=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#82590)))
      "610=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#82590))) "611=after tuple")
    ((stack (Value (Ident _#82590))) "613=after tuple")

     { "contents": [ { "value": "f : a -> b", "language": "cameligo" } ] };((stack ((Ident _#82987))) "617=before tuple")
    ((stack ((Ident _#82987))) "616=before tuple")
    ((stack (Value (Ident _#82987))) "616=after tuple")
    ((stack (Value (Ident _#82987))) "615=before tuple")
    ((stack (Value (Ident _#82987)))
      "614=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#82987)))
      "614=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#82987))) "615=after tuple")
    ((stack (Value (Ident _#82987))) "617=after tuple")

     { "contents": [ { "value": "x : a", "language": "cameligo" } ] };((stack ((Ident _#83384))) "621=before tuple")
    ((stack ((Ident _#83384))) "620=before tuple")
    ((stack (Value (Ident _#83384))) "620=after tuple")
    ((stack (Value (Ident _#83384))) "619=before tuple")
    ((stack (Value (Ident _#83384)))
      "618=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#83384)))
      "618=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#83384))) "619=after tuple")
    ((stack (Value (Ident _#83384))) "621=after tuple")

     {
       "contents": [
         {
           "value": "apply_func : 'a 'b.('a, 'b) func -> 'a -> 'b",
           "language": "cameligo"
         }
       ]
     };((stack ((Ident _#83781))) "625=before tuple")
    ((stack ((Ident _#83781))) "624=before tuple")
    ((stack (Value (Ident _#83781))) "624=after tuple")
    ((stack (Value (Ident _#83781))) "623=before tuple")
    ((stack (Value (Ident _#83781)))
      "622=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#83781)))
      "622=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#83781))) "623=after tuple")
    ((stack (Value (Ident _#83781))) "625=after tuple")

     { "contents": [ { "value": "f : (a, b) func", "language": "cameligo" } ] };((stack ((Ident _#84178))) "629=before tuple")
    ((stack ((Ident _#84178))) "628=before tuple")
    ((stack (Value (Ident _#84178))) "628=after tuple")
    ((stack (Value (Ident _#84178))) "627=before tuple")
    ((stack (Value (Ident _#84178)))
      "626=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#84178)))
      "626=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#84178))) "627=after tuple")
    ((stack (Value (Ident _#84178))) "629=after tuple")

     { "contents": [ { "value": "x : a", "language": "cameligo" } ] };((stack ((Ident _#84575))) "633=before tuple")
    ((stack ((Ident _#84575))) "632=before tuple")
    ((stack (Value (Ident _#84575))) "632=after tuple")
    ((stack (Value (Ident _#84575))) "631=before tuple")
    ((stack (Value (Ident _#84575)))
      "630=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#84575)))
      "630=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#84575))) "631=after tuple")
    ((stack (Value (Ident _#84575))) "633=after tuple")

     { "contents": [ { "value": "f : (a, b) func", "language": "cameligo" } ] };((stack ((Ident _#84972))) "637=before tuple")
    ((stack ((Ident _#84972))) "636=before tuple")
    ((stack (Value (Ident _#84972))) "636=after tuple")
    ((stack (Value (Ident _#84972))) "635=before tuple")
    ((stack (Value (Ident _#84972)))
      "634=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#84972)))
      "634=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#84972))) "635=after tuple")
    ((stack (Value (Ident _#84972))) "637=after tuple")

     { "contents": [ { "value": "f : a -> b", "language": "cameligo" } ] };((stack ((Ident _#85369))) "641=before tuple")
    ((stack ((Ident _#85369))) "640=before tuple")
    ((stack (Value (Ident _#85369))) "640=after tuple")
    ((stack (Value (Ident _#85369))) "639=before tuple")
    ((stack (Value (Ident _#85369)))
      "638=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#85369)))
      "638=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#85369))) "639=after tuple")
    ((stack (Value (Ident _#85369))) "641=after tuple")

     { "contents": [ { "value": "f : a -> b", "language": "cameligo" } ] };((stack ((Ident _#85766))) "645=before tuple")
    ((stack ((Ident _#85766))) "644=before tuple")
    ((stack (Value (Ident _#85766))) "644=after tuple")
    ((stack (Value (Ident _#85766))) "643=before tuple")
    ((stack (Value (Ident _#85766)))
      "642=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#85766)))
      "642=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#85766))) "643=after tuple")
    ((stack (Value (Ident _#85766))) "645=after tuple")

     { "contents": [ { "value": "x : a", "language": "cameligo" } ] };((stack ((Ident _#86163))) "649=before tuple")
    ((stack ((Ident _#86163))) "648=before tuple")
    ((stack (Value (Ident _#86163))) "648=after tuple")
    ((stack (Value (Ident _#86163))) "647=before tuple")
    ((stack (Value (Ident _#86163)))
      "646=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#86163)))
      "646=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#86163))) "647=after tuple")
    ((stack (Value (Ident _#86163))) "649=after tuple")

     {
       "contents": [
         {
           "value": "apply_func :\n  'dom\n  'codom.('dom, 'codom) func -> 'dom -> 'codom",
           "language": "cameligo"
         }
       ]
     };((stack ((Ident _#86560))) "653=before tuple")
    ((stack ((Ident _#86560))) "652=before tuple")
    ((stack (Value (Ident _#86560))) "652=after tuple")
    ((stack (Value (Ident _#86560))) "651=before tuple")
    ((stack (Value (Ident _#86560)))
      "650=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#86560)))
      "650=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#86560))) "651=after tuple")
    ((stack (Value (Ident _#86560))) "653=after tuple")

     {
       "contents": [
         { "value": "f : (dom, codom) func", "language": "cameligo" }
       ]
     };((stack ((Ident _#86957))) "657=before tuple")
    ((stack ((Ident _#86957))) "656=before tuple")
    ((stack (Value (Ident _#86957))) "656=after tuple")
    ((stack (Value (Ident _#86957))) "655=before tuple")
    ((stack (Value (Ident _#86957)))
      "654=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#86957)))
      "654=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#86957))) "655=after tuple")
    ((stack (Value (Ident _#86957))) "657=after tuple")
    ((stack ((Ident _#87354))) "661=before tuple")
    ((stack ((Ident _#87354))) "660=before tuple")
    ((stack (Value (Ident _#87354))) "660=after tuple")
    ((stack (Value (Ident _#87354))) "659=before tuple")
    ((stack (Value (Ident _#87354)))
      "658=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#87354)))
      "658=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#87354))) "659=after tuple")
    ((stack (Value (Ident _#87354))) "661=after tuple")
     { "contents": [ { "value": "x : dom", "language": "cameligo" } ] };
     {
       "contents": [ { "value": "f : 'a.'a t -> 'a t", "language": "cameligo" } ]
     };((stack ((Ident _#87751))) "665=before tuple")
    ((stack ((Ident _#87751))) "664=before tuple")
    ((stack (Value (Ident _#87751))) "664=after tuple")
    ((stack (Value (Ident _#87751))) "663=before tuple")
    ((stack (Value (Ident _#87751)))
      "662=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#87751)))
      "662=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#87751))) "663=after tuple")
    ((stack (Value (Ident _#87751))) "665=after tuple")
    ((stack ((Ident _#88148))) "669=before tuple")
    ((stack ((Ident _#88148))) "668=before tuple")
    ((stack (Value (Ident _#88148))) "668=after tuple")
    ((stack (Value (Ident _#88148))) "667=before tuple")
    ((stack (Value (Ident _#88148)))
      "666=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#88148)))
      "666=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#88148))) "667=after tuple")
    ((stack (Value (Ident _#88148))) "669=after tuple")
     { "contents": [ { "value": "r : a t", "language": "cameligo" } ] };
     {
       "contents": [
         { "value": "ticket : nat ticket option", "language": "cameligo" }
       ]
     };((stack ((Ident _#88545))) "673=before tuple")
    ((stack ((Ident _#88545))) "672=before tuple")
    ((stack (Value (Ident _#88545))) "672=after tuple")
    ((stack (Value (Ident _#88545))) "671=before tuple")
    ((stack (Value (Ident _#88545)))
      "670=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#88545)))
      "670=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#88545))) "671=after tuple")
    ((stack (Value (Ident _#88545))) "673=after tuple")

     { "contents": [ { "value": "lst : int list", "language": "cameligo" } ] };((stack ((Ident _#88942))) "677=before tuple")
    ((stack ((Ident _#88942))) "676=before tuple")
    ((stack (Value (Ident _#88942))) "676=after tuple")
    ((stack (Value (Ident _#88942))) "675=before tuple")
    ((stack (Value (Ident _#88942)))
      "674=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#88942)))
      "674=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#88942))) "675=after tuple")
    ((stack (Value (Ident _#88942))) "677=after tuple")

     { "contents": [ { "value": "opt : int option", "language": "cameligo" } ] };((stack ((Ident _#89339))) "681=before tuple")
    ((stack ((Ident _#89339))) "680=before tuple")
    ((stack (Value (Ident _#89339))) "680=after tuple")
    ((stack (Value (Ident _#89339))) "679=before tuple")
    ((stack (Value (Ident _#89339)))
      "678=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#89339)))
      "678=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#89339))) "679=after tuple")
    ((stack (Value (Ident _#89339))) "681=after tuple")

     {
       "contents": [
         { "value": "big_map : (int, nat) t", "language": "cameligo" }
       ]
     };((stack ((Ident _#89736))) "685=before tuple")
    ((stack ((Ident _#89736))) "684=before tuple")
    ((stack (Value (Ident _#89736))) "684=after tuple")
    ((stack (Value (Ident _#89736))) "683=before tuple")
    ((stack (Value (Ident _#89736)))
      "682=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#89736)))
      "682=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#89736))) "683=after tuple")
    ((stack (Value (Ident _#89736))) "685=after tuple")

     { "contents": [ { "value": "x : string M.t", "language": "cameligo" } ] };((stack ((Ident _#90133))) "689=before tuple")
    ((stack ((Ident _#90133))) "688=before tuple")
    ((stack (Value (Ident _#90133))) "688=after tuple")
    ((stack (Value (Ident _#90133))) "687=before tuple")
    ((stack (Value (Ident _#90133)))
      "686=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#90133)))
      "686=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#90133))) "687=after tuple")
    ((stack (Value (Ident _#90133))) "689=after tuple")

     {
       "contents": [
         { "value": "foo : int Common.foo", "language": "cameligo" }
       ]
     };((stack ((Ident _#90530))) "693=before tuple")
    ((stack ((Ident _#90530))) "692=before tuple")
    ((stack (Value (Ident _#90530))) "692=after tuple")
    ((stack (Value (Ident _#90530))) "691=before tuple")
    ((stack (Value (Ident _#90530)))
      "690=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#90530)))
      "690=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#90530))) "691=after tuple")
    ((stack (Value (Ident _#90530))) "693=after tuple")

     {
       "contents": [
         { "value": "ok_result : (int, string) result", "language": "cameligo" }
       ]
     };((stack ((Ident _#90927))) "697=before tuple")
    ((stack ((Ident _#90927))) "696=before tuple")
    ((stack (Value (Ident _#90927))) "696=after tuple")
    ((stack (Value (Ident _#90927))) "695=before tuple")
    ((stack (Value (Ident _#90927)))
      "694=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#90927)))
      "694=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#90927))) "695=after tuple")
    ((stack (Value (Ident _#90927))) "697=after tuple")

     {
       "contents": [
         {
           "value": "error_result : (int, string) result",
           "language": "cameligo"
         }
       ]
     };((stack ((Ident _#91324))) "701=before tuple")
    ((stack ((Ident _#91324))) "700=before tuple")
    ((stack (Value (Ident _#91324))) "700=after tuple")
    ((stack (Value (Ident _#91324))) "699=before tuple")
    ((stack (Value (Ident _#91324)))
      "698=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#91324)))
      "698=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#91324))) "699=after tuple")
    ((stack (Value (Ident _#91324))) "701=after tuple")

     {
       "contents": [
         { "value": "type 'a b = B of 'a X.x", "language": "cameligo" }
       ]
     };((stack ((Ident _#91721))) "705=before tuple")
    ((stack ((Ident _#91721))) "704=before tuple")
    ((stack (Value (Ident _#91721))) "704=after tuple")
    ((stack (Value (Ident _#91721))) "703=before tuple")
    ((stack (Value (Ident _#91721)))
      "702=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#91721)))
      "702=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#91721))) "703=after tuple")
    ((stack (Value (Ident _#91721))) "705=after tuple")

     { "contents": [ { "value": "type t = int list", "language": "cameligo" } ] };((stack ((Ident _#92118))) "709=before tuple")
    ((stack ((Ident _#92118))) "708=before tuple")
    ((stack (Value (Ident _#92118))) "708=after tuple")
    ((stack (Value (Ident _#92118))) "707=before tuple")
    ((stack (Value (Ident _#92118)))
      "706=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#92118)))
      "706=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#92118))) "707=after tuple")
    ((stack (Value (Ident _#92118))) "709=after tuple")

     { "contents": [ { "value": "f : t -> int list", "language": "cameligo" } ] }] |}]

let%expect_test "Polymorphic types (JsLIGO)" =
  get_hover_test
    { file = "contracts/lsp/hover/poly_types.jsligo"
    ; hover_positions =
        [ pos ~line:0 ~character:5
        ; pos ~line:2 ~character:6
        ; pos ~line:4 ~character:9
        ; pos ~line:4 ~character:21
        ; pos ~line:4 ~character:24
        ; pos ~line:5 ~character:9
        ; pos ~line:6 ~character:15
        ; pos ~line:6 ~character:20
        ; pos ~line:6 ~character:22
        ; pos ~line:10 ~character:9
        ; pos ~line:10 ~character:34
        ; pos ~line:10 ~character:56
        ; pos ~line:18 ~character:9
        ; pos ~line:18 ~character:15
        ; pos ~line:22 ~character:6
        ; pos ~line:24 ~character:6
        ; pos ~line:26 ~character:6
        ; pos ~line:28 ~character:6
        ; pos ~line:34 ~character:6
        ; pos ~line:38 ~character:6
        ; pos ~line:40 ~character:6
        ; pos ~line:49 ~character:7
        ; pos ~line:52 ~character:5
        ; pos ~line:54 ~character:6
        ]
    };
  [%expect
    {|
    ((stack ((Ident _#92514))) "713=before tuple")
    ((stack ((Ident _#92514))) "712=before tuple")
    ((stack (Value (Ident _#92514))) "712=after tuple")
    ((stack (Value (Ident _#92514))) "711=before tuple")
    ((stack (Value (Ident _#92514)))
      "710=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#92514)))
      "710=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#92514))) "711=after tuple")
    ((stack (Value (Ident _#92514))) "713=after tuple")
    [{
       "contents": [
         {
           "value": "type func<a, b> = | [\"Func\", (x: a) => b]",
           "language": "jsligo"
         }
       ]
     };((stack ((Ident _#92910))) "717=before tuple")
    ((stack ((Ident _#92910))) "716=before tuple")
    ((stack (Value (Ident _#92910))) "716=after tuple")
    ((stack (Value (Ident _#92910))) "715=before tuple")
    ((stack (Value (Ident _#92910)))
      "714=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#92910)))
      "714=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#92910))) "715=after tuple")
    ((stack (Value (Ident _#92910))) "717=after tuple")

     {
       "contents": [ { "value": "incr : func<int, int>", "language": "jsligo" } ]
     };((stack ((Ident _#93306))) "721=before tuple")
    ((stack ((Ident _#93306))) "720=before tuple")
    ((stack (Value (Ident _#93306))) "720=after tuple")
    ((stack (Value (Ident _#93306))) "719=before tuple")
    ((stack (Value (Ident _#93306)))
      "718=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#93306)))
      "718=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#93306))) "719=after tuple")
    ((stack (Value (Ident _#93306))) "721=after tuple")

     {
       "contents": [
         {
           "value": "apply_func : <a, b>(f: func<a, b>, x: a) => b",
           "language": "jsligo"
         }
       ]
     };((stack ((Ident _#93702))) "725=before tuple")
    ((stack ((Ident _#93702))) "724=before tuple")
    ((stack (Value (Ident _#93702))) "724=after tuple")
    ((stack (Value (Ident _#93702))) "723=before tuple")
    ((stack (Value (Ident _#93702)))
      "722=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#93702)))
      "722=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#93702))) "723=after tuple")
    ((stack (Value (Ident _#93702))) "725=after tuple")
    ((stack ((Ident _#94098))) "729=before tuple")
    ((stack ((Ident _#94098))) "728=before tuple")
    ((stack (Value (Ident _#94098))) "728=after tuple")
    ((stack (Value (Ident _#94098))) "727=before tuple")
    ((stack (Value (Ident _#94098)))
      "726=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#94098)))
      "726=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#94098))) "727=after tuple")
    ((stack (Value (Ident _#94098))) "729=after tuple")
     { "contents": [ { "value": "f : func<a, b>", "language": "jsligo" } ] };
     { "contents": [ { "value": "x : a", "language": "jsligo" } ] };((stack ((Ident _#94494))) "733=before tuple")
    ((stack ((Ident _#94494))) "732=before tuple")
    ((stack (Value (Ident _#94494))) "732=after tuple")
    ((stack (Value (Ident _#94494))) "731=before tuple")
    ((stack (Value (Ident _#94494)))
      "730=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#94494)))
      "730=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#94494))) "731=after tuple")
    ((stack (Value (Ident _#94494))) "733=after tuple")

     { "contents": [ { "value": "f : func<a, b>", "language": "jsligo" } ] };((stack ((Ident _#94890))) "737=before tuple")
    ((stack ((Ident _#94890))) "736=before tuple")
    ((stack (Value (Ident _#94890))) "736=after tuple")
    ((stack (Value (Ident _#94890))) "735=before tuple")
    ((stack (Value (Ident _#94890)))
      "734=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#94890)))
      "734=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#94890))) "735=after tuple")
    ((stack (Value (Ident _#94890))) "737=after tuple")

     { "contents": [ { "value": "f : (x: a) => b", "language": "jsligo" } ] };((stack ((Ident _#95286))) "741=before tuple")
    ((stack ((Ident _#95286))) "740=before tuple")
    ((stack (Value (Ident _#95286))) "740=after tuple")
    ((stack (Value (Ident _#95286))) "739=before tuple")
    ((stack (Value (Ident _#95286)))
      "738=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#95286)))
      "738=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#95286))) "739=after tuple")
    ((stack (Value (Ident _#95286))) "741=after tuple")

     { "contents": [ { "value": "f : (x: a) => b", "language": "jsligo" } ] };((stack ((Ident _#95682))) "745=before tuple")
    ((stack ((Ident _#95682))) "744=before tuple")
    ((stack (Value (Ident _#95682))) "744=after tuple")
    ((stack (Value (Ident _#95682))) "743=before tuple")
    ((stack (Value (Ident _#95682)))
      "742=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#95682)))
      "742=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#95682))) "743=after tuple")
    ((stack (Value (Ident _#95682))) "745=after tuple")

     { "contents": [ { "value": "x : a", "language": "jsligo" } ] };((stack ((Ident _#96078))) "749=before tuple")
    ((stack ((Ident _#96078))) "748=before tuple")
    ((stack (Value (Ident _#96078))) "748=after tuple")
    ((stack (Value (Ident _#96078))) "747=before tuple")
    ((stack (Value (Ident _#96078)))
      "746=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#96078)))
      "746=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#96078))) "747=after tuple")
    ((stack (Value (Ident _#96078))) "749=after tuple")

     {
       "contents": [
         {
           "value": "apply_func2 :\n  <dom, codom>(f: func<dom, codom>, x: dom) => codom",
           "language": "jsligo"
         }
       ]
     };((stack ((Ident _#96474))) "753=before tuple")
    ((stack ((Ident _#96474))) "752=before tuple")
    ((stack (Value (Ident _#96474))) "752=after tuple")
    ((stack (Value (Ident _#96474))) "751=before tuple")
    ((stack (Value (Ident _#96474)))
      "750=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#96474)))
      "750=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#96474))) "751=after tuple")
    ((stack (Value (Ident _#96474))) "753=after tuple")

     {
       "contents": [ { "value": "f : func<dom, codom>", "language": "jsligo" } ]
     };((stack ((Ident _#96870))) "757=before tuple")
    ((stack ((Ident _#96870))) "756=before tuple")
    ((stack (Value (Ident _#96870))) "756=after tuple")
    ((stack (Value (Ident _#96870))) "755=before tuple")
    ((stack (Value (Ident _#96870)))
      "754=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#96870)))
      "754=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#96870))) "755=after tuple")
    ((stack (Value (Ident _#96870))) "757=after tuple")
    ((stack ((Ident _#97266))) "761=before tuple")
    ((stack ((Ident _#97266))) "760=before tuple")
    ((stack (Value (Ident _#97266))) "760=after tuple")
    ((stack (Value (Ident _#97266))) "759=before tuple")
    ((stack (Value (Ident _#97266)))
      "758=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#97266)))
      "758=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#97266))) "759=after tuple")
    ((stack (Value (Ident _#97266))) "761=after tuple")
     { "contents": [ { "value": "x : dom", "language": "jsligo" } ] };
     {
       "contents": [
         { "value": "f : <a>(r: t<a>) => t<a>", "language": "jsligo" }
       ]
     };((stack ((Ident _#97662))) "765=before tuple")
    ((stack ((Ident _#97662))) "764=before tuple")
    ((stack (Value (Ident _#97662))) "764=after tuple")
    ((stack (Value (Ident _#97662))) "763=before tuple")
    ((stack (Value (Ident _#97662)))
      "762=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#97662)))
      "762=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#97662))) "763=after tuple")
    ((stack (Value (Ident _#97662))) "765=after tuple")
    ((stack ((Ident _#98058))) "769=before tuple")
    ((stack ((Ident _#98058))) "768=before tuple")
    ((stack (Value (Ident _#98058))) "768=after tuple")
    ((stack (Value (Ident _#98058))) "767=before tuple")
    ((stack (Value (Ident _#98058)))
      "766=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#98058)))
      "766=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#98058))) "767=after tuple")
    ((stack (Value (Ident _#98058))) "769=after tuple")
     { "contents": [ { "value": "r : t<a>", "language": "jsligo" } ] };
     {
       "contents": [
         { "value": "ticket : option<ticket<nat>>", "language": "jsligo" }
       ]
     };((stack ((Ident _#98454))) "773=before tuple")
    ((stack ((Ident _#98454))) "772=before tuple")
    ((stack (Value (Ident _#98454))) "772=after tuple")
    ((stack (Value (Ident _#98454))) "771=before tuple")
    ((stack (Value (Ident _#98454)))
      "770=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#98454)))
      "770=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#98454))) "771=after tuple")
    ((stack (Value (Ident _#98454))) "773=after tuple")
    ((stack ((Ident _#98850))) "777=before tuple")
    ((stack ((Ident _#98850))) "776=before tuple")
    ((stack (Value (Ident _#98850))) "776=after tuple")
    ((stack (Value (Ident _#98850))) "775=before tuple")
    ((stack (Value (Ident _#98850)))
      "774=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#98850)))
      "774=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#98850))) "775=after tuple")
    ((stack (Value (Ident _#98850))) "777=after tuple")
     { "contents": [ { "value": "lst : list<int>", "language": "jsligo" } ] };
     { "contents": [ { "value": "opt : option<int>", "language": "jsligo" } ] };((stack ((Ident _#99246))) "781=before tuple")
    ((stack ((Ident _#99246))) "780=before tuple")
    ((stack (Value (Ident _#99246))) "780=after tuple")
    ((stack (Value (Ident _#99246))) "779=before tuple")
    ((stack (Value (Ident _#99246)))
      "778=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#99246)))
      "778=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#99246))) "779=after tuple")
    ((stack (Value (Ident _#99246))) "781=after tuple")

     {
       "contents": [ { "value": "big_map : t<int, nat>", "language": "jsligo" } ]
     };((stack ((Ident _#99642))) "785=before tuple")
    ((stack ((Ident _#99642))) "784=before tuple")
    ((stack (Value (Ident _#99642))) "784=after tuple")
    ((stack (Value (Ident _#99642))) "783=before tuple")
    ((stack (Value (Ident _#99642)))
      "782=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#99642)))
      "782=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#99642))) "783=after tuple")
    ((stack (Value (Ident _#99642))) "785=after tuple")
    ((stack ((Ident _#100038))) "789=before tuple")
    ((stack ((Ident _#100038))) "788=before tuple")
    ((stack (Value (Ident _#100038))) "788=after tuple")
    ((stack (Value (Ident _#100038))) "787=before tuple")
    ((stack (Value (Ident _#100038)))
      "786=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#100038)))
      "786=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#100038))) "787=after tuple")
    ((stack (Value (Ident _#100038))) "789=after tuple")
     { "contents": [ { "value": "x : M.t<string>", "language": "jsligo" } ] };
     {
       "contents": [ { "value": "foo : Common.foo<int>", "language": "jsligo" } ]
     };((stack ((Ident _#100434))) "793=before tuple")
    ((stack ((Ident _#100434))) "792=before tuple")
    ((stack (Value (Ident _#100434))) "792=after tuple")
    ((stack (Value (Ident _#100434))) "791=before tuple")
    ((stack (Value (Ident _#100434)))
      "790=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#100434)))
      "790=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#100434))) "791=after tuple")
    ((stack (Value (Ident _#100434))) "793=after tuple")

     {
       "contents": [
         {
           "value": "ok_result : Common.result<int, string>",
           "language": "jsligo"
         }
       ]
     };((stack ((Ident _#100830))) "797=before tuple")
    ((stack ((Ident _#100830))) "796=before tuple")
    ((stack (Value (Ident _#100830))) "796=after tuple")
    ((stack (Value (Ident _#100830))) "795=before tuple")
    ((stack (Value (Ident _#100830)))
      "794=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#100830)))
      "794=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#100830))) "795=after tuple")
    ((stack (Value (Ident _#100830))) "797=after tuple")

     {
       "contents": [
         { "value": "type b<a> = | [\"B\", X.x<a>]", "language": "jsligo" }
       ]
     };((stack ((Ident _#101226))) "801=before tuple")
    ((stack ((Ident _#101226))) "800=before tuple")
    ((stack (Value (Ident _#101226))) "800=after tuple")
    ((stack (Value (Ident _#101226))) "799=before tuple")
    ((stack (Value (Ident _#101226)))
      "798=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#101226)))
      "798=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#101226))) "799=after tuple")
    ((stack (Value (Ident _#101226))) "801=after tuple")

     { "contents": [ { "value": "type t = list<int>", "language": "jsligo" } ] };((stack ((Ident _#101622))) "805=before tuple")
    ((stack ((Ident _#101622))) "804=before tuple")
    ((stack (Value (Ident _#101622))) "804=after tuple")
    ((stack (Value (Ident _#101622))) "803=before tuple")
    ((stack (Value (Ident _#101622)))
      "802=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#101622)))
      "802=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#101622))) "803=after tuple")
    ((stack (Value (Ident _#101622))) "805=after tuple")

     {
       "contents": [ { "value": "f3 : (x: t) => t<int>", "language": "jsligo" } ]
     }] |}]

let%expect_test "Recover from missing variable" =
  get_hover_test
    { file = "contracts/lsp/hover/recover_missing_variable.mligo"
    ; hover_positions = [ pos ~line:0 ~character:4; pos ~line:1 ~character:4 ]
    };
  [%expect
    {|
    [{ "contents": [ { "value": "g : ^a", "language": "cameligo" } ] };
     { "contents": [ { "value": "h : ^a", "language": "cameligo" } ] }] |}]

let%expect_test "Recover from missing module" =
  get_hover_test
    { file = "contracts/lsp/hover/recover_missing_module.mligo"
    ; hover_positions = [ pos ~line:0 ~character:4 ]
    };
  [%expect {|
    [{ "contents": [ { "value": "a : int", "language": "cameligo" } ] }] |}]

let%expect_test "Recover from missing record field" =
  get_hover_test
    { file = "contracts/lsp/hover/recover_missing_record_field.mligo"
    ; hover_positions = [ pos ~line:0 ~character:4 ]
    };
  [%expect {|
    [{ "contents": [ { "value": "a : ^a", "language": "cameligo" } ] }] |}]

let%expect_test "Recover from type error" =
  get_hover_test
    { file = "contracts/lsp/hover/recover_type_error_1.mligo"
    ; hover_positions =
        [ pos ~line:1 ~character:6; pos ~line:2 ~character:6; pos ~line:5 ~character:4 ]
    };
  [%expect
    {|
    [{ "contents": [ { "value": "x : int", "language": "cameligo" } ] };
     { "contents": [ { "value": "y : int", "language": "cameligo" } ] };
     { "contents": [ { "value": "y : int", "language": "cameligo" } ] }] |}]

let%expect_test "Recover from type error 2" =
  get_hover_test
    { file = "contracts/lsp/hover/recover_type_error_2.mligo"
    ; hover_positions =
        [ pos ~line:0 ~character:4; pos ~line:1 ~character:4; pos ~line:2 ~character:4 ]
    };
  [%expect
    {|
    [{ "contents": [ { "value": "a : string", "language": "cameligo" } ] };
     {
       "contents": [
         { "value": "f : string -> string", "language": "cameligo" }
       ]
     }; { "contents": [ { "value": "g : string", "language": "cameligo" } ] }] |}]

let%expect_test "Recover from type error 3" =
  get_hover_test
    { file = "contracts/lsp/hover/recover_type_error_3.mligo"
    ; hover_positions = [ pos ~line:0 ~character:4; pos ~line:1 ~character:4 ]
    };
  [%expect
    {|
    ((stack ((Ident _#105876))) "809=before tuple")
    ((stack ((Ident _#105876))) "808=before tuple")
    ((stack (Value (Ident _#105876))) "808=after tuple")
    ((stack (Value (Ident _#105876))) "807=before tuple")
    ((stack (Value (Ident _#105876)))
      "806=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#105876)))
      "806=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#105876))) "807=after tuple")
    ((stack (Value (Ident _#105876))) "809=after tuple")
    [{
       "contents": [ { "value": "g : 'a 'b.'a -> 'b", "language": "cameligo" } ]
     };((stack ((Ident _#106264))) "813=before tuple")
    ((stack ((Ident _#106264))) "812=before tuple")
    ((stack (Value (Ident _#106264))) "812=after tuple")
    ((stack (Value (Ident _#106264))) "811=before tuple")
    ((stack (Value (Ident _#106264)))
      "810=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#106264)))
      "810=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#106264))) "811=after tuple")
    ((stack (Value (Ident _#106264))) "813=after tuple")

     {
       "contents": [ { "value": "h : 'a 'b.'a -> 'b", "language": "cameligo" } ]
     }] |}]
