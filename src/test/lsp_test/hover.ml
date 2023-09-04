open Alcotest_extras
open Handlers
open Lsp_helpers
module Requests = Ligo_lsp.Server.Requests
open Requests.Handler

type hover_test =
  { test_name : string
  ; file : string
  ; hovers : (Position.t * string) list
        (* Each element of a list is position and a hover message that should appear
           when mouse is on that position.
           This was introduced to make test output for multiple cases for one file more compact. *)
  }

let get_hover_test ({ test_name; file; hovers } : hover_test) : unit Alcotest.test_case =
  Alcotest.test_case test_name `Quick
  @@ fun () ->
  let test_hover_for_position (position, expected_message) =
    let path = Path.from_relative file in
    let actual_hover, diagnostics =
      test_run_session
      @@ let@ uri = open_file path in
         Requests.on_req_hover position uri
    in
    let test_info =
      Format.asprintf
        "Hover request for: %s, %a.\nDiagnostics for this test: %a"
        file
        Position.pp
        position
        Fmt.Dump.(list (pair Path.pp (list Diagnostic.pp)))
        (Path_hashtbl.to_alist diagnostics)
    in
    match actual_hover with
    | None -> fail @@ "Expected a hover message, got none.\n" ^ test_info
    | Some
        (* hover message is much more important than hover range so we're checking just messages *)
        { range = _; contents } ->
      (match contents with
      | `List _ | `MarkedString _ ->
        fail @@ "Expected a MarkupContent in hover contents.\n" ^ test_info
      | `MarkupContent { kind; value = actual } ->
        if Caml.( != ) kind Markdown
        then fail @@ "Expected a Markdown kind in hover contents.\n" ^ test_info;
        let syntax =
          Option.value_exn ~message:"Expected a file with LIGO syntax"
          @@ Path.get_syntax path
        in
        let expected =
          Format.asprintf "```%s\n%s\n```" (Syntax.to_string syntax) expected_message
        in
        let msg = "Hover message mismatch.\n" ^ test_info in
        check Alcotest.string msg expected actual)
  in
  List.iter hovers ~f:test_hover_for_position


(* TODO after resolving some issues new hovers tests should be added:
   - #1748 add tests for modules
   - #1959 add tests for typer error recovery (introduced in !2713)
   - #1676 add tests for hovers on constructors and record fields
   - #1965 add tests for e.g. `compose_endo` from `hovers.mligo`
*)
let test_cases =
  let pos = Position.create in
  [ { test_name = "simple.mligo"
    ; file = "contracts/lsp/simple.mligo"
    ; hovers =
        [ pos ~line:0 ~character:4, "x : int"
        ; pos ~line:0 ~character:5, "x : int"
        ; pos ~line:1 ~character:8, "x : int"
        ; pos ~line:1 ~character:9, "x : int"
        ; pos ~line:1 ~character:4, "y : int"
        ; pos ~line:1 ~character:5, "y : int"
        ]
    }
    (* FIXME #2024 *)
    (* ; { test_name = "registry.jsligo"
    ; file = "contracts/lsp/registry.jsligo"
    ; hovers =
        [ pos ~line:11 ~character:19, "get_exn : (_: list<a>) => (_: int) => a"
        ; pos ~line:26 ~character:10, "map : (_: (_: a) => b) => (_: list<a>) => list<b>"
        ; pos ~line:28 ~character:31, "primes : list<int>"
        ; pos ~line:39 ~character:28, "store : storage"
        ; pos ~line:40 ~character:50, "store : storage"
        ; pos ~line:39 ~character:40, "type storage = list<int>"
        ; pos ~line:39 ~character:55, "type return_ = [list<operation>, list<int>]"
        ]
    } *)
  ; { test_name = "hovers.mligo"
    ; file = "contracts/lsp/hovers.mligo"
    ; hovers =
        [ pos ~line:0 ~character:8, "type 'a endo = Endo of ('a -> 'a)"
        ; pos ~line:4 ~character:61, "type 'a endo = Endo of ('a -> 'a)"
        ; ( pos ~line:4 ~character:20
          , "compose_endo_with_type_annotation :\n  a endo -> a endo -> a endo" )
        ; pos ~line:5 ~character:12, "f : a -> a"
        ; pos ~line:5 ~character:37, "x : a"
        ; pos ~line:5 ~character:47, "x : a"
        ; pos ~line:9 ~character:31, "f : int -> int"
        ; pos ~line:9 ~character:70, "x : int"
        ; pos ~line:21 ~character:17, "f1 : a -> b"
        ; pos ~line:40 ~character:18, "f : int -> int"
        ; pos ~line:56 ~character:12, "x : t"
        ; pos ~line:58 ~character:28, "f : a -> a"
        ; pos ~line:59 ~character:15, "map : (a -> b) -> a list -> b list"
        ; pos ~line:65 ~character:41, "type 'a list2 = 'a list list"
        ; pos ~line:70 ~character:5, "x1 : int list list -> int list list list"
        ; pos ~line:72 ~character:23, "endo_list2 : a endo -> a list2 endo"
        ; ( pos ~line:75 ~character:4
          , "z : key_hash option -> tez -> int -> (operation * address)" )
        ; pos ~line:75 ~character:35, "x : int"
        ; ( pos ~line:75 ~character:27
          , "create_contract :\n\
            \  (p -> s -> (operation list * s)) ->\n\
            \  key_hash option -> tez -> s -> (operation * address)" )
        ; ( pos ~line:77 ~character:28
          , "type 'v proxy_address =\n  ('v * nat * address, unit) typed_address" )
        ; pos ~line:79 ~character:8, "type 'v p = 'v proxy_address"
        ]
        @ List.map
            ~f:(fun p ->
              p, "type ('a, 'b) iso =\n  {\n   from : 'a -> 'b;\n   to : 'b -> 'a\n  }")
            [ pos ~line:17 ~character:61
            ; pos ~line:11 ~character:15
            ; pos ~line:46 ~character:55
            ]
    }
  ]


let tests = "hover", List.map ~f:get_hover_test test_cases
