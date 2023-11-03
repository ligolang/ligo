open Alcotest_extras
open Handlers
open Lsp_helpers
module Requests = Ligo_lsp.Server.Requests
open Requests.Handler

type hover_type =
  | List of string list
  | MarkedString of string
  | MarkupContent of string

let one elt = List [ elt ]

type hover_test =
  { test_name : string
  ; file : string
  ; hovers : (Position.t * hover_type) list
        (* Each element of a list is position and a hover message that should appear
           when mouse is on that position.
           This was introduced to make test output for multiple cases for one file more compact. *)
  }

let get_hover_test ({ test_name; file; hovers } : hover_test) : unit Alcotest.test_case =
  Alcotest.test_case test_name `Quick
  @@ fun () ->
  let test_hover_for_position (position, expected_hover) =
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
    let syntax =
      Option.value_exn ~message:"Expected a file with LIGO syntax"
      @@ Option.map ~f:Syntax.to_string
      @@ Path.get_syntax path
    in
    let mk_marked_string value = MarkedString.{ language = Some syntax; value } in
    let mk_markup_content value =
      let value = Format.asprintf "```%s\n%s\n```" syntax value in
      MarkupContent.{ kind = Markdown; value }
    in
    match actual_hover with
    | None -> fail @@ "Expected a hover message, got none.\n" ^ test_info
    | Some
        (* hover message is much more important than hover range so we're checking just messages *)
        { range = _; contents } ->
      (match contents, expected_hover with
      | `List actual_hovers, List expected_hovers ->
        let expected_hovers = List.map ~f:mk_marked_string expected_hovers in
        let msg = "List hovers mismatch.\n" ^ test_info in
        check (Alcotest.list MarkedString.testable) msg expected_hovers actual_hovers
      | `MarkedString actual_hover, MarkedString expected_hover ->
        let msg = "Marked string hover mismatch.\n" ^ test_info in
        check MarkedString.testable msg (mk_marked_string expected_hover) actual_hover
      | `MarkupContent actual_hover, MarkupContent expected_hover ->
        let msg = "Hover message mismatch.\n" ^ test_info in
        check MarkupContent.testable msg (mk_markup_content expected_hover) actual_hover
      | _ -> fail @@ "Hover types mismatch.\n" ^ test_info)
  in
  List.iter hovers ~f:test_hover_for_position


(* TODO after resolving some issues new hovers tests should be added:
   - #1959 add tests for typer error recovery (introduced in !2713)
   - #1676 add tests for hovers on constructors and record fields
   - #1965 add tests for e.g. `compose_endo` from `hovers.mligo`
*)
(* TODO JsLIGO tests *)
let test_cases =
  let pos = Position.create in
  [ { test_name = "simple.mligo"
    ; file = "contracts/lsp/simple.mligo"
    ; hovers =
        [ pos ~line:0 ~character:4, one "x : int"
        ; pos ~line:0 ~character:5, one "x : int"
        ; pos ~line:1 ~character:8, one "x : int"
        ; pos ~line:1 ~character:9, one "x : int"
        ; pos ~line:1 ~character:4, one "y : int"
        ; pos ~line:1 ~character:5, one "y : int"
        ]
    }
  ; { test_name = "registry.jsligo"
    ; file = "contracts/lsp/registry.jsligo"
    ; hovers =
        [ pos ~line:11 ~character:19, one "get_exn : (_: list<a>) => (_: int) => a"
        ; ( pos ~line:26 ~character:10
          , one "map : (_: (_: a) => b) => (_: list<a>) => list<b>" )
        ; pos ~line:28 ~character:31, one "primes : list<int>"
        ; pos ~line:39 ~character:28, one "store : storage"
        ; pos ~line:40 ~character:50, one "store : storage"
        ; pos ~line:39 ~character:40, one "type storage = list<int>"
        ; pos ~line:39 ~character:55, one "type return_ = [list<operation>, list<int>]"
        ]
    }
  ; { test_name = "hovers.mligo"
    ; file = "contracts/lsp/hovers.mligo"
    ; hovers =
        [ pos ~line:0 ~character:8, one "type 'a endo = Endo of ('a -> 'a)"
        ; pos ~line:4 ~character:61, one "type 'a endo = Endo of ('a -> 'a)"
        ; ( pos ~line:4 ~character:20
          , one "compose_endo_with_type_annotation :\n  a endo -> a endo -> a endo" )
        ; pos ~line:5 ~character:12, one "f : a -> a"
        ; pos ~line:5 ~character:37, one "x : a"
        ; pos ~line:5 ~character:47, one "x : a"
        ; pos ~line:9 ~character:31, one "f : int -> int"
        ; pos ~line:9 ~character:70, one "x : int"
        ; pos ~line:21 ~character:17, one "f1 : a -> b"
        ; pos ~line:40 ~character:18, one "f : int -> int"
        ; pos ~line:56 ~character:12, one "x : t"
        ; pos ~line:58 ~character:28, one "f : a -> a"
        ; pos ~line:59 ~character:15, one "map : (a -> b) -> a list -> b list"
        ; pos ~line:65 ~character:41, one "type 'a list2 = 'a list list"
        ; pos ~line:70 ~character:5, one "x1 : int list list -> int list list list"
        ; pos ~line:72 ~character:23, one "endo_list2 : a endo -> a list2 endo"
        ; ( pos ~line:75 ~character:4
          , one "z : key_hash option -> tez -> int -> (operation * address)" )
        ; pos ~line:75 ~character:35, one "x : int"
        ; ( pos ~line:75 ~character:27
          , one
              "create_contract :\n\
              \  (p -> s -> (operation list * s)) ->\n\
              \  key_hash option -> tez -> s -> (operation * address)" )
        ; ( pos ~line:77 ~character:28
          , one "type 'v proxy_address =\n  ('v * nat * address, unit) typed_address" )
        ; pos ~line:79 ~character:8, one "type 'v p = 'v proxy_address"
        ; pos ~line:83 ~character:20, one "type int_endo = IntEndo of (int -> int)"
        ]
        @ List.map
            ~f:(fun p ->
              ( p
              , one "type ('a, 'b) iso =\n  {\n   from : 'a -> 'b;\n   to : 'b -> 'a\n  }"
              ))
            [ pos ~line:17 ~character:61
            ; pos ~line:11 ~character:15
            ; pos ~line:46 ~character:55
            ]
    }
  ; { test_name = "C.mligo"
    ; file = "contracts/lsp/hover/imports/C.mligo"
    ; hovers =
        [ pos ~line:3 ~character:11, one "#import \"B.mligo\" \"M\""
        ; pos ~line:3 ~character:13, one "#import \"A.mligo\" \"C\""
        ; pos ~line:5 ~character:11, one "#import \"A.mligo\" \"K\""
        ]
        @ List.map
            ~f:(fun p -> p, one "module \"B.mligo\" = struct (* ... *) end")
            [ pos ~line:0 ~character:11
            ; pos ~line:0 ~character:19
            ; pos ~line:1 ~character:11
            ; pos ~line:1 ~character:19 (* [#2090] The last 2 hoverings are incorrect *)
            ]
    }
  ; { test_name = "outer.mligo"
    ; file = "contracts/lsp/hover/imports/outer.mligo"
    ; hovers =
        [ pos ~line:2 ~character:12, one "#import \"inner/inner.mligo\" \"Inner\""
        ; pos ~line:2 ~character:20, one "#import \"C.mligo\" \"Outer\""
        ; pos ~line:2 ~character:23, one "#import \"A.mligo\" \"K\""
        ]
        @ List.map
            ~f:(fun p -> p, one "module \"inner/inner.mligo\" = struct (* ... *) end")
            [ pos ~line:0 ~character:17; pos ~line:0 ~character:31 ]
    }
  ; { test_name = "inner.mligo"
    ; file = "contracts/lsp/hover/imports/inner/inner.mligo"
    ; hovers =
        [ pos ~line:2 ~character:12, one "#import \"../C.mligo\" \"Outer\""
        ; pos ~line:2 ~character:17, one "#import \"../A.mligo\" \"K\""
        ]
        @ List.map
            ~f:(fun p -> p, one "module \"../C.mligo\" = struct (* ... *) end")
            [ pos ~line:0 ~character:15; pos ~line:0 ~character:23 ]
    }
  ]


let tests = "hover", List.map ~f:get_hover_test test_cases
