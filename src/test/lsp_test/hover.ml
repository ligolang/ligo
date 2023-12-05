open Alcotest_extras
open Handlers
open Lsp_helpers
module Requests = Ligo_lsp.Server.Requests
open Requests.Handler

(* when we send code, we wrap it in "```cameligo ... ```" or in "```jsligo ... ```"
   to have a syntax highlight,
   but when we show a documentation to user, we show it just as a markdown *)
type language =
  | Ligo
  | Markdown

type hover_type =
  | List of (string * language) list
  | MarkedString of string
  | MarkupContent of string

let one elt = List [ elt, Ligo ]

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
    let mk_marked_string language value =
      match language with
      | Ligo -> MarkedString.{ language = Some syntax; value }
      | Markdown -> MarkedString.{ language = None; value }
    in
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
        let expected_hovers =
          List.map ~f:(fun (lang, s) -> mk_marked_string s lang) expected_hovers
        in
        let msg = "List hovers mismatch.\n" ^ test_info in
        check (Alcotest.list MarkedString.testable) msg expected_hovers actual_hovers
      | `MarkedString actual_hover, MarkedString expected_hover ->
        let msg = "Marked string hover mismatch.\n" ^ test_info in
        check
          MarkedString.testable
          msg
          (mk_marked_string Ligo expected_hover)
          actual_hover
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
        [ pos ~line:11 ~character:19, one "get_exn : <a>(_: list<a>) => (_: int) => a"
        ; ( pos ~line:26 ~character:10
          , one "map : <a, b>(_: (_: a) => b) => (_: list<a>) => list<b>" )
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
          , one "compose_endo_with_type_annotation :\n  'a.'a endo -> 'a endo -> 'a endo"
          )
        ; pos ~line:5 ~character:12, one "f : a -> a"
        ; pos ~line:5 ~character:37, one "x : a"
        ; pos ~line:5 ~character:47, one "x : a"
        ; pos ~line:9 ~character:31, one "f : int -> int"
        ; pos ~line:9 ~character:70, one "x : int"
        ; pos ~line:21 ~character:17, one "f1 : a -> b"
        ; pos ~line:40 ~character:18, one "f : int -> int"
        ; pos ~line:56 ~character:12, one "x : t"
        ; pos ~line:58 ~character:28, one "f : a -> a"
        ; pos ~line:59 ~character:15, one "map : 'a 'b.('a -> 'b) -> 'a list -> 'b list"
        ; pos ~line:65 ~character:41, one "type 'a list2 = 'a list list"
        ; pos ~line:70 ~character:5, one "x1 : int list list -> int list list list"
        ; pos ~line:72 ~character:23, one "endo_list2 : 'a.'a endo -> 'a list2 endo"
        ; ( pos ~line:75 ~character:4
          , one "z : key_hash option -> tez -> int -> (operation * address)" )
        ; pos ~line:75 ~character:35, one "x : int"
        ; ( pos ~line:75 ~character:27
          , one
              "create_contract :\n\
              \  'p\n\
              \  's.('p -> 's -> (operation list * 's)) ->\n\
              \  key_hash option -> tez -> 's -> (operation * address)" )
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
            ~f:(fun p -> p, one "module \"B.mligo\" : sig end")
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
            ~f:(fun p ->
              p, one "module \"inner/inner.mligo\" : sig\n  val test : int\n  end")
            [ pos ~line:0 ~character:17; pos ~line:0 ~character:31 ]
    }
  ; { test_name = "inner.mligo"
    ; file = "contracts/lsp/hover/imports/inner/inner.mligo"
    ; hovers =
        [ pos ~line:2 ~character:12, one "#import \"../C.mligo\" \"Outer\""
        ; pos ~line:2 ~character:17, one "#import \"../A.mligo\" \"K\""
        ]
        @ List.map
            ~f:(fun p -> p, one "module \"../C.mligo\" : sig\n  val test : int\n  end")
            [ pos ~line:0 ~character:15; pos ~line:0 ~character:23 ]
    }
  ; { test_name = "hover_module.mligo"
    ; file = "contracts/lsp/hover/hover_module.mligo"
    ; hovers =
        [ ( pos ~line:0 ~character:7
          , one "module A : sig\n  val foo : int\n\n  val bar : int\n  end" )
        ; ( pos ~line:10 ~character:7
          , one "module B : sig\n  type t =  nat\n\n  type int =  string\n  end" )
        ; ( pos ~line:17 ~character:7
          , one "module C : sig\n  val another : int\n\n  val foo : tez\n  end" )
        ; ( pos ~line:33 ~character:24
          , one
              "module Bytes : sig\n\
              \  val concats : bytes list -> bytes\n\n\
              \  val pack : 'a.'a -> bytes\n\n\
              \  val unpack : 'a.bytes -> 'a option\n\n\
              \  val length : bytes -> nat\n\n\
              \  val concat : bytes -> bytes -> bytes\n\n\
              \  val sub : nat -> nat -> bytes -> bytes\n\
              \  end" )
        ; pos ~line:43 ~character:13, one "module Mangled : (* Unresolved *)"
        ; ( pos ~line:48 ~character:12
          , one "module Mangled_with_sig : sig\n  type t\n\n  type int =  string\n  end" )
        ; ( pos ~line:54 ~character:10
          , one "module Mangled_with_inlined_sig : sig\n  val foo : int\n  end" )
        ; ( pos ~line:70 ~character:20
          , one
              "module With_included : sig\n\
              \  type t\n\n\
              \  type int =  string\n\n\
              \  val b : bool\n\n\
              \  val z : string\n\
              \  end" )
        ; ( pos ~line:77 ~character:14
          , one
              "module With_included : sig\n\
              \  type t =  int\n\n\
              \  type int =  string\n\n\
              \  val b : bool\n\
              \  end" )
        ]
        @ List.map
            ~f:(fun p ->
              p, one "module Outer : sig\n  val outer_foo : int -> int -> int\n  end")
            [ pos ~line:25 ~character:9; pos ~line:41 ~character:21 ]
        @ List.map
            ~f:(fun p ->
              p, one "module Inner : sig\n  val inner_foo : int -> int -> int\n  end")
            [ pos ~line:28 ~character:11; pos ~line:41 ~character:25 ]
        @ List.map
            ~f:(fun p -> p, one "module Bytes : sig\n  val overwritten : string\n  end")
            [ pos ~line:35 ~character:8; pos ~line:39 ~character:35 ]
        @ List.map
            ~f:(fun p -> p, one "module M : sig\n  val v : int\n  end")
            [ pos ~line:62 ~character:9; pos ~line:64 ~character:9 ]
        @ List.map
            ~f:(fun p -> p, one "module T : sig\n  type t\n\n  type int =  string\n  end")
            [ pos ~line:5 ~character:12
            ; pos ~line:10 ~character:11
            ; pos ~line:48 ~character:26
            ; pos ~line:71 ~character:10
            ]
        @ List.map
            ~f:(fun p -> p, one "module I : sig\n  val b : bool\n  end")
            [ pos ~line:66 ~character:12; pos ~line:72 ~character:10 ]
    }
  ; { test_name = "hover_module.jsligo"
    ; file = "contracts/lsp/hover/hover_module.jsligo"
    ; hovers =
        [ ( pos ~line:0 ~character:10
          , one "namespace A implements {\n  const foo: int;\n  const bar: int\n}" )
        ; ( pos ~line:10 ~character:10
          , one
              "namespace B implements {\n\
              \  type t = nat;\n\
              \  type int = string;\n\
              \  const b: t\n\
               }" )
        ; ( pos ~line:17 ~character:10
          , one "namespace C implements {\n  const foo: tez;\n  const another: int\n}" )
        ; ( pos ~line:33 ~character:27
          , one
              "namespace Bytes implements {\n\
              \  const concats: (_: list<bytes>) => bytes;\n\
              \  const pack: <a>(_: a) => bytes;\n\
              \  const unpack: <a>(_: bytes) => option<a>;\n\
              \  const length: (_: bytes) => nat;\n\
              \  const concat: (_: bytes) => (_: bytes) => bytes;\n\
              \  const sub: (_: nat) => (_: nat) => (_: bytes) => bytes\n\
               }" )
        ; pos ~line:43 ~character:11, one "namespace Mangled implements /* Unresolved */"
        ; ( pos ~line:48 ~character:13
          , one
              "namespace Mangled_with_sig implements {\n  type t;\n  type int = string\n}"
          )
        ; ( pos ~line:54 ~character:26
          , one "namespace Mangled_with_inlined_sig implements {\n  const foo: int\n}" )
        ; ( pos ~line:72 ~character:17
          , one
              "namespace With_included implements {\n\
              \  type t;\n\
              \  type int = string;\n\
              \  const b: bool;\n\
              \  const z: string\n\
               }" )
        ; ( pos ~line:76 ~character:22
          , one
              "namespace With_included implements {\n\
              \  type t = int;\n\
              \  type int = string;\n\
              \  const b: bool\n\
               }" )
        ]
        @ List.map
            ~f:(fun p ->
              ( p
              , one
                  "namespace Outer implements {\n\
                  \  const outer_foo: (_: int) => (_: int) => int\n\
                   }" ))
            [ pos ~line:25 ~character:11; pos ~line:41 ~character:21 ]
        @ List.map
            ~f:(fun p ->
              ( p
              , one
                  "namespace Inner implements {\n\
                  \  const inner_foo: (_: int) => (_: int) => int\n\
                   }" ))
            [ pos ~line:28 ~character:20; pos ~line:41 ~character:26 ]
        @ List.map
            ~f:(fun p ->
              p, one "namespace Bytes implements {\n  const overwritten: string\n}")
            [ pos ~line:35 ~character:13; pos ~line:39 ~character:35 ]
        @ List.map
            ~f:(fun p -> p, one "namespace M implements {\n  const v: int\n}")
            [ pos ~line:62 ~character:12; pos ~line:65 ~character:9 ]
        @ List.map
            ~f:(fun p ->
              p, one "namespace T implements {\n  type t;\n  type int = string\n}")
            [ pos ~line:5 ~character:10
            ; pos ~line:10 ~character:23
            ; pos ~line:48 ~character:38
            ]
        @ List.map
            ~f:(fun p -> p, one "namespace I implements {\n  const b: bool\n}")
            [ pos ~line:68 ~character:10
            ; pos ~line:72 ~character:35
            ; pos ~line:76 ~character:38
            ]
    }
  ; { test_name = "doc_comments.mligo"
    ; file = "contracts/lsp/hover/doc_comments.mligo"
    ; hovers =
        (let hover_for_module_type_X =
           List
             [ (* FIXME I'm a module type not a module *)
               ( "module X : sig\n\
                 \  [@view]\n\
                 \  val y : int -> int\n\n\
                 \  type t\n\n\
                 \  val p : t option\n\
                 \  end"
               , Ligo )
             ; "MODULE SIG", Markdown
             ]
         and hover_for_term_x =
           List
             [ "x : int t", Ligo
             ; ( "JUST A TERM\n\n\
                 \  with some doc\n\
                 \  in **several** lines\n\n\
                 \  one ~~more~~ `line`"
               , Markdown )
             ]
         in
         [ pos ~line:1 ~character:12, hover_for_module_type_X
         ; pos ~line:15 ~character:11, hover_for_module_type_X
         ; pos ~line:5 ~character:6, List [ "y : int -> int", Ligo; "SIG ITEM", Markdown ]
         ; pos ~line:8 ~character:7, List [ "type t", Ligo; "SIG TYPE", Markdown ]
         ; pos ~line:11 ~character:6, List [ "p : t option", Ligo; "SIG ITEM", Markdown ]
         ; ( pos ~line:15 ~character:7
           , List
               [ ( "module M : sig\n\
                   \  type t =  {foo : nat}\n\n\
                   \  val p : t option\n\n\
                   \  [@view]\n\
                   \  val y : int -> int\n\
                   \  end"
                 , Ligo )
               ; "MODULE", Markdown
               ] )
         ; ( pos ~line:19 ~character:6
           , List [ "y : int -> int", Ligo; "TERM IN MODULE", Markdown ] )
         ; ( pos ~line:22 ~character:7
           , List [ "type t = {foo : nat}", Ligo; "TYPE IN MODULE", Markdown ] )
         ; ( pos ~line:25 ~character:6
           , List [ "p : t option", Ligo; "TERM IN MODULE", Markdown ] )
         ; ( pos ~line:29 ~character:8
           , List [ "type 'a t = 'a list", Ligo; "JUST A TYPE", Markdown ] )
         ; pos ~line:38 ~character:4, hover_for_term_x
         ; pos ~line:40 ~character:8, hover_for_term_x
         ; ( pos ~line:43 ~character:7
           , List
               [ ( "module M1 : sig\n\
                   \  [@entry]\n\
                   \  val y : int -> int -> (operation list * int)\n\
                   \  end"
                 , Ligo )
               ; "MODULE WITH ENTRY POINT", Markdown
               ] )
         ; ( pos ~line:49 ~character:6
           , List
               [ "y : int -> int -> (operation list * int)", Ligo
               ; "BEFORE DECORATOR", Markdown
               ; "AFTER DECORATOR", Markdown
               ; "ENTRY POINT TERM", Markdown
               ] )
         ; ( pos ~line:52 ~character:9
           , List
               [ "module C : sig\n  val f : int -> int\n  end", Ligo
               ; "NESTED MODULE", Markdown
               ] )
         ; ( pos ~line:54 ~character:8
           , List [ "f : int -> int", Ligo; "NESTED MODULE TERM", Markdown ] )
         ; ( pos ~line:59 ~character:4
           , List [ "t : int list", Ligo; "Has type with comment inside", Markdown ] )
         ])
    }
  ; { test_name = "doc_comments.jsligo"
    ; file = "contracts/lsp/hover/doc_comments.jsligo"
    ; hovers =
        (let hover_for_namespace_X =
           List
             [ (* FIXME I'm an interface, not a namespace *)
               ( "namespace X implements {\n\
                 \  @view\n\
                 \  const y: (_: int) => int;\n\
                 \  type t;\n\
                 \  const p: option<t>\n\
                  }"
               , Ligo )
             ; "INTERFACE", Markdown
             ]
         and hover_for_term_x =
           List
             [ "x : t<int>", Ligo
             ; ( "JUST A TERM\nwith some doc\nin **several** lines\n\none ~~more~~ `line`"
               , Markdown )
             ]
         in
         [ pos ~line:1 ~character:10, hover_for_namespace_X
         ; pos ~line:14 ~character:23, hover_for_namespace_X
         ; ( pos ~line:5 ~character:8
           , List [ "y : (_: int) => int", Ligo; "INTERFACE ITEM", Markdown ] )
         ; pos ~line:8 ~character:7, List [ "type t", Ligo; "INTERFACE TYPE", Markdown ]
         ; ( pos ~line:10 ~character:8
           , List [ "p : option<t>", Ligo; "INTERFACE ITEM", Markdown ] )
         ; ( pos ~line:14 ~character:10
           , List
               [ ( "namespace M implements {\n\
                   \  @view\n\
                   \  const y: (_: int) => int;\n\
                   \  type t = { foo: nat };\n\
                   \  const p: option<t>\n\
                    }"
                 , Ligo )
               ; "NAMESPACE", Markdown
               ] )
         ; ( pos ~line:18 ~character:15
           , List [ "y : (_: int) => int", Ligo; "TERM IN NAMESPACE", Markdown ] )
         ; ( pos ~line:20 ~character:14
           , List [ "type t = { foo: nat }", Ligo; "TYPE IN NAMESPACE", Markdown ] )
         ; ( pos ~line:22 ~character:15
           , List [ "p : option<t>", Ligo; "TERM IN NAMESPACE", Markdown ] )
         ; ( pos ~line:26 ~character:5
           , List [ "type t<a> = list<a>", Ligo; "JUST A TYPE", Markdown ] )
         ; pos ~line:35 ~character:6, hover_for_term_x
         ; pos ~line:37 ~character:10, hover_for_term_x
         ; ( pos ~line:40 ~character:11
           , List
               [ ( "namespace M1 implements {\n\
                   \  @entry\n\
                   \  const y: (_: int) => (_: int) => [list<operation>, int]\n\
                    }"
                 , Ligo )
               ; "NAMESPACE WITH ENTRY POINT", Markdown
               ] )
         ; ( pos ~line:46 ~character:15
           , List
               [ "y : (_: int) => (_: int) => [list<operation>, int]", Ligo
               ; "BEFORE DECORATOR", Markdown
               ; "AFTER DECORATOR", Markdown
               ; "ENTRY POINT TERM", Markdown
               ] )
         ; ( pos ~line:49 ~character:19
           , List
               [ "namespace C implements {\n  const f: (_: int) => int\n}", Ligo
               ; "NESTED NAMESPACE", Markdown
               ] )
         ; ( pos ~line:51 ~character:17
           , List [ "f : (_: int) => int", Ligo; "NESTED NAMESPACE TERM", Markdown ] )
         ; ( pos ~line:56 ~character:6
           , List
               [ "t : (_: unit) => list<int>", Ligo
               ; "Has type with comment inside", Markdown
               ] )
         ])
    }
  ; { test_name = "dynamic_entrypoints.mligo"
    ; file = "contracts/dynamic_entrypoints.mligo"
    ; hovers =
        List.map
          ~f:(fun p -> p, one "one : (unit, int) dynamic_entrypoint")
          [ pos ~line:7 ~character:5
          ; pos ~line:15 ~character:33
          ; pos ~line:32 ~character:30
          ]
        @ List.map
            ~f:(fun p -> p, one "tick : (int ticket, int * int) dynamic_entrypoint")
            [ pos ~line:10 ~character:7; pos ~line:23 ~character:35 ]
    }
  ; { test_name = "dynamic_entrypoints.jsligo"
    ; file = "contracts/dynamic_entrypoints.jsligo"
    ; hovers =
        List.map
          ~f:(fun p -> p, one "one : dynamic_entrypoint<unit, int>")
          [ pos ~line:7 ~character:9
          ; pos ~line:15 ~character:35
          ; pos ~line:36 ~character:29
          ]
        @ List.map
            ~f:(fun p -> p, one "tick : dynamic_entrypoint<ticket<int>, [int, int]>")
            [ pos ~line:11 ~character:9; pos ~line:25 ~character:36 ]
    }
  ]


let tests = "hover", List.map ~f:get_hover_test test_cases
