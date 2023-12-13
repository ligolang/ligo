(* Tests for decompiling, i.e. transformation of Ast_core to CST.
   It's done through 3 steps: using [Nanopasses.From_core],
   applying backwards nanopasses, and then applying the decompiler.
   First two steps are handled by Nanopasses.decompile_<...>,last step uses
   Unification.Jsligo.decompile_ty_expr *)

module Trace = Simple_utils.Trace
open Alcotest_extras

let remove_extra_spaces : string -> string =
  Simple_utils.Function.(
    String.concat ~sep:" "
    <@ List.filter ~f:(not <@ String.equal "")
    <@ String.split_on_chars ~on:[ ' '; '\r'; '\n' ])


let get_defs ~(code : string) ~(syntax : Syntax_types.t) : Scopes.def list Lwt.t =
  let open Lwt.Let_syntax in
  let options =
    Compiler_options.Raw_options.make
      ~with_types:true
      ~no_stdlib:true
      ~syntax:(Syntax.to_string syntax)
      ()
  in
  let%map ({ errors; warnings = _; definitions }
            : Lsp_helpers.Ligo_interface.Get_scope.defs_and_diagnostics)
    =
    Lsp_helpers.Ligo_interface.Get_scope.get_defs_and_diagnostics
      ~logger:(fun ~type_:_ _ -> Lwt.return_unit)
      options
      (Raw_input_lsp { file = "test" ^ Syntax.to_ext syntax; code })
  in
  if not (List.is_empty errors)
  then (
    let formatter =
      Fmt.list
      @@ Main_errors.Formatter.error_format.pp
           ~no_colour:false
           ~display_format:Human_readable
    in
    failf "%a" formatter errors);
  definitions


type decompiler_test_ty_expr =
  { name : string
  ; code : string
        (* expects 1 decl
          "type t = ..." to check decompiling of concrete type expretsion
           or "let x = ..." to check decompiling of inferred type.
           In case of inferred type, the untyper Ast_typed -> Ast_core is also involved *)
  ; syntax : Syntax_types.t
  ; expected : string
  }

let mk_decompiler_test { code; expected; syntax; name } =
  let open Lwt.Let_syntax in
  Alcotest.test_case (Syntax.to_string syntax ^ ": " ^ name) `Quick
  @@ fun () ->
  let ast_core =
    match%map get_defs ~code ~syntax with
    | [] -> fail "No defs"
    | Variable vdef :: _ ->
      (* ^ E.g. "let x a = a + 1" creates two vdefs but the one for x is always first*)
      (match vdef.t with
      | Core t -> t
      | Resolved ast_typed -> Checking.untype_type_expression ast_typed
      | Unresolved -> failwith "Got a vdef with unresolved type")
    | Type tdef :: _ ->
      Option.value_or_thunk
        ~default:(fun () -> fail "Expected tdef with Some type contents, but got None")
        tdef.content
    | Module _ :: _ -> fail "Expected vdef or tdef at the beginning of defs list"
  in
  (* uncomment code below if you want to print Ast_core (no sexps here for some reason) *)
  (* let () =
    let ty_expr_to_formatter : Ast_core.ty_expr Fmt.t = Ast_core.PP.type_expression in
    print_string @@ Format.asprintf "Ast_core:\n%a\n" ty_expr_to_formatter ast_core
  in *)
  let ast_unified =
    let result =
      Trace.to_stdlib_result @@ Nanopasses.decompile_ty_expr @@ Lwt_main.run ast_core
    in
    match result with
    | Ok (s, _warnings) -> s ~syntax
    | Error (errors, _warnings) ->
      let errors_formatter =
        Nanopasses.Errors.error_ppformat ~no_colour:false ~display_format:Human_readable
      in
      failf "Nanopasses decompiler failed: %a" errors_formatter errors
  in
  (* uncomment code below  if you want to print Ast_unified *)
  (* let () =
    let rec ty_expr_to_sexp : Ast_unified.ty_expr -> Sexp.t =
     fun te -> Ast_unified.sexp_of_type_expression_ ty_expr_to_sexp te.fp
    in
    printf "Ast_unified:\n%s\n" (Sexp.to_string_hum @@ ty_expr_to_sexp ast_unified)
  in *)
  let cst =
    match syntax with
    | JsLIGO -> `JsLIGO (Unification.Jsligo.decompile_ty_expr ast_unified)
    | CameLIGO -> `Cameligo (Unification.Cameligo.decompile_ty_expr ast_unified)
  in
  let doc =
    match cst with
    | `JsLIGO cst -> Parsing.Jsligo.Pretty.(print_type_expr default_state cst)
    | `Cameligo cst -> Parsing.Cameligo.Pretty.(print_type_expr default_state cst)
  in
  check
    raw_string
    "Pretty printed CST match expected"
    expected
    (let buffer = Buffer.create 131 in
     PPrint.ToBuffer.compact buffer doc;
     remove_extra_spaces @@ Buffer.contents buffer)


let decompiler_ty_expr_tests =
  List.map
    ~f:mk_decompiler_test
    [ (*
       CameLIGO
       *)
      { name = "int"; code = "type t = int"; expected = "int"; syntax = CameLIGO }
    ; { name = "int (inferred)"; code = "let x = 5"; expected = "int"; syntax = CameLIGO }
    ; { name = "bool (inferred)"
      ; code = "let x = true"
      ; expected = "bool"
      ; syntax = CameLIGO
      }
    ; { name = "option"
      ; code = "type t = int option"
      ; expected = "int option"
      ; syntax = CameLIGO
      }
    ; { name = "option (inferred)"
      ; code = "let x = Some 5"
      ; expected = "int option"
      ; syntax = CameLIGO
      }
    ; { name = "nested option (inferred)"
      ; code = "let x = Some (Some 3)"
      ; expected = "int option option"
      ; syntax = CameLIGO
      }
    ; { name = "arrow"
      ; code = "type t = int option -> int"
      ; expected = "int option -> int"
      ; syntax = CameLIGO
      }
    ; { name = "arrow (inferred)"
      ; code = "let x = fun a -> a + 1"
      ; expected = "int -> int"
      ; syntax = CameLIGO
      }
    ; { name = "nested arrow (inferred)"
      ; code = "let x = fun a b -> a + b + 1"
      ; expected = "int -> int -> int"
      ; syntax = CameLIGO
      }
    ; { name = "arrow with parens on RHS"
      ; code = "type t = int -> (A of int)"
      ; expected = "int -> (A of int)" (* This is correct: parse error without parens *)
      ; syntax = CameLIGO
      }
    ; { name = "product (inferred)"
      ; code = "let x = (5, Some \"s\")"
      ; expected = "int * string option"
      ; syntax = CameLIGO
      }
    ; { name = "union"
      ; code = "type t = A of int | B of string | C"
      ; expected = "A of int | B of string | C of unit" (* TODO #1758 *)
      ; syntax = CameLIGO
      }
    ; { name = "record"
      ; code = "type t = {a : int; b : string option}"
      ; expected = "{a : int; b : string option}"
      ; syntax = CameLIGO
      }
    ; { name = "nested union"
      ; code = "type t = A of (B of int | C of string) | D of bool option"
      ; expected = "A of (B of int | C of string) | D of bool option"
      ; syntax = CameLIGO
      }
    ; { name = "option applied to union"
      ; code = "type t = (A of int) option"
      ; expected = "(A of int) option"
      ; syntax = CameLIGO
      }
    ; { name =
          "Erased quantifier (inferred)"
          (* [∀ a : *] is erased here since it can't be a part of CST type expression.
             Such behavior is useful for LSP hovers. *)
      ; code =
          "let x (type a) (v : a option) : bool = match v with | None -> False | Some _ \
           -> True"
      ; expected = "'a.'a option -> bool"
      ; syntax = CameLIGO
      }
    ; { name = "modules"
      ; code =
          {|
      module A = struct
        module B = struct
          type t = X
        end
      end

      type a = A.B.t option
      |}
      ; expected = "A.B.t option" (* How name is preserved here? *)
      ; syntax = CameLIGO
      }
      (*
      JsLIGO
      *)
    ; { name = "int"; code = "type t = int"; expected = "int"; syntax = JsLIGO }
    ; { name = "int (inferred)"; code = "let x = 5"; expected = "int"; syntax = JsLIGO }
    ; { name = "bool (inferred)"
      ; code = "let x = true"
      ; expected = "bool"
      ; syntax = JsLIGO
      }
    ; { name = "option"
      ; code = "type t = option<int>"
      ; expected = "option<int>"
      ; syntax = JsLIGO
      }
    ; { name = "option (inferred)"
      ; code = "let x = Some(5)"
      ; expected = "option<int>"
      ; syntax = JsLIGO
      }
    ; { name = "nested option (inferred)"
      ; code = "let x = Some (Some (3))"
      ; expected = "option<option<int>>"
      ; syntax = JsLIGO
      }
    ; { name = "fun"
      ; code = "type t = (a: option<int>) => int"
      ; expected = (* TODO #1811 save arg names *)
                   "(_: option<int>) => int"
      ; syntax = JsLIGO
      }
    ; { name = "fun (inferred)"
      ; code = "let x = a => a + 1"
      ; expected = "(_: int) => int" (* TODO #1811 *)
      ; syntax = JsLIGO
      }
    ; { name = "nested fun (inferred)"
      ; code = "let x = a => b => a + b + 1"
      ; expected = "(_: int) => (_: int) => int" (* TODO #1811 *)
      ; syntax = JsLIGO
      }
    ; { name = "fun with multiple args"
      ; code = "type t = (a: option<int>, b: bool) => string"
      ; expected =
          (* TODO #1811 save arity as metadata or workaround: always create multi-arg fun *)
          "(_: option<int>) => (_: bool) => string"
      ; syntax = JsLIGO
      }
    ; { name = "product (inferred)"
      ; code = "let x = [5, Some (\"s\")]"
      ; expected = "[int, option<string>]"
      ; syntax = JsLIGO
      }
    ; { name = "disc union"
      ; code = {|type t = {a : "1"; b : int} | {a : "2"; b : bool}|}
      ; expected =
          (* TODO #1811 save disc union field name *)
          {|["1", { b: int }] | ["2", { b: bool }]|}
      ; syntax = JsLIGO
      }
    ; { name = "union"
      ; code = {|type t = ["A", int] | ["B", string] | ["C"]|}
      ; expected = {|["A", int] | ["B", string] | ["C", unit]|} (* TODO #1758 *)
      ; syntax = JsLIGO
      }
    ; { name = "nested union"
      ; code = {|type t = | ["A", | ["X", int] | ["Y", bool]] | ["D", bool]|}
      ; expected =
          {|["A", ["X", int] | ["Y", bool]] | ["D", bool]|}
          (* ^ XXX parser requires lead vbar, pretty printer omit lead vbar 0_0 *)
      ; syntax = JsLIGO
      }
    ; { name = "record"
      ; code = "type t = { a: int; b: option<string> }"
      ; expected = "{ a: int; b: option<string> }"
      ; syntax = JsLIGO
      }
    ; { name = "option applied to union"
      ; code = {|type t = option<| ["X", int]>|}
      ; expected = {|option<| ["X", int]>|}
      ; syntax = JsLIGO
      }
    ; { name = "modules"
      ; code =
          {|
      namespace A {
        export namespace B {
          export type t = | ["X"]
        }
      };

      type a = A.B.t;
      |}
      ; expected = "A.B.t"
      ; syntax = JsLIGO
      }
    ]


let _main =
  Printexc.record_backtrace true;
  Alcotest.run "decompiler tests" [ "type expression", decompiler_ty_expr_tests ]
