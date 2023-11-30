open Cli_expect

let contract basename = "../../test/contracts/" ^ basename

let%expect_test _ =
  (* Imports contain absolute paths. Let's replace them with relative ones. *)
  let replace_import : string -> string =
    let test_import_regex =
      Re.compile
      @@ Re.Perl.re
           {|import \* as (.+) from(\n| )+'.*\/\.\.\/\.\.\/test\/contracts\/(.+)'|}
    in
    Re.replace ~all:true test_import_regex ~f:(fun group ->
        let module_name = Re.Group.get group 1 in
        let import_file = Re.Group.get group 3 in
        Format.asprintf
          {|import * as %s from '/../../test/contracts/%s'|}
          module_name
          import_file)
  in
  run_ligo_good [ "print"; "ast-typed"; "--type-doc"; contract "type_doc.jsligo" ];
  print_endline @@ replace_import @@ [%expect.output];
  [%expect
    {|
    //@ts-nocheck
    import * as SomeFile from '/../../test/contracts/tuples_sequences_functions'
    export import SomeFile = SomeFile

    /**
     * Top level value doc
     */
    export const top_level_value : int = "..."
    import * as Map from '/../../test/contracts/map'
    export import Map = Map

    /** Doc for type */
    export type t <a,b> = ["A", int] | ["B", a] | ["C", b]

    /**
     * Extracts `int` value from `t<int, int>`
     */
    export const extract_int : (_: ["A", int] | ["B", int] | ["C", int]) => int =
    "..."

    /** Some storage */
    export type storage = { a: int; b: bool; c: string }

    /** Doc for all 3 values */
    export const z : string = "..."

    /** Doc for all 3 values */
    export const y : bool = "..."

    /** Doc for all 3 values */
    export const x : int = "..."
    const process_storage : (_: storage) => bool = "..."
    export type d = | ["D", int]
    export const c : int = "..."
    export const b : int = "..."
    export const a : int = "..."

    /** Doc for namespace */
    export namespace Outer {
    /** Doc for inner type */
    export type t = int

    /** Doc for inner function */
    export const add : (_: int) => (_: int) => int = "..."

    /** Doc for inner namespace */
    export namespace Inner {
    /** Doc for deep const */
    export const inner : storage = "..."}}

    /** Doc for interface */
    export namespace $Iface {
    /** Doc for type var in interface */
    export type t = $type_var

    /** Doc for type in interface */
    export type string = int

    /** Doc for value in interface */
    export const v : t =
    "..."} |}]
