open Trace
open Test_helpers

module SnippetsGroup = Map.Make(struct type t = (string * string) let compare a b = compare a b end)

let failed_to_compile_md_file md_file (s,group,prg) =
  let title () = "Failed to compile ```"^s^" block (group '"^group^"') in file '"^md_file^"'" in
  let content () = "\n"^prg in
  error title content

let bad_code_block_argument arg =
  let title () = Format.asprintf "Bad code block argument '%s'" arg in
  let content () = "only 'group=NAME' or 'skip' are allowed" in
  error title content

(**
  binds the snippets by (syntax, group_name)
  e.g. :(pascaligo, a) -> "let .. in let .. in"
        (cameligo,  a) -> "let .. in let .. in"
  syntax and group_name being retrieved from the .md file header & arguments
  e.g. : ```syntax group=group_name ...some code ...  ```
**)
let get_groups md_file =
  let channel = open_in md_file in
  let lexbuf = Lexing.from_channel channel in
  let code_blocks = Md.token lexbuf in
  bind_fold_list
    (fun (grp_map: _ SnippetsGroup.t) (el:Md.block) ->
      match el.header  with
      | Some ("pascaligo" as s) | Some ("cameligo" as s) | Some ("reasonligo" as s) -> (
        let%bind () = bind_iter_list
          (fun arg -> match arg with
          | Md.Field "" | Md.Field "skip" | Md.NameValue ("group",_) -> ok ()
          | Md.Field f | Md.NameValue (f,_) -> fail @@ bad_code_block_argument f)
          el.arguments in
        match el.arguments with
        | [Md.Field ""] ->
          ok @@ SnippetsGroup.update (s,"ungrouped")
                  (fun arg_content ->
                    match arg_content with
                    | Some ct -> Some (String.concat "\n" (ct::el.contents))
                    | None -> Some (String.concat "\n" el.contents)
                  )
                  grp_map
        | [Md.Field "skip"] -> ok grp_map
        | _ -> bind_fold_list
          (fun grp_map arg -> match arg with
              | Md.NameValue ("group", name) ->
                ok @@ SnippetsGroup.update (s,name)
                  (fun arg_content ->
                    match arg_content with
                    | Some ct -> Some (String.concat "\n" (ct::el.contents))
                    | None -> Some (String.concat "\n" el.contents)
                  )
                  grp_map
              | _ -> ok grp_map
          )
          grp_map el.arguments )
      | None | Some _ -> ok grp_map
    )
    SnippetsGroup.empty code_blocks

(**
  evaluate each expression in each programs from the snippets group map
**)
let compile_groups _filename grp_list =
  let%bind (_michelsons : Compiler.compiled_expression list list) = bind_map_list
    (fun ((s,grp),contents) ->
      trace (failed_to_compile_md_file _filename (s,grp,contents)) @@
      let%bind v_syntax   = Compile.Helpers.syntax_to_variant (Syntax_name s) None in
      let%bind simplified = Compile.Of_source.compile_string contents v_syntax in
      let%bind typed,_    = Compile.Of_simplified.compile simplified in
      let%bind mini_c     = Compile.Of_typed.compile typed in
      bind_map_list
        (fun ((_, _, exp),_) -> Compile.Of_mini_c.aggregate_and_compile_expression mini_c exp)
        mini_c
    )
    grp_list in
  ok ()

let compile filename () =
  let%bind groups = get_groups filename in
  let groups_map = SnippetsGroup.bindings groups in
  let%bind () = compile_groups filename groups_map in
  ok ()

(*
find ./gitlab-pages/ -iname "*.md"
*)
let md_files = [
  "/gitlab-pages/docs/tutorials/get-started/tezos-taco-shop-smart-contract.md";
  "/gitlab-pages/docs/tutorials/get-started/tezos-taco-shop-payout.md";
  "/gitlab-pages/docs/intro/installation.md";
  "/gitlab-pages/docs/intro/editor-support.md";
  "/gitlab-pages/docs/intro/what-and-why.md";
  "/gitlab-pages/docs/language-basics/math-numbers-tez.md";
  "/gitlab-pages/docs/language-basics/functions.md";
  "/gitlab-pages/docs/language-basics/boolean-if-else.md";
  "/gitlab-pages/docs/language-basics/types.md";
  "/gitlab-pages/docs/language-basics/strings.md";
  "/gitlab-pages/docs/language-basics/maps-records.md";
  "/gitlab-pages/docs/language-basics/variables-and-constants.md";
  "/gitlab-pages/docs/language-basics/sets-lists-tuples.md";
  "/gitlab-pages/docs/language-basics/operators.md";
  "/gitlab-pages/docs/language-basics/unit-option-pattern-matching.md";
  "/gitlab-pages/docs/language-basics/loops.md";
  "/gitlab-pages/docs/language-basics/tezos-specific.md";
  "/gitlab-pages/docs/contributors/big-picture/back-end.md";
  "/gitlab-pages/docs/contributors/big-picture/vendors.md";
  "/gitlab-pages/docs/contributors/big-picture/front-end.md";
  "/gitlab-pages/docs/contributors/big-picture/overview.md";
  "/gitlab-pages/docs/contributors/big-picture/middle-end.md";
  "/gitlab-pages/docs/contributors/documentation-and-releases.md";
  "/gitlab-pages/docs/contributors/getting-started.md";
  "/gitlab-pages/docs/contributors/philosophy.md";
  "/gitlab-pages/docs/contributors/ligo_test_guide.md";
  "/gitlab-pages/docs/contributors/road-map/short-term.md";
  "/gitlab-pages/docs/contributors/road-map/long-term.md";
  "/gitlab-pages/docs/contributors/origin.md";
  "/gitlab-pages/docs/advanced/first-contract.md";
  "/gitlab-pages/docs/advanced/entrypoints-contracts.md";
  "/gitlab-pages/docs/advanced/timestamps-addresses.md";
  "/gitlab-pages/docs/api/cli-commands.md";
  "/gitlab-pages/docs/api/cheat-sheet.md";
  "/gitlab-pages/docs/reference/big_map.md";
]

let md_root = "../../gitlab-pages/docs/language-basics/"
let main = test_suite "Markdown files"
  (List.map (fun md_file ->
    let test_name = "File : \"."^md_file^"\"" in
    let md_path = "../.."^md_file in
    test test_name (compile md_path))
  md_files)
