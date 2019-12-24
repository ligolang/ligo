open Trace
open Test_helpers

module SnippetsGroup = Map.Make(struct type t = (string * string) let compare a b = compare a b end)

let failed_to_compile_md_file md_file (s,group,prg) =
  let title () = "Failed to compile ```"^s^" block (group '"^group^"') in file '"^md_file^"'" in
  let content () = "unable to compile the program down to michelson" in
  let data = [
    ("source" , fun () -> Format.asprintf "%s" prg) ;
  ] in
  error ~data title content

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
  List.fold_left
    (fun (grp_map: _ SnippetsGroup.t) (el:Md.block) ->
      match el.header  with
      | Some s when (String.equal s "pascaligo") || (String.equal s "cameligo") || (String.equal s "reasonligo") -> (
        match el.arguments with
        | [Md.Field ""] -> SnippetsGroup.update (s,"ungrouped")
                  (fun arg_content ->
                    match arg_content with
                    | Some ct -> Some (String.concat "\n" (ct::el.contents))
                    | None -> Some (String.concat "\n" el.contents)
                  )
                  grp_map
        | _ ->
        List.fold_left
          (fun grp_map arg -> match arg with
              | Md.NameValue ("group", name) ->
                SnippetsGroup.update (s,name)
                  (fun arg_content ->
                    match arg_content with
                    | Some ct -> Some (String.concat "\n" (ct::el.contents))
                    | None -> Some (String.concat "\n" el.contents)
                  )
                  grp_map
              | _ -> grp_map
          )
          grp_map el.arguments )
      | None | Some _ -> grp_map
    )
    SnippetsGroup.empty code_blocks

(**
  evaluate each expression in each programs from the snippets group map
**)
let compile_groups _filename grp_list =
  let%bind (_michelsons : Compiler.compiled_expression list list) = bind_map_list
    (fun ((s,_grp),contents) ->
      trace (failed_to_compile_md_file _filename (s,_grp,contents)) @@
      let%bind v_syntax   = Compile.Helpers.syntax_to_variant (Syntax_name s) None in
      let%bind simplified = Compile.Of_source.compile_string contents v_syntax in
      let%bind typed,_    = Compile.Of_simplified.compile simplified in
      let%bind mini_c     = Compile.Of_typed.compile typed in
      bind_map_list
        (fun ((_,exp),_) -> Compile.Of_mini_c.aggregate_and_compile_expression mini_c exp)
        mini_c
    )
    grp_list in
  ok ()

let compile filename () =
  let groups = SnippetsGroup.bindings @@ get_groups filename in
  let%bind () = compile_groups filename groups in
  ok ()

(*
find ./gitlab-pages/ -iname "*.md"
*)
let md_files = [
  (*
  "/gitlab-pages/docs/tutorials/get-started/tezos-taco-shop-smart-contract.md";
  "/gitlab-pages/docs/tutorials/get-started/tezos-taco-shop-payout.md";
  "/gitlab-pages/docs/intro/installation.md";
  "/gitlab-pages/docs/intro/editor-support.md";
  "/gitlab-pages/docs/intro/what-and-why.md";
  "/gitlab-pages/docs/language-basics/functions.md";
  "/gitlab-pages/docs/language-basics/math-numbers-tez.md";
  *)
  (* antislash problem:
  "/gitlab-pages/docs/language-basics/strings.md";
  "/gitlab-pages/docs/language-basics/boolean-if-else.md";
  "/gitlab-pages/docs/language-basics/types.md";
  *)
  "/gitlab-pages/docs/language-basics/maps-records.md";
  "/gitlab-pages/docs/language-basics/variables-and-constants.md";
  "/gitlab-pages/docs/language-basics/sets-lists-touples.md";
  (*
  "/gitlab-pages/docs/language-basics/operators.md";
  "/gitlab-pages/docs/language-basics/unit-option-pattern-matching.md";
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
  "/gitlab-pages/website/blog/2019-07-11-ligo-update.md";
  "/gitlab-pages/website/blog/2019-06-13-public-launch-of-ligo.md";
  "/gitlab-pages/website/versioned_docs/version-next/contributors/big-picture/back-end.md";
  "/gitlab-pages/website/versioned_docs/version-next/contributors/big-picture/vendors.md";
  "/gitlab-pages/website/versioned_docs/version-next/contributors/big-picture/front-end.md";
  "/gitlab-pages/website/versioned_docs/version-next/contributors/big-picture/overview.md";
  "/gitlab-pages/website/versioned_docs/version-next/contributors/big-picture/middle-end.md";
  "/gitlab-pages/website/versioned_docs/version-next/contributors/philosophy.md";
  "/gitlab-pages/website/versioned_docs/version-next/contributors/road-map/short-term.md";
  "/gitlab-pages/website/versioned_docs/version-next/contributors/road-map/long-term.md";
  "/gitlab-pages/website/versioned_docs/version-next/contributors/origin.md";
  "/gitlab-pages/website/versioned_docs/version-next/api-cli-commands.md";
  "/gitlab-pages/website/README.md";
  "/gitlab-pages/README.md";
  *)
]

let md_root = "../../gitlab-pages/docs/language-basics/"
let main = test_suite "Markdown files"
  (List.map (fun md_file ->
    let test_name = "File : \"."^md_file^"\"" in
    let md_path = "../.."^md_file in
    test test_name (compile md_path))
  md_files)
