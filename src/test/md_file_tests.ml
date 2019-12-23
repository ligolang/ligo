open Trace
open Test_helpers

module SnippetsGroup = Map.Make(struct type t = (string * string) let compare a b = compare a b end)

let failed_to_compile_md_file md_file (s,group,prg) =
  let title () = "Failed to compile "^s^" group '"^group^"' in file '"^md_file^"'" in
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
      | Some s ->
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
          grp_map el.arguments
      | None -> grp_map
    )
    SnippetsGroup.empty code_blocks

(**
  evaluate each expression in each programs from the snippets group map
**)
let compile_groups _filename (grp_map: _ SnippetsGroup.t) =
  let grp_list = SnippetsGroup.bindings grp_map in
  let%bind _michelsons = bind_map_list 
    (fun ((s,_grp),contents) ->
      (*TODO: hierarchical error ?*)
      trace_strong (failed_to_compile_md_file _filename (s,_grp,contents)) @@
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
  let groups = get_groups filename in
  let%bind () = compile_groups filename groups in
  ok ()

let md_root = "../../gitlab-pages/docs/language-basics/"
let main = test_suite "Markdown files" [
    test "sets_lists_touples" (compile (md_root^"sets-lists-touples.md")) ;
  ]
