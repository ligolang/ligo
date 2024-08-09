open Parsetree

let loc = Location.none

module OCaml = struct
  open Ocaml_common

  let env = Env.initial_safe_string
  let _type_expr env expr = Typecore.type_expression env expr

  let type_str env str =
    let str, sig_, names, shape, env = Typemod.type_structure env str in
    str
end

module Ligo = struct
  open Cameligo2
  open Var_solving

  let env = Env.empty

  let compile_str env str =
    let str = From_ocaml.compile_str str in
    let env, str = Var_solving.solve_module env str in
    str
end

let main () =
  let code = [%str let sequence x y = x] in
  let code = OCaml.(type_str env code) in
  let code = Ligo.(compile_str env code) in
  let code =
    let raw_options = Compiler_options.Raw_options.make () in
    let options = Compiler_options.make ~raw_options () in
    Simple_utils.Trace.to_stdlib_result ~fast_fail:No_fast_fail
    @@ Simple_utils.Trace.trace Main_errors.checking_tracer
    @@ Checking.type_program ~options:options.middle_end code
  in
  let code =
    match code with
    | Ok (code, [], warnings) ->
      (* TODO: warnings? *)
      code
    | Ok _ | Error _ -> failwith "an error happened"
  in
  let () =
    (* TODO: something hijacks Format buffer? *)
    print_endline
    @@ Format.asprintf "%a\n%!" (Ast_typed.PP.program ~use_hidden:false) code
  in
  ()


let () = main ()
