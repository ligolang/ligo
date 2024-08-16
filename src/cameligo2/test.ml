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

  let env = Env.initial

  let compile_str env str =
    let str = From_ocaml.compile_str str in
    let env, str = Var_solving.solve_module env str in
    str
end

(* [%str let sequence x y = x] *)
(*  [%str
      type nonrec packed_id = { f : 'a. 'a -> 'a }

      let id x = x
      let packed_id = { f = id }] *)
(*     [%str
      module M = struct
        let id x = x
      end]
      *)
(* [%str type nonrec user = { id : 'a. 'a -> 'a }]  *)
(* type nonrec user = { id : 'a. 'a -> 'a }

  type nonrec example =
    | A of unit
    | B of user
    | C of user * user
    | D of { id : 'a. 'a -> 'a } *)
(* [%str
      type nonrec user =
        { id : int
        ; name : string
        }

      let michael = { id = 0; name = "Michael" } *)
(* type nonrec status =
        | Default
        | Blocked of string
        | Unblocked of string * string

      let x = Default
      let y = Blocked "bad user"
      let z = Unblocked ("bad user", "but nice")
*)
(* 
   type nonrec user =
        | Not_user
        | User of
            { id : int
            ; name : string
            }

      let a = Not_user
      let b = User { id = 1; name = "Billie Jean" }
*)
(*
  type nonrec user =
    | A
    | B of int
    | C of int * int
    | User of { id : int; name : string }

  let f x =
    match x with
    | A -> 0
    | B x -> x
    | C (x, y) -> y
    | User { id; name } -> id
*)
(* let f x : int = x *)
let main () =
  let code = [%str let f x : int = x] in
  let code = OCaml.(type_str env code) in
  let code = Ligo.(compile_str env code) in
  let code =
    let raw_options = Compiler_options.Raw_options.make () in
    let options = Compiler_options.make ~raw_options () in
    let stdlib = Build.Stdlib.get ~options in
    let stdlib = stdlib.content_typed.pr_sig in
    Simple_utils.Trace.to_stdlib_result ~fast_fail:No_fast_fail
    @@ Simple_utils.Trace.trace Main_errors.checking_tracer
    @@ Checking.type_program ~options:options.middle_end ~env:stdlib code
  in
  (* TODO: this is weird *)
  Format.set_formatter_out_channel stderr;
  let rec pp_errors fmt errors =
    match errors with
    | [] -> ()
    | error :: errors ->
      Format.fprintf
        fmt
        "%a\n%a"
        (Main_errors.Formatter.error_ppformat ~display_format:Dev ~no_colour:false)
        error
        pp_errors
        errors
  in
  let code =
    match code with
    | Ok (code, [], []) ->
      (* TODO: warnings? *)
      code
    | Ok (code, [], warnings) ->
      Format.printf "warnings : %d\n%!" @@ List.length warnings;
      failwith "warnings"
    | Ok _ -> failwith "an error happened, but ok"
    | Error (errors, _warnings) ->
      Format.printf "%a\n%!" pp_errors errors;
      failwith "an error happened"
  in
  let () =
    (* TODO: something hijacks Format buffer? *)
    Format.printf "%a\n%!" (Ast_typed.PP.program ~use_hidden:false) code
  in
  ()


let () = main ()
