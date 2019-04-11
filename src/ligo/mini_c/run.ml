open Trace
open Types
open Compiler
open Memory_proto_alpha.Script_ir_translator

let run_aux (program:compiled_program) (input_michelson:Michelson.t) : ex_typed_value result =
  let open Meta_michelson.Wrap in
  let Compiler.{input;output;body} : compiled_program = program in
  let (Ex_ty input_ty) = input in
  let (Ex_ty output_ty) = output in
  let%bind input =
    Trace.trace_tzresult_lwt (fun () -> simple_error (thunk "error parsing input") ()) @@
    Tezos_utils.Memory_proto_alpha.parse_michelson_data input_michelson input_ty in
  let body = Michelson.strip_annots body in
  let%bind descr =
    Trace.trace_tzresult_lwt (fun () -> simple_error (thunk "error parsing program code") ()) @@
    Tezos_utils.Memory_proto_alpha.parse_michelson body
      (Stack.(input_ty @: nil)) (Stack.(output_ty @: nil)) in
  let open! Memory_proto_alpha.Script_interpreter in
  let%bind (Item(output, Empty)) =
    Trace.trace_tzresult_lwt (fun () -> simple_error (thunk "error of execution") ()) @@
    Tezos_utils.Memory_proto_alpha.interpret descr (Item(input, Empty)) in
  ok (Ex_typed_value (output_ty, output))

let run_node (program:program) (input:Michelson.t) : Michelson.t result =
  let%bind compiled = translate_program program "main" in
  let%bind (Ex_typed_value (output_ty, output)) = run_aux compiled input in
  let%bind output =
    Trace.trace_tzresult_lwt (fun () -> simple_error (thunk "error unparsing output") ()) @@
    Tezos_utils.Memory_proto_alpha.unparse_michelson_data output_ty output in
  ok output

let run_entry (entry:anon_function) (input:value) : value result =
  let%bind compiled = translate_entry entry in
  let%bind input_michelson = translate_value input in
  let%bind ex_ty_value = run_aux compiled input_michelson in
  let%bind (result : value) = Uncompiler.translate_value ex_ty_value in
  ok result

let run (program:program) (input:value) : value result =
  let%bind input_michelson = translate_value input in
  let%bind compiled = translate_program program "main" in
  let%bind ex_ty_value = run_aux compiled input_michelson in
  let%bind (result : value) = Uncompiler.translate_value ex_ty_value in
  ok result

let expression_to_value ((e', _, _) as e:expression) : value result =
  match e' with
  | E_literal v -> ok v
  | _ -> fail
      @@ error (thunk "not a value")
      @@ (fun () -> Format.asprintf "%a" PP.expression e)
