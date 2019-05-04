open Trace
open Mini_c
open Environment
open Micheline.Michelson
open Memory_proto_alpha.Script_ir_translator

module Stack = Meta_michelson.Stack

let get : environment -> string -> michelson result = fun e s ->
  let%bind (type_value , position) =
    let error =
      let title () = "Environment.get" in
      let content () = Format.asprintf "%s in %a"
          s PP.environment e in
      error title content in
    generic_try error @@
    (fun () -> Environment.get_i s e) in
  let rec aux = fun n ->
    match n with
    | 0 -> i_dup
    | n -> seq [
        dip @@ aux (n - 1) ;
        i_swap ;
      ]
  in
  let code = aux position in

  let%bind () =
    let error () = ok @@ simple_error "error producing Env.get" in
    let%bind (Stack.Ex_stack_ty input_stack_ty) = Compiler_type.Ty.environment e in
    let%bind (Ex_ty ty) = Compiler_type.Ty.type_ type_value in
    let output_stack_ty = Stack.(ty @: input_stack_ty) in
    let%bind _ =
      Trace.trace_tzresult_lwt_r error @@
      Memory_proto_alpha.parse_michelson code
        input_stack_ty output_stack_ty in
    ok ()
  in

  ok code

let set : environment -> string -> michelson result = fun e s ->
  let%bind (type_value , position) =
    generic_try (simple_error "Environment.get") @@
    (fun () -> Environment.get_i s e) in
  let rec aux = fun n ->
    match n with
    | 0 -> dip i_drop
    | n -> seq [
        i_swap ;
        dip (aux (n - 1)) ;
      ]
  in
  let code = aux position in

  let%bind () =
    let error () = ok @@ simple_error "error producing Env.set" in
    let%bind (Stack.Ex_stack_ty env_stack_ty) = Compiler_type.Ty.environment e in
    let%bind (Ex_ty ty) = Compiler_type.Ty.type_ type_value in
    let input_stack_ty = Stack.(ty @: env_stack_ty) in
    let output_stack_ty = env_stack_ty in
    let%bind _ =
      Trace.trace_tzresult_lwt_r error @@
      Memory_proto_alpha.parse_michelson code
        input_stack_ty output_stack_ty in
    ok ()
  in

  ok code

let add : environment -> (string * type_value) -> michelson result = fun e (_s , type_value) ->
  let code = seq [] in

  let%bind () =
    let error () = ok @@ simple_error "error producing Env.get" in
    let%bind (Stack.Ex_stack_ty env_stack_ty) = Compiler_type.Ty.environment e in
    let%bind (Ex_ty ty) = Compiler_type.Ty.type_ type_value in
    let input_stack_ty = Stack.(ty @: env_stack_ty) in
    let output_stack_ty = Stack.(ty @: env_stack_ty) in
    let%bind _ =
      Trace.trace_tzresult_lwt_r error @@
      Memory_proto_alpha.parse_michelson code
        input_stack_ty output_stack_ty in
    ok ()
  in

  ok code

let select : environment -> string list -> michelson result = fun e lst ->
  let code =
    let aux = fun acc (s , _) ->
      seq [
        dip acc ;
        if List.mem s lst
        then seq []
        else i_drop ;
      ]
    in
    Environment.fold aux (seq []) e in

  let%bind () =
    let error () = ok @@ simple_error "error producing Env.select" in
    let%bind (Stack.Ex_stack_ty input_stack_ty) = Compiler_type.Ty.environment e in
    let e' = Environment.filter (fun (s , _) -> List.mem s lst) e in
    let%bind (Stack.Ex_stack_ty output_stack_ty) = Compiler_type.Ty.environment e' in
    let%bind _ =
      Trace.trace_tzresult_lwt_r error @@
      Memory_proto_alpha.parse_michelson code
        input_stack_ty output_stack_ty in
    ok ()
  in

  ok code

let clear : environment -> michelson result = fun e -> select e []

let select_env : environment -> environment -> michelson result = fun e e' ->
  let lst = Environment.get_names e' in
  select e lst
