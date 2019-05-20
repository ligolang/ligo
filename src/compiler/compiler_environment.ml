open Proto_alpha_utils
open Trace
open Mini_c
open Environment
open Michelson
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
  let module L = Logger.Stateful() in
  let e_lst =
    let e_lst = Environment.to_list e in
    let aux selector (s , _) =
      L.log @@ Format.asprintf "Selector : %a\n" PP_helpers.(list_sep string (const " , ")) selector ;
      match List.mem s selector with
      | true -> List.remove_element s selector , true
      | false -> selector , false in
    let e_lst' = List.fold_map_right aux lst e_lst in
    let e_lst'' = List.combine e_lst e_lst' in
    e_lst'' in
  let code =
    let aux = fun code (_ , b) ->
      match b with
      | false -> seq [dip code ; i_drop]
      | true -> dip code
    in
    List.fold_right' aux (seq []) e_lst in

  let%bind () =
    let%bind (Stack.Ex_stack_ty input_stack_ty) = Compiler_type.Ty.environment e in
    let e' =
      Environment.of_list
      @@ List.map fst
      @@ List.filter snd
      @@ e_lst
    in
    let%bind (Stack.Ex_stack_ty output_stack_ty) = Compiler_type.Ty.environment e' in
    let error () =
      let title () = "error producing Env.select" in
      let content () = Format.asprintf "\nInput : %a\nOutput : %a\nList : {%a}\nCode : %a\nLog : %s\n"
          PP.environment e
          PP.environment e'
          PP_helpers.(list_sep (pair PP.environment_element bool) (const " || ")) e_lst
          Michelson.pp code
          (L.get ())
      in
      ok @@ (error title content) in
    let%bind _ =
      Trace.trace_tzresult_lwt_r error @@
      Memory_proto_alpha.parse_michelson code
        input_stack_ty output_stack_ty in
    ok ()
  in

  ok code

let select_env : environment -> environment -> michelson result = fun source filter ->
  let lst = Environment.get_names filter in
  select source lst

let clear : environment -> (michelson * environment) result = fun e ->
  let lst = Environment.get_names e in
  let%bind first_name =
    trace_option (simple_error "try to clear empty env") @@
    List.nth_opt lst 0 in
  let%bind code = select e [ first_name ] in
  let e' = Environment.select [ first_name ] e in
  ok (code , e')

let pack : environment -> michelson result = fun e ->
  let%bind () =
    trace_strong (simple_error "pack empty env") @@
    Assert.assert_true (List.length e <> 0) in
  let code = seq @@ List.map (Function.constant i_pair) @@ List.tl e in

  let%bind () =
    let%bind (Stack.Ex_stack_ty input_stack_ty) = Compiler_type.Ty.environment e in
    let repr = Environment.closure_representation e in
    let%bind (Ex_ty output_ty) = Compiler_type.Ty.type_ repr in
    let output_stack_ty = Stack.(output_ty @: nil) in
    let error () =
      let title () = "error producing Env.pack" in
      let content () = Format.asprintf ""
      in
      ok @@ (error title content) in
    let%bind _ =
      Trace.trace_tzresult_lwt_r error @@
      Memory_proto_alpha.parse_michelson code
        input_stack_ty output_stack_ty in
    ok ()
  in

  ok code

let unpack : environment -> michelson result = fun e ->
  let%bind () =
    trace_strong (simple_error "unpack empty env") @@
    Assert.assert_true (List.length e <> 0) in

  let l = List.length e - 1 in
  let rec aux n =
    match n with
    | 0 -> seq []
    | n -> seq [
        i_unpair ;
        dip (aux (n - 1)) ;
      ] in
  let code = aux l in

  let%bind () =
    let%bind (Stack.Ex_stack_ty output_stack_ty) = Compiler_type.Ty.environment e in
    let repr = Environment.closure_representation e in
    let%bind (Ex_ty input_ty) = Compiler_type.Ty.type_ repr in
    let input_stack_ty = Stack.(input_ty @: nil) in
    let error () =
      let title () = "error producing Env.unpack" in
      let content () = Format.asprintf "\nEnvironment:%a\nType Representation:%a\nCode:%a\n"
          PP.environment e
          PP.type_ repr
          Michelson.pp code
      in
      ok @@ (error title content) in
    let%bind _ =
      Trace.trace_tzresult_lwt_r error @@
      Memory_proto_alpha.parse_michelson code
        input_stack_ty output_stack_ty in
    ok ()
  in

  ok code


let pack_select : environment -> string list -> michelson result = fun e lst ->
  let module L = Logger.Stateful() in
  let e_lst =
    let e_lst = Environment.to_list e in
    let aux selector (s , _) =
      L.log @@ Format.asprintf "Selector : %a\n" PP_helpers.(list_sep string (const " , ")) selector ;
      match List.mem s selector with
      | true -> List.remove_element s selector , true
      | false -> selector , false in
    let e_lst' = List.fold_map_right aux lst e_lst in
    let e_lst'' = List.combine e_lst e_lst' in
    e_lst'' in
  let (_ , code) =
    let aux = fun (first , code) (_ , b) ->
      match b with
      | false -> (first , seq [dip code ; i_swap])
      | true -> (false ,
                 match first with
                 | true -> i_dup
                 | false -> seq [dip code ; i_dup ; dip i_pair ; i_swap]
                )
    in
    List.fold_right' aux (true , seq []) e_lst in

  let%bind () =
    let%bind (Stack.Ex_stack_ty input_stack_ty) = Compiler_type.Ty.environment e in
    let e' =
      Environment.of_list
      @@ List.map fst
      @@ List.filter snd
      @@ e_lst
    in
    let%bind (Ex_ty output_ty) = Compiler_type.Ty.environment_representation e' in
    let output_stack_ty = Stack.(output_ty @: input_stack_ty) in
    let error () =
      let title () = "error producing Env.pack_select" in
      let content () = Format.asprintf "\nInput : %a\nOutput : %a\nList : {%a}\nCode : %a\nLog : %s\n"
          PP.environment e
          PP.environment e'
          PP_helpers.(list_sep (pair PP.environment_element bool) (const " || ")) e_lst
          Michelson.pp code
          (L.get ())
      in
      ok @@ (error title content) in
    let%bind _ =
      Trace.trace_tzresult_lwt_r error @@
      Memory_proto_alpha.parse_michelson code
        input_stack_ty output_stack_ty in
    ok ()
  in

  ok code

let add_packed_anon : environment -> type_value -> michelson result = fun e type_value ->
  let code = seq [i_pair] in

  let%bind () =
    let error () = ok @@ simple_error "error producing add packed" in
    let%bind (Ex_ty input_ty) = Compiler_type.Ty.environment_representation e in
    let e' = Environment.add ("_add_packed_anon" , type_value) e in
    let%bind (Ex_ty output_ty) = Compiler_type.Ty.environment_representation e' in
    let%bind (Ex_ty ty) = Compiler_type.Ty.type_ type_value in
    let input_stack_ty = Stack.(ty @: input_ty @: nil) in
    let output_stack_ty = Stack.(output_ty @: nil) in
    let%bind _ =
      Trace.trace_tzresult_lwt_r error @@
      Memory_proto_alpha.parse_michelson code
        input_stack_ty output_stack_ty in
    ok ()
  in

  ok code

let pop : environment -> environment result = fun e ->
  match e with
  | [] -> simple_fail "pop empty env"
  | _ :: tl -> ok tl
