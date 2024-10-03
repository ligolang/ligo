open Main_errors
open Mini_c
open Proto_alpha_utils
open Trace
open! Stacking
open Tezos_micheline
open Ligo_lltz_codegen
open Lltz_codegen

let dummy : Stacking.meta =
  { location = Location.dummy
  ; env = []
  ; binder = None
  ; source_type = None
  ; application = None
  }


let dummy_locations : 'l 'p. ('l, 'p) Micheline.node -> (meta, 'p) Micheline.node =
 fun e -> Micheline.(inject_locations (fun _ -> dummy) (strip_locations e))


(* Tells optimizer whether a node has an important comment, in order
   to preserve Seq nodes which are used only for comments. Currently
   only env data is important. *)
let has_comment : Compiler_options.t -> meta -> bool =
 fun options { env; location; binder = _; source_type = _; application = _ } ->
  options.backend.has_env_comments
  && ((not (List.is_empty env)) || not (Location.is_dummy_or_generated location))


(* this function exist to satisfy 'print mini-c' .. *)
let optimize_for_contract ~raise options e : type_expression * anon_function =
  let input_ty, _ =
    trace ~raise self_mini_c_tracer @@ Self_mini_c.get_t_function e.type_expression
  in
  let contract : anon_function =
    trace ~raise self_mini_c_tracer @@ Self_mini_c.get_function_or_eta_expand e
  in
  let contract =
    { contract with
      body =
        trace ~raise self_mini_c_tracer
        @@ Self_mini_c.all_expression options contract.body
    }
  in
  let optimized =
    trace ~raise self_mini_c_tracer @@ Self_mini_c.contract_check ~options contract
  in
  input_ty, optimized


let compile_type e =
  let expr_ty = Scoping.translate_type e in
  let expr_ty = Lltz_codegen.convert_type expr_ty in
  dummy_locations
  @@ Micheline.map_node
       (fun _ -> dummy)
       (fun prim -> Michelson.Ast.Prim.to_string prim)
       expr_ty


let compile_contract ~raise
    : options:Compiler_options.t -> expression -> Stacking.compiled_expression Lwt.t
  =
 fun ~options e_contract ->
  let open Lwt.Let_syntax in
  let input_ty, contract = optimize_for_contract ~raise options e_contract in
  let%map expr =
    (* TODO: this trace was already done by optimize_for_contract *)
    let e_optimised =
      trace ~raise self_mini_c_tracer @@ Self_mini_c.all_expression options contract.body
    in
    let expr =
      let Var lltz_var, lltz_ty, lltz_body =
        Ligo_lltz_codegen.compile_contract contract.binder input_ty e_optimised
      in
      Lltz_codegen.compile_contract_to_micheline lltz_var lltz_ty lltz_body []
    in
    Lwt.return
      (Micheline.map_node
         (fun _ -> dummy)
         (fun prim -> Michelson.Ast.Prim.to_string prim)
         expr)
  in
  let expr =
    Self_michelson.optimize
      ~experimental_disable_optimizations_for_debugging:
        options.backend.experimental_disable_optimizations_for_debugging
      ~has_comment:(has_comment options)
      expr
  in
  let expr_ty = compile_type e_contract.type_expression in
  let expr_ty = dummy_locations expr_ty in
  ({ expr_ty; expr } : Stacking.Program.compiled_expression)


let compile_view ~raise
    : options:Compiler_options.t -> expression -> Stacking.compiled_expression Lwt.t
  =
 fun ~options e ->
  let open Lwt.Let_syntax in
  let input_ty, output_ty =
    trace ~raise self_mini_c_tracer @@ Self_mini_c.get_t_function e.type_expression
  in
  let view : anon_function =
    trace ~raise self_mini_c_tracer @@ Self_mini_c.get_function_or_eta_expand e
  in
  let view =
    { view with
      body =
        trace ~raise self_mini_c_tracer @@ Self_mini_c.all_expression options view.body
    }
  in
  let%map expr =
    (* TODO: is this correct? get Alan to check *)
    let expr =
      Lltz_codegen.compile_to_micheline
        (Ligo_lltz_codegen.compile_expression view.body)
        []
    in
    Lwt.return
      (Micheline.map_node
         (fun _ -> dummy)
         (fun prim -> Michelson.Ast.Prim.to_string prim)
         expr)
  in
  let expr =
    Self_michelson.optimize
      ~experimental_disable_optimizations_for_debugging:
        options.backend.experimental_disable_optimizations_for_debugging
      ~has_comment:(has_comment options)
      expr
  in
  let l, r = trace ~raise self_mini_c_tracer @@ Self_mini_c.get_t_pair input_ty in
  let l = compile_type l in
  let r = compile_type r in
  let input_ty = Micheline.(Prim (dummy, "pair", [ l; r ], [])) in
  let output_ty = compile_type output_ty in
  let expr_ty = Micheline.(Prim (dummy, "lambda", [ input_ty; output_ty ], [])) in
  let expr_ty = dummy_locations expr_ty in
  ({ expr_ty; expr } : Stacking.Program.compiled_expression)


let compile_expression ~raise
    : options:Compiler_options.t -> expression -> compiled_expression Lwt.t
  =
 fun ~options e ->
  let e = trace ~raise self_mini_c_tracer @@ Self_mini_c.all_expression options e in
  let open Lwt.Let_syntax in
  let%map expr =
    let expr =
      Lltz_codegen.compile_to_micheline (Ligo_lltz_codegen.compile_expression e) []
    in
    Lwt.return
      (Micheline.map_node
         (fun _ -> dummy)
         (fun prim -> Michelson.Ast.Prim.to_string prim)
         expr)
  in
  let expr =
    Self_michelson.optimize
      ~experimental_disable_optimizations_for_debugging:
        options.backend.experimental_disable_optimizations_for_debugging
      ~has_comment:(has_comment options)
      expr
  in
  let expr_ty = compile_type e.type_expression in
  ({ expr_ty; expr } : Program.compiled_expression)


let compile_expression_function ~raise
    : options:Compiler_options.t -> expression -> compiled_expression Lwt.t
  =
 fun ~options e ->
  let open Lwt.Let_syntax in
  (* TODO: what about this input_ty? *)
  let _input_ty, _ =
    trace ~raise self_mini_c_tracer @@ Self_mini_c.get_t_function e.type_expression
  in
  let expr : anon_function =
    trace ~raise self_mini_c_tracer @@ Self_mini_c.get_function_or_eta_expand e
  in
  let expr =
    { expr with
      body =
        trace ~raise self_mini_c_tracer @@ Self_mini_c.all_expression options expr.body
    }
  in
  let%map expr =
    (* TODO: is this correct? get Alan to check *)
    let expr =
      Lltz_codegen.compile_to_micheline
        (Ligo_lltz_codegen.compile_expression expr.body)
        []
    in
    Lwt.return
      (Micheline.map_node
         (fun _ -> dummy)
         (fun prim -> Michelson.Ast.Prim.to_string prim)
         expr)
  in
  let expr =
    Self_michelson.optimize
      ~experimental_disable_optimizations_for_debugging:
        options.backend.experimental_disable_optimizations_for_debugging
      ~has_comment:(has_comment options)
      expr
  in
  let expr_ty = compile_type e.type_expression in
  ({ expr_ty; expr } : Program.compiled_expression)
