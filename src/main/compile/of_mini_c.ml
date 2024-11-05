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
  dummy_locations (To_micheline.translate_type expr_ty)


let compile_contract ~raise
    : options:Compiler_options.t -> expression -> Stacking.compiled_expression Lwt.t
  =
 fun ~options e_contract ->
  let open Lwt.Let_syntax in
  let input_ty, contract = optimize_for_contract ~raise options e_contract in

  let optimize = 
    function expr -> 
        Self_michelson.optimize
          ~experimental_disable_optimizations_for_debugging:
            options.backend.experimental_disable_optimizations_for_debugging
          ~has_comment:(has_comment options)
          expr
  in
  let%map expr =
    if options.backend.lltz_ir then
      let e_simplified = trace ~raise self_mini_c_tracer @@ Self_mini_c.all_expression options contract.body in

      let expr_unoptimised = 
        let (Var lltz_var, lltz_ty, lltz_body) = Ligo_lltz_codegen.compile_contract contract.binder input_ty e_simplified in
        Lltz_codegen.compile_contract_to_micheline ~optimize:false lltz_var lltz_ty lltz_body [] in
      let expr_unoptimised = Micheline.map_node (fun _ -> dummy) (fun prim -> Michelson.Ast.Prim.to_string prim) expr_unoptimised in 
      
      let expr_optimised = 
        let (Var lltz_var, lltz_ty, lltz_body) = Ligo_lltz_codegen.compile_contract contract.binder input_ty e_simplified in
        Lltz_codegen.compile_contract_to_micheline ~optimize:true lltz_var lltz_ty lltz_body [] in
      let expr_optimised = Micheline.map_node (fun _ -> dummy) (fun prim -> Michelson.Ast.Prim.to_string prim) expr_optimised in 

      let%bind size_optimised = Of_michelson.measure ~raise (expr_optimised) in
      let%bind size_unoptimised = Of_michelson.measure ~raise expr_unoptimised in

      (* Assert that size with lltz is smaller than without *)
      if size_optimised > size_unoptimised then
        assert false
      else
        Lwt.return expr_optimised
    else
      let de_bruijn =
        trace ~raise scoping_tracer @@ Scoping.translate_closed_function contract input_ty
      in
      let%bind expr = Stacking.Program.compile_function_body de_bruijn in
      let optimised_expr = optimize expr in

      Lwt.return optimised_expr
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
  let de_bruijn =
    trace ~raise scoping_tracer @@ Scoping.translate_closed_function view input_ty
  in
  let%map de_bruijn = Stacking.Program.compile_function_body de_bruijn in
  let expr =
    Self_michelson.optimize
      ~experimental_disable_optimizations_for_debugging:
        options.backend.experimental_disable_optimizations_for_debugging
      ~has_comment:(has_comment options)
      de_bruijn
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

      let optimize = 
        function expr -> 
            Self_michelson.optimize
              ~experimental_disable_optimizations_for_debugging:
                options.backend.experimental_disable_optimizations_for_debugging
              ~has_comment:(has_comment options)
              expr
      in

      let%map expr = 
        if options.backend.lltz_ir then
          let expr_lltz_opt = Lltz_codegen.compile_to_micheline ~optimize:true (Ligo_lltz_codegen.compile_expression e) [] in
          let expr_optimised =
            Micheline.map_node (fun _ -> dummy) (fun prim -> Michelson.Ast.Prim.to_string prim) expr_lltz_opt
          in

          let expr_lltz_unopt = Lltz_codegen.compile_to_micheline ~optimize:false (Ligo_lltz_codegen.compile_expression e) [] in
          let expr_unoptimised =
            Micheline.map_node (fun _ -> dummy) (fun prim -> Michelson.Ast.Prim.to_string prim) expr_lltz_unopt
          in

          let%bind size_optimised = Of_michelson.measure ~raise expr_optimised in
          let%bind size_unoptimised = Of_michelson.measure ~raise expr_unoptimised in

          (* Assert that size with lltz michelson optimisations is smaller than without *)
          if size_optimised > size_unoptimised then
            assert false
          else
            Lwt.return expr_optimised
        else
          let expr = trace ~raise scoping_tracer @@ Scoping.translate_expression e [] in
          let%bind expr = Stacking.Program.compile_expr [] expr in
          Lwt.return (optimize expr)
      in
        let expr_ty = compile_type e.type_expression in
        (({ expr_ty; expr } : Program.compiled_expression))

let compile_expression_function ~raise
    : options:Compiler_options.t -> expression -> compiled_expression Lwt.t
  =
 fun ~options e ->
  let open Lwt.Let_syntax in
  let input_ty, _ =
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
  let de_bruijn =
    trace ~raise scoping_tracer @@ Scoping.translate_closed_function expr input_ty
  in
  let%map de_bruijn = Stacking.Program.compile_function_body de_bruijn in
  let expr =
    Self_michelson.optimize
      ~experimental_disable_optimizations_for_debugging:
        options.backend.experimental_disable_optimizations_for_debugging
      ~has_comment:(has_comment options)
      de_bruijn
  in
  let expr_ty = compile_type e.type_expression in
  ({ expr_ty; expr } : Program.compiled_expression)
