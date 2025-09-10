open Main_errors
open Mini_c
open Proto_alpha_utils
open Trace
open Stacking
open Tezos_micheline
open Ligo_lltz_codegen
open Lltz_codegen

let dummy : Mini_c.meta =
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


let compile_type ty =
  let ty = Ligo_lltz_codegen.compile_type_expression ty in
  let ty = Lltz_codegen.convert_type ty in
  Micheline.map_node (fun _ -> dummy) (fun prim -> Michelson.Ast.Prim.to_string prim) ty


(* TODO: LLTZ from here *)
let lambda_body ~environment ~lam_var ~return_type return =
  let open Lltz_codegen in
  let open Instruction in
  (* TODO: export this *)
  let module Config = Lltz_codegen__Config in
  let n = List.length environment + 1 in
  let environment_slots = List.map environment ~f:(fun (ident, _) -> `Ident ident) in
  let parameter_slot = `Ident (fst lam_var) in
  let lambda_stack = [ `Value ] in
  let { Config.stack = _; instructions } =
    let defined_slots = environment_slots @ [ parameter_slot ] in
    seq
      [ unpair_n n
      ; Slot.def_all defined_slots ~in_:return
      ; Slot.collect_all defined_slots
      ]
      lambda_stack
  in
  instructions


let compile_lambda var var_type ret_type body =
  let open Lltz_codegen in
  (* TODO: this is copied from LLTZ *)
  let lam_var = var, convert_type var_type in
  let return_type = convert_type ret_type in
  let environment = LLTZ.Free_vars.free_vars_with_types body in
  let environment = Map.remove environment var in
  lambda_body
    ~environment:(environment |> Map.map ~f:convert_type |> Map.to_alist)
    ~lam_var
    ~return_type
    (compile body)


let compile_function_body ~var_ty expr =
  let var_ty = Ligo_lltz_codegen.compile_type_expression var_ty in
  let Var var, ret_ty, expr = Ligo_lltz_codegen.compile_function expr in
  let expr = Last_vars.compute_last_vars expr in
  let compiled = compile_lambda var var_ty ret_ty expr in
  let micheline = Michelson.Ast.seq compiled in
  (* TODO: strip annotations? *)
  let strip_annots = true in
  Michelson_optimisations.Rewriter.optimise_micheline ~strip_annots micheline


(* TODO: LLTZ to here *)

let compile_contract ~raise
    : options:Compiler_options.t -> expression -> compiled_expression Lwt.t
  =
 fun ~options e_contract ->
  let input_ty, contract = optimize_for_contract ~raise options e_contract in
  (* Compile without IR *)
  (* Return the value as before *)
  let expr_to_return =
    (* Compile with IR *)
    let e_optimised =
      trace ~raise self_mini_c_tracer @@ Self_mini_c.all_expression options contract.body
    in
    let expr =
      let Var lltz_var, lltz_ty, lltz_body =
        Ligo_lltz_codegen.compile_contract contract.binder input_ty e_optimised
      in
      Lltz_codegen.compile_contract_to_micheline lltz_var lltz_body []
    in
    Micheline.map_node
      (fun _ -> dummy)
      (fun prim -> Michelson.Ast.Prim.to_string prim)
      expr
  in
  let expr_ty = compile_type e_contract.type_expression in
  let expr_ty = dummy_locations expr_ty in
  Lwt.return { expr_ty; expr = expr_to_return }


let compile_view ~raise
    : options:Compiler_options.t -> expression -> compiled_expression Lwt.t
  =
 fun ~options e ->
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
  let expr = compile_function_body ~var_ty:input_ty view in
  let expr =
    Micheline.map_node
      (fun _ -> dummy)
      (fun prim -> Michelson.Ast.Prim.to_string prim)
      expr
  in
  let l, r = trace ~raise self_mini_c_tracer @@ Self_mini_c.get_t_pair input_ty in
  let l = compile_type l in
  let r = compile_type r in
  let input_ty = Micheline.(Prim (dummy, "pair", [ l; r ], [])) in
  let output_ty = compile_type output_ty in
  let expr_ty = Micheline.(Prim (dummy, "lambda", [ input_ty; output_ty ], [])) in
  let expr_ty = dummy_locations expr_ty in
  Lwt.return { expr_ty; expr }


let compile_expression ~raise
    : options:Compiler_options.t -> expression -> compiled_expression Lwt.t
  =
 fun ~options e ->
  (* Preprocess the expression using Self_mini_c. *)
  let e = trace ~raise self_mini_c_tracer @@ Self_mini_c.all_expression options e in
  let expr =
    Lltz_codegen.compile_to_micheline (Ligo_lltz_codegen.compile_expression e) []
  in
  let expr =
    Micheline.map_node
      (fun _ -> dummy)
      (fun prim -> Michelson.Ast.Prim.to_string prim)
      expr
  in
  let expr_ty = compile_type e.type_expression in
  Lwt.return { expr_ty; expr }


let compile_expression_function ~raise
    : options:Compiler_options.t -> expression -> compiled_expression Lwt.t
  =
 fun ~options e ->
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
  (* TODO: is this `Ident the correct usage? *)
  let expr = compile_function_body ~var_ty:input_ty expr in
  let expr =
    Micheline.map_node
      (fun _ -> dummy)
      (fun prim -> Michelson.Ast.Prim.to_string prim)
      expr
  in
  let expr_ty = compile_type e.type_expression in
  Lwt.return { expr_ty; expr }
