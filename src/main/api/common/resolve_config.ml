module Display = Simple_utils.Display
open Ast_typed
module Run = Ligo_run.Of_michelson
module Trace = Simple_utils.Trace

type z = Z.t

let yojson_of_z (n : z) : Display.json = `String (Z.to_string n)

type voting_powers = Simple of (string * z) list

let yojson_of_voting_powers : voting_powers -> Display.json = function
  | Simple lst ->
    `Assoc
      [ "kind", `String "simple"
      ; "contents", `Assoc (List.map ~f:(fun (addr, n) -> addr, yojson_of_z n) lst)
      ]


type contract_env =
  { now : string option
  ; level : z option
  ; sender : string option
  ; source : string option
  ; self : string option
  ; amount : z option
  ; balance : z option
  ; chain_id : string option
  ; voting_powers : voting_powers option
  }
[@@deriving yojson_of]

type evaluated_michelson = (Mini_c.meta, string) Scoping.Micheline.node

let yojson_of_evaluated_michelson (em : evaluated_michelson) : Display.json =
  let open Tezos_utils.Michelson in
  let str_mich = Format.asprintf "%a" (pp_comment ?comment:None) em in
  `String str_mich


type config =
  { parameter : evaluated_michelson option
  ; storage : evaluated_michelson option
  ; program : string option
  ; module_name : string option
  ; entrypoint : string option
  ; log_dir : string option
  ; contract_env : contract_env option
  }
[@@deriving yojson_of]

let pp_type_expression ~raise ~syntax f type_expr =
  let core_type_expr = Checking.untype_type_expression type_expr in
  try
    let unified_type_expr =
      Trace.trace ~raise Main_errors.nanopasses_tracer
      @@ Nanopasses.decompile_ty_expr ~syntax core_type_expr
    in
    let pp =
      match syntax with
      | CameLIGO ->
        let open Unification.Cameligo in
        let open Parsing.Cameligo in
        let cst = decompile_ty_expr unified_type_expr in
        Pretty.print_type_expr Pretty.default_state cst
      | JsLIGO ->
        let open Unification.Jsligo in
        let open Parsing.Jsligo in
        let cst = decompile_ty_expr unified_type_expr in
        Pretty.print_type_expr Pretty.default_state cst
    in
    PPrint.ToFormatter.compact f pp
  with
  | _ -> Ast_typed.PP.type_expression f type_expr


let build_expression ~raise ?(check_config_type = false)
    :  options:Compiler_options.t -> Syntax_types.t -> string -> program
    -> (Stacking.compiled_expression * type_expression) Lwt.t
  =
 fun ~options syntax expression init_prg ->
  let open Lwt.Let_syntax in
  let typed_exp =
    Ligo_compile.Utils.type_expression ~raise ~options syntax expression init_prg.pr_sig
  in
  if check_config_type
  then (
    match typed_exp.type_expression.type_content with
    | T_record _ -> ()
    | _ ->
      raise.error
        (`Resolve_config_config_type_mismatch
          (typed_exp.type_expression, pp_type_expression ~raise ~syntax)));
  let aggregated =
    Ligo_compile.Of_typed.compile_expression_in_context
      ~raise
      ~options:options.middle_end
      None
      init_prg
      typed_exp
  in
  let expanded_exp = Ligo_compile.Of_aggregated.compile_expression ~raise aggregated in
  let locations_and_types_with_forall, _ =
    Ast_expanded.Helpers.fold_map_expression
      (fun acc expr ->
        let return a = true, a, expr in
        if Ast_expanded.equal_expr expr expanded_exp
        then return acc
        else (
          match expr.type_expression.type_content with
          | T_for_all _ -> return ((expr.location, expr.type_expression) :: acc)
          | _ -> return acc))
      []
      expanded_exp
  in
  let pp_type f (typ : Ast_expanded.type_expression) =
    match typ.source_type with
    | Some typ -> pp_type_expression ~raise ~syntax f typ
    | None -> Ast_expanded.PP.type_expression f typ
  in
  if not @@ List.is_empty locations_and_types_with_forall
  then
    raise.error (`Resolve_config_type_uncaught (locations_and_types_with_forall, pp_type));
  let mini_c_exp = Ligo_compile.Of_expanded.compile_expression ~raise expanded_exp in
  let%map stacking_exp =
    Ligo_compile.Of_mini_c.compile_expression ~raise ~options mini_c_exp
  in
  stacking_exp, typed_exp.type_expression


let resolve_config ~raise ~options syntax init_file : config Lwt.t =
  let open Lwt.Let_syntax in
  let init_prg =
    let f : BuildSystem.Source_input.file_name -> Ast_typed.program =
     fun filename ->
      Build.qualified_typed ~raise ~options (BuildSystem.Source_input.From_file filename)
    in
    let default = Build.Stdlib.select_lib_typed syntax (Build.Stdlib.get ~options) in
    Option.value_map (Some init_file) ~f ~default
  in
  (* Check that config is compiling... *)
  let%bind _ =
    build_expression ~raise ~check_config_type:true ~options syntax "config" init_prg
  in
  let get_field_opt field =
    Trace.try_with_lwt
      (fun ~raise ~catch ->
        let%bind expression, typ =
          build_expression ~raise ~options syntax ("config." ^ field) init_prg
        in
        let%bind options =
          Run.make_dry_run_options
            ~raise
            { now = None
            ; amount = "0"
            ; balance = "0"
            ; sender = None
            ; source = None
            ; parameter_ty = None
            }
        in
        let%bind result =
          Run.evaluate_expression ~raise ~options expression.expr expression.expr_ty
        in
        Lwt.return (Some (Compile.no_comment result, typ)))
      (fun ~catch -> function
        | (`Resolve_config_type_uncaught _ | `Resolve_config_config_type_mismatch _) as
          exn -> raise.error exn
        | _ -> Lwt.return None)
  in
  let extract_string (field : string) : evaluated_michelson * type_expression -> string
    = function
    | String (_, str), _ -> str
    | _, typ ->
      raise.error
      @@ `Resolve_config_type_mismatch
           (field, "string", typ, pp_type_expression ~raise ~syntax)
  in
  let extract_int (field : string) : evaluated_michelson * type_expression -> z = function
    | Int (_, n), _ -> n
    | _, typ ->
      raise.error
      @@ `Resolve_config_type_mismatch
           (field, "integral", typ, pp_type_expression ~raise ~syntax)
  in
  let get_field_opt ~f field = Lwt.map (Option.map ~f) @@ get_field_opt field in
  let%bind parameter = get_field_opt ~f:fst "parameter" in
  let%bind storage = get_field_opt ~f:fst "storage" in
  let%bind program = get_field_opt ~f:(extract_string "program") "program" in
  let%bind module_name = get_field_opt ~f:(extract_string "module_name") "module_name" in
  let%bind entrypoint = get_field_opt ~f:(extract_string "entrypoint") "entrypoint" in
  let%bind log_dir = get_field_opt ~f:(extract_string "log_dir") "log_dir" in
  let%map contract_env =
    match%bind get_field_opt ~f:Fn.id "contract_env" with
    | None -> Lwt.return None
    | Some _ ->
      let get_field_opt field = get_field_opt ~f:Fn.id @@ "contract_env." ^ field in
      let get_inner_field_result ~extractor field =
        Lwt.map (Option.map ~f:(extractor field)) (get_field_opt field)
      in
      let%bind now = get_inner_field_result ~extractor:extract_string "now" in
      let%bind level = get_inner_field_result ~extractor:extract_int "level" in
      let%bind sender = get_inner_field_result ~extractor:extract_string "sender" in
      let%bind source = get_inner_field_result ~extractor:extract_string "source" in
      let%bind self = get_inner_field_result ~extractor:extract_string "self" in
      let%bind amount = get_inner_field_result ~extractor:extract_int "amount" in
      let%bind balance = get_inner_field_result ~extractor:extract_int "balance" in
      let%bind chain_id = get_inner_field_result ~extractor:extract_string "chain_id" in
      let%bind voting_powers =
        let get_voting_power (elt_type : type_expression)
            : evaluated_michelson -> string * z
          = function
          | Prim (_, _, [ String (_, addr); Int (_, power) ], _) -> addr, power
          | _ ->
            raise.error
            @@ `Resolve_config_type_mismatch
                 ( "voting_powers.$elem"
                 , "(address, nat)"
                 , elt_type
                 , pp_type_expression ~raise ~syntax )
        in
        match%map get_field_opt "voting_powers" with
        | None -> None
        | Some (Seq (_, lst), typ) ->
          let map_inner_type =
            match typ.type_content with
            | T_constant { parameters = [ parameter ]; _ } -> parameter
            | T_constant { parameters = _ :: _ as parameters; _ } ->
              make_t ~loc:Location.generated @@ T_record (Row.create_tuple parameters)
            | _ ->
              raise.error
              @@ `Resolve_config_corner_case "expected map type in 'voting_powers'"
          in
          Some (Simple (List.map ~f:(get_voting_power map_inner_type) lst))
        | Some (_, typ) ->
          raise.error
          @@ `Resolve_config_type_mismatch
               ( "voting_powers"
               , "map (address, nat)"
               , typ
               , pp_type_expression ~raise ~syntax )
      in
      Lwt.return_some
        { now; level; sender; source; self; amount; balance; chain_id; voting_powers }
  in
  { parameter; storage; program; module_name; entrypoint; log_dir; contract_env }


let config_format : config Display.format =
  let config_format_pp ~display_format ~no_colour f config =
    ignore (display_format, no_colour, config);
    Format.fprintf f "No human-readable format. Use --format json"
  in
  { pp = config_format_pp; to_json = yojson_of_config }
