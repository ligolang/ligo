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
  ; entrypoint : string option
  ; michelson_entrypoint : string option
  ; log_dir : string option
  ; contract_env : contract_env option
  }
[@@deriving yojson_of]

let build_expression ~raise
    :  options:Compiler_options.t -> Syntax_types.t -> string -> program
    -> Stacking.compiled_expression * type_expression
  =
 fun ~options syntax expression init_prg ->
  let typed_exp =
    Ligo_compile.Utils.type_expression
      ~raise
      ~options
      syntax
      expression
      (Ast_typed.Misc.to_signature init_prg)
  in
  let aggregated =
    Ligo_compile.Of_typed.compile_expression_in_context
      ~raise
      ~options:options.middle_end
      init_prg
      typed_exp
  in
  let expanded_exp = Ligo_compile.Of_aggregated.compile_expression ~raise aggregated in
  let mini_c_exp = Ligo_compile.Of_expanded.compile_expression ~raise expanded_exp in
  let stacking_exp =
    Ligo_compile.Of_mini_c.compile_expression ~raise ~options mini_c_exp
  in
  stacking_exp, typed_exp.type_expression


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
      | PascaLIGO ->
        let open Unification.Pascaligo in
        let open Parsing.Pascaligo in
        let cst = decompile_ty_expr unified_type_expr in
        Pretty.print_type_expr Pretty.default_state cst
    in
    PPrint.ToFormatter.compact f pp
  with
  | _ -> Ast_typed.PP.type_expression f type_expr


let resolve_config ~raise ~options syntax init_file =
  let init_prg =
    let f : BuildSystem.Source_input.file_name -> Ast_typed.program =
     fun filename ->
      Build.qualified_typed ~raise ~options (BuildSystem.Source_input.From_file filename)
    in
    let default = Build.Stdlib.select_lib_typed syntax (Build.Stdlib.get ~options) in
    Option.value_map (Some init_file) ~f ~default
  in
  let get_field_opt field =
    try
      Some
        (let expression, typ =
           build_expression ~raise ~options syntax ("config." ^ field) init_prg
         in
         let options =
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
         ( Compile.no_comment
           @@ Run.evaluate_expression ~raise ~options expression.expr expression.expr_ty
         , typ ))
    with
    | _ -> None
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
  let parameter = Option.map ~f:fst @@ get_field_opt "parameter" in
  let storage = Option.map ~f:fst @@ get_field_opt "storage" in
  let program = Option.map ~f:(extract_string "program") @@ get_field_opt "program" in
  let entrypoint =
    Option.map ~f:(extract_string "entrypoint") @@ get_field_opt "entrypoint"
  in
  let michelson_entrypoint =
    Option.map ~f:(extract_string "michelson_entrypoint")
    @@ get_field_opt "michelson_entrypoint"
  in
  let log_dir = Option.map ~f:(extract_string "log_dir") @@ get_field_opt "log_dir" in
  let contract_env =
    let open Option.Let_syntax in
    let%bind _ = get_field_opt "contract_env" in
    let get_field_opt field = get_field_opt @@ "contract_env." ^ field in
    let now = Option.map ~f:(extract_string "now") @@ get_field_opt "now" in
    let level = Option.map ~f:(extract_int "level") @@ get_field_opt "level" in
    let sender = Option.map ~f:(extract_string "sender") @@ get_field_opt "sender" in
    let source = Option.map ~f:(extract_string "source") @@ get_field_opt "source" in
    let self = Option.map ~f:(extract_string "self") @@ get_field_opt "self" in
    let amount = Option.map ~f:(extract_int "amount") @@ get_field_opt "amount" in
    let balance = Option.map ~f:(extract_int "balance") @@ get_field_opt "balance" in
    let chain_id =
      Option.map ~f:(extract_string "chain_id") @@ get_field_opt "chain_id"
    in
    let voting_powers =
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
      match%bind get_field_opt "voting_powers" with
      | Seq (_, lst), typ ->
        let map_inner_type =
          match typ.type_content with
          | T_constant { parameters = [ parameter ]; _ } -> parameter
          | T_constant { parameters = _ :: _ as parameters; _ } ->
            make_t ~loc:Location.generated @@ T_record (Row.create_tuple parameters)
          | _ ->
            raise.error
            @@ `Resolve_config_corner_case "expected map type in 'voting_powers'"
        in
        return @@ Simple (List.map ~f:(get_voting_power map_inner_type) lst)
      | _, typ ->
        raise.error
        @@ `Resolve_config_type_mismatch
             ( "voting_powers"
             , "map (address, nat)"
             , typ
             , pp_type_expression ~raise ~syntax )
    in
    return { now; level; sender; source; self; amount; balance; chain_id; voting_powers }
  in
  { parameter; storage; program; entrypoint; michelson_entrypoint; log_dir; contract_env }


let config_format : config Display.format =
  let config_format_pp ~display_format ~no_colour f config =
    ignore (display_format, no_colour, config);
    Format.fprintf f "No human-readable format. Use --format json"
  in
  { pp = config_format_pp; to_json = yojson_of_config }
