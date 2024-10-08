open Core
open Lsp_helpers
open Handler
open Cst_shared.Fold
open Ligo_interface
module Range_set = Set.Make (Range)
module Loc = Simple_utils.Location
module Region = Simple_utils.Region

type syntax_node =
  (Cst_cameligo.Fold.some_node, Cst_jsligo.Fold.some_node) Dialect_cst.dialect

(** Checks that the given CST node is located inside the given range. *)
let is_node_in_range (selection_range : Range.t)
    : syntax_node -> 'a Cst_shared.Fold.fold_control option
  =
  let open Cst_shared.Fold in
  let get_fold_control (region : Region.t) : 'a fold_control =
    let range = Range.of_region region in
    if Range.intersects range selection_range then Skip else Stop
  in
  Dialect_cst.from_dialect
    { cameligo =
        (let open Cst_cameligo.Fold in
        fun (Some_node (value, sing)) ->
          match sing, value with
          | S_reg _, reg -> Some (get_fold_control reg.region)
          | S_wrap _, wrap -> Some (get_fold_control wrap#region)
          | _ -> None)
    ; jsligo =
        (let open Cst_jsligo.Fold in
        fun (Some_node (value, sing)) ->
          match sing, value with
          | S_reg _, reg -> Some (get_fold_control reg.region)
          | S_wrap _, wrap -> Some (get_fold_control wrap#region)
          | _ -> None)
    }

(** An auxiliary type which represents a value
    for function definition in [LMap.t]. *)
type fun_def_value =
  { position : Position.t (** A shifted position for inlay hint. *)
  ; num_of_binders : int
        (** A number which represents how many binders this function definition has.
            It's used for removing redundant types in arrows.

            Suppose we have the next function definition in [CameLIGO]
            {[
              let f a b = a + b
            ]}
            [f] definition will have a [int -> int -> int] type.
            In inlay hint we want to display it like [: int] since the other 2 [int]s
            already represented in its arguments. *)
  }

(** A helper signature to deal with differences between JsLIGO and CameLIGO CSTs. *)
module type Inlay_hint_syntax = sig
  type some_node
  type cst
  type 'a instruction = some_node -> 'a fold_control

  val wrap_node : some_node -> syntax_node

  (** See {!Cst_cameligo.Fold} and {!Cst_jsligo.Fold}. *)
  val fold_cst : 'b -> ('b -> 'a -> 'b) -> 'a instruction -> cst -> 'b

  (** Extracts [fun_def_value] for all function variables definitions. *)
  val function_definitions : (Loc.t * fun_def_value) instruction

  (** Extracts ranges where inlay hints wouldn't be provided.
      It could be some explicit types (e.g. indices in [CameLIGO]'s for-loops)
      or arguments that appear in some error-recovered positions. *)
  val ban_definitions : parse_error_ranges:Range.t list -> Range.t instruction

  (** Extracts ranges for definitions that require parenthesis.
      For example:
      {[
        let f a b = a + b
      ]}
      will have the next inlay hints:
      {[
        let f (a: int) (b: int): int = a + b
      ]}
      [a] and [b] variables require parenthesis in this case. *)
  val need_parenthesis_definitions : Range.t list instruction

  (** Extracts ranges for definitions with core types that don't need inlay hints.
      By default we provide hints for all resolved types and for some core ones.

      For example, a binder for recursive function always has a core annotation
      regardless of whether it has it in code or not. *)
  val banned_for_core : Range.t instruction
end

(** Info obtained from [Inlay_hint_syntax]. *)
type inlay_hint_info =
  { fun_defs : fun_def_value Location.Map.t
  ; ban_defs : Range.t list
  ; need_par_defs : Range_set.t
  ; banned_in_core : Range.t list
  }

module Inlay_hint_provider_Make (S : Inlay_hint_syntax) : sig
  (** Provides all the necessary info for generating inlay hints. *)
  val provide_inlay_hint_info
    :  parse_error_ranges:Range.t list
    -> Range.t
    -> S.cst
    -> inlay_hint_info
end = struct
  let select_in_range (selection_range : Range.t) : 'a S.instruction -> 'a S.instruction =
   fun instruction node ->
    Option.value_or_thunk
      (is_node_in_range selection_range (S.wrap_node node))
      ~default:(fun () -> instruction node)

  let function_definitions (selection_range : Range.t)
      : S.cst -> fun_def_value Location.Map.t
    =
    S.fold_cst
      Location.Map.empty
      (fun acc (key, data) -> Map.set ~key ~data acc)
      (select_in_range selection_range S.function_definitions)

  let ban_definitions ~(parse_error_ranges : Range.t list) (selection_range : Range.t)
      : S.cst -> Range.t list
    =
    S.fold_cst
      []
      (fun acc range -> range :: acc)
      (select_in_range selection_range (S.ban_definitions ~parse_error_ranges))

  let need_parenthesis_definitions (selection_range : Range.t) : S.cst -> Range_set.t =
    S.fold_cst
      Range_set.empty
      (fun acc ranges -> Set.union (Range_set.of_list ranges) acc)
      (select_in_range selection_range S.need_parenthesis_definitions)

  let banned_for_core (selection_range : Range.t) : S.cst -> Range.t list =
    S.fold_cst
      []
      (fun acc range -> range :: acc)
      (select_in_range selection_range S.banned_for_core)

  let provide_inlay_hint_info
      ~(parse_error_ranges : Range.t list)
      (selection_range : Range.t)
      : S.cst -> inlay_hint_info
    =
   fun cst ->
    let fun_defs = function_definitions selection_range cst in
    let ban_defs = ban_definitions ~parse_error_ranges selection_range cst in
    let need_par_defs = need_parenthesis_definitions selection_range cst in
    let banned_in_core = banned_for_core selection_range cst in
    { fun_defs; ban_defs; need_par_defs; banned_in_core }
end

module Cameligo_inlay_hint_provider = Inlay_hint_provider_Make (struct
  open Cst_cameligo.Fold
  open Cst_cameligo.CST

  type nonrec some_node = some_node
  type nonrec cst = cst
  type 'a instruction = some_node -> 'a fold_control

  let wrap_node node = Dialect_cst.CameLIGO node
  let fold_cst = fold_cst

  let function_definitions (Some_node (value, sing))
      : (Loc.t * fun_def_value) fold_control
    =
    match sing, value with
    | S_let_binding, let_binding when Option.is_none let_binding.rhs_type ->
      (match let_binding.binders with
      | P_Var v :: _ ->
        let variable_loc = Loc.lift @@ variable_to_region v in
        let end_position =
          let end_pos_type_opt =
            Option.map let_binding.type_params ~f:(fun params ->
                Position.of_pos params.region#stop)
          in
          let end_pos_binders =
            Position.of_pos
              (pattern_to_region @@ Nonempty_list.last let_binding.binders)#stop
          in
          Option.value_map
            end_pos_type_opt
            ~default:end_pos_binders
            ~f:(fun end_pos_type ->
              if Position.(end_pos_type <= end_pos_binders)
              then end_pos_binders
              else end_pos_type)
        in
        Continue
          ( variable_loc
          , { position = end_position
            ; num_of_binders = Nonempty_list.length let_binding.binders - 1
            } )
      | _ -> Skip)
    | S_expr, E_Fun { region; value = fun_expr } when Option.is_none fun_expr.rhs_type ->
      let end_position =
        Position.of_pos (pattern_to_region @@ Nonempty_list.last fun_expr.binders)#stop
      in
      Continue (Loc.lift region, { position = end_position; num_of_binders = 0 })
    | _ -> Skip

  let ban_definitions ~parse_error_ranges (Some_node (value, sing)) : Range.t fold_control
    =
    let check_range range =
      if List.exists parse_error_ranges ~f:(Range.intersects range)
      then Continue range
      else Skip
    in
    match sing, value with
    | S_for_loop, loop -> Continue (Range.of_region @@ variable_to_region loop.index)
    | S_let_decl, (kwd_let, _, binding)
    | S_let_in, { kwd_let; binding = { value = binding; _ }; _ }
    | S_let_mut_in, { kwd_let; binding = { value = binding; _ }; _ } ->
      let range = Range.of_region @@ Region.cover kwd_let#region binding.eq#region in
      check_range range
    | S_let_binding, let_binding when Option.is_some let_binding.rhs_type ->
      (match let_binding.binders with
      | P_Var v :: _ -> Continue (Range.of_region @@ variable_to_region v)
      | _ -> Skip)
    | S_fun_expr, { kwd_fun; arrow; _ } ->
      let range = Range.of_region @@ Region.cover kwd_fun#region arrow#region in
      check_range range
    | _ -> Skip

  let need_parenthesis_definitions (Some_node (value, sing)) : Range.t list fold_control =
    match sing, value with
    | S_let_binding, { binders; _ } | S_expr, E_Fun { value = { binders; _ }; _ } ->
      let binders = Nonempty_list.to_list binders in
      let interesting_ranges =
        List.filter_map binders ~f:(function
            | P_Var v -> Some (Range.of_region @@ variable_to_region v)
            | _ -> None)
      in
      Continue interesting_ranges
    | _ -> Skip

  let banned_for_core (Some_node (value, sing)) : Range.t fold_control =
    let rec is_var_pat = function
      | P_Var _ -> true
      | P_Par inner -> is_var_pat inner.value.inside
      | _ -> false
    in
    match sing, value with
    | S_let_binding, { rhs_type; binders = name :: _; _ }
      when Option.is_some rhs_type && is_var_pat name ->
      let reg = pattern_to_region name in
      Continue (Range.of_region reg)
    | S_field_decl, { field_type; field_name; _ } when Option.is_some field_type ->
      let reg = variable_to_region field_name in
      Continue (Range.of_region reg)
    | S_typed_pattern, (pat, _) ->
      let reg = pattern_to_region pat in
      Continue (Range.of_region reg)
    | S_typed_expr, (expr, _) ->
      let reg = expr_to_region expr in
      Continue (Range.of_region reg)
    | S_sig_value, { kwd_val; var; _ } ->
      let reg = Region.cover kwd_val#region @@ variable_to_region var in
      Continue (Range.of_region reg)
    | _ -> Skip
end)

module Jsligo_inlay_hint_provider = Inlay_hint_provider_Make (struct
  open Cst_jsligo.Fold
  open Cst_jsligo.CST

  type nonrec some_node = some_node
  type nonrec cst = cst
  type 'a instruction = some_node -> 'a fold_control

  let wrap_node node = Dialect_cst.JsLIGO node
  let fold_cst = fold_cst

  let function_definitions (Some_node (value, sing))
      : (Loc.t * fun_def_value) fold_control
    =
    match sing, value with
    | S_fun_decl, { rhs_type; fun_name; parameters; _ } when Option.is_none rhs_type ->
      let name_location = Loc.lift @@ variable_to_region fun_name in
      let end_position = Position.of_pos parameters.region#stop in
      let num_of_binders =
        (* The type of [function f() {...}] is [unit => a] which is not the correct inlay hint.
           Let's handle this by adding [max 1] to this. *)
        max 1 (List.length @@ Utils.sep_or_term_to_list parameters.value.inside)
      in
      Continue (name_location, { position = end_position; num_of_binders })
    | S_expr, E_ArrowFun { region; value = { rhs_type; parameters; _ } }
    | S_expr, E_Function { region; value = { rhs_type; parameters; _ } }
      when Option.is_none rhs_type ->
      let reg =
        match parameters with
        | ParParams params -> params.region
        | NakedParam param -> pattern_to_region param
      in
      let fun_loc = Loc.lift region in
      let end_position = Position.of_pos reg#stop in
      Continue (fun_loc, { position = end_position; num_of_binders = 0 })
    | _ -> Skip

  let ban_definitions ~(parse_error_ranges : Range.t list) (Some_node (value, sing))
      : Range.t fold_control
    =
    ignore parse_error_ranges;
    match sing, value with
    | S_fun_decl, { rhs_type; fun_name; _ } when Option.is_some rhs_type ->
      Continue (Range.of_region @@ variable_to_region fun_name)
    | _ -> Skip

  let need_parenthesis_definitions (Some_node (value, sing)) : Range.t list fold_control =
    match sing, value with
    | S_arrow_fun_params, arrow_fun_params ->
      (match arrow_fun_params with
      | ParParams _ -> Skip
      | NakedParam param ->
        let reg = pattern_to_region param in
        Continue [ Range.of_region reg ])
    | _ -> Skip

  let banned_for_core (Some_node (value, sing)) : Range.t fold_control =
    match sing, value with
    | S_fun_decl, { rhs_type; fun_name; _ } when Option.is_some rhs_type ->
      let reg = variable_to_region fun_name in
      Continue (Range.of_region reg)
    | S_val_binding, { rhs_type; pattern; _ } when Option.is_some rhs_type ->
      let reg = pattern_to_region pattern in
      Continue (Range.of_region reg)
    | S_fun_type_param, (pat, _) | S_typed_pattern, (pat, _) ->
      let reg = pattern_to_region pat in
      Continue (Range.of_region reg)
    | S_arrow_fun_expr, { rhs_type; parameters; _ }
    | S_function_expr, { rhs_type; parameters; _ }
      when Option.is_some rhs_type ->
      let reg =
        match parameters with
        | NakedParam pat -> pattern_to_region pat
        | ParParams fun_params -> fun_params.region
      in
      Continue (Range.of_region reg)
    | S_intf_const, { kwd_const; const_name; _ } ->
      let reg = Region.cover kwd_const#region @@ variable_to_region const_name in
      Continue (Range.of_region reg)
    | _ -> Skip
end)

(** Provides all the necessary info for generating inlay hints. *)
let provide_inlay_hint_info
    ~(parse_error_ranges : Range.t list)
    (selection_range : Range.t)
    : Dialect_cst.t -> inlay_hint_info
  = function
  | CameLIGO cst ->
    Cameligo_inlay_hint_provider.provide_inlay_hint_info
      ~parse_error_ranges
      selection_range
      cst
  | JsLIGO cst ->
    Jsligo_inlay_hint_provider.provide_inlay_hint_info
      ~parse_error_ranges
      selection_range
      cst

(** In most cases we want to display hints right next to the definition.
    However, for function definitions it's not true.
    For example:
    {[
      let f a b = a + b
    ]}
    For [f] definition we want to show hint right after the [b] definition.
    So, we should account that. *)
type hint_position_kind =
  | Original of Range.t
  | Shifted of Position.t
[@@deriving compare]

(* Note that the order of constructors in [hint_position_kind] matters.
   LSP spec says that hints with the same positions would be shown
   in the same order they appear in the response. So, in order not to mess up
   them with shifted ones we will sort them. *)

(** Intermediate representation of inlay hint *)
type inlay_hint =
  { position : hint_position_kind
  ; typ : Ast_core.type_expression [@compare.ignore]
  }
[@@deriving compare]

(** Anonymous functions is an exceptional case because they don't have
    binders for their types.

    [lambda_types] map stores these function types associated to the whole lambda body locations.
    Using [fun_defs] we'll resolve the lambda body location to its shifted position. *)
let provide_hints_for_lambda_types
    ~(lambda_types : Ast_typed.type_expression Location.Map.t)
    ~(fun_defs : fun_def_value Location.Map.t)
  =
  Map.fold lambda_types ~init:[] ~f:(fun ~key:loc ~data:typ acc ->
      Option.value ~default:acc
      @@ Option.bind (Map.find fun_defs loc) ~f:(fun { position; _ } ->
             let%map.Option typ =
               try
                 Trace.to_option ~fast_fail:false
                 @@ Checking.untype_type_expression ~use_orig_var:true typ
               with
               | _exn -> None
             in
             { position = Shifted position; typ } :: acc))

(** Provides an inlay hint for the given variable definition. *)
let process_variable_def
    ~(banned_in_core : Range.t list)
    ~(fun_defs : fun_def_value Location.Map.t)
    ~(ban_defs : Range.t list)
    ~(syntax : Syntax_types.t)
    : Scopes.Types.vdef -> inlay_hint option
  =
  let is_ghost = function
    | Ast_core.T_variable var ->
      let open Ligo_prim in
      (not @@ Type_var.is_generated var)
      && (Parsing.Errors.ErrorWrapper.is_wrapped @@ Type_var.to_name_exn var)
    | _ -> false
  in
  let open Option.Let_syntax in
  let process_variable (range : Loc.t) (typ : Ast_core.ty_expr) =
    let%bind file_range = Loc.get_file range in
    let file_range = Range.of_region file_range in
    if is_ghost typ.type_content
    then (
      let typ_content =
        Ast_core.T_variable
          (Lsp_helpers.Helpers_pretty.unresolved_type_as_comment syntax
          |> Ligo_prim.Type_var.of_input_var ~loc:Location.dummy)
      in
      Some
        { position = Original file_range; typ = { typ with type_content = typ_content } })
    else (
      match Map.find fun_defs range with
      | Some { position; num_of_binders } ->
        let rec strip_args (typ : Ast_core.ty_expr) = function
          | 0 -> Some typ
          | n ->
            (match typ.type_content with
            | T_arrow { type2; _ } -> strip_args type2 (n - 1)
            | T_abstraction { type_; _ } | T_for_all { type_; _ } -> strip_args type_ n
            | _ -> None)
        in
        let%bind typ = strip_args typ num_of_binders in
        Option.some_if
          (not @@ List.exists ban_defs ~f:(Range.contains_position position))
          { position = Shifted position; typ }
      | None ->
        let%map () =
          Option.some_if (not @@ List.exists ban_defs ~f:(Range.intersects file_range)) ()
        in
        { position = Original file_range; typ })
  in
  function
  | { range; t = Resolved typ; _ } ->
    let%bind typ =
      try
        Trace.to_option ~fast_fail:false
        @@ Checking.untype_type_expression ~use_orig_var:true typ
      with
      | _exn -> None
    in
    process_variable range typ
  | { range; t = Core typ; _ } ->
    let%bind region = Loc.get_file range in
    let range' = Range.of_region region in
    if (not @@ is_ghost typ.type_content)
       && List.exists banned_in_core ~f:(fun banned_range ->
              Range.inside ~big:banned_range ~small:range')
    then None
    else process_variable range typ
  | { t = Unresolved; _ } -> None

(** Computes inlay hints for variables with core types (i.e., unnanotated variables whose
    types that were inferred). *)
let provide_hints_for_variables
    ~(normalize : Path.normalization)
    ~(banned_in_core : Range.t list)
    ~(fun_defs : fun_def_value Location.Map.t)
    ~(ban_defs : Range.t list)
    ~(syntax : Syntax_types.t)
    (definitions : Def.definitions)
    (file : Path.t)
    : inlay_hint list
  =
  let vdefs =
    let open Scopes.Types in
    definitions
    |> Def.filter_map ~f:(function
           | Variable vdef ->
             (match vdef.decl_range with
             | File region ->
               Option.some_if (Path.equal (normalize region#file) file) vdef
             | Virtual _ -> None)
           | Type _ | Module _ | Label _ -> None)
       (* Sometimes we can see duplicate definitions (e.g. in JsLIGO's cycles).
          Since we're creating one inlay hint per definition this may result into duplicate hints ([x : int : int]). *)
    |> List.dedup_and_sort ~compare:(fun (lhs : (* WTF OCaml? *) vdef) rhs ->
           Uid.compare lhs.uid rhs.uid)
  in
  List.filter_map
    vdefs
    ~f:(process_variable_def ~banned_in_core ~fun_defs ~ban_defs ~syntax)

(** Compile our intermediate representation of inlay hint into [InlayHint.t]. *)
let compile_inlay_hints ~(syntax : Syntax_types.t) ~(need_par_defs : Range_set.t)
    : inlay_hint list -> InlayHint.t list
  =
  List.concat_map ~f:(fun { position; typ } ->
      let lpar, rpar =
        match position with
        | Shifted _ -> None, None
        | Original reg ->
          if Set.mem need_par_defs reg
          then
            InlayHint.(
              ( Some (create ~label:(`String "(") ~position:reg.start ())
              , Some (create ~label:(`String ")") ~position:reg.end_ ()) ))
          else None, None
      in
      let position =
        match position with
        | Original reg -> reg.end_
        | Shifted pos -> pos
      in
      let typ =
        Pretty.show_type ~doc_to_string:Helpers_pretty.doc_to_compact_string ~syntax typ
      in
      let typ = String.map typ ~f:(fun ch -> if Char.equal '\n' ch then ' ' else ch) in
      let label = `String (": " ^ typ) in
      List.filter_map
        ~f:Fn.id
        [ lpar; Some (InlayHint.create ~label ~kind:Type ~position ()); rpar ])
  <@ List.sort ~compare:compare_inlay_hint

let on_req_inlay_hint (file : Path.t) (range : Range.t)
    : InlayHint.t list option Handler.t
  =
  let open Handler.Let_syntax in
  with_cst file ~default:None
  @@ fun cst ->
  with_cached_doc file ~default:None
  @@ fun { definitions; syntax; parse_error_ranges; lambda_types; _ } ->
  let { fun_defs; need_par_defs; ban_defs; banned_in_core } =
    provide_inlay_hint_info ~parse_error_ranges range cst
  in
  let inlay_hints = provide_hints_for_lambda_types ~lambda_types ~fun_defs in
  let%bind normalize = ask_normalize in
  let inlay_hints =
    inlay_hints
    @ provide_hints_for_variables
        ~normalize
        ~banned_in_core
        ~fun_defs
        ~ban_defs
        ~syntax
        definitions
        file
  in
  return @@ Some (compile_inlay_hints ~syntax ~need_par_defs inlay_hints)
