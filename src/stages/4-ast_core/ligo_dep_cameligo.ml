open Core
open Ligo_prim
module Location = Simple_utils.Location
module MSet = Set.Make (Module_var)

type acc =
  { deps : MSet.t
  ; scope : MSet.t
  }

let rec add_to_deps { deps; scope } var =
  if Set.mem scope var then deps else Set.add deps var


(** Collects external deps and builds up global module scope.
    Only decl affecting global scope is `D_module`, others are
    used only to collect deps *)
and collect_from_decl ({ deps; scope } as acc) decl =
  match Location.unwrap decl with
  | Types.D_module decl -> collect_from_module_decl acc decl
  | D_signature sig_decl ->
    let { deps; _ } = collect_from_signature_decl acc sig_decl in
    { acc with deps }
  | D_value decl ->
    let { deps; _ } = collect_from_value acc decl in
    { acc with deps }
  | D_irrefutable_match { pattern; expr; attr = _ } ->
    let { deps; _ } = collect_from_expr acc expr in
    let { deps; _ } = collect_from_pattern { deps; scope } pattern in
    { acc with deps }
  | D_type { type_expr; type_binder = _; type_attr = _ } ->
    let { deps; _ } = collect_from_ty_expr acc type_expr in
    { acc with deps }
  | D_module_include mod_expr ->
    let { deps; _ } = collect_from_mod_expr acc mod_expr in
    { acc with deps }
  (* Imports shouldn't be used for external deps anymore *)
  | D_import _ -> acc


(** Collects external deps from signature decl. *)
and collect_from_signature_decl
    ({ deps; scope } as acc)
    { signature_binder; signature; signature_attr = _ }
  =
  let { deps; _ } = collect_from_signature_expr acc signature in
  { acc with deps }


(** Collect external deps from signature expr. Singleton paths
    cannot be external. Other paths are external iff the
    first item of the path is external *)
and collect_from_signature_expr ({ deps; scope } as acc) sig_expr =
  let deps =
    match Location.unwrap sig_expr with
    (* It's not possible to be external being the module type *)
    | Types.S_path [ mvar ] -> deps
    (* Only the first module var from the path could be external *)
    | S_path (mvar :: _) -> add_to_deps acc mvar
    | S_sig signature ->
      (* We have to discard scope accumulated inside signature *)
      let { deps; _ } = collect_from_signature acc signature in
      deps
  in
  { acc with deps }


(** Collects external deps from signature.
    Built up scope gets discarded after folding it. *)
and collect_from_signature ({ deps; scope } as acc) Types.{ items } =
  (* We have to discard scope accumulated inside signature *)
  let { deps; _ } = List.fold items ~init:acc ~f:collect_from_sig_item in
  { acc with deps }


(** Collects external deps from sig items.
    Only decl affecting local signature scope is `S_module`.
    Other branches are used only to collect deps *)
and collect_from_sig_item ({ deps; scope } as acc) sig_item =
  match Location.unwrap sig_item with
  | Types.S_module (mvar, signature) ->
    let { deps; _ } = collect_from_signature acc signature in
    { deps; scope = Set.add scope mvar }
  | Types.S_value (_, ty, _) ->
    let { deps; _ } = collect_from_ty_expr acc ty in
    { acc with deps }
  | S_type (_, ty, _) ->
    let { deps; _ } = collect_from_ty_expr acc ty in
    { acc with deps }
  | S_type_var (_, _) -> acc
  | S_module_type (mvar, signature) ->
    let { deps; _ } = collect_from_signature acc signature in
    { acc with deps }
  | S_include sig_expr ->
    let { deps; _ } = collect_from_signature_expr acc sig_expr in
    { acc with deps }


(** Collects external deps from module decl. *)
and collect_from_module_decl
    ({ deps; scope } as acc)
    { module_binder; module_; annotation; module_attr = _ }
  =
  let deps =
    Option.value_map
      ~default:deps
      ~f:(fun annot ->
        let { deps; _ } = collect_from_signature_expr acc annot.signature in
        deps)
      annotation
  in
  collect_from_mod_in { acc with deps } module_binder module_


(** Collects external deps from `mod in` expression.
    It evaluates module expression in previous scope
    and adds its binder to the resulting scope *)
and collect_from_mod_in ({ deps; scope } as acc) binder mod_expr =
  let { deps; _ } = collect_from_mod_expr acc mod_expr in
  { deps; scope = Set.add scope binder }


(** Collects external deps from module expression. *)
and collect_from_mod_expr ({ deps; scope } as acc) mod_expr =
  let module_ = Location.unwrap mod_expr in
  let deps =
    match module_ with
    | Module_expr.M_struct decls ->
      (* Discarding scope accumulated inside module struct *)
      let { deps; _ } = List.fold decls ~init:acc ~f:collect_from_decl in
      deps
    | M_variable var -> add_to_deps acc var
    | M_module_path (var :: _) -> add_to_deps acc var
  in
  { acc with deps }


(** Collects external deps from value decl. *)
and collect_from_value
    ({ deps; scope } as acc)
    { binder = { ascr; var = _ }; expr; attr = _ }
  =
  let { deps; _ } = collect_from_expr acc expr in
  Option.value_map
    ~default:{ acc with deps }
    ~f:(collect_from_ty_expr { acc with deps })
    ascr


(** Collects external deps from ty expr. *)
and collect_from_ty_expr ({ deps; scope } as acc) { type_content; location = _ } =
  match type_content with
  | T_variable _ -> acc
  | T_constant (_, _) -> acc
  | T_contract_parameter (h :: tl) ->
    let deps =
      List.fold (h :: tl) ~init:deps ~f:(fun deps m -> add_to_deps { deps; scope } m)
    in
    { acc with deps }
  | T_sum { fields; _ } ->
    let { deps; _ } =
      List.fold
        (Map.to_alist fields)
        ~init:acc
        ~f:(fun ({ deps; scope } as acc) (_, ty) -> collect_from_ty_expr acc ty)
    in
    { acc with deps }
  | T_union union ->
    let { deps; _ }, _ =
      Union.fold_map
        (fun ({ deps; scope } as acc) ty -> collect_from_ty_expr acc ty, ty)
        acc
        union
    in
    { acc with deps }
  | T_record { fields; layout = _ } ->
    let { deps; _ } =
      List.fold
        (Record.to_list fields)
        ~init:acc
        ~f:(fun ({ deps; scope } as acc) (_, ty) -> collect_from_ty_expr acc ty)
    in
    { acc with deps }
  | T_arrow { type1; type2; param_names = _ } ->
    let { deps; _ } = collect_from_ty_expr acc type1 in
    collect_from_ty_expr { acc with deps } type2
  | T_app { type_operator = { module_path = []; element = _ }; arguments = _ } -> acc
  | T_app { type_operator = { module_path = h :: _; element = _ }; arguments } ->
    let deps = add_to_deps acc h in
    let { deps; _ } =
      List.fold arguments ~init:{ acc with deps } ~f:(fun ({ deps; scope } as acc) ty ->
          collect_from_ty_expr acc ty)
    in
    { acc with deps }
  | T_module_accessor { module_path = []; element = _ } -> acc
  | T_module_accessor { module_path = h :: _; element = _ } ->
    { acc with deps = add_to_deps acc h }
  | T_singleton _ -> acc
  | T_abstraction { type_; kind = _; ty_binder = _ } ->
    let { deps; _ } = collect_from_ty_expr acc type_ in
    { acc with deps }
  | T_for_all { type_; ty_binder = _; kind = _ } ->
    let { deps; _ } = collect_from_ty_expr acc type_ in
    { acc with deps }


(** Collects external deps from expression.
    Only thing affecting local expression scope is `E_mod_in`.
    Other items are used only to collect deps *)
and collect_from_expr ({ deps; scope } as acc) { expression_content; location = _ } =
  match expression_content with
  | E_variable _ -> acc
  | E_literal _ -> acc
  | E_mod_in { rhs; let_result; module_binder } ->
    (* Building up new scope *)
    let acc = collect_from_mod_in acc module_binder rhs in
    (* Discarding scope after evaluation *)
    let { deps; _ } = collect_from_expr acc let_result in
    { acc with deps }
  | Types.E_contract (mvar :: _) ->
    (* Only the first module variable in the list could be external *)
    let deps = add_to_deps acc mvar in
    { acc with deps }
  | E_constant { arguments; cons_name = _ } ->
    List.fold arguments ~init:acc ~f:collect_from_expr
  | E_application { lamb; args } ->
    let { deps; _ } = collect_from_expr acc lamb in
    let { deps; _ } = collect_from_expr { acc with deps } args in
    { acc with deps }
  | E_lambda { binder; output_type; result } ->
    let Binder.{ ascr; var = _ } = binder.binder in
    let { deps; _ } = collect_from_expr acc result in
    let { deps; _ } =
      Option.value_map
        ~default:{ acc with deps }
        ~f:(collect_from_ty_expr { acc with deps })
        output_type
    in
    let { deps; _ } =
      Option.value_map
        ~default:{ acc with deps }
        ~f:(collect_from_ty_expr { acc with deps })
        ascr
    in
    { acc with deps }
  | E_recursive
      { fun_type
      ; fun_name = _
      ; force_lambdarec = _
      ; lambda = { binder; output_type; result }
      } ->
    let Binder.{ ascr; var = _ } = binder.binder in
    let { deps; _ } = collect_from_expr acc result in
    let { deps; _ } = collect_from_ty_expr { acc with deps } output_type in
    let { deps; _ } = collect_from_ty_expr { acc with deps } ascr in
    let { deps; _ } = collect_from_ty_expr { acc with deps } fun_type in
    { acc with deps }
  | E_type_abstraction { result; type_binder = _ } -> collect_from_expr acc result
  | E_let_in { let_binder; rhs; let_result; attributes = _ } ->
    let { deps; _ } = collect_from_expr acc rhs in
    let { deps; _ } = collect_from_expr { acc with deps } let_result in
    let { deps; _ } = collect_from_pattern { acc with deps } let_binder in
    { acc with deps }
  | E_type_in { rhs; let_result; type_binder = _ } ->
    let { deps; _ } = collect_from_expr acc let_result in
    let { deps; _ } = collect_from_ty_expr { deps; scope } rhs in
    { acc with deps }
  | E_raw_code { code; language = _ } ->
    let { deps; _ } = collect_from_expr acc code in
    { acc with deps }
  | E_constructor { element; constructor = _ } ->
    let { deps; _ } = collect_from_expr acc element in
    { acc with deps }
  | E_matching { matchee; cases } ->
    let { deps; _ } = collect_from_expr acc matchee in
    let { deps; _ } =
      List.fold
        cases
        ~init:{ acc with deps }
        ~f:(fun ({ deps; scope } as acc) { pattern; body } ->
          let { deps; _ } = collect_from_pattern acc pattern in
          collect_from_expr { acc with deps } body)
    in
    { acc with deps }
  | E_record l ->
    let { deps; _ } =
      List.fold (Record.to_list l) ~init:acc ~f:(fun ({ deps; scope } as acc) (_, expr) ->
          collect_from_expr acc expr)
    in
    { acc with deps }
  | E_tuple (h :: tl) ->
    let { deps; _ } =
      List.fold (h :: tl) ~init:acc ~f:(fun ({ deps; scope } as acc) expr ->
          collect_from_expr acc expr)
    in
    { acc with deps }
  | E_array l ->
    let { deps; _ } =
      List.fold l ~init:acc ~f:(fun ({ deps; scope } as acc) expr ->
          match expr with
          | Expr_entry expr -> collect_from_expr acc expr
          | Rest_entry expr -> collect_from_expr acc expr)
    in
    { acc with deps }
  | E_array_as_list l ->
    let { deps; _ } =
      List.fold l ~init:acc ~f:(fun ({ deps; scope } as acc) expr ->
          match expr with
          | Expr_entry expr -> collect_from_expr acc expr
          | Rest_entry expr -> collect_from_expr acc expr)
    in
    { acc with deps }
  | E_accessor { struct_; path = _ } ->
    let { deps; _ } = collect_from_expr acc struct_ in
    { acc with deps }
  | E_update { struct_; update; path = _ } ->
    let { deps; _ } = collect_from_expr acc update in
    let { deps; _ } = collect_from_expr { acc with deps } struct_ in
    { acc with deps }
  | E_ascription { anno_expr; type_annotation } ->
    let { deps; _ } = collect_from_expr acc anno_expr in
    let { deps; _ } = collect_from_ty_expr { acc with deps } type_annotation in
    { acc with deps }
  | E_module_accessor { module_path = []; element = _ } -> acc
  | E_module_accessor { module_path = h :: _; element = _ } ->
    { acc with deps = add_to_deps acc h }
  | E_let_mut_in { let_binder; rhs; let_result; attributes = _ } ->
    let { deps; _ } = collect_from_expr acc rhs in
    let { deps; _ } = collect_from_expr { acc with deps } let_result in
    let { deps; _ } = collect_from_pattern { acc with deps } let_binder in
    { acc with deps }
  | E_assign { expression; binder = { ascr; var = _ } } ->
    let { deps; _ } = collect_from_expr acc expression in
    let { deps; _ } =
      Option.value_map
        ~default:{ acc with deps }
        ~f:(collect_from_ty_expr { acc with deps })
        ascr
    in
    { acc with deps }
  | E_for { start; final; incr; f_body; binder = _ } ->
    let { deps; _ } = collect_from_expr acc start in
    let { deps; _ } = collect_from_expr { acc with deps } final in
    let { deps; _ } = collect_from_expr { acc with deps } incr in
    let { deps; _ } = collect_from_expr { acc with deps } f_body in
    { acc with deps }
  | E_for_each { collection; fe_body; fe_binder = _; collection_type = _ } ->
    let { deps; _ } = collect_from_expr acc collection in
    let { deps; _ } = collect_from_expr { acc with deps } fe_body in
    { acc with deps }
  | E_while { cond; body } ->
    let { deps; _ } = collect_from_expr acc cond in
    let { deps; _ } = collect_from_expr { acc with deps } body in
    { acc with deps }


(** Collects external deps from pattern. *)
and collect_from_pattern ({ deps; scope } as acc) pat =
  let deps =
    match Location.unwrap pat with
    | P_unit -> deps
    | P_var { ascr; var = _ } ->
      let { deps; _ } =
        Option.value_map ~default:acc ~f:(collect_from_ty_expr acc) ascr
      in
      deps
    | P_list pat ->
      let { deps; _ } =
        match pat with
        | Cons (p1, p2) -> collect_from_pattern (collect_from_pattern acc p1) p2
        | List l -> List.fold l ~init:acc ~f:collect_from_pattern
      in
      deps
    | P_variant (_, p) ->
      let { deps; _ } = collect_from_pattern acc p in
      deps
    | P_tuple l ->
      let { deps; _ } = List.fold l ~init:acc ~f:collect_from_pattern in
      deps
    | P_record l ->
      let { deps; _ } =
        List.fold (Record.to_list l) ~init:acc ~f:(fun acc (_, p) ->
            collect_from_pattern acc p)
      in
      deps
  in
  { acc with deps }


(* Accepts stdlib since stdlib modules shouldn't be considered external *)
let dependencies ~std_lib prg =
  let scope =
    List.fold std_lib ~init:MSet.empty ~f:(fun acc decl ->
        match Location.unwrap decl with
        | Types.D_module module_ -> Set.add acc module_.module_binder
        | _ -> acc)
  in
  let deps = MSet.empty in
  let { deps; _ } = List.fold prg ~init:{ deps; scope } ~f:collect_from_decl in
  deps |> Set.to_list |> List.map ~f:Module_var.to_name_exn
