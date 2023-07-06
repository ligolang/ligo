open Simple_utils.Trace
open Errors
open Ligo_prim

let default_entrypoint = "main"

let default_entrypoint_var =
  Value_var.of_input_var ~loc:Location.generated default_entrypoint


let default_built_entrypoint = "$main"

let default_built_entrypoint_var =
  Value_var.of_input_var ~loc:Location.generated default_built_entrypoint


let default_views = "$views"
let default_views_var = Value_var.of_input_var ~loc:Location.generated default_views
let default_contract = "$contract"
let default_contract_var = Value_var.of_input_var ~loc:Location.generated default_contract
let default_parameter = "$parameter"

let default_parameter_var =
  Type_var.of_input_var ~loc:Location.generated default_parameter


let get_entries_of_signature (sign : Ast_typed.signature) =
  let f d =
    match d with
    | Ast_typed.S_value (binder, ty, attr) when attr.entry -> Some (binder, ty)
    | _ -> None
  in
  sign |> List.filter_map ~f


let get_entries_of_module ?signature (prg : Ast_typed.module_) =
  let f d =
    match Location.unwrap d with
    | Ast_typed.D_value { binder; attr; _ } when attr.entry ->
      [ Binder.get_var binder, Binder.get_ascr binder ]
    | Ast_typed.D_irrefutable_match { pattern; attr; _ } when attr.entry ->
      (match Location.unwrap pattern with
      | P_var binder -> [ Binder.get_var binder, Binder.get_ascr binder ]
      | _ -> [])
    | Ast_typed.D_module_include { signature; _ } -> get_entries_of_signature signature
    | _ -> []
  in
  let filter =
    match signature with
    | None -> fun _ -> true
    | Some signature ->
      let signature_entries = get_entries_of_signature signature in
      fun e ->
        List.mem ~equal:(fun (v, _) (v', _) -> Value_var.equal v v') signature_entries e
  in
  prg |> List.concat_map ~f |> List.filter ~f:filter


let get_views_of_signature (sign : Ast_typed.signature) =
  let f d =
    match d with
    | Ast_typed.S_value (binder, ty, attr) when attr.view -> Some (binder, ty)
    | _ -> None
  in
  sign |> List.filter_map ~f


let get_views_of_module ?signature (prg : Ast_typed.module_) =
  let f d =
    match Location.unwrap d with
    | Ast_typed.D_value { binder; attr; _ } when attr.view ->
      [ Binder.get_var binder, Binder.get_ascr binder ]
    | Ast_typed.D_irrefutable_match { pattern; attr; _ } when attr.view ->
      (match Location.unwrap pattern with
      | P_var binder -> [ Binder.get_var binder, Binder.get_ascr binder ]
      | _ -> [])
    | Ast_typed.D_module_include { signature; _ } -> get_views_of_signature signature
    | _ -> []
  in
  let filter =
    match signature with
    | None -> fun _ -> true
    | Some signature ->
      let signature_views = get_views_of_signature signature in
      fun e ->
        List.mem ~equal:(fun (v, _) (v', _) -> Value_var.equal v v') signature_views e
  in
  prg |> List.concat_map ~f |> List.filter ~f:filter


let create_entrypoint_function_expr entrypoints parameter_type storage_type =
  let open Ast_typed in
  let loc = Location.generated in
  let p_var = Value_var.of_input_var ~loc "p" in
  let s_var = Value_var.of_input_var ~loc "s" in
  let p = e_a_variable ~loc p_var parameter_type in
  let s = e_a_variable ~loc s_var storage_type in
  let params = Value_var.fresh ~name:"param" ~loc () in
  let fields =
    Record.record_of_tuple
      [ (Location.wrap ~loc @@ Pattern.(P_var (Binder.make p_var parameter_type)))
      ; (Location.wrap ~loc @@ Pattern.(P_var (Binder.make s_var storage_type)))
      ]
  in
  let param_storage = e_a_pair ~loc p s in
  let fun_type = Misc.build_entry_type parameter_type storage_type in
  let oplst_storage = t_pair ~loc (t_list ~loc @@ t_operation ~loc ()) storage_type in
  let cases =
    List.map entrypoints ~f:(fun (entrypoint, entrypoint_type) ->
        let constructor =
          Label.of_string (String.capitalize (Value_var.to_name_exn entrypoint))
        in
        let pattern = Value_var.fresh ~name:"pattern" ~loc () in
        let body =
          match Misc.should_uncurry_entry entrypoint_type with
          | `Yes _ ->
            e_a_application
              ~loc
              (e_a_application
                 ~loc
                 (e_a_variable ~loc entrypoint fun_type)
                 (e_variable ~loc pattern (t_string ~loc ()))
                 oplst_storage)
              s
              (t_arrow ~loc storage_type oplst_storage ())
          | `No _ ->
            e_a_application
              ~loc
              (e_a_variable ~loc entrypoint fun_type)
              (e_a_pair ~loc (e_variable ~loc pattern (t_string ~loc ())) s)
              oplst_storage
          | `Bad -> failwith "what?"
        in
        let pattern =
          Location.wrap ~loc
          @@ Pattern.(
               P_variant
                 ( constructor
                 , Location.wrap ~loc
                   @@ P_var (Binder.make pattern param_storage.type_expression) ))
        in
        ({ pattern; body } : _ Match_expr.match_case))
  in
  let body = e_a_matching ~loc p cases oplst_storage in
  let pattern = Location.wrap ~loc @@ Pattern.(P_record fields) in
  let result =
    e_a_matching
      ~loc
      (e_a_variable ~loc params param_storage.type_expression)
      [ { pattern; body } ]
      oplst_storage
  in
  e_lambda
    { binder = Param.(make params param_storage.type_expression)
    ; result
    ; output_type = oplst_storage
    }
    fun_type


let create_views_function_expr ~loc views storage_type =
  let open Ast_typed in
  let f (view, view_type) result =
    let name = Value_var.to_name_exn view in
    let view_expr =
      match Ast_typed.should_uncurry_view ~storage_ty:storage_type view_type with
      | `Yes _ -> Option.value_exn @@ Ast_typed.uncurry_wrap ~loc ~type_:view_type view
      | `No _ -> e_a_variable ~loc view view_type
      | `Bad | `Bad_not_function | `Bad_storage _ -> failwith "wrong view"
    in
    e_a_test_cons_views
      ~loc
      storage_type
      (e_a_string ~loc (Ligo_string.standard name))
      view_expr
      result
  in
  List.fold_right ~f ~init:(e_a_test_nil_views ~loc storage_type) views


let program ~raise ?signature : Ast_typed.module_ -> Ast_typed.declaration list =
 fun module_ ->
  let loc = Location.generated in
  match Simple_utils.List.Ne.of_list_opt @@ get_entries_of_module ?signature module_ with
  | None -> []
  | Some entries ->
    let parameter_type, storage_type =
      match Ast_typed.Misc.parameter_from_entrypoints entries with
      | Error (`Not_entry_point_form (ep, ep_type)) ->
        raise.error (bad_contract_io ep ep_type ep_type.location)
      | Error (`Storage_does_not_match (ep_1, storage_1, ep_2, storage_2)) ->
        raise.error
          (corner_case
          @@ Format.asprintf
               "@[<hv>Storage types do not match for different entrypoints:@.%a : %a@.%a \
                : %a@]"
               Value_var.pp
               ep_1
               Ast_typed.PP.type_expression
               storage_1
               Value_var.pp
               ep_2
               Ast_typed.PP.type_expression
               storage_2)
      | Ok (p, s) -> p, s
    in
    let type_binder = default_parameter_var in
    let entrypoint_type_decl =
      Location.wrap ~loc
      @@ Ast_typed.D_type
           { type_binder
           ; type_expr = parameter_type
           ; type_attr = { public = true; hidden = false }
           }
    in
    let entrypoint_function_decl, entrypoint_var =
      let expr =
        create_entrypoint_function_expr
          ~loc
          Simple_utils.List.Ne.(to_list @@ entries)
          parameter_type
          storage_type
      in
      let binder = Binder.make default_built_entrypoint_var expr.type_expression in
      ( Location.wrap ~loc
        @@ Ast_typed.D_value
             { binder
             ; expr
             ; attr =
                 { inline = false
                 ; no_mutation = false
                 ; entry = false
                 ; view = false
                 ; public = true
                 ; thunk = false
                 ; hidden = false
                 }
             }
      , Ast_typed.e_a_variable ~loc (Binder.get_var binder) expr.type_expression )
    in
    let views = get_views_of_module ?signature module_ in
    let views_decl, views_var =
      let expr = create_views_function_expr ~loc views storage_type in
      let binder = Binder.make default_views_var expr.type_expression in
      ( Location.wrap ~loc
        @@ Ast_typed.D_value
             { binder
             ; expr
             ; attr =
                 { inline = false
                 ; no_mutation = false
                 ; entry = false
                 ; view = false
                 ; public = true
                 ; thunk = false
                 ; hidden = false
                 }
             }
      , Ast_typed.e_a_variable ~loc (Binder.get_var binder) expr.type_expression )
    in
    let contract_decl =
      let expr = Ast_typed.e_a_pair ~loc entrypoint_var views_var in
      let binder = Binder.make default_contract_var expr.type_expression in
      Location.wrap ~loc
      @@ Ast_typed.D_value
           { binder
           ; expr
           ; attr =
               { inline = false
               ; no_mutation = false
               ; entry = false
               ; view = false
               ; public = true
               ; thunk = false
               ; hidden = false
               }
           }
    in
    [ entrypoint_type_decl; entrypoint_function_decl; views_decl; contract_decl ]


let make_main_module_expr ~raise ?signature (module_content : Ast_typed.module_content) =
  match module_content with
  | M_struct ds ->
    let postfix = program ~raise ?signature ds in
    Module_expr.M_struct (ds @ postfix)
  | _ -> module_content


let make_main_module ~raise ?signature (prg : Ast_typed.program) =
  let f d =
    match Location.unwrap d with
    | Ast_typed.D_module
        { module_binder
        ; module_attr
        ; module_ = { module_content; module_location; signature }
        ; annotation
        } ->
      let module_content = make_main_module_expr ~raise ~signature module_content in
      Location.wrap ~loc:(Location.get_location d)
      @@ Ast_typed.D_module
           { module_binder
           ; module_attr
           ; module_ = { module_content; module_location; signature }
           ; annotation
           }
    | _ -> d
  in
  let prg = Helpers.Declaration_mapper.map_module f prg in
  prg @ program ~raise ?signature prg


let make_main_entrypoint ~raise
    :  Ast_typed.expression_variable Simple_utils.List.Ne.t -> Ast_typed.program
    -> Ast_typed.expression_variable * Ast_typed.program
  =
 fun entrypoints prg ->
  let loc = Location.generated in
  let prg = make_main_module ~raise prg in
  match entrypoints with
  | entrypoint, [] -> entrypoint, prg
  | entrypoint, rest ->
    let entries =
      let f ep =
        ep, fst @@ Helpers.fetch_entry_type ~raise (Value_var.to_name_exn ep) prg
      in
      Simple_utils.List.Ne.map f (entrypoint, rest)
    in
    let parameter_type, storage_type =
      match Ast_typed.Misc.parameter_from_entrypoints entries with
      | Error (`Not_entry_point_form (ep, ep_type)) ->
        raise.error (bad_contract_io ep ep_type ep_type.location)
      | Error (`Storage_does_not_match (ep_1, storage_1, ep_2, storage_2)) ->
        raise.error
          (corner_case
          @@ Format.asprintf
               "@[<hv>Storage types do not match for different entrypoints:@.%a : %a@.%a \
                : %a@]"
               Value_var.pp
               ep_1
               Ast_typed.PP.type_expression
               storage_1
               Value_var.pp
               ep_2
               Ast_typed.PP.type_expression
               storage_2)
      | Ok (p, s) -> p, s
    in
    let type_binder = default_parameter_var in
    let entrypoint_type_decl =
      Location.wrap ~loc
      @@ Ast_typed.D_type
           { type_binder
           ; type_expr = parameter_type
           ; type_attr = { public = true; hidden = false }
           }
    in
    let entrypoint_function_decl =
      let expr =
        create_entrypoint_function_expr
          ~loc
          (Simple_utils.List.Ne.to_list @@ entries)
          parameter_type
          storage_type
      in
      let binder = Binder.make default_built_entrypoint_var expr.type_expression in
      Location.wrap ~loc
      @@ Ast_typed.D_value
           { binder
           ; expr
           ; attr =
               { inline = false
               ; no_mutation = false
               ; entry = false
               ; view = false
               ; public = true
               ; thunk = false
               ; hidden = false
               }
           }
    in
    let prg = prg @ [ entrypoint_type_decl; entrypoint_function_decl ] in
    default_built_entrypoint_var, prg


let program ~raise ?signature
    : Ast_typed.program -> Ast_typed.expression_variable * Ast_typed.program
  =
 fun prg ->
  let annoted_entry_points = get_entries_of_module ?signature prg |> List.map ~f:fst in
  match annoted_entry_points with
  | [] -> default_entrypoint_var, prg
  | hd :: tl -> make_main_entrypoint ~raise (hd, tl) prg
