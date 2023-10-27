open Errors
open Ligo_prim
open Simple_utils.Trace

(* Approach:
    For a module with a contract sort:
      - Generate a type binder $storage
      - Generate a type binder $parameter
      - Generate an entrypoint $main
      - Generate a list of views $views
      - Generate a pair $contract = ($main, $views)
*)

let loc = Location.generated

module Module = struct
  type t = Ast_typed.module_ * Ast_typed.signature
  type item = Ast_typed.decl * Ast_typed.sig_item
  type items = Ast_typed.decl list * Ast_typed.sig_item list

  let ( <@ ) : t -> items -> t =
   fun (module_, sig_) (decls, sig_items) ->
    module_ @ decls, { sig_ with sig_items = sig_.sig_items @ sig_items }


  let empty = [], []
  let ( >:: ) (decl, sig_item) (module_, sig_) = decl :: module_, sig_item :: sig_
  let of_items = List.unzip
end

let def_type var type_ : Module.item =
  ( Location.wrap ~loc
    @@ Ast_typed.D_type
         { type_binder = var
         ; type_expr = type_
         ; type_attr = { public = true; hidden = false }
         }
  , S_type (var, type_) )


let def_value var (expr : Ast_typed.expression) : Module.item =
  ( Location.wrap ~loc
    @@ Ast_typed.D_value
         { binder = Binder.make var expr.type_expression
         ; expr
         ; attr = Value_attr.default_attributes
         }
  , S_value
      ( var
      , expr.type_expression
      , { entry = false; dyn_entry = false; view = false; optional = false } ) )


let let_value var expr ~in_ =
  let open Module in
  let item = def_value var expr in
  item >:: in_ (Ast_typed.e_a_variable ~loc var expr.type_expression)


let let_storage_and_parameter_types ~parameter_type ~storage_type ~in_ =
  let open Module in
  (* Note: parameter type is lifted, do we really need this ? *)
  def_type Magic_vars.parameter parameter_type
  >:: (def_type Magic_vars.storage storage_type >:: in_)


let e_views ~storage_type ~raise view_types =
  let open Ast_typed in
  List.fold_right
    view_types
    ~init:(e_a_test_nil_views ~loc storage_type)
    ~f:(fun (view, view_type) views_list ->
      let name = Value_var.to_name_exn view in
      let loc = Value_var.get_location view in
      let view_expr =
        match Ast_typed.should_uncurry_view ~storage_ty:storage_type view_type with
        | `Yes _ -> Option.value_exn @@ Ast_typed.uncurry_wrap ~loc ~type_:view_type view
        | `Bad -> raise.error (`Self_ast_typed_bad_view_too_few_arguments (name, loc))
        | `Bad_not_function ->
          raise.error (`Self_ast_typed_bad_view_not_a_function (name, loc))
      in
      e_a_test_cons_views
        ~loc
        storage_type
        (e_a_string ~loc (Ligo_string.standard name))
        view_expr
        views_list)


let e_main ~storage_type ~parameter_type ~raise entrypoint_types =
  let open Ast_typed in
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
    List.map entrypoint_types ~f:(fun (entrypoint, entrypoint_type) ->
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
    ~loc
    { binder = Param.(make params param_storage.type_expression)
    ; result
    ; output_type = oplst_storage
    }
    fun_type


let let_main
    ~storage_type
    ~parameter_type
    ~raise
    (sig_items : Ast_typed.sig_item list)
    ~in_
  =
  let entrypoint_types =
    List.filter_map sig_items ~f:(function
        | S_value (var, type_, attr) when attr.entry -> Some (var, type_)
        | _ -> None)
  in
  let main = e_main ~storage_type ~parameter_type ~raise entrypoint_types in
  let_value Magic_vars.generated_main main ~in_


let let_views ~storage_type (sig_items : Ast_typed.sig_item list) ~raise ~in_ =
  let view_types =
    List.filter_map sig_items ~f:(function
        | S_value (var, type_, attr) when attr.view -> Some (var, type_)
        | _ -> None)
  in
  let views = e_views ~storage_type ~raise view_types in
  let_value Magic_vars.views views ~in_


let let_initial_dynamic_entrypoints
    ~storage_type
    (sig_items : Ast_typed.sig_item list)
    ~in_
    ~raise
  =
  let view_types =
    List.filter_map sig_items ~f:(function
        | S_value (var, type_, attr) when attr.view -> Some (var, type_)
        | _ -> None)
  in
  let views = e_views ~storage_type ~raise view_types in
  let_value Magic_vars.views views ~in_


let def_contract ~main ~views ~has_dynamic_entrypoints : Module.item =
  let dynamic_entrypoint =
    let open Ast_typed in
    let ty = t_big_map ~loc (t_nat ~loc ()) (t_bytes ~loc ()) in
    let e = e_variable ~loc Magic_vars.initial_dynamic_entrypoints ty in
    make_e ~loc (if has_dynamic_entrypoints then e_some e else e_none ()) ty
  in
  let contract = Ast_typed.e_a_tuple ~loc [ main; views; dynamic_entrypoint ] in
  def_value Magic_vars.contract contract


let map_contract ~storage_type ~parameter_type ~raise sig_items module_ =
  let open Module in
  let has_dynamic_entrypoints =
    List.exists sig_items ~f:(function
        | Ast_typed.S_value (_, _, attr) when attr.dyn_entry -> true
        | _ -> false)
  in
  let items =
    let_storage_and_parameter_types
      ~parameter_type
      ~storage_type
      ~in_:
        (let_main ~storage_type ~parameter_type ~raise sig_items ~in_:(fun main ->
             let_views ~storage_type ~raise sig_items ~in_:(fun views ->
                 def_contract ~main ~views ~has_dynamic_entrypoints >:: empty)))
  in
  module_ <@ items


let map_module : raise:_ -> Module.t -> Module.t =
 fun ~raise ((_, { sig_items; sig_sort }) as module_) ->
  match sig_sort with
  | Ss_module -> module_
  | Ss_contract { storage = storage_type; parameter = parameter_type } ->
    map_contract ~storage_type ~parameter_type ~raise sig_items module_


let mapper ~raise =
  Helpers.Declaration_mapper.map_module (fun decl ->
      match Location.unwrap decl with
      | Ast_typed.D_module
          { module_binder
          ; module_attr
          ; module_ = { module_content = M_struct module_; module_location; signature }
          ; annotation
          } ->
        let module_, signature = map_module ~raise (module_, signature) in
        Location.wrap ~loc:(Location.get_location decl)
        @@ Ast_typed.D_module
             { module_binder
             ; module_attr
             ; module_ = { module_content = M_struct module_; module_location; signature }
             ; annotation
             }
      | _ -> decl)


let generate_entry_logic : raise:_ -> Ast_typed.program -> Ast_typed.program =
 fun ~raise prg ->
  match prg.pr_sig.sig_sort with
  | Ss_module ->
    let pr_module = mapper ~raise prg.pr_module in
    { pr_module; pr_sig = Ast_typed.to_signature pr_module }
  | Ss_contract { storage = storage_type; parameter = parameter_type } ->
    (* Map the entire program first  *)
    let module_ = mapper ~raise prg.pr_module in
    (* Determine new signature of module_ *)
    let sig_items = Ast_typed.Misc.to_sig_items module_ in
    (* Generate the main entrypoint for the program's module *)
    let module_, sig_ =
      map_contract
        ~storage_type
        ~parameter_type
        ~raise
        sig_items
        (module_, { prg.pr_sig with sig_items })
    in
    { pr_module = module_; pr_sig = sig_ }


let expand_e_contract =
  let open Ast_typed in
  Helpers.map_expression (fun exp ->
      Simple_utils.Option.(
        value
          ~default:exp
          (let* mods = get_e_contract exp in
           return
           @@ e_module_accessor
                ~loc:exp.location
                { module_path = List.Ne.to_list mods; element = Magic_vars.contract }
                exp.type_expression)))


let program
    :  raise:(Errors.self_ast_typed_error, _) raise -> Ast_typed.program
    -> Ast_typed.program
  =
 fun ~raise prg -> prg |> generate_entry_logic ~raise |> expand_e_contract
