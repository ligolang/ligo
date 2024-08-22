open Ocaml_common
open Ligo_prim
open Ast_core
open Caml_core

let error_unimplemented () = failwith "unimplemented"

(* TODO: drop all Location.dummy *)
let fresh_value ident =
  (* TODO: location *)
  let name = Ident.name ident in
  Value_var.fresh ~loc:Location.dummy ~name ~generated:false ()


let fresh_type ident =
  (* TODO: location *)
  let name = Ident.name ident in
  Type_var.fresh ~loc:Location.dummy ~name ~generated:false ()


let fresh_module ident =
  (* TODO: location *)
  let name = Ident.name ident in
  Module_var.fresh ~loc:Location.dummy ~name ~generated:false ()


module OCaml = struct
  module Env = Env
  module Types = Ocaml_common.Types
end

module Env : sig
  type env
  type t = env

  type type_info =
    | Type_unsupported_predef
    | Type_subst_to of Type_var.t

  val initial : env
  val enter_value : Ident.t -> Value_var.t -> env -> env
  val enter_type : Ident.t -> Type_var.t -> env -> env
  val enter_module : Ident.t -> Module_var.t -> env -> (env -> env * 'a) -> env * 'a
  val solve_value_path : Path.t -> env -> Value_var.t Module_access.t
  val solve_type_path : Path.t -> env -> Type_var.t Module_access.t
  val solve_module_path : Path.t -> env -> Module_var.t Module_access.t
end = struct
  (* TODO: core map *)
  module String_map = Stdlib.Map.Make (String)

  type type_info =
    | Type_unsupported_predef
    | Type_subst_to of Type_var.t

  type md_env =
    { md_values : Value_var.t String_map.t
    ; md_types : type_info String_map.t
    ; md_modules : (Module_var.t * md_env) String_map.t
    }

  (* TODO: try with in all OCaml functions *)
  type env =
    { values : Value_var.t Ident.Map.t
    ; types : type_info Ident.Map.t
    ; modules : (Module_var.t * md_env) Ident.Map.t
    ; local : md_env
    }

  type t = env

  let empty_local =
    { md_values = String_map.empty
    ; md_types = String_map.empty
    ; md_modules = String_map.empty
    }


  let empty =
    { values = Ident.Map.empty
    ; types = Ident.Map.empty
    ; modules = Ident.Map.empty
    ; local = empty_local
    }


  let enter_value ident value_var env =
    let name = Ident.name ident in
    let values = Ident.Map.add ident value_var env.values in
    let md_values = String_map.add name value_var env.local.md_values in
    { env with values; local = { env.local with md_values } }


  let enter_type_info ident type_info env =
    let { values = _; types; modules = _; local } = env in
    let name = Ident.name ident in
    let types = Ident.Map.add ident type_info types in
    let md_types = String_map.add name type_info env.local.md_types in
    { env with types; local = { env.local with md_types } }


  let enter_module ident module_var env k =
    let { values = _; types = _; modules; local } = env in
    let inner_env, x = k { env with local = empty_local } in
    let name = Ident.name ident in
    let mod_data = module_var, inner_env.local in
    let modules = Ident.Map.add ident mod_data modules in
    let local =
      let { md_values = _; md_types = _; md_modules } = local in
      let md_modules = String_map.add name mod_data md_modules in
      { local with md_modules }
    in
    { env with modules; local }, x


  let enter_type ident type_var env =
    let type_info = Type_subst_to type_var in
    enter_type_info ident type_info env


  (* TODO: drop all failwith *)
  (* TODO: reduce boilerplate below *)

  let rec solve_module_path path env =
    let open Path in
    match path with
    | Pident ident ->
      (* TODO: use list not empty here *)
      (match Ident.Map.find_opt ident env.modules with
      | Some (module_, env) -> (module_, []), env
      | None -> failwith "unexpected module ident")
    | Pdot (left, right) ->
      let (rev_left_hd, rev_left_tl), env = solve_module_path left env in
      (match String_map.find_opt right env.md_modules with
      | Some (module_, env) -> (module_, rev_left_hd :: rev_left_tl), env
      | None -> failwith "unexpected module name")
    | Papply (_, _) -> failwith "Papply not supported"


  let solve_value_path path env =
    let open Path in
    match path with
    | Pident ident ->
      (match Ident.Map.find_opt ident env.values with
      | Some value ->
        (* TODO: what about module_path *)
        Module_access.{ module_path = []; element = value }
      | None -> failwith "unexpected value ident")
    | Pdot (module_, right) ->
      let (rev_module_hd, rev_module_tl), env = solve_module_path module_ env in
      (match String_map.find_opt right env.md_values with
      | Some value ->
        let module_path = List.rev (rev_module_hd :: rev_module_tl) in
        Module_access.{ module_path; element = value }
      | None -> failwith "unexpected value name")
    | Papply (_, _) -> failwith "Papply not supported"


  let solve_type_path path env =
    let open Path in
    match path with
    | Pident ident ->
      (match Ident.Map.find_opt ident env.types with
      | Some Type_unsupported_predef -> failwith "unsupported type predef"
      | Some (Type_subst_to type_) ->
        (* TODO: what about module_path *)
        Module_access.{ module_path = []; element = type_ }
      | None -> failwith "unexpected type ident")
    | Pdot (module_, right) ->
      let (rev_module_hd, rev_module_tl), env = solve_module_path module_ env in
      (match String_map.find_opt right env.md_types with
      | Some Type_unsupported_predef -> failwith "unsupported type predef"
      | Some (Type_subst_to type_) ->
        let module_path = List.rev (rev_module_hd :: rev_module_tl) in
        Module_access.{ module_path; element = type_ }
      | None -> failwith "unexpected type name")
    | Papply (_, _) -> failwith "Papply not supported"


  let solve_module_path path env =
    let (rev_module_hd, rev_module_tl), env = solve_module_path path env in
    let module_path = List.rev rev_module_tl in
    Module_access.{ module_path; element = rev_module_hd }


  (* TODO: failwith *)
  (* TODO: this is hackish *)
  let extract_ident path =
    match path with
    | Path.Pident ident -> ident
    | Path.Pdot (_, _) | Path.Papply (_, _) -> failwith "unexpected"


  let enter_type_predef_unsupported path =
    enter_type_info (extract_ident path) @@ Type_unsupported_predef


  let enter_type_predef_alias path var = enter_type (extract_ident path) var
  (* let enter_value_predef_alias ident var = enter_value ident var *)

  let initial =
    let open Predef in
    (* TODO: hopefully this is a temporary solution *)
    (* TODO: move ligo away from T_constant? *)
    (* TODO: location here *)
    let loc = Location.dummy in
    (* TODO: this is hackish *)
    let int_var = Type_var.of_input_var ~loc @@ Literal_types.to_string Int in
    let string_var = Type_var.of_input_var ~loc @@ Literal_types.to_string String in
    let unit_var = Type_var.of_input_var ~loc @@ Literal_types.to_string Unit in
    empty
    |> enter_type_predef_alias path_unit unit_var
    |> enter_type_predef_alias path_int int_var
    |> enter_type_predef_unsupported path_char
    |> enter_type_predef_alias path_string string_var
    |> enter_type_predef_unsupported path_bytes
    |> enter_type_predef_unsupported path_float
    |> enter_type_predef_unsupported path_bool
    |> enter_type_predef_alias path_unit unit_var
    |> enter_type_predef_unsupported path_exn
    |> enter_type_predef_unsupported path_array
    |> enter_type_predef_unsupported path_list
    |> enter_type_predef_unsupported path_option
    |> enter_type_predef_unsupported path_nativeint
    |> enter_type_predef_unsupported path_int32
    |> enter_type_predef_unsupported path_int64
    |> enter_type_predef_unsupported path_lazy_t
    |> enter_type_predef_unsupported path_extension_constructor
    |> enter_type_predef_unsupported path_floatarray
  (* |> enter_type_predef_unsupported *)
end

(* TODO: solve is a bad word *)
let type_wrap location content = { type_content = content; location }

let pat_wrap location content : _ Ast_core.Pattern.t =
  Location.{ wrap_content = content; location }


(* TODO: use Ligo e_stuff function *)
let expr_wrap location content =
  (* TODO: type *)
  { expression_content = content; location }


let decl_wrap location content : Ast_core.decl =
  (* TODO: type *)
  { wrap_content = content; location }


let mod_expr_wrap location content : Ast_core.module_expr =
  (* TODO: type *)
  { wrap_content = content; location }


(* TODO: merge env and vars *)
let rec solve_type env vars typ_ =
  let { type_desc; type_loc = loc } = typ_ in
  match type_desc with
  | T_var (name, id) ->
    let var =
      match Hashtbl.find vars id with
      | Some var -> var
      | None ->
        (* TODO: name *)
        let var = Type_var.fresh ~loc:Location.dummy ?name ~generated:false () in
        Hashtbl.set vars ~key:id ~data:var;
        var
    in
    type_wrap loc @@ T_variable var
  | T_constr (path, args) ->
    let type_operator = Env.solve_type_path path env in
    (match args with
    | [] ->
      (* TODO: fix this on ligo *)
      (match type_operator with
      | { module_path = []; element } -> type_wrap loc @@ T_variable element
      | { module_path; element } as var -> type_wrap loc @@ T_module_accessor var)
    | args ->
      (* TODO: what about no args? *)
      let arguments = List.map args ~f:(fun type_ -> solve_type env vars type_) in
      type_wrap loc @@ T_app { type_operator; arguments })
  | T_arrow (param, return) ->
    let param = solve_type env vars param in
    let return = solve_type env vars return in
    (* TODO: param names? *)
    type_wrap loc @@ T_arrow { type1 = param; type2 = return; param_names = [] }
  | T_tuple fields ->
    let fields = List.map fields ~f:(fun field -> solve_type env vars field) in
    type_wrap loc @@ T_record (Row.create_tuple fields)
  | T_forall (bound, body) ->
    (* TODO: test poly and univar *)
    solve_type_forall loc env vars ~bound body


and solve_type_forall loc env vars ~bound body =
  match bound with
  | [] -> solve_type env vars body
  | (name, id) :: bound ->
    let var : Type_var.t = Type_var.fresh ~loc:Location.dummy ?name ~generated:false () in
    (* TODO: ensures id is not in vars *)
    Hashtbl.set vars ~key:id ~data:var;
    let body = solve_type_forall loc env vars ~bound body in
    Hashtbl.remove vars id;
    type_wrap loc @@ T_for_all { ty_binder = var; kind = Type; type_ = body }


let solve_type_poly env vars type_ =
  (* TODO: this is bad *)
  let external_vars = Hashtbl.copy vars in
  let { type_desc = _; type_loc = loc } = type_ in
  let type_ = solve_type env vars type_ in
  let internal_vars =
    List.filter_map (Hashtbl.to_alist vars) ~f:(fun (id, var) ->
        match Hashtbl.mem external_vars id with
        | true -> None
        | false ->
          Hashtbl.remove vars id;
          Some var)
  in
  List.fold_left internal_vars ~init:type_ ~f:(fun body var ->
      type_wrap loc @@ T_for_all { ty_binder = var; kind = Type; type_ = body })


let solve_type_record loc env vars fields =
  let fields =
    List.map fields ~f:(fun decl_label ->
        let { dl_id; dl_type; dl_loc } = decl_label in
        let label = Ident.name dl_id in
        let type_ = solve_type_poly env vars dl_type in
        Label.Label (label, dl_loc), type_)
  in
  let fields = Label.Map.of_alist_exn fields in
  (* TODO: layout *)
  let layout = None in
  type_wrap loc @@ T_record { fields; layout }


let rec solve_type_decl env vars decl : type_expression =
  let { type_decl_desc; type_decl_params; type_decl_loc = loc } = decl in
  solve_type_decl_lambda env vars loc type_decl_params type_decl_desc


and solve_type_decl_lambda env vars loc params desc =
  match params with
  | [] -> solve_type_decl_body env vars loc desc
  | (name, id) :: params ->
    let var : Type_var.t = Type_var.fresh ~loc:Location.dummy ?name ~generated:false () in
    (* TODO: ensures id is not in vars *)
    Hashtbl.set vars ~key:id ~data:var;
    let body = solve_type_decl_lambda env vars loc params desc in
    Hashtbl.remove vars id;
    type_wrap loc @@ T_abstraction { ty_binder = var; kind = Type; type_ = body }


and solve_type_decl_body env vars loc desc =
  match desc with
  | T_record fields -> solve_type_record loc env vars fields
  | T_variant cases ->
    let cases =
      List.map cases ~f:(fun decl_case ->
          match decl_case with
          | C_tuple { dc_id; dc_fields; dc_loc } ->
            let label = Ident.name dc_id in
            let content =
              match dc_fields with
              | [] -> t_unit ~loc:dc_loc ()
              | [ field ] -> solve_type env vars field
              | fields ->
                let fields =
                  List.map fields ~f:(fun field -> solve_type env vars field)
                in
                type_wrap loc @@ T_record (Row.create_tuple fields)
            in
            Label.Label (label, dc_loc), content
          | C_record { dc_id; dc_fields; dc_loc } ->
            let label = Ident.name dc_id in
            let content = solve_type_record loc env vars dc_fields in
            Label.Label (label, dc_loc), content)
    in
    let cases = Label.Map.of_alist_exn cases in
    (* TODO: layout *)
    let layout = None in
    type_wrap loc @@ T_sum { fields = cases; layout }
  | T_alias manifest ->
    (* TODO: not poly tho *)
    solve_type_poly env vars manifest
  | T_constant (constant, arity) -> type_wrap loc @@ T_constant (constant, arity)


let solve_var_pat env vars var_pat =
  let { var_pat_desc; var_pat_type; var_pat_loc } = var_pat in
  let var_pat_type = solve_type env vars var_pat_type in
  var_pat_desc, var_pat_type, var_pat_loc


let rec solve_pat env vars pat : _ * _ Pattern.t =
  let { pat_desc; pat_type; pat_loc = loc } = pat in
  (* TODO: ascription in all types *)
  (* TODO: dummy *)
  (* TODO: poly type *)
  let type_ = Some (solve_type env vars pat_type) in
  match pat_desc with
  | P_unit -> env, pat_wrap loc @@ P_unit
  | P_var ident ->
    let var = fresh_value ident in
    let binder = Binder.make var type_ in
    let env = Env.enter_value ident var env in
    env, pat_wrap loc @@ P_var binder
  | P_tuple fields ->
    let env, rev_fields =
      List.fold_left fields ~init:(env, []) ~f:(fun (env, rev_fields) pat ->
          let env, field = solve_pat env vars pat in
          env, field :: rev_fields)
    in
    let fields = List.rev rev_fields in
    env, pat_wrap loc @@ P_tuple fields
  | P_record fields ->
    let env, rev_fields =
      List.fold_left fields ~init:(env, []) ~f:(fun (env, rev_fields) (label, pat) ->
          let env, pat = solve_pat env vars pat in
          env, (label, pat) :: rev_fields)
    in
    let fields = List.rev rev_fields in
    env, pat_wrap loc @@ P_record (Record.of_list fields)
  | P_variant (label, fields) ->
    let env, fields = solve_pat env vars fields in
    env, pat_wrap loc @@ P_variant (label, fields)


let rec solve_expr env vars expr =
  let { expr_desc; expr_type; expr_loc = loc } = expr in
  match expr_desc with
  | E_var path ->
    let var = Env.solve_value_path path env in
    (* TODO: fix this on ligo *)
    (match var with
    | { module_path = []; element } -> expr_wrap loc @@ E_variable element
    | { module_path; element } as var -> expr_wrap loc @@ E_module_accessor var)
  | E_literal lit -> expr_wrap loc @@ E_literal lit
  | E_let (pat, value, body) ->
    (* TODO: this is poly pat? *)
    let inner_env, pat = solve_pat env vars pat in
    let value = solve_expr_poly env vars value in
    let body = solve_expr inner_env vars body in
    expr_wrap loc
    @@ E_let_in
         { let_binder = pat
         ; rhs = value
         ; let_result = body
         ; attributes = Value_attr.default_attributes
         }
  | E_let_rec _ ->
    (* TODO: high priority *)
    error_unimplemented ()
  | E_lambda (var_pat, body) ->
    (* TODO: use var_pat_loc *)
    let { var_pat_desc; var_pat_type; var_pat_loc = _ } = var_pat in
    let var = fresh_value var_pat_desc in
    let env = Env.enter_value var_pat_desc var env in
    let body = solve_expr env vars body in
    let binder =
      (* TODO: {mut,forced,initial} flag *)
      let param_annot = solve_type env vars var_pat_type in
      Param.make var (Some param_annot)
    in
    (* TODO: output_type *)
    expr_wrap loc @@ E_lambda { binder; output_type = None; result = body }
  | E_apply (lambda, args) ->
    let lambda = solve_expr env vars lambda in
    List.fold_left args ~init:lambda ~f:(fun lambda arg ->
        let arg = solve_expr env vars arg in
        expr_wrap loc @@ E_application { lamb = lambda; args = arg })
  | E_match (matchee, cases) ->
    let matchee = solve_expr env vars matchee in
    let cases =
      List.map cases ~f:(fun (pat, body) ->
          let env, pat = solve_pat env vars pat in
          let body = solve_expr env vars body in
          Match_expr.{ pattern = pat; body })
    in
    expr_wrap loc @@ E_matching { matchee; cases }
  | E_tuple fields ->
    let fields = Nonempty_list.map fields ~f:(fun field -> solve_expr env vars field) in
    expr_wrap loc @@ E_tuple fields
  | E_constructor (constructor, fields) ->
    (* TODO: location? *)
    let fields = List.map fields ~f:(fun field -> solve_expr env vars field) in
    let element =
      match fields with
      | [] -> e_unit ~loc ()
      | [ field ] -> field
      | field :: fields -> e_tuple ~loc (field :: fields) ()
    in
    expr_wrap loc @@ E_constructor { constructor; element }
  | E_record fields ->
    let fields =
      List.map fields ~f:(fun (label, field) ->
          (* TODO: location *)
          Label.Label (label, Location.dummy), solve_expr env vars field)
    in
    expr_wrap loc @@ E_record (Record.of_list fields)
  | E_field (struct_, label) ->
    let struct_ = solve_expr env vars struct_ in
    let path = Label.Label (label, Location.dummy) in
    expr_wrap loc @@ E_accessor { struct_; path }


and solve_expr_poly env vars expr =
  (* TODO: this is also duplicated *)
  (* TODO: this is bad *)
  let { expr_desc = _; expr_type = _; expr_loc = loc } = expr in
  let external_vars = Hashtbl.copy vars in
  (* TODO: looks weird to extract here *)
  let expr = solve_expr env vars expr in
  let internal_vars =
    List.filter_map (Hashtbl.to_alist vars) ~f:(fun (id, var) ->
        match Hashtbl.mem external_vars id with
        | true -> None
        | false ->
          Hashtbl.remove vars id;
          Some var)
  in
  List.fold_left internal_vars ~init:expr ~f:(fun body var ->
      expr_wrap loc @@ E_type_abstraction { type_binder = var; result = body })


let rec solve_module env module_ =
  let env, rev_decl =
    List.fold_left module_ ~init:(env, []) ~f:(fun (env, rev_module) decl ->
        let env, decl = solve_decl env decl in
        env, decl :: rev_module)
  in
  env, List.rev rev_decl


and solve_decl env decl =
  let vars = Hashtbl.create (module Int) in
  let env, decl = solve_decl_inner env vars decl in
  assert (Hashtbl.is_empty vars);
  env, decl


and solve_decl_inner env vars decl =
  let { decl_desc; decl_loc = loc } = decl in
  match decl_desc with
  | D_value (var_pat, value) ->
    (* TODO: duplicated logic regarding var_pat and Binder *)
    (* TODO: use var_pat_loc *)
    let { var_pat_desc = ident; var_pat_type; var_pat_loc = _ } = var_pat in
    let var = fresh_value ident in
    let type_ = solve_type_poly env vars var_pat_type in
    let binder = Binder.make var (Some type_) in
    let value = solve_expr_poly env vars value in
    let env = Env.enter_value ident var env in
    ( env
    , decl_wrap loc
      @@ D_value { binder; expr = value; attr = Value_attr.default_attributes } )
  | D_value_rec _ ->
    (* TODO: high priority *)
    error_unimplemented ()
  | D_type (ident, type_decl) ->
    let type_decl = solve_type_decl env vars type_decl in
    let var = fresh_type ident in
    let env = Env.enter_type ident var env in
    ( env
    , decl_wrap loc
      @@ D_type
           { type_binder = var
           ; type_expr = type_decl
           ; type_attr = Type_or_module_attr.default_attributes
           } )
  | D_module (ident, mod_expr) ->
    let var = fresh_module ident in
    let env, module_ =
      Env.enter_module ident var env @@ fun env -> solve_mod_expr env mod_expr
    in
    (* TODO: use this inner_env? *)
    ( env
    , decl_wrap loc
      @@ D_module
           { module_binder = var
           ; module_ (* TODO: annotation *)
           ; annotation = None
           ; module_attr = TypeOrModuleAttr.default_attributes
           } )


and solve_mod_expr env mod_expr =
  let { mod_expr_desc; mod_expr_loc = loc } = mod_expr in
  match mod_expr_desc with
  | M_struct decls ->
    let env, decls = solve_module env decls in
    env, mod_expr_wrap loc @@ M_struct decls
  | M_var path ->
    (* TODO: improve this on Ligo *)
    (match Env.solve_module_path path env with
    | { module_path = []; element } -> env, mod_expr_wrap loc @@ M_variable element
    | { module_path; element } ->
      env, mod_expr_wrap loc @@ M_module_path (element :: module_path))
