open Ocaml_common
open Types
open Ligo_prim
open Ast_core
open Caml_core

let error_unimplemented () = failwith "unimplemented"

let fresh_value ident =
  (* TODO: location *)
  let name = Ident.name ident in
  Value_var.fresh ~loc:Location.dummy ~name ~generated:false ()


let fresh_type ident =
  (* TODO: location *)
  let name = Ident.name ident in
  Type_var.fresh ~loc:Location.dummy ~name ~generated:false ()


module OCaml = struct
  module Env = Env
  module Types = Ocaml_common.Types
end

module Env : sig
  type env
  type t = env

  val empty : env
  val enter_value : Ident.t -> Value_var.t -> env -> env
  val enter_type : Ident.t -> Type_var.t -> env -> env
  val solve_value_path : Path.t -> env -> Value_var.t Module_access.t
  val solve_type_path : Path.t -> env -> Type_var.t Module_access.t
end = struct
  (* TODO: core map *)
  module String_map = Stdlib.Map.Make (String)

  type env =
    { all_values : Value_var.t Ident.Map.t
    ; values : Value_var.t String_map.t
    ; all_types : Type_var.t Ident.Map.t
    ; types : Type_var.t String_map.t
    ; all_modules : (Module_var.t * env) Ident.Map.t
    ; modules : (Module_var.t * env) String_map.t
    }

  type t = env

  let empty =
    { all_values = Ident.Map.empty
    ; values = String_map.empty
    ; all_types = Ident.Map.empty
    ; types = String_map.empty
    ; all_modules = Ident.Map.empty
    ; modules = String_map.empty
    }


  let enter_value ident value_var env =
    let name = Ident.name ident in
    let all_values = Ident.Map.add ident value_var env.all_values in
    let values = String_map.add name value_var env.values in
    { env with all_values; values }


  let enter_type ident type_var env =
    let name = Ident.name ident in
    let all_types = Ident.Map.add ident type_var env.all_types in
    let types = String_map.add name type_var env.types in
    { env with all_types; types }


  (* TODO: drop all failwith *)

  let rec solve_module_path path env =
    let open Path in
    match path with
    | Pident ident ->
      let { all_values; values; all_types; types; all_modules; modules } = env in
      (match Ident.Map.find_opt ident all_modules with
      | Some (module_, env) -> [ module_ ], env
      | None -> failwith "unexpected module ident")
    | Pdot (left, right) ->
      let rev_left, env = solve_module_path left env in
      (match String_map.find_opt right env.modules with
      | Some (module_, env) -> module_ :: rev_left, env
      | None -> failwith "unexpected module name")
    | Papply (_, _) -> failwith "Papply not supported"


  let solve_value_path path env =
    let open Path in
    match path with
    | Pident ident ->
      (match Ident.Map.find_opt ident env.all_values with
      | Some value -> Module_access.{ module_path = []; element = value }
      | None -> failwith "unexpected value ident")
    | Pdot (module_, right) ->
      let rev_module, env = solve_module_path module_ env in
      (match String_map.find_opt right env.values with
      | Some value ->
        let module_path = List.rev rev_module in
        Module_access.{ module_path; element = value }
      | None -> failwith "unexpected value name")
    | Papply (_, _) -> failwith "Papply not supported"


  let solve_type_path path env =
    let open Path in
    match path with
    | Pident ident ->
      (match Ident.Map.find_opt ident env.all_types with
      | Some type_ -> Module_access.{ module_path = []; element = type_ }
      | None -> failwith "unexpected type ident")
    | Pdot (module_, right) ->
      let rev_module, env = solve_module_path module_ env in
      (match String_map.find_opt right env.types with
      | Some type_ ->
        let module_path = List.rev rev_module in
        Module_access.{ module_path; element = type_ }
      | None -> failwith "unexpected type name")
    | Papply (_, _) -> failwith "Papply not supported"
end

(* TODO: solve is a bad word *)
let type_wrap content =
  (* TODO: location *)
  let location = Location.dummy in
  { type_content = content; location }


let pat_wrap content : _ Ast_core.Pattern.t =
  (* TODO: location *)
  let location = Location.dummy in
  Location.{ wrap_content = content; location }


let expr_wrap content =
  (* TODO: location *)
  (* TODO: type *)
  let location = Location.dummy in
  { expression_content = content; location }


let decl_wrap content : Ast_core.decl =
  (* TODO: location *)
  (* TODO: type *)
  let location = Location.dummy in
  { wrap_content = content; location }


(* TODO: merge env and vars *)
let rec solve_type env vars typ_ =
  match typ_ with
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
    type_wrap @@ T_variable var
  | T_constr (path, args) ->
    let type_operator = Env.solve_type_path path env in
    let arguments = List.map args ~f:(fun type_ -> solve_type env vars type_) in
    type_wrap @@ T_app { type_operator; arguments }
  | T_arrow (param, return) ->
    let param = solve_type env vars param in
    let return = solve_type env vars return in
    (* TODO: param names? *)
    type_wrap @@ T_arrow { type1 = param; type2 = return; param_names = [] }
  | T_tuple fields ->
    let fields = List.map fields ~f:(fun field -> solve_type env vars field) in
    type_wrap @@ T_record (Row.create_tuple fields)
  | T_forall (bound, body) ->
    (* TODO: test poly and univar *)
    solve_type_forall env vars ~bound body


and solve_type_forall env vars ~bound body =
  match bound with
  | [] -> solve_type env vars body
  | (name, id) :: bound ->
    let var : Type_var.t = Type_var.fresh ~loc:Location.dummy ~name ~generated:false () in
    (* TODO: ensures id is not in vars *)
    Hashtbl.set vars ~key:id ~data:var;
    let body = solve_type_forall env vars ~bound body in
    Hashtbl.remove vars id;
    type_wrap @@ T_for_all { ty_binder = var; kind = Type; type_ = body }


let solve_type_poly env vars type_ =
  (* TODO: this is bad *)
  let external_vars = Hashtbl.copy vars in
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
      type_wrap @@ T_for_all { ty_binder = var; kind = Type; type_ = body })


let solve_type_decl env vars decl : type_expression =
  match decl with
  | T_record fields ->
    (* TODO: test rank-2 poly  *)
    let fields =
      List.map fields ~f:(fun (ident, type_) ->
          let label = Ident.name ident in
          let type_ = solve_type_poly env vars type_ in
          Label.Label (label, Location.dummy), type_)
    in
    let fields = Record.of_list fields in
    (* TODO: layout *)
    let layout = None in
    type_wrap @@ T_record { fields; layout }
  | T_alias manifest ->
    (* TODO: not poly tho *)
    solve_type_poly env vars manifest


let solve_var_pat env vars var_pat =
  let { var_pat_desc; var_pat_type } = var_pat in
  let var_pat_type = solve_type env vars var_pat_type in
  var_pat_desc, var_pat_type


let rec solve_pat env vars pat : _ * _ Pattern.t =
  let { pat_desc; pat_type } = pat in
  (* TODO: ascription in all types *)
  (* TODO: dummy *)
  (* TODO: poly type *)
  let type_ = Some (solve_type env vars pat_type) in
  match pat_desc with
  | P_unit -> env, pat_wrap @@ P_unit
  | P_var ident ->
    let var = fresh_value ident in
    let binder = Binder.make var type_ in
    let env = Env.enter_value ident var env in
    env, pat_wrap @@ P_var binder
  | P_tuple fields ->
    let env, rev_fields =
      List.fold_left fields ~init:(env, []) ~f:(fun (env, rev_fields) pat ->
          let env, field = solve_pat env vars pat in
          env, field :: rev_fields)
    in
    let fields = List.rev rev_fields in
    env, pat_wrap @@ P_tuple fields
  | P_record fields ->
    let env, rev_fields =
      List.fold_left fields ~init:(env, []) ~f:(fun (env, rev_fields) (label, pat) ->
          let { lbl_name
              ; lbl_res = _
              ; lbl_arg = _
              ; lbl_mut = _
              ; lbl_pos = _
              ; lbl_all = _
              ; lbl_repres = _
              ; lbl_private = _
              ; lbl_loc = _
              ; lbl_attributes = _
              ; lbl_uid = _
              }
            =
            label
          in
          let env, pat = solve_pat env vars pat in
          env, (Label.Label (lbl_name, Location.dummy), pat) :: rev_fields)
    in
    let fields = List.rev rev_fields in
    env, pat_wrap @@ P_record (Record.of_list fields)
  | P_variant _ -> error_unimplemented ()


let rec solve_expr env vars expr =
  let { expr_desc; expr_type } = expr in
  match expr_desc with
  | E_var path -> solve_expr_var env vars path
  | E_literal lit -> expr_wrap @@ E_literal lit
  | E_let (pat, value, body) ->
    (* TODO: this is poly pat? *)
    let inner_env, pat = solve_pat env vars pat in
    let value = solve_expr_poly env vars value in
    let body = solve_expr inner_env vars body in
    expr_wrap
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
    let { var_pat_desc; var_pat_type } = var_pat in
    let var = fresh_value var_pat_desc in
    let env = Env.enter_value var_pat_desc var env in
    let body = solve_expr env vars body in
    let binder =
      (* TODO: {mut,forced,initial} flag *)
      let param_annot = solve_type env vars var_pat_type in
      Param.make var (Some param_annot)
    in
    (* TODO: output_type *)
    expr_wrap @@ E_lambda { binder; output_type = None; result = body }
  | E_apply (lambda, args) ->
    let lambda = solve_expr env vars lambda in
    List.fold_left args ~init:lambda ~f:(fun lambda arg ->
        let arg = solve_expr env vars arg in
        expr_wrap @@ E_application { lamb = lambda; args = arg })
  | E_match (matchee, cases) ->
    let matchee = solve_expr env vars matchee in
    let cases =
      List.map cases ~f:(fun (pat, body) ->
          let env, pat = solve_pat env vars pat in
          let body = solve_expr env vars body in
          Match_expr.{ pattern = pat; body })
    in
    expr_wrap @@ E_matching { matchee; cases }
  | E_tuple fields ->
    let fields = Nonempty_list.map fields ~f:(fun field -> solve_expr env vars field) in
    expr_wrap @@ E_tuple fields
  | E_record fields ->
    let fields =
      List.map fields ~f:(fun (label, field) ->
          (* TODO: location *)
          Label.Label (label, Location.dummy), solve_expr env vars field)
    in
    expr_wrap @@ E_record (Record.of_list fields)
  | E_field (struct_, label) ->
    let struct_ = solve_expr env vars struct_ in
    let path = Label.Label (label, Location.dummy) in
    expr_wrap @@ E_accessor { struct_; path }


and solve_expr_var env vars path =
  let var = Env.solve_value_path path env in
  (* TODO: fix this on ligo *)
  match var with
  | { module_path = []; element } -> expr_wrap @@ E_variable element
  | { module_path; element } as var -> expr_wrap @@ E_module_accessor var


and solve_expr_poly env vars expr =
  (* TODO: this is also duplicated *)
  (* TODO: this is bad *)
  let external_vars = Hashtbl.copy vars in
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
      expr_wrap @@ E_type_abstraction { type_binder = var; result = body })


let solve_decl env vars decl =
  let { decl_desc } = decl in
  match decl_desc with
  | D_value (var_pat, value) ->
    (* TODO: duplicated logic regarding var_pat and Binder *)
    let { var_pat_desc = ident; var_pat_type } = var_pat in
    let var = fresh_value ident in
    let type_ = solve_type_poly env vars var_pat_type in
    let binder = Binder.make var (Some type_) in
    let value = solve_expr_poly env vars value in
    let env = Env.enter_value ident var env in
    ( env
    , decl_wrap @@ D_value { binder; expr = value; attr = Value_attr.default_attributes }
    )
  | D_type (ident, type_decl) ->
    let type_decl = solve_type_decl env vars type_decl in
    let var = fresh_type ident in
    let env = Env.enter_type ident var env in
    ( env
    , decl_wrap
      @@ D_type
           { type_binder = var
           ; type_expr = type_decl
           ; type_attr = Type_or_module_attr.default_attributes
           } )


let solve_decl env decl =
  let vars = Hashtbl.create (module Int) in
  let env, decl = solve_decl env vars decl in
  assert (Hashtbl.is_empty vars);
  env, decl


let solve_module env module_ =
  let env, rev_decl =
    List.fold_left module_ ~init:(env, []) ~f:(fun (env, rev_module) decl ->
        let env, decl = solve_decl env decl in
        env, decl :: rev_module)
  in
  env, List.rev rev_decl
