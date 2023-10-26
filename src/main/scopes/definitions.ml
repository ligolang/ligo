open Ligo_prim
open Types
module AST = Ast_core
module VVar = Value_var
module TVar = Type_var
module MVar = Module_var
module Formatter = Formatter
module Api_helper = Api_helper
module LSet = Types.LSet
module Location = Simple_utils.Location
module Trace = Simple_utils.Trace
module Types = Types

let get_location_of_module_path : Module_var.t list -> Location.t =
 fun mvs ->
  List.fold mvs ~init:Location.dummy ~f:(fun loc m ->
      Location.cover loc (Module_var.get_location m))


let defs_of_vvar ?(body : AST.expression option)
    : VVar.t -> def_type -> string list -> def list -> def list
  =
 fun vvar def_type mod_path acc ->
  if VVar.is_generated vvar
  then acc
  else (
    let name = get_binder_name vvar in
    let vdef : vdef =
      let uid : Uid.t = Uid.make name (VVar.get_location vvar) in
      let range : Location.t = VVar.get_location vvar in
      let body_range : Location.t option =
        Option.map body ~f:(fun body ->
            match body.expression_content with
            (* For [E_recursive], we have to dig into [r.lambda.result] to get the real
               body range because otherwise [body.location] will just return the "rec"
               keyword's range, for some reason *)
            | E_recursive r -> r.lambda.result.location
            | _ -> body.location)
      in
      let t : type_case = Unresolved (* Filled in a later pass *) in
      let references : LSet.t = LSet.empty (* Filled in a later pass *) in
      { name; uid; range; body_range; t; references; def_type; mod_path }
    in
    Variable vdef :: acc)


let defs_of_binder ~(body : AST.expression)
    : _ Binder.t -> def_type -> string list -> def list -> def list
  =
 fun binder def_type mod_path acc ->
  defs_of_vvar ~body (Binder.get_var binder) def_type mod_path acc


let defs_of_tvar ?(bindee : Ast_core.type_expression option)
    : TVar.t -> def_type -> string list -> def list -> def list
  =
 fun tvar def_type mod_path acc ->
  if TVar.is_generated tvar
  then acc
  else (
    let name = get_type_binder_name tvar in
    let tdef : tdef =
      let uid : Uid.t = Uid.make name (TVar.get_location tvar) in
      let range : Location.t = TVar.get_location tvar in
      let body_range : Location.t option =
        Option.map bindee ~f:(fun bindee -> bindee.location (* How to get this ? *))
      in
      let content : Ast_core.type_expression option = bindee in
      let references : LSet.t = LSet.empty (* Filled in a later pass *) in
      { name; uid; range; body_range; content; def_type; references; mod_path }
    in
    Type tdef :: acc)


let defs_of_mvar ?(bindee_location : Location.t option) ~(mod_case : mod_case)
    : MVar.t -> def_type -> string list -> def list -> def list
  =
 fun mvar def_type mod_path acc ->
  if MVar.is_generated mvar
  then acc
  else (
    let name = get_mod_binder_name mvar in
    let mdef : mdef =
      let uid : Uid.t = Uid.make name (MVar.get_location mvar) in
      let range : Location.t = MVar.get_location mvar in
      let body_range : Location.t option = bindee_location in
      let references : LSet.t = LSet.empty (* Filled in a later pass *) in
      { name; uid; range; body_range; references; mod_case; def_type; mod_path }
    in
    Module mdef :: acc)


let defs_of_mvar_mod_expr ~(bindee : Ast_core.module_expr)
    : mod_case:mod_case -> MVar.t -> def_type -> string list -> def list -> def list
  =
  let bindee_location =
    match Location.unwrap bindee with
    | M_struct _ -> Location.get_location bindee
    | M_variable mvar -> MVar.get_location mvar
    | M_module_path mpath -> get_location_of_module_path @@ List.Ne.to_list mpath
  in
  defs_of_mvar ~bindee_location


let defs_of_mvar_sig_expr ~(bindee : Ast_core.signature_expr)
    : mod_case:mod_case -> MVar.t -> def_type -> string list -> def list -> def list
  =
  let bindee_location =
    match Location.unwrap bindee with
    | S_sig _ -> Location.get_location bindee
    | S_path mpath -> get_location_of_module_path @@ List.Ne.to_list mpath
  in
  defs_of_mvar ~bindee_location


let defs_of_mvar_signature ~(bindee : Ast_core.signature)
    : mod_case:mod_case -> MVar.t -> def_type -> string list -> def list -> def list
  =
  defs_of_mvar ?bindee_location:None


let defs_of_pattern ~(body : AST.expression)
    :  AST.type_expression option Linear_pattern.t -> def_type -> string list -> def list
    -> def list
  =
 fun ptrn def_type mod_path acc ->
  let ptrn_binders = AST.Pattern.binders ptrn in
  let f defs binder = defs_of_binder ~body binder def_type mod_path defs in
  let defs = List.fold ~init:acc ~f ptrn_binders in
  defs


let add_inner_mod_path (module_binder : MVar.t) (mod_path : string list) : string list =
  mod_path @ [ Format.asprintf "%a" MVar.pp module_binder ]


let rec defs_of_expr : AST.expression -> string list -> def list -> def list =
 fun e mod_path acc ->
  let self = defs_of_expr in
  let defs_of_lambda : _ Lambda.t -> def list -> def list =
   fun { binder; output_type; result } acc ->
    let vvar = Param.get_var binder in
    self result mod_path @@ defs_of_vvar ~body:result vvar Parameter mod_path acc
  in
  match e.expression_content with
  (* Base *)
  | E_variable v -> acc
  | E_contract x -> acc
  | E_literal l -> acc
  | E_constant c -> acc
  | E_application { lamb; args } -> self lamb mod_path @@ self args mod_path acc
  | E_lambda lambda -> defs_of_lambda lambda acc
  | E_recursive { fun_name; fun_type; lambda; force_lambdarec } ->
    (* fun_name is already added by the parent E_let_in so don't need to add it here *)
    defs_of_lambda lambda acc
  | E_type_abstraction { type_binder; result } -> self result mod_path acc
  | E_let_in { let_binder; rhs; let_result; attributes }
  | E_let_mut_in { let_binder; rhs; let_result; attributes } ->
    defs_of_pattern ~body:rhs let_binder Local mod_path
    @@ self rhs mod_path
    @@ self let_result mod_path acc
  | E_type_in { type_binder; rhs; let_result } ->
    defs_of_tvar ~bindee:rhs type_binder Local mod_path @@ self let_result mod_path acc
  | E_mod_in { module_binder; rhs; let_result } ->
    let inner_mod_path = add_inner_mod_path module_binder mod_path in
    let mod_case = mod_case_of_mod_expr ~defs_of_decls rhs inner_mod_path in
    defs_of_mvar_mod_expr ~mod_case ~bindee:rhs module_binder Local inner_mod_path
    @@ self let_result mod_path acc
  | E_raw_code { language = _; code = _ } -> acc
  (* Variant *)
  | E_constructor { constructor; element } -> self element mod_path acc
  | E_matching { matchee; cases } ->
    let defs_of_match_cases cases acc =
      let defs_of_match_case acc ({ pattern; body } : _ AST.Match_expr.match_case) =
        defs_of_pattern ~body pattern Local mod_path @@ defs_of_expr body mod_path acc
      in
      List.fold ~init:acc ~f:defs_of_match_case cases
    in
    defs_of_match_cases cases @@ self matchee mod_path acc
  (* Record *)
  | E_record r -> Record.fold ~init:acc ~f:(fun acc entry -> self entry mod_path acc) r
  | E_accessor { struct_; path } ->
    self struct_ mod_path acc (* Is it possible to have decl in there ? *)
  | E_update { struct_; path; update } ->
    self struct_ mod_path @@ self update mod_path acc
  (* Advanced *)
  | E_ascription { anno_expr; type_annotation } -> self anno_expr mod_path acc
  | E_module_accessor macc -> acc
  (* Imperative *)
  | E_assign { binder; expression } ->
    (* binder := new_value, the binder is already declared so we don't add it to the dec list *)
    self expression mod_path acc
  | E_for { binder; start; final; incr; f_body } ->
    defs_of_vvar ~body:f_body binder Local mod_path
    @@ self start mod_path
    @@ self final mod_path
    @@ self incr mod_path
    @@ self f_body mod_path
    @@ acc
  | E_for_each { fe_binder = vvar1, vvar2_opt; collection; collection_type = _; fe_body }
    ->
    let body = fe_body in
    let acc =
      match vvar2_opt with
      | Some vvar -> defs_of_vvar ~body vvar Local mod_path acc
      | None -> acc
    in
    self fe_body mod_path
    @@ self collection mod_path
    @@ defs_of_vvar ~body vvar1 Local mod_path acc
  | E_while { cond; body } -> self cond mod_path @@ self body mod_path acc


and alias_of_mvars : Module_var.t list -> mod_case =
 fun mvars ->
  let path = List.map ~f:mvar_to_id mvars in
  (* The resolved name will be filled later. *)
  Alias (path, None)


and mod_case_of_mod_expr
    :  defs_of_decls:
         (AST.declaration list -> def_type -> string list -> def list -> def list)
    -> AST.module_expr -> string list -> mod_case
  =
 fun ~defs_of_decls mod_expr mod_path ->
  match Location.unwrap mod_expr with
  | M_struct decls -> Def (defs_of_decls decls Module_field mod_path [])
  | M_variable mod_var -> alias_of_mvars [ mod_var ]
  | M_module_path mod_path -> alias_of_mvars @@ List.Ne.to_list mod_path


and mod_case_of_signature : AST.signature -> string list -> mod_case =
 fun sig' mod_path -> Def (defs_of_signature sig' Module_field mod_path [])


and mod_case_of_sig_expr : AST.signature_expr -> string list -> mod_case =
 fun sig_expr mod_path ->
  match Location.unwrap sig_expr with
  | S_sig decls -> Def (defs_of_signature decls Module_field mod_path [])
  | S_path mod_path -> alias_of_mvars @@ List.Ne.to_list mod_path


and defs_of_decl : AST.declaration -> def_type -> string list -> def list -> def list =
 fun decl def_type mod_path acc ->
  match Location.unwrap decl with
  | D_value { binder; expr; attr } ->
    defs_of_binder ~body:expr binder def_type mod_path @@ defs_of_expr expr mod_path acc
  | D_irrefutable_match { pattern; expr; attr } ->
    defs_of_pattern ~body:expr pattern def_type mod_path @@ defs_of_expr expr mod_path acc
  | D_type { type_binder; type_expr; type_attr } ->
    defs_of_tvar ~bindee:type_expr type_binder def_type mod_path acc
  | D_module { module_binder; module_; module_attr; annotation } ->
    let inner_mod_path = add_inner_mod_path module_binder mod_path in
    (* Here, the module body's defs are within the lhs_def, mod_case_of_mod_expr
       recursively calls defs_of_decl *)
    let mod_case : mod_case =
      mod_case_of_mod_expr ~defs_of_decls module_ inner_mod_path
    in
    let acc =
      defs_of_mvar_mod_expr ~mod_case ~bindee:module_ module_binder def_type mod_path acc
    in
    Option.value_map annotation ~default:acc ~f:(fun annotation ->
        match mod_case_of_sig_expr annotation.signature mod_path with
        | Def sig_defs -> sig_defs @ acc
        | Alias _ -> acc)
  | D_module_include module_ ->
    (match mod_case_of_mod_expr ~defs_of_decls module_ mod_path with
    | Alias _ -> acc
    | Def x -> x @ acc)
  | D_signature { signature_binder; signature; signature_attr = _ } ->
    let inner_mod_path = add_inner_mod_path signature_binder mod_path in
    let mod_case = mod_case_of_sig_expr signature inner_mod_path in
    defs_of_mvar_sig_expr
      ~mod_case
      ~bindee:signature
      signature_binder
      def_type
      mod_path
      acc


and defs_of_sig_expr
    : AST.signature_expr -> def_type -> string list -> def list -> def list
  =
 fun sig_expr def_type mod_path ->
  match Location.unwrap sig_expr with
  | S_sig sig' -> defs_of_signature sig' def_type mod_path
  | S_path _ -> Fn.id


and defs_of_sig_item : AST.sig_item -> def_type -> string list -> def list -> def list =
 fun item def_type mod_path acc ->
  match item with
  | S_value (var, ty_expr, _attrs) -> defs_of_vvar var def_type mod_path acc
  | S_type (var, ty_expr) -> defs_of_tvar ~bindee:ty_expr var def_type mod_path acc
  | S_type_var var -> defs_of_tvar var def_type mod_path acc
  | S_module (var, sig') | S_module_type (var, sig') ->
    let inner_mod_path = add_inner_mod_path var mod_path in
    let mod_case : mod_case = mod_case_of_signature sig' inner_mod_path in
    defs_of_mvar_signature ~mod_case ~bindee:sig' var def_type mod_path acc
  | S_include sig_expr -> defs_of_sig_expr sig_expr def_type mod_path acc


and defs_of_signature : AST.signature -> def_type -> string list -> def list -> def list =
 fun { items } def_type mod_path acc ->
  List.fold_left items ~init:acc ~f:(fun acc sig_item ->
      defs_of_sig_item sig_item def_type mod_path acc)


and defs_of_decls
    : AST.declaration list -> def_type -> string list -> def list -> def list
  =
 fun decls def_type mod_path acc ->
  List.fold ~init:acc ~f:(fun accu decl -> defs_of_decl decl def_type mod_path accu) decls


let definitions : AST.program -> def list -> def list =
 fun prg acc -> defs_of_decls prg Global [] acc


module Of_Stdlib = struct
  let defs_of_decl : AST.declaration -> def_type -> string list -> def list -> def list =
   fun decl def_type mod_path acc ->
    match Location.unwrap decl with
    | D_value { binder; expr; attr } ->
      defs_of_binder ~body:expr binder def_type mod_path acc
    | D_irrefutable_match { pattern; expr; attr } ->
      defs_of_pattern ~body:expr pattern def_type mod_path acc
    | D_type { type_binder; type_expr; type_attr } ->
      defs_of_tvar ~bindee:type_expr type_binder def_type mod_path acc
    | D_module { module_binder; module_; module_attr; annotation } ->
      let inner_mod_path = add_inner_mod_path module_binder mod_path in
      (* Here, the module body's defs are within the lhs_def,
         mod_case_of_mod_expr recursively calls defs_of_decl *)
      let mod_case : mod_case =
        mod_case_of_mod_expr module_ ~defs_of_decls inner_mod_path
      in
      let acc =
        defs_of_mvar_mod_expr
          ~mod_case
          ~bindee:module_
          module_binder
          def_type
          mod_path
          acc
      in
      Option.value_map annotation ~default:acc ~f:(fun annotation ->
          defs_of_mvar_sig_expr
            ~mod_case:(mod_case_of_sig_expr annotation.signature mod_path)
            ~bindee:annotation.signature
            module_binder
            def_type
            mod_path
            acc)
    | D_module_include module_ ->
      (match mod_case_of_mod_expr ~defs_of_decls module_ mod_path with
      | Alias _ -> acc
      | Def x -> x @ acc)
    | D_signature { signature_binder; signature; signature_attr = _ } ->
      let inner_mod_path = add_inner_mod_path signature_binder mod_path in
      let mod_case = mod_case_of_sig_expr signature inner_mod_path in
      defs_of_mvar_sig_expr
        ~mod_case
        ~bindee:signature
        signature_binder
        def_type
        mod_path
        acc


  let defs_of_decls
      : AST.declaration list -> def_type -> string list -> def list -> def list
    =
   fun decls def_type mod_path acc ->
    List.fold
      ~init:acc
      ~f:(fun accu decl -> defs_of_decl decl def_type mod_path accu)
      decls


  let definitions : AST.program -> def list = fun prg -> defs_of_decls prg Global [] []
end
