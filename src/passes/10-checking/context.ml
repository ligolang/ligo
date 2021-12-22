(* This file represente the context which give the association of values to types *)
module Location = Simple_utils.Location
module Var      = Simple_utils.Var
module Types = struct 
  open Ast_typed 

  (* Use of list to allow type shadowing, which is weird *)
  (* We should use data structure that are better for lookup but we first need 
   to agree on typechecker property *)
  type values  = (expression_variable * type_expression) List.t
  type types   = (type_variable       * type_expression) List.t
  type modules = (module_variable     * context        ) List.t
  and  context = { (* TODO : move to sets, requires new architecture *)
    values  : values  ;
    types   : types   ;
    modules : modules ;
  }
end

type t = Types.context
let empty : t = { values = []; types = [] ; modules = [] }

module PP = struct
  open Format
  open Ast_typed.PP
  open Simple_utils.PP_helpers
  open Types

  let list_sep_scope x = list_sep x (const " | ")
  let value_binding ppf (ev,te) = 
    fprintf ppf "%a => %a" expression_variable ev type_expression te
  let type_binding ppf (type_var,type_) =
    fprintf ppf "%a => %a" type_variable type_var type_expression type_

  let rec module_binding ppf (mod_var,type_) =
    fprintf ppf "%a => %a" module_variable mod_var context type_

  and context ppf {values;types;modules} =
    fprintf ppf "{[ %a; @; %a; %a; ]}"
      (list_sep_scope value_binding ) values
      (list_sep_scope type_binding  ) types
      (list_sep_scope module_binding) modules

end
let pp =  PP.context


let _get_values : t -> Types.values = fun { values ; types=_ ; modules=_ } -> values
(* TODO: generate *)
let get_types  : t -> Types.types  = fun { values=_ ; types ; modules=_ } -> types
(* TODO: generate *)
let get_modules : t -> Types.modules = fun { values=_ ; types=_ ; modules } -> modules


(* TODO: generate : these are now messy, clean them up. *)
let add_value : Ast_typed.expression_variable -> Ast_typed.type_expression -> t -> t = fun ev te e -> 
  let values = (ev,te)::e.values in
  {e with values}

let add_type : Ast_typed.type_variable -> Ast_typed.type_expression -> t -> t = fun tv te e -> 
  let types = (tv,te)::e.types in
  {e with types}

(* Im not super happy with the representation of for_all types.. *)
let add_type_var : Ast_typed.type_variable -> unit -> t -> t = fun tv () e -> 
  add_type tv (Ast_typed.t_variable tv) e
let add_module : Ast_typed.module_variable -> t -> t -> t = fun mv te e -> 
  let modules = (mv,te)::e.modules in
  {e with modules}

let get_value (e:t)  = List.Assoc.find ~equal:(Location.equal_content ~equal:Var.equal) e.values
let get_type (e:t)   = List.Assoc.find ~equal:Var.equal e.types
let get_module (e:t) = List.Assoc.find ~equal:String.equal e.modules

let get_type_vars : t -> Ast_typed.type_variable list  = fun { values=_ ; types ; modules=_ } -> fst @@ List.unzip types

(* Load context from the outside declarations *)
let rec add_ez_module : Ast_typed.module_variable -> Ast_typed.module' ->t -> t = fun mv m c ->
  let f c d = match Location.unwrap d with
    Ast_typed.Declaration_constant {name=_;binder;expr;attr={public;_}}  -> if public then add_value binder expr.type_expression c else c
  | Declaration_type {type_binder;type_expr;type_attr={public}} -> if public then add_type  type_binder type_expr c else c
  | Declaration_module {module_binder;module_;module_attr={public}} -> if public then add_ez_module module_binder module_ c else c
  | Module_alias {alias;binders} -> 
    let m = Simple_utils.List.Ne.fold_left ~f:(fun c b -> Option.bind ~f:(fun c -> get_module c b) c) ~init:(Some c) binders in
    let c' = Option.map ~f:(fun m -> add_module alias m c) m in
    Option.value ~default:c c'
  in
  let context = List.fold ~f ~init:c @@ m in
  let modules = (mv,context)::c.modules in
  {c with modules}

let init ?env () = 
  match env with None -> empty
  | Some (env) ->
    let f c d = match Location.unwrap d with
      Ast_typed.Declaration_constant {name=_;binder;expr;attr=_}  -> add_value binder expr.type_expression c
    | Declaration_type {type_binder;type_expr;type_attr=_} -> add_type  type_binder type_expr c
    | Declaration_module {module_binder;module_;module_attr=_} -> add_ez_module module_binder module_ c
    | Module_alias {alias;binders} -> 
      (* value_exn is ok since the env as pass the typer or is written by us *)
      add_module alias (Simple_utils.List.Ne.fold_left ~f:(fun c b -> Option.value_exn (get_module c b)) ~init:c binders) c
    in
    Environment.fold ~f ~init:empty @@ env
  
open Ast_typed.Types


let get_constructor : label -> t -> (type_expression * type_expression) option = fun k x -> (* Left is the constructor, right is the sum type *)
  let rec rec_aux e =
    let aux = fun (_,type_) ->
    match type_.type_content with
    | T_sum m ->
      (match LMap.find_opt k m.content with
          Some {associated_type ; _} -> Some (associated_type , type_)
        | None -> None)
    | _ -> None
    in
    match List.find_map ~f:aux @@ get_types e with
      Some _ as s -> s
    | None ->
      let modules = get_modules e in
      List.fold_left ~f:(fun res (_,module_) ->
        match res with Some _ as s -> s | None -> rec_aux module_
      ) ~init:None modules
  in rec_aux x

let get_constructor_parametric : label -> t -> (type_variable list * type_expression * type_expression) option = fun k x -> (* Left is the constructor, right is the sum type *)
  let rec rec_aux e =
    let rec aux av = fun (_t,type_) ->
      match type_.type_content with
      | T_sum m ->
         (match LMap.find_opt k m.content with
            Some {associated_type ; _} -> Some (av, associated_type , type_)
          | None -> None)
      | T_abstraction { ty_binder ; kind = _ ; type_ } ->
         aux (Location.unwrap ty_binder :: av) (_t,type_)
      | _ -> None in
    let aux = aux []in
    match List.find_map ~f:aux (get_types e) with
      Some _ as s -> s
    | None ->
      let modules = get_modules e in
      List.fold_left ~f:(fun res (_,module_) ->
        match res with Some _ as s -> s | None -> rec_aux module_
      ) ~init:None modules
  in rec_aux x

let get_record : _ label_map -> t -> (type_variable option * rows) option = fun lmap e ->
  let rec rec_aux e =
    let aux = fun (_,type_) ->
    match type_.type_content with
    | T_record m -> Simple_utils.Option.(
      let lst_kv  = LMap.to_kv_list_rev lmap in
      let lst_kv' = LMap.to_kv_list_rev m.content in
      let m = map ~f:(fun () -> m) @@ Ast_typed.Misc.assert_list_eq
        ( fun (ka,va) (kb,vb) ->
          let Label ka = ka in
          let Label kb = kb in
          let* () = Ast_typed.Misc.assert_eq ka kb in
          Ast_typed.Misc.assert_type_expression_eq (va.associated_type, vb.associated_type)
        ) lst_kv lst_kv' in
      map ~f:(fun m -> (type_.orig_var,m)) @@ m
    )
    | _ -> None
    in
    match List.find_map ~f:aux (get_types e) with
      Some _ as s -> s
    | None ->
      let modules = get_modules e in
      List.fold_left ~f:(fun res (__,module_) ->
        match res with Some _ as s -> s | None -> rec_aux module_
      ) ~init:None modules
  in rec_aux e


let get_sum : _ label_map -> t -> rows option = fun lmap e ->
  let rec rec_aux e =
    let aux = fun (_,type_) ->
    match type_.type_content with
    | T_sum m -> Simple_utils.Option.(
      let lst_kv  = LMap.to_kv_list_rev lmap in
      let lst_kv' = LMap.to_kv_list_rev m.content in
      map ~f:(fun () -> m) @@ Ast_typed.Misc.assert_list_eq (
        fun (ka,va) (kb,vb) ->
          let Label ka = ka in
          let Label kb = kb in
          let* () = Ast_typed.Misc.assert_eq ka kb in
          Ast_typed.Misc.assert_type_expression_eq (va.associated_type, vb.associated_type)
      ) lst_kv lst_kv'
    )
    | _ -> None
    in
    match List.find_map ~f:aux @@ (get_types e) with
      Some _ as s -> s
    | None ->
      let modules = get_modules e in
      List.fold_left ~f:(fun res (_,module_) ->
        match res with Some _ as s -> s | None -> rec_aux module_
      ) ~init:None modules
  in rec_aux e

