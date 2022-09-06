(* This file represente the context which give the association of values to types *)
module Location = Simple_utils.Location
open Ligo_prim
open Ast_typed

module HMap = Simple_utils.Map.Make(struct type t = type_expression
                                           let compare t1 t2 = Int.compare (hash_type_expression t1) (hash_type_expression t2)
                                    end)

module Typing = struct

  module Types = struct

    module ValueMap  = Simple_utils.Map.Make(ValueVar)
    module TypeMap   = Ast_typed.Helpers.IdMap.Make(TypeVar)
    module ModuleMap = Ast_typed.Helpers.IdMap.Make(ModuleVar)

    type values  = type_expression ValueMap.t
    type types   = type_expression TypeMap.t
    type modules = context ModuleMap.t
    and  context = {
        values  : values  ;
        types   : types   ;
        modules : modules ;
      }

    (* Recursively fetches all types from the given module and its submodules

    For example, to get the list of all types declared in a module and its submodules,
    we perform a recusive search in the context maps and accumulate the types found.
    Then, in order to convert those maps into a id-sorted list, we can :
    1. Use [merge], and convert the merged map into a (sorted) kv_list. This will remove duplicate eponym types
    2. Use [to_kvi_list], append all the kvi_lists, and sort the resulting kvi_list by id, into a kv_list, this keeps duplicates *)
    let get_module_types : context -> (TypeVar.t * type_expression) list =
      fun ctxt ->
      let rec aux : context -> type_expression TypeMap.kvi_list =
        fun ctxt ->
          (* First, get types in the current scope *)
          let accu_types = TypeMap.to_kvi_list @@ ctxt.types in
          (* Then recursively fetch those in the submodules*)
          let module_list = ModuleMap.to_kv_list ctxt.modules in
          List.fold module_list
            ~init:accu_types
            ~f:(fun accu_types (_, ctxt) -> List.rev_append accu_types @@ aux ctxt)
      in
      TypeMap.sort_to_kv_list @@ aux ctxt

  end (* of module Types *)


  type t = Types.context
  let empty : t = { values = Types.ValueMap.empty ; types = Types.TypeMap.empty ; modules = Types.ModuleMap.empty }

  module PP = struct
    open Format
    open Ast_typed.PP
    open Types

    let print_list elt_printer ppf l =
      let rec aux ppf = function
      | [] -> fprintf ppf ""
      | hd :: tl -> Format.fprintf ppf "| %a@,%a" elt_printer hd aux tl
      in
      fprintf ppf "@[<hv>%a@]" aux l

    let value_binding ppf (ev,te) =
      fprintf ppf "%a => %a" ValueVar.pp ev type_expression te
    let type_binding ppf (type_var,type_) =
      fprintf ppf "%a => %a" TypeVar.pp type_var type_expression type_

    let rec module_binding ppf (mod_var,type_) =
      fprintf ppf "%a => %a" ModuleVar.pp mod_var context type_

    and context ppf {values;types;modules} =
      fprintf ppf "context:@,{[@[<v 2>@,%a; @,%a; @,%a; ]}@]"
        (print_list value_binding)  (ValueMap.to_kv_list  values)
        (print_list type_binding)   (TypeMap.to_kv_list   types)
        (print_list module_binding) (ModuleMap.to_kv_list modules)

  end (* of module PP *)
  let pp =  PP.context

  (* Not commutative as a shadows b*)
  let union : t -> t -> t = fun a b ->
    let merger : Types.ValueMap.key -> 'a option -> 'a option -> 'a option =
      fun _ v1 v2 ->
        match (v1, v2) with
        | None,   None   -> None
        | Some v, None   -> Some v
        | None,   Some v -> Some v
        | Some v1, Some _ -> Some v1 (* not commutative : a shadows b *)
    in
    Types.{values = ValueMap.merge merger a.values b.values; types = TypeMap.merge a.types b.types ; modules = ModuleMap.merge a.modules b.modules}

  (* TODO: generate *)
  let get_types  : t -> Types.types  = fun { values=_ ; types ; modules=_ } -> types
  (* TODO: generate *)
  let get_modules : t -> Types.modules = fun { values=_ ; types=_ ; modules } -> modules


  (* TODO: generate : these are now messy, clean them up. *)
  let add_value : t -> ValueVar.t -> Ast_typed.type_expression -> t = fun c ev te ->
    let values =  Types.ValueMap.add ev te c.values in
    {c with values}

  let add_type : t -> TypeVar.t -> Ast_typed.type_expression -> t = fun c tv te ->
    let types = Types.TypeMap.add c.types tv te in
    {c with types}

  (* we represent for_all types as themselves because we don't have typechecking yet *)
  let add_type_var : t -> TypeVar.t -> unit -> t = fun c tv () ->
    add_type c tv (Ast_typed.t_variable tv ())

  (* we use type_var while we don't have kind checking *)
  let add_kind : t -> TypeVar.t -> unit -> t = fun c tv () ->
    add_type_var c tv ()
  let add_module : t -> ModuleVar.t -> t -> t = fun c mv te ->
    let modules = Types.ModuleMap.add c.modules mv te in
    {c with modules}

  let get_value (e:t)  = List.Assoc.find ~equal:ValueVar.equal @@ Types.ValueMap.to_kv_list e.values
  let get_type (e:t)   = List.Assoc.find ~equal:TypeVar.equal @@ Types.TypeMap.to_kv_list e.types
  let get_module (e:t) = List.Assoc.find ~equal:ModuleVar.equal @@ Types.ModuleMap.to_kv_list e.modules

  let get_type_vars : t -> TypeVar.t list  = fun { values=_ ; types ; modules=_ } -> fst @@ List.unzip @@ Types.TypeMap.to_kv_list types

  let rec context_of_module_expr : outer_context:t -> Ast_typed.module_expr -> t = fun ~outer_context me ->
    match me.wrap_content with
    | M_struct declarations -> (
      let f : t -> Ast_typed.decl -> t = fun acc d ->
        match Location.unwrap d with
        | D_value {binder;expr;attr={public;_}} ->
           if public then add_value acc binder.var expr.type_expression
           else acc
        | D_type {type_binder;type_expr;type_attr={public;_}} ->
           if public then add_type acc type_binder type_expr
           else acc
        | D_module {module_binder;module_;module_attr={public;_}} ->
           if public then
             let context = context_of_module_expr ~outer_context:(union acc outer_context) module_ in
             add_module acc module_binder context
           else acc
      in
      List.fold ~f ~init:empty declarations
    )
    | M_variable module_binder -> (
      let ctxt_opt = get_module outer_context module_binder in
      match ctxt_opt with
      | Some x -> x
      | None -> empty
    )
    | M_module_path path -> (
      Simple_utils.List.Ne.fold_left path
        ~f:(fun ctxt name ->
          match get_module ctxt name with
          | Some x -> x
          | None -> empty
        )
        ~init:outer_context
    )

  (* Load context from the outside declarations *)
  let init ?env () =
    match env with None -> empty
                 | Some (env) ->
                    let f : t -> Ast_typed.decl -> t = fun c d ->
                      match Location.unwrap d with
                      | D_value {binder;expr;attr=_}  -> add_value c binder.var expr.type_expression
                      | D_type {type_binder;type_expr;type_attr=_} -> add_type c type_binder type_expr
                      | D_module {module_binder;module_;module_attr=_} ->
                         let mod_context = context_of_module_expr ~outer_context:c module_ in
                         add_module c module_binder mod_context
                    in
                    Environment.fold ~f ~init:empty env

  open Ast_typed.Types


(*
  for any constructor [ctor] that belong to a sum-type `t` in the context [ctxt] return a 4-uple list:
  1. the declaration name for type `t`
  2. list of abstracted type variables in the constructor parameter (e.g. ['a ; 'b] for `Foo of ('a * int * 'b)`)
  3. type of the constructor parameter (e.g. `'a * int * 'b` for `Foo of ('a * int * 'b)`)
  4. type of the sum-type found in the context

  NOTE : Here, we return all the matching types found in the module and its submodules, even if we found matching types in current scope.
  Indeed, we want to check for other matching types in submodules anyway, to warn the user in case of conflict.
  For example :
    module Mod_a = struct
      type tx = A of int
    end
    type ty = A of int
    let a = A 42
  Here, for [a], we find a matching type [ty] in the current scope, but we still want to warn the user that type [Mod_a.tx] matches too.
*)
  let get_sum: Label.t -> t -> (TypeVar.t * TypeVar.t list * type_expression * type_expression) list =
    fun ctor ctxt ->
        let filter_tsum = fun (var,type_) ->
          let t_params, type_ = Ast_typed.Helpers.destruct_type_abstraction type_ in
          match type_.type_content with
          | T_sum m -> (
            match Record.LMap.find_opt ctor m.fields with
            | Some ({associated_type ; _}: row_element) -> Some (var,t_params, associated_type , type_)
            | None -> None
          )
          | _ -> None
        in
        (* Fetch all types declared in current module and its submodules *)
        let module_types = Types.get_module_types ctxt in
        (*  Also add the shadowed t_sum types nested in the fetched types.
            Since context is made of maps, all shadowed types are absent from the context.
            However we still want the shadowed nested t_sum, see [add_shadowed_nested_t_sum] *)
        let module_types = List.fold (List.rev module_types) ~init:[] ~f:Ast_typed.Helpers.add_shadowed_nested_t_sum in
        (* For all types found, pick only the T_sum, and make 4-uple out of them  *)
        let matching_t_sum = List.filter_map ~f:filter_tsum @@ module_types in
        (* Filter out duplicates (this prevents false warnings of "infered type is X but could also be X"
           when a same type is present several times in the context) *)
        let remove_doubles l : (TypeVar.t * TypeVar.t list * type_expression * type_expression) list =
          let add_no_dup l elt : (TypeVar.t * TypeVar.t list * type_expression * type_expression) list =
            let (_tv, _tvs, _te, te) : (TypeVar.t * TypeVar.t list * type_expression * type_expression) = elt in
            match List.find l ~f:(fun (_tv, _tvs, _te, te') -> hash_type_expression te = hash_type_expression te') with
            | Some _ -> l
            | None -> elt :: l
          in
            List.rev @@ List.fold l ~f:add_no_dup ~init:[]
        in
        let matching_t_sum = remove_doubles matching_t_sum in
        let general_type_opt = List.find ~f:(fun (_, tvs, _, _) -> not @@ List.is_empty tvs) matching_t_sum in
        match general_type_opt with
          Some general_type -> [general_type]
        | None -> matching_t_sum

  let get_record : _ Record.t -> t -> (TypeVar.t option * rows) option = fun lmap e ->
    let lst_kv  = Record.LMap.to_kv_list_rev lmap in
    let rec rec_aux e =
      let aux = fun (_,type_) ->
        match type_.type_content with
        | T_record m -> Simple_utils.Option.(
            let lst_kv' = Record.LMap.to_kv_list_rev m.fields in
            let m = map ~f:(fun () -> m) @@ Ast_typed.Misc.assert_list_eq
                                              ( fun (ka,(va:row_element)) (kb,vb) ->
                                                let* () = Ast_typed.Misc.assert_eq ka kb in
                                                Ast_typed.Misc.assert_type_expression_eq (va.associated_type, vb.associated_type)
                                              ) lst_kv lst_kv' in
            map ~f:(fun m -> (type_.orig_var,m)) @@ m
                        )
        | _ -> None
      in
      match List.find_map ~f:aux @@ Types.TypeMap.to_kv_list @@ get_types e with
        Some _ as s -> s
      | None ->
         let modules = get_modules e in
         List.fold_left ~f:(fun res (__,module_) ->
             match res with Some _ as s -> s | None -> rec_aux module_
           ) ~init:None (Types.ModuleMap.to_kv_list modules)
    in rec_aux e
end

module Hashes = struct
  module HTBL = Caml.Hashtbl.Make(struct type t = type_expression
                                         let hash = hash_type_expression
                                         let equal t1 t2 = match assert_type_expression_eq (t1, t2) with
                                           | Some _ -> true
                                           | None -> false
                                  end)

  let hashtbl : (ModuleVar.t list * TypeVar.t) HTBL.t = HTBL.create 256

  let context = ref (false, Typing.empty)
  let set_context (t : Typing.t) : unit = context := (false, t)

  let hash_types () : unit =
    let (hashed, t) = ! context in
    if hashed then
      ()
    else
      let rec aux path (t : Typing.t) =
        let types = Typing.Types.TypeMap.to_kv_list @@ Typing.get_types t in
        let modules = Typing.Types.ModuleMap.to_kv_list @@ Typing.get_modules t in
        List.iter (List.rev types) ~f:(fun (v, t) -> HTBL.add hashtbl t (path, v)) ;
        List.iter (List.rev modules) ~f:(fun (v, t) -> aux (path @ [v]) t) in
      HTBL.clear hashtbl ;
      aux [] t ;
      context := (true, t)

  let find_type (t : type_expression) : (ModuleVar.t list * TypeVar.t) option =
    HTBL.find_opt hashtbl t
end

module App = struct
  type e = { args : type_expression list ; old_expect : type_expression option  }
  type t = { expect : type_expression option ;
             history : e list }
  let pop ({ history ; _ } : t) = match history with
      { args ; _ }  :: _ -> Some args | [] -> None
  let create expect : t = { expect ; history = [] }
  let push expect args ctxt : t =
    let { expect = old_expect ; history } = ctxt in
    { expect ; history = { args ; old_expect } :: history }
  let get_expect ({ expect ; _ } : t) = expect
  let update_expect expect { history ; _ } : t = { expect ; history }
end

type typing_context = Typing.t
type app_context = App.t
type t = app_context * typing_context
