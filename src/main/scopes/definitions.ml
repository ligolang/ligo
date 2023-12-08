open Ligo_prim
open Types
module AST = Ast_core
module VVar = Value_var
module TVar = Type_var
module MVar = Module_var
module LSet = Types.LSet
module SMap = Map.Make (String)

let ( <@ ) = Simple_utils.Function.( <@ )

type t = def list

let get_location_of_module_path : Module_var.t list -> Location.t =
 fun mvs ->
  List.fold mvs ~init:Location.dummy ~f:(fun loc m ->
      Location.cover loc (Module_var.get_location m))


(**
    Add a variable definition to the provided list of definitions `t`,
    using the information provided in the given `vvar`.

    @param body The expression bound to the given variable.
    @return The provided list of definitions augmented with the given variable.
    *)
let defs_of_vvar ?(body : AST.expression option) ~(attributes : vdef_attributes option)
    : VVar.t -> def_type -> Uid.t list -> t -> t
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
      let attributes = Option.value attributes ~default:No_attributes in
      { name; uid; range; body_range; t; references; def_type; mod_path; attributes }
    in
    Variable vdef :: acc)


(**
    Add a variable definition to the provided list of definitions `t`,
    using the information provided in the given `binder`.

    It's a wrapper over {!add_vvar}, calling it with the binder's extracted vvar.

    @param body The expression bound to the given variable.
    @return The provided list of definitions augmented with the given binder.
    *)
let defs_of_binder ~(body : AST.expression) ~(attributes : vdef_attributes option)
    : 'a Binder.t -> def_type -> Uid.t list -> t -> t
  =
 fun binder def_type mod_path acc ->
  defs_of_vvar ~attributes ~body (Binder.get_var binder) def_type mod_path acc


(**
    Add a type variable definition to the provided list of definitions `t`,
    using the information provided in the given `tvar`.

    @param bindee The type expression bound to the given type variable.
    @return The provided list of definitions augmented with the given type variable.
    *)
let defs_of_tvar
    ?(bindee : Ast_core.type_expression option)
    ~(attributes : tdef_attributes option)
    : TVar.t -> def_type -> Uid.t list -> t -> t
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
      let attributes = Option.value ~default:No_attributes attributes in
      { name
      ; uid
      ; range
      ; body_range
      ; content
      ; def_type
      ; references
      ; mod_path
      ; attributes
      }
    in
    Type tdef :: acc)


(**
    Add a module variable definition to the provided list of definitions `t`,
    using the information provided in the given `mvar`.

    @param bindee The module expression bound to the given module variable.
    @return The provided list of definitions augmented with the given module variable.
    *)
let defs_of_mvar
    ?(bindee_location : Location.t option)
    ~(attributes : mdef_attributes option)
    ~(mod_case : mod_case)
    ~(implements : implementation list)
    : mdef_type -> string SMap.t -> MVar.t -> def_type -> Uid.t list -> t -> t
  =
 fun mdef_type module_deps mvar def_type mod_path acc ->
  if MVar.is_generated mvar
  then acc
  else (
    let name =
      let name = get_mod_binder_name mvar in
      Option.value_map
        ~default:(Original name)
        (SMap.find module_deps name)
        ~f:(fun name -> Filename name)
    in
    let mdef : mdef =
      let uid : Uid.t = Uid.make (get_mod_name_name name) (MVar.get_location mvar) in
      let range : Location.t = MVar.get_location mvar in
      let body_range : Location.t option = bindee_location in
      let references : LSet.t = LSet.empty (* Filled in a later pass *) in
      let signature = Unresolved (* Filled in a later pass *) in
      let attributes = Option.value ~default:No_attributes attributes in
      { name
      ; uid
      ; range
      ; body_range
      ; references
      ; mod_case
      ; def_type
      ; mod_path
      ; signature
      ; attributes
      ; implements
      ; mdef_type
      }
    in
    Module mdef :: acc)


(**
    This module contains the functions traversing the {!Ast_core}
    to fetch its definitions.

    During the traversal, some fields will be
    left blank or filled with a dummy value,
    they are meant to be filled in later passes.

*)
module Of_Ast = struct
  (**
    Options specifying which parts of the AST should not be traversed.

    By default, the whole AST should be traversed.

    The user, however, can provide a custom value with some fields set to [true]
    in order to perform a custom AST-traversal without traversing certain specific nodes.
    *)
  module Waivers = struct
    type t =
      { (* Useful for Stdlib AST traversal, when declaration rhs are unwanted *)
        d_value_expr : bool
      ; d_type_expr : bool
      ; d_irrefutable_match_expr : bool
      }

    let default : t =
      { d_value_expr = false; d_type_expr = false; d_irrefutable_match_expr = false }


    let of_opt : t option -> t = function
      | Some t -> t
      | None -> default


    (** Takes a function [f] and returns a wrapper function which :
          - Takes an optional [unless] boolean argument
            (defaults to [false])
          - Returns [f] if [unless] is [false]
          - Returns the identity function if [unless] is [true]

          It is meant for wrapping AST-traversal functions without re-implementing the [unless] logic each time.
          *)
    let wrap_with_unless (type x acc) (f : x -> acc -> acc) ?(unless = false) =
      if unless then fun _ acc -> acc else f
  end

  let defs_of_mvar_mod_expr
      ~(bindee : Ast_core.module_expr)
      ~(attributes : mdef_attributes option)
      :  mod_case:mod_case -> implements:implementation list -> string SMap.t -> MVar.t
      -> def_type -> Uid.t list -> t -> t
    =
    let bindee_location =
      match Location.unwrap bindee with
      | M_struct _ -> Location.get_location bindee
      | M_variable mvar -> MVar.get_location mvar
      | M_module_path mpath -> get_location_of_module_path @@ List.Ne.to_list mpath
    in
    defs_of_mvar Module ~bindee_location ~attributes


  let defs_of_mvar_sig_expr
      ~(bindee : Ast_core.signature_expr)
      ~(attributes : mdef_attributes option)
      :  mod_case:mod_case -> implements:implementation list -> string SMap.t -> MVar.t
      -> def_type -> Uid.t list -> t -> t
    =
    let bindee_location =
      match Location.unwrap bindee with
      | S_sig _ -> Location.get_location bindee
      | S_path mpath -> get_location_of_module_path @@ List.Ne.to_list mpath
    in
    defs_of_mvar Signature ~attributes ~bindee_location


  let defs_of_mvar_signature
      :  mod_case:mod_case -> string SMap.t -> MVar.t -> def_type -> Uid.t list
      -> def list -> def list
    =
    defs_of_mvar Signature ~attributes:None ?bindee_location:None ~implements:[]


  let defs_of_pattern ~(body : AST.expression) ~(attributes : vdef_attributes option)
      : AST.type_expression option Linear_pattern.t -> def_type -> Uid.t list -> t -> t
    =
   fun ptrn def_type mod_path acc ->
    let ptrn_binders = AST.Pattern.binders ptrn in
    let f defs binder = defs_of_binder ~attributes ~body binder def_type mod_path defs in
    let defs = List.fold ~init:acc ~f ptrn_binders in
    defs


  let add_inner_mod_path (module_binder : MVar.t) (mod_path : Uid.t list) : Uid.t list =
    mod_path
    @ [ Uid.make
          (Format.asprintf "%a" MVar.pp module_binder)
          (MVar.get_location module_binder)
      ]


  let rec defs_of_expr ~(waivers : Waivers.t)
      : string SMap.t -> Uid.t list -> AST.expression -> t -> t
    =
   fun module_deps mod_path e acc ->
    let self = Waivers.wrap_with_unless @@ defs_of_expr ~waivers module_deps mod_path in
    let defs_of_decls =
      Waivers.wrap_with_unless
      @@ fun (decls, def_type, mod_path) ->
      defs_of_decls ~waivers module_deps mod_path def_type decls
    in
    let defs_of_lambda : _ Lambda.t -> t -> t =
     fun { binder; output_type; result } acc ->
      let vvar = Param.get_var binder in
      self result
      @@ defs_of_vvar ~attributes:None ~body:result vvar Parameter mod_path acc
    in
    match e.expression_content with
    (* Base *)
    | E_variable v -> acc
    | E_contract x -> acc
    | E_literal l -> acc
    | E_constant c -> acc
    | E_application { lamb; args } -> self lamb @@ self args acc
    | E_lambda lambda -> defs_of_lambda lambda acc
    | E_recursive { fun_name; fun_type; lambda; force_lambdarec } ->
      (* fun_name is already added by the parent E_let_in so don't need to add it here *)
      defs_of_lambda lambda acc
    | E_type_abstraction { type_binder; result } -> self result acc
    | E_let_in { let_binder; rhs; let_result; attributes }
    | E_let_mut_in { let_binder; rhs; let_result; attributes } ->
      defs_of_pattern ~attributes:None ~body:rhs let_binder Local mod_path
      @@ self rhs
      @@ self let_result acc
    | E_type_in { type_binder; rhs; let_result } ->
      defs_of_tvar ~attributes:None ~bindee:rhs type_binder Local mod_path
      @@ self let_result acc
    | E_mod_in { module_binder; rhs; let_result } ->
      let inner_mod_path = add_inner_mod_path module_binder mod_path in
      let mod_case = mod_case_of_mod_expr ~defs_of_decls module_deps rhs inner_mod_path in
      defs_of_mvar_mod_expr
        ~mod_case
        ~attributes:None
        ~bindee:rhs
        ~implements:[]
        module_deps
        module_binder
        Local
        inner_mod_path
      @@ self let_result acc
    | E_raw_code { language = _; code = _ } -> acc
    (* Variant *)
    | E_constructor { constructor; element } -> self element acc
    | E_matching { matchee; cases } ->
      let defs_of_match_cases cases acc =
        let defs_of_match_case acc ({ pattern; body } : _ AST.Match_expr.match_case) =
          defs_of_pattern ~attributes:None ~body pattern Local mod_path @@ self body acc
        in
        List.fold ~init:acc ~f:defs_of_match_case cases
      in
      defs_of_match_cases cases @@ self matchee acc
    (* Record *)
    | E_record r -> Record.fold ~init:acc ~f:(fun acc entry -> self entry acc) r
    | E_accessor { struct_; path } ->
      self struct_ acc (* Is it possible to have decl in there ? *)
    | E_update { struct_; path; update } -> self struct_ @@ self update acc
    (* Advanced *)
    | E_ascription { anno_expr; type_annotation } -> self anno_expr acc
    | E_module_accessor macc -> acc
    (* Imperative *)
    | E_assign { binder; expression } ->
      (* binder := new_value, the binder is already declared so we don't add it to the dec list *)
      self expression acc
    | E_for { binder; start; final; incr; f_body } ->
      defs_of_vvar ~attributes:None ~body:f_body binder Local mod_path
      @@ self start
      @@ self final
      @@ self incr
      @@ self f_body
      @@ acc
    | E_for_each
        { fe_binder = vvar1, vvar2_opt; collection; collection_type = _; fe_body } ->
      let body = fe_body in
      let acc =
        match vvar2_opt with
        | Some vvar -> defs_of_vvar ~attributes:None ~body vvar Local mod_path acc
        | None -> acc
      in
      self fe_body
      @@ self collection
      @@ defs_of_vvar ~attributes:None ~body vvar1 Local mod_path acc
    | E_while { cond; body } -> self cond @@ self body acc


  and standalone_mvars : Module_var.t list -> implementation =
   fun mvars ->
    let module_path = First (List.map ~f:(Format.asprintf "%a" Module_var.pp) mvars) in
    (* The resolved path and name will be filled later. *)
    Standalone_signature_or_module { module_path; resolved_module = None }


  and alias_of_mvars : string SMap.t -> Module_var.t list -> alias =
   fun module_deps mvars ->
    let module_path = List.map ~f:mvar_to_id mvars in
    let file_name =
      match module_path with
      | [ mangled_name ] -> SMap.find module_deps (Uid.to_name mangled_name)
      | _ -> None
    in
    (* The resolved name will be filled later. *)
    { module_path; resolved_module = None; file_name }


  and mod_case_of_mvars : string SMap.t -> Module_var.t list -> mod_case =
   fun module_deps mvars -> Alias (alias_of_mvars module_deps mvars)


  and mod_case_of_mod_expr
      :  defs_of_decls:(AST.declaration list * def_type * Uid.t list -> t -> t)
      -> string SMap.t -> AST.module_expr -> Uid.t list -> mod_case
    =
   fun ~defs_of_decls module_deps mod_expr mod_path ->
    match Location.unwrap mod_expr with
    | M_struct decls -> Def (defs_of_decls (decls, Module_field, mod_path) [])
    | M_variable mod_var -> mod_case_of_mvars module_deps [ mod_var ]
    | M_module_path mod_path -> mod_case_of_mvars module_deps @@ List.Ne.to_list mod_path


  and mod_case_of_signature : string SMap.t -> AST.signature -> Uid.t list -> mod_case =
   fun module_deps sig' mod_path ->
    Def (defs_of_signature module_deps sig' Module_field mod_path [])


  (* If we have an [interface I { ... }] or [module type I = sig ... end], it will be
     confusingly translated into the following:

     {[
       module type I = sig
         include sig
           ...
         end
       end
     ]}

     On the other hand, an alias [module type A = ...] will be translated into this:

     {[
      module type A = sig
        include ...
      end
     ]}

     Rather than directly calling [defs_of_sig_expr], we directly handle this
     inconsistency here.

     Moreover, signatures in CameLIGO and interfaces in JsLIGO handle extension
     differently.

     In JsLIGO, we get [sig include I1 include I2 ... include In include M end] where each
     [Ik] ([k ≥ 0]) are the interfaces that [M] (which is [sig ... end]) is extended with.

     In CameLIGO, this is done by simply manually [include]'ing the signatures into [M].
     Unfortunately, this means that there is no notion of what is being extended and what
     is the intended part of the definition.

     To work around this, we assume that all [S_include]s of [S_path]s are being
     implemented, while everything else is "extra".

     This has one downside. Suppose the following:

     {[
       module type I = sig val x : unit end
       module M = struct let x = () end
     ]}

     Note that we do not indicate that [M : I]. What should happen if we try to find the
     definition of [M.x]? Even if [M] is compatible with [I], we won't assume it as a
     definition. As of this writing, LIGO does not support first-class modules, but in
     case it ever will, we still won't consider [I.x] and [M.x] as references. *)
  and implementations_of_sig_expr_from_D_signature
      :  string SMap.t -> AST.signature_expr -> Uid.t list
      -> [ `Alias of alias | `Implementations of implementation list ]
    =
   fun module_deps sig_expr mod_path ->
    match Location.unwrap sig_expr with
    | S_sig { items = [ S_include { wrap_content = S_path mod_path; location = _ } ] } ->
      `Alias (alias_of_mvars module_deps @@ List.Ne.to_list mod_path)
    | S_sig sig' ->
      let Ast_core.{ items } = Misc.flatten_includes sig' in
      `Implementations
        (List.map items ~f:(function
            | S_include { wrap_content = S_path mod_path; location = _ } ->
              standalone_mvars @@ List.Ne.to_list mod_path
            | (S_value _ | S_type _ | S_type_var _ | S_module _ | S_module_type _) as item
              ->
              Ad_hoc_signature
                (defs_of_sig_item module_deps item Module_field mod_path [])
            | S_include { wrap_content = S_sig _; location = _ } ->
              failwith
              @@ Format.asprintf
                   "implementations_of_sig_expr_from_D_signature: corner case reached: %a"
                   AST.PP.signature_expr
                   sig_expr))
    | S_path mod_path -> `Alias (alias_of_mvars module_deps @@ List.Ne.to_list mod_path)


  and implementations_of_sig_expr_from_annotation
      : string SMap.t -> AST.signature_expr -> Uid.t list -> implementation list
    =
   fun module_deps sig_expr mod_path ->
    match Location.unwrap sig_expr with
    | S_sig { items } ->
      List.map items ~f:(function
          | S_include { wrap_content = S_sig sig'; location = _ } ->
            Ad_hoc_signature (defs_of_signature module_deps sig' Module_field mod_path [])
          | S_include { wrap_content = S_path mod_path; location = _ } ->
            standalone_mvars @@ List.Ne.to_list mod_path
          | S_value _ | S_type _ | S_type_var _ | S_module _ | S_module_type _ ->
            failwith
            @@ Format.asprintf
                 "implementations_of_sig_expr_from_annotation: corner case reached: %a"
                 AST.PP.signature_expr
                 sig_expr)
    | S_path mod_path -> [ standalone_mvars @@ List.Ne.to_list mod_path ]


  (* When we get a signature like [namespace M extends I1, { ... }, I3 { ... }], it will
     be translated into the following:

     {[
       module M : sig
         include I1
         include { ... }
         include I3
       end = struct
         ...
       end
     ]}

     [implementation_of_sig_expr] will handle this and interpret each [include] as the
     [implements] field of a [mdef]. *)
  and implementations_of_module_annotation
      : string SMap.t -> AST.module_annotation -> Uid.t list -> implementation list
    =
   fun module_deps { signature; filter = _ } ->
    implementations_of_sig_expr_from_annotation module_deps signature


  and defs_of_sig_expr
      :  string SMap.t -> AST.signature_expr -> def_type -> Uid.t list -> def list
      -> def list
    =
   fun module_deps sig_expr def_type mod_path acc ->
    match Location.unwrap sig_expr with
    | S_sig sig' -> defs_of_signature module_deps sig' def_type mod_path acc
    | S_path _mod_path -> acc


  and defs_of_sig_item
      : string SMap.t -> AST.sig_item -> def_type -> Uid.t list -> def list -> def list
    =
   fun module_deps item def_type mod_path acc ->
    match item with
    | S_value (var, ty_expr, attr) ->
      defs_of_vvar ~attributes:(Some (Sig_item attr)) var def_type mod_path acc
    | S_type (var, ty_expr, attr) ->
      defs_of_tvar
        ~attributes:(Some (Sig_type attr))
        ~bindee:ty_expr
        var
        def_type
        mod_path
        acc
    | S_type_var (var, attr) ->
      defs_of_tvar ~attributes:(Some (Sig_type attr)) var def_type mod_path acc
    | S_module (var, sig') | S_module_type (var, sig') ->
      let inner_mod_path = add_inner_mod_path var mod_path in
      let mod_case = mod_case_of_signature module_deps sig' inner_mod_path in
      defs_of_mvar_signature ~mod_case module_deps var def_type mod_path acc
    | S_include sig_expr -> defs_of_sig_expr module_deps sig_expr def_type mod_path acc


  and defs_of_signature
      : string SMap.t -> AST.signature -> def_type -> Uid.t list -> def list -> def list
    =
   fun module_deps { items } def_type mod_path acc ->
    List.fold items ~init:acc ~f:(fun acc sig_item ->
        defs_of_sig_item module_deps sig_item def_type mod_path acc)


  and defs_of_decl ~(waivers : Waivers.t)
      : string SMap.t -> def_type -> Uid.t list -> AST.declaration -> t -> t
    =
   fun module_deps def_type mod_path decl acc ->
    let defs_of_expr =
      Waivers.wrap_with_unless @@ defs_of_expr ~waivers module_deps mod_path
    in
    let defs_of_decls =
      Waivers.wrap_with_unless
      @@ fun (decls, def_type, mod_path) ->
      defs_of_decls ~waivers module_deps mod_path def_type decls
    in
    match Location.unwrap decl with
    | D_value { binder; expr; attr } ->
      defs_of_binder
        ~attributes:(Some (Value_attr attr))
        ~body:expr
        binder
        def_type
        mod_path
      @@ defs_of_expr ~unless:waivers.d_value_expr expr acc
    | D_irrefutable_match { pattern; expr; attr } ->
      defs_of_pattern
        ~attributes:(Some (Value_attr attr))
        ~body:expr
        pattern
        def_type
        mod_path
      @@ defs_of_expr ~unless:waivers.d_irrefutable_match_expr expr acc
    | D_type { type_binder; type_expr; type_attr } ->
      defs_of_tvar
        ~attributes:(Some (Type_attr type_attr))
        ~bindee:type_expr
        type_binder
        def_type
        mod_path
        acc
    | D_module { module_binder; module_; module_attr; annotation } ->
      let sig_defs_and_mod_cases =
        Option.map annotation ~f:(fun annotation ->
            let sig_mod_cases =
              implementations_of_module_annotation module_deps annotation mod_path
            in
            ( List.concat_map sig_mod_cases ~f:(function
                  | Ad_hoc_signature defs -> defs
                  | Standalone_signature_or_module _ -> [])
            , sig_mod_cases ))
      in
      let inner_mod_path = add_inner_mod_path module_binder mod_path in
      (* Here, the module body's defs are within the lhs_def, mod_case_of_mod_expr
       recursively calls defs_of_decl *)
      let mod_case =
        mod_case_of_mod_expr ~defs_of_decls module_deps module_ inner_mod_path
      in
      defs_of_mvar_mod_expr
        ~mod_case
        ~attributes:(Some (Module_attr module_attr))
        ~bindee:module_
        ~implements:(List.concat_map ~f:snd @@ Option.to_list sig_defs_and_mod_cases)
        module_deps
        module_binder
        def_type
        mod_path
        (Option.value_map
           ~default:acc
           ~f:(Fn.flip ( @ ) acc <@ fst)
           sig_defs_and_mod_cases)
    | D_module_include module_ ->
      (match mod_case_of_mod_expr ~defs_of_decls module_deps module_ mod_path with
      | Alias _ -> acc
      | Def x -> x @ acc)
    | D_signature { signature_binder; signature; signature_attr } ->
      let inner_mod_path = add_inner_mod_path signature_binder mod_path in
      let impls =
        implementations_of_sig_expr_from_D_signature module_deps signature inner_mod_path
      in
      let implements, mod_case =
        match impls with
        | `Alias alias -> [], Alias alias
        | `Implementations impls ->
          List.fold_left impls ~init:([], Def []) ~f:(fun (implements, impl) -> function
            | Ad_hoc_signature defs ->
              ( implements
              , (match impl with
                | Def defs' -> Def (defs' @ defs)
                | Alias _ -> impl) )
            | Standalone_signature_or_module _ as inclusion ->
              inclusion :: implements, impl)
      in
      let defs =
        match mod_case with
        | Def defs -> defs
        | Alias _ -> []
      in
      defs_of_mvar_sig_expr
        ~mod_case
        ~attributes:(Some (Signature_attr signature_attr))
        ~bindee:signature
        ~implements
        module_deps
        signature_binder
        def_type
        mod_path
        (defs @ acc)


  and defs_of_decls ~(waivers : Waivers.t)
      : string SMap.t -> Uid.t list -> def_type -> AST.declaration list -> t -> t
    =
   fun module_deps mod_path def_type decls acc ->
    List.fold decls ~init:acc ~f:(fun accu decl ->
        defs_of_decl ~waivers module_deps def_type mod_path decl accu)


  let definitions ?(waivers = Waivers.default) : AST.program -> string SMap.t -> t -> t =
   fun prg module_deps acc -> defs_of_decls ~waivers module_deps [] Global prg acc
end

module Of_Stdlib_Ast = struct
  let definitions : AST.program -> string SMap.t -> t =
   fun prg module_deps ->
    let waivers =
      { Of_Ast.Waivers.default with d_value_expr = true; d_irrefutable_match_expr = true }
    in
    Of_Ast.definitions ~waivers prg module_deps []
end
