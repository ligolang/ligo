open Ligo_prim
open Types
module AST = Ast_core
module VVar = Value_var
module TVar = Type_var
module MVar = Module_var
module LSet = Types.LSet
module SMap = Map.Make (String)
module Mangled_pass = Inline_mangled_modules_pass

let ( <@ ) = Simple_utils.Function.( <@ )

type t = def list

let mangled_uids_hashtbl = Hashtbl.create (module Uid)

(**
    Add a variable definition to the provided list of definitions `t`,
    using the information provided in the given `vvar`.

    @param decl The declaration bound to the given variable.
    @return The provided list of definitions augmented with the given variable.
    *)
let defs_of_vvar ~(decl_range : Location.t) ~(attributes : vdef_attributes option)
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
      let t : type_case = Unresolved (* Filled in a later pass *) in
      let references : LSet.t = LSet.empty (* Filled in a later pass *) in
      let attributes = Option.value attributes ~default:No_attributes in
      { name; uid; range; decl_range; t; references; def_type; mod_path; attributes }
    in
    Variable vdef :: acc)


let rec defs_of_ty_expr ?(orig_type_loc : Location.t option)
    : AST.type_expression option -> def_type -> Uid.t list -> t -> t
  =
 fun ty_expr def_type mod_path acc ->
  match ty_expr with
  | None -> acc
  | Some ty_expr ->
    let self ty_expr = defs_of_ty_expr (Some ty_expr) def_type mod_path in
    let orig_type_loc = Option.value ~default:ty_expr.location orig_type_loc in
    (match ty_expr.type_content with
    (* Actual traversals *)
    | T_sum (row, Some (Label (orig_label, _))) ->
      (* Disc union type case *)
      let labels = Record.labels row.fields in
      let sum_string_type =
        let record =
          Record.of_list
          @@ List.map
               ~f:(fun (Label (label, _)) ->
                 (* Let's remove a location since we don't want to create generated ctors
                    in disc union types. *)
                 Label.of_string label, AST.t_unit ~loc:Location.generated ())
               labels
        in
        let row = AST.Row.create ~layout:None record in
        AST.t_sum row None ~loc:Location.generated ()
      in
      let common_field_row =
        (* Locations in generated labels correspond to each common field. *)
        List.map labels ~f:(fun (Label (_, loc)) ->
            Label.T.create ~loc orig_label, sum_string_type)
      in
      let acc =
        defs_of_row ~label_case:Field orig_type_loc common_field_row def_type mod_path acc
      in
      let inner_types = Record.values row.fields in
      List.fold_right ~init:acc ~f:self inner_types
    | T_sum (row, None) ->
      defs_of_row
        ~label_case:Ctor
        orig_type_loc
        (Label.Map.to_alist row.fields)
        def_type
        mod_path
        acc
    | T_record row ->
      defs_of_row
        ~label_case:Field
        orig_type_loc
        (Label.Map.to_alist row.fields)
        def_type
        mod_path
        acc
    (* Structure traversals *)
    | T_arrow { type1; type2; param_names = _ } -> self type2 @@ self type1 acc
    | T_app { arguments; type_operator = _ } ->
      List.fold_right ~init:acc ~f:self arguments
    | T_abstraction { type_; ty_binder = _; kind = _ }
    | T_for_all { type_; ty_binder = _; kind = _ } -> self type_ acc
    | T_variable _
    | T_constant _
    | T_contract_parameter _
    | T_module_accessor _
    | T_singleton _ -> acc)


and defs_of_row ~(label_case : label_case) (orig_type_loc : Location.t)
    : (Label.t * AST.type_expression) list -> def_type -> Uid.t list -> t -> t
  =
 fun row def_type mod_path acc ->
  List.fold_right
    ~init:acc
    ~f:(fun (Label (label, loc), ty_expr) acc ->
      if Location.is_dummy_or_generated loc
      then acc
      else (
        let acc = defs_of_ty_expr (Some ty_expr) def_type mod_path acc in
        let ldef : ldef =
          let name = label in
          let range = loc in
          let uid = Uid.make name range in
          let decl_range = Location.cover loc ty_expr.location in
          let references = LSet.empty (* Filled in a later pass *) in
          let content = ty_expr in
          { name
          ; uid
          ; range
          ; decl_range
          ; references
          ; content
          ; def_type
          ; orig_type_loc
          ; label_case
          ; mod_path
          }
        in
        Label ldef :: acc))
    row


(**
    Add a variable definition to the provided list of definitions `t`,
    using the information provided in the given `binder`.

    It's a wrapper over {!add_vvar}, calling it with the binder's extracted vvar.

    @param decl The expression bound to the given variable.
    @return The provided list of definitions augmented with the given binder.
    *)
let defs_of_binder ~(decl_range : Location.t) ~(attributes : vdef_attributes option)
    : AST.type_expression option Binder.t -> def_type -> Uid.t list -> t -> t
  =
 fun binder def_type mod_path acc ->
  defs_of_vvar ~attributes ~decl_range (Binder.get_var binder) def_type mod_path
  @@
  if Location.is_dummy_or_generated (Binder.get_loc binder)
  then acc
  else defs_of_ty_expr (Binder.get_ascr binder) Local mod_path acc


(**
    Add a type variable definition to the provided list of definitions `t`,
    using the information provided in the given `tvar`.

    @param bindee The type expression bound to the given type variable.
    @return The provided list of definitions augmented with the given type variable.
    *)
let defs_of_tvar
    ~(decl_range : Location.t)
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
      let content : Ast_core.type_expression option = bindee in
      let references : LSet.t = LSet.empty (* Filled in a later pass *) in
      let attributes = Option.value ~default:No_attributes attributes in
      { name
      ; uid
      ; range
      ; decl_range
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
    ~(decl_range : Location.t)
    ~(attributes : mdef_attributes option)
    ~(mod_case : mod_case)
    ~(implements : implementation list)
    ~(extends : extension list)
    : mdef_type -> string SMap.t -> MVar.t -> def_type -> Uid.t list -> t -> t
  =
 fun mdef_type module_deps mvar def_type mod_path acc ->
  if MVar.is_generated mvar
  then acc
  else (
    let name, file_name_opt =
      let name = get_mod_binder_name mvar in
      name, SMap.find module_deps name
    in
    let mdef : mdef =
      let uid : Uid.t = Uid.make name (MVar.get_location mvar) in
      Option.iter file_name_opt ~f:(fun file_name ->
          Hashtbl.update mangled_uids_hashtbl uid ~f:(Fn.const file_name));
      let range : Location.t = MVar.get_location mvar in
      let references : LSet.t = LSet.empty (* Filled in a later pass *) in
      let signature = Unresolved (* Filled in a later pass *) in
      let attributes = Option.value ~default:No_attributes attributes in
      let inlined_name = None (* Filled in a later pass *) in
      { name
      ; uid
      ; range
      ; decl_range
      ; references
      ; mod_case
      ; def_type
      ; mod_path
      ; signature
      ; attributes
      ; implements
      ; extends
      ; mdef_type
      ; inlined_name
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
      ~(decl_range : Location.t)
      ~(attributes : mdef_attributes option)
      :  mod_case:mod_case -> implements:implementation list -> extends:extension list
      -> string SMap.t -> MVar.t -> def_type -> Uid.t list -> t -> t
    =
    defs_of_mvar Module ~decl_range ~attributes


  let defs_of_mvar_sig_expr
      ~(decl_range : Location.t)
      ~(attributes : mdef_attributes option)
      :  mod_case:mod_case -> implements:implementation list -> string SMap.t -> MVar.t
      -> def_type -> Uid.t list -> t -> t
    =
    defs_of_mvar Signature ~attributes ~decl_range ~extends:[]


  let defs_of_mvar_signature
      :  decl_range:Location.t -> mod_case:mod_case -> string SMap.t -> MVar.t -> def_type
      -> Uid.t list -> def list -> def list
    =
    defs_of_mvar Signature ~attributes:None ~implements:[] ~extends:[]


  let defs_of_pattern ~(decl_range : Location.t) ~(attributes : vdef_attributes option)
      : AST.type_expression option Linear_pattern.t -> def_type -> Uid.t list -> t -> t
    =
   fun ptrn def_type mod_path acc ->
    let ptrn_binders = AST.Pattern.binders ptrn in
    let f defs binder =
      defs_of_binder ~attributes ~decl_range binder def_type mod_path defs
    in
    let defs = List.fold ~init:acc ~f ptrn_binders in
    defs


  let add_inner_mod_path (module_binder : MVar.t) (mod_path : Uid.t list) : Uid.t list =
    mod_path
    @ [ Uid.make
          (Format.asprintf "%a" MVar.pp module_binder)
          (MVar.get_location module_binder)
      ]


  let rec defs_of_expr ~(waivers : Waivers.t) ~(decl_range : Location.t)
      : string SMap.t -> Uid.t list -> AST.expression -> t -> t
    =
   fun module_deps mod_path e acc ->
    let self =
      Waivers.wrap_with_unless @@ defs_of_expr ~waivers ~decl_range module_deps mod_path
    in
    let defs_of_decls =
      Waivers.wrap_with_unless
      @@ fun (decls, def_type, mod_path) ->
      defs_of_decls ~waivers module_deps mod_path def_type decls
    in
    let defs_of_lambda ~decl_range : (_, AST.type_expression option) Lambda.t -> t -> t =
     fun { binder; output_type; result } acc ->
      let vvar = Param.get_var binder in
      let ty_expr = Param.get_ascr binder in
      self result
      @@ defs_of_vvar ~attributes:None ~decl_range vvar Parameter mod_path
      @@ defs_of_ty_expr ty_expr Parameter mod_path acc
    in
    let uncover_let_result (let_result : AST.expression) : Location.t =
      let uncover (l1 : Location.t) (l2 : Location.t) : Location.t =
        match l1, l2 with
        | File r1, File r2
          when not Location.(is_dummy_or_generated l1 || is_dummy_or_generated l2) ->
          File (Simple_utils.Region.make ~start:r1#start ~stop:r2#start)
        | _, _ -> l1
      in
      (* FIXME: LIGO regions are not always accurate, and sometimes when calling
         [Simple_utils.Region.make], it might throw an exception since the smaller
         region's range might be bigger than the big region's range. For now, we'll try
         and catch these exceptions, but we should someday write a test that checks that
         every region is well-formed and fix everything that we can find.
           These regions are currently used just for document symbols (see `decl_body`),
         so the effect of this [try] block will be some symbols with bigger regions than
         they really have but it's better than having the LSP crash due to some oversights
         made during the development of LIGO.
           As of the writing of this comment, here are some known failing contracts:
         * src/test/contracts/lsp/hovers.mligo
         * src/test/contracts/lsp/entrypoints_repeated.mligo
         * src/test/contracts/lsp/entrypoints_different_storage.mligo
         * src/test/contracts/lsp/entrypoints_views.mligo
         * src/test/contracts/lsp/entrypoints_modules.mligo
         * tools/debugger/ligo-debugger/test/contracts/partially-applied-cases.jsligo *)
      try uncover e.location let_result.location with
      | Simple_utils.Region.Out_of_order_pos _ -> e.location
    in
    match e.expression_content with
    (* Base *)
    | E_variable v -> acc
    | E_contract x -> acc
    | E_literal l -> acc
    | E_constant c -> acc
    | E_application { lamb; args } -> self lamb @@ self args acc
    | E_lambda lambda ->
      let decl_range =
        let var_loc = VVar.get_location @@ Param.get_var lambda.binder in
        Location.cover
          var_loc
          (Option.value_map ~default:var_loc ~f:(fun (te : AST.type_expression) ->
               te.location)
          @@ Param.get_ascr lambda.binder)
      in
      defs_of_lambda ~decl_range lambda acc
    | E_recursive { fun_name; fun_type; lambda; force_lambdarec } ->
      let decl_range =
        let var_loc = VVar.get_location @@ Param.get_var lambda.binder in
        Location.cover var_loc (Param.get_ascr lambda.binder).location
      in
      (* fun_name is already added by the parent E_let_in so don't need to add it here *)
      defs_of_lambda ~decl_range (Lambda.map Fn.id Option.some lambda)
      @@ defs_of_ty_expr (Some fun_type) Local mod_path acc
    | E_type_abstraction { type_binder; result } ->
      let decl_range = TVar.get_location type_binder in
      defs_of_tvar ~decl_range ~attributes:None type_binder Parameter mod_path
      @@ self result acc
    | E_let_in { let_binder; rhs; let_result; attributes }
    | E_let_mut_in { let_binder; rhs; let_result; attributes } ->
      let decl_range = uncover_let_result let_result in
      defs_of_pattern ~attributes:None ~decl_range let_binder Local mod_path
      @@ self rhs
      @@ self let_result acc
    | E_type_in { type_binder; rhs; let_result } ->
      let decl_range = uncover_let_result let_result in
      defs_of_tvar ~decl_range ~attributes:None ~bindee:rhs type_binder Local mod_path
      @@ defs_of_ty_expr
           ~orig_type_loc:(TVar.get_location type_binder)
           (Some rhs)
           Local
           mod_path
      @@ self let_result acc
    | E_mod_in { module_binder; rhs; let_result } ->
      let decl_range = uncover_let_result let_result in
      let inner_mod_path = add_inner_mod_path module_binder mod_path in
      let mod_case = mod_case_of_mod_expr ~defs_of_decls module_deps rhs inner_mod_path in
      let extends = extends rhs in
      defs_of_mvar_mod_expr
        ~mod_case
        ~attributes:None
        ~decl_range
        ~implements:[]
        ~extends
        module_deps
        module_binder
        Local
        inner_mod_path
      @@ self let_result acc
    | E_raw_code { language = _; code = _ } -> acc
    (* Variant *)
    | E_constructor { constructor; element } -> self element acc
    | E_matching { matchee; disc_label; cases } ->
      let unwrap_ascription (expr : AST.expression) =
        match expr.expression_content with
        | E_ascription { anno_expr; _ } -> anno_expr
        | _ -> expr
      in
      let matchee =
        (* We don't want to create the same type definition in ascription twice *)
        if Option.is_some disc_label then unwrap_ascription matchee else matchee
      in
      let defs_of_match_cases cases acc =
        let defs_of_match_case acc ({ pattern; body } : _ AST.Match_expr.match_case) =
          let decl_range = pattern.location in
          defs_of_pattern ~attributes:None ~decl_range pattern Local mod_path
          @@ self body acc
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
    | E_ascription { anno_expr; type_annotation } ->
      self anno_expr @@ defs_of_ty_expr (Some type_annotation) Local mod_path acc
    | E_module_accessor macc -> acc
    (* Imperative *)
    | E_assign { binder; expression } ->
      (* binder := new_value, the binder is already declared so we don't add it to the dec list *)
      self expression acc
    | E_for { binder; start; final; incr; f_body } ->
      let decl_range = VVar.get_location binder in
      defs_of_vvar ~attributes:None ~decl_range binder Local mod_path
      @@ self start
      @@ self final
      @@ self incr
      @@ self f_body
      @@ acc
    | E_for_each
        { fe_binder = vvar1, vvar2_opt; collection; collection_type = _; fe_body } ->
      let acc =
        match vvar2_opt with
        | Some vvar2 ->
          let decl_range = VVar.get_location vvar2 in
          defs_of_vvar ~attributes:None ~decl_range vvar2 Local mod_path acc
        | None -> acc
      in
      let decl_range = VVar.get_location vvar1 in
      self fe_body
      @@ self collection
      @@ defs_of_vvar ~attributes:None ~decl_range vvar1 Local mod_path acc
    | E_while { cond; body } -> self cond @@ self body acc


  and unresolved_path : Module_var.t list -> resolve_mod_name =
   fun mvars ->
    let module_path = List.map ~f:mvar_to_id mvars in
    (* The resolved path and name will be filled later. *)
    Unresolved_path { module_path }


  and standalone_mvars : Module_var.t list -> implementation =
   fun mvars -> Standalone_signature_or_module (unresolved_path mvars)


  and alias_of_mvars : Module_var.t list -> alias =
   fun mvars ->
    let module_path = List.map ~f:mvar_to_id mvars in
    (* The resolved name will be filled later. *)
    { resolve_mod_name = Unresolved_path { module_path } }


  and mod_case_of_mvars : Module_var.t list -> mod_case =
   fun mvars -> Alias (alias_of_mvars mvars)


  and extends_of_declaration : AST.declaration -> extension list -> extension list =
   fun decl acc ->
    match Location.unwrap decl with
    | D_module_include mod_expr -> extends_of_mod_expr mod_expr acc
    | D_value _
    | D_irrefutable_match _
    | D_type _
    | D_module _
    | D_signature _
    | D_import _ -> acc


  and extends_of_declarations : AST.declaration list -> extension list -> extension list =
   fun decls acc -> List.fold_right decls ~init:acc ~f:extends_of_declaration


  and extends_of_mod_expr : AST.module_expr -> extension list -> extension list =
   fun mod_expr acc ->
    match Location.unwrap mod_expr with
    | M_struct decls -> extends_of_declarations decls acc
    | M_variable mod_var -> unresolved_path [ mod_var ] :: acc
    | M_module_path mod_path -> unresolved_path (List.Ne.to_list mod_path) :: acc


  and extends : AST.module_expr -> extension list =
   fun mod_expr ->
    match Location.unwrap mod_expr with
    | M_struct decls -> extends_of_declarations decls []
    | M_variable _mod_var -> []
    | M_module_path _mod_path -> []


  and mod_case_of_mod_expr
      :  defs_of_decls:(AST.declaration list * def_type * Uid.t list -> t -> t)
      -> string SMap.t -> AST.module_expr -> Uid.t list -> mod_case
    =
   fun ~defs_of_decls module_deps mod_expr mod_path ->
    match Location.unwrap mod_expr with
    | M_struct decls -> Def (defs_of_decls (decls, Module_field, mod_path) [])
    | M_variable mod_var -> mod_case_of_mvars [ mod_var ]
    | M_module_path mod_path -> mod_case_of_mvars @@ List.Ne.to_list mod_path


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
    | S_sig
        { items =
            [ { wrap_content = S_include { wrap_content = S_path mod_path; location = _ }
              ; location = _
              }
            ]
        } -> `Alias (alias_of_mvars @@ List.Ne.to_list mod_path)
    | S_sig sig' ->
      let Ast_core.{ items } = Misc.flatten_includes sig' in
      `Implementations
        (List.map items ~f:(fun item ->
             match Location.unwrap item with
             | S_include { wrap_content = S_path mod_path; location = _ } ->
               standalone_mvars @@ List.Ne.to_list mod_path
             | S_value _ | S_type _ | S_type_var _ | S_module _ | S_module_type _ ->
               Ad_hoc_signature
                 (defs_of_sig_item module_deps item Module_field mod_path [])
             | S_include { wrap_content = S_sig _; location = _ } ->
               failwith
               @@ Format.asprintf
                    "implementations_of_sig_expr_from_D_signature: corner case reached: \
                     %a"
                    AST.PP.signature_expr
                    sig_expr))
    | S_path mod_path -> `Alias (alias_of_mvars @@ List.Ne.to_list mod_path)


  and implementations_of_sig_expr_from_annotation
      : string SMap.t -> AST.signature_expr -> Uid.t list -> implementation list
    =
   fun module_deps sig_expr mod_path ->
    match Location.unwrap sig_expr with
    | S_sig { items } ->
      List.map items ~f:(fun item ->
          match Location.unwrap item with
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
    let decl_range = Location.get_location item in
    match Location.unwrap item with
    | S_value (var, ty_expr, attr) ->
      defs_of_vvar ~decl_range ~attributes:(Some (Sig_item attr)) var def_type mod_path
      @@ defs_of_ty_expr (Some ty_expr) def_type mod_path acc
    | S_type (var, ty_expr, attr) ->
      defs_of_tvar
        ~decl_range
        ~attributes:(Some (Sig_type attr))
        ~bindee:ty_expr
        var
        def_type
        mod_path
      @@ defs_of_ty_expr
           ~orig_type_loc:(TVar.get_location var)
           (Some ty_expr)
           def_type
           mod_path
           acc
    | S_type_var (var, attr) ->
      defs_of_tvar
        ~decl_range
        ~attributes:(Some (Sig_type attr))
        var
        def_type
        mod_path
        acc
    | S_module (var, sig') | S_module_type (var, sig') ->
      let inner_mod_path = add_inner_mod_path var mod_path in
      let mod_case = mod_case_of_signature module_deps sig' inner_mod_path in
      defs_of_mvar_signature ~decl_range ~mod_case module_deps var def_type mod_path acc
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
    let decl_range = Location.get_location decl in
    let defs_of_expr =
      Waivers.wrap_with_unless @@ defs_of_expr ~waivers ~decl_range module_deps mod_path
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
        ~decl_range
        binder
        def_type
        mod_path
      @@ defs_of_expr ~unless:waivers.d_value_expr expr acc
    | D_irrefutable_match { pattern; expr; attr } ->
      defs_of_pattern
        ~attributes:(Some (Value_attr attr))
        ~decl_range
        pattern
        def_type
        mod_path
      @@ defs_of_expr ~unless:waivers.d_irrefutable_match_expr expr acc
    | D_type { type_binder; type_expr; type_attr } ->
      defs_of_tvar
        ~decl_range
        ~attributes:(Some (Type_attr type_attr))
        ~bindee:type_expr
        type_binder
        def_type
        mod_path
      @@ defs_of_ty_expr
           ~orig_type_loc:(TVar.get_location type_binder)
           (Some type_expr)
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
      (* Here, the module decl's defs are within the lhs_def, mod_case_of_mod_expr
         recursively calls defs_of_decl *)
      let mod_case =
        mod_case_of_mod_expr ~defs_of_decls module_deps module_ inner_mod_path
      in
      let extends = extends module_ in
      defs_of_mvar_mod_expr
        ~mod_case
        ~attributes:(Some (Module_attr module_attr))
        ~decl_range
        ~implements:(List.concat_map ~f:snd @@ Option.to_list sig_defs_and_mod_cases)
        ~extends
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
          let implements, defs =
            List.fold_left impls ~init:([], []) ~f:(fun (implements, defs) -> function
              | Ad_hoc_signature defs' -> implements, defs' @ defs
              | Standalone_signature_or_module _ as inclusion ->
                inclusion :: implements, defs)
          in
          implements, Def defs
      in
      let defs =
        match mod_case with
        | Def defs -> defs
        | Alias _ -> []
      in
      defs_of_mvar_sig_expr
        ~mod_case
        ~attributes:(Some (Signature_attr signature_attr))
        ~decl_range
        ~implements
        module_deps
        signature_binder
        def_type
        mod_path
        (defs @ acc)
    | D_import { import_name; imported_module; import_attr } ->
      let inner_mod_path = add_inner_mod_path import_name mod_path in
      let module_expr =
        Location.wrap ~loc:Location.generated (Module_expr.M_variable imported_module)
      in
      let mod_case =
        mod_case_of_mod_expr ~defs_of_decls module_deps module_expr inner_mod_path
      in
      defs_of_mvar_mod_expr
        ~mod_case
        ~attributes:(Some (Module_attr import_attr))
        ~decl_range
        ~implements:[]
        ~extends:[]
        module_deps
        imported_module
        def_type
        mod_path
        acc


  and defs_of_decls ~(waivers : Waivers.t)
      : string SMap.t -> Uid.t list -> def_type -> AST.declaration list -> t -> t
    =
   fun module_deps mod_path def_type decls acc ->
    List.fold decls ~init:acc ~f:(fun accu decl ->
        defs_of_decl ~waivers module_deps def_type mod_path decl accu)


  let definitions ?(waivers = Waivers.default)
      : AST.program -> string SMap.t -> t -> Mangled_pass.t * t
    =
   fun prg module_deps acc ->
    Hashtbl.clear mangled_uids_hashtbl;
    let defs = defs_of_decls ~waivers module_deps [] Global prg acc in
    let mangled_uids_map =
      mangled_uids_hashtbl
      |> Hashtbl.to_alist
      |> Caml.List.to_seq
      |> Mangled_pass.UidMap.of_seq
    in
    mangled_uids_map, defs
end

module Of_Stdlib_Ast = struct
  let definitions : AST.program -> string SMap.t -> t =
   fun prg module_deps ->
    let waivers =
      { Of_Ast.Waivers.default with d_value_expr = true; d_irrefutable_match_expr = true }
    in
    snd @@ Of_Ast.definitions ~waivers prg module_deps []
end
