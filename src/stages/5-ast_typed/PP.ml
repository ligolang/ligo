[@@@coverage exclude_file]

module Location = Simple_utils.Location
module Var = Simple_utils.Var
module List = Simple_utils.List
module Ligo_string = Simple_utils.Ligo_string
module Int64 = Caml.Int64
open Ligo_prim
open Format
open Types
open Simple_utils.PP_helpers

let rec type_content : formatter -> type_content -> unit =
 fun ppf tc ->
  match tc with
  | T_variable tv -> Type_var.pp ppf tv
  | T_sum row -> Row.PP.sum_type type_expression layout ppf row
  | T_record row -> Row.PP.tuple_or_record_type type_expression layout ppf row
  | T_arrow a -> Arrow.pp type_expression ppf a
  | T_constant tc -> type_injection ppf tc
  | T_singleton x -> Literal_value.pp ppf x
  | T_abstraction x -> Abstraction.pp_type_abs type_expression ppf x
  | T_for_all x -> Abstraction.pp_forall type_expression ppf x


(* and row : formatter -> row_element -> unit =
 fun ppf t ->
  fprintf ppf "%a" type_expression t *)

and type_injection ppf { language; injection; parameters } =
  (* fprintf ppf "[%s {| %s %a |}]" language (Ligo_string.extract injection) (list_sep_d_par type_expression) parameters *)
  ignore language;
  fprintf
    ppf
    "%s%a"
    (Literal_types.to_string injection)
    (list_sep_d_par type_expression)
    parameters


and bool ppf : unit = fprintf ppf "bool"

(* ... don't print layouts because they are ugly and usually useless *)
and layout _ _ = ()

and option ppf (te : type_expression) : unit =
  let t = Combinators.get_t_option te in
  match t with
  | Some t -> fprintf ppf "option (%a)" type_expression t
  | None -> fprintf ppf "option ('a)"


and type_expression ppf (te : type_expression) : unit =
  (* TODO: we should have a way to hook custom pretty-printers for some types and/or track the "origin" of types as they flow through the constraint solver. This is a temporary quick fix *)
  if Option.is_some (Combinators.get_t_bool te)
  then bool ppf
  else if Option.is_some (Combinators.get_t_option te)
  then option ppf te
  else fprintf ppf "%a" type_content te.type_content


let type_expression_annot ppf (te : type_expression) : unit =
  fprintf ppf " : %a" type_expression te


let rec type_content_orig : formatter -> type_content -> unit =
 fun ppf tc ->
  match tc with
  | T_variable tv -> Type_var.pp ppf tv
  | T_sum row -> Row.PP.sum_type type_expression layout ppf row
  | T_record row -> Row.PP.tuple_or_record_type type_expression layout ppf row
  | T_arrow a -> Arrow.pp type_expression ppf a
  | T_constant tc -> type_injection ppf tc
  | T_singleton x -> Literal_value.pp ppf x
  | T_abstraction x -> Abstraction.pp_type_abs type_expression ppf x
  | T_for_all x -> Abstraction.pp_forall type_expression ppf x


and type_expression_orig ppf (te : type_expression) : unit =
  (* TODO: we should have a way to hook custom pretty-printers for some types and/or track the "origin" of types as they flow through the constraint solver. This is a temporary quick fix *)
  match te.orig_var with
  | None ->
    if Option.is_some (Combinators.get_t_bool te)
    then bool ppf
    else if Option.is_some (Combinators.get_t_option te)
    then option ppf te
    else fprintf ppf "%a" type_content_orig te.type_content
  | Some v -> type_expression ppf (Combinators.t_variable ~loc:te.location v ())


let rec expression ppf (e : expression) =
  fprintf ppf "%a" expression_content e.expression_content


and expression_content ppf (ec : expression_content) =
  match ec with
  | E_literal l -> Literal_value.pp ppf l
  | E_variable n -> Value_var.pp ppf n
  | E_contract n ->
    Format.fprintf
      ppf
      "Contract_of(%a)"
      Simple_utils.PP_helpers.(list_sep Module_var.pp (const "."))
      (List.Ne.to_list n)
  | E_application a -> Application.pp expression ppf a
  | E_constructor c -> Constructor.pp expression ppf c
  | E_constant c -> Constant.pp expression ppf c
  | E_record m -> Record.pp expression ppf m
  | E_accessor a -> Types.Accessor.pp expression ppf a
  | E_update u -> Types.Update.pp expression ppf u
  | E_lambda l -> Lambda.pp expression type_expression ppf l
  | E_type_abstraction e -> Type_abs.pp expression ppf e
  | E_matching m -> Types.Match_expr.pp expression type_expression_annot ppf m
  | E_recursive r -> Recursive.pp expression type_expression ppf r
  | E_let_in { let_binder; rhs; let_result; attributes = { hidden = false; _ } as attr }
    ->
    fprintf
      ppf
      "@[let %a =@;<1 2>%a%a in@ %a@]"
      (Pattern.pp type_expression_annot)
      let_binder
      expression
      rhs
      Types.ValueAttr.pp
      attr
      expression
      let_result
  | E_let_in
      { let_binder = _
      ; rhs = _
      ; let_result
      ; attributes =
          { inline = _
          ; no_mutation = _
          ; public = __LOC__
          ; view = _
          ; entry = _
          ; dyn_entry = _
          ; hidden = true
          ; thunk = _
          ; deprecated = _
          ; leading_comments = _
          }
      } -> fprintf ppf "%a" expression let_result
  | E_mod_in mi -> Mod_in.pp expression module_expr ppf mi
  | E_raw_code r -> Raw_code.pp expression ppf r
  | E_module_accessor ma -> Module_access.pp Value_var.pp ppf ma
  | E_type_inst ti -> type_inst ppf ti
  | E_let_mut_in { let_binder; rhs; let_result; attributes } ->
    Format.fprintf
      ppf
      "@[let mut %a =@;<1 2>%a%a in@ %a@]"
      (Pattern.pp type_expression_annot)
      let_binder
      expression
      rhs
      Types.ValueAttr.pp
      attributes
      expression
      let_result
  | E_coerce a -> Ascription.pp expression type_expression ppf a
  | E_assign a -> Assign.pp expression type_expression ppf a
  | E_deref n -> Format.fprintf ppf "!%a" Value_var.pp n
  | E_for for_loop -> For_loop.pp expression ppf for_loop
  | E_for_each for_each -> For_each_loop.pp expression ppf for_each
  | E_while while_loop -> While_loop.pp expression ppf while_loop


and type_inst ppf { forall; type_ } =
  fprintf ppf "%a@@{%a}" expression forall type_expression type_


and declaration ?(use_hidden = true) ppf (d : declaration) =
  match Location.unwrap d with
  | D_value vd ->
    if vd.attr.hidden && use_hidden
    then ()
    else Types.Value_decl.pp expression type_expression_annot ppf vd
  | D_irrefutable_match pd ->
    if pd.attr.hidden && use_hidden
    then ()
    else Types.Pattern_decl.pp expression type_expression_annot ppf pd
  | D_type td ->
    if td.type_attr.hidden && use_hidden
    then ()
    else Types.Type_decl.pp type_expression ppf td
  | D_module md ->
    if md.module_attr.hidden && use_hidden
    then ()
    else Types.Module_decl.pp module_expr (fun _ () -> ()) ppf md
  | D_module_include x -> fprintf ppf "include %a" module_expr x
  | D_signature sd -> Types.Signature_decl.pp signature ppf sd


and decl ppf d = declaration ppf d

and module_expr ppf (me : module_expr) : unit =
  Format.fprintf
    ppf
    "@[%a : %a@]"
    (Module_expr.pp decl)
    me.module_content
    signature
    me.signature


and sig_item ppf (d : sig_item) =
  match d with
  | S_value (var, type_, _) ->
    Format.fprintf ppf "@[<2>val %a :@ %a@]" Value_var.pp var type_expression type_
  | S_type (var, type_, _) ->
    Format.fprintf ppf "@[<2>type %a =@ %a@]" Type_var.pp var type_expression type_
  | S_type_var (var, _) -> Format.fprintf ppf "@[<2>type %a@]" Type_var.pp var
  | S_module (var, sig_) ->
    Format.fprintf ppf "@[<2>module %a =@ %a@]" Module_var.pp var signature sig_
  | S_module_type (var, sig_) ->
    Format.fprintf ppf "@[<2>module type %a =@ %a@]" Module_var.pp var signature sig_


and signature ppf (sig_ : signature) : unit =
  Format.fprintf
    ppf
    "@[<v>sig : %a@,@[<v1>@,%a@]@,end@]"
    signature_sort
    sig_.sig_sort
    (list_sep sig_item (tag "@,"))
    sig_.sig_items


and signature_sort ppf (sig_ : signature_sort) : unit =
  match sig_ with
  | Ss_module -> ()
  | Ss_contract { storage; parameter } ->
    Format.fprintf
      ppf
      "@[<v> < @, storage : %a @, parameter : %a >@]"
      type_expression
      storage
      type_expression
      parameter


let program_with_sig ?(use_hidden = false) ppf (p : program) =
  Format.fprintf
    ppf
    "@[<v>@,%a @,:@, %a@]"
    (list_sep (declaration ~use_hidden) (tag "@,"))
    p.pr_module
    signature
    p.pr_sig


let program ?(use_hidden = false) ppf (p : program) =
  list_sep (declaration ~use_hidden) (tag "@,") ppf p.pr_module
