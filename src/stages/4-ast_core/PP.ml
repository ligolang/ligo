[@@@coverage exclude_file]

module Int64 = Caml.Int64
open Ligo_prim
open Types
open Format
open Simple_utils.PP_helpers

type 'a pretty_printer = Format.formatter -> 'a -> unit

let rec type_expression ppf (te : type_expression) : unit =
  (* TODO: we should have a way to hook custom pretty-printers for some types and/or track the "origin" of types as they flow through the constraint solver. This is a temporary quick fix *)
  if Option.is_some (Combinators.get_t_bool te)
  then bool ppf
  else if Option.is_some (Combinators.get_t_option te)
  then option ppf te
  else fprintf ppf "%a" type_content te.type_content


and bool ppf = fprintf ppf "bool"
and layout = Simple_utils.PP_helpers.if_present Layout.pp

and option ppf (te : type_expression) =
  let t = Combinators.get_t_option te in
  match t with
  | Some t -> fprintf ppf "option (%a)" type_expression t
  | None -> fprintf ppf "option ('a)"


and type_content : formatter -> type_content -> unit =
 fun ppf te ->
  match te with
  | T_variable tv -> Type_var.pp ppf tv
  | T_contract_parameter x ->
    Format.fprintf
      ppf
      "Parameter_of(%a)"
      Simple_utils.PP_helpers.(list_sep Module_var.pp (const "."))
      (List.Ne.to_list x)
  | T_constant (t, _) -> string ppf (Literal_types.to_string t)
  | T_sum row -> Row.PP.sum_type type_expression layout ppf row
  | T_record row -> Row.PP.record_type type_expression layout ppf row
  | T_arrow a -> Arrow.pp type_expression ppf a
  | T_app app -> Type_app.pp (Module_access.pp Type_var.pp) type_expression ppf app
  | T_module_accessor ma -> Module_access.pp Type_var.pp ppf ma
  | T_singleton x -> Literal_value.pp ppf x
  | T_abstraction x -> Abstraction.pp_type_abs type_expression ppf x
  | T_for_all x -> Abstraction.pp_forall type_expression ppf x


let type_expression_option ppf (te : type_expression option) : unit =
  match te with
  | Some te -> fprintf ppf " : %a" type_expression te
  | None -> fprintf ppf ""


let rec expression ppf (e : expression) =
  fprintf ppf "@[%a@]" expression_content e.expression_content


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
  | E_lambda l -> Lambda.pp expression type_expression_option ppf l
  | E_type_abstraction e -> Type_abs.pp expression ppf e
  | E_matching m -> Match_expr.pp expression type_expression_option ppf m
  | E_recursive r -> Recursive.pp expression type_expression ppf r
  | E_let_in { let_binder; rhs; let_result; attributes } ->
    fprintf
      ppf
      "@[let %a =@;<1 2>%a%a in@ %a@]"
      (Pattern.pp type_expression_option)
      let_binder
      expression
      rhs
      Value_attr.pp
      attributes
      expression
      let_result
  | E_type_in ti -> Type_in.pp expression type_expression ppf ti
  | E_mod_in mi -> Mod_in.pp expression module_expr ppf mi
  | E_raw_code r -> Raw_code.pp expression ppf r
  | E_ascription a -> Ascription.pp expression type_expression ppf a
  | E_module_accessor ma -> Module_access.pp Value_var.pp ppf ma
  | E_let_mut_in { let_binder; rhs; let_result; attributes } ->
    fprintf
      ppf
      "@[let mut %a =@;<1 2>%a%a in@ %a@]"
      (Pattern.pp type_expression_option)
      let_binder
      expression
      rhs
      Value_attr.pp
      attributes
      expression
      let_result
  | E_assign a -> Assign.pp expression type_expression_option ppf a
  | E_for for_loop -> For_loop.pp expression ppf for_loop
  | E_for_each for_each -> For_each_loop.pp expression ppf for_each
  | E_while while_loop -> While_loop.pp expression ppf while_loop


and declaration ppf (d : declaration) =
  match Location.unwrap d with
  | D_value vd ->
    if not vd.attr.hidden
    then Types.Value_decl.pp expression type_expression_option ppf vd
  | D_irrefutable_match pd ->
    if not pd.attr.hidden
    then Types.Pattern_decl.pp expression type_expression_option ppf pd
  | D_type td -> Types.Type_decl.pp type_expression ppf td
  | D_module md ->
    Types.Module_decl.pp
      module_expr
      Simple_utils.PP_helpers.(option module_annotation)
      ppf
      md
  | D_module_include me -> fprintf ppf "include (%a)" module_expr me
  | D_signature sd -> Types.Signature_decl.pp signature_expr ppf sd


and decl ppf d = declaration ppf d

and module_expr ppf (me : module_expr) : unit =
  Location.pp_wrap (Module_expr.pp decl) ppf me


and module_annotation ppf (annot : module_annotation) : unit =
  fprintf ppf "%a" signature_expr annot.signature


and sig_item_attribute ppf { view; entry; dyn_entry; optional } =
  let pp_if_set str ppf attr =
    if attr then fprintf ppf "[@@%s]" str else fprintf ppf ""
  in
  fprintf
    ppf
    "%a%a%a%a"
    (pp_if_set "view")
    view
    (pp_if_set "entry")
    entry
    (pp_if_set "dyn_entry")
    dyn_entry
    (pp_if_set "optional")
    optional


and sig_item ppf (d : sig_item) =
  match d with
  | S_value (var, type_, attr) ->
    Format.fprintf
      ppf
      "@[<2>val %a :@ %a@;<1 2>%a@]"
      Value_var.pp
      var
      type_expression
      type_
      sig_item_attribute
      attr
  | S_type (var, type_) ->
    Format.fprintf ppf "@[<2>type %a =@ %a@]" Type_var.pp var type_expression type_
  | S_type_var var -> Format.fprintf ppf "@[<2>type %a@]" Type_var.pp var
  | S_module (var, sig_) ->
    Format.fprintf ppf "@[<2>module %a :@ %a@]" Module_var.pp var signature sig_
  | S_module_type (var, sig_) ->
    Format.fprintf ppf "@[<2>module type %a =@ %a@]" Module_var.pp var signature sig_
  | S_include sig_ -> Format.fprintf ppf "@[<2>include %a@]" signature_expr sig_


and signature ppf (sig_ : signature) : unit =
  Format.fprintf
    ppf
    "@[<v>sig@[<v1>@,%a@]@,end@]"
    (list_sep sig_item (tag "@,"))
    sig_.items


and signature_expr ppf (sig_expr : signature_expr) : unit =
  match Location.unwrap sig_expr with
  | S_sig sig_ -> Format.fprintf ppf "%a" signature sig_
  | S_path path -> Simple_utils.PP_helpers.(ne_list_sep Module_var.pp (tag ".")) ppf path


let program ppf (p : program) = list_sep declaration (tag "@,") ppf p
