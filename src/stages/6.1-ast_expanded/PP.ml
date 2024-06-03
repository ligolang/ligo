[@@@coverage exclude_file]

module Location = Simple_utils.Location
module Var = Simple_utils.Var
module List = Simple_utils.List
module Ligo_string = Simple_utils.Ligo_string
open Simple_utils.PP_helpers
open Ligo_prim
open Types

let rec type_content : Format.formatter -> type_content -> unit =
 fun ppf tc ->
  match tc with
  | T_variable tv -> Type_var.pp ppf tv
  | T_sum row -> Row.PP.sum_type type_expression Layout.pp ppf row
  | T_record row -> Row.PP.record_type type_expression Layout.pp ppf row
  | T_arrow a -> Arrow.pp type_expression ppf a
  | T_constant tc -> type_injection ppf tc
  | T_singleton x -> Literal_value.pp ppf x
  | T_for_all x -> Abstraction.pp_forall type_expression ppf x


and type_injection ppf { language; injection; parameters } =
  ignore language;
  Format.fprintf
    ppf
    "%s%a"
    (Literal_types.to_string injection)
    (list_sep_d_par type_expression)
    parameters


and bool ppf : unit = Format.fprintf ppf "bool"

and option ppf (te : type_expression) : unit =
  let t = Combinators.get_t_option te in
  match t with
  | Some t -> Format.fprintf ppf "option (%a)" type_expression t
  | None -> Format.fprintf ppf "option ('a)"


and type_expression ppf (te : type_expression) : unit =
  (* TODO: we should have a way to hook custom pretty-printers for some types and/or track the "origin" of types as they flow through the constraint solver. This is a temporary quick fix *)
  if Option.is_some (Combinators.get_t_bool te)
  then bool ppf
  else if Option.is_some (Combinators.get_t_option te)
  then option ppf te
  else Format.fprintf ppf "%a" type_content te.type_content


let rec type_content_orig : Format.formatter -> type_content -> unit =
 fun ppf tc ->
  match tc with
  | T_variable tv -> Type_var.pp ppf tv
  | T_sum row -> Row.PP.sum_type type_expression (fun _ _ -> ()) ppf row
  | T_record row -> Row.PP.record_type type_expression (fun _ _ -> ()) ppf row
  | T_arrow a -> Arrow.pp type_expression ppf a
  | T_constant tc -> type_injection ppf tc
  | T_singleton x -> Literal_value.pp ppf x
  | T_for_all x -> Abstraction.pp_forall type_expression ppf x


and type_expression_orig ppf (te : type_expression) : unit =
  (* TODO: we should have a way to hook custom pretty-printers for some types and/or track the "origin" of types as they flow through the constraint solver. This is a temporary quick fix *)
  match te.orig_var with
  | None ->
    if Option.is_some (Combinators.get_t_bool te)
    then bool ppf
    else if Option.is_some (Combinators.get_t_option te)
    then option ppf te
    else Format.fprintf ppf "%a" type_content_orig te.type_content
  | Some v -> Ast_core.(PP.type_expression ppf (t_variable ~loc:te.location v ()))


let type_expression_annot ppf (te : type_expression) =
  Format.fprintf ppf " : %a" type_expression te


let rec expression ppf (e : expression) =
  Format.fprintf ppf "%a" expression_content e.expression_content


and expression_content ppf (ec : expression_content) =
  match ec with
  | E_literal l -> Literal_value.pp ppf l
  | E_variable n -> Value_var.pp ppf n
  | E_application a -> Application.pp expression ppf a
  | E_constructor c -> Constructor.pp expression ppf c
  | E_constant c -> Constant.pp expression ppf c
  | E_record m -> Record.pp expression ppf m
  | E_accessor a -> Types.Accessor.pp expression ppf a
  | E_update u -> Types.Update.pp expression ppf u
  | E_lambda l -> Lambda.pp expression (fun _ _ -> ()) ppf l
  | E_type_abstraction e -> Type_abs.pp expression ppf e
  | E_matching { matchee; cases } ->
    Format.fprintf
      ppf
      "@[<v 2> match @[%a@] with@ %a@]"
      expression
      matchee
      (matching expression)
      cases
  | E_recursive r -> Recursive.pp expression (fun _ _ -> ()) ppf r
  | E_let_in x when x.attributes.hidden ->
    Format.fprintf ppf "@[<h>%a@]" expression x.let_result
  | E_let_in x -> Let_in.pp expression (fun _ _ -> ()) ppf x
  | E_raw_code r -> Raw_code.pp expression ppf r
  | E_type_inst ti -> type_inst ppf ti
  | E_let_mut_in x -> Let_in.pp_mut expression type_expression_annot ppf x
  | E_assign a -> Assign.pp expression type_expression_annot ppf a
  | E_deref var -> Format.fprintf ppf "!%a" Value_var.pp var
  | E_for for_loop -> For_loop.pp expression ppf for_loop
  | E_for_each for_each -> For_each_loop.pp expression ppf for_each
  | E_while while_loop -> While_loop.pp expression ppf while_loop


and type_inst ppf { forall; type_ } =
  Format.fprintf ppf "%a@@{%a}" expression forall type_expression_annot type_


and matching_variant_case f ppf { constructor = c; pattern; body } =
  Format.fprintf ppf "@[<v 2>| %a %a ->@ %a@]" Label.pp c Value_var.pp pattern f body


and matching f ppf m =
  match m with
  | Match_variant { cases; tv = _ } ->
    Format.fprintf ppf "@[%a@]" (list_sep (matching_variant_case f) (tag "@ ")) cases
  | Match_record { fields; body; tv = _ } ->
    (* let with_annots f g ppf (a , b) = fprintf ppf "%a:%a" f a g b in *)
    Format.fprintf
      ppf
      "| @[%a@] ->@ @[%a@]"
      (Record.pp (Binder.pp type_expression))
      fields
      f
      body
