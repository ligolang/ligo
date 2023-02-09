[@@@coverage exclude_file]

module Location = Simple_utils.Location
module Var = Simple_utils.Var
module List = Simple_utils.List
module Ligo_string = Simple_utils.Ligo_string
module Int64 = Caml.Int64
open Simple_utils.PP_helpers
open Ligo_prim
open Types

let ( type_content
    , type_injection
    , bool
    , option
    , type_expression
    , type_content_orig
    , type_expression_orig
    , type_expression_annot )
  =
  Ast_aggregated.PP.(
    ( type_content
    , type_injection
    , bool
    , option
    , type_expression
    , type_content_orig
    , type_expression_orig
    , type_expression_annot ))


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


and option_inline ppf inline =
  Format.(if inline then fprintf ppf "[@inline]" else fprintf ppf "")


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
