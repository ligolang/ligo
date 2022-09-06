[@@@coverage exclude_file]
module Int64 = Caml.Int64
open Types
open Format
open Simple_utils.PP_helpers
open Ligo_prim

type 'a pretty_printer = Format.formatter -> 'a -> unit

let lmap_sep value sep ppf m =
  let lst = List.sort ~compare:(fun (a,_) (b,_) -> Label.compare a b) m in
  let new_pp ppf (k, v) = fprintf ppf "@[<h>%a -> %a@]" Label.pp k value v in
  fprintf ppf "%a" (list_sep new_pp sep) lst

let record_sep value sep ppf (m : 'a Record.t) =
  let lst = Record.LMap.to_kv_list m in
  fprintf ppf "%a" (lmap_sep value sep) lst

let tuple_sep value sep ppf m =
  assert (Record.is_tuple m);
  let lst = Record.tuple_of_record m in
  let new_pp ppf (_, v) = fprintf ppf "%a" value v in
  fprintf ppf "%a" (list_sep new_pp sep) lst

let tuple_or_record_sep_t value format_record sep_record format_tuple sep_tuple ppf m =
  if Record.is_tuple m then
    fprintf ppf format_tuple (tuple_sep value (tag sep_tuple)) m
  else
    fprintf ppf format_record (record_sep value (tag sep_record)) m

let tuple_or_record_sep_type value = tuple_or_record_sep_t value "@[<h>record[%a]@]" " ,@ " "@[<h>( %a )@]" " *@ "
let lmap_sep_d x = lmap_sep x (tag " ,@ ")

let rec type_expression ppf (te : type_expression) : unit =
  (* TODO: we should have a way to hook custom pretty-printers for some types and/or track the "origin" of types as they flow through the constraint solver. This is a temporary quick fix *)
  if Option.is_some (Combinators.get_t_bool   te) then bool   ppf    else
  if Option.is_some (Combinators.get_t_option te) then option ppf te
  else
    fprintf ppf "%a" type_content te.type_content
and bool ppf = fprintf ppf "%a" Type_var.pp Ligo_prim.Literal_types.v_bool
and option ppf (te : type_expression) =
  let t = Combinators.get_t_option te in
    (match t with
      Some t -> fprintf ppf "option (%a)" type_expression t
    | None   -> fprintf ppf "option ('a)")
and type_content : formatter -> type_content -> unit =
  fun ppf te ->
  match te with
  | T_variable        tv -> Type_var.pp ppf tv
  | T_sum              m -> fprintf ppf "@[<h>sum[%a]@]" (lmap_sep_d row) (Record.LMap.to_kv_list_rev m.fields)
  | T_record           m -> fprintf ppf "%a" (tuple_or_record_sep_type row) m.fields
  | T_arrow            a -> Arrow.pp      type_expression ppf a
  | T_app            app -> Type_app.pp      type_expression ppf app
  | T_module_accessor ma -> Module_access.pp Type_var.pp ppf ma
  | T_singleton       x  -> Literal_value.pp            ppf x
  | T_abstraction     x  -> Abstraction.pp   type_expression ppf x
  | T_for_all         x  -> Abstraction.pp   type_expression ppf x

and row : formatter -> row_element -> unit =
  fun ppf { associated_type ; michelson_annotation=_ ; decl_pos=_ } ->
    fprintf ppf "%a"
      type_expression associated_type

let type_expression_option ppf (te : type_expression option) : unit =
  match te with
  | Some te -> fprintf ppf " : %a" type_expression te
  | None    -> fprintf ppf ""

let rec expression ppf (e : expression) =
  fprintf ppf "@[%a@]" expression_content e.expression_content
and expression_content ppf (ec : expression_content) =
  match ec with
  | E_literal     l -> Literal_value.pp   ppf l
  | E_variable    n -> Value_var.pp        ppf n
  | E_application a -> Application.pp expression ppf a
  | E_constructor c -> Constructor.pp expression ppf c
  | E_constant    c -> Constant.pp expression ppf c
  | E_record      m -> Record.pp expression ppf m
  | E_accessor    a -> Types.Accessor.pp    expression ppf a
  | E_update      u -> Types.Update.pp      expression ppf u
  | E_lambda      l -> Lambda.pp      expression type_expression_option ppf l
  | E_type_abstraction e -> Type_abs.pp expression ppf e
  | E_matching    m -> Match_expr.pp expression type_expression_option ppf m
  | E_recursive  r -> Recursive.pp expression type_expression ppf r
  | E_let_in { let_binder ;rhs ; let_result; attr } ->
    fprintf ppf "@[let %a =@;<1 2>%a%a in@ %a@]"
      (Binder.pp type_expression_option) let_binder
      expression rhs
      Types.ValueAttr.pp attr
      expression let_result
  | E_type_in   ti -> Type_in.pp expression type_expression ppf ti
  | E_mod_in    mi -> Mod_in.pp  expression module_expr ppf mi
  | E_raw_code   r -> Raw_code.pp   expression ppf r
  | E_ascription a -> Ascription.pp expression type_expression ppf a
  | E_module_accessor ma -> Module_access.pp Value_var.pp ppf ma
  | E_assign     a -> Assign.pp     expression type_expression_option ppf a

and declaration ppf (d : declaration) = match Location.unwrap d with
    D_value vd  -> Types.Value_decl.pp expression type_expression_option ppf vd
  | D_type  td  -> Types.Type_decl.pp type_expression ppf td
  | D_module md -> Types.Module_decl.pp module_expr ppf md

and decl ppf d = declaration ppf d
and module_expr ppf (me : module_expr) : unit =
    Location.pp_wrap (Module_expr.pp decl) ppf me

let program ppf (p : program) = list_sep declaration (tag "@,") ppf p
