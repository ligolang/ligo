open Ligo_prim
open Types
open Format
open Simple_utils.PP_helpers


let sum_set_t value sep ppf m =
  let lst = List.sort ~compare:(fun (a,_) (b,_) -> Label.compare a b) m in
  let new_pp ppf (k, Rows.{associated_type;_}) = fprintf ppf "@[<h>%a -> %a@]" Label.pp k value associated_type in
  fprintf ppf "%a" (list_sep new_pp sep) lst

let sum_set_t x = sum_set_t x (tag " ,@ ")

let record_sep_t value sep ppf (m : (Label.t * type_expression Rows.row_element) list) =
  let attributes_2 (attr: string list) : string =
    List.map ~f:(fun s -> "[@@" ^ s ^ "]") attr |> String.concat
  in
  let lst = List.sort ~compare:(fun (a,_) (b,_) -> Label.compare a b) m in
  let new_pp ppf (k, Rows.{associated_type; attributes; _}) =
    let attr = attributes_2 attributes in
    fprintf ppf "@[<h>%a -> %a %s@]" Label.pp k value associated_type attr
  in fprintf ppf "%a" (list_sep new_pp sep) lst

let attributes_1 (attr: string list) : string =
  List.map ~f:(fun s -> "[@" ^ s ^ "]") attr |> String.concat

let rec type_content : formatter -> type_content -> unit =
  fun ppf te ->
  match te with
  | T_sum m -> (
    let s ppf = fprintf ppf "@[<hv 4>sum[%a]@]" (sum_set_t type_expression) in
    match m.attributes with [] -> fprintf ppf "%a" s m.fields
    |_ ->
      let attr = attributes_1 m.attributes in
      fprintf ppf "(%a %s)" s m.fields attr
  )
  | T_record m -> (
    let r = record_sep_t type_expression (const ";") in
    match m.attributes with
    | [] -> fprintf ppf "{%a}" r m.fields
    | _ ->
      let attr : string = attributes_1 m.attributes in
      fprintf ppf "({%a} %s)" r m.fields attr
  )
  | T_variable        tv -> Type_var.pp ppf tv
  | T_tuple            t -> Rows.PP.type_tuple  type_expression ppf t
  | T_arrow            a -> Arrow.pp      type_expression ppf a
  | T_annoted  (ty, str) -> fprintf ppf "(%a%%%s)" type_expression ty str
  | T_app            app -> Type_app.pp      type_expression ppf app
  | T_module_accessor ma -> Module_access.pp Type_var.pp ppf ma
  | T_singleton       x  -> Literal_value.pp            ppf x
  | T_abstraction     x  -> Abstraction.pp   type_expression ppf x
  | T_for_all         x  -> Abstraction.pp   type_expression ppf x

and type_expression ppf (te : type_expression) : unit =
  fprintf ppf "%a" type_content (te.type_content)

and type_expression_annot ppf (te : type_expression) : unit =
  fprintf ppf " : %a" type_expression te

let type_expression_option ppf (te : type_expression option) : unit =
  match te with
  | Some te -> fprintf ppf " : %a" type_expression te
  | None    -> fprintf ppf ""

let rec expression ppf (e : expression) =
  fprintf ppf "%a" expression_content @@ e.expression_content
and expression_content ppf (ec : expression_content) =
  match ec with
  | E_literal     l -> Literal_value.pp   ppf l
  | E_variable    n -> Value_var.pp        ppf n
  | E_application a -> Application.pp expression ppf a
  | E_constructor c -> Constructor.pp expression ppf c
  | E_constant c ->
      fprintf ppf "%a(%a)"
        Constant.pp_constant' ((fun (Constant.Const c) -> c) c.cons_name)
        (list_sep expression (tag " ,")) c.arguments
  | E_record      r -> record ppf r
  | E_tuple       t -> Record.pp_tuple       expression ppf t
  | E_accessor    a -> Types.Accessor.pp    expression ppf a
  | E_update      u -> Types.Update.pp      expression ppf u
  | E_lambda      l -> Lambda.pp      expression type_expression_option ppf l
  | E_type_abstraction e -> Type_abs.pp expression ppf e
  | E_matching    m -> Match_expr.pp expression type_expression_option ppf m
  | E_recursive  r -> Recursive.pp expression type_expression_annot ppf r
  | E_let_in     li -> Let_in.pp expression type_expression_option ppf li
  | E_type_in   ti -> Type_in.pp expression type_expression ppf ti
  | E_mod_in    mi -> Mod_in.pp  expression module_expr ppf mi
  | E_raw_code   r -> Raw_code.pp   expression ppf r
  | E_ascription a -> Ascription.pp expression type_expression ppf a
  | E_module_accessor ma -> Module_access.pp Value_var.pp ppf ma
  | E_cond       c -> Conditional.pp expression ppf c
  | E_sequence   s -> Sequence.pp   expression ppf s
  | E_skip       _ -> Skip.pp                  ppf ()
  | E_map        m -> Map_expr.pp   expression ppf m
  | E_big_map    m -> Map_expr.pp   expression ppf m
  | E_list       l -> Set_expr.pp   expression ppf l
  | E_set        s -> Set_expr.pp   expression ppf s
  | E_let_mut_in li ->
    Let_in.pp_mut expression type_expression_option ppf li
  | E_assign     a -> Assign.pp     expression type_expression_option ppf a
  | E_for        f -> For_loop.pp   expression ppf f
  | E_for_each   f -> For_each_loop.pp expression ppf f
  | E_while      w -> While_loop.pp    expression ppf w

and record ppf kvl =
  fprintf ppf "@[<v>{ %a }@]" (list_sep (fun ppf (l,e)-> Format.fprintf ppf "%a : %a" Label.pp l expression e) (tag " ;")) kvl


and declaration ppf (d : declaration) = match Location.unwrap d with
    D_value vd  -> Types.Value_decl.pp expression type_expression_option ppf vd
  | D_type  td  -> Types.Type_decl.pp type_expression ppf td
  | D_module md -> Types.Module_decl.pp module_expr ppf md

and decl ppf d = declaration ppf d

and module_expr ppf (me : module_expr) : unit =
    Location.pp_wrap (Module_expr.pp decl) ppf me
let program ppf (p : program) = list_sep declaration (tag "@,") ppf p
