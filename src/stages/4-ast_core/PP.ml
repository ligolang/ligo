[@@@coverage exclude_file]
module Int64 = Caml.Int64
open Types
open Format
open Simple_utils.PP_helpers
include Stage_common.PP

type 'a pretty_printer = Format.formatter -> 'a -> unit

let lmap_sep value sep ppf m =
  let lst = List.sort ~compare:(fun (Label a,_) (Label b,_) -> String.compare a b) m in
  let new_pp ppf (k, v) = fprintf ppf "@[<h>%a -> %a@]" label k value v in
  fprintf ppf "%a" (list_sep new_pp sep) lst

let record_sep value sep ppf (m : 'a label_map) =
  let lst = LMap.to_kv_list m in
  let lst = List.dedup_and_sort ~compare:(fun (Label a,_) (Label b,_) -> String.compare a b) lst in
  let new_pp ppf (k, v) = fprintf ppf "@[<h>%a -> %a@]" label k value v in
  fprintf ppf "%a" (list_sep new_pp sep) lst

let tuple_sep value sep ppf m =
  assert (Helpers.is_tuple_lmap m);
  let lst = Helpers.tuple_of_record m in
  let new_pp ppf (_, v) = fprintf ppf "%a" value v in
  fprintf ppf "%a" (list_sep new_pp sep) lst

let tuple_or_record_sep_t value format_record sep_record format_tuple sep_tuple ppf m =
  if Helpers.is_tuple_lmap m then
    fprintf ppf format_tuple (tuple_sep value (tag sep_tuple)) m
  else
    fprintf ppf format_record (record_sep value (tag sep_record)) m

let tuple_or_record_sep_type value = tuple_or_record_sep_t value "@[<h>record[%a]@]" " ,@ " "@[<h>( %a )@]" " *@ "

let list_sep_d_short x = list_sep x (tag " , ")
let list_sep_d x = list_sep x (tag " ,@ ")
let kv_short value_pp ~assoc ppf (k, v) = fprintf ppf "%a%s%a" label k assoc value_pp v
let lmap_sep_short x ~sep ~assoc ppf m =
  let lst = List.sort ~compare:(fun (Label a,_) (Label b,_) -> String.compare a b) m in
  list_sep (kv_short x ~assoc) (tag sep) ppf lst
let lmap_sep_d x = lmap_sep x (tag " ,@ ")

let rec constraint_identifier_unicode (ci : Int64.t) =
  let digit =
    let ( - ) = Int64.sub in
    let ( / ) = Int64.div in
    let ( * ) = Int64.mul in
    match (ci - ((ci / 10L) * 10L)) with
      a when Int64.equal a 0L -> "₀"
    | a when Int64.equal a 1L -> "₁"
    | a when Int64.equal a 2L -> "₂"
    | a when Int64.equal a 3L -> "₃"
    | a when Int64.equal a 4L -> "₄"
    | a when Int64.equal a 5L -> "₅"
    | a when Int64.equal a 6L -> "₆"
    | a when Int64.equal a 7L -> "₇"
    | a when Int64.equal a 8L -> "₈"
    | a when Int64.equal a 9L -> "₉"
    | _ -> failwith (Format.asprintf "internal error: couldn't pretty-print int64: %Li (is it a negative number?)" ci)
  in
  if Int64.equal ci 0L then "" else (constraint_identifier_unicode (Int64.div ci 10L)) ^ digit

let t_attributes ppf ({ public ; hidden } : type_attribute) =
  fprintf ppf "%a%a"
    option_public public
    option_hidden hidden
let m_attributes ppf x = t_attributes ppf x

let constraint_identifier_short ppf x =
  if Int64.equal x 0L
  then Format.fprintf ppf "₀"
  else Format.fprintf ppf "%s" (constraint_identifier_unicode x)

let list_sep_d_par f ppf lst =
  match lst with
  | [] -> ()
  | _ -> fprintf ppf " (%a)" (list_sep_d f) lst

let rec type_expression ppf (te : type_expression) : unit =
  (* TODO: we should have a way to hook custom pretty-printers for some types and/or track the "origin" of types as they flow through the constraint solver. This is a temporary quick fix *)
  if Option.is_some (Combinators.get_t_bool   te) then bool   ppf    else 
  if Option.is_some (Combinators.get_t_option te) then option ppf te 
  else
    fprintf ppf "%a" type_content te.type_content
and bool ppf = fprintf ppf "%a" type_variable Stage_common.Constant.v_bool
and option ppf (te : type_expression) = 
  let t = Combinators.get_t_option te in
    (match t with
      Some t -> fprintf ppf "option (%a)" type_expression t
    | None   -> fprintf ppf "option ('a)")
and type_content : formatter -> type_content -> unit =
  fun ppf te ->
  match te with
  | T_variable        tv -> type_variable ppf tv
  | T_sum              m -> fprintf ppf "@[<h>sum[%a]@]" (lmap_sep_d row) (LMap.to_kv_list_rev m.fields)
  | T_record           m -> fprintf ppf "%a" (tuple_or_record_sep_type row) m.fields
  | T_arrow            a -> arrow         type_expression ppf a
  | T_app              a -> type_app type_expression ppf a
  | T_module_accessor ma -> module_access type_variable ppf ma
  | T_singleton       x  -> literal       ppf             x
  | T_abstraction     x  -> abstraction   type_expression ppf x
  | T_for_all         x  -> for_all       type_expression ppf x

and row : formatter -> row_element -> unit =
  fun ppf { associated_type ; michelson_annotation=_ ; decl_pos=_ } ->
    fprintf ppf "%a"
      type_expression associated_type

let rec expression ppf (e : expression) =
  fprintf ppf "@[%a@]" expression_content e.expression_content
and expression_content ppf (ec : expression_content) =
  match ec with
  | E_literal          l -> literal                    ppf l
  | E_variable         n -> expression_variable        ppf n
  | E_application      a -> application     expression ppf a
  | E_constructor      c -> constructor     expression ppf c
  | E_constant         c -> constant        expression ppf c
  | E_record           r -> record          expression ppf r
  | E_record_accessor ra -> record_accessor expression ppf ra
  | E_record_update   ru -> record_update   expression ppf ru
  | E_lambda    l -> lambda expression type_expression ppf l
  | E_type_abstraction e -> type_abs expression ppf e
  | E_recursive r -> recursive expression type_expression ppf r
  | E_matching x -> fprintf ppf "%a" (match_exp expression type_expression) x
  | E_let_in { let_binder ;rhs ; let_result; attr } ->
    fprintf ppf "@[let %a =@;<1 2>%a%a in@ %a@]"
      (binder type_expression) let_binder
      expression rhs
      e_attributes attr
      expression let_result
  | E_type_in   {type_binder; rhs; let_result} ->
    fprintf ppf "@[let %a =@;<1 2>%a in@ %a@]"
      type_variable type_binder
      type_expression rhs
      expression let_result
  | E_mod_in {module_binder; rhs ; let_result;} ->
    fprintf ppf "@[let module %a = struct@; @[<v>%a@] end in@ %a@]"
      module_variable module_binder
      (module_expr expression type_expression e_attributes t_attributes m_attributes) rhs
      expression let_result
  | E_raw_code r -> raw_code expression ppf r
  | E_ascription a -> ascription expression type_expression ppf a
  | E_module_accessor ma -> module_access expression_variable ppf ma
  | E_assign a -> assign expression type_expression ppf a

let module_ ppf (p : module_) = declarations expression type_expression e_attributes t_attributes m_attributes ppf p
