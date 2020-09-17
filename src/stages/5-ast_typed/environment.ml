open Types
open Combinators

type t = environment
type element = environment_element

let make_element : type_expression -> environment -> environment_element_definition -> element =
  fun type_value source_environment definition -> {type_value ; source_environment ; definition}

let make_element_binder = fun t s -> make_element t s ED_binder

let make_element_declaration = fun s (expr : expression) ->
  let free_variables = Misc.Free_variables.(expression empty expr) in
  make_element (get_type_expression expr) s (ED_declaration {expression=expr ; free_variables})

let empty : t = { expression_environment = [] ; type_environment = [] }

let get_expr_environment : t -> expression_environment = fun { expression_environment ; type_environment=_ } -> expression_environment
(* TODO: generate *)
let get_type_environment : t -> type_environment = fun { expression_environment=_ ; type_environment } -> type_environment
(* TODO: generate *)
let map_expr_environment : _ -> t -> t = fun f { expression_environment ; type_environment } -> { expression_environment = f expression_environment ; type_environment }
let map_type_environment : _ -> t -> t = fun f { expression_environment ; type_environment } -> { expression_environment ; type_environment = f type_environment }

let add_expr : expression_variable -> element -> t -> t = fun expr_var env_elt -> map_expr_environment (fun x -> {expr_var ; env_elt} :: x)
let add_type : type_variable -> type_expression -> t -> t = fun type_variable type_ -> map_type_environment (fun x -> { type_variable ; type_ } :: x)
(* TODO: generate : these are now messy, clean them up. *)
let get_opt : expression_variable -> t -> element option = fun k x ->
  Option.bind (fun {expr_var=_ ; env_elt} -> Some env_elt) @@
    List.find_opt (fun {expr_var ; env_elt=_} -> Var.equal expr_var.wrap_content k.wrap_content) (get_expr_environment x)
let get_type_opt : type_variable -> t -> type_expression option = fun k x ->
  Option.bind (fun {type_variable=_ ; type_} -> Some type_) @@
    List.find_opt (fun {type_variable ; type_=_} -> Var.equal type_variable k) (get_type_environment x)

let add_ez_binder : expression_variable -> type_expression -> t -> t = fun k v e ->
  add_expr k (make_element_binder v e) e

let add_ez_declaration : expression_variable -> expression -> t -> t = fun k ae e ->
  add_expr k (make_element_declaration e ae) e

let add_ez_sum_type ?(env = empty) ?(type_name = Var.of_name "a_sum_type") (lst : (label * row_element) list) =
  add_type type_name (make_t_ez_sum lst) env

let get_constructor : label -> t -> (type_expression * type_expression) option = fun k x -> (* Left is the constructor, right is the sum type *)
    let aux = fun {type_variable=_ ; type_} ->
      match type_.type_content with
      | T_sum m ->
        (match LMap.find_opt k m with
           Some {associated_type ; _} -> Some (associated_type , type_)
         | None -> None)
      | _ -> None
    in
    List.find_map aux (get_type_environment x)


module PP = struct
  open Format
  include PP
  open PP_helpers

  let list_sep_scope x = list_sep x (const " | ")

  let environment_element = fun ppf {expr_var ; env_elt} ->
    fprintf ppf "%a -> %a" PP.expression_variable expr_var PP.type_expression env_elt.type_value

  let type_environment_element = fun ppf {type_variable ; type_} ->
    fprintf ppf "%a -> %a" PP.type_variable type_variable PP.type_expression type_

  let expr_environment : _ -> expression_environment -> unit = fun ppf lst ->
    fprintf ppf "Env:[%a]" (list_sep environment_element (tag "@,")) lst

  let type_environment = fun ppf lst ->
    fprintf ppf "Type env:[%a]" (list_sep type_environment_element (tag "@,")) lst

  let environment : _ -> environment -> unit = fun ppf e ->
    fprintf ppf "- %a\t%a"
      expr_environment (get_expr_environment e)
      type_environment (get_type_environment e)

end
