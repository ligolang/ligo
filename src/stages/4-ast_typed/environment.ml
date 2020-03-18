open Types
open Combinators

type element = environment_element
let make_element : type_expression -> full_environment -> environment_element_definition -> element =
  fun type_value source_environment definition -> {type_value ; source_environment ; definition}

let make_element_binder = fun t s -> make_element t s ED_binder
let make_element_declaration = fun s (expr : expression) ->
  let free_variables = Misc.Free_variables.(expression empty expr) in
  make_element (get_type_expression expr) s (ED_declaration {expr ; free_variables})

module Small = struct
  type t = small_environment

  let empty : t = { expression_environment = [] ; type_environment = [] }

  (* TODO: generate *)
  let get_environment : t -> environment = fun { expression_environment ; type_environment=_ } -> expression_environment
  (* TODO: generate *)
  let get_type_environment : t -> type_environment = fun { expression_environment=_ ; type_environment } -> type_environment
  (* TODO: generate *)
  let map_environment : _ -> t -> t = fun f { expression_environment ; type_environment } -> { expression_environment = f expression_environment ; type_environment }
  let map_type_environment : _ -> t -> t = fun f { expression_environment ; type_environment } -> { expression_environment ; type_environment = f type_environment }

  let add : expression_variable -> element -> t -> t = fun expr_var env_elt -> map_environment (fun x -> {expr_var ; env_elt} :: x)
  let add_type : type_variable -> type_expression -> t -> t = fun type_variable type_ -> map_type_environment (fun x -> { type_variable ; type_ } :: x)
  (* TODO: generate : these are now messy, clean them up. *)
  let get_opt : expression_variable -> t -> element option = fun k x -> Option.bind (fun {expr_var=_ ; env_elt} -> Some env_elt) @@ List.find_opt (fun {expr_var ; env_elt=_} -> Var.equal expr_var k) (get_environment x)
  let get_type_opt : type_variable -> t -> type_expression option = fun k x -> Option.bind (fun {type_variable=_ ; type_} -> Some type_) @@ List.find_opt (fun {type_variable ; type_=_} -> Var.equal type_variable k) (get_type_environment x)
end

type t = full_environment
let empty : environment = Small.(get_environment empty)
let full_empty : t = List.Ne.singleton Small.empty
let add : expression_variable -> element -> t -> t = fun k v -> List.Ne.hd_map (Small.add k v)
let add_ez_binder : expression_variable -> type_expression -> t -> t = fun k v e ->
  List.Ne.hd_map (Small.add k (make_element_binder v e)) e
let add_ez_declaration : expression_variable -> expression -> t -> t = fun k ae e ->
  List.Ne.hd_map (Small.add k (make_element_declaration e ae)) e
let add_ez_ae = add_ez_declaration
let add_type : type_variable -> type_expression -> t -> t = fun k v -> List.Ne.hd_map (Small.add_type k v)
let get_opt : expression_variable -> t -> element option = fun k x -> List.Ne.find_map (Small.get_opt k) x
let get_type_opt : type_variable -> t -> type_expression option = fun k x -> List.Ne.find_map (Small.get_type_opt k) x

let get_constructor : constructor' -> t -> (type_expression * type_expression) option = fun k x -> (* Left is the constructor, right is the sum type *)
  let aux = fun x ->
    let aux = fun {type_variable=_ ; type_} ->
      match type_.type_content with
      | T_sum m ->
        (match CMap.find_opt k m with
           Some km -> Some (km , type_)
         | None -> None)
      | _ -> None
    in
    List.find_map aux (Small.get_type_environment x) in
  List.Ne.find_map aux x


module PP = struct
  open Format
  include PP
  open PP_helpers

  let list_sep_scope x = list_sep x (const " | ")

  let environment_element = fun ppf {expr_var ; env_elt} ->
    fprintf ppf "%a -> %a" PP.expression_variable expr_var PP.type_expression env_elt.type_value

  let type_environment_element = fun ppf {type_variable ; type_} ->
    fprintf ppf "%a -> %a" PP.type_variable type_variable PP.type_expression type_

  let environment : _ -> environment -> unit = fun ppf lst ->
    fprintf ppf "E[%a]" (list_sep environment_element (const " , ")) lst

  let type_environment = fun ppf lst ->
    fprintf ppf "T[%a]" (list_sep type_environment_element (const " , ")) lst

  let small_environment : _ -> small_environment -> unit = fun ppf e ->
    fprintf ppf "- %a\t%a"
      environment (Small.get_environment e)
      type_environment (Small.get_type_environment e)

  let full_environment : _ -> full_environment -> unit = fun ppf e ->
    fprintf ppf "@[%a]"
      (ne_list_sep small_environment (tag "@;")) e
end

open Trace

let get_trace : expression_variable -> t -> element result = fun s env ->
  let error =
    let title () = "missing var not in env" in
    let content () = Format.asprintf "\nvar: %a\nenv: %a\n" PP. expression_variable s PP.full_environment env in
    error title content in
  trace_option error @@ get_opt s env
