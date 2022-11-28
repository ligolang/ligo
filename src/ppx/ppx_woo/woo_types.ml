module P = Ppxlib
module A = P.Ast_builder.Default

module SMap = struct
  include Map.Make (String)

  let to_kv_list_rev : 'a t -> (string * 'a) list =
   fun m -> Map.fold ~f:(fun ~key ~data prev -> (key, data) :: prev) m ~init:[]


  let to_kv_list : 'a t -> (string * 'a) list = fun m -> List.rev (to_kv_list_rev m)
end

type variant =
  { constructor_declarations : type_expression list SMap.t
  ; polymorphic : bool
  }

and record_field =
  { type_expression : type_expression
  ; default_value : P.expression option
  ; index : int
  }

and record = record_field SMap.t

and type_expression =
  | T_variant of variant
  | T_record of record
  | T_core of P.core_type

let get_t_core_opt = function
  | T_core ct -> Some ct
  | _ -> None


let record_field ?default_value index type_expression =
  { type_expression; default_value; index }


type label = string
type labelled_type = label * type_expression
type labelled_record_field = label * record_field
type labelled_types = label * type_expression list
type variant_map = type_expression list SMap.t
type non_recursive = bool

type type_declaration =
  { labelled_type : labelled_type
  ; non_recursive : non_recursive
  }

let type_declaration ?(non_recursive = false) labelled_type =
  { non_recursive; labelled_type }


type type_declarations = type_declaration list

let c_decls : (string * type_expression list) list -> type_expression list SMap.t =
 fun cds ->
  List.fold_left ~f:(fun old (key, data) -> SMap.set ~key ~data old) ~init:SMap.empty cds


let r_decls : labelled_record_field list -> record =
 fun rds ->
  List.fold_left ~f:(fun old (key, data) -> SMap.set ~key ~data old) ~init:SMap.empty rds
