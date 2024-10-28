(* Some additional wrappers for decoding the nodes of the
   tree-sitter-generated parser in C *)

module Pos = Simple_utils.Pos
module Region = Simple_utils.Region

(* Misc *)

let ( <@ ) = Simple_utils.Ligo_fun.( <@ )
let sprintf = Printf.sprintf

(* To print the AST in ASCII art *)

module Tree = Cst_shared.Tree

(* Tree-sitter ctypes-APIs for types and related functions *)

module TS_types = Tree_sitter.Api.Types
module TS_fun = Tree_sitter.Api.Functions

(* Integers needed by the tree-sitter APIs above *)

module UInt32 = Unsigned.UInt32
module UInt = Unsigned.UInt

(* Tree-sitter API for TypeScript *)

let tree_sitter_typescript = Tree_sitter_typescript.Api.Functions.tree_sitter_typescript

(* ocaml-ctypes types and bindings (only global module opening) *)

open Ctypes

(* Type aliases *)

type ts_tree = TS_types.ts_tree structure
type ts_tree_ptr = TS_types.ts_tree structure Ctypes_static.ptr
type ts_forest = ts_tree list
type ts_point = TS_types.ts_point structure
type ts_range = TS_types.ts_range structure

(* Converting C strings of type 'char*' to OCaml strings of type
   [string]. *)

let string_of_char_ptr (ptr : char ptr) : string =
  let rec get_length (p : char ptr) : int =
    if !@p = '\000' then 0 else 1 + get_length (p +@ 1)
  in
  let length = get_length ptr in
  let buffer = Bytes.create length in
  for i = 0 to length - 1 do
    Bytes.set buffer i !@(ptr +@ i)
  done;
  Bytes.to_string buffer

(* Length of string as an unsigned integer *)

let uint32_len string = UInt32.of_int (String.length string)

(* Handling of null nodes *)

let node_to_opt node = if TS_fun.ts_node_is_null node then None else Some node

let node_to_res node =
  if TS_fun.ts_node_is_null node then Error "INVALID: Missing node." else Ok node

let opt_to_res = function
  | None -> Error "INVALID: Missing node."
  | Some node -> Ok node

(* Wrappers for filtering fields (failure on null node or optional value) *)

let child_with_field field node =
  let child = TS_fun.ts_node_child_by_field_name node field (uint32_len field) in
  if TS_fun.ts_node_is_null child
  then Error (sprintf "INVALID: Missing field %S." field)
  else Result.Ok child

let child_with_field_opt field node =
  node_to_opt @@ TS_fun.ts_node_child_by_field_name node field (uint32_len field)

(* Printing the tree *)
(*
let print_node (node: ts_tree) : unit =
  let ptr_char = TS_fun.ts_node_string node in
  Printf.printf "%s\n%!" @@ string_of_char_ptr ptr_char
*)

(* Converting a node to an OCaml string *)

let string_of_ts_node_type (node : ts_tree) : string =
  if TS_fun.ts_node_is_null node
  then "NULL"
  else string_of_char_ptr @@ TS_fun.ts_node_type node

(* Parsing a string expected to contain a valid TypeScript program *)

let parse_typescript_string (source_code : string) : ts_tree_ptr =
  let open Core in
  let parser = TS_fun.ts_parser_new ()
  and language = tree_sitter_typescript () in
  let (_ : bool) = TS_fun.ts_parser_set_language parser language in
  let null_tree = from_voidp TS_types.ts_tree null in
  let parse_tree =
    TS_fun.ts_parser_parse_string
      parser
      null_tree
      source_code
      (UInt32.of_int @@ String.length source_code)
  in
  TS_fun.ts_parser_delete parser;
  parse_tree

(* Collating named/all children of a given node, except
   comment/error/missing nodes *)

let collect select_child arity node =
  if TS_fun.ts_node_is_null node
  then []
  else (
    let rec fold acc n =
      if UInt32.(equal zero n)
      then acc
      else (
        let index = UInt32.pred n in
        let child = select_child node index in
        match string_of_ts_node_type child with
        | "comment" | "ERROR" | "MISSING" -> fold acc index
        | _ -> fold (child :: acc) index)
    in
    fold [] (arity node))

let collect_named_children (node : ts_tree) : ts_forest =
  TS_fun.(collect ts_node_named_child ts_node_named_child_count node)

let collect_children (node : ts_tree) : ts_forest =
  TS_fun.(collect ts_node_child ts_node_child_count node)

let collect_error_children (node : ts_tree) : ts_forest =
  let children = collect_named_children node in
  let f child acc =
    match string_of_ts_node_type child with
    | "ERROR" -> child :: acc
    | _ -> acc
  in
  Core.List.fold_right ~f ~init:[] children

(* Extracting a named child by its index amongst its siblings that are
   not comment/error/missing nodes *)

let named_child_ranked index node =
  let raw_children = collect_named_children node in
  match Core.List.nth raw_children index with
  | None -> Error (sprintf "INVALID: Missing named child at index %i." index)
  | Some child -> Ok child

let named_child_ranked_opt index node =
  let raw_children = collect_named_children node in
  Core.List.nth raw_children index

(* Extracting a child by its index *)

let child_ranked index (node : ts_tree) =
  let raw_children = collect_children node in
  match Core.List.nth raw_children index with
  | None -> Error (sprintf "INVALID: Missing child at index %i" index)
  | Some child -> Ok child

let child_ranked_opt index (node : ts_tree) =
  let raw_children = collect_children node in
  Core.List.nth raw_children index

(* Extracting the name of a node *)

let get_name = string_of_ts_node_type

let get_name_res = function
  | Ok node -> get_name node
  | Error name -> name

(* Getting the sibling of a node (if any) *)

let rec next_sibling_opt (node : ts_tree) =
  if TS_fun.ts_node_is_null node
  then None
  else (
    let next = TS_fun.ts_node_next_sibling node in
    match get_name next with
    | "comment" | "ERROR" | "MISSING " -> next_sibling_opt next
    | _ -> Some next)

let rec prev_sibling_opt (node : ts_tree) =
  if TS_fun.ts_node_is_null node
  then None
  else (
    let prev = TS_fun.ts_node_prev_sibling node in
    match get_name prev with
    | "comment" | "ERROR" | "MISSING " -> prev_sibling_opt prev
    | _ -> Some prev)

let next_sibling (node : ts_tree) = opt_to_res @@ next_sibling_opt node
let prev_sibling (node : ts_tree) = opt_to_res @@ prev_sibling_opt node

let next_sibling_opt' (node : ts_tree) =
  let rec aux comments node =
    if TS_fun.ts_node_is_null node
    then None (* Drop comments *)
    else (
      let next = TS_fun.ts_node_next_sibling node in
      match get_name next with
      | "comment" -> aux (next :: comments) next (* Accumulate comments *)
      | "ERROR" | "MISSING " -> aux [] next (* Skip error/missing, drop comments *)
      | _ -> Some (List.rev comments, next))
    (* Return comments *)
  in
  aux [] node (* No comments to start with *)

(* Filtering by name a list of nodes *)

let filter_by_name name nodes =
  let f = String.equal name <@ string_of_ts_node_type in
  Core.List.filter nodes ~f

let filter_first_by_name_opt name nodes =
  match filter_by_name name nodes with
  | node :: _ -> Some node
  | [] -> None

let filter_first_by_name name nodes =
  match filter_first_by_name_opt name nodes with
  | None -> Error (sprintf "INVALID: Name %S missing" name)
  | Some node -> Ok node

let first_child_named_opt name node =
  filter_first_by_name_opt name @@ collect_children node

let first_child_named name node = filter_first_by_name name @@ collect_children node
let children_named name node = filter_by_name name @@ collect_children node

(* Arity *)

let arity node = UInt32.to_int (TS_fun.ts_node_child_count node)

(* Source locations *)

type range = ts_point * ts_point (* NOT [ts_range] *)

let string_of_point (point : ts_point) : string =
  let row = getf point TS_types.row
  and column = getf point TS_types.column in
  let row_string = UInt.to_string row
  and column_string = UInt.to_string column in
  sprintf "[%s, %s]" row_string column_string

let pos_of_point file map (point : ts_point) : Pos.t =
  let row = getf point TS_types.row
  and column = getf point TS_types.column in
  let line = UInt.to_int row
  and column = UInt.to_int column in
  match Loc_map.convert file map line column with
  | None -> Pos.ghost
  | Some position ->
    let point_num = position.Lexing.pos_cnum
    and point_bol = position.Lexing.pos_bol in
    Pos.make ~byte:position ~point_num ~point_bol

let range (node : ts_tree) : range =
  TS_fun.(ts_node_start_point node, ts_node_end_point node)

let string_of_range (range : range) : string =
  let start_point, end_point = range in
  let start_string = string_of_point start_point
  and end_string = string_of_point end_point in
  sprintf "%s - %s" start_string end_string

let region_of_range file map (range : range) : Region.t =
  let start_point, end_point = range in
  let start = pos_of_point file map start_point
  and stop = pos_of_point file map end_point in
  Region.make ~start ~stop

(*
let get_label (node : ts_tree) : string =
  let name = string_of_ts_node_type node
  and range_string = string_of_range @@ range node in
  sprintf "%s %s" name range_string
*)

let get_region file map (node : ts_tree) : Region.t =
  region_of_range file map @@ range node
