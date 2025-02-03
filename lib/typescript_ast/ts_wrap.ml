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

let is_null = TS_fun.ts_node_is_null
let node_to_opt node = if is_null node then None else Some node

(* Converting a node to an OCaml string ("NULL" is null) *)

let get_name (node : ts_tree) : string =
  if is_null node then "NULL" else string_of_char_ptr @@ TS_fun.ts_node_type node

let get_name_res = function
  | Ok node -> get_name node
  | Error name -> name

(* Wrappers for filtering fields (failure on null node or optional value) *)

let child_with_field ?(debug = true) ?get_region ?msg field node =
  let child = TS_fun.ts_node_child_by_field_name node field (uint32_len field) in
  let region =
    match get_region with
    | None -> ""
    | Some get_region ->
      let region = !get_region child in
      if Region.is_empty region then "" else " (" ^ region#compact `Byte ^ ")"
  in
  if is_null child || Core.String.is_empty region
  then (
    let name = get_name node in
    let default = sprintf "Node %S%s is missing the field %S." name region field in
    let msg' =
      match debug with
      | true -> default
      | false ->
        (match msg with
        | None -> default
        | Some msg -> sprintf "%s%s" msg region)
    in
    Error ("ERROR: " ^ msg'))
  else Result.Ok child

let child_with_field_opt field node =
  node_to_opt @@ TS_fun.ts_node_child_by_field_name node field (uint32_len field)

(* Printing the tree (debug) *)

let print_node (node : ts_tree) : unit =
  let ptr_char = TS_fun.ts_node_string node in
  Printf.printf "%s\n%!" @@ string_of_char_ptr ptr_char

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

let collect ?(comments = false) select_child arity node =
  if is_null node
  then []
  else (
    let rec fold acc n =
      if UInt32.(equal zero n)
      then acc
      else (
        let index = UInt32.pred n in
        let child = select_child node index in
        match get_name child with
        | "comment" when not comments -> fold acc index
        | _ -> fold (child :: acc) index)
    in
    fold [] (arity node))

let collect_named_children ?(comments = false) (node : ts_tree) : ts_forest =
  TS_fun.(collect ~comments ts_node_named_child ts_node_named_child_count node)

let collect_children ?(comments = false) (node : ts_tree) : ts_forest =
  TS_fun.(collect ~comments ts_node_child ts_node_child_count node)

(* Extracting a named child by its index amongst its siblings that are
   not comment/error/missing nodes *)

let named_child_ranked ?(get_region : (ts_tree -> Region.t) ref option) index node =
  let raw_children = collect_named_children node in
  match Core.List.nth raw_children index with
  | None ->
    let name = get_name node in
    let region =
      match get_region with
      | None -> ""
      | Some get_region -> " (" ^ (!get_region node)#compact `Byte ^ ")"
    in
    Error
      (sprintf "INTERNAL: Node %S%s has no named child at index %i." name region index)
  | Some child -> Ok child

let named_child_ranked_opt index node =
  let raw_children = collect_named_children node in
  Core.List.nth raw_children index

(* Extracting a child by its index *)

let child_ranked ?(get_region : (ts_tree -> Region.t) ref option) index (node : ts_tree) =
  let raw_children = collect_children node in
  match Core.List.nth raw_children index with
  | None ->
    let name = get_name node in
    let region =
      match get_region with
      | None -> ""
      | Some get_region -> " (" ^ (!get_region node)#compact `Byte ^ ")"
    in
    Error (sprintf "INTERNAL: Node %S%s has no child at index %i" name region index)
  | Some child -> Ok child

let child_ranked_opt index (node : ts_tree) =
  let raw_children = collect_children node in
  Core.List.nth raw_children index

(* Getting the sibling of a node (if any) *)

let sibling_opt get_sibling (node : ts_tree) : ts_tree option =
  if is_null node then None else node_to_opt (get_sibling node)

let next_sibling_opt (node : ts_tree) : ts_tree option =
  sibling_opt TS_fun.ts_node_next_sibling node

let prev_sibling_opt (node : ts_tree) : ts_tree option =
  sibling_opt TS_fun.ts_node_prev_sibling node

let next_sibling (node : ts_tree) : (ts_tree, string) result =
  match next_sibling_opt node with
  | Some sibling -> Ok sibling
  | None -> Error (sprintf "ERROR: Node %S has no next sibling." (get_name node))

let prev_sibling (node : ts_tree) : (ts_tree, string) result =
  match prev_sibling_opt node with
  | Some sibling -> Ok sibling
  | None -> Error (sprintf "ERROR: Node %S has no previous sibling." (get_name node))

let next_sibling_res (node : (ts_tree, string) result) : (ts_tree, string) result =
  Core.Result.bind node ~f:next_sibling

let prev_sibling_res (node : (ts_tree, string) result) : (ts_tree, string) result =
  Core.Result.bind node ~f:prev_sibling

(* Getting the comments immediately to the left of a given node *)

let prev_comments (node : ts_tree) : ts_forest =
  let rec aux comments node =
    let prev = TS_fun.ts_node_prev_sibling node in
    if is_null prev
    then comments
    else (
      match get_name prev with
      | "comment" -> aux (prev :: comments) prev
      | _ -> comments)
  in
  if is_null node then [] else aux [] node

(* Filtering by name a list of nodes *)

let filter_by_name name nodes =
  let f = String.equal name <@ get_name in
  Core.List.filter nodes ~f

let filter_first_by_name_opt name nodes =
  match filter_by_name name nodes with
  | node :: _ -> Some node
  | [] -> None

let filter_first_by_name name nodes =
  match filter_first_by_name_opt name nodes with
  | None -> Error (sprintf "INTERNAL: Name %S missing" name)
  | Some node -> Ok node

let first_child_named_opt name node =
  filter_first_by_name_opt name @@ collect_children node

let first_child_named name node = filter_first_by_name name @@ collect_children node
let children_named name node = filter_by_name name @@ collect_children node

(* Arity *)

let arity node = UInt32.to_int (TS_fun.ts_node_child_count node)
let last_child node = child_ranked (arity node - 1) node

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
  let name = get_name node
  and range_string = string_of_range @@ range node in
  sprintf "%s %s" name range_string
*)

let get_region file map (node : ts_tree) : Region.t =
  region_of_range file map @@ range node
