(* This module provides a translation from the TypeScript abstract
   grammar, as provided in the JSON file node-types.json by
   tree-sitter, to an OCaml equivalent.

   Run with:

   $ dune build Static_nodes_reader.exe
   $ <git path>/_build/default/lib/typescript_ast/Static_nodes_reader.exe node-types.json | less

   NOTE: The conversion from JSON could perhaps be derived by means of
   ppx_yojson_conv.
*)

(* Dependencies *)

open Core
module Tree = Cst_shared.Tree
module J = Yojson.Basic.Util

(* The abstract grammar (made of rules) *)

type json = Yojson.Basic.t

type type_named =
  { type_ : string
  ; named : bool
  }

type subtype_rule =
  { type_ : string
  ; named : bool
  ; subtypes : type_named Nonempty_list.t
  }

type children =
  { multiple : bool
  ; required : bool
  ; types : type_named Nonempty_list.t
  }

type key = string

type prod_rule =
  { type_ : string
  ; named : bool
  ; fields : (key * children) list
  ; children : children option
  }

type rule =
  | Union of subtype_rule
  | Prod of prod_rule

type abs_gram = rule list

(* Constructing the abstract grammar (productions/rules) from JSON *)

type error = string

let empty_member (member : string) (json : json) =
  let msg = Printf.sprintf "The %S member is empty." member in
  raise (J.Type_error (msg, json))

let mk_types first more =
  let types = Nonempty_list.(first :: more) in
  let f json : type_named =
    let type_ = J.(member "type" json |> to_string)
    and named = J.(member "named" json |> to_bool) in
    { type_; named }
  in
  Nonempty_list.map ~f types

let mk_children json : children =
  let multiple = J.(member "multiple" json |> to_bool)
  and required = J.(member "required" json |> to_bool) in
  let types =
    match J.(member "types" json |> to_list) with
    | [] -> empty_member "types" json
    | first :: more -> mk_types first more
  in
  { multiple; required; types }

let read_prod_rule type_ named json : rule =
  let mk_field (key, json) = key, mk_children json in
  let mk_fields = List.map ~f:mk_field in
  let fields =
    try J.(member "fields" json |> to_assoc |> mk_fields) with
    | J.Type_error _ -> []
  and children = J.(member "children" json |> to_option mk_children) in
  Prod { type_; named; fields; children }

let mk_subtype type_ named subtypes : rule =
  match J.to_list subtypes with
  | [] -> empty_member "subtypes" subtypes
  | subtype :: subtypes -> Union { type_; named; subtypes = mk_types subtype subtypes }

let read_abs_gram (path : string) : (abs_gram, error * json) Result.t =
  try
    let list = Yojson.Basic.from_file path |> J.to_list in
    let mk_rule json : rule =
      let type_ = J.(member "type" json |> to_string)
      and named = J.(member "named" json |> to_bool) in
      match J.member "subtypes" json with
      | `Null -> read_prod_rule type_ named json
      | subtypes -> mk_subtype type_ named subtypes
    in
    Ok (List.map ~f:mk_rule list)
  with
  | J.Type_error (msg, json) -> Error (msg, json)
  | Yojson.Json_error msg -> Error (msg, `Null)

(* Printing TS abstract grammar *)

let print_type state type_ = Tree.(make_unary state "type" make_node type_)
let print_bool state bool = Tree.make_node state (string_of_bool bool)
let print_named state named = Tree.make_unary state "named" print_bool named

let print_type_named state type_named =
  let ({ type_; named } : type_named) = type_named in
  let children = Tree.[ mk_child print_type type_; mk_child print_named named ] in
  Tree.make state "┐" children

let print_types_or_sub state root subtypes =
  let children =
    Nonempty_list.to_list subtypes |> List.map ~f:(Tree.mk_child print_type_named)
  in
  Tree.make state root children

let print_subtypes state = print_types_or_sub state "subtypes"

let print_union state rule =
  let { type_; named; subtypes } = rule in
  let children =
    Tree.
      [ mk_child print_type type_
      ; mk_child print_named named
      ; mk_child print_subtypes subtypes
      ]
  in
  Tree.make state "Union" children

let print_multiple state multiple = Tree.make_unary state "multiple" print_bool multiple
let print_required state required = Tree.make_unary state "require" print_bool required
let print_types state = print_types_or_sub state "types"

let print_field state (key, children) =
  let { multiple; required; types } = children in
  let children =
    Tree.
      [ mk_child print_multiple multiple
      ; mk_child print_required required
      ; mk_child print_types types
      ]
  in
  Tree.make state key children

let print_fields state fields =
  let children = List.map ~f:(Tree.mk_child print_field) fields in
  Tree.make state "fields" children

let print_children state children = print_field state ("children", children)

let print_prod state rule =
  let { type_; named; fields; children } = rule in
  let children' =
    Tree.
      [ mk_child print_type type_
      ; mk_child print_named named
      ; mk_child print_fields fields
      ; mk_child_opt print_children children
      ]
  in
  Tree.make state "Prod" children'

let print_rule state = function
  | Union rule -> print_union state rule
  | Prod rule -> print_prod state rule

let print_abs_gram state abs_gram = Tree.of_list state "<abs_gram>" print_rule abs_gram

let print_abs_gram_to_buffer state abs_gram : Buffer.t =
  print_abs_gram state abs_gram;
  Tree.to_buffer state

let print_abs_gram_to_string state abs_gram : string =
  Buffer.contents (print_abs_gram_to_buffer state abs_gram)

let () =
  match read_abs_gram "node-types.json" with
  | Ok abs_gram ->
    let buffer = Buffer.create 2000 in
    let state = Tree.mk_state ~buffer ~regions:false ~layout:true ~offsets:true `Point in
    let abs_gram = print_abs_gram_to_string state abs_gram in
    Printf.printf "%s\n%!" abs_gram
  | Error (msg, json) ->
    let str = Yojson.Basic.pretty_to_string json in
    Printf.eprintf "Error: %s.\n%s\n%!" msg str
