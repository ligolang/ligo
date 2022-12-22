(* This module is a DSL for building textual representations of
   Catalan trees (general trees) *)

(* Vendor dependencies *)

module Region = Simple_utils.Region

(* Local dependencies *)

module Wrap = Lexing_shared.Wrap

(* Utilities *)

let sprintf = Printf.sprintf

(* STATE *)

(* The printing of the CST makes use of a threaded data structure: the
   _state_. The printing is done to the string buffer bound to the
   field [buffer], which is imperatively updated (see module
   [Stdlib.Buffer].) The method [pad] updates the current padding,
   which is comprised of two components: the padding to reach the new
   node (space before reaching a subtree, then a vertical bar for it)
   and the padding for the new node itself. (Is it the last child of
   its parent?) *)

type state = <
  offsets  : bool;
  mode     : [ `Point | `Byte ];
  buffer   : Buffer.t;
  pad_path : string;
  pad_node : string;
  pad      : int -> int -> state
>

let mk_state ?(buffer = Buffer.create 131) ~offsets mode =
  object
    method offsets  = offsets
    method mode     = mode
    method buffer   = buffer
    val pad_path    = ""
    method pad_path = pad_path
    val pad_node    = ""
    method pad_node = pad_node

    method pad arity rank =
      {<pad_path = pad_node ^ if rank = arity - 1 then "└ " else "├ ";
        pad_node = pad_node ^ if rank = arity - 1 then "  " else "| ">}
  end

(* Printing nodes *)

type 'a printer = state -> 'a -> unit

let compact state (region : Region.t) =
  region#compact ~offsets:state#offsets state#mode

let print_node ?region state label =
  let node =
    match region with
      None -> sprintf "%s%s\n" state#pad_path label
    | Some region ->
        let region = compact state region in
        sprintf "%s%s (%s)\n" state#pad_path label region
  in Buffer.add_string state#buffer node

let print_literal state (wrap : string Wrap.t) =
  print_node ~region:wrap#region state wrap#payload

let print_literal_wo_reg state (wrap : string Wrap.t) =
  print_node state wrap#payload

(* Making subtrees (children) from general values ([mk_child]),
   optional values ([mk_child_opt]) or list values
   ([mk_child_list]). The type of a subtree ("child") is a ['a
   option], with the interpretation that [None] means "no subtree
   printed". In the case of a list, the empty list is interpreted as
   meaning "no subtree printed." *)

type child = (state -> unit) option

let mk_child print child = Some (fun state -> print state child)

let mk_child_opt print = function
  None -> None
| Some value -> mk_child print value

let mk_child_list print_list = function
  [] -> None
| l -> mk_child print_list l

(* Printing trees (root + subtrees). The call [print_tree state label
   ?region children] prints a node whose label is [label] and optional
   region is [region], and whose subtrees are [children]. The latter
   is a list of optional values, with the interpretation of [None] as
   meaning "no subtree printed". *)

type label = string

let print_tree ?region state label children =
  let () = print_node state ?region label in
  let children = List.filter_map ~f:(fun x -> x) children in
  let arity = List.length children in
  let f rank print = print (state#pad arity rank)
  in List.iteri ~f children

let print = print_tree

(* A special case of tree occurs often: the unary tree, that is, a
   tree with exactly one subtree. *)

let print_unary ?region state label print_sub node =
  print_tree state label ?region [mk_child print_sub node]

(* PRINTING LEAVES *)

type lexeme = string

(* Strings *)

let print_string state (wrap : string Wrap.t) =
  let region = compact state wrap#region in
  let node   = sprintf "%s%S (%s)\n" state#pad_path wrap#payload region
  in Buffer.add_string state#buffer node

(* Verbatim strings *)

let print_verbatim state (wrap : string Wrap.t) =
  let region = compact state wrap#region in
  let node   = sprintf "%s{|%s|} (%s)\n" state#pad_path wrap#payload region
  in Buffer.add_string state#buffer node

(* Numbers *)

let print_num to_string label state (wrap : 'a Wrap.t) =
  let lexeme, num = wrap#payload in
  let children = [
    mk_child (print_node ~region:wrap#region) lexeme;
    mk_child print_node (to_string num)
  ]
  in print_tree state label children

let print_int   = print_num Z.to_string
let print_nat   = print_int
let print_bytes = print_num Hex.show
let print_mutez = print_num Int64.to_string
