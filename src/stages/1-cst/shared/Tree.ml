(* This module is a DSL for building textual representations of
   Catalan trees (general trees) *)

(* Vendor dependencies *)

module Region   = Simple_utils.Region
module Utils    = Simple_utils.Utils
module Ne       = Nonempty_list
module Ligo_fun = Simple_utils.Ligo_fun

(* Utilities *)

let (<@) = Ligo_fun.(<@)

(* Local dependencies *)

module Wrap = Lexing_shared.Wrap

(* Utilities *)

let sprintf = Printf.sprintf

(* STATE *)

(* The printing of the CST makes use of a threaded data structure: the
   _state_. The printing is done to the string buffer bound to the
   field [buffer], which is imperatively updated (see module
   [Buffer].) The method [pad] updates the current padding, which is
   comprised of two components: the padding to reach the new node
   (space before reaching a subtree, then a vertical bar for it) and
   the padding for the new node itself. (Is it the last child of its
   parent?) *)

type state = <
  regions  : bool;
  layout   : bool;
  offsets  : bool;
  mode     : [`Point | `Byte];
  buffer   : Buffer.t;
  pad_path : string;
  pad_node : string;
  pad      : int -> int -> state
>

let mk_state ?(buffer = Buffer.create 131) ~regions ~layout ~offsets mode =
  object (self)
    method regions  = regions
    method layout   = layout
    method offsets  = offsets
    method mode     = mode
    method buffer   = buffer
    val pad_path    = ""
    method pad_path = pad_path
    val pad_node    = ""
    method pad_node = pad_node

    method pad arity rank =
      if layout then
        {< pad_path = pad_node ^ if rank = arity - 1 then "└ " else "├ ";
           pad_node = pad_node ^ if rank = arity - 1 then "  " else "| ">}
      else self
  end

let to_buffer (state: state) = state#buffer

(* ROOTS (labels at the root node) *)

type root = string

(* PRINTERS *)

type 'a printer = state -> 'a -> unit

(* PRINTING NODES (trees without children) *)

let compact state (region : Region.t) =
  if state#regions then
    region#compact ~offsets:state#offsets state#mode |> sprintf " (%s)"
  else ""

let make_node ?region state root =
  let node =
    match region with
      None -> sprintf "%s%s\n" state#pad_path root
    | Some region ->
        let region = compact state region in
        sprintf "%s%s%s\n" state#pad_path root region
  in Buffer.add_string state#buffer node

let make_literal state (wrap : string Wrap.t) =
  make_node ~region:wrap#region state wrap#payload

(* PRINTING GENERAL TREES *)

type child = (state -> unit) option

let make_forest state children = (* DO NOT EXPORT *)
  let children     = List.filter_map ~f:(fun x -> x) children in
  let arity        = List.length children in
  let f rank print = print (state#pad arity rank)
  in List.iteri ~f children

let make_tree ?region state root children =
  make_node   state ?region root;
  make_forest state children

let make = make_tree

(* MAKING SUBTREES (children) *)

let mk_child print child = Some (fun state -> print state child)

let mk_child_opt print = function
  None       -> None
| Some value -> mk_child print value

let mk_children_list print ?root = function
  []   -> []
| list ->
    let children = List.map ~f:(mk_child print) list in
    match root with
      None      -> children
    | Some root -> [Some (fun state -> make_tree state root children)]

let mk_children_nsepseq print ?root =
  mk_children_list print ?root <@ Utils.nsepseq_to_list

let mk_children_nsepseq_opt print ?root = function
  None -> []
| Some value -> mk_children_nsepseq print ?root value

let mk_children_sepseq print ?root =
  mk_children_list print ?root <@ Utils.sepseq_to_list

let mk_children_ne_list print ?root =
  mk_children_list print ?root <@ Ne.to_list

let mk_children_sep_or_term print ?root =
  mk_children_list print ?root <@ Utils.sep_or_term_to_list

let mk_children_nsep_or_term print ?root =
  mk_children_list print ?root <@ Utils.nsep_or_term_to_list

let mk_children_nsep_or_pref print ?root =
  mk_children_list print ?root <@ Utils.nsep_or_pref_to_list

(* PRINTING UNARY TREES *)

let make_unary ?region state root print node =
  make_tree state root ?region [mk_child print node]

(* PRINTING LISTS AND SEQUENCES *)

let of_list ?region state root print list =
  let children = List.map ~f:(mk_child print) list
  in make_tree ?region state root children

let of_nsepseq ?region state root print =
  of_list ?region state root print <@ Utils.nsepseq_to_list

let of_sepseq ?region state root print =
  of_list ?region state root print <@ Utils.sepseq_to_list

let of_ne_list ?region state root print =
  of_list ?region state root print <@ Ne.to_list

let of_nsep_or_term ?region state root print = function
  `Sep s -> of_nsepseq ?region state root print s
| `Term Ne.(hd :: tl) ->
     of_list ?region state root print @@ List.map ~f:fst (hd::tl)

let of_sep_or_term ?region state root print = function
  None   -> make_node ?region state root
| Some s -> of_nsep_or_term ?region state root print s

let of_nsep_or_pref ?region state root print = function
  `Sep s -> of_nsepseq ?region state root print s
| `Pref Ne.(hd :: tl) ->
     of_list ?region state root print @@ List.map ~f:snd (hd::tl)

(* PRINTING LEAVES *)

type lexeme = string

(* Strings *)

let make_string state (wrap : string Wrap.t) : unit =
  let region = compact state wrap#region in
  let node   = sprintf "%s%S%s\n" state#pad_path wrap#payload region
  in Buffer.add_string state#buffer node

let make_string root state (wrap : string Wrap.t) =
  make_unary state root make_string wrap

(* Verbatim strings *)

let make_verbatim state (wrap : string Wrap.t) : unit =
  let region = compact state wrap#region in
  let node   = sprintf "%s{|%s|}%s\n" state#pad_path wrap#payload region
  in Buffer.add_string state#buffer node

let make_verbatim root state (wrap : string Wrap.t) =
  make_unary state root make_verbatim wrap

(* Numbers *)

let make_num to_string root state (wrap : 'a Wrap.t) =
  let lexeme, num = wrap#payload in
  let children = [
    mk_child (make_node ~region:wrap#region) lexeme;
    mk_child make_node                       (to_string num)]
  in make_tree state root children

let make_int   = make_num Z.to_string
let make_nat   = make_int
let make_bytes = make_num Hex.show
let make_mutez = make_num Int64.to_string
let make_tez   = make_num Q.to_string
