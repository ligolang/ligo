(* PRINTING THE CST *)

[@@@coverage exclude_file]

(* Vendor dependencies *)

module Directive = Preprocessor.Directive
module Utils     = Simple_utils.Utils
module Region    = Simple_utils.Region

(* Internal dependencies *)

module Attr = Lexing_shared.Attr
module Tree = Cst_shared.Tree

(*
type state = Tree.state
type label = Tree.label

open CST (* THE ONLY GLOBAL OPENING *)
*)

(* UTILITIES *)

(*type ('a, 'sep) nsepseq = ('a, 'sep) Utils.nsepseq*)

(* Higher-order printers *)

(*
let print_list :
  state -> ?region:Region.t -> label -> 'a Tree.printer -> 'a list -> unit =
  fun state ?region label print list ->
    let children = List.map ~f:(Tree.mk_child print) list
    in Tree.print ?region state label children
*)

(*let print_nsepseq state ?region label print node =
  print_list state ?region label print (Utils.nsepseq_to_list node)

let print_sepseq state ?region label print node =
  print_list state ?region label print (Utils.sepseq_to_list node)

let print_nseq state ?region label print node =
  print_list state ?region label print (Utils.nseq_to_list node)

let print_attribute state (node : Attr.t reg) =
  let key, val_opt = node.value in
  match val_opt with
    None ->
      Tree.print_unary state "<attribute>" Tree.print_node key
  | Some String value ->
      let children = [
        Tree.mk_child Tree.print_node key;
        Tree.mk_child Tree.print_node value]
      in Tree.print state "<attributes>" children

let print_attributes state (node : Attr.attribute reg list) =
  print_list state "<attributes>" print_attribute node
*)

(* Preprocessing directives *)
(*
let print_D_Directive state (node : Directive.t) =
  let region, string = Directive.project node in
  Tree.print_unary state "D_Directive" Tree.print_node ~region string
*)
(* PRINTING THE CST *)

(* The names of the printing functions are all prefixed by
   "print_". The rest of the name is either

     * the name of the type whose value is printed, for example
       [print_declaration] prints values of type [declaration]; it can
       also be the type in a region, like [val print_variant : state
       -> variant reg -> unit], instead of [val print_variant : state
       -> variant -> unit];

     * the name of a CST constructor, for example, [print_E_Int],
       meaning that the argument of the constructor [CST.E_Int] is
       printed.

   Another design pattern we used here was to make all pattern
   matching on CST constructors a simple routing function, that is,
   devoid of logic. For example:

   and print_type_expr state = function
     T_App     t -> print_T_App     state t
   | T_Cart    t -> print_T_Cart    state t
   ...

   This means that those functions can be ignored by the maintainers
   if they know the data constructor.

   Guideline: When destructuring a value [v] of type [Region.t], use
   the following order: [let {value; region} = v in ...].

   The higher-order printers take as first argument the printer for
   the subtree(s), instead of the state first. For example,

   [val print_case_clause :
     'a.'a Tree.printer -> label -> state -> 'a case_clause reg -> unit]

   except for the printers from [Tree], like [Tree.print_unary], which
   are always qualified. *)

type ('src, 'dst) printer = Tree.state -> 'src -> 'dst

let print_to_buffer _ _ = Buffer.create 10
let print_to_string _ _ = ""
let print_pattern_to_string _ _ = ""

let to_buffer : (CST.t,     Buffer.t) printer = print_to_buffer
let to_string : (CST.t,       string) printer = print_to_string
let pattern_to_string : (CST.pattern, string) printer = print_pattern_to_string
