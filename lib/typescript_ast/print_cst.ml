(* Printing the tree-sitter CST for TypeScript *)

open Core

[@@@warning "-32"]

let debug = false

(* Dependencies and scopes *)

open Typescript_ast.Ts_wrap
module Lexeme = Typescript_ast.Lexeme
module Ts_wrap = Typescript_ast.Ts_wrap
module Loc_map = Typescript_ast.Loc_map
module Syntax_err = Typescript_ast.Syntax_err
module Ast = Typescript_ast.Ast (* Only for numbers *)
module Number = Typescript_ast.Number
module Wrap = Lexing_shared.Wrap

(* Monadic let-binder for result values *)

let ( let* ) v f = Result.bind v ~f

(* Source map for converting vertical and horizontal offset ranges
   into regions *)

let get_region : (ts_tree -> Region.t) ref =
  ref (fun _ -> failwith "Internal error: Print_cst.get_region")

(* The input source (default: a hundred lines) *)

let input : Buffer.t ref = ref (Buffer.create (80 * 100))

(* Tayloring the fetching of a field, with an error message in case of
   failure. *)

let child_with_field ~err field node =
  match Ts_wrap.child_with_field ~get_region field node with
  | Ok child -> Ok child
  | Error () ->
    let region = !get_region node in
    let region =
      if Region.is_empty region then "" else " (" ^ region#compact `Byte ^ ")"
    in
    let msg =
      if debug
      then (
        let name = get_name node in
        if String.equal name "NULL"
        then sprintf "ERROR: NULL parent of field %S." field
        else sprintf "ERROR: Node %S%s is missing the field %S." name region field)
      else sprintf "ERROR: %s%s" (Syntax_err.to_string err) region
    in
    Error msg

(* Tayloring the fetching of a node by name, with an error message in
   case of failure *)

let first_child_named name node ~err =
  let region = " (" ^ (!get_region node)#compact `Byte ^ ")" in
  let msg = "ERROR: " ^ Syntax_err.to_string err ^ region in
  Ts_wrap.first_child_named name node ~msg

(* Partially evaluating wrappers so they print regions in case of
   error (shadowing) *)

let child_ranked index node ~err =
  let region = " (" ^ (!get_region node)#compact `Byte ^ ")" in
  let msg = "ERROR: " ^ Syntax_err.to_string err ^ region in
  Ts_wrap.child_ranked index node ~msg

let named_child_ranked index node ~err =
  let region = " (" ^ (!get_region node)#compact `Byte ^ ")" in
  let msg = "ERROR: " ^ Syntax_err.to_string err ^ region in
  Ts_wrap.named_child_ranked index node ~msg

let last_child node ~err =
  let region = " (" ^ (!get_region node)#compact `Byte ^ ")" in
  let msg = "ERROR: " ^ Syntax_err.to_string err ^ region in
  Ts_wrap.last_child node ~msg

(* To print the AST in ASCII art *)

module Tree = Cst_shared.Tree

(* Making trees and nodes with labels (name + location) *)

let mk_child_opt = Tree.mk_child_opt
let mk_child = Tree.mk_child
let mk_children_list = Tree.mk_children_list

let make_unary state root printer child =
  let region = !get_region root
  and label = get_name root in
  Tree.make_unary ~region state label printer child

let make_node state node =
  let region = !get_region node in
  let lexeme = Lexeme.read !input region in
  make_unary state node Tree.make_node lexeme

let print_comment state node = make_node state node

let print_error_node state node ~err =
  let region = !get_region node
  and msg =
    if debug
    then sprintf "ERROR: Unexpected node %S." (get_name node)
    else sprintf "ERROR: %s" (Syntax_err.to_string err)
  in
  if arity node = 0
  then Tree.make_node ~region state msg
  else Tree.make_unary ~region state msg Tree.make_node "UNMATCHED children."

let mk_error_child node ~err =
  let region = !get_region node
  and msg =
    if debug
    then sprintf "ERROR: Unexpected node %S." (get_name node)
    else sprintf "ERROR: %s" (Syntax_err.to_string err)
  in
  if arity node = 0
  then Some (fun state -> Tree.make_node ~region state msg)
  else
    Some
      (fun state ->
        Tree.make_unary ~region state msg Tree.make_node "UNMATCHED children.")

let make_tree state node children =
  let region = !get_region node
  and label = get_name node in
  Tree.make ~region state label children

let tree_of_list ?(comments = []) state node printer raw_children =
  let f raw_child nodes = mk_child (printer ?comments:None) raw_child :: nodes in
  let children =
    match raw_children with
    | [] -> []
    | fst_raw_child :: siblings ->
      let printer = printer ?comments:(Some comments) in
      let fst_child = mk_child printer fst_raw_child in
      fst_child :: List.fold_right ~f ~init:[] siblings
  in
  make_tree state node children

let tree_of_named_children ?(comments = []) state node printer =
  let raw_children = collect_named_children node in
  tree_of_list ~comments state node printer raw_children

(* We shadow [make_node] above *)

let make_node ?(comments = []) state node =
  let region = !get_region node in
  let lexeme = Lexeme.read !input region in
  let comments = comments @ prev_comments node in
  let children =
    mk_children_list print_comment comments @ [ mk_child Tree.make_node lexeme ]
  in
  make_tree state node children

(* Keywords *)

let make_kwd ?(comments = []) state node ~err =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> print_error_node state node ~err
  | _ ->
    let region = !get_region node in
    let root = Lexeme.read !input region ^ " [keyword]" in
    let comments = comments @ prev_comments node in
    Tree.of_list ~region state root print_comment comments

let mk_kwd_infer = make_kwd ~err:Syntax_err.Infer
let mk_kwd_keyof = make_kwd ~err:Syntax_err.Keyof
let mk_kwd_meta = make_kwd ~err:Syntax_err.Meta
let mk_kwd_target = make_kwd ~err:Syntax_err.Target
let mk_kwd_false = make_kwd ~err:Syntax_err.False
let mk_kwd_true = make_kwd ~err:Syntax_err.True
let mk_kwd_super = make_kwd ~err:Syntax_err.Super
let mk_kwd_null = make_kwd ~err:Syntax_err.Null
let mk_kwd_satisfies = make_kwd ~err:Syntax_err.Satisfies
let mk_kwd_yield = make_kwd ~err:Syntax_err.Yield
let mk_kwd_new = make_kwd ~err:Syntax_err.New
let mk_kwd_instanceof = make_kwd ~err:Syntax_err.Instanceof
let mk_kwd_implements = make_kwd ~err:Syntax_err.Implements
let mk_kwd_assert = make_kwd ~err:Syntax_err.Assert
let mk_kwd_as = make_kwd ~err:Syntax_err.As
let mk_kwd_async = make_kwd ~err:Syntax_err.Async
let mk_kwd_function = make_kwd ~err:Syntax_err.Function
let mk_kwd_override = make_kwd ~err:Syntax_err.Override
let mk_kwd_readonly = make_kwd ~err:Syntax_err.Readonly
let mk_kwd_public = make_kwd ~err:Syntax_err.Public
let mk_kwd_private = make_kwd ~err:Syntax_err.Private
let mk_kwd_protected = make_kwd ~err:Syntax_err.Protected
let mk_kwd_set = make_kwd ~err:Syntax_err.Set
let mk_kwd_get = make_kwd ~err:Syntax_err.Get
let mk_kwd_all = make_kwd ~err:Syntax_err.All
let mk_kwd_static = make_kwd ~err:Syntax_err.Static
let mk_kwd_this = make_kwd ~err:Syntax_err.This
let mk_kwd_is = make_kwd ~err:Syntax_err.Is
let mk_kwd_class = make_kwd ~err:Syntax_err.Class
let mk_kwd_const = make_kwd ~err:Syntax_err.Const
let mk_kwd_constraint = make_kwd ~err:Syntax_err.Constraint
let mk_kwd_let = make_kwd ~err:Syntax_err.Let
let mk_kwd_undefined = make_kwd ~err:Syntax_err.Undefined
let mk_kwd_abstract = make_kwd ~err:Syntax_err.Abstract
let mk_kwd_declare = make_kwd ~err:Syntax_err.Declare
let mk_kwd_accessor = make_kwd ~err:Syntax_err.Accessor
let mk_kwd_global = make_kwd ~err:Syntax_err.Global
let mk_kwd_module = make_kwd ~err:Syntax_err.Module
let mk_kwd_enum = make_kwd ~err:Syntax_err.Enum
let mk_kwd_import = make_kwd ~err:Syntax_err.Import
let mk_kwd_interface = make_kwd ~err:Syntax_err.Interface
let mk_kwd_extends = make_kwd ~err:Syntax_err.Extends
let mk_kwd_namespace = make_kwd ~err:Syntax_err.Namespace
let mk_kwd_type = make_kwd ~err:Syntax_err.Type
let mk_kwd_using = make_kwd ~err:Syntax_err.Using
let mk_kwd_return = make_kwd ~err:Syntax_err.Return
let mk_kwd_switch = make_kwd ~err:Syntax_err.Switch
let mk_kwd_case = make_kwd ~err:Syntax_err.Case
let mk_kwd_default = make_kwd ~err:Syntax_err.Default
let mk_kwd_throw = make_kwd ~err:Syntax_err.Throw
let mk_kwd_while = make_kwd ~err:Syntax_err.While
let mk_kwd_with = make_kwd ~err:Syntax_err.With
let mk_kwd_any = make_kwd ~err:Syntax_err.Any
let mk_kwd_number = make_kwd ~err:Syntax_err.Number
let mk_kwd_boolean = make_kwd ~err:Syntax_err.Boolean
let mk_kwd_string = make_kwd ~err:Syntax_err.String
let mk_kwd_symbol = make_kwd ~err:Syntax_err.Symbol
let mk_kwd_unique_symbol = make_kwd ~err:Syntax_err.Unique_symbol
let mk_kwd_void = make_kwd ~err:Syntax_err.Void
let mk_kwd_unknown = make_kwd ~err:Syntax_err.Unknown
let mk_kwd_never = make_kwd ~err:Syntax_err.Never
let mk_kwd_object = make_kwd ~err:Syntax_err.Object
let mk_kwd_asserts = make_kwd ~err:Syntax_err.Asserts
let mk_kwd_debugger = make_kwd ~err:Syntax_err.Debugger
let mk_kwd_break = make_kwd ~err:Syntax_err.Break
let mk_kwd_continue = make_kwd ~err:Syntax_err.Continue
let mk_kwd_do = make_kwd ~err:Syntax_err.Do
let mk_kwd_export = make_kwd ~err:Syntax_err.Export
let mk_kwd_for = make_kwd ~err:Syntax_err.For
let mk_kwd_from = make_kwd ~err:Syntax_err.From
let mk_kwd_await = make_kwd ~err:Syntax_err.Await
let mk_kwd_var = make_kwd ~err:Syntax_err.Var
let mk_kwd_in = make_kwd ~err:Syntax_err.In
let mk_kwd_of = make_kwd ~err:Syntax_err.Of
let mk_kwd_if = make_kwd ~err:Syntax_err.If
let mk_kwd_else = make_kwd ~err:Syntax_err.Else
let mk_kwd_typeof = make_kwd ~err:Syntax_err.Typeof
let mk_kwd_try = make_kwd ~err:Syntax_err.Try
let mk_kwd_catch = make_kwd ~err:Syntax_err.Catch
let mk_kwd_require = make_kwd ~err:Syntax_err.Require
let mk_kwd_delete = make_kwd ~err:Syntax_err.Delete
let mk_kwd_finally = make_kwd ~err:Syntax_err.Finally
let mk_kwd_instanceof = make_kwd ~err:Syntax_err.Instanceof

(* Symbols *)

let make_sym ?(comments = []) state node ~err =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> print_error_node state node ~err
  | _ ->
    let region = !get_region node in
    let root = Lexeme.read !input region in
    let comments = comments @ prev_comments node in
    Tree.of_list ~region state root print_comment comments

let mk_sym_asterisk = make_sym ~err:Syntax_err.Asterisk
let mk_sym_equal = make_sym ~err:Syntax_err.Equal
let mk_sym_lparen = make_sym ~err:Syntax_err.Left_parenthesis
let mk_sym_rparen = make_sym ~err:Syntax_err.Right_parenthesis
let mk_sym_qmark = make_sym ~err:Syntax_err.Question_mark
let mk_sym_plus_equal = make_sym ~err:Syntax_err.Plus_equal
let mk_sym_minus_equal = make_sym ~err:Syntax_err.Minus_equal
let mk_sym_mult_equal = make_sym ~err:Syntax_err.Mult_equal
let mk_sym_div_equal = make_sym ~err:Syntax_err.Div_equal
let mk_sym_rem_equal = make_sym ~err:Syntax_err.Rem_equal
let mk_sym_xor_equal = make_sym ~err:Syntax_err.Xor_equal
let mk_sym_and_equal = make_sym ~err:Syntax_err.And_equal
let mk_sym_or_equal = make_sym ~err:Syntax_err.Or_equal
let mk_sym_right_shift_equal = make_sym ~err:Syntax_err.Right_shift_equal
let mk_sym_increment = make_sym ~err:Syntax_err.Increment
let mk_sym_decrement = make_sym ~err:Syntax_err.Decrement
let mk_sym_lbrace = make_sym ~err:Syntax_err.Left_brace
let mk_sym_rbrace = make_sym ~err:Syntax_err.Right_brace
let mk_sym_lbracket = make_sym ~err:Syntax_err.Left_bracket
let mk_sym_rbracket = make_sym ~err:Syntax_err.Right_bracket
let mk_sym_optional_chain = make_sym ~err:Syntax_err.Optional_chain
let mk_sym_backquote = make_sym ~err:Syntax_err.Backquote
let mk_sym_colon = make_sym ~err:Syntax_err.Colon
let mk_sym_ellipsis = make_sym ~err:Syntax_err.Ellipsis
let mk_sym_arrow = make_sym ~err:Syntax_err.Arrow
let mk_sym_asterisk = make_sym ~err:Syntax_err.Asterisk
let mk_sym_qmark = make_sym ~err:Syntax_err.Question_mark
let mk_sym_emark = make_sym ~err:Syntax_err.Exclamation_mark
let mk_sym_dot = make_sym ~err:Syntax_err.Dot
let mk_sym_omitting = make_sym ~err:Syntax_err.Omitting_type_annotation
let mk_sym_adding = make_sym ~err:Syntax_err.Adding_type_annotation
let mk_sym_opting = make_sym ~err:Syntax_err.Opting_type_annotation
let mk_sym_ampersand = make_sym ~err:Syntax_err.Ampersand
let mk_sym_vbar = make_sym ~err:Syntax_err.Vertical_bar

let mk_sym_unsigned_right_shift_equal =
  make_sym ~err:Syntax_err.Unsigned_right_shift_equal

let mk_sym_left_shift_equal = make_sym ~err:Syntax_err.Left_shift_equal
let mk_sym_unsigned_left_shift_equal = make_sym ~err:Syntax_err.Unsigned_left_shift_equal
let mk_sym_exponent_equal = make_sym ~err:Syntax_err.Exponent_equal
let mk_sym_conjunction_equal = make_sym ~err:Syntax_err.Conjunction_equal
let mk_sym_disjunction_equal = make_sym ~err:Syntax_err.Disjunction_equal
let mk_sym_non_null_equal = make_sym ~err:Syntax_err.Non_null_equal
let mk_sym_bang = make_sym ~err:Syntax_err.Exclamation_mark
let mk_sym_tilde = make_sym ~err:Syntax_err.Tilde
let mk_sym_minus = make_sym ~err:Syntax_err.Minus
let mk_sym_plus = make_sym ~err:Syntax_err.Plus
let mk_sym_conjunction = make_sym ~err:Syntax_err.Conjunction
let mk_sym_disjunction = make_sym ~err:Syntax_err.Disjunction
let mk_sym_right_shift = make_sym ~err:Syntax_err.Right_shift
let mk_sym_unsigned_right_shift = make_sym ~err:Syntax_err.Unsigned_right_shift
let mk_sym_left_shift = make_sym ~err:Syntax_err.Left_shift
let mk_sym_unsigned_left_shift = make_sym ~err:Syntax_err.Unsigned_left_shift
let mk_sym_and = make_sym ~err:Syntax_err.And
let mk_sym_xor = make_sym ~err:Syntax_err.Xor
let mk_sym_or = make_sym ~err:Syntax_err.Or
let mk_sym_div = make_sym ~err:Syntax_err.Div
let mk_sym_rem = make_sym ~err:Syntax_err.Rem
let mk_sym_exponent = make_sym ~err:Syntax_err.Exponent
let mk_sym_lower_than = make_sym ~err:Syntax_err.Lower_than
let mk_sym_lower_than_or_equal = make_sym ~err:Syntax_err.Lower_than_or_equal
let mk_sym_no_conv_equal = make_sym ~err:Syntax_err.No_conv_equal
let mk_sym_different = make_sym ~err:Syntax_err.Different
let mk_sym_no_conv_different = make_sym ~err:Syntax_err.No_conv_different
let mk_sym_greater_than_or_equal = make_sym ~err:Syntax_err.Greater_than_or_equal
let mk_sym_greater_than = make_sym ~err:Syntax_err.Greater_than
let mk_sym_non_null = make_sym ~err:Syntax_err.Non_null

(* Making children and unary trees *)

let mk_child_res print = function
  | Result.Ok child -> mk_child print child
  | Error name -> mk_child Tree.make_node name

let make_unary_res state node print = function
  | Result.Ok child -> make_unary state node print child
  | Error child_name -> make_unary state node Tree.make_node child_name

(* Some literals *)

let print_identifier ?comments state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> print_error_node state node ~err:Syntax_err.Identifier
  | _ -> make_node ?comments state node

let print_string ?comments state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    print_error_node state node ~err:Syntax_err.String_literal
  | _ -> make_node ?comments state node

let print_regex ?comments state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> print_error_node state node ~err:Syntax_err.Regexp
  | _ -> make_node ?comments state node

let decode_comments ?(comments = []) node : Wrap.comment list =
  let f node =
    let region = !get_region node in
    let value = Lexeme.read !input region in
    Wrap.Block Region.{ value; region }
  in
  List.map ~f (comments @ prev_comments node)

let print_number ?(comments = []) state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    print_error_node state node ~err:Syntax_err.Number_literal
  | _ ->
    let region = !get_region node in
    let lexeme = Lexeme.read !input region in
    let lexbuf = Lexing.from_string lexeme in
    let w_comments = decode_comments ~comments node in
    (match Number.scan w_comments region lexbuf with
    | Ok num ->
      let print_hex w = Hex.show (snd w#payload) in
      let print_dec w = Q.to_string (snd w#payload) in
      let open Ast in
      let print_kind state = function
        | Hex (w, false) -> Tree.make_node state ("hex (" ^ print_hex w ^ ")")
        | Hex (w, true) -> Tree.make_node state ("bigint/hex (" ^ print_hex w ^ ")")
        | Bin (w, false) -> Tree.make_node state ("bin (" ^ print_hex w ^ ")")
        | Bin (w, true) -> Tree.make_node state ("bigint/bin (" ^ print_hex w ^ ")")
        | Oct (w, false) -> Tree.make_node state ("oct (" ^ print_hex w ^ ")")
        | Oct (w, true) -> Tree.make_node state ("bigint/oct (" ^ print_hex w ^ ")")
        | Dec (w, false) -> Tree.make_node state ("dec (" ^ print_dec w ^ ")")
        | Dec (w, true) -> Tree.make_node state ("bigint/dec (" ^ print_dec w ^ ")")
      in
      let comments = comments @ prev_comments node in
      let children =
        mk_children_list print_comment comments
        @ [ mk_child print_kind num; mk_child Tree.make_node lexeme ]
      in
      make_tree state node children
    | Error msg -> make_unary state node Tree.make_node msg)

(* Printing enclosed constructs *)

let print_enclosed
    ?(comments = [])
    state
    node
    printer
    opening
    closing
    ~open_err
    ~close_err
  =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> print_error_node state node ~err:open_err
  | _ ->
    let comments = comments @ prev_comments node in
    let opening = first_child_named opening node ~err:open_err
    and closing = first_child_named closing node ~err:close_err
    and clauses = collect_named_children node in
    let children =
      (mk_child_res (make_sym ~comments ~err:open_err) opening
      :: mk_children_list printer clauses)
      @ [ mk_child_res (make_sym ~err:close_err) closing ]
    in
    make_tree state node children

let print_braces ?(comments = []) state node printer ~err =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> print_error_node state node ~err
  | _ ->
    print_enclosed
      ~comments
      state
      node
      printer
      "{"
      "}"
      ~open_err:Syntax_err.Left_brace
      ~close_err:Syntax_err.Right_brace

let print_chevrons ?(comments = []) state node printer ~err =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> print_error_node state node ~err
  | _ ->
    print_enclosed
      ~comments
      state
      node
      printer
      "<"
      ">"
      ~open_err:Syntax_err.Left_chevron
      ~close_err:Syntax_err.Right_chevron

let print_brackets ?(comments = []) state node printer ~err =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> print_error_node state node ~err
  | _ ->
    print_enclosed
      ~comments
      state
      node
      printer
      "["
      "]"
      ~open_err:Syntax_err.Left_bracket
      ~close_err:Syntax_err.Right_bracket

let print_parens ?(comments = []) state node printer ~err =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> print_error_node state node ~err
  | _ ->
    print_enclosed
      ~comments
      state
      node
      printer
      "("
      ")"
      ~open_err:Syntax_err.Left_parenthesis
      ~close_err:Syntax_err.Right_parenthesis

(* Concluding a pattern matching with a default printer. Dropping comments. *)

(* Printing the CST *)

let rec print_program ~filename ~file (map : Loc_map.t) node =
  (* Setting up the extracting of source regions *)
  let () = get_region := Ts_wrap.get_region filename map in
  (* Setting the input as a top-level string buffer *)
  let () = Buffer.add_string !input file in
  (* Empty state for building the AST *)
  let buffer = Buffer.create 1023 in
  let state = Tree.mk_state ~buffer ~regions:true ~layout:true ~offsets:true `Byte in
  (* Printing the CST into a string buffer in [state] *)
  let () = print_statements state node in
  (* Making the output string *)
  Buffer.contents @@ Tree.to_buffer state

(* STATEMENTS

   The JavaScript tree-sitter grammar has the non-terminal
   "statement" be a supertype, that is, a hidden rule. *)

and print_statements state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> print_error_node state node ~err:Syntax_err.Statement
  | _ -> tree_of_named_children state node print_statement

and print_statement ?(comments = []) state node =
  match get_name node with
  | "export_statement" -> print_export_statement ~comments state node
  | "import_statement" -> print_import_statement ~comments state node
  | "debugger_statement" -> print_debugger_statement ~comments state node
  | "expression_statement" -> print_expression_statement ~comments state node
  | "statement_block" -> print_statement_block ~comments state node
  | "if_statement" -> print_if_statement ~comments state node
  | "switch_statement" -> print_switch_statement state node
  | "for_statement" -> print_for_statement state node
  | "for_in_statement" -> print_for_in_statement state node
  | "while_statement" -> print_while_statement state node
  | "do_statement" -> print_do_statement state node
  | "try_statement" -> print_try_statement state node
  | "with_statement" -> print_with_statement state node
  | "break_statement" -> print_break_statement state node
  | "continue_statement" -> print_continue_statement state node
  | "return_statement" -> print_return_statement state node
  | "throw_statement" -> print_throw_statement state node
  | "empty_statement" -> print_empty_statement state node
  | "labeled_statement" -> print_labeled_statement state node
  (* Inlining declarations cases (hidden rule) *)
  | "function_declaration" -> print_function_declaration ~comments state node
  | "generator_function_declaration" -> print_generator_function_declaration state node
  | "class_declaration" -> print_class_declaration ~comments state node
  | "lexical_declaration" -> print_lexical_declaration ~comments state node
  | "variable_declaration" -> print_variable_declaration ~comments state node
  | "function_signature" -> print_function_signature state node
  | "abstract_class_declaration" -> print_abstract_class_declaration state node
  | "module" -> print_module state node
  | "internal_module" -> print_internal_module ~comments state node
  | "type_alias_declaration" -> print_type_alias_declaration state node
  | "enum_declaration" -> print_enum_declaration state node
  | "interface_declaration" -> print_interface_declaration state node
  | "import_alias" -> print_import_alias state node
  | "ambient_declaration" -> print_ambient_declaration state node
  | _ -> print_error_node state node ~err:Syntax_err.Statement

(* Export statement *)

and print_export_statement ?(comments = []) state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> print_error_node state node ~err:Syntax_err.Export
  | _ ->
    let comments = comments @ prev_comments node
    and decorators = children_named "decorator" node
    and kwd_export = first_child_named_opt "export" node in
    let decorators = mk_children_list print_decorator decorators in
    let children =
      match kwd_export with
      | None -> [ mk_error_child node ~err:Syntax_err.Export ]
      | Some kwd_export ->
        (* Previous comments are hooked to the keyword "export" *)
        mk_child (mk_kwd_export ~comments) kwd_export
        ::
        (match next_sibling kwd_export with
        | Error _ -> [ mk_error_child node ~err:Syntax_err.Export_clause_or_all ]
        | Ok after_export ->
          (match get_name after_export with
          | "*" ->
            let kwd_from = first_child_named "from" node ~err:Syntax_err.From in
            [ mk_child mk_sym_asterisk after_export; mk_child_from_clause kwd_from node ]
          | "namespace_export" ->
            let kwd_from = first_child_named "from" node ~err:Syntax_err.From in
            [ mk_child print_namespace_export after_export
            ; mk_child_from_clause kwd_from node
            ]
          | "export_clause" ->
            mk_child print_export_clause after_export :: mk_child_from_clause_opt node
          | "default" ->
            let declaration_field = child_with_field_opt "declaration" node in
            decorators
            @ [ mk_child mk_kwd_default after_export ]
            @
            (match declaration_field with
            | Some declaration_field -> [ mk_child print_declaration declaration_field ]
            | None ->
              let value_field =
                child_with_field "value" node ~err:Syntax_err.Expression
              in
              [ mk_child_res print_expression value_field ])
          | "type" ->
            (match next_sibling after_export with
            | Error _ -> [ mk_error_child node ~err:Syntax_err.Export_clause ]
            | Ok export_clause ->
              mk_child mk_kwd_type after_export
              :: mk_child print_export_clause export_clause
              :: mk_child_from_clause_opt node)
          | "=" ->
            (match next_sibling after_export with
            | Error _ -> [ mk_error_child node ~err:Syntax_err.Expression ]
            | Ok expression ->
              [ mk_child mk_sym_equal after_export; mk_child print_expression expression ])
          | "as" ->
            let kwd_namespace =
              first_child_named "namespace" node ~err:Syntax_err.Namespace
            and identifier =
              first_child_named "identifier" node ~err:Syntax_err.Identifier
            in
            [ mk_child mk_kwd_as after_export
            ; mk_child_res mk_kwd_namespace kwd_namespace
            ; mk_child_res print_identifier identifier
            ]
          | _ -> decorators @ [ mk_child print_declaration after_export ]))
    in
    make_tree state node children

and print_namespace_export ?(comments = []) state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    print_error_node state node ~err:Syntax_err.Namespace_export
  | _ ->
    let comments = comments @ prev_comments node
    and sym_asterisk = first_child_named "*" node ~err:Syntax_err.Asterisk
    and kwd_as = first_child_named "as" node ~err:Syntax_err.As in
    let module_export_name = next_sibling_res kwd_as in
    let children =
      [ mk_child_res (mk_sym_asterisk ~comments) sym_asterisk
      ; mk_child_res mk_kwd_as kwd_as
      ; mk_child_res print_module_export_name module_export_name
      ]
    in
    make_tree state node children

(* Argument [node] cannot be an ERROR/MISSING node. See [print_export_statement]. *)

and mk_child_from_clause kwd_from node =
  let source_field = child_with_field "source" node ~err:Syntax_err.File_path in
  let children =
    [ mk_child_res mk_kwd_from kwd_from; mk_child_res print_string source_field ]
  in
  Some (fun state -> Tree.make_tree state "from_clause" children)

(* Argument [node] cannot be an ERROR/MISSING node. See [print_export_statement]. *)

and mk_child_from_clause_opt node =
  match first_child_named_opt "from" node with
  | None -> []
  | Some kwd_from -> [ mk_child_from_clause (Ok kwd_from) node ]

and print_export_clause state node =
  print_braces state node print_export_specifier ~err:Export_clause

and print_module_export_name ?(comments = []) state node =
  match get_name node with
  | "identifier" -> print_identifier ~comments state node
  | "string" -> print_string ~comments state node
  | _ -> print_error_node state node ~err:Syntax_err.Identifier_or_string

and print_export_specifier ?(comments = []) state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    print_error_node state node ~err:Syntax_err.Identifier_or_string
  | _ ->
    let comments = comments @ prev_comments node in
    let name_field = child_with_field "name" node ~err:Syntax_err.Identifier_or_string in
    let children =
      mk_child_res (print_module_export_name ~comments) name_field
      ::
      (match child_with_field_opt "alias" node with
      | None -> []
      | Some alias_field ->
        let kwd_as = first_child_named "as" node ~err:Syntax_err.As in
        [ mk_child_res mk_kwd_as kwd_as; mk_child print_module_export_name alias_field ])
    in
    make_tree state node children

(* Import statement *)

and print_import_statement ?(comments = []) state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> print_error_node state node ~err:Syntax_err.Import
  | _ ->
    let comments = comments @ prev_comments node
    and kwd_import = first_child_named "import" node ~err:Syntax_err.Import
    and kind_node =
      match first_child_named_opt "type" node with
      | None -> first_child_named_opt "typeof" node
      | some -> some
    and import_attribute = first_child_named_opt "import_attribute" node in
    let middle_children =
      match first_child_named_opt "import_clause" node with
      | Some import_clause ->
        let kwd_from = first_child_named "from" node ~err:Syntax_err.From in
        [ mk_child print_import_clause import_clause; mk_child_from_clause kwd_from node ]
      | None ->
        (match first_child_named_opt "import_require_clause" node with
        | Some clause -> [ mk_child print_import_require_clause clause ]
        | None ->
          let source_field = child_with_field "source" node ~err:Syntax_err.String in
          [ mk_child_res print_string source_field ])
    in
    let children =
      (* Previous comments are hooked to the keyword "import" *)
      (mk_child_res (mk_kwd_import ~comments) kwd_import
      :: mk_child_opt (make_kwd ~err:Syntax_err.Type_or_typeof) kind_node
      :: middle_children)
      @ [ mk_child_opt print_import_attribute import_attribute ]
    in
    make_tree state node children

and print_import_clause ?(comments = []) state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    print_error_node state node ~err:Syntax_err.Import_clause
  | _ ->
    let comments = comments @ prev_comments node in
    let print_rest state node =
      match get_name node with
      | "namespace_import" -> print_namespace_import state node
      | "named_imports" -> print_named_imports state node
      | _ -> print_error_node state node ~err:Syntax_err.Namespace_or_named_imports
    in
    let children =
      match child_ranked_opt 0 node with
      | None -> [ mk_error_child node ~err:Syntax_err.Named_imports_or_all_or_id ]
      | Some fst_child ->
        (match get_name fst_child with
        | "namespace_import" -> [ mk_child (print_namespace_import ~comments) fst_child ]
        | "named_imports" -> [ mk_child (print_named_imports ~comments) fst_child ]
        | "identifier" ->
          mk_child (print_identifier ~comments) fst_child
          ::
          (match next_sibling_opt fst_child with
          | None -> []
          | Some comma ->
            (match next_sibling comma with
            | Error _ -> [ mk_error_child node ~err:Syntax_err.Named_imports_or_all ]
            | Ok next -> [ mk_child print_rest next ]))
        | _ ->
          [ mk_error_child fst_child ~err:Syntax_err.Namespace_or_named_imports_or_ident ])
    in
    make_tree state node children

and print_namespace_import ?(comments = []) state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    print_error_node state node ~err:Syntax_err.Namespace_import
  | _ ->
    let comments = comments @ prev_comments node
    and sym_asterisk = first_child_named "*" node ~err:Syntax_err.Asterisk
    and kwd_as = first_child_named "as" node ~err:Syntax_err.As in
    let identifier = next_sibling_res kwd_as in
    let children =
      [ mk_child_res (mk_sym_asterisk ~comments) sym_asterisk
      ; mk_child_res mk_kwd_as kwd_as
      ; mk_child_res print_identifier identifier
      ]
    in
    make_tree state node children

and print_named_imports ?(comments = []) state node =
  print_braces ~comments state node print_import_specifier ~err:Syntax_err.Named_imports

and print_import_specifier ?(comments = []) state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    print_error_node state node ~err:Syntax_err.Import_specifier
  | _ ->
    let comments = comments @ prev_comments node in
    let kind_node =
      match first_child_named_opt "type" node with
      | None -> first_child_named_opt "typeof" node
      | some -> some
    in
    let fst_child_comments, snd_child_comments =
      match kind_node with
      | None -> [], comments
      | Some _ -> comments, []
    and name_field = child_with_field "name" node ~err:Syntax_err.Identifier
    and alias_field = child_with_field_opt "alias" node in
    let children =
      mk_child_opt
        (make_kwd ~comments:fst_child_comments ~err:Syntax_err.Type_or_typeof)
        kind_node
      ::
      (match alias_field with
      | None ->
        [ mk_child_res (print_identifier ~comments:snd_child_comments) name_field ]
      | Some alias_field ->
        let kwd_as = first_child_named "as" node ~err:Syntax_err.As in
        [ mk_child_res (print_module_export_name ~comments:snd_child_comments) name_field
        ; mk_child_res mk_kwd_as kwd_as
        ; mk_child print_identifier alias_field
        ])
    in
    make_tree state node children

and print_import_require_clause ?(comments = []) state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    print_error_node state node ~err:Syntax_err.Import_require_clause
  | _ ->
    let comments = comments @ prev_comments node
    and identifier = child_ranked 0 node ~err:Syntax_err.Identifier
    and sym_equal = first_child_named "=" node ~err:Syntax_err.Equal
    and id_require = first_child_named "require" node ~err:Syntax_err.Require
    and sym_lparen = first_child_named "(" node ~err:Syntax_err.Left_parenthesis
    and source_field = child_with_field "source" node ~err:Syntax_err.String
    and sym_rparen = first_child_named ")" node ~err:Syntax_err.Right_parenthesis in
    let children =
      [ mk_child_res (print_identifier ~comments) identifier
      ; mk_child_res mk_sym_equal sym_equal
      ; mk_child_res print_identifier id_require
      ; mk_child_res mk_sym_lparen sym_lparen
      ; mk_child_res print_string source_field
      ; mk_child_res mk_sym_rparen sym_rparen
      ]
    in
    make_tree state node children

and print_import_attribute state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    print_error_node state node ~err:Syntax_err.Import_attribute
  | _ ->
    let kind_node = child_ranked 0 node ~err:Syntax_err.Import_attribute
    and object_node = child_ranked 1 node ~err:Syntax_err.Object_expression
    and print_kind state node =
      match get_name node with
      | "with" -> mk_kwd_with state node
      | "assert" -> mk_kwd_assert state node
      | _ -> print_error_node state node ~err:Syntax_err.Import_attribute
    in
    let children =
      [ mk_child_res print_kind kind_node; mk_child_res print_object object_node ]
    in
    make_tree state node children

(* Debugger statement *)

and print_debugger_statement ?(comments = []) state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> print_error_node state node ~err:Syntax_err.Debugger
  | _ ->
    let kwd_debugger = first_child_named "debugger" node ~err:Syntax_err.Debugger in
    let children = [ mk_child_res (mk_kwd_debugger ~comments) kwd_debugger ] in
    make_tree state node children

(* Expression statements

   {@js[
   expression_statement: $ => seq($._expressions, $._semicolon),
   _expressions: $ => choice($.expression, $.sequence_expression),
   sequence_expression: $ => prec.right(commaSep1($.expression))
   ]}

   See [print_expression]. *)

and print_expression_statement ?(comments = []) state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> print_error_node state node ~err:Syntax_err.Expression
  | _ ->
    let comments = comments @ prev_comments node
    and child = named_child_ranked 0 node ~err:Syntax_err.Expression in
    make_unary_res state node (print_expressions ~comments) child

and print_expressions ?(comments = []) state (node : ts_tree) =
  match get_name node with
  | "sequence_expression" -> print_sequence_expression ~comments state node
  | _ -> print_expression ~comments state node

(* Statement blocks *)

and print_statement_block ?(comments = []) state node =
  print_braces ~comments state node print_statement ~err:Syntax_err.Block

(* If statement *)

and print_if_statement ?(comments = []) state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> print_error_node state node ~err:Syntax_err.If
  | _ ->
    let kwd_if = first_child_named "if" node ~err:Syntax_err.If
    and condition_field =
      child_with_field "condition" node ~err:Syntax_err.Parenthesized_expression
    and consequence_field = child_with_field "consequence" node ~err:Syntax_err.Statement
    and alternative_field = child_with_field_opt "alternative" node in
    let children =
      [ mk_child_res (mk_kwd_if ~comments) kwd_if
      ; mk_child_res print_parenthesized_expression condition_field
      ; mk_child_res print_statement consequence_field
      ; mk_child_opt print_else_clause alternative_field
      ]
    in
    make_tree state node children

and print_else_clause ?(comments = []) state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> print_error_node state node ~err:Syntax_err.Else
  | _ ->
    let comments = comments @ prev_comments node in
    let kwd_else = first_child_named "else" node ~err:Syntax_err.Else in
    let statement = next_sibling_res kwd_else in
    let children =
      [ mk_child_res (mk_kwd_else ~comments) kwd_else
      ; mk_child_res print_statement statement
      ]
    in
    make_tree state node children

(* Switch statement *)

and print_switch_statement state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> print_error_node state node ~err:Syntax_err.Switch
  | _ ->
    let kwd_switch = first_child_named "switch" node ~err:Syntax_err.Switch
    and value_field =
      child_with_field "value" node ~err:Syntax_err.Parenthesized_expression
    and body_field = child_with_field "body" node ~err:Syntax_err.Switch_body in
    let children =
      [ mk_child_res mk_kwd_switch kwd_switch
      ; mk_child_res print_parenthesized_expression value_field
      ; mk_child_res print_switch_body body_field
      ]
    in
    make_tree state node children

and print_switch_body state node =
  let print state node =
    match get_name node with
    | "switch_case" -> print_switch_case state node
    | "switch_default" -> print_switch_default state node
    | _ -> print_error_node state node ~err:Syntax_err.Switch_body
  in
  print_braces state node print ~err:Syntax_err.Switch_body

and print_switch_case state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> print_error_node state node ~err:Syntax_err.Case
  | _ ->
    let kwd_case = first_child_named "case" node ~err:Syntax_err.Case
    and children = collect_children node in
    let rec skip_until_colon = function
      | [] -> []
      | node :: nodes ->
        (match get_name node with
        | ":" -> nodes
        | _ -> skip_until_colon nodes)
    in
    let stmt_children = skip_until_colon children
    and value_field = child_with_field "value" node ~err:Syntax_err.Expression in
    let children =
      mk_child_res mk_kwd_case kwd_case
      :: mk_child_res print_expressions value_field
      :: mk_children_list print_statement stmt_children
    in
    make_tree state node children

and print_switch_default state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> print_error_node state node ~err:Syntax_err.Default
  | _ ->
    let kwd_default = first_child_named "default" node ~err:Syntax_err.Default
    and statements = collect_named_children node in
    let children =
      mk_child_res mk_kwd_default kwd_default
      :: mk_children_list print_statement statements
    in
    make_tree state node children

(* For statement *)

and print_for_statement state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> print_error_node state node ~err:Syntax_err.For
  | _ ->
    let kwd_for = first_child_named "for" node ~err:Syntax_err.For
    and sym_lparen = first_child_named "(" node ~err:Syntax_err.Left_parenthesis
    and initializer_field =
      child_with_field "initializer" node ~err:Syntax_err.Initial_assignment
    and condition_field =
      child_with_field "condition" node ~err:Syntax_err.Expression_or_semicolon
    and increment_field = child_with_field_opt "increment" node
    and sym_rparen = first_child_named ")" node ~err:Syntax_err.Right_parenthesis
    and body_field = child_with_field "body" node ~err:Syntax_err.Statement
    and print_initializer state node =
      match get_name node with
      | "lexical_declaration" -> print_lexical_declaration state node
      | "variable_declaration" -> print_variable_declaration state node
      | "expression_statement" -> print_expression_statement state node
      | "empty_statement" -> print_empty_statement state node
      | _ -> print_error_node state node ~err:Syntax_err.Initial_assignment
    and print_condition state node =
      match get_name node with
      | "expression_statement" -> print_expression_statement state node
      | "empty_statement" -> print_empty_statement state node
      | _ -> print_error_node state node ~err:Syntax_err.Expression_or_semicolon
    in
    let children =
      [ mk_child_res mk_kwd_for kwd_for
      ; mk_child_res mk_sym_lparen sym_lparen
      ; mk_child_res print_initializer initializer_field
      ; mk_child_res print_condition condition_field
      ; mk_child_opt print_expressions increment_field
      ; mk_child_res mk_sym_rparen sym_rparen
      ; mk_child_res print_statement body_field
      ]
    in
    make_tree state node children

(* For-in statement *)

and print_for_in_statement state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    print_error_node state node ~err:Syntax_err.For_or_await
  | _ ->
    let kwd_await = first_child_named_opt "await" node
    and kwd_for = first_child_named "for" node ~err:Syntax_err.For
    and sym_lparen = first_child_named "(" node ~err:Syntax_err.Left_parenthesis
    and left_field = child_with_field "left" node ~err:Syntax_err.Expression
    and sym_rparen = first_child_named ")" node ~err:Syntax_err.Right_parenthesis
    and body_field = child_with_field "body" node ~err:Syntax_err.Statement
    and operator_field = child_with_field "operator" node ~err:Syntax_err.In_or_of
    and right_field = child_with_field "right" node ~err:Syntax_err.Expression
    and kind_field = child_with_field_opt "kind" node in
    let print_operator state node =
      match get_name node with
      | "in" -> mk_kwd_in state node
      | "of" -> mk_kwd_of state node
      | _ -> print_error_node state node ~err:Syntax_err.In_or_of
    in
    let header_children =
      match kind_field with
      | None ->
        let print_left state node =
          match get_name node with
          | "ERROR" | "MISSING" | "NULL" ->
            print_error_node state node ~err:Syntax_err.Expression
          | "parenthesized_expression" -> print_parenthesized_expression state node
          | _ -> print_lhs_expression state node
        in
        [ mk_child_res print_left left_field ]
      | Some kind_field ->
        let print_left state node =
          match get_name node with
          | "ERROR" | "MISSING" | "NULL" ->
            print_error_node state node ~err:Syntax_err.Pattern
          | "identifier" -> print_identifier state node
          | _ -> print_destructuring_pattern state node (* Hidden *)
        in
        (match get_name kind_field with
        | "var" ->
          let value_field = child_with_field_opt "value" node in
          [ mk_child mk_kwd_var kind_field
          ; mk_child_res print_left left_field
          ; mk_child_opt print_expression value_field
          ]
        | "let" -> [ mk_child mk_kwd_let kind_field; mk_child_res print_left left_field ]
        | "const" ->
          [ mk_child mk_kwd_const kind_field; mk_child_res print_left left_field ]
        | _ ->
          [ mk_child (print_error_node ~err:Syntax_err.Let_or_const_or_var) kind_field ])
    in
    let children =
      (mk_child_res mk_kwd_for kwd_for
      :: mk_child_opt mk_kwd_await kwd_await
      :: mk_child_res mk_sym_lparen sym_lparen
      :: header_children)
      @ [ mk_child_res print_operator operator_field
        ; mk_child_res print_expressions right_field
        ; mk_child_res mk_sym_rparen sym_rparen
        ; mk_child_res print_statement body_field
        ]
    in
    make_tree state node children

(* While statement *)

and print_while_statement state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> print_error_node state node ~err:Syntax_err.While
  | _ ->
    let kwd_while = first_child_named "while" node ~err:Syntax_err.While
    and condition_field =
      child_with_field "condition" node ~err:Syntax_err.Parenthesized_expression
    and body_field = child_with_field "body" node ~err:Syntax_err.Statement in
    let children =
      [ mk_child_res mk_kwd_while kwd_while
      ; mk_child_res print_parenthesized_expression condition_field
      ; mk_child_res print_statement body_field
      ]
    in
    make_tree state node children

(* Do statement *)

and print_do_statement state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> print_error_node state node ~err:Syntax_err.Do
  | _ ->
    let kwd_do = first_child_named "do" node ~err:Syntax_err.Do
    and body_field = child_with_field "body" node ~err:Syntax_err.Statement
    and kwd_while = first_child_named "while" node ~err:Syntax_err.While
    and condition_field =
      child_with_field "condition" node ~err:Syntax_err.Parenthesized_expression
    in
    let children =
      [ mk_child_res mk_kwd_do kwd_do
      ; mk_child_res print_statement body_field
      ; mk_child_res mk_kwd_while kwd_while
      ; mk_child_res print_parenthesized_expression condition_field
      ]
    in
    make_tree state node children

(* Try statement *)

and print_try_statement state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> print_error_node state node ~err:Syntax_err.Try
  | _ ->
    let kwd_try = first_child_named "try" node ~err:Syntax_err.Try
    and body_field = child_with_field "body" node ~err:Syntax_err.Block
    and handler_field = child_with_field_opt "handler" node
    and finalizer_field = child_with_field_opt "finalizer" node in
    let children =
      [ mk_child_res mk_kwd_try kwd_try
      ; mk_child_res print_statement_block body_field
      ; mk_child_opt print_catch_clause handler_field
      ; mk_child_opt print_finally_clause finalizer_field
      ]
    in
    make_tree state node children

and print_catch_clause state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> print_error_node state node ~err:Syntax_err.Catch
  | _ ->
    let kwd_catch = first_child_named "catch" node ~err:Syntax_err.Catch
    and body_field = child_with_field "body" node ~err:Syntax_err.Block
    and parameter_field = child_with_field_opt "parameter" node
    and print_parameter state node =
      match get_name node with
      | "ERROR" | "MISSING" | "NULL" ->
        print_error_node state node ~err:Syntax_err.Pattern
      | "identifier" -> print_identifier state node
      | _ -> print_destructuring_pattern state node
    in
    let children =
      match parameter_field with
      | Some parameter_field ->
        let sym_lparen = first_child_named "(" node ~err:Syntax_err.Left_parenthesis
        and type_field = child_with_field_opt "type" node
        and sym_rparen = first_child_named ")" node ~err:Syntax_err.Right_parenthesis in
        [ mk_child_res mk_sym_lparen sym_lparen
        ; mk_child print_parameter parameter_field
        ; mk_child_opt print_type_annotation type_field
        ; mk_child_res mk_sym_rparen sym_rparen
        ]
      | None -> []
    in
    let children = mk_child_res mk_kwd_catch kwd_catch :: children in
    let children = children @ [ mk_child_res print_statement_block body_field ] in
    make_tree state node children

and print_finally_clause state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> print_error_node state node ~err:Syntax_err.Finally
  | _ ->
    let kwd_finally = first_child_named "finally" node ~err:Syntax_err.Finally
    and body_field = child_with_field "body" node ~err:Syntax_err.Block in
    let children =
      [ mk_child_res mk_kwd_finally kwd_finally
      ; mk_child_res print_statement_block body_field
      ]
    in
    make_tree state node children

(* With statement *)

and print_with_statement state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> print_error_node state node ~err:Syntax_err.With
  | _ ->
    let kwd_with = first_child_named "with" node ~err:Syntax_err.With
    and object_field =
      child_with_field "object" node ~err:Syntax_err.Parenthesized_expression
    and body_field = child_with_field "body" node ~err:Syntax_err.Statement in
    let children =
      [ mk_child_res mk_kwd_with kwd_with
      ; mk_child_res print_parenthesized_expression object_field
      ; mk_child_res print_statement body_field
      ]
    in
    make_tree state node children

(* Break statement *)

and print_break_statement state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> print_error_node state node ~err:Syntax_err.Break
  | _ ->
    let kwd_break = first_child_named "break" node ~err:Syntax_err.Break
    and label_field = child_with_field_opt "label" node in
    let children =
      [ mk_child_res mk_kwd_break kwd_break; mk_child_opt print_identifier label_field ]
    in
    make_tree state node children

(* Continue statement *)

and print_continue_statement state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> print_error_node state node ~err:Syntax_err.Continue
  | _ ->
    let kwd_continue = first_child_named "continue" node ~err:Syntax_err.Continue
    and label_field = child_with_field_opt "label" node in
    let children =
      [ mk_child_res mk_kwd_continue kwd_continue
      ; mk_child_opt print_identifier label_field
      ]
    in
    make_tree state node children

(* Return statement

   NOTE: The Javascript grammar states:

   {@js[
   return_statement: $ =>
   seq('return', optional($._expressions), $._semicolon),

   _semicolon: $ => choice($._automatic_semicolon, ';')
   ]}

   but the child of rank 1 is sometimes missing, as if
   "_automatic_semicolon" can be the empty word. Other rules use
   `optional(_automatic_semicolon)`, which adds to the mystery. *)

and print_return_statement state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> print_error_node state node ~err:Syntax_err.Return
  | _ ->
    let kwd_return = first_child_named "return" node ~err:Syntax_err.Return in
    (match child_ranked_opt 1 node with
    | None -> make_unary_res state node mk_kwd_return kwd_return
    | Some snd_child ->
      (match get_name snd_child with
      | ";" -> make_unary_res state node mk_kwd_return kwd_return
      | _ ->
        let children =
          [ mk_child_res mk_kwd_return kwd_return; mk_child print_expressions snd_child ]
        in
        make_tree state node children))

(* Throw statement *)

and print_throw_statement state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> print_error_node state node ~err:Syntax_err.Throw
  | _ ->
    let kwd_throw = first_child_named "throw" node ~err:Syntax_err.Throw
    and expr = child_ranked 1 node ~err:Syntax_err.Expression in
    let children =
      [ mk_child_res mk_kwd_throw kwd_throw; mk_child_res print_expressions expr ]
    in
    make_tree state node children

(* Empty statement *)

and print_empty_statement state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    print_error_node state node ~err:Syntax_err.Empty_statement
  | _ ->
    let region = !get_region node
    and label = get_name node in
    Tree.make ~region state label []

(* Labeled statement *)

and print_labeled_statement state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> print_error_node state node ~err:Syntax_err.Label
  | _ ->
    let label_field = child_with_field "label" node ~err:Syntax_err.Label
    and body_field = child_with_field "body" node ~err:Syntax_err.Statement in
    let children =
      [ mk_child_res print_identifier label_field
      ; mk_child_res print_statement body_field
      ]
    in
    make_tree state node children

(* DECLARATION

   The JavaScript tree-sitter grammar has the non-terminal
   "declaration" be a supertype, that is, a hidden rule. *)

and print_declaration ?(comments = []) state node =
  let comments = comments @ prev_comments node in
  match get_name node with
  | "function_declaration" -> print_function_declaration state node
  | "generator_function_declaration" -> print_generator_function_declaration state node
  | "class_declaration" -> print_class_declaration ~comments state node
  | "lexical_declaration" -> print_lexical_declaration ~comments state node
  | "variable_declaration" -> print_variable_declaration state node
  | "function_signature" -> print_function_signature state node
  | "abstract_class_declaration" -> print_abstract_class_declaration state node
  | "module" -> print_module state node
  | "internal_module" -> print_internal_module ~comments state node
  | "type_alias_declaration" -> print_type_alias_declaration ~comments state node
  | "enum_declaration" -> print_enum_declaration state node
  | "interface_declaration" -> print_interface_declaration state node
  | "import_alias" -> print_import_alias state node
  | "ambient_declaration" -> print_ambient_declaration state node
  | _ -> print_error_node state node ~err:Syntax_err.Declaration

(* Function declaration (see [print_function_signature]) *)

and print_function_declaration ?(comments = []) state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    print_error_node state node ~err:Syntax_err.Function_declaration
  | _ ->
    let comments = comments @ prev_comments node
    and kwd_async = first_child_named_opt "async" node
    and kwd_function = first_child_named "function" node ~err:Syntax_err.Function
    and name_field = child_with_field "name" node ~err:Syntax_err.Function_name
    (* "_call_signature" inlined: *)
    and type_parameters_field = child_with_field_opt "type_parameters" node
    and parameters_field = child_with_field "parameters" node ~err:Syntax_err.Parameters
    and return_type_field = child_with_field_opt "return_type" node
    (* "statement_block" *)
    and body_field = child_with_field "body" node ~err:Syntax_err.Block in
    let async_comments, function_comments =
      match kwd_async with
      | None -> [], comments
      | Some _ -> comments, []
    in
    let children =
      [ mk_child_opt (mk_kwd_async ~comments:async_comments) kwd_async
      ; mk_child_res (mk_kwd_function ~comments:function_comments) kwd_function
      ; mk_child_res print_identifier name_field
      ; mk_child_opt print_type_parameters type_parameters_field
      ; mk_child_res print_formal_parameters parameters_field
      ; mk_child_opt print_return_type return_type_field
      ; mk_child_res print_statement_block body_field
      ]
    in
    make_tree state node children

and print_return_type state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    print_error_node state node ~err:Syntax_err.Type_expression
  | "type_annotation" -> print_type_annotation state node
  | "asserts_annotation" -> print_asserts_annotation state node
  | _ -> print_type_predicate_annotation state node

(* Generator function declaration (see function declaration) *)

and print_generator_function_declaration state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    print_error_node state node ~err:Syntax_err.Generator_function_declaration
  | _ ->
    let kwd_async = first_child_named_opt "async" node
    and kwd_function = first_child_named "function" node ~err:Syntax_err.Function
    and sym_asterisk = first_child_named "*" node ~err:Syntax_err.Asterisk
    and name_field = child_with_field "name" node ~err:Syntax_err.Function_name
    (* "_call_signature" inlined: *)
    and type_parameters_field = child_with_field_opt "type_parameters" node
    and parameters_field = child_with_field "parameters" node ~err:Syntax_err.Parameters
    and return_type_field = child_with_field_opt "return_type" node
    (* "statement_block" *)
    and body_field = child_with_field "body" node ~err:Syntax_err.Block in
    let children =
      [ mk_child_opt mk_kwd_async kwd_async
      ; mk_child_res mk_kwd_function kwd_function
      ; mk_child_res mk_sym_asterisk sym_asterisk
      ; mk_child_res print_identifier name_field
      ; mk_child_opt print_type_parameters type_parameters_field
      ; mk_child_res print_formal_parameters parameters_field
      ; mk_child_opt print_return_type return_type_field
      ; mk_child_res print_statement_block body_field
      ]
    in
    make_tree state node children

(* Class declaration (see [print_class]) *)

and print_class_declaration ?(comments = []) state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    print_error_node state node ~err:Syntax_err.Class_declaration
  | _ ->
    let comments = comments @ prev_comments node
    and decorators = children_named "decorator" node
    and kwd_class = first_child_named "class" node ~err:Syntax_err.Class
    and name_field = child_with_field "name" node ~err:Syntax_err.Class_name
    and type_parameters_field = child_with_field_opt "type_parameters" node
    and heritage_child = first_child_named_opt "class_heritage" node
    and body_field = child_with_field "body" node ~err:Syntax_err.Class_body in
    let children =
      mk_children_list print_decorator decorators
      @ [ mk_child_res (mk_kwd_class ~comments) kwd_class
        ; mk_child_res print_type_identifier name_field
        ; mk_child_opt print_type_parameters type_parameters_field
        ; mk_child_opt print_class_heritage heritage_child
        ; mk_child_res print_class_body body_field
        ]
    in
    make_tree state node children

(* Lexical declaration (see [print_variable_declaration]) *)

and print_lexical_declaration ?(comments = []) state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    print_error_node state node ~err:Syntax_err.Let_or_const
  | _ ->
    let comments = comments @ prev_comments node
    and kind_field = child_with_field "kind" node ~err:Syntax_err.Let_or_const
    and var_decls = children_named "variable_declarator" node in
    let print_set_or_const state node =
      match get_name node with
      | "let" -> mk_kwd_let ~comments state node
      | "const" -> mk_kwd_const ~comments state node
      | _ -> print_error_node state node ~err:Syntax_err.Let_or_const
    in
    let children =
      mk_child_res print_set_or_const kind_field
      :: mk_children_list print_variable_declarator var_decls
    in
    make_tree state node children

and print_variable_declarator state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> print_error_node state node ~err:Syntax_err.Variable
  | _ ->
    let name_field = child_with_field "name" node ~err:Syntax_err.Variable in
    let sym_qmark = first_child_named_opt "!" node in
    let children =
      match sym_qmark with
      | None ->
        let type_field = child_with_field_opt "type" node in
        mk_child_res print_lhs_pattern name_field
        :: mk_child_opt print_type_annotation type_field
        :: mk_child_initializer_opt node (* "_initializer" inlined *)
      | Some sym_qmark ->
        let type_field = child_with_field "type" node ~err:Syntax_err.Type_annotation in
        mk_child_res print_identifier name_field
        :: mk_child mk_sym_qmark sym_qmark
        :: [ mk_child_res print_type_annotation type_field ]
    in
    make_tree state node children

and print_lhs_pattern state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> print_error_node state node ~err:Syntax_err.Pattern
  | "identifier" -> print_identifier state node
  | _ -> print_destructuring_pattern state node

(* Variable declaration (see [print_lexical_declaration]) *)

and print_variable_declaration ?(comments = []) state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> print_error_node state node ~err:Syntax_err.Var
  | _ ->
    let comments = comments @ prev_comments node
    and kwd_var = first_child_named "var" node ~err:Syntax_err.Var
    and var_decls = children_named "variable_declarator" node in
    let children =
      mk_child_res (mk_kwd_var ~comments) kwd_var
      :: mk_children_list print_variable_declarator var_decls
    in
    make_tree state node children

(* Function signature (See [print_function_declaration]) *)

and print_function_signature state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    print_error_node state node ~err:Syntax_err.Async_or_function
  | _ ->
    let kwd_async = first_child_named_opt "async" node
    and kwd_function = first_child_named "function" node ~err:Syntax_err.Function
    and name_field = child_with_field "name" node ~err:Syntax_err.Function_name
    (* "_call_signature" inlined: *)
    and type_parameters_field = child_with_field_opt "type_parameters" node
    and parameters_field = child_with_field "parameters" node ~err:Syntax_err.Parameters
    and return_type_field = child_with_field_opt "return_type" node in
    (* "statement_block" *)
    let children =
      [ mk_child_opt mk_kwd_async kwd_async
      ; mk_child_res mk_kwd_function kwd_function
      ; mk_child_res print_identifier name_field
      ; mk_child_opt print_type_parameters type_parameters_field
      ; mk_child_res print_formal_parameters parameters_field
      ; mk_child_opt print_return_type return_type_field
      ]
    in
    make_tree state node children

(* Abstract class declaration ( see [print_class_declaration]) *)

and print_abstract_class_declaration state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> print_error_node state node ~err:Syntax_err.Abstract
  | _ ->
    let decorators = children_named "decorator" node
    and kwd_abstract = first_child_named "abstract" node ~err:Syntax_err.Abstract
    and kwd_class = first_child_named "class" node ~err:Syntax_err.Class
    and name_field = child_with_field "name" node ~err:Syntax_err.Class_name
    and type_parameters_field = child_with_field_opt "type_parameters" node
    and heritage_child = first_child_named_opt "class_heritage" node
    and body_field = child_with_field "body" node ~err:Syntax_err.Class_body in
    let children =
      mk_children_list print_decorator decorators
      @ [ mk_child_res mk_kwd_abstract kwd_abstract
        ; mk_child_res mk_kwd_class kwd_class
        ; mk_child_res print_type_identifier name_field
        ; mk_child_opt print_type_parameters type_parameters_field
        ; mk_child_opt print_class_heritage heritage_child
        ; mk_child_res print_class_body body_field
        ]
    in
    make_tree state node children

(* Module *)

and print_module ?(comments = []) state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    print_error_node state node ~err:Syntax_err.Module_name
  | _ ->
    let comments = comments @ prev_comments node
    and kwd_module = first_child_named "module" node ~err:Syntax_err.Module
    and name_field = child_with_field "name" node ~err:Syntax_err.Module_name
    and body_field = child_with_field_opt "body" node
    and print_name state node =
      match get_name node with
      | "string" -> print_string state node
      | "identifier" -> print_identifier state node
      | "nested_identifier" -> print_nested_identifier state node
      | _ -> print_error_node state node ~err:Syntax_err.Module_name
    in
    let children =
      [ mk_child_res (mk_kwd_module ~comments) kwd_module
      ; mk_child_res print_name name_field
      ; mk_child_opt print_statement_block body_field
      ]
    in
    make_tree state node children

(* Internal module (a.k.a. namespaces) *)

and print_internal_module ?(comments = []) state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    print_error_node state node ~err:Syntax_err.Namespace_name
  | _ ->
    let comments = comments @ prev_comments node
    and kwd_namespace = first_child_named "namespace" node ~err:Syntax_err.Namespace
    and name_field = child_with_field "name" node ~err:Syntax_err.Namespace_name
    and body_field = child_with_field_opt "body" node
    and print_name state node =
      match get_name node with
      | "string" -> print_string state node
      | "identifier" -> print_identifier state node
      | "nested_identifier" -> print_nested_identifier state node
      | _ -> print_error_node state node ~err:Syntax_err.Namespace_name
    in
    let children =
      [ mk_child_res (mk_kwd_namespace ~comments) kwd_namespace
      ; mk_child_res print_name name_field
      ; mk_child_opt print_statement_block body_field
      ]
    in
    make_tree state node children

(* Type alias declaration *)

and print_type_alias_declaration ?(comments = []) state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> print_error_node state node ~err:Syntax_err.Type_name
  | _ ->
    let comments = comments @ prev_comments node
    and kwd_type = first_child_named "type" node ~err:Syntax_err.Type
    and name_field = child_with_field "name" node ~err:Syntax_err.Type_name
    and sym_equal = first_child_named "=" node ~err:Syntax_err.Equal
    and type_parameters_field = child_with_field_opt "type_parameters" node
    and value_field = child_with_field "value" node ~err:Syntax_err.Type_expression in
    let children =
      [ mk_child_res (mk_kwd_type ~comments) kwd_type
      ; mk_child_res print_identifier name_field
      ; mk_child_res mk_sym_equal sym_equal
      ; mk_child_opt print_type_parameters type_parameters_field
      ; mk_child_res print_type value_field
      ]
    in
    make_tree state node children

(* Type parameters *)

and print_type_parameters state node =
  print_chevrons state node print_type_parameter ~err:Syntax_err.Type_parameters

and print_type_parameter state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    print_error_node state node ~err:Syntax_err.Const_or_type_name
  | _ ->
    let kwd_const = first_child_named_opt "const" node
    and name_field = child_with_field "name" node ~err:Syntax_err.Type_parameter
    and constraint_field = child_with_field_opt "constraint" node
    and value_field = child_with_field_opt "value" node in
    let children =
      [ mk_child_opt mk_kwd_const kwd_const
      ; mk_child_res print_identifier name_field
      ; mk_child_opt print_constraint constraint_field
      ; mk_child_opt print_default_type value_field
      ]
    in
    make_tree state node children

and print_constraint state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> print_error_node state node ~err:Syntax_err.Extends
  | _ ->
    (* The grammar says:

         constraint: $ => seq(choice('extends', ':'), $.type),

       What is ':'? *)
    let kwd_extends = first_child_named "extends" node ~err:Syntax_err.Extends
    and type_child = child_ranked 1 node ~err:Syntax_err.Type_expression in
    let children =
      [ mk_child_res mk_kwd_extends kwd_extends; mk_child_res print_type type_child ]
    in
    make_tree state node children

and print_default_type state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> print_error_node state node ~err:Syntax_err.Equal
  | _ ->
    let sym_equal = first_child_named "=" node ~err:Syntax_err.Equal
    and type_node = child_ranked 1 node ~err:Syntax_err.Type_expression in
    let children =
      [ mk_child_res mk_sym_equal sym_equal; mk_child_res print_type type_node ]
    in
    make_tree state node children

(* Enum declaration *)

and print_enum_declaration state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    print_error_node state node ~err:Syntax_err.Const_or_enum
  | _ ->
    let kwd_const = first_child_named_opt "const" node
    and kwd_enum = first_child_named "enum" node ~err:Syntax_err.Enum
    and name_field = child_with_field "name" node ~err:Syntax_err.Enumeration_name
    and body_field = child_with_field "body" node ~err:Syntax_err.Enumeration in
    let children =
      [ mk_child_opt mk_kwd_const kwd_const
      ; mk_child_res mk_kwd_enum kwd_enum
      ; mk_child_res print_identifier name_field
      ; mk_child_res print_enum_body body_field
      ]
    in
    make_tree state node children

and print_enum_body state node =
  let print state node =
    match get_name node with
    | "ERROR" | "MISSING" | "NULL" ->
      print_error_node state node ~err:Syntax_err.Enumeration_name
    | "enum_assignment" -> print_enum_assignment state node
    | _ -> print_property_name state node
  in
  print_braces state node print ~err:Syntax_err.Left_brace

and print_enum_assignment state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    print_error_node state node ~err:Syntax_err.Enumeration_name
  | _ ->
    let name_field = child_with_field "name" node ~err:Syntax_err.Enumeration_name in
    let children =
      mk_child_res print_property_name name_field
      :: mk_child_initializer_opt node (* "_initializer" inlined *)
    in
    make_tree state node children

(* Interface declaration *)

and print_interface_declaration state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> print_error_node state node ~err:Syntax_err.Interface
  | _ ->
    let kwd_interface = first_child_named "interface" node ~err:Syntax_err.Interface
    and name_field = child_with_field "name" node ~err:Syntax_err.Interface_name
    and type_parameters_field = child_with_field_opt "type_parameters" node
    and extends_type_clause = first_child_named_opt "extends_type_clause" node
    and body_field = child_with_field "body" node ~err:Syntax_err.Interface_body in
    let children =
      [ mk_child_res mk_kwd_interface kwd_interface
      ; mk_child_res print_type_identifier name_field
      ; mk_child_opt print_type_parameters type_parameters_field
      ; mk_child_opt print_extends_type_clause extends_type_clause
      ; mk_child_res print_interface_body body_field
      ]
    in
    make_tree state node children

and print_interface_body state node = print_object_type state node

and print_extends_type_clause state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> print_error_node state node ~err:Syntax_err.Extends
  | _ ->
    let kwd_extends = first_child_named "extends" node ~err:Syntax_err.Extends
    and print state node =
      match get_name node with
      | "type_identifier" -> print_type_identifier state node
      | "nested_type_identifier" -> print_nested_type_identifier state node
      | "generic_type" -> print_generic_type state node
      | _ -> print_error_node state node ~err:Syntax_err.Type_expression
    in
    let children =
      mk_child_res mk_kwd_extends kwd_extends
      :: mk_children_list print (collect_named_children node)
    in
    make_tree state node children

(* Import alias *)

and print_import_alias state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> print_error_node state node ~err:Syntax_err.Import
  | _ ->
    let kwd_import = first_child_named "import" node ~err:Syntax_err.Import
    and lhs = child_ranked 1 node ~err:Syntax_err.Identifier
    and rhs = child_ranked 3 node ~err:Syntax_err.Identifier_or_path
    and sym_equal = first_child_named "=" node ~err:Syntax_err.Equal
    and print_rhs state node =
      match get_name node with
      | "identifier" -> print_identifier state node
      | "nested_identifier" -> print_nested_identifier state node
      | _ -> print_error_node state node ~err:Syntax_err.Identifier_or_path
    in
    let children =
      [ mk_child_res mk_kwd_import kwd_import
      ; mk_child_res print_identifier lhs
      ; mk_child_res mk_sym_equal sym_equal
      ; mk_child_res print_rhs rhs
      ]
    in
    make_tree state node children

(* Ambient declaration *)

and print_ambient_declaration state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> print_error_node state node ~err:Syntax_err.Declare
  | _ ->
    let kwd_declare = first_child_named "declare" node ~err:Syntax_err.Declare
    and fst_child = named_child_ranked 0 node ~err:Syntax_err.Block_or_ident_or_decl in
    let children =
      mk_child_res mk_kwd_declare kwd_declare
      ::
      (match get_name_res fst_child with
      | "statement_block" ->
        let kwd_global = first_child_named "global" node ~err:Syntax_err.Global in
        [ mk_child_res mk_kwd_global kwd_global
        ; mk_child_res print_statement_block fst_child
        ]
      | "property_identifier" ->
        let kwd_module = first_child_named "module" node ~err:Syntax_err.Module
        and type_child = child_ranked 5 node ~err:Syntax_err.Type_expression in
        [ mk_child_res mk_kwd_module kwd_module
        ; mk_child_res print_identifier fst_child
        ; mk_child_res print_type type_child
        ]
      | _ -> [ mk_child_res print_declaration fst_child ])
    in
    make_tree state node children

(* EXPRESSION

   The JavaScript tree-sitter grammar has the non-terminals
   "expression" and "primary_expression" be supertypes, that is,
   hidden rules. Therefore we have to match all the RHS of those
   non-terminals in [print_expression]. *)

and print_expression ?(comments = []) state (node : ts_tree) =
  match get_name node with
  (* Rest of "expression": *)
  | "glimmer_template" -> print_glimmer_template state node
  | "assignment_expression" -> print_assignment_expression state node
  | "augmented_assignment_expression" -> print_augmented_assignment_expression state node
  | "await_expression" -> print_await_expression state node
  | "unary_expression" -> print_unary_expression state node
  | "binary_expression" -> print_binary_expression ~comments state node
  | "ternary_expression" -> print_ternary_expression state node
  | "update_expression" -> print_update_expression state node
  | "new_expression" -> print_new_expression state node
  | "yield_expression" -> print_yield_expression state node
  | "as_expression" -> print_as_expression state node
  | "satisfies_expression" -> print_satisfies_expression state node
  | "instantiation_expression" -> print_instantiation_expression state node
  | "internal_module" -> print_internal_module ~comments state node
  | "type_assertion" -> print_type_assertion state node
  | _ -> print_primary_expression ~comments state node

and print_primary_expression ?(comments = []) state node =
  match get_name node with
  | "subscript_expression" -> print_subscript_expression state node
  | "member_expression" -> print_member_expression state node
  | "parenthesized_expression" -> print_parenthesized_expression state node
  | "identifier" -> print_identifier ~comments state node
  | "undefined" -> mk_kwd_undefined state node
  | "this" -> mk_kwd_this state node
  | "super" -> mk_kwd_super state node
  | "number" -> print_number ~comments state node
  | "string" -> print_string state node
  | "template_string" -> print_template_string state node
  | "regex" -> print_regex state node
  | "true" -> mk_kwd_true state node
  | "false" -> mk_kwd_false state node
  | "null" -> mk_kwd_null state node
  | "object" -> print_object state node
  | "array" -> print_array state node
  | "function_expression" -> print_function_expression state node
  | "arrow_function" -> print_arrow_function state node
  | "generator_function" -> print_generator_function state node
  | "class" -> print_class state node
  | "meta_property" -> print_meta_property state node
  | "call_expression" -> print_call_expression state node
  | "non_null_expression" -> print_non_null_expression state node
  | _ -> print_error_node state node ~err:Syntax_err.Expression

(* Glimmer template (not supported) *)

and print_glimmer_template state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    print_error_node state node ~err:Syntax_err.Glimmer_template
  | _ -> make_node state node

(* Assignment expression *)

and print_assignment_expression state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    print_error_node state node ~err:Syntax_err.Using_or_expression
  | _ ->
    let kwd_using = first_child_named_opt "using" node
    and left_field = child_with_field "left" node ~err:Syntax_err.Expression
    and sym_equal = first_child_named "=" node ~err:Syntax_err.Equal
    and right_field = child_with_field "right" node ~err:Syntax_err.Expression
    and print_left state node =
      match get_name node with
      | "ERROR" | "MISSING" | "NULL" ->
        print_error_node state node ~err:Syntax_err.Expression
      | "parenthesized_expression" -> print_parenthesized_expression state node
      | _ -> print_lhs_expression state node
    in
    let children =
      [ mk_child_opt mk_kwd_using kwd_using
      ; mk_child_res print_left left_field
      ; mk_child_res mk_sym_equal sym_equal
      ; mk_child_res print_expression right_field
      ]
    in
    make_tree state node children

(* Augmented assignment expression *)

and print_augmented_assignment_expression state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    print_error_node state node ~err:Syntax_err.LHS_of_augmented_assgmnt
  | _ ->
    let left_field = child_with_field "left" node ~err:Syntax_err.Expression
    and right_field = child_with_field "right" node ~err:Syntax_err.Expression
    and operator = child_with_field "operator" node ~err:Syntax_err.Augmented_assignment
    and print_left state node =
      (* "_augmented_assignment_lhs" is inlined here (hidden rule): *)
      match get_name node with
      | "member_expression" -> print_member_expression state node
      | "subscript_expression" -> print_subscript_expression state node
      | "identifier" -> print_identifier state node
      | "parenthesized_expression" -> print_parenthesized_expression state node
      | _ -> print_error_node state node ~err:Syntax_err.Expression
    and print_assignment state node =
      match get_name node with
      | "+=" -> mk_sym_plus_equal state node
      | "-=" -> mk_sym_minus_equal state node
      | "*=" -> mk_sym_mult_equal state node
      | "/=" -> mk_sym_div_equal state node
      | "%=" -> mk_sym_rem_equal state node
      | "^=" -> mk_sym_xor_equal state node
      | "&=" -> mk_sym_and_equal state node
      | "|=" -> mk_sym_or_equal state node
      | ">>=" -> mk_sym_right_shift_equal state node
      | ">>>=" -> mk_sym_unsigned_right_shift_equal state node
      | "<<=" -> mk_sym_left_shift_equal state node
      | "**=" -> mk_sym_unsigned_left_shift_equal state node
      | "&&=" -> mk_sym_conjunction_equal state node
      | "||=" -> mk_sym_disjunction_equal state node
      | "??=" -> mk_sym_non_null_equal state node
      | _ -> print_error_node state node ~err:Syntax_err.Augmented_assignment
    in
    let children =
      [ mk_child_res print_assignment operator
      ; mk_child_res print_left left_field
      ; mk_child_res print_expression right_field
      ]
    in
    make_tree state node children

(* Await expression *)

and print_await_expression state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> print_error_node state node ~err:Syntax_err.Await
  | _ ->
    let kwd_await = first_child_named "await" node ~err:Syntax_err.Await
    and expression = child_ranked 1 node ~err:Syntax_err.Expression in
    let children =
      [ mk_child_res mk_kwd_await kwd_await; mk_child_res print_expression expression ]
    in
    make_tree state node children

(* Unary expression *)

and print_unary_expression state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    print_error_node state node ~err:Syntax_err.Unary_operator
  | _ ->
    let operator_field = child_with_field "operator" node ~err:Syntax_err.Unary_operator
    and argument_field = child_with_field "argument" node ~err:Syntax_err.Expression
    and print_unary_operator state node =
      match get_name node with
      | "!" -> mk_sym_bang state node
      | "~" -> mk_sym_tilde state node
      | "-" -> mk_sym_minus state node
      | "+" -> mk_sym_plus state node
      | "typeof" -> mk_kwd_typeof state node
      | "void" -> mk_kwd_void state node
      | "delete" -> mk_kwd_delete state node
      | _ -> print_error_node state node ~err:Syntax_err.Unary_operator
    in
    let children =
      [ mk_child_res print_unary_operator operator_field
      ; mk_child_res print_expression argument_field
      ]
    in
    make_tree state node children

(* Binary expression *)

and print_binary_expression ?(comments = []) state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> print_error_node state node ~err:Syntax_err.Expression
  | _ ->
    let comments = comments @ prev_comments node
    and left_field = child_with_field "left" node ~err:Syntax_err.Expression
    and right_field = child_with_field "right" node ~err:Syntax_err.Expression
    and operator = child_with_field "operator" node ~err:Syntax_err.Binary_operator in
    let print_left state node =
      match get_name node with
      | "ERROR" | "MISSING" | "NULL" ->
        print_error_node state node ~err:Syntax_err.Expression
      | "private_property_identifier" -> print_identifier ~comments state node
      | _ -> print_expression ~comments state node
    and print_bin_operator state node =
      match get_name node with
      | "&&" -> mk_sym_conjunction state node
      | "||" -> mk_sym_disjunction state node
      | ">>" -> mk_sym_right_shift state node
      | ">>>" -> mk_sym_unsigned_right_shift state node
      | "<<" -> mk_sym_left_shift state node
      | "&" -> mk_sym_and state node
      | "^" -> mk_sym_xor state node
      | "|" -> mk_sym_or state node
      | "+" -> mk_sym_plus state node
      | "-" -> mk_sym_minus state node
      | "*" -> mk_sym_asterisk state node
      | "/" -> mk_sym_div state node
      | "%" -> mk_sym_rem state node
      | "**" -> mk_sym_exponent state node
      | "<" -> mk_sym_lower_than state node
      | "<=" -> mk_sym_lower_than_or_equal state node
      | "==" -> mk_sym_equal state node
      | "===" -> mk_sym_no_conv_equal state node
      | "!=" -> mk_sym_different state node
      | "!==" -> mk_sym_no_conv_different state node
      | ">=" -> mk_sym_greater_than_or_equal state node
      | ">" -> mk_sym_greater_than state node
      | "??" -> mk_sym_non_null state node
      | "instanceof" -> mk_kwd_instanceof state node
      | "in" -> mk_kwd_in state node
      | _ -> print_error_node state node ~err:Syntax_err.Binary_operator
    in
    let children =
      [ mk_child_res print_left left_field
      ; mk_child_res print_bin_operator operator
      ; mk_child_res print_expression right_field
      ]
    in
    make_tree state node children

(* Ternary expression *)

and print_ternary_expression state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> print_error_node state node ~err:Syntax_err.Expression
  | _ ->
    let condition_field = child_with_field "condition" node ~err:Syntax_err.Expression
    and consequence_field = child_with_field "consequence" node ~err:Syntax_err.Expression
    and alternative_field =
      child_with_field "alternative" node ~err:Syntax_err.Expression
    in
    let children =
      [ mk_child_res print_expression condition_field
      ; mk_child_res print_expression consequence_field
      ; mk_child_res print_expression alternative_field
      ]
    in
    make_tree state node children

(* Update expression *)

and print_update_expression state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> print_error_node state node ~err:Syntax_err.Expression
  | _ ->
    let argument_field = child_with_field "argument" node ~err:Syntax_err.Expression
    and first_child = child_ranked 0 node ~err:Syntax_err.Incr_or_decr_or_expr in
    let children =
      match get_name_res first_child with
      | "++" ->
        (* Prefix *)
        [ mk_child_res mk_sym_increment first_child
        ; mk_child_res print_expression argument_field
        ]
      | "--" ->
        (* Prefix *)
        [ mk_child_res mk_sym_decrement first_child
        ; mk_child_res print_expression argument_field
        ]
      | _ ->
        let snd_child = child_ranked 1 node ~err:Syntax_err.Increment_or_decrement in
        (match get_name_res snd_child with
        | "++" ->
          (* Postfix *)
          [ mk_child_res print_expression argument_field
          ; mk_child_res mk_sym_increment snd_child
          ]
        | "--" ->
          (* Postfix *)
          [ mk_child_res print_expression argument_field
          ; mk_child_res mk_sym_decrement snd_child
          ]
        | _ -> [] (* Should not happen. *))
    in
    make_tree state node children

(* New expression

   Note that the constructor field is a primary expression, but
   "primary_expression" is a supertype, that is, a hidden rule. We
   assume it is an "expression", since primary expressions are a subset
   of them. *)

and print_new_expression state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> print_error_node state node ~err:Syntax_err.New
  | _ ->
    let kwd_new = first_child_named "new" node ~err:Syntax_err.New
    and constructor_field = child_with_field "constructor" node ~err:Syntax_err.Expression
    and type_arguments_field = child_with_field_opt "type_arguments" node
    and arguments_field = child_with_field_opt "arguments" node in
    let children =
      [ mk_child_res mk_kwd_new kwd_new
      ; mk_child_res print_expression constructor_field
      ; mk_child_opt print_type_arguments type_arguments_field
      ; mk_child_opt print_arguments arguments_field
      ]
    in
    make_tree state node children

(* Yield expression *)

and print_yield_expression state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> print_error_node state node ~err:Syntax_err.Yield
  | _ ->
    let kwd_yield = first_child_named "yield" node ~err:Syntax_err.Yield in
    (match child_ranked_opt 1 node with
    | None -> make_unary_res state node mk_kwd_yield kwd_yield
    | Some snd_child ->
      let snd_child =
        match get_name snd_child with
        | "*" -> child_ranked 2 node ~err:Syntax_err.Asterisk
        | _ -> Ok snd_child
      in
      let children =
        [ mk_child_res mk_kwd_yield kwd_yield; mk_child_res print_expression snd_child ]
      in
      make_tree state node children)

(* As-expression *)

and print_as_expression state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> print_error_node state node ~err:Syntax_err.As
  | _ ->
    let expression = child_ranked 0 node ~err:Syntax_err.Expression
    and kwd_as = first_child_named "as" node ~err:Syntax_err.As
    and as_what = child_ranked 2 node ~err:Const_or_type
    and print_as state node =
      match get_name node with
      | "ERROR" | "MISSING" | "NULL" ->
        print_error_node state node ~err:Syntax_err.Const_or_type
      | "const" -> mk_kwd_const state node
      | _ -> print_type state node
    in
    let children =
      [ mk_child_res print_expression expression
      ; mk_child_res mk_kwd_as kwd_as
      ; mk_child_res print_as as_what
      ]
    in
    make_tree state node children

(* Statisfies-expression *)

and print_satisfies_expression state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> print_error_node state node ~err:Syntax_err.Satisfies
  | _ ->
    let expression = child_ranked 0 node ~err:Syntax_err.Expression
    and kwd_satisfies = first_child_named "satisfies" node ~err:Syntax_err.Satisfies
    and type_child = child_ranked 2 node ~err:Syntax_err.Type_expression in
    let children =
      [ mk_child_res print_expression expression
      ; mk_child_res mk_kwd_satisfies kwd_satisfies
      ; mk_child_res print_type type_child
      ]
    in
    make_tree state node children

(* Instantiation expression *)

and print_instantiation_expression state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> print_error_node state node ~err:Syntax_err.Expression
  | _ ->
    let expression = named_child_ranked 0 node ~err:Syntax_err.Expression
    and type_arguments_field =
      child_with_field "type_arguments" node ~err:Syntax_err.Type_arguments
    in
    let children =
      [ mk_child_res print_expression expression
      ; mk_child_res print_type_arguments type_arguments_field
      ]
    in
    make_tree state node children

(* Type assertion *)

and print_type_assertion state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    print_error_node state node ~err:Syntax_err.Type_arguments
  | _ ->
    let type_arguments = named_child_ranked 0 node ~err:Syntax_err.Type_arguments
    and expression = named_child_ranked 1 node ~err:Syntax_err.Expression in
    let children =
      [ mk_child_res print_type_arguments type_arguments
      ; mk_child_res print_expression expression
      ]
    in
    make_tree state node children

(* Subscript expression (see [print_member_expression]) *)

and print_subscript_expression state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> print_error_node state node ~err:Syntax_err.Expression
  | _ ->
    let object_field = child_with_field "object" node ~err:Syntax_err.Expression
    and optional_chain_field = child_with_field_opt "optional_chain" node
    and index_field = child_with_field "index" node ~err:Syntax_err.Expression
    and sym_lbracket = first_child_named "[" node ~err:Syntax_err.Left_bracket
    and sym_rbracket = first_child_named "]" node ~err:Syntax_err.Right_bracket
    and print_chain state node =
      match get_name node with
      | "optional_chain" -> make_node state node
      | _ -> print_error_node state node ~err:Syntax_err.Optional_chain
    and print_index state node =
      match get_name node with
      | "ERROR" | "MISSING" | "NULL" ->
        print_error_node state node ~err:Syntax_err.Index_expression
      | "sequence_expression" -> print_sequence_expression state node
      | _ -> print_expression state node
    in
    let children =
      [ mk_child_res print_expression object_field
      ; mk_child_opt print_chain optional_chain_field
      ; mk_child_res mk_sym_lbracket sym_lbracket
      ; mk_child_res print_index index_field
      ; mk_child_res mk_sym_rbracket sym_rbracket
      ]
    in
    make_tree state node children

(* Member expression *)

and print_member_expression state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    print_error_node state node ~err:Syntax_err.Member_expression
  | _ ->
    let object_field = child_with_field "object" node ~err:Syntax_err.Expression
    and optional_chain_field = child_with_field_opt "optional_chain" node
    and property_field =
      child_with_field "property" node ~err:Syntax_err.Property_identifier
    and print_object state node =
      match get_name node with
      | "ERROR" | "MISSING" | "NULL" ->
        print_error_node state node ~err:Syntax_err.Object_denotation
      | "import" -> mk_kwd_import state node
      | _ -> print_expression state node
    and print_selector state = function
      | None -> () (* "." *)
      | Some node ->
        (* "?." *)
        mk_sym_optional_chain state node
    in
    let children =
      [ mk_child_res print_object object_field
      ; mk_child print_selector optional_chain_field
      ; mk_child_res print_property_field property_field
      ]
    in
    make_tree state node children

(* Parenthesised expression *)

and print_parenthesized_expression ?(comments = []) state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    print_error_node state node ~err:Syntax_err.Parenthesized_expression
  | _ ->
    let comments = comments @ prev_comments node in
    let opening = first_child_named "(" node ~err:Syntax_err.Left_parenthesis
    and closing = first_child_named ")" node ~err:Syntax_err.Right_parenthesis
    and second_child = child_ranked 1 node ~err:Syntax_err.Expression
    and type_field = child_with_field_opt "type" node in
    let children =
      match type_field with
      | Some type_field ->
        [ mk_child_res print_expression second_child
        ; mk_child print_type_annotation type_field
        ]
      | None ->
        let print state node =
          match get_name node with
          | "sequence_expression" -> print_sequence_expression state node
          | _ -> print_expression state node
        in
        [ mk_child_res print second_child ]
    in
    let children =
      (mk_child_res (mk_sym_lparen ~comments) opening :: children)
      @ [ mk_child_res mk_sym_rparen closing ]
    in
    make_tree state node children

(* Template strings *)

and print_template_string ?(comments = []) state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    print_error_node state node ~err:Syntax_err.Template_string
  | _ ->
    let opening = child_ranked 0 node ~err:Syntax_err.Backquote in
    let closing = last_child node ~err:Syntax_err.Backquote in
    let raw_children = collect_named_children node in
    let print ?comments state node =
      match get_name node with
      | "string_fragment" -> make_node ?comments state node
      | "escape_sequence" -> make_node ?comments state node
      | "template_substitution" -> make_node ?comments state node
      | _ -> print_error_node state node ~err:Template_string
    in
    let children =
      (mk_child_res (mk_sym_backquote ~comments) opening
      :: mk_children_list print raw_children)
      @ [ mk_child_res mk_sym_backquote closing ]
    in
    make_tree state node children

(* Object *)

and print_object state node =
  let print state node =
    match get_name node with
    | "pair" -> print_pair state node
    | "spread_element" -> print_spread_element state node
    | "method_definition" -> print_method_definition state node
    | "shorthand_property_identifier" ->
      print_shorthand_property_identifier_pattern state node
    | _ -> print_error_node state node ~err:Syntax_err.Object_field
  in
  print_braces state node print ~err:Syntax_err.Object_expression

(* Pairs *)

and print_pair state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    print_error_node state node ~err:Syntax_err.Key_value_pair
  | _ ->
    let key_field = child_with_field "key" node ~err:Syntax_err.Property_name
    and value_field = child_with_field "value" node ~err:Syntax_err.Expression
    and sym_colon = first_child_named ":" node ~err:Syntax_err.Colon in
    let children =
      [ mk_child_res print_property_name key_field
      ; mk_child_res mk_sym_colon sym_colon
      ; mk_child_res print_expression value_field
      ]
    in
    make_tree state node children

(* Array (expression) *)

and print_array state node =
  print_brackets state node print_array_cell ~err:Syntax_err.Array

and print_array_cell state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> print_error_node state node ~err:Syntax_err.Array_cell
  | "spread_element" -> print_spread_element state node
  | _ -> print_expression state node (* Hidden *)

and print_spread_element state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> print_error_node state node ~err:Syntax_err.Spread
  | _ ->
    let sym_ellipsis = first_child_named "..." node ~err:Syntax_err.Ellipsis
    and expr_node = named_child_ranked 0 node ~err:Syntax_err.Expression in
    let children =
      [ mk_child_res mk_sym_ellipsis sym_ellipsis
      ; mk_child_res print_expression expr_node
      ]
    in
    make_tree state node children

(* Function (expression) *)

and print_function_expression state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    print_error_node state node ~err:Syntax_err.Function_expression
  | _ ->
    let kwd_async = first_child_named_opt "async" node
    and kwd_function = first_child_named "function" node ~err:Syntax_err.Function
    and name_field = child_with_field_opt "name" node
    (* "_call_signature" inlined: *)
    and type_parameters_field = child_with_field_opt "type_parameters" node
    and parameters_field = child_with_field "parameters" node ~err:Syntax_err.Parameters
    and return_type_field = child_with_field_opt "return_type" node
    (* "statement_block" *)
    and body_field = child_with_field "body" node ~err:Syntax_err.Block in
    let children =
      [ mk_child_opt mk_kwd_async kwd_async
      ; mk_child_res mk_kwd_function kwd_function
      ; mk_child_opt print_identifier name_field
      ; mk_child_opt print_type_parameters type_parameters_field
      ; mk_child_res print_formal_parameters parameters_field
      ; mk_child_opt print_return_type return_type_field
      ; mk_child_res print_statement_block body_field
      ]
    in
    make_tree state node children

(* Arrow function *)

and print_arrow_function state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    print_error_node state node ~err:Syntax_err.Arrow_function
  | _ ->
    let kwd_async = first_child_named_opt "async" node
    and parameter_field = child_with_field_opt "parameter" node
    and sym_arrow = first_child_named "=>" node ~err:Syntax_err.Arrow
    and body_field = child_with_field "body" node ~err:Syntax_err.Block_or_expression in
    let children =
      match parameter_field with
      | Some parameter_field ->
        [ mk_child_opt mk_kwd_async kwd_async
        ; mk_child print_identifier parameter_field
        ; mk_child_res mk_sym_arrow sym_arrow
        ; mk_child_res print_arrow_function_body body_field
        ]
      | None ->
        (* "_call_signature" inlined: *)
        let type_parameters_field = child_with_field_opt "type_parameters" node
        and parameters_field = child_with_field "parameters" node ~err:Parameters
        and return_type_field = child_with_field_opt "return_type" node in
        [ mk_child_opt mk_kwd_async kwd_async
        ; mk_child_opt print_type_parameters type_parameters_field
        ; mk_child_res print_formal_parameters parameters_field
        ; mk_child_opt print_return_type return_type_field
        ; mk_child_res mk_sym_arrow sym_arrow
        ; mk_child_res print_arrow_function_body body_field
        ]
    in
    make_tree state node children

and print_arrow_function_body state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    print_error_node state node ~err:Syntax_err.Block_or_expression
  | "statement_block" -> print_statement_block state node
  | _ -> print_expression state node (* Hidden *)

(* Generator function *)

and print_generator_function state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    print_error_node state node ~err:Syntax_err.Generator_function
  | _ ->
    let kwd_async = first_child_named_opt "async" node
    and kwd_function = first_child_named "function" node ~err:Syntax_err.Function
    and sym_asterisk = first_child_named "*" node ~err:Syntax_err.Asterisk
    and name_field = child_with_field_opt "name" node
    (* "_call_signature" inlined: *)
    and type_parameters_field = child_with_field_opt "type_parameters" node
    and parameters_field = child_with_field "parameters" node ~err:Syntax_err.Parameters
    and return_type_field = child_with_field_opt "return_type" node
    (* "statement_block" *)
    and body_field = child_with_field "body" node ~err:Syntax_err.Block in
    let children =
      [ mk_child_opt mk_kwd_async kwd_async
      ; mk_child_res mk_kwd_function kwd_function
      ; mk_child_res mk_sym_asterisk sym_asterisk
      ; mk_child_opt print_identifier name_field
      ; mk_child_opt print_type_parameters type_parameters_field
      ; mk_child_res print_formal_parameters parameters_field
      ; mk_child_opt print_return_type return_type_field
      ; mk_child_res print_statement_block body_field
      ]
    in
    make_tree state node children

(* Class *)

and print_class state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    print_error_node state node ~err:Syntax_err.Class_expression
  | _ ->
    let decorators = children_named "decorator" node
    and kwd_class = first_child_named "class" node ~err:Syntax_err.Class
    and name_field = child_with_field_opt "name" node
    and type_parameters_field = child_with_field_opt "type_parameters" node
    and heritage_child = first_child_named_opt "class_heritage" node
    and body_field = child_with_field "body" node ~err:Syntax_err.Class_body in
    let children =
      mk_children_list print_decorator decorators
      @ [ mk_child_res mk_kwd_class kwd_class
        ; mk_child_opt print_type_identifier name_field
        ; mk_child_opt print_type_parameters type_parameters_field
        ; mk_child_opt print_class_heritage heritage_child
        ; mk_child_res print_class_body body_field
        ]
    in
    make_tree state node children

and print_class_heritage state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    print_error_node state node ~err:Syntax_err.Extends_or_implements
  | _ ->
    let children =
      match first_child_named_opt "extends_clause" node with
      | Some extends_clause ->
        let implements_clause = first_child_named_opt "implements_clause" node in
        [ mk_child print_extends_clause extends_clause
        ; mk_child_opt print_implements_clause implements_clause
        ]
      | None ->
        (* [implements_clause] is never [None]. *)
        let implements_clause = first_child_named_opt "implements_clause" node in
        [ mk_child_opt print_implements_clause implements_clause ]
    in
    make_tree state node children

and print_implements_clause state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    print_error_node state node ~err:Syntax_err.Implements_clause
  | _ ->
    let kwd_implements = first_child_named "implements" node ~err:Syntax_err.Implements
    and named_children = collect_named_children node in
    let children =
      mk_child_res mk_kwd_implements kwd_implements
      :: mk_children_list print_type named_children
    in
    make_tree state node children

and print_extends_clause state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    print_error_node state node ~err:Syntax_err.Extends_clause
  | _ ->
    let kwd_extends = first_child_named "extends" node ~err:Syntax_err.Extends
    and children =
      match collect_children node with
      | [] -> []
      | _extends :: clauses -> clauses
    in
    let not_comma child = String.(get_name child <> ",") in
    let children = List.filter children ~f:not_comma in
    let rec pair_up acc = function
      | value :: snd :: nodes ->
        if String.equal (get_name snd) "type_arguments"
        then pair_up ((value, Some snd) :: acc) nodes
        else pair_up ((value, None) :: acc) (snd :: nodes)
      | [ value ] -> List.rev ((value, None) :: acc)
      | [] -> List.rev acc
    in
    let pairs = pair_up [] children in
    let mk_children (value, type_arguments_opt) acc =
      let value_child = mk_child print_expression value in
      match type_arguments_opt with
      | None -> value_child :: acc
      | Some type_arguments ->
        value_child :: mk_child print_type_arguments type_arguments :: acc
    in
    let children =
      mk_child_res mk_kwd_extends kwd_extends
      :: List.fold_right ~f:mk_children pairs ~init:[]
    in
    make_tree state node children

and print_class_body ?(comments = []) state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> print_error_node state node ~err:Syntax_err.Class_body
  | _ ->
    let comments = comments @ prev_comments node in
    let opening = first_child_named "{" node ~err:Syntax_err.Left_brace
    and closing = first_child_named "}" node ~err:Syntax_err.Right_brace
    and named_children = collect_named_children node in
    let pair (decorators, acc) child =
      match get_name child with
      | "decorator" -> child :: decorators, acc
      | _ -> [], (List.rev decorators, child) :: acc
    in
    let _, pairs = List.fold_left ~f:pair ~init:([], []) named_children in
    let pairs = List.rev pairs in
    let children =
      (mk_child_res (mk_sym_lbracket ~comments) opening
      :: mk_children_list print_class_member pairs)
      @ [ mk_child_res mk_sym_rbracket closing ]
    in
    make_tree state node children

and print_class_member state (decorators, node) =
  match get_name node with
  | "method_definition" ->
    List.iter ~f:(print_decorator state) decorators;
    print_method_definition state node
  | "method_signature" -> print_method_signature state node
  | "class_static_block" -> print_class_static_block state node
  | "abstract_method_signature" -> print_abstract_method_signature state node
  | "index_signature" -> print_index_signature state node
  | "public_field_definition" -> print_public_field_definition state node
  | _ -> print_error_node state node ~err:Syntax_err.Class_member

and print_method_definition state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    print_error_node state node ~err:Syntax_err.Method_definition
  | _ ->
    let accessibility_modifier = first_child_named_opt "accessibility_modifier" node
    and kwd_static = first_child_named_opt "static" node
    and override_modifier = first_child_named_opt "override_modifier" node
    and kwd_readonly = first_child_named_opt "readonly" node
    and kwd_async = first_child_named_opt "async" node
    and kwd_set = first_child_named_opt "set" node
    and kwd_get = first_child_named_opt "get" node
    and sym_asterisk = first_child_named_opt "*" node
    and name_field = child_with_field "name" node ~err:Syntax_err.Property_name
    and qmark = first_child_named_opt "?" node
    (* "_call_signature" inlined: *)
    and type_parameters_field = child_with_field_opt "type_parameters" node
    and parameters_field = child_with_field "parameters" node ~err:Syntax_err.Parameters
    and return_type_field = child_with_field_opt "return_type" node
    (* "statement_block" *)
    and body_field = child_with_field "body" node ~err:Syntax_err.Block in
    let children =
      [ mk_child_opt print_accessibility_modifier accessibility_modifier
      ; mk_child_opt mk_kwd_static kwd_static
      ; mk_child_opt print_override_modifier override_modifier
      ; mk_child_opt mk_kwd_readonly kwd_readonly
      ; mk_child_opt mk_kwd_async kwd_async
      ; mk_child_opt mk_kwd_set kwd_set
      ; mk_child_opt mk_kwd_get kwd_get
      ; mk_child_opt mk_sym_asterisk sym_asterisk
      ; mk_child_res print_property_name name_field
      ; mk_child_opt make_node qmark
      ; mk_child_opt print_type_parameters type_parameters_field
      ; mk_child_res print_formal_parameters parameters_field
      ; mk_child_opt print_return_type return_type_field
      ; mk_child_res print_statement_block body_field
      ]
    in
    make_tree state node children

and print_class_static_block state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    print_error_node state node ~err:Syntax_err.Static_block
  | _ ->
    let kwd_static = first_child_named "static" node ~err:Syntax_err.Static
    and body_field = child_with_field "body" node ~err:Syntax_err.Block in
    let children =
      [ mk_child_res mk_kwd_static kwd_static
      ; mk_child_res print_statement_block body_field
      ]
    in
    make_tree state node children

and print_abstract_method_signature state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    print_error_node state node ~err:Syntax_err.Abstract_method_signature
  | _ ->
    let accessibility_modifier = first_child_named_opt "accessibility_modifier" node
    and kwd_abstract = first_child_named_opt "abstract" node
    and override_modifier = first_child_named_opt "override_modifier" node
    and kwd_set = first_child_named_opt "set" node
    and kwd_get = first_child_named_opt "get" node
    and sym_asterisk = first_child_named_opt "*" node
    and name_field = child_with_field "name" node ~err:Syntax_err.Property_name
    and sym_qmark = first_child_named_opt "?" node
    (* "_call_signature" inlined: *)
    and type_parameters_field = child_with_field_opt "type_parameters" node
    and parameters_field = child_with_field "parameters" node ~err:Syntax_err.Parameters
    and return_type_field = child_with_field_opt "return_type" node in
    let children =
      [ mk_child_opt print_accessibility_modifier accessibility_modifier
      ; mk_child_opt mk_kwd_abstract kwd_abstract
      ; mk_child_opt print_override_modifier override_modifier
      ; mk_child_opt mk_kwd_set kwd_set
      ; mk_child_opt mk_kwd_get kwd_get
      ; mk_child_opt make_node sym_asterisk
      ; mk_child_res print_property_name name_field
      ; mk_child_opt mk_sym_qmark sym_qmark
      ; mk_child_opt print_type_parameters type_parameters_field
      ; mk_child_res print_formal_parameters parameters_field
      ; mk_child_opt print_return_type return_type_field
      ]
    in
    make_tree state node children

and print_public_field_definition state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    print_error_node state node ~err:Syntax_err.Public_field_definition
  | _ ->
    let decorators = children_named "decorator" node
    and accessibility_modifier = first_child_named_opt "accessibility_modifier" node
    and override_modifier = first_child_named_opt "override_modifier" node
    and kwd_declare = first_child_named_opt "declare" node
    and kwd_static = first_child_named_opt "static" node
    and kwd_readonly = first_child_named_opt "readonly" node
    and kwd_accessor = first_child_named_opt "accessor" node
    and kwd_abstract = first_child_named_opt "abstract" node
    and name_field = child_with_field "name" node ~err:Syntax_err.Property_name
    and type_field = child_with_field_opt "type" node
    and sym_qmark = first_child_named_opt "?" node
    and sym_emark = first_child_named_opt "!" node in
    let children =
      mk_children_list print_decorator decorators
      @ [ mk_child_opt mk_kwd_declare kwd_declare
        ; mk_child_opt print_accessibility_modifier accessibility_modifier
        ; mk_child_opt print_override_modifier override_modifier
        ; mk_child_opt mk_kwd_static kwd_static
        ; mk_child_opt mk_kwd_readonly kwd_readonly
        ; mk_child_opt mk_kwd_accessor kwd_accessor
        ; mk_child_opt mk_kwd_abstract kwd_abstract
        ; mk_child_res print_property_name name_field
        ; mk_child_opt mk_sym_qmark sym_qmark
        ; mk_child_opt mk_sym_emark sym_emark
        ; mk_child_opt print_type_annotation type_field
        ]
      @ mk_child_initializer_opt node (* "_initializer" inlined *)
    in
    make_tree state node children

(* Meta-property *)

and print_meta_property state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    print_error_node state node ~err:Syntax_err.Meta_property
  | _ ->
    let fst_child = child_ranked 0 node ~err:Syntax_err.New_or_import
    and snd_child = child_ranked 2 node ~err:Syntax_err.Target_or_meta in
    let children =
      [ mk_child_res (make_kwd ~err:Syntax_err.New_or_import) fst_child
      ; mk_child_res (make_kwd ~err:Syntax_err.Target_or_meta) snd_child
      ]
    in
    make_tree state node children

(* Call expression *)

and print_call_expression state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    print_error_node state node ~err:Syntax_err.Call_expression
  | _ ->
    let function_field = child_with_field "function" node ~err:Syntax_err.Expression
    and member_selection = first_child_named_opt "?." node
    and type_arguments_field = child_with_field_opt "type_arguments" node
    and arguments_field = child_with_field "arguments" node ~err:Syntax_err.Arguments in
    let children =
      match member_selection with
      | None ->
        let print_function state node =
          match get_name node with
          | "ERROR" | "MISSING" | "NULL" ->
            print_error_node state node ~err:Syntax_err.Import_or_expression
          | "import" -> mk_kwd_import state node
          | _ -> print_expression state node
        and print_arguments state node =
          match get_name node with
          | "template_string" -> print_template_string state node
          | _ -> print_arguments state node
        in
        [ mk_child_res print_function function_field
        ; mk_child_opt print_type_arguments type_arguments_field
        ; mk_child_res print_arguments arguments_field
        ]
      | Some _ ->
        [ mk_child_res print_primary_expression function_field
        ; mk_child_opt print_type_arguments type_arguments_field
        ; mk_child_res print_arguments arguments_field
        ]
    in
    make_tree state node children

and print_type_arguments state node =
  print_chevrons state node print_type ~err:Syntax_err.Type_arguments

and print_arguments state node =
  print_parens state node print_argument ~err:Syntax_err.Arguments

and print_argument state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> print_error_node state node ~err:Syntax_err.Argument
  | "spread_element" -> print_spread_element state node
  | _ -> print_expression state node (* Hidden *)

(* Non-null expression *)

and print_non_null_expression state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    print_error_node state node ~err:Syntax_err.Non_null_expression
  | _ ->
    let child = named_child_ranked 0 node ~err:Syntax_err.Non_null_expression in
    make_unary_res state node print_expression child

(* Sequence expression *)

and print_sequence_expression ?(comments = []) state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> print_error_node state node ~err:Syntax_err.Expression
  | _ -> tree_of_named_children ~comments state node print_expression

(* TYPE

   The non-terminals "type" and "primary_type" are supertypes in the
   TypeScript grammar, which means that they are hidden rules. *)

and print_type state node =
  match get_name node with
  | "function_type" -> print_function_type state node
  | "readonly_type" -> print_readonly_type state node
  | "constructor_type" -> print_constructor_type state node
  | "infer_type" -> print_infer_type state node
  (* A couple of aliases *)
  | "member_expression" ->
    print_type_query_member_expression_in_type_annotation state node
  | "call_expression" -> print_type_query_call_expression_in_type_annotation state node
  (* "primary_type" is hidden *)
  | _ -> print_primary_type state node

(* Type queries in type annotations (expressions) *)

and print_type_query_member_expression_in_type_annotation state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    print_error_node state node ~err:Syntax_err.Member_or_call
  | _ ->
    let object_field = child_with_field "object" node ~err:Syntax_err.Member_or_call
    and selector = first_child_named "." node ~err:Syntax_err.Dot
    and property_field =
      child_with_field "property" node ~err:Syntax_err.Property_identifier
    and print_object_field state node =
      match get_name node with
      | "import" -> mk_kwd_import state node
      | "member_expression" ->
        print_type_query_member_expression_in_type_annotation state node
      | "call_expression" ->
        print_type_query_call_expression_in_type_annotation state node
      | _ -> print_error_node state node ~err:Syntax_err.Object_field
    in
    let children =
      [ mk_child_res print_object_field object_field
      ; mk_child_res mk_sym_dot selector
      ; mk_child_res print_type_query_property property_field
      ]
    in
    make_tree state node children

and print_type_query_property state node =
  match get_name node with
  | "property_identifier" -> print_identifier state node
  | "private_property_identifier" -> print_identifier state node
  | _ -> print_error_node state node ~err:Syntax_err.Property_identifier

and print_type_query_call_expression_in_type_annotation state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    print_error_node state node ~err:Syntax_err.Member_expression
  | _ ->
    let function_field =
      child_with_field "function" node ~err:Syntax_err.Member_expression
    and arguments_field = child_with_field "arguments" node ~err:Syntax_err.Arguments
    and print_function_field state node =
      match get_name node with
      | "import" -> mk_kwd_import state node
      | "member_expression" ->
        print_type_query_member_expression_in_type_annotation state node
      | _ -> print_error_node state node ~err:Syntax_err.Member_expression
    in
    let children =
      [ mk_child_res print_function_field function_field
      ; mk_child_res print_arguments arguments_field
      ]
    in
    make_tree state node children

(* Primary type *)

and print_primary_type state node =
  match get_name node with
  | "parenthesized_type" -> print_parenthesized_type state node
  | "predefined_type" -> print_predefined_type state node
  | "type_identifier" -> print_type_identifier state node (* Including "const" *)
  | "nested_type_identifier" -> print_nested_type_identifier state node
  | "generic_type" -> print_generic_type state node
  | "object_type" -> print_object_type state node
  | "array_type" -> print_array_type state node
  | "tuple_type" -> print_tuple_type state node
  | "flow_maybe_type" -> print_flow_maybe_type state node
  | "type_query" -> print_type_query state node
  | "index_type_query" -> print_index_type_query state node
  | "this_type" -> mk_kwd_this state node
  | "existential_type" -> print_existential_type state node
  | "literal_type" -> print_literal_type state node
  | "lookup_type" -> print_lookup_type state node
  | "conditional_type" -> print_conditional_type state node
  | "template_literal_type" -> print_template_literal_type state node
  | "intersection_type" -> print_intersection_type state node
  | "union_type" -> print_union_type state node
  | _ -> print_error_node state node ~err:Syntax_err.Type_expression

(* Flow maybe type

   flow_maybe_type: $ => prec.right(seq('?', $.primary_type))
 *)

and print_flow_maybe_type state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> print_error_node state node ~err:Syntax_err.Type
  | _ ->
    let child = named_child_ranked 0 node ~err:Syntax_err.Type_expression in
    make_unary_res state node print_primary_type child

(* Type identifier *)

and print_type_identifier ?comments state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> print_error_node state node ~err:Syntax_err.Type_name
  | _ -> print_identifier ?comments state node

(* Parenthesized type *)

and print_parenthesized_type state node =
  print_parens state node print_type ~err:Syntax_err.Parenthesized_type

(* Predefined type *)

and print_predefined_type ?(comments = []) state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    print_error_node state node ~err:Syntax_err.Predefined_type
  | _ ->
    let comments = comments @ prev_comments node in
    (match collect_children node with
    | [] -> () (* Should not happen *)
    | child :: _ ->
      (* The tree-sitter parser for TypeScript has a bug: a child node
        "unique symbol" occurs repeated, for some mysterious
        reason. This case of the pattern matching is a hack to work
        around the issue. For reference, here is the production:

        predefined_type: _ => choice(
        ...
        alias(seq('unique', 'symbol'), 'unique symbol')
        ...)
      *)
      let print state node =
        match get_name node with
        | "any" -> mk_kwd_any ~comments state node
        | "number" -> mk_kwd_number ~comments state node
        | "boolean" -> mk_kwd_boolean ~comments state node
        | "string" -> mk_kwd_string ~comments state node
        | "symbol" -> mk_kwd_symbol ~comments state node
        | "unique symbol" -> mk_kwd_unique_symbol ~comments state node
        | "void" -> mk_kwd_void ~comments state node
        | "unknown" -> mk_kwd_unknown ~comments state node
        | "never" -> mk_kwd_never ~comments state node
        | "object" -> mk_kwd_object ~comments state node
        | _ -> print_error_node state node ~err:Syntax_err.Predefined_type
      in
      make_unary state node print child)

(* Nested type identifier *)

and print_nested_type_identifier ?comments state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    print_error_node state node ~err:Syntax_err.Nested_type_identifier
  | _ ->
    let module_field = child_with_field "module" node ~err:Syntax_err.Identifier_or_path
    and name_field = child_with_field "name" node ~err:Syntax_err.Type_name
    and print_module_field state node =
      match get_name node with
      | "identifier" -> print_identifier ?comments state node
      | "nested_identifier" -> print_nested_identifier ?comments state node
      | _ -> print_error_node state node ~err:Syntax_err.Identifier_or_path
    in
    let children =
      [ mk_child_res print_module_field module_field
      ; mk_child_res print_type_identifier name_field
      ]
    in
    make_tree state node children

(* Nested identifier *)

and print_nested_identifier ?comments state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    print_error_node state node ~err:Syntax_err.Identifier_or_member
  | _ ->
    let object_field = child_with_field "object" node ~err:Syntax_err.Identifier_or_member
    and property_field =
      child_with_field "property" node ~err:Syntax_err.Property_identifier
    and print_object_field state node =
      match get_name node with
      | "identifier" -> print_identifier ?comments state node
      | "member_expression" -> print_nested_identifier ?comments state node
      | _ -> print_error_node state node ~err:Syntax_err.Identifier_or_member
    and print_property_field state node =
      match get_name node with
      | "property_identifier" -> print_identifier state node
      | _ -> print_error_node state node ~err:Syntax_err.Property_identifier
    in
    let children =
      [ mk_child_res print_object_field object_field
      ; mk_child_res print_property_field property_field
      ]
    in
    make_tree state node children

(* Generic type *)

and print_generic_type ?(comments = []) state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    print_error_node state node ~err:Syntax_err.Generic_type
  | _ ->
    let comments = comments @ prev_comments node in
    let name_field = child_with_field "name" node ~err:Syntax_err.Type_identifier_or_path
    and type_arguments_field =
      child_with_field "type_arguments" node ~err:Syntax_err.Type_arguments
    and print_name_field state node =
      match get_name node with
      | "type_identifier" -> print_type_identifier ~comments state node
      | "nested_type_identifier" -> print_nested_type_identifier ~comments state node
      | _ -> print_error_node state node ~err:Syntax_err.Type_identifier_or_path
    in
    let children =
      [ mk_child_res print_name_field name_field
      ; mk_child_res print_type_arguments type_arguments_field
      ]
    in
    make_tree state node children

(* Object type *)

and print_object_type state node =
  print_braces state node print_object_type_field ~err:Syntax_err.Object_type

and print_object_type_field state node =
  match get_name node with
  | "export_statement" -> print_export_statement state node
  | "property_signature" -> print_property_signature state node
  | "call_signature" -> print_call_signature state node
  | "construct_signature" -> print_construct_signature state node
  | "index_signature" -> print_index_signature state node
  | "method_signature" -> print_method_signature state node
  | _ -> print_error_node state node ~err:Syntax_err.Object_type_field

and print_property_signature state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    print_error_node state node ~err:Syntax_err.Property_signature
  | _ ->
    let accessibility_modifier = first_child_named_opt "accessibility_modifier" node
    and kwd_static = first_child_named_opt "static" node
    and override_modifier = first_child_named_opt "override_modifier" node
    and kwd_readonly = first_child_named_opt "readonly" node
    and name_field = child_with_field "name" node ~err:Syntax_err.Identifier
    and sym_qmark = first_child_named_opt "?" node
    and type_field = child_with_field_opt "type" node in
    let children =
      [ mk_child_opt print_accessibility_modifier accessibility_modifier
      ; mk_child_opt mk_kwd_static kwd_static
      ; mk_child_opt print_override_modifier override_modifier
      ; mk_child_opt mk_kwd_readonly kwd_readonly
      ; mk_child_res print_identifier name_field
      ; mk_child_opt mk_sym_qmark sym_qmark
      ; mk_child_opt print_type_annotation type_field
      ]
    in
    make_tree state node children

(* Call signature *)

and print_call_signature state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    print_error_node state node ~err:Syntax_err.Call_signature
  | _ ->
    let type_parameters_field = child_with_field_opt "type_parameters" node
    and parameters_field = child_with_field "parameters" node ~err:Syntax_err.Parameters
    and return_type_field = child_with_field_opt "return_type" node in
    let children =
      [ mk_child_opt print_type_parameters type_parameters_field
      ; mk_child_res print_formal_parameters parameters_field
      ; mk_child_opt print_return_type return_type_field
      ]
    in
    make_tree state node children

(* Asserts annotation *)

and print_asserts_annotation state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    print_error_node state node ~err:Syntax_err.Asserts_annotation
  | _ ->
    let asserts = first_child_named "asserts" node ~err:Syntax_err.Asserts in
    make_unary_res state node print_asserts asserts

and print_asserts state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> print_error_node state node ~err:Syntax_err.Asserts
  | _ ->
    let kwd_asserts = first_child_named "asserts" ~err:Syntax_err.Asserts node
    and child = child_ranked 1 node ~err:Syntax_err.Asserted
    and print state node =
      match get_name node with
      | "type_predicate" -> print_type_predicate state node
      | "identifier" -> print_identifier state node
      | "this" -> mk_kwd_this state node
      | _ -> print_error_node state node ~err:Syntax_err.Asserted
    in
    let children =
      [ mk_child_res mk_kwd_asserts kwd_asserts; mk_child_res print child ]
    in
    make_tree state node children

(* Type predicate annotation *)

and print_type_predicate_annotation state node =
  let predicate = child_ranked 1 node ~err:Syntax_err.Type_predicate in
  make_unary_res state node print_type_predicate predicate

(* Construct signature *)

and print_construct_signature state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    print_error_node state node ~err:Syntax_err.Construct_signature
  | _ ->
    let kwd_abstract = first_child_named_opt "abstract" node
    and kwd_new = first_child_named "new" node ~err:Syntax_err.New
    and type_parameters_field = child_with_field_opt "type_parameters" node
    and parameters_field = child_with_field "parameters" node ~err:Syntax_err.Parameters
    and type_field = child_with_field_opt "type" node in
    let children =
      [ mk_child_opt mk_kwd_abstract kwd_abstract
      ; mk_child_res mk_kwd_new kwd_new
      ; mk_child_opt print_type_parameters type_parameters_field
      ; mk_child_res print_formal_parameters parameters_field
      ; mk_child_opt print_type_annotation type_field
      ]
    in
    make_tree state node children

(* Index signature *)

and print_index_signature state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    print_error_node state node ~err:Syntax_err.Index_signature
  | _ ->
    let kwd_readonly = first_child_named_opt "readonly" node
    and sign_field = child_with_field_opt "sign" node
    and name_field = child_with_field_opt "name" node
    and type_field = child_with_field "type" node ~err:Syntax_err.Type_annotation
    and sym_lbracket = first_child_named "[" node ~err:Syntax_err.Left_bracket
    and sym_rbracket = first_child_named "]" node ~err:Syntax_err.Right_bracket
    and print_type_field state node =
      match get_name node with
      | "type_annotation" -> print_type_annotation state node
      | "omitting_type_annotation" -> print_omitting_type_annotation state node
      | "adding_type_annotation" -> print_adding_type_annotation state node
      | "opting_type_annotation" -> print_opting_type_annotation state node
      | _ -> print_error_node state node ~err:Syntax_err.Type_of_index_signature
    in
    let prefix =
      [ mk_child_opt print_plus_minus sign_field
      ; mk_child_opt mk_kwd_readonly kwd_readonly
      ]
    in
    let children =
      prefix
      @ [ mk_child_res mk_sym_lbracket sym_lbracket ]
      @ (match name_field with
        | Some name_field ->
          let sym_colon = first_child_named ":" node ~err:Syntax_err.Colon
          and index_type_field =
            child_with_field "index_type" node ~err:Syntax_err.Type
          in
          [ mk_child print_identifier name_field
          ; mk_child_res mk_sym_colon sym_colon
          ; mk_child_res print_type index_type_field
          ; mk_child_res print_type_field type_field
          ]
        | None ->
          let mapped_type_clause =
            named_child_ranked 0 node ~err:Syntax_err.Mapped_type_signature
          in
          [ mk_child_res print_mapped_type_clause mapped_type_clause
          ; mk_child_res print_type_field type_field
          ])
      @ [ mk_child_res mk_sym_rbracket sym_rbracket ]
    in
    make_tree state node children

and print_plus_minus state node =
  match get_name node with
  | "+" -> mk_sym_plus state node
  | "-" -> mk_sym_minus state node
  | _ -> print_error_node state node ~err:Syntax_err.Plus_or_minus

and print_mapped_type_clause state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    print_error_node state node ~err:Syntax_err.Mapped_type_signature
  | _ ->
    let name_field = child_with_field "name" node ~err:Syntax_err.Type_name
    and kwd_in = first_child_named "in" node ~err:Syntax_err.In
    and type_field = child_with_field "type" node ~err:Syntax_err.Type
    and alias_field = child_with_field_opt "alias" node in
    let alias_children =
      match alias_field with
      | None -> []
      | Some alias ->
        let kwd_as = first_child_named "as" node ~err:Syntax_err.As in
        [ mk_child_res mk_kwd_as kwd_as; mk_child print_type alias ]
    in
    let children =
      [ mk_child_res print_type_identifier name_field
      ; mk_child_res mk_kwd_in kwd_in
      ; mk_child_res print_type type_field
      ]
      @ alias_children
    in
    make_tree state node children

and print_omitting_type_annotation state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    print_error_node state node ~err:Syntax_err.Omitting_type_annotation
  | _ ->
    let sym_kind = first_child_named "-?:" node ~err:Syntax_err.Omitting_type_annotation
    and type_child = named_child_ranked 0 node ~err:Syntax_err.Type_expression in
    let children =
      [ mk_child_res mk_sym_omitting sym_kind; mk_child_res print_type type_child ]
    in
    make_tree state node children

and print_adding_type_annotation state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    print_error_node state node ~err:Syntax_err.Adding_type_annotation
  | _ ->
    let sym_kind = first_child_named "+?:" node ~err:Syntax_err.Adding_type_annotation
    and type_child = named_child_ranked 0 node ~err:Syntax_err.Type_expression in
    let children =
      [ mk_child_res mk_sym_adding sym_kind; mk_child_res print_type type_child ]
    in
    make_tree state node children

and print_opting_type_annotation state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    print_error_node state node ~err:Syntax_err.Opting_type_annotation
  | _ ->
    let sym_kind = first_child_named "?:" node ~err:Syntax_err.Adding_type_annotation
    and type_child = named_child_ranked 0 node ~err:Syntax_err.Type_expression in
    let children =
      [ mk_child_res mk_sym_opting sym_kind; mk_child_res print_type type_child ]
    in
    make_tree state node children

(* Method signature *)

and print_method_signature state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    print_error_node state node ~err:Syntax_err.Method_signature
  | _ ->
    let accessibility_modifier = first_child_named_opt "accessibility_modifier" node
    and kwd_static = first_child_named_opt "static" node
    and override_modifier = first_child_named_opt "override_modifier" node
    and kwd_readonly = first_child_named_opt "readonly" node
    and kwd_async = first_child_named_opt "async" node
    and kwd_set = first_child_named_opt "set" node
    and kwd_get = first_child_named_opt "get" node
    and sym_asterisk = first_child_named_opt "*" node
    and name_field = child_with_field "name" node ~err:Syntax_err.Property_name
    and sym_qmark = first_child_named_opt "?" node
    (* "_call_signature" inlined: *)
    and type_parameters_field = child_with_field_opt "type_parameters" node
    and parameters_field = child_with_field "parameters" node ~err:Syntax_err.Parameters
    and return_type_field = child_with_field_opt "return_type" node in
    let children =
      [ mk_child_opt print_accessibility_modifier accessibility_modifier
      ; mk_child_opt mk_kwd_static kwd_static
      ; mk_child_opt print_override_modifier override_modifier
      ; mk_child_opt mk_kwd_readonly kwd_readonly
      ; mk_child_opt mk_kwd_async kwd_async
      ; mk_child_opt mk_kwd_set kwd_set
      ; mk_child_opt mk_kwd_get kwd_get
      ; mk_child_opt mk_sym_asterisk sym_asterisk
      ; mk_child_res print_property_name name_field
      ; mk_child_opt mk_sym_qmark sym_qmark
      ; mk_child_opt print_type_parameters type_parameters_field
      ; mk_child_res print_formal_parameters parameters_field
      ; mk_child_opt print_return_type return_type_field
      ]
    in
    make_tree state node children

(* Array type *)

and print_array_type state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> print_error_node state node ~err:Syntax_err.Array_type
  | _ ->
    let type_child = child_ranked 0 node ~err:Syntax_err.Type_expression
    and sym_lbracket = first_child_named "[" node ~err:Syntax_err.Left_bracket
    and sym_rbracket = first_child_named "]" node ~err:Syntax_err.Right_bracket in
    let children =
      [ mk_child_res print_type type_child
      ; mk_child_res mk_sym_lbracket sym_lbracket
      ; mk_child_res mk_sym_rbracket sym_rbracket
      ]
    in
    make_tree state node children

(* Tuple type *)

and print_tuple_type state node =
  print_brackets state node print_tuple_type_member ~err:Syntax_err.Tuple_type

and print_tuple_type_member state node =
  match get_name node with
  | "required_parameter" -> print_tuple_parameter state node (* Alias *)
  | "optional_parameter" -> print_optional_tuple_parameter state node (* Alias *)
  | "optional_type" -> print_optional_type state node
  | "rest_type" -> print_rest_type state node
  | _ -> print_type state node (* "type" is a hidden rule *)

and print_tuple_parameter state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    print_error_node state node ~err:Syntax_err.Tuple_parameter
  | _ ->
    let name_field = child_with_field "name" node ~err:Syntax_err.Identifier_or_rest
    and type_field = child_with_field "type" node ~err:Syntax_err.Type_annotation
    and print_name_field state node =
      match get_name node with
      | "identifier" -> print_identifier state node
      | "rest_pattern" -> print_rest_pattern state node
      | _ -> print_error_node state node ~err:Syntax_err.Identifier_or_rest
    in
    let children =
      [ mk_child_res print_name_field name_field
      ; mk_child_res print_type_annotation type_field
      ]
    in
    make_tree state node children

and print_optional_tuple_parameter state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    print_error_node state node ~err:Syntax_err.Optional_tuple_parameter
  | _ ->
    let name_field = child_with_field "name" node ~err:Syntax_err.Identifier
    and type_field = child_with_field "type" node ~err:Syntax_err.Type_annotation in
    let children =
      [ mk_child_res print_identifier name_field
      ; mk_child_res print_type_annotation type_field
      ]
    in
    make_tree state node children

(* Type annotation *)

and print_type_annotation state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    print_error_node state node ~err:Syntax_err.Type_annotation
  | _ ->
    let sym_colon = first_child_named ":" node ~err:Syntax_err.Colon
    and type_child = named_child_ranked 0 node ~err:Syntax_err.Type_expression in
    let children =
      [ mk_child_res mk_sym_colon sym_colon; mk_child_res print_type type_child ]
    in
    make_tree state node children

(* Rest pattern *)

and print_rest_pattern state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    print_error_node state node ~err:Syntax_err.Rest_pattern
  | _ ->
    let sym_ellipsis = first_child_named "..." node ~err:Syntax_err.Ellipsis
    and expr_child = named_child_ranked 0 node ~err:Syntax_err.Expression in
    let children =
      [ mk_child_res mk_sym_ellipsis sym_ellipsis
      ; mk_child_res print_lhs_expression expr_child
      ]
    in
    make_tree state node children

(* LHS expression *)

and print_lhs_expression state node =
  match get_name node with
  | "member_expression" -> print_member_expression state node
  | "subscript_expression" -> print_subscript_expression state node
  | "identifier" -> print_identifier state node
  | "undefined" -> mk_kwd_undefined state node
  | "object_pattern" -> print_object_pattern state node
  | "array_pattern" -> print_array_pattern state node
  | "non_null_expression" -> print_non_null_expression state node
  | _ -> print_error_node state node ~err:Syntax_err.Expression

and print_optional_type state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    print_error_node state node ~err:Syntax_err.Optional_type
  | _ ->
    let child = named_child_ranked 0 node ~err:Syntax_err.Optional_type in
    make_unary_res state node print_type child

and print_rest_type state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> print_error_node state node ~err:Syntax_err.Rest_type
  | _ ->
    let sym_ellipsis = first_child_named "..." node ~err:Syntax_err.Ellipsis
    and type_child = named_child_ranked 0 node ~err:Syntax_err.Type_expression in
    let children =
      [ mk_child_res mk_sym_ellipsis sym_ellipsis; mk_child_res print_type type_child ]
    in
    make_tree state node children

(* Type query *)

and print_type_query state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> print_error_node state node ~err:Syntax_err.Type_query
  | _ ->
    let kwd_typeof = first_child_named "typeof" node ~err:Syntax_err.Typeof
    and child = child_ranked 1 node ~err:Syntax_err.Type_query
    and print state node =
      match get_name node with
      | "subscript_expression" -> print_type_query_subscript_expression state node
      | "member_expression" -> print_type_query_member_expression state node
      | "call_expression" -> print_type_query_call_expression state node
      | "instantiation_expression" -> print_type_query_instantiation_expression state node
      | "identifier" -> print_identifier state node
      | "this" -> mk_kwd_this state node
      | _ -> print_error_node state node ~err:Syntax_err.Type_query
    in
    let children = [ mk_child_res mk_kwd_typeof kwd_typeof; mk_child_res print child ] in
    make_tree state node children

and print_type_query_subscript_expression state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    print_error_node state node ~err:Syntax_err.Type_query_subscript
  | _ ->
    let object_field = child_with_field "object" node ~err:Syntax_err.Object_denotation
    and index_field =
      child_with_field "index" node ~err:Syntax_err.Type_or_string_or_number
    and sym_lbracket = first_child_named "[" node ~err:Syntax_err.Left_bracket
    and sym_rbracket = first_child_named "]" node ~err:Syntax_err.Right_bracket
    and print_index_field state node =
      match get_name node with
      | "predefined_type" -> print_predefined_type state node
      | "string" -> print_string state node
      | "number" -> print_number state node
      | _ -> print_error_node state node ~err:Syntax_err.Predefined_type
    in
    let children =
      [ mk_child_res print_object_field object_field
      ; mk_child_res mk_sym_lbracket sym_lbracket
      ; mk_child_res print_index_field index_field
      ; mk_child_res mk_sym_rbracket sym_rbracket
      ]
    in
    make_tree state node children

and print_type_query_member_expression state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    print_error_node state node ~err:Syntax_err.Type_query_member
  | _ ->
    let object_field = child_with_field "object" node ~err:Syntax_err.Object_denotation
    and property_field =
      child_with_field "property" node ~err:Syntax_err.Property_identifier
    in
    let children =
      [ mk_child_res print_object_field object_field
      ; mk_child_res print_property_field property_field
      ]
    in
    make_tree state node children

and print_object_field state node =
  match get_name node with
  | "identifier" -> print_identifier state node
  | "this" -> mk_kwd_this state node
  | "subscript_expression" -> print_type_query_subscript_expression state node
  | "member_expression" -> print_type_query_member_expression state node
  | "call_expression" -> print_type_query_call_expression state node
  | _ -> print_error_node state node ~err:Syntax_err.Object_denotation

and print_property_field state node = print_type_query_property state node

and print_type_query_instantiation_expression state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    print_error_node state node ~err:Syntax_err.Type_query_instantiation
  | _ ->
    let function_field =
      child_with_field "function" node ~err:Syntax_err.Function_denotation
    and type_arguments_field =
      child_with_field "type_arguments" node ~err:Syntax_err.Type_arguments
    in
    let children =
      [ mk_child_res print_function_field function_field
      ; mk_child_res print_type_arguments type_arguments_field
      ]
    in
    make_tree state node children

and print_function_field state node =
  match get_name node with
  | "import" -> mk_kwd_import state node
  | "identifier" -> print_identifier state node
  | "member_expression" -> print_type_query_member_expression state node
  | "subscript_expression" -> print_type_query_subscript_expression state node
  | _ -> print_error_node state node ~err:Syntax_err.Function_denotation

and print_type_query_call_expression state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    print_error_node state node ~err:Syntax_err.Type_query_call
  | _ ->
    let function_field =
      child_with_field "function" node ~err:Syntax_err.Function_denotation
    and arguments_field = child_with_field "arguments" node ~err:Syntax_err.Arguments in
    let children =
      [ mk_child_res print_function_field function_field
      ; mk_child_res print_arguments arguments_field
      ]
    in
    make_tree state node children

(* Index type query *)

and print_index_type_query state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    print_error_node state node ~err:Syntax_err.Index_type_query
  | _ ->
    let child = named_child_ranked 0 node ~err:Syntax_err.Index_type_query in
    make_unary_res state node print_primary_type child

(* Existential type *)

and print_existential_type state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    print_error_node state node ~err:Syntax_err.Existential_type
  | _ -> make_node state node

(* Literal type *)

and print_literal_type state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    print_error_node state node ~err:Syntax_err.Literal_type
  | _ ->
    let child = named_child_ranked 0 node ~err:Syntax_err.Literal_type
    and print state node =
      match get_name node with
      | "unary_expression" -> print_unary_expression state node
      | "number" -> print_number state node
      | "string" -> print_string state node
      | "true" -> mk_kwd_true state node
      | "false" -> mk_kwd_false state node
      | "null" -> mk_kwd_null state node
      | "undefined" -> mk_kwd_undefined state node
      | _ -> print_error_node state node ~err:Syntax_err.Literal_type
    in
    make_unary_res state node print child

(* Look up type

   The non-terminals "type" and "primary_type" are supertypes in the
   TypeScript grammar, which means that they are hidden rules. *)

and print_lookup_type state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    print_error_node state node ~err:Syntax_err.Lookup_type
  | _ ->
    let primary_type_child = named_child_ranked 0 node ~err:Syntax_err.Type_expression
    and sym_lbracket = first_child_named "[" node ~err:Syntax_err.Left_bracket
    and sym_rbracket = first_child_named "]" node ~err:Syntax_err.Right_bracket
    and type_child = named_child_ranked 1 node ~err:Syntax_err.Type_expression in
    let children =
      [ mk_child_res print_primary_type primary_type_child
      ; mk_child_res mk_sym_lbracket sym_lbracket
      ; mk_child_res print_type type_child
      ; mk_child_res mk_sym_rbracket sym_rbracket
      ]
    in
    make_tree state node children

(* Conditional type *)

and print_conditional_type state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    print_error_node state node ~err:Syntax_err.Conditional_type
  | _ ->
    let left_field = child_with_field "left" node ~err:Syntax_err.Type
    and kwd_extends = first_child_named "extends" node ~err:Syntax_err.Extends
    and right_field = child_with_field "right" node ~err:Syntax_err.Type
    and consequence_field = child_with_field "consequence" node ~err:Syntax_err.Type
    and alternative_field = child_with_field "alternative" node ~err:Syntax_err.Type
    and sym_qmark = first_child_named "?" node ~err:Syntax_err.Question_mark
    and sym_colon = first_child_named ":" node ~err:Syntax_err.Colon in
    let children =
      [ mk_child_res print_type left_field
      ; mk_child_res mk_kwd_extends kwd_extends
      ; mk_child_res print_type right_field
      ; mk_child_res mk_sym_qmark sym_qmark
      ; mk_child_res print_type consequence_field
      ; mk_child_res mk_sym_colon sym_colon
      ; mk_child_res print_type alternative_field
      ]
    in
    make_tree state node children

(* Template literal type *)

and print_template_literal_type state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    print_error_node state node ~err:Syntax_err.Template_literal_type
  | _ -> make_node state node

(* Intersection type *)

and print_intersection_type state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    print_error_node state node ~err:Syntax_err.Intersection_type
  | _ ->
    let first_child = child_ranked_opt 0 node
    and sym_ampersand = first_child_named "&" node ~err:Syntax_err.Ampersand in
    let children =
      match first_child with
      | None -> [ mk_error_child node ~err:Syntax_err.Type_or_conjunction ]
      | Some left_type ->
        (match get_name left_type with
        | "&" ->
          let type_node = child_ranked 1 node ~err:Syntax_err.Type_expression in
          [ mk_child_res mk_sym_ampersand sym_ampersand
          ; mk_child_res print_type type_node
          ]
        | _ ->
          (* "type" is a supertype, therefore a hidden rule *)
          let right_type = child_ranked 2 node ~err:Syntax_err.Type_expression in
          [ mk_child print_type left_type
          ; mk_child_res mk_sym_ampersand sym_ampersand
          ; mk_child_res print_type right_type
          ])
    in
    make_tree state node children

(* Union type *)

and print_union_type state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> print_error_node state node ~err:Syntax_err.Union_type
  | _ ->
    let first_child = child_ranked_opt 0 node
    and sym_vbar = first_child_named "|" node ~err:Syntax_err.Vertical_bar in
    let children =
      match first_child with
      | None -> [ mk_error_child node ~err:Syntax_err.Type_or_disjunction ]
      | Some left_type ->
        (match get_name left_type with
        | "|" ->
          let type_node = child_ranked 1 node ~err:Syntax_err.Type_expression in
          [ mk_child_res mk_sym_vbar sym_vbar; mk_child_res print_type type_node ]
        | _ ->
          (* "type" is a supertype, therefore a hidden rule *)
          let right_type = child_ranked 2 node ~err:Syntax_err.Type_expression in
          [ mk_child print_type left_type
          ; mk_child_res mk_sym_vbar sym_vbar
          ; mk_child_res print_type right_type
          ])
    in
    make_tree state node children

(* Function type *)

and print_function_type state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    print_error_node state node ~err:Syntax_err.Function_type
  | _ ->
    let type_parameters_field = child_with_field_opt "type_parameters" node
    and parameters_field = child_with_field "parameters" node ~err:Syntax_err.Parameters
    and return_type_field = child_with_field "return_type" node ~err:Syntax_err.Type
    and sym_arrow = first_child_named "=>" node ~err:Syntax_err.Arrow
    and print_return_type state node =
      match get_name node with
      | "asserts" -> print_asserts state node
      | "type_predicate" -> print_type_predicate state node
      | _ -> print_type state node
    in
    let children =
      [ mk_child_opt print_type_parameters type_parameters_field
      ; mk_child_res print_formal_parameters parameters_field
      ; mk_child_res mk_sym_arrow sym_arrow
      ; mk_child_res print_return_type return_type_field
      ]
    in
    make_tree state node children

and print_type_predicate state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    print_error_node state node ~err:Syntax_err.Type_predicate
  | _ ->
    let name_field = child_with_field "name" node ~err:Syntax_err.Identifier_or_type
    and kwd_is = first_child_named "is" node ~err:Syntax_err.Is
    and type_field = child_with_field "type" node ~err:Syntax_err.Type_expression in
    let print_name_field state node =
      match get_name node with
      | "identifier" -> print_identifier state node
      | "this" -> mk_kwd_this state node
      | _ -> print_predefined_type state node
    in
    let children =
      [ mk_child_res print_name_field name_field
      ; mk_child_res mk_kwd_is kwd_is
      ; mk_child_res print_type type_field
      ]
    in
    make_tree state node children

(* Readonly type *)

and print_readonly_type state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    print_error_node state node ~err:Syntax_err.Readonly_type
  | _ ->
    let kwd_readonly = first_child_named "readonly" node ~err:Syntax_err.Readonly
    and type_child = child_ranked 1 node ~err:Syntax_err.Type_expression in
    let children =
      [ mk_child_res mk_kwd_readonly kwd_readonly; mk_child_res print_type type_child ]
    in
    make_tree state node children

(* Constructor type *)

and print_constructor_type state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    print_error_node state node ~err:Syntax_err.Constructor_type
  | _ ->
    let kwd_abstract = first_child_named_opt "abstract" node
    and kwd_new = first_child_named "new" node ~err:Syntax_err.New
    and type_parameters_field = child_with_field_opt "type_parameters" node
    and parameters_field = child_with_field "parameters" node ~err:Syntax_err.Parameters
    and sym_arrow = first_child_named "=>" node ~err:Syntax_err.Arrow
    and type_field = child_with_field "type" node ~err:Syntax_err.Type_expression in
    let children =
      [ mk_child_opt mk_kwd_abstract kwd_abstract
      ; mk_child_res mk_kwd_new kwd_new
      ; mk_child_opt print_type_parameters type_parameters_field
      ; mk_child_res print_formal_parameters parameters_field
      ; mk_child_res mk_sym_arrow sym_arrow
      ; mk_child_res print_type type_field
      ]
    in
    make_tree state node children

and print_formal_parameters state node =
  print_parens state node print_formal_parameter ~err:Syntax_err.Parameters

and print_formal_parameter state node =
  match get_name node with
  | "required_parameter" -> print_required_parameter state node
  | "optional_parameter" -> print_optional_parameter state node
  | _ -> print_error_node state node ~err:Syntax_err.Parameter

and print_optional_parameter state node = print_required_parameter state node

and print_required_parameter state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    print_error_node state node ~err:Syntax_err.Required_parameter
  | _ ->
    (* "_parameter_name" inlined: *)
    let decorators = children_named "decorator" node
    and accessibility_modifier = first_child_named_opt "accessibility_modifier" node
    and override_modifier = first_child_named_opt "override_modifier" node
    and kwd_readonly = first_child_named_opt "readonly" node
    and pattern_field = child_with_field "pattern" node ~err:Syntax_err.Pattern
    (* *)
    and type_field = child_with_field_opt "type" node
    and print_pattern_field state node =
      match get_name node with
      | "this" -> mk_kwd_this state node
      | _ -> print_pattern state node
    in
    let children =
      mk_children_list print_decorator decorators
      @ [ mk_child_opt print_accessibility_modifier accessibility_modifier
        ; mk_child_opt print_override_modifier override_modifier
        ; mk_child_opt mk_kwd_readonly kwd_readonly
        ; mk_child_res print_pattern_field pattern_field
        ; mk_child_opt print_type_annotation type_field
        ]
      @ mk_child_initializer_opt node (* "_initializer" inlined *)
    in
    make_tree state node children

and mk_child_initializer sym_equal node =
  let value_field = child_with_field "value" node ~err:Syntax_err.Expression in
  let children =
    [ mk_child_res mk_sym_equal sym_equal; mk_child_res print_expression value_field ]
  in
  Some (fun state -> Tree.make_tree state "initializer" children)

and mk_child_initializer_opt node =
  match first_child_named_opt "=" node with
  | None -> []
  | Some sym_equal -> [ mk_child_initializer (Ok sym_equal) node ]

(* Decorator *)

and print_decorator state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> print_error_node state node ~err:Syntax_err.Decorator
  | _ ->
    let child = named_child_ranked 0 node ~err:Syntax_err.Decorator
    and print state node =
      match get_name node with
      | "identifier" -> print_identifier state node
      | "member_expression" -> print_decorator_member_expression state node
      | "call_expression" -> print_decorator_call_expression state node
      | "parenthesized_expression" -> print_decorator_parenthesized_expression state node
      | _ -> print_error_node state node ~err:Syntax_err.Decorator
    in
    make_unary_res state node print child

and print_decorator_member_expression state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    print_error_node state node ~err:Syntax_err.Decorator_member
  | _ ->
    let object_field = child_with_field "object" node ~err:Syntax_err.Identifier_or_member
    and selector = first_child_named "." node ~err:Syntax_err.Dot
    and property_field =
      child_with_field "property" node ~err:Syntax_err.Property_identifier
    and print_object state node =
      match get_name node with
      | "identifier" -> print_identifier state node
      | _ -> print_decorator_member_expression state node
    in
    let children =
      [ mk_child_res print_object object_field
      ; mk_child_res mk_sym_dot selector
      ; mk_child_res print_identifier property_field
      ]
    in
    make_tree state node children

and print_decorator_call_expression state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    print_error_node state node ~err:Syntax_err.Decorator_call
  | _ ->
    let function_field =
      child_with_field "function" node ~err:Syntax_err.Identifier_or_member
    and type_arguments_field = child_with_field_opt "type_arguments" node
    and arguments_field = child_with_field "arguments" node ~err:Syntax_err.Arguments
    and print_function state node =
      match get_name node with
      | "identifier" -> print_identifier state node
      | "member_expression" -> print_decorator_member_expression state node
      | _ -> print_error_node state node ~err:Syntax_err.Decorator_call
    in
    let children =
      [ mk_child_res print_function function_field
      ; mk_child_opt print_type_arguments type_arguments_field
      ; mk_child_res print_arguments arguments_field
      ]
    in
    make_tree state node children

and print_decorator_parenthesized_expression ?comments state node =
  let print state node =
    match get_name node with
    | "identifier" -> print_identifier state node
    | "member_expression" -> print_decorator_member_expression state node
    | _ -> print_call_expression state node
  in
  print_parens ?comments state node print ~err:Syntax_err.Parenthesized_decorator

(* Accessibility modifier *)

and print_accessibility_modifier state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    print_error_node state node ~err:Syntax_err.Public_private_protected
  | _ ->
    let child = child_ranked 0 node ~err:Syntax_err.Public_private_protected
    and print state node =
      match get_name node with
      | "public" -> mk_kwd_public state node
      | "private" -> mk_kwd_private state node
      | "protected" -> mk_kwd_protected state node
      | _ -> print_error_node state node ~err:Syntax_err.Public_private_protected
    in
    make_unary_res state node print child

(* Override modifier *)

and print_override_modifier state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> print_error_node state node ~err:Syntax_err.Override
  | _ ->
    let child = child_ranked 0 node ~err:Syntax_err.Override in
    make_unary_res state node mk_kwd_override child

(* Infer type *)

and print_infer_type state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> print_error_node state node ~err:Syntax_err.Infer
  | _ ->
    let kwd_infer = first_child_named "infer" node ~err:Syntax_err.Infer
    and type_identifier_child =
      child_ranked 1 node ~err:Syntax_err.Identifier (* name "type_identifier"? *)
    and kwd_extends = first_child_named_opt "extends" node
    and type_child = child_ranked_opt 3 node in
    let children =
      [ mk_child_res mk_kwd_infer kwd_infer
      ; mk_child_res print_identifier type_identifier_child
      ; mk_child_opt mk_kwd_extends kwd_extends
      ; mk_child_opt print_type type_child
      ]
    in
    make_tree state node children

(* PATTERN

   The JavasScript tree-sitter grammar have the non-terminal
   "pattern" be a supertype, that is, a hidden rule. *)

(* Object pattern *)

and print_object_pattern state node =
  print_braces state node print_object_pattern_field ~err:Syntax_err.Object_pattern

and print_object_pattern_field state node =
  match get_name node with
  | "pair_pattern" -> print_pair_pattern state node
  | "rest_pattern" -> print_rest_pattern state node
  | "object_assignment_pattern" -> print_object_assignment_pattern state node
  | "shorthand_property_identifier_pattern" ->
    print_shorthand_property_identifier_pattern state node
  | _ -> print_error_node state node ~err:Syntax_err.Object_pattern_field

(* Pair pattern *)

and print_pair_pattern state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    print_error_node state node ~err:Syntax_err.Pair_pattern
  | _ ->
    let key_field = child_with_field "key" node ~err:Syntax_err.Property_name
    and sym_colon = first_child_named ":" node ~err:Syntax_err.Colon
    and value_field = child_with_field "value" node ~err:Syntax_err.Pattern
    and print_value state node =
      match get_name node with
      | "ERROR" | "MISSING" | "NULL" ->
        print_error_node state node ~err:Syntax_err.Value_of_pair_pattern
      | "assignment_pattern" -> print_assignment_pattern state node
      | _ -> print_pattern state node (* Hidden rule *)
    in
    let children =
      [ mk_child_res print_property_name key_field
      ; mk_child_res mk_sym_colon sym_colon
      ; mk_child_res print_value value_field
      ]
    in
    make_tree state node children

(* Assignment pattern *)

and print_assignment_pattern state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    print_error_node state node ~err:Syntax_err.Assignment_pattern
  | _ ->
    let left_field = child_with_field "left" node ~err:Syntax_err.Pattern
    and right_field = child_with_field "right" node ~err:Syntax_err.Expression
    and sym_equal = first_child_named "=" node ~err:Syntax_err.Equal in
    let children =
      [ mk_child_res print_pattern left_field
      ; mk_child_res mk_sym_equal sym_equal
      ; mk_child_res print_expression right_field
      ]
    in
    make_tree state node children

(* Property names *)

and print_property_name state node =
  match get_name node with
  | "property_identifier" -> print_identifier state node
  | "private_property_identifier" -> print_identifier state node
  | "string" -> print_string state node
  | "number" -> print_number state node
  | "computed_property_name" -> print_computed_property_name state node
  | _ -> print_error_node state node ~err:Syntax_err.Property_name

and print_computed_property_name state node =
  print_brackets state node print_expression ~err:Syntax_err.Computed_property_name

and print_shorthand_property_identifier_pattern state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> print_error_node state node ~err:Syntax_err.Identifier
  | _ -> make_node state node

(* Object assignment pattern *)

and print_object_assignment_pattern state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    print_error_node state node ~err:Syntax_err.Object_assignment_pattern
  | _ ->
    let left_field = child_with_field "left" node ~err:Syntax_err.Pattern
    and sym_equal = first_child_named "=" node ~err:Syntax_err.Equal
    and right_field = child_with_field "right" node ~err:Syntax_err.Expression in
    let children =
      [ mk_child_res print_object_lhs_pattern left_field
      ; mk_child_res mk_sym_equal sym_equal
      ; mk_child_res print_expression right_field
      ]
    in
    make_tree state node children

and print_object_lhs_pattern state node =
  match get_name node with
  | "shorthand_property_identifier_pattern" ->
    print_shorthand_property_identifier_pattern state node
  | _ -> print_destructuring_pattern state node

(* Rule "_destructuring_pattern" is inlined. *)

and print_destructuring_pattern state node =
  match get_name node with
  | "object_pattern" -> print_object_pattern state node
  | "array_pattern" -> print_array_pattern state node
  | _ -> print_error_node state node ~err:Syntax_err.Object_or_array_pattern

(* Array pattern *)

and print_array_pattern state node =
  print_brackets state node print_array_pattern_cell ~err:Syntax_err.Array_pattern

and print_array_pattern_cell state node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" ->
    print_error_node state node ~err:Syntax_err.Array_cell_pattern
  | "assignment_pattern" -> print_assignment_pattern state node
  | _ -> print_pattern state node (* hidden rule *)

(* General patterns (hidden rule) *)

and print_pattern state node =
  match get_name node with
  | "rest_pattern" -> print_rest_pattern state node
  | _ -> print_lhs_expression state node
