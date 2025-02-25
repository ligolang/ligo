(* Decoding the tree-sitter CST for TypeScript *)

open Core

(* Dependencies and scopes *)

module Region = Simple_utils.Region
module Wrap = Lexing_shared.Wrap
module Ts_wrap = Typescript_ast.Ts_wrap
module Loc_map = Typescript_ast.Loc_map
module Syntax_err = Typescript_ast.Syntax_err
module Ast = Typescript_ast.Ast
module Lexeme = Typescript_ast.Lexeme
module Number = Typescript_ast.Number
open Syntax_err
open Ts_wrap
open Ast

(* Utilities *)

let sprintf = Printf.sprintf

(* Monadic let-binder for result values *)

let ( let* ) v f = Result.bind v ~f

(* Skipping strings in a list until a colon is found *)

let rec skip_until_colon = function
  | [] -> []
  | node :: nodes ->
    (match get_name node with
    | ":" -> nodes
    | _ -> skip_until_colon nodes)

(* Source map for converting vertical and horizontal offset ranges
   into regions *)

let get_region : (Ts_wrap.ts_tree -> Region.t) ref =
  ref (fun _ -> failwith "Internal error: Decode.get_region")

(* The input source (default: a hundred lines) *)

let input : Buffer.t ref = ref (Buffer.create (80 * 100))

(* Utilities *)

let wrap decode ?comments node : ('a Wrap.t, _) result =
  let* decoded_node = decode ?comments node in
  Ok (Wrap.make decoded_node (!get_region node))

(* Tayloring the fetching of a field, with an error message in case of
   failure. If [!debug], a missing field yields internal information. *)

let debug = ref false

let child_with_field field node ~err : (_, string Region.reg) result =
  match Ts_wrap.child_with_field ~get_region field node with
  | Ok _ as ok -> ok
  | Error () ->
    let region = !get_region node in
    let region' =
      if Region.is_empty region then "empty region" else region#compact `Byte
    in
    let value =
      if !debug
      then (
        let name = get_name node in
        if String.equal name "NULL"
        then sprintf "ERROR: NULL parent of field %S." field
        else sprintf "ERROR: Node %S (%s) is missing the field %S." name region' field)
      else Syntax_err.to_string err
    in
    Error Region.{ region; value }

(* Wrapping the fetching of nodes *)

let pack_err err node =
  let value = Syntax_err.to_string err
  and region = !get_region node in
  Region.{ value; region }

let first_child_named name node ~err =
  Ts_wrap.first_child_named name node ~msg:(pack_err err node)

let child_ranked index node ~err =
  Ts_wrap.child_ranked index node ~msg:(pack_err err node)

let named_child_ranked index node ~err =
  Ts_wrap.named_child_ranked index node ~msg:(pack_err err node)

let last_child node ~err = Ts_wrap.last_child node ~msg:(pack_err err node)
let next_sibling node ~err = Ts_wrap.next_sibling node ~msg:(pack_err err node)
let prev_sibling node ~err = Ts_wrap.prev_sibling node ~msg:(pack_err err node)

(* Making errors *)

let mk_err err node = Error (pack_err err node)

(* Decoding literals *)

let dec_comments ?(comments = []) node : Wrap.comment list =
  let f node =
    let region = !get_region node in
    let value = Lexeme.read !input region in
    Wrap.Block Region.{ value; region }
  in
  List.map ~f (comments @ prev_comments node)

let make_node ?comments node : string wrap =
  let region = !get_region node in
  let root = Lexeme.read !input region
  and comments = dec_comments ?comments node in
  Wrap.make ~comments root region

let dec_identifier ?comments node : identifier = make_node ?comments node
let dec_string ?comments node : string_literal = make_node ?comments node
let dec_regex ?comments node : string_literal = make_node ?comments node

let dec_number ?(comments = []) node : (number, _) result =
  let region = !get_region node in
  let lexeme = Lexeme.read !input region in
  let lexbuf = Lexing.from_string lexeme
  and comments = dec_comments ~comments node in
  Number.scan comments region lexbuf

(* Keywords *)

let dec_kwd ?comments node ~err =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err err node
  | _ -> Ok (make_node ?comments node)

let dec_kwd_infer = dec_kwd ~err:Syntax_err.Infer
let dec_kwd_keyof = dec_kwd ~err:Syntax_err.Keyof
let dec_kwd_meta = dec_kwd ~err:Syntax_err.Meta
let dec_kwd_target = dec_kwd ~err:Syntax_err.Target
let dec_kwd_false = dec_kwd ~err:Syntax_err.False
let dec_kwd_true = dec_kwd ~err:Syntax_err.True
let dec_kwd_super = dec_kwd ~err:Syntax_err.Super
let dec_kwd_null = dec_kwd ~err:Syntax_err.Null
let dec_kwd_satisfies = dec_kwd ~err:Syntax_err.Satisfies
let dec_kwd_yield = dec_kwd ~err:Syntax_err.Yield
let dec_kwd_new = dec_kwd ~err:Syntax_err.New
let dec_kwd_instanceof = dec_kwd ~err:Syntax_err.Instanceof
let dec_kwd_implements = dec_kwd ~err:Syntax_err.Implements
let dec_kwd_assert = dec_kwd ~err:Syntax_err.Assert
let dec_kwd_as = dec_kwd ~err:Syntax_err.As
let dec_kwd_async = dec_kwd ~err:Syntax_err.Async
let dec_kwd_function = dec_kwd ~err:Syntax_err.Function
let dec_kwd_override = dec_kwd ~err:Syntax_err.Override
let dec_kwd_readonly = dec_kwd ~err:Syntax_err.Readonly
let dec_kwd_public = dec_kwd ~err:Syntax_err.Public
let dec_kwd_private = dec_kwd ~err:Syntax_err.Private
let dec_kwd_protected = dec_kwd ~err:Syntax_err.Protected
let dec_kwd_set = dec_kwd ~err:Syntax_err.Set
let dec_kwd_get = dec_kwd ~err:Syntax_err.Get
let dec_kwd_static = dec_kwd ~err:Syntax_err.Static
let dec_kwd_this = dec_kwd ~err:Syntax_err.This
let dec_kwd_is = dec_kwd ~err:Syntax_err.Is
let dec_kwd_class = dec_kwd ~err:Syntax_err.Class
let dec_kwd_const = dec_kwd ~err:Syntax_err.Const
let dec_kwd_let = dec_kwd ~err:Syntax_err.Let
let dec_kwd_undefined = dec_kwd ~err:Syntax_err.Undefined
let dec_kwd_abstract = dec_kwd ~err:Syntax_err.Abstract
let dec_kwd_declare = dec_kwd ~err:Syntax_err.Declare
let dec_kwd_accessor = dec_kwd ~err:Syntax_err.Accessor
let dec_kwd_global = dec_kwd ~err:Syntax_err.Global
let dec_kwd_module = dec_kwd ~err:Syntax_err.Module
let dec_kwd_enum = dec_kwd ~err:Syntax_err.Enum
let dec_kwd_import = dec_kwd ~err:Syntax_err.Import
let dec_kwd_interface = dec_kwd ~err:Syntax_err.Interface
let dec_kwd_extends = dec_kwd ~err:Syntax_err.Extends
let dec_kwd_namespace = dec_kwd ~err:Syntax_err.Namespace
let dec_kwd_type = dec_kwd ~err:Syntax_err.Type
let dec_kwd_using = dec_kwd ~err:Syntax_err.Using
let dec_kwd_return = dec_kwd ~err:Syntax_err.Return
let dec_kwd_switch = dec_kwd ~err:Syntax_err.Switch
let dec_kwd_case = dec_kwd ~err:Syntax_err.Case
let dec_kwd_default = dec_kwd ~err:Syntax_err.Default
let dec_kwd_throw = dec_kwd ~err:Syntax_err.Throw
let dec_kwd_while = dec_kwd ~err:Syntax_err.While
let dec_kwd_with = dec_kwd ~err:Syntax_err.With
let dec_kwd_any = dec_kwd ~err:Syntax_err.Any
let dec_kwd_number = dec_kwd ~err:Syntax_err.Number
let dec_kwd_boolean = dec_kwd ~err:Syntax_err.Boolean
let dec_kwd_string = dec_kwd ~err:Syntax_err.String
let dec_kwd_symbol = dec_kwd ~err:Syntax_err.Symbol
let dec_kwd_unique_symbol = dec_kwd ~err:Syntax_err.Unique_symbol
let dec_kwd_void = dec_kwd ~err:Syntax_err.Void
let dec_kwd_unknown = dec_kwd ~err:Syntax_err.Unknown
let dec_kwd_never = dec_kwd ~err:Syntax_err.Never
let dec_kwd_object = dec_kwd ~err:Syntax_err.Object
let dec_kwd_asserts = dec_kwd ~err:Syntax_err.Asserts
let dec_kwd_debugger = dec_kwd ~err:Syntax_err.Debugger
let dec_kwd_break = dec_kwd ~err:Syntax_err.Break
let dec_kwd_continue = dec_kwd ~err:Syntax_err.Continue
let dec_kwd_do = dec_kwd ~err:Syntax_err.Do
let dec_kwd_export = dec_kwd ~err:Syntax_err.Export
let dec_kwd_for = dec_kwd ~err:Syntax_err.For
let dec_kwd_from = dec_kwd ~err:Syntax_err.From
let dec_kwd_await = dec_kwd ~err:Syntax_err.Await
let dec_kwd_var = dec_kwd ~err:Syntax_err.Var
let dec_kwd_in = dec_kwd ~err:Syntax_err.In
let dec_kwd_of = dec_kwd ~err:Syntax_err.Of
let dec_kwd_if = dec_kwd ~err:Syntax_err.If
let dec_kwd_else = dec_kwd ~err:Syntax_err.Else
let dec_kwd_typeof = dec_kwd ~err:Syntax_err.Typeof
let dec_kwd_try = dec_kwd ~err:Syntax_err.Try
let dec_kwd_catch = dec_kwd ~err:Syntax_err.Catch
let dec_kwd_require = dec_kwd ~err:Syntax_err.Require
let dec_kwd_delete = dec_kwd ~err:Syntax_err.Delete
let dec_kwd_finally = dec_kwd ~err:Syntax_err.Finally

(* Symbols *)

let dec_sym ?comments node ~err =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err err node
  | _ -> Ok (make_node ?comments node)

let dec_sym_asterisk = dec_sym ~err:Asterisk
let dec_sym_equal = dec_sym ~err:Equal
let dec_sym_strict_equal = dec_sym ~err:Strict_equal
let dec_sym_lparen = dec_sym ~err:Left_parenthesis
let dec_sym_rparen = dec_sym ~err:Right_parenthesis
let dec_sym_qmark = dec_sym ~err:Question_mark
let dec_sym_plus_equal = dec_sym ~err:Plus_equal
let dec_sym_minus_equal = dec_sym ~err:Minus_equal
let dec_sym_mult_equal = dec_sym ~err:Mult_equal
let dec_sym_div_equal = dec_sym ~err:Div_equal
let dec_sym_rem_equal = dec_sym ~err:Rem_equal
let dec_sym_xor_equal = dec_sym ~err:Xor_equal
let dec_sym_and_equal = dec_sym ~err:And_equal
let dec_sym_or_equal = dec_sym ~err:Or_equal
let dec_sym_shift_right_equal = dec_sym ~err:Right_shift_equal
let dec_sym_increment = dec_sym ~err:Increment
let dec_sym_decrement = dec_sym ~err:Decrement
let dec_sym_lbrace = dec_sym ~err:Left_brace
let dec_sym_rbrace = dec_sym ~err:Right_brace
let dec_sym_lbracket = dec_sym ~err:Left_bracket
let dec_sym_rbracket = dec_sym ~err:Right_bracket
let dec_sym_optional_chain = dec_sym ~err:Optional_chain
let dec_sym_backquote = dec_sym ~err:Backquote
let dec_sym_colon = dec_sym ~err:Colon
let dec_sym_ellipsis = dec_sym ~err:Ellipsis
let dec_sym_arrow = dec_sym ~err:Arrow
let dec_sym_emark = dec_sym ~err:Exclamation_mark
let dec_sym_dot = dec_sym ~err:Dot
let dec_sym_omitting = dec_sym ~err:Omitting_type_annotation
let dec_sym_adding = dec_sym ~err:Adding_type_annotation
let dec_sym_opting = dec_sym ~err:Opting_type_annotation
let dec_sym_and = dec_sym ~err:And
let dec_sym_vbar = dec_sym ~err:Vertical_bar
let dec_sym_unsigned_shift_right_equal = dec_sym ~err:Unsigned_shift_right_equal
let dec_sym_shift_left_equal = dec_sym ~err:Left_shift_equal
let dec_sym_exponent_equal = dec_sym ~err:Exponent_equal
let dec_sym_conjunction_equal = dec_sym ~err:Conjunction_equal
let dec_sym_disjunction_equal = dec_sym ~err:Disjunction_equal
let dec_sym_non_null_equal = dec_sym ~err:Non_null_equal
let dec_sym_tilde = dec_sym ~err:Tilde
let dec_sym_minus = dec_sym ~err:Minus
let dec_sym_plus = dec_sym ~err:Plus
let dec_sym_conjunction = dec_sym ~err:Conjunction
let dec_sym_disjunction = dec_sym ~err:Disjunction
let dec_sym_shift_right = dec_sym ~err:Right_shift
let dec_sym_unsigned_shift_right = dec_sym ~err:Unsigned_shift_right
let dec_sym_shift_left = dec_sym ~err:Left_shift
let dec_sym_xor = dec_sym ~err:Xor
let dec_sym_or = dec_sym ~err:Or
let dec_sym_div = dec_sym ~err:Div
let dec_sym_rem = dec_sym ~err:Rem
let dec_sym_exponent = dec_sym ~err:Exponent
let dec_sym_less_than = dec_sym ~err:Less_than
let dec_sym_less_than_or_equal = dec_sym ~err:Less_than_or_equal
let dec_sym_no_conv_equal = dec_sym ~err:No_conv_equal
let dec_sym_different = dec_sym ~err:Different
let dec_sym_no_conv_different = dec_sym ~err:No_conv_different
let dec_sym_greater_than_or_equal = dec_sym ~err:Greater_than_or_equal
let dec_sym_greater_than = dec_sym ~err:Greater_than
let dec_sym_non_null = dec_sym ~err:Non_null

(* Optional nodes *)

let make_opt decoder node = Option.map ~f:decoder node

let make_opt_res decode = function
  | None -> Ok None
  | Some value ->
    let* decoded = decode value in
    Ok (Some decoded)

(* Handling some modifiers *)

let mk_set_get_all node : (set_get_all option, _) result =
  let kwd_set = first_child_named_opt "set" node
  and kwd_get = first_child_named_opt "get" node
  and sym_asterisk = first_child_named_opt "*" node in
  match kwd_set, kwd_get, sym_asterisk with
  | None, None, None -> Ok None
  | Some kwd_set, _, _ ->
    let* kwd_set = dec_kwd_set kwd_set in
    Ok (Some (Set kwd_set))
  | _, Some kwd_get, _ ->
    let* kwd_get = dec_kwd_get kwd_get in
    Ok (Some (Get kwd_get))
  | _, _, Some sym_asterisk ->
    let* sym_asterisk = dec_sym_asterisk sym_asterisk in
    Ok (Some (All sym_asterisk))

(* Decoding children of the same type *)

let list_of_children ?(comments = []) decode children : ('a list, _) result =
  let f raw_child = List.cons (decode ?comments:None raw_child) in
  match children with
  | [] -> Ok []
  | fst_raw_child :: siblings ->
    let fst_child = decode ?comments:(Some comments) fst_raw_child in
    let children = fst_child :: List.fold_right ~f ~init:[] siblings in
    Result.all children

let ne_list_opt_of_children ?(comments = []) decode children
    : ('a ne_list option, _) result
  =
  let f raw_child = List.cons (decode ?comments:None raw_child) in
  match children with
  | [] -> Ok None
  | fst_raw_child :: siblings ->
    let* fst_child = decode ?comments:(Some comments) fst_raw_child in
    let* tail = List.fold_right ~f ~init:[] siblings |> Result.all in
    Ok (Some Nonempty_list.(fst_child :: tail))

let ne_list_of_children ?(comments = []) decode error children : ('a ne_list, _) result =
  let* list = ne_list_opt_of_children ~comments decode children in
  match list with
  | None -> error
  | Some ne_list -> Ok ne_list

let wrap_ne_list_opt_of_children ?(comments = []) decode children
    : ('a ne_list wrap option, _) result
  =
  let f raw_child = List.cons (decode ?comments:None raw_child) in
  match children with
  | [] -> Ok None
  | fst_raw_child :: siblings ->
    let* fst_child = decode ?comments:(Some comments) fst_raw_child in
    let fst_region = !get_region fst_raw_child in
    let* tail = List.fold_right ~f ~init:[] siblings |> Result.all in
    let region =
      match List.last siblings with
      | None -> fst_region
      | Some last_child -> Region.cover fst_region (!get_region last_child)
    in
    let ne_list = Nonempty_list.(fst_child :: tail) in
    Ok (Some (Wrap.make ne_list region))

(* Decoding enclosed unique child *)

let dec_enclosed ?(comments = []) node decode opening closing ~open_err ~close_err ~err
    : ('a enclosed wrap, _) result
  =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err err node
  | _ ->
    let comments = comments @ prev_comments node in
    let* opening = first_child_named opening node ~err:open_err in
    let* opening = dec_sym ~comments opening ~err:open_err in
    let* closing = first_child_named closing node ~err:close_err in
    let* closing = dec_sym closing ~err:close_err in
    let* child = (* We assume one child *) child_ranked 1 node ~err in
    let* contents = decode child in
    let region = !get_region node in
    Ok (Wrap.make { opening; contents; closing } region)

let dec_brackets ?comments node decode ~err : ('a brackets, _) result =
  let* brackets =
    dec_enclosed
      ?comments
      node
      decode
      "["
      "]"
      ~open_err:Left_bracket
      ~close_err:Right_bracket
      ~err
  in
  Ok (Brackets brackets)

let dec_parens ?comments node decode ~err : ('a parens, _) result =
  let* parens =
    dec_enclosed
      ?comments
      node
      decode
      "("
      ")"
      ~open_err:Left_parenthesis
      ~close_err:Right_parenthesis
      ~err
  in
  Ok (Parens parens)

(* Decoding enclosed lists of children *)

let dec_enclosed_list
    ?(comments = [])
    node
    decode
    opening
    closing
    ~open_err
    ~close_err
    ~err
    : ('a list enclosed wrap, _) result
  =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err err node
  | _ ->
    let comments = comments @ prev_comments node in
    let* opening = first_child_named opening node ~err:open_err in
    let* opening = dec_sym ~comments opening ~err:open_err in
    let* closing = first_child_named closing node ~err:close_err in
    let* closing = dec_sym closing ~err:close_err in
    let clauses = collect_named_children node in
    let* contents = list_of_children decode clauses in
    let region = !get_region node in
    Ok (Wrap.make { opening; contents; closing } region)

let dec_list_in_braces ?comments node decode ~err : ('a list braces, _) result =
  let* list =
    dec_enclosed_list
      ?comments
      node
      decode
      "{"
      "}"
      ~open_err:Left_brace
      ~close_err:Right_brace
      ~err
  in
  Ok (Braces list)

let dec_list_in_chevrons ?comments node decode ~err : ('a list chevrons, _) result =
  let* list =
    dec_enclosed_list
      ?comments
      node
      decode
      "<"
      ">"
      ~open_err:Left_chevron
      ~close_err:Right_chevron
      ~err
  in
  Ok (Chevrons list)

let dec_list_in_brackets ?comments node decode ~err : ('a list brackets, _) result =
  let* list =
    dec_enclosed_list
      ?comments
      node
      decode
      "["
      "]"
      ~open_err:Left_bracket
      ~close_err:Right_bracket
      ~err
  in
  Ok (Brackets list)

let dec_list_in_parens ?comments node decode ~err : ('a list parens, _) result =
  let* list =
    dec_enclosed_list
      ?comments
      node
      decode
      "("
      ")"
      ~open_err:Left_parenthesis
      ~close_err:Right_parenthesis
      ~err
  in
  Ok (Parens list)

(* Decoding enclosed non-empty lists *)

let dec_enclosed_ne_list
    ?(comments = [])
    node
    decode
    opening
    closing
    ~open_err
    ~close_err
    ~err
    : ('a ne_list enclosed wrap, _) result
  =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err err node
  | _ ->
    let comments = comments @ prev_comments node in
    let* opening = first_child_named opening node ~err:open_err in
    let* opening = dec_sym ~comments opening ~err:open_err in
    let* closing = first_child_named closing node ~err:close_err in
    let* closing = dec_sym closing ~err:close_err in
    let clauses = collect_named_children node in
    let error = mk_err err node in
    let* contents = ne_list_of_children decode error clauses in
    let region = !get_region node in
    Ok (Wrap.make { opening; contents; closing } region)

let dec_ne_list_in_chevrons ?comments node decode ~err : ('a ne_list chevrons, _) result =
  let* chevrons =
    dec_enclosed_ne_list
      ?comments
      node
      decode
      "<"
      ">"
      ~open_err:Left_chevron
      ~close_err:Right_chevron
      ~err
  in
  Ok (Chevrons chevrons)

(*
let dec_ne_list_in_brackets ?comments node decode error ~err
    : ('a ne_list brackets, _) result
  =
  let* brackets =
    dec_enclosed_ne_list
      ?comments
      node
      decode
      error
      "["
      "]"
      ~open_err:Left_bracket
      ~close_err:Right_bracket
      ~err
  in
  Ok (Brackets brackets)
 *)

(* STATEMENTS

   The JavaScript tree-sitter grammar has the non-terminals
   "statement" be a supertype, that is, a hidden rule. *)

let rec dec_statements ?(comments = []) node : (statements, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Statement node
  | _ ->
    let children = collect_named_children node in
    wrap_ne_list_opt_of_children ~comments dec_statement children

and dec_statement ?(comments = []) node : (statement, _) result =
  match get_name node with
  | "export_statement" ->
    let* statement = wrap dec_export_statement ~comments node in
    Ok (S_export_statement statement)
  | "import_statement" ->
    let* statement = wrap dec_import_statement ~comments node in
    Ok (S_import_statement statement)
  | "debugger_statement" ->
    let* kwd_debugger = dec_kwd_debugger ~comments node in
    Ok (S_debugger_statement kwd_debugger)
  | "expression_statement" ->
    let* expression = dec_expression_statement ~comments node in
    Ok (S_expression_statement expression)
  | "statement_block" ->
    let* statement = dec_statement_block ~comments node in
    Ok (S_statement_block statement)
  | "if_statement" ->
    let* statement = wrap dec_if_statement ~comments node in
    Ok (S_if_statement statement)
  | "switch_statement" ->
    let* statement = wrap dec_switch_statement node in
    Ok (S_switch_statement statement)
  | "for_statement" ->
    let* statement = wrap dec_for_statement node in
    Ok (S_for_statement statement)
  | "for_in_statement" ->
    let* statement = wrap dec_for_in_statement node in
    Ok (S_for_in_statement statement)
  | "while_statement" ->
    let* statement = wrap dec_while_statement node in
    Ok (S_while_statement statement)
  | "do_statement" ->
    let* statement = wrap dec_do_statement ~comments node in
    Ok (S_do_statement statement)
  | "try_statement" ->
    let* statement = wrap dec_try_statement node in
    Ok (S_try_statement statement)
  | "with_statement" ->
    let* statement = wrap dec_with_statement node in
    Ok (S_with_statement statement)
  | "break_statement" ->
    let* statement = wrap dec_break_statement node in
    Ok (S_break_statement statement)
  | "continue_statement" ->
    let* statement = wrap dec_continue_statement node in
    Ok (S_continue_statement statement)
  | "return_statement" ->
    let* statement = wrap dec_return_statement node in
    Ok (S_return_statement statement)
  | "throw_statement" ->
    let* statement = wrap dec_throw_statement node in
    Ok (S_throw_statement statement)
  | "empty_statement" -> Ok (S_empty_statement (!get_region node))
  | "labeled_statement" ->
    let* statement = wrap dec_labeled_statement node in
    Ok (S_labeled_statement statement)
  (* "declaration" is a hidden rule *)
  | _ ->
    let* declaration = dec_declaration ~comments node in
    Ok (S_declaration_statement declaration)

(* Export statement *)

and dec_export_statement ?(comments = []) node : (export_statement, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Export node
  | _ ->
    let comments = comments @ prev_comments node in
    (* Decorators? *)
    let* kwd_export = first_child_named "export" node ~err:Export in
    let* after_export = next_sibling kwd_export ~err:Export_clause_or_all in
    let* kwd_export = dec_kwd_export ~comments kwd_export in
    let* export_kind =
      match get_name after_export with
      | "*" ->
        let* kwd_from = first_child_named "from" node ~err:From in
        let* from_clause = dec_from_clause node kwd_from in
        Ok (Export_from from_clause)
      | "namespace_export" ->
        let* kwd_from = first_child_named "from" node ~err:From in
        let* namespace_export = dec_namespace_export after_export in
        let* from_clause = dec_from_clause node kwd_from in
        Ok (Export_as (namespace_export, from_clause))
      | "export_clause" ->
        let kwd_from = first_child_named_opt "from" node in
        let* export_clause = dec_export_clause after_export in
        let* from_clause = make_opt_res (dec_from_clause node) kwd_from in
        Ok (Export_clause (export_clause, from_clause))
      | "default" -> dec_export_default after_export node
      | "type" ->
        let* export_type = dec_export_type after_export node in
        Ok (Export_type export_type)
      | "=" ->
        let* expression = next_sibling after_export ~err:Expression in
        let* expression = dec_expression expression in
        let* sym_equal = dec_sym_equal after_export in
        Ok (Export_equal (sym_equal, expression))
      | "as" ->
        let* kwd_namespace = first_child_named "namespace" node ~err:Namespace in
        let* kwd_namespace = dec_kwd_namespace kwd_namespace in
        let* identifier = first_child_named "identifier" node ~err:Identifier in
        Ok (Export_as_namespace (kwd_namespace, dec_identifier identifier))
      | _ ->
        let* export_declaration = dec_export_declaration after_export node in
        Ok (Export_declaration export_declaration)
    in
    Ok { kwd_export; export_kind }

and dec_export_type after_export node : (export_type, _) result =
  let* export_clause = next_sibling after_export ~err:Export_clause in
  let* kwd_type = dec_kwd_type after_export in
  let* export_clause = dec_export_clause export_clause in
  let kwd_from = first_child_named_opt "from" node in
  let* from_clause = make_opt_res (dec_from_clause node) kwd_from in
  Ok { kwd_type; export_clause; from_clause }

and dec_export_declaration after_export node : (declaration decorated, _) result =
  let decorators = children_named "decorator" node in
  let* declaration = dec_declaration after_export in
  dec_decorated decorators declaration

and dec_decorated : 'a. ts_forest -> 'a -> ('a decorated, _) result =
 fun decorators decorated ->
  let* decorators = list_of_children dec_decorator decorators in
  Ok { decorators; decorated }

and dec_export_clause node : (export_clause, _) result =
  dec_list_in_braces node dec_export_specifier ~err:Export_clause

and dec_export_specifier ?(comments = []) node : (export_specifier, _) result =
  let comments = comments @ prev_comments node in
  let* name_field = child_with_field "name" node ~err:Identifier_or_string in
  let* name = dec_module_export_name ~comments name_field in
  let alias_field = child_with_field_opt "alias" node in
  let* alias = make_opt_res dec_module_export_name alias_field in
  let* alias =
    match alias with
    | None -> Ok None
    | Some alias ->
      let* kwd_as = first_child_named "as" node ~err:As in
      let* kwd_as = dec_kwd_as kwd_as in
      Ok (Some (kwd_as, alias))
  in
  Ok ({ name; alias } : export_specifier)

and dec_module_export_name ?(comments = []) node : (module_export_name, _) result =
  match get_name node with
  | "identifier" -> Ok (Export_ident (dec_identifier ~comments node))
  | "string" -> Ok (Export_string (dec_string ~comments node))
  | _ -> mk_err Identifier_or_string node

and dec_from_clause node kwd_from : (from_clause, _) result =
  let* source_field = child_with_field "source" node ~err:File_path in
  let* kwd_from = dec_kwd_from kwd_from in
  Ok (kwd_from, dec_string source_field)

and dec_namespace_export ?(comments = []) node : (namespace_export, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Namespace_export node
  | _ ->
    let comments = comments @ prev_comments node in
    let* sym_asterisk = first_child_named "*" node ~err:Asterisk in
    let* sym_asterisk = dec_sym_asterisk ~comments sym_asterisk in
    let* kwd_as = first_child_named "as" node ~err:As in
    let* module_export_name = next_sibling kwd_as ~err:Identifier_or_string in
    let* kwd_as = dec_kwd_as kwd_as in
    let* namespace_name = dec_module_export_name module_export_name in
    Ok { sym_asterisk; kwd_as; namespace_name }

and dec_export_default after_export node : (export_kind, _) result =
  let decorators = children_named "decorator" node in
  let* kwd_default = dec_kwd_default after_export in
  match child_with_field_opt "declaration" node with
  | None ->
    let* value_field = child_with_field "value" node ~err:Expression in
    let* expression = dec_expression value_field in
    let contents = kwd_default, expression in
    let* decorated = dec_decorated decorators contents in
    Ok (Export_default_expression decorated)
  | Some declaration ->
    let* declaration = dec_declaration declaration in
    let contents = kwd_default, declaration in
    let* decorated = dec_decorated decorators contents in
    Ok (Export_default_declaration decorated)

(* Import statement *)

and dec_import_statement ?(comments = []) node : (import_statement, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Import node
  | _ ->
    let comments = comments @ prev_comments node in
    let* kwd_import = first_child_named "import" node ~err:Import in
    let* kwd_import = dec_kwd_import ~comments kwd_import in
    let* import_kind =
      match first_child_named_opt "type" node with
      | Some kwd_type ->
        let* kwd_type = dec_kwd_type kwd_type in
        Ok (Some (Import_type kwd_type))
      | None ->
        (match first_child_named_opt "typeof" node with
        | None -> Ok None
        | Some kwd_typeof ->
          let* kwd_typeof = dec_kwd_typeof kwd_typeof in
          Ok (Some (Import_typeof kwd_typeof)))
    in
    let import_attribute = first_child_named_opt "import_attribute" node in
    let* import_attribute = make_opt_res dec_import_attribute import_attribute in
    let* (import : import) =
      match first_child_named_opt "import_clause" node with
      | Some import_clause ->
        let* kwd_from = first_child_named "from" node ~err:From in
        let* import_clause = dec_import_clause import_clause in
        let* from_clause = dec_from_clause node kwd_from in
        Ok (Import_clause (import_clause, from_clause))
      | None ->
        (match first_child_named_opt "import_require_clause" node with
        | Some clause ->
          let* require_clause = wrap dec_import_require_clause clause in
          Ok (Import_require_clause require_clause)
        | None ->
          let* source_field = child_with_field "source" node ~err:String in
          Ok (Import_source (dec_string source_field)))
    in
    Ok { kwd_import; import_kind; import; import_attribute }

and dec_import_clause ?(comments = []) node : (import_clause, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Import_clause node
  | _ ->
    let comments = comments @ prev_comments node in
    let* fst_child = child_ranked 0 node ~err:Named_imports_or_all_or_id in
    (match get_name fst_child with
    | "namespace_import" ->
      let* namespace_import = wrap dec_namespace_import ~comments fst_child in
      Ok (Import_namespace namespace_import : import_clause)
    | "named_imports" ->
      let* named_imports = dec_named_imports ~comments fst_child in
      Ok (Import_named named_imports : import_clause)
    | "identifier" ->
      let ident = dec_identifier ~comments fst_child in
      let* from =
        match next_sibling_opt fst_child with
        | None -> Ok None
        | Some comma ->
          let* next = next_sibling comma ~err:Named_imports_or_all in
          let* next = dec_namespace_or_named_imports next in
          Ok (Some next)
      in
      Ok (Import_ident (ident, from))
    | _ -> mk_err Namespace_or_named_imports_or_ident fst_child)

and dec_namespace_or_named_imports node : (namespace_or_named_imports, _) result =
  match get_name node with
  | "namespace_import" ->
    let* namespace_import = wrap dec_namespace_import node in
    Ok (Import_namespace namespace_import)
  | "named_imports" ->
    let* named_imports = dec_named_imports node in
    Ok (Import_named named_imports)
  | _ -> mk_err Namespace_or_named_imports node

and dec_namespace_import ?(comments = []) node : (namespace_import, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Namespace_import node
  | _ ->
    let comments = comments @ prev_comments node in
    let* sym_asterisk = first_child_named "*" node ~err:Asterisk in
    let* sym_asterisk = dec_sym_asterisk ~comments sym_asterisk in
    let* kwd_as = first_child_named "as" node ~err:As in
    let* identifier = next_sibling kwd_as ~err:Identifier in
    let* kwd_as = dec_kwd_as kwd_as in
    let identifier = dec_identifier identifier in
    Ok { sym_asterisk; kwd_as; identifier }

and dec_named_imports ?(comments = []) node : (named_imports, _) result =
  dec_list_in_braces ~comments node dec_import_specifier ~err:Named_imports

and dec_import_specifier ?(comments = []) node : (import_specifier, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Import_specifier node
  | _ ->
    let comments = comments @ prev_comments node in
    let* (import_kind : import_kind option) =
      match first_child_named_opt "type" node with
      | Some kwd_type ->
        let* kwd_type = dec_kwd_type ~comments kwd_type in
        Ok (Some (Import_type kwd_type))
      | None ->
        (match first_child_named_opt "typeof" node with
        | None -> Ok None
        | Some kwd_typeof ->
          let* kwd_typeof = dec_kwd_typeof ~comments kwd_typeof in
          Ok (Some (Import_typeof kwd_typeof)))
    in
    let snd_child_comments =
      match import_kind with
      | None -> comments
      | Some _ -> []
    in
    let* name_field = child_with_field "name" node ~err:Identifier in
    let* (import_specifier' : import_specifier') =
      match child_with_field_opt "alias" node with
      | None ->
        Ok (Import_spec_name (dec_identifier ~comments:snd_child_comments name_field))
      | Some alias_field ->
        let* kwd_as = first_child_named "as" node ~err:As in
        let* name = dec_module_export_name ~comments:snd_child_comments name_field in
        let* kwd_as = dec_kwd_as kwd_as in
        let alias = dec_identifier alias_field in
        Ok (Import_spec_alias { name; kwd_as; alias })
    in
    Ok (import_kind, import_specifier')

and dec_import_require_clause ?(comments = []) node : (import_require_clause, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Import_require_clause node
  | _ ->
    let comments = comments @ prev_comments node in
    let* identifier = child_ranked 0 node ~err:Identifier in
    let ident = dec_identifier ~comments identifier in
    let* sym_equal = first_child_named "=" node ~err:Equal in
    let* sym_equal = dec_sym_equal sym_equal in
    let* kwd_require = first_child_named "require" node ~err:Require in
    let* kwd_require = dec_kwd_require kwd_require in
    let* sym_lparen = first_child_named "(" node ~err:Left_parenthesis in
    let* sym_lparen = dec_sym_lparen sym_lparen in
    let* source_field = child_with_field "source" node ~err:String in
    let source = dec_string source_field in
    let* sym_rparen = first_child_named ")" node ~err:Right_parenthesis in
    let* sym_rparen = dec_sym_rparen sym_rparen in
    Ok { ident; sym_equal; kwd_require; sym_lparen; source; sym_rparen }

and dec_import_attribute node : (import_attribute, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Import_attribute node
  | _ ->
    let* kind_node = child_ranked 0 node ~err:Import_attribute in
    let* object_node = child_ranked 1 node ~err:Object_expression in
    let* expression = dec_object_expr object_node in
    (match get_name kind_node with
    | "with" ->
      let* kwd_with = dec_kwd_with kind_node in
      Ok (Import_with (kwd_with, expression))
    | "assert" ->
      let* kwd_assert = dec_kwd_assert kind_node in
      Ok (Import_assert (kwd_assert, expression))
    | _ -> mk_err Import_attribute node)

(* Expression statements

   {@js[
    expression_statement: $ => seq($._expressions, $._semicolon),
    _expressions: $ => choice($.expression, $.sequence_expression),
    sequence_expression: $ => prec.right(commaSep1($.expression))
   ]}

   See [dec_expression]. *)

and dec_expression_statement ?(comments = []) node : (expression_statement, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Expression node
  | _ ->
    let comments = comments @ prev_comments node in
    let* child = named_child_ranked 0 node ~err:Expression in
    dec_expressions ~comments child

and dec_expressions ?(comments = []) (node : ts_tree) : (expressions, _) result =
  match get_name node with
  | "sequence_expression" -> dec_sequence_expression ~comments node
  | _ ->
    let* expression = dec_expression ~comments node in
    let region = !get_region node in
    Ok (Wrap.make Nonempty_list.[ expression ] region)

(* Statement blocks *)

and dec_statement_block ?(comments = []) node : (statement_block, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Block node
  | _ ->
    let comments = comments @ prev_comments node in
    let* opening = first_child_named "{" node ~err:Left_brace in
    let* opening = dec_sym_lbrace ~comments opening in
    let* closing = first_child_named "}" node ~err:Right_brace in
    let* closing = dec_sym_rbrace closing in
    let clauses = collect_named_children node in
    let* contents = wrap_ne_list_opt_of_children dec_statement clauses in
    let region = !get_region node in
    let braces = Wrap.make { opening; contents; closing } region in
    Ok (Braces braces)

(* If statement *)

and dec_if_statement ?(comments = []) node : (if_statement, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err If node
  | _ ->
    let* kwd_if = first_child_named "if" node ~err:If in
    let* kwd_if = dec_kwd_if ~comments kwd_if in
    let* condition_field =
      child_with_field "condition" node ~err:Parenthesized_expression
    in
    let* condition = dec_parenthesized_expression condition_field in
    let* consequence_field = child_with_field "consequence" node ~err:Statement in
    let* consequence = dec_statement consequence_field in
    let alternative_field = child_with_field_opt "alternative" node in
    let* alternative = make_opt_res dec_else_clause alternative_field in
    Ok { kwd_if; condition; consequence; alternative }

and dec_else_clause ?(comments = []) node : (kwd_else * statement, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Else node
  | _ ->
    let comments = comments @ prev_comments node in
    let* kwd_else = first_child_named "else" node ~err:Else in
    let* statement = next_sibling kwd_else ~err:Statement in
    let* statement = dec_statement statement in
    let* kwd_else = dec_kwd_else ~comments kwd_else in
    Ok (kwd_else, statement)

(* Switch statement *)

and dec_switch_statement ?(comments = []) node : (switch_statement, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Switch node
  | _ ->
    let* kwd_switch = first_child_named "switch" node ~err:Switch in
    let* kwd_switch = dec_kwd_switch ~comments kwd_switch in
    let* value_field = child_with_field "value" node ~err:Parenthesized_expression in
    let* value = dec_parenthesized_expression value_field in
    let* body_field = child_with_field "body" node ~err:Switch_body in
    let* body = dec_switch_body body_field in
    Ok { kwd_switch; value; body }

and dec_switch_body node : (switch_body, _) result =
  dec_list_in_braces node dec_switch_entry ~err:Switch_body

and dec_switch_entry ?(comments = []) node : (switch_entry, _) result =
  match get_name node with
  | "switch_case" ->
    let* switch_case = wrap dec_switch_case ~comments node in
    Ok (Switch_case switch_case)
  | "switch_default" ->
    let* default = wrap dec_switch_default ~comments node in
    Ok (Switch_default default)
  | _ -> mk_err Switch_body node

and dec_switch_case ?(comments = []) node : (switch_case, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Case node
  | _ ->
    let comments = comments @ prev_comments node in
    let* kwd_case = first_child_named "case" node ~err:Case in
    let* kwd_case = dec_kwd_case ~comments kwd_case in
    let* value_field = child_with_field "value" node ~err:Expression in
    let* value = dec_expressions value_field in
    let children = collect_children node in
    let stmt_children = skip_until_colon children in
    let* body = wrap_ne_list_opt_of_children dec_statement stmt_children in
    Ok { kwd_case; value; body }

and dec_switch_default ?(comments = []) node : (switch_default, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Default node
  | _ ->
    let comments = comments @ prev_comments node in
    let* kwd_default = first_child_named "default" node ~err:Default in
    let* kwd_default = dec_kwd_default ~comments kwd_default in
    let statements = collect_named_children node in
    let* statements = wrap_ne_list_opt_of_children dec_statement statements in
    Ok { kwd_default; statements }

(* For statement *)

and dec_for_statement ?(comments = []) node : (for_statement, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err For node
  | _ ->
    let* kwd_for = first_child_named "for" node ~err:For in
    let* kwd_for = dec_kwd_for ~comments kwd_for in
    let* sym_lparen = first_child_named "(" node ~err:Left_parenthesis in
    let* sym_lparen = dec_sym_lparen sym_lparen in
    let* initializer_field =
      child_with_field "initializer" node ~err:Initial_assignment
    in
    let* initializer_ = dec_for_initializer initializer_field in
    let* condition_field =
      child_with_field "condition" node ~err:Expression_or_semicolon
    in
    let* condition = dec_for_condition condition_field in
    let increment_field = child_with_field_opt "increment" node in
    let* increment = make_opt_res dec_expressions increment_field in
    let* sym_rparen = first_child_named ")" node ~err:Right_parenthesis in
    let* sym_rparen = dec_sym_rparen sym_rparen in
    let* body_field = child_with_field "body" node ~err:Statement in
    let* body = dec_statement body_field in
    Ok { kwd_for; sym_lparen; initializer_; condition; increment; sym_rparen; body }

and dec_for_initializer node : (for_initializer, _) result =
  match get_name node with
  | "lexical_declaration" ->
    let* declaration = wrap dec_lexical_declaration node in
    Ok (For_lexical_declaration declaration)
  | "variable_declaration" ->
    let* declaration = wrap dec_variable_declaration node in
    Ok (For_variable_declaration declaration)
  | "expression_statement" ->
    let* expression = dec_expression_statement node in
    Ok (For_expression_statement expression)
  | "empty_statement" -> Ok (For_empty_statement (!get_region node))
  | _ -> mk_err Initial_assignment node

and dec_for_condition node : (for_condition, _) result =
  match get_name node with
  | "expression_statement" ->
    let* expression = dec_expression_statement node in
    Ok (For_condition_expression expression)
  | "empty_statement" -> Ok (For_condition_empty (!get_region node))
  | _ -> mk_err Expression_or_semicolon node

(* For-in statement *)

and dec_for_in_statement ?(comments = []) node : (for_in_statement, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err For_or_await node
  | _ ->
    let* kwd_for = first_child_named "for" node ~err:For in
    let* kwd_for = dec_kwd_for ~comments kwd_for in
    let kwd_await = first_child_named_opt "await" node in
    let* kwd_await = make_opt_res dec_kwd_await kwd_await in
    let* sym_lparen = first_child_named "(" node ~err:Left_parenthesis in
    let* sym_lparen = dec_sym_lparen sym_lparen in
    let kind_field = child_with_field_opt "kind" node in
    let* left_field = child_with_field "left" node ~err:Expression in
    let* sym_rparen = first_child_named ")" node ~err:Right_parenthesis in
    let* sym_rparen = dec_sym_rparen sym_rparen in
    let* body_field = child_with_field "body" node ~err:Statement in
    let* body = dec_statement body_field in
    let* operator_field = child_with_field "operator" node ~err:In_or_of in
    let* operator = dec_for_operator operator_field in
    let* right_field = child_with_field "right" node ~err:Expression in
    let* collection = dec_expressions right_field in
    let* (range : for_range) =
      match kind_field with
      | None ->
        (match get_name left_field with
        | "parenthesized_expression" ->
          let* expression = dec_parenthesized_expression left_field in
          Ok (For_in_parenthesized expression)
        | _ ->
          let* expression = dec_lhs_expression left_field in
          Ok (For_in_expression expression))
      | Some kind_field ->
        let* variable =
          match get_name left_field with
          | "ERROR" | "MISSING" | "NULL" -> mk_err Pattern left_field
          | "identifier" -> Ok (For_in_ident (dec_identifier left_field))
          | _ ->
            let* pattern = dec_destructuring_pattern left_field in
            Ok (For_in_pattern pattern)
        in
        (match get_name kind_field with
        | "var" ->
          let* kwd_var = dec_kwd_var kind_field in
          let value_field = child_with_field_opt "value" node in
          let* default = make_opt_res dec_expression value_field in
          Ok (For_in_var { kwd_var; variable; default })
        | "let" ->
          let* kwd_let = dec_kwd_let kind_field in
          Ok (For_in_let (kwd_let, variable))
        | "const" ->
          let* kwd_const = dec_kwd_const kind_field in
          Ok (For_in_const (kwd_const, variable))
        | _ -> mk_err Let_or_const_or_var kind_field)
    in
    let for_header : for_header = { range; operator; collection } in
    Ok { kwd_for; kwd_await; sym_lparen; for_header; sym_rparen; body }

and dec_for_operator node : (for_operator, _) result =
  match get_name node with
  | "in" ->
    let* kwd_in = dec_kwd_in node in
    Ok (In kwd_in)
  | "of" ->
    let* kwd_of = dec_kwd_of node in
    Ok (Of kwd_of)
  | _ -> mk_err In_or_of node

(* While statement *)

and dec_while_statement ?(comments = []) node : (while_statement, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err While node
  | _ ->
    let* kwd_while = first_child_named "while" node ~err:While in
    let* kwd_while = dec_kwd_while ~comments kwd_while in
    let* condition_field =
      child_with_field "condition" node ~err:Parenthesized_expression
    in
    let* condition = dec_parenthesized_expression condition_field in
    let* body_field = child_with_field "body" node ~err:Statement in
    let* body = dec_statement body_field in
    Ok { kwd_while; condition; body }

(* Do statement *)

and dec_do_statement ?(comments = []) node : (do_statement, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Do node
  | _ ->
    let* kwd_do = first_child_named "do" node ~err:Do in
    let* kwd_do = dec_kwd_do ~comments kwd_do in
    let* body_field = child_with_field "body" node ~err:Statement in
    let* body = dec_statement body_field in
    let* kwd_while = first_child_named "while" node ~err:While in
    let* kwd_while = dec_kwd_while kwd_while in
    let* condition_field =
      child_with_field "condition" node ~err:Parenthesized_expression
    in
    let* condition = dec_parenthesized_expression condition_field in
    Ok { kwd_do; body; kwd_while; condition }

(* Try statement *)

and dec_try_statement ?(comments = []) node : (try_statement, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Try node
  | _ ->
    let* kwd_try = first_child_named "try" node ~err:Try in
    let* kwd_try = dec_kwd_try ~comments kwd_try in
    let* body_field = child_with_field "body" node ~err:Block in
    let* body = dec_statement_block body_field in
    let handler_field = child_with_field_opt "handler" node in
    let* handler = make_opt_res dec_catch_clause handler_field in
    let finalizer_field = child_with_field_opt "finalizer" node in
    let* finalizer = make_opt_res dec_finally_clause finalizer_field in
    Ok { kwd_try; body; handler; finalizer }

and dec_catch_clause node : (catch_clause, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Catch node
  | _ ->
    let* kwd_catch = first_child_named "catch" node ~err:Catch in
    let* kwd_catch = dec_kwd_catch kwd_catch in
    let parameter_field = child_with_field_opt "parameter" node in
    let* parameter = make_opt_res (dec_catch_parameter node) parameter_field in
    let* body_field = child_with_field "body" node ~err:Block in
    let* body = dec_statement_block body_field in
    Ok { kwd_catch; parameter; body }

and dec_catch_parameter node param : (catch_parameter, _) result =
  let* catch_parameter = dec_catch_parameter_kind param in
  let* sym_lparen = first_child_named "(" node ~err:Left_parenthesis in
  let* sym_lparen = dec_sym_lparen sym_lparen in
  let type_field = child_with_field_opt "type" node in
  let* type_opt = make_opt_res dec_type_annotation type_field in
  let* sym_rparen = first_child_named ")" node ~err:Right_parenthesis in
  let* sym_rparen = dec_sym_rparen sym_rparen in
  Ok { sym_lparen; catch_parameter; type_opt; sym_rparen }

and dec_catch_parameter_kind node : (catch_parameter_kind, _) result =
  match get_name node with
  | "identifier" -> Ok (Catch_identifier (dec_identifier node))
  | "object_pattern" ->
    let* pattern = dec_object_pattern node in
    Ok (Catch_object_pattern pattern)
  | "array_pattern" ->
    let* pattern = dec_array_pattern node in
    Ok (Catch_array_pattern pattern)
  | _ -> mk_err Pattern node

and dec_finally_clause node : (finally_clause, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Finally node
  | _ ->
    let* kwd_finally = first_child_named "finally" node ~err:Finally in
    let* kwd_finally = dec_kwd_finally kwd_finally in
    let* body_field = child_with_field "body" node ~err:Block in
    let* finalizer_block = dec_statement_block body_field in
    Ok (kwd_finally, finalizer_block)

(* Type annotation *)

and dec_type_annotation node : (type_annotation, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Type_annotation node
  | _ ->
    let* sym_colon = first_child_named ":" node ~err:Colon in
    let* sym_colon = dec_sym_colon sym_colon in
    let* type_child = named_child_ranked 0 node ~err:Type_expression in
    let* type_expr = dec_type type_child in
    Ok (sym_colon, type_expr)

(* With statement *)

and dec_with_statement ?(comments = []) node : (with_statement, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err With node
  | _ ->
    let* kwd_with = first_child_named "with" node ~err:With in
    let* kwd_with = dec_kwd_with ~comments kwd_with in
    let* object_field = child_with_field "object" node ~err:Parenthesized_expression in
    let* object_expr = dec_parenthesized_expression object_field in
    let* body_field = child_with_field "body" node ~err:Statement in
    let* body = dec_statement body_field in
    Ok { kwd_with; object_expr; body }

(* Break statement *)

and dec_break_statement ?(comments = []) node : (break_statement, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Break node
  | _ ->
    let* kwd_break = first_child_named "break" node ~err:Break in
    let* kwd_break = dec_kwd_break ~comments kwd_break in
    let label_field = child_with_field_opt "label" node in
    let stmt_id = make_opt dec_identifier label_field in
    Ok { kwd_break; stmt_id }

(* Continue statement *)

and dec_continue_statement ?(comments = []) node : (continue_statement, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Continue node
  | _ ->
    let* kwd_continue = first_child_named "continue" node ~err:Continue in
    let* kwd_continue = dec_kwd_continue ~comments kwd_continue in
    let label_field = child_with_field_opt "label" node in
    let stmt_id = make_opt dec_identifier label_field in
    Ok { kwd_continue; stmt_id }

(* Return statement

   NOTE: The Javascript grammar states:

   {@js[
   return_statement: $ =>
     seq('return', optional($._expressions), $._semicolon),

   _semicolon: $ => choice($._automatic_semicolon, ';')
   ]}

   but the child of rank 1 is sometimes missing, as if
   "_automatic_semicolon" could derive the empty word. Other rules use
   "optional(_automatic_semicolon)", which adds to the mystery. *)

and dec_return_statement ?(comments = []) node : (return_statement, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Return node
  | _ ->
    let* kwd_return = first_child_named "return" node ~err:Return in
    let* kwd_return = dec_kwd_return ~comments kwd_return in
    (match child_ranked_opt 1 node with
    | None -> Ok { kwd_return; expressions = None }
    | Some snd_child ->
      (match get_name snd_child with
      | ";" -> Ok { kwd_return; expressions = None }
      | _ ->
        let* expressions = dec_expressions snd_child in
        Ok { kwd_return; expressions = Some expressions }))

(* Throw statement *)

and dec_throw_statement ?(comments = []) node : (throw_statement, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Throw node
  | _ ->
    let* kwd_throw = first_child_named "throw" node ~err:Throw in
    let* kwd_throw = dec_kwd_throw ~comments kwd_throw in
    let* expr = child_ranked 1 node ~err:Expression in
    let* expressions = dec_expressions expr in
    Ok { kwd_throw; expressions }

(* Labeled statement *)

and dec_labeled_statement ?(comments = []) node : (labeled_statement, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Label node
  | _ ->
    let* label_field = child_with_field "label" node ~err:Label in
    let label = dec_identifier ~comments label_field in
    let* sym_colon = first_child_named ":" node ~err:Colon in
    let* sym_colon = dec_sym_colon sym_colon in
    let* body_field = child_with_field "body" node ~err:Statement in
    let* body = dec_statement body_field in
    Ok { label; sym_colon; body }

(* DECLARATION

   The JavaScript tree-sitter grammar has the non-terminal
   "declaration" be a supertype, that is, a hidden rule. *)

and dec_declaration ?(comments = []) node : (declaration, _) result =
  let comments = comments @ prev_comments node in
  match get_name node with
  | "function_declaration" ->
    let* fun_decl = wrap dec_function_declaration ~comments node in
    Ok (D_function_declaration fun_decl)
  | "generator_function_declaration" ->
    let* generator = wrap dec_generator_function_declaration ~comments node in
    Ok (D_generator_function_declaration generator)
  | "class_declaration" ->
    let* declaration = wrap dec_class_declaration ~comments node in
    Ok (D_class_declaration declaration)
  | "lexical_declaration" ->
    let* declaration = wrap dec_lexical_declaration ~comments node in
    Ok (D_lexical_declaration declaration)
  | "variable_declaration" ->
    let* declaration = wrap dec_variable_declaration node in
    Ok (D_variable_declaration declaration)
  | "function_signature" ->
    let* declaration = wrap dec_function_signature node in
    Ok (D_function_signature declaration)
  | "abstract_class_declaration" ->
    let* declaration = wrap dec_abstract_class_declaration node in
    Ok (D_abstract_class_declaration declaration)
  | "module" ->
    let* declaration = wrap dec_module_declaration node in
    Ok (D_module declaration)
  | "internal_module" ->
    let* declaration = wrap dec_internal_module ~comments node in
    Ok (D_internal_module declaration)
  | "type_alias_declaration" ->
    let* declaration = wrap dec_type_alias_declaration ~comments node in
    Ok (D_type_alias_declaration declaration)
  | "enum_declaration" ->
    let* declaration = wrap dec_enum_declaration node in
    Ok (D_enum_declaration declaration)
  | "interface_declaration" ->
    let* declaration = wrap dec_interface_declaration node in
    Ok (D_interface_declaration declaration)
  | "import_alias" ->
    let* declaration = wrap dec_import_alias node in
    Ok (D_import_alias declaration)
  | "ambient_declaration" ->
    let* declaration = wrap dec_ambient_declaration node in
    Ok (D_ambient_declaration declaration)
  | _ -> mk_err Declaration node

(* Function declaration (see [dec_function_signature]) *)

and dec_function_declaration ?(comments = []) node : (function_declaration, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Function_declaration node
  | _ ->
    let* fun_sig = dec_function_signature ~comments node in
    let* body_field = child_with_field "body" node ~err:Block in
    let* body = dec_statement_block body_field in
    Ok { fun_sig; body }

(* Accessibility modifier *)

and dec_accessibility_modifier node : (accessibility_modifier, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Public_private_protected node
  | _ ->
    let* child = child_ranked 0 node ~err:Public_private_protected in
    (match get_name child with
    | "public" ->
      let* kwd_public = dec_kwd_public child in
      Ok (Public kwd_public)
    | "private" ->
      let* kwd_private = dec_kwd_private child in
      Ok (Private kwd_private)
    | "protected" ->
      let* kwd_protected = dec_kwd_protected child in
      Ok (Protected kwd_protected)
    | _ -> mk_err Public_private_protected node)

(* Override modifier *)

and dec_override_modifier node : (kwd_override, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Override node
  | _ ->
    let* child = first_child_named "override" node ~err:Override in
    let* kwd_override = dec_kwd_override child in
    (* TODO: Test. See [Print_cst] *)
    Ok kwd_override

(* Return type annotation *)

and dec_call_return_type node : (call_return_type, _) result =
  match get_name node with
  | "type_annotation" ->
    let* annotation = dec_type_annotation node in
    Ok (Type_annotation annotation : call_return_type)
  | "asserts_annotation" ->
    let* annotation = dec_asserts_annotation node in
    Ok (Asserts_annotation annotation)
  | "type_predicate_annotation" ->
    let* annotation = wrap dec_type_predicate_annotation node in
    Ok (Type_predicate_annotation annotation)
  | _ -> mk_err Type_expression node

(* Asserts annotation *)

and dec_asserts_annotation node : (asserts_annotation, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Asserts_annotation node
  | _ ->
    let* asserts = first_child_named "asserts" node ~err:Asserts in
    dec_asserts asserts

and dec_asserts node : (asserts_annotation, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Asserts node
  | _ ->
    let* kwd_asserts = first_child_named "asserts" node ~err:Asserted in
    let* kwd_asserts = dec_kwd_asserts kwd_asserts in
    let* child = child_ranked 1 node ~err:Asserted in
    (match get_name child with
    | "type_predicate" ->
      let* predicate = wrap dec_type_predicate child in
      Ok (Assert_predicate (kwd_asserts, predicate))
    | "identifier" -> Ok (Assert_type (kwd_asserts, dec_identifier child))
    | "this" ->
      let* kwd_this = dec_kwd_this child in
      Ok (Assert_this (kwd_asserts, kwd_this))
    | _ -> mk_err Asserted child)

(* Type predicate annotation *)

and dec_type_predicate_annotation ?comments node : (type_predicate, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Type_predicate node
  | _ ->
    let* predicate = child_ranked 1 node ~err:Type_predicate in
    dec_type_predicate ?comments predicate

(* Type predicate *)

and dec_type_predicate ?(comments = []) node : (type_predicate, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Type_predicate node
  | _ ->
    let* name_field = child_with_field "name" node ~err:Identifier_or_type in
    let* name = dec_type_predicate_name ~comments name_field in
    let* kwd_is = first_child_named "is" node ~err:Is in
    let* kwd_is = dec_kwd_is kwd_is in
    let* type_field = child_with_field "type" node ~err:Type_expression in
    let* type_expr = dec_type type_field in
    Ok { name; kwd_is; type_expr }

and dec_type_predicate_name ?(comments = []) node : (type_predicate_name, _) result =
  match get_name node with
  | "identifier" ->
    let ident = dec_identifier ~comments node in
    Ok (Type_predicate_identifier ident)
  | "this" ->
    let* kwd_this = dec_kwd_this ~comments node in
    Ok (Type_predicate_this kwd_this)
  | _ ->
    let* type_expr = dec_predefined_type ~comments node in
    Ok (Type_predicate_type type_expr)

(* Predefined type *)

and dec_predefined_type ?(comments = []) node : (predefined_type, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Predefined_type node
  | _ ->
    let comments = comments @ prev_comments node in
    (match collect_children node with
    | [] -> mk_err Predefined_type node
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
      (match get_name child with
      | "any" ->
        let* kwd_any = dec_kwd_any ~comments child in
        Ok (T_any kwd_any)
      | "number" ->
        let* kwd_number = dec_kwd_number ~comments child in
        Ok (T_number kwd_number)
      | "boolean" ->
        let* kwd_boolean = dec_kwd_boolean ~comments child in
        Ok (T_boolean kwd_boolean)
      | "string" ->
        let* kwd_string = dec_kwd_string ~comments child in
        Ok (T_string kwd_string)
      | "symbol" ->
        let* kwd_symbol = dec_kwd_symbol ~comments child in
        Ok (T_symbol kwd_symbol)
      | "unique symbol" ->
        let* kwd_unique_symbol = dec_kwd_unique_symbol ~comments child in
        Ok (T_unique_symbol kwd_unique_symbol)
      | "void" ->
        let* kwd_void = dec_kwd_void ~comments child in
        Ok (T_void kwd_void)
      | "unknown" ->
        let* kwd_unknown = dec_kwd_unknown ~comments child in
        Ok (T_unknown kwd_unknown)
      | "never" ->
        let* kwd_never = dec_kwd_never ~comments child in
        Ok (T_never kwd_never)
      | "object" ->
        let* kwd_object = dec_kwd_object ~comments child in
        Ok (T_object kwd_object)
      | _ -> mk_err Predefined_type child))

(* Decorator *)

and dec_decorator ?(comments = []) node : (decorator, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Decorator node
  | _ ->
    let* child = named_child_ranked 0 node ~err:Decorator in
    (match get_name child with
    | "identifier" -> Ok (Decorator_identifier (dec_identifier ~comments child))
    | "member_expression" ->
      let* member_expression = wrap dec_decorator_member_expression ~comments child in
      Ok (Decorator_member_expression member_expression)
    | "call_expression" ->
      let* call_expression = wrap dec_decorator_call_expression ~comments child in
      Ok (Decorator_call_expression call_expression)
    | "parenthesized_expression" ->
      let* expression = dec_decorator_parenthesized_expression ~comments child in
      Ok (Decorator_parenthesized_expression expression)
    | _ -> mk_err Decorator child)

and dec_decorator_member_expression ?(comments = []) node
    : (decorator_member_expression, _) result
  =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Decorator_member node
  | _ ->
    let* object_field = child_with_field "object" node ~err:Identifier_or_member in
    let* object_expr = dec_object_member_expression ~comments object_field in
    let* sym_dot = first_child_named "." node ~err:Dot in
    let* sym_dot = dec_sym_dot sym_dot in
    let* property_field = child_with_field "property" node ~err:Property_identifier in
    let property = dec_identifier property_field in
    Ok { object_expr; sym_dot; property }

and dec_object_member_expression ?(comments = []) node
    : (object_member_expression, _) result
  =
  match get_name node with
  | "identifier" -> Ok (Object_name (dec_identifier ~comments node))
  | _ ->
    let* member_expression = wrap dec_decorator_member_expression ~comments node in
    Ok (Qualified_member_expression member_expression : object_member_expression)

and dec_decorator_call_expression ?(comments = []) node
    : (decorator_call_expression, _) result
  =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Decorator_call node
  | _ ->
    let* function_field = child_with_field "function" node ~err:Identifier_or_member in
    let* function_ = dec_function_or_property ~comments function_field in
    let type_arguments_field = child_with_field_opt "type_arguments" node in
    let* type_arguments = make_opt_res dec_type_arguments type_arguments_field in
    let* arguments_field = child_with_field "arguments" node ~err:Arguments in
    let* arguments = dec_arguments arguments_field in
    Ok { function_; type_arguments; arguments }

and dec_function_or_property ?(comments = []) node : (function_or_property, _) result =
  match get_name node with
  | "identifier" -> Ok (Function_name (dec_identifier ~comments node))
  | "member_expression" ->
    let* member_expression = wrap dec_decorator_member_expression ~comments node in
    Ok (Qualified_member_expression member_expression)
  | _ -> mk_err Decorator_call node

and dec_decorator_parenthesized_expression ?comments node
    : (decorator_parenthesized_expression parens, _) result
  =
  dec_parens ?comments node decode_decorator_in_parens ~err:Parenthesized_decorator

and decode_decorator_in_parens node =
  match get_name node with
  | "identifier" -> Ok (Parenthesized_ident (dec_identifier node))
  | "member_expression" ->
    let* member_expression = wrap dec_decorator_member_expression node in
    Ok (Parenthesized_member member_expression)
  | _ ->
    let* call_expression = wrap dec_decorator_call_expression node in
    Ok (Parenthesized_call call_expression)

(* Type arguments *)

and dec_type_arguments ?comments node : (type_arguments, _) result =
  dec_ne_list_in_chevrons ?comments node dec_type ~err:Type_arguments

(* Function arguments *)

and dec_arguments ?comments node : (arguments, _) result =
  dec_list_in_parens ?comments node dec_argument ~err:Arguments

and dec_argument ?comments node : (argument, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Argument node
  | "spread_element" ->
    let* spread = wrap dec_spread_element node in
    Ok (Spread_element spread)
  | _ ->
    let* expression = dec_expression ?comments node in
    Ok (Expression expression : argument)

and dec_spread_element ?(comments = []) node : (spread_element, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Spread node
  | _ ->
    let* sym_ellipsis = first_child_named "..." node ~err:Ellipsis in
    let* sym_ellipsis = dec_sym_ellipsis ~comments sym_ellipsis in
    let* expression = named_child_ranked 0 node ~err:Expression in
    let* expression = dec_expression expression in
    Ok (sym_ellipsis, expression)

(* Generator function declaration (see function declaration) *)

and dec_generator_function_declaration ?(comments = []) node
    : (generator_function_declaration, _) result
  =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Generator_function node
  | _ ->
    let* fun_decl = dec_function_declaration ~comments node in
    let* sym_asterisk = first_child_named "*" node ~err:Asterisk in
    let* sym_asterisk = dec_sym_asterisk sym_asterisk in
    Ok (sym_asterisk, fun_decl)

(* Class declaration (see [dec_class]) *)

and dec_class_declaration ?(comments = []) node : (class_declaration, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Class_declaration node
  | _ ->
    let comments = comments @ prev_comments node in
    let decorators = children_named "decorator" node in
    let* decorators = list_of_children dec_decorator decorators in
    let* kwd_class = first_child_named "class" node ~err:Class in
    let* kwd_class = dec_kwd_class ~comments kwd_class in
    let* name_field = child_with_field "name" node ~err:Class_name in
    let name = dec_identifier name_field in
    let type_parameters_field = child_with_field_opt "type_parameters" node in
    let* type_parameters = make_opt_res dec_type_parameters type_parameters_field in
    let heritage_child = first_child_named_opt "class_heritage" node in
    let* class_heritage = make_opt_res dec_class_heritage heritage_child in
    let* body_field = child_with_field "body" node ~err:Class_body in
    let* body = dec_class_body body_field in
    Ok
      ({ decorators; kwd_class; name; type_parameters; class_heritage; body }
        : class_declaration)

and dec_class_heritage node : (class_heritage, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Extends_or_implements node
  | _ ->
    let* first_child = child_ranked 0 node ~err:Extends_or_implements in
    (match get_name first_child with
    | "extends_clause" ->
      let* extends_clause = dec_extends_clause first_child in
      let implements_clause = first_child_named_opt "implements_clause" node in
      let* implements_clause = make_opt_res dec_implements_clause implements_clause in
      Ok (Extends_clause (extends_clause, implements_clause))
    | "implements_clause" ->
      let* implements_clause = dec_implements_clause first_child in
      Ok (Implements_clause implements_clause)
    | _ -> mk_err Extends_or_implements first_child)

and dec_extends_clause node : (extends_clause, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Extends_clause node
  | _ ->
    let* kwd_extends = first_child_named "extends" node ~err:Extends in
    let* kwd_extends = dec_kwd_extends kwd_extends in
    let raw_children : ts_forest =
      match collect_children node with
      | [] | [ _ ] -> []
      | _extends :: clauses -> clauses
    in
    let not_comma child = String.(get_name child <> ",") in
    let raw_clauses : ts_forest = List.filter raw_children ~f:not_comma in
    let rec pair_up acc = function
      | value :: snd :: nodes ->
        if String.equal (get_name snd) "type_arguments"
        then pair_up ((value, Some snd) :: acc) nodes
        else pair_up ((value, None) :: acc) (snd :: nodes)
      | [ value ] -> List.rev ((value, None) :: acc)
      | [] -> List.rev acc
    in
    let pairs : (ts_tree * ts_tree option) list = pair_up [] raw_clauses in
    let mk_clause (value, type_arguments_opt) : (extends_clause_single wrap, _) result =
      let region =
        match type_arguments_opt with
        | None -> !get_region value
        | Some type_args -> Region.cover (!get_region value) (!get_region type_args)
      in
      let* value = dec_expression value in
      let* type_arguments =
        match type_arguments_opt with
        | None -> Ok None
        | Some type_arguments ->
          let* args = dec_type_arguments type_arguments in
          Ok (Some args)
      in
      Ok (Wrap.make { value; type_arguments } region)
    in
    let* extends_clauses = Result.all @@ List.map ~f:mk_clause pairs in
    let* extends_clauses =
      match extends_clauses with
      | [] -> mk_err Extends_clause node
      | clause :: clauses -> Ok Nonempty_list.(clause :: clauses)
    in
    Ok (kwd_extends, extends_clauses)

and dec_implements_clause node : (implements_clause, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Implements_clause node
  | _ ->
    let* kwd_implements = first_child_named "implements" node ~err:Implements in
    let* kwd_implements = dec_kwd_implements kwd_implements in
    let raw_clauses = collect_named_children node in
    let error = mk_err Implements_clause node in
    let* type_exprs = ne_list_of_children dec_type error raw_clauses in
    Ok (kwd_implements, type_exprs)

and dec_class_body ?(comments = []) node : (class_body, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Class_body node
  | _ ->
    let comments = comments @ prev_comments node in
    let* opening = first_child_named "{" node ~err:Left_brace in
    let* opening = dec_sym_lbrace ~comments opening in
    let* closing = first_child_named "}" node ~err:Right_brace in
    let* closing = dec_sym_rbrace closing in
    let named_children = collect_named_children node in
    let pair (decorators, acc) child =
      match get_name child with
      | "decorator" -> child :: decorators, acc
      | _ -> [], (List.rev decorators, child) :: acc
    in
    let _, pairs = List.fold_left ~f:pair ~init:([], []) named_children in
    let contents = List.map ~f:dec_class_member @@ List.rev pairs in
    let* contents = Result.all contents in
    let region = !get_region node in
    Ok (Braces (Wrap.make { opening; contents; closing } region))

and dec_class_member ?(comments = []) (decorators, node) : (class_member, _) result =
  match get_name node with
  | "method_definition" ->
    let* decorators = list_of_children dec_decorator decorators in
    let* definition = wrap dec_method_definition ~comments node in
    (* Not ideal *)
    Ok (Method_definition (decorators, definition))
  | "method_signature" ->
    let* signature = wrap dec_method_signature node in
    Ok (Method_signature signature : class_member)
  | "class_static_block" ->
    let* static_block = dec_class_static_block node in
    Ok (Call_static_block static_block)
  | "abstract_method_signature" ->
    let* signature = wrap dec_abstract_method_signature node in
    Ok (Abstract_method_signature signature)
  | "index_signature" ->
    let* signature = wrap dec_index_signature node in
    Ok (Index_signature signature : class_member)
  | "public_field_definition" ->
    let* definition = wrap dec_public_field_definition node in
    Ok (Public_field_definition definition)
  | _ -> mk_err Class_member node

(* Method definition *)

and dec_method_definition ?(comments = []) node : (method_definition, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Method_definition node
  | _ ->
    let* signature = wrap dec_method_signature ~comments node in
    let* body_field = child_with_field "body" node ~err:Block in
    let* body = dec_statement_block body_field in
    Ok { signature; body }

(* Method signature *)

and dec_method_signature ?(comments = []) node : (method_signature, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Method_signature node
  | _ ->
    let accessibility_modifier = first_child_named_opt "accessibility_modifier" node in
    let* access = make_opt_res dec_accessibility_modifier accessibility_modifier in
    let* scope = dec_method_scope node in
    let kwd_async = first_child_named_opt "async" node in
    let* kwd_async = make_opt_res dec_kwd_async kwd_async in
    let* set_get_all = mk_set_get_all node in
    let* name_field = child_with_field "name" node ~err:Property_name in
    let* name = dec_property_name ~comments name_field in
    let sym_qmark = first_child_named_opt "?" node in
    let* optional = make_opt_res dec_sym_qmark sym_qmark in
    let* call_sig = dec_call_signature node in
    Ok { access; scope; kwd_async; set_get_all; name; optional; call_sig }

(* Method scope *)

and dec_method_scope node : (method_scope, _) result =
  let kwd_static = first_child_named_opt "static" node in
  let* kwd_static = make_opt_res dec_kwd_static kwd_static in
  let override_modifier = first_child_named_opt "override_modifier" node in
  let* kwd_override = make_opt_res dec_override_modifier override_modifier in
  let kwd_readonly = first_child_named_opt "readonly" node in
  let* kwd_readonly = make_opt_res dec_kwd_readonly kwd_readonly in
  Ok { kwd_static; kwd_override; kwd_readonly }

(* Class static block *)

and dec_class_static_block ?(comments = []) node
    : (kwd_static * statement_block, _) result
  =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Static_block node
  | _ ->
    let* kwd_static = first_child_named "static" node ~err:Static in
    let* kwd_static = dec_kwd_static ~comments kwd_static in
    let* body_field = child_with_field "body" node ~err:Block in
    let* block = dec_statement_block body_field in
    Ok (kwd_static, block)

(* Abstract method signature *)

and dec_abstract_method_signature ?(comments = []) node
    : (abstract_method_signature, _) result
  =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Abstract_method_signature node
  | _ ->
    let accessibility_modifier = first_child_named_opt "accessibility_modifier" node in
    let* access = make_opt_res dec_accessibility_modifier accessibility_modifier in
    let* kwd_abstract = first_child_named "abstract" node ~err:Abstract in
    let* kwd_abstract = dec_kwd_abstract kwd_abstract in
    let override_modifier = first_child_named_opt "override_modifier" node in
    let* kwd_override = make_opt_res dec_override_modifier override_modifier in
    let* set_get_all = mk_set_get_all node in
    let* name_field = child_with_field "name" node ~err:Property_name in
    (* Not ideal *)
    let* name = dec_property_name ~comments name_field in
    let sym_qmark = first_child_named_opt "?" node in
    let* optional = make_opt_res dec_sym_qmark sym_qmark in
    let* call_sig = dec_call_signature node in
    Ok { access; kwd_abstract; kwd_override; set_get_all; name; optional; call_sig }

(* Call signature *)

and dec_call_signature ?(comments = []) node : (call_signature wrap, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Call_signature node
  | _ ->
    let region = !get_region node in
    let type_parameters_field = child_with_field_opt "type_parameters" node in
    let* parameters_field = child_with_field "parameters" node ~err:Parameters in
    let type_params_comments, params_comments =
      match type_parameters_field with
      | None -> [], comments
      | _ -> comments, []
    in
    let* type_parameters =
      make_opt_res
        (dec_type_parameters ~comments:type_params_comments)
        type_parameters_field
    in
    let* parameters = dec_formal_parameters ~comments:params_comments parameters_field in
    let return_type_field = child_with_field_opt "return_type" node in
    let* return_type = make_opt_res dec_call_return_type return_type_field in
    let call_sig : call_signature = { type_parameters; parameters; return_type } in
    Ok (Wrap.make call_sig region)

(* Index signature *)

and dec_index_signature ?(comments = []) node : (index_signature, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Index_signature node
  | _ ->
    let kwd_readonly = first_child_named_opt "readonly" node in
    let* kwd_readonly = make_opt_res dec_kwd_readonly kwd_readonly in
    let sign_field = child_with_field_opt "sign" node in
    let* sign = make_opt_res dec_sign sign_field in
    let sign =
      match kwd_readonly with
      | None -> None
      | Some kwd -> Some (sign, kwd)
    in
    let name_field = child_with_field_opt "name" node in
    let* type_field = child_with_field "type" node ~err:Type_annotation in
    let* annotation = dec_index_annotation type_field in
    let* sym_lbracket = first_child_named "[" node ~err:Left_bracket in
    (* Not ideal *)
    let* opening = dec_sym_lbracket ~comments sym_lbracket in
    let* sym_rbracket = first_child_named "]" node ~err:Right_bracket in
    let* closing = dec_sym_rbracket sym_rbracket in
    let* (range : index_range) =
      match name_field with
      | Some name_field ->
        let name = dec_type_identifier name_field in
        let* sym_colon = first_child_named ":" node ~err:Colon in
        let* sym_colon = dec_sym_colon sym_colon in
        let* index_type_field = child_with_field "index_type" node ~err:Type in
        let* index_type = dec_type index_type_field in
        Ok (Typed_index_clause { name; sym_colon; index_type })
      | None ->
        let* mapped_type_clause = named_child_ranked 0 node ~err:Mapped_type_signature in
        let* mapped_type_clause = dec_mapped_type_clause mapped_type_clause in
        Ok (Mapped_type_clause mapped_type_clause)
    in
    let region = !get_region node in
    let brackets = { opening; contents = range; closing } in
    let range = Brackets (Wrap.make brackets region) in
    Ok { sign; range; annotation }

and dec_mapped_type_clause node : (mapped_type_clause, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Mapped_type_signature node
  | _ ->
    let* name_field = child_with_field "name" node ~err:Type_name in
    let name = dec_type_identifier name_field in
    let* kwd_in = first_child_named "in" node ~err:In in
    let* kwd_in = dec_kwd_in kwd_in in
    let* type_field = child_with_field "type" node ~err:Type in
    let* type_expr = dec_type type_field in
    let alias_field = child_with_field_opt "alias" node in
    let* alias =
      match alias_field with
      | None -> Ok None
      | Some alias ->
        let* kwd_as = first_child_named "as" node ~err:As in
        let* type_expr = dec_type alias in
        let* kwd_as = dec_kwd_as kwd_as in
        Ok (Some (kwd_as, type_expr))
    in
    Ok { name; kwd_in; type_expr; alias }

and dec_omitting_type_annotation node : (symbol * type_expr, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Omitting_type_annotation node
  | _ ->
    let* sym_kind = first_child_named "-?:" node ~err:Omitting_type_annotation in
    let* sym_kind = dec_sym_omitting sym_kind in
    let* type_child = named_child_ranked 0 node ~err:Type_expression in
    let* type_expr = dec_type type_child in
    Ok (sym_kind, type_expr)

and dec_adding_type_annotation node : (symbol * type_expr, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Adding_type_annotation node
  | _ ->
    let* sym_kind = first_child_named "+?:" node ~err:Adding_type_annotation in
    let* sym_kind = dec_sym_adding sym_kind in
    let* type_child = named_child_ranked 0 node ~err:Type_expression in
    let* type_expr = dec_type type_child in
    Ok (sym_kind, type_expr)

and dec_opting_type_annotation node : (symbol * type_expr, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Opting_type_annotation node
  | _ ->
    let* sym_kind = first_child_named "?:" node ~err:Opting_type_annotation in
    let* sym_kind = dec_sym_opting sym_kind in
    let* type_child = named_child_ranked 0 node ~err:Type_expression in
    let* type_expr = dec_type type_child in
    Ok (sym_kind, type_expr)

and dec_index_annotation node : (index_annotation, _) result =
  match get_name node with
  | "type_annotation" ->
    let* annotation = dec_type_annotation node in
    Ok (Type_annotation annotation)
  | "omitting_type_annotation" ->
    let* annotation = dec_omitting_type_annotation node in
    Ok (Omitting_type_annotation annotation)
  | "adding_type_annotation" ->
    let* annotation = dec_adding_type_annotation node in
    Ok (Adding_type_annotation annotation)
  | "opting_type_annotation" ->
    let* annotation = dec_opting_type_annotation node in
    Ok (Opting_type_annotation annotation)
  | _ -> mk_err Type_of_index_signature node

and dec_sign node : (sign, _) result =
  match get_name node with
  | "+" ->
    let* sym_plus = dec_sym_plus node in
    Ok (Plus sym_plus)
  | "-" ->
    let* sym_minus = dec_sym_minus node in
    Ok (Minus sym_minus)
  | _ -> mk_err Plus_or_minus node

(* Public field definition *)

and dec_public_field_definition ?(comments = []) node
    : (public_field_definition, _) result
  =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Public_field_definition node
  | _ ->
    let decorators = children_named "decorator" node in
    let* decorators = list_of_children dec_decorator decorators in
    let accessibility_modifier = first_child_named_opt "accessibility_modifier" node in
    let* access = make_opt_res dec_accessibility_modifier accessibility_modifier in
    let kwd_declare = first_child_named_opt "declare" node in
    let* kwd_declare = make_opt_res dec_kwd_declare kwd_declare in
    let* scope = dec_field_scope node in
    let* name_field = child_with_field "name" node ~err:Property_name in
    let* name = dec_property_name ~comments name_field in
    let* mode = dec_field_mode_opt node in
    let type_field = child_with_field_opt "type" node in
    let* type_ = make_opt_res dec_type_annotation type_field in
    let* default = mk_child_initializer_opt node in
    Ok { decorators; access; kwd_declare; scope; name; mode; type_; default }

and dec_field_mode_opt node : (field_mode option, _) result =
  let sym_qmark = first_child_named_opt "?" node in
  match sym_qmark with
  | Some sym_qmark ->
    let* sym_qmark = dec_sym_qmark sym_qmark in
    Ok (Some (Optional sym_qmark))
  | None ->
    (match first_child_named_opt "!" node with
    | None -> Ok None
    | Some sym_emark ->
      let* sym_emark = dec_sym_emark sym_emark in
      Ok (Some (Definite_assert sym_emark)))

and dec_field_scope node : (field_scope, _) result =
  let override_modifier = first_child_named_opt "override_modifier" node in
  let* kwd_override = make_opt_res dec_override_modifier override_modifier in
  let kwd_abstract = first_child_named_opt "abstract" node in
  let* kwd_abstract = make_opt_res dec_kwd_abstract kwd_abstract in
  let kwd_static = first_child_named_opt "static" node in
  let* kwd_static = make_opt_res dec_kwd_static kwd_static in
  let kwd_readonly = first_child_named_opt "readonly" node in
  let* kwd_readonly = make_opt_res dec_kwd_readonly kwd_readonly in
  let kwd_accessor = first_child_named_opt "accessor" node in
  let* kwd_accessor = make_opt_res dec_kwd_accessor kwd_accessor in
  Ok { kwd_static; kwd_override; kwd_readonly; kwd_abstract; kwd_accessor }

(* Lexical declaration (see [dec_variable_declaration]) *)

and dec_lexical_declaration ?(comments = []) node : (lexical_declaration, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Let_or_const node
  | _ ->
    let comments = comments @ prev_comments node in
    let* kind_field = child_with_field "kind" node ~err:Let_or_const in
    let decls = children_named "variable_declarator" node in
    let error = mk_err Let_or_const node in
    let* decls = ne_list_of_children dec_variable_declarator error decls in
    let* kind =
      match get_name kind_field with
      | "let" ->
        let* kwd_let = dec_kwd_let ~comments kind_field in
        Ok (Let kwd_let)
      | "const" ->
        let* kwd_const = dec_kwd_const ~comments kind_field in
        Ok (Const kwd_const)
      | _ -> mk_err Let_or_const kind_field
    in
    Ok { kind; decls }

(* Variable declaration (see [dec_lexical_declaration]) *)

and dec_variable_declaration ?(comments = []) node : (variable_declaration, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Variable_declaration node
  | _ ->
    let comments = comments @ prev_comments node in
    let* kwd_var = first_child_named "var" node ~err:Var in
    let* kwd_var = dec_kwd_var ~comments kwd_var in
    let var_decls = children_named "variable_declarator" node in
    let error = mk_err Variable_declaration node in
    let* var_decls = ne_list_of_children dec_variable_declarator error var_decls in
    Ok (kwd_var, var_decls)

and dec_variable_declarator ?(comments = []) node : (variable_declarator, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Variable node
  | _ ->
    let comments = comments @ prev_comments node in
    let* name_field = child_with_field "name" node ~err:Variable in
    let sym_qmark = first_child_named_opt "!" node in
    (match sym_qmark with
    | None ->
      let* var_names = dec_lhs_pattern ~comments name_field in
      let type_field = child_with_field_opt "type" node in
      let* var_type = make_opt_res dec_type_annotation type_field in
      let* default = mk_child_initializer_opt node in
      let decl = { var_names; var_type; default } in
      Ok (Var_decl (Wrap.make decl (!get_region node)))
    | Some sym_qmark ->
      let identifier = dec_identifier ~comments name_field in
      let* sym_qmark = dec_sym_qmark sym_qmark in
      let* type_field = child_with_field "type" node ~err:Type_annotation in
      let* var_type = dec_type_annotation type_field in
      Ok (Var_decl_assertion (identifier, sym_qmark, var_type)))

and dec_lhs_pattern ?comments node : (lhs_pattern, _) result =
  match get_name node with
  | "identifier" -> Ok (Decl_ident (dec_identifier ?comments node))
  | _ ->
    let* pattern = dec_destructuring_pattern ?comments node in
    Ok (Decl_pattern pattern)

(* Function signature (See [dec_function_declaration]) *)

and dec_function_signature ?(comments = []) node : (function_signature, _) result =
  let comments = comments @ prev_comments node in
  let kwd_async = first_child_named_opt "async" node in
  let async_comments, function_comments =
    match kwd_async with
    | None -> [], comments
    | Some _ -> comments, []
  in
  let* kwd_async = make_opt_res (dec_kwd_async ~comments:async_comments) kwd_async in
  let* kwd_function = first_child_named "function" node ~err:Function in
  let* kwd_function = dec_kwd_function ~comments:function_comments kwd_function in
  let* name_field = child_with_field "name" node ~err:Function_name in
  let name = dec_identifier name_field in
  let* call_sig = dec_call_signature node in
  Ok { kwd_async; kwd_function; name; call_sig }

(* Formal parameters *)

(* TODO: Test with Tests/formal.ts *)

and dec_formal_parameters ?comments node : (formal_parameters, _) result =
  dec_list_in_parens ?comments node (wrap dec_formal_parameter) ~err:Parameters

and dec_formal_parameter ?(comments = []) node : (formal_parameter, _) result =
  match get_name node with
  | "required_parameter" -> dec_required_parameter ~comments node
  | "optional_parameter" -> dec_optional_parameter ~comments node
  | _ -> mk_err Parameter node

and dec_required_parameter ?(comments = []) node : (formal_parameter, _) result =
  let* parameter_name = wrap dec_parameter_name ~comments node in
  let optional = None in
  let type_field = child_with_field_opt "type" node in
  let* type_opt = make_opt_res dec_type_annotation type_field in
  let* default = mk_child_initializer_opt node in
  Ok { parameter_name; optional; type_opt; default }

and dec_optional_parameter ?(comments = []) node : (formal_parameter, _) result =
  let* parameter_name = wrap dec_parameter_name ~comments node in
  let* sym_qmark = first_child_named "?" node ~err:Question_mark in
  let* sym_qmark = dec_sym_qmark sym_qmark in
  let optional = Some sym_qmark in
  let type_field = child_with_field_opt "type" node in
  let* type_opt = make_opt_res dec_type_annotation type_field in
  let* default = mk_child_initializer_opt node in
  Ok { parameter_name; optional; type_opt; default }

and dec_parameter_name ?(comments = []) node : (parameter_name, _) result =
  let comments = comments @ prev_comments node in
  let decorators = children_named "decorator" node in
  let* decorators = list_of_children dec_decorator decorators in
  let accessibility_modifier = first_child_named_opt "accessibility_modifier" node in
  let* access = make_opt_res dec_accessibility_modifier accessibility_modifier in
  let override_modifier = first_child_named_opt "override_modifier" node in
  let* kwd_override = make_opt_res dec_override_modifier override_modifier in
  let kwd_readonly = first_child_named_opt "readonly" node in
  let* kwd_readonly = make_opt_res dec_kwd_readonly kwd_readonly in
  let* pattern_field = child_with_field "pattern" node ~err:Pattern in
  let* pattern = dec_parameter_pattern ~comments pattern_field (* Not ideal *) in
  Ok { decorators; access; kwd_override; kwd_readonly; pattern }

and dec_parameter_pattern ?(comments = []) node : (parameter_pattern, _) result =
  match get_name node with
  | "this" ->
    let* kwd_this = dec_kwd_this ~comments node in
    Ok (Parameter_this kwd_this)
  | _ ->
    let* pattern = dec_pattern ~comments node in
    Ok (Parameter_pattern pattern)

and mk_child_initializer_opt node : ((sym_equal * expression) option, _) result =
  match first_child_named_opt "=" node with
  | None -> Ok None
  | Some sym_equal ->
    let* init = mk_child_initializer sym_equal node in
    Ok (Some init)

and mk_child_initializer sym_equal node : (sym_equal * expression, _) result =
  let* sym_equal = dec_sym_equal sym_equal in
  let* value_field = child_with_field "value" node ~err:Expression in
  let* expression = dec_expression value_field in
  Ok (sym_equal, expression)

(* Abstract class declaration

   The difference with [dec_class_declaration] is the decoding of the
   keyword "abstract". The AST of abstract class definitions do not
   reuse that for class definitions because of the handling of the
   comments, which should be hooked either on the keyword "class" or
   "abstract". *)

and dec_abstract_class_declaration ?(comments = []) node
    : (abstract_class_declaration, _) result
  =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Abstract node
  | _ ->
    let comments = comments @ prev_comments node in
    let decorators = children_named "decorator" node in
    let* decorators = list_of_children dec_decorator decorators in
    let* kwd_abstract = first_child_named "abstract" node ~err:Abstract in
    let* kwd_abstract = dec_kwd_abstract ~comments kwd_abstract in
    let* kwd_class = first_child_named "class" node ~err:Class in
    let* kwd_class = dec_kwd_class kwd_class in
    let* name_field = child_with_field "name" node ~err:Class_name in
    let name = dec_identifier name_field in
    let type_parameters_field = child_with_field_opt "type_parameters" node in
    let* type_parameters = make_opt_res dec_type_parameters type_parameters_field in
    let heritage_child = first_child_named_opt "class_heritage" node in
    let* class_heritage = make_opt_res dec_class_heritage heritage_child in
    let* body_field = child_with_field "body" node ~err:Class_body in
    let* body = dec_class_body body_field in
    Ok
      { decorators; kwd_abstract; kwd_class; name; type_parameters; class_heritage; body }

(* Module *)

and dec_module_declaration ?(comments = []) node : (module_declaration, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Module_declaration node
  | _ ->
    let comments = comments @ prev_comments node in
    let* kwd_module = first_child_named "module" node ~err:Module in
    let* kwd_module = dec_kwd_module ~comments kwd_module in
    let* name_field = child_with_field "name" node ~err:Module_name in
    let* module_name = dec_module_name name_field in
    let body_field = child_with_field_opt "body" node in
    let* module_body = make_opt_res dec_statement_block body_field in
    Ok { kwd_module; module_name; module_body }

and dec_module_name node : (module_name, _) result =
  match get_name node with
  | "string" -> Ok (Module_string (dec_string node))
  | "identifier" -> Ok (Module_ident (dec_identifier node))
  | "nested_identifier" ->
    let* nested = dec_nested_identifier node in
    let nested = Wrap.make nested (!get_region node) in
    Ok (Module_nested nested)
  | _ -> mk_err Module_name node

(* Internal module (a.k.a. namespaces) *)

and dec_internal_module ?(comments = []) node : (internal_module, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Namespace_declaration node
  | _ ->
    let comments = comments @ prev_comments node in
    let* kwd_namespace = first_child_named "namespace" node ~err:Namespace in
    let* kwd_namespace = dec_kwd_namespace ~comments kwd_namespace in
    let* name_field = child_with_field "name" node ~err:Namespace_name in
    let* module_name = dec_module_name name_field in
    let body_field = child_with_field_opt "body" node in
    let* module_body = make_opt_res dec_statement_block body_field in
    Ok { kwd_namespace; module_name; module_body }

(* Type alias declaration *)

and dec_type_alias_declaration ?(comments = []) node : (type_alias_declaration, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Type_alias_declaration node
  | _ ->
    let comments = comments @ prev_comments node in
    let* kwd_type = first_child_named "type" node ~err:Type in
    let* kwd_type = dec_kwd_type ~comments kwd_type in
    let* name_field = child_with_field "name" node ~err:Type_name in
    let name = dec_type_identifier name_field in
    let* sym_equal = first_child_named "=" node ~err:Equal in
    let* sym_equal = dec_sym_equal sym_equal in
    let type_parameters_field = child_with_field_opt "type_parameters" node in
    let* type_parameters = make_opt_res dec_type_parameters type_parameters_field in
    let* value_field = child_with_field "value" node ~err:Type_expression in
    let* type_expr = dec_type value_field in
    Ok { kwd_type; name; type_parameters; sym_equal; type_expr }

(* Type parameters *)

and dec_type_parameters ?comments node : (type_parameters, _) result =
  dec_list_in_chevrons ?comments node dec_type_parameter ~err:Type_parameters

and dec_type_parameter ?(comments = []) node : (type_parameter wrap, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Const_or_type_name node
  | _ ->
    let comments = comments @ prev_comments node in
    let kwd_const = first_child_named_opt "const" node in
    let* kwd_const = make_opt_res dec_kwd_const kwd_const in
    let* name_field = child_with_field "name" node ~err:Type_parameter in
    let name = dec_type_identifier ~comments name_field (* Not ideal *) in
    let constraint_field = child_with_field_opt "constraint" node in
    let* constraint_expr = make_opt_res dec_constraint constraint_field in
    let value_field = child_with_field_opt "value" node in
    let* default_type = make_opt_res dec_default_type value_field in
    let type_parameter = { kwd_const; name; constraint_expr; default_type } in
    Ok (Wrap.make type_parameter (!get_region node))

and dec_type_identifier ?comments node : type_identifier = dec_identifier ?comments node

and dec_constraint node : (kwd_extends * type_expr, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Extends node
  | _ ->
    let* kwd_extends = first_child_named "extends" node ~err:Extends in
    let* kwd_extends = dec_kwd_extends kwd_extends in
    let* type_child = child_ranked 1 node ~err:Type_expression in
    let* type_expr = dec_type type_child in
    Ok (kwd_extends, type_expr)

and dec_default_type node : (sym_equal * type_expr, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Equal node
  | _ ->
    let* sym_equal = first_child_named "=" node ~err:Equal in
    let* sym_equal = dec_sym_equal sym_equal in
    let* type_node = child_ranked 1 node ~err:Type_expression in
    let* type_expr = dec_type type_node in
    Ok (sym_equal, type_expr)

(* Enum declaration *)

and dec_enum_declaration ?(comments = []) node : (enum_declaration, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Const_or_enum node
  | _ ->
    let kwd_const = first_child_named_opt "const" node in
    let* kwd_const = make_opt_res dec_kwd_const kwd_const in
    let* kwd_enum = first_child_named "enum" node ~err:Enum in
    let* kwd_enum = dec_kwd_enum ~comments kwd_enum in
    let* name_field = child_with_field "name" node ~err:Enumeration_name in
    let name = dec_identifier name_field in
    let* body_field = child_with_field "body" node ~err:Enumeration in
    let* body = dec_enum_entries body_field in
    Ok { kwd_const; kwd_enum; name; body }

and dec_enum_entries node : (enum_body list braces, _) result =
  dec_list_in_braces node dec_enum_body ~err:Enumeration

and dec_enum_body ?(comments = []) node : (enum_body, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Enumeration_name node
  | "enum_assignment" ->
    let* assignment = wrap dec_enum_assignment ~comments node in
    Ok (Enum_assignment assignment)
  | _ ->
    let* property = dec_property_name ~comments node in
    Ok (Enum_name property)

and dec_enum_assignment ?comments node : (enum_assignment, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Enumeration_name node
  | _ ->
    let* name_field = child_with_field "name" node ~err:Enumeration_name in
    let* name = dec_property_name ?comments name_field in
    let* sym_equal = first_child_named "=" node ~err:Equal in
    let* default = mk_child_initializer sym_equal node in
    Ok { name; default }

(* Property names *)

and dec_property_name ?(comments = []) node : (property_name, _) result =
  match get_name node with
  | "property_identifier" -> Ok (Property_identifier (dec_identifier ~comments node))
  | "private_property_identifier" ->
    Ok (Private_property_identifier (dec_private_property_identifier ~comments node))
  | "string" -> Ok (String (dec_string ~comments node))
  | "number" ->
    let* number = dec_number ~comments node in
    Ok (Number number)
  | "computed_property_name" ->
    let* expression = dec_computed_property_name ~comments node in
    Ok (Computed_property_name expression)
  | _ -> mk_err Property_name node

and dec_private_property_identifier ?(comments = []) node : private_property_identifier =
  dec_identifier ~comments node

and dec_computed_property_name ?comments node : (expression brackets, _) result =
  dec_brackets ?comments node dec_expression ~err:Computed_property_name

(* Interface declaration *)

and dec_interface_declaration ?(comments = []) node : (interface_declaration, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Interface node
  | _ ->
    let* kwd_interface = first_child_named "interface" node ~err:Interface in
    let* kwd_interface = dec_kwd_interface ~comments kwd_interface in
    let* name_field = child_with_field "name" node ~err:Interface_name in
    let name = dec_type_identifier name_field in
    let type_parameters_field = child_with_field_opt "type_parameters" node in
    let* type_parameters = make_opt_res dec_type_parameters type_parameters_field in
    let extends_type_clause = first_child_named_opt "extends_type_clause" node in
    let* extends = make_opt_res dec_extends_type_clause extends_type_clause in
    let* body_field = child_with_field "body" node ~err:Interface_body in
    let* body = dec_object_type body_field in
    Ok { kwd_interface; name; type_parameters; extends; body }

and dec_extends_type_clause node : (extends_type_clause, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Extends node
  | _ ->
    let* kwd_extends = first_child_named "extends" node ~err:Extends in
    let* kwd_extends = dec_kwd_extends kwd_extends in
    let named_children = collect_named_children node in
    let error = mk_err Extends node in
    let* extensions = ne_list_of_children dec_type_extension error named_children in
    Ok { kwd_extends; extensions }

and dec_type_extension ?(comments = []) node : (type_extension, _) result =
  match get_name node with
  | "type_identifier" -> Ok (Extends_type (dec_type_identifier ~comments node))
  | "nested_type_identifier" ->
    let* nested = wrap dec_nested_type_identifier ~comments node in
    Ok (Extends_nested nested)
  | "generic_type" ->
    let* type_expr = wrap dec_generic_type ~comments node in
    Ok (Extends_generic type_expr)
  | _ -> mk_err Type_expression node

(* Nested type identifier *)

and dec_nested_type_identifier ?(comments = []) node : (nested_type_identifier, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Nested_type_identifier node
  | _ ->
    let* module_field = child_with_field "module" node ~err:Identifier_or_path in
    let* name_field = child_with_field "name" node ~err:Type_name in
    let* path = dec_module_path ~comments module_field in
    Ok (path, dec_type_identifier name_field)

and dec_module_path ?(comments = []) node : (identifier ne_list, _) result =
  match get_name node with
  | "identifier" -> Ok Nonempty_list.[ dec_type_identifier ~comments node ]
  | "nested_identifier" ->
    let* path, id = dec_nested_identifier ~comments node in
    Ok (Nonempty_list.cons id path)
  | _ -> mk_err Identifier_or_path node

(* Import alias *)

and dec_import_alias ?(comments = []) node : (import_alias, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Import node
  | _ ->
    let* kwd_import = first_child_named "import" node ~err:Import in
    let* kwd_import = dec_kwd_import ~comments kwd_import in
    let* lhs = child_ranked 1 node ~err:Identifier in
    let alias = dec_identifier lhs in
    let* sym_equal = first_child_named "=" node ~err:Equal in
    let* sym_equal = dec_sym_equal sym_equal in
    let* rhs = child_ranked 3 node ~err:Identifier_or_path in
    let* aliased = dec_aliased rhs in
    Ok { kwd_import; alias; sym_equal; aliased }

and dec_aliased node : (aliased, _) result =
  match get_name node with
  | "identifier" -> Ok (Ident (dec_identifier node))
  | "nested_identifier" ->
    let* nested = wrap dec_nested_identifier node in
    Ok (Nested nested)
  | _ -> mk_err Identifier_or_path node

(* Nested identifier *)

and dec_nested_identifier ?(comments = []) node : (nested_identifier, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Identifier_or_member node
  | _ ->
    let* object_field = child_with_field "object" node ~err:Identifier_or_member in
    let* property_field = child_with_field "property" node ~err:Property_identifier in
    let* path = dec_object_path ~comments object_field in
    let* property = dec_property property_field in
    Ok (path, property)

and dec_object_path ?(comments = []) node : (identifier ne_list, _) result =
  match get_name node with
  | "identifier" -> Ok Nonempty_list.[ dec_identifier ~comments node ]
  | "member_expression" ->
    let* path, id = dec_nested_identifier ~comments node in
    Ok (Nonempty_list.cons id path)
  | _ -> mk_err Identifier_or_member node

and dec_property node : (identifier, _) result =
  match get_name node with
  | "property_identifier" -> Ok (dec_identifier node)
  | _ -> mk_err Property_identifier node

(* Ambient declaration *)

and dec_ambient_declaration ?comments node : (ambient_declaration, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Declare node
  | _ ->
    let* kwd_declare = first_child_named "declare" node ~err:Declare in
    let* kwd_declare = dec_kwd_declare ?comments kwd_declare in
    let* fst_child = named_child_ranked 0 node ~err:Block_or_ident_or_decl in
    let* ambient_kind =
      match get_name fst_child with
      | "statement_block" ->
        let* kwd_global = first_child_named "global" node ~err:Global in
        let* kwd_global = dec_kwd_global kwd_global in
        let* block = dec_statement_block fst_child in
        Ok (Global_declaration (kwd_global, block))
      | "property_identifier" ->
        let* kwd_module = first_child_named "module" node ~err:Module in
        let* kwd_module = dec_kwd_module kwd_module in
        let* type_child = child_ranked 5 node ~err:Type_expression in
        let identifier = dec_identifier fst_child in
        let* type_expr = dec_type type_child in
        Ok (Module_declaration (kwd_module, identifier, type_expr))
      | _ ->
        let* declaration = dec_declaration fst_child in
        Ok (Declaration declaration)
    in
    Ok { kwd_declare; ambient_kind }

(* EXPRESSION

   The JavasScript tree-sitter grammar have the non-terminals
   "expression" and "primary_expression" be supertypes, that is,
   hidden rules. Therefore we have to match all the RHS of those
   non-terminals in [dec_expression]. *)

and dec_expression ?(comments = []) node : (expression, _) result =
  match get_name node with
  (*| "glimmer_template" -> Ok (E_glimmer_template (dec_glimmer_template node))*)
  | "assignment_expression" ->
    let* expression = wrap dec_assignment_expression node in
    Ok (E_assignment_expression expression)
  | "augmented_assignment_expression" ->
    let* expression = wrap dec_augmented_assignment_expression node in
    Ok (E_augmented_assignment_expression expression)
  | "await_expression" ->
    let* expression = wrap dec_await_expression node in
    Ok (E_await_expression expression)
  | "unary_expression" ->
    let* expression = wrap dec_unary_expression ~comments node in
    Ok (E_unary_expression expression)
  | "binary_expression" ->
    let* expression = wrap dec_binary_expression ~comments node in
    Ok (E_binary_expression expression)
  | "ternary_expression" ->
    let* expression = wrap dec_ternary_expression ~comments node in
    Ok (E_ternary_expression expression)
  | "update_expression" ->
    let* expression = dec_update_expression ~comments node in
    Ok (E_update_expression expression)
  | "new_expression" ->
    let* expression = wrap dec_new_expression ~comments node in
    Ok (E_new_expression expression)
  | "yield_expression" ->
    let* expression = dec_yield_expression node in
    Ok (E_yield_expression expression)
  | "as_expression" ->
    let* expression = wrap dec_as_expression ~comments node in
    Ok (E_as_expression expression)
  | "satisfies_expression" ->
    let* expression = wrap dec_satisfies_expression ~comments node in
    Ok (E_satisfies_expression expression)
  | "instantiation_expression" ->
    let* expression = wrap dec_instantiation_expression ~comments node in
    Ok (E_instantiation_expression expression)
  | "internal_module" ->
    let* declaration = wrap dec_internal_module ~comments node in
    Ok (E_internal_module declaration)
  | "type_assertion" ->
    let* assertion = wrap dec_type_assertion ~comments node in
    Ok (E_type_assertion assertion)
  | _ ->
    let* expression = dec_primary_expression ~comments node in
    Ok (E_primary_expression expression)

(* Assignment expression *)

and dec_assignment_expression ?(comments = []) node : (assignment_expression, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Using_or_expression node
  | _ ->
    let kwd_using = first_child_named_opt "using" node in
    let* kwd_using = make_opt_res dec_kwd_using kwd_using in
    let* left_field = child_with_field "left" node ~err:Expression in
    let* left = dec_assignment_lhs ~comments left_field in
    let* sym_equal = first_child_named "=" node ~err:Equal in
    let* sym_equal = dec_sym_equal sym_equal in
    let* right_field = child_with_field "right" node ~err:Expression in
    let* right = dec_expression right_field in
    Ok { kwd_using; left; sym_equal; right }

and dec_assignment_lhs ?(comments = []) node : (assignment_lhs, _) result =
  match get_name node with
  | "parenthesized_expression" ->
    let* expression = dec_parenthesized_expression ~comments node in
    Ok (Assign_lhs_parens expression)
  | _ ->
    let* expression = dec_lhs_expression ~comments node in
    Ok (Assign_lhs expression)

(* Augmented assignment expression *)

and dec_augmented_assignment_expression ?(comments = []) node
    : (augmented_assignment_expression, _) result
  =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err LHS_of_augmented_assgmnt node
  | _ ->
    let* left_field = child_with_field "left" node ~err:Expression in
    let* left = dec_augmented_assignment_lhs ~comments left_field in
    let* operator = child_with_field "operator" node ~err:Augmented_assignment in
    let* operator = dec_assignment_operator operator in
    let* right_field = child_with_field "right" node ~err:Expression in
    let* right = dec_expression right_field in
    Ok { left; operator; right }

and dec_assignment_operator node : (assignment_operator, _) result =
  match get_name node with
  | "+=" ->
    let* sym_plus_equal = dec_sym_plus_equal node in
    Ok (Add_eq sym_plus_equal)
  | "-=" ->
    let* sym_minus_equal = dec_sym_minus_equal node in
    Ok (Sub_eq sym_minus_equal)
  | "*=" ->
    let* sym_mult_equal = dec_sym_mult_equal node in
    Ok (Mult_eq sym_mult_equal)
  | "/=" ->
    let* sym_div_equal = dec_sym_div_equal node in
    Ok (Div_eq sym_div_equal)
  | "%=" ->
    let* sym_rem_equal = dec_sym_rem_equal node in
    Ok (Rem_eq sym_rem_equal)
  | "^=" ->
    let* sym_xor_equal = dec_sym_xor_equal node in
    Ok (Bitwise_xor_eq sym_xor_equal)
  | "&=" ->
    let* sym_and_equal = dec_sym_and_equal node in
    Ok (Bitwise_and_eq sym_and_equal)
  | "|=" ->
    let* sym_or_equal = dec_sym_or_equal node in
    Ok (Bitwise_or_eq sym_or_equal)
  | ">>=" ->
    let* sym_shift_right_equal = dec_sym_shift_right_equal node in
    Ok (Bitwise_sr_eq sym_shift_right_equal)
  | ">>>=" ->
    let* sym_unsigned_shift_right_equal = dec_sym_unsigned_shift_right_equal node in
    Ok (Bitwise_usr_eq sym_unsigned_shift_right_equal)
  | "<<=" ->
    let* sym_shift_left_equal = dec_sym_shift_left_equal node in
    Ok (Bitwise_sl_eq sym_shift_left_equal)
  | "**=" ->
    let* sym_exponent_equal = dec_sym_exponent_equal node in
    Ok (Exp_eq sym_exponent_equal)
  | "&&=" ->
    let* sym_conjunction_equal = dec_sym_conjunction_equal node in
    Ok (Logical_and_eq sym_conjunction_equal)
  | "||=" ->
    let* sym_disjunction_equal = dec_sym_disjunction_equal node in
    Ok (Logical_or_eq sym_disjunction_equal)
  | "??=" ->
    let* sym_non_null_equal = dec_sym_non_null_equal node in
    Ok (Non_null_eq sym_non_null_equal)
  | _ -> mk_err Augmented_assignment node

and dec_augmented_assignment_lhs ?(comments = []) node
    : (augmented_assignment_lhs, _) result
  =
  match get_name node with
  | "member_expression" ->
    let* expression = wrap dec_member_expression ~comments node in
    Ok (Member_expression expression)
  | "subscript_expression" ->
    let* expression = wrap dec_subscript_expression ~comments node in
    Ok (Subscript_expression expression)
  | "identifier" -> Ok (Identifier (dec_identifier ~comments node))
  | "parenthesized_expression" ->
    let* expression = dec_parenthesized_expression ~comments node in
    Ok (Parenthesized_expression expression)
  | _ -> mk_err Expression node

(* Await expression *)

and dec_await_expression ?(comments = []) node : (await_expression, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Await node
  | _ ->
    let* kwd_await = first_child_named "await" node ~err:Await in
    let* kwd_await = dec_kwd_await ~comments kwd_await in
    let* expression = child_ranked 1 node ~err:Expression in
    let* expression = dec_expression expression in
    Ok { kwd_await; expression }

(* Unary expression *)

and dec_unary_expression ?(comments = []) node : (unary_expression, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Unary_operator node
  | _ ->
    let* operator_field = child_with_field "operator" node ~err:Unary_operator in
    let* operator = dec_unary_operator ~comments operator_field in
    let* argument_field = child_with_field "argument" node ~err:Expression in
    let* argument = dec_expression argument_field in
    Ok ({ operator; argument } : unary_expression)

and dec_unary_operator ?(comments = []) node : (unary_operator, _) result =
  match get_name node with
  | "!" ->
    let* sym_qmark = dec_sym_qmark ~comments node in
    Ok (Logical_neg sym_qmark)
  | "~" ->
    let* sym_tilde = dec_sym_tilde ~comments node in
    Ok (Bitwise_not sym_tilde)
  | "-" ->
    let* sym_minus = dec_sym_minus ~comments node in
    Ok (Neg sym_minus)
  | "+" ->
    let* sym_plus = dec_sym_plus ~comments node in
    Ok (Plus_zero sym_plus)
  | "typeof" ->
    let* kwd_typeof = dec_kwd_typeof ~comments node in
    Ok (Typeof kwd_typeof)
  | "void" ->
    let* kwd_void = dec_kwd_void ~comments node in
    Ok (Void kwd_void)
  | "delete" ->
    let* kwd_delete = dec_kwd_delete ~comments node in
    Ok (Delete kwd_delete)
  | _ -> mk_err Unary_operator node

(* Binary expression *)

and dec_binary_expression ?(comments = []) node : (binary_expression, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Expression node
  | _ ->
    let comments = comments @ prev_comments node in
    let* left_field = child_with_field "left" node ~err:Expression in
    let* lhs_expr = dec_lhs_bin_expression ~comments left_field in
    let* operator = child_with_field "operator" node ~err:Binary_operator in
    let* operator = dec_binary_operator operator in
    let* right_field = child_with_field "right" node ~err:Expression in
    let* rhs_expr = dec_expression right_field in
    Ok { lhs_expr; operator; rhs_expr }

and dec_lhs_bin_expression ~comments node : (lhs_bin_expression, _) result =
  match get_name node with
  | "private_property_identifier" ->
    Ok (Lhs_bin_hash (dec_private_property_identifier ~comments node))
  | _ ->
    let* hash = dec_expression ~comments node in
    Ok (Lhs_bin_expression hash)

and dec_binary_operator node : (binary_operator, _) result =
  match get_name node with
  | "&&" ->
    let* sym_conjunction = dec_sym_conjunction node in
    Ok (Logical_and sym_conjunction)
  | "||" ->
    let* sym_disjunction = dec_sym_disjunction node in
    Ok (Logical_or sym_disjunction)
  | ">>" ->
    let* sym_shift_right = dec_sym_shift_right node in
    Ok (Bitwise_sr sym_shift_right)
  | ">>>" ->
    let* sym_unsigned_shift_right = dec_sym_unsigned_shift_right node in
    Ok (Bitwise_usr sym_unsigned_shift_right)
  | "<<" ->
    let* sym_shift_left = dec_sym_shift_left node in
    Ok (Bitwise_sl sym_shift_left)
  | "&" ->
    let* sym_and = dec_sym_and node in
    Ok (Bitwise_and sym_and)
  | "^" ->
    let* sym_xor = dec_sym_xor node in
    Ok (Bitwise_xor sym_xor)
  | "|" ->
    let* sym_or = dec_sym_or node in
    Ok (Bitwise_or sym_or)
  | "+" ->
    let* sym_plus = dec_sym_plus node in
    Ok (Add sym_plus)
  | "-" ->
    let* sym_minus = dec_sym_minus node in
    Ok (Sub sym_minus)
  | "*" ->
    let* sym_asterisk = dec_sym_asterisk node in
    Ok (Mult sym_asterisk)
  | "/" ->
    let* sym_div = dec_sym_div node in
    Ok (Div sym_div)
  | "%" ->
    let* sym_rem = dec_sym_rem node in
    Ok (Rem sym_rem)
  | "**" ->
    let* sym_exponent = dec_sym_exponent node in
    Ok (Exp sym_exponent)
  | "<" ->
    let* sym_less_than = dec_sym_less_than node in
    Ok (Lt sym_less_than)
  | "<=" ->
    let* sym_less_than_or_equal = dec_sym_less_than_or_equal node in
    Ok (Leq sym_less_than_or_equal)
  | "==" ->
    let* sym_strict_equal = dec_sym_strict_equal node in
    Ok (Equal sym_strict_equal)
  | "===" ->
    let* sym_no_conv_equal = dec_sym_no_conv_equal node in
    Ok (Strict_eq sym_no_conv_equal)
  | "!=" ->
    let* sym_different = dec_sym_different node in
    Ok (Neq sym_different)
  | "!==" ->
    let* sym_no_conv_different = dec_sym_no_conv_different node in
    Ok (Strict_neq sym_no_conv_different)
  | ">=" ->
    let* sym_greater_than_or_equal = dec_sym_greater_than_or_equal node in
    Ok (Geq sym_greater_than_or_equal)
  | ">" ->
    let* sym_greater_than = dec_sym_greater_than node in
    Ok (Gt sym_greater_than)
  | "??" ->
    let* sym_non_null = dec_sym_non_null node in
    Ok (Non_null sym_non_null)
  | "instanceof" ->
    let* kwd_instanceof = dec_kwd_instanceof node in
    Ok (Instance_of kwd_instanceof)
  | "in" ->
    let* kwd_in = dec_kwd_in node in
    Ok (In kwd_in : binary_operator)
  | _ -> mk_err Binary_operator node

(* Ternary expression *)

and dec_ternary_expression ?(comments = []) node : (ternary_expression, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Expression node
  | _ ->
    let* condition_field = child_with_field "condition" node ~err:Expression in
    let* condition = dec_expression ~comments condition_field in
    let* sym_qmark = first_child_named "?" node ~err:Question_mark in
    let* sym_qmark = dec_sym_qmark sym_qmark in
    let* consequence_field = child_with_field "consequence" node ~err:Expression in
    let* consequence = dec_expression consequence_field in
    let* sym_colon = first_child_named ":" node ~err:Colon in
    let* sym_colon = dec_sym_colon sym_colon in
    let* alternative_field = child_with_field "alternative" node ~err:Expression in
    let* alternative = dec_expression alternative_field in
    Ok { condition; sym_qmark; consequence; sym_colon; alternative }

(* Update expression *)

and dec_update_expression ?(comments = []) node : (update_expression, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Expression node
  | _ ->
    let region = !get_region node in
    let* first_child = child_ranked 0 node ~err:Incr_or_decr_or_expr in
    (match get_name first_child with
    | "++" | "--" ->
      let* operator = dec_incr_decr_operator ~comments first_child in
      let* argument_field = child_with_field "argument" node ~err:Expression in
      let* argument = dec_expression argument_field in
      let update : update = { argument; operator } in
      let update = Wrap.make update region in
      Ok (Update_prefix update)
    | _ ->
      let* argument = dec_expression ~comments first_child in
      let* operator_field =
        child_with_field "operator" node ~err:Increment_or_decrement
      in
      let* operator = dec_incr_decr_operator operator_field in
      let update : update = { argument; operator } in
      let update = Wrap.make update region in
      Ok (Update_postfix update))

and dec_incr_decr_operator ?(comments = []) node : (incr_decr_operator, _) result =
  match get_name node with
  | "++" ->
    let* sym_increment = dec_sym_increment ~comments node in
    Ok (Increment sym_increment)
  | "--" ->
    let* sym_decrement = dec_sym_decrement ~comments node in
    Ok (Decrement sym_decrement)
  | _ -> mk_err Increment_or_decrement node

(* New expression

   Note that the constructor field is a primary expression, but
   "primary_expression" is a supertype, that is, a hidden rule. We
   assume it is an "expression", since primary expressions are a subset
   of them. *)

and dec_new_expression ?(comments = []) node : (new_expression, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err New node
  | _ ->
    let* kwd_new = first_child_named "new" node ~err:New in
    let* kwd_new = dec_kwd_new ~comments kwd_new in
    let* constructor_field = child_with_field "constructor" node ~err:Expression in
    let* constructor = dec_primary_expression constructor_field in
    let type_arguments_field = child_with_field_opt "type_arguments" node in
    let* type_arguments = make_opt_res dec_type_arguments type_arguments_field in
    let arguments_field = child_with_field_opt "arguments" node in
    let* arguments = make_opt_res dec_arguments arguments_field in
    Ok { kwd_new; constructor; type_arguments; arguments }

(* Yield expression *)

and dec_yield_expression node : (yield_expression, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Yield node
  | _ ->
    let region = !get_region node in
    let* kwd_yield = first_child_named "yield" node ~err:Yield in
    let* kwd_yield = dec_kwd_yield kwd_yield in
    (match child_ranked_opt 1 node with
    | None -> Ok (Yield (Wrap.make (kwd_yield, None) region))
    | Some snd_child ->
      (match get_name snd_child with
      | "*" ->
        let* sym_asterisk = dec_sym_asterisk snd_child in
        let* expression = child_ranked 2 node ~err:Expression in
        let* expression = dec_expression expression in
        let iterable = kwd_yield, sym_asterisk, expression in
        Ok (Yield_iterable (Wrap.make iterable region))
      | _ ->
        let* expression = dec_expression snd_child in
        let yield = kwd_yield, Some expression in
        Ok (Yield (Wrap.make yield region))))

(* As-expression *)

and dec_as_expression ?(comments = []) node : (as_expression, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Expression node
  | _ ->
    let* expression = child_ranked 0 node ~err:Expression in
    let* expression = dec_expression ~comments expression in
    let* kwd_as = first_child_named "as" node ~err:As in
    let* kwd_as = dec_kwd_as kwd_as in
    let* as_what = child_ranked 2 node ~err:Const_or_type in
    let* as_what = dec_as_what as_what in
    Ok (expression, kwd_as, as_what)

and dec_as_what node : (as_what, _) result =
  match get_name node with
  | "const" ->
    let* kwd_const = dec_kwd_const node in
    Ok (As_const kwd_const)
  | _ ->
    let* type_expr = dec_type node in
    Ok (As_type type_expr)

(* Statisfies-expression *)

and dec_satisfies_expression ?(comments = []) node : (satisfies_expression, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Expression node
  | _ ->
    let* expression = child_ranked 0 node ~err:Expression in
    let* expression = dec_expression ~comments expression in
    let* kwd_satisfies = first_child_named "satisfies" node ~err:Satisfies in
    let* kwd_satisfies = dec_kwd_satisfies kwd_satisfies in
    let* type_child = child_ranked 2 node ~err:Type_expression in
    let* type_expr = dec_type type_child in
    Ok (expression, kwd_satisfies, type_expr)

(* Instantiation expression *)

and dec_instantiation_expression ?(comments = []) node
    : (instantiation_expression, _) result
  =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Expression node
  | _ ->
    let* expression = named_child_ranked 0 node ~err:Expression in
    let* expression = dec_expression ~comments expression in
    let* type_arguments_field =
      child_with_field "type_arguments" node ~err:Type_arguments
    in
    let* type_arguments = dec_type_arguments type_arguments_field in
    Ok (expression, type_arguments)

(* Type assertion *)

and dec_type_assertion ?(comments = []) node : (type_assertion, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Type_arguments node
  | _ ->
    let* type_arguments = named_child_ranked 0 node ~err:Type_arguments in
    let* type_arguments = dec_type_arguments ~comments type_arguments in
    let* expression = named_child_ranked 1 node ~err:Expression in
    let* expression = dec_expression expression in
    Ok (type_arguments, expression)

(* Subscript expression (see [dec_member_expression]) *)

and dec_subscript_expression ?(comments = []) node : (subscript_expression, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Expression node
  | _ ->
    let* object_field = child_with_field "object" node ~err:Expression in
    let* object_expr = dec_expression ~comments object_field in
    let optional_chain_field = child_with_field_opt "optional_chain" node in
    let* optional_chain = make_opt_res dec_optional_chain optional_chain_field in
    let* index_field = child_with_field "index" node ~err:Expression in
    let* contents = dec_index index_field in
    let* sym_lbracket = first_child_named "[" node ~err:Left_bracket in
    let* opening = dec_sym_lbracket sym_lbracket in
    let* sym_rbracket = first_child_named "]" node ~err:Right_bracket in
    let* closing = dec_sym_rbracket sym_rbracket in
    let region = !get_region node in
    let brackets = { opening; contents; closing } in
    let index = Brackets (Wrap.make brackets region) in
    Ok { object_expr; optional_chain; index }

and dec_optional_chain node : (optional_chain, _) result =
  match get_name node with
  | "optional_chain" ->
    let* sym_optional_chain = dec_sym_optional_chain node in
    Ok (Optional_chain sym_optional_chain)
  | _ -> mk_err Optional_chain node

and dec_index ?(comments = []) node : (sequence_expression, _) result =
  (* See [dec_expressions] *)
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Index_expression node
  | "sequence_expression" -> dec_sequence_expression ~comments node
  | _ ->
    let* expression = dec_expression ~comments node in
    let region = !get_region node in
    Ok (Wrap.make Nonempty_list.[ expression ] region)

(* Member expression *)

and dec_member_expression ?(comments = []) node : (member_expression, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Member_expression node
  | _ ->
    let* object_field = child_with_field "object" node ~err:Expression in
    let* object_expr = dec_object_member ~comments object_field in
    let optional_chain_field = child_with_field_opt "optional_chain" node in
    let* property_field = child_with_field "property" node ~err:Property_identifier in
    let* property = dec_property_ident property_field in
    let* selector =
      match optional_chain_field with
      | None ->
        let* sym_dot = first_child_named "." node ~err:Dot in
        let* sym_dot = dec_sym_dot sym_dot in
        Ok (Dot sym_dot)
      | Some node ->
        let* sym_optional_chain = dec_sym_optional_chain node in
        Ok (Optional_chain sym_optional_chain : selector)
    in
    Ok ({ object_expr; selector; property } : member_expression)

and dec_object_member ?comments node : (object_member, _) result =
  match get_name node with
  | "import" ->
    let* kwd_import = dec_kwd_import ?comments node in
    Ok (Object_member_import kwd_import)
  | _ ->
    let* expression = dec_expression ?comments node in
    Ok (Object_member_expression expression)

and dec_property_ident ?comments node : (property_ident, _) result =
  let identifier = dec_identifier ?comments node in
  match get_name node with
  | "private_property_identifier" -> Ok (Private_property_identifier identifier)
  | "property_identifier" -> Ok (Property_identifier identifier)
  | _ -> mk_err Property_identifier node

(* Parenthesised expression *)

and dec_parenthesized_expression ?(comments = []) node
    : (parenthesized_expression, _) result
  =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Parenthesized_expression node
  | _ ->
    let comments = comments @ prev_comments node in
    let* sym_lparen = first_child_named "(" node ~err:Left_parenthesis in
    let* opening = dec_sym_lparen ~comments sym_lparen in
    let* sym_rparen = first_child_named ")" node ~err:Right_parenthesis in
    let* closing = dec_sym_rparen sym_rparen in
    let* first_named_child = child_ranked 1 node ~err:Expression in
    let type_field = child_with_field_opt "type" node in
    let* (contents : in_expressions) =
      match type_field with
      | Some type_field ->
        let* expression = dec_expression first_named_child in
        let* type_annotation = dec_type_annotation type_field in
        Ok (Typed_expression (expression, type_annotation))
      | None ->
        let* seq_expr =
          match get_name first_named_child with
          | "sequence_expression" -> dec_sequence_expression first_named_child
          | _ ->
            let* expression = dec_expression first_named_child in
            let expressions = Nonempty_list.singleton expression in
            let region = !get_region first_named_child in
            Ok (Wrap.make expressions region)
        in
        Ok (Sequence_expression seq_expr)
    in
    let region = !get_region node in
    Ok (Parens (Wrap.make { opening; contents; closing } region))

(* Sequence expression *)

and dec_sequence_expression ?(comments = []) node : (sequence_expression, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Expression node
  | _ ->
    let raw_children = collect_named_children node in
    let* list = wrap_ne_list_opt_of_children ~comments dec_expression raw_children in
    (match list with
    | Some ne_list -> Ok ne_list
    | None -> mk_err Expression node)

(* Object expression *)

and dec_object_expr ?(comments = []) node : (object_expr, _) result =
  dec_list_in_braces ~comments node dec_object_entry ~err:Object_expression

and dec_object_entry ?(comments = []) node : (object_entry, _) result =
  match get_name node with
  | "pair" ->
    let* pair = wrap dec_pair ~comments node in
    Ok (Object_entry_pair pair)
  | "spread_element" ->
    let* spread = wrap dec_spread_element ~comments node in
    Ok (Object_entry_spread spread)
  | "method_definition" ->
    let* definition = wrap dec_method_definition ~comments node in
    Ok (Object_entry_method definition)
  | "shorthand_property_identifier" ->
    let* pattern = dec_shorthand_property_identifier_pattern ~comments node in
    Ok (Object_entry_shorthand pattern)
  | _ -> mk_err Object_field node

(* Pairs *)

and dec_pair ?(comments = []) node : (pair, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Key_value_pair node
  | _ ->
    let* key_field = child_with_field "key" node ~err:Property_name in
    let* key = dec_property_name ~comments key_field in
    let* sym_colon = first_child_named ":" node ~err:Colon in
    let* sym_colon = dec_sym_colon sym_colon in
    let* value_field = child_with_field "value" node ~err:Expression in
    let* value = dec_expression value_field in
    Ok { key; sym_colon; value }

(* LHS expression *)

and dec_lhs_expression ?comments node : (lhs_expression, _) result =
  match get_name node with
  | "member_expression" ->
    let* expression = wrap dec_member_expression ?comments node in
    Ok (Member_expression expression : lhs_expression)
  | "subscript_expression" ->
    let* expression = wrap dec_subscript_expression ?comments node in
    Ok (Subscript_expression expression : lhs_expression)
  | "identifier" -> Ok (Identifier (dec_identifier ?comments node))
  | "undefined" ->
    let* kwd_undefined = dec_kwd_undefined ?comments node in
    Ok (Undefined kwd_undefined)
  | "object_pattern" ->
    let* pattern = dec_object_pattern ?comments node in
    Ok (Pattern (Pattern_object pattern))
  | "array_pattern" ->
    let* pattern = dec_array_pattern ?comments node in
    Ok (Pattern (Pattern_array pattern))
  | "non_null_expression" ->
    let* expression = dec_non_null_expression ?comments node in
    Ok (Non_null_expression expression)
  | _ -> mk_err Pattern node

(* Non-null expression *)

and dec_non_null_expression ?comments node : (expression, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Non_null_expression node
  | _ ->
    let* child = named_child_ranked 0 node ~err:Non_null_expression in
    dec_expression ?comments child

(* PRIMARY EXPRESSION *)

and dec_primary_expression ?(comments = []) node : (primary_expression, _) result =
  match get_name node with
  | "subscript_expression" ->
    let* expression = wrap dec_subscript_expression node in
    Ok (E_subscript_expression expression)
  | "member_expression" ->
    let* expression = wrap dec_member_expression node in
    Ok (E_member_expression expression)
  | "parenthesized_expression" ->
    let* expression = dec_parenthesized_expression node in
    Ok (E_parenthesized_expression expression)
  | "identifier" -> Ok (E_identifier (dec_identifier ~comments node))
  | "undefined" ->
    let* kwd_undefined = dec_kwd_undefined node in
    Ok (E_undefined kwd_undefined)
  | "this" ->
    let* kwd_this = dec_kwd_this node in
    Ok (E_this kwd_this)
  | "super" ->
    let* kwd_super = dec_kwd_super node in
    Ok (E_super kwd_super)
  | "number" ->
    let* number = dec_number ~comments node in
    Ok (E_number number)
  | "string" -> Ok (E_string (dec_string node))
  | "template_string" ->
    let* expression = wrap dec_template_string ~comments node in
    Ok (E_template_string expression)
  | "regex" -> Ok (E_regex (dec_regex node))
  | "true" ->
    let* kwd_true = dec_kwd_true node in
    Ok (E_true kwd_true)
  | "false" ->
    let* kwd_false = dec_kwd_false node in
    Ok (E_false kwd_false)
  | "null" ->
    let* kwd_null = dec_kwd_null node in
    Ok (E_null kwd_null)
  | "object" ->
    let* expression = dec_object_expr node in
    Ok (E_object expression)
  | "array" ->
    let* expression = dec_array node in
    Ok (E_array expression)
  | "function_expression" ->
    let* expression = wrap dec_function_expression node in
    Ok (E_function_expression expression)
  | "arrow_function" ->
    let* expression = wrap dec_arrow_function node in
    Ok (E_arrow_function expression)
  | "generator_function" ->
    let* expression = wrap dec_generator_function node in
    Ok (E_generator_function expression)
  | "class" ->
    let* expression = wrap dec_class node in
    Ok (E_class expression)
  | "meta_property" ->
    let* expression = dec_meta_property node in
    Ok (E_meta_property expression)
  | "call_expression" ->
    let* expression = dec_call_expression node in
    Ok (E_call_expression expression)
  | "non_null_expression" ->
    let* expression = dec_non_null_expression node in
    Ok (E_non_null_expression expression)
  | _ -> mk_err Expression node

(* Call expression *)

and dec_call_expression ?(comments = []) node : (call_expression, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Call_expression node
  | _ ->
    let* function_field = child_with_field "function" node ~err:Expression in
    let member_selection = first_child_named_opt "?." node in
    let type_arguments_field = child_with_field_opt "type_arguments" node in
    let* type_arguments = make_opt_res dec_type_arguments type_arguments_field in
    let* arguments_field = child_with_field "arguments" node ~err:Arguments in
    (match member_selection with
    | None ->
      let* lambda = dec_fun_call ~comments function_field in
      let* arguments = dec_arguments_to_call arguments_field in
      let call = { lambda; type_arguments; arguments } in
      let region = !get_region node in
      Ok (Call (Wrap.make call region))
    | Some _ ->
      let* lambda = dec_primary_expression ~comments function_field in
      let* arguments = dec_arguments arguments_field in
      let call = { lambda; type_arguments; arguments } in
      let region = !get_region node in
      Ok (Member (Wrap.make call region)))

and dec_fun_call ?(comments = []) node : (fun_call, _) result =
  match get_name node with
  | "import" ->
    let* kwd_import = dec_kwd_import ~comments node in
    Ok (Import kwd_import)
  | _ ->
    let* expression = dec_expression ~comments node in
    Ok (Fun_call expression)

and dec_arguments_to_call node : (arguments_to_call, _) result =
  match get_name node with
  | "template_string" ->
    let* expression = wrap dec_template_string node in
    Ok (Template_string expression)
  | _ ->
    let* arguments = dec_arguments node in
    Ok (Arguments arguments)

(* Meta-property *)

and dec_meta_property ?(comments = []) node : (meta_property, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Meta_property node
  | _ ->
    let region = !get_region node in
    let* fst_child = child_ranked 0 node ~err:New_or_import in
    let* snd_child = child_ranked 2 node ~err:Target_or_meta in
    (match get_name fst_child with
    | "new" ->
      let* kwd_new = dec_kwd_new ~comments fst_child in
      let* kwd_target = dec_kwd_target snd_child in
      let meta = kwd_new, kwd_target in
      Ok (Meta_new_target (Wrap.make meta region))
    | "import" ->
      let* kwd_import = dec_kwd_import ~comments fst_child in
      let* kwd_meta = dec_kwd_meta snd_child in
      let meta = kwd_import, kwd_meta in
      Ok (Meta_import_meta (Wrap.make meta region))
    | _ -> mk_err Meta_property fst_child)

(* Class *)

and dec_class ?(comments = []) node : (class_expression, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Class_expression node
  | _ ->
    let comments = comments @ prev_comments node in
    let decorators = children_named "decorator" node in
    let* decorators = list_of_children dec_decorator decorators in
    let* kwd_class = first_child_named "class" node ~err:Class in
    let* kwd_class = dec_kwd_class ~comments kwd_class in
    let name_field = child_with_field_opt "name" node in
    let name = make_opt dec_identifier name_field in
    let type_parameters_field = child_with_field_opt "type_parameters" node in
    let* type_parameters = make_opt_res dec_type_parameters type_parameters_field in
    let heritage_child = first_child_named_opt "class_heritage" node in
    let* class_heritage = make_opt_res dec_class_heritage heritage_child in
    let* body_field = child_with_field "body" node ~err:Class_body in
    let* body = dec_class_body body_field in
    Ok { decorators; kwd_class; name; type_parameters; class_heritage; body }

(* Generator function *)

and dec_generator_function ?(comments = []) node : (generator_function, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Generator_function node
  | _ ->
    let* fun_decl = wrap dec_function_expression ~comments node in
    let* sym_asterisk = first_child_named "*" node ~err:Asterisk in
    let* sym_asterisk = dec_sym_asterisk sym_asterisk in
    Ok (sym_asterisk, fun_decl)

(* Arrow function *)

and dec_arrow_function ?(comments = []) node : (arrow_function, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Arrow_function node
  | _ ->
    let kwd_async = first_child_named_opt "async" node in
    let* kwd_async = make_opt_res dec_kwd_async kwd_async in
    let* sym_arrow = first_child_named "=>" node ~err:Arrow in
    let* sym_arrow = dec_sym_arrow sym_arrow in
    let* body_field = child_with_field "body" node ~err:Block_or_expression in
    let* body = dec_function_body body_field in
    let parameter_field = child_with_field_opt "parameter" node in
    (match parameter_field with
    | Some parameter_field ->
      let parameters = Parameter (dec_identifier ~comments parameter_field) in
      Ok { kwd_async; parameters; sym_arrow; body }
    | None ->
      let* signature = dec_call_signature ~comments node in
      let parameters : parameters = Call_signature signature in
      Ok { kwd_async; parameters; sym_arrow; body })

and dec_function_body ?(comments = []) node : (function_body, _) result =
  match get_name node with
  | "statement_block" ->
    let* statement = dec_statement_block ~comments node in
    Ok (Statement_block statement)
  | _ ->
    let* expression = dec_expression ~comments node in
    Ok (Expression expression)

(* Function (expression) *)

and dec_function_expression ?(comments = []) node : (function_expression, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Function_expression node
  | _ ->
    let comments = comments @ prev_comments node in
    let kwd_async = first_child_named_opt "async" node in
    let async_comments, function_comments =
      match kwd_async with
      | None -> [], comments
      | Some _ -> comments, []
    in
    let* kwd_async = make_opt_res (dec_kwd_async ~comments:async_comments) kwd_async in
    let* kwd_function = first_child_named "function" node ~err:Function in
    let* kwd_function = dec_kwd_function ~comments:function_comments kwd_function in
    let name_field = child_with_field_opt "name" node in
    let name = make_opt dec_identifier name_field in
    let* call_sig = dec_call_signature node in
    let* body_field = child_with_field "body" node ~err:Block in
    let* body = dec_statement_block body_field in
    Ok { kwd_async; kwd_function; name; call_sig; body }

(* Array (expression) *)

and dec_array ?comments node : (array, _) result =
  dec_list_in_brackets ?comments node dec_array_cell ~err:Array

and dec_array_cell ?comments node =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Array_cell node
  | "spread_element" ->
    let* spread = wrap dec_spread_element node in
    Ok (Spread_element spread)
  | _ ->
    (* Hidden rule: *)
    let* expression = dec_expression ?comments node in
    Ok (Expression expression : argument)

(* Template strings *)

and dec_template_string ?(comments = []) node : (template_string, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Template_string node
  | _ ->
    let* opening_bquote = child_ranked 0 node ~err:Backquote in
    let* opening_bquote = dec_sym_backquote ~comments opening_bquote in
    let named_children = collect_named_children node in
    let fragments = List.map ~f:dec_template_string_fragment named_children in
    let* fragments = Result.all fragments in
    let* closing_bquote = last_child node ~err:Backquote in
    let* closing_bquote = dec_sym_backquote closing_bquote in
    Ok (opening_bquote, fragments, closing_bquote)

and dec_template_string_fragment ?(comments = []) node
    : (template_string_fragment, _) result
  =
  match get_name node with
  | "string_fragment" -> Ok (String_fragment (make_node ~comments node))
  | "escape_sequence" -> Ok (Escape_sequence (make_node ~comments node))
  | "template_substitution" -> Ok (Template_substitution (make_node ~comments node))
  | _ -> mk_err Template_string node

(* PATTERN

   The JavaScript tree-sitter grammar has the non-terminal
   "pattern" be a supertype, that is, a hidden rule. *)

and dec_pattern ?(comments = []) node : (pattern, _) result =
  match get_name node with
  | "rest_pattern" ->
    let* pattern = wrap dec_rest_pattern ~comments node in
    Ok (P_rest_pattern pattern)
  | _ ->
    let* expression = dec_lhs_expression ~comments node in
    (match expression with
    | Member_expression expression -> Ok (P_member_expression expression)
    | Subscript_expression expression -> Ok (P_subscript_expression expression)
    | Identifier identifier -> Ok (P_identifier identifier)
    | Undefined kwd_undefined -> Ok (P_undefined kwd_undefined)
    | Pattern pattern -> Ok (P_destructuring_pattern pattern)
    | Non_null_expression expression -> Ok (P_non_null_expression expression))

(* Object pattern *)

and dec_object_pattern ?comments node : (object_pattern, _) result =
  dec_list_in_braces ?comments node dec_member_pattern ~err:Object_pattern

and dec_member_pattern ?(comments = []) node : (member_pattern, _) result =
  match get_name node with
  | "pair_pattern" ->
    let* pattern = dec_pair_pattern ~comments node in
    Ok (Member_pair_pattern pattern)
  | "rest_pattern" ->
    let* pattern = wrap dec_rest_pattern ~comments node in
    Ok (Member_rest_pattern pattern)
  | "object_assignment_pattern" ->
    let* pattern = wrap dec_object_assignment_pattern node in
    Ok (Member_object_assignment pattern)
  | "shorthand_property_identifier_pattern" ->
    let* shorthand = dec_shorthand_property_identifier_pattern node in
    Ok (Member_shorthand_property shorthand)
  | _ -> mk_err Object_pattern_field node

(* Pair pattern *)

and dec_pair_pattern ?(comments = []) node : (pair_pattern wrap, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Pair_pattern node
  | _ ->
    let* key_field = child_with_field "key" node ~err:Property_name in
    let* key = dec_property_name ~comments key_field in
    let* sym_colon = first_child_named ":" node ~err:Colon in
    let* sym_colon = dec_sym_colon sym_colon in
    let* value_field = child_with_field "value" node ~err:Pattern in
    let* value = dec_pair_value_pattern value_field in
    let region = !get_region node in
    Ok (Wrap.make { key; sym_colon; value } region)

and dec_pair_value_pattern node : (pair_value_pattern, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Value_of_pair_pattern node
  | "assignment_pattern" ->
    let* pattern = wrap dec_assignment_pattern node in
    Ok (Pair_value_assignment pattern)
  | _ ->
    (* Hidden rule *)
    let* pattern = dec_pattern node in
    Ok (Pair_value pattern)

(* Rest pattern *)

and dec_rest_pattern ?(comments = []) node : (rest_pattern, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Rest_pattern node
  | _ ->
    let* sym_ellipsis = first_child_named "..." node ~err:Ellipsis in
    let* sym_ellipsis = dec_sym_ellipsis ~comments sym_ellipsis in
    let* expr_child = named_child_ranked 0 node ~err:Expression in
    let* expression = dec_lhs_expression expr_child in
    Ok { sym_ellipsis; expression }

(* Object assignment pattern *)

and dec_object_assignment_pattern ?(comments = []) node
    : (object_assignment_pattern, _) result
  =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Object_assignment_pattern node
  | _ ->
    let* left_field = child_with_field "left" node ~err:Pattern in
    let* left = dec_object_lhs_pattern ~comments left_field in
    let* sym_equal = first_child_named "=" node ~err:Equal in
    let* sym_equal = dec_sym_equal sym_equal in
    let* right_field = child_with_field "right" node ~err:Expression in
    let* right = dec_expression right_field in
    Ok ({ left; sym_equal; right } : object_assignment_pattern)

and dec_object_lhs_pattern ?comments node : (object_lhs_pattern, _) result =
  match get_name node with
  | "shorthand_property_identifier_pattern" ->
    let* shorthand = dec_shorthand_property_identifier_pattern ?comments node in
    Ok (Decl_ident shorthand)
  | _ ->
    (* Hidden rule *)
    let* pattern = dec_destructuring_pattern ?comments node in
    Ok (Decl_pattern pattern)

(* Shorthand property identifier pattern *)

and dec_shorthand_property_identifier_pattern ?comments node : (identifier, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Identifier node
  | _ -> Ok (dec_identifier ?comments node)

(* Array pattern *)

and dec_array_pattern ?comments node : (array_pattern, _) result =
  dec_list_in_brackets ?comments node dec_array_cell_pattern ~err:Array_pattern

and dec_array_cell_pattern ?comments node : (array_cell_pattern, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Array_cell_pattern node
  | "assignment_pattern" ->
    let* pattern = dec_assignment_pattern ?comments node in
    let region = !get_region node in
    Ok (Cell_assignment (Wrap.make pattern region))
  | _ ->
    (* hidden rule *)
    let* pattern = dec_pattern ?comments node in
    Ok (Cell_pattern pattern)

(* Assignment pattern *)

and dec_assignment_pattern ?(comments = []) node : (assignment_pattern, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Assignment_pattern node
  | _ ->
    let* left_field = child_with_field "left" node ~err:Pattern in
    let* left = dec_pattern ~comments left_field in
    let* sym_equal = first_child_named "=" node ~err:Equal in
    let* sym_equal = dec_sym_equal sym_equal in
    let* right_field = child_with_field "right" node ~err:Expression in
    let* right = dec_expression right_field in
    Ok { left; sym_equal; right }

(* Rule "_destructuring_pattern" is inlined. *)

and dec_destructuring_pattern ?comments node : (destructuring_pattern, _) result =
  match get_name node with
  | "object_pattern" ->
    let* pattern = dec_object_pattern ?comments node in
    Ok (Pattern_object pattern)
  | "array_pattern" ->
    let* pattern = dec_array_pattern ?comments node in
    Ok (Pattern_array pattern)
  | _ -> mk_err Object_or_array_pattern node

(* TYPES

   The non-terminals "type" and "primary_type" are supertypes in the
   TypeScript grammar, which means that they are hidden rules. *)

and dec_type ?(comments = []) node : (type_expr, _) result =
  match get_name node with
  | "function_type" ->
    let* type_expr = wrap dec_function_type ~comments node in
    Ok (T_function_type type_expr)
  | "readonly_type" ->
    let* type_expr = wrap dec_readonly_type ~comments node in
    Ok (T_readonly_type type_expr)
  | "constructor_type" ->
    let* type_expr = wrap dec_constructor_type ~comments node in
    Ok (T_constructor_type type_expr)
  | "infer_type" ->
    let* type_expr = wrap dec_infer_type ~comments node in
    Ok (T_infer_type type_expr)
  (* A couple of aliases *)
  | "member_expression" ->
    let* expression =
      wrap dec_type_query_member_expression_in_type_annotation ~comments node
    in
    Ok (T_member_expression expression)
  | "call_expression" ->
    let* expression =
      wrap dec_type_query_call_expression_in_type_annotation ~comments node
    in
    Ok (T_call_expression expression)
  (* "primary_type" is hidden *)
  | _ ->
    let* type_expr = dec_primary_type ~comments node in
    Ok (T_primary_type type_expr)

(* Type queries in type annotations (expressions)
   NOTE: Rather mysterious. See ast.ml. *)

and dec_type_query_member_expression_in_type_annotation ?(comments = []) node
    : (type_query_member_expression_in_type_annotation, _) result
  =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Member_or_call node
  | _ ->
    let* object_field = child_with_field "object" node ~err:Member_or_call in
    let* sym_dot = first_child_named "." node ~err:Dot in
    let* property_field = child_with_field "property" node ~err:Property_identifier in
    let dec_object_field node =
      match get_name node with
      | "import" ->
        let* kwd_import = dec_kwd_import ~comments node in
        Ok (Type_query_object_import kwd_import)
      | "member_expression" ->
        let* member =
          wrap dec_type_query_member_expression_in_type_annotation ~comments node
        in
        Ok (Type_query_object_member member)
      | "call_expression" ->
        let* expression =
          wrap dec_type_query_call_expression_in_type_annotation ~comments node
        in
        Ok (Type_query_object_call expression)
      | _ -> mk_err Object_field node
    in
    let* object_expr = dec_object_field object_field in
    let* selector = dec_sym_dot sym_dot in
    let* property = dec_type_query_property property_field in
    Ok
      ({ object_expr; selector; property }
        : type_query_member_expression_in_type_annotation)

and dec_type_query_call_expression_in_type_annotation ?(comments = []) node
    : (type_query_call_expression_in_type_annotation, _) result
  =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Member_expression node
  | _ ->
    let* function_field = child_with_field "function" node ~err:Member_expression in
    let* arguments_field = child_with_field "arguments" node ~err:Arguments in
    let* lambda = dec_type_query_call_lambda ~comments function_field in
    let* arguments = dec_arguments arguments_field in
    Ok ({ lambda; arguments } : type_query_call_expression_in_type_annotation)

and dec_type_query_call_lambda ?(comments = []) node : (type_query_call_lambda, _) result =
  match get_name node with
  | "import" ->
    let* kwd_import = dec_kwd_import ~comments node in
    Ok (Type_query_call_import kwd_import : type_query_call_lambda)
  | "member_expression" ->
    let* expression =
      dec_type_query_member_expression_in_type_annotation ~comments node
    in
    Ok (Type_query_call_member expression)
  | _ -> mk_err Member_expression node

(* Primary type *)

and dec_primary_type ?(comments = []) node : (primary_type, _) result =
  match get_name node with
  | "parenthesized_type" ->
    let* type_expr = dec_parenthesized_type ~comments node in
    Ok (T_parenthesized_type type_expr)
  | "predefined_type" ->
    let* type_expr = dec_predefined_type ~comments node in
    Ok (T_predefined_type type_expr)
  | "type_identifier" ->
    let identifier = dec_type_identifier ~comments node in
    Ok (T_type_identifier identifier)
  | "nested_type_identifier" ->
    let* nested_id = wrap dec_nested_type_identifier ~comments node in
    Ok (T_nested_type_identifier nested_id)
  | "generic_type" ->
    let* type_expr = wrap dec_generic_type ~comments node in
    Ok (T_generic_type type_expr)
  | "object_type" ->
    let* type_expr = dec_object_type ~comments node in
    Ok (T_object_type type_expr)
  | "array_type" ->
    let* type_expr = wrap dec_array_type ~comments node in
    Ok (T_array_type type_expr)
  | "tuple_type" ->
    let* type_expr = dec_tuple_type ~comments node in
    Ok (T_tuple_type type_expr)
  | "flow_maybe_type" ->
    let* type_expr = wrap dec_flow_maybe_type ~comments node in
    Ok (T_flow_maybe_type type_expr)
  | "type_query" ->
    let* type_query = wrap dec_type_query ~comments node in
    Ok (T_type_query type_query)
  | "index_type_query" ->
    let* type_expr = wrap dec_index_type_query ~comments node in
    Ok (T_index_type_query type_expr)
  | "this_type" ->
    let* kwd_this = dec_kwd_this ~comments node in
    Ok (T_this kwd_this)
  | "existential_type" ->
    let* sym_asterisk = dec_existential_type ~comments node in
    Ok (T_existential_type sym_asterisk)
  | "literal_type" ->
    let* type_expr = dec_literal_type ~comments node in
    Ok (T_literal_type type_expr)
  | "lookup_type" ->
    let* type_expr = wrap dec_lookup_type ~comments node in
    Ok (T_lookup_type type_expr)
  | "conditional_type" ->
    let* type_expr = wrap dec_conditional_type ~comments node in
    Ok (T_conditional_type type_expr)
  | "template_literal_type" ->
    let* type_expr = wrap dec_template_literal_type ~comments node in
    Ok (T_template_literal_type type_expr)
  | "intersection_type" ->
    let* type_expr = wrap dec_intersection_type ~comments node in
    Ok (T_intersection_type type_expr)
  | "union_type" ->
    let* type_expr = wrap dec_union_type ~comments node in
    Ok (T_union_type type_expr)
  | _ -> mk_err Type_expression node

(* Existential type *)

and dec_existential_type ?(comments = []) node : (sym_asterisk, _) result =
  let* sym_asterisk = first_child_named "*" node ~err:Asterisk in
  dec_sym_asterisk ~comments sym_asterisk

(* Union type *)

and dec_union_type ?(comments = []) node : (union_type, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Union_type node
  | _ ->
    let* first_child = child_ranked 0 node ~err:Type_or_disjunction in
    let* sym_vbar = first_child_named "|" node ~err:Vertical_bar in
    (match get_name first_child with
    | "|" ->
      let* sym_vbar = dec_sym_vbar ~comments sym_vbar in
      let* single_type_node = child_ranked 1 node ~err:Type_expression in
      let* type_expr = dec_type single_type_node in
      Ok (None, sym_vbar, type_expr)
    | _ ->
      (* "type" is a supertype, therefore a hidden rule *)
      let* left_type = dec_type ~comments first_child in
      let* sym_vbar = dec_sym_vbar sym_vbar in
      let* right_type = child_ranked 2 node ~err:Type_expression in
      let* right_type = dec_type right_type in
      Ok (Some left_type, sym_vbar, right_type))

(* Intersection type *)

and dec_intersection_type ?(comments = []) node : (intersection_type, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Intersection_type node
  | _ ->
    let* first_child = child_ranked 0 node ~err:Type_or_conjunction in
    let* sym_and = first_child_named "&" node ~err:And in
    (match get_name first_child with
    | "&" ->
      let* sym_and = dec_sym_and ~comments sym_and in
      let* single_type_node = child_ranked 1 node ~err:Type_expression in
      let* type_expr = dec_type single_type_node in
      Ok (None, sym_and, type_expr)
    | _ ->
      (* "type" is a supertype, therefore a hidden rule *)
      let* left_type = dec_type ~comments first_child in
      let* sym_and = dec_sym_and sym_and in
      let* right_type = child_ranked 2 node ~err:Type_expression in
      let* right_type = dec_type right_type in
      Ok (Some left_type, sym_and, right_type))

(* Template literal type *)

and dec_template_literal_type ?(comments = []) node : (template_literal_type, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Template_literal_type node
  | _ ->
    let* opening_bquote = child_ranked 0 node ~err:Backquote in
    let* opening_bquote = dec_sym_backquote ~comments opening_bquote in
    let named_children = collect_named_children node in
    let fragments = List.map ~f:dec_template_type_fragment named_children in
    let* fragments = Result.all fragments in
    let* closing_bquote = last_child node ~err:Backquote in
    let* closing_bquote = dec_sym_backquote closing_bquote in
    Ok (opening_bquote, fragments, closing_bquote)

and dec_template_type_fragment ?(comments = []) node : (template_type_fragment, _) result =
  match get_name node with
  | "string_fragment" -> Ok (Template_type_string (dec_string ~comments node))
  | "template_type" ->
    let* type_expr = dec_template_type ~comments node in
    Ok (Template_type type_expr)
  | _ -> mk_err String_or_type node

and dec_template_type ?(comments = []) node : (template_type, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Template_literal_type node
  | _ ->
    let* type_node = child_ranked 1 node ~err:Type_expression in
    (match get_name type_node with
    | "infer_type" ->
      let* type_expr = wrap dec_infer_type ~comments type_node in
      Ok (Template_type_infer type_expr)
      (* "primary_type" is hidden *)
    | _ ->
      let* type_expr = dec_primary_type ~comments type_node in
      Ok (Template_type_primary type_expr))

(* Conditional type *)

and dec_conditional_type ?(comments = []) node : (conditional_type, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Conditional_type node
  | _ ->
    let* left_field = child_with_field "left" node ~err:Type in
    let* left = dec_type ~comments left_field in
    let* kwd_extends = first_child_named "extends" node ~err:Extends in
    let* kwd_extends = dec_kwd_extends kwd_extends in
    let* right_field = child_with_field "right" node ~err:Type_expression in
    let* right = dec_type right_field in
    let* sym_qmark = first_child_named "?" node ~err:Question_mark in
    let* sym_qmark = dec_sym_qmark sym_qmark in
    let* consequence_field = child_with_field "consequence" node ~err:Type_expression in
    let* consequence = dec_type consequence_field in
    let* sym_colon = first_child_named ":" node ~err:Colon in
    let* sym_colon = dec_sym_colon sym_colon in
    let* alternative_field = child_with_field "alternative" node ~err:Type_expression in
    let* alternative = dec_type alternative_field in
    Ok { left; kwd_extends; right; sym_qmark; consequence; sym_colon; alternative }

(* Look up type

   The non-terminals "type" and "primary_type" are supertypes in the
   TypeScript grammar, which means that they are hidden rules. *)

and dec_lookup_type ?(comments = []) node : (lookup_type, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Lookup_type node
  | _ ->
    let* primary_type_child = named_child_ranked 0 node ~err:Type_expression in
    let* primary_type = dec_primary_type ~comments primary_type_child in
    let* sym_lbracket = first_child_named "[" node ~err:Left_bracket in
    let* opening = dec_sym_lbracket sym_lbracket in
    let* type_child = next_sibling sym_lbracket ~err:Type_expression in
    let* contents = dec_type type_child in
    let* sym_rbracket = first_child_named "]" node ~err:Right_bracket in
    let* closing = dec_sym_rbracket sym_rbracket in
    let region = !get_region node in
    let brackets = { opening; contents; closing } in
    let index_type = Brackets (Wrap.make brackets region) in
    Ok (primary_type, index_type)

(* Literal type *)

and dec_literal_type ?(comments = []) node : (literal_type, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Literal_type node
  | _ ->
    let* child = named_child_ranked 0 node ~err:Literal_type in
    (match get_name child with
    | "unary_expression" ->
      let* expression = wrap dec_unary_expression ~comments child in
      Ok (T_unary_type expression)
    | "number" ->
      let* number = dec_number ~comments child in
      Ok (T_number number : literal_type)
    | "string" -> Ok (T_string (dec_string ~comments child))
    | "true" ->
      let* kwd_true = dec_kwd_true ~comments child in
      Ok (T_true kwd_true)
    | "false" ->
      let* kwd_false = dec_kwd_false ~comments child in
      Ok (T_false kwd_false)
    | "null" ->
      let* kwd_null = dec_kwd_null ~comments child in
      Ok (T_null kwd_null)
    | "undefined" ->
      let* kwd_undefined = dec_kwd_undefined ~comments child in
      Ok (T_undefined kwd_undefined)
    | _ -> mk_err Literal_type node)

(* Index type query *)

and dec_index_type_query ?(comments = []) node : (kwd_keyof * primary_type, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Index_type_query node
  | _ ->
    let* kwd_keyof = first_child_named "keyof" node ~err:Keyof in
    let* kwd_keyof = dec_kwd_keyof ~comments kwd_keyof in
    let* type_node = child_ranked 1 node ~err:Type_expression in
    let* primary_type = dec_primary_type type_node in
    Ok (kwd_keyof, primary_type)

(* Type query *)

and dec_type_query ?(comments = []) node : (kwd_keyof * type_query, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Type_query node
  | _ ->
    let* kwd_typeof = first_child_named "typeof" node ~err:Keyof in
    let* kwd_typeof = dec_kwd_typeof ~comments kwd_typeof in
    let* child = child_ranked 1 node ~err:Type_query in
    let* type_query =
      match get_name child with
      | "subscript_expression" ->
        let* expression = dec_type_query_subscript_expression child in
        Ok (Typeof_subscript_expression expression)
      | "member_expression" ->
        let* expression = dec_type_query_member_expression child in
        Ok (Typeof_member_expression expression)
      | "call_expression" ->
        let* expression = dec_type_query_call_expression child in
        Ok (Typeof_call_expression expression)
      | "instantiation_expression" ->
        let* expression = dec_type_query_instantiation_expression child in
        Ok (Typeof_instantiation_expression expression)
      | "identifier" -> Ok (Typeof_identifier (dec_identifier child))
      | "this" ->
        let* kwd_this = dec_kwd_this child in
        Ok (Typeof_this kwd_this)
      | _ -> mk_err Type_query node
    in
    Ok (kwd_typeof, type_query)

and dec_type_query_subscript_expression ?(comments = []) node
    : (type_query_subscript_expression, _) result
  =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Type_query_subscript node
  | _ ->
    let* object_field = child_with_field "object" node ~err:Object_denotation in
    let* object_expr = dec_type_query_object ~comments object_field in
    let optional_chain = first_child_named_opt "?." node in
    let* optional = make_opt_res dec_sym_optional_chain optional_chain in
    let* sym_lbracket = first_child_named "[" node ~err:Left_bracket in
    let* opening = dec_sym_lbracket sym_lbracket in
    let* index_field = child_with_field "index" node ~err:Type_or_string_or_number in
    let* contents = dec_type_query_index index_field in
    let* sym_rbracket = first_child_named "]" node ~err:Right_bracket in
    let* closing = dec_sym_rbracket sym_rbracket in
    let region = !get_region node in
    let brackets = { opening; contents; closing } in
    let index = Brackets (Wrap.make brackets region) in
    Ok { object_expr; optional; index }

and dec_type_query_object ?(comments = []) node : (type_query_object, _) result =
  match get_name node with
  | "identifier" -> Ok (Type_query_object_identifier (dec_identifier ~comments node))
  | "this" ->
    let* kwd_this = dec_kwd_this ~comments node in
    Ok (Type_query_object_this kwd_this)
  | "subscript_expression" ->
    let* expression = dec_type_query_subscript_expression ~comments node in
    Ok (Type_query_object_subscript_expression expression)
  | "member_expression" ->
    let* expression = dec_type_query_member_expression ~comments node in
    Ok (Type_query_object_member_expression expression)
  | "call_expression" ->
    let* expression = dec_type_query_call_expression ~comments node in
    Ok (Type_query_object_call_expression expression)
  | _ -> mk_err Object_denotation node

and dec_type_query_index node : (type_query_index, _) result =
  match get_name node with
  | "predefined_type" ->
    let* type_expr = dec_predefined_type node in
    Ok (Type_query_index_predefined_type type_expr)
  | "string" -> Ok (Type_query_index_string (dec_string node))
  | "number" ->
    let* number = dec_number node in
    Ok (Type_query_index_number number)
  | _ -> mk_err Predefined_type node

and dec_type_query_member_expression ?(comments = []) node
    : (type_query_member_expression, _) result
  =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Type_query_member node
  | _ ->
    let* object_field = child_with_field "object" node ~err:Object_denotation in
    let* object_expr = dec_type_query_object ~comments object_field in
    let* property_field = child_with_field "property" node ~err:Property_identifier in
    let* property = dec_type_query_property property_field in
    let* selector = prev_sibling property_field ~err:Selector_or_optional_chain in
    let* selector = dec_query_selector selector in
    Ok { object_expr; selector; property }

and dec_query_selector node : (query_selector, _) result =
  match get_name node with
  | "." ->
    let* sym_dot = dec_sym_dot node in
    Ok (Query_selector_dot sym_dot)
  | "?." ->
    let* sym_optional_chain = dec_sym_optional_chain node in
    Ok (Query_selector_opt_chain sym_optional_chain)
  | _ -> mk_err Selector_or_optional_chain node

and dec_type_query_property node : (type_query_property, _) result =
  match get_name node with
  | "private_property_identifier" ->
    Ok (Type_query_property_private (dec_private_property_identifier node))
  | "property_identifier" -> Ok (Type_query_property_identifier (dec_identifier node))
  | _ -> mk_err Property_identifier node

and dec_type_query_call_expression ?(comments = []) node
    : (type_query_call_expression, _) result
  =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Type_query_call node
  | _ ->
    let* function_field = child_with_field "function" node ~err:Function_denotation in
    let* lambda = dec_type_query_call_function ~comments function_field in
    let* arguments_field = child_with_field "arguments" node ~err:Arguments in
    let* arguments = dec_type_query_call_arguments arguments_field in
    Ok ({ lambda; arguments } : type_query_call_expression)

and dec_type_query_call_function ?(comments = []) node
    : (type_query_call_function, _) result
  =
  match get_name node with
  | "import" ->
    let* kwd_import = dec_kwd_export ~comments node in
    Ok (Type_query_call_import kwd_import)
  | "identifier" -> Ok (Type_query_call_identifier (dec_identifier ~comments node))
  | "member_expression" ->
    let* expression = dec_type_query_member_expression ~comments node in
    Ok (Type_query_call_member_expression expression)
  | "subscript_expression" ->
    let* expression = dec_type_query_subscript_expression ~comments node in
    Ok (Type_query_call_subscript_expression expression)
  | _ -> mk_err Function_denotation node

and dec_type_query_call_arguments node : (type_query_call_arguments, _) result =
  dec_arguments node

and dec_type_query_instantiation_expression ?(comments = []) node
    : (type_query_instantiation_expression, _) result
  =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Type_query_instantiation node
  | _ ->
    let* function_field = child_with_field "function" node ~err:Function_denotation in
    let* lambda = dec_type_query_call_function ~comments function_field in
    let* type_arguments_field =
      child_with_field "type_arguments" node ~err:Type_arguments
    in
    let* type_arguments = dec_type_arguments type_arguments_field in
    Ok { lambda; type_arguments }

(* Flow maybe type

   flow_maybe_type: $ => prec.right(seq('?', $.primary_type))
 *)

and dec_flow_maybe_type ?(comments = []) node : (sym_qmark * primary_type, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Type node
  | _ ->
    let* sym_qmark = child_ranked 0 node ~err:Question_mark in
    let* sym_qmark = dec_sym_qmark ~comments sym_qmark in
    let* type_node = child_ranked 1 node ~err:Type_expression in
    let* primary_type = dec_primary_type type_node in
    Ok (sym_qmark, primary_type)

(* Tuple type *)

and dec_tuple_type ?comments node : (tuple_type, _) result =
  dec_list_in_brackets ?comments node dec_tuple_type_member ~err:Tuple_type

and dec_tuple_type_member ?(comments = []) node : (tuple_type_member, _) result =
  match get_name node with
  | "required_parameter" ->
    (* Alias *)
    let* parameter = wrap dec_tuple_parameter ~comments node in
    Ok (Tuple_parameter parameter)
  | "optional_parameter" ->
    (* Alias *)
    let* parameter = wrap dec_optional_tuple_parameter ~comments node in
    Ok (Tuple_optional_parameter parameter)
  | "optional_type" ->
    let* opt_type = wrap dec_optional_type ~comments node in
    Ok (Tuple_optional_type opt_type)
  | "rest_type" ->
    let* type_expr = wrap dec_rest_type ~comments node in
    Ok (Tuple_rest_type type_expr)
  | _ ->
    (* "type" is a hidden rule *)
    let* type_expr = dec_type ~comments node in
    Ok (Tuple_type type_expr)

and dec_tuple_parameter ?(comments = []) node : (tuple_parameter, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Tuple_parameter node
  | _ ->
    let* name_field = child_with_field "name" node ~err:Identifier_or_rest in
    let* name = dec_tuple_parameter_name ~comments name_field in
    let* type_field = child_with_field "type" node ~err:Type_annotation in
    let* annotation = dec_type_annotation type_field in
    Ok (name, annotation)

and dec_tuple_parameter_name ?(comments = []) node : (tuple_parameter_name, _) result =
  match get_name node with
  | "identifier" -> Ok (Tuple_parameter_ident (dec_identifier ~comments node))
  | "rest_pattern" ->
    let* pattern = wrap dec_rest_pattern ~comments node in
    Ok (Tuple_parameter_rest pattern)
  | _ -> mk_err Identifier_or_rest node

and dec_optional_tuple_parameter ?(comments = []) node
    : (optional_tuple_parameter, _) result
  =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Optional_tuple_parameter node
  | _ ->
    let* name_field = child_with_field "name" node ~err:Identifier in
    let name = dec_identifier ~comments name_field in
    let* sym_qmark = first_child_named "?" node ~err:Question_mark in
    let* sym_qmark = dec_sym_qmark sym_qmark in
    let* type_field = child_with_field "type" node ~err:Type_annotation in
    let* annotation = dec_type_annotation type_field in
    Ok (name, sym_qmark, annotation)

(* Optional type *)

and dec_optional_type ?(comments = []) node : (type_expr * sym_qmark, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Optional_type node
  | _ ->
    let* type_node = child_ranked 0 node ~err:Optional_type in
    let* type_expr = dec_type ~comments type_node in
    let* sym_qmark = child_ranked 1 node ~err:Question_mark in
    let* sym_qmark = dec_sym_qmark sym_qmark in
    Ok (type_expr, sym_qmark)

(* Rest type *)

and dec_rest_type ?(comments = []) node : (sym_ellipsis * type_expr, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Rest_type node
  | _ ->
    let* sym_ellipsis = first_child_named "..." node ~err:Ellipsis in
    let* sym_ellipsis = dec_sym_ellipsis ~comments sym_ellipsis in
    let* type_child = named_child_ranked 0 node ~err:Type_expression in
    let* type_expr = dec_type type_child in
    Ok (sym_ellipsis, type_expr)

(* Array type *)

and dec_array_type ?(comments = []) node : (array_type, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Array_type node
  | _ ->
    let* type_child = child_ranked 0 node ~err:Type_expression in
    let* type_expr = dec_primary_type ~comments type_child in
    let* sym_lbracket = first_child_named "[" node ~err:Left_bracket in
    let* sym_lbracket = dec_sym_lbracket sym_lbracket in
    let* sym_rbracket = first_child_named "]" node ~err:Right_bracket in
    let* sym_rbracket = dec_sym_rbracket sym_rbracket in
    Ok (type_expr, sym_lbracket, sym_rbracket)

(* Object type *)

and dec_object_type ?comments node : (object_type, _) result =
  dec_list_in_braces ?comments node dec_member_type ~err:Object_type

and dec_member_type ?(comments = []) node : (member_type, _) result =
  match get_name node with
  | "export_statement" ->
    let* statement = wrap dec_export_statement ~comments node in
    Ok (Export_statement statement)
  | "property_signature" ->
    let* signature = wrap dec_property_signature ~comments node in
    Ok (Property_signature signature)
  | "call_signature" ->
    let* signature = dec_call_signature ~comments node in
    Ok (Call_signature signature)
  | "construct_signature" ->
    let* signature = wrap dec_construct_signature ~comments node in
    Ok (Construct_signature signature)
  | "index_signature" ->
    let* signature = wrap dec_index_signature ~comments node in
    Ok (Index_signature signature)
  | "method_signature" ->
    let* signature = wrap dec_method_signature ~comments node in
    Ok (Method_signature signature)
  | _ -> mk_err Object_type_field node

(* Property signature *)

and dec_property_signature ?(comments = []) node : (property_signature, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Property_signature node
  | _ ->
    let accessibility_modifier = first_child_named_opt "accessibility_modifier" node in
    let* access = make_opt_res dec_accessibility_modifier accessibility_modifier in
    let* scope = dec_method_scope node in
    let* name_field = child_with_field "name" node ~err:Identifier in
    let* name = dec_property_name ~comments name_field in
    let sym_qmark = first_child_named_opt "?" node in
    let* sym_qmark = make_opt_res dec_sym_qmark sym_qmark in
    let type_field = child_with_field_opt "type" node in
    let* type_ = make_opt_res dec_type_annotation type_field in
    Ok { access; scope; name; sym_qmark; type_ }

(* Construct signature *)

and dec_construct_signature ?(comments = []) node : (construct_signature, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Construct_signature node
  | _ ->
    let kwd_abstract = first_child_named_opt "abstract" node in
    let* kwd_abstract = make_opt_res dec_kwd_abstract kwd_abstract in
    let* kwd_new = first_child_named "new" node ~err:New in
    let* kwd_new = dec_kwd_new ~comments kwd_new in
    let type_parameters_field = child_with_field_opt "type_parameters" node in
    let* type_parameters = make_opt_res dec_type_parameters type_parameters_field in
    let* parameters_field = child_with_field "parameters" node ~err:Parameters in
    let* parameters = dec_formal_parameters parameters_field in
    let type_field = child_with_field_opt "type" node in
    let* type_ = make_opt_res dec_type_annotation type_field in
    Ok { kwd_abstract; kwd_new; type_parameters; parameters; type_ }

(* Parenthesized type *)

and dec_parenthesized_type ?comments node : (type_expr parens, _) result =
  dec_parens ?comments node dec_type ~err:Parenthesized_type

(* Infer type *)

and dec_infer_type ?(comments = []) node : (infer_type, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Infer node
  | _ ->
    let* kwd_infer = first_child_named "infer" node ~err:Infer in
    let* kwd_infer = dec_kwd_infer ~comments kwd_infer in
    let* type_identifier_child =
      child_ranked 1 node ~err:Identifier (* name "type_identifier"? *)
    in
    let type_id = dec_type_identifier type_identifier_child in
    let* extends =
      match first_child_named_opt "extends" node with
      | None -> Ok None
      | Some kwd_extends ->
        let* kwd_extends = dec_kwd_extends kwd_extends in
        let* type_child = child_ranked 3 node ~err:Type_expression in
        let* type_expr = dec_type type_child in
        Ok (Some (kwd_extends, type_expr))
    in
    Ok { kwd_infer; type_id; extends }

(* Constructor type *)

and dec_constructor_type ?(comments = []) node : (constructor_type, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Constructor_type node
  | _ ->
    let kwd_abstract = first_child_named_opt "abstract" node in
    let* kwd_abstract = make_opt_res dec_kwd_abstract kwd_abstract in
    let* kwd_new = first_child_named "new" node ~err:New in
    let* kwd_new = dec_kwd_new kwd_new in
    let type_parameters_field = child_with_field_opt "type_parameters" node in
    let* type_parameters = make_opt_res dec_type_parameters type_parameters_field in
    let* parameters_field = child_with_field "parameters" node ~err:Parameters in
    let* parameters = dec_formal_parameters ~comments parameters_field in
    let* sym_arrow = first_child_named "=>" node ~err:Arrow in
    let* sym_arrow = dec_sym_arrow sym_arrow in
    let* type_field = child_with_field "type" node ~err:Type_expression in
    let* type_expr = dec_type type_field in
    Ok { kwd_abstract; kwd_new; type_parameters; parameters; sym_arrow; type_expr }

(* Function type *)

and dec_function_type ?(comments = []) node : (function_type, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Function_type node
  | _ ->
    let type_parameters_field = child_with_field_opt "type_parameters" node in
    let* type_parameters = make_opt_res dec_type_parameters type_parameters_field in
    let* parameters_field = child_with_field "parameters" node ~err:Parameters in
    let* parameters = dec_formal_parameters ~comments parameters_field in
    let* sym_arrow = first_child_named "=>" node ~err:Arrow in
    let* sym_arrow = dec_sym_arrow sym_arrow in
    let* return_type_field = child_with_field "return_type" node ~err:Type_expression in
    let* return_type = dec_return_type return_type_field in
    Ok { type_parameters; parameters; sym_arrow; return_type }

and dec_return_type node : (return_type, _) result =
  match get_name node with
  | "asserts" ->
    let* annotation = dec_asserts node in
    Ok (Return_asserts annotation)
  | "type_predicate" ->
    let* predicate = wrap dec_type_predicate node in
    Ok (Return_type_predicate predicate)
  | _ ->
    let* type_expr = dec_type node in
    Ok (Return_type type_expr)

(* Readonly type *)

and dec_readonly_type ?(comments = []) node : (readonly_type, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Readonly_type node
  | _ ->
    let* kwd_readonly = first_child_named "readonly" node ~err:Readonly in
    let* kwd_readonly = dec_kwd_readonly ~comments kwd_readonly in
    let* type_child = child_ranked 1 node ~err:Type_expression in
    let* type_expr = dec_type type_child in
    Ok (kwd_readonly, type_expr)

(* Generic type *)

and dec_generic_type ?(comments = []) node : (generic_type, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Generic_type node
  | _ ->
    let comments = comments @ prev_comments node in
    let* name_field = child_with_field "name" node ~err:Type_identifier_or_path in
    let* generic_name = dec_generic_name ~comments name_field in
    let* type_arguments_field =
      child_with_field "type_arguments" node ~err:Type_arguments
    in
    let* type_arguments = dec_type_arguments type_arguments_field in
    Ok (generic_name, type_arguments)

and dec_generic_name ?(comments = []) node : (generic_name, _) result =
  match get_name node with
  | "type_identifier" ->
    let identifier = dec_type_identifier ~comments node in
    Ok (Generic_type identifier)
  | "nested_type_identifier" ->
    let* nested = wrap dec_nested_type_identifier ~comments node in
    Ok (Generic_nested nested)
  | _ -> mk_err Type_identifier_or_path node

(* Decoding the CST *)

let dec_program ?(debug_arg = false) ~filename ~file (map : Loc_map.t) node
    : (Ast.t, _) result
  =
  (* Setting up the extraction of source regions *)
  let () = get_region := Ts_wrap.get_region filename map in
  (* Setting the input as a top-level string buffer *)
  let () = Buffer.add_string !input file in
  (* Setting debug mode *)
  let () = debug := debug_arg in
  (* Decoding the CST into an AST *)
  dec_statements node

let dec_standalone_expression ~file map node : (Ast.expression, _) result =
  (* Setting up the extraction of source regions *)
  let () = get_region := Ts_wrap.get_region "" map in
  (* Setting the input as a top-level string buffer *)
  let () = Buffer.add_string !input file in
  (* Decoding the CST into an AST *)
  let* ast = dec_statements node in
  match ast with
  | None -> Decode_err.(make (Region.min ~file:"") No_single_expression)
  | Some stmts ->
    (match stmts#payload with
    | _ :: stmt2 :: _ ->
      Decode_err.(make (region_of_statement stmt2) No_single_expression)
    | Nonempty_list.[ stmt ] ->
      (match stmt with
      | S_expression_statement expr_stmt ->
        (match expr_stmt#payload with
        | _ :: expr2 :: _ ->
          Decode_err.(make (region_of_expression expr2) No_single_expression)
        | Nonempty_list.[ expr ] -> Ok expr)
      | _ -> Decode_err.(make (region_of_statement stmt) No_single_expression)))

let dec_standalone_type_expr map node : (Ast.type_expr, _) result =
  (* Setting up the extraction of source regions *)
  let () = get_region := Ts_wrap.get_region "" map in
  (* Decoding the CST into an AST *)
  let* ast = dec_statements node in
  match ast with
  | None -> Decode_err.(make (Region.min ~file:"") No_single_type_expr)
  | Some stmts ->
    (match stmts#payload with
    | _ :: stmt2 :: _ -> Decode_err.(make (region_of_statement stmt2) No_single_type_expr)
    | Nonempty_list.[ stmt ] ->
      (match stmt with
      | S_declaration_statement (D_type_alias_declaration decl) ->
        let Ast.{ kwd_type = _; name = _; type_parameters = _; sym_equal = _; type_expr } =
          decl#payload
        in
        Ok type_expr
      | _ -> Decode_err.(make (region_of_statement stmt) No_single_type_expr)))
