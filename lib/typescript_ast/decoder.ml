(* Decoding the tree-sitter CST for TypeScript *)

module Region = Simple_utils.Region
module Wrap = Lexing_shared.Wrap
module Ts_wrap = Typescript_ast.Ts_wrap
module Lexeme = Typescript_ast.Lexeme
module Ast = Typescript_ast.Ast
module Number = Typescript_ast.Number
open Core
open Typescript_ast.Ts_wrap
open Ast

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
  ref (fun _ -> failwith "Internal error: Decoder.get_region")

(* Handling results and failing in case of error *)

let ensure_Ok node = function
  | Result.Ok ok -> ok
  | Error msg -> failwith ((!get_region node)#compact `Byte ^ "\n" ^ msg)

(* Decoding literals *)

let decode_comments ?(comments = []) node : Wrap.comment list =
  let f node =
    let region = !get_region node in
    let value = Lexeme.read region in
    Wrap.Block Region.{ value; region }
  in
  List.map ~f (comments @ prev_comments node)

let make_node ?comments node : string wrap =
  let region = !get_region node in
  let root = Lexeme.read region
  and comments = decode_comments ?comments node in
  Wrap.make ~comments root region

let make_kwd ?comments node : keyword = make_node ?comments node
let make_sym ?comments node : symbol = make_node ?comments node
let dec_identifier ?comments node : identifier = make_node ?comments node
let dec_string ?comments node : string_literal = make_node ?comments node

let dec_number ?(comments = []) node : number =
  let region = !get_region node in
  let lexeme = Lexeme.read region in
  let lexbuf = Lexing.from_string lexeme
  and comments = decode_comments ~comments node in
  Number.scan comments region lexbuf

(* Optional nodes *)

let make_opt decoder node = Option.map ~f:decoder node

(* Decoding children of the same type *)

let list_of_children ?(comments = []) decoder children : 'a list =
  let f raw_child = List.cons (decoder ?comments:None raw_child) in
  match children with
  | [] -> []
  | fst_raw_child :: siblings ->
    let fst_child = decoder ?comments:(Some comments) fst_raw_child in
    fst_child :: List.fold_right ~f ~init:[] siblings

let ne_list_opt_of_children ?(comments = []) decoder children : 'a ne_list option =
  let f raw_child = List.cons (decoder ?comments:None raw_child) in
  match children with
  | [] -> None
  | fst_raw_child :: siblings ->
    let fst_child = decoder ?comments:(Some comments) fst_raw_child in
    Some Nonempty_list.(fst_child :: List.fold_right ~f ~init:[] siblings)

let ne_list_of_children ?(comments = []) decoder children : ('a ne_list, _) result =
  match ne_list_opt_of_children ~comments decoder children with
  | None -> Error "Expected at least one child."
  | Some ne_list -> Ok ne_list

(*
let wrap_children ?(comments = []) decoder node : 'a ne_list wrap option =
  let f raw_child = List.cons (decoder ?comments:None raw_child) in
  match collect_named_children node with
  | [] -> None
  | fst_raw_child :: siblings ->
    let fst_child = decoder ?comments:(Some comments) fst_raw_child in
    let stmts = Nonempty_list.(fst_child :: List.fold_right ~f ~init:[] siblings) in
    let region = !get_region node in
    Some (Wrap.make stmts region)
*)

(* Decoding enclosed unique child *)

let decode_enclosed ?(comments = []) node decoder opening closing : 'a enclosed =
  ensure_Ok node
  @@
  let comments = comments @ prev_comments node in
  let* opening = first_child_named opening node in
  let* closing = first_child_named closing node in
  let* child = (* We assume one child *) child_ranked 1 node in
  Ok
    { opening = make_sym ~comments opening
    ; contents = decoder child
    ; closing = make_sym closing
    }

let decode_braces ?comments node decoder : 'a braces =
  Braces (decode_enclosed ?comments node decoder "{" "}")

let decode_chevrons ?comments node decoder : 'a chevrons =
  Chevrons (decode_enclosed ?comments node decoder "<" ">")

let decode_brackets ?comments node decoder : 'a brackets =
  Brackets (decode_enclosed ?comments node decoder "[" "]")

let decode_parens ?comments node decoder : 'a parens =
  Parens (decode_enclosed ?comments node decoder "(" ")")

(* Decoding enclosed lists *)

let decode_enclosed_list ?(comments = []) node decoder opening closing : 'a list enclosed =
  ensure_Ok node
  @@
  let comments = comments @ prev_comments node in
  let* opening = first_child_named opening node in
  let* closing = first_child_named closing node in
  let clauses = collect_named_children node in
  Ok
    { opening = make_sym ~comments opening
    ; contents = list_of_children decoder clauses
    ; closing = make_sym closing
    }

let decode_list_in_braces ?comments node decoder : 'a list braces =
  Braces (decode_enclosed_list ?comments node decoder "{" "}")

let decode_list_in_chevrons ?comments node decoder : 'a list chevrons =
  Chevrons (decode_enclosed_list ?comments node decoder "<" ">")

let decode_list_in_brackets ?comments node decoder : 'a list brackets =
  Brackets (decode_enclosed_list ?comments node decoder "[" "]")

let decode_list_in_parens ?comments node decoder : 'a list parens =
  Parens (decode_enclosed_list ?comments node decoder "(" ")")

(* Decoding enclosed non-empty lists *)

let decode_enclosed_ne_list ?(comments = []) node decoder opening closing
    : 'a ne_list enclosed
  =
  ensure_Ok node
  @@
  let comments = comments @ prev_comments node in
  let* opening = first_child_named opening node in
  let* closing = first_child_named closing node in
  let clauses = collect_named_children node in
  let* contents = ne_list_of_children decoder clauses in
  Ok { opening = make_sym ~comments opening; contents; closing = make_sym closing }

let decode_ne_list_in_braces ?comments node decoder : 'a ne_list braces =
  Braces (decode_enclosed_ne_list ?comments node decoder "{" "}")

let decode_ne_list_in_chevrons ?comments node decoder : 'a ne_list chevrons =
  Chevrons (decode_enclosed_ne_list ?comments node decoder "<" ">")

let decode_ne_list_in_brackets ?comments node decoder : 'a ne_list brackets =
  Brackets (decode_enclosed_ne_list ?comments node decoder "[" "]")

let decode_ne_list_in_parens ?comments node decoder : 'a ne_list parens =
  Parens (decode_enclosed_ne_list ?comments node decoder "(" ")")

(* Decoding the CST *)

let rec dec_program file map node =
  (* Opening a read channel for lexemes *)
  let () = Lexeme.open_input ~file in
  (* Setting up the extracting of source regions *)
  let () = get_region := Ts_wrap.get_region file map in
  (* Decoding the CST into an AST *)
  let ast = dec_statements node in
  (* Closing the input channel for reading lexemes *)
  let () = Lexeme.close_input () in
  ast

(* STATEMENTS

   The JavaScript tree-sitter grammar has the non-terminal
   "statement" be a supertype, that is, a hidden rule. *)

and dec_statements ?(comments = []) node : statements =
  let children = collect_named_children node in
  ne_list_opt_of_children ~comments dec_statement children

and dec_statement ?(comments = []) node : statement =
  match get_name node with
  | "export_statement" -> S_export_statement (dec_export_statement ~comments node)
  | "import_statement" -> S_import_statement (dec_import_statement ~comments node)
  | "debugger_statement" -> S_debugger_statement (make_kwd ~comments node)
  | "expression_statement" ->
    S_expression_statement (dec_expression_statement ~comments node)
  | "statement_block" -> S_statement_block (dec_statement_block ~comments node)
  | "if_statement" -> S_if_statement (dec_if_statement ~comments node)
  | "switch_statement" -> S_switch_statement (dec_switch_statement node)
  | "for_statement" -> S_for_statement (dec_for_statement node)
  | "for_in_statement" -> S_for_in_statement (dec_for_in_statement node)
  | "while_statement" -> S_while_statement (dec_while_statement node)
  | "do_statement" -> S_do_statement (dec_do_statement ~comments node)
  | "try_statement" -> S_try_statement (dec_try_statement node)
  | "with_statement" -> S_with_statement (dec_with_statement node)
  | "break_statement" -> S_break_statement (dec_break_statement node)
  | "continue_statement" -> S_continue_statement (dec_continue_statement node)
  | "return_statement" -> S_return_statement (dec_return_statement node)
  | "throw_statement" -> S_throw_statement (dec_throw_statement node)
  | "empty_statement" -> S_empty_statement (!get_region node)
  (* Inlining declarations cases (hidden rule) *)
  | "function_declaration" ->
    S_declaration (D_function_declaration (dec_function_declaration ~comments node))
  | "generator_function_declaration" ->
    S_declaration
      (D_generator_function_declaration (dec_generator_function_declaration node))
  | "class_declaration" ->
    S_declaration (D_class_declaration (dec_class_declaration ~comments node))
  | "lexical_declaration" ->
    S_declaration (D_lexical_declaration (dec_lexical_declaration ~comments node))
  | "variable_declaration" ->
    S_declaration (D_variable_declaration (dec_variable_declaration ~comments node))
  | "function_signature" ->
    S_declaration (D_function_signature (dec_function_signature node))
  | "abstract_class_declaration" ->
    S_declaration (D_abstract_class_declaration (dec_abstract_class_declaration node))
  | "module" -> S_declaration (D_module (dec_module node))
  | "internal_module" ->
    S_declaration (D_internal_module (dec_internal_module ~comments node))
  | "type_alias_declaration" ->
    S_declaration (D_type_alias_declaration (dec_type_alias_declaration node))
  | "enum_declaration" -> S_declaration (D_enum_declaration (dec_enum_declaration node))
  | "interface_declaration" ->
    S_declaration (D_interface_declaration (dec_interface_declaration node))
  | "import_alias" -> S_declaration (D_import_alias (dec_import_alias node))
  | "ambient_declaration" ->
    S_declaration (D_ambient_declaration (dec_ambient_declaration node))
  | s -> failwith ("dec_statement: " ^ s)

(* Export statement *)

and dec_export_statement ?(comments = []) node : export_statement =
  ensure_Ok node
  @@
  let comments = comments @ prev_comments node in
  let* kwd_export = first_child_named "export" node in
  let* after_export = next_sibling kwd_export in
  let kwd_export = make_kwd ~comments kwd_export in
  let* export_kind =
    match get_name after_export with
    | "*" ->
      let* kwd_from = first_child_named "from" node in
      Ok (Export_from (dec_from_clause node kwd_from))
    | "namespace_export" ->
      let* kwd_from = first_child_named "from" node in
      let namespace_export = dec_namespace_export after_export in
      Ok (Export_as (namespace_export, dec_from_clause node kwd_from))
    | "export_clause" ->
      let kwd_from = first_child_named_opt "from" node in
      let export_clause = dec_export_clause after_export
      and from_clause = make_opt (dec_from_clause node) kwd_from in
      Ok (Export_clause (export_clause, from_clause))
    | "default" -> Ok (dec_export_default after_export node)
    | "type" -> Ok (Export_type (dec_export_type after_export node))
    | "=" ->
      let* expression = next_sibling after_export in
      let expression = dec_expression expression in
      Ok (Export_equal (make_sym after_export, expression))
    | "as" ->
      let* kwd_namespace = first_child_named "namespace" node in
      let* identifier = first_child_named "identifier" node in
      Ok (Export_as_namespace (make_kwd kwd_namespace, dec_identifier identifier))
    | _ -> Ok (Export_declaration (dec_export_declaration after_export node))
  in
  Ok { kwd_export; export_kind }

and dec_export_type after_export node : export_type =
  ensure_Ok node
  @@ let* export_clause = next_sibling after_export in
     let kwd_type = make_kwd after_export in
     let export_clause = dec_export_clause export_clause in
     let kwd_from = first_child_named_opt "from" node in
     let from_clause = make_opt (dec_from_clause node) kwd_from in
     Ok { kwd_type; export_clause; from_clause }

and dec_export_declaration after_export node : declaration decorated =
  ensure_Ok node
  @@
  let decorators = children_named "decorator" node in
  let declaration = dec_declaration after_export in
  Ok (dec_decorated decorators declaration)

and dec_decorated : 'a. ts_forest -> 'a -> 'a decorated =
 fun decorators decorated ->
  let decorators = ne_list_opt_of_children dec_decorator decorators in
  { decorators; decorated }

and dec_export_clause node : export_clause =
  decode_list_in_braces node dec_export_specifier

and dec_export_specifier ?(comments = []) node : export_specifier =
  ensure_Ok node
  @@
  let comments = comments @ prev_comments node in
  let* name_field = child_with_field "name" node in
  let name = dec_module_export_name ~comments name_field in
  let alias_field = child_with_field_opt "alias" node in
  let alias = make_opt dec_module_export_name alias_field in
  let* alias =
    match alias with
    | None -> Ok None
    | Some alias ->
      let* kwd_as = first_child_named "as" node in
      Ok (Some (make_kwd kwd_as, alias))
  in
  Ok ({ name; alias } : export_specifier)

and dec_module_export_name ?(comments = []) node : module_export_name =
  match get_name node with
  | "identifier" -> Export_ident (dec_identifier ~comments node)
  | "string" -> Export_string (dec_string ~comments node)
  | s -> failwith ("dec_module_export_name: " ^ s)

and dec_from_clause node kwd_from : from_clause =
  ensure_Ok node
  @@ let* source_field = child_with_field "source" node in
     Ok (make_kwd kwd_from, dec_string source_field)

and dec_namespace_export ?(comments = []) node : namespace_export =
  ensure_Ok node
  @@
  let comments = comments @ prev_comments node in
  let* sym_star = first_child_named "*" node in
  let sym_star = make_sym ~comments sym_star in
  let* kwd_as = first_child_named "as" node in
  let* module_export_name = next_sibling kwd_as in
  let kwd_as = make_kwd kwd_as in
  let namespace_name = dec_module_export_name module_export_name in
  Ok { sym_star; kwd_as; namespace_name }

and dec_export_default after_export node : export_kind =
  ensure_Ok node
  @@
  let decorators = children_named "decorator" node in
  let kwd_default = make_kwd after_export in
  match child_with_field_opt "declaration" node with
  | None ->
    let* value_field = child_with_field "value" node in
    let contents = kwd_default, dec_expression value_field in
    Ok (Export_default_expression (dec_decorated decorators contents))
  | Some declaration ->
    let contents = kwd_default, dec_declaration declaration in
    Ok (Export_default_declaration (dec_decorated decorators contents))

(* Import statement *)

and dec_import_statement ?(comments = []) node : import_statement =
  ensure_Ok node
  @@
  let comments = comments @ prev_comments node in
  let* kwd_import = first_child_named "import" node in
  let kwd_import = make_kwd ~comments kwd_import in
  let import_kind =
    match first_child_named_opt "type" node with
    | Some kwd_type -> Some (Import_type (make_kwd kwd_type))
    | None ->
      (match first_child_named_opt "typeof" node with
      | None -> None
      | Some kwd_typeof -> Some (Import_typeof (make_kwd kwd_typeof)))
  in
  let import_attribute = first_child_named_opt "import_attribute" node in
  let import_attribute = make_opt dec_import_attribute import_attribute in
  let* (import : import) =
    match first_child_named_opt "import_clause" node with
    | Some import_clause ->
      let* kwd_from = first_child_named "from" node in
      let import_clause = dec_import_clause import_clause in
      let from_clause = dec_from_clause node kwd_from in
      Ok (Import_clause (import_clause, from_clause))
    | None ->
      (match first_child_named_opt "import_require_clause" node with
      | Some clause -> Ok (Import_require_clause (dec_import_require_clause clause))
      | None ->
        let* source_field = child_with_field "source" node in
        Ok (Import_source (dec_string source_field)))
  in
  Ok { kwd_import; import_kind; import; import_attribute }

and dec_import_clause ?(comments = []) node : import_clause =
  ensure_Ok node
  @@
  let comments = comments @ prev_comments node in
  let* fst_child = child_ranked 0 node in
  match get_name fst_child with
  | "namespace_import" ->
    let namespace_import = dec_namespace_import ~comments fst_child in
    Ok (Import_namespace namespace_import : import_clause)
  | "named_imports" -> Ok (Import_named (dec_named_imports ~comments fst_child))
  | "identifier" ->
    let ident = dec_identifier ~comments fst_child in
    let* from =
      match next_sibling_opt fst_child with
      | None -> Ok None
      | Some comma ->
        let* next = next_sibling comma in
        Ok (Some (dec_namespace_or_named_imports next))
    in
    Ok (Import_ident (ident, from))
  | s -> failwith ("dec_import_clause: " ^ s)

and dec_namespace_or_named_imports node : namespace_or_named_imports =
  match get_name node with
  | "namespace_import" -> Import_namespace (dec_namespace_import node)
  | "named_imports" -> Import_named (dec_named_imports node)
  | s -> failwith ("dec_namespace_or_named_imports: " ^ s)

and dec_namespace_import ?(comments = []) node : namespace_import =
  ensure_Ok node
  @@
  let comments = comments @ prev_comments node in
  let* sym_star = first_child_named "*" node in
  let sym_star = make_sym ~comments sym_star in
  let* kwd_as = first_child_named "as" node in
  let* identifier = next_sibling kwd_as in
  let kwd_as = make_kwd kwd_as in
  let identifier = dec_identifier identifier in
  Ok { sym_star; kwd_as; identifier }

and dec_named_imports ?(comments = []) node : named_imports =
  decode_list_in_braces ~comments node dec_import_specifier

and dec_import_specifier ?(comments = []) node : import_specifier =
  ensure_Ok node
  @@
  let comments = comments @ prev_comments node in
  let import_kind : import_kind option =
    match first_child_named_opt "type" node with
    | Some kwd_type -> Some (Import_type (make_kwd ~comments kwd_type))
    | None ->
      (match first_child_named_opt "typeof" node with
      | None -> None
      | Some kwd_typeof -> Some (Import_typeof (make_kwd ~comments kwd_typeof)))
  in
  let snd_child_comments =
    match import_kind with
    | None -> comments
    | Some _ -> []
  in
  let* name_field = child_with_field "name" node in
  let* (import_specifier' : import_specifier') =
    match child_with_field_opt "alias" node with
    | None ->
      Ok (Import_spec_name (dec_identifier ~comments:snd_child_comments name_field))
    | Some alias_field ->
      let* kwd_as = first_child_named "as" node in
      let name = dec_module_export_name ~comments:snd_child_comments name_field in
      let kwd_as = make_kwd kwd_as in
      let alias = dec_identifier alias_field in
      Ok (Import_spec_alias { name; kwd_as; alias })
  in
  Ok (import_kind, import_specifier')

and dec_import_require_clause ?(comments = []) node : import_require_clause =
  ensure_Ok node
  @@
  let comments = comments @ prev_comments node in
  let* identifier = child_ranked 0 node in
  let ident = dec_identifier ~comments identifier in
  let* sym_equal = first_child_named "=" node in
  let sym_equal = make_sym sym_equal in
  let* kwd_require = first_child_named "require" node in
  let kwd_require = make_kwd kwd_require in
  let* sym_lpar = first_child_named "(" node in
  let sym_lpar = make_sym sym_lpar in
  let* source_field = child_with_field "source" node in
  let source = dec_string source_field in
  let* sym_rpar = first_child_named ")" node in
  let sym_rpar = make_sym sym_rpar in
  Ok { ident; sym_equal; kwd_require; sym_lpar; source; sym_rpar }

and dec_import_attribute node : import_attribute =
  ensure_Ok node
  @@ let* kind_node = child_ranked 0 node in
     let* object_node = child_ranked 1 node in
     match get_name kind_node with
     | "with" -> Ok (Import_with (make_kwd kind_node, dec_object object_node))
     | "assert" -> Ok (Import_assert (make_kwd kind_node, dec_object object_node))
     | s -> failwith ("dec_import_attribute: " ^ s)

(* Expression statements

   {@js[
    expression_statement: $ => seq($._expressions, $._semicolon),
    _expressions: $ => choice($.expression, $.sequence_expression),
    sequence_expression: $ => prec.right(commaSep1($.expression))
   ]}

   See [dc_expression]. *)

and dec_expression_statement ?(comments = []) node : expression_statement =
  dec_expressions ~comments node

and dec_expressions ?(comments = []) (node : ts_tree) : expressions =
  match get_name node with
  | "sequence_expression" -> dec_sequence_expression ~comments node
  | _ -> [ dec_expression ~comments node ]

(* Statement blocks *)

and dec_statement_block ?(comments = []) node : statement_block =
  dec_statements ~comments node

(* If statement *)

and dec_if_statement ?(comments = []) node : if_statement =
  ensure_Ok node
  @@ let* kwd_if = first_child_named "if" node in
     let kwd_if = make_kwd ~comments kwd_if in
     let* condition_field = child_with_field "condition" node in
     let condition = dec_parenthesized_expression condition_field in
     let* consequence_field = child_with_field "consequence" node in
     let consequence = dec_statement consequence_field in
     let alternative_field = child_with_field_opt "alternative" node in
     let alternative = make_opt dec_else_clause alternative_field in
     Ok { kwd_if; condition; consequence; alternative }

and dec_else_clause ?(comments = []) node : keyword * statement =
  ensure_Ok node
  @@
  let comments = comments @ prev_comments node in
  let* kwd_else = first_child_named "else" node in
  let* statement = next_sibling kwd_else in
  Ok (make_kwd ~comments kwd_else, dec_statement statement)

(* Switch statement *)

and dec_switch_statement node : switch_statement =
  ensure_Ok node
  @@ let* kwd_switch = first_child_named "switch" node in
     let kwd_switch = make_kwd kwd_switch in
     let* value_field = child_with_field "value" node in
     let value = dec_parenthesized_expression value_field in
     let* body_field = child_with_field "body" node in
     let body = dec_switch_body body_field in
     Ok { kwd_switch; value; body }

and dec_switch_body node : switch_body =
  let decode ?comments node =
    match get_name node with
    | "switch_case" -> Switch_case (dec_switch_case ?comments node)
    | "switch_default" -> Switch_default (dec_switch_default ?comments node)
    | s -> failwith ("dec_switch_body: " ^ s)
  in
  decode_list_in_braces node decode

and dec_switch_case ?(comments = []) node : switch_case =
  ensure_Ok node
  @@
  let comments = comments @ prev_comments node in
  let* kwd_case = first_child_named "case" node in
  let kwd_case = make_kwd ~comments kwd_case in
  let* value_field = child_with_field "value" node in
  let value = dec_expressions value_field in
  let children = collect_children node in
  let stmt_children = skip_until_colon children in
  let body = list_of_children dec_statement stmt_children in
  Ok { kwd_case; value; body }

and dec_switch_default ?(comments = []) node : switch_default =
  ensure_Ok node
  @@
  let comments = comments @ prev_comments node in
  let* kwd_default = first_child_named "default" node in
  let kwd_default = make_kwd ~comments kwd_default in
  let statements = collect_named_children node in
  let statements = list_of_children dec_statement statements in
  Ok { kwd_default; statements }

(* For statement *)

and dec_for_statement node : for_statement =
  ensure_Ok node
  @@ let* kwd_for = first_child_named "for" node in
     let kwd_for = make_kwd kwd_for in
     let* sym_lpar = first_child_named "(" node in
     let sym_lpar = make_sym sym_lpar in
     let* initializer_field = child_with_field "initializer" node in
     let initializer_ = decode_for_initializer initializer_field in
     let* condition_field = child_with_field "condition" node in
     let condition = decode_for_condition condition_field in
     let increment_field = child_with_field_opt "increment" node in
     let increment = make_opt dec_expressions increment_field in
     let* sym_rpar = first_child_named ")" node in
     let sym_rpar = make_sym sym_rpar in
     let* body_field = child_with_field "body" node in
     let body = dec_statement body_field in
     Ok { kwd_for; sym_lpar; initializer_; condition; increment; sym_rpar; body }

and decode_for_initializer node : for_initializer =
  match get_name node with
  | "lexical_declaration" -> For_lexical_declaration (dec_lexical_declaration node)
  | "variable_declaration" -> For_variable_declaration (dec_variable_declaration node)
  | "expression_statement" -> For_expression_statement (dec_expression_statement node)
  | "empty_statement" -> For_empty_statement (!get_region node)
  | s -> failwith ("decode_for_initializer: " ^ s)

and decode_for_condition node : for_condition =
  match get_name node with
  | "expression_statement" -> For_condition_expression (dec_expression_statement node)
  | "empty_statement" -> For_condition_empty (!get_region node)
  | s -> failwith ("decode_for_condition: " ^ s)

(* For-in statement *)

and dec_for_in_statement node : for_in_statement =
  ensure_Ok node
  @@ let* kwd_for = first_child_named "for" node in
     let kwd_for = make_kwd kwd_for in
     let kwd_await = first_child_named_opt "await" node in
     let kwd_await = make_opt make_kwd kwd_await in
     let* sym_lpar = first_child_named "(" node in
     let sym_lpar = make_sym sym_lpar in
     let kind_field = child_with_field_opt "kind" node in
     let* left_field = child_with_field "left" node in
     let* sym_rpar = first_child_named ")" node in
     let sym_rpar = make_sym sym_rpar in
     let* body_field = child_with_field "body" node in
     let body = dec_statement body_field in
     let* operator_field = child_with_field "operator" node in
     let operator = decode_for_operator operator_field in
     let* right_field = child_with_field "right" node in
     let collection = dec_expressions right_field in
     let range : for_range =
       match kind_field with
       | None ->
         (match get_name left_field with
         | "parenthesized_expression" ->
           For_in_parenthesized (dec_parenthesized_expression node)
         | _ -> For_in_expression (dec_lhs_expression node))
       | Some kind_field ->
         let keyword = make_kwd kind_field
         and variable =
           match get_name left_field with
           | "identifier" -> For_in_ident (dec_identifier left_field)
           | _ -> For_in_pattern (dec_destructuring_pattern left_field)
         in
         (match get_name kind_field with
         | "var" ->
           let value_field = child_with_field_opt "value" node in
           let default = make_opt dec_expression value_field in
           For_in_var { kwd_var = keyword; variable; default }
         | "let" -> For_in_let (keyword, variable)
         | "const" -> For_in_const (keyword, variable)
         | s -> failwith ("dec_for_in_statement/range:" ^ s))
     in
     let for_header : for_header = { range; operator; collection } in
     Ok { kwd_for; kwd_await; sym_lpar; for_header; sym_rpar; body }

and decode_for_operator node : for_operator =
  match get_name node with
  | "in" -> In (make_kwd node)
  | "of" -> Of (make_kwd node)
  | s -> failwith ("decode_for_operator: " ^ s)

(* While statement *)

and dec_while_statement node : while_statement =
  ensure_Ok node
  @@ let* kwd_while = first_child_named "while" node in
     let kwd_while = make_kwd kwd_while in
     let* condition_field = child_with_field "condition" node in
     let condition = dec_parenthesized_expression condition_field in
     let* body_field = child_with_field "body" node in
     let body = dec_statement body_field in
     Ok { kwd_while; condition; body }

(* Do statement *)

and dec_do_statement ?(comments = []) node : do_statement =
  ensure_Ok node
  @@ let* kwd_do = first_child_named "do" node in
     let kwd_do = make_kwd ~comments kwd_do in
     let* body_field = child_with_field "body" node in
     let body = dec_statement body_field in
     let* kwd_while = first_child_named "while" node in
     let kwd_while = make_kwd kwd_while in
     let* condition_field = child_with_field "condition" node in
     let condition = dec_parenthesized_expression condition_field in
     Ok { kwd_do; body; kwd_while; condition }

(* Try statement *)

and dec_try_statement node : try_statement =
  ensure_Ok node
  @@ let* kwd_try = first_child_named "try" node in
     let kwd_try = make_kwd kwd_try in
     let* body_field = child_with_field "body" node in
     let body = dec_statement_block body_field in
     let handler_field = child_with_field_opt "handler" node in
     let handler = make_opt dec_catch_clause handler_field in
     let finalizer_field = child_with_field_opt "finalizer" node in
     let finalizer = make_opt dec_finally_clause finalizer_field in
     Ok { kwd_try; body; handler; finalizer }

and dec_catch_clause node : catch_clause =
  ensure_Ok node
  @@ let* kwd_catch = first_child_named "catch" node in
     let kwd_catch = make_kwd kwd_catch in
     let parameter_field = child_with_field_opt "parameter" node in
     let parameter = make_opt (dec_catch_parameter node) parameter_field in
     let* body_field = child_with_field "body" node in
     let body = dec_statement_block body_field in
     Ok { kwd_catch; parameter; body }

and dec_catch_parameter node param : catch_parameter =
  ensure_Ok node
  @@
  let catch_parameter = dec_catch_parameter_kind param in
  let* sym_lpar = first_child_named "(" node in
  let sym_lpar = make_sym sym_lpar in
  let type_field = child_with_field_opt "type" node in
  let type_opt = make_opt dec_type_annotation type_field in
  let* sym_rpar = first_child_named ")" node in
  let sym_rpar = make_sym sym_rpar in
  Ok { sym_lpar; catch_parameter; type_opt; sym_rpar }

and dec_catch_parameter_kind node : catch_parameter_kind =
  match get_name node with
  | "identifier" -> Catch_identifier (dec_identifier node)
  | "object_pattern" -> Catch_object_pattern (dec_object_pattern node)
  | "array_pattern" -> Catch_array_pattern (dec_array_pattern node)
  | s -> failwith ("dec_catch_parameter_kind: " ^ s)

and dec_type_annotation node : type_annotation =
  ensure_Ok node
  @@ let* sym_colon = first_child_named ":" node in
     let* type_child = named_child_ranked 0 node in
     Ok (make_sym sym_colon, dec_type type_child)

and dec_finally_clause node : finally_clause = dec_statement_block node

(* With statement *)

and dec_with_statement node : with_statement =
  ensure_Ok node
  @@ let* kwd_with = first_child_named "with" node in
     let kwd_with = make_kwd kwd_with in
     let* object_field = child_with_field "object" node in
     let object_expr = dec_parenthesized_expression object_field in
     let* body_field = child_with_field "body" node in
     let body = dec_statement body_field in
     Ok { kwd_with; object_expr; body }

(* Break statement *)

and dec_break_statement node : break_statement =
  ensure_Ok node
  @@ let* kwd_break = first_child_named "break" node in
     let kwd_break = make_kwd kwd_break in
     let label_field = child_with_field_opt "label" node in
     let stmt_id = make_opt dec_identifier label_field in
     Ok { kwd_break; stmt_id }

(* Continue statement *)

and dec_continue_statement node : continue_statement =
  ensure_Ok node
  @@ let* kwd_continue = first_child_named "continue" node in
     let kwd_continue = make_kwd kwd_continue in
     let label_field = child_with_field_opt "label" node in
     let stmt_id = make_opt dec_identifier label_field in
     Ok { kwd_continue; stmt_id }

(* Return statement *)

and dec_return_statement node : return_statement =
  ensure_Ok node
  @@ let* kwd_return = first_child_named "return" node in
     let expr = child_ranked_opt 1 node in
     Ok { kwd_return = make_kwd kwd_return; expressions = make_opt dec_expressions expr }

(* Throw statement *)

and dec_throw_statement node : throw_statement =
  ensure_Ok node
  @@ let* kwd_throw = first_child_named "throw" node in
     let* expr = child_ranked 1 node in
     Ok { kwd_throw = make_kwd kwd_throw; expressions = dec_expressions expr }

(* DECLARATION

   The JavaScript tree-sitter grammar has the non-terminal
   "declaration" be a supertype, that is, a hidden rule. *)

and dec_declaration ?(comments = []) node : declaration =
  let comments = comments @ prev_comments node in
  match get_name node with
  | "function_declaration" ->
    D_function_declaration (dec_function_declaration ~comments node)
  | "generator_function_declaration" ->
    D_generator_function_declaration (dec_generator_function_declaration ~comments node)
  | "class_declaration" -> D_class_declaration (dec_class_declaration ~comments node)
  | "lexical_declaration" ->
    D_lexical_declaration (dec_lexical_declaration ~comments node)
  | "variable_declaration" -> D_variable_declaration (dec_variable_declaration node)
  | "function_signature" -> D_function_signature (dec_function_signature node)
  | "abstract_class_declaration" ->
    D_abstract_class_declaration (dec_abstract_class_declaration node)
  | "module" -> D_module (dec_module node)
  | "internal_module" -> D_internal_module (dec_internal_module ~comments node)
  | "type_alias_declaration" ->
    D_type_alias_declaration (dec_type_alias_declaration ~comments node)
  | "enum_declaration" -> D_enum_declaration (dec_enum_declaration node)
  | "interface_declaration" -> D_interface_declaration (dec_interface_declaration node)
  | "import_alias" -> D_import_alias (dec_import_alias node)
  | "ambient_declaration" -> D_ambient_declaration (dec_ambient_declaration node)
  | s -> failwith ("dec_declaration: " ^ s)

(* Function declaration (see [dec_function_signature]) *)

and dec_function_declaration ?(comments = []) node : function_declaration =
  ensure_Ok node
  @@
  (* "statement_block" *)
  let fun_sig = dec_function_signature ~comments node in
  let* body_field = child_with_field "body" node in
  Ok { fun_sig; body = dec_statement_block body_field }

(* Accessibility modifier *)

and dec_accessibility_modifier node : accessibility_modifier =
  ensure_Ok node
  @@ let* child = child_ranked 0 node in
     match get_name child with
     | "public" -> Ok (Public (make_kwd node))
     | "private" -> Ok (Private (make_kwd node))
     | "protected" -> Ok (Protected (make_kwd node))
     | s -> failwith ("dec_accessibility_modifier: " ^ s)

(* Override modifier *)

and dec_override_modifier node : keyword =
  ensure_Ok node
  @@ let* child = child_ranked 0 node in
     Ok (make_kwd child)

(* Return type annotation *)

and dec_return_type node : call_return_type =
  match get_name node with
  | "type_annotation" -> Type_annotation (dec_type_annotation node)
  | "asserts_annotation" -> Asserts_annotation (dec_asserts_annotation node)
  | "type_predicate_annotation" ->
    Type_predicate_annotation (dec_type_predicate_annotation node)
  | s -> failwith ("dec_return_type: " ^ s)

(* Asserts annotation *)

and dec_asserts_annotation node : asserts_annotation =
  ensure_Ok node
  @@ let* asserts = first_child_named "asserts" node in
     Ok (dec_asserts asserts)

and dec_asserts node : asserts_annotation =
  ensure_Ok node
  @@ let* kwd_asserts = first_child_named "asserts" node in
     let kwd_asserts = make_kwd kwd_asserts in
     let* child = child_ranked 1 node in
     match get_name child with
     | "type_predicate" -> Ok (Assert_predicate (kwd_asserts, dec_type_predicate node))
     | "identifier" -> Ok (Assert_type (kwd_asserts, dec_identifier node))
     | "this" -> Ok (Assert_this (kwd_asserts, make_kwd node))
     | s -> failwith ("dec_asserts: " ^ s)

(* Type predicate annotation *)

and dec_type_predicate_annotation node : type_predicate =
  ensure_Ok node
  @@ let* predicate = child_ranked 1 node in
     Ok (dec_type_predicate predicate)

(* Type predicate *)

and dec_type_predicate node : type_predicate =
  ensure_Ok node
  @@ let* name_field = child_with_field "name" node in
     let name = decode_type_predicate_name name_field in
     let* kwd_is = first_child_named "is" node in
     let kwd_is = make_kwd kwd_is in
     let* type_field = child_with_field "type" node in
     let type_expr = dec_type type_field in
     Ok { name; kwd_is; type_expr }

and decode_type_predicate_name node : type_predicate_name =
  match get_name node with
  | "identifier" -> Type_predicate_identifier (dec_identifier node)
  | "this" -> Type_predicate_this (make_kwd node)
  | _ -> Type_predicate_type (dec_predefined_type node)

(* Predefined type *)

and dec_predefined_type ?(comments = []) node : predefined_type =
  let comments = comments @ prev_comments node in
  match collect_children node with
  | [] -> failwith "dec_predefined_type: No children."
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
    | "any" -> T_any (make_kwd ~comments child)
    | "number" -> T_number (make_kwd ~comments child)
    | "boolean" -> T_boolean (make_kwd ~comments child)
    | "string" -> T_string (make_kwd ~comments child)
    | "symbol" -> T_symbol (make_kwd ~comments child)
    | "unique symbol" -> T_unique_symbol (make_kwd ~comments child)
    | "void" -> T_void (make_kwd ~comments child)
    | "unknown" -> T_unknown (make_kwd ~comments child)
    | "never" -> T_never (make_kwd ~comments child)
    | "object" -> T_object (make_kwd ~comments child)
    | s -> failwith ("dec_predefined_type/decode: " ^ s))

(* Decorator *)

and dec_decorator ?(comments = []) node : decorator =
  ensure_Ok node
  @@ let* child = named_child_ranked 0 node in
     Ok
       (match get_name child with
       | "identifier" -> Decorator_identifier (dec_identifier ~comments node)
       | "member_expression" ->
         Decorator_member_expression (dec_decorator_member_expression ~comments node)
       | "call_expression" ->
         Decorator_call_expression (dec_decorator_call_expression ~comments node)
       | "parenthesized_expression" ->
         Decorator_parenthesized_expression
           (dec_decorator_parenthesized_expression ~comments node)
       | _ -> failwith "dec_decorator")

and dec_decorator_member_expression ?(comments = []) node : decorator_member_expression =
  ensure_Ok node
  @@ let* object_field = child_with_field "object" node in
     let object_expr = decode_object_member_expression ~comments object_field in
     let* dot = first_child_named "." node in
     let sym_dot = make_sym dot in
     let* property_field = child_with_field "property" node in
     let property = dec_identifier property_field in
     Ok { object_expr; sym_dot; property }

and decode_object_member_expression ?(comments = []) node : object_member_expression =
  match get_name node with
  | "identifier" -> Object_name (dec_identifier ~comments node)
  | _ -> Qualified_member_expression (dec_decorator_member_expression ~comments node)

and dec_decorator_call_expression ?(comments = []) node : decorator_call_expression =
  ensure_Ok node
  @@ let* function_field = child_with_field "function" node in
     let function_ = decode_function_or_property ~comments function_field in
     let type_arguments_field = child_with_field_opt "type_arguments" node in
     let type_arguments = make_opt dec_type_arguments type_arguments_field in
     let* arguments_field = child_with_field "arguments" node in
     let arguments = dec_arguments arguments_field in
     Ok { function_; type_arguments; arguments }

and decode_function_or_property ?(comments = []) node : function_or_property =
  match get_name node with
  | "identifier" -> Function_name (dec_identifier ~comments node)
  | "member_expression" ->
    Qualified_member_expression (dec_decorator_member_expression ~comments node)
  | s -> failwith ("decode_function_or_property: " ^ s)

and dec_decorator_parenthesized_expression ?comments node
    : decorator_parenthesized_expression parens
  =
  let decode node =
    match get_name node with
    | "identifier" -> Parenthesized_ident (dec_identifier node)
    | "member_expression" -> Parenthesized_member (dec_decorator_member_expression node)
    | _ -> Parenthesized_call (dec_decorator_call_expression node)
  in
  decode_parens ?comments node decode

(* Type arguments *)

and dec_type_arguments ?comments node : type_arguments =
  decode_ne_list_in_chevrons ?comments node dec_type

(* Function arguments *)

and dec_arguments ?comments node : arguments =
  decode_list_in_parens ?comments node dec_argument

and dec_argument ?comments node : argument =
  match get_name node with
  | "spread_element" -> Spread_element (dec_expression ?comments node)
  | _ -> Expression (dec_expression ?comments node)

(* Generator function declaration (see function declaration) *)

and dec_generator_function_declaration ?(comments = []) node
    : generator_function_declaration
  =
  ensure_Ok node
  @@
  let fun_decl = dec_function_declaration ~comments node in
  let* sym_star = first_child_named "*" node in
  Ok (make_sym sym_star, fun_decl)

(* Class declaration (see [dec_class]) *)

and dec_class_declaration ?(comments = []) node : class_declaration =
  ensure_Ok node
  @@
  let comments = comments @ prev_comments node in
  let decorators = children_named "decorator" node in
  let decorators = ne_list_opt_of_children dec_decorator decorators in
  let* kwd_class = first_child_named "class" node in
  let kwd_class = make_kwd ~comments kwd_class in
  let* name_field = child_with_field "name" node in
  let name = dec_identifier name_field in
  let type_parameters_field = child_with_field_opt "type_parameters" node in
  let type_parameters = make_opt dec_type_parameters type_parameters_field in
  let heritage_child = first_child_named_opt "class_heritage" node in
  let class_heritage = make_opt dec_class_heritage heritage_child in
  let* body_field = child_with_field "body" node in
  let body = dec_class_body body_field in
  Ok { decorators; kwd_class; name; type_parameters; class_heritage; body }

and dec_class_heritage node : class_heritage =
  ignore node;
  failwith "TODO: dec_class_heritage"

and dec_class_body node : class_body =
  ignore node;
  failwith "TODO: dec_class_body"

(* Lexical declaration (see [dec_variable_declaration]) *)

and dec_lexical_declaration ?(comments = []) node : lexical_declaration =
  ensure_Ok node
  @@
  let comments = comments @ prev_comments node in
  let* kind_field = child_with_field "kind" node in
  let decls = children_named "variable_declarator" node in
  let* decls = ne_list_of_children dec_variable_declarator decls in
  let decode_kind node =
    match get_name node with
    | "let" -> Let (make_kwd ~comments node)
    | "const" -> Const (make_kwd ~comments node)
    | s -> failwith ("dec_lexical_declaration/decode_kind: " ^ s)
  in
  Ok { kind = decode_kind kind_field; decls }

(* Variable declaration (see [dec_lexical_declaration]) *)

and dec_variable_declaration ?(comments = []) node : variable_declaration =
  ensure_Ok node
  @@
  let comments = comments @ prev_comments node in
  let* kwd_var = first_child_named "var" node in
  let var_decls = children_named "variable_declarator" node in
  let* var_decls = ne_list_of_children dec_variable_declarator var_decls in
  Ok (make_sym ~comments kwd_var, var_decls)

and dec_variable_declarator ?comments node : variable_declarator =
  ensure_Ok node
  @@ let* name_field = child_with_field "name" node in
     match get_name name_field with
     | "identifier" -> Ok (Decl_ident (dec_identifier ?comments name_field))
     | _ -> Ok (Decl_pattern (dec_destructuring_pattern ?comments name_field))

(* Function signature (See [dec_function_declaration]) *)

and dec_function_signature ?(comments = []) node : function_signature =
  ensure_Ok node
  @@
  let comments = comments @ prev_comments node in
  let kwd_async = first_child_named_opt "async" node in
  let async_comments, function_comments =
    match kwd_async with
    | None -> [], comments
    | Some _ -> comments, []
  in
  let kwd_async = make_opt (make_kwd ~comments:async_comments) kwd_async in
  let* kwd_function = first_child_named "function" node in
  let kwd_function = make_kwd ~comments:function_comments kwd_function in
  let* name_field = child_with_field "name" node in
  let name = dec_identifier name_field in
  (* "_call_signature" inlined: *)
  let type_parameters_field = child_with_field_opt "type_parameters" node in
  let type_parameters = make_opt dec_type_parameters type_parameters_field in
  let* parameters_field = child_with_field "parameters" node in
  let parameters = dec_formal_parameters parameters_field in
  let return_type_field = child_with_field_opt "return_type" node in
  let return_type = make_opt dec_return_type return_type_field in
  let call_sig : call_signature = { type_parameters; parameters; return_type } in
  Ok { kwd_async; kwd_function; name; call_sig }

(* Formal parameters *)

and dec_formal_parameters node : formal_parameters =
  decode_list_in_parens node dec_formal_parameter

and dec_formal_parameter ?(comments = []) node : formal_parameter =
  ensure_Ok node
  @@
  let comments = comments @ prev_comments node in
  (* "_parameter_name" inlined: *)
  let decorators = children_named "decorator" node in
  let decorators = ne_list_opt_of_children dec_decorator decorators in
  let accessibility_modifier = first_child_named_opt "accessibility_modifier" node in
  let access = make_opt dec_accessibility_modifier accessibility_modifier in
  let override_modifier = first_child_named_opt "override_modifier" node in
  let kwd_override = make_opt dec_override_modifier override_modifier in
  let kwd_readonly = first_child_named_opt "readonly" node in
  let kwd_readonly = make_opt make_kwd kwd_readonly in
  let* pattern_field = child_with_field "pattern" node in
  let pattern = decode_parameter_pattern ~comments pattern_field (* Not perfect *) in
  (* *)
  let parameter_name : parameter_name =
    { decorators; access; kwd_override; kwd_readonly; pattern }
  in
  let qmark = first_child_named_opt "?" node in
  let optional = make_opt make_sym qmark in
  let type_field = child_with_field_opt "type" node in
  let type_opt = make_opt dec_type_annotation type_field in
  let default = mk_child_initializer_opt node in
  Ok { parameter_name; optional; type_opt; default }

and decode_parameter_pattern ~comments node : parameter_pattern =
  match get_name node with
  | "this" -> Parameter_this (make_kwd ~comments node)
  | _ -> Parameter_pattern (dec_pattern ~comments node)

and mk_child_initializer_opt node : (symbol * expression) option =
  match first_child_named_opt "=" node with
  | None -> None
  | Some sym_equal -> Some (mk_child_initializer sym_equal node)

and mk_child_initializer sym_equal node : symbol * expression =
  ensure_Ok node
  @@ let* value_field = child_with_field "value" node in
     Ok (make_sym sym_equal, dec_expression value_field)

(* Abstract class declaration ( see [dec_class_declaration]) *)

and dec_abstract_class_declaration node : abstract_class_declaration =
  ignore node;
  failwith "TODO: dec_abstract_class_declaration"

(* Module *)

and dec_module ?(comments = []) node : module_ =
  ensure_Ok node
  @@
  let comments = comments @ prev_comments node in
  let* kwd_module = first_child_named "module" node in
  let kwd_module = make_kwd ~comments kwd_module in
  let* name_field = child_with_field "name" node in
  let module_name =
    match get_name name_field with
    | "string" -> Module_string (dec_string name_field)
    | "identifier" -> Module_ident (dec_identifier name_field)
    | "nested_identifier" -> Module_nested (dec_nested_identifier name_field)
    | s -> failwith ("dec_module: " ^ s)
  in
  let body_field = child_with_field_opt "body" node in
  let module_body = make_opt dec_statement_block body_field in
  Ok { kwd_module; module_name; module_body }

(* Internal module (a.k.a. namespaces) *)

and dec_internal_module ?(comments = []) node : internal_module =
  ensure_Ok node
  @@
  let comments = comments @ prev_comments node in
  let* kwd_namespace = first_child_named "namespace" node in
  let kwd_namespace = make_kwd ~comments kwd_namespace in
  let* name_field = child_with_field "name" node in
  let module_name =
    match get_name name_field with
    | "string" -> Module_string (dec_string name_field)
    | "identifier" -> Module_ident (dec_identifier name_field)
    | "nested_identifier" -> Module_nested (dec_nested_identifier name_field)
    | s -> failwith ("dec_internal_module: " ^ s)
  in
  let body_field = child_with_field_opt "body" node in
  let module_body = make_opt dec_statement_block body_field in
  Ok { kwd_namespace; module_name; module_body }

(* Type alias declaration *)

and dec_type_alias_declaration ?(comments = []) node : type_alias_declaration =
  ensure_Ok node
  @@
  let comments = comments @ prev_comments node in
  let* kwd_type = first_child_named "type" node in
  let kwd_type = make_kwd ~comments kwd_type in
  let* name_field = child_with_field "name" node in
  let name = dec_type_identifier name_field in
  let* sym_equal = first_child_named "=" node in
  let sym_equal = make_sym sym_equal in
  let type_parameters_field = child_with_field_opt "type_parameters" node in
  let type_parameters = make_opt dec_type_parameters type_parameters_field in
  let* value_field = child_with_field "value" node in
  let type_expr = dec_type value_field in
  Ok { kwd_type; name; type_parameters; sym_equal; type_expr }

(* Type parameters *)

and dec_type_parameters node : type_parameters =
  decode_list_in_chevrons node dec_type_parameter

and dec_type_parameter ?(comments = []) node : type_parameter =
  ensure_Ok node
  @@
  let comments = comments @ prev_comments node in
  let kwd_const = first_child_named_opt "const" node in
  let kwd_const = make_opt make_kwd kwd_const in
  let* name_field = child_with_field "name" node in
  let name = dec_type_identifier ~comments name_field (* Not perfect *) in
  let constraint_field = child_with_field_opt "constraint" node in
  let constraint_ = make_opt dec_constraint constraint_field in
  let value_field = child_with_field_opt "value" node in
  let default_type = make_opt dec_default_type value_field in
  Ok { kwd_const; name; constraint_; default_type }

and dec_type_identifier ?comments node : type_identifier = dec_identifier ?comments node

and dec_constraint node : keyword * type_ =
  ensure_Ok node
  @@ let* kwd_extends = first_child_named "extends" node in
     let* type_child = child_ranked 1 node in
     Ok (make_kwd kwd_extends, dec_type type_child)

and dec_default_type node : symbol * type_ =
  ensure_Ok node
  @@ let* sym_equal = first_child_named "=" node in
     let* type_node = child_ranked 1 node in
     Ok (make_sym sym_equal, dec_type type_node)

(* Enum declaration *)

and dec_enum_declaration node : enum_declaration =
  ensure_Ok node
  @@
  let kwd_const = first_child_named_opt "const" node in
  let kwd_const = make_opt make_kwd kwd_const in
  let* kwd_enum = first_child_named "enum" node in
  let kwd_enum = make_kwd kwd_enum in
  let* name_field = child_with_field "name" node in
  let name = dec_identifier name_field in
  let* body_field = child_with_field "body" node in
  let body = dec_enum_entries body_field in
  Ok { kwd_const; kwd_enum; name; body }

and dec_enum_entries node : enum_body list braces =
  decode_list_in_braces node dec_enum_body

and dec_enum_body ?(comments = []) node : enum_body =
  match get_name node with
  | "enum_assignment" -> Enum_assignment (dec_enum_assignment ~comments node)
  | _ -> Enum_name (dec_property_name ~comments node)

and dec_enum_assignment ?comments node : enum_assignment =
  ensure_Ok node
  @@ let* name_field = child_with_field "name" node in
     let name = dec_property_name ?comments name_field in
     let* sym_equal = first_child_named "=" node in
     let default = mk_child_initializer sym_equal node in
     Ok { name; default }

(* Property names *)

and dec_property_name ?(comments = []) node : property_name =
  match get_name node with
  | "property_identifier" -> Property_identifier (dec_identifier ~comments node)
  | "private_property_identifier" ->
    Private_property_identifier (dec_private_property_identifier ~comments node)
  | "string" -> String (dec_string ~comments node)
  | "number" -> Number (dec_number ~comments node)
  | "computed_property_name" ->
    Computed_property_name (dec_computed_property_name ~comments node)
  | s -> failwith ("dec_property_name: " ^ s)

and dec_private_property_identifier ?(comments = []) node : private_property_identifier =
  dec_identifier ~comments node

and dec_computed_property_name ?comments node : expression brackets =
  decode_brackets ?comments node dec_expression

(* Interface declaration *)

and dec_interface_declaration node : interface_declaration =
  ensure_Ok node
  @@ let* kwd_interface = first_child_named "interface" node in
     let kwd_interface = make_kwd kwd_interface in
     let* name_field = child_with_field "name" node in
     let name = dec_type_identifier name_field in
     let type_parameters_field = child_with_field_opt "type_parameters" node in
     let type_parameters = make_opt dec_type_parameters type_parameters_field in
     let extends_type_clause = first_child_named_opt "extends_type_clause" node in
     let extends = make_opt dec_extends_type_clause extends_type_clause in
     let* body_field = child_with_field "body" node in
     let body = dec_object_type body_field in
     Ok { kwd_interface; name; type_parameters; extends; body }

and dec_extends_type_clause node : extends_type_clause =
  ensure_Ok node
  @@ let* kwd_extends = first_child_named "extends" node in
     let named_children = collect_named_children node in
     let* extensions = ne_list_of_children decode_type_extension named_children in
     Ok { kwd_extends = make_kwd kwd_extends; extensions }

and decode_type_extension ?comments node : type_extension =
  match get_name node with
  | "type_identifier" -> Extends_type (dec_type_identifier ?comments node)
  | "nested_type_identifier" -> Extends_nested (dec_nested_type_identifier ?comments node)
  | "generic_type" -> Extends_generic (dec_generic_type ?comments node)
  | s -> failwith ("dec_extends_type_clause: " ^ s)

(* Nested type identifier *)

and dec_nested_type_identifier ?comments node : nested_type_identifier =
  ensure_Ok node
  @@ let* module_field = child_with_field "module" node in
     let* name_field = child_with_field "name" node in
     let decode_module_path node : identifier ne_list =
       match get_name node with
       | "identifier" -> Nonempty_list.[ dec_type_identifier ?comments node ]
       | "nested_identifier" ->
         let path, id = dec_nested_identifier ?comments node in
         Nonempty_list.cons id path
       | s -> failwith ("dec_nested_identifier/decode_module_path: " ^ s)
     in
     let path = Nonempty_list.reverse (decode_module_path module_field) in
     Ok (path, dec_type_identifier name_field)

(* Import alias *)

and dec_import_alias ?comments node : import_alias =
  ensure_Ok node
  @@ let* kwd_import = first_child_named "import" node in
     let kwd_import = make_kwd ?comments kwd_import in
     let* lhs = child_ranked 1 node in
     let alias = dec_identifier lhs in
     let* sym_equal = first_child_named "=" node in
     let sym_equal = make_sym sym_equal in
     let* rhs = child_ranked 3 node in
     let aliased = decode_aliased rhs in
     Ok { kwd_import; alias; sym_equal; aliased }

and decode_aliased node : aliased =
  match get_name node with
  | "identifier" -> Ident (dec_identifier node)
  | "nested_identifier" -> Nested (dec_nested_identifier node)
  | s -> failwith ("dec_aliased: " ^ s)

(* Nested identifier *)

and dec_nested_identifier ?comments node : nested_identifier =
  ensure_Ok node
  @@ let* object_field = child_with_field "object" node in
     let* property_field = child_with_field "property" node in
     let decode_object node : identifier ne_list =
       match get_name node with
       | "identifier" -> Nonempty_list.[ dec_identifier ?comments node ]
       | "member_expression" ->
         let path, id = dec_nested_identifier ?comments node in
         Nonempty_list.cons id path
       | s -> failwith ("dec_nested_identifier/decode_object: " ^ s)
     in
     let decode_property node : identifier =
       match get_name node with
       | "property_identifier" -> dec_identifier node
       | s -> failwith ("dec_nested_identifier/decode_property: " ^ s)
     in
     let path = Nonempty_list.reverse (decode_object object_field) in
     Ok (path, decode_property property_field)

(* Ambient declaration *)

and dec_ambient_declaration ?comments node : ambient_declaration =
  ensure_Ok node
  @@ let* kwd_declare = first_child_named "declare" node in
     let kwd_declare = make_kwd ?comments kwd_declare in
     let* fst_child = named_child_ranked 0 node in
     let* ambient_kind =
       match get_name fst_child with
       | "statement_block" ->
         let* kwd_global = first_child_named "global" node in
         Ok (Global_declaration (make_kwd kwd_global, dec_statement_block fst_child))
       | "property_identifier" ->
         let* kwd_module = first_child_named "module" node in
         let* type_child = child_ranked 5 node in
         let keyword = make_kwd kwd_module
         and identifier = dec_identifier fst_child
         and type_ = dec_type type_child in
         Ok (Module_declaration (keyword, identifier, type_))
       | _ -> Ok (Declaration (dec_declaration fst_child))
     in
     Ok { kwd_declare; ambient_kind }

(* EXPRESSION

   The JavasScript tree-sitter grammar have the non-terminals
   "expression" and "primary_expression" be supertypes, that is,
   hidden rules. Therefore we have to match all the RHS of those
   non-terminals in [print_expression]. *)

and dec_expression ?(comments = []) node : expression =
  match get_name node with
  (* Rest of "expression": *)
  (*  | "glimmer_template" -> E_glimmer_template (dec_glimmer_template node) *)
  | "assignment_expression" -> E_assignment_expression (dec_assignment_expression node)
  | "augmented_assignment_expression" ->
    E_augmented_assignment_expression (dec_augmented_assignment_expression node)
  | "await_expression" -> E_await_expression (dec_await_expression node)
  | "unary_expression" -> E_unary_expression (dec_unary_expression node)
  | "binary_expression" -> E_binary_expression (dec_binary_expression ~comments node)
  | "ternary_expression" -> E_ternary_expression (dec_ternary_expression node)
  | "update_expression" -> E_update_expression (dec_update_expression node)
  | "new_expression" -> E_new_expression (dec_new_expression node)
  | "yield_expression" -> E_yield_expression (dec_yield_expression node)
  | "as_expression" -> E_as_expression (dec_as_expression node)
  | "satisfies_expression" -> E_satisfies_expression (dec_satisfies_expression node)
  | "instantiation_expression" ->
    E_instantiation_expression (dec_instantiation_expression node)
  | "internal_module" -> E_internal_module (dec_internal_module ~comments node)
  | "type_assertion" -> E_type_assertion (dec_type_assertion node)
  | _ -> E_primary_expression (dec_primary_expression ~comments node)

(* Assignment expression *)

and dec_assignment_expression node : assignment_expression =
  ignore node;
  failwith "TODO: dec_assignment_expression"

(* Augmented assignment expression *)

and dec_augmented_assignment_expression node : augmented_assignment_expression =
  ignore node;
  failwith "TODO: dec_augmented_assignment_expression"

(* Await expression *)

and dec_await_expression node : await_expression =
  ignore node;
  failwith "TODO: dec_await_expression"

(* Unary expression *)

and dec_unary_expression node : unary_expression =
  ignore node;
  failwith "TODO: dec_unary_expression"

(* Binary expression *)

and dec_binary_expression ?(comments = []) node : binary_expression =
  ignore comments;
  ignore node;
  failwith "TODO: dec_binary_expression"

(* Ternary expression *)

and dec_ternary_expression node : ternary_expression =
  ignore node;
  failwith "dec_ternary_expression"

(* Update expression *)

and dec_update_expression node : update_expression =
  ignore node;
  failwith "TODO: dec_update_expression"

(* New expression

   Note that the constructor field is a primary expression, but
   "primary_expression" is a supertype, that is, a hidden rule. We
   assume it is an "expression", since primary expressions are a subset
   of them. *)

and dec_new_expression node : new_expression =
  ignore node;
  failwith "TODO: dec_new_expression"

(* Yield expression *)

and dec_yield_expression node : yield_expression =
  ignore node;
  failwith "TODO: dec_yield_expression"

(* As-expression *)

and dec_as_expression node : as_expression =
  ignore node;
  failwith "TODO: dec_as_expression"

(* Statisfies-expression *)

and dec_satisfies_expression node : satisfies_expression =
  ignore node;
  failwith "TODO: dec_satisfies_expression"

(* Instantiation expression *)

and dec_instantiation_expression node : instantiation_expression =
  ignore node;
  failwith "TODO: dec_instantiation_expression"

(* Type assertion *)

and dec_type_assertion node : type_assertion =
  ignore node;
  failwith "TODO: dec_type_assertion"

(* Subscript expression (see [dec_member_expression]) *)

and dec_subscript_expression ?(comments = []) node : subscript_expression =
  ensure_Ok node
  @@ let* object_field = child_with_field "object" node in
     let object_ = dec_expression ~comments object_field in
     let optional_chain_field = child_with_field_opt "optional_chain" node in
     let optional_chain = make_opt dec_optional_chain optional_chain_field in
     let* index_field = child_with_field "index" node in
     let contents = dec_expressions index_field in
     let* sym_lbracket = first_child_named "[" node in
     let opening = make_sym sym_lbracket in
     let* sym_rbracket = first_child_named "]" node in
     let closing = make_sym sym_rbracket in
     let index = Brackets { opening; contents; closing } in
     Ok { object_; optional_chain; index }

and dec_optional_chain node : optional_chain =
  match get_name node with
  | "optional_chain" -> Optional_chain (make_sym node)
  | s -> failwith ("dec_optional_chain: " ^ s)

(* Member expression *)

and dec_member_expression ?(comments = []) node =
  ensure_Ok node
  @@ let* object_field = child_with_field "object" node in
     let object_ = dec_object_member ~comments object_field in
     let optional_chain_field = child_with_field_opt "optional_chain" node in
     let* property_field = child_with_field "property" node in
     let property = dec_property_ident property_field in
     let* selector =
       match optional_chain_field with
       | None ->
         let* selector = first_child_named "." node in
         Ok (Dot (make_sym selector))
       | Some node -> Ok (Optional_chain (make_sym node))
     in
     Ok { object_; selector; property }

and dec_object_member ?comments node : object_member =
  match get_name node with
  | "import" -> Object_member_import (make_kwd ?comments node)
  | _ -> Object_member_expression (dec_expression ?comments node)

and dec_property_ident ?comments node : property_ident =
  match get_name node with
  | "private_property_identifier" ->
    Private_property_identifier (dec_identifier ?comments node)
  | "property_identifier" -> Property_identifier (dec_identifier ?comments node)
  | s -> failwith ("dec_property_ident: " ^ s)

(* Parenthesised expression *)

and dec_parenthesized_expression ?(comments = []) node : parenthesized_expression =
  decode_ne_list_in_parens ~comments node dec_expression

(* Primary expression *)

and dec_primary_expression ?(comments = []) node : primary_expression =
  ignore comments;
  ignore node;
  failwith "TODO: dec_primary_expression"

(* Sequence expression *)

and dec_sequence_expression ?(comments = []) node : sequence_expression =
  ensure_Ok node
  @@
  let raw_children = collect_named_children node in
  ne_list_of_children ~comments dec_expression raw_children

(* Object expression *)

and dec_object ?(comments = []) node : object_expr =
  ignore comments;
  ignore node;
  failwith "dec_object"

(* LHS expression *)

and dec_lhs_expression ?comments node : lhs_expression =
  match get_name node with
  | "member_expression" -> Member_expression (dec_member_expression ?comments node)
  | "subscript_expression" ->
    Subscript_expression (dec_subscript_expression ?comments node)
  | "identifier" -> Identifier (dec_identifier ?comments node)
  | "undefined" -> Undefined (make_kwd ?comments node)
  | "object_pattern" -> Pattern (Pattern_object (dec_object_pattern ?comments node))
  | "array_pattern" -> Pattern (Pattern_array (dec_array_pattern ?comments node))
  | "non_null_expression" -> Non_null_expression (dec_non_null_expression ?comments node)
  | s -> failwith ("dec_lhs_expression: " ^ s)

(* Non-null expression *)

and dec_non_null_expression ?comments node : expression = dec_expression ?comments node

(* PATTERN

   The JavasScript tree-sitter grammar have the non-terminal
   "pattern" be a supertype, that is, a hidden rule. *)

and dec_pattern ?(comments = []) node : pattern =
  ignore comments;
  ignore node;
  failwith "TODO: dec_pattern"

(* Object pattern *)

and dec_object_pattern ?comments node : object_pattern =
  decode_list_in_braces ?comments node dec_member_pattern

and dec_member_pattern ?(comments = []) node : member_pattern =
  match get_name node with
  | "pair_pattern" -> Member_pair_pattern (dec_pair_pattern ~comments node)
  | "rest_pattern" -> Member_rest_pattern (dec_rest_pattern ~comments node)
  | "object_assignment_pattern" ->
    Member_object_assignment (dec_object_assignment_pattern node)
  | "shorthand_property_identifier_pattern" ->
    Member_shorthand_property (dec_shorthand_property_identifier_pattern node)
  | s -> failwith ("dec_object_pattern: " ^ s)

(* Pair pattern *)

and dec_pair_pattern ?(comments = []) node : pair_pattern =
  ensure_Ok node
  @@ let* key_field = child_with_field "key" node in
     let key = dec_property_name ~comments key_field in
     let* sym_colon = first_child_named ":" node in
     let sym_colon = make_sym sym_colon in
     let* value_field = child_with_field "value" node in
     let value = decode_pair_value_pattern value_field in
     Ok { key; sym_colon; value }

and decode_pair_value_pattern node : pair_value_pattern =
  match get_name node with
  | "assignment_pattern" -> Pair_value_assignment (dec_assignment_pattern node)
  | _ ->
    (* Hidden rule *)
    Pair_value (dec_pattern node)

(* Rest pattern *)

and dec_rest_pattern ?(comments = []) node : rest_pattern =
  ensure_Ok node
  @@ let* sym_ellipsis = first_child_named "..." node in
     let sym_ellipsis = make_sym ~comments sym_ellipsis in
     let* expr_child = named_child_ranked 0 node in
     let expression = dec_lhs_expression expr_child in
     Ok { sym_ellipsis; expression }

(* Assignment pattern *)

and dec_object_assignment_pattern ?comments node : object_assignment_pattern =
  ensure_Ok node
  @@ let* left_field = child_with_field "left" node in
     let left = dec_object_lhs_pattern ?comments left_field in
     let* sym_equal = first_child_named "=" node in
     let sym_equal = make_kwd sym_equal in
     let* right_field = child_with_field "right" node in
     let right = dec_expression right_field in
     Ok ({ left; sym_equal; right } : object_assignment_pattern)

and dec_object_lhs_pattern ?comments node : object_lhs_pattern =
  dec_lhs_pattern ?comments node

and dec_lhs_pattern ?comments node : lhs_pattern =
  match get_name node with
  | "shorthand_property_identifier_pattern" ->
    Decl_ident (dec_shorthand_property_identifier_pattern ?comments node)
  | _ ->
    (* Hidden rule *)
    Decl_pattern (dec_destructuring_pattern ?comments node)

(* Shorthand property identifier pattern *)

and dec_shorthand_property_identifier_pattern ?comments node : identifier =
  dec_identifier ?comments node

(* Array pattern *)

and dec_array_pattern ?comments node : array_pattern =
  decode_list_in_brackets ?comments node dec_array_cell_pattern

and dec_array_cell_pattern ?comments node : array_cell_pattern =
  match get_name node with
  | "assignment_pattern" -> Cell_assignment (dec_assignment_pattern ?comments node)
  | _ ->
    (* hidden rule *)
    Cell_pattern (dec_pattern ?comments node)

(* Assignment pattern *)

and dec_assignment_pattern ?comments node =
  ensure_Ok node
  @@ let* left_field = child_with_field "left" node in
     let left = dec_pattern ?comments left_field in
     let* sym_equal = first_child_named "=" node in
     let sym_equal = make_sym sym_equal in
     let* right_field = child_with_field "right" node in
     let right = dec_expression right_field in
     Ok { left; sym_equal; right }

(* Rule "_destructuring_pattern" is inlined. *)

and dec_destructuring_pattern ?comments node : destructuring_pattern =
  match get_name node with
  | "object_pattern" -> Pattern_object (dec_object_pattern ?comments node)
  | "array_pattern" -> Pattern_array (dec_array_pattern ?comments node)
  | s -> failwith ("dec_destructuring_pattern: " ^ s)

(** TYPES
*)
and dec_type ?comments node : type_ =
  ignore comments;
  ignore node;
  failwith "TODO: dec_type"

and dec_generic_type ?comments node : generic_type =
  ignore comments;
  ignore node;
  failwith "TODO: dec_generic_type"

and dec_object_type node : object_type =
  ignore node;
  failwith "TODO: dec_object_type"
