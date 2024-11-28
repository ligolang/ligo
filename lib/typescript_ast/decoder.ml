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

let make_opt_res decoder = function
  | None -> Ok None
  | Some value ->
    (match decoder value with
    | Ok decoded -> Ok (Some decoded)
    | Error msg -> Error msg)

(* Handling some modifiers *)

let mk_set_get_all node : set_get_all option =
  let kwd_set = first_child_named_opt "set" node
  and kwd_get = first_child_named_opt "get" node
  and sym_star = first_child_named_opt "*" node in
  match kwd_set, kwd_get, sym_star with
  | None, None, None -> None
  | Some kwd_set, _, _ -> Some (Set (make_kwd kwd_set))
  | _, Some kwd_get, _ -> Some (Get (make_kwd kwd_get))
  | _, _, Some sym_star -> Some (All (make_sym sym_star))

(* Decoding children of the same type *)

let list_of_children_res ?(comments = []) decoder children : ('a list, _) result =
  let f raw_child = List.cons (decoder ?comments:None raw_child) in
  match children with
  | [] -> Ok []
  | fst_raw_child :: siblings ->
    let fst_child = decoder ?comments:(Some comments) fst_raw_child in
    let children_res = fst_child :: List.fold_right ~f ~init:[] siblings in
    Result.all children_res

let ne_list_opt_of_children_res ?(comments = []) decoder children
    : ('a ne_list option, _) result
  =
  let f raw_child = List.cons (decoder ?comments:None raw_child) in
  match children with
  | [] -> Ok None
  | fst_raw_child :: siblings ->
    let fst_child = decoder ?comments:(Some comments) fst_raw_child in
    (match fst_child with
    | Ok fst_child ->
      let* tail = List.fold_right ~f ~init:[] siblings |> Result.all in
      Ok (Some Nonempty_list.(fst_child :: tail))
    | Error msg -> Error msg)

let ne_list_of_children_res ?(comments = []) decoder children : ('a ne_list, _) result =
  match ne_list_opt_of_children_res ~comments decoder children with
  | Ok None -> Error "Expected at least one child."
  | Ok (Some ne_list) -> Ok ne_list
  | Error msg -> Error msg

(* Decoding enclosed unique child *)

let decode_enclosed_res ?(comments = []) node decoder opening closing
    : ('a enclosed, _) result
  =
  let comments = comments @ prev_comments node in
  let* opening = first_child_named opening node in
  let opening = make_sym ~comments opening in
  let* closing = first_child_named closing node in
  let closing = make_sym closing in
  let* child = (* We assume one child *) child_ranked 1 node in
  let* contents = decoder child in
  Ok { opening; contents; closing }

let decode_braces_res ?comments node decoder : ('a braces, _) result =
  let* braces = decode_enclosed_res ?comments node decoder "{" "}" in
  Ok (Braces braces)

let decode_chevrons_res ?comments node decoder : ('a chevrons, _) result =
  let* chevrons = decode_enclosed_res ?comments node decoder "<" ">" in
  Ok (Chevrons chevrons)

let decode_brackets_res ?comments node decoder : ('a brackets, _) result =
  let* brackets = decode_enclosed_res ?comments node decoder "[" "]" in
  Ok (Brackets brackets)

let decode_parens_res ?comments node decoder : ('a parens, _) result =
  let* parens = decode_enclosed_res ?comments node decoder "(" ")" in
  Ok (Parens parens)

(* Decoding enclosed lists *)
(*
let decode_enclosed_list ?(comments = []) node decoder opening closing : 'a list enclosed =
  ensure_Ok node
  @@
  let comments = comments @ prev_comments node in
  let* opening = first_child_named opening node in
  let opening = make_sym ~comments opening in
  let* closing = first_child_named closing node in
  let closing = make_sym closing in
  let clauses = collect_named_children node in
  let contents = list_of_children decoder clauses in
  Ok { opening; contents; closing }

let decode_list_in_braces ?comments node decoder : 'a list braces =
  Braces (decode_enclosed_list ?comments node decoder "{" "}")

let decode_list_in_chevrons ?comments node decoder : 'a list chevrons =
  Chevrons (decode_enclosed_list ?comments node decoder "<" ">")

let decode_list_in_brackets ?comments node decoder : 'a list brackets =
  Brackets (decode_enclosed_list ?comments node decoder "[" "]")

let decode_list_in_parens ?comments node decoder : 'a list parens =
  Parens (decode_enclosed_list ?comments node decoder "(" ")")
*)

let decode_enclosed_list_res ?(comments = []) node decoder opening closing
    : ('a list enclosed, _) result
  =
  let comments = comments @ prev_comments node in
  let* opening = first_child_named opening node in
  let opening = make_sym ~comments opening in
  let* closing = first_child_named closing node in
  let closing = make_sym closing in
  let clauses = collect_named_children node in
  let* contents = list_of_children_res decoder clauses in
  Ok { opening; contents; closing }

let decode_list_in_braces_res ?comments node decoder : ('a list braces, _) result =
  let* list = decode_enclosed_list_res ?comments node decoder "{" "}" in
  Ok (Braces list)

let decode_list_in_chevrons_res ?comments node decoder : ('a list chevrons, _) result =
  let* list = decode_enclosed_list_res ?comments node decoder "<" ">" in
  Ok (Chevrons list)

let decode_list_in_brackets_res ?comments node decoder : ('a list brackets, _) result =
  let* list = decode_enclosed_list_res ?comments node decoder "[" "]" in
  Ok (Brackets list)

let decode_list_in_parens_res ?comments node decoder : ('a list parens, _) result =
  let* list = decode_enclosed_list_res ?comments node decoder "(" ")" in
  Ok (Parens list)

(* Decoding enclosed non-empty lists *)
(*
let decode_enclosed_ne_list ?(comments = []) node decoder opening closing
    : 'a ne_list enclosed
  =
  ensure_Ok node
  @@
  let comments = comments @ prev_comments node in
  let* opening = first_child_named opening node in
  let opening = make_sym ~comments opening in
  let* closing = first_child_named closing node in
  let closing = make_sym closing in
  let clauses = collect_named_children node in
  let* contents = ne_list_of_children decoder clauses in
  Ok { opening; contents; closing }

let decode_ne_list_in_braces ?comments node decoder : 'a ne_list braces =
  Braces (decode_enclosed_ne_list ?comments node decoder "{" "}")

let decode_ne_list_in_chevrons ?comments node decoder : 'a ne_list chevrons =
  Chevrons (decode_enclosed_ne_list ?comments node decoder "<" ">")

let decode_ne_list_in_brackets ?comments node decoder : 'a ne_list brackets =
  Brackets (decode_enclosed_ne_list ?comments node decoder "[" "]")

let decode_ne_list_in_parens ?comments node decoder : 'a ne_list parens =
  Parens (decode_enclosed_ne_list ?comments node decoder "(" ")")
*)

let decode_enclosed_ne_list_res ?(comments = []) node decoder opening closing
    : ('a ne_list enclosed, string) result
  =
  let comments = comments @ prev_comments node in
  let* opening = first_child_named opening node in
  let opening = make_sym ~comments opening in
  let* closing = first_child_named closing node in
  let closing = make_sym closing in
  let clauses = collect_named_children node in
  let* contents = ne_list_of_children_res decoder clauses in
  Ok { opening; contents; closing }

let decode_ne_list_in_braces_res ?comments node decoder : ('a ne_list braces, _) result =
  let* braces = decode_enclosed_ne_list_res ?comments node decoder "{" "}" in
  Ok (Braces braces)

let decode_ne_list_in_chevrons_res ?comments node decoder
    : ('a ne_list chevrons, _) result
  =
  let* chevrons = decode_enclosed_ne_list_res ?comments node decoder "<" ">" in
  Ok (Chevrons chevrons)

let decode_ne_list_in_brackets_res ?comments node decoder
    : ('a ne_list brackets, _) result
  =
  let* brackets = decode_enclosed_ne_list_res ?comments node decoder "[" "]" in
  Ok (Brackets brackets)

let decode_ne_list_in_parens_res ?comments node decoder : ('a ne_list parens, _) result =
  let* parens = decode_enclosed_ne_list_res ?comments node decoder "(" ")" in
  Ok (Parens parens)

(* Decoding the CST *)

let rec dec_program file map node : (Ast.t, string) result =
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

and dec_statements ?(comments = []) node : (statements, _) result =
  let children = collect_named_children node in
  ne_list_opt_of_children_res ~comments dec_statement children

and dec_statement ?(comments = []) node : (statement, _) result =
  match get_name node with
  | "export_statement" ->
    let* statement = dec_export_statement ~comments node in
    Ok (S_export_statement statement)
  | "import_statement" ->
    let* statement = dec_import_statement ~comments node in
    Ok (S_import_statement statement)
  | "debugger_statement" -> Ok (S_debugger_statement (make_kwd ~comments node))
  | "expression_statement" ->
    Ok (S_expression_statement (dec_expression_statement ~comments node))
  | "statement_block" ->
    let* statement = dec_statement_block ~comments node in
    Ok (S_statement_block statement)
  | "if_statement" ->
    let* statement = dec_if_statement ~comments node in
    Ok (S_if_statement statement)
  | "switch_statement" ->
    let* statement = dec_switch_statement node in
    Ok (S_switch_statement statement)
  | "for_statement" ->
    let* statement = dec_for_statement node in
    Ok (S_for_statement statement)
  | "for_in_statement" ->
    let* statement = dec_for_in_statement node in
    Ok (S_for_in_statement statement)
  | "while_statement" ->
    let* statement = dec_while_statement node in
    Ok (S_while_statement statement)
  | "do_statement" ->
    let* statement = dec_do_statement ~comments node in
    Ok (S_do_statement statement)
  | "try_statement" ->
    let* statement = dec_try_statement node in
    Ok (S_try_statement statement)
  | "with_statement" ->
    let* statement = dec_with_statement node in
    Ok (S_with_statement statement)
  | "break_statement" ->
    let* statement = dec_break_statement node in
    Ok (S_break_statement statement)
  | "continue_statement" ->
    let* statement = dec_continue_statement node in
    Ok (S_continue_statement statement)
  | "return_statement" ->
    let* statement = dec_return_statement node in
    Ok (S_return_statement statement)
  | "throw_statement" ->
    let* statement = dec_throw_statement node in
    Ok (S_throw_statement statement)
  | "empty_statement" -> Ok (S_empty_statement (!get_region node))
  (* Inlining declarations cases (hidden rule) *)
  | "function_declaration" ->
    let* declaration = dec_function_declaration ~comments node in
    Ok (S_declaration (D_function_declaration declaration))
  | "generator_function_declaration" ->
    let* declaration = dec_generator_function_declaration node in
    Ok (S_declaration (D_generator_function_declaration declaration))
  | "class_declaration" ->
    let* declaration = dec_class_declaration ~comments node in
    Ok (S_declaration (D_class_declaration declaration))
  | "lexical_declaration" ->
    let* declaration = dec_lexical_declaration ~comments node in
    Ok (S_declaration (D_lexical_declaration declaration))
  | "variable_declaration" ->
    let* declaration = dec_variable_declaration ~comments node in
    Ok (S_declaration (D_variable_declaration declaration))
  | "function_signature" ->
    let* declaration = dec_function_signature node in
    Ok (S_declaration (D_function_signature declaration))
  | "abstract_class_declaration" ->
    let* declaration = dec_abstract_class_declaration node in
    Ok (S_declaration (D_abstract_class_declaration declaration))
  | "module" ->
    let* declaration = dec_module_declaration node in
    Ok (S_declaration (D_module declaration))
  | "internal_module" ->
    let* declaration = dec_internal_module ~comments node in
    Ok (S_declaration (D_internal_module declaration))
  | "type_alias_declaration" ->
    let* declaration = dec_type_alias_declaration node in
    Ok (S_declaration (D_type_alias_declaration declaration))
  | "enum_declaration" ->
    let* declaration = dec_enum_declaration node in
    Ok (S_declaration (D_enum_declaration declaration))
  | "interface_declaration" ->
    let* declaration = dec_interface_declaration node in
    Ok (S_declaration (D_interface_declaration declaration))
  | "import_alias" ->
    let* declaration = dec_import_alias node in
    Ok (S_declaration (D_import_alias declaration))
  | "ambient_declaration" ->
    let* declaration = dec_ambient_declaration node in
    Ok (S_declaration (D_ambient_declaration declaration))
  | s -> Error ("dec_statement: " ^ s)

(* Export statement *)

and dec_export_statement ?(comments = []) node : (export_statement, _) result =
  let comments = comments @ prev_comments node in
  let* kwd_export = first_child_named "export" node in
  let* after_export = next_sibling kwd_export in
  let kwd_export = make_kwd ~comments kwd_export in
  let* export_kind =
    match get_name after_export with
    | "*" ->
      let* kwd_from = first_child_named "from" node in
      let* from_clause = dec_from_clause node kwd_from in
      Ok (Export_from from_clause)
    | "namespace_export" ->
      let* kwd_from = first_child_named "from" node in
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
      let* expression = next_sibling after_export in
      let* expression = dec_expression expression in
      Ok (Export_equal (make_sym after_export, expression))
    | "as" ->
      let* kwd_namespace = first_child_named "namespace" node in
      let* identifier = first_child_named "identifier" node in
      Ok (Export_as_namespace (make_kwd kwd_namespace, dec_identifier identifier))
    | _ ->
      let* export_declaration = dec_export_declaration after_export node in
      Ok (Export_declaration export_declaration)
  in
  Ok { kwd_export; export_kind }

and dec_export_type after_export node : (export_type, _) result =
  let* export_clause = next_sibling after_export in
  let kwd_type = make_kwd after_export in
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
  let* decorators = list_of_children_res dec_decorator decorators in
  Ok { decorators; decorated }

and dec_export_clause node : (export_clause, _) result =
  decode_list_in_braces_res node dec_export_specifier

and dec_export_specifier ?(comments = []) node : (export_specifier, _) result =
  let comments = comments @ prev_comments node in
  let* name_field = child_with_field "name" node in
  let* name = dec_module_export_name ~comments name_field in
  let alias_field = child_with_field_opt "alias" node in
  let* alias = make_opt_res dec_module_export_name alias_field in
  let* alias =
    match alias with
    | None -> Ok None
    | Some alias ->
      let* kwd_as = first_child_named "as" node in
      Ok (Some (make_kwd kwd_as, alias))
  in
  Ok ({ name; alias } : export_specifier)

and dec_module_export_name ?(comments = []) node : (module_export_name, _) result =
  match get_name node with
  | "identifier" -> Ok (Export_ident (dec_identifier ~comments node))
  | "string" -> Ok (Export_string (dec_string ~comments node))
  | s -> Error ("dec_module_export_name: " ^ s)

and dec_from_clause node kwd_from : (from_clause, _) result =
  let* source_field = child_with_field "source" node in
  Ok (make_kwd kwd_from, dec_string source_field)

and dec_namespace_export ?(comments = []) node : (namespace_export, _) result =
  let comments = comments @ prev_comments node in
  let* sym_star = first_child_named "*" node in
  let sym_star = make_sym ~comments sym_star in
  let* kwd_as = first_child_named "as" node in
  let* module_export_name = next_sibling kwd_as in
  let kwd_as = make_kwd kwd_as in
  let* namespace_name = dec_module_export_name module_export_name in
  Ok { sym_star; kwd_as; namespace_name }

and dec_export_default after_export node : (export_kind, _) result =
  let decorators = children_named "decorator" node in
  let kwd_default = make_kwd after_export in
  match child_with_field_opt "declaration" node with
  | None ->
    let* value_field = child_with_field "value" node in
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
  let* import_attribute = make_opt_res dec_import_attribute import_attribute in
  let* (import : import) =
    match first_child_named_opt "import_clause" node with
    | Some import_clause ->
      let* kwd_from = first_child_named "from" node in
      let* import_clause = dec_import_clause import_clause in
      let* from_clause = dec_from_clause node kwd_from in
      Ok (Import_clause (import_clause, from_clause))
    | None ->
      (match first_child_named_opt "import_require_clause" node with
      | Some clause ->
        let* require_clause = dec_import_require_clause clause in
        Ok (Import_require_clause require_clause)
      | None ->
        let* source_field = child_with_field "source" node in
        Ok (Import_source (dec_string source_field)))
  in
  Ok { kwd_import; import_kind; import; import_attribute }

and dec_import_clause ?(comments = []) node : (import_clause, _) result =
  let comments = comments @ prev_comments node in
  let* fst_child = child_ranked 0 node in
  match get_name fst_child with
  | "namespace_import" ->
    let* namespace_import = dec_namespace_import ~comments fst_child in
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
        let* next = next_sibling comma in
        let* next = dec_namespace_or_named_imports next in
        Ok (Some next)
    in
    Ok (Import_ident (ident, from))
  | s -> Error ("dec_import_clause: " ^ s)

and dec_namespace_or_named_imports node : (namespace_or_named_imports, _) result =
  match get_name node with
  | "namespace_import" ->
    let* namespace_import = dec_namespace_import node in
    Ok (Import_namespace namespace_import)
  | "named_imports" ->
    let* named_imports = dec_named_imports node in
    Ok (Import_named named_imports)
  | s -> Error ("dec_namespace_or_named_imports: " ^ s)

and dec_namespace_import ?(comments = []) node : (namespace_import, _) result =
  let comments = comments @ prev_comments node in
  let* sym_star = first_child_named "*" node in
  let sym_star = make_sym ~comments sym_star in
  let* kwd_as = first_child_named "as" node in
  let* identifier = next_sibling kwd_as in
  let kwd_as = make_kwd kwd_as in
  let identifier = dec_identifier identifier in
  Ok { sym_star; kwd_as; identifier }

and dec_named_imports ?(comments = []) node : (named_imports, _) result =
  decode_list_in_braces_res ~comments node dec_import_specifier

and dec_import_specifier ?(comments = []) node : (import_specifier, _) result =
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
      let* name = dec_module_export_name ~comments:snd_child_comments name_field in
      let kwd_as = make_kwd kwd_as in
      let alias = dec_identifier alias_field in
      Ok (Import_spec_alias { name; kwd_as; alias })
  in
  Ok (import_kind, import_specifier')

and dec_import_require_clause ?(comments = []) node : (import_require_clause, _) result =
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

and dec_import_attribute node : (import_attribute, _) result =
  let* kind_node = child_ranked 0 node in
  let* object_node = child_ranked 1 node in
  match get_name kind_node with
  | "with" ->
    let* expression = dec_object object_node in
    Ok (Import_with (make_kwd kind_node, expression))
  | "assert" ->
    let* expression = dec_object object_node in
    Ok (Import_assert (make_kwd kind_node, expression))
  | s -> Error ("dec_import_attribute: " ^ s)

(* Expression statements

   {@js[
    expression_statement: $ => seq($._expressions, $._semicolon),
    _expressions: $ => choice($.expression, $.sequence_expression),
    sequence_expression: $ => prec.right(commaSep1($.expression))
   ]}

   See [dec_expression]. *)

and dec_expression_statement ?(comments = []) node : expression_statement =
  dec_expressions ~comments node

and dec_expressions ?(comments = []) (node : ts_tree) : expressions =
  ensure_Ok node
  @@
  match get_name node with
  | "sequence_expression" -> dec_sequence_expression ~comments node
  | _ ->
    let* expression = dec_expression ~comments node in
    Ok Nonempty_list.[ expression ]

(* Statement blocks *)

and dec_statement_block ?(comments = []) node : (statement_block, _) result =
  dec_statements ~comments node

(* If statement *)

and dec_if_statement ?(comments = []) node : (if_statement, _) result =
  let* kwd_if = first_child_named "if" node in
  let kwd_if = make_kwd ~comments kwd_if in
  let* condition_field = child_with_field "condition" node in
  let* condition = dec_parenthesized_expression condition_field in
  let* consequence_field = child_with_field "consequence" node in
  let* consequence = dec_statement consequence_field in
  let alternative_field = child_with_field_opt "alternative" node in
  let* alternative = make_opt_res dec_else_clause alternative_field in
  Ok { kwd_if; condition; consequence; alternative }

and dec_else_clause ?(comments = []) node : (kwd_else * statement, _) result =
  let comments = comments @ prev_comments node in
  let* kwd_else = first_child_named "else" node in
  let* statement = next_sibling kwd_else in
  let* statement = dec_statement statement in
  Ok (make_kwd ~comments kwd_else, statement)

(* Switch statement *)

and dec_switch_statement node : (switch_statement, _) result =
  let* kwd_switch = first_child_named "switch" node in
  let kwd_switch = make_kwd kwd_switch in
  let* value_field = child_with_field "value" node in
  let* value = dec_parenthesized_expression value_field in
  let* body_field = child_with_field "body" node in
  let* body = dec_switch_body body_field in
  Ok { kwd_switch; value; body }

and dec_switch_body node : (switch_body, _) result =
  decode_list_in_braces_res node decode_switch_entry

and decode_switch_entry ?(comments = []) node : (switch_entry, _) result =
  match get_name node with
  | "switch_case" ->
    let* switch_case = dec_switch_case ~comments node in
    Ok (Switch_case switch_case)
  | "switch_default" ->
    let* default = dec_switch_default ~comments node in
    Ok (Switch_default default)
  | s -> Error ("dec_switch_entry: " ^ s)

and dec_switch_case ?(comments = []) node : (switch_case, _) result =
  let comments = comments @ prev_comments node in
  let* kwd_case = first_child_named "case" node in
  let kwd_case = make_kwd ~comments kwd_case in
  let* value_field = child_with_field "value" node in
  let value = dec_expressions value_field in
  let children = collect_children node in
  let stmt_children = skip_until_colon children in
  let* body = list_of_children_res dec_statement stmt_children in
  Ok { kwd_case; value; body }

and dec_switch_default ?(comments = []) node : (switch_default, _) result =
  let comments = comments @ prev_comments node in
  let* kwd_default = first_child_named "default" node in
  let kwd_default = make_kwd ~comments kwd_default in
  let statements = collect_named_children node in
  let* statements = list_of_children_res dec_statement statements in
  Ok { kwd_default; statements }

(* For statement *)

and dec_for_statement node : (for_statement, _) result =
  let* kwd_for = first_child_named "for" node in
  let kwd_for = make_kwd kwd_for in
  let* sym_lpar = first_child_named "(" node in
  let sym_lpar = make_sym sym_lpar in
  let* initializer_field = child_with_field "initializer" node in
  let* initializer_ = decode_for_initializer initializer_field in
  let* condition_field = child_with_field "condition" node in
  let* condition = decode_for_condition condition_field in
  let increment_field = child_with_field_opt "increment" node in
  let increment = make_opt dec_expressions increment_field in
  let* sym_rpar = first_child_named ")" node in
  let sym_rpar = make_sym sym_rpar in
  let* body_field = child_with_field "body" node in
  let* body = dec_statement body_field in
  Ok { kwd_for; sym_lpar; initializer_; condition; increment; sym_rpar; body }

and decode_for_initializer node : (for_initializer, _) result =
  match get_name node with
  | "lexical_declaration" ->
    let* declaration = dec_lexical_declaration node in
    Ok (For_lexical_declaration declaration)
  | "variable_declaration" ->
    let* declaration = dec_variable_declaration node in
    Ok (For_variable_declaration declaration)
  | "expression_statement" ->
    Ok (For_expression_statement (dec_expression_statement node))
  | "empty_statement" -> Ok (For_empty_statement (!get_region node))
  | s -> Error ("decode_for_initializer: " ^ s)

and decode_for_condition node : (for_condition, _) result =
  match get_name node with
  | "expression_statement" ->
    Ok (For_condition_expression (dec_expression_statement node))
  | "empty_statement" -> Ok (For_condition_empty (!get_region node))
  | s -> Error ("decode_for_condition: " ^ s)

(* For-in statement *)

and dec_for_in_statement node : (for_in_statement, _) result =
  let* kwd_for = first_child_named "for" node in
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
  let* body = dec_statement body_field in
  let* operator_field = child_with_field "operator" node in
  let* operator = decode_for_operator operator_field in
  let* right_field = child_with_field "right" node in
  let collection = dec_expressions right_field in
  let* (range : for_range) =
    match kind_field with
    | None ->
      (match get_name left_field with
      | "parenthesized_expression" ->
        let* expression = dec_parenthesized_expression node in
        Ok (For_in_parenthesized expression)
      | _ ->
        let* expression = dec_lhs_expression node in
        Ok (For_in_expression expression))
    | Some kind_field ->
      let keyword = make_kwd kind_field in
      let* variable =
        match get_name left_field with
        | "identifier" -> Ok (For_in_ident (dec_identifier left_field))
        | _ ->
          let* pattern = dec_destructuring_pattern left_field in
          Ok (For_in_pattern pattern)
      in
      (match get_name kind_field with
      | "var" ->
        let value_field = child_with_field_opt "value" node in
        let* default = make_opt_res dec_expression value_field in
        Ok (For_in_var { kwd_var = keyword; variable; default })
      | "let" -> Ok (For_in_let (keyword, variable))
      | "const" -> Ok (For_in_const (keyword, variable))
      | s -> Error ("dec_for_in_statement: " ^ s))
  in
  let for_header : for_header = { range; operator; collection } in
  Ok { kwd_for; kwd_await; sym_lpar; for_header; sym_rpar; body }

and decode_for_operator node : (for_operator, _) result =
  match get_name node with
  | "in" -> Ok (In (make_kwd node))
  | "of" -> Ok (Of (make_kwd node))
  | s -> Error ("decode_for_operator: " ^ s)

(* While statement *)

and dec_while_statement node : (while_statement, _) result =
  let* kwd_while = first_child_named "while" node in
  let kwd_while = make_kwd kwd_while in
  let* condition_field = child_with_field "condition" node in
  let* condition = dec_parenthesized_expression condition_field in
  let* body_field = child_with_field "body" node in
  let* body = dec_statement body_field in
  Ok { kwd_while; condition; body }

(* Do statement *)

and dec_do_statement ?(comments = []) node : (do_statement, _) result =
  let* kwd_do = first_child_named "do" node in
  let kwd_do = make_kwd ~comments kwd_do in
  let* body_field = child_with_field "body" node in
  let* body = dec_statement body_field in
  let* kwd_while = first_child_named "while" node in
  let kwd_while = make_kwd kwd_while in
  let* condition_field = child_with_field "condition" node in
  let* condition = dec_parenthesized_expression condition_field in
  Ok { kwd_do; body; kwd_while; condition }

(* Try statement *)

and dec_try_statement node : (try_statement, _) result =
  let* kwd_try = first_child_named "try" node in
  let kwd_try = make_kwd kwd_try in
  let* body_field = child_with_field "body" node in
  let* body = dec_statement_block body_field in
  let handler_field = child_with_field_opt "handler" node in
  let* handler = make_opt_res dec_catch_clause handler_field in
  let finalizer_field = child_with_field_opt "finalizer" node in
  let* finalizer = make_opt_res dec_finally_clause finalizer_field in
  Ok { kwd_try; body; handler; finalizer }

and dec_catch_clause node : (catch_clause, _) result =
  let* kwd_catch = first_child_named "catch" node in
  let kwd_catch = make_kwd kwd_catch in
  let parameter_field = child_with_field_opt "parameter" node in
  let* parameter = make_opt_res (dec_catch_parameter node) parameter_field in
  let* body_field = child_with_field "body" node in
  let* body = dec_statement_block body_field in
  Ok { kwd_catch; parameter; body }

and dec_catch_parameter node param : (catch_parameter, _) result =
  let* catch_parameter = dec_catch_parameter_kind param in
  let* sym_lpar = first_child_named "(" node in
  let sym_lpar = make_sym sym_lpar in
  let type_field = child_with_field_opt "type" node in
  let* type_opt = make_opt_res dec_type_annotation type_field in
  let* sym_rpar = first_child_named ")" node in
  let sym_rpar = make_sym sym_rpar in
  Ok { sym_lpar; catch_parameter; type_opt; sym_rpar }

and dec_catch_parameter_kind node : (catch_parameter_kind, _) result =
  match get_name node with
  | "identifier" -> Ok (Catch_identifier (dec_identifier node))
  | "object_pattern" ->
    let* pattern = dec_object_pattern node in
    Ok (Catch_object_pattern pattern)
  | "array_pattern" ->
    let* pattern = dec_array_pattern node in
    Ok (Catch_array_pattern pattern)
  | s -> Error ("dec_catch_parameter_kind: " ^ s)

and dec_type_annotation node : (type_annotation, _) result =
  let* sym_colon = first_child_named ":" node in
  let* type_child = named_child_ranked 0 node in
  let* type_expr = dec_type type_child in
  Ok (make_sym sym_colon, type_expr)

and dec_finally_clause node : (finally_clause, _) result = dec_statement_block node

(* With statement *)

and dec_with_statement node : (with_statement, _) result =
  let* kwd_with = first_child_named "with" node in
  let kwd_with = make_kwd kwd_with in
  let* object_field = child_with_field "object" node in
  let* object_expr = dec_parenthesized_expression object_field in
  let* body_field = child_with_field "body" node in
  let* body = dec_statement body_field in
  Ok { kwd_with; object_expr; body }

(* Break statement *)

and dec_break_statement node : (break_statement, _) result =
  let* kwd_break = first_child_named "break" node in
  let kwd_break = make_kwd kwd_break in
  let label_field = child_with_field_opt "label" node in
  let stmt_id = make_opt dec_identifier label_field in
  Ok { kwd_break; stmt_id }

(* Continue statement *)

and dec_continue_statement node : (continue_statement, _) result =
  let* kwd_continue = first_child_named "continue" node in
  let kwd_continue = make_kwd kwd_continue in
  let label_field = child_with_field_opt "label" node in
  let stmt_id = make_opt dec_identifier label_field in
  Ok { kwd_continue; stmt_id }

(* Return statement *)

and dec_return_statement node : (return_statement, _) result =
  let* kwd_return = first_child_named "return" node in
  let kwd_return = make_kwd kwd_return in
  let expr = child_ranked_opt 1 node in
  let expressions = make_opt dec_expressions expr in
  Ok { kwd_return; expressions }

(* Throw statement *)

and dec_throw_statement node : (throw_statement, _) result =
  let* kwd_throw = first_child_named "throw" node in
  let kwd_throw = make_kwd kwd_throw in
  let* expr = child_ranked 1 node in
  let expressions = dec_expressions expr in
  Ok { kwd_throw; expressions }

(* DECLARATION

   The JavaScript tree-sitter grammar has the non-terminal
   "declaration" be a supertype, that is, a hidden rule. *)

and dec_declaration ?(comments = []) node : (declaration, _) result =
  let comments = comments @ prev_comments node in
  match get_name node with
  | "function_declaration" ->
    let* fun_decl = dec_function_declaration ~comments node in
    Ok (D_function_declaration fun_decl)
  | "generator_function_declaration" ->
    let* generator = dec_generator_function_declaration ~comments node in
    Ok (D_generator_function_declaration generator)
  | "class_declaration" ->
    let* declaration = dec_class_declaration ~comments node in
    Ok (D_class_declaration declaration)
  | "lexical_declaration" ->
    let* declaration = dec_lexical_declaration ~comments node in
    Ok (D_lexical_declaration declaration)
  | "variable_declaration" ->
    let* declaration = dec_variable_declaration node in
    Ok (D_variable_declaration declaration)
  | "function_signature" ->
    let* declaration = dec_function_signature node in
    Ok (D_function_signature declaration)
  | "abstract_class_declaration" ->
    let* declaration = dec_abstract_class_declaration node in
    Ok (D_abstract_class_declaration declaration)
  | "module" ->
    let* declaration = dec_module_declaration node in
    Ok (D_module declaration)
  | "internal_module" ->
    let* declaration = dec_internal_module ~comments node in
    Ok (D_internal_module declaration)
  | "type_alias_declaration" ->
    let* declaration = dec_type_alias_declaration ~comments node in
    Ok (D_type_alias_declaration declaration)
  | "enum_declaration" ->
    let* declaration = dec_enum_declaration node in
    Ok (D_enum_declaration declaration)
  | "interface_declaration" ->
    let* declaration = dec_interface_declaration node in
    Ok (D_interface_declaration declaration)
  | "import_alias" ->
    let* declaration = dec_import_alias node in
    Ok (D_import_alias declaration)
  | "ambient_declaration" ->
    let* declaration = dec_ambient_declaration node in
    Ok (D_ambient_declaration declaration)
  | s -> Error ("dec_declaration: " ^ s)

(* Function declaration (see [dec_function_signature]) *)

and dec_function_declaration ?(comments = []) node : (function_declaration, _) result =
  (* "statement_block" *)
  let* fun_sig = dec_function_signature ~comments node in
  let* body_field = child_with_field "body" node in
  let* body = dec_statement_block body_field in
  Ok { fun_sig; body }

(* Accessibility modifier *)

and dec_accessibility_modifier node : (accessibility_modifier, _) result =
  let* child = child_ranked 0 node in
  match get_name child with
  | "public" -> Ok (Public (make_kwd node))
  | "private" -> Ok (Private (make_kwd node))
  | "protected" -> Ok (Protected (make_kwd node))
  | s -> Error ("dec_accessibility_modifier: " ^ s)

(* Override modifier *)

and dec_override_modifier node : (kwd_override, _) result =
  let* child = first_child_named "override" node in
  (* TODO: Test. See [Print_cst] *)
  Ok (make_kwd child)

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
    let* annotation = dec_type_predicate_annotation node in
    Ok (Type_predicate_annotation annotation)
  | s -> Error ("dec_call_return_type: " ^ s)

(* Asserts annotation *)

and dec_asserts_annotation node : (asserts_annotation, _) result =
  let* asserts = first_child_named "asserts" node in
  dec_asserts asserts

and dec_asserts node : (asserts_annotation, _) result =
  let* kwd_asserts = first_child_named "asserts" node in
  let kwd_asserts = make_kwd kwd_asserts in
  let* child = child_ranked 1 node in
  match get_name child with
  | "type_predicate" ->
    let* predicate = dec_type_predicate node in
    Ok (Assert_predicate (kwd_asserts, predicate))
  | "identifier" -> Ok (Assert_type (kwd_asserts, dec_identifier node))
  | "this" -> Ok (Assert_this (kwd_asserts, make_kwd node))
  | s -> Error ("dec_asserts: " ^ s)

(* Type predicate annotation *)

and dec_type_predicate_annotation node : (type_predicate, _) result =
  let* predicate = child_ranked 1 node in
  dec_type_predicate predicate

(* Type predicate *)

and dec_type_predicate node : (type_predicate, _) result =
  let* name_field = child_with_field "name" node in
  let* name = decode_type_predicate_name name_field in
  let* kwd_is = first_child_named "is" node in
  let kwd_is = make_kwd kwd_is in
  let* type_field = child_with_field "type" node in
  let* type_expr = dec_type type_field in
  Ok { name; kwd_is; type_expr }

and decode_type_predicate_name node : (type_predicate_name, _) result =
  match get_name node with
  | "identifier" -> Ok (Type_predicate_identifier (dec_identifier node))
  | "this" -> Ok (Type_predicate_this (make_kwd node))
  | _ ->
    let* type_expr = dec_predefined_type node in
    Ok (Type_predicate_type type_expr)

(* Predefined type *)

and dec_predefined_type ?(comments = []) node : (predefined_type, _) result =
  let comments = comments @ prev_comments node in
  match collect_children node with
  | [] -> Error "dec_predefined_type: No children."
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
    | "any" -> Ok (T_any (make_kwd ~comments child))
    | "number" -> Ok (T_number (make_kwd ~comments child))
    | "boolean" -> Ok (T_boolean (make_kwd ~comments child))
    | "string" -> Ok (T_string (make_kwd ~comments child))
    | "symbol" -> Ok (T_symbol (make_kwd ~comments child))
    | "unique symbol" -> Ok (T_unique_symbol (make_kwd ~comments child))
    | "void" -> Ok (T_void (make_kwd ~comments child))
    | "unknown" -> Ok (T_unknown (make_kwd ~comments child))
    | "never" -> Ok (T_never (make_kwd ~comments child))
    | "object" -> Ok (T_object (make_kwd ~comments child))
    | s -> Error ("dec_predefined_type: " ^ s))

(* Decorator *)

and dec_decorator ?(comments = []) node : (decorator, _) result =
  let* child = named_child_ranked 0 node in
  match get_name child with
  | "identifier" -> Ok (Decorator_identifier (dec_identifier ~comments node))
  | "member_expression" ->
    let* member_expression = dec_decorator_member_expression ~comments node in
    Ok (Decorator_member_expression member_expression)
  | "call_expression" ->
    let* call_expression = dec_decorator_call_expression ~comments node in
    Ok (Decorator_call_expression call_expression)
  | "parenthesized_expression" ->
    let* expression = dec_decorator_parenthesized_expression ~comments node in
    Ok (Decorator_parenthesized_expression expression)
  | s -> Error ("dec_decorator: " ^ s)

and dec_decorator_member_expression ?(comments = []) node
    : (decorator_member_expression, _) result
  =
  let* object_field = child_with_field "object" node in
  let* object_expr = decode_object_member_expression ~comments object_field in
  let* dot = first_child_named "." node in
  let sym_dot = make_sym dot in
  let* property_field = child_with_field "property" node in
  let property = dec_identifier property_field in
  Ok { object_expr; sym_dot; property }

and decode_object_member_expression ?(comments = []) node
    : (object_member_expression, _) result
  =
  match get_name node with
  | "identifier" -> Ok (Object_name (dec_identifier ~comments node))
  | _ ->
    let* member_expression = dec_decorator_member_expression ~comments node in
    Ok (Qualified_member_expression member_expression : object_member_expression)

and dec_decorator_call_expression ?(comments = []) node
    : (decorator_call_expression, _) result
  =
  let* function_field = child_with_field "function" node in
  let* function_ = decode_function_or_property ~comments function_field in
  let type_arguments_field = child_with_field_opt "type_arguments" node in
  let* type_arguments = make_opt_res dec_type_arguments type_arguments_field in
  let* arguments_field = child_with_field "arguments" node in
  let* arguments = dec_arguments arguments_field in
  Ok { function_; type_arguments; arguments }

and decode_function_or_property ?(comments = []) node : (function_or_property, _) result =
  match get_name node with
  | "identifier" -> Ok (Function_name (dec_identifier ~comments node))
  | "member_expression" ->
    let* member_expression = dec_decorator_member_expression ~comments node in
    Ok (Qualified_member_expression member_expression)
  | s -> Error ("decode_function_or_property: " ^ s)

and dec_decorator_parenthesized_expression ?comments node
    : (decorator_parenthesized_expression parens, _) result
  =
  let decode node =
    match get_name node with
    | "identifier" -> Ok (Parenthesized_ident (dec_identifier node))
    | "member_expression" ->
      let* member_expression = dec_decorator_member_expression node in
      Ok (Parenthesized_member member_expression)
    | _ ->
      let* call_expression = dec_decorator_call_expression node in
      Ok (Parenthesized_call call_expression)
  in
  decode_parens_res ?comments node decode

(* Type arguments *)

and dec_type_arguments ?comments node : (type_arguments, _) result =
  decode_ne_list_in_chevrons_res ?comments node dec_type

(* Function arguments *)

and dec_arguments ?comments node : (arguments, _) result =
  decode_list_in_parens_res ?comments node dec_argument

and dec_argument ?comments node : (argument, _) result =
  let* expression = dec_expression ?comments node in
  match get_name node with
  | "spread_element" -> Ok (Spread_element expression)
  | _ -> Ok (Expression expression)

(* Generator function declaration (see function declaration) *)

and dec_generator_function_declaration ?(comments = []) node
    : (generator_function_declaration, _) result
  =
  let* fun_decl = dec_function_declaration ~comments node in
  let* sym_star = first_child_named "*" node in
  Ok (make_sym sym_star, fun_decl)

(* Class declaration (see [dec_class]) *)

and dec_class_declaration ?(comments = []) node : (class_declaration, _) result =
  let comments = comments @ prev_comments node in
  let decorators = children_named "decorator" node in
  let* decorators = list_of_children_res dec_decorator decorators in
  let* kwd_class = first_child_named "class" node in
  let kwd_class = make_kwd ~comments kwd_class in
  let* name_field = child_with_field "name" node in
  let name = dec_identifier name_field in
  let type_parameters_field = child_with_field_opt "type_parameters" node in
  let* type_parameters = make_opt_res dec_type_parameters type_parameters_field in
  let heritage_child = first_child_named_opt "class_heritage" node in
  let* class_heritage = make_opt_res dec_class_heritage heritage_child in
  let* body_field = child_with_field "body" node in
  let* body = dec_class_body body_field in
  Ok { decorators; kwd_class; name; type_parameters; class_heritage; body }

and dec_class_heritage node : (class_heritage, _) result =
  match first_child_named_opt "extends_clause" node with
  | Some extends_clause ->
    let* extends_clause = dec_extends_clause extends_clause in
    let implements_clause = first_child_named_opt "implements_clause" node in
    let* implements_clause = make_opt_res dec_implements_clause implements_clause in
    Ok (Extends_clause (extends_clause, implements_clause))
  | None ->
    let* implements_clause = first_child_named "implements_clause" node in
    let* implements_clause = dec_implements_clause implements_clause in
    Ok (Implements_clause implements_clause)

and dec_extends_clause node : (extends_clause, _) result =
  let* kwd_extends = first_child_named "extends" node in
  let kwd_extends = make_kwd kwd_extends in
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
  let mk_clause (value, type_arguments_opt) : (extends_clause_single, string) result =
    let* value = dec_expression value in
    let* type_arguments =
      match type_arguments_opt with
      | None -> Ok None
      | Some type_arguments ->
        let* args = dec_type_arguments type_arguments in
        Ok (Some args)
    in
    Ok { value; type_arguments }
  in
  let* extends_clauses = Result.all @@ List.map ~f:mk_clause pairs in
  let* extends_clauses =
    match extends_clauses with
    | [] -> Error "dec_extends_clause: One clause is expected."
    | clause :: clauses -> Ok Nonempty_list.(clause :: clauses)
  in
  Ok (kwd_extends, extends_clauses)

and dec_implements_clause node : (implements_clause, _) result =
  let* kwd_implements = first_child_named "implements" node in
  let kwd_implements = make_kwd kwd_implements in
  let raw_clauses = collect_named_children node in
  let* type_exprs = ne_list_of_children_res dec_type raw_clauses in
  Ok (kwd_implements, type_exprs)

and dec_class_body ?(comments = []) node : (class_body, _) result =
  let comments = comments @ prev_comments node in
  let* opening = first_child_named "{" node in
  let opening = make_sym ~comments opening in
  let* closing = first_child_named "}" node in
  let closing = make_sym closing in
  let named_children = collect_named_children node in
  let pair (decorators, acc) child =
    match get_name child with
    | "decorator" -> child :: decorators, acc
    | _ -> [], (List.rev decorators, child) :: acc
  in
  let _, pairs = List.fold_left ~f:pair ~init:([], []) named_children in
  let contents = List.map ~f:decode_class_member @@ List.rev pairs in
  let* contents = Result.all contents in
  Ok (Braces { opening; contents; closing })

and decode_class_member ?(comments = []) (decorators, node) : (class_member, _) result =
  match get_name node with
  | "method_definition" ->
    let* decorators = list_of_children_res dec_decorator decorators in
    let* definition = dec_method_definition ~comments node in
    (* Not ideal *)
    Ok (Method_definition (decorators, definition))
  | "method_signature" ->
    let* signature = dec_method_signature node in
    Ok (Method_signature signature : class_member)
  | "class_static_block" ->
    let* static_block = dec_class_static_block node in
    Ok (Call_static_block static_block)
  | "abstract_method_signature" ->
    let* signature = dec_abstract_method_signature node in
    Ok (Abstract_method_signature signature)
  | "index_signature" ->
    let* signature = dec_index_signature node in
    Ok (Index_signature signature : class_member)
  | "public_field_definition" ->
    let* definition = dec_public_field_definition node in
    Ok (Public_field_definition definition)
  | s -> Error ("decode_class_member: " ^ s)

(* Method definition *)

and dec_method_definition ?(comments = []) node : (method_definition, _) result =
  let* signature = dec_method_signature ~comments node in
  let* body_field = child_with_field "body" node in
  let* body = dec_statement_block body_field in
  Ok { signature; body }

(* Method signature *)

and dec_method_signature ?(comments = []) node : (method_signature, _) result =
  let accessibility_modifier = first_child_named_opt "accessibility_modifier" node in
  let* access = make_opt_res dec_accessibility_modifier accessibility_modifier in
  let* scope = dec_method_scope node in
  let kwd_async = first_child_named_opt "async" node in
  let kwd_async = make_opt make_kwd kwd_async in
  let set_get_all = mk_set_get_all node in
  let* name_field = child_with_field "name" node in
  let* name = dec_property_name ~comments name_field in
  let sym_qmark = first_child_named_opt "?" node in
  let optional = make_opt make_sym sym_qmark in
  let* call_sig = dec_call_signature node in
  Ok { access; scope; kwd_async; set_get_all; name; optional; call_sig }

(* Method scope *)

and dec_method_scope node : (method_scope, _) result =
  let kwd_static = first_child_named_opt "static" node in
  let kwd_static = make_opt make_kwd kwd_static in
  let override_modifier = first_child_named_opt "override_modifier" node in
  let* kwd_override = make_opt_res dec_override_modifier override_modifier in
  let kwd_readonly = first_child_named_opt "readonly" node in
  let kwd_readonly = make_opt make_kwd kwd_readonly in
  Ok { kwd_static; kwd_override; kwd_readonly }

(* Class static block *)

and dec_class_static_block ?(comments = []) node
    : (kwd_static * statement_block, _) result
  =
  let* kwd_static = first_child_named "static" node in
  let kwd_static = make_kwd ~comments kwd_static in
  let* body_field = child_with_field "bodya" node in
  let* block = dec_statement_block body_field in
  Ok (kwd_static, block)

(* Abstract method signature *)

and dec_abstract_method_signature ?(comments = []) node
    : (abstract_method_signature, _) result
  =
  let accessibility_modifier = first_child_named_opt "accessibility_modifier" node in
  let* access = make_opt_res dec_accessibility_modifier accessibility_modifier in
  let* kwd_abstract = first_child_named "abstract" node in
  let kwd_abstract = make_kwd kwd_abstract in
  let override_modifier = first_child_named_opt "override_modifier" node in
  let* kwd_override = make_opt_res dec_override_modifier override_modifier in
  let set_get_all = mk_set_get_all node in
  let* name_field = child_with_field "name" node in
  (* Not ideal *)
  let* name = dec_property_name ~comments name_field in
  let sym_qmark = first_child_named_opt "?" node in
  let optional = make_opt make_sym sym_qmark in
  let* call_sig = dec_call_signature node in
  Ok { access; kwd_abstract; kwd_override; set_get_all; name; optional; call_sig }

(* Call signature *)

and dec_call_signature node : (call_signature, _) result =
  let type_parameters_field = child_with_field_opt "type_parameters" node in
  let* type_parameters = make_opt_res dec_type_parameters type_parameters_field in
  let* parameters_field = child_with_field "parameters" node in
  let* parameters = dec_formal_parameters parameters_field in
  let return_type_field = child_with_field_opt "return_type" node in
  let* return_type = make_opt_res dec_call_return_type return_type_field in
  Ok ({ type_parameters; parameters; return_type } : call_signature)

(* Index signature *)

and dec_index_signature ?(comments = []) node : (index_signature, _) result =
  let kwd_readonly = first_child_named_opt "readonly" node in
  let kwd_readonly = make_opt make_kwd kwd_readonly in
  let sign_field = child_with_field_opt "sign" node in
  let* sign = make_opt_res dec_sign sign_field in
  let sign =
    match kwd_readonly with
    | None -> None
    | Some kwd -> Some (sign, kwd)
  in
  let name_field = child_with_field_opt "name" node in
  let* type_field = child_with_field "type" node in
  let* annotation = decode_index_annotation type_field in
  let* sym_lbracket = first_child_named "[" node in
  (* Not ideal *)
  let opening = make_sym ~comments sym_lbracket in
  let* sym_rbracket = first_child_named "]" node in
  let closing = make_sym sym_rbracket in
  let* (range : index_range) =
    match name_field with
    | Some name_field ->
      let name = dec_type_identifier name_field in
      let* sym_colon = first_child_named ":" node in
      let sym_colon = make_sym sym_colon in
      let* index_type_field = child_with_field "index_type" node in
      let* index_type = dec_type index_type_field in
      Ok (Typed_index_clause { name; sym_colon; index_type })
    | None ->
      let* mapped_type_clause = named_child_ranked 0 node in
      let* mapped_type_clause = dec_mapped_type_clause mapped_type_clause in
      Ok (Mapped_type_clause mapped_type_clause)
  in
  let range = Brackets { opening; contents = range; closing } in
  Ok { sign; range; annotation }

and dec_mapped_type_clause node : (mapped_type_clause, _) result =
  let* name_field = child_with_field "name" node in
  let name = dec_type_identifier name_field in
  let* kwd_in = first_child_named "in" node in
  let kwd_in = make_kwd kwd_in in
  let* type_field = child_with_field "type" node in
  let* type_expr = dec_type type_field in
  let alias_field = child_with_field_opt "alias" node in
  let* alias =
    match alias_field with
    | None -> Ok None
    | Some alias ->
      let* kwd_as = first_child_named "as" node in
      let* type_expr = dec_type alias in
      Ok (Some (make_kwd kwd_as, type_expr))
  in
  Ok { name; kwd_in; type_expr; alias }

and dec_omitting_type_annotation node : (symbol * type_expr, _) result =
  let* sym_kind = first_child_named "-?:" node in
  let* type_child = named_child_ranked 0 node in
  let* type_expr = dec_type type_child in
  Ok (make_kwd sym_kind, type_expr)

and dec_adding_type_annotation node : (symbol * type_expr, _) result =
  let* sym_kind = first_child_named "+?:" node in
  let* type_child = named_child_ranked 0 node in
  let* type_expr = dec_type type_child in
  Ok (make_kwd sym_kind, type_expr)

and dec_opting_type_annotation node : (symbol * type_expr, _) result =
  let* sym_kind = first_child_named "?:" node in
  let* type_child = named_child_ranked 0 node in
  let* type_expr = dec_type type_child in
  Ok (make_kwd sym_kind, type_expr)

and decode_index_annotation node : (index_annotation, _) result =
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
  | s -> Error ("dec_index_annotation: " ^ s)

and dec_sign node : (sign, _) result =
  match get_name node with
  | "+" -> Ok (Plus (make_sym node))
  | "-" -> Ok (Minus (make_sym node))
  | s -> Error ("dec_sign: " ^ s)

(* Public field definition *)

and dec_public_field_definition ?(comments = []) node
    : (public_field_definition, _) result
  =
  let decorators = children_named "decorator" node in
  let* decorators = list_of_children_res dec_decorator decorators in
  let accessibility_modifier = first_child_named_opt "accessibility_modifier" node in
  let* access = make_opt_res dec_accessibility_modifier accessibility_modifier in
  let kwd_declare = first_child_named_opt "declare" node in
  let kwd_declare = make_opt make_kwd kwd_declare in
  let* scope = decode_field_scope node in
  let* name_field = child_with_field "name" node in
  let* name = dec_property_name ~comments name_field in
  let mode = decode_field_mode_opt node in
  let type_field = child_with_field_opt "type" node in
  let* type_ = make_opt_res dec_type_annotation type_field in
  let* default = mk_child_initializer_opt node in
  Ok { decorators; access; kwd_declare; scope; name; mode; type_; default }

and decode_field_mode_opt node : field_mode option =
  let sym_qmark = first_child_named_opt "?" node in
  match sym_qmark with
  | Some sym -> Some (Optional (make_sym sym))
  | None ->
    (match first_child_named_opt "!" node with
    | None -> None
    | Some sym -> Some (Definite_assert (make_sym sym)))

and decode_field_scope node : (field_scope, _) result =
  let override_modifier = first_child_named_opt "override_modifier" node in
  let* kwd_override = make_opt_res dec_override_modifier override_modifier in
  let kwd_abstract = first_child_named_opt "abstract" node in
  let kwd_abstract = make_opt make_kwd kwd_abstract in
  let kwd_static = first_child_named_opt "static" node in
  let kwd_static = make_opt make_kwd kwd_static in
  let kwd_readonly = first_child_named_opt "readonly" node in
  let kwd_readonly = make_opt make_kwd kwd_readonly in
  let kwd_accessor = first_child_named_opt "accessor" node in
  let kwd_accessor = make_opt make_kwd kwd_accessor in
  Ok { kwd_static; kwd_override; kwd_readonly; kwd_abstract; kwd_accessor }

(* Lexical declaration (see [dec_variable_declaration]) *)

and dec_lexical_declaration ?(comments = []) node : (lexical_declaration, _) result =
  let comments = comments @ prev_comments node in
  let* kind_field = child_with_field "kind" node in
  let decls = children_named "variable_declarator" node in
  let* decls = ne_list_of_children_res dec_variable_declarator decls in
  let* kind =
    match get_name kind_field with
    | "let" -> Ok (Let (make_kwd ~comments kind_field))
    | "const" -> Ok (Const (make_kwd ~comments kind_field))
    | s -> Error ("dec_lexical_declaration: " ^ s)
  in
  Ok { kind; decls }

(* Variable declaration (see [dec_lexical_declaration]) *)

and dec_variable_declaration ?(comments = []) node : (variable_declaration, _) result =
  let comments = comments @ prev_comments node in
  let* kwd_var = first_child_named "var" node in
  let var_decls = children_named "variable_declarator" node in
  let* var_decls = ne_list_of_children_res dec_variable_declarator var_decls in
  Ok (make_sym ~comments kwd_var, var_decls)

and dec_variable_declarator ?comments node : (variable_declarator, _) result =
  let* name_field = child_with_field "name" node in
  match get_name name_field with
  | "identifier" -> Ok (Decl_ident (dec_identifier ?comments name_field))
  | _ ->
    let* pattern = dec_destructuring_pattern ?comments name_field in
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
  let kwd_async = make_opt (make_kwd ~comments:async_comments) kwd_async in
  let* kwd_function = first_child_named "function" node in
  let kwd_function = make_kwd ~comments:function_comments kwd_function in
  let* name_field = child_with_field "name" node in
  let name = dec_identifier name_field in
  let* call_sig = dec_call_signature node in
  Ok { kwd_async; kwd_function; name; call_sig }

(* Formal parameters *)

and dec_formal_parameters node : (formal_parameters, _) result =
  decode_list_in_parens_res node dec_formal_parameter

and dec_formal_parameter ?(comments = []) node : (formal_parameter, _) result =
  let comments = comments @ prev_comments node in
  (* "_parameter_name" inlined: *)
  let decorators = children_named "decorator" node in
  let* decorators = list_of_children_res dec_decorator decorators in
  let accessibility_modifier = first_child_named_opt "accessibility_modifier" node in
  let* access = make_opt_res dec_accessibility_modifier accessibility_modifier in
  let override_modifier = first_child_named_opt "override_modifier" node in
  let* kwd_override = make_opt_res dec_override_modifier override_modifier in
  let kwd_readonly = first_child_named_opt "readonly" node in
  let kwd_readonly = make_opt make_kwd kwd_readonly in
  let* pattern_field = child_with_field "pattern" node in
  let* pattern = decode_parameter_pattern ~comments pattern_field (* Not ideal *) in
  (* *)
  let parameter_name = { decorators; access; kwd_override; kwd_readonly; pattern } in
  let qmark = first_child_named_opt "?" node in
  let optional = make_opt make_sym qmark in
  let type_field = child_with_field_opt "type" node in
  let* type_opt = make_opt_res dec_type_annotation type_field in
  let* default = mk_child_initializer_opt node in
  Ok { parameter_name; optional; type_opt; default }

and decode_parameter_pattern ?(comments = []) node : (parameter_pattern, _) result =
  match get_name node with
  | "this" -> Ok (Parameter_this (make_kwd ~comments node))
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
  let* value_field = child_with_field "value" node in
  let* expression = dec_expression value_field in
  Ok (make_sym sym_equal, expression)

(* Abstract class declaration

   The difference with [dec_class_declaration] is the decoding of the
   keyword "abstract". The AST of abstract class definitions do not
   reuse that for class definitions because of the handling of the
   comments, which should be hooked either on the keyword "class" or
   "abstract". *)

and dec_abstract_class_declaration ?(comments = []) node
    : (abstract_class_declaration, _) result
  =
  let comments = comments @ prev_comments node in
  let decorators = children_named "decorator" node in
  let* decorators = list_of_children_res dec_decorator decorators in
  let* kwd_abstract = first_child_named "abstract" node in
  let kwd_abstract = make_kwd ~comments kwd_abstract in
  let* kwd_class = first_child_named "class" node in
  let kwd_class = make_kwd kwd_class in
  let* name_field = child_with_field "name" node in
  let name = dec_identifier name_field in
  let type_parameters_field = child_with_field_opt "type_parameters" node in
  let* type_parameters = make_opt_res dec_type_parameters type_parameters_field in
  let heritage_child = first_child_named_opt "class_heritage" node in
  let* class_heritage = make_opt_res dec_class_heritage heritage_child in
  let* body_field = child_with_field "body" node in
  let* body = dec_class_body body_field in
  Ok { decorators; kwd_abstract; kwd_class; name; type_parameters; class_heritage; body }

(* Module *)

and dec_module_declaration ?(comments = []) node : (module_declaration, _) result =
  let comments = comments @ prev_comments node in
  let* kwd_module = first_child_named "module" node in
  let kwd_module = make_kwd ~comments kwd_module in
  let* name_field = child_with_field "name" node in
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
    Ok (Module_nested nested)
  | s -> Error ("dec_module_name: " ^ s)

(* Internal module (a.k.a. namespaces) *)

and dec_internal_module ?(comments = []) node : (internal_module, _) result =
  let comments = comments @ prev_comments node in
  let* kwd_namespace = first_child_named "namespace" node in
  let kwd_namespace = make_kwd ~comments kwd_namespace in
  let* name_field = child_with_field "name" node in
  let* module_name = dec_module_name name_field in
  let body_field = child_with_field_opt "body" node in
  let* module_body = make_opt_res dec_statement_block body_field in
  Ok { kwd_namespace; module_name; module_body }

(* Type alias declaration *)

and dec_type_alias_declaration ?(comments = []) node : (type_alias_declaration, _) result =
  let comments = comments @ prev_comments node in
  let* kwd_type = first_child_named "type" node in
  let kwd_type = make_kwd ~comments kwd_type in
  let* name_field = child_with_field "name" node in
  let name = dec_type_identifier name_field in
  let* sym_equal = first_child_named "=" node in
  let sym_equal = make_sym sym_equal in
  let type_parameters_field = child_with_field_opt "type_parameters" node in
  let* type_parameters = make_opt_res dec_type_parameters type_parameters_field in
  let* value_field = child_with_field "value" node in
  let* type_expr = dec_type value_field in
  Ok { kwd_type; name; type_parameters; sym_equal; type_expr }

(* Type parameters *)

and dec_type_parameters node : (type_parameters, _) result =
  decode_list_in_chevrons_res node dec_type_parameter

and dec_type_parameter ?(comments = []) node : (type_parameter, _) result =
  let comments = comments @ prev_comments node in
  let kwd_const = first_child_named_opt "const" node in
  let kwd_const = make_opt make_kwd kwd_const in
  let* name_field = child_with_field "name" node in
  let name = dec_type_identifier ~comments name_field (* Not ideal *) in
  let constraint_field = child_with_field_opt "constraint" node in
  let* constraint_expr = make_opt_res dec_constraint constraint_field in
  let value_field = child_with_field_opt "value" node in
  let* default_type = make_opt_res dec_default_type value_field in
  Ok { kwd_const; name; constraint_expr; default_type }

and dec_type_identifier ?comments node : type_identifier = dec_identifier ?comments node

and dec_constraint node : (kwd_extends * type_expr, _) result =
  let* kwd_extends = first_child_named "extends" node in
  let* type_child = child_ranked 1 node in
  let* type_expr = dec_type type_child in
  Ok (make_kwd kwd_extends, type_expr)

and dec_default_type node : (sym_equal * type_expr, _) result =
  let* sym_equal = first_child_named "=" node in
  let* type_node = child_ranked 1 node in
  let* type_expr = dec_type type_node in
  Ok (make_sym sym_equal, type_expr)

(* Enum declaration *)

and dec_enum_declaration node : (enum_declaration, _) result =
  let kwd_const = first_child_named_opt "const" node in
  let kwd_const = make_opt make_kwd kwd_const in
  let* kwd_enum = first_child_named "enum" node in
  let kwd_enum = make_kwd kwd_enum in
  let* name_field = child_with_field "name" node in
  let name = dec_identifier name_field in
  let* body_field = child_with_field "body" node in
  let* body = dec_enum_entries body_field in
  Ok { kwd_const; kwd_enum; name; body }

and dec_enum_entries node : (enum_body list braces, _) result =
  decode_list_in_braces_res node dec_enum_body

and dec_enum_body ?(comments = []) node : (enum_body, _) result =
  match get_name node with
  | "enum_assignment" ->
    let* assignment = dec_enum_assignment ~comments node in
    Ok (Enum_assignment assignment)
  | _ ->
    let* property = dec_property_name ~comments node in
    Ok (Enum_name property)

and dec_enum_assignment ?comments node : (enum_assignment, _) result =
  let* name_field = child_with_field "name" node in
  let* name = dec_property_name ?comments name_field in
  let* sym_equal = first_child_named "=" node in
  let* default = mk_child_initializer sym_equal node in
  Ok { name; default }

(* Property names *)

and dec_property_name ?(comments = []) node : (property_name, _) result =
  match get_name node with
  | "property_identifier" -> Ok (Property_identifier (dec_identifier ~comments node))
  | "private_property_identifier" ->
    Ok (Private_property_identifier (dec_private_property_identifier ~comments node))
  | "string" -> Ok (String (dec_string ~comments node))
  | "number" -> Ok (Number (dec_number ~comments node))
  | "computed_property_name" ->
    Ok (Computed_property_name (dec_computed_property_name ~comments node))
  | s -> Error ("dec_property_name: " ^ s)

and dec_private_property_identifier ?(comments = []) node : private_property_identifier =
  dec_identifier ~comments node

and dec_computed_property_name ?comments node : expression brackets =
  ensure_Ok node @@ decode_brackets_res ?comments node dec_expression

(* Interface declaration *)

and dec_interface_declaration node : (interface_declaration, _) result =
  let* kwd_interface = first_child_named "interface" node in
  let kwd_interface = make_kwd kwd_interface in
  let* name_field = child_with_field "name" node in
  let name = dec_type_identifier name_field in
  let type_parameters_field = child_with_field_opt "type_parameters" node in
  let* type_parameters = make_opt_res dec_type_parameters type_parameters_field in
  let extends_type_clause = first_child_named_opt "extends_type_clause" node in
  let* extends = make_opt_res dec_extends_type_clause extends_type_clause in
  let* body_field = child_with_field "body" node in
  let* body = dec_object_type body_field in
  Ok { kwd_interface; name; type_parameters; extends; body }

and dec_extends_type_clause node : (extends_type_clause, _) result =
  let* kwd_extends = first_child_named "extends" node in
  let named_children = collect_named_children node in
  let* extensions = ne_list_of_children_res dec_type_extension named_children in
  Ok { kwd_extends = make_kwd kwd_extends; extensions }

and dec_type_extension ?(comments = []) node : (type_extension, _) result =
  match get_name node with
  | "type_identifier" -> Ok (Extends_type (dec_type_identifier ~comments node))
  | "nested_type_identifier" ->
    let* nested = dec_nested_type_identifier ~comments node in
    Ok (Extends_nested nested)
  | "generic_type" ->
    let* type_expr = dec_generic_type ~comments node in
    Ok (Extends_generic type_expr)
  | s -> Error ("dec_type_extension: " ^ s)

(* Nested type identifier *)

and dec_nested_type_identifier ?(comments = []) node : (nested_type_identifier, _) result =
  let* module_field = child_with_field "module" node in
  let* name_field = child_with_field "name" node in
  let* path = decode_module_path ~comments module_field in
  let path = Nonempty_list.reverse path in
  Ok (path, dec_type_identifier name_field)

and decode_module_path ?(comments = []) node : (identifier ne_list, _) result =
  match get_name node with
  | "identifier" -> Ok Nonempty_list.[ dec_type_identifier ~comments node ]
  | "nested_identifier" ->
    let* path, id = dec_nested_identifier ~comments node in
    Ok (Nonempty_list.cons id path)
  | s -> Error ("decode_module_path: " ^ s)

(* Import alias *)

and dec_import_alias ?(comments = []) node : (import_alias, _) result =
  let* kwd_import = first_child_named "import" node in
  let kwd_import = make_kwd ~comments kwd_import in
  let* lhs = child_ranked 1 node in
  let alias = dec_identifier lhs in
  let* sym_equal = first_child_named "=" node in
  let sym_equal = make_sym sym_equal in
  let* rhs = child_ranked 3 node in
  let* aliased = decode_aliased rhs in
  Ok { kwd_import; alias; sym_equal; aliased }

and decode_aliased node : (aliased, _) result =
  match get_name node with
  | "identifier" -> Ok (Ident (dec_identifier node))
  | "nested_identifier" ->
    let* nested = dec_nested_identifier node in
    Ok (Nested nested)
  | s -> Error ("decode_aliased: " ^ s)

(* Nested identifier *)

and dec_nested_identifier ?(comments = []) node : (nested_identifier, _) result =
  let* object_field = child_with_field "object" node in
  let* property_field = child_with_field "property" node in
  let* path = decode_object_path ~comments object_field in
  let path = Nonempty_list.reverse path in
  let* property = decode_property property_field in
  Ok (path, property)

and decode_property node : (identifier, _) result =
  match get_name node with
  | "property_identifier" -> Ok (dec_identifier node)
  | s -> Error ("decode_property: " ^ s)

and decode_object_path ?(comments = []) node : (identifier ne_list, _) result =
  match get_name node with
  | "identifier" -> Ok Nonempty_list.[ dec_identifier ~comments node ]
  | "member_expression" ->
    let* path, id = dec_nested_identifier ~comments node in
    Ok (Nonempty_list.cons id path)
  | s -> Error ("decode_object_path: " ^ s)

(* Ambient declaration *)

and dec_ambient_declaration ?comments node : (ambient_declaration, _) result =
  let* kwd_declare = first_child_named "declare" node in
  let kwd_declare = make_kwd ?comments kwd_declare in
  let* fst_child = named_child_ranked 0 node in
  let* ambient_kind =
    match get_name fst_child with
    | "statement_block" ->
      let* kwd_global = first_child_named "global" node in
      let* block = dec_statement_block fst_child in
      Ok (Global_declaration (make_kwd kwd_global, block))
    | "property_identifier" ->
      let* kwd_module = first_child_named "module" node in
      let* type_child = child_ranked 5 node in
      let keyword = make_kwd kwd_module
      and identifier = dec_identifier fst_child in
      let* type_expr = dec_type type_child in
      Ok (Module_declaration (keyword, identifier, type_expr))
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
  (* Rest of "expression": *)
  (*  | "glimmer_template" -> Ok (E_glimmer_template (dec_glimmer_template node)) *)
  | "assignment_expression" ->
    let* expression = dec_assignment_expression node in
    Ok (E_assignment_expression expression)
  | "augmented_assignment_expression" ->
    let* expression = dec_augmented_assignment_expression node in
    Ok (E_augmented_assignment_expression expression)
  | "await_expression" ->
    let* expression = dec_await_expression node in
    Ok (E_await_expression expression)
  | "unary_expression" ->
    let* expression = dec_unary_expression node in
    Ok (E_unary_expression expression)
  | "binary_expression" ->
    let* expression = dec_binary_expression ~comments node in
    Ok (E_binary_expression expression)
  | "ternary_expression" ->
    let* expression = dec_ternary_expression node in
    Ok (E_ternary_expression expression)
  | "update_expression" ->
    let* expression = dec_update_expression node in
    Ok (E_update_expression expression)
  | "new_expression" ->
    let* expression = dec_new_expression node in
    Ok (E_new_expression expression)
  | "yield_expression" ->
    let* expression = dec_yield_expression node in
    Ok (E_yield_expression expression)
  | "as_expression" ->
    let* expression = dec_as_expression node in
    Ok (E_as_expression expression)
  | "satisfies_expression" ->
    let* expression = dec_satisfies_expression node in
    Ok (E_satisfies_expression expression)
  | "instantiation_expression" ->
    let* expression = dec_instantiation_expression node in
    Ok (E_instantiation_expression expression)
  | "internal_module" ->
    let* declaration = dec_internal_module ~comments node in
    Ok (E_internal_module declaration)
  | "type_assertion" ->
    let* assertion = dec_type_assertion node in
    Ok (E_type_assertion assertion)
  | _ ->
    let* expression = dec_primary_expression ~comments node in
    Ok (E_primary_expression expression)

(* Assignment expression *)

and dec_assignment_expression node : (assignment_expression, _) result =
  let kwd_using = first_child_named_opt "using" node in
  let kwd_using = make_opt make_kwd kwd_using in
  let* left_field = child_with_field "left" node in
  let* left = decode_assignment_lhs left_field in
  let* sym_equal = first_child_named "=" node in
  let sym_equal = make_sym sym_equal in
  let* right_field = child_with_field "right" node in
  let* right = dec_expression right_field in
  Ok { kwd_using; left; sym_equal; right }

and decode_assignment_lhs node : (assignment_lhs, _) result =
  match get_name node with
  | "parenthesized_expression" ->
    let* expression = dec_parenthesized_expression node in
    Ok (Assign_lhs_parens expression)
  | _ ->
    let* expression = dec_lhs_expression node in
    Ok (Assign_lhs expression)

(* Augmented assignment expression *)

and dec_augmented_assignment_expression node : (augmented_assignment_expression, _) result
  =
  let* left_field = child_with_field "left" node in
  let* left = decode_augmented_assignment_lhs left_field in
  let* operator = child_with_field "operator" node in
  let* operator = decode_assignment_operator operator in
  let* right_field = child_with_field "right" node in
  let* right = dec_expression right_field in
  Ok { left; operator; right }

and decode_assignment_operator node : (assignment_operator, _) result =
  match get_name node with
  | "+=" -> Ok (Add_eq (make_sym node))
  | "-=" -> Ok (Sub_eq (make_sym node))
  | "*=" -> Ok (Mult_eq (make_sym node))
  | "/=" -> Ok (Div_eq (make_sym node))
  | "%=" -> Ok (Rem_eq (make_sym node))
  | "^=" -> Ok (Bit_xor_eq (make_sym node))
  | "&=" -> Ok (Bit_and_eq (make_sym node))
  | "|=" -> Ok (Bit_or_eq (make_sym node))
  | ">>=" -> Ok (Bit_sr_eq (make_sym node))
  | ">>>=" -> Ok (Bit_usr_eq (make_sym node))
  | "<<=" -> Ok (Bit_sl_eq (make_sym node))
  | "**=" -> Ok (Exp_eq (make_sym node))
  | "&&=" -> Ok (Log_and_eq (make_sym node))
  | "||=" -> Ok (Log_or_eq (make_sym node))
  | "??=" -> Ok (Non_null_eq (make_sym node))
  | s -> Error ("decode_assignment_operator: " ^ s)

and decode_augmented_assignment_lhs node : (augmented_assignment_lhs, _) result =
  match get_name node with
  | "member_expression" ->
    let* expression = dec_member_expression node in
    Ok (Member_expression expression)
  | "subscript_expression" ->
    let* expression = dec_subscript_expression node in
    Ok (Subscript_expression expression)
  | "identifier" -> Ok (Identifier (dec_identifier node))
  | "parenthesized_expression" ->
    let* expression = dec_parenthesized_expression node in
    Ok (Parenthesized_expression expression)
  | s -> Error ("decode_augmented_assignment_lhs: " ^ s)

(* Await expression *)

and dec_await_expression node : (await_expression, _) result =
  let* kwd_await = first_child_named "await" node in
  let kwd_await = make_kwd kwd_await in
  let* expression = child_ranked 1 node in
  let* expression = dec_expression expression in
  Ok { kwd_await; expression }

(* Unary expression *)

and dec_unary_expression node : (unary_expression, _) result =
  let* operator_field = child_with_field "operator" node in
  let* operator = decode_unary_operator operator_field in
  let* argument_field = child_with_field "argument" node in
  let* argument = dec_expression argument_field in
  Ok ({ operator; argument } : unary_expression)

and decode_unary_operator node : (unary_operator, _) result =
  match get_name node with
  | "!" -> Ok (Bang (make_sym node))
  | "~" -> Ok (Not (make_sym node))
  | "-" -> Ok (Unary_sub (make_sym node))
  | "+" -> Ok (Unary_add (make_sym node))
  | "typeof" -> Ok (Typeof (make_kwd node))
  | "void" -> Ok (Void (make_kwd node))
  | "delete" -> Ok (Delete (make_kwd node))
  | s -> Error ("decode_unary_operator: " ^ s)

(* Binary expression *)

and dec_binary_expression ?(comments = []) node : (binary_expression, _) result =
  let comments = comments @ prev_comments node in
  let* left_field = child_with_field "left" node in
  let* lhs_expr = decode_lhs_bin_expression ~comments left_field in
  let* operator = child_with_field "operator" node in
  let* operator = decode_binary_operator operator in
  let* right_field = child_with_field "right" node in
  let* rhs_expr = dec_expression right_field in
  Ok { lhs_expr; operator; rhs_expr }

and decode_lhs_bin_expression ~comments node : (lhs_bin_expression, _) result =
  match get_name node with
  | "private_property_identifier" ->
    Ok (Lhs_bin_hash (dec_private_property_identifier ~comments node))
  | _ ->
    let* hash = dec_expression ~comments node in
    Ok (Lhs_bin_expression hash)

and decode_binary_operator node : (binary_operator, _) result =
  match get_name node with
  | "&&" -> Ok (Log_and (make_sym node))
  | "||" -> Ok (Log_or (make_sym node))
  | ">>" -> Ok (Bit_sr (make_sym node))
  | ">>>" -> Ok (Bit_usr (make_sym node))
  | "<<" -> Ok (Bit_sl (make_sym node))
  | "&" -> Ok (Bit_and (make_sym node))
  | "^" -> Ok (Bit_xor (make_sym node))
  | "|" -> Ok (Bit_or (make_sym node))
  | "+" -> Ok (Add (make_sym node))
  | "-" -> Ok (Sub (make_sym node))
  | "*" -> Ok (Mult (make_sym node))
  | "/" -> Ok (Div (make_sym node))
  | "%" -> Ok (Rem (make_sym node))
  | "**" -> Ok (Exp (make_sym node))
  | "<" -> Ok (Lt (make_sym node))
  | "<=" -> Ok (Leq (make_sym node))
  | "==" -> Ok (Equal (make_sym node))
  | "===" -> Ok (Strict_eq (make_sym node))
  | "!=" -> Ok (Neq (make_sym node))
  | "!==" -> Ok (Strict_neq (make_sym node))
  | ">=" -> Ok (Geq (make_sym node))
  | ">" -> Ok (Gt (make_sym node))
  | "??" -> Ok (Non_null (make_sym node))
  | "instanceof" -> Ok (Instance_of (make_kwd node))
  | "in" -> Ok (In (make_kwd node))
  | s -> Error ("decode_binary_operator: " ^ s)

(* Ternary expression *)

and dec_ternary_expression node : (ternary_expression, _) result =
  let* condition_field = child_with_field "condition" node in
  let* condition = dec_expression condition_field in
  let* sym_qmark = first_child_named "?" node in
  let sym_qmark = make_sym sym_qmark in
  let* consequence_field = child_with_field "consequence" node in
  let* consequence = dec_expression consequence_field in
  let* sym_colon = first_child_named ":" node in
  let sym_colon = make_sym sym_colon in
  let* alternative_field = child_with_field "alternative" node in
  let* alternative = dec_expression alternative_field in
  Ok { condition; sym_qmark; consequence; sym_colon; alternative }

(* Update expression *)

and dec_update_expression node : (update_expression, _) result =
  let* argument_field = child_with_field "argument" node in
  let* argument = dec_expression argument_field in
  let* operator_field = child_with_field "operator" node in
  let* operator = decode_incr_decr_operator operator_field in
  let update : update = { argument; operator } in
  let* first_child = child_ranked 0 node in
  match get_name first_child with
  | "++" | "--" -> Ok (Update_prefix update)
  | _ -> Ok (Update_postfix update)

and decode_incr_decr_operator node : (incr_decr_operator, _) result =
  match get_name node with
  | "++" -> Ok (Increment (make_sym node))
  | "--" -> Ok (Decrement (make_sym node))
  | s -> Error ("decode_incr_decr_operator: " ^ s)

(* New expression *)

and dec_new_expression node : (new_expression, _) result =
  let* kwd_new = first_child_named "new" node in
  let kwd_new = make_kwd kwd_new in
  let* constructor_field = child_with_field "constructor" node in
  let* constructor = dec_primary_expression constructor_field in
  let type_arguments_field = child_with_field_opt "type_arguments" node in
  let* type_arguments = make_opt_res dec_type_arguments type_arguments_field in
  let arguments_field = child_with_field_opt "arguments" node in
  let* arguments = make_opt_res dec_arguments arguments_field in
  Ok { kwd_new; constructor; type_arguments; arguments }

(* Yield expression *)

and dec_yield_expression node : (yield_expression, _) result =
  let* kwd_yield = first_child_named "yield" node in
  let kwd_yield = make_kwd kwd_yield in
  match child_ranked_opt 1 node with
  | None -> Ok (Yield (kwd_yield, None))
  | Some snd_child ->
    (match get_name snd_child with
    | "*" ->
      let sym_star = make_sym snd_child in
      let* expression = child_ranked 2 node in
      let* expression = dec_expression expression in
      Ok (Yield_iterable (kwd_yield, sym_star, expression))
    | _ ->
      let* expression = dec_expression snd_child in
      Ok (Yield (kwd_yield, Some expression)))

(* As-expression *)

and dec_as_expression node : (as_expression, _) result =
  let* expression = child_ranked 0 node in
  let* expression = dec_expression expression in
  let* kwd_as = first_child_named "as" node in
  let kwd_as = make_kwd kwd_as in
  let* as_what = child_ranked 2 node in
  let* as_what = decode_as_what as_what in
  Ok (expression, kwd_as, as_what)

and decode_as_what node : (as_what, _) result =
  match get_name node with
  | "const" -> Ok (As_const (make_kwd node))
  | _ ->
    let* type_expr = dec_type node in
    Ok (As_type type_expr)

(* Statisfies-expression *)

and dec_satisfies_expression node : (satisfies_expression, _) result =
  let* expression = child_ranked 0 node in
  let* expression = dec_expression expression in
  let* kwd_satisfies = first_child_named "satisfies" node in
  let kwd_satisfies = make_kwd kwd_satisfies in
  let* type_child = child_ranked 2 node in
  let* type_expr = dec_type type_child in
  Ok (expression, kwd_satisfies, type_expr)

(* Instantiation expression *)

and dec_instantiation_expression node : (instantiation_expression, _) result =
  let* expression = named_child_ranked 0 node in
  let* expression = dec_expression expression in
  let* type_arguments_field = child_with_field "type_arguments" node in
  let* type_arguments = dec_type_arguments type_arguments_field in
  Ok (expression, type_arguments)

(* Type assertion *)

and dec_type_assertion node : (type_assertion, _) result =
  ignore node;
  Error "TODO: dec_type_assertion"

(* Subscript expression (see [dec_member_expression]) *)

and dec_subscript_expression ?(comments = []) node : (subscript_expression, _) result =
  let* object_field = child_with_field "object" node in
  let* object_expr = dec_expression ~comments object_field in
  let optional_chain_field = child_with_field_opt "optional_chain" node in
  let* optional_chain = make_opt_res dec_optional_chain optional_chain_field in
  let* index_field = child_with_field "index" node in
  let contents = dec_expressions index_field in
  let* sym_lbracket = first_child_named "[" node in
  let opening = make_sym sym_lbracket in
  let* sym_rbracket = first_child_named "]" node in
  let closing = make_sym sym_rbracket in
  let index = Brackets { opening; contents; closing } in
  Ok { object_expr; optional_chain; index }

and dec_optional_chain node : (optional_chain, _) result =
  match get_name node with
  | "optional_chain" -> Ok (Optional_chain (make_sym node))
  | s -> Error ("dec_optional_chain: " ^ s)

(* Member expression *)

and dec_member_expression ?(comments = []) node : (member_expression, _) result =
  let* object_field = child_with_field "object" node in
  let* object_expr = dec_object_member ~comments object_field in
  let optional_chain_field = child_with_field_opt "optional_chain" node in
  let* property_field = child_with_field "property" node in
  let* property = dec_property_ident property_field in
  let* selector =
    match optional_chain_field with
    | None ->
      let* selector = first_child_named "." node in
      Ok (Dot (make_sym selector))
    | Some node -> Ok (Optional_chain (make_sym node))
  in
  Ok { object_expr; selector; property }

and dec_object_member ?comments node : (object_member, _) result =
  match get_name node with
  | "import" -> Ok (Object_member_import (make_kwd ?comments node))
  | _ ->
    let* expression = dec_expression ?comments node in
    Ok (Object_member_expression expression)

and dec_property_ident ?comments node : (property_ident, _) result =
  let identifier = dec_identifier ?comments node in
  match get_name node with
  | "private_property_identifier" -> Ok (Private_property_identifier identifier)
  | "property_identifier" -> Ok (Property_identifier identifier)
  | s -> Error ("dec_property_ident: " ^ s)

(* Parenthesised expression *)

and dec_parenthesized_expression ?comments node : (parenthesized_expression, _) result =
  decode_ne_list_in_parens_res ?comments node dec_expression

(* Sequence expression *)

and dec_sequence_expression ?(comments = []) node : (sequence_expression, _) result =
  let raw_children = collect_named_children node in
  ne_list_of_children_res ~comments dec_expression raw_children

(* Object expression *)

and dec_object ?(comments = []) node : (object_expr, _) result =
  ignore comments;
  ignore node;
  Error "TODO: dec_object"

(* LHS expression *)

and dec_lhs_expression ?comments node : (lhs_expression, _) result =
  match get_name node with
  | "member_expression" ->
    let* expression = dec_member_expression ?comments node in
    Ok (Member_expression expression : lhs_expression)
  | "subscript_expression" ->
    let* expression = dec_subscript_expression ?comments node in
    Ok (Subscript_expression expression : lhs_expression)
  | "identifier" -> Ok (Identifier (dec_identifier ?comments node))
  | "undefined" -> Ok (Undefined (make_kwd ?comments node))
  | "object_pattern" ->
    let* pattern = dec_object_pattern ?comments node in
    Ok (Pattern (Pattern_object pattern))
  | "array_pattern" ->
    let* pattern = dec_array_pattern ?comments node in
    Ok (Pattern (Pattern_array pattern))
  | "non_null_expression" ->
    let* expression = dec_non_null_expression ?comments node in
    Ok (Non_null_expression expression)
  | s -> Error ("dec_lhs_expression: " ^ s)

(* Non-null expression *)

and dec_non_null_expression ?comments node : (expression, _) result =
  dec_expression ?comments node

(* PRIMARY EXPRESSION *)

and dec_primary_expression ?(comments = []) node : (primary_expression, _) result =
  ignore comments;
  ignore node;
  Error "TODO: dec_primary_expression"

(* Class expression ("class_" in the grammar) *)

and dec_class_expression ?(comments = []) node : (class_expression, _) result =
  ignore comments;
  ignore node;
  Error "TODO: dec_class_expression"

(* PATTERN

   The JavasScript tree-sitter grammar have the non-terminal
   "pattern" be a supertype, that is, a hidden rule. *)

and dec_pattern ?(comments = []) node : (pattern, _) result =
  ignore comments;
  ignore node;
  Error "TODO: dec_pattern"

(* Object pattern *)

and dec_object_pattern ?comments node : (object_pattern, _) result =
  decode_list_in_braces_res ?comments node dec_member_pattern

and dec_member_pattern ?(comments = []) node : (member_pattern, _) result =
  match get_name node with
  | "pair_pattern" ->
    let* pattern = dec_pair_pattern ~comments node in
    Ok (Member_pair_pattern pattern)
  | "rest_pattern" ->
    let* pattern = dec_rest_pattern ~comments node in
    Ok (Member_rest_pattern pattern)
  | "object_assignment_pattern" ->
    let* pattern = dec_object_assignment_pattern node in
    Ok (Member_object_assignment pattern)
  | "shorthand_property_identifier_pattern" ->
    Ok (Member_shorthand_property (dec_shorthand_property_identifier_pattern node))
  | s -> Error ("dec_member_pattern: " ^ s)

(* Pair pattern *)

and dec_pair_pattern ?(comments = []) node : (pair_pattern, _) result =
  let* key_field = child_with_field "key" node in
  let* key = dec_property_name ~comments key_field in
  let* sym_colon = first_child_named ":" node in
  let sym_colon = make_sym sym_colon in
  let* value_field = child_with_field "value" node in
  let* value = decode_pair_value_pattern value_field in
  Ok { key; sym_colon; value }

and decode_pair_value_pattern node : (pair_value_pattern, _) result =
  match get_name node with
  | "assignment_pattern" ->
    let* pattern = dec_assignment_pattern node in
    Ok (Pair_value_assignment pattern)
  | _ ->
    (* Hidden rule *)
    let* pattern = dec_pattern node in
    Ok (Pair_value pattern)

(* Rest pattern *)

and dec_rest_pattern ?(comments = []) node : (rest_pattern, _) result =
  let* sym_ellipsis = first_child_named "..." node in
  let sym_ellipsis = make_sym ~comments sym_ellipsis in
  let* expr_child = named_child_ranked 0 node in
  let* expression = dec_lhs_expression expr_child in
  Ok { sym_ellipsis; expression }

(* Assignment pattern *)

and dec_object_assignment_pattern ?(comments = []) node
    : (object_assignment_pattern, _) result
  =
  let* left_field = child_with_field "left" node in
  let* left = dec_object_lhs_pattern ~comments left_field in
  let* sym_equal = first_child_named "=" node in
  let sym_equal = make_kwd sym_equal in
  let* right_field = child_with_field "right" node in
  let* right = dec_expression right_field in
  Ok ({ left; sym_equal; right } : object_assignment_pattern)

and dec_object_lhs_pattern ?comments node : (object_lhs_pattern, _) result =
  dec_lhs_pattern ?comments node

and dec_lhs_pattern ?comments node : (lhs_pattern, _) result =
  match get_name node with
  | "shorthand_property_identifier_pattern" ->
    Ok (Decl_ident (dec_shorthand_property_identifier_pattern ?comments node))
  | _ ->
    (* Hidden rule *)
    let* pattern = dec_destructuring_pattern ?comments node in
    Ok (Decl_pattern pattern)

(* Shorthand property identifier pattern *)

and dec_shorthand_property_identifier_pattern ?comments node : identifier =
  dec_identifier ?comments node

(* Array pattern *)

and dec_array_pattern ?comments node : (array_pattern, _) result =
  decode_list_in_brackets_res ?comments node dec_array_cell_pattern

and dec_array_cell_pattern ?comments node : (array_cell_pattern, _) result =
  match get_name node with
  | "assignment_pattern" ->
    let* pattern = dec_assignment_pattern ?comments node in
    Ok (Cell_assignment pattern)
  | _ ->
    (* hidden rule *)
    let* pattern = dec_pattern ?comments node in
    Ok (Cell_pattern pattern)

(* Assignment pattern *)

and dec_assignment_pattern ?(comments = []) node : (assignment_pattern, _) result =
  let* left_field = child_with_field "left" node in
  let* left = dec_pattern ~comments left_field in
  let* sym_equal = first_child_named "=" node in
  let sym_equal = make_sym sym_equal in
  let* right_field = child_with_field "right" node in
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
  | s -> Error ("dec_destructuring_pattern: " ^ s)

(** TYPES
*)
and dec_type ?comments node : (type_expr, _) result =
  ignore comments;
  ignore node;
  Error "TODO: dec_type"

and dec_generic_type ?comments node : (generic_type, _) result =
  ignore comments;
  ignore node;
  Error "TODO: dec_generic_type"

and dec_object_type node : (object_type, _) result =
  ignore node;
  Error "TODO: dec_object_type"
