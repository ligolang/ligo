(* Decoding the tree-sitter CST for TypeScript *)

module Region = Simple_utils.Region
module Wrap = Lexing_shared.Wrap
module Ts_wrap = Typescript_ast.Ts_wrap
module Lexeme = Typescript_ast.Lexeme
module Ast = Typescript_ast.Ast
open Core
open Typescript_ast.Ts_wrap
open Ast

(* Monadic let for result values *)

let ( let* ) v f = Result.bind v ~f

(* Source map for converting vertical and horizontal offset ranges
   into regions *)

let get_region : (Ts_wrap.ts_tree -> Region.t) ref =
  ref (fun _ -> failwith "Internal error: Decoder.get_region")

(* Handling results and failing in case of error *)

let ensure_Ok node = function
  | Result.Ok ok -> ok
  | Error msg -> failwith ((!get_region node)#compact `Byte ^ "\n" ^ msg)

(* Decoding literals *)

let make_node ?(comments = []) node : string wrap =
  let region = !get_region node in
  let root = Lexeme.read region
  and comments = comments @ prev_comments node in
  let f node =
    let region = !get_region node in
    let value = Lexeme.read region in
    Wrap.Block Region.{ value; region }
  in
  let comments = List.map ~f comments in
  Wrap.make ~comments root region

let make_kwd ?comments node : keyword = make_node ?comments node
let make_sym ?comments node : symbol = make_node ?comments node
let dec_identifier ?comments node : identifier = make_node ?comments node
let dec_string ?comments node : string_literal = make_node ?comments node

let dec_number ?comments node : bigint_literal =
  ignore comments;  ignore node; failwith "dec_number"

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

let ne_list_of_children ?(comments = []) decoder children : 'a ne_list option =
  let f raw_child = List.cons (decoder ?comments:None raw_child) in
  match children with
  | [] -> None
  | fst_raw_child :: siblings ->
    let fst_child = decoder ?comments:(Some comments) fst_raw_child in
    Some Nonempty_list.(fst_child :: List.fold_right ~f ~init:[] siblings)

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

(* Decoding enclosed constructs *)

let decode_enclosed ?(comments = []) node decoder opening closing : 'a enclosed =
  ensure_Ok node
  @@
  let comments = comments @ prev_comments node in
  let* opening = first_child_named opening node in
  let* closing = first_child_named closing node in
  let clauses = collect_named_children node in
  Ok
    { opening = make_sym ~comments opening
    ; contents = decoder clauses
    ; closing = make_sym closing
    }

let decode_braces ?(comments = []) node decoder : 'a braces =
  Braces (decode_enclosed ~comments node decoder "{" "}")

let decode_chevrons ?(comments = []) node decoder : 'a chevrons =
  Chevrons (decode_enclosed ~comments node decoder "<" ">")

let decode_brackets ?(comments = []) node decoder : 'a brackets =
  Brackets (decode_enclosed ~comments node decoder "[" "]")

let decode_parens ?(comments = []) node decoder : 'a parens =
  Parens (decode_enclosed ~comments node decoder "(" ")")

let decode_list_in_braces ?comments node decoder : 'a list braces =
  decode_braces ?comments node (list_of_children decoder)

let decode_list_in_chevrons ?comments node decoder : 'a list chevrons =
  decode_chevrons ?comments node (list_of_children decoder)

let decode_list_in_brackets ?comments node decoder : 'a list brackets =
  decode_brackets ?comments node (list_of_children decoder)

let decode_list_in_parens ?comments node decoder : 'a list parens =
  decode_parens ?comments node (list_of_children decoder)

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
  ne_list_of_children ~comments dec_statement children

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
  | s -> failwith ("dec_statement: " ^ s ^ "\n")

(* Export statement *)

and dec_export_statement ?(comments = []) node : export_statement =
  ignore comments;
  ignore node;
  failwith "TODO: dec_export_statement"

(* Import statement *)

and dec_import_statement ?(comments = []) node : import_statement =
  ignore comments;
  ignore node;
  failwith "TODO: dec_import_statement"

(* Expression statements

   {@js[
    expression_statement: $ => seq($._expressions, $._semicolon),
    _expressions: $ => choice($.expression, $.sequence_expression),
    sequence_expression: $ => prec.right(commaSep1($.expression))
   ]}

   See [doc_expression]. *)

and dec_expression_statement ?(comments = []) node : expression_statement =
  dec_expressions ~comments node

and dec_expressions ?(comments = []) (node : ts_tree) : expressions =
  match get_name node with
  | "sequence_expression" -> Sequence_expression (dec_sequence_expression ~comments node)
  | _ -> General_expression (dec_expression ~comments node)

(* Statement blocks *)

and dec_statement_block ?(comments = []) node : statement_block =
  dec_statements ~comments node

(* If statement *)

and dec_if_statement ?(comments = []) node : if_statement =
  ensure_Ok node
  @@ let* kwd_if = first_child_named "if" node in
     let* condition_field = child_with_field "condition" node in
     let* consequence_field = child_with_field "consequence" node in
     let alternative_field = child_with_field_opt "alternative" node in
     Ok
       { kwd_if = make_kwd ~comments kwd_if
       ; condition = dec_parenthesized_expression condition_field
       ; consequence = dec_statement consequence_field
       ; alternative = make_opt dec_else_clause alternative_field
       }

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
     let* value_field = child_with_field "value" node in
     let* body_field = child_with_field "body" node in
     Ok
       { kwd_switch = make_kwd kwd_switch
       ; value = dec_parenthesized_expression value_field
       ; body = dec_switch_body body_field
       }

and dec_switch_body node : switch_body =
  let decode ?comments node =
    match get_name node with
    | "switch_case" -> Switch_case (dec_switch_case ?comments node)
    | "switch_default" -> Switch_default (dec_switch_default ?comments node)
    | s -> failwith ("dec_switch_body: " ^ s ^ "\n")
  in
  decode_list_in_braces node decode

and dec_switch_case ?(comments = []) node : switch_case =
  ensure_Ok node
  @@
  let comments = comments @ prev_comments node in
  let* kwd_case = first_child_named "case" node in
  let children = collect_children node in
  let rec skip_until_colon = function
    | [] -> []
    | node :: nodes ->
      (match get_name node with
      | ":" -> nodes
      | _ -> skip_until_colon nodes)
  in
  let stmt_children = skip_until_colon children in
  let* value_field = child_with_field "value" node in
  Ok
    { kwd_case = make_kwd ~comments kwd_case
    ; value = dec_expressions value_field
    ; body = list_of_children dec_statement stmt_children
    }

and dec_switch_default ?(comments = []) node : switch_default =
  ensure_Ok node
  @@
  let comments = comments @ prev_comments node in
  let* kwd_default = first_child_named "default" node in
  let statements = collect_named_children node in
  Ok
    { kwd_default = make_kwd ~comments kwd_default
    ; statements = list_of_children dec_statement statements
    }

(* For statement *)

and dec_for_statement node : for_statement =
  ensure_Ok node
  @@ let* kwd_for = first_child_named "for" node in
     let* sym_lparen = first_child_named "(" node in
     let* initializer_field = child_with_field "initializer" node in
     let* condition_field = child_with_field "condition" node in
     let increment_field = child_with_field_opt "increment" node in
     let* sym_rparen = first_child_named ")" node in
     let* body_field = child_with_field "body" node in
     let dec_initializer node : for_initializer =
       match get_name node with
       | "lexical_declaration" -> For_lexical_declaration (dec_lexical_declaration node)
       | "variable_declaration" ->
         For_variable_declaration (dec_variable_declaration node)
       | "expression_statement" ->
         For_expression_statement (dec_expression_statement node)
       | "empty_statement" -> For_empty_statement (!get_region node)
       | s -> failwith ("dec_for_statement/dec_initializer: " ^ s ^ "\n")
     and dec_condition node : for_condition =
       match get_name node with
       | "expression_statement" ->
         For_condition_expression (dec_expression_statement node)
       | "empty_statement" -> For_condition_empty (!get_region node)
       | s -> failwith ("dec_for_statement/dec_condition: " ^ s ^ "\n")
     in
     Ok
       { kwd_for = make_kwd kwd_for
       ; sym_lparen = make_sym sym_lparen
       ; initializer_ = dec_initializer initializer_field
       ; condition = dec_condition condition_field
       ; increment = make_opt dec_expressions increment_field
       ; sym_rparen = make_sym sym_rparen
       ; body = dec_statement body_field
       }

(* For-in statement *)

and dec_for_in_statement node : for_in_statement =
  ensure_Ok node
  @@ let* kwd_for = first_child_named "for" node in
     let kwd_await = first_child_named_opt "await" node in
     let* sym_lparen = first_child_named "(" node in
     let kind_field = child_with_field_opt "kind" node in
     let* left_field = child_with_field "left" node in
     let* sym_rparen = first_child_named ")" node in
     let* body_field = child_with_field "body" node in
     let* operator_field = child_with_field "operator" node in
     let* right_field = child_with_field "right" node in
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
         | s -> failwith ("dec_for_in_statement/range:" ^ s ^ "\n"))
     in
     let operator : for_operator =
       match get_name operator_field with
       | "in" -> In (make_kwd operator_field)
       | "of" -> Of (make_kwd operator_field)
       | s -> failwith ("dec_for_in_statement/operator: " ^ s ^ "\n")
     in
     let for_header : for_header =
       { range; operator; collection = dec_expressions right_field }
     in
     Ok
       { kwd_for = make_kwd kwd_for
       ; kwd_await = make_opt make_kwd kwd_await
       ; sym_lparen = make_sym sym_lparen
       ; for_header
       ; sym_rparen = make_sym sym_rparen
       ; body = dec_statement body_field
       }

(* While statement *)

and dec_while_statement node : while_statement =
  ensure_Ok node
  @@ let* kwd_while = first_child_named "while" node in
     let* condition_field = child_with_field "condition" node in
     let* body_field = child_with_field "body" node in
     Ok
       { kwd_while = make_kwd kwd_while
       ; condition = dec_parenthesized_expression condition_field
       ; body = dec_statement body_field
       }

(* Do statement *)

and dec_do_statement ?(comments = []) node : do_statement =
  ensure_Ok node
  @@ let* kwd_do = first_child_named "do" node in
     let* body_field = child_with_field "body" node in
     let* kwd_while = first_child_named "while" node in
     let* condition_field = child_with_field "condition" node in
     Ok
       { kwd_do = make_kwd ~comments kwd_do
       ; body = dec_statement body_field
       ; kwd_while = make_kwd kwd_while
       ; condition = dec_parenthesized_expression condition_field
       }

(* Try statement *)

and dec_try_statement node : try_statement =
  ensure_Ok node
  @@ let* kwd_try = first_child_named "try" node in
     let* body_field = child_with_field "body" node in
     let handler_field = child_with_field_opt "handler" node in
     let finalizer_field = child_with_field_opt "finalizer" node in
     Ok
       { kwd_try = make_kwd kwd_try
       ; body = dec_statement_block body_field
       ; handler = make_opt dec_catch_clause handler_field
       ; finalizer = make_opt dec_finally_clause finalizer_field
       }

and dec_catch_clause node : catch_clause =
  ensure_Ok node
  @@ let* kwd_catch = first_child_named "catch" node in
     let parameter_field = child_with_field_opt "parameter" node in
     let* body_field = child_with_field "body" node in
     Ok
       { kwd_catch = make_kwd kwd_catch
       ; parameter = make_opt (dec_catch_parameter node) parameter_field
       ; body = dec_statement_block body_field
       }

and dec_catch_parameter node param : catch_parameter =
  ensure_Ok node
  @@ let* sym_lparen = first_child_named "(" node in
     let type_field = child_with_field_opt "type" node in
     let* sym_rparen = first_child_named ")" node in
     Ok
       { sym_lparen = make_sym sym_lparen
       ; catch_parameter = dec_catch_parameter_kind param
       ; type_ = make_opt dec_type_annotation type_field
       ; sym_rparen = make_sym sym_rparen
       }

and dec_catch_parameter_kind node : catch_parameter_kind =
  match get_name node with
  | "identifier" -> Catch_identifier (dec_identifier node)
  | "object_pattern" -> Catch_object_pattern (dec_object_pattern node)
  | "array_pattern" -> Catch_array_pattern (dec_array_pattern node)
  | s -> failwith ("dec_catch_parameter_kind: " ^ s ^ "\n")

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
     let* object_field = child_with_field "object" node in
     let* body_field = child_with_field "body" node in
     Ok
       { kwd_with = make_kwd kwd_with
       ; object_ = dec_parenthesized_expression object_field
       ; body = dec_statement body_field
       }

(* Break statement *)

and dec_break_statement node : break_statement =
  ensure_Ok node
  @@ let* kwd_break = first_child_named "break" node in
     let label_field = child_with_field_opt "label" node in
     Ok { kwd_break = make_kwd kwd_break; stmt_id = make_opt dec_identifier label_field }

(* Continue statement *)

and dec_continue_statement node : continue_statement =
  ensure_Ok node
  @@ let* kwd_continue = first_child_named "continue" node in
     let label_field = child_with_field_opt "label" node in
     Ok
       { kwd_continue = make_kwd kwd_continue
       ; stmt_id = make_opt dec_identifier label_field
       }

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

(* Function declaration (see [dec_function_signature]) *)

and dec_function_declaration ?(comments = []) node : function_declaration =
  ensure_Ok node
  @@
  let comments = comments @ prev_comments node in
  let kwd_async = first_child_named_opt "async" node in
  let* kwd_function = first_child_named "function" node in
  let* name_field = child_with_field "name" node in
  (* "_call_signature" inlined: *)
  let type_parameters_field = child_with_field_opt "type_parameters" node in
  let* parameters_field = child_with_field "parameters" node in
  let return_type_field = child_with_field_opt "return_type" node in
  (* "statement_block" *)
  let* body_field = child_with_field "body" node in
  let async_comments, function_comments =
    match kwd_async with
    | None -> [], comments
    | Some _ -> comments, []
  in
  let call_sig : call_signature =
    { type_parameters = make_opt dec_type_parameters type_parameters_field
    ; parameters = dec_formal_parameters parameters_field
    ; return_type = make_opt dec_return_type return_type_field
    }
  in
  let fun_sig : function_signature =
    { kwd_async = make_opt (make_kwd ~comments:async_comments) kwd_async
    ; kwd_function = make_kwd ~comments:function_comments kwd_function
    ; name = dec_identifier name_field
    ; call_sig
    }
  in
  Ok { fun_sig; body = dec_statement_block body_field }

and dec_formal_parameters node : formal_parameters =
  decode_list_in_parens node dec_formal_parameter

and dec_formal_parameter ?(comments = []) node : formal_parameter =
  ensure_Ok node
  @@
  let comments = comments @ prev_comments node in
  (* "_parameter_name" inlined: *)
  let decorators = children_named "decorator" node
  and accessibility_modifier = first_child_named_opt "accessibility_modifier" node
  and override_modifier = first_child_named_opt "override_modifier" node
  and kwd_readonly = first_child_named_opt "readonly" node in
  let* pattern_field = child_with_field "pattern" node in
  (* *)
  let qmark = first_child_named_opt "?" node
  and type_field = child_with_field_opt "type" node
  and dec_pattern_field ~comments node : parameter_pattern =
    match get_name node with
    | "this" -> Parameter_this (make_kwd ~comments node)
    | _ -> Parameter_pattern (dec_pattern ~comments node)
  in
  let parameter_name : parameter_name =
    { decorators = ne_list_of_children dec_decorator decorators
    ; access = make_opt dec_accessibility_modifier accessibility_modifier
    ; override = make_opt dec_override_modifier override_modifier
    ; readonly = make_opt make_kwd kwd_readonly
    ; pattern = (dec_pattern_field ~comments) pattern_field (* Not perfect *)
    }
  in
  Ok
    { parameter_name
    ; optional = make_opt make_sym qmark
    ; type_ = make_opt dec_type_annotation type_field
    ; default = mk_child_initializer_opt node
    }

and mk_child_initializer_opt node : (keyword * expression) option =
  match first_child_named_opt "=" node with
  | None -> None
  | Some sym_equal -> Some (mk_child_initializer sym_equal node)

and mk_child_initializer sym_equal node : keyword * expression =
  ensure_Ok node
  @@ let* value_field = child_with_field "value" node in
     Ok (make_sym sym_equal, dec_expression value_field)

(* Accessibility modifier *)

and dec_accessibility_modifier node : accessibility_modifier =
  ensure_Ok node
  @@ let* child = child_ranked 0 node in
     Ok
       (match get_name child with
       | "public" -> Public (make_kwd node)
       | "private" -> Private (make_kwd node)
       | "protected" -> Protected (make_kwd node)
       | _ -> failwith "dec_accessibility_modifier/decode")

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
  | s -> failwith ("dec_return_type: " ^ s ^ "\n")

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
     Ok
       (match get_name child with
       | "type_predicate" -> Assert_predicate (kwd_asserts, dec_type_predicate node)
       | "identifier" -> Assert_type (kwd_asserts, dec_identifier node)
       | "this" -> Assert_this (kwd_asserts, make_kwd node)
       | s -> failwith ("dec_asserts/decode: " ^ s ^ "\n"))

(* Type predicate annotation *)

and dec_type_predicate_annotation node : type_predicate =
  ensure_Ok node
  @@ let* predicate = child_ranked 1 node in
     Ok (dec_type_predicate predicate)

(* Type predicate *)

and dec_type_predicate node : type_predicate =
  ensure_Ok node
  @@ let* name_field = child_with_field "name" node in
     let* kwd_is = first_child_named "is" node in
     let* type_field = child_with_field "type" node in
     let dec_name_field node =
       match get_name node with
       | "identifier" -> Type_predicate_identifier (dec_identifier node)
       | "this" -> Type_predicate_this (make_kwd node)
       | _ -> Type_predicate_type (dec_predefined_type node)
     in
     Ok
       { name = dec_name_field name_field
       ; kwd_is = make_kwd kwd_is
       ; type_ = dec_type type_field
       }

(* Predefined type *)

and dec_predefined_type ?(comments = []) node : predefined_type =
  ignore comments;
  ignore node;
  failwith "dec_predefined_type"

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
  ignore comments;
  ignore node;
  failwith "dec_decorator_member_expression"

and dec_decorator_call_expression ?(comments = []) node : decorator_call_expression =
  ignore comments;
  ignore node;
  failwith "dec_decorator_call_expression"

and dec_decorator_parenthesized_expression ?(comments = []) node
    : decorator_parenthesized_expression
  =
  ignore comments;
  ignore node;
  failwith "dec_decorator_parenthesized_expression"

(* Generator function declaration (see function declaration) *)

and dec_generator_function_declaration node : generator_function_declaration =
  ignore node;
  failwith "dec_generator_function_declaration"

(* Class declaration (see [dec_class]) *)

and dec_class_declaration ?(comments = []) node : class_declaration =
  ignore comments;
  ignore node;
  failwith "dec_class_declaration"

(* Lexical declaration (see [dec_variable_declaration]) *)

and dec_lexical_declaration ?(comments = []) node : lexical_declaration =
  ignore comments;
  ignore node;
  failwith "dec_lexical_declaration"

(* Variable declaration (see [dec_lexical_declaration]) *)

and dec_variable_declaration ?(comments = []) node : variable_declaration =
  ignore comments;
  ignore node;
  failwith "dec_variable_declaration"

(* Function signature (See [dec_function_declaration]) *)

and dec_function_signature node : function_signature =
  ignore node;
  failwith "dec_function_signature"

(* Abstract class declaration ( see [dec_class_declaration]) *)

and dec_abstract_class_declaration node : abstract_class_declaration =
  ignore node;
  failwith "dec_abstract_class_declaration"

(* Module *)

and dec_module ?(comments = []) node : module_ =
  ignore comments;
  ignore node;
  failwith "dec_module"

(* Internal module (a.k.a. namespaces) *)

and dec_internal_module ?comments node : internal_module =
  ignore comments;
  ignore node;
  failwith "dec_internal_module"

(* Type alias declaration *)

and dec_type_alias_declaration ?(comments = []) node : type_alias_declaration =
  ignore comments;
  ignore node;
  failwith "dec_type_alias_declaration"

(* Type parameters *)

and dec_type_parameters node : type_parameters =
  decode_list_in_chevrons node dec_type_parameter

and dec_type_parameter ?(comments = []) node : type_parameter =
  ensure_Ok node
  @@
  let comments = comments @ prev_comments node in
  let kwd_const = first_child_named_opt "const" node in
  let* name_field = child_with_field "name" node in
  let constraint_field = child_with_field_opt "constraint" node in
  let value_field = child_with_field_opt "value" node in
  Ok
    { const = make_opt make_kwd kwd_const
    ; name = dec_type_identifier ~comments name_field (* Not perfect *)
    ; constraint_ = make_opt dec_constraint constraint_field
    ; default_type = make_opt dec_default_type value_field
    }

and dec_type_identifier ?comments node : type_identifier =
  dec_identifier ?comments node

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
  @@ let kwd_const = first_child_named_opt "const" node in
     let* kwd_enum = first_child_named "enum" node in
     let* name_field = child_with_field "name" node in
     let* body_field = child_with_field "body" node in
     Ok
       { const = make_opt make_kwd kwd_const
       ; enum = make_kwd kwd_enum
       ; name = dec_identifier name_field
       ; body = dec_enum_entries body_field
       }

and dec_enum_entries node : enum_body list braces =
  decode_list_in_braces node dec_enum_body

and dec_enum_body ?(comments = []) node : enum_body =
  ensure_Ok node
  @@
  Ok (match get_name node with
     | "enum_assignment" -> Enum_assignment (dec_enum_assignment ~comments node)
     | _ -> Enum_name (dec_property_name ~comments node))

and dec_enum_assignment ?comments node : enum_assignment =
  ignore comments; ignore node; failwith "dec_enum_assignment"

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
  | s -> failwith ("dec_property_name: " ^ s ^ "\n")

and dec_private_property_identifier ?(comments = []) node :  private_property_identifier =
  dec_identifier ~comments node

and dec_computed_property_name ?(comments = []) node : expression brackets =
  ignore comments; ignore node; failwith "dec_computed_property_name"

(* Interface declaration *)

and dec_interface_declaration node : interface_declaration =
  ignore node;
  failwith "dec_interface_declaration"

(* Import alias *)

and dec_import_alias node : import_alias =
  ignore node;
  failwith "dec_import_alias"

(* Ambient declaration *)

and dec_ambient_declaration node : ambient_declaration =
  ignore node;
  failwith "dec_ambient_declaration"

(* EXPRESSIONS *)

and dec_expression ?(comments = []) node : expression =
  ignore comments;
  ignore node;
  failwith "dec_expression"

and dec_parenthesized_expression ?(comments = []) node : parenthesized_expression =
  ignore comments;
  ignore node;
  failwith "dec_parenthesized_expression"

(* Sequence expression *)

and dec_sequence_expression ?(comments = []) node : sequence_expression =
  ignore comments;
  ignore node;
  failwith "dec_sequence_expression"

(* LHS expression *)

and dec_lhs_expression ?(comments = []) node : lhs_expression =
  ignore comments;
  ignore node;
  failwith "dec_lhs_expression"

(* PATTERN

   The JavasScript tree-sitter grammar have the non-terminal
   "pattern" be a supertype, that is, a hidden rule. *)

and dec_pattern ?(comments = []) node : pattern =
  ignore comments;
  ignore node;
  failwith "dec_pattern"

(* Object pattern *)

and dec_object_pattern node : object_pattern =
  ignore node;
  failwith "dec_object_pattern"

(* Array pattern *)

and dec_array_pattern node : array_pattern =
  ignore node;
  failwith "dec_array_pattern"

(* Rule "_destructuring_pattern" is inlined. *)

and dec_destructuring_pattern node : destructuring_pattern =
  ignore node;
  failwith "dec_destructuring_pattern"

(** TYPES
*)
and dec_type node : type_ =
  ignore node;
  failwith "dec_type"
