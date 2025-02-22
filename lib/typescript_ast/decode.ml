(* Decoding the tree-sitter CST for TypeScript *)

open Core

(* Dependencies and scopes *)

module Region = Simple_utils.Region
module Snippet = Simple_utils.Snippet
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

(* Formatting error messages (snippets) *)

let no_colour = ref false

let format_msg error node =
  let region = !get_region node in
  sprintf
    "%sError: %s"
    (Format.asprintf "%a" (Snippet.pp_lift ~no_colour:!no_colour) region)
    (Syntax_err.to_string error)

(* Utilities *)

let wrap decode ?comments node : ('a Wrap.t, _) result =
  let* decoded_node = decode ?comments node in
  Ok (Wrap.make decoded_node (!get_region node))

(* Tayloring the fetching of a field, with an error message in case of
   failure. If [!debug], a missing field yields internal information. *)

let debug = ref false

let child_with_field field node ~err =
  match Ts_wrap.child_with_field ~get_region field node with
  | Ok _ as ok -> ok
  | Error () ->
    let region = !get_region node in
    let region =
      if Region.is_empty region then "empty region" else region#compact `Byte
    in
    let msg =
      if !debug
      then (
        let name = get_name node in
        if String.equal name "NULL"
        then sprintf "ERROR: NULL parent of field %S." field
        else sprintf "ERROR: Node %S (%s) is missing the field %S." name region field)
      else format_msg err node
    in
    Error msg

(* Wrapping the fetching of nodes *)

let first_child_named name node ~err =
  Ts_wrap.first_child_named name node ~msg:(format_msg err node)

let child_ranked index node ~err =
  Ts_wrap.child_ranked index node ~msg:(format_msg err node)

let named_child_ranked index node ~err =
  Ts_wrap.named_child_ranked index node ~msg:(format_msg err node)

let last_child node ~err = Ts_wrap.last_child node ~msg:(format_msg err node)
let next_sibling node ~err = Ts_wrap.next_sibling node ~msg:(format_msg err node)
let prev_sibling node ~err = Ts_wrap.prev_sibling node ~msg:(format_msg err node)

(* Region of a node as a string *)

let mk_err err node = Error (format_msg err node)

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

let make_sym ?comments node : symbol = make_node ?comments node
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

let make_kwd ?comments node : keyword = make_node ?comments node

(*
let make_kwd ?comments node ~err =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> Error err
  | _ -> Ok (make_node ?comments node)

let mk_kwd_infer = make_kwd ~err:Infer
let mk_kwd_keyof = make_kwd ~err:Keyof
let mk_kwd_meta = make_kwd ~err:Meta
let mk_kwd_target = make_kwd ~err:Target
let mk_kwd_false = make_kwd ~err:False
let mk_kwd_true = make_kwd ~err:True
let mk_kwd_super = make_kwd ~err:Super
let mk_kwd_null = make_kwd ~err:Null
let mk_kwd_satisfies = make_kwd ~err:Satisfies
let mk_kwd_yield = make_kwd ~err:Yield
let mk_kwd_new = make_kwd ~err:New
let mk_kwd_instanceof = make_kwd ~err:Instanceof
let mk_kwd_implements = make_kwd ~err:Implements
let mk_kwd_assert = make_kwd ~err:Assert
let mk_kwd_as = make_kwd ~err:As
let mk_kwd_async = make_kwd ~err:Async
let mk_kwd_function = make_kwd ~err:Function
let mk_kwd_override = make_kwd ~err:Override
let mk_kwd_readonly = make_kwd ~err:Readonly
let mk_kwd_public = make_kwd ~err:Public
let mk_kwd_private = make_kwd ~err:Private
let mk_kwd_protected = make_kwd ~err:Protected
let mk_kwd_set = make_kwd ~err:Set
let mk_kwd_get = make_kwd ~err:Get
let mk_kwd_all = make_kwd ~err:All
let mk_kwd_static = make_kwd ~err:Static
let mk_kwd_this = make_kwd ~err:This
let mk_kwd_is = make_kwd ~err:Is
let mk_kwd_class = make_kwd ~err:Class
let mk_kwd_const = make_kwd ~err:Const
let mk_kwd_constraint = make_kwd ~err:Constraint
let mk_kwd_let = make_kwd ~err:Let
let mk_kwd_undefined = make_kwd ~err:Undefined
let mk_kwd_abstract = make_kwd ~err:Abstract
let mk_kwd_declare = make_kwd ~err:Declare
let mk_kwd_accessor = make_kwd ~err:Accessor
let mk_kwd_global = make_kwd ~err:Global
let mk_kwd_module = make_kwd ~err:Module
let mk_kwd_enum = make_kwd ~err:Enum
let mk_kwd_import = make_kwd ~err:Import
let mk_kwd_interface = make_kwd ~err:Interface
let mk_kwd_extends = make_kwd ~err:Extends
let mk_kwd_namespace = make_kwd ~err:Namespace
let mk_kwd_type = make_kwd ~err:Type
let mk_kwd_using = make_kwd ~err:Using
let mk_kwd_return = make_kwd ~err:Return
let mk_kwd_switch = make_kwd ~err:Switch
let mk_kwd_case = make_kwd ~err:Case
let mk_kwd_default = make_kwd ~err:Default
let mk_kwd_throw = make_kwd ~err:Throw
let mk_kwd_while = make_kwd ~err:While
let mk_kwd_with = make_kwd ~err:With
let mk_kwd_any = make_kwd ~err:Any
let mk_kwd_number = make_kwd ~err:Number
let mk_kwd_boolean = make_kwd ~err:Boolean
let mk_kwd_string = make_kwd ~err:String
let mk_kwd_symbol = make_kwd ~err:Symbol
let mk_kwd_unique_symbol = make_kwd ~err:Unique_symbol
let mk_kwd_void = make_kwd ~err:Void
let mk_kwd_unknown = make_kwd ~err:Unknown
let mk_kwd_never = make_kwd ~err:Never
let mk_kwd_object = make_kwd ~err:Object
let mk_kwd_asserts = make_kwd ~err:Asserts
let mk_kwd_debugger = make_kwd ~err:Debugger
let mk_kwd_break = make_kwd ~err:Break
let mk_kwd_continue = make_kwd ~err:Continue
let mk_kwd_do = make_kwd ~err:Do
let mk_kwd_export = make_kwd ~err:Export
let mk_kwd_for = make_kwd ~err:For
let mk_kwd_from = make_kwd ~err:From
let mk_kwd_await = make_kwd ~err:Await
let mk_kwd_var = make_kwd ~err:Var
let mk_kwd_in = make_kwd ~err:In
let mk_kwd_of = make_kwd ~err:Of
let mk_kwd_if = make_kwd ~err:If
let mk_kwd_else = make_kwd ~err:Else
let mk_kwd_typeof = make_kwd ~err:Typeof
let mk_kwd_try = make_kwd ~err:Try
let mk_kwd_catch = make_kwd ~err:Catch
let mk_kwd_require = make_kwd ~err:Require
let mk_kwd_delete = make_kwd ~err:Delete
let mk_kwd_finally = make_kwd ~err:Finally
let mk_kwd_instanceof = make_kwd ~err:Instanceof
 *)

(* Optional nodes *)

let make_opt decoder node = Option.map ~f:decoder node

let make_opt_res decode = function
  | None -> Ok None
  | Some value ->
    let* decoded = decode value in
    Ok (Some decoded)

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
    let opening = make_sym ~comments opening in
    let* closing = first_child_named closing node ~err:close_err in
    let closing = make_sym closing in
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
    let opening = make_sym ~comments opening in
    let* closing = first_child_named closing node ~err:close_err in
    let closing = make_sym closing in
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
    let opening = make_sym ~comments opening in
    let* closing = first_child_named closing node ~err:close_err in
    let closing = make_sym closing in
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
  | "debugger_statement" -> Ok (S_debugger_statement (make_kwd ~comments node))
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
    let kwd_export = make_kwd ~comments kwd_export in
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
        Ok (Export_equal (make_sym after_export, expression))
      | "as" ->
        let* kwd_namespace = first_child_named "namespace" node ~err:Namespace in
        let* identifier = first_child_named "identifier" node ~err:Identifier in
        Ok (Export_as_namespace (make_kwd kwd_namespace, dec_identifier identifier))
      | _ ->
        let* export_declaration = dec_export_declaration after_export node in
        Ok (Export_declaration export_declaration)
    in
    Ok { kwd_export; export_kind }

and dec_export_type after_export node : (export_type, _) result =
  let* export_clause = next_sibling after_export ~err:Export_clause in
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
      Ok (Some (make_kwd kwd_as, alias))
  in
  Ok ({ name; alias } : export_specifier)

and dec_module_export_name ?(comments = []) node : (module_export_name, _) result =
  match get_name node with
  | "identifier" -> Ok (Export_ident (dec_identifier ~comments node))
  | "string" -> Ok (Export_string (dec_string ~comments node))
  | _ -> mk_err Identifier_or_string node

and dec_from_clause node kwd_from : (from_clause, _) result =
  let* source_field = child_with_field "source" node ~err:File_path in
  Ok (make_kwd kwd_from, dec_string source_field)

and dec_namespace_export ?(comments = []) node : (namespace_export, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Namespace_export node
  | _ ->
    let comments = comments @ prev_comments node in
    let* sym_star = first_child_named "*" node ~err:Asterisk in
    let sym_star = make_sym ~comments sym_star in
    let* kwd_as = first_child_named "as" node ~err:As in
    let* module_export_name = next_sibling kwd_as ~err:Identifier_or_string in
    let kwd_as = make_kwd kwd_as in
    let* namespace_name = dec_module_export_name module_export_name in
    Ok { sym_star; kwd_as; namespace_name }

and dec_export_default after_export node : (export_kind, _) result =
  let decorators = children_named "decorator" node in
  let kwd_default = make_kwd after_export in
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
    let* sym_star = first_child_named "*" node ~err:Asterisk in
    let sym_star = make_sym ~comments sym_star in
    let* kwd_as = first_child_named "as" node ~err:As in
    let* identifier = next_sibling kwd_as ~err:Identifier in
    let kwd_as = make_kwd kwd_as in
    let identifier = dec_identifier identifier in
    Ok { sym_star; kwd_as; identifier }

and dec_named_imports ?(comments = []) node : (named_imports, _) result =
  dec_list_in_braces ~comments node dec_import_specifier ~err:Named_imports

and dec_import_specifier ?(comments = []) node : (import_specifier, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Import_specifier node
  | _ ->
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
    let* name_field = child_with_field "name" node ~err:Identifier in
    let* (import_specifier' : import_specifier') =
      match child_with_field_opt "alias" node with
      | None ->
        Ok (Import_spec_name (dec_identifier ~comments:snd_child_comments name_field))
      | Some alias_field ->
        let* kwd_as = first_child_named "as" node ~err:As in
        let* name = dec_module_export_name ~comments:snd_child_comments name_field in
        let kwd_as = make_kwd kwd_as in
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
    let sym_equal = make_sym sym_equal in
    let* kwd_require = first_child_named "require" node ~err:Require in
    let kwd_require = make_kwd kwd_require in
    let* sym_lpar = first_child_named "(" node ~err:Left_parenthesis in
    let sym_lpar = make_sym sym_lpar in
    let* source_field = child_with_field "source" node ~err:String in
    let source = dec_string source_field in
    let* sym_rpar = first_child_named ")" node ~err:Right_parenthesis in
    let sym_rpar = make_sym sym_rpar in
    Ok { ident; sym_equal; kwd_require; sym_lpar; source; sym_rpar }

and dec_import_attribute node : (import_attribute, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Import_attribute node
  | _ ->
    let* kind_node = child_ranked 0 node ~err:Import_attribute in
    let* object_node = child_ranked 1 node ~err:Object_expression in
    let* expression = dec_object_expr object_node in
    (match get_name kind_node with
    | "with" -> Ok (Import_with (make_kwd kind_node, expression))
    | "assert" -> Ok (Import_assert (make_kwd kind_node, expression))
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
    let opening = make_sym ~comments opening in
    let* closing = first_child_named "}" node ~err:Right_brace in
    let closing = make_sym closing in
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
    let kwd_if = make_kwd ~comments kwd_if in
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
    Ok (make_kwd ~comments kwd_else, statement)

(* Switch statement *)

and dec_switch_statement ?(comments = []) node : (switch_statement, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Switch node
  | _ ->
    let* kwd_switch = first_child_named "switch" node ~err:Switch in
    let kwd_switch = make_kwd ~comments kwd_switch in
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
    let kwd_case = make_kwd ~comments kwd_case in
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
    let kwd_default = make_kwd ~comments kwd_default in
    let statements = collect_named_children node in
    let* statements = wrap_ne_list_opt_of_children dec_statement statements in
    Ok { kwd_default; statements }

(* For statement *)

and dec_for_statement ?(comments = []) node : (for_statement, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err For node
  | _ ->
    let* kwd_for = first_child_named "for" node ~err:For in
    let kwd_for = make_kwd ~comments kwd_for in
    let* sym_lpar = first_child_named "(" node ~err:Left_parenthesis in
    let sym_lpar = make_sym sym_lpar in
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
    let* sym_rpar = first_child_named ")" node ~err:Right_parenthesis in
    let sym_rpar = make_sym sym_rpar in
    let* body_field = child_with_field "body" node ~err:Statement in
    let* body = dec_statement body_field in
    Ok { kwd_for; sym_lpar; initializer_; condition; increment; sym_rpar; body }

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
    let kwd_for = make_kwd ~comments kwd_for in
    let kwd_await = first_child_named_opt "await" node in
    let kwd_await = make_opt make_kwd kwd_await in
    let* sym_lpar = first_child_named "(" node ~err:Left_parenthesis in
    let sym_lpar = make_sym sym_lpar in
    let kind_field = child_with_field_opt "kind" node in
    let* left_field = child_with_field "left" node ~err:Expression in
    let* sym_rpar = first_child_named ")" node ~err:Right_parenthesis in
    let sym_rpar = make_sym sym_rpar in
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
        let keyword = make_kwd kind_field in
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
          let value_field = child_with_field_opt "value" node in
          let* default = make_opt_res dec_expression value_field in
          Ok (For_in_var { kwd_var = keyword; variable; default })
        | "let" -> Ok (For_in_let (keyword, variable))
        | "const" -> Ok (For_in_const (keyword, variable))
        | _ -> mk_err Let_or_const_or_var kind_field)
    in
    let for_header : for_header = { range; operator; collection } in
    Ok { kwd_for; kwd_await; sym_lpar; for_header; sym_rpar; body }

and dec_for_operator node : (for_operator, _) result =
  match get_name node with
  | "in" -> Ok (In (make_kwd node))
  | "of" -> Ok (Of (make_kwd node))
  | _ -> mk_err In_or_of node

(* While statement *)

and dec_while_statement ?(comments = []) node : (while_statement, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err While node
  | _ ->
    let* kwd_while = first_child_named "while" node ~err:While in
    let kwd_while = make_kwd ~comments kwd_while in
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
    let kwd_do = make_kwd ~comments kwd_do in
    let* body_field = child_with_field "body" node ~err:Statement in
    let* body = dec_statement body_field in
    let* kwd_while = first_child_named "while" node ~err:While in
    let kwd_while = make_kwd kwd_while in
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
    let kwd_try = make_kwd ~comments kwd_try in
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
    let kwd_catch = make_kwd kwd_catch in
    let parameter_field = child_with_field_opt "parameter" node in
    let* parameter = make_opt_res (dec_catch_parameter node) parameter_field in
    let* body_field = child_with_field "body" node ~err:Block in
    let* body = dec_statement_block body_field in
    Ok { kwd_catch; parameter; body }

and dec_catch_parameter node param : (catch_parameter, _) result =
  let* catch_parameter = dec_catch_parameter_kind param in
  let* sym_lpar = first_child_named "(" node ~err:Left_parenthesis in
  let sym_lpar = make_sym sym_lpar in
  let type_field = child_with_field_opt "type" node in
  let* type_opt = make_opt_res dec_type_annotation type_field in
  let* sym_rpar = first_child_named ")" node ~err:Right_parenthesis in
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
  | _ -> mk_err Pattern node

and dec_finally_clause node : (finally_clause, _) result = dec_statement_block node

(* Type annotation *)

and dec_type_annotation node : (type_annotation, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Type_annotation node
  | _ ->
    let* sym_colon = first_child_named ":" node ~err:Colon in
    let* type_child = named_child_ranked 0 node ~err:Type_expression in
    let* type_expr = dec_type type_child in
    Ok (make_sym sym_colon, type_expr)

(* With statement *)

and dec_with_statement ?(comments = []) node : (with_statement, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err With node
  | _ ->
    let* kwd_with = first_child_named "with" node ~err:With in
    let kwd_with = make_kwd ~comments kwd_with in
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
    let kwd_break = make_kwd ~comments kwd_break in
    let label_field = child_with_field_opt "label" node in
    let stmt_id = make_opt dec_identifier label_field in
    Ok { kwd_break; stmt_id }

(* Continue statement *)

and dec_continue_statement ?(comments = []) node : (continue_statement, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Continue node
  | _ ->
    let* kwd_continue = first_child_named "continue" node ~err:Continue in
    let kwd_continue = make_kwd ~comments kwd_continue in
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
    let kwd_return = make_kwd ~comments kwd_return in
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
    let kwd_throw = make_kwd ~comments kwd_throw in
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
    let sym_colon = make_sym sym_colon in
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
    | "public" -> Ok (Public (make_kwd child))
    | "private" -> Ok (Private (make_kwd child))
    | "protected" -> Ok (Protected (make_kwd child))
    | _ -> mk_err Public_private_protected node)

(* Override modifier *)

and dec_override_modifier node : (kwd_override, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Override node
  | _ ->
    let* child = first_child_named "override" node ~err:Override in
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
    let kwd_asserts = make_kwd kwd_asserts in
    let* child = child_ranked 1 node ~err:Asserted in
    (match get_name child with
    | "type_predicate" ->
      let* predicate = wrap dec_type_predicate child in
      Ok (Assert_predicate (kwd_asserts, predicate))
    | "identifier" -> Ok (Assert_type (kwd_asserts, dec_identifier child))
    | "this" -> Ok (Assert_this (kwd_asserts, make_kwd child))
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
    let kwd_is = make_kwd kwd_is in
    let* type_field = child_with_field "type" node ~err:Type_expression in
    let* type_expr = dec_type type_field in
    Ok { name; kwd_is; type_expr }

and dec_type_predicate_name ?(comments = []) node : (type_predicate_name, _) result =
  match get_name node with
  | "identifier" ->
    let ident = dec_identifier ~comments node in
    Ok (Type_predicate_identifier ident)
  | "this" -> Ok (Type_predicate_this (make_kwd ~comments node))
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
    let* dot = first_child_named "." node ~err:Dot in
    let sym_dot = make_sym dot in
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
    let sym_ellipsis = make_sym ~comments sym_ellipsis in
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
    let* sym_star = first_child_named "*" node ~err:Asterisk in
    Ok (make_sym sym_star, fun_decl)

(* Class declaration (see [dec_class]) *)

and dec_class_declaration ?(comments = []) node : (class_declaration, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Class_declaration node
  | _ ->
    let comments = comments @ prev_comments node in
    let decorators = children_named "decorator" node in
    let* decorators = list_of_children dec_decorator decorators in
    let* kwd_class = first_child_named "class" node ~err:Class in
    let kwd_class = make_kwd ~comments kwd_class in
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
    let kwd_implements = make_kwd kwd_implements in
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
    let opening = make_sym ~comments opening in
    let* closing = first_child_named "}" node ~err:Right_brace in
    let closing = make_sym closing in
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
    let kwd_async = make_opt make_kwd kwd_async in
    let set_get_all = mk_set_get_all node in
    let* name_field = child_with_field "name" node ~err:Property_name in
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
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Static_block node
  | _ ->
    let* kwd_static = first_child_named "static" node ~err:Static in
    let kwd_static = make_kwd ~comments kwd_static in
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
    let kwd_abstract = make_kwd kwd_abstract in
    let override_modifier = first_child_named_opt "override_modifier" node in
    let* kwd_override = make_opt_res dec_override_modifier override_modifier in
    let set_get_all = mk_set_get_all node in
    let* name_field = child_with_field "name" node ~err:Property_name in
    (* Not ideal *)
    let* name = dec_property_name ~comments name_field in
    let sym_qmark = first_child_named_opt "?" node in
    let optional = make_opt make_sym sym_qmark in
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
    let kwd_readonly = make_opt make_kwd kwd_readonly in
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
    let opening = make_sym ~comments sym_lbracket in
    let* sym_rbracket = first_child_named "]" node ~err:Right_bracket in
    let closing = make_sym sym_rbracket in
    let* (range : index_range) =
      match name_field with
      | Some name_field ->
        let name = dec_type_identifier name_field in
        let* sym_colon = first_child_named ":" node ~err:Colon in
        let sym_colon = make_sym sym_colon in
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
    let kwd_in = make_kwd kwd_in in
    let* type_field = child_with_field "type" node ~err:Type in
    let* type_expr = dec_type type_field in
    let alias_field = child_with_field_opt "alias" node in
    let* alias =
      match alias_field with
      | None -> Ok None
      | Some alias ->
        let* kwd_as = first_child_named "as" node ~err:As in
        let* type_expr = dec_type alias in
        Ok (Some (make_kwd kwd_as, type_expr))
    in
    Ok { name; kwd_in; type_expr; alias }

and dec_omitting_type_annotation node : (symbol * type_expr, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Omitting_type_annotation node
  | _ ->
    let* sym_kind = first_child_named "-?:" node ~err:Omitting_type_annotation in
    let* type_child = named_child_ranked 0 node ~err:Type_expression in
    let* type_expr = dec_type type_child in
    Ok (make_kwd sym_kind, type_expr)

and dec_adding_type_annotation node : (symbol * type_expr, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Adding_type_annotation node
  | _ ->
    let* sym_kind = first_child_named "+?:" node ~err:Adding_type_annotation in
    let* type_child = named_child_ranked 0 node ~err:Type_expression in
    let* type_expr = dec_type type_child in
    Ok (make_kwd sym_kind, type_expr)

and dec_opting_type_annotation node : (symbol * type_expr, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Opting_type_annotation node
  | _ ->
    let* sym_kind = first_child_named "?:" node ~err:Adding_type_annotation in
    let* type_child = named_child_ranked 0 node ~err:Type_expression in
    let* type_expr = dec_type type_child in
    Ok (make_kwd sym_kind, type_expr)

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
  | "+" -> Ok (Plus (make_sym node))
  | "-" -> Ok (Minus (make_sym node))
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
    let kwd_declare = make_opt make_kwd kwd_declare in
    let* scope = dec_field_scope node in
    let* name_field = child_with_field "name" node ~err:Property_name in
    let* name = dec_property_name ~comments name_field in
    let mode = dec_field_mode_opt node in
    let type_field = child_with_field_opt "type" node in
    let* type_ = make_opt_res dec_type_annotation type_field in
    let* default = mk_child_initializer_opt node in
    Ok { decorators; access; kwd_declare; scope; name; mode; type_; default }

and dec_field_mode_opt node : field_mode option =
  let sym_qmark = first_child_named_opt "?" node in
  match sym_qmark with
  | Some sym -> Some (Optional (make_sym sym))
  | None ->
    (match first_child_named_opt "!" node with
    | None -> None
    | Some sym -> Some (Definite_assert (make_sym sym)))

and dec_field_scope node : (field_scope, _) result =
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
      | "let" -> Ok (Let (make_kwd ~comments kind_field))
      | "const" -> Ok (Const (make_kwd ~comments kind_field))
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
    let var_decls = children_named "variable_declarator" node in
    let error = mk_err Variable_declaration node in
    let* var_decls = ne_list_of_children dec_variable_declarator error var_decls in
    Ok (make_sym ~comments kwd_var, var_decls)

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
      let sym_qmark = make_sym sym_qmark in
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
  let kwd_async = make_opt (make_kwd ~comments:async_comments) kwd_async in
  let* kwd_function = first_child_named "function" node ~err:Function in
  let kwd_function = make_kwd ~comments:function_comments kwd_function in
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
  let* qmark = first_child_named "?" node ~err:Question_mark in
  let optional = Some (make_sym qmark) in
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
  let kwd_readonly = make_opt make_kwd kwd_readonly in
  let* pattern_field = child_with_field "pattern" node ~err:Pattern in
  let* pattern = dec_parameter_pattern ~comments pattern_field (* Not ideal *) in
  Ok { decorators; access; kwd_override; kwd_readonly; pattern }

and dec_parameter_pattern ?(comments = []) node : (parameter_pattern, _) result =
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
  let* value_field = child_with_field "value" node ~err:Expression in
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
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Abstract node
  | _ ->
    let comments = comments @ prev_comments node in
    let decorators = children_named "decorator" node in
    let* decorators = list_of_children dec_decorator decorators in
    let* kwd_abstract = first_child_named "abstract" node ~err:Abstract in
    let kwd_abstract = make_kwd ~comments kwd_abstract in
    let* kwd_class = first_child_named "class" node ~err:Class in
    let kwd_class = make_kwd kwd_class in
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
    let kwd_module = make_kwd ~comments kwd_module in
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
    let kwd_namespace = make_kwd ~comments kwd_namespace in
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
    let kwd_type = make_kwd ~comments kwd_type in
    let* name_field = child_with_field "name" node ~err:Type_name in
    let name = dec_type_identifier name_field in
    let* sym_equal = first_child_named "=" node ~err:Equal in
    let sym_equal = make_sym sym_equal in
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
    let kwd_const = make_opt make_kwd kwd_const in
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
    let* type_child = child_ranked 1 node ~err:Type_expression in
    let* type_expr = dec_type type_child in
    Ok (make_kwd kwd_extends, type_expr)

and dec_default_type node : (sym_equal * type_expr, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Equal node
  | _ ->
    let* sym_equal = first_child_named "=" node ~err:Equal in
    let* type_node = child_ranked 1 node ~err:Type_expression in
    let* type_expr = dec_type type_node in
    Ok (make_sym sym_equal, type_expr)

(* Enum declaration *)

and dec_enum_declaration ?(comments = []) node : (enum_declaration, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Const_or_enum node
  | _ ->
    let kwd_const = first_child_named_opt "const" node in
    let kwd_const = make_opt make_kwd kwd_const in
    let* kwd_enum = first_child_named "enum" node ~err:Enum in
    let kwd_enum = make_kwd ~comments kwd_enum in
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
    let kwd_interface = make_kwd ~comments kwd_interface in
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
    let named_children = collect_named_children node in
    let error = mk_err Extends node in
    let* extensions = ne_list_of_children dec_type_extension error named_children in
    Ok { kwd_extends = make_kwd kwd_extends; extensions }

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
    let kwd_import = make_kwd ~comments kwd_import in
    let* lhs = child_ranked 1 node ~err:Identifier in
    let alias = dec_identifier lhs in
    let* sym_equal = first_child_named "=" node ~err:Equal in
    let sym_equal = make_sym sym_equal in
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
    let kwd_declare = make_kwd ?comments kwd_declare in
    let* fst_child = named_child_ranked 0 node ~err:Block_or_ident_or_decl in
    let* ambient_kind =
      match get_name fst_child with
      | "statement_block" ->
        let* kwd_global = first_child_named "global" node ~err:Global in
        let* block = dec_statement_block fst_child in
        Ok (Global_declaration (make_kwd kwd_global, block))
      | "property_identifier" ->
        let* kwd_module = first_child_named "module" node ~err:Module in
        let* type_child = child_ranked 5 node ~err:Type_expression in
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
    let kwd_using = make_opt make_kwd kwd_using in
    let* left_field = child_with_field "left" node ~err:Expression in
    let* left = dec_assignment_lhs ~comments left_field in
    let* sym_equal = first_child_named "=" node ~err:Equal in
    let sym_equal = make_sym sym_equal in
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
    let kwd_await = make_kwd ~comments kwd_await in
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
  | "!" -> Ok (Bang (make_sym ~comments node))
  | "~" -> Ok (Not (make_sym ~comments node))
  | "-" -> Ok (Unary_sub (make_sym ~comments node))
  | "+" -> Ok (Unary_add (make_sym ~comments node))
  | "typeof" -> Ok (Typeof (make_kwd ~comments node))
  | "void" -> Ok (Void (make_kwd ~comments node))
  | "delete" -> Ok (Delete (make_kwd ~comments node))
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
  | _ -> mk_err Binary_operator node

(* Ternary expression *)

and dec_ternary_expression ?(comments = []) node : (ternary_expression, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Expression node
  | _ ->
    let* condition_field = child_with_field "condition" node ~err:Expression in
    let* condition = dec_expression ~comments condition_field in
    let* sym_qmark = first_child_named "?" node ~err:Question_mark in
    let sym_qmark = make_sym sym_qmark in
    let* consequence_field = child_with_field "consequence" node ~err:Expression in
    let* consequence = dec_expression consequence_field in
    let* sym_colon = first_child_named ":" node ~err:Colon in
    let sym_colon = make_sym sym_colon in
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
  | "++" -> Ok (Increment (make_sym ~comments node))
  | "--" -> Ok (Decrement (make_sym ~comments node))
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
    let kwd_new = make_kwd ~comments kwd_new in
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
    let kwd_yield = make_kwd kwd_yield in
    (match child_ranked_opt 1 node with
    | None -> Ok (Yield (Wrap.make (kwd_yield, None) region))
    | Some snd_child ->
      (match get_name snd_child with
      | "*" ->
        let sym_star = make_sym snd_child in
        let* expression = child_ranked 2 node ~err:Expression in
        let* expression = dec_expression expression in
        let iterable = kwd_yield, sym_star, expression in
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
    let kwd_as = make_kwd kwd_as in
    let* as_what = child_ranked 2 node ~err:Const_or_type in
    let* as_what = dec_as_what as_what in
    Ok (expression, kwd_as, as_what)

and dec_as_what node : (as_what, _) result =
  match get_name node with
  | "const" -> Ok (As_const (make_kwd node))
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
    let kwd_satisfies = make_kwd kwd_satisfies in
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
    let* contents = dec_expressions index_field in
    let* sym_lbracket = first_child_named "[" node ~err:Left_bracket in
    let opening = make_sym sym_lbracket in
    let* sym_rbracket = first_child_named "]" node ~err:Right_bracket in
    let closing = make_sym sym_rbracket in
    let region = !get_region node in
    let brackets = { opening; contents; closing } in
    let index = Brackets (Wrap.make brackets region) in
    Ok { object_expr; optional_chain; index }

and dec_optional_chain node : (optional_chain, _) result =
  match get_name node with
  | "optional_chain" -> Ok (Optional_chain (make_sym node))
  | _ -> mk_err Optional_chain node

and dec_index node ~comments : (sequence_expression, _) result =
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
        let* selector = first_child_named "." node ~err:Dot in
        Ok (Dot (make_sym selector))
      | Some node -> Ok (Optional_chain (make_sym node))
    in
    Ok ({ object_expr; selector; property } : member_expression)

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
  | _ -> mk_err Property_identifier node

(* Parenthesised expression *)

and dec_parenthesized_expression ?(comments = []) node
    : (parenthesized_expression, _) result
  =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Parenthesized_expression node
  | _ ->
    let comments = comments @ prev_comments node in
    let* sym_lpar = first_child_named "(" node ~err:Left_parenthesis in
    let opening = make_sym ~comments sym_lpar in
    let* sym_rpar = first_child_named ")" node ~err:Right_parenthesis in
    let closing = make_sym sym_rpar in
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
    let sym_colon = make_sym sym_colon in
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
  | "undefined" -> Ok (E_undefined (make_kwd node))
  | "this" -> Ok (E_this (make_kwd node))
  | "super" -> Ok (E_super (make_kwd node))
  | "number" ->
    let* number = dec_number ~comments node in
    Ok (E_number number)
  | "string" -> Ok (E_string (dec_string node))
  | "template_string" ->
    let* expression = wrap dec_template_string ~comments node in
    Ok (E_template_string expression)
  | "regex" -> Ok (E_regex (dec_regex node))
  | "true" -> Ok (E_true (make_kwd node))
  | "false" -> Ok (E_false (make_kwd node))
  | "null" -> Ok (E_null (make_kwd node))
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
  | "import" -> Ok (Import (make_kwd ~comments node))
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
      let kwd_new = make_kwd ~comments fst_child
      and kwd_target = make_kwd snd_child in
      let meta = kwd_new, kwd_target in
      Ok (Meta_new_target (Wrap.make meta region))
    | "import" ->
      let kwd_import = make_kwd ~comments fst_child
      and kwd_meta = make_kwd snd_child in
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
    let kwd_class = make_kwd ~comments kwd_class in
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
    let* sym_star = first_child_named "*" node ~err:Asterisk in
    Ok (make_sym sym_star, fun_decl)

(* Arrow function *)

and dec_arrow_function ?(comments = []) node : (arrow_function, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Arrow_function node
  | _ ->
    let kwd_async = first_child_named_opt "async" node in
    let kwd_async = make_opt make_kwd kwd_async in
    let* sym_arrow = first_child_named "=>" node ~err:Arrow in
    let sym_arrow = make_sym sym_arrow in
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
    let kwd_async = make_opt (make_kwd ~comments:async_comments) kwd_async in
    let* kwd_function = first_child_named "function" node ~err:Function in
    let kwd_function = make_kwd ~comments:function_comments kwd_function in
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
    let opening_bquote = make_sym ~comments opening_bquote in
    let named_children = collect_named_children node in
    let fragments = List.map ~f:dec_template_string_fragment named_children in
    let* fragments = Result.all fragments in
    let* closing_bquote = last_child node ~err:Backquote in
    let closing_bquote = make_sym closing_bquote in
    Ok (opening_bquote, fragments, closing_bquote)

and dec_template_string_fragment ?(comments = []) node
    : (template_string_fragment, _) result
  =
  match get_name node with
  | "string_fragment" -> Ok (String_fragment (make_node ~comments node))
  | "escape_sequence" -> Ok (Escape_sequence (make_node ~comments node))
  | "template_substitution" -> Ok (Template_substitution (make_node ~comments node))
  | _ -> mk_err Template_string node

(* Class expression ("class_" in the grammar) *)

and dec_class_expression ?(comments = []) node : (class_expression, _) result =
  let decorators = children_named "decorator" node in
  let* decorators = list_of_children dec_decorator decorators in
  let* kwd_class = first_child_named "class" node ~err:Class in
  let kwd_class = make_kwd ~comments kwd_class in
  let name_field = child_with_field_opt "name" node in
  let name = make_opt dec_identifier name_field in
  let type_parameters_field = child_with_field_opt "type_parameters" node in
  let* type_parameters = make_opt_res dec_type_parameters type_parameters_field in
  let heritage_child = first_child_named_opt "class_heritage" node in
  let* class_heritage = make_opt_res dec_class_heritage heritage_child in
  let* body_field = child_with_field "body" node ~err:Class_body in
  let* body = dec_class_body body_field in
  Ok { decorators; kwd_class; name; type_parameters; class_heritage; body }

(* PATTERN

   The JavasScript tree-sitter grammar have the non-terminal
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
    let sym_colon = make_sym sym_colon in
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
    let sym_ellipsis = make_sym ~comments sym_ellipsis in
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
    let sym_equal = make_kwd sym_equal in
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
    let sym_equal = make_sym sym_equal in
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
    let* selector = first_child_named "." node ~err:Dot in
    let* property_field = child_with_field "property" node ~err:Property_identifier in
    let dec_object_field node =
      match get_name node with
      | "import" -> Ok (Type_query_object_import (make_kwd ~comments node))
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
    let selector = make_sym selector in
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
  | "import" -> Ok (Type_query_call_import (make_kwd ~comments node))
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
  | "this_type" -> Ok (T_this (make_kwd ~comments node))
  | "existential_type" -> Ok (T_existential_type (make_sym ~comments node))
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

(* Union type *)

and dec_union_type ?(comments = []) node : (union_type, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Union_type node
  | _ ->
    let* first_child = child_ranked 0 node ~err:Type_or_disjunction in
    let* sym_vbar = first_child_named "|" node ~err:Vertical_bar in
    (match get_name first_child with
    | "|" ->
      let sym_vbar = make_sym ~comments sym_vbar in
      let* single_type_node = child_ranked 1 node ~err:Type_expression in
      let* type_expr = dec_type single_type_node in
      Ok (None, sym_vbar, type_expr)
    | _ ->
      (* "type" is a supertype, therefore a hidden rule *)
      let* left_type = dec_type ~comments first_child in
      let sym_vbar = make_sym sym_vbar in
      let* right_type = child_ranked 2 node ~err:Type_expression in
      let* right_type = dec_type right_type in
      Ok (Some left_type, sym_vbar, right_type))

(* Intersection type *)

and dec_intersection_type ?(comments = []) node : (intersection_type, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Intersection_type node
  | _ ->
    let* first_child = child_ranked 0 node ~err:Type_or_conjunction in
    let* sym_ampersand = first_child_named "&" node ~err:Ampersand in
    (match get_name first_child with
    | "&" ->
      let sym_ampersand = make_sym ~comments sym_ampersand in
      let* single_type_node = child_ranked 1 node ~err:Type_expression in
      let* type_expr = dec_type single_type_node in
      Ok (None, sym_ampersand, type_expr)
    | _ ->
      (* "type" is a supertype, therefore a hidden rule *)
      let* left_type = dec_type ~comments first_child in
      let sym_ampersand = make_sym sym_ampersand in
      let* right_type = child_ranked 2 node ~err:Type_expression in
      let* right_type = dec_type right_type in
      Ok (Some left_type, sym_ampersand, right_type))

(* Template literal type *)

and dec_template_literal_type ?(comments = []) node : (template_literal_type, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Template_literal_type node
  | _ ->
    let* opening_bquote = child_ranked 0 node ~err:Backquote in
    let opening_bquote = make_sym ~comments opening_bquote in
    let named_children = collect_named_children node in
    let fragments = List.map ~f:dec_template_type_fragment named_children in
    let* fragments = Result.all fragments in
    let* closing_bquote = last_child node ~err:Backquote in
    let closing_bquote = make_sym closing_bquote in
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
    let kwd_extends = make_kwd kwd_extends in
    let* right_field = child_with_field "right" node ~err:Type_expression in
    let* right = dec_type right_field in
    let* sym_qmark = first_child_named "?" node ~err:Question_mark in
    let sym_qmark = make_sym sym_qmark in
    let* consequence_field = child_with_field "consequence" node ~err:Type_expression in
    let* consequence = dec_type consequence_field in
    let* sym_colon = first_child_named ":" node ~err:Colon in
    let sym_colon = make_sym sym_colon in
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
    let opening = make_sym sym_lbracket in
    let* type_child = next_sibling sym_lbracket ~err:Type_expression in
    let* contents = dec_type type_child in
    let* sym_rbracket = first_child_named "]" node ~err:Right_bracket in
    let closing = make_sym sym_rbracket in
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
    | "true" -> Ok (T_true (make_kwd ~comments child))
    | "false" -> Ok (T_false (make_kwd ~comments child))
    | "null" -> Ok (T_null (make_kwd ~comments child))
    | "undefined" -> Ok (T_undefined (make_kwd ~comments child))
    | _ -> mk_err Literal_type node)

(* Index type query *)

and dec_index_type_query ?(comments = []) node : (kwd_keyof * primary_type, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Index_type_query node
  | _ ->
    let* kwd_keyof = first_child_named "keyof" node ~err:Keyof in
    let kwd_keyof = make_kwd ~comments kwd_keyof in
    let* type_node = child_ranked 1 node ~err:Type_expression in
    let* primary_type = dec_primary_type type_node in
    Ok (kwd_keyof, primary_type)

(* Type query *)

and dec_type_query ?(comments = []) node : (kwd_keyof * type_query, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Type_query node
  | _ ->
    let* kwd_typeof = first_child_named "typeof" node ~err:Keyof in
    let kwd_typeof = make_kwd ~comments kwd_typeof in
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
      | "this" -> Ok (Typeof_this (make_kwd child))
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
    let optional = make_opt make_sym optional_chain in
    let* sym_lbracket = first_child_named "[" node ~err:Left_bracket in
    let opening = make_sym sym_lbracket in
    let* index_field = child_with_field "index" node ~err:Type_or_string_or_number in
    let* contents = dec_type_query_index index_field in
    let* sym_rbracket = first_child_named "]" node ~err:Right_bracket in
    let closing = make_sym sym_rbracket in
    let region = !get_region node in
    let brackets = { opening; contents; closing } in
    let index = Brackets (Wrap.make brackets region) in
    Ok { object_expr; optional; index }

and dec_type_query_object ?(comments = []) node : (type_query_object, _) result =
  match get_name node with
  | "identifier" -> Ok (Type_query_object_identifier (dec_identifier ~comments node))
  | "this" -> Ok (Type_query_object_this (make_kwd ~comments node))
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
  | "." -> Ok (Query_selector_dot (make_sym node))
  | "?." -> Ok (Query_selector_opt_chain (make_sym node))
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
  | "import" -> Ok (Type_query_call_import (make_kwd ~comments node))
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
    let sym_qmark = make_sym ~comments sym_qmark in
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
    let sym_qmark = make_sym sym_qmark in
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
    let sym_qmark = make_sym sym_qmark in
    Ok (type_expr, sym_qmark)

(* Rest type *)

and dec_rest_type ?(comments = []) node : (sym_ellipsis * type_expr, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Rest_type node
  | _ ->
    let* sym_ellipsis = first_child_named "..." node ~err:Ellipsis in
    let sym_ellipsis = make_sym ~comments sym_ellipsis in
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
    let sym_lbracket = make_sym sym_lbracket in
    let* sym_rbracket = first_child_named "]" node ~err:Right_bracket in
    let sym_rbracket = make_sym sym_rbracket in
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
    let sym_qmark = make_opt make_sym sym_qmark in
    let type_field = child_with_field_opt "type" node in
    let* type_ = make_opt_res dec_type_annotation type_field in
    Ok { access; scope; name; sym_qmark; type_ }

(* Construct signature *)

and dec_construct_signature ?(comments = []) node : (construct_signature, _) result =
  match get_name node with
  | "ERROR" | "MISSING" | "NULL" -> mk_err Construct_signature node
  | _ ->
    let kwd_abstract = first_child_named_opt "abstract" node in
    let kwd_abstract = make_opt make_kwd kwd_abstract in
    let* kwd_new = first_child_named "new" node ~err:New in
    let kwd_new = make_kwd ~comments kwd_new in
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
    let kwd_infer = make_kwd ~comments kwd_infer in
    let* type_identifier_child =
      child_ranked 1 node ~err:Identifier (* name "type_identifier"? *)
    in
    let type_id = dec_type_identifier type_identifier_child in
    let* extends =
      match first_child_named_opt "extends" node with
      | None -> Ok None
      | Some kwd_extends ->
        let kwd_extends = make_kwd kwd_extends in
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
    let kwd_abstract = make_opt make_kwd kwd_abstract in
    let* kwd_new = first_child_named "new" node ~err:New in
    let kwd_new = make_kwd kwd_new in
    let type_parameters_field = child_with_field_opt "type_parameters" node in
    let* type_parameters = make_opt_res dec_type_parameters type_parameters_field in
    let* parameters_field = child_with_field "parameters" node ~err:Parameters in
    let* parameters = dec_formal_parameters ~comments parameters_field in
    let* sym_arrow = first_child_named "=>" node ~err:Arrow in
    let sym_arrow = make_sym sym_arrow in
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
    let sym_arrow = make_sym sym_arrow in
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
    let kwd_readonly = make_kwd ~comments kwd_readonly in
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

let dec_program ~no_colour_arg ~debug_arg ~filename ~file (map : Loc_map.t) node
    : (Ast.t, _) result
  =
  (* Setting up the extraction of source regions *)
  let () = get_region := Ts_wrap.get_region filename map in
  (* Setting the input as a top-level string buffer *)
  let () = Buffer.add_string !input file in
  (* Setting colour/no colour for code snippets in syntax error messages *)
  let () = no_colour := no_colour_arg in
  (* Setting debug mode *)
  let () = debug := debug_arg in
  (* Decoding the CST into an AST *)
  dec_statements node

(* The parameter [node] is the root of a Typescript CST, *not of an
   expression*. That's why we have to find the expression below the
   root. This is because tree-sitter does not provide the generated
   parsers with multiple entry-points. *)

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

(* The parameter [node] is the root of a Typescript CST, *not of a
   type expression*. tree-sitter does not provide the generated
   parsers with multiple entry-points, so, in order to parse a type
   expression, we assume that the input string starts with "type t = ",
   so we fetch the type in the produced CST (last child of the root,
   which is an type_alias_declaration). *)

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
