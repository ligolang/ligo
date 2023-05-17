(* Transpilation of PascaLIGO to JsLIGO *)

(* Vendors dependencies *)

module Region  = Simple_utils.Region
module Utils   = Simple_utils.Utils
module Std     = Simple_utils.Std
module Snippet = Simple_utils.Snippet

(* Local dependencies *)

module Wrap  = Lexing_shared.Wrap
module Attr  = Lexing_shared.Attr
module CST   = Cst_pascaligo.CST
module Js    = Cst_jsligo.CST
module Token = Lexing_jsligo.Token

open! CST

(* Utilities *)

let (let*) = Utils.(let*)

let format_error ~no_colour ~file value (region: Region.t) =
  let value =
    if file then
      Printf.sprintf "%s%s"
        (Format.asprintf "%a" (Snippet.pp_lift ~no_colour) region)
        (Std.redden value)
    else
      let header = region#to_string ~file ~offsets:true `Point
      in Printf.sprintf "%s:\n%s" header value
  in Region.{region; value}

let warn_about msg region =
  let msg = format_error ~no_colour:false ~file:true msg region
  in Printf.eprintf "%s\n%!" msg.value

let exit_with msg region = warn_about msg region; exit 1

let pattern_not_transpiled (node: pattern) =
  exit_with "Error: This pattern is not transpiled."
  @@ pattern_to_region node

let type_not_transpiled (node: type_expr) =
  exit_with "Error: This type is not transpiled."
  @@ type_expr_to_region node

let expr_not_transpiled (node: expr) =
  let region = expr_to_region node in
  exit_with "Error: This expression is not transpiled" region

let deep_module_paths_not_transpiled region =
  exit_with "Error: Deep module paths are not transpiled." region

let empty_records_not_transpiled region =
  exit_with "Error: Empty records are not transpiled." region

let type_punning_not_transpiled region =
  exit_with "Error: Type punning is not transpiled." region

let skips_only_not_transpiled (node: statements) =
  let region = CST.nsepseq_to_region CST.statement_to_region node
  in exit_with "Error: Only skip instructions are not transpiled" region

let field_lens_not_transpiled (node: field_lens) =
  let region = field_lens_to_region node in
  exit_with "Error: Field lens are not transpiled." region

let multiple_map_lookups_not_transpiled (index: expr) =
  let region = expr_to_region index in
  exit_with "Multiple map lookups are not transpiled." region

let block_with_not_transpiled region =
  exit_with "Error: Blocks with values are not transpiled in general."
            region

let for_int_not_transpiled region =
  exit_with "Error: Loops on integers are not transpiled." region

let drop_skip (node: kwd_skip) =
  warn_about "Warning: Skip instruction dropped." node#region

(* Attributes *)

let drop_attribute (attr: attribute) =
  warn_about "Warning: Attribute dropped." attr#region

let drop_js_attribute (attr: Js.attribute) =
  warn_about "Warning: Attribute (JS) dropped." attr#region

(* Chevrons *)

let chevrons_of node = Js.{
  lchevron = Token.ghost_lt;
  inside   = node;
  rchevron = Token.ghost_gt}

(* Parentheses *)

let par_of node = Js.{
  lpar   = Token.ghost_lpar;
  inside = node;
  rpar   = Token.ghost_rpar}

(* Braces *)

let braces_of node = Js.{
  lbrace = Token.ghost_lbrace;
  inside = node;
  rbrace = Token.ghost_rbrace}

(* Brackets *)

let brackets_of node = Js.{
  lbracket = Token.ghost_lbracket;
  inside   = node;
  rbracket = Token.ghost_rbracket}

(* Regions *)

let reg_of = Region.wrap_ghost

(* Variables *)

module SSet = Caml.Set.Make (String)

let js_keywords =
  let open SSet in
    empty
    |> add "break"
    |> add "case"
    |> add "const"
    |> add "default"
    |> add "else"
    |> add "export"
    |> add "for"
    |> add "from"
    |> add "if"
    |> add "import"
    |> add "let"
    |> add "of"
    |> add "return"
    |> add "switch"
    |> add "while"
    |> add "as"
    |> add "namespace"
    |> add "type"

let mk_var (variable: lexeme wrap) =
  let v = variable#payload in
  let v = if SSet.mem v js_keywords then "@" ^ v else v
  in Wrap.ghost v


(* CST *)

let rec of_cst (node : CST.t) =
  let {decl; eof} = node in
  let statements = toplevel_statements_of_declarations decl
  in Js.{statements; eof}

(* DECLARATIONS *)

and toplevel_statements_of_declarations (node: declarations) =
  Utils.nseq_map toplevel_statement_of_declaration node

and toplevel_statement_of_declaration = function
  D_Attr      d -> toplevel_statement_of_D_Attr      d
| D_Const     d -> toplevel_statement_of_D_Const     d
| D_Directive d -> toplevel_statement_of_D_Directive d
| D_Fun       d -> toplevel_statement_of_D_Fun       d
| D_Module    d -> toplevel_statement_of_D_Module    d
| D_Type      d -> toplevel_statement_of_D_Type      d

and statement_of_declaration = function
  D_Attr      d -> statement_of_D_Attr      d
| D_Const     d -> statement_of_D_Const     d
| D_Directive d -> statement_of_D_Directive d
| D_Fun       d -> statement_of_D_Fun       d
| D_Module    d -> statement_of_D_Module    d
| D_Type      d -> statement_of_D_Type      d

and statements_of_declarations (node: declarations) =
  let hd, tl          = Utils.nseq_map statement_of_declaration node
  and semi            = Token.ghost_semi in
  let folded stmt acc = (semi, stmt) :: acc
  in hd, List.fold_right folded tl []

(* Attribute declarations *)

and toplevel_statement_of_D_Attr (node: (attribute * declaration) reg) =
  let attribute, declaration = node.value in
  let toplevel_statement =
    toplevel_statement_of_declaration declaration in
  add_attribute_to_toplevel_statement attribute toplevel_statement

and statement_of_D_Attr (node: (attribute * declaration) reg) =
  let attribute, declaration = node.value in
  let statement = statement_of_declaration declaration in
  add_attribute_to_statement attribute statement

and add_attribute_to_toplevel_statement
    (attr: attribute) (toplevel_statement: Js.toplevel_statement) =
  match toplevel_statement with
    Js.TopLevel (statement, terminator) ->
      let statement = add_attribute_to_statement attr statement
      in Js.TopLevel (statement, terminator)
  | Directive _ as d -> drop_attribute attr; d

and add_attribute_to_statement (attr: attribute) = function
  SConst     s -> add_attribute_to_SConst     attr s
| SLet       s -> add_attribute_to_SLet       attr s
| SNamespace s -> add_attribute_to_SNamespace attr s
| SType      s -> add_attribute_to_SType      attr s
| SExpr      s -> add_attribute_to_SExpr      attr s
| SCond      s -> add_attribute_to_SCond      attr s
| SForOf     s -> add_attribute_to_SForOf     attr s
| stmt         -> drop_attribute attr; stmt

and add_attribute_to_SConst (attr: attribute) (node: Js.const_decl reg) =
  let attributes = attr :: node.value.attributes in
  let value = {node.value with attributes}
  in Js.SConst {node with value}

and add_attribute_to_SLet (attr: attribute) (node: Js.let_decl reg) =
  let attributes = attr :: node.value.attributes in
  let value = {node.value with attributes}
  in Js.SLet {node with value}

and add_attribute_to_SNamespace
    (attr: attribute)
    (node: Js.namespace_statement reg) =
  let kwd_namespace, module_name, statements, attributes = node.value in
  let attributes = attr :: attributes in
  let namespace  = kwd_namespace, module_name, statements, attributes
  in Js.SNamespace (reg_of namespace)

and add_attribute_to_SType (attr: attribute) (node: Js.type_decl reg) =
  let attributes = attr :: node.value.attributes in
  let value = {node.value with attributes}
  in Js.SType {node with value}

and add_attribute_to_SExpr (attr: attribute) (node: attribute list * Js.expr) =
  let attributes, expr = node in
  Js.SExpr (attr :: attributes, expr)

and add_attribute_to_SCond (attr: attribute) (node: Js.cond_statement reg) =
  let attributes = attr :: node.value.attributes in
  let value = {node.value with attributes}
  in Js.SCond {node with value}

and add_attribute_to_SForOf (attr: attribute) (node: Js.for_of reg) =
  let attributes = attr :: node.value.attributes in
  let value = {node.value with attributes}
  in Js.SForOf {node with value}

(* Constant declarations *)

and toplevel_statement_of_D_Const (node: const_decl reg)
  : Js.toplevel_statement =
  Js.TopLevel (statement_of_D_Const node, None)

and statement_of_D_Const (node: const_decl reg) : Js.statement =
  let {kwd_const; pattern; type_params; const_type; init; _} = node.value in
  let binders     = pattern_of_pattern pattern
  and type_params = type_params_of_type_params type_params
  and lhs_type    = of_type_annotation const_type
  and eq          = Token.ghost_eq
  and expr        = expr_of_expr init in
  let val_binding = Js.{binders; type_params; lhs_type; eq; expr} in
  let val_binding = reg_of val_binding in
  let const_decl  = Js.{attributes = [];
                        kwd_const;
                        bindings   = val_binding, []}
  in Js.SConst (reg_of const_decl)

and pattern_of_pattern (node: pattern) : Js.pattern =
  match node with
    P_List   p -> pattern_of_P_List   p
  | P_Par    p -> pattern_of_P_Par    p
  | P_Record p -> pattern_of_P_Record p
  | P_Tuple  p -> pattern_of_P_Tuple  p
  | P_Var    p -> pattern_of_P_Var    p
  | _          -> pattern_not_transpiled node

(* Non-empty list patterns (e.g. "list [4;x]") *)

and pattern_of_P_List (node: pattern compound reg) =
  let elements = node.value.elements in
  match elements with
    None -> pattern_not_transpiled (P_List node)
  | Some nsepseq ->
      (* We let the semicolons be, as they are ignored by the
         pretty-printer *)
      let nsepseq = Utils.nsepseq_map pattern_of_pattern nsepseq in
      let value   = brackets_of nsepseq in
      let js_list_pattern = reg_of value
      in Js.PArray js_list_pattern

(* Parenthesised pattern (e.g. "(C, 4)" *)

and pattern_of_P_Par (node: pattern par reg) =
  pattern_of_pattern node.value.inside

(* Record pattern (e.g. "record [x=y; z]") *)

and pattern_of_P_Record (node: record_pattern) =
  let field_patterns = node.value.elements in
  match field_patterns with
    None -> pattern_not_transpiled (P_Record node)
  | Some nsepseq ->
      (* We let the semicolons be, as they are ignored by the
         pretty-printer *)
      let nsepseq = Utils.nsepseq_map pattern_of_field_pattern nsepseq in
      let list    = reg_of (braces_of nsepseq)
      in Js.PObject list

and pattern_of_field_pattern (node: field_pattern reg) =
  match node.value with
    Punned {pun; attributes} -> (
      match pun with
        P_Var v ->
          let var_pattern : Js.var_pattern = {
            variable = mk_var v; attributes;}
          in Js.PVar (reg_of var_pattern)
      | _ -> pattern_not_transpiled pun
    )
  | Complete {field_lhs; field_lens=_; field_rhs; attributes} ->
      let () = check_attributes attributes in
      let property : Js.variable =
        match field_lhs with
          P_Var v -> mk_var v
        | _ -> pattern_not_transpiled field_lhs in
      let rhs_var =
        match field_rhs with
          P_Var v -> mk_var v
        | _ -> pattern_not_transpiled field_rhs in
      let val_binding : Js.val_binding =
        Js.{binders     = pattern_of_pattern field_rhs;
            type_params = None;
            lhs_type    = None;
            eq          = Token.ghost_eq;
            expr        = EVar rhs_var} in
      let target   = reg_of val_binding in
      let destruct = Js.{property; colon=Token.ghost_colon; target} in
      let destruct = reg_of destruct
      in Js.(PDestruct destruct)

(* Tuple patterns (e.g. "(1, x)") *)

and pattern_of_P_Tuple (node: pattern tuple) =
  let components    = node.value.inside in
  let components    = Utils.nsepseq_map pattern_of_pattern components in
  let array_pattern = brackets_of components
  in PArray (reg_of array_pattern)

(* Pattern variables *)

and pattern_of_P_Var (node: variable) =
  let var_pattern = Js.{variable = mk_var node; attributes=[]}
  in PVar (reg_of var_pattern)

(* Preprocessing directives *)

and toplevel_statement_of_D_Directive (node: Directive.t) = Directive node

and statement_of_D_Directive (node: Directive.t) =
  exit_with "Directives must be top-level." (Directive.to_region node)

(* Function declaration *)

and toplevel_statement_of_D_Fun (node: fun_decl reg) : Js.toplevel_statement =
  Js.TopLevel (statement_of_D_Fun node, None)

and statement_of_D_Fun (node: fun_decl reg) : Js.statement =
  let {kwd_recursive; fun_name; type_params;
       parameters; ret_type; return; _} = node.value in
  let kwd_const   = match kwd_recursive with
                      None     -> Token.ghost_const
                    | Some kwd -> kwd in
  let binders     = pattern_of_P_Var fun_name
  and type_params = type_params_of_type_params type_params in
  let lhs_type    = of_type_annotation ret_type
  and eq          = Token.ghost_eq in
  let expr        = fun_expr_of type_params parameters lhs_type return in
  let val_binding = Js.{binders; type_params; lhs_type=None; eq; expr} in
  let val_binding = reg_of val_binding in
  let const_decl  = Js.{attributes = [];
                        kwd_const;
                        bindings = val_binding, []}
  in Js.SConst (reg_of const_decl)

and type_params_of_type_params (node: type_params chevrons reg option) =
  match node with
    None   -> None
  | Some p ->
      let type_params = Utils.nsepseq_map mk_var p.value.inside
      in Some (reg_of (chevrons_of type_params))

and of_type_annotation (node: type_annotation option) =
  match node with
    None -> None
  | Some (_, type_expr) ->
      let type_expr = type_expr_of_type_expr type_expr
      in Some (Token.ghost_colon, type_expr)

and fun_expr_of
    (type_params: Js.type_generics option)
    (parameters: parameters)
    (lhs_type: (colon * Js.type_expr) option)
    (return: expr) =
  let of_param_decl (node: param_decl reg) =
    let {pattern; param_type; _} = node.value in
    let expr = expr_of_pattern pattern in
    match param_type with
       None -> expr
     | Some (_, t) ->
         let param_type = type_expr_of_type_expr t
         and colon      = Token.ghost_colon in
         let e_annot    = expr, colon, param_type
         in Js.EAnnot (reg_of e_annot) in
  let body = body_of_expr ~parameters return in
  let parameters =
    match parameters.value.inside with
      None ->
        Js.EUnit (reg_of Token.(ghost_lpar, ghost_rpar))
    | Some (head, tail) ->
        let comma   = Token.ghost_comma in
        let head    = of_param_decl head
        and tail    = List.map (fun (_,d) -> comma, of_param_decl d) tail in
        let nsepseq = Js.ESeq (reg_of (head, tail)) in
        Js.EPar (reg_of (par_of nsepseq)) in
  let arrow    = Token.ghost_arrow in
  let fun_expr = Js.{type_params; parameters; lhs_type; arrow; body}
  in Js.EFun (reg_of fun_expr)

and body_of_expr ?(parameters: parameters option) = function
  E_Block block_with ->
    let body = statements_of_block_with ?parameters block_with
    in Js.FunctionBody (reg_of (braces_of body))
| expr -> Js.ExpressionBody (expr_of_expr expr)

and let_of_param_decl (node: param_decl reg) =
  let {pattern; param_kind; _} : param_decl = node.value in
  match param_kind with
    `Var _ ->
       let binders      = pattern_of_pattern pattern
       and type_params  = None
       and lhs_type     = None
       and eq           = Token.ghost_eq
       and expr         = expr_of_pattern pattern in
       let val_binding  = Js.{binders; type_params; lhs_type; eq; expr} in
       let val_binding  = reg_of val_binding in
       let let_decl     = Js.{attributes = [];
                              kwd_let    = Token.ghost_let;
                              bindings   = val_binding, []}
       in Some (Js.SLet (reg_of let_decl))
  | `Const _ -> None

and statements_of_block_with ?(parameters: parameters option)
                             (node: block_with reg) =
  let {block; expr; _} = node.value in
  let {statements; _}  = block.value in
  let statements_opt   = statements_of_statements statements
  and kwd_return       = Token.ghost_return
  and expr             = Some (expr_of_expr expr) in
  match statements_opt with
    None -> Js.SReturn (reg_of Js.{kwd_return; expr}), []
  | Some statements ->
      let let_stmt =
        match parameters with
          None -> None
        | Some parameters ->
            match parameters.value.inside with
              None -> None
            | Some nsepseq ->
                Some (Utils.nsepseq_map let_of_param_decl nsepseq) in
      let return      = Js.{kwd_return; expr} in
      let return_stmt = Js.SReturn (reg_of return) in
      let init        = return_stmt, []
      and semi        = Token.ghost_semi in
      let folded stmt = Utils.nsepseq_cons stmt semi in
      let stmts       = Utils.nsepseq_foldr folded statements init in
      let folded stmt acc =
        match stmt with
          None -> acc
        | Some stmt -> Utils.nsepseq_cons stmt semi acc
      in Utils.sepseq_foldr folded let_stmt stmts

and region_of_param_kind = function
  `Var   kwd_var   -> kwd_var#region
| `Const kwd_const -> kwd_const#region

and expr_of_pattern (node: pattern) : Js.expr =
  match node with
    P_Tuple p -> expr_of_P_Tuple p
  | P_Var   p -> Js.EVar (mk_var p)
  | _         -> pattern_not_transpiled node

and expr_of_P_Tuple (node: pattern tuple) : Js.expr =
  let components  = node.value.inside in
  let array_items = Utils.nsepseq_map array_item_of_pattern components in
  let array_items = Some array_items in
  let e_array     = brackets_of array_items
  in Js.EArray (reg_of e_array)

and array_item_of_pattern (node: pattern) : Js.array_item =
  Js.Expr_entry (expr_of_pattern node)

(* Module declaration *)

and toplevel_statement_of_D_Module (node: module_decl reg)
  : Js.toplevel_statement =
  Js.TopLevel (statement_of_D_Module node, None)

and statement_of_D_Module (node: module_decl reg) : Js.statement =
  let {kwd_module; name; module_expr; _} = node.value in
  match module_expr with
    M_Body m -> namespace_of_M_Body kwd_module name m
  | M_Path m -> import_of_M_Path    kwd_module name m
  | M_Var  m -> import_of_M_Var     kwd_module name m

and namespace_of_M_Body
    kwd_namespace (name: module_name) (m: module_body reg) =
  let declarations  = m.value.declarations in
  let module_name   = Wrap.ghost name#payload
  and attributes    = []
  and first_decl,
      other_decls   = declarations in
  let first_stmt    = statement_of_declaration first_decl
  and other_stmts   = List.map statement_of_declaration other_decls in
  let comma         = Token.ghost_comma in
  let statements    = first_stmt,
                      List.map (fun stmt -> comma, stmt) other_stmts in
  let statements    = reg_of (braces_of statements) in
  let namespace     = kwd_namespace, module_name, statements, attributes
  in Js.SNamespace (reg_of namespace)

and import_of_M_Path
    kwd_import (alias: module_name) (m: module_name module_path reg) =
  let {module_path; field; _} = m.value in
  let dot          = Token.ghost_dot
  and equal        = Token.ghost_eq in
  let first_mod,
      other_mods   = module_path in
  let other_mods   =
    List.fold_right (fun x a -> x::a) other_mods [(dot, field)] in
  let module_path  = first_mod, other_mods in
  let import       =
    Js.Import_rename {kwd_import; alias; equal; module_path} in
  let import       = reg_of import
  in Js.SImport import

and import_of_M_Var
    kwd_import (alias: module_name) (m: module_name) =
  let equal       = Token.ghost_eq
  and module_path = m, [] in
  let import      =
    Js.Import_rename {kwd_import; alias; equal; module_path}
  in Js.SImport (reg_of import)

(* Type declaration *)

and toplevel_statement_of_D_Type (node: type_decl reg)
  : Js.toplevel_statement =
  Js.TopLevel (statement_of_D_Type node, None)

and statement_of_D_Type (node: type_decl reg) : Js.statement =
  let {kwd_type; name; params; type_expr; _} = node.value in
  let mk_params (params: variable tuple) =
    let type_var_of_variable (node: variable) : Js.type_var =
      mk_var node in
    let type_vars =
      Utils.nsepseq_map type_var_of_variable params.value.inside in
    let type_vars = chevrons_of type_vars
    in Some (reg_of type_vars) in
  let attributes = []
  and name       = mk_var name in
  let params     = Option.bind params mk_params in
  let eq         = Token.ghost_eq
  and type_expr  = type_expr_of_type_expr type_expr in
  let type_decl  =
    Js.{attributes; kwd_type; name; params; eq; type_expr}
  in Js.SType (reg_of type_decl)

(* TYPE EXPRESSIONS *)

and type_expr_of_type_expr = function
  T_App     t -> of_T_App     t
| T_Attr    t -> of_T_Attr    t
| T_Cart    t -> of_T_Cart    t
| T_Fun     t -> of_T_Fun     t
| T_Int     t -> of_T_Int     t
| T_ModPath t -> of_T_ModPath t
| T_Par     t -> of_T_Par     t
| T_Record  t -> of_T_Record  t
| T_String  t -> of_T_String  t
| T_Sum     t -> of_T_Sum     t
| T_Var     t -> of_T_Var     t

(* Application of a data constructor (e.g. "M.t (x,y,z)") *)

and of_T_App (node: (type_expr * type_tuple) reg) =
  let type_expr, type_tuple = node.value in
  let type_ctor =
    match type_expr with
      T_Var v -> mk_var v
    | _ -> type_not_transpiled type_expr in
  let nsepseq = type_tuple.value.inside in
  let nsepseq = Utils.nsepseq_map type_expr_of_type_expr nsepseq in
  let type_params = reg_of @@ chevrons_of nsepseq
  in Js.TApp (reg_of (type_ctor, type_params))

(* Attributed type (e.g. "[@a] x") *)

and of_T_Attr (node: attribute * type_expr) =
  let attribute, type_expr = node in
  let type_expr = type_expr_of_type_expr type_expr
  in add_attribute_to_type_expr attribute type_expr

and add_attribute_to_type_expr (attr: Js.attribute) = function
  TProd   t -> add_attribute_to_TProd   attr t
| TSum    t -> add_attribute_to_TSum    attr t
| TObject t -> add_attribute_to_TObject attr t
| TModA   t -> add_attribute_to_TModA   attr t
|         t -> drop_js_attribute attr; t

and add_attribute_to_TProd (attr: Js.attribute) (node: Js.cartesian) =
  Js.TProd {node with attributes = attr :: node.attributes}

and add_attribute_to_TSum (attr: Js.attribute) (node: Js.sum_type reg) =
  let value = {node.value with attributes = attr :: node.value.attributes}
  in Js.TSum {node with value}

and add_attribute_to_TObject (attr: Js.attribute) (node: Js.obj_type) =
  let value = {node.value with attributes = attr :: node.value.attributes}
  in Js.TObject {node with value}

and add_attribute_to_TModA
    (attr: Js.attribute)
    (node: Js.type_expr Js.module_access reg) =
  let field = add_attribute_to_type_expr attr node.value.field in
  let value = {node.value with field}
  in Js.TModA {node with value}

(* Product types (e.g. "x * (y * z)") *)

and of_T_Cart (node: cartesian) =
  let type_expr, _, nsepseq = node.value in
  let first_arg  = type_expr_of_type_expr type_expr in
  let other_args = Utils.nsepseq_map type_expr_of_type_expr nsepseq in
  let comma      = Token.ghost_comma in
  let all_args   = Utils.nsepseq_cons first_arg comma other_args in
  let inside     = reg_of @@ brackets_of all_args
  in Js.TProd Js.{inside; attributes=[]}

(* Functional types (e.g. "x -> y") *)

and of_T_Fun (node: (type_expr * arrow * type_expr) reg) =
  let domain, _, codomain = node.value in
  let name          = Wrap.ghost (Utils.gen_sym ())
  and colon         = Token.ghost_colon
  and type_expr     = type_expr_of_type_expr domain in
  let fun_type_arg  = Js.{name; colon; type_expr} in
  let fun_type_args = par_of (fun_type_arg,[]) in
  let codomain      = type_expr_of_type_expr codomain
  in Js.TFun (reg_of (fun_type_args, colon, codomain))

(* Integer literal in types (e.g. "42") *)

and of_T_Int (node: (lexeme * Z.t) wrap) = Js.TInt node

(* Qualified types (e.g. "A.B.(x * y)") *)

and of_T_ModPath (node: type_expr module_path reg) =
  let {module_path; selector; field} = node.value in
  match module_path with
    module_name, [] ->
      let field  = type_expr_of_type_expr field in
      let access = Js.{module_name; selector; field}
      in Js.TModA (reg_of access)
  | _ -> deep_module_paths_not_transpiled node.region

(* Parenthesised types (e.g. "(x -> y)" *)

and of_T_Par (node: type_expr par reg) =
  let type_expr = type_expr_of_type_expr node.value.inside
  in Js.TPar (reg_of (par_of type_expr))

(* Record type (e.g. "record [a; [@a1] b : t]") *)

and of_T_Record (node: field_decl reg compound reg) =
  match node.value.elements with
    None -> empty_records_not_transpiled node.region
  | Some elms ->
      let ne_elements =
        Utils.nsepseq_map field_decls_of_field_decls elms
      and compound   = Js.Braces (Token.ghost_lbrace, Token.ghost_rbrace)
      and terminator = None in
      let compound   = Some compound in
      let obj_type   = Js.{compound; ne_elements; terminator; attributes=[]}
      in Js.TObject (reg_of obj_type)

and field_decls_of_field_decls (node: field_decl reg) : Js.field_decl reg =
  let {field_name; field_type; attributes; _} = node.value in
  let field_name = mk_var field_name
  and colon      = Token.ghost_colon
  and field_type =
    match field_type with
      None -> type_punning_not_transpiled node.region
    | Some (_,t) -> type_expr_of_type_expr t in
  let field_decl = Js.{field_name; colon; field_type; attributes}
  in reg_of field_decl

(* String literal in types (e.g. "foo") *)

and of_T_String (node: lexeme wrap) = Js.TString node

(* Sum types (e.g. "[@a] A | B of t") *)

and of_T_Sum (node: sum_type reg) : Js.type_expr =
  let node         = node.value in
  let leading_vbar = node.lead_vbar
  and attributes   = []
  and variants     = node.variants in
  let variants     = Utils.nsepseq_map variant_of_variant variants in
  let variants     = reg_of variants in
  let sum_type     = Js.{leading_vbar; variants; attributes}
  in Js.TSum (reg_of sum_type)

and variant_of_variant (node: variant reg) : Js.variant reg =
  let node         = node.value in
  let attributes   = node.attributes
  and constr       = node.ctor
  and params       = params_of_ctor_args node.ctor_args in
  let variant_comp = Js.{constr; params} in
  let tuple        = reg_of @@ brackets_of variant_comp
  in reg_of Js.{tuple; attributes}

and params_of_ctor_args (node: (kwd_of * type_expr) option) =
  match node with
    None -> None
  | Some (_, type_expr) ->
      match type_expr_of_type_expr type_expr with
        Js.TProd Js.{inside; attributes} -> (
          let () = match attributes with
                     []      -> ()
                   | attr::_ -> drop_js_attribute attr in
          let nsepseq = inside.value.inside in
          Some (Token.ghost_comma, nsepseq))
      | type_expr -> Some (Token.ghost_comma, (type_expr,[]))

(* Type variable (e.g. "t") *)

and of_T_Var (node: variable) = Js.TVar (mk_var node)

(* EXPRESSIONS *)

and expr_of_expr = function
  E_Add       e -> expr_of_E_Add       e
| E_And       e -> expr_of_E_And       e
| E_App       e -> expr_of_E_App       e
| E_Attr      e -> expr_of_E_Attr      e
| E_BigMap    e -> expr_of_E_BigMap    e
| E_Block     e -> expr_of_E_Block     e
| E_Bytes     e -> expr_of_E_Bytes     e
| E_Case      e -> expr_of_E_Case      e
| E_Cat       e -> expr_of_E_Cat       e
| E_CodeInj   e -> expr_of_E_CodeInj   e
| E_Ctor      e -> expr_of_E_Ctor      e
| E_Cond      e -> expr_of_E_Cond      e
| E_Cons      e -> expr_of_E_Cons      e
| E_Div       e -> expr_of_E_Div       e
| E_Equal     e -> expr_of_E_Equal     e
| E_Fun       e -> expr_of_E_Fun       e
| E_Geq       e -> expr_of_E_Geq       e
| E_Gt        e -> expr_of_E_Gt        e
| E_Int       e -> expr_of_E_Int       e
| E_Leq       e -> expr_of_E_Leq       e
| E_List      e -> expr_of_E_List      e
| E_Lt        e -> expr_of_E_Lt        e
| E_Map       e -> expr_of_E_Map       e
| E_MapLookup e -> expr_of_E_MapLookup e
| E_Mod       e -> expr_of_E_Mod       e
| E_ModPath   e -> expr_of_E_ModPath   e
| E_Mult      e -> expr_of_E_Mult      e
| E_Mutez     e -> expr_of_E_Mutez     e
| E_Nat       e -> expr_of_E_Nat       e
| E_Neg       e -> expr_of_E_Neg       e
| E_Neq       e -> expr_of_E_Neq       e
| E_Nil       e -> expr_of_E_Nil       e
| E_Not       e -> expr_of_E_Not       e
| E_Or        e -> expr_of_E_Or        e
| E_Par       e -> expr_of_E_Par       e
| E_Proj      e -> expr_of_E_Proj      e
| E_Record    e -> expr_of_E_Record    e
| E_Set       e -> expr_of_E_Set       e
| E_SetMem    e -> expr_of_E_SetMem    e
| E_String    e -> expr_of_E_String    e
| E_Sub       e -> expr_of_E_Sub       e
| E_Tuple     e -> expr_of_E_Tuple     e
| E_Typed     e -> expr_of_E_Typed     e
| E_Update    e -> expr_of_E_Update    e
| E_Var       e -> expr_of_E_Var       e
| E_Verbatim  e -> expr_of_E_Verbatim  e

(* Arithmetic addition (e.g. "x + y") *)

and expr_of_E_Add (node: plus bin_op reg) =
  Js.(EArith (Add (expr_of_bin_op node)))

and expr_of_bin_op (node: lexeme wrap bin_op reg) =
  let {op; arg1; arg2} = node.value in
  let arg1 = expr_of_expr arg1
  and arg2 = expr_of_expr arg2 in
  reg_of Js.{op; arg1; arg2}

(* Logical conjunction (e.g. "x and y") *)

and expr_of_E_And (node: kwd_and bin_op reg) =
  Js.(ELogic (BoolExpr (And (expr_of_bin_op node))))

(* Application of data constructor (e.g. "Foo (x,y)") *)

and expr_of_E_App (node: call) =
  let fun_or_ctor, args = node.value in
  let args = args.value.inside in
  match fun_or_ctor, args with
    E_Ctor ctor, Some args ->
      let econstr = Wrap.ghost ctor#payload in
      let args    = Utils.nsepseq_map expr_of_expr args in
      let args    = Js.ESeq (reg_of args) in
      let app     = econstr, Some args
      in Js.EConstr (reg_of app)
  | E_Ctor ctor, None ->
      let econstr = Wrap.ghost ctor#payload in
      let app     = econstr, None
      in Js.EConstr (reg_of app)
  | _, None ->
      let fun_expr = expr_of_expr fun_or_ctor
      and unit     = Token.(ghost_lpar, ghost_rpar) in
      let unit     = Js.Unit (reg_of unit)
      in Js.ECall (reg_of (fun_expr, unit))
  | _, Some args ->
      let fun_expr = expr_of_expr fun_or_ctor
      and args     = Utils.nsepseq_map expr_of_expr args in
      let args     = reg_of (par_of args) in
      let ecall    = fun_expr, Js.Multiple args
      in Js.ECall (reg_of ecall)

and item_of_expr (expr: expr) : Js.array_item =
  Js.Expr_entry (expr_of_expr expr)

(* Attributes expression (e.g., "[@a] (x,y)" *)

and expr_of_E_Attr (node: attribute * expr) =
  let attr, expr = node in
  let () = drop_attribute attr
  in expr_of_expr expr

(* Big map expression *)

and expr_of_E_BigMap (node: binding reg compound reg) =
  expr_of_map "Big_map" node

(* Block with expression *)

and expr_of_E_Block (node: block_with reg) =
  block_with_not_transpiled node.region

(* Bytes as expression (e.g. "0xFFFA") *)

and expr_of_E_Bytes (node: (lexeme * Hex.t) wrap) = Js.EBytes node

(* Case expression *)
(*
case action of [
  Assets (params) -> {
    const res = h (params, s.assets);
    s.assets := res.1;
  } with (res.0, s)
| Transfer_ownership(params) -> j (params, s)
]
==>
match (action, {
  Assets: params => {
    const rest = h (params, s.assets);
    s.assets = res[1];
    return [res[0], s]},
  Transfer_ownership: (params) => j (params, s)
})
*)

and expr_of_E_Case (node: expr case reg) =
  let {expr; cases; _} = node.value in
  let fun_name  = Js.EVar (Wrap.ghost "match")
  and obj_expr  = obj_of_cases body_of_expr cases
  and comma     = Token.ghost_comma
  and subject   = expr_of_expr expr in
  let arguments = subject, [comma, obj_expr] in
  let arguments = Js.Multiple (reg_of (par_of arguments))
  in Js.ECall (reg_of (fun_name, arguments))

and obj_of_cases :
  'a.('a -> Js.body) -> ('a case_clause reg, vbar) Utils.nsepseq -> Js.expr =
  fun mk_body node ->
    let properties : (Js.property, comma) Utils.nsepseq =
      let head, tail = node in
      let head = property_of_case_clause mk_body head
      and f (_, case) =
        Token.ghost_comma, property_of_case_clause mk_body case
      in head, List.map f tail
    in Js.EObject (reg_of (braces_of properties))

and property_of_case_clause :
  'a.('a -> Js.body) -> 'a case_clause reg -> Js.property =
  fun mk_body node ->
    let {pattern; rhs; _} = node.value in
    match pattern with
      P_App  p -> property_of_P_App  (mk_body rhs) p
    | P_Ctor p -> property_of_P_Ctor (mk_body rhs) p
    | _        -> pattern_not_transpiled pattern

and property_of_P_App
    (body: Js.body)
    (node: (pattern * pattern tuple option) reg) =
  match node.value with
    P_Ctor p, None -> property_of_P_Ctor body p
  | P_Ctor p, Some tuple ->
      let name        = Js.EVar p
      and type_params = None in
      let parameters  = tuple.value.inside in
      let parameters  = Utils.nsepseq_map expr_of_pattern parameters in
      let parameters  = Js.ESeq (reg_of parameters)
      and lhs_type    = None
      and arrow       = Token.ghost_arrow in
      let value       = Js.{type_params; parameters; lhs_type;
                            arrow; body} in
      let value       = Js.EFun (reg_of value)
      and colon       = Token.ghost_colon in
      Js.(Property (reg_of {attributes=[]; name; colon; value}))
  | pattern, _ -> pattern_not_transpiled pattern

and property_of_P_Ctor (body: Js.body) (node: ctor) : Js.property =
  let name        = Js.EVar node
  and type_params = None
  and unit        = Token.(ghost_lpar, ghost_rpar) in
  let parameters  = Js.EUnit (reg_of unit)
  and lhs_type    = None
  and arrow       = Token.ghost_arrow in
  let value       = Js.{type_params; parameters; lhs_type; arrow; body} in
  let value       = Js.EFun (reg_of value)
  and colon       = Token.ghost_colon in
  Js.(Property (reg_of {attributes=[]; name; colon; value}))

(* String concatenation (e.g., "Hello" ^ world) *)

and expr_of_E_Cat (node: caret bin_op reg) =
  Js.(EArith (Add (expr_of_bin_op node)))

(* Code injection *)

and expr_of_E_CodeInj (node: code_inj reg) =
  let {language; code; _} = node.value in
  let language            = Wrap.ghost language#payload.value
  and code                = expr_of_expr code
  in Js.(ECodeInj (reg_of {language; code}))

(* Data constructor as expression (e.g., "C") *)

and expr_of_E_Ctor (node: ctor) =
  if node#payload = "Unit" then
    let unit = Token.(ghost_lpar, ghost_rpar)
    in Js.EUnit (reg_of unit)
  else
    let constr = Wrap.ghost node#payload
    in Js.EConstr (reg_of (constr, None))

(* Conditional expression *)

and expr_of_E_Cond (node: expr conditional reg) =
  let {test; if_so; if_not; _} = node.value in
  let condition = expr_of_expr test
  and truthy    = expr_of_expr if_so
  and qmark     = Token.ghost_qmark
  and colon     = Token.ghost_colon in
  let falsy =
    match if_not with
      Some (_, if_not) -> expr_of_expr if_not
    | None ->
        let unit = Token.(ghost_lpar, ghost_rpar)
        in Js.EUnit (reg_of unit) in
  let ternary = Js.{condition; qmark; truthy; colon; falsy}
  in Js.ETernary (reg_of ternary)

(* Consing (e.g. "head :: tail") *)

and expr_of_E_Cons (node: sharp bin_op reg) =
  let {arg1; arg2; _} = node.value in
  let item        = expr_of_expr arg1 in
  let item        = Js.Expr_entry item in

  let expr        = expr_of_expr arg2 in
  let ellipsis    = Token.ghost_ellipsis in
  let rest : Js.array_item_rest
                  = Js.{ellipsis; expr} in
  let rest_entry  = Js.Rest_entry (reg_of rest)
  in
  let comma       = Token.ghost_comma in
  let array_items = item, [comma, rest_entry] in
  let array_items = Some array_items in
  let e_array     = brackets_of array_items in
  let e_array     = Js.EArray (reg_of e_array) in
  let arg         = Js.Multiple (reg_of (par_of (e_array,[]))) in
  let list        = Js.EVar (Wrap.ghost "list")
  in Js.ECall (reg_of (list, arg))

(* Integer division (e.g. "x / y") *)

and expr_of_E_Div (node: slash bin_op reg) =
  Js.(EArith (Div (expr_of_bin_op node)))

(* Equality (e.g. "x = y") *)

and expr_of_E_Equal (node: equal bin_op reg) =
  Js.(ELogic (CompExpr (Equal (expr_of_bin_op node))))

(* Functional expression (e.g. "fun x -> x") *)

and expr_of_E_Fun (node: fun_expr reg) =
  let {type_params; parameters; ret_type; return; _} = node.value in
  let type_params = type_params_of_type_params type_params in
  let lhs_type    = of_type_annotation ret_type in
  fun_expr_of type_params parameters lhs_type return

(* Greater of Equal than (e.g., "x >= y") *)

and expr_of_E_Geq (node: geq bin_op reg) =
  Js.(ELogic (CompExpr (Geq (expr_of_bin_op node))))

(* Greater than "x > y" *)

and expr_of_E_Gt (node: gt bin_op reg) =
  Js.(ELogic (CompExpr (Gt (expr_of_bin_op node))))

(* Integers as expressions (e.g. "42") *)

and expr_of_E_Int (node: (lexeme * Z.t) wrap) = Js.(EArith (Int node))

(* Lower or Equal then (e.g., "x <= y") *)

and expr_of_E_Leq (node: leq bin_op reg) =
  Js.(ELogic (CompExpr (Leq (expr_of_bin_op node))))

(* List of expressions (e.g. "list [4;5]") *)

and expr_of_E_List (node: expr compound reg) =
  match node.value.elements with
    None -> expr_of_nil ()
  | Some elems ->
      let array_items = Utils.nsepseq_map array_item_of_expr elems in
      let array_items = Some array_items in
      let e_array     = brackets_of array_items in
      let e_array     = Js.EArray (reg_of e_array) in
      let arg         = Js.Multiple (reg_of (par_of (e_array,[]))) in
      let list        = Js.EVar (Wrap.ghost "list")
      in Js.ECall (reg_of (list, arg))

(* Lower than (e.g. "x < y") *)

and expr_of_E_Lt (node: lt bin_op reg) =
  Js.(ELogic (CompExpr (Lt (expr_of_bin_op node))))

(* Map expressions (e.g. map [3 -> "x"]) *)

and expr_of_E_Map (node: binding reg compound reg) =
  expr_of_map "Map" node

and expr_of_map (kind: string) (node: binding reg compound reg) =
  match node.value.elements with
    None ->
      let module_name = Wrap.ghost kind
      and selector    = Token.ghost_dot
      and field       = Js.EVar (Wrap.ghost "empty") in
      let access      = Js.{module_name; selector; field}
      in Js.EModA (reg_of access)
  | Some elems ->
      let array_items = Utils.nsepseq_map array_item_of_binding elems in
      let array_items = Some array_items in
      let e_array     = brackets_of array_items
      in
      let e_array     = Js.EArray (reg_of e_array) in
      let list_args   = Js.Multiple (reg_of (par_of (e_array,[]))) in
      let list        = Js.EVar (Wrap.ghost "list") in
      let list_call   = Js.ECall (reg_of (list, list_args))
      in
      let module_name = Wrap.ghost "Map"
      and selector    = Token.ghost_dot
      and field       = Js.EVar (Wrap.ghost "literal") in
      let map_literal = Js.{module_name; selector; field} in
      let map_literal = Js.EModA (reg_of map_literal)
      in
      let map_literal_args =
        Js.Multiple (reg_of (par_of (list_call,[])))
      in Js.ECall (reg_of (map_literal, map_literal_args))

and array_item_of_binding (node: binding reg) =
  let {key; value; _} = node.value in
  let key   = array_item_of_expr key
  and value = array_item_of_expr value in
  let comma = Token.ghost_comma in
  let pair  = Some (key, [comma, value]) in
  let pair  = Js.EArray (reg_of (brackets_of pair))
  in Js.Expr_entry pair

(* Map lookup (e.g. "M.m [i]") *)

and expr_of_E_MapLookup (node: map_lookup reg) =
  let {map; keys} = node.value in
  let keys =
    Utils.nseq_map (fun (x: expr brackets reg) -> x.value.inside) keys in
  match keys with
    _, snd_index::_ ->
      multiple_map_lookups_not_transpiled snd_index
  | index, [] ->
      let map          = expr_of_expr map in
      let index        = expr_of_expr index in
      let module_name  = Wrap.ghost "Transpiled"
      and selector     = Token.ghost_dot
      and field        = Js.EVar (Wrap.ghost "map_find_opt") in
      let map_find_opt = Js.{module_name; selector; field} in
      let map_find_opt = Js.EModA (reg_of map_find_opt) in
      let comma        = Token.ghost_comma in
      let args         = index, [comma, map] in
      let args         = Js.Multiple (reg_of (par_of args)) in
      Js.ECall (reg_of (map_find_opt, args))

(* Modulo (e.g. "x mod n") *)

and expr_of_E_Mod (node: kwd_mod bin_op reg) =
  Js.(EArith (Mod (expr_of_bin_op node)))

(* Module paths (e.g. "M.N.x") *)

and expr_of_E_ModPath (node: expr module_path reg) =
  let {module_path; field; _} = node.value in
  let rev_path = Utils.nsepseq_rev module_path in
  let module_name,
      rev_path = rev_path in
  let selector = Token.ghost_dot in
  let field    = expr_of_expr field in
  let last     = Js.{module_name; selector; field} in
  let last     = Js.EModA (reg_of last) in
  let folded field (_, module_name) =
    Js.(EModA (reg_of {module_name; selector; field}))
  in List.fold_left folded last rev_path

(* Multiplication (e.g. "x * y") *)

and expr_of_E_Mult (node: times bin_op reg) =
  Js.(EArith (Mult (expr_of_bin_op node)))

(* Mutez literals as expressions (e.g. "5mutez") *)

and expr_of_E_Mutez (node: (lexeme * Int64.t) wrap) =
  let lexeme, int64 = node#payload in
  let int           = Z.of_int64 int64 in
  let int           = Wrap.ghost (lexeme, int) in
  let expr          = Js.(EArith (Int int))
  and kwd_as        = Token.ghost_as
  and type_expr     = Js.TVar (Wrap.ghost "mutez") in
  let e_annot       = expr, kwd_as, type_expr in
  let e_annot       = Js.EAnnot (reg_of e_annot)
  in Js.EPar (reg_of (par_of e_annot))

(* Naturals as expressions (e.g. "4n") *)

and expr_of_E_Nat (node: (lexeme * Z.t) wrap) =
  let expr      = Js.(EArith (Int node))
  and kwd_as    = Token.ghost_as
  and type_expr = Js.TVar (Wrap.ghost "nat") in
  let e_annot   = expr, kwd_as, type_expr in
  let e_annot   = Js.EAnnot (reg_of e_annot)
  in Js.EPar (reg_of (par_of e_annot))

(* Arithmetic negation (e.g. "-a") *)

and expr_of_E_Neg (node: minus un_op reg) =
  Js.(EArith (Neg (expr_of_un_op node)))

and expr_of_un_op (node: lexeme wrap un_op reg) =
  let {op; arg} = node.value in
  let arg = expr_of_expr arg
  in reg_of Js.{op; arg}

(* Non-equality (e.g. "x =/= y") *)

and expr_of_E_Neq (node: neq bin_op reg) =
  Js.(ELogic (CompExpr (Neq (expr_of_bin_op node))))

(* Empty list ("nil") *)

and expr_of_E_Nil (node: kwd_nil) =
  ignore node; expr_of_nil ()

and expr_of_nil () =
  let arg  = Js.EArray (reg_of (brackets_of None)) in
  let arg  = Js.Multiple (reg_of (par_of (arg,[]))) in
  let list = Js.EVar (Wrap.ghost "list")
  in Js.ECall (reg_of (list, arg))

(* Logical negation (e.g. "not x") *)

and expr_of_E_Not (node: kwd_not un_op reg) =
  Js.(ELogic (BoolExpr (Not (expr_of_un_op node))))

(* Logical disjunction (e.g. "x or y") *)

and expr_of_E_Or (node: kwd_or bin_op reg) =
  Js.(ELogic (BoolExpr (Or (expr_of_bin_op node))))

(* Parenthesised expressions (e.g. "(x - M.y)") *)

and expr_of_E_Par (node: expr par reg) =
  let expr = node.value.inside in
  let epar = par_of (expr_of_expr expr)
  in Js.EPar (reg_of epar)

(* Selection expression (e.g. "e.x.1") *)

and expr_of_E_Proj (node: projection reg) =
  let {record_or_tuple; field_path; _} = node.value in
  let init_expr = expr_of_expr record_or_tuple in
  Utils.nsepseq_foldl mk_projection init_expr field_path

and mk_projection (expr: Js.expr) (selection: selection) : Js.expr =
  let dot = Token.ghost_dot in
  let selection =
    match selection with
      FieldName name ->
        let value = mk_var name in
        Js.(FieldName (reg_of {dot; value}))
    | Component index ->
        let index = expr_of_expr (E_Nat index) in
        let index = reg_of (brackets_of index)
        in Js.Component index
  in Js.(EProj (reg_of {expr; selection}))

(* Record expressions (e.g., "record [x=7]") *)

and expr_of_E_Record (node: record_expr) =
  match properties_of_record_expr node with
    None -> empty_records_not_transpiled node.region
  | Some properties -> Js.EObject (reg_of (braces_of properties))

(* Set expressions (e.g. "set [x; 1]") *)

and expr_of_E_Set (node: expr compound reg) =
  match node.value.elements with
    None ->
      let module_name = Wrap.ghost "Set"
      and selector    = Token.ghost_dot
      and field       = Js.EVar (Wrap.ghost "empty") in
      let access      = Js.{module_name; selector; field}
      in Js.EModA (reg_of access)
  | Some elems ->
      let array_items = Utils.nsepseq_map array_item_of_expr elems in
      let e_array     = brackets_of (Some array_items) in
      let e_array     = Js.EArray (reg_of e_array) in
      let list_args   = Js.Multiple (reg_of (par_of (e_array,[]))) in
      let list        = Js.EVar (Wrap.ghost "list") in
      let list_call   = Js.ECall (reg_of (list, list_args))
      in
      let module_name = Wrap.ghost "Set"
      and selector    = Token.ghost_dot
      and field       = Js.EVar (Wrap.ghost "literal") in
      let set_literal = Js.{module_name; selector; field} in
      let set_literal = Js.EModA (reg_of set_literal)
      in
      let set_literal_args =
        Js.Multiple (reg_of (par_of (list_call,[])))
      in Js.ECall (reg_of (set_literal, set_literal_args))

(* Set membership expression (e.g. "x contains y") *)

and expr_of_E_SetMem (node: set_membership reg) =
  let {set; element; _} = node.value in
  let set          = expr_of_expr set
  and elem         = expr_of_expr element
  and module_name  = Wrap.ghost "Set"
  and selector     = Token.ghost_dot
  and field        = Js.EVar (Wrap.ghost "mem") in
  let set_mem      = Js.{module_name; selector; field} in
  let set_mem      = Js.EModA (reg_of set_mem) in
  let comma        = Token.ghost_comma in
  let args         = elem, [(comma, set)] in
  let set_mem_args = Js.Multiple (reg_of (par_of args))
  in Js.ECall (reg_of (set_mem, set_mem_args))

(* String literals as expressions (e.g. "foo") *)

and expr_of_E_String (node: lexeme wrap) = Js.(EString (String node))

(* Subtraction (e.g. "a - b") *)

and expr_of_E_Sub (node: minus bin_op reg) =
  Js.(EArith (Sub (expr_of_bin_op node)))

(* Tuple of expressions (e.g., "(1, x)") *)

and expr_of_E_Tuple (node: expr tuple) =
  let components  = node.value.inside in
  let array_items = Utils.nsepseq_map array_item_of_expr components in
  let array_items = Some array_items in
  let array_items = brackets_of array_items
  in Js.EArray (reg_of array_items)

and array_item_of_expr (node: expr) : Js.array_item =
  Js.Expr_entry (expr_of_expr node)

(* Typed expressions (e.g. "(x : int)") *)

and expr_of_E_Typed (node: typed_expr par reg) =
  let expr, (_, type_expr) = node.value.inside in
  let expr                 = expr_of_expr expr
  and type_expr            = type_expr_of_type_expr type_expr in
  let annot_expr           = expr, Token.ghost_as, type_expr
  in Js.EAnnot (reg_of annot_expr)

(* Function updates of records (e.g. "x with y") *)

and expr_of_E_Update (node: update reg) =
  (* r with record [x = e1; y = e2]
     ==>
     {...r, x : e1, y : e2}
     and nested:
     r with record [a.x = e]
     ==>
     {...r, a: {...a.x, x : e}}
  *)
  let {structure; update; _} = node.value in
  let structure =
    match structure with
      E_Var _ -> expr_of_expr structure
    | _       -> expr_not_transpiled structure in
  match update with
    E_Record fields -> (
      match expr_of_record_update structure fields with
        Some update -> update
      | None        -> structure (* No-op *)
    )
  | _ -> expr_not_transpiled update

and expr_of_record_update (structure: Js.expr) (fields: record_expr) =
  match properties_of_record_expr fields with
    None -> None
  | Some properties ->
      let ellipsis    = Token.ghost_ellipsis
      and expr        = structure in
      let rest        = Js.{ellipsis; expr} in
      let rest        = Js.Property_rest (reg_of rest)
      and comma       = Token.ghost_comma in
      let properties  = Utils.nsepseq_cons rest comma properties in
      let object_expr = reg_of (braces_of properties)
      in Some (Js.EObject object_expr)

and check_attributes = function
  []   -> ()
| a::_ -> drop_attribute a

and properties_of_record_expr (fields: record_expr) : _ option =
  match fields.value.elements with
    None -> None
  | Some fields ->
      let property_of_field (field: (expr, expr) field reg) =
        match field.value with
          Punned {pun; attributes} ->
            check_attributes attributes;
            Js.Punned_property (reg_of (expr_of_expr pun))
        | Complete {field_lhs; field_lens; field_rhs; attributes} ->
            match field_lhs with
              E_Var _ ->
                let () = check_attributes attributes in
                let () =
                  match field_lens with
                    Lens_Id _ -> ()
                  | _ -> field_lens_not_transpiled field_lens in
                let name     = expr_of_expr field_lhs
                and colon    = Token.ghost_colon
                and value    = expr_of_expr field_rhs in
                let property = Js.{attributes=[]; name; colon; value}
                in Property (reg_of property)
            | _ -> expr_not_transpiled field_lhs in
      Some (Utils.nsepseq_map property_of_field fields)

(* Variables denoting expressions (e.g. "x") *)

and expr_of_E_Var (node: variable) = Js.EVar (mk_var node)

(* Verbatim string as expressions (e.g. "{|foo|}") *)

and expr_of_E_Verbatim (node: lexeme wrap) = Js.(EString (Verbatim node))

(* STATEMENTS *)

and statements_of_statements (node: statements) =
  let semi = Token.ghost_semi in
  let statements' =
    Utils.nsepseq_map statement_of_statement node in
  let statements' = Utils.nsepseq_to_list statements' in
  match normalise_statements statements' with
    []     -> None
  | hd::tl -> Some (hd, List.map (fun stmt -> (semi, stmt)) tl)

and normalise_statements (node: Js.statement option list) =
  let folded item acc =
    match item with
      None -> acc
    | Some item -> item :: acc
  in List.fold_right folded node []

and statement_of_statement = function
  S_Attr    s -> statement_of_S_Attr s
| S_Decl    s -> Some (statement_of_S_Decl s)
| S_Instr   s -> statement_of_S_Instr s
| S_VarDecl s -> Some (statement_of_S_VarDecl s)

(* Attributed statements *)

and statement_of_S_Attr (node: attribute * statement) =
  let attr, stmt = node in
  match statement_of_statement stmt with
    None      -> drop_attribute attr; None
  | Some stmt -> Some (add_attribute_to_statement attr stmt)

(* Declaration as a statement *)

and statement_of_S_Decl (node: declaration) =
  statement_of_declaration node

(* Mutable variable declaration (only as a statement) *)

and statement_of_S_VarDecl (node: var_decl reg) =
  let {pattern; type_params; var_type; init; _} = node.value in
  let binders     = pattern_of_pattern pattern
  and type_params = type_params_of_type_params type_params
  and lhs_type    = of_type_annotation var_type
  and eq          = Token.ghost_eq
  and expr        = expr_of_expr init in
  let val_binding = Js.{binders; type_params; lhs_type; eq; expr} in
  let val_binding = reg_of val_binding in
  let let_decl    = Js.{attributes = [];
                        kwd_let    = Token.ghost_let;
                        bindings   = val_binding, []}
  in Js.SLet (reg_of let_decl)

(* INSTRUCTIONS *)

and statement_of_S_Instr (node: instruction) =
  statement_of_instruction node

and statement_of_instruction = function
  I_Assign i -> Some (statement_of_I_Assign i)
| I_Call   i -> Some (statement_of_I_Call   i)
| I_Case   i -> Some (statement_of_I_Case   i)
| I_Cond   i -> statement_of_I_Cond i
| I_For    i -> statement_of_I_For i
| I_ForIn  i -> statement_of_I_ForIn i
| I_Patch  i -> statement_of_I_Patch i
| I_Remove i -> Some (statement_of_I_Remove i)
| I_Skip   i -> statement_of_I_Skip i
| I_While  i -> statement_of_I_While i

(* Assignments *)

and statement_of_I_Assign (node: assignment reg) =
  let {lhs; rhs; _} = node.value in
  let rhs = expr_of_expr rhs
  and op  = reg_of Js.Eq in
  match lhs with
    E_MapLookup e -> (
      (* e[k] := v ==> e = Map.add (k, v, e); *)
      (* We filter the cases where "e" is a variable or a path,
         like "x.y[k] := v" *)
      let {map; keys} = e.value in
      match map with
        E_Var _ | E_Proj _ -> (
          match keys with
            key, [] ->
              let module_name = Wrap.ghost "Transpiled"
              and selector    = Token.ghost_dot
              and field       = Js.EVar (Wrap.ghost "map_add") in
              let map_add     = Js.{module_name; selector; field} in
              let map_add     = Js.EModA (reg_of map_add)
              in
              let map         = expr_of_expr map in
              let comma       = Token.ghost_comma in
              let key         = expr_of_expr key.value.inside in
              let map_args    = key, [(comma, rhs); (comma, map)] in
              let map_args    = reg_of (par_of map_args) in
              let map_args    = Js.Multiple map_args
              in
              let map_call    = reg_of (map_add, map_args) in
              let map_call    = Js.ECall map_call
              in Js.(SExpr ([], EAssign (map, op, map_call)))
          | _, key :: _ ->
              multiple_map_lookups_not_transpiled key.value.inside
        )
      | _ -> expr_not_transpiled map
    )
  | E_Var _ | E_Proj _ ->
      let lhs = expr_of_expr lhs
      in Js.(SExpr ([], EAssign (lhs, op, rhs)))
  | _ -> expr_not_transpiled lhs

(* Procedure calls *)

and statement_of_I_Call (node: call) =
  Js.SExpr ([], expr_of_E_App node)

(* Case instruction *)
(*
case action with [
  A (x) -> { ... }
| B ()  -> { ... }
]
==>
const _ =
  match (action, {
    A : (x) => { ...; return }
  | B : ()  => { ...; return }});
*)

and statement_of_I_Case (node: test_clause case reg) =
  let {expr; cases; _} = node.value in
  let mk_body clause =
    let stmts =
      match statement_of_test_clause clause with
        Some Js.SBlock stmts -> stmts
      | Some stmt -> reg_of (braces_of (stmt, []))
      | None ->
          let kwd_return = Token.ghost_return in
          let return     = Js.{kwd_return; expr=None} in
          let return     = Js.SReturn (reg_of return)
          in reg_of (braces_of (return, []))
    in Js.FunctionBody stmts
  in
  let fun_name    = Js.EVar (Wrap.ghost "match")
  and obj_expr    = obj_of_cases mk_body cases
  and comma       = Token.ghost_comma
  and subject     = expr_of_expr expr in
  let arguments   = subject, [comma, obj_expr] in
  let arguments   = Js.Multiple (reg_of (par_of arguments)) in
  let match_call  = Js.ECall (reg_of (fun_name, arguments))
  in
  let kwd_const   = Token.ghost_const in
  let variable    = Wrap.ghost "_" in
  let var_pattern = Js.{variable; attributes=[]} in
  let binders     = Js.PVar (reg_of var_pattern)
  and type_params = None
  and lhs_type    = None
  and eq          = Token.ghost_eq
  and expr        = match_call in
  let binding     = Js.{binders; type_params; lhs_type; eq; expr} in
  let bindings    = reg_of binding, [] in
  let const_decl  = Js.{attributes=[]; kwd_const; bindings}
  in Js.SConst (reg_of const_decl)

(* Conditional instruction *)

and statement_of_I_Cond (node: test_clause conditional reg) =
  let {test; if_so; if_not; _} = node.value in
  let attributes = [] in
  let kwd_if     = Token.ghost_if
  and ifso_opt   = statement_of_test_clause if_so
  and ifnot_opt  = ifnot_from_if_not if_not in
  match ifso_opt, ifnot_opt with
    None, None -> None
  | None, Some (_, ifnot) ->
      let op        = Token.ghost_bool_not in
      let neg       = reg_of Js.{op; arg = expr_of_expr test} in
      let test      = par_of Js.(ELogic (BoolExpr (Not neg))) in
      let cond_stmt = Js.{attributes; kwd_if; test; ifso=ifnot; ifnot=None}
      in Some (Js.SCond (reg_of cond_stmt))
  | Some ifso, ifnot ->
      let test      = par_of (expr_of_expr test) in
      let cond_stmt = Js.{attributes; kwd_if; test; ifso; ifnot}
      in Some (Js.SCond (reg_of cond_stmt))

and statement_of_test_clause (node: test_clause) =
  match node with
    ClauseInstr i -> statement_of_ClauseInstr i
  | ClauseBlock b -> statement_of_ClauseBlock b

and statement_of_ClauseInstr (node: instruction) : Js.statement option =
  statement_of_instruction node

and statement_of_ClauseBlock (node: block reg) =
  let {statements; _} = node.value in
  match statements_of_statements statements with
    None       -> None
  | Some stmts -> Some (Js.SBlock (reg_of (braces_of stmts)))

and ifnot_from_if_not (node: (kwd_else * test_clause) option) =
  match node with
    None -> None
  | Some (_, test_clause) ->
      match statement_of_test_clause test_clause with
        None      -> None
      | Some stmt -> Some (Token.ghost_else, stmt)

(* For-loops *)

and statement_of_I_For (node: for_int reg) =
  let block      = node.value.block in
  let statements = block.value.statements in
  let statements = statements_of_statements statements in
  match statements with
    None -> None
  | Some _ -> for_int_not_transpiled node.region

(* Iteration over a collection *)

and statement_of_I_ForIn = function
  ForMap       loop -> statement_of_ForMap loop
| ForSetOrList loop -> statement_of_ForSetOrList loop

and statement_of_ForMap (node: for_map reg) =
  let {binding; collection; block; _} = node.value in
  let statements = block.value.statements in
  let statements = statements_of_statements statements in
  match statements with
    None -> None
  | Some statements ->
      let key, _, value = binding in
      let variable      = mk_var key in
      let key           = Js.{attributes=[]; variable} in
      let key           = Js.PVar (reg_of key) in
      let variable      = mk_var value in
      let value         = Js.{attributes=[]; variable} in
      let value         = Js.PVar (reg_of value) in
      let comma         = Token.ghost_comma in
      let p_array       = key, [comma, value] in
      let p_array       = reg_of (brackets_of p_array)
      in
      let kwd_let       = Token.ghost_let in
      let binders       = Js.PArray p_array
      and type_params   = None
      and lhs_type      = None
      and eq            = Token.ghost_eq
      and expr          = Js.EVar (Wrap.ghost "generated") in
      let val_binding   = Js.{binders; type_params; lhs_type; eq; expr} in
      let binding       = reg_of val_binding in
      let bindings      = binding, [] in
      let let_decl      = Js.{attributes=[]; kwd_let; bindings} in
      let let_stmt      = Js.SLet (reg_of let_decl)
      in
      let statements    = Utils.nsepseq_cons let_stmt comma statements in
      let statement     = Js.SBlock (reg_of (braces_of statements))
      and kwd_for       = Token.ghost_for
      and lpar          = Token.ghost_lpar
      and index         = Wrap.ghost "generated" (* Hopefully no collision *)
      and index_kind    = `Let Token.ghost_let
      and kwd_of        = Token.ghost_of
      and expr          = expr_of_expr collection
      and rpar          = Token.ghost_rpar in
      let for_of        = Js.{attributes=[]; kwd_for; lpar; index; index_kind;
                              kwd_of; expr; rpar; statement}
      in Some (Js.SForOf (reg_of for_of))

and statement_of_ForSetOrList (node: for_set_or_list reg) =
  let {var; collection; block; _} = node.value in
  let statements = block.value.statements in
  let statements = statements_of_statements statements in
  match statements with
    None -> None
  | Some statements ->
      let statement  = Js.SBlock (reg_of (braces_of statements))
      and kwd_for    = Token.ghost_for
      and lpar       = Token.ghost_lpar
      and index      = mk_var var
      and index_kind = `Let Token.ghost_let
      and kwd_of     = Token.ghost_of
      and expr       = expr_of_expr collection
      and rpar       = Token.ghost_rpar in
      let for_of     = Js.{attributes=[]; kwd_for; lpar; index; index_kind;
                           kwd_of; expr; rpar; statement} in
      Some (Js.SForOf (reg_of for_of))

(* Patches *)

and statement_of_I_Patch (node: patch reg) =
  let {collection; patch_kind; patch; _} = node.value in
  let collection = expr_of_expr collection in
  match patch_kind, patch with
    `Map _, E_Map bindings -> (
      (* patch m with map [x -> e1; y -> e2]
         ==>
         m = Map.add (y, e2, Map.add (x, e1, m))
      *)
      match bindings.value.elements with
        None -> None
      | Some bindings ->
          let module_name = Wrap.ghost "Map"
          and selector    = Token.ghost_dot
          and field       = Js.EVar (Wrap.ghost "add") in
          let map_add     = Js.{module_name; selector; field} in
          let map_add     = Js.EModA (reg_of map_add) in
          let comma       = Token.ghost_comma in
          let map_add_of_binding (map: Js.expr) (node: binding reg) =
            let {key; value; _} = node.value in
            let key      = expr_of_expr key
            and value    = expr_of_expr value in
            let add_args = key, [(comma, value); (comma, map)] in
            let add_args = Js.Multiple (reg_of (par_of add_args))
            in Js.ECall (reg_of (map_add, add_args)) in
          let composition : Js.expr =
            Utils.nsepseq_foldl map_add_of_binding collection bindings in
          let operator = reg_of Js.Eq in
          let e_assign = Js.EAssign (collection, operator, composition)
          in Some (Js.SExpr ([], e_assign))
    )
  | `Set _, E_Set elms -> (
      (* patch s with set [e1; e2]
         ==>
         s = Set.add (e2, Set.add (e1, s))
      *)
      match elms.value.elements with
          None -> None
      | Some elms ->
          let module_name = Wrap.ghost "Set"
          and selector    = Token.ghost_dot
          and field       = Js.EVar (Wrap.ghost "add") in
          let set_add     = Js.{module_name; selector; field} in
          let set_add     = Js.EModA (reg_of set_add) in
          let comma       = Token.ghost_comma in
          let set_add_of_expr (set: Js.expr) (elm: expr) =
            let elm      = expr_of_expr elm in
            let add_args = elm, [comma, set] in
            let add_args = Js.Multiple (reg_of (par_of add_args))
            in Js.ECall (reg_of (set_add, add_args)) in
          let composition : Js.expr =
            Utils.nsepseq_foldl set_add_of_expr collection elms in
          let operator = reg_of Js.Eq in
          let e_assign = Js.EAssign (collection, operator, composition)
          in Some (Js.SExpr ([], e_assign))
    )
  | `Record _, E_Record fields -> (
      (* patch r with record [x = e1; y = e2]
         ==>
         r = {...r, x = e1, y = e2}
      *)
      match expr_of_record_update collection fields with
        Some expr -> Some (Js.SExpr ([], expr))
      | None      -> None
    )
  | _, expr -> expr_not_transpiled expr

(* Removals from collections *)
(* remove x from set s ==> s = Set.remove (x, s); *)

and statement_of_I_Remove (node: removal reg) =
  let {item; remove_kind; collection; _} = node.value in
  let item       = expr_of_expr item
  and collection = expr_of_expr collection
  and op         = reg_of Js.Eq in
  match remove_kind with
    `Set _ ->
      let module_name = Wrap.ghost "Set"
      and selector    = Token.ghost_dot
      and field       = Js.EVar (Wrap.ghost "remove") in
      let set_remove  = Js.{module_name; selector; field} in
      let set_remove  = Js.EModA (reg_of set_remove)
      in
      let comma       = Token.ghost_comma in
      let remove_args = item, [comma, collection] in
      let remove_args = Js.Multiple (reg_of (par_of remove_args)) in
      let remove_call = Js.ECall (reg_of (set_remove, remove_args)) in
      let e_assign    = Js.EAssign (collection, op, remove_call)
      in Js.SExpr ([], e_assign)
  | `Map _ ->
      let module_name = Wrap.ghost "Transpiled"
      and selector    = Token.ghost_dot
      and field       = Js.EVar (Wrap.ghost "map_remove") in
      let map_remove  = Js.{module_name; selector; field} in
      let map_remove  = Js.EModA (reg_of map_remove) in
      let comma       = Token.ghost_comma in
      let remove_args = item, [comma, collection] in
      let remove_args = Js.Multiple (reg_of (par_of remove_args)) in
      let remove_call = Js.ECall (reg_of (map_remove, remove_args)) in
      let e_assign    = Js.EAssign (collection, op, remove_call)
      in Js.SExpr ([], e_assign)

(* Skip instruction (no-operation) *)

and statement_of_I_Skip (node: kwd_skip) =
  drop_skip node; None

(* While-loops *)

and statement_of_I_While (node: while_loop reg) =
  let {cond; block; _} = node.value in
  let statements = block.value.statements in
  let statements = statements_of_statements statements in
  match statements with
    None -> None
  | Some statements ->
      let statement  = Js.SBlock (reg_of (braces_of statements))
      and kwd_while  = Token.ghost_while
      and lpar       = Token.ghost_lpar
      and expr       = expr_of_expr cond
      and rpar       = Token.ghost_rpar in
      let while_stmt = Js.{kwd_while; lpar; expr; rpar; statement}
      in Some (Js.SWhile (reg_of while_stmt))
