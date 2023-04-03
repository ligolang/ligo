[@@@warning "-42"]

module CST = Cst_jsligo.CST
open CST
module Region = Simple_utils.Region
open! Region
open! PPrint
module Option = Simple_utils.Option
module List = Core.List

type leading_bar =
    Always
  | Only_on_new_line
  | Avoid

type environment =
  { indent : int
  ; leading_vbar : leading_bar
  }

let default_environment : environment =
  { indent = 2
  ; leading_vbar = Only_on_new_line
  }

(** [PPrint] has a problem: if [x == empty] then we will produce an extraneous
    space, which is undesirable. This is a fix which shadows [prefix]. *)
let prefix n b x y =
  group (x ^^ nest n ((if x == empty then empty else break b) ^^ y))

(** The same as [( ^/^ )], doesn't output an extra space if the first operand is
    [empty]. *)
let ( ^/^ ) x y =
  if x == empty
  then y
  else x ^/^ y

let pp_enclosed_document state ?(force_hardline : bool option) (thread : document) break_size left right =
  group (
    match force_hardline with
      None | Some false ->
        nest state.indent (left ^^ break break_size ^^ thread) ^^ break break_size ^^ right
    | Some true ->
        nest state.indent (left ^^ hardline ^^ thread) ^^ hardline ^^ right)

let pp_braces_like_document state ?(force_hardline : bool option) (thread : document) =
  pp_enclosed_document state ?force_hardline thread 1 lbrace rbrace

let pp_braces state printer ?(force_hardline : bool option) (node : 'a braces reg) =
  pp_braces_like_document state ?force_hardline (printer node.value.inside)

let pp_brackets_like_document state (thread : document) =
  pp_enclosed_document state ~force_hardline:false thread 0 lbracket rbracket

let pp_brackets state printer (node : 'a brackets reg) =
  pp_brackets_like_document state (printer node.value.inside)

let pp_chevrons_like_document state (thread : document) =
  pp_enclosed_document state ~force_hardline:false thread 0 langle rangle

let pp_chevrons state printer (node : 'a chevrons reg) =
  pp_chevrons_like_document state (printer node.value.inside)

let pp_par_like_document state (thread : document) =
  pp_enclosed_document state ~force_hardline:false thread 0 lparen rparen

let pp_par state printer (node : 'a par reg) =
  pp_par_like_document state (printer node.value.inside)

let pp_nsepseq :
  'a. document -> ('a -> document) -> ('a, lexeme Wrap.t) Utils.nsepseq -> document =
  fun sep printer elements ->
    let elems = Utils.nsepseq_to_list elements
    in separate_map sep printer elems

let is_enclosed_expression = function
  EPar _ | ESeq _ | EArray _ -> true
| _ -> false

let is_enclosed_statement = function
  SBlock _ -> true
| SExpr (_, e) -> is_enclosed_expression e
| _ -> false

let is_enclosed_type = function
  TPar _ | TProd _ | TObject _ -> true
| _ -> false

let rec print state cst =
  Utils.nseq_to_list cst.statements
|> List.map ~f:(pp_toplevel_statement state)
|> separate_map hardline group

(*
  let stmts    = Utils.nseq_to_list cst.statements in
  let stmts    = List.map ~f:pp_toplevel_statement stmts in
  let app stmt = group (stmt ^^ string ";")
  in separate_map (hardline ^^ hardline) app stmts *)

and pp_toplevel_statement state = function
  TopLevel (stmt, _) ->
    pp_statement state ?top:(Some true) stmt ^^ semi ^^ hardline
| Directive dir ->
    string (Directive.to_lexeme dir).Region.value

and pp_statement state ?top = function
  SBlock      s -> pp_SBlock state s
| SExpr       s -> pp_SExpr state s
| SCond       s -> group (pp_cond_expr state s)
| SReturn     s -> pp_return state s
| SLet        s -> pp_let state ?top s
| SConst      s -> pp_const state s
| SType       s -> pp_type state s
| SSwitch     s -> pp_switch state s
| SBreak      _ -> string "break"
| SNamespace  s -> pp_namespace state ?top s
| SExport     s -> pp_export state s
| SImport     s -> pp_import state s
| SForOf      s -> pp_for_of state s
| SWhile      s -> pp_while state s

and pp_SBlock state stmt =
  let print = pp_nsepseq (semi ^^ break 1) (pp_statement state)
  in pp_braces state ~force_hardline:true print stmt

and pp_SExpr state (node: attribute list * expr) =
  let attr, expr = node in
  let expr_doc   = pp_expr state expr in
  if List.is_empty attr then expr_doc
  else pp_attributes state attr ^/^ expr_doc

and pp_for_of state {value; _} =
  string "for" ^^ space
  ^^ pp_par_like_document state (
       pp_index_kind value.index_kind ^^ space
    ^^ string value.index.value
    ^^ space ^^ string "of" ^^ space
    ^^ pp_expr state value.expr)
  ^^ space ^^ pp_statement state value.statement

and pp_index_kind = function
  `Let _ -> string "let"
| `Const _ -> string "const"

and pp_while state {value; _} =
  string "while" ^^ space
  ^^ pp_par_like_document state (pp_expr state value.expr)
  ^^ space ^^ pp_statement state value.statement

and pp_import state (node : CST.import Region.reg) =
  let {value; _} : CST.import Region.reg = node in
  match value with
    Import_rename value ->
    string "import" ^^ space ^^ string value.alias.value
    ^^ space ^^ equals ^^ space
    ^^ pp_nsepseq dot (fun a -> string a.Region.value) value.module_path
  | Import_all_as value ->
    string "import" ^^ space ^^ star ^^ space ^^ string "as" ^^ space ^^ string value.alias.value
    ^^ space ^^ string "from" ^^ space
    ^^ pp_string value.module_path
  | Import_selected value ->
    let pp_idents = pp_nsepseq (comma ^^ break 1) pp_ident in
    string "import" ^^ space ^^
    pp_braces state pp_idents value.imported ^^
    space ^^ string "from" ^^ space
    ^^ pp_string value.module_path

and pp_export state {value = (_, statement); _} =
  string "export" ^^ space ^^ pp_statement state statement

and pp_namespace state ?top {value = (_, name, statements, attributes); _} =
  let top = match top with
    Some true -> true
  | _ -> false
  in
  let is_private =
    List.exists ~f:(fun a -> String.equal (fst a#payload) "private") attributes in
  let attributes = filter_private attributes in
  let pp_statements = pp_nsepseq (semi ^^ break 1) (pp_statement state) in
  (if List.is_empty attributes then empty else pp_attributes state attributes)
  ^/^ string "namespace" ^^ space ^^ string name.value
  ^^ (if ((top && is_private) || not top) then empty else string "export" ^^ space)
  ^^ space ^^ pp_braces state ~force_hardline:true pp_statements statements

and pp_cond_expr state {value; _} =
  let {attributes; test; ifso; ifnot; _} = value in
  let if_then = string "if" ^^ space ^^ pp_par_expr state test ^^ space ^^ pp_statement state ifso in
  let cond_doc =
    match ifnot with
      None -> if_then
    | Some (_,statement) ->
      if_then ^^ space ^^ string "else" ^^ space ^^ pp_statement state statement in
  if List.is_empty attributes then cond_doc
  else pp_attributes state attributes ^/^ cond_doc

and pp_return state {value = {expr; _}; _} =
  match expr with
    Some s -> string "return" ^^ space ^^ pp_expr state s
  | None -> string "return"

and filter_private (attributes: CST.attribute list) : CST.attribute list =
  List.filter ~f:(fun (v: CST.attribute) -> not (String.equal (fst v#payload) "private")) attributes

and pp_let_or_const state ?top (node : (let_decl reg, const_decl reg) Either.t) : document =
  let attributes, kwd, bindings =
    Either.fold node
      ~left:(fun ({value; _} : let_decl reg) -> value.attributes, "let", value.bindings)
      ~right:(fun ({value; _} : const_decl reg) -> value.attributes, "const", value.bindings)
  in
  let top = match top with
    Some true -> true
  | _ -> false
  in
  let is_private = List.exists ~f:(fun a -> String.equal (fst a#payload) "private") attributes in
  let attributes = filter_private attributes in
  pp_attributes state attributes
  ^/^
  (if ((top && is_private) || not top) then empty else string "export" ^^ space)
  ^^ string kwd ^^ space ^^ pp_nsepseq (comma ^^ break 1) (pp_val_binding state) bindings

and pp_let state ?top (node : let_decl reg) = pp_let_or_const state ?top (Left node)

and pp_const state (node : const_decl reg) = pp_let_or_const state (Right node)

and pp_val_binding state {value = {binders; lhs_type; expr; _}; _} =
  (* In case the RHS is a lambda function, we want to try to display it in the
     same line instead of causing a line break. For example, we want to see this:

     let f = (x: int) => x

     And not this:

     let f =
       (x: int) => x
  *)
  let join_lhs_with_rhs =
    function
      EFun _ -> ( ^^ )
    | expr when is_enclosed_expression expr -> ( ^^ )
    | _      -> prefix state.indent 0
  in
  join_lhs_with_rhs expr
    ((match lhs_type with
        Some (_, type_expr) ->
          pp_pattern state binders ^^ pp_type_annot_rhs state type_expr
      | None -> pp_pattern state binders)
     ^^ space ^^ equals ^^ space)
    (pp_expr state expr)

and pp_switch state {value = {expr; cases; _}; _} =
  string "switch" ^^ space ^^ pp_par_like_document state (pp_expr state expr)
  ^^ space ^^ pp_braces_like_document state ~force_hardline:true (pp_cases state cases)

and pp_cases state (hd, tl) =
  List.fold ~f:(fun a i -> a ^^ break 0 ^^ pp_case state i) ~init:(pp_case state hd) tl

and pp_case state node =
  let pp_statements statements =
    group (
      match statements with
        Some s ->
          let app s = group (pp_statement state s) in
          separate_map (semi ^^ hardline) app (Utils.nsepseq_to_list s)
          ^^ semi
      | None -> hardline)
  in
  let pp_label_and_statements label =
    function
      Some (s, []) when is_enclosed_statement s ->
        label ^^ space ^^ group (pp_statement state s) ^^ semi
    | statements ->
        hang state.indent (label ^/^ pp_statements statements)
  in
  match node with
    Switch_case {expr; statements; _} ->
      let label = string "case" ^^ space ^^ pp_expr state expr ^^ colon in
      pp_label_and_statements label statements
  | Switch_default_case {statements; _} ->
      let label = string "default:" in
      pp_label_and_statements label statements

and pp_type state {value; _} =
  let ({attributes; name; params; type_expr; _}: type_decl) = value in
  let attributes  = filter_private attributes in
  let lhs =
    string "type" ^^ space ^^ string name.value
    ^^ pp_type_params state params
  in
  let rhs = group (pp_type_expr state type_expr) in
  let type_doc =
    if is_enclosed_type type_expr
    then lhs ^^ space ^^ equals ^^ space ^^ rhs
    else lhs ^^ prefix state.indent 1 (space ^^ equals) rhs
  in
  if List.is_empty attributes
  then type_doc
  else pp_attributes state attributes ^/^ type_doc

and pp_type_params state = function
  None -> empty
| Some value -> pp_chevrons state (pp_nsepseq (comma ^^ break 1) pp_ident) value

and pp_ident (node : string Region.reg) = string node.value

and pp_string s = dquotes (pp_ident s)

and pp_verbatim s = bquotes (pp_ident s)

and pp_bytes (byte: (string * Hex.t) reg) =
  let _, hex = byte.Region.value
  in string ("0x" ^ Hex.show hex)

and pp_expr state = function
  EFun     e -> pp_fun state e
| EPar     e -> pp_par_expr state e.value
| ESeq     e -> pp_seq state e
| EVar     v -> pp_ident v
| EModA    e -> pp_module_access (pp_expr state) e
| ELogic   e -> pp_logic_expr state e
| EArith   e -> group (pp_arith_expr state e)
| ECall    e -> pp_call_expr state e
| EBytes   e -> pp_bytes e
| EArray   e -> pp_array state e
| EObject  e -> group (pp_object_expr state e)
| EString  e -> pp_string_expr e
| EProj    e -> pp_projection state e
| EAssign  e -> pp_assign state e
| EAnnot   e -> group (pp_annot_expr state e)
| EConstr  e -> pp_constr_expr state e
| EUnit    _ -> string "unit"
| ECodeInj e -> pp_code_inj state e
| ETernary e -> pp_ternary state e
| EContract e -> pp_contract state e

and pp_code_inj state (node: code_inj reg) =
  let {language; code} = node.value in
  let language = string language#payload
  and code     = pp_expr state code in
  group (language ^/^ code)

and pp_array state (node: (array_item, comma) Utils.sepseq brackets reg) =
  match node.value.inside with
    Some node ->
      let pp_items = pp_nsepseq (comma ^^ break 1) (pp_array_item state) in
      pp_brackets_like_document state (pp_items node)
  | None ->
      pp_brackets state (fun _ -> empty) node

and pp_call_expr state {value; _} =
  let lambda, arguments = value in
  let arguments =
    match arguments with
    | Unit _ -> []
    | Multiple xs -> Utils.nsepseq_to_list xs.value.inside in
  let arguments =
    pp_par_like_document state (separate_map (comma ^^ break 1) (pp_expr state) arguments)
  in pp_expr state lambda ^^ arguments

and pp_array_item state = function
  Expr_entry e -> pp_expr state e
| Rest_entry {value = {expr; _}; _} -> string "..." ^^ pp_expr state expr

and pp_constr_expr state {value; _} =
  let constr, arg = value in
  let constr = string constr.value in
  constr ^^ group (
    Option.value_map
      ~default:(string "()")
      ~f:Simple_utils.Function.(pp_par_like_document state <@ pp_expr state)
      arg)

and pp_object_property state = function
  Punned_property {value; _} ->
    pp_expr state value
| Property {value = {name; value; _}; _} ->
    pp_expr state name ^^ colon ^^ space ^^ pp_expr state value
| Property_rest {value = {expr; _}; _} ->
    string "..." ^^ pp_expr state expr

and pp_object_expr state (node: (property, comma) Utils.nsepseq braces reg) =
  let pp_properties = pp_nsepseq (comma ^^ break 1) (pp_object_property state)
  in pp_braces state pp_properties node

and pp_string_expr = function
  String e -> pp_string e
| Verbatim e -> pp_verbatim e

and pp_selection state = function
  FieldName {value = {value;_ }; _} -> dot ^^ pp_ident value
| Component value -> pp_brackets state (pp_expr state) value

and pp_projection state {value = {expr; selection}; _} =
  pp_expr state expr ^^ pp_selection state selection

and pp_infix state lhs middle rhs =
  group (lhs ^^ space ^^ prefix state.indent 1 middle rhs)

and pp_assign state (a, op, b) =
  let operator = match op.value with
      Eq                           -> "="
    | Assignment_operator Times_eq -> "*="
    | Assignment_operator Div_eq   -> "/="
    | Assignment_operator Min_eq   -> "-="
    | Assignment_operator Plus_eq  -> "+="
    | Assignment_operator Mod_eq   -> "%="
  in
  pp_infix state (pp_expr state a) (string operator) (pp_expr state b)

and pp_annot_expr state {value; _} =
  let expr, _, type_expr = value in
  let lhs = pp_expr state expr in
  let rhs = pp_type_expr state type_expr in
  pp_infix state lhs (string "as") rhs

and pp_ternary state {value; _} =
  pp_expr state value.condition ^^
  space ^^ qmark ^^ space ^^
  nest state.indent (pp_expr state value.truthy) ^^
  space ^^ colon ^^ space ^^
  nest state.indent (pp_expr state value.falsy)

and pp_logic_expr state = function
  BoolExpr e -> pp_bool_expr state e
| CompExpr e -> pp_comp_expr state e

and pp_bool_expr state = function
  Or   e  -> pp_bin_op state "||" e
| And  e  -> pp_bin_op state "&&" e
| Not  e  -> pp_un_op state '!' e

and pp_bin_op state op {value; _} =
  let {arg1; arg2; _} = value in
  let lhs = pp_expr state arg1 in
  let rhs = pp_expr state arg2 in
  pp_infix state lhs (string op) rhs

and pp_un_op state op {value; _} =
  char op ^^ pp_expr state value.arg

and pp_comp_expr state = function
  Lt    e -> pp_bin_op state "<"  e
| Leq   e -> pp_bin_op state "<=" e
| Gt    e -> pp_bin_op state ">"  e
| Geq   e -> pp_bin_op state ">=" e
| Equal e -> pp_bin_op state "==" e
| Neq   e -> pp_bin_op state "!=" e

and pp_arith_expr state = function
  Add   e -> pp_bin_op state "+" e
| Sub   e -> pp_bin_op state "-" e
| Mult  e -> pp_bin_op state "*" e
| Div   e -> pp_bin_op state "/" e
| Mod   e -> pp_bin_op state "%" e
| Neg   e -> pp_un_op state '-' e
| Int   e -> pp_int e

and pp_int {value; _} =
  string (Z.to_string (snd value))

and pp_par_expr state value = pp_par_like_document state (pp_expr state value.inside)

and pp_type_annot_rhs state value =
  group (nest state.indent (break 0 ^^ colon ^^ space ^^ pp_type_expr state value))

(* In flat mode, we may render the arguments like so:

   let f = (x: int, y: int): int => /* */

   Otherwise we'll try to render it like so:

   let f = (
     x: int,
     y: int
   ): int => /* */
*)
and pp_expr_fun state = function
  EPar {value; _} ->
    pp_par_like_document state (pp_expr_fun state value.inside)
| ESeq {value; _} ->
    pp_nsepseq (comma ^^ break 1) (pp_expr_fun state) value
| EAnnot {value; _} ->
    let expr, _, type_expr = value in
    pp_expr_fun state expr ^^ pp_type_annot_rhs state type_expr
| EUnit _ -> string "()"
| c -> pp_expr state c

and pp_fun state {value; _} =
  let {type_params; parameters; lhs_type; arrow = _; body} = value in
  let type_params = pp_type_params state type_params in
  let parameters = pp_expr_fun state parameters in
  let annot =
    match lhs_type with
      None        -> empty
    | Some (_, e) -> pp_type_annot_rhs state e
  in
  match body with
  | FunctionBody fb ->
      let pp_statements = pp_nsepseq (semi ^^ break 1) (pp_statement state) in
      (* If the function has only one statement we may try to display it inline
         rather than in a new one.
      *)
      let force_hardline = not @@ List.is_empty @@ snd fb.value.inside in
      type_params ^^ parameters ^^ annot ^^ space ^^ string "=>" ^^ space
      ^^ pp_braces state ~force_hardline pp_statements fb
  | ExpressionBody e ->
      prefix state.indent 1
        (type_params ^^ parameters ^^ annot ^^ space ^^ string "=>")
        (pp_expr state e)

and pp_seq state {value; _} =
  pp_nsepseq (comma ^^ break 1) (pp_expr state) value

and pp_disc state value = pp_disc_or_sum state (Either.left value)

and pp_parameter {value; _} =
    string "parameter_of"
    ^^ group (nest 0 (break 1 ^^ pp_nsepseq (dot ^^ break 1) pp_ident value))

and pp_type_expr state: type_expr -> document = function
  TProd      t -> pp_cartesian state t
| TSum       t -> pp_sum_type state t
| TObject    t -> pp_object_type state t
| TApp       t -> pp_type_app state t
| TFun       t -> pp_fun_type state t
| TPar       t -> pp_type_par state t
| TVar       t -> pp_ident t
| TString    s -> pp_string s
| TModA      t -> pp_module_access (pp_type_expr state) t
| TInt       t -> pp_int t
| TDisc      t -> pp_disc state t
| TParameter t -> pp_parameter t

and pp_module_access : type a. (a -> document) -> a module_access reg -> document
= fun f {value; _} ->
  let {module_name; field; _} = value in
  group (pp_ident module_name ^^ dot ^^ break 0 ^^ f field)

and pp_cartesian state (node: CST.cartesian) =
  let pp_type_exprs = pp_nsepseq (comma ^^ break 1) (pp_type_expr state) in
  prefix state.indent 1
    (pp_attributes state node.attributes)
    (pp_brackets state pp_type_exprs node.inside)

and pp_sum_type state (node : sum_type reg) = pp_disc_or_sum state (Either.right node)

and pp_disc_or_sum state (value : ((obj_type, vbar) Utils.nsepseq, sum_type reg) Either.t) =
  (* We provide an extra layer of indentation so that instead of this:

     type t =
       | [
         "Alt",
         unit
         ]

     We will get:

     type t =
       | [
           "Alt",
           unit
         ]
   *)
  let variants, attributes =
    let open Simple_utils.Function in
    Either.fold value
      ~left:(fun (disc : (obj_type, vbar) Utils.nsepseq) ->
        let variants = Utils.nsepseq_map (nest state.indent <@ pp_object_type state) disc in
        let attributes = [] in
        variants, attributes)
      (* JsLIGO's lexer always injects leading vertical bars in sum types, so we
         don't check for them here. *)
      ~right:(fun {value = {leading_vbar = _; variants; attributes}; _} ->
        let variants = Utils.nsepseq_map (nest state.indent <@ pp_variant state) variants.value in
        variants, attributes)
  in
  let head, tail = variants in
  let padding_flat =
    match state.leading_vbar with
      Avoid | Only_on_new_line -> empty
    | Always -> bar ^^ space
  in
  let padding_non_flat =
    match state.leading_vbar with
      Avoid -> blank state.indent
    | Always | Only_on_new_line -> bar ^^ space
  in
  (* Do not append a vertical bar if we are in flat mode, unless we have
     attributes. The reason is that those two are different:

     type t = [@annot] | Ctor
     type t = [@annot] Ctor
  *)
  let head =
    if List.is_empty tail || not (List.is_empty attributes) then bar ^^ space ^^ head
    else ifflat padding_flat padding_non_flat ^^ head
  in
  let tail = List.map ~f:snd tail in
  let app variant = break 1 ^^ bar ^^ space ^^ variant in
  let thread = group (head ^^ concat_map app tail) in
  if attributes = [] then thread
  else group (pp_attributes state attributes ^/^ thread)

and pp_variant state (node : variant reg) =
  let {tuple; attributes; _} = node.value in
  let tuple = pp_brackets state (pp_variant_comp state) tuple in
  if List.is_empty attributes then tuple
  else group (pp_attributes state attributes ^/^ tuple)

and pp_variant_comp state (node: variant_comp) =
  let {constr; params} = node in
  let constr, params =
    match params with
      None -> pp_string constr, []
    | Some (_comma, params) ->
       pp_string constr , Utils.nsepseq_to_list params
  in if params = [] then constr
     else let sep = comma ^^ break 1
          in group (constr ^^ sep ^^ separate_map sep (pp_type_expr state) params)

and pp_attribute state (node : Attr.t wrap) =
  let key, val_opt = node#payload in
  let thread = string "/* @" ^^ string key in
  let thread = match val_opt with
                 Some Ident value ->
                   group (thread ^/^ nest state.indent (string value))
               | Some String value ->
                   group (thread ^/^ nest state.indent (string ("\"" ^ value ^ "\"")))
               | None -> thread in
  let thread = thread ^^ space ^^ string "*/"
  in thread

and pp_attributes state = function
  [] -> empty
| attrs ->
  separate_map (break 0) (pp_attribute state) attrs

and pp_object_type state fields = group (pp_ne_injection state (pp_field_decl state) fields)

and pp_field_decl state {value; _} =
  let {field_name; field_type; attributes; _} = value in
  let attr = pp_attributes state attributes in
  let name = if List.is_empty attributes then pp_ident field_name
             else attr ^/^ pp_ident field_name in
  match field_type with
    TVar v when String.equal v.value field_name.value -> name
  | _ ->
      let t_expr = pp_type_expr state field_type in
      group (name ^^ colon ^^ space ^^ group t_expr)

and pp_ne_injection :
  'a. environment -> ('a -> document) -> 'a ne_injection reg -> document =
  fun state printer {value; _} ->
    let {compound; ne_elements; attributes; _} = value in
    let elements = pp_nsepseq (comma ^^ break 1) printer ne_elements in
    let inj =
      match compound with
        None -> elements
      | Some Braces _ -> pp_braces_like_document state elements
      | Some Brackets _ -> pp_brackets_like_document state elements
    in
    let inj = if List.is_empty attributes then inj
              else break 0 ^^ pp_attributes state attributes ^/^ inj
    in inj

and pp_type_app state {value; _} =
  let ctor, tuple = value in
  pp_ident ctor
  ^^ pp_chevrons_like_document state (pp_type_tuple state tuple)

and pp_type_tuple state {value; _} =
  let head, tail = value.inside in
  let rec app = function
    []  -> empty
  | [e] -> group (break 1 ^^ pp_type_expr state e)
  | e::items ->
      group (break 1 ^^ pp_type_expr state e ^^ comma) ^^ app items in
  if List.is_empty tail
  then pp_type_expr state head
  else
    let components =
      pp_type_expr state head ^^ comma ^^ app (List.map ~f:snd tail)
    in components

and pp_fun_type_arg state ({name; type_expr; _} : CST.fun_type_arg) =
  group (pp_ident name ^^ colon ^^ space ^^ pp_type_expr state type_expr)

and pp_fun_type state {value; _} =
  let lhs, _, rhs = value in
  let lhs =
    pp_par_like_document state
      (pp_nsepseq (comma ^^ break 1) (pp_fun_type_arg state) lhs.inside)
  in
  let rhs = pp_type_expr state rhs in
  group (lhs ^^ space ^^ string "=>" ^^ space ^^ rhs)

and pp_type_par state value = pp_par state (pp_type_expr state) value

and pp_pattern state = function
  PRest     p -> pp_rest_pattern p
| PAssign   p -> pp_assign_pattern state p
| PVar      v -> pp_pvar state v
| PConstr   p -> pp_ident p
| PDestruct p -> pp_destruct state p
| PObject   p -> pp_pobject state p
| PArray    p -> pp_parray state p

and pp_parray state (node:  (pattern, comma) Utils.nsepseq brackets reg) =
  let pp_patterns = pp_nsepseq (comma ^^ break 1) (pp_pattern state)
  in group (pp_brackets state pp_patterns node)

and pp_pobject state (node: (pattern, comma) Utils.nsepseq braces reg) =
  pp_braces state (pp_nsepseq (comma ^^ break 1) (pp_pattern state)) node

and pp_pvar state {value; _} =
  let {variable; attributes} = value in
  let v = pp_ident variable in
  if List.is_empty attributes then v
  else group (pp_attributes state attributes ^/^ v)

and pp_rest_pattern {value = {rest; _}; _} =
  string "..." ^^ pp_ident rest

and pp_assign_pattern state {value = {property; value; _}; _} =
  pp_ident property ^^ equals ^^ pp_expr state value

and pp_destruct state {value = {property; target; _}; _} =
  pp_ident property ^^ colon ^^ pp_val_binding state target

and pp_contract state {value; _} =
  pp_par_like_document state (
    string "contract_of"
    ^^ group (nest 0 (break 1 ^^ pp_nsepseq (dot ^^ break 1) pp_ident value)))

let print_type_expr = pp_type_expr
let print_pattern   = pp_pattern
let print_expr      = pp_expr

type cst       = CST.t
type expr      = CST.expr
type type_expr = CST.type_expr
type pattern   = CST.pattern
