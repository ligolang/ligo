(* A pretty printer for JsLIGO *)

[@@@warning "-42"]

(* Jane Street dependency *)

module List = Core.List

(* Vendored dependencies *)

module Utils  = Simple_utils.Utils
module Region = Simple_utils.Region
module Option = Simple_utils.Option

(* Local dependencies *)

module CST = Cst_jsligo.CST
module PrettyComb = Parsing_shared.PrettyComb

(* Global openings *)

open CST
open! Region
open! PPrint

(* Utilities and local shadowings *)

let prefix = PrettyComb.prefix
let (^/^)  = PrettyComb.(^/^)
type state = PrettyComb.state

(* Placement *)

let default_state : PrettyComb.state =
  object
    method indent       = 2
    method leading_vbar = PrettyComb.Only_on_new_line
  end

(* Comments *)

let pp_comments : Wrap.comment list -> document =
  separate_map
    hardline
    (fun (comment : Wrap.comment) ->
       (*group (
         ifflat
         (string "//" ^^ string comment.value)
         (string "(*" ^^ string comment.value ^^ string "*)")))*)
       group (string "/*" ^^ string comment.value ^^ string "*/"))

(* Tokens *)

let token (t : string Wrap.t) : document =
  pp_comments t#comments ^/^ string t#payload

(* Enclosed documents *)

let pp_enclosed_document
    state ?(force_hardline : bool option) (thread : document)
    break_size left right =
  let left  = token left
  and right = token right in
  group (
    match force_hardline with
      None | Some false ->
        nest state#indent (left ^^ break break_size ^^ thread)
        ^^ break break_size ^^ right
    | Some true ->
        nest state#indent (left ^^ hardline ^^ thread)
        ^^ hardline ^^ right)

let pp_braces_like_document
  state inside ?(force_hardline : bool option) left right =
  pp_enclosed_document state ?force_hardline inside 1 left right

let pp_braces state printer
  ?(force_hardline : bool option) (node : 'a braces reg) =
  let {lbrace; inside; rbrace} = node.value in
  pp_braces_like_document
    state ?force_hardline (printer inside) lbrace rbrace

let pp_brackets_like_document
  state inside ?(force_hardline : bool option) left right =
  pp_enclosed_document state ?force_hardline inside 0 left right

let pp_brackets state printer (node : 'a brackets reg) =
  let {lbracket; inside; rbracket} = node.value in
  pp_brackets_like_document
    state ~force_hardline:false (printer inside) lbracket rbracket

let pp_chevrons_like_document
  state inside ?(force_hardline : bool option) left right =
  pp_enclosed_document state ?force_hardline inside 0 left right

let pp_chevrons state printer (node : 'a chevrons reg) =
  let {lchevron; inside; rchevron} = node.value in
  pp_chevrons_like_document
    state ~force_hardline:false (printer inside) lchevron rchevron

let pp_par_like_document
  state inside ?(force_hardline : bool option) left right =
  pp_enclosed_document state ?force_hardline inside 0 left right

let pp_par state printer (node : 'a par reg) =
  let {lpar; inside; rpar} = node.value in
  pp_par_like_document state ~force_hardline:false (printer inside) lpar rpar

(* The separator [sep] here represents some extra spacing (like spaces
   or newlines) that will be printed after every separator in a
   sequence of type [Utils.nsepseq]. *)

let pp_nsepseq :
  'a.document ->
  ('a -> document) -> ('a, lexeme Wrap.t) Utils.nsepseq -> document =
  fun sep printer elements ->
    let hd, tl = elements in
    let rec separate_map = function
      []            -> empty
    | (sep', x)::xs -> token sep' ^^ sep ^^ printer x ^^ separate_map xs
    in printer hd ^^ separate_map tl

(* Enclosed structures *)

let is_enclosed_expr = function
  EPar _ | ESeq _ | EArray _ -> true
| _ -> false

let is_enclosed_statement = function
  SBlock _ -> true
| SExpr (_, e) -> is_enclosed_expr e
| _ -> false

let is_enclosed_type = function
  TPar _ | TProd _ | TObject _ -> true
| _ -> false

(* PRINTING THE CST *)

let rec print state (node : CST.t) =
  Utils.nseq_to_list node.statements
|> List.map ~f:(pp_toplevel_statement state)
|> separate_map hardline group

and pp_toplevel_statement state = function
  TopLevel (stmt, _) ->
    pp_statement state ?top:(Some true) stmt ^^ semi ^^ hardline
| Directive dir ->
    string (Directive.to_lexeme dir).Region.value

and pp_statement state ?top = function
  SBlock     s -> pp_SBlock state s
| SExpr      s -> pp_SExpr state s
| SCond      s -> group (pp_cond_expr state s)
| SReturn    s -> pp_return state s
| SLet       s -> pp_let state ?top s
| SConst     s -> pp_const state s
| SType      s -> pp_type state s
| SSwitch    s -> pp_switch state s
| SBreak     s -> token s
| SNamespace s -> pp_namespace state ?top s
| SExport    s -> pp_export state s
| SImport    s -> pp_import state s
| SForOf     s -> pp_for_of state s
| SWhile     s -> pp_while state s

and pp_SBlock state stmt =
  let print = pp_nsepseq (break 1) (pp_statement state)
  in pp_braces state ~force_hardline:true print stmt

and pp_SExpr state (node: attribute list * expr) =
  let attr, expr = node in
  let expr_doc   = pp_expr state expr in
  if List.is_empty attr then expr_doc
  else pp_attributes state attr ^/^ expr_doc

and pp_for_of state (node: for_of reg) =
  let {kwd_for; lpar; index_kind; index; kwd_of;
       expr; rpar; statement; _} = node.value in
  let par =
    pp_index_kind index_kind ^^ space
    ^^ token index ^^ space ^^ token kwd_of ^^ space
    ^^ pp_expr state expr in
  let par = pp_par_like_document state par lpar rpar
  in token kwd_for ^^ space ^^ par ^^ space ^^ pp_statement state statement

and pp_index_kind = function
  `Let   i -> token i
| `Const i -> token i

and pp_while state (node: while_stmt reg) =
  let {kwd_while; lpar; expr; rpar; statement} = node.value in
  token kwd_while ^^ space
  ^^ pp_par_like_document state (pp_expr state expr) lpar rpar
  ^^ space ^^ pp_statement state statement

and pp_import state (node : CST.import Region.reg) =
  match node.value with
    Import_rename {kwd_import; alias; equal; module_path} ->
      token kwd_import ^^ space ^^ token alias
      ^^ space ^^ token equal ^^ space
      ^^ pp_nsepseq empty (fun a -> string a#payload) module_path
  | Import_all_as {kwd_import; times; kwd_as; alias; kwd_from; module_path} ->
      token kwd_import ^^ space ^^ token times ^^ space
      ^^ token kwd_as ^^ space ^^ token alias ^^ space
      ^^ token kwd_from ^^ space ^^ pp_string module_path
  | Import_selected {kwd_import; imported; kwd_from; module_path} ->
      let pp_idents = pp_nsepseq (break 1) pp_ident in
      token kwd_import ^^ space ^^
      pp_braces state pp_idents imported ^^ space
      ^^ token kwd_from ^^ space ^^ pp_string module_path

and pp_export state {value = (kwd_export, statement); _} =
  token kwd_export ^^ space ^^ pp_statement state statement

and pp_namespace state ?top (node: namespace_statement reg) =
  let kwd_namespace, name, statements, attributes = node.value in
  let top = match top with Some true -> true | _ -> false in
  let is_private =
    List.exists ~f:(fun a -> String.equal (fst a#payload) "private")
                attributes in
  let attributes = filter_private attributes in
  let pp_statements = pp_nsepseq (break 1) (pp_statement state) in
  group (
    (if List.is_empty attributes then empty
     else pp_attributes state attributes)
    ^/^ token kwd_namespace ^^ space ^^ token name
    ^^ (if ((top && is_private) || not top) then empty
        else string "export" ^^ space)
    ^^ space
    ^^ pp_braces state ~force_hardline:true pp_statements statements)

and pp_cond_expr state {value; _} =
  let {attributes; test; ifso; ifnot; _} = value in
  let if_then =
    token value.kwd_if ^^ space ^^ pp_par_expr state test ^^ space
    ^^ pp_statement state ifso in
  let cond_doc =
    match ifnot with
      None -> if_then
    | Some (kwd_else, statement) ->
        if_then ^^ space ^^ token kwd_else ^^ space
        ^^ pp_statement state statement in
  if List.is_empty attributes then cond_doc
  else pp_attributes state attributes ^/^ cond_doc

and pp_return state {value = {kwd_return; expr}; _} =
  match expr with
    Some s -> token kwd_return ^^ space ^^ pp_expr state s
  | None -> token kwd_return

and filter_private (attributes: CST.attribute list) : CST.attribute list =
  List.filter ~f:(fun (v: CST.attribute) -> not (String.equal (fst v#payload) "private")) attributes

and pp_let_or_const state ?top (node : (let_decl reg, const_decl reg) Either.t) : document =
  let attributes, kwd, bindings =
    Either.fold node
      ~left:(fun ({value; _} : let_decl reg) -> value.attributes,
                                                value.kwd_let,
                                                value.bindings)
      ~right:(fun ({value; _} : const_decl reg) -> value.attributes,
                                                   value.kwd_const,
                                                   value.bindings) in
  let top = match top with Some true -> true | _ -> false in
  let is_private =
    List.exists ~f:(fun a -> String.equal (fst a#payload) "private")
                attributes in
  let attributes = filter_private attributes in
  pp_attributes state attributes ^/^
  (if (top && is_private || not top) then empty else string "export" ^^ space)
  ^^ token kwd ^^ space
  ^^ pp_nsepseq (break 1) (pp_val_binding state) bindings

and pp_let state ?top (node : let_decl reg) =
  pp_let_or_const state ?top (Left node)

and pp_const state (node : const_decl reg) = pp_let_or_const state (Right node)

and pp_val_binding state {value = {binders; type_params = _; lhs_type; eq; expr}; _} =
  (* In case the RHS is a lambda function, we want to try to display
     it in the same line instead of causing a line break. For example,
     we want to see this:

     let f = (x: int) => x

     And not this:

     let f =
       (x: int) => x
  *)
  let join_lhs_with_rhs =
    function
      EFun _ -> ( ^^ )
    | expr when is_enclosed_expr expr -> ( ^^ )
    | _      -> prefix state#indent 0
  in
  join_lhs_with_rhs expr
    ((match lhs_type with
        Some (colon, type_expr) ->
          pp_pattern state binders ^^ pp_type_annot_rhs state colon type_expr
      | None -> pp_pattern state binders)
     ^^ space ^^ token eq ^^ space)
    (pp_expr state expr)

and pp_switch state {value = {kwd_switch; lpar; expr; rpar; lbrace; cases; rbrace}; _} =
  token kwd_switch ^^ space ^^ pp_par_like_document state (pp_expr state expr) lpar rpar
  ^^ space ^^ pp_braces_like_document state ~force_hardline:true (pp_cases state cases) lbrace rbrace

and pp_cases state (hd, tl) =
  List.fold ~f:(fun a i -> a ^^ break 0 ^^ pp_case state i) ~init:(pp_case state hd) tl

and pp_case state node =
  let pp_statements statements =
    group (
      match statements with
        Some s ->
          let app s = group (pp_statement state s)
          in pp_nsepseq hardline app s ^^ semi
      | None -> hardline)
  in
  let pp_label_and_statements label =
    function
      Some (s, []) when is_enclosed_statement s ->
        label ^^ space ^^ group (pp_statement state s) ^^ semi
    | statements ->
        hang state#indent (label ^/^ pp_statements statements)
  in
  match node with
    Switch_case {kwd_case; expr; colon; statements} ->
      let label =
        token kwd_case ^^ space ^^ pp_expr state expr ^^ token colon
      in pp_label_and_statements label statements
  | Switch_default_case {kwd_default; colon; statements} ->
      let label = token kwd_default ^^ token colon
      in pp_label_and_statements label statements

and pp_type state {value; _} =
  let ({attributes; kwd_type; name; params; eq; type_expr}: type_decl) = value in
  let attributes = filter_private attributes in
  let lhs = token kwd_type ^^ space ^^ token name
            ^^ pp_type_params state params
  in
  let rhs = group (pp_type_expr state type_expr) in
  let type_doc =
    if is_enclosed_type type_expr
    then lhs ^^ space ^^ token eq ^^ space ^^ rhs
    else lhs ^^ prefix state#indent 1 (space ^^ token eq) rhs
  in
  if List.is_empty attributes
  then type_doc
  else pp_attributes state attributes ^/^ type_doc

and pp_type_params state = function
  None -> empty
| Some value -> pp_chevrons state (pp_nsepseq (break 1) pp_ident) value

and pp_ident (node : variable) = token node

and pp_string s = dquotes (pp_ident s)

and pp_verbatim s = bquotes (pp_ident s)

and pp_bytes (node: (string * Hex.t) wrap)  =
  pp_comments node#comments ^/^
  string ("0x" ^ Hex.show (snd node#payload))

and pp_expr state = function
  EFun      e -> pp_fun state e
| EPar      e -> pp_par_expr state e.value
| ESeq      e -> pp_seq state e
| EVar      e -> pp_ident e
| EModA     e -> pp_module_access (pp_expr state) e
| ELogic    e -> pp_logic_expr state e
| EArith    e -> group (pp_arith_expr state e)
| ECall     e -> pp_call_expr state e
| EBytes    e -> pp_bytes e
| EArray    e -> pp_array state e
| EObject   e -> group (pp_object_expr state e)
| EString   e -> pp_string_expr e
| EProj     e -> pp_projection state e
| EAssign   e -> pp_assign state e
| EAnnot    e -> group (pp_annot_expr state e)
| EConstr   e -> pp_constr_expr state e
| EUnit     e -> token (fst e.value) ^^ token (snd e.value)
| ECodeInj  e -> pp_code_inj state e
| ETernary  e -> pp_ternary state e
| EContract e -> pp_contract state e
| EPrefix   e -> pp_prefix e
| EPostfix  e -> pp_postfix e

and pp_code_inj state (node: code_inj reg) =
  let {language; code} = node.value in
  let language = token language in
  let code     = pp_expr state code in
  group (language ^/^ code)

and pp_array state (node: (array_item, comma) Utils.sepseq brackets reg) =
  match node.value.inside with
    Some inside ->
      let pp_items = pp_nsepseq (break 1) (pp_array_item state) in
      pp_brackets_like_document state (pp_items inside) node.value.lbracket node.value.rbracket
  | None ->
      pp_brackets state (fun _ -> empty) node

and pp_call_expr state {value; _} =
  let lambda, arguments = value in
  let lpar, arguments, rpar =
    match arguments with
    | Unit unit -> fst (unit.value), None, snd (unit.value)
    | Multiple xs -> xs.value.lpar, Some xs.value.inside, xs.value.rpar
  in
  let arguments =
    pp_par_like_document state
      (Option.value_map ~default:empty ~f:(pp_nsepseq (break 1) (pp_expr state)) arguments)
      lpar
      rpar
  in
  pp_expr state lambda ^^ arguments

and pp_array_item state = function
  Expr_entry e -> pp_expr state e
| Rest_entry {value = {expr; ellipsis}; _} -> token ellipsis ^^ pp_expr state expr

and pp_constr_expr state {value; _} =
  let constr, arg = value in
  let constr = token constr in
  (* FIXME: parentheses in ctor_expr are not saved. *)
  let left = Wrap.ghost "(" in
  let right = Wrap.ghost ")" in
  constr ^^ group (
    Option.value_map
      ~default:(string "()")
      ~f:(fun exp -> pp_par_like_document state (pp_expr state exp) left right)
      arg)

and pp_object_property state = function
  Punned_property {value; _} ->
    pp_expr state value
| Property {value = {name; colon; value; _}; _} ->
    pp_expr state name ^^ token colon ^^ space ^^ pp_expr state value
| Property_rest {value = {expr; ellipsis}; _} ->
    token ellipsis ^^ pp_expr state expr

and pp_object_expr state (node: (property, comma) Utils.nsepseq braces reg) =
  let pp_properties = pp_nsepseq (break 1) (pp_object_property state)
  in pp_braces state pp_properties node

and pp_string_expr = function
  String e -> pp_string e
| Verbatim e -> pp_verbatim e

and pp_selection state = function
  FieldName {value = {dot; value}; _} -> token dot ^^ pp_ident value
| Component value -> pp_brackets state (pp_expr state) value

and pp_projection state {value = {expr; selection}; _} =
  pp_expr state expr ^^ pp_selection state selection

and pp_infix state lhs middle rhs =
  group (lhs ^^ space ^^ prefix state#indent 1 middle rhs)

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
  let expr, kwd_as, type_expr = value in
  let lhs = pp_expr state expr in
  let rhs = pp_type_expr state type_expr in
  pp_infix state lhs (token kwd_as) rhs

and pp_ternary state {value; _} =
  pp_expr state value.condition ^^
  space ^^ token value.qmark ^^ space ^^
  nest state#indent (pp_expr state value.truthy) ^^
  space ^^ token value.colon ^^ space ^^
  nest state#indent (pp_expr state value.falsy)

and pp_prefix = function
  {value = {update_type=Increment _; variable}; _} ->
    plus ^^ plus ^^ pp_ident variable
| {value = {update_type=Decrement _; variable}; _} ->
    minus ^^ minus ^^ pp_ident variable

and pp_postfix = function
  {value = {update_type=Increment _; variable}; _} ->
    pp_ident variable ^^ plus  ^^ plus
| {value = {update_type=Decrement _; variable}; _} ->
    pp_ident variable ^^ minus ^^ minus

and pp_logic_expr state = function
  BoolExpr e -> pp_bool_expr state e
| CompExpr e -> pp_comp_expr state e

and pp_bool_expr state = function
  Or   e  -> pp_bin_op state e
| And  e  -> pp_bin_op state e
| Not  e  -> pp_un_op  state e

and pp_bin_op state {value; _} =
  let {arg1; arg2; op} = value in
  let lhs = pp_expr state arg1 in
  let rhs = pp_expr state arg2 in
  pp_infix state lhs (token op) rhs

and pp_un_op state {value; _} =
  let {arg; op} = value in
  token op ^^ pp_expr state arg

and pp_comp_expr state = function
  Lt    e -> pp_bin_op state e
| Leq   e -> pp_bin_op state e
| Gt    e -> pp_bin_op state e
| Geq   e -> pp_bin_op state e
| Equal e -> pp_bin_op state e
| Neq   e -> pp_bin_op state e

and pp_arith_expr state = function
  Add   e -> pp_bin_op state e
| Sub   e -> pp_bin_op state e
| Mult  e -> pp_bin_op state e
| Div   e -> pp_bin_op state e
| Mod   e -> pp_bin_op state e
| Neg   e -> pp_un_op  state e
| Int   e -> pp_int e

and pp_int (node : (lexeme * Z.t) wrap) =
  pp_comments node#comments ^/^
  string (Z.to_string (snd node#payload))

and pp_par_expr state (node: expr par) =
  let {lpar; inside; rpar} = node in
  pp_par_like_document state (pp_expr state inside) lpar rpar

and pp_type_annot_rhs state colon value =
  group (nest state#indent (break 0 ^^ token colon ^^ space ^^ pp_type_expr state value))

(* In flat mode, we may render the arguments like so:

   let f = (x: int, y: int): int => /* */

   Otherwise we'll try to render it like so:

   let f = (
     x: int,
     y: int
   ): int => /* */
*)

and pp_expr_fun state = function
  EPar node -> pp_par state (pp_expr_fun state) node
| ESeq {value; _} ->
    pp_nsepseq (break 1) (pp_expr_fun state) value
| EAnnot {value; _} ->
    let expr, kwd_as, type_expr = value in
    pp_expr_fun state expr ^^ pp_type_annot_rhs state kwd_as type_expr
| EUnit {value; _} -> token (fst value) ^^ token (snd value)
| c -> pp_expr state c

and pp_fun state {value; _} =
  let {type_params; parameters; lhs_type; arrow; body} = value in
  let type_params = pp_type_params state type_params in
  let parameters = pp_expr_fun state parameters in
  let annot =
    match lhs_type with
      None            -> empty
    | Some (colon, e) -> pp_type_annot_rhs state colon e
  in
  match body with
  | FunctionBody fb ->
      let pp_statements = pp_nsepseq (break 1) (pp_statement state) in
      (* If the function has only one statement we may try to display
         it inline rather than in a new one.  *)
      let force_hardline = not @@ List.is_empty @@ snd fb.value.inside in
      type_params ^^ parameters ^^ annot ^^ space ^^ token arrow ^^ space
      ^^ pp_braces state ~force_hardline pp_statements fb
  | ExpressionBody e ->
      prefix state#indent 1
        (type_params ^^ parameters ^^ annot ^^ space ^^ token arrow)
        (pp_expr state e)

and pp_seq state {value; _} =
  pp_nsepseq (break 1) (pp_expr state) value

and pp_disc state value = pp_disc_or_sum state (Either.left value)

and pp_parameter {value; _} =
    string "parameter_of"
    ^^ group (nest 0 (break 1 ^^ pp_nsepseq (break 1) pp_ident value))

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
  let {module_name; selector; field} = value in
  group (pp_ident module_name ^^ token selector ^^ break 0 ^^ f field)

and pp_cartesian state (node: CST.cartesian) =
  let pp_type_exprs = pp_nsepseq (break 1) (pp_type_expr state) in
  prefix state#indent 1
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
               let variants =
                 Utils.nsepseq_map
                   (nest state#indent <@ pp_object_type state) disc
               in variants, [])
      (* JsLIGO's lexer always injects leading vertical bars in sum
         types, so we don't check for them here. *)
      ~right:(fun {value = {leading_vbar = _; variants; attributes}; _} ->
                let variants =
                  Utils.nsepseq_map
                    (nest state#indent <@ pp_variant state) variants.value
                in variants, attributes)
  in
  let head, tail = variants in
  let padding_flat =
    let open PrettyComb in
    match state#leading_vbar with
      Avoid | Only_on_new_line -> empty
    | Always -> bar ^^ space
  in
  let padding_non_flat =
    let open PrettyComb in
    match state#leading_vbar with
      Avoid -> blank state#indent
    | Always | Only_on_new_line -> bar ^^ space
  in

  (* Do not append a vertical bar if we are in flat mode, unless we
     have attributes. The reason is that those two are different:

     type t = [@annot] | Ctor
     type t = [@annot] Ctor
  *)

  let head =
    if List.is_empty tail || not (List.is_empty attributes)
    then bar ^^ space ^^ head
    else ifflat padding_flat padding_non_flat ^^ head
  in
  let app (bar, variant) = break 1 ^^ token bar ^^ space ^^ variant in
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
  let constr = pp_string constr in
  match params with
    None -> constr
  | Some (comma, params) ->
      group (constr ^^ token comma ^^ break 1
             ^^ pp_nsepseq (break 1) (pp_type_expr state) params)

and pp_attribute state (node : Attr.t wrap) =
  let key, val_opt = node#payload in
  let thread = string "/* @" ^^ string key in
  let thread = match val_opt with
                 Some Ident value ->
                   group (thread ^/^ nest state#indent (string value))
               | Some String value ->
                   group (thread ^/^
                          nest state#indent (string ("\"" ^ value ^ "\"")))
               | None -> thread in
  let thread = thread ^^ space ^^ string "*/"
  in thread

and pp_attributes state = function
  []    -> empty
| attrs -> separate_map (break 0) (pp_attribute state) attrs

and pp_object_type state fields =
  group (pp_ne_injection state (pp_field_decl state) fields)

and pp_field_decl state {value; _} =
  let {field_name; colon; field_type; attributes} = value in
  let attr = pp_attributes state attributes in
  let name = if List.is_empty attributes then pp_ident field_name
             else attr ^/^ pp_ident field_name in
  match field_type with
    TVar v when String.equal v#payload field_name#payload -> name
  | _ -> let t_expr = pp_type_expr state field_type in
         group (name ^^ token colon ^^ space ^^ group t_expr)

and pp_ne_injection :
  'a.state -> ('a -> document) -> 'a ne_injection reg -> document =
  fun state printer {value; _} ->
    let {compound; ne_elements; attributes; _} = value in
    let elements = pp_nsepseq (break 1) printer ne_elements in
    let inj =
      match compound with
        None -> elements
      | Some Braces (lbrace, rbrace) ->
        pp_braces_like_document state elements lbrace rbrace
      | Some Brackets (lbracket, rbracket) ->
        pp_brackets_like_document state elements lbracket rbracket
    in
    let inj = if List.is_empty attributes then inj
              else break 0 ^^ pp_attributes state attributes ^/^ inj
    in inj

and pp_type_app state (node: (type_constr * type_params) reg) =
  let ctor, tuple = node.value in
  pp_ident ctor ^^ pp_type_tuple state tuple

and pp_type_tuple state (node: type_params) =
  let {lchevron; inside; rchevron} = node.value in
  pp_chevrons_like_document
    state
    (pp_nsepseq (break 1) (pp_type_expr state) inside)
    lchevron
    rchevron

and pp_fun_type_arg state ({name; colon; type_expr} : CST.fun_type_arg) =
  group (pp_ident name ^^ token colon ^^ space
         ^^ pp_type_expr state type_expr)

and pp_fun_type state {value; _} =
  let lhs, arrow, rhs = value in
  let lhs = pp_par_like_document
             state
             (pp_nsepseq (break 1) (pp_fun_type_arg state) lhs.inside)
             lhs.lpar
             lhs.rpar in
  let rhs = pp_type_expr state rhs in
  group (lhs ^^ space ^^ token arrow ^^ space ^^ rhs)

and pp_type_par state value = pp_par state (pp_type_expr state) value

and pp_pattern state = function
  PRest     p -> pp_rest_pattern p
| PAssign   p -> pp_assign_pattern state p
| PVar      v -> pp_pvar state v
| PConstr   p -> pp_ident p
| PDestruct p -> pp_destruct state p
| PObject   p -> pp_pobject state p
| PArray    p -> pp_parray state p

and pp_parray state (node: (pattern, comma) Utils.nsepseq brackets reg) =
  let pp_patterns = pp_nsepseq (break 1) (pp_pattern state)
  in group (pp_brackets state pp_patterns node)

and pp_pobject state (node: (pattern, comma) Utils.nsepseq braces reg) =
  pp_braces state (pp_nsepseq (break 1) (pp_pattern state)) node

and pp_pvar state {value; _} =
  let {variable; attributes} = value in
  let v = pp_ident variable in
  if List.is_empty attributes then v
  else group (pp_attributes state attributes ^/^ v)

and pp_rest_pattern {value = {ellipsis; rest}; _} =
  token ellipsis ^^ pp_ident rest

and pp_assign_pattern state {value = {property; eq; value}; _} =
  pp_ident property ^^ token eq ^^ pp_expr state value

and pp_destruct state {value = {property; colon; target}; _} =
  pp_ident property ^^ token colon ^^ pp_val_binding state target

and pp_contract state {value; _} =
  (* FIXME: parentheses are not saved *)
  let left  = Wrap.ghost "("
  and right = Wrap.ghost ")" in
  string "contract_of"
  ^^ space
  ^^ pp_par_like_document
    state
    (group (nest 0 (break 0 ^^ pp_nsepseq (break 0) pp_ident value)))
    left
    right

let print_type_expr = pp_type_expr
let print_pattern   = pp_pattern
let print_expr      = pp_expr
let print_toplevel_statement = pp_toplevel_statement

type cst                = CST.t
type expr               = CST.expr
type type_expr          = CST.type_expr
type pattern            = CST.pattern
type toplevel_statement = CST.toplevel_statement
