[@@@warning "-42"]

module CST = Cst.Jsligo
open CST
module Region = Simple_utils.Region
open! Region
open! PPrint
module Option = Simple_utils.Option

let rec print ast =
  let stmt     = Utils.nseq_to_list ast.statements in
  let stmt     = List.filter_map pp_toplevel_statement stmt in
  let app stmt = group (stmt ^^ string ";")
  in separate_map (hardline ^^ hardline) app stmt

and pp_toplevel_statement = function
  TopLevel (stmt, _) -> Some (pp_statement stmt)
| Directive _   -> None

and pp_braced :
  'a. string -> ('a -> document) -> ('a, t) Utils.nsepseq braced reg -> document =
  fun sep printer {value; _}  ->
    let ({inside; _}: _ Utils.nsepseq braced) = value in
    let elements = pp_nsepseq sep printer inside in
    nest 2 (string "{" ^^ hardline ^^ elements) ^^ hardline ^^ string "}"

and pp_brackets :
  'a. string -> ('a -> document) -> ('a, t) Utils.nsepseq brackets reg -> document =
  fun sep printer {value; _} ->
    let ({inside; _}: _ Utils.nsepseq brackets) = value in
    let elements = pp_nsepseq sep printer inside in
    string "[" ^^ break 0 ^^ group elements ^^ string "]"

and pp_nsepseq :
  'a. string -> ('a -> document) -> ('a, t) Utils.nsepseq -> document =
  fun sep printer elements ->
    let elems = Utils.nsepseq_to_list elements
    and sep   = string sep ^^ break 1
    in separate_map sep printer elems

and pp_statement = function
  SBlock      s -> group (pp_braced ";" pp_statement s)
| SExpr       s -> pp_expr s
| SCond       s -> group (pp_cond_expr s)
| SReturn     s -> pp_return s
| SLet        s -> pp_let s
| SConst      s -> pp_const s
| SType       s -> pp_type s
| SSwitch     s -> pp_switch s
| SBreak      _ -> string "break" ^^ hardline
| SNamespace  s -> pp_namespace s
| SExport     s -> pp_export s
| SImport     s -> pp_import s
| SForOf      s -> pp_for_of s
| SWhile      s -> pp_while s

and pp_for_of {value; _} =
  string "for" ^^ string "(" ^^ string 
    (if value.const then "const" else "let") ^^ string value.name.value ^^ 
    string " of " ^^ 
    pp_expr value.expr ^^ string ")" ^^ pp_statement value.statement

and pp_while {value; _} =
  string "while" ^^ string "(" ^^ pp_expr value.expr ^^ string ")" ^^ pp_statement value.statement 

and pp_import {value; _} = 
  string "import" ^^ string value.alias.value ^^ string "=" ^^ pp_nsepseq "." (fun a -> string a.value) value.module_path

and pp_export {value = (_, statement); _} =
  string "export" ^^ pp_statement statement

and pp_namespace {value = (_, name, statements); _} =
  string "namespace" ^^ string name.value ^^ group (pp_braced ";" pp_statement statements) 

and pp_cond_expr {value; _} =
  let {test; ifso; ifnot; _} = value in
  let if_then = string "if" ^^ pp_par_expr test ^^ string " " ^^ pp_statement ifso in
  match ifnot with
    None -> if_then
  | Some (_,statement) -> if_then ^^ string " else " ^^ pp_statement statement

and pp_return {value = {expr; _}; _} =
  match expr with
    Some s -> string "return " ^^ pp_expr s
  | None -> string "return"

and pp_let {value = {bindings; _}; _} =
  string "let " ^^ pp_nsepseq "," pp_let_binding bindings

and pp_const {value = {bindings; _}; _} =
  string "const " ^^ pp_nsepseq "," pp_let_binding bindings

and pp_let_binding {value = {binders; lhs_type; expr; attributes; _}; _} =
  (if attributes = [] then empty else 
  pp_attributes attributes)
  ^^ 
  prefix 2 0 ((match lhs_type with
    Some (_, type_expr) -> pp_pattern binders ^^ string ": " ^^ pp_type_expr type_expr
  | None -> pp_pattern binders)
  ^^
  string " = "
  )

  (pp_expr expr)

and pp_switch {value = {expr; cases; _}; _} =
  string "switch(" ^^
  pp_expr expr ^^
  string ") {" ^^
  pp_cases cases ^^
  string "}"

and pp_cases cases =
  Utils.nseq_foldl (fun a i -> a ^^ pp_case i) empty cases

and pp_case = function
  Switch_case {expr; statements; _} ->
    string "case " ^^ pp_expr expr ^^ string ":" ^^
      (match statements with
         Some s ->
          let app s = group (pp_statement s) in
          separate_map (hardline ^^ hardline) app (Utils.nsepseq_to_list s)
       | None -> empty )
| Switch_default_case {statements; _} ->
    string "default: " ^^
    (match statements with
      Some s ->
      let app s = group (pp_statement s) in
      separate_map (hardline ^^ hardline) app (Utils.nsepseq_to_list s)
    | None -> empty)


and pp_type {value; _} =
  let ({name; type_expr; _}: type_decl) = value
  in
  string "type " ^^ string name.value ^^ string " = "
  ^^ group (pp_type_expr type_expr)

and pp_ident Region.{value; _} = string value

and pp_string s = string "\"" ^^ pp_ident s ^^ string "\""

and pp_verbatim s = string "`" ^^ pp_ident s ^^ string "`"

and pp_bytes (byte: (string * Hex.t) reg)  =
  let _, hex = byte.Region.value
  in string ("0x" ^ Hex.show hex)

and pp_expr = function
  EFun     e -> pp_fun e
| EPar     e -> pp_par_expr e.value
| ESeq     e -> pp_seq e
| EVar     v -> pp_ident v
| EModA    e -> pp_module_access pp_expr e
| ELogic   e -> pp_logic_expr e
| EArith   e -> group (pp_arith_expr e)
| ECall    e -> pp_call_expr e
| ENew     e -> pp_new_expr e
| EBytes   e -> pp_bytes e
| EArray   e -> group(pp_brackets "," pp_array_item e)
| EObject  e -> group(pp_object_expr e)
| EString  e -> pp_string_expr e
| EProj    e -> pp_projection e
| EAssign  (a,b,c) -> pp_assign (a,b,c)
| EAnnot   e -> pp_annot_expr e
| EConstr  e -> pp_constr_expr e
| EUnit    _ -> string "unit"
| ECodeInj _ -> failwith "TODO: ECodeInj"

and pp_call_expr {value; _} =
  let lambda, arguments = value in
  let arguments =
    match arguments with
    | Unit _ -> []
    | Multiple xs -> Utils.nsepseq_to_list xs.value.inside in
  let arguments = string "(" ^^ group (separate_map (string ", ") pp_expr arguments) ^^ string ")" in
  pp_expr lambda ^^ arguments

and pp_new_expr {value =(_, e);_} =
  string "new " ^^ pp_expr e

and pp_array_item = function
  Empty_entry _ -> empty
| Expr_entry e -> pp_expr e
| Rest_entry {value = {expr; _}; _} -> string "..." ^^ pp_expr expr


and pp_constr_expr = function
  ENone      _ -> string "None ()" 
| ESomeApp   a -> pp_some a
| EConstrApp a -> pp_constr_app a

and pp_some {value=_, e; _} =
  prefix 4 1 (string "Some") (string "(" ^^ pp_expr e ^^ string ")")

and pp_constr_app {value; _} =
  let constr, arg = value in
  let constr = string constr.value in
  match arg with
      None -> constr ^^ string "()"
  | Some e -> prefix 2 1 constr (string "(" ^^ pp_expr e ^^ string ")")

and pp_object_property = function
  Punned_property {value; _} -> pp_expr value
| Property {value = {name; value; _}; _} -> pp_expr name ^^ string ": " ^^ pp_expr value
| Property_rest {value = {expr; _}; _} -> string "..." ^^ pp_expr expr

and pp_object_expr oe =
  pp_braced "," pp_object_property oe

and pp_string_expr = function
  String e -> pp_string e
| Verbatim e -> pp_verbatim e

and pp_selection = function
  FieldName {value = {value;_ }; _} -> string "." ^^ pp_ident value
| Component {value = {inside; _}; _} -> string "[" ^^ pp_expr inside ^^ string "]"

and pp_projection {value = {expr; selection}; _} =
  pp_expr expr ^^ pp_selection selection

and pp_assign (a, _, b) =
  pp_expr a ^^ string " = " ^^ pp_expr b

and pp_annot_expr {value; _} =
  let expr, _, type_expr = value in
    group (nest 1 (pp_expr expr ^/^ string "as "
    ^^ pp_type_expr type_expr))

and pp_logic_expr = function
  BoolExpr e -> pp_bool_expr e
| CompExpr e -> pp_comp_expr e

and pp_bool_expr = function
  Or   e  -> pp_bin_op "||" e
| And  e  -> pp_bin_op "&&" e
| Not  e  -> pp_un_op "!" e
| True  _ -> string "true"
| False _ -> string "false"

and pp_bin_op op {value; _} =
  let {arg1; arg2; _} = value
  and length = String.length op + 1 in
  pp_expr arg1 ^^ string " " ^^ string (op ^ " ") ^^ nest length (pp_expr arg2)

and pp_un_op op {value; _} =
  string op ^^ pp_expr value.arg

and pp_comp_expr = function
  Lt    e -> pp_bin_op "<"  e
| Leq   e -> pp_bin_op "<=" e
| Gt    e -> pp_bin_op ">"  e
| Geq   e -> pp_bin_op ">=" e
| Equal e -> pp_bin_op "=="  e
| Neq   e -> pp_bin_op "!=" e

and pp_arith_expr = function
  Add   e -> pp_bin_op "+" e
| Sub   e -> pp_bin_op "-" e
| Mult  e -> pp_bin_op "*" e
| Div   e -> pp_bin_op "/" e
| Mod   e -> pp_bin_op "%" e
| Neg   e -> string "-" ^^ pp_expr e.value.arg
| Int   e -> pp_int e

and pp_int {value; _} =
  string (Z.to_string (snd value))

and pp_par_expr value =
  string "(" ^^ nest 1 (pp_expr value.inside ^^ string ")")

and pp_expr_fun = function
  EPar {value; _} ->
    string "(" ^^ nest 1 (pp_expr_fun value.inside ^^ string ")")
| ESeq {value; _} ->
    group (pp_nsepseq "," pp_expr_fun value)
| EAnnot   {value; _} ->
    let expr, _, type_expr = value in
      group (nest 1 (pp_expr_fun expr ^^ string ": "
                     ^^ pp_type_expr type_expr))
| EUnit _ -> string "()"
| _ as c -> pp_expr c

and pp_fun {value; _} =
  let {parameters; lhs_type; body; _} = value in
  let parameters = pp_expr_fun parameters
  and annot   =
    match lhs_type with
      None -> empty
    | Some (_,e) ->
       string ": " ^^ nest 2 (pp_type_expr e)
  in
  match body with
  | FunctionBody fb -> parameters ^^ annot ^^ string " => " ^^ (pp_braced ";" pp_statement fb)
  | ExpressionBody e -> (prefix 2 0 (nest 1 parameters ^^ annot ^^ string " => ") (pp_expr e))

and pp_seq {value; _} =
  pp_nsepseq "," pp_expr value

and pp_type_expr: type_expr -> document = function
  TProd   t -> pp_cartesian t
| TSum    t -> break 0 ^^ pp_sum_type t
| TObject t -> pp_object_type t
| TApp    t -> pp_type_app t
| TFun    t -> pp_fun_type t
| TPar    t -> pp_type_par t
| TVar    t -> pp_ident t
| TWild   _ -> string "_"
| TString s -> pp_string s
| TModA   t -> pp_module_access pp_type_expr t
| TInt    t -> pp_int t

and pp_module_access : type a.(a -> document) -> a module_access reg -> document
= fun f {value; _} ->
  let {module_name; field; _} = value in
  group (pp_ident module_name ^^ string "." ^^ break 0 ^^ f field)

and pp_cartesian v =
  (if v.attributes = [] then empty
  else pp_attributes v.attributes)
  ^/^
  group(pp_brackets "," pp_type_expr v.inside)

and pp_sum_type {value; _} =
  let {variants; attributes; _} = value in
  let head, tail = variants in
  let head = pp_type_expr head in
  let head =
    if tail = [] then string "|" ^^ head
    else ifflat (string "| " ^^ head) (string "| " ^^ head) in
  let rest = List.map snd tail in
  let app variant = break 1 ^^ string "| " ^^ pp_type_expr variant in
  let whole = head ^^ concat_map app rest in
  if attributes = [] then whole
  else pp_attributes attributes ^/^ whole

and pp_attributes = function
  [] -> empty
| attr -> 
  let make s = string "@" ^^ string s.value ^^ string " "
  in 
  string "/* " ^^ concat_map make attr ^^ string "*/ "

and pp_object_type fields = group (pp_ne_injection pp_field_decl fields)

and pp_field_decl {value; _} =
  let {field_name; field_type; attributes; _} = value in
  let attr = pp_attributes attributes in
  let name = if attributes = [] then pp_ident field_name
             else attr ^/^ pp_ident field_name in
  match field_type with
    TVar v when v = field_name -> name
  | _ -> let t_expr = pp_type_expr field_type
        in prefix 2 1 (name ^^ string ":") t_expr

and pp_ne_injection :
  'a.('a -> document) -> 'a ne_injection reg -> document =
  fun printer {value; _} ->
    let {compound; ne_elements; attributes; _} = value in
    let elements = pp_nsepseq "," printer ne_elements in
    let inj =
      match Option.map ~f:pp_compound compound with
        None -> elements
      | Some (opening, closing) ->
          string opening ^^ nest 2 (break 0 ^^ elements)
          ^^ break 0 ^^ string closing in
    let inj = if attributes = [] then inj
              else break 0 ^^ pp_attributes attributes ^/^ inj
    in inj

and pp_compound = function
| Braces   (_, _) -> ("{","}")
| Brackets (_, _) -> ("[","]")


and pp_type_app {value; _} =
  let ctor, tuple = value in
  (pp_ident ctor) ^^ (string "<" ^^ nest 1 (pp_type_tuple tuple) ^^ string ">")

and pp_type_tuple {value; _} =
  let head, tail = value.inside in
  let rec app = function
    []  -> empty
  | [e] -> group (break 1 ^^ pp_type_expr e)
  | e::items ->
      group (break 1 ^^ pp_type_expr e ^^ string ",") ^^ app items in
  if tail = []
  then pp_type_expr head
  else
    let components =
      pp_type_expr head ^^ string "," ^^ app (List.map snd tail)
    in components

and pp_fun_type_arg ({name; type_expr; _} : CST.fun_type_arg) =
  pp_ident name ^^ string ":" ^^ pp_type_expr type_expr

and pp_fun_type {value; _} =
  let lhs, _, rhs = value in
  group ( nest 1 (string "(" ^^ pp_nsepseq "," pp_fun_type_arg lhs.inside ^^ string ")") ^^ string " =>" ^/^ pp_type_expr rhs)

and pp_type_par {value; _} =
  string "(" ^^ nest 1 (pp_type_expr value.inside ^^ string ")")

and pp_pattern = function
  PRest     p -> pp_rest_pattern p
| PAssign   p -> pp_assign_pattern p
| PVar      v -> pp_ident v
| PConstr   p -> pp_ident p
| PDestruct p -> pp_destruct p
| PObject   p -> pp_braced "," pp_pattern p
| PWild     _ -> string "_"
| PArray    p -> group(pp_brackets "," pp_pattern p)

and pp_rest_pattern {value = {rest; _}; _} =
  string "..." ^^ pp_ident rest

and pp_assign_pattern {value = {property; value; _}; _} =
  pp_ident property ^^ string "=" ^^ pp_expr value

and pp_destruct {value = {property; target; _}; _} =
  pp_ident property ^^ string ":" ^^ pp_let_binding target

let print_type_expr = pp_type_expr
let print_pattern   = pp_pattern
let print_expr      = pp_expr

type cst        = Cst.Jsligo.t
type expr       = Cst.Jsligo.expr
type type_expr  = Cst.Jsligo.type_expr
type pattern    = Cst.Jsligo.pattern
