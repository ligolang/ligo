[@@@warning "-42"]

module CST = Cst.Reasonligo
open CST
module Region = Simple_utils.Region
open! Region
open! PPrint
module Option = Simple_utils.Option

let rec print ast =
  let app decl = group (pp_declaration decl) in
  separate_map (hardline ^^ hardline) app (Utils.nseq_to_list ast.decl)

and pp_declaration = function
  ConstDecl decl -> pp_const_decl decl
| TypeDecl  decl -> pp_type_decl decl

and pp_const_decl = function
| {value = (_,rec_opt, binding, attr); _} ->
  let let_str =
    match rec_opt with
        None -> "let "
    | Some _ -> "let rec " in
  let bindings = pp_let_binding let_str binding
  and attr    = pp_attributes attr
  in group (attr ^^ bindings ^^ string ";")

and pp_attributes = function
    [] -> empty
| attr ->
    let make s = string "[@" ^^ string s.value ^^ string "]" in
    group (break 0 ^^ separate_map (break 0) make attr) ^^ hardline

and pp_ident {value; _} = string value

and pp_string s = string "\"" ^^ pp_ident s ^^ string "\""

and pp_verbatim s = string "{|" ^^ pp_ident s ^^ string "|}"

and pp_let_binding let_ (binding : let_binding) =
  let {binders; lhs_type; let_rhs; _} = binding in
  let patterns = group (pp_pattern binders) in
  let lhs =
    string let_ ^^
      match lhs_type with
        None -> patterns ^^ string " = "
      | Some (_,e) ->
          patterns ^^ group (break 0 ^^ string ": " ^^ pp_type_expr e ^^ string " = ")
  in
  let rhs = pp_expr let_rhs in
  match let_rhs with
  | EFun _
  | ESeq _
  | ERecord _ -> lhs ^^ rhs
  | _ -> prefix 2 0 lhs rhs

and pp_pattern = function
  PConstr p -> pp_pconstr p
| PUnit   _ -> string "()"
| PVar    v -> pp_ident v
| PInt    i -> pp_int i
| PNat    n -> pp_nat n
| PBytes  b -> pp_bytes b
| PString s -> pp_string s
| PVerbatim s -> pp_verbatim s
| PWild   _ -> string "_"
| PList   l -> pp_plist l
| PTuple  t -> pp_ptuple t
| PPar    p -> pp_ppar p
| PRecord r -> pp_precord r
| PTyped  t -> pp_ptyped t

and pp_pconstr = function
  PNone      _ -> string "None"
| PSomeApp   p -> pp_patt_some p
| PFalse  _ -> string "false"
| PTrue   _ -> string "true"
| PConstrApp a -> pp_patt_c_app a

and pp_patt_c_app {value; _} =
  match value with
    constr, None -> pp_ident constr
  | constr, Some (PVar _ as pat) ->
      prefix 2 1 (pp_ident constr)  (pp_pattern pat)
  | constr, Some (_ as pat)->
      prefix 2 0 (pp_ident constr)  (pp_pattern pat)

and pp_patt_some {value; _} =
  prefix 2 0 (string "Some") (pp_pattern (snd value))

and pp_int {value; _} =
  string (Z.to_string (snd value))

and pp_nat {value; _} =
  string (Z.to_string (snd value) ^ "n")

and pp_bytes {value; _} =
  string ("0x" ^ Hex.show (snd value))

and pp_ppar {value; _} =
  string "(" ^^ nest 1 (pp_pattern value.inside) ^^ string ")"

and pp_plist = function
  PListComp cmp -> pp_list_comp cmp
| PCons cons -> pp_cons cons

and pp_list_comp e = group (pp_injection pp_pattern e)

and pp_cons {value; _} =
  let patt1, _, patt2 = value in
  string "[" ^^ (pp_pattern patt1 ^^ string ", ") ^^ group ( break 0 ^^ string "..." ^^ pp_pattern patt2) ^^ string "]"

and pp_ptuple {value; _} =
  let head, tail = value in
  let rec app = function
    []  -> empty
  | [p] -> group (break 1 ^^ pp_pattern p)
  | p::items ->
      group (break 1 ^^ pp_pattern p ^^ string ",") ^^ app items
  in if tail = []
     then nest 1 (pp_pattern head)
     else nest 1 (pp_pattern head ^^ string "," ^^ app (List.map snd tail))

and pp_precord fields = pp_ne_injection pp_field_pattern fields

and pp_field_pattern {value; _} =
  let {field_name; pattern; _} = value in
  prefix 2 1 (pp_ident field_name ^^ string " =") (pp_pattern pattern)

and pp_ptyped {value; _} =
  let {pattern; type_expr; _} = value in
  group (pp_pattern pattern ^^ string ": " ^^ pp_type_expr type_expr)

and pp_type_decl decl =
  let {name; type_expr; _} = decl.value in
  string "type " ^^ string name.value ^^ string " = "
  ^^ group (pp_type_expr type_expr) ^^ string ";"

and pp_expr = function
  ECase       e -> pp_case_expr e
| ECond       e -> group (pp_cond_expr e)
| EAnnot      e -> pp_annot_expr e
| ELogic      e -> pp_logic_expr e
| EArith      e -> group (pp_arith_expr e)
| EString     e -> pp_string_expr e
| EList       e -> group (pp_list_expr e)
| EConstr     e -> pp_constr_expr e
| ERecord     e -> pp_record_expr e
| EProj       e -> pp_projection e
| EUpdate     e -> pp_update e
| EVar        v -> pp_ident v
| ECall       e -> pp_call_expr e
| EBytes      e -> pp_bytes e
| EUnit       _ -> string "()"
| ETuple      e -> pp_tuple_expr e
| EPar        e -> pp_par_expr e
| ELetIn      e -> pp_let_in e
| EFun        e -> pp_fun e
| ESeq        e -> pp_seq e
| ECodeInj e -> pp_code_inj e

and pp_case_expr {value; _} =
  let {expr; cases; _} = value in
  group (string "switch" ^^ string "(" ^^ nest 1 (pp_expr expr)
         ^^ string ") " ^^ string "{"
         ^^ pp_cases cases ^^ hardline ^^ string "}")

and pp_cases {value; _} =
  let head, tail = value in
  let rest = List.map snd tail in
  let app clause = break 1 ^^ string "| " ^^ pp_clause clause
  in  concat_map app (head :: rest)

and pp_clause {value; _} =
  let {pattern; rhs; _} = value in
  prefix 4 1 (pp_pattern pattern ^^ string " =>") (pp_expr rhs)

and pp_cond_expr {value; _} =
  let {test; ifso; ifnot; _} = value in
  let if_then =
    string "if" ^^ string " (" ^^ pp_expr test ^^ string ")" ^^ string " {" ^^ break 0
    ^^ group (nest 2 (break 2 ^^ pp_expr ifso)) ^^ hardline ^^ string "}" in
  match ifnot with
    None -> if_then
  | Some (_,ifnot) ->
    if_then
    ^^ string " else" ^^ string " {" ^^ break 0 ^^ group (nest 2 (break 2 ^^ pp_expr ifnot)) ^^ hardline ^^ string "}"

and pp_annot_expr {value; _} =
  let expr, _, type_expr = value.inside in
    group (nest 1 (pp_expr expr ^/^ string ": "
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
  string (op ^ " ") ^^ pp_expr value.arg

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
| Mod   e -> pp_bin_op "mod" e
| Neg   e -> string "-" ^^ pp_expr e.value.arg
| Int   e -> pp_int e
| Nat   e -> pp_nat e
| Mutez e -> pp_mutez e

and pp_mutez {value; _} =
  Z.to_string (snd value) ^ "mutez" |> string

and pp_string_expr = function
     Cat e -> pp_bin_op "++" e
| String e -> pp_string e
| Verbatim e -> pp_verbatim e

and pp_list_expr = function
| ECons {value = {arg1; arg2; _}; _ } ->
  string "[" ^^ pp_expr arg1 ^^ string "," ^^ break 1 ^^ string "..." ^^ pp_expr arg2 ^^ string "]"
| EListComp e -> group (pp_injection pp_expr e)

and pp_injection :
  'a.('a -> document) -> 'a injection reg -> document =
  fun printer {value; _} ->
    let {compound; elements; _} = value in
    let sep = (string ",") ^^ break 1 in
    let elements = Utils.sepseq_to_list elements in
    let elements = separate_map sep printer elements in
    match Option.map pp_compound compound with
      None -> elements
    | Some (opening, closing) ->
        string opening ^^ nest 1 elements ^^ string closing

and pp_compound = function
  BeginEnd (_, _) -> ("begin","end")
| Braces   (_, _) -> ("{","}")
| Brackets (_, _) -> ("[","]")

and pp_constr_expr = function
  ENone      _ -> string "None"
| ESomeApp   a -> pp_some a
| EConstrApp a -> pp_constr_app a

and pp_some {value=_, e; _} =
  prefix 4 1 (string "Some") (pp_expr e)

and pp_constr_app {value; _} =
  let constr, arg = value in
  let constr = string constr.value in
  match arg with
      None -> constr
  | Some e -> prefix 2 1 constr (pp_expr e)

and pp_record_expr ne_inj = pp_ne_injection pp_field_assign ne_inj

and pp_field_assign {value; _} =
  let {field_name; field_expr; _} = value in
  prefix 2 1 (pp_ident field_name ^^ string ":") (pp_expr field_expr)

and pp_ne_injection :
  'a.('a -> document) -> 'a ne_injection reg -> document =
  fun printer {value; _} ->
    let {compound; ne_elements; _} = value in
    let elements = pp_nsepseq "," printer ne_elements in
    match Option.map pp_compound compound with
      None -> elements
    | Some (opening, closing) ->
        string opening ^^ nest 2 (break 0 ^^ elements) ^^ break 1 ^^ string closing

and pp_nsepseq :
  'a.string -> ('a -> document) -> ('a, t) Utils.nsepseq -> document =
  fun sep printer elements ->
    let elems = Utils.nsepseq_to_list elements
    and sep   = string sep ^^ break 1
    in separate_map sep printer elems

and pp_projection {value; _} =
  let {struct_name; field_path; _} = value in
  let subpath = Utils.nsepseq_to_list field_path in
  let subpath = concat_map pp_selection subpath in
  group (pp_ident struct_name ^^ subpath)

and pp_selection = function
  FieldName v   -> string "." ^^ break 0 ^^ string v.value
| Component cmp ->
    string "[" ^^ (cmp.value |> snd |> Z.to_string |> string) ^^ string "]"

and pp_update {value; _} =
  let {record; updates; _} = value in
  let updates = group (pp_ne_injection pp_field_path_assign updates)
  and record  = pp_path record in
  string "{..." ^^ record ^^ string ","
  ^^ nest 2 (break 1 ^^ updates ^^ string "}")

and pp_code_inj {value; _} =
  let {language; code; _} = value in
  let language = pp_string language.value
  and code     = pp_expr code in
  string "[%" ^^ language ^/^ code ^^ string "]"

and pp_field_path_assign {value; _} =
  let {field_path; field_expr; _} = value in
  let path = pp_path field_path in
  prefix 2 1 (path ^^ string ":") (pp_expr field_expr)

and pp_path = function
  Name v -> pp_ident v
| Path p -> pp_projection p

and pp_call_expr {value; _} =
  let lambda, arguments = value in
  let arguments = Utils.nseq_to_list arguments in
  let arguments = string "(" ^^ group (separate_map (string "," ^^ break 0 ^^ string " ") pp_expr arguments) ^^ string ")" in
  group (break 0 ^^ pp_expr lambda ^^ nest 2 arguments)

and pp_tuple_expr {value; _} =
  let head, tail = value in
  let rec app = function
    []  -> empty
  | [e] -> group (break 1 ^^ pp_expr e)
  | e::items ->
      group (break 1 ^^ pp_expr e ^^ string ",") ^^ app items
  in if tail = []
     then nest 1 (pp_expr head)
     else nest 1 (pp_expr head ^^ string "," ^^ app (List.map snd tail))

and pp_par_expr {value; _} =
  string "(" ^^ nest 1 (pp_expr value.inside ^^ string ")")

and pp_let_in {value; _} =
  let {binding; kwd_rec; body; attributes; _} = value in
  let let_str =
    match kwd_rec with
        None -> "let "
    | Some _ -> "let rec " in
  let bindings = pp_let_binding let_str binding
  and attr    = pp_attributes attributes
  in attr ^^ bindings
     ^^ string ";" ^^ hardline ^^ pp_expr body

and pp_fun {value; _} =
  let {binders; lhs_type; body; _} = value in
  let binders = pp_pattern binders
  and annot   =
    match lhs_type with
      None -> empty
    | Some (_,e) ->
        group (break 0 ^^ string ": " ^^ nest 2 (pp_type_expr e))
  in
  match body with
  | ESeq _ -> nest 1 binders ^^ annot ^^ string " => " ^^ pp_expr body
  | _ -> (prefix 2 0 (nest 1 binders ^^ annot ^^ string " => ") (pp_expr body))

and pp_seq {value; _} =
  let {compound; elements; _} = value in
  let sep = string ";" ^^ hardline in
  let elements = Utils.sepseq_to_list elements in
  let elements = separate_map sep pp_expr elements in
  match Option.map pp_compound compound with
    None -> elements
  | Some (opening, closing) ->
     string opening
     ^^ nest 2 (hardline ^^ elements) ^^ hardline
     ^^ string closing

and pp_type_expr = function
  TProd t   -> pp_cartesian t
| TSum t    -> break 0 ^^ pp_variants t
| TRecord t -> pp_fields t
| TApp t    -> pp_type_app t
| TFun t    -> pp_fun_type t
| TPar t    -> pp_type_par t
| TVar t    -> pp_ident t
| TWild   _ -> string "_"
| TString s -> pp_string s

and pp_cartesian {value; _} =
  let head, tail = value in
  let rec app = function
    []  -> empty
  | [e] -> group (break 1 ^^ pp_type_expr e)
  | e::items ->
      group (break 1 ^^ pp_type_expr e ^^ string ",") ^^ app items
  in
  string "(" ^^ nest 1 (pp_type_expr head ^^ (if tail <> [] then string "," else empty) ^^ app (List.map snd tail)) ^^ string ")"

and pp_variants {value; _} =
  let head, tail = value in
  let head = pp_variant head in
  let head = if tail = [] then head
             else ifflat head (string "  " ^^ head) in
  let rest = List.map snd tail in
  let app variant = break 1 ^^ string "| " ^^ pp_variant variant
  in head ^^ concat_map app rest

and pp_variant {value; _} =
  let {constr; arg} = value in
  match arg with
    None -> pp_ident constr
  | Some (_, e) ->
      prefix 2 0 (pp_ident constr) (string "(" ^^ pp_type_expr e ^^ string ")")

and pp_fields fields = group (pp_ne_injection pp_field_decl fields)

and pp_field_decl {value; _} =
  let {field_name; field_type; _} = value in
  let name = pp_ident field_name in
  match field_type with
  | TVar v when v = field_name ->
    name
  | _ ->
    let t_expr = pp_type_expr field_type
    in prefix 2 1 (name ^^ string ":") t_expr

and pp_type_app {value; _} =
  let ctor, tuple = value in
  prefix 2 0 (pp_type_constr ctor) (string "(" ^^ nest 1 (pp_type_tuple tuple) ^^ string ")")

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

and pp_type_constr ctor = string ctor.value

and pp_fun_args {value; _} =
  let lhs, _, rhs = value in
  match rhs with
  | TFun tf -> group (pp_type_expr lhs ^^ string ", " ^^ pp_fun_args tf)
  | _ -> group (pp_type_expr lhs ^^ string ")" ^^ string " =>" ^/^ pp_type_expr rhs)

and pp_fun_type {value; _} =
  let lhs, _, rhs = value in
  group ( nest 1 (pp_type_expr lhs) ^^ string " =>" ^/^ pp_type_expr rhs)

and pp_type_par {value; _} =
  string "(" ^^ nest 1 (pp_type_expr value.inside ^^ string ")")
