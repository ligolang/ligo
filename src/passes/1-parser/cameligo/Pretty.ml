[@@@warning "-42"]

open AST
module Region = Simple_utils.Region
open! Region
open! PPrint

let paragraph (s : string) = flow (break 1) (words s)

let rec make ast =
  let app decl = group (pp_declaration decl) in
  separate_map hardline app (Utils.nseq_to_list ast.decl)

and pp_declaration = function
  Let decl -> pp_let_decl decl
| TypeDecl decl -> pp_type_decl decl

and pp_let_decl Region.{value; _} =
  let _, rec_opt, binding, attr = value in
  let rec_str =
    match rec_opt with
      None -> ""
    | Some _ -> " rec" in
  string "let" ^^ string rec_str
  ^/^ pp_let_binding binding ^/^ pp_attributes attr

and pp_attributes attr =
  let sep   = string ";" ^^ break 1 in
  let make s = string "[@@" ^^ string s.value ^^ string "]"
  in separate_map sep make attr

and pp_string Region.{value; _} = string value

and pp_let_binding (binding : let_binding) =
  let {binders; lhs_type; let_rhs; _} = binding in
  let patterns = Utils.nseq_to_list binders in
  let patterns = flow (break 1) (List.map pp_pattern patterns) in
  let lhs_type =
    match lhs_type with
      None -> string ""
    | Some (_, t_expr) -> string " :" ^/^ pp_type_expr t_expr in
  let let_rhs = pp_expr let_rhs in
  patterns ^^ lhs_type ^^ string " =" ^/^ let_rhs

and pp_pattern = function
  PConstr p -> pp_pconstr p
| PUnit   _ -> string "()"
| PFalse  _ -> string "false"
| PTrue   _ -> string "true"
| PVar    v -> pp_string v
| PInt    i -> pp_int i
| PNat    n -> pp_nat n
| PBytes  b -> pp_bytes b
| PString s -> pp_string s
| PWild   _ -> string "_"
| PList   l -> pp_plist l
| PTuple  t -> pp_ptuple t
| PPar    p -> pp_ppar p
| PRecord r -> pp_precord r
| PTyped  t -> pp_ptyped t

and pp_pconstr = function
  PNone      _ -> string "None"
| PSomeApp   p -> pp_some p
| PConstrApp a -> pp_c_app a

and pp_c_app Region.{value; _} =
  match value with
    constr, None -> pp_string constr
  | constr, Some pat -> pp_string constr ^/^ pp_pattern pat

and pp_some Region.{value; _} =
  string "Some" ^/^ pp_pattern (snd value)

and pp_int Region.{value; _} = string (fst value)
and pp_nat Region.{value; _} = string (fst value)
and pp_bytes Region.{value; _} = string (fst value)

and pp_ppar Region.{value; _} =
  let {lpar; inside; rpar} = value in
  string "(" ^^ pp_pattern inside ^^ string ")"

and pp_plist = function
  PListComp cmp -> pp_list_comp cmp
| PCons cons -> pp_cons cons

and pp_list_comp Region.{value; _} =
  let items = Utils.sepseq_to_list value.elements in
  let sep = string ";" ^^ break 1 in
  let items = separate_map sep pp_pattern items
  in string "[" ^^ items ^^ string "]"

and pp_cons Region.{value; _} =
  let patt1, _, patt2 = value in
  pp_pattern patt1 ^^ string " ::" ^/^ pp_pattern patt2

and pp_ptuple Region.{value; _} =
  let cmp = Utils.nsepseq_to_list value in
  let sep = string "," ^^ break 1 in
  separate_map sep pp_pattern cmp

and pp_precord Region.{value; _} =
  let fields = value.ne_elements in
  let fields = Utils.nsepseq_to_list fields in
  let sep    = string ";" ^^ break 1 in
  let fields = separate_map sep pp_field_pattern fields
  in string "{" ^^ fields ^^ string "}"

and pp_field_pattern Region.{value; _} =
  let {field_name; pattern; _} = value in
  pp_string field_name ^^ string " =" ^/^ pp_pattern pattern

and pp_ptyped Region.{value; _} =
  let {pattern; type_expr; _} = value in
  pp_pattern pattern ^^ string " :" ^/^ pp_type_expr type_expr

and pp_type_decl decl =
  let {name; type_expr; _} = decl.value in
  string "type" ^/^ string name.value
  ^/^ string "=" ^/^ pp_type_expr type_expr

and pp_expr expr =
  string "TODO:pp_expr"

and pp_type_expr = function
  TProd cartesian -> pp_cartesian cartesian
| TSum sum        -> pp_variants sum
| TRecord fields  -> pp_fields fields
| TApp t_app      -> pp_type_app t_app
| TFun fun_type   -> pp_fun_type fun_type
| TPar par        -> pp_type_par par
| TVar var        -> pp_string var
| TString s       -> pp_string s

and pp_cartesian Region.{value; _} =
  let cmp = Utils.nsepseq_to_list value in
  let sep = string " *" ^^ break 1 in
  separate_map sep pp_type_expr cmp

and pp_variants Region.{value; _} =
  let variants = Utils.nsepseq_to_list value
  and sep      = string " |" ^^ break 1 in
  separate_map sep pp_variant variants

and pp_variant Region.{value; _} =
  let {constr; arg} = value in
  match arg with
    None -> pp_string constr
  | Some (_, t_expr) ->
     pp_string constr ^^ string " of" ^/^ pp_type_expr t_expr

and pp_fields Region.{value; _} =
  let fields = value.ne_elements in
  let fields = Utils.nsepseq_to_list fields in
  let sep    = string ";" ^^ break 1 in
  let fields = separate_map sep pp_field_decl fields
  in string "{" ^^ fields ^^ string "}"

and pp_field_decl Region.{value; _} =
  let {field_name; field_type; _} = value in
  let name = pp_string field_name in
  let t_expr = pp_type_expr field_type
  in name ^^ string " :" ^/^ t_expr

and pp_type_app Region.{value; _} =
  let ctor, tuple = value
  in pp_type_tuple tuple ^/^ pp_type_constr ctor

and pp_type_tuple Region.{value; _} =
  let {lpar; inside; rpar} = value in
  match inside with
    t_expr, [] -> pp_type_expr t_expr
  |        seq -> let sep = string "," ^^ break 1 in
                 let lst = Utils.nsepseq_to_list seq in
                 let cmp = separate_map sep pp_type_expr lst
                 in string "(" ^^ cmp ^^ string ")"

and pp_type_constr ctor =
  string ctor.value

and pp_fun_type Region.{value; _} =
  let lhs, _, rhs = value in
  pp_type_expr lhs ^^ string " ->" ^/^ pp_type_expr rhs

and pp_type_par Region.{value; _} =
  let {lpar; inside; rpar} = value in
  string "(" ^^ pp_type_expr inside ^^ string ")"
