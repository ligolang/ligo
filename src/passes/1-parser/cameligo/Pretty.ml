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
| PSomeApp   p -> pp_patt_some p
| PConstrApp a -> pp_patt_c_app a

and pp_patt_c_app Region.{value; _} =
  match value with
    constr, None -> pp_string constr
  | constr, Some pat -> pp_string constr ^^ pp_pattern pat

and pp_patt_some Region.{value; _} =
  string "Some" ^/^ pp_pattern (snd value)

and pp_int Region.{value; _} =
  string (Z.to_string (snd value))

and pp_nat Region.{value; _} =
  string (Z.to_string (snd value) ^ "n")

and pp_bytes Region.{value; _} =
  string ("0x" ^ Hex.show (snd value))

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

and pp_expr = function
  ECase   e -> pp_case_expr e
| ECond   e -> pp_cond_expr e
| EAnnot  e -> pp_annot_expr e
| ELogic  e -> pp_logic_expr e
| EArith  e -> pp_arith_expr e
| EString e -> pp_string_expr e
| EList   e -> pp_list_expr e
| EConstr e -> pp_constr_expr e
| ERecord e -> pp_record_expr e
| EProj   e -> pp_projection e
| EUpdate e -> pp_update e
| EVar    v -> pp_string v
| ECall   e -> pp_call_expr e
| EBytes  e -> pp_bytes e
| EUnit   _ -> string "()"
| ETuple  e -> pp_tuple_expr e
| EPar    e -> pp_par_expr e
| ELetIn  e -> pp_let_in e
| EFun    e -> pp_fun e
| ESeq    e -> pp_seq e

and pp_case_expr Region.{value; _} =
  let {expr; cases; _} = value in
  string "match " ^^ pp_expr expr ^/^ string "with" ^/^ pp_cases cases

and pp_cases Region.{value; _} =
  let cases = Utils.nsepseq_to_list value
  and sep   = break 1 ^^ string "| "
  in separate_map sep pp_clause cases

and pp_clause Region.{value; _} =
  let {pattern; rhs; _} = value in
  pp_pattern pattern ^^ string " ->" ^/^ pp_expr rhs

and pp_cond_expr Region.{value; _} =
  let {test; ifso; kwd_else; ifnot; _} = value in
  let if_then = string "if " ^^ pp_expr test
                ^/^ string "then " ^^ pp_expr ifso in
  if kwd_else#is_ghost then if_then
  else if_then ^/^ string "else " ^^ pp_expr ifnot

and pp_annot_expr Region.{value; _} =
  let expr, _, type_expr = value.inside in
  string "(" ^^ pp_expr expr ^^ string " :"
  ^/^ pp_type_expr type_expr ^^ string ")"

and pp_logic_expr = function
  BoolExpr e -> pp_bool_expr e
| CompExpr e -> pp_comp_expr e

and pp_bool_expr = function
  Or   e  -> pp_bin_op "||" e
| And  e  -> pp_bin_op "&&" e
| Not  e  -> pp_un_op "not" e
| True  _ -> string "true"
| False _ -> string "false"

and pp_bin_op op Region.{value; _} =
  let {arg1; arg2; _} = value
  in pp_expr arg1 ^/^ string (op ^ " ") ^^ pp_expr arg2

and pp_un_op op Region.{value; _} =
  string (op ^ " ") ^^ pp_expr value.arg

and pp_comp_expr = function
  Lt    e -> pp_bin_op "<"  e
| Leq   e -> pp_bin_op "<=" e
| Gt    e -> pp_bin_op ">"  e
| Geq   e -> pp_bin_op ">=" e
| Equal e -> pp_bin_op "="  e
| Neq   e -> pp_bin_op "<>" e

and pp_arith_expr = function
  Add   e -> pp_bin_op "+" e
| Sub   e -> pp_bin_op "-" e
| Mult  e -> pp_bin_op "*" e
| Div   e -> pp_bin_op "/" e
| Mod   e -> pp_bin_op "mod" e
| Neg   e -> pp_un_op "-" e
| Int   e -> pp_int e
| Nat   e -> pp_nat e
| Mutez e -> pp_mutez e

and pp_mutez Region.{value; _} =
  string (Z.to_string (snd value) ^ "mutez")

and pp_string_expr = function
     Cat e -> pp_bin_op "^" e
| String e -> pp_string e

and pp_list_expr = function
      ECons e -> pp_bin_op "::" e
| EListComp e -> pp_injection pp_expr e

and pp_injection printer Region.{value; _} =
  let {compound; elements; terminator} = value in
  let elements = pp_sepseq ";" elements in
  let doc =
    match pp_compound compound with
      None -> elements
    | Some (opening, closing) ->
       string opening ^^ elements ^^ string closing
  in match terminator with
         None -> doc
     | Some _ -> doc ^^ string ";"

and pp_compound = function
  BeginEnd (start, _) ->
    if start#is_ghost then None
    else Some ("begin", "end")
| Braces _ -> Some ("{","}")
| Brackets _ -> Some ("[","]")

and pp_sepseq sep elements =
  let exprs = Utils.sepseq_to_list elements
  and sep   = string ";" ^/^ break 1
  in separate_map sep pp_expr exprs

and pp_constr_expr = function
  ENone      _ -> string "None"
| ESomeApp   a -> pp_some a
| EConstrApp a -> pp_constr_app a

and pp_some Region.{value=_, e; _} = string "Some" ^^ pp_expr e

and pp_constr_app Region.{value; _} =
  let constr, arg = value in
  let constr = string constr.value in
  match arg with
      None -> constr
  | Some e -> constr ^^ pp_expr e

and pp_record_expr ne_inj = pp_ne_injection pp_field_assign ne_inj

and pp_field_assign e =
  string "TODO:pp_field_assign"

and pp_ne_injection printer Region.{value; _} =
  let {compound; ne_elements; terminator} = value in
  let elements = pp_nsepseq printer ";" ne_elements in
  let doc =
    match pp_compound compound with
      None -> elements
    | Some (opening, closing) ->
       string opening ^^ elements ^^ string closing
  in match terminator with
         None -> doc
     | Some _ -> doc ^^ string ";"

and pp_nsepseq printer sep elements =
  let elems = Utils.nsepseq_to_list elements
  and sep   = string ";" ^/^ break 1
  in separate_map sep printer elems

and pp_projection _ = string "TODO:pp_projection"
and pp_update _ = string "TODO:pp_update"
and pp_call_expr _ = string "TODO:pp_call_expr"
and pp_tuple_expr _ = string "TODO:pp_tuple_expr"
and pp_par_expr _ = string "TODO:pp_par_expr"
and pp_let_in _ = string "TODO:pp_let_in"
and pp_fun _ = string "TODO:pp_fun_expr"
and pp_seq _ = string "TODO:pp_seq"

and pp_type_expr = function
  TProd t   -> pp_cartesian t
| TSum t    -> pp_variants t
| TRecord t -> pp_fields t
| TApp t    -> pp_type_app t
| TFun t    -> pp_fun_type t
| TPar t    -> pp_type_par t
| TVar t    -> pp_string t
| TString s -> pp_string s

and pp_cartesian Region.{value; _} =
  let cmp = Utils.nsepseq_to_list value in
  let sep = string " *" ^^ break 1 in
  separate_map sep pp_type_expr cmp

and pp_variants Region.{value; _} =
  let variants = Utils.nsepseq_to_list value
  and sep      = break 1 ^^ string "| " in
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
