[@@@warning "-42"]

module CST = Cst_cameligo.CST
open CST
module Region = Simple_utils.Region
open! Region
open! PPrint

let pp_markup markup pos =
  let rec inner result previous_after = function
    (LineCom (c, Before))  :: rest when pos = Before -> inner (result ^^ string "//" ^^ string c.value ^^ hardline) None rest
  | (BlockCom (c, Before)) :: rest when pos = Before -> inner (result ^^ string "(*" ^^ string c.value ^^ string "*)"  ^^ hardline) None rest
  | (LineCom (c, Inline)) :: rest when pos = Inline -> inner (result ^^ string "//" ^^ string c.value) None rest
  | (BlockCom (c, Inline)) :: rest when pos = Inline -> inner (result ^^ string " (*" ^^ string c.value ^^ string "*) ") None rest
  | (LineCom (c, After)) :: rest when pos = After ->
      let line = c.region#stop#line in
      let hl = match previous_after with
        Some previous_after ->
          if line > previous_after + 1 then
            hardline ^^ hardline
          else
            hardline
      | None ->
          hardline
      in
      inner (result ^^ hl ^^ string "//" ^^ string c.value) (Some line) rest
  | (BlockCom (c, After)) :: rest  when pos = After ->
    let line = c.region#stop#line in
    let hl = match previous_after with
      Some previous_after ->
        if line > previous_after + 1 then
          hardline ^^ hardline
        else
          hardline
    | None ->
        hardline
    in
    inner (result ^^ hl ^^ string ("(*" ^ c.value ^ "*)")) (Some line) rest
  | [] -> result
  | _ -> empty
  in
  inner empty None markup

let pp_region_reg func token  =
  (pp_markup token.region#markup Before) ^^
  func token ^^
  (pp_markup token.region#markup Inline) ^^
  (pp_markup token.region#markup After)

let pp_region_t func token  =
  (pp_markup token#markup Before) ^^
  func ^^
  (pp_markup token#markup Inline) ^^
  (pp_markup token#markup After)

let pp_par printer {value; _} =
  (pp_region_t lparen value.lpar) ^^ printer value.inside ^^ (pp_region_t rparen value.rpar)


let rec print ast =
  let decl = Utils.nseq_to_list ast.decl in
  let decl = List.filter_map pp_declaration decl
  in separate_map (hardline ^^ hardline) group decl ^^ pp_region_t (empty) ast.eof

and pp_declaration = function
  Let         decl -> Some (pp_let_decl     decl)
| TypeDecl    decl -> Some (pp_type_decl    decl)
| ModuleDecl  decl -> Some (pp_module_decl  decl)
| ModuleAlias decl -> Some (pp_module_alias decl)
| Directive      _ -> None

(*
and pp_dir_decl = function
  Directive.Linemarker {value; _} ->
    let open Directive in
    let linenum, file_path, flag_opt = value in
    let flag =
      match flag_opt with
        Some Push -> " 1"
      | Some Pop  -> " 2"
      | None      -> "" in
    let lexeme = Printf.sprintf "# %d %S%s" linenum file_path flag
    in string lexeme
*)


and pp_let_decl {value; _} =
  let kwd_let, rec_opt, binding, attr = value in
  let let_str =
    match rec_opt with
        None -> (pp_region_t (string "let ") kwd_let)
    | Some r -> (pp_region_t (string "let ") kwd_let) ^^ (pp_region_t (string "rec ") r)
  in
  let let_str = if attr = [] then let_str
                else pp_attributes attr ^/^ let_str
  in let_str ^^ pp_let_binding binding

and pp_attributes = function
    [] -> empty
| attr ->
   let make s =
    pp_region_reg (fun _ -> string "[@" ^^ string s.value ^^ string "]") s
   in separate_map (break 0) make attr

and pp_ident {value; _} = string value

and pp_string s = string "\"" ^^ pp_ident s ^^ string "\""

and pp_verbatim s = string "{|" ^^ pp_ident s ^^ string "|}"

and pp_let_binding (binding : let_binding) =
  let {binders; type_params; lhs_type; let_rhs; eq; _} = binding in
  let head, tail = binders in
  let lhs = pp_pattern head in
  let lhs =
    match type_params with
      None -> lhs
    | Some params -> lhs ^/^ pp_par pp_type_params params in
  let lhs =
    match tail with
      [] -> lhs
    |  _ -> group (nest 2 (lhs ^/^ separate_map (break 1) pp_pattern tail)) in
  let lhs =
    lhs ^^
    match lhs_type with
            None -> empty
    | Some (c,e) -> prefix 2 1 (pp_region_t (string " :") c) (pp_type_expr e)
  in prefix 2 1 (group lhs ^^ group (pp_region_t (string " =") eq))
            (group (pp_expr let_rhs))

and pp_type_params (node : type_params) =
  let type_vars = Utils.nseq_to_list node.type_vars in
  let type_vars = separate_map (string " ") pp_ident type_vars
  in string "type " ^^ type_vars

and pp_pattern = function
  PConstr   p -> pp_region_reg pp_pconstr p
| PUnit     t -> pp_region_reg (fun _ -> string "()") t
| PVar      v -> pp_pvar v
| PInt      i -> pp_region_reg pp_int i
| PNat      n -> pp_region_reg pp_nat n
| PBytes    b -> pp_region_reg pp_bytes b
| PString   s -> pp_region_reg pp_string s
| PVerbatim s -> pp_region_reg pp_verbatim s
| PList     l -> pp_plist l
| PTuple    t -> pp_region_reg pp_ptuple t
| PPar      p -> pp_region_reg pp_ppar p
| PRecord   r -> pp_region_reg pp_precord r
| PTyped    t -> pp_region_reg pp_ptyped t

and pp_pvar {value; _} =
  let {variable; attributes} = value in
  let v = pp_region_reg pp_ident variable in
  if attributes = [] then v
  else group (pp_attributes attributes ^/^ v)

and pp_pconstr {value; _} =
  match value with
    constr, None -> pp_region_reg pp_ident constr
  | constr, Some pat ->
      prefix 4 1 (pp_region_reg pp_ident constr) (pp_pattern pat)

and pp_int {value; _} =
  string (Z.to_string (snd value))

and pp_nat {value; _} =
  string (Z.to_string (snd value) ^ "n")

and pp_bytes {value; _} =
  string ("0x" ^ Hex.show (snd value))

and pp_ppar p = pp_par pp_pattern p

and pp_plist = function
  PListComp cmp -> pp_region_reg pp_list_comp cmp
| PCons cons -> pp_region_reg pp_pcons cons

and pp_list_comp e = group (pp_injection pp_pattern e)

and pp_pcons {value; _} =
  let patt1, cons, patt2 = value in
  prefix 2 1 (pp_pattern patt1 ^^ (pp_region_t (string " ::") cons)) (pp_pattern patt2)

and pp_ptuple {value; _} =
  let head, tail = value in
  let rec app = function
    []  -> empty
  | [(_, p)] -> group (break 1 ^^ pp_pattern p)
  | (c, p)::items ->
      group (break 1 ^^ pp_pattern p ^^ (pp_region_t (string ",") c)) ^^ app items
  in match tail with
    [] -> pp_pattern head
  | h :: tail -> pp_pattern head ^^ (pp_region_t (string ",") (fst h)) ^^ app (h :: tail)

and pp_precord fields = pp_ne_injection pp_field_pattern fields

and pp_field_pattern {value; _} =
  let {field_name; pattern; eq; _} = value in
  prefix 2 1 (pp_region_reg pp_ident field_name ^^ (pp_region_t (string " =") eq)) (pp_pattern pattern)

and pp_ptyped {value; _} =
  let {pattern; type_expr; colon; _} = value in
  group (pp_pattern pattern ^^ (pp_region_t (string " :") colon) ^/^ pp_type_expr type_expr)

and pp_type_decl decl =
  let {kwd_type; params; name; type_expr; eq; _} = decl.value in
  group (pp_region_t (string "type ") kwd_type
         ^^ (match params with
               None -> empty
             | Some params -> pp_quoted_params params ^^ string " ")
         ^^ pp_region_reg pp_ident name
         ^^ (pp_region_t (string " = ") eq)
         ^/^ pp_type_expr type_expr)

and pp_quoted_params = function
  QParam p -> pp_quoted_param p
| QParamTuple tuple -> pp_par pp_quoted_params_nsepseq tuple

and pp_quoted_params_nsepseq seq = pp_nsepseq "," pp_quoted_param seq

and pp_module_decl decl =
  let {kwd_module; name; module_; eq; kwd_struct; kwd_end} = decl.value in
  (pp_region_t (string "module ") kwd_module) ^^ pp_region_reg pp_ident name ^^ pp_region_t (string " =") eq ^^ pp_region_t (string " struct") kwd_struct
  ^^ group (nest 0 (break 1 ^^ print module_))
  ^^ pp_region_t (string " end") kwd_end

and pp_module_alias decl =
  let {kwd_module; alias; eq; binders} = decl.value in
  pp_region_t (string "module ") kwd_module ^^ pp_region_reg pp_ident alias ^^ pp_region_t (string " =") eq
  ^^ group (nest 0 (break 1 ^^ pp_nsepseq "." (pp_region_reg pp_ident) binders))

and pp_expr = function
  ECase       e -> pp_region_reg pp_case_expr e
| ECond       e -> group (pp_region_reg pp_cond_expr e)
| EAnnot      e -> pp_region_reg pp_annot_expr e
| ELogic      e -> group (pp_logic_expr e)
| EArith      e -> group (pp_arith_expr e)
| EString     e -> pp_string_expr e
| EList       e -> group (pp_list_expr e)
| EConstr     e -> pp_region_reg pp_constr_expr e
| ERecord     e -> pp_region_reg pp_record_expr e
| EProj       e -> pp_region_reg pp_projection e
| EModA       e -> pp_module_access pp_expr e
| EUpdate     e -> pp_region_reg pp_update e
| EVar        v -> pp_region_reg pp_ident v
| ECall       e -> pp_region_reg pp_call_expr e
| EBytes      e -> pp_region_reg pp_bytes e
| EUnit       _ -> string "()"
| ETuple      e -> pp_region_reg pp_tuple_expr e
| EPar        e -> pp_region_reg pp_par_expr e
| ELetIn      e -> pp_region_reg pp_let_in e
| ETypeIn     e -> pp_region_reg pp_type_in e
| EModIn      e -> pp_region_reg pp_mod_in e
| EModAlias   e -> pp_region_reg pp_mod_alias e
| EFun        e -> pp_region_reg pp_fun e
| ESeq        e -> pp_region_reg pp_seq e
| ECodeInj    e -> pp_region_reg pp_code_inj e

and pp_case_expr {value; _} =
  let {kwd_match; expr; kwd_with; lead_vbar; cases} = value in
  group (pp_region_t (string "match ") kwd_match ^^ nest 6 (pp_expr expr) ^/^ pp_region_t (string "with") kwd_with)
  ^^ hardline ^^
  (match lead_vbar with
    Some v -> pp_region_t (string "| ") v
  | None -> blank 2) ^^
  pp_cases cases

and pp_cases {value; _} =
  let head, tail = value in
  let head       = pp_clause head in
  let rest       = tail in
  let app (vbar, clause) = break 1 ^^ pp_region_t (string "| ") vbar ^^ pp_clause clause
  in  head ^^ concat_map app rest

and pp_clause {value; _} =
  let {pattern; rhs; arrow} = value in
    pp_pattern pattern ^^ prefix 4 1 (pp_region_t (string " ->") arrow) (pp_expr rhs)

and pp_cond_expr {value; _} =
  let {kwd_if; test; kwd_then; ifso; ifnot} = value in
  let test = pp_region_t (string "if ") kwd_if ^^ group (nest 3 (pp_expr test))
  and ifso = pp_region_t (string " then") kwd_then ^^ group (nest 2 (break 1 ^^ pp_expr ifso))
  in match ifnot with
    Some (kwd_else, ifnot) ->
    let ifnot = pp_region_t (string "else") kwd_else ^^ group (nest 2 (break 1 ^^ pp_expr ifnot)) in
    group(test ^^ ifso) ^/^ ifnot
  | None ->
    test ^/^ ifso

and pp_annot_expr {value; _} =
  let expr, _, type_expr = value.inside in
    group ((pp_region_t (string "(") value.lpar) ^^ nest 1 (pp_expr expr ^/^ string ": "
    ^^ pp_type_expr type_expr ^^ (pp_region_t (string ")") value.rpar)))

and pp_logic_expr = function
  BoolExpr e -> pp_bool_expr e
| CompExpr e -> pp_comp_expr e

and pp_bool_expr = function
  Or   e  -> pp_region_reg (pp_bin_op "||") e
| And  e  -> pp_region_reg (pp_bin_op "&&") e
| Not  e  -> pp_region_reg (pp_un_op "not") e

and pp_bin_op op {value; _} =
let {arg1; arg2; _} = value
and length = String.length op + 1 in
pp_expr arg1 ^/^ string (op ^ " ") ^^ nest length (pp_expr arg2)

and pp_un_op op {value; _} =
  string (op ^ " ") ^^ pp_expr value.arg

and pp_comp_expr = function
  Lt    e -> pp_region_reg (pp_bin_op "<")  e
| Leq   e -> pp_region_reg (pp_bin_op "<=") e
| Gt    e -> pp_region_reg (pp_bin_op ">") e
| Geq   e -> pp_region_reg (pp_bin_op ">=") e
| Equal e -> pp_region_reg (pp_bin_op "=")  e
| Neq   e -> pp_region_reg (pp_bin_op "<>") e

and pp_arith_expr = function
  Add   e -> pp_region_reg (pp_bin_op "+") e
| Sub   e -> pp_region_reg (pp_bin_op "-") e
| Mult  e -> pp_region_reg (pp_bin_op "*") e
| Div   e -> pp_region_reg (pp_bin_op "/") e
| Mod   e -> pp_region_reg (pp_bin_op "mod") e
| Land  e -> pp_region_reg (pp_bin_op "land") e
| Lor   e -> pp_region_reg (pp_bin_op "lor") e
| Lxor  e -> pp_region_reg (pp_bin_op "lxor") e
| Lsl   e -> pp_region_reg (pp_bin_op "lsl") e
| Lsr   e -> pp_region_reg (pp_bin_op "lsr") e
| Neg   e -> pp_region_reg (fun e -> string "-" ^^ pp_expr e.value.arg) e
| Int   e -> pp_region_reg pp_int e
| Nat   e -> pp_region_reg pp_nat e
| Mutez e -> pp_region_reg pp_mutez e

and pp_mutez {value; _} =
  Z.to_string (snd value) ^ "mutez" |> string

and pp_string_expr = function
     Cat e -> pp_region_reg (pp_bin_op "^") e
| String e -> pp_region_reg pp_string e
| Verbatim e -> pp_region_reg pp_verbatim e

and pp_list_expr = function
      ECons e -> pp_region_reg (pp_bin_op "::") e
| EListComp e -> group (pp_injection pp_expr e)

and pp_injection :
  'a.('a -> document) -> 'a injection reg -> document =
  fun printer {value; _} ->
    let {compound; elements; _} = value in
    let rec inner result = function
    | (c, item) :: rest -> inner (result ^^ pp_region_t (string ";") c ^^ break 1 ^^ (printer item)) rest
    | [] -> result
    in
    let elements = match elements with
    | Some elements -> printer (fst elements) ^^ inner empty (snd elements)
    | None -> empty
    in
    match compound with
      None -> elements
    | Some compound ->
        let (opening, a), (closing, b) = pp_compound compound
        in (pp_region_t (string opening) a) ^^ nest 1 elements ^^
             (pp_region_t (string closing) b)

and pp_compound = function
  BeginEnd (a, b) -> (("begin", a), ("end", b))
| Braces (a, b)   -> (("{", a), ("}", b))
| Brackets (a, b) -> (("[", a), ("]",b))

and pp_constr_expr {value; _} =
  let constr, arg = value in
  let constr = pp_region_reg (fun t -> string t.value) constr in
  match arg with
      None -> constr
  | Some e -> prefix 2 1 constr (pp_expr e)

and pp_record_expr ne_inj = group (pp_ne_injection pp_field_assign ne_inj)

and pp_field_assign {value; _} =
  let {field_name; field_expr; assignment} = value in
  prefix 2 1 (pp_region_reg pp_ident field_name ^^ pp_region_t (string " =") assignment) (pp_expr field_expr)

and pp_ne_injection :
  'a.('a -> document) -> 'a ne_injection reg -> document =
  fun printer {value; _} ->
    let {compound; ne_elements; attributes; _} = value in
    let elements = pp_nsepseq ";" printer ne_elements in
    let inj =
      match compound with
        None -> elements
      | Some compound ->
         let (opening, a), (closing, b) = pp_compound compound
         in surround 2 1 (pp_region_t (string opening) a)
                     elements (pp_region_t (string closing) b)
    in
    let inj = if attributes = [] then inj
              else pp_attributes attributes ^^ blank 1 ^^ inj
    in inj

and pp_nsepseq :
  'a.string -> ('a -> document) -> ('a, t) Utils.nsepseq -> document =
  fun sep printer elements ->
    let rec inner result = function
    | (c, item) :: rest -> inner (result ^^ pp_region_t (string sep) c ^^ break 1 ^^ (printer item)) rest
    | [] -> result
    in
    printer (fst elements) ^^ inner empty (snd elements)

and pp_nseq : 'a.('a -> document) -> 'a Utils.nseq -> document =
  fun printer (head, tail) ->
    separate_map (break 1) printer (head::tail)

and pp_projection {value; _} =
  let {struct_name; field_path; _} = value in
  let fields = Utils.nsepseq_to_list field_path
  and sep    = string "." ^^ break 0 in
  let fields = separate_map sep pp_selection fields in
  group (pp_region_reg pp_ident struct_name ^^ string "." ^^ break 0 ^^ fields)

and pp_module_access : type a.(a -> document) -> a module_access reg -> document
= fun f {value; _} ->
  let {module_name; field; _} = value in
  group (pp_region_reg pp_ident module_name ^^ string "." ^^ break 0 ^^ f field)

and pp_selection = function
  FieldName v   -> pp_region_reg (fun v -> string v.value) v
| Component cmp -> pp_region_t (cmp.value |> snd |> Z.to_string |> string) cmp.region

and pp_update {value; _} =
  let {record; updates; _} = value in
  let updates = group (pp_ne_injection pp_field_path_assign updates)
  and record  = pp_path record in
  string "{" ^^ record ^^ string " with"
  ^^ nest 2 (break 1 ^^ updates ^^ string "}")

and pp_code_inj {value; _} =
  let {language; code; rbracket} = value in
  let language = string language.value.value
  and code     = pp_expr code in
  string "[%" ^^ language ^/^ code ^^ pp_region_t (string "]") rbracket

and pp_field_path_assign {value; _} =
  let {field_path; field_expr; assignment} = value in
  let path = pp_path field_path in
  prefix 2 1 (path ^^ pp_region_t (string " =") assignment) (pp_expr field_expr)

and pp_path = function
  Name v -> pp_region_reg pp_ident v
| Path p -> pp_region_reg pp_projection p

and pp_call_expr {value; _} =
  let lambda, arguments = value in
  let arguments = pp_nseq pp_expr arguments in
  prefix 2 1 (pp_expr lambda) arguments

and pp_tuple_expr {value; _} =
  let head, tail = value in
  let rec app = function
    []  -> empty
  | [(_, p)] -> group (break 1 ^^ pp_expr p)
  | (c, p)::items ->
      group (break 1 ^^ pp_expr p ^^ (pp_region_t (string ",") c)) ^^ app items
  in match tail with
    [] -> pp_expr head
  | h :: tail -> pp_expr head ^^ (pp_region_t (string ",") (fst h)) ^^ app (h :: tail)

and pp_par_expr e = pp_par pp_expr e

and pp_let_in {value; _} =
  let {kwd_let; binding; kwd_rec; body; attributes=attr; kwd_in; _} = value in
  let let_str =
    match kwd_rec with
        None -> (pp_region_t (string "let ") kwd_let)
    | Some r -> (pp_region_t (string "let ") kwd_let) ^^ (pp_region_t (string "rec ") r)
  in
  let let_str = if attr = [] then let_str
                else pp_attributes attr ^/^ let_str
  in let_str ^^ pp_let_binding binding
     ^^ (pp_region_t (string " in") kwd_in) ^^ hardline ^^ group (pp_expr body)

and pp_type_in {value; _} =
  let {type_decl; kwd_in; body} = value in
  let {kwd_type; name; type_expr;  _} = type_decl
  in pp_region_t (string "type ") kwd_type
     ^^ prefix 2 1 (pp_region_reg pp_ident name ^^ string " =")
                   (pp_type_expr type_expr)
     ^^ pp_region_t (string " in") kwd_in ^^ hardline ^^ group (pp_expr body)

and pp_mod_in {value; _} =
  let {mod_decl; kwd_in; body} = value in
  let {kwd_module; name; eq; kwd_struct; module_; kwd_end} = mod_decl
  in pp_region_t (string "module") kwd_module
     ^^ prefix 2 1 (pp_region_reg pp_ident name ^^ pp_region_t (string "=") eq ^^ pp_region_t (string "struct") kwd_struct)
                   (print module_)
     ^^ pp_region_t (string " end") kwd_end
     ^^ pp_region_t (string " in") kwd_in ^^ hardline ^^ group (pp_expr body)

and pp_mod_alias {value; _} =
  let {mod_alias; body; kwd_in} = value in
  let {kwd_module; alias; eq; binders} = mod_alias in
  pp_region_t (string "module ") kwd_module ^^ pp_region_reg pp_ident alias ^^ pp_region_t (string " =") eq
  ^^ group (nest 0 (break 1 ^^ pp_nsepseq "." (pp_region_reg pp_ident) binders))
  ^^ pp_region_t (string " in") kwd_in ^^ hardline ^^ group (pp_expr body)

and pp_fun {value; _} =
  let {kwd_fun; binders; lhs_type; body; arrow; _} = value in
  let binders = pp_nseq pp_pattern binders
  and annot   =
    match lhs_type with
      None -> empty
    | Some (c,e) ->
        group (break 1 ^^ pp_region_t (string ": ") c
               ^^ nest 2 (break 1 ^^ pp_type_expr e))
  in group (pp_region_t (string "fun ") kwd_fun ^^ nest 4 binders ^^ annot
     ^^ (pp_region_t (string " ->") arrow) ^^ nest 2 (break 1 ^^ pp_expr body))

and pp_seq {value; _} =
  let {compound; elements; _} = value in
  let rec inner result = function
  | (c, item) :: rest -> inner (result ^^ pp_region_t (string ";") c ^^ hardline ^^ (pp_expr item)) rest
  | [] -> result
  in
  let elements = match elements with
  | Some elements -> pp_expr (fst elements) ^^ inner empty (snd elements)
  | None -> empty
  in
  match compound with
    None -> elements
  | Some compound ->
      let (opening, a), (closing, b) = pp_compound compound
      in (pp_region_t (string opening) a)
         ^^ nest 2 (hardline ^^ elements) ^^ hardline
         ^^ (pp_region_t (string closing) b)

and pp_type_expr = function
  TProd t   -> pp_region_reg pp_cartesian t
| TSum t    -> pp_region_reg pp_sum_type t
| TRecord t -> pp_region_reg pp_record_type t
| TApp t    -> pp_region_reg pp_type_app t
| TFun t    -> pp_region_reg pp_fun_type t
| TPar t    -> pp_region_reg pp_type_par t
| TVar t    -> pp_region_reg pp_ident t
| TString s -> pp_region_reg pp_string s
| TInt i    -> pp_region_reg pp_int i
| TModA   t -> pp_region_reg (pp_module_access pp_type_expr) t
| TArg    t -> pp_region_reg pp_quoted_param t

and pp_quoted_param param =
  let quoted = {param with value = "'" ^ param.value.name.value}
  in pp_region_reg pp_ident quoted

and pp_cartesian {value; _} =
  let head, tail = value in
  let rec app = function
    []  -> empty
  | [(_, p)] -> group (break 1 ^^ pp_type_expr p)
  | (c, p)::items ->
      group (break 1 ^^ pp_type_expr p ^^ (pp_region_t (string " *") c)) ^^ app items
  in match tail with
    [] -> pp_type_expr head
  | h :: tail -> pp_type_expr head ^^ (pp_region_t (string " *") (fst h)) ^^ app (h :: tail)

and pp_sum_type {value; _} =
  let {lead_vbar; variants; attributes; _} = value in
  let rec inner result = function
  | (c, item) :: rest ->
       inner (result ^^ break 1 ^^ pp_region_t (string "| ") c  ^^ (pp_variant item)) rest
  | [] -> result
  in
  let whole =
    (match lead_vbar with
      Some v -> pp_region_t (string "| ") v
    | None -> empty)
    ^^
    pp_variant (fst variants) ^^ inner empty (snd variants)
  in
  let whole = nest 2 (break 0 ^^
  (match lead_vbar with
    Some _ -> whole
  | None -> ifflat whole (blank 2 ^^ whole)
  ))
   in
  if attributes = [] then whole
  else pp_attributes attributes ^^ whole

and pp_variant {value; _} =
  let {constr; arg; attributes=attr} = value in
  let pre = if attr = [] then pp_region_reg pp_ident constr
            else group (pp_attributes attr ^/^ pp_region_reg pp_ident constr) in
  match arg with
    None -> pre
  | Some (kwd_of,e) -> prefix 4 1 (pre ^^ pp_region_t (string " of") kwd_of) (pp_type_expr e)

and pp_record_type fields = group (pp_ne_injection pp_field_decl fields)

and pp_field_decl {value; _} =
  let {field_name; colon; field_type; attributes} = value in
  let attr   = pp_attributes attributes in
  let name   = if attributes = [] then pp_region_reg pp_ident field_name
               else attr ^/^ pp_region_reg pp_ident field_name in
  let t_expr = pp_type_expr field_type
  in prefix 2 1 (name ^^ pp_region_t (string " :") colon) t_expr

and pp_type_app {value = (ctor, type_constr_arg); _} =
  pp_type_constr_arg type_constr_arg
  ^^ pp_region_t (group (nest 2 (break 1 ^^ pp_type_constr ctor)))
                 ctor.region

and pp_type_constr_arg = function
  CArg t -> pp_type_expr t
| CArgTuple t -> pp_constr_arg_tuple t

and pp_constr_arg_tuple {value; _} =
  let head, tail = value.inside in
  let rec app = function
    []  -> empty
  | [(_, e)] -> group (break 1 ^^ pp_type_expr e)
  | (c, e)::items ->
      group (break 1 ^^ pp_type_expr e ^^ pp_region_t (string ",") c) ^^ app items
  in
  match tail with
    []        -> pp_type_expr head
  | h :: tail ->
    let components =
      pp_type_expr head ^^ (pp_region_t (string ",") (fst h)) ^^ app (h :: tail)
    in (pp_region_t (string "(") value.lpar) ^^ nest 1 (components ^^ (pp_region_t (string ")") value.rpar))

and pp_type_constr ctor = string ctor.value

and pp_fun_type {value; _} =
  let lhs, arrow, rhs = value in
  group (pp_type_expr lhs ^^ (pp_region_t (string " ->") arrow) ^/^ pp_type_expr rhs)

and pp_type_par t = pp_par pp_type_expr t

let print_type_expr = pp_type_expr
let print_pattern   = pp_pattern
let print_expr      = pp_expr

type cst        = CST.t
type expr       = CST.expr
type type_expr  = CST.type_expr
type pattern    = CST.pattern
