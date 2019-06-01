[@@@warning "-30-40-42"]

(* Abstract Syntax Tree (AST) for Mini-ML *)

type 'a reg = 'a Region.reg

(* Keywords of OCaml *)

type keyword   = Region.t
type kwd_and   = Region.t
type kwd_begin = Region.t
type kwd_else  = Region.t
type kwd_end   = Region.t
type kwd_false = Region.t
type kwd_fun   = Region.t
type kwd_if    = Region.t
type kwd_in    = Region.t
type kwd_let   = Region.t
type kwd_match = Region.t
type kwd_mod   = Region.t
type kwd_not   = Region.t
type kwd_of    = Region.t
type kwd_or    = Region.t
type kwd_then  = Region.t
type kwd_true  = Region.t
type kwd_type  = Region.t
type kwd_with  = Region.t
type kwd_let_entry = Region.t

(* Symbols *)

type arrow    = Region.t  (* "->" *)
type cons     = Region.t  (* "::" *)
type cat      = Region.t  (* "^"  *)
type append   = Region.t  (* "@"  *)
type dot      = Region.t  (* "."  *)

(* Arithmetic operators *)

type minus    = Region.t  (* "-" *)
type plus     = Region.t  (* "+" *)
type slash    = Region.t  (* "/" *)
type times    = Region.t  (* "*" *)

(* Boolean operators *)

type bool_or  = Region.t  (* "||" *)
type bool_and = Region.t  (* "&&" *)

(* Comparisons *)

type equal = Region.t  (* "="  *)
type neq   = Region.t  (* "<>" *)
type lt    = Region.t  (* "<"  *)
type gt    = Region.t  (* ">"  *)
type leq   = Region.t  (* "=<" *)
type geq   = Region.t  (* ">=" *)

(* Compounds *)

type lpar     = Region.t  (* "(" *)
type rpar     = Region.t  (* ")" *)
type lbracket = Region.t  (* "[" *)
type rbracket = Region.t  (* "]" *)
type lbrace   = Region.t  (* "{" *)
type rbrace   = Region.t  (* "}" *)

(* Separators *)

type comma = Region.t  (* "," *)
type semi  = Region.t  (* ";" *)
type vbar  = Region.t  (* "|" *)
type colon = Region.t  (* ":" *)

(* Wildcard *)

type wild = Region.t  (* "_" *)

(* Virtual tokens *)

type eof = Region.t

(* Literals *)

type variable    = string reg
type fun_name    = string reg
type type_name   = string reg
type field_name  = string reg
type type_constr = string reg
type constr      = string reg

(* Parentheses *)

type 'a par = {
  lpar   : lpar;
  inside : 'a;
  rpar   : rpar
}

type the_unit = lpar * rpar

(* The Abstract Syntax Tree *)

type t = {
  decl : declaration Utils.nseq;
  eof  : eof
}

and ast = t

and declaration =
  Let      of (kwd_let * let_binding) reg
| LetEntry of (kwd_let_entry * let_binding) reg
| TypeDecl of type_decl reg

(* Non-recursive values *)

and let_binding = {
  bindings : pattern list;
  lhs_type : (colon * type_expr) option;
  eq       : equal;
  let_rhs  : expr
}

(* Type declarations *)

and type_decl = {
  kwd_type   : kwd_type;
  name       : type_name;
  eq         : equal;
  type_expr  : type_expr
}

and type_expr =
  TProd   of cartesian
| TSum    of (variant reg, vbar) Utils.nsepseq reg
| TRecord of record_type
| TApp    of (type_constr * type_tuple) reg
| TFun    of (type_expr * arrow  * type_expr) reg
| TPar    of type_expr par reg
| TAlias  of variable

and cartesian = (type_expr, times) Utils.nsepseq reg

and variant = {
  constr : constr;
  args   : (kwd_of * cartesian) option
}

and record_type = field_decl reg injection reg

and field_decl = {
  field_name : field_name;
  colon      : colon;
  field_type : type_expr
}

and type_tuple = (type_expr, comma) Utils.nsepseq par reg

and pattern =
  PTuple  of (pattern, comma) Utils.nsepseq reg
| PList   of list_pattern
| PVar    of variable
| PUnit   of the_unit reg
| PInt    of (string * Z.t) reg
| PTrue   of kwd_true
| PFalse  of kwd_false
| PString of string reg
| PWild   of wild
| PPar    of pattern par reg
| PConstr of (constr * pattern option) reg
| PRecord of record_pattern
| PTyped  of typed_pattern reg

and list_pattern =
  Sugar of pattern injection reg
| PCons of (pattern * cons * pattern) reg

and typed_pattern = {
  pattern   : pattern;
  colon     : colon;
  type_expr : type_expr
}

and record_pattern = field_pattern reg injection reg

and field_pattern = {
  field_name : field_name;
  eq         : equal;
  pattern    : pattern
}

and expr =
  ECase   of expr case reg
| EAnnot  of annot_expr reg
| ELogic  of logic_expr
| EArith  of arith_expr
| EString of string_expr
| EList   of list_expr
| EConstr of constr_expr reg
| ERecord of record_expr
| EProj   of projection reg
| EVar    of variable
| ECall   of (expr * expr Utils.nseq) reg
| EBytes  of (string * Hex.t) reg
| EUnit   of the_unit reg
| ETuple  of (expr, comma) Utils.nsepseq reg
| EPar    of expr par reg
| ELetIn  of let_in reg
| EFun    of fun_expr reg
| ECond   of conditional reg
| ESeq    of sequence

and constr_expr = constr * expr option

and annot_expr = expr * type_expr

and 'a injection = {
  opening    : opening;
  elements   : ('a, semi) Utils.sepseq;
  terminator : semi option;
  closing    : closing
}

and opening =
  Begin    of kwd_begin
| With     of kwd_with
| LBrace   of lbrace
| LBracket of lbracket

and closing =
  End      of kwd_end
| RBrace   of rbrace
| RBracket of rbracket

and list_expr =
  Cons   of cons bin_op reg
| List   of expr injection reg
  (*| Append of (expr * append * expr) reg*)

and string_expr =
  Cat    of cat bin_op reg
| String of string reg

and arith_expr =
  Add  of plus bin_op reg
| Sub  of minus bin_op reg
| Mult of times bin_op reg
| Div  of slash bin_op reg
| Mod  of kwd_mod bin_op reg
| Neg  of minus un_op reg
| Int  of (string * Z.t) reg
| Nat  of (string * Z.t) reg
| Mtz  of (string * Z.t) reg

and logic_expr =
  BoolExpr of bool_expr
| CompExpr of comp_expr

and bool_expr =
  Or       of kwd_or bin_op reg
| And      of kwd_and bin_op reg
| Not      of kwd_not un_op reg
| True     of kwd_true
| False    of kwd_false

and 'a bin_op = {
  op   : 'a;
  arg1 : expr;
  arg2 : expr
}

and 'a un_op = {
  op  : 'a;
  arg : expr
}

and comp_expr =
  Lt    of lt    bin_op reg
| Leq   of leq   bin_op reg
| Gt    of gt    bin_op reg
| Geq   of geq   bin_op reg
| Equal of equal bin_op reg
| Neq   of neq   bin_op reg

and projection = {
  struct_name : variable;
  selector    : dot;
  field_path  : (selection, dot) Utils.nsepseq
}

and selection =
  FieldName of variable
| Component of (string * Z.t) reg par reg

and record_expr = field_assign reg injection reg

and field_assign = {
  field_name : field_name;
  assignment : equal;
  field_expr : expr
}

and sequence = expr injection reg

and 'a case = {
  kwd_match : kwd_match;
  expr      : expr;
  opening   : opening;
  lead_vbar : vbar option;
  cases     : ('a case_clause reg, vbar) Utils.nsepseq reg;
  closing   : closing
}

and 'a case_clause = {
  pattern : pattern;
  arrow   : arrow;
  rhs     : 'a
}

and let_in = {
  kwd_let : kwd_let;
  binding : let_binding;
  kwd_in  : kwd_in;
  body    : expr
}

and fun_expr = {
  kwd_fun : kwd_fun;
  params  : pattern list;
  p_annot : (colon * type_expr) option;
  arrow   : arrow;
  body    : expr
}

and conditional = {
  kwd_if   : kwd_if;
  test     : expr;
  kwd_then : kwd_then;
  ifso     : expr;
  kwd_else : kwd_else;
  ifnot    : expr
}

(* Projecting regions of the input source code *)

let sprintf = Printf.sprintf

let region_of_type_expr = function
  TProd {region; _}
| TSum {region; _}
| TRecord {region; _}
| TApp {region; _}
| TFun {region; _}
| TPar {region; _}
| TAlias {region; _} -> region


let region_of_list_pattern = function
  Sugar {region; _} | PCons {region; _} -> region

let region_of_pattern = function
  PList p -> region_of_list_pattern p
| PTuple {region;_} | PVar {region;_}
| PUnit {region;_} | PInt {region;_}
| PTrue region | PFalse region
| PString {region;_} | PWild region
| PConstr {region; _} | PPar {region;_}
| PRecord {region; _} | PTyped {region; _} -> region

let region_of_bool_expr = function
  Or {region;_} | And {region;_}
| True region | False region
| Not {region;_} -> region

let region_of_comp_expr = function
  Lt {region;_} | Leq {region;_}
| Gt {region;_} | Geq {region;_}
| Neq {region;_} | Equal {region;_} -> region

let region_of_logic_expr = function
  BoolExpr e -> region_of_bool_expr e
| CompExpr e -> region_of_comp_expr e

let region_of_arith_expr = function
  Add {region;_} | Sub {region;_} | Mult {region;_}
| Div {region;_} | Mod {region;_} | Neg {region;_}
| Int {region;_} | Mtz {region; _}
| Nat {region; _} -> region

let region_of_string_expr = function
  String {region;_} | Cat {region;_} -> region

let region_of_list_expr = function
  Cons {region; _} | List {region; _}
(* | Append {region; _}*) -> region

let region_of_expr = function
  ELogic e -> region_of_logic_expr e
| EArith e -> region_of_arith_expr e
| EString e -> region_of_string_expr e
| EList e -> region_of_list_expr e
| EAnnot {region;_ } | ELetIn {region;_} | EFun {region;_}
| ECond {region;_} | ETuple {region;_} | ECase {region;_}
| ECall {region;_} | EVar {region; _} | EProj {region; _}
| EUnit {region;_} | EPar {region;_} | EBytes {region; _}
| ESeq {region; _} | ERecord {region; _}
| EConstr {region; _} -> region

(* Printing the tokens with their source locations *)

let print_nsepseq sep print (head,tail) =
  let print_aux ((sep_reg:Region.t), item) =
    Printf.printf "%s: %s\n" (sep_reg#compact `Byte) sep;
    print item
  in print head; List.iter print_aux tail

let print_sepseq sep print = function
      None -> ()
| Some seq -> print_nsepseq sep print seq

let print_csv print = print_nsepseq "," print

let print_token (reg: Region.t) conc =
  Printf.printf "%s: %s\n" (reg#compact `Byte) conc

let print_var Region.{region; value} =
  Printf.printf "%s: Ident %s\n" (region#compact `Byte) value

let print_uident Region.{region; value} =
  Printf.printf "%s: Uident %s\n" (region#compact `Byte) value

let print_str Region.{region; value} =
  Printf.printf "%s: Str \"%s\"\n" (region#compact `Byte) value

let print_bytes Region.{region; value=lexeme, abstract} =
  Printf.printf "%s: Bytes (\"%s\", \"0x%s\")\n"
    (region#compact `Byte) lexeme (Hex.to_string abstract)

let rec print_tokens {decl;eof} =
  Utils.nseq_iter print_statement decl; print_token eof "EOF"

and print_statement = function
  Let {value=kwd_let, let_binding; _} ->
    print_token kwd_let "let";
    print_let_binding let_binding
| LetEntry {value=kwd_let_entry, let_binding; _} ->
    print_token kwd_let_entry "let%entry";
    print_let_binding let_binding
| TypeDecl {value={kwd_type; name; eq; type_expr}; _} ->
    print_token kwd_type "type";
    print_var name;
    print_token eq "=";
    print_type_expr type_expr

and print_type_expr = function
  TProd prod       -> print_cartesian prod
| TSum {value; _}  -> print_nsepseq "|" print_variant value
| TRecord t        -> print_record_type t
| TApp app         -> print_type_app app
| TPar par         -> print_type_par par
| TAlias var       -> print_var var
| TFun t           -> print_fun_type t

and print_fun_type {value; _} =
  let domain, arrow, range = value in
  print_type_expr domain;
  print_token arrow "->";
  print_type_expr range

and print_type_app {value; _} =
  let type_constr, type_tuple = value in
  print_type_tuple type_tuple;
  print_var type_constr

and print_type_tuple {value; _} =
  let {lpar; inside; rpar} = value in
  print_token lpar "(";
  print_nsepseq "," print_type_expr inside;
  print_token rpar ")"

and print_type_par {value={lpar;inside=t;rpar}; _} =
  print_token lpar "(";
  print_type_expr t;
  print_token rpar ")"

and print_projection node =
  let {struct_name; selector; field_path} = node in
  print_var struct_name;
  print_token selector ".";
  print_nsepseq "." print_selection field_path

and print_selection = function
  FieldName id -> print_var id
| Component {value; _} ->
    let {lpar; inside; rpar} = value in
    let Region.{value=lexeme,z; region} = inside in
    print_token lpar "(";
    print_token region
      (sprintf "Int %s (%s)" lexeme (Z.to_string z));
    print_token rpar ")"

and print_cartesian Region.{value;_} =
  print_nsepseq "*" print_type_expr value

and print_variant {value = {constr; args}; _} =
  print_uident constr;
  match args with
    None -> ()
  | Some (kwd_of, cartesian) ->
      print_token kwd_of "of";
      print_cartesian cartesian

and print_record_type record_type =
  print_injection print_field_decl record_type

and print_field_decl {value; _} =
  let {field_name; colon; field_type} = value
  in print_var field_name;
     print_token colon ":";
     print_type_expr field_type

and print_injection :
  'a.('a -> unit) -> 'a injection reg -> unit =
  fun print {value; _} ->
    let {opening; elements; terminator; closing} = value in
    print_opening opening;
    print_sepseq ";" print elements;
    print_terminator terminator;
    print_closing closing

and print_opening = function
  Begin    region -> print_token region "begin"
| With     region -> print_token region "with"
| LBrace   region -> print_token region "{"
| LBracket region -> print_token region "["

and print_closing = function
  End      region -> print_token region "end"
| RBrace   region -> print_token region "}"
| RBracket region -> print_token region "]"

and print_terminator = function
  Some semi -> print_token semi ";"
| None -> ()

and print_let_binding {bindings; lhs_type; eq; let_rhs} =
  List.iter print_pattern bindings;
  (match lhs_type with
     None -> ()
   | Some (colon, type_expr) ->
       print_token colon ":";
       print_type_expr type_expr);
  (print_token eq "="; print_expr let_rhs)

and print_pattern = function
  PTuple {value=patterns;_} -> print_csv print_pattern patterns
| PList p -> print_list_pattern p
| PVar {region; value} ->
    Printf.printf "%s: PVar %s\n" (region#compact `Byte) value
| PUnit {value=lpar,rpar; _} ->
    print_token lpar "("; print_token rpar ")"
| PInt {region; value=lex,z} ->
    print_token region (sprintf "PInt %s (%s)" lex (Z.to_string z))
| PTrue kwd_true -> print_token kwd_true "true"
| PFalse kwd_false -> print_token kwd_false "false"
| PString s -> print_str s
| PWild wild -> print_token wild "_"
| PPar {value={lpar;inside=p;rpar}; _} ->
    print_token lpar "("; print_pattern p; print_token rpar ")"
| PConstr p -> print_constr_pattern p
| PRecord r -> print_record_pattern r
| PTyped t -> print_typed_pattern t

and print_list_pattern = function
  Sugar p -> print_injection print_pattern p
| PCons p -> print_raw p

and print_raw {value=p1,c,p2; _} =
  print_pattern p1; print_token c "::"; print_pattern p2

and print_typed_pattern {value; _} =
  let {pattern; colon; type_expr} = value in
  print_pattern pattern;
  print_token colon ":";
  print_type_expr type_expr

and print_record_pattern record_pattern =
  print_injection print_field_pattern record_pattern

and print_field_pattern {value; _} =
  let {field_name; eq; pattern} = value in
  print_var field_name;
  print_token eq "=";
  print_pattern pattern

and print_constr_pattern {value=constr, p_opt; _} =
  print_uident constr;
  match p_opt with
    None -> ()
  | Some pattern -> print_pattern pattern

and print_expr = function
  ELetIn {value;_} -> print_let_in value
|       ECond cond -> print_conditional cond
| ETuple {value;_} -> print_csv print_expr value
| ECase {value;_}  -> print_match_expr value
| EFun e           -> print_fun_expr e

| EAnnot e -> print_annot_expr e
| ELogic e -> print_logic_expr e
| EArith e -> print_arith_expr e
| EString e -> print_string_expr e

| ECall {value=f,l; _} ->
    print_expr f; Utils.nseq_iter print_expr l
| EVar v -> print_var v
| EProj p -> print_projection p.value
| EUnit {value=lpar,rpar; _} ->
    print_token lpar "("; print_token rpar ")"
| EBytes b -> print_bytes b
| EPar {value={lpar;inside=e;rpar}; _} ->
    print_token lpar "("; print_expr e; print_token rpar ")"
| EList e -> print_list_expr e
| ESeq seq -> print_sequence seq
| ERecord e -> print_record_expr e
| EConstr {value=constr,None; _} -> print_uident constr
| EConstr {value=(constr, Some arg); _} ->
    print_uident constr; print_expr arg

and print_annot_expr {value=e,t; _} =
  print_expr e;
  print_token Region.ghost ":";
  print_type_expr t

and print_list_expr = function
  Cons {value={arg1;op;arg2}; _} ->
    print_expr arg1;
    print_token op "::";
    print_expr arg2
| List e -> print_injection print_expr e
(*| Append {value=e1,append,e2; _} ->
    print_expr e1;
    print_token append "@";
    print_expr e2 *)

and print_arith_expr = function
  Add {value={arg1;op;arg2}; _} ->
    print_expr arg1; print_token op "+"; print_expr arg2
| Sub {value={arg1;op;arg2}; _} ->
    print_expr arg1; print_token op "-"; print_expr arg2
| Mult {value={arg1;op;arg2}; _} ->
    print_expr arg1; print_token op "*"; print_expr arg2
| Div {value={arg1;op;arg2}; _} ->
    print_expr arg1; print_token op "/"; print_expr arg2
| Mod {value={arg1;op;arg2}; _} ->
    print_expr arg1; print_token op "mod"; print_expr arg2
| Neg {value={op;arg}; _} -> print_token op "-"; print_expr arg
| Int {region; value=lex,z} ->
    print_token region (sprintf "Int %s (%s)" lex (Z.to_string z))
| Mtz {region; value=lex,z} ->
    print_token region (sprintf "Mtz %s (%s)" lex (Z.to_string z))
| Nat {region; value=lex,z} ->
    print_token region (sprintf "Nat %s (%s)" lex (Z.to_string z))

and print_string_expr = function
  Cat {value={arg1;op;arg2}; _} ->
    print_expr arg1; print_token op "^"; print_expr arg2
| String s -> print_str s

and print_logic_expr = function
  BoolExpr e -> print_bool_expr e
| CompExpr e -> print_comp_expr e

and print_bool_expr = function
  Or {value={arg1;op;arg2}; _} ->
    print_expr arg1; print_token op "||"; print_expr arg2
| And {value={arg1;op;arg2}; _} ->
    print_expr arg1; print_token op "&&"; print_expr arg2
| Not {value={op;arg}; _} -> print_token op "not"; print_expr arg
| True kwd_true -> print_token kwd_true "true"
| False kwd_false -> print_token kwd_false "false"

and print_comp_expr = function
  Lt {value={arg1;op;arg2}; _} ->
    print_expr arg1; print_token op "<"; print_expr arg2
| Leq {value={arg1;op;arg2}; _} ->
    print_expr arg1; print_token op "<="; print_expr arg2
| Gt {value={arg1;op;arg2}; _} ->
    print_expr arg1; print_token op ">"; print_expr arg2
| Geq {value={arg1;op;arg2}; _} ->
    print_expr arg1; print_token op ">="; print_expr arg2
| Neq {value={arg1;op;arg2}; _} ->
    print_expr arg1; print_token op "<>"; print_expr arg2
| Equal {value={arg1;op;arg2}; _} ->
    print_expr arg1; print_token op "="; print_expr arg2

and print_record_expr e =
  print_injection print_field_assign e

and print_field_assign {value; _} =
  let {field_name; assignment; field_expr} = value in
  print_var field_name;
  print_token assignment "=";
  print_expr field_expr

and print_sequence seq = print_injection print_expr seq

and print_match_expr expr =
  let {kwd_match; expr; opening;
       lead_vbar; cases; closing} = expr in
  print_token kwd_match "match";
  print_expr expr;
  print_opening opening;
  print_token_opt lead_vbar "|";
  print_cases cases;
  print_closing closing

and print_token_opt = function
         None -> fun _ -> ()
| Some region -> print_token region

and print_cases {value; _} =
  print_nsepseq "|" print_case_clause value

and print_case_clause {value; _} =
  let {pattern; arrow; rhs} = value in
  print_pattern pattern;
  print_token arrow "->";
  print_expr rhs

and print_let_in (bind: let_in) =
  let {kwd_let; binding; kwd_in; body} = bind in
  print_token kwd_let "let";
  print_let_binding binding;
  print_token kwd_in "in";
  print_expr body

and print_fun_expr {value; _} =
  let {kwd_fun; params; p_annot; arrow; body} = value in
  print_token kwd_fun "fun";
  (match p_annot with
     None -> List.iter print_pattern params
   | Some (colon, type_expr) ->
      print_token colon ":";
      print_type_expr type_expr);
  print_token arrow "->";
  print_expr body

and print_conditional {value; _} =
   let open Region in
   let {kwd_if; test; kwd_then; ifso; kwd_else; ifnot} = value
   in print_token ghost "(";
   print_token kwd_if "if";
   print_expr test;
   print_token kwd_then "then";
   print_expr ifso;
   print_token kwd_else "else";
   print_expr ifnot;
   print_token ghost ")"

let rec unpar = function
  EPar {value={inside=expr;_}; _} -> unpar expr
| e -> e
