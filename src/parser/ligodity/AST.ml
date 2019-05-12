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

(* Brackets compounds *)

type 'a brackets = {
  lbracket   : lbracket;
  inside : 'a;
  rbracket   : rbracket
}

(* The Abstract Syntax Tree *)

type t = {
  decl : declaration Utils.nseq;
  eof  : eof
}

and ast = t

and eof = Region.t

and declaration =
  Let      of (kwd_let * let_bindings) reg
| LetEntry of (kwd_let_entry * let_binding) reg
| TypeDecl of type_decl reg

(* Non-recursive values *)

and let_bindings = (let_binding, kwd_and) Utils.nsepseq

and let_binding = {
  pattern  : pattern;
  lhs_type : (colon * type_expr) option;
  eq       : equal;
  let_rhs  : expr
}

(* Recursive types *)

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

and type_tuple = (type_expr, comma) Utils.nsepseq par

and 'a injection = {
  opening    : opening;
  elements   : ('a, semi) Utils.sepseq;
  terminator : semi option;
  closing    : closing
}

and opening =
  Begin  of kwd_begin
| LBrace of lbrace

and closing =
  End    of kwd_end
| RBrace of rbrace

and pattern =
  PTuple  of (pattern, comma) Utils.nsepseq reg
| PList   of (pattern, semi) Utils.sepseq brackets reg
| PVar    of variable
| PUnit   of the_unit reg
| PInt    of (string * Z.t) reg
| PTrue   of kwd_true
| PFalse  of kwd_false
| PString of string reg
| PWild   of wild
| PCons   of (pattern * cons * pattern) reg
| PPar    of pattern par reg
| PConstr of (constr * pattern option) reg
| PRecord of record_pattern
| PTyped  of typed_pattern reg

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
  LetIn    of let_in reg
| Fun      of fun_expr
| If       of conditional
| ETuple   of (expr, comma) Utils.nsepseq reg
| Match    of match_expr reg
| Seq      of sequence
| ERecord  of record_expr
| Append   of (expr * append * expr) reg
| Cons     of (expr * cons * expr) reg

| ELogic   of logic_expr
| EArith   of arith_expr
| EString  of string_expr

| Call     of (expr * expr) reg

| Path     of path reg
| Unit     of the_unit reg
| Par      of expr par reg
| EList     of (expr, semi) Utils.sepseq brackets reg
| EConstr  of constr

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

and path = {
  module_proj : (constr * dot) option;
  value_proj  : (selection, dot) Utils.nsepseq
}

and selection =
  Name      of variable
| Component of (string * Z.t) reg par reg

and record_expr = field_assignment reg injection reg

and field_assignment = {
  field_name : field_name;
  assignment : equal;
  field_expr : expr
}

and sequence = expr injection reg

and match_expr = kwd_match * expr * kwd_with * cases

and cases =
  vbar option * (pattern * arrow * expr, vbar) Utils.nsepseq

and let_in = kwd_let * let_bindings * kwd_in * expr

and fun_expr = (kwd_fun * variable * arrow * expr) reg

and conditional =
  IfThen     of (kwd_if * expr * kwd_then * expr) reg
| IfThenElse of (kwd_if * expr * kwd_then * expr * kwd_else * expr) reg

(* Projecting regions of the input source code *)

let sprintf = Printf.sprintf

let region_of_pattern = function
  PList {region;_} | PTuple {region;_} | PVar {region;_}
| PUnit {region;_} | PInt {region;_} | PTrue region | PFalse region
| PString {region;_} | PWild region | PCons {region;_}
| PConstr {region; _} | PPar {region;_} | PRecord {region; _}
| PTyped {region; _} -> region

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

let region_of_expr = function
  ELogic e -> region_of_logic_expr e
| EArith e -> region_of_arith_expr e
| EString e -> region_of_string_expr e
| LetIn {region;_} | Fun {region;_}
| If IfThen {region;_} | If IfThenElse {region; _}
| ETuple {region;_} | Match {region;_} | Cons {region;_}
| Call {region;_} | Path {region;_}
| Unit {region;_} | Par {region;_} | EList {region;_}
| Seq {region; _} | ERecord {region; _}
| Append {region; _} | EConstr {region; _} -> region


(* Predicates *)

let rec is_var = function
  Par {value={inside=e;_};_} -> is_var e
|           Path _ -> true
|                   _ -> false

let rec is_call = function
  Par {value={inside=e;_};_} -> is_call e
|              Call _ -> true
|                   _ -> false

let rec is_fun = function
  Par {value={inside=e;_};_} -> is_fun e
|               Fun _ -> true
|                   _ -> false

let rec rm_par = function
  Par {value={inside=e;_};_} -> rm_par e
|                   e -> e

(* Rewriting let-expressions and fun-expressions, with some optimisations *)

type sep = Region.t

let ghost_fun, ghost_arrow, ghost_let, ghost_eq, ghost_in =
  let ghost = Region.ghost in ghost, ghost, ghost, ghost, ghost

let norm_fun region kwd_fun pattern eq expr =
  let value =
    match pattern with
      PVar v -> kwd_fun, v, eq, expr
    |      _ -> let value     = Utils.gen_sym () in
                let fresh    = Region.{region=Region.ghost; value} in
                let proj     = Name fresh, [] in
                let path     = {module_proj=None; value_proj=proj} in
                let path     = Region.{region=Region.ghost; value=path} in
                let bindings = {pattern; eq;
                                lhs_type=None; let_rhs = Path path}, [] in
                let let_in   = ghost_let, bindings, ghost_in, expr in
                let expr     = LetIn {value=let_in; region=Region.ghost}
    in kwd_fun, fresh, ghost_arrow, expr
  in Region.{region; value}

let norm ?reg (pattern, patterns) sep expr =
  let reg, fun_reg =
    match reg with
        None -> Region.ghost, ghost_fun
    | Some p -> p in
  let apply pattern (sep, expr) =
    ghost_eq, Fun (norm_fun Region.ghost ghost_fun pattern sep expr) in
  let sep, expr = List.fold_right apply patterns (sep, expr)
  in norm_fun reg fun_reg pattern sep expr

(* Unparsing expressions *)

type unparsed = [
  `Fun  of (kwd_fun * (pattern Utils.nseq * arrow * expr))
| `Let  of (pattern Utils.nseq * equal * expr)
| `Idem of expr
]

(* The function [unparse'] returns a triple [patterns,
   separator_region, expression], and the context (handled by
   [unparse]) decides if [separator_region] is the region of a "="
   sign or "->". *)

let rec unparse' = function
  Fun {value=_,var,arrow,expr; _} ->
    if var.region#is_ghost then
      match expr with
        LetIn {value = _,({pattern;eq;_},[]),_,expr; _} ->
          if eq#is_ghost then
            let patterns, sep, e = unparse' expr
            in Utils.nseq_cons pattern patterns, sep, e
          else (pattern,[]), eq, expr
      | _ -> assert false
    else if arrow#is_ghost then
           let patterns, sep, e = unparse' expr
           in Utils.nseq_cons (PVar var) patterns, sep, e
         else (PVar var, []), arrow, expr
| _ -> assert false

let unparse = function
  Fun {value=kwd_fun,_,_,_; _} as e ->
    let binding = unparse' e in
    if kwd_fun#is_ghost then `Let binding else `Fun (kwd_fun, binding)
| e -> `Idem e

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
let print_bsv print = print_nsepseq "|" print
let print_ssv print = print_sepseq  ";" print

let print_token (reg: Region.t) conc =
  Printf.printf "%s: %s\n" (reg#compact `Byte) conc

let print_var Region.{region; value} =
  Printf.printf "%s: Ident %s\n" (region#compact `Byte) value

let print_uident Region.{region; value} =
  Printf.printf "%s: Uident %s\n" (region#compact `Byte) value

let print_str Region.{region; value} =
  Printf.printf "%s: Str \"%s\"\n" (region#compact `Byte) value

let rec print_tokens ?(undo=false) {decl;eof} =
  Utils.nseq_iter (print_statement undo) decl; print_token eof "EOF"

and print_statement undo = function
  Let {value=kwd_let, let_bindings; _} ->
    print_token kwd_let "let";
    print_let_bindings undo let_bindings
| LetEntry {value=kwd_let_entry, let_binding; _} ->
    print_token kwd_let_entry "let%entry";
    print_let_binding undo let_binding
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

and print_type_tuple {lpar; inside; rpar} =
  print_token lpar "(";
  print_nsepseq "," print_type_expr inside;
  print_token rpar ")"

and print_type_par {value={lpar;inside=t;rpar}; _} =
  print_token lpar "(";
  print_type_expr t;
  print_token rpar ")"

and print_path Region.{value; _} =
  let {module_proj; value_proj} = value in
  let () =
    match module_proj with
      None -> ()
    | Some (name, dot) ->
        print_uident name;
        print_token dot "."
  in print_nsepseq "." print_selection value_proj

and print_selection = function
  Name id -> print_var id
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
  Begin region  -> print_token region "begin"
| LBrace region -> print_token region "{"

and print_closing = function
  End region    -> print_token region "end"
| RBrace region -> print_token region "}"

and print_terminator = function
  Some semi -> print_token semi ";"
| None -> ()

and print_let_bindings undo = print_nsepseq "and" (print_let_binding undo)

and print_let_binding undo {pattern; lhs_type; eq; let_rhs} =
  print_pattern pattern;
  (match lhs_type with
     None -> ()
   | Some (colon, type_expr) ->
       print_token colon ":";
       print_type_expr type_expr);
  if undo then
    match unparse let_rhs with
      `Let (patterns, eq, e) ->
         Utils.nseq_iter print_pattern patterns;
         print_token eq "=";
         print_expr undo e
    | `Fun (kwd_fun, (patterns, arrow, e)) ->
         print_token eq "=";
         print_token kwd_fun "fun";
         Utils.nseq_iter print_pattern patterns;
         print_token arrow "->";
         print_expr undo e
    | `Idem _ ->
         print_token eq "="; print_expr undo let_rhs
  else (print_token eq "="; print_expr undo let_rhs)

and print_pattern = function
  PTuple {value=patterns;_} -> print_csv print_pattern patterns
| PList {value={lbracket; inside=patterns; rbracket}; _} ->
    print_token lbracket "[";
    print_ssv print_pattern patterns;
    print_token rbracket "]"
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
| PCons {value=p1,c,p2; _} ->
    print_pattern p1; print_token c "::"; print_pattern p2
| PPar {value={lpar;inside=p;rpar}; _} ->
    print_token lpar "("; print_pattern p; print_token rpar ")"
| PConstr p -> print_constr_pattern p
| PRecord r -> print_record_pattern r
| PTyped t -> print_typed_pattern t

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

and print_expr undo = function
  LetIn {value;_} -> print_let_in undo value
|           If cond -> print_conditional undo cond
|  ETuple {value;_} -> print_csv (print_expr undo) value
|   Match {value;_} -> print_match_expr undo value
| Fun {value=(kwd_fun,_,_,_) as f; _} as e ->
    if undo then
      let patterns, arrow, expr = unparse' e in
      print_token kwd_fun "fun";
      Utils.nseq_iter print_pattern patterns;
      print_token arrow "->";
      print_expr undo expr
    else print_fun_expr undo f

| Cons {value=e1,cons,e2; _} ->
    print_expr undo e1; print_token cons "::"; print_expr undo e2
| ELogic e -> print_logic_expr undo e
| EArith e -> print_arith_expr undo e
| EString e -> print_string_expr undo e

| Call {value=e1,e2; _} -> print_expr undo e1; print_expr undo e2
| Path p -> print_path p
| Unit {value=lpar,rpar; _} ->
    print_token lpar "("; print_token rpar ")"
| Par {value={lpar;inside=e;rpar}; _} ->
    print_token lpar "("; print_expr undo e; print_token rpar ")"
| EList {value={lbracket; inside=ssv; rbracket}; _} ->
    print_token lbracket "["; print_ssv (print_expr undo) ssv; print_token rbracket "]"
| Seq seq -> print_sequence undo seq
| ERecord e -> print_record_expr undo e
| Append {value=e1,append,e2; _} ->
    print_expr undo e1; print_token append "@"; print_expr undo e2
| EConstr constr -> print_uident constr

and print_arith_expr undo = function
  Add {value={arg1;op;arg2}; _} ->
    print_expr undo arg1; print_token op "+"; print_expr undo arg2
| Sub {value={arg1;op;arg2}; _} ->
    print_expr undo arg1; print_token op "-"; print_expr undo arg2
| Mult {value={arg1;op;arg2}; _} ->
    print_expr undo arg1; print_token op "*"; print_expr undo arg2
| Div {value={arg1;op;arg2}; _} ->
    print_expr undo arg1; print_token op "/"; print_expr undo arg2
| Mod {value={arg1;op;arg2}; _} ->
    print_expr undo arg1; print_token op "mod"; print_expr undo arg2
| Neg {value={op;arg}; _} -> print_token op "-"; print_expr undo arg
| Int {region; value=lex,z} ->
    print_token region (sprintf "Int %s (%s)" lex (Z.to_string z))
| Mtz {region; value=lex,z} ->
    print_token region (sprintf "Mtz %s (%s)" lex (Z.to_string z))
| Nat {region; value=lex,z} ->
    print_token region (sprintf "Nat %s (%s)" lex (Z.to_string z))

and print_string_expr undo = function
  Cat {value={arg1;op;arg2}; _} ->
    print_expr undo arg1; print_token op "^"; print_expr undo arg2
| String s -> print_str s

and print_logic_expr undo = function
  BoolExpr e -> print_bool_expr undo e
| CompExpr e -> print_comp_expr undo e

and print_bool_expr undo = function
  Or {value={arg1;op;arg2}; _} ->
    print_expr undo arg1; print_token op "||"; print_expr undo arg2
| And {value={arg1;op;arg2}; _} ->
    print_expr undo arg1; print_token op "&&"; print_expr undo arg2
| Not {value={op;arg}; _} -> print_token op "not"; print_expr undo arg
| True kwd_true -> print_token kwd_true "true"
| False kwd_false -> print_token kwd_false "false"

and print_comp_expr undo = function
  Lt {value={arg1;op;arg2}; _} ->
    print_expr undo arg1; print_token op "<"; print_expr undo arg2
| Leq {value={arg1;op;arg2}; _} ->
    print_expr undo arg1; print_token op "<="; print_expr undo arg2
| Gt {value={arg1;op;arg2}; _} ->
    print_expr undo arg1; print_token op ">"; print_expr undo arg2
| Geq {value={arg1;op;arg2}; _} ->
    print_expr undo arg1; print_token op ">="; print_expr undo arg2
| Neq {value={arg1;op;arg2}; _} ->
    print_expr undo arg1; print_token op "<>"; print_expr undo arg2
| Equal {value={arg1;op;arg2}; _} ->
    print_expr undo arg1; print_token op "="; print_expr undo arg2

and print_record_expr undo e =
  print_injection (print_field_assignment undo) e

and print_field_assignment undo {value; _} =
  let {field_name; assignment; field_expr} = value in
  print_var field_name;
  print_token assignment "=";
  print_expr undo field_expr

and print_sequence undo seq = print_injection (print_expr undo) seq

and print_match_expr undo (kwd_match, expr, kwd_with, (_,cases)) =
  print_token kwd_match "match";
  print_expr undo expr;
  print_token kwd_with "with";
  print_bsv (print_case undo) cases;
  print_token Region.ghost "end"

and print_case undo (pattern, arrow, expr) =
  print_pattern pattern;
  print_token arrow "->";
  print_expr undo expr

and print_let_in undo (kwd_let, let_bindings, kwd_in, expr) =
  print_token kwd_let "let";
  print_let_bindings undo let_bindings;
  print_token kwd_in "in";
  print_expr undo expr

and print_fun_expr undo (kwd_fun, rvar, arrow, expr) =
  print_token kwd_fun "fun";
  print_var rvar;
  print_token arrow "->";
  print_expr undo expr

and print_conditional undo = function
  IfThenElse Region.{value=kwd_if, e1, kwd_then, e2, kwd_else, e3; _} ->
    print_token Region.ghost "(";
    print_token kwd_if "if";
    print_expr undo e1;
    print_token kwd_then "then";
    print_expr undo e2;
    print_token kwd_else "else";
    print_expr undo e3;
    print_token Region.ghost ")"
| IfThen Region.{value=kwd_if, e1, kwd_then, e2; _} ->
    print_token Region.ghost "(";
    print_token kwd_if "if";
    print_expr undo e1;
    print_token kwd_then "then";
    print_expr undo e2;
    print_token Region.ghost ")"
