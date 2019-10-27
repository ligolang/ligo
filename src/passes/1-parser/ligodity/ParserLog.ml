[@@@warning "-42"]

open AST
open! Region

(* Printing the tokens with their source locations *)

let sprintf = Printf.sprintf

let offsets = ref true
let mode = ref `Point

let compact (region: Region.t) =
  region#compact ~offsets:!offsets !mode

let print_nsepseq buffer sep print (head,tail) =
  let print_aux ((sep_reg:Region.t), item) =
    let sep_line = sprintf "%s: %s\n" (compact sep_reg) sep
    in Buffer.add_string buffer sep_line;
    print buffer item
  in print buffer head; List.iter print_aux tail

let print_sepseq buffer sep print = function
      None -> ()
| Some seq -> print_nsepseq buffer sep print seq

let print_csv buffer print = print_nsepseq buffer "," print

let print_token buffer (reg: Region.t) conc =
  let line = sprintf "%s: %s\n" (compact reg) conc
  in Buffer.add_string buffer line

let print_var buffer Region.{region; value} =
  let line = sprintf "%s: Ident %s\n" (compact region) value
  in Buffer.add_string buffer line

let print_pvar buffer Region.{region; value} =
  let line = sprintf "%s: PVar %s\n" (compact region) value
  in Buffer.add_string buffer line

let print_uident buffer Region.{region; value} =
  let line = sprintf "%s: Uident %s\n" (compact region) value
  in Buffer.add_string buffer line

let print_str buffer Region.{region; value} =
  let line = sprintf "%s: Str \"%s\"\n" (compact region) value
  in Buffer.add_string buffer line

let print_bytes buffer Region.{region; value=lexeme, abstract} =
  let line = sprintf "%s: Bytes (\"%s\", \"0x%s\")\n"
               (compact region) lexeme (Hex.to_string abstract)
  in Buffer.add_string buffer line

let print_int buffer Region.{region; value=lex,z} =
  let line = sprintf "PInt %s (%s)" lex (Z.to_string z)
  in print_token buffer region line

let rec print_tokens buffer {decl;eof} =
  Utils.nseq_iter (print_statement buffer) decl;
  print_token buffer eof "EOF"

and print_statement buffer = function
  Let {value=kwd_let, let_binding; _} ->
    print_token       buffer kwd_let "let";
    print_let_binding buffer let_binding
| LetEntry {value=kwd_let_entry, let_binding; _} ->
    print_token       buffer kwd_let_entry "let%entry";
    print_let_binding buffer let_binding
| TypeDecl {value={kwd_type; name; eq; type_expr}; _} ->
    print_token     buffer kwd_type "type";
    print_var       buffer name;
    print_token     buffer eq "=";
    print_type_expr buffer type_expr

and print_type_expr buffer = function
  TProd prod       -> print_cartesian buffer prod
| TSum {value; _}  -> print_nsepseq buffer "|" print_variant value
| TRecord t        -> print_record_type buffer t
| TApp app         -> print_type_app buffer app
| TPar par         -> print_type_par buffer par
| TAlias var       -> print_var buffer var
| TFun t           -> print_fun_type buffer t

and print_fun_type buffer {value; _} =
  let domain, arrow, range = value in
  print_type_expr buffer domain;
  print_token     buffer arrow "->";
  print_type_expr buffer range

and print_type_app buffer {value; _} =
  let type_constr, type_tuple = value in
  print_type_tuple buffer type_tuple;
  print_var        buffer type_constr

and print_type_tuple buffer {value; _} =
  let {lpar; inside; rpar} = value in
  print_token   buffer lpar "(";
  print_nsepseq buffer "," print_type_expr inside;
  print_token   buffer rpar ")"

and print_type_par buffer {value={lpar;inside=t;rpar}; _} =
  print_token     buffer lpar "(";
  print_type_expr buffer t;
  print_token     buffer rpar ")"

and print_projection buffer node =
  let {struct_name; selector; field_path} = node in
  print_var     buffer struct_name;
  print_token   buffer selector ".";
  print_nsepseq buffer "." print_selection field_path

and print_selection buffer = function
  FieldName id ->
    print_var buffer id
| Component {value; _} ->
    let {lpar; inside; rpar} = value in
    let Region.{value=lexeme,z; region} = inside in
    print_token buffer lpar "(";
    print_token buffer region
      (sprintf "Int %s (%s)" lexeme (Z.to_string z));
    print_token buffer rpar ")"

and print_cartesian buffer Region.{value;_} =
  print_nsepseq buffer "*" print_type_expr value

and print_variant buffer {value = {constr; args}; _} =
  print_uident buffer constr;
  match args with
    None -> ()
  | Some (kwd_of, cartesian) ->
      print_token     buffer kwd_of "of";
      print_cartesian buffer cartesian

and print_record_type buffer record_type =
  print_injection buffer print_field_decl record_type

and print_field_decl buffer {value; _} =
  let {field_name; colon; field_type} = value
  in print_var       buffer field_name;
     print_token     buffer colon ":";
     print_type_expr buffer field_type

and print_injection :
  'a.Buffer.t -> (Buffer.t -> 'a -> unit) -> 'a injection reg -> unit =
  fun buffer print {value; _} ->
    let {opening; elements; terminator; closing} = value in
    print_opening    buffer opening;
    print_sepseq     buffer ";" print elements;
    print_terminator buffer terminator;
    print_closing    buffer closing

and print_opening buffer = function
  Begin    region -> print_token buffer region "begin"
| With     region -> print_token buffer region "with"
| LBrace   region -> print_token buffer region "{"
| LBracket region -> print_token buffer region "["

and print_closing buffer = function
  End      region -> print_token buffer region "end"
| RBrace   region -> print_token buffer region "}"
| RBracket region -> print_token buffer region "]"

and print_terminator buffer = function
  Some semi -> print_token buffer semi ";"
| None -> ()

and print_let_binding buffer {bindings; lhs_type; eq; let_rhs} =
  let () = List.iter (print_pattern buffer) bindings in
  let () =
    match lhs_type with
      None -> ()
    | Some (colon, type_expr) ->
        print_token     buffer colon ":";
        print_type_expr buffer type_expr in
  let () = print_token buffer eq "="
  in print_expr buffer let_rhs

and print_pattern buffer = function
  PTuple {value=patterns;_} ->
    print_csv buffer print_pattern patterns
| PList p ->
    print_list_pattern buffer p
| PVar v ->
    print_pvar buffer v
| PUnit {value=lpar,rpar; _} ->
    print_token buffer lpar "(";
    print_token buffer rpar ")"
| PInt i ->
    print_int buffer i
| PTrue kwd_true ->
    print_token buffer kwd_true "true"
| PFalse kwd_false ->
    print_token buffer kwd_false "false"
| PString s ->
    print_str buffer s
| PWild wild ->
    print_token buffer wild "_"
| PPar {value={lpar;inside=p;rpar}; _} ->
    print_token   buffer lpar "(";
    print_pattern buffer p;
    print_token   buffer rpar ")"
| PConstr p ->
    print_constr_pattern buffer p
| PRecord r ->
    print_record_pattern buffer r
| PTyped t ->
    print_typed_pattern buffer t

and print_list_pattern buffer = function
  Sugar p -> print_injection buffer print_pattern p
| PCons p -> print_raw       buffer p

and print_raw buffer {value=p1,c,p2; _} =
  print_pattern buffer p1;
  print_token   buffer c "::";
  print_pattern buffer p2

and print_typed_pattern buffer {value; _} =
  let {pattern; colon; type_expr} = value in
  print_pattern   buffer pattern;
  print_token     buffer colon ":";
  print_type_expr buffer type_expr

and print_record_pattern buffer record_pattern =
  print_injection buffer print_field_pattern record_pattern

and print_field_pattern buffer {value; _} =
  let {field_name; eq; pattern} = value in
  print_var     buffer field_name;
  print_token   buffer eq "=";
  print_pattern buffer pattern

and print_constr_pattern buffer {value=constr, p_opt; _} =
  print_uident buffer constr;
  match p_opt with
    None -> ()
  | Some pattern -> print_pattern buffer pattern

and print_expr buffer = function
  ELetIn {value;_} -> print_let_in      buffer value
|       ECond cond -> print_conditional buffer cond
| ETuple {value;_} -> print_csv         buffer print_expr value
| ECase {value;_}  -> print_match_expr  buffer value
| EFun e           -> print_fun_expr    buffer e

| EAnnot e  -> print_annot_expr  buffer e
| ELogic e  -> print_logic_expr  buffer e
| EArith e  -> print_arith_expr  buffer e
| EString e -> print_string_expr buffer e

| ECall {value=f,l; _} ->
    print_expr buffer f;
    Utils.nseq_iter (print_expr buffer) l
| EVar v ->
    print_var buffer v
| EProj p ->
    print_projection buffer p.value
| EUnit {value=lpar,rpar; _} ->
    print_token buffer lpar "(";
    print_token buffer rpar ")"
| EBytes b ->
    print_bytes buffer b
| EPar {value={lpar;inside=e;rpar}; _} ->
    print_token buffer lpar "(";
    print_expr  buffer e;
    print_token buffer rpar ")"
| EList e ->
    print_list_expr buffer e
| ESeq seq ->
    print_sequence buffer seq
| ERecord e ->
    print_record_expr buffer e
| EConstr {value=constr,None; _} ->
    print_uident buffer constr
| EConstr {value=(constr, Some arg); _} ->
    print_uident buffer constr;
    print_expr   buffer arg

and print_annot_expr buffer {value=e,t; _} =
  print_expr      buffer e;
  print_token     buffer Region.ghost ":";
  print_type_expr buffer t

and print_list_expr buffer = function
  Cons {value={arg1;op;arg2}; _} ->
    print_expr  buffer arg1;
    print_token buffer op "::";
    print_expr  buffer arg2
| List e -> print_injection buffer print_expr e
(*
| Append {value=e1,append,e2; _} ->
    print_expr  buffer e1;
    print_token buffer append "@";
    print_expr  buffer e2
*)

and print_arith_expr buffer = function
  Add {value={arg1;op;arg2}; _} ->
    print_expr  buffer arg1;
    print_token buffer op "+";
    print_expr  buffer arg2
| Sub {value={arg1;op;arg2}; _} ->
    print_expr  buffer arg1;
    print_token buffer op "-";
    print_expr  buffer arg2
| Mult {value={arg1;op;arg2}; _} ->
    print_expr  buffer arg1;
    print_token buffer op "*";
    print_expr  buffer arg2
| Div {value={arg1;op;arg2}; _} ->
    print_expr  buffer arg1;
    print_token buffer op "/";
    print_expr  buffer arg2
| Mod {value={arg1;op;arg2}; _} ->
    print_expr  buffer arg1;
    print_token buffer op "mod";
    print_expr  buffer arg2
| Neg {value={op;arg}; _} ->
    print_token buffer op "-";
    print_expr  buffer arg
| Int {region; value=lex,z} ->
    let line = sprintf "Int %s (%s)" lex (Z.to_string z)
    in print_token buffer region line
| Mutez {region; value=lex,z} ->
    let line = sprintf "Mutez %s (%s)" lex (Z.to_string z)
    in print_token buffer region line
| Nat {region; value=lex,z} ->
    let line = sprintf "Nat %s (%s)" lex (Z.to_string z)
    in print_token buffer region line

and print_string_expr buffer = function
  Cat {value={arg1;op;arg2}; _} ->
    print_expr  buffer arg1;
    print_token buffer op "^";
    print_expr  buffer arg2
| String s ->
    print_str buffer s

and print_logic_expr buffer = function
  BoolExpr e -> print_bool_expr buffer e
| CompExpr e -> print_comp_expr buffer e

and print_bool_expr buffer = function
  Or {value={arg1;op;arg2}; _} ->
    print_expr  buffer arg1;
    print_token buffer op "||";
    print_expr  buffer arg2
| And {value={arg1;op;arg2}; _} ->
    print_expr  buffer arg1;
    print_token buffer op "&&";
    print_expr  buffer arg2
| Not {value={op;arg}; _} ->
    print_token buffer op "not";
    print_expr  buffer arg
| True kwd_true ->
    print_token buffer kwd_true "true"
| False kwd_false ->
    print_token buffer kwd_false "false"

and print_comp_expr buffer = function
  Lt {value={arg1;op;arg2}; _} ->
    print_expr  buffer arg1;
    print_token buffer op "<";
    print_expr  buffer arg2
| Leq {value={arg1;op;arg2}; _} ->
    print_expr  buffer arg1;
    print_token buffer op "<=";
    print_expr  buffer arg2
| Gt {value={arg1;op;arg2}; _} ->
    print_expr  buffer arg1;
    print_token buffer op ">";
    print_expr  buffer arg2
| Geq {value={arg1;op;arg2}; _} ->
    print_expr  buffer arg1;
    print_token buffer op ">=";
    print_expr  buffer arg2
| Neq {value={arg1;op;arg2}; _} ->
    print_expr  buffer arg1;
    print_token buffer op "<>";
    print_expr  buffer arg2
| Equal {value={arg1;op;arg2}; _} ->
    print_expr  buffer arg1;
    print_token buffer op "=";
    print_expr  buffer arg2

and print_record_expr buffer e =
  print_injection buffer print_field_assign e

and print_field_assign buffer {value; _} =
  let {field_name; assignment; field_expr} = value in
  print_var   buffer field_name;
  print_token buffer assignment "=";
  print_expr  buffer field_expr

and print_sequence buffer seq =
  print_injection buffer print_expr seq

and print_match_expr buffer expr =
  let {kwd_match; expr; opening;
       lead_vbar; cases; closing} = expr in
  print_token     buffer kwd_match "match";
  print_expr      buffer expr;
  print_opening   buffer opening;
  print_token_opt buffer lead_vbar "|";
  print_cases     buffer cases;
  print_closing   buffer closing

and print_token_opt buffer = function
         None -> fun _ -> ()
| Some region -> print_token buffer region

and print_cases buffer {value; _} =
  print_nsepseq buffer "|" print_case_clause value

and print_case_clause buffer {value; _} =
  let {pattern; arrow; rhs} = value in
  print_pattern buffer pattern;
  print_token   buffer arrow "->";
  print_expr    buffer rhs

and print_let_in buffer (bind: let_in) =
  let {kwd_let; binding; kwd_in; body} = bind in
  print_token       buffer kwd_let "let";
  print_let_binding buffer binding;
  print_token       buffer kwd_in "in";
  print_expr        buffer body

and print_fun_expr buffer {value; _} =
  let {kwd_fun; params; p_annot; arrow; body} = value in
  let () = print_token buffer kwd_fun "fun" in
  let () =
    match p_annot with
      None -> List.iter (print_pattern buffer) params
    | Some (colon, type_expr) ->
       print_token     buffer colon ":";
       print_type_expr buffer type_expr in
  let () =
    print_token buffer arrow "->"
  in print_expr buffer body

and print_conditional buffer {value; _} =
  let {kwd_if; test; kwd_then;
       ifso; kwd_else; ifnot} = value in
   print_token buffer ghost "(";
   print_token    buffer kwd_if "if";
   print_expr     buffer test;
   print_token    buffer kwd_then "then";
   print_expr     buffer ifso;
   print_token    buffer kwd_else "else";
   print_expr     buffer ifnot;
   print_token    buffer ghost ")"

(* Conversion to string *)

let to_string printer node =
  let buffer = Buffer.create 131 in
  let () = printer buffer node
  in Buffer.contents buffer

let tokens_to_string  = to_string print_tokens
let pattern_to_string = to_string print_pattern
let expr_to_string    = to_string print_expr
