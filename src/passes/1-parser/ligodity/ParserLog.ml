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

let print_csv buffer print {value; _} =
  print_nsepseq buffer "," print value

let print_token buffer (reg: Region.t) conc =
  let line = sprintf "%s: %s\n" (compact reg) conc
  in Buffer.add_string buffer line

let print_var buffer Region.{region; value} =
  let line = sprintf "%s: Ident %s\n" (compact region) value
  in Buffer.add_string buffer line

let print_constr buffer {region; value=lexeme} =
  let line = sprintf "%s: Constr \"%s\"\n"
               (compact region) lexeme
  in Buffer.add_string buffer line

let print_pvar buffer Region.{region; value} =
  let line = sprintf "%s: PVar %s\n" (compact region) value
  in Buffer.add_string buffer line

let print_uident buffer Region.{region; value} =
  let line = sprintf "%s: Uident %s\n" (compact region) value
  in Buffer.add_string buffer line

let print_string buffer Region.{region; value} =
  let line = sprintf "%s: StrLit %s\n" (compact region) value
  in Buffer.add_string buffer line

let print_bytes buffer Region.{region; value=lexeme, abstract} =
  let line = sprintf "%s: Bytes (\"%s\", \"0x%s\")\n"
               (compact region) lexeme (Hex.to_string abstract)
  in Buffer.add_string buffer line

let print_int buffer Region.{region; value=lex,z} =
  let line = sprintf "Int %s (%s)" lex (Z.to_string z)
  in print_token buffer region line

let print_nat buffer {region; value = lexeme, abstract} =
  let line = sprintf "%s: Nat (\"%s\", %s)\n"
               (compact region) lexeme
               (Z.to_string abstract)
  in Buffer.add_string buffer line

let rec print_tokens buffer {decl;eof} =
  Utils.nseq_iter (print_statement buffer) decl;
  print_token buffer eof "EOF"

and print_statement buffer = function
  Let {value=kwd_let, let_binding; _} ->
    print_token       buffer kwd_let "let";
    print_let_binding buffer let_binding
| TypeDecl {value={kwd_type; name; eq; type_expr}; _} ->
    print_token     buffer kwd_type "type";
    print_var       buffer name;
    print_token     buffer eq "=";
    print_type_expr buffer type_expr

and print_type_expr buffer = function
  TProd prod      -> print_cartesian buffer prod
| TSum {value; _} -> print_nsepseq buffer "|" print_variant value
| TRecord t       -> print_rec_type_expr buffer t
| TApp app        -> print_type_app buffer app
| TPar par        -> print_type_par buffer par
| TVar var        -> print_var buffer var
| TFun t          -> print_fun_type buffer t

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

and print_projection buffer {value; _} =
  let {struct_name; selector; field_path} = value in
  print_var     buffer struct_name;
  print_token   buffer selector ".";
  print_nsepseq buffer "." print_selection field_path

and print_selection buffer = function
  FieldName id -> print_var buffer id
| Component c  -> print_int buffer c

and print_cartesian buffer Region.{value;_} =
  print_nsepseq buffer "*" print_type_expr value

and print_variant buffer {value = {constr; arg}; _} =
  print_uident buffer constr;
  match arg with
    None -> ()
  | Some (kwd_of, t_expr) ->
      print_token     buffer kwd_of "of";
      print_type_expr buffer t_expr

and print_rec_type_expr buffer {value; _} =
  let {compound; ne_elements; terminator} = value in
  print_open_compound  buffer compound;
  print_nsepseq        buffer ";" print_field_decl ne_elements;
  print_terminator     buffer terminator;
  print_close_compound buffer compound

and print_field_decl buffer {value; _} =
  let {field_name; colon; field_type} = value
  in print_var       buffer field_name;
     print_token     buffer colon ":";
     print_type_expr buffer field_type

and print_injection :
  'a.Buffer.t -> (Buffer.t -> 'a -> unit) -> 'a injection reg -> unit =
  fun buffer print {value; _} ->
    let {compound; elements; terminator} = value in
    print_open_compound  buffer compound;
    print_sepseq         buffer ";" print elements;
    print_terminator     buffer terminator;
    print_close_compound buffer compound

and print_ne_injection :
  'a.Buffer.t -> (Buffer.t -> 'a -> unit) -> 'a ne_injection reg -> unit =
  fun buffer print {value; _} ->
    let {compound; ne_elements; terminator} = value in
    print_open_compound  buffer compound;
    print_nsepseq        buffer ";" print ne_elements;
    print_terminator     buffer terminator;
    print_close_compound buffer compound

and print_open_compound buffer = function
  BeginEnd (kwd_begin,_) -> print_token buffer kwd_begin "begin"
| Braces   (lbrace,_)    -> print_token buffer lbrace    "{"
| Brackets (lbracket,_)  -> print_token buffer lbracket  "["

and print_close_compound buffer = function
  BeginEnd (_,kwd_end)  -> print_token buffer kwd_end  "end"
| Braces   (_,rbrace)   -> print_token buffer rbrace   "}"
| Brackets (_,rbracket) -> print_token buffer rbracket "]"

and print_terminator buffer = function
  Some semi -> print_token buffer semi ";"
| None -> ()

and print_let_binding buffer {binders; lhs_type; eq; let_rhs} =
  let () = Utils.nseq_iter (print_pattern buffer) binders in
  let () =
    match lhs_type with
      None -> ()
    | Some (colon, type_expr) ->
        print_token     buffer colon ":";
        print_type_expr buffer type_expr in
  let () = print_token buffer eq "="
  in print_expr buffer let_rhs

and print_pattern buffer = function
  PTuple ptuple ->
    print_csv buffer print_pattern ptuple
| PList p ->
    print_list_pattern buffer p
| PVar v ->
    print_pvar buffer v
| PInt i -> print_int buffer i
| PNat i -> print_nat buffer i
| PBytes b -> print_bytes buffer b
| PString s -> print_string buffer s
| PWild wild -> print_token buffer wild "_"
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
| PUnit p -> print_unit buffer p
| PFalse kwd_false -> print_token buffer kwd_false "false"
| PTrue kwd_true -> print_token buffer kwd_true "true"

and print_list_pattern buffer = function
  PListComp p -> print_injection buffer print_pattern p
| PCons p     -> print_raw       buffer p

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
  print_ne_injection buffer print_field_pattern record_pattern

and print_field_pattern buffer {value; _} =
  let {field_name; eq; pattern} = value in
  print_var     buffer field_name;
  print_token   buffer eq "=";
  print_pattern buffer pattern

and print_constr_pattern buffer = function
  PNone p      -> print_none_pattern buffer p
| PSomeApp p   -> print_some_app_pattern buffer p
| PConstrApp p -> print_constr_app_pattern buffer p

and print_none_pattern buffer value =
  print_token buffer value "None"

and print_some_app_pattern buffer {value; _} =
  let c_Some, argument = value in
  print_token   buffer c_Some "Some";
  print_pattern buffer argument

and print_constr_app_pattern buffer node =
  let {value=constr, p_opt; _} = node in
  print_uident buffer constr;
  match p_opt with
    None -> ()
  | Some pattern -> print_pattern buffer pattern

and print_expr buffer = function
  ELetIn let_in -> print_let_in      buffer let_in
| ECond cond    -> print_conditional buffer cond
| ETuple tuple  -> print_csv         buffer print_expr tuple
| ECase case    -> print_match_expr  buffer case
| EFun e        -> print_fun_expr    buffer e
| EAnnot e      -> print_annot_expr  buffer e
| ELogic e      -> print_logic_expr  buffer e
| EArith e      -> print_arith_expr  buffer e
| EString e     -> print_string_expr buffer e
| ECall e       -> print_fun_call buffer e
| EVar v        -> print_var buffer v
| EProj p       -> print_projection buffer p
| EUnit e       -> print_unit buffer e
| EBytes b      -> print_bytes buffer b
| EPar e        -> print_expr_par buffer e
| EList e       -> print_list_expr buffer e
| ESeq seq      -> print_sequence buffer seq
| ERecord e     -> print_record_expr buffer e
| EConstr e     -> print_constr_expr buffer e

and print_constr_expr buffer = function
  ENone e      -> print_none_expr       buffer e
| ESomeApp e   -> print_some_app_expr   buffer e
| EConstrApp e -> print_constr_app_expr buffer e

and print_none_expr buffer value = print_token buffer value "None"

and print_some_app_expr buffer {value; _} =
  let c_Some, argument = value in
  print_token buffer c_Some "Some";
  print_expr  buffer argument

and print_constr_app_expr buffer {value; _} =
  let constr, argument = value in
  print_constr buffer constr;
  match argument with
    None -> ()
  | Some arg -> print_expr buffer arg

and print_expr_par buffer {value; _} =
  let {lpar;inside=e;rpar} = value in
  print_token buffer lpar "(";
  print_expr  buffer e;
  print_token buffer rpar ")"

and print_unit buffer {value=lpar,rpar; _} =
  print_token buffer lpar "(";
  print_token buffer rpar ")"

and print_fun_call buffer {value=f,l; _} =
  print_expr buffer f;
  Utils.nseq_iter (print_expr buffer) l

and print_annot_expr buffer {value=e,t; _} =
  print_expr      buffer e;
  print_token     buffer Region.ghost ":";
  print_type_expr buffer t

and print_list_expr buffer = function
  ECons {value={arg1;op;arg2}; _} ->
    print_expr  buffer arg1;
    print_token buffer op "::";
    print_expr  buffer arg2
| EListComp e ->
   if e.value.elements = None
   then print_token buffer e.region "[]"
   else print_injection buffer print_expr e
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
| StrLit s ->
    print_string buffer s

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
  print_ne_injection buffer print_field_assign e

and print_field_assign buffer {value; _} =
  let {field_name; assignment; field_expr} = value in
  print_var   buffer field_name;
  print_token buffer assignment "=";
  print_expr  buffer field_expr

and print_sequence buffer seq =
  print_injection buffer print_expr seq

and print_match_expr buffer {value; _} =
  let {kwd_match; expr; kwd_with; lead_vbar; cases} = value in
  print_token     buffer kwd_match "match";
  print_expr      buffer expr;
  print_token     buffer kwd_with "with";
  print_token_opt buffer lead_vbar "|";
  print_cases     buffer cases

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

and print_let_in buffer {value; _} =
  let {kwd_let; binding; kwd_in; body} = value in
  print_token       buffer kwd_let "let";
  print_let_binding buffer binding;
  print_token       buffer kwd_in "in";
  print_expr        buffer body

and print_fun_expr buffer {value; _} =
  let {kwd_fun; binders; lhs_type; arrow; body} = value in
  let () = print_token buffer kwd_fun "fun" in
  let () = Utils.nseq_iter (print_pattern buffer) binders in
  let () =
    match lhs_type with
      None -> ()
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
   print_token buffer kwd_if "if";
   print_expr  buffer test;
   print_token buffer kwd_then "then";
   print_expr  buffer ifso;
   print_token buffer kwd_else "else";
   print_expr  buffer ifnot;
   print_token buffer ghost ")"

(* Conversion to string *)

let to_string printer node =
  let buffer = Buffer.create 131 in
  printer buffer node;
  Buffer.contents buffer

let tokens_to_string  = to_string print_tokens
let pattern_to_string = to_string print_pattern
let expr_to_string    = to_string print_expr

(* Pretty-printing the AST *)

let mk_pad len rank pc =
  pc ^ (if rank = len-1 then "`-- " else "|-- "),
  pc ^ (if rank = len-1 then "    " else "|   ")

let pp_ident buffer ~pad:(pd,_) Region.{value=name; region} =
  let node = sprintf "%s%s (%s)\n" pd name (region#compact `Byte)
  in Buffer.add_string buffer node

let pp_node buffer ~pad:(pd,_) name =
  let node = sprintf "%s%s\n" pd name
  in Buffer.add_string buffer node

let pp_string buffer = pp_ident buffer

let pp_loc_node buffer ~pad name region =
  pp_ident buffer ~pad Region.{value=name; region}

let rec pp_ast buffer ~pad:(_,pc as pad) {decl; _} =
  let apply len rank =
    let pad = mk_pad len rank pc in
    pp_declaration buffer ~pad in
  let decls = Utils.nseq_to_list decl in
  pp_node buffer ~pad "<ast>";
  List.iteri (List.length decls |> apply) decls

and pp_declaration buffer ~pad = function
  Let {value; region} ->
    pp_loc_node buffer ~pad "Let" region;
    pp_let_binding buffer ~pad (snd value)
| TypeDecl {value; region} ->
    pp_loc_node buffer ~pad "TypeDecl" region;
    pp_type_decl buffer ~pad value

and pp_let_binding buffer ~pad:(_,pc) node =
  let {binders; lhs_type; let_rhs; _} = node in
  let fields = if lhs_type = None then 2 else 3 in
  let () =
    let pad = mk_pad fields 0 pc in
    pp_node buffer ~pad "<binders>";
    pp_binders buffer ~pad binders in
  let () =
    match lhs_type with
      None -> ()
    | Some (_, type_expr) ->
       let _, pc as pad = mk_pad fields 1 pc in
       pp_node buffer ~pad "<lhs type>";
       pp_type_expr buffer ~pad:(mk_pad 1 0 pc) type_expr in
  let () =
    let _, pc as pad = mk_pad fields (fields - 1) pc in
    pp_node buffer ~pad "<rhs>";
    pp_expr buffer ~pad:(mk_pad 1 0 pc) let_rhs
  in ()

and pp_type_decl buffer ~pad:(_,pc) decl =
  pp_ident buffer ~pad:(mk_pad 2 0 pc) decl.name;
  pp_type_expr buffer ~pad:(mk_pad 2 1 pc) decl.type_expr

and pp_binders buffer ~pad:(_,pc) patterns =
  let patterns = Utils.nseq_to_list patterns in
  let arity = List.length patterns in
  let apply len rank =
    pp_pattern buffer ~pad:(mk_pad len rank pc)
  in List.iteri (apply arity) patterns

and pp_pattern buffer ~pad:(_,pc as pad) = function
  PConstr p ->
    pp_node buffer ~pad "PConstr";
    pp_constr_pattern buffer ~pad:(mk_pad 1 0 pc) p
| PVar v ->
    pp_node buffer ~pad "PVar";
    pp_ident buffer ~pad:(mk_pad 1 0 pc) v
| PWild region ->
    pp_loc_node buffer ~pad "PWild" region
| PInt i ->
    pp_node buffer ~pad "PInt";
    pp_int buffer ~pad i
| PNat n ->
    pp_node buffer ~pad "PNat";
    pp_int buffer ~pad n
| PBytes b ->
    pp_node buffer ~pad "PBytes";
    pp_bytes buffer ~pad b
| PString s ->
    pp_node buffer ~pad "PString";
    pp_string buffer ~pad:(mk_pad 1 0 pc) s
| PUnit {region; _} ->
    pp_loc_node buffer ~pad "PUnit" region
| PFalse region ->
    pp_loc_node buffer ~pad "PFalse" region
| PTrue region ->
    pp_loc_node buffer ~pad "PTrue" region
| PList plist ->
    pp_node buffer ~pad "PList";
    pp_list_pattern buffer ~pad:(mk_pad 1 0 pc) plist
| PTuple t ->
    pp_loc_node buffer ~pad "PTuple" t.region;
    pp_tuple_pattern buffer ~pad:(mk_pad 1 0 pc) t.value
| PPar {value; _} ->
    pp_node buffer ~pad "PPar";
    pp_pattern buffer ~pad:(mk_pad 1 0 pc) value.inside
| PRecord {value; _} ->
    pp_node buffer ~pad "PRecord";
    pp_ne_injection pp_field_pattern buffer ~pad value
| PTyped {value; _} ->
    pp_node buffer ~pad "PTyped";
    pp_typed_pattern buffer ~pad value

and pp_field_pattern buffer ~pad:(_,pc as pad) {value; _} =
  pp_node buffer ~pad value.field_name.value;
  pp_pattern buffer ~pad:(mk_pad 1 0 pc) value.pattern

and pp_typed_pattern buffer ~pad:(_,pc) node =
  pp_pattern buffer ~pad:(mk_pad 2 0 pc) node.pattern;
  pp_type_expr buffer ~pad:(mk_pad 2 1 pc) node.type_expr

and pp_tuple_pattern buffer ~pad:(_,pc) tuple =
  let patterns = Utils.nsepseq_to_list tuple in
  let length   = List.length patterns in
  let apply len rank =
    pp_pattern buffer ~pad:(mk_pad len rank pc)
  in List.iteri (apply length) patterns

and pp_list_pattern buffer ~pad:(_,pc as pad) = function
  PCons {value; region} ->
    let pat1, _, pat2 = value in
    pp_loc_node buffer ~pad "PCons" region;
    pp_pattern buffer ~pad:(mk_pad 2 0 pc) pat1;
    pp_pattern buffer ~pad:(mk_pad 2 1 pc) pat2
| PListComp {value; region} ->
    pp_loc_node buffer ~pad "PListComp" region;
    if value.elements = None
    then pp_node buffer ~pad:(mk_pad 1 0 pc) "<nil>"
    else pp_injection pp_pattern buffer ~pad value

and pp_injection :
  'a.(Buffer.t -> pad:(string*string) -> 'a -> unit)
  -> Buffer.t -> pad:(string*string) -> 'a injection -> unit =
  fun printer buffer ~pad:(_,pc) inj ->
  let elements = Utils.sepseq_to_list inj.elements in
  let length   = List.length elements in
  let apply len rank = printer buffer ~pad:(mk_pad len rank pc)
  in List.iteri (apply length) elements

and pp_ne_injection :
  'a.(Buffer.t -> pad:(string*string) -> 'a -> unit)
  -> Buffer.t -> pad:(string*string) -> 'a ne_injection -> unit =
  fun printer buffer ~pad:(_,pc) inj ->
  let ne_elements = Utils.nsepseq_to_list inj.ne_elements in
  let length      = List.length ne_elements in
  let apply len rank = printer buffer ~pad:(mk_pad len rank pc)
  in List.iteri (apply length) ne_elements

and pp_bytes buffer ~pad:(_,pc) {value=lexeme,hex; region} =
  pp_loc_node buffer ~pad:(mk_pad 2 0 pc) lexeme region;
  pp_node     buffer ~pad:(mk_pad 2 1 pc) (Hex.to_string hex)

and pp_int buffer ~pad:(_,pc) {value=lexeme,z; region} =
  pp_loc_node buffer ~pad:(mk_pad 2 0 pc) lexeme region;
  pp_node     buffer ~pad:(mk_pad 2 1 pc) (Z.to_string z)

and pp_constr_pattern buffer ~pad:(_,pc as pad) = function
  PNone region ->
    pp_loc_node buffer ~pad "PNone" region
| PSomeApp {value=_,param; region} ->
    pp_loc_node buffer ~pad "PSomeApp" region;
    pp_pattern buffer ~pad:(mk_pad 1 0 pc) param
| PConstrApp {value; region} ->
    pp_loc_node buffer ~pad "PConstrApp" region;
    pp_constr_app_pattern buffer ~pad:(mk_pad 1 0 pc) value

and pp_constr_app_pattern buffer ~pad (constr, pat_opt) =
  pp_ident buffer ~pad constr;
  match pat_opt with
    None -> ()
  | Some pat -> pp_pattern buffer ~pad pat

and pp_expr buffer ~pad:(_,pc as pad) = function
  ECase {value; region} ->
    pp_loc_node buffer ~pad "ECase" region;
    pp_case pp_expr buffer ~pad value
| ECond {value; region} ->
    pp_loc_node buffer ~pad "ECond" region;
    pp_cond_expr buffer ~pad value
| EAnnot {value; region} ->
    pp_loc_node buffer ~pad "EAnnot" region;
    pp_annotated buffer ~pad value
| ELogic e_logic ->
    pp_node buffer ~pad "ELogic";
    pp_e_logic buffer ~pad:(mk_pad 1 0 pc) e_logic
| EArith e_arith ->
    pp_node buffer ~pad "EArith";
    pp_arith_expr buffer ~pad:(mk_pad 1 0 pc) e_arith
| EString e_string ->
    pp_node buffer ~pad "EString";
    pp_string_expr buffer ~pad:(mk_pad 1 0 pc) e_string
| EList e_list ->
    pp_node buffer ~pad "EList";
    pp_list_expr buffer ~pad:(mk_pad 1 0 pc) e_list
| EConstr e_constr ->
    pp_node buffer ~pad "EConstr";
    pp_constr_expr buffer ~pad:(mk_pad 1 0 pc) e_constr
| ERecord {value; region} ->
    pp_loc_node buffer ~pad "ERecord" region;
    pp_ne_injection pp_field_assign buffer ~pad value
| EProj {value; region} ->
    pp_loc_node buffer ~pad "EProj" region;
    pp_projection buffer ~pad value
| EVar v ->
    pp_node buffer ~pad "EVar";
    pp_ident buffer ~pad:(mk_pad 1 0 pc) v
| ECall {value; region} ->
    pp_loc_node buffer ~pad "ECall" region;
    pp_fun_call buffer ~pad value
| EBytes b ->
    pp_node buffer ~pad "EBytes";
    pp_bytes buffer ~pad b
| EUnit u ->
    pp_loc_node buffer ~pad "EUnit" u.region
| ETuple e_tuple ->
    pp_node buffer ~pad "ETuple";
    pp_tuple_expr buffer ~pad e_tuple
| EPar {value; region} ->
    pp_loc_node buffer ~pad "EPar" region;
    pp_expr buffer ~pad:(mk_pad 1 0 pc) value.inside
| ELetIn {value; region} ->
    pp_loc_node buffer ~pad "ELetIn" region;
    pp_let_in buffer ~pad value
| EFun {value; region} ->
    pp_loc_node buffer ~pad "EFun" region;
    pp_fun_expr buffer ~pad value
| ESeq {value; region} ->
    pp_loc_node buffer ~pad "ESeq" region;
    pp_injection pp_expr buffer ~pad value

and pp_fun_expr buffer ~pad:(_,pc) node =
  let {binders; lhs_type; body; _} = node in
  let fields = if lhs_type = None then 2 else 3 in
  let () =
    let pad = mk_pad fields 0 pc in
    pp_node buffer ~pad "<parameters>";
    pp_binders buffer ~pad binders in
  let () =
    match lhs_type with
      None -> ()
    | Some (_, type_expr) ->
       let _, pc as pad = mk_pad fields 1 pc in
       pp_node buffer ~pad "<lhs type>";
       pp_type_expr buffer ~pad:(mk_pad 1 0 pc) type_expr in
  let () =
    let pad = mk_pad fields (fields - 1) pc in
    pp_node buffer ~pad "<body>";
    pp_expr buffer ~pad:(mk_pad 1 0 pc) body
  in ()

and pp_let_in buffer ~pad:(_,pc) node =
  let {binding; body; _} = node in
  let {binders; lhs_type; let_rhs; _} = binding in
  let fields = if lhs_type = None then 3 else 4 in
  let () =
    let pad = mk_pad fields 0 pc in
    pp_node buffer ~pad "<binders>";
    pp_binders buffer ~pad binders in
  let () =
    match lhs_type with
      None -> ()
    | Some (_, type_expr) ->
       let _, pc as pad = mk_pad fields 1 pc in
       pp_node buffer ~pad "<lhs type>";
       pp_type_expr buffer ~pad:(mk_pad 1 0 pc) type_expr in
  let () =
    let _, pc as pad = mk_pad fields (fields - 2) pc in
    pp_node buffer ~pad "<rhs>";
    pp_expr buffer ~pad:(mk_pad 1 0 pc) let_rhs in
  let () =
    let _, pc as pad = mk_pad fields (fields - 1) pc in
    pp_node buffer ~pad "<body>";
    pp_expr buffer ~pad:(mk_pad 1 0 pc) body
  in ()

and pp_tuple_expr buffer ~pad:(_,pc) {value; _} =
  let exprs  = Utils.nsepseq_to_list value in
  let length = List.length exprs in
  let apply len rank =
    pp_expr buffer ~pad:(mk_pad len rank pc)
  in List.iteri (apply length) exprs

and pp_fun_call buffer ~pad:(_,pc) (fun_expr, args) =
  let args  = Utils.nseq_to_list args in
  let arity = List.length args in
  let apply len rank =
    pp_expr buffer ~pad:(mk_pad len rank pc)
  in pp_expr buffer ~pad:(mk_pad (1+arity) 0 pc) fun_expr;
     List.iteri (apply arity) args

and pp_projection buffer ~pad:(_,pc) proj =
  let selections = Utils.nsepseq_to_list proj.field_path in
  let len = List.length selections in
  let apply len rank =
    pp_selection buffer ~pad:(mk_pad len rank pc) in
  pp_ident buffer ~pad:(mk_pad (1+len) 0 pc) proj.struct_name;
  List.iteri (apply len) selections

and pp_selection buffer ~pad:(_,pc as pad) = function
  FieldName fn ->
    pp_node buffer ~pad "FieldName";
    pp_ident buffer ~pad:(mk_pad 1 0 pc) fn
| Component c ->
    pp_node buffer ~pad "Component";
    pp_int buffer ~pad c

and pp_field_assign buffer ~pad:(_,pc as pad) {value; _} =
  pp_node buffer ~pad "<field assignment>";
  pp_ident buffer ~pad:(mk_pad 2 0 pc) value.field_name;
  pp_expr  buffer ~pad:(mk_pad 2 1 pc) value.field_expr

and pp_constr_expr buffer ~pad:(_,pc as pad) = function
  ENone region ->
    pp_loc_node buffer ~pad "ENone" region
| ESomeApp {value=_,arg; region} ->
    pp_loc_node buffer ~pad "ESomeApp" region;
    pp_expr buffer ~pad:(mk_pad 1 0 pc) arg
| EConstrApp {value; region} ->
    pp_loc_node buffer ~pad "EConstrApp" region;
    pp_constr_app_expr buffer ~pad value

and pp_constr_app_expr buffer ~pad:(_,pc) (constr, expr_opt) =
  match expr_opt with
    None -> pp_ident buffer ~pad:(mk_pad 1 0 pc) constr
  | Some expr ->
     pp_ident buffer ~pad:(mk_pad 2 0 pc) constr;
     pp_expr  buffer ~pad:(mk_pad 2 1 pc) expr

and pp_list_expr buffer ~pad:(_,pc as pad) = function
  ECons {value; region} ->
    pp_loc_node buffer ~pad "Cons" region;
    pp_expr buffer ~pad:(mk_pad 2 0 pc) value.arg1;
    pp_expr buffer ~pad:(mk_pad 2 1 pc) value.arg2
| EListComp {value; region} ->
    pp_loc_node buffer ~pad "List" region;
    if   value.elements = None
    then pp_node buffer ~pad:(mk_pad 1 0 pc) "<nil>"
    else pp_injection pp_expr buffer ~pad value

and pp_string_expr buffer ~pad:(_,pc as pad) = function
  Cat {value; region} ->
    pp_loc_node buffer ~pad "Cat" region;
    pp_expr buffer ~pad:(mk_pad 2 0 pc) value.arg1;
    pp_expr buffer ~pad:(mk_pad 2 1 pc) value.arg2;
| StrLit s ->
    pp_node buffer ~pad "StrLit";
    pp_string buffer ~pad:(mk_pad 1 0 pc) s

and pp_arith_expr buffer ~pad:(_,pc as pad) = function
  Add {value; region} ->
    pp_bin_op "Add" region buffer ~pad value
| Sub {value; region} ->
    pp_bin_op "Sub" region buffer ~pad value
| Mult {value; region} ->
    pp_bin_op "Mult" region buffer ~pad value
| Div {value; region} ->
    pp_bin_op "Div" region buffer ~pad value
| Mod {value; region} ->
    pp_bin_op "Mod" region buffer ~pad value
| Neg {value; region} ->
    pp_loc_node buffer ~pad "Neg" region;
    pp_expr buffer ~pad:(mk_pad 1 0 pc) value.arg;
| Int i ->
    pp_node buffer ~pad "Int";
    pp_int  buffer ~pad i
| Nat n ->
    pp_node buffer ~pad "Nat";
    pp_int  buffer ~pad n
| Mutez m ->
    pp_node buffer ~pad "Mutez";
    pp_int  buffer ~pad m

and pp_e_logic buffer ~pad:(_,pc as pad) = function
  BoolExpr e ->
    pp_node buffer ~pad "BoolExpr";
    pp_bool_expr buffer ~pad:(mk_pad 1 0 pc) e
| CompExpr e ->
    pp_node buffer ~pad "CompExpr";
    pp_comp_expr buffer ~pad:(mk_pad 1 0 pc) e

and pp_bool_expr buffer ~pad:(_,pc as pad) = function
  Or {value; region} ->
    pp_bin_op "Or" region buffer ~pad value
| And {value; region} ->
    pp_bin_op "And" region buffer ~pad value
| Not {value; _} ->
    pp_node buffer ~pad "Not";
    pp_expr buffer ~pad:(mk_pad 1 0 pc) value.arg
| False region ->
    pp_loc_node buffer ~pad "False" region
| True region ->
    pp_loc_node buffer ~pad "True" region

and pp_comp_expr buffer ~pad = function
  Lt {value; region} ->
    pp_bin_op "Lt" region buffer ~pad value
| Leq {value; region} ->
    pp_bin_op "Leq" region buffer ~pad value
| Gt {value; region} ->
    pp_bin_op "Gt" region buffer ~pad value
| Geq {value; region} ->
    pp_bin_op "Geq" region buffer ~pad value
| Equal {value; region} ->
    pp_bin_op "Equal" region buffer ~pad value
| Neq {value; region} ->
    pp_bin_op "Neq" region buffer ~pad value

and pp_bin_op node region buffer ~pad:(_,pc as pad) op =
  pp_loc_node buffer ~pad node region;
  pp_expr buffer ~pad:(mk_pad 2 0 pc) op.arg1;
  pp_expr buffer ~pad:(mk_pad 2 1 pc) op.arg2

and pp_annotated buffer ~pad:(_,pc) (expr, t_expr) =
  pp_expr buffer ~pad:(mk_pad 2 0 pc) expr;
  pp_type_expr buffer ~pad:(mk_pad 2 1 pc) t_expr

and pp_cond_expr buffer ~pad:(_,pc) (cond: cond_expr) =
  let () =
    let _, pc as pad = mk_pad 3 0 pc in
    pp_node buffer ~pad "<condition>";
    pp_expr buffer ~pad:(mk_pad 1 0 pc) cond.test in
  let () =
    let _, pc as pad = mk_pad 3 1 pc in
    pp_node buffer ~pad "<true>";
    pp_expr buffer ~pad:(mk_pad 1 0 pc) cond.ifso in
  let () =
    let _, pc as pad = mk_pad 3 2 pc in
    pp_node buffer ~pad "<false>";
    pp_expr buffer ~pad:(mk_pad 1 0 pc) cond.ifnot
  in ()

and pp_case :
      'a.(Buffer.t -> pad:(string*string) -> 'a -> unit)
      -> Buffer.t -> pad:(string*string) -> 'a case -> unit =
  fun printer buffer ~pad:(_,pc) case ->
  let clauses = Utils.nsepseq_to_list case.cases.value in
  let clauses = List.map (fun {value; _} -> value) clauses in
  let length  = List.length clauses + 1 in
  let apply len rank =
    pp_case_clause printer buffer ~pad:(mk_pad len (rank+1) pc)
  in pp_expr buffer ~pad:(mk_pad length 0 pc) case.expr;
     List.iteri (apply length) clauses

and pp_case_clause :
      'a.(Buffer.t -> pad:(string*string) -> 'a -> unit)
      -> Buffer.t -> pad:(string*string) -> 'a case_clause -> unit =
  fun printer buffer ~pad:(_,pc as pad) clause ->
  pp_node buffer ~pad "<clause>";
  pp_pattern buffer ~pad:(mk_pad 2 0 pc) clause.pattern;
  printer buffer ~pad:(mk_pad 2 1 pc) clause.rhs

and pp_type_expr buffer ~pad:(_,pc as pad) = function
  TProd {value; region} ->
    pp_loc_node buffer ~pad "TProd" region;
    pp_cartesian buffer ~pad value
| TSum {value; region} ->
    pp_loc_node buffer ~pad "TSum" region;
    let apply len rank variant =
      let pad = mk_pad len rank pc in
      pp_variant buffer ~pad variant.value in
    let variants = Utils.nsepseq_to_list value in
    List.iteri (List.length variants |> apply) variants
| TRecord {value; region} ->
    pp_loc_node buffer ~pad "TRecord" region;
    pp_ne_injection pp_field_decl buffer ~pad value
| TApp {value=name,tuple; region} ->
    pp_loc_node buffer ~pad "TApp" region;
    pp_ident buffer ~pad:(mk_pad 1 0 pc) name;
    pp_type_tuple buffer ~pad:(mk_pad 2 1 pc) tuple
| TFun {value; region} ->
    pp_loc_node buffer ~pad "TFun" region;
    let apply len rank =
      let pad = mk_pad len rank pc in
      pp_type_expr buffer ~pad in
    let domain, _, range = value in
    List.iteri (apply 2) [domain; range]
 | TPar {value={inside;_}; region} ->
    pp_loc_node buffer ~pad "TPar" region;
    pp_type_expr buffer ~pad:(mk_pad 1 0 pc) inside
 | TVar v ->
    pp_node buffer ~pad "TVar";
    pp_ident buffer ~pad:(mk_pad 1 0 pc) v

and pp_type_tuple buffer ~pad:(_,pc) {value; _} =
  let components = Utils.nsepseq_to_list value.inside in
  let apply len rank =
    pp_type_expr buffer ~pad:(mk_pad len rank pc)
  in List.iteri (List.length components |> apply) components

and pp_field_decl buffer ~pad:(_,pc as pad) {value; _} =
  pp_ident buffer ~pad value.field_name;
  pp_type_expr buffer ~pad:(mk_pad 1 0 pc) value.field_type

and pp_cartesian buffer ~pad:(_,pc) t_exprs =
  let t_exprs = Utils.nsepseq_to_list t_exprs in
  let arity   = List.length t_exprs in
  let apply len rank =
    pp_type_expr buffer ~pad:(mk_pad len rank pc)
  in List.iteri (apply arity) t_exprs

and pp_variant buffer ~pad:(_,pc as pad) {constr; arg} =
  pp_ident buffer ~pad constr;
  match arg with
    None -> ()
  | Some (_,c) ->
     pp_type_expr buffer ~pad:(mk_pad 1 0 pc) c

let pp_ast buffer = pp_ast buffer ~pad:("","")
