(* PRINTING THE CST *)

(* This module produces an arborescent, textual representation of a
   subset of the Concrete Abstract Tree (CST). It aims at a readable
   format with the most relevant nodes, with source locations. This
   functionality is most useful when testing the parser, for example,
   checking that a particular node corresponding to an operator has
   the expected associativity with the same kind, or the expected
   priority over another. *)

[@@@coverage exclude_file]

(* Vendor dependencies *)

module Directive = Preprocessor.Directive
module Utils     = Simple_utils.Utils
module Region    = Simple_utils.Region

open! Region (* TODO: Remove *)

(* Internal dependencies *)

module Tree = Cst_shared.Tree

type state = Tree.state

open CST (* THE ONLY GLOBAL OPENING *)

(* UTILITIES *)

let sprintf  = Printf.sprintf

let compact state (region: Region.t) =
  region#compact ~offsets:state#offsets state#mode

let print_attribute state (node : Attr.t reg) =
  let key, val_opt = node.value in
  match val_opt with
    None ->
      Tree.print_unary state "<attribute>" Tree.print_node key
  | Some String value ->
      let children = [
        Tree.mk_child Tree.print_node key;
        Tree.mk_child Tree.print_node value]
      in Tree.print state "<attributes>" children

type label = Tree.label

let print_list :
  state -> ?region:Region.t -> label -> 'a Tree.printer -> 'a list -> unit =
  fun state ?region label print list ->
    let children = List.map ~f:(Tree.mk_child print) list
    in Tree.print ?region state label children

let print_attributes state (node : Attr.attribute reg list) =
  print_list state "<attributes>" print_attribute node


(* Pretty-printing the CST *)

let print_ident state {value=name; region} =
  let reg  = compact state region in
  let node = sprintf "%s%s (%s)\n" state#pad_path name reg
  in Buffer.add_string state#buffer node

let print_node state name =
  let node = sprintf "%s%s\n" state#pad_path name
  in Buffer.add_string state#buffer node

let print_string state {value=name; region} =
  let reg  = compact state region in
  let node = sprintf "%s%S (%s)\n" state#pad_path name reg
  in Buffer.add_string state#buffer node

let print_verbatim state {value=name; region} =
  let reg  = compact state region in
  let node = sprintf "%s{|%s|} (%s)\n" state#pad_path name reg
  in Buffer.add_string state#buffer node

let print_loc_node state name region =
  print_ident state {value=name; region}

let rec print_cst state {decl; _} =
  let apply len rank =
    print_declaration (state#pad len rank) in
  let decls = Utils.nseq_to_list decl in
  print_node state "<ast>";
  List.iteri ~f:(List.length decls |> apply) decls

and print_declaration state = function
  Let {value = (_kwd_let, kwd_rec, let_binding, attr); region} ->
    print_loc_node state "Let" region;
    (if Option.is_some kwd_rec then print_node (state#pad 0 0) "rec"); (* Hack *)
    print_let_binding state let_binding attr
| TypeDecl {value; region} ->
    print_loc_node  state "TypeDecl" region;
    print_type_decl state value
| ModuleDecl {value; region} ->
    print_loc_node    state "ModuleDecl" region;
    print_module_decl state value
| ModuleAlias {value; region} ->
    print_loc_node     state "ModuleDecl" region;
    print_module_alias state value
| Directive dir ->
    let region, string = Directive.project dir in
    print_loc_node state "Directive" region;
    print_node     (state#pad 1 0) string

and print_let_binding state node attr =
  let {binders; type_params; rhs_type; let_rhs; _} = node in
  let arity =
    match type_params, rhs_type with
      None,   None   -> 2
    | Some _, None
    | None,   Some _ -> 3
    | Some _, Some _ -> 4 in
  let arity = if List.is_empty attr then arity else arity+1 in
  let rank = 0 in
  let rank =
    match type_params with
      None -> rank
    | Some params ->
        let state = state#pad arity rank in
        print_node state "<type_params>";
        print_type_params state params; rank+1 in
  let rank =
    let state = state#pad arity rank in
    print_node    state "<binders>";
    print_binders state binders; rank+1 in
  let rank =
    match rhs_type with
      None -> rank
    | Some (_, type_expr) ->
        let state = state#pad arity rank in
        print_node state "<rhs type>";
        print_type_expr (state#pad 1 0) type_expr;
        rank+1 in
  let rank =
    let state = state#pad arity rank in
    print_node state "<rhs>";
    print_expr (state#pad 1 0) let_rhs;
    rank+1 in
  let () =
    if not (List.is_empty attr) then
      let state = state#pad arity rank
      in print_attributes state attr
  in ()

and print_binders state patterns =
  let patterns       = Utils.nseq_to_list patterns in
  let arity          = List.length patterns in
  let apply len rank = print_pattern (state#pad len rank)
  in List.iteri ~f:(apply arity) patterns

and print_type_params state (node : type_params par reg) =
  let {value={inside; _}; _} = node in
  let vars = Utils.nseq_to_list inside.type_vars in
  let arity = List.length vars in
  let apply len rank = print_ident (state#pad len rank)
  in List.iteri ~f:(apply arity) vars

and print_type_decl state decl =
  let arity = if Option.is_none decl.params then 2 else 3 in
  let rank =
    print_ident (state#pad arity 0) decl.name; 1 in
  let rank =
    match decl.params with
      Some params ->
        print_type_vars (state#pad arity rank) params; rank+1
    | None -> rank in
  print_type_expr (state#pad arity rank) decl.type_expr

and print_type_vars state = function
  QParam p -> print_type_var (state#pad 1 0) p
| QParamTuple p ->
    let {value = {inside; _}; _} = p in
    let type_vars = Utils.nsepseq_to_list inside in
    let arity = List.length type_vars in
    let apply len rank = print_type_var (state#pad len rank)
    in List.iteri ~f:(apply arity) type_vars

and print_type_var state (node : type_var reg) =
  print_ident state {node with value = "'" ^ node.value.name.value}

and print_module_decl state decl =
  print_ident     (state#pad 2 0) decl.name;
  print_cst       (state#pad 2 1) decl.module_

and print_module_alias state decl =
  let binders        = Utils.nsepseq_to_list decl.binders in
  let len            = List.length binders in
  let apply len rank = print_ident (state#pad len rank) in
  print_ident (state#pad (1+len) 0) decl.alias;
  List.iteri ~f:(apply len) binders

and print_pvar state {value; _} =
  let {variable; attributes} = value in
  if List.is_empty attributes then
    print_ident state variable
  else
    (print_node       state "PVar";
     print_ident      (state#pad 2 0) variable;
     print_attributes (state#pad 2 1) attributes)

and print_pattern state = function
  PConstr p ->
    print_node state "PConstr";
    print_constr_pattern (state#pad 1 0) p
| PVar p -> print_pvar state p
| PInt i ->
    print_node state "PInt";
    print_int  state i
| PNat n ->
    print_node state "PNat";
    print_int  state n
| PBytes b ->
    print_node  state "PBytes";
    print_bytes state b
| PString s ->
    print_node   state "PString";
    print_string (state#pad 1 0) s
| PVerbatim v ->
    print_node   state "PVerbatim";
    print_verbatim (state#pad 1 0) v
| PUnit {region; _} ->
    print_loc_node state "PUnit" region
| PList plist ->
    print_node state "PList";
    print_list_pattern (state#pad 1 0) plist
| PTuple t ->
    print_loc_node state "PTuple" t.region;
    print_tuple_pattern (state#pad 1 0) t.value
| PPar {value; _} ->
    print_node state "PPar";
    print_pattern (state#pad 1 0) value.inside
| PRecord {value; _} ->
    print_node state "PRecord";
    print_ne_injection print_field_pattern state value
| PTyped {value; _} ->
    print_node state "PTyped";
    print_typed_pattern state value

and print_field_pattern state {value; _} =
  print_node    state value.field_name.value;
  print_pattern (state#pad 1 0) value.pattern

and print_typed_pattern state node =
  print_pattern   (state#pad 2 0) node.pattern;
  print_type_expr (state#pad 2 1) node.type_expr

and print_tuple_pattern state tuple =
  let patterns       = Utils.nsepseq_to_list tuple in
  let length         = List.length patterns in
  let apply len rank = print_pattern (state#pad len rank)
  in List.iteri ~f:(apply length) patterns

and print_list_pattern state = function
  PCons {value; region} ->
    let pat1, _, pat2 = value in
    print_loc_node state "PCons" region;
    print_pattern  (state#pad 2 0) pat1;
    print_pattern  (state#pad 2 1) pat2
| PListComp {value; region} ->
    print_loc_node state "PListComp" region;
    if Option.is_none value.elements
    then print_node (state#pad 1 0) "<nil>"
    else print_injection print_pattern state value

and print_injection :
  'a.(state -> 'a -> unit) -> state -> 'a injection -> unit =
  fun printer state inj ->
    let elements       = Utils.sepseq_to_list inj.elements in
    let length         = List.length elements in
    let apply len rank = printer (state#pad len rank)
    in List.iteri ~f:(apply length) elements

and print_ne_injection :
  'a.(state -> 'a -> unit) -> state -> 'a ne_injection -> unit =
  fun printer state inj ->
    let ne_elements = Utils.nsepseq_to_list inj.ne_elements in
    let length      = List.length ne_elements in
    let arity       = if List.is_empty inj.attributes then length else length+1
    and apply len rank = printer (state#pad len rank)
    in List.iteri ~f:(apply arity) ne_elements;
       if not (List.is_empty inj.attributes) then
         let state = state#pad arity (arity-1)
         in print_attributes state inj.attributes

and print_record_type state = print_ne_injection print_field_decl state

and print_bytes state {value=lexeme,hex; region} =
  print_loc_node (state#pad 2 0) lexeme region;
  print_node     (state#pad 2 1) (Hex.show hex)

and print_int state {value=lexeme,z; region} =
  print_loc_node (state#pad 2 0) lexeme region;
  print_node     (state#pad 2 1) (Z.to_string z)

and print_mutez state {value=lexeme,int64; region} =
  print_loc_node (state#pad 2 0) lexeme region;
  print_node     (state#pad 2 1) (Int64.to_string int64)

and print_constr_pattern state {value; _} =
  let constr, pat_opt = value in
  print_ident state constr;
  match pat_opt with
    None -> ()
  | Some pat -> print_pattern state pat

and print_expr state = function
  ECase {value; region} ->
    print_loc_node state "ECase" region;
    print_case print_expr state value
| ECond {value; region} ->
    print_loc_node state "ECond" region;
    print_cond_expr state value
| EAnnot {value; region} ->
    print_loc_node  state "EAnnot" region;
    print_annotated state value
| ELogic e_logic ->
    print_node state "ELogic";
    print_e_logic (state#pad 1 0) e_logic
| EArith e_arith ->
    print_node state "EArith";
    print_arith_expr (state#pad 1 0) e_arith
| EString e_string ->
    print_node state "EString";
    print_string_expr (state#pad 1 0) e_string
| EList e_list ->
    print_node state "EList";
    print_list_expr (state#pad 1 0) e_list
| EConstr e_constr ->
    print_node state "EConstr";
    print_constr_expr (state#pad 1 0) e_constr
| ERecord {value; region} ->
    print_loc_node state "ERecord" region;
    print_ne_injection print_field_assign state value
| EProj {value; region} ->
    print_loc_node state "EProj" region;
    print_projection state value
| EModA {value; region} ->
    print_loc_node state "EModA" region;
    print_module_access print_expr state value
| EUpdate {value; region} ->
    print_loc_node state "EUpdate" region;
    print_update state value
| EVar v ->
    print_node  state "EVar";
    print_ident (state#pad 1 0) v
| ECall {value; region} ->
    print_loc_node state "ECall" region;
    print_fun_call state value
| EBytes b ->
    print_node state "EBytes";
    print_bytes state b
| EUnit u ->
    print_loc_node state "EUnit" u.region
| ETuple e_tuple ->
    print_node state "ETuple";
    print_tuple_expr state e_tuple
| EPar {value; region} ->
    print_loc_node state "EPar" region;
    print_expr (state#pad 1 0) value.inside
| ELetIn {value; region} ->
    print_loc_node state  "ELetIn" region;
    print_let_in state value
| ETypeIn {value; region} ->
    print_loc_node state  "ETypeIn" region;
    print_type_in state value
| EModIn {value; region} ->
    print_loc_node state  "EModIn" region;
    print_mod_in state value
| EModAlias {value; region} ->
    print_loc_node state  "EModAlias" region;
    print_mod_alias state value
| EFun {value; region} ->
    print_loc_node state "EFun" region;
    print_fun_expr state value
| ESeq {value; region} ->
    print_loc_node state "ESeq" region;
    print_injection print_expr state value
| ECodeInj {value; region} ->
    print_loc_node state "ECodeInj" region;
    print_code_inj state value
| ERevApp {value; region} ->
    print_bin_op "ERevApp" region state value

and print_module_access :
  type a. (state -> a -> unit ) -> state -> a module_access -> unit =
  fun f state ma ->
    print_ident (state#pad 2 0) ma.module_name;
    f (state#pad 2 1) ma.field

and print_fun_expr state node =
  let {binders; rhs_type; body; _} = node in
  let arity = if Option.is_none rhs_type then 2 else 3 in
  let () =
    let state = state#pad arity 0 in
    print_node state "<parameters>";
    print_binders state binders in
  let () =
    match rhs_type with
      None -> ()
    | Some (_, type_expr) ->
       let state = state#pad arity 1 in
       print_node state "<lhs type>";
       print_type_expr (state#pad 1 0) type_expr in
  let () =
    let state = state#pad arity (arity - 1) in
    print_node state "<body>";
    print_expr (state#pad 1 0) body
  in ()

and print_code_inj state rc =
  let () =
    let state = state#pad 2 0 in
    print_node state "<language>";
    print_string (state#pad 1 0) rc.language.value in
  let () =
    let state = state#pad 2 1 in
    print_node state "<code>";
    print_expr (state#pad 1 0) rc.code
  in ()

and print_let_in state node =
  let {binding; body; attributes; kwd_rec; _} = node in
  let {binders; rhs_type; let_rhs; _} = binding in
  let arity = if Option.is_none rhs_type then 3 else 4 in
  let arity = if Option.is_none kwd_rec then arity else arity+1 in
  let arity = if List.is_empty attributes then arity else arity+1 in
  let rank =
    match kwd_rec with
      None -> 0
    | Some (_) ->
      let state = state#pad arity 0 in
      print_node state "rec"; 0 in
  let rank =
    let state = state#pad arity 0 in
    print_node state "<binders>";
    print_binders state binders; rank in
  let rank =
    match rhs_type with
      None -> rank
    | Some (_, type_expr) ->
       let state = state#pad arity (rank+1) in
       print_node state "<lhs type>";
       print_type_expr (state#pad 1 0) type_expr;
       rank+1 in
  let rank =
    let state = state#pad arity (rank+1) in
    print_node state "<rhs>";
    print_expr (state#pad 1 0) let_rhs;
    rank+1 in
  let rank =
    let state = state#pad arity (rank+1) in
    print_node state "<body>";
    print_expr (state#pad 1 0) body;
    rank+1 in
  let () =
    if not (List.is_empty attributes) then
      let state = state#pad arity (rank+1)
      in print_attributes state attributes
  in ()

and print_type_in state node =
  let {type_decl; body; _} = node in
  let {name; type_expr; _} = type_decl in
  let () =
    let state = state#pad 3 0 in
    print_node  state "<name>";
    print_ident state name in
  let () =
    let state = state#pad 3 1 in
    print_node state "<type>";
    print_type_expr (state#pad 1 0) type_expr in
  let () =
    let state = state#pad 3 2 in
    print_node state "<body>";
    print_expr (state#pad 1 0) body
  in ()

and print_mod_in state node =
  let {mod_decl; body; _} = node in
  let {name; module_; _} = mod_decl in
  let () =
    let state = state#pad 3 0 in
    print_node state "<name>";
    print_ident state name in
  let () =
    let state = state#pad 3 1 in
    print_node state "<module>";
    print_cst (state#pad 1 0) module_ in
  let () =
    let state = state#pad 3 2 in
    print_node state "<body>";
    print_expr (state#pad 1 0) body
  in ()

and print_mod_alias state node =
  let {mod_alias; body; _} = node in
  let {alias;binders; _} = mod_alias in
  let () =
    let state = state#pad 3 0 in
    print_node  state "<alias>";
    print_ident state alias in
  let () =
    let state          = state#pad 3 1 in
    let binders        = Utils.nsepseq_to_list binders in
    let len            = List.length binders in
    let apply len rank = print_ident (state#pad len rank) in
    print_node state "<module>";
    List.iteri ~f:(apply len) binders in
  let () =
    let state = state#pad 3 2 in
    print_node state "<body>";
    print_expr (state#pad 1 0) body
  in ()

(*
and print_attributes state attributes =
  let apply {value = attribute; region} =
    let attribute_formatted = sprintf "[@%s]" attribute in
    let token = Token.wrap attribute_formatted region in
    print_token state token attribute_formatted
  in List.iter ~f:apply attributes
 *)

and print_tuple_expr state {value; _} =
  let exprs          = Utils.nsepseq_to_list value in
  let length         = List.length exprs in
  let apply len rank = print_expr (state#pad len rank)
  in List.iteri ~f:(apply length) exprs

and print_fun_call state (fun_expr, args) =
  let args           = Utils.nseq_to_list args in
  let arity          = List.length args in
  let apply len rank = print_expr (state#pad len rank)
  in print_expr (state#pad (1+arity) 0) fun_expr;
     List.iteri ~f:(apply arity) args

and print_projection state proj =
  let selections     = Utils.nsepseq_to_list proj.field_path in
  let len            = List.length selections in
  let apply len rank = print_selection (state#pad len rank) in
  print_ident (state#pad (1+len) 0) proj.struct_name;
  List.iteri ~f:(apply len) selections

and print_update state update =
  print_path (state#pad 2 0) update.record;
  print_ne_injection print_field_path_assign state update.updates.value

and print_path state = function
  Name name ->
    print_node state "Name";
    print_ident (state#pad 1 0) name
| Path {value; region} ->
    print_loc_node state "Path" region;
    print_projection state value

and print_selection state = function
  FieldName fn ->
    print_node state "FieldName";
    print_ident (state#pad 1 0) fn
| Component c ->
    print_node state "Component";
    print_int state c

and print_field_assign state {value; _} =
  print_node  state  "<field assignment>";
  print_ident (state#pad 2 0) value.field_name;
  print_expr  (state#pad 2 1) value.field_expr

and print_field_path_assign state {value; _} =
  let {field_path; field_expr; _} = value in
  print_node state "<update>";
  print_path (state#pad 2 0) field_path;
  print_expr (state#pad 2 1) field_expr

and print_constr_expr state {value; _} =
  let constr, expr_opt = value in
  match expr_opt with
    None -> print_ident (state#pad 1 0) constr
  | Some expr ->
     print_ident (state#pad 2 0) constr;
     print_expr  (state#pad 2 1) expr

and print_list_expr state = function
  ECons {value; region} ->
    print_loc_node state "ECons" region;
    print_expr (state#pad 2 0) value.arg1;
    print_expr (state#pad 2 1) value.arg2
| EListComp {value; region} ->
    print_loc_node state "EListComp" region;
    if   Option.is_none value.elements
    then print_node (state#pad 1 0) "<nil>"
    else print_injection print_expr state value

and print_string_expr state = function
  Cat {value; region} ->
    print_loc_node state "Cat" region;
    print_expr (state#pad 2 0) value.arg1;
    print_expr (state#pad 2 1) value.arg2;
| String s ->
    print_node   state "String";
    print_string (state#pad 1 0) s
| Verbatim v ->
    print_node   state "Verbatim";
    print_string (state#pad 1 0) v

and print_arith_expr state = function
  Add {value; region} ->
    print_bin_op "Add" region state value
| Sub {value; region} ->
    print_bin_op "Sub" region state value
| Mult {value; region} ->
    print_bin_op "Mult" region state value
| Div {value; region} ->
    print_bin_op "Div" region state value
| Mod {value; region} ->
    print_bin_op "Mod" region state value
| Land {value; region} ->
    print_bin_op "Land" region state value
| Lor {value; region} ->
    print_bin_op "Lor" region state value
| Lxor {value; region} ->
    print_bin_op "Lxor" region state value
| Lsl {value; region} ->
    print_bin_op "Lsl" region state value
| Lsr {value; region} ->
    print_bin_op "Lsr" region state value
| Neg {value; region} ->
    print_loc_node state "Neg" region;
    print_expr (state#pad 1 0) value.arg;
| Int i ->
    print_node state "Int";
    print_int  state i
| Nat n ->
    print_node state "Nat";
    print_int  state n
| Mutez m ->
    print_node state "Mutez";
    print_mutez  state m

and print_e_logic state = function
  BoolExpr e ->
    print_node state "BoolExpr";
    print_bool_expr (state#pad 1 0) e
| CompExpr e ->
    print_node state "CompExpr";
    print_comp_expr (state#pad 1 0) e

and print_bool_expr state = function
  Or {value; region} ->
    print_bin_op "Or" region state value
| And {value; region} ->
    print_bin_op "And" region state value
| Not {value; _} ->
    print_node state "Not";
    print_expr (state#pad 1 0) value.arg

and print_comp_expr state = function
  Lt {value; region} ->
    print_bin_op "Lt" region state value
| Leq {value; region} ->
    print_bin_op "Leq" region state value
| Gt {value; region} ->
    print_bin_op "Gt" region state value
| Geq {value; region} ->
    print_bin_op "Geq" region state value
| Equal {value; region} ->
    print_bin_op "Equal" region state value
| Neq {value; region} ->
    print_bin_op "Neq" region state value

and print_bin_op node region state op =
  print_loc_node state node region;
  print_expr (state#pad 2 0) op.arg1;
  print_expr (state#pad 2 1) op.arg2

and print_annotated state annot =
  let expr, _, t_expr = annot.inside in
  print_expr      (state#pad 2 0) expr;
  print_type_expr (state#pad 2 1) t_expr

and print_cond_expr state (cond: cond_expr) =
  let arity = if Option.is_none cond.ifnot then 2 else 3 in
  let () =
    let state = state#pad arity 0 in
    print_node state "<condition>";
    print_expr (state#pad 1 0) cond.test in
  let () =
    let state = state#pad arity 1 in
    print_node state "<true>";
    print_expr (state#pad 1 0) cond.ifso in
  let () = match cond.ifnot with
    Some (_, ifnot) ->
      let state = state#pad arity 2 in
      print_node state "<false>";
      print_expr (state#pad 1 0) ifnot
  | None -> ()
  in ()

and print_case :
  'a.(state -> 'a -> unit) -> state -> 'a case -> unit =
  fun printer state case ->
  let clauses = Utils.nsepseq_to_list case.cases.value in
  let clauses = List.map ~f:(fun x -> x.value) clauses in
  let arity  = List.length clauses + 1 in
  let apply len rank =
    print_case_clause printer (state#pad len (rank+1))
  in print_expr (state#pad arity 0) case.expr;
     List.iteri ~f:(apply arity) clauses

and print_case_clause :
  'a.(state -> 'a -> unit) -> state -> 'a case_clause -> unit =
  fun printer state clause ->
  print_node    state "<clause>";
  print_pattern (state#pad 2 0) clause.pattern;
  printer    (state#pad 2 1) clause.rhs

and print_type_expr state = function
  TProd {value; region} ->
    print_loc_node state "TProd" region;
    print_cartesian state value
| TSum {value; region} ->
    print_loc_node state "TSum" region;
    print_sum_type state value
| TRecord {value; region} ->
    print_loc_node    state "TRecord" region;
    print_record_type state value
| TApp {value; region} ->
    let name, tuple = value in
    print_loc_node        state "TApp" region;
    print_ident           (state#pad 2 0) name;
    print_type_constr_arg (state#pad 2 1) tuple
| TFun {value; region} ->
    print_loc_node state "TFun" region;
    let apply len rank =
      print_type_expr (state#pad len rank) in
    let domain, _, range = value in
    List.iteri ~f:(apply 2) [domain; range]
| TPar {value={inside;_}; region} ->
    print_loc_node  state "TPar" region;
    print_type_expr (state#pad 1 0) inside
| TVar v ->
    print_node  state "TVar";
    print_ident (state#pad 1 0) v
| TString s ->
    print_node   state "TString";
    print_string (state#pad 1 0) s
| TInt s ->
    print_node   state "TInt";
    print_int (state#pad 1 0) s
| TModA {region; value} ->
    print_loc_node state "TModA" region;
    print_module_access print_type_expr state value
| TArg t ->
    print_node state "TArg";
    print_type_var (state#pad 1 0) t

and print_sum_type state {variants; attributes; _} =
  let variants = Utils.nsepseq_to_list variants in
  let arity    = List.length variants in
  let arity    = if List.is_empty attributes then arity else arity+1 in
  let apply arity rank variant =
    let state = state#pad arity rank in
    print_variant state variant.value in
  let () = List.iteri ~f:(apply arity) variants in
  if not (List.is_empty attributes) then
    let state = state#pad arity (arity-1)
    in print_attributes state attributes

and print_type_constr_arg state = function
  CArg  t -> print_type_expr state t
| CArgTuple t -> print_arg_tuple state t

and print_arg_tuple state node =
  let {value={inside; _}; _} = node in
  let args = Utils.nsepseq_to_list inside in
  let arity = List.length args in
  let apply len rank = print_type_expr (state#pad len rank)
  in List.iteri ~f:(apply arity) args

and print_field_decl state {value; _} =
  let arity = if List.is_empty value.attributes then 1 else 2 in
  print_ident     state value.field_name;
  print_type_expr (state#pad arity 0) value.field_type;
  if not (List.is_empty value.attributes) then
    print_attributes (state#pad arity 1) value.attributes

and print_cartesian state t_exprs =
  let t_exprs        = Utils.nsepseq_to_list t_exprs in
  let arity          = List.length t_exprs in
  let apply len rank = print_type_expr (state#pad len rank)
  in List.iteri ~f:(apply arity) t_exprs

and print_variant state {constr; arg; attributes=attr} =
  let arity = if List.is_empty attr then 0 else 1 in
  let arity = if Option.is_none arg then arity else arity + 1 in
  let rank  = 0 in
  let () = print_ident state constr in
  let rank =
    match arg with
      None -> rank
    | Some (_,c) ->
        print_type_expr (state#pad arity rank) c; rank+1 in
  let () = if not (List.is_empty attr) then
             print_attributes (state#pad arity rank) attr
  in ()

(* PRINTING (client-slide) *)

type ('src, 'dst) printer = Tree.state -> 'src -> 'dst

let print_to_buffer state cst = print_cst state cst; state#buffer

let print_to_string state cst =
  Buffer.contents (print_to_buffer state cst)

let print_pattern_to_string state pattern =
  print_pattern state pattern; Buffer.contents state#buffer

(* Aliases *)

let to_buffer = print_to_buffer
let to_string = print_to_string
let pattern_to_string = print_pattern_to_string
