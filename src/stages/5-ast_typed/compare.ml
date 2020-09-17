open Types
open Compare_enum

let cmp2 f a1 b1 g a2 b2 = match f a1 b1 with 0 -> g a2 b2 | c -> c
let cmp3 f a1 b1 g a2 b2 h a3 b3 = match f a1 b1 with 0 -> (match g a2 b2 with 0 -> h a3 b3 | c -> c) | c -> c
let cmp4 f a1 b1 g a2 b2 h a3 b3 i a4 b4 = match f a1 b1 with 0 -> (match g a2 b2 with 0 -> (match h a3 b3 with 0 -> i a4 b4 | c -> c) | c -> c) | c -> c

let compare_lmap_entry  compare (Label na, va) (Label nb, vb) = cmp2 String.compare na nb compare va vb
let compare_tvmap_entry compare (tva, va) (tvb, vb) = cmp2 Var.compare tva tvb compare va vb

let bool a b = (Stdlib.compare : bool -> bool -> int) a b
let label (Label a) (Label b) = String.compare a b
let label_map ~compare lma lmb =
  let ra = LMap.to_kv_list lma in
  let rb = LMap.to_kv_list lmb in
  let aux (la,a) (lb,b) =
    cmp2 label la lb compare a b in
  List.compare ~compare:aux ra rb

let typeVariableMap compare a b = List.compare ~compare:(compare_tvmap_entry compare) a b

let type_variable = Var.compare
let expression_variable = Location.compare_wrap ~compare:Var.compare

let type_expression_tag ty_expr =
  match ty_expr.type_content with
    T_variable _ -> 1
  | T_constant _ -> 2
  | T_sum      _ -> 3
  | T_record   _ -> 4
  | T_arrow    _ -> 5
  (* TODO: remove this when we remove the old typer *)
  | T_wildcard   -> 6

let rec type_expression a b =
  match a.type_content,b.type_content with
    T_variable a, T_variable b -> type_variable a b
  | T_constant a, T_constant b -> type_operator a b
  | T_sum      a, T_sum      b -> label_map ~compare:row a b
  | T_record   a, T_record   b -> label_map ~compare:row a b
  | T_arrow    a, T_arrow    b -> arrow a b
  | T_wildcard, _ -> 0
  | _, T_wildcard -> 0
  | (T_variable _| T_constant _| T_sum _| T_record _| T_arrow _),
    (T_variable _| T_constant _| T_sum _| T_record _| T_arrow _) ->
    Int.compare (type_expression_tag a) (type_expression_tag b)

and type_operator {type_constant=tca;arguments=la} {type_constant=tcb;arguments=lb} =
  cmp2
    type_constant tca tcb
    (List.compare ~compare:type_expression) la lb

and row {associated_type=aa;michelson_annotation=ma;decl_pos=da} {associated_type=ab;michelson_annotation=mb;decl_pos=db} =
  cmp3
    type_expression aa ab
    (Option.compare String.compare) ma mb
    Int.compare     da db

and arrow {type1=ta1;type2=tb1} {type1=ta2;type2=tb2} =
  cmp2
    type_expression ta1 ta2
    type_expression tb1 tb2

let binder (va,ta) (vb,tb) =
  cmp2 expression_variable va vb type_expression ta tb

let expression_tag expr =
  match expr.expression_content with
    E_literal         _ -> 1
  | E_constant        _ -> 2
  | E_variable        _ -> 3
  | E_application     _ -> 4
  | E_lambda          _ -> 5
  | E_recursive       _ -> 6
  | E_let_in          _ -> 7
  | E_raw_code        _ -> 8
  (* Variant *)
  | E_constructor     _ -> 9
  | E_matching        _ -> 10
  (* Record *)
  | E_record          _ -> 11
  | E_record_accessor _ -> 12
  | E_record_update   _ -> 13

let rec expression a b =
  match a.expression_content,b.expression_content with
    E_literal  a, E_literal  b -> compare a b
  | E_constant a, E_constant b -> constant a b
  | E_variable a, E_variable b -> expression_variable a b
  | E_application a, E_application b -> application a b
  | E_lambda a, E_lambda b -> lambda a b
  | E_recursive a, E_recursive b -> recursive a b
  | E_let_in a, E_let_in b -> let_in a b
  | E_raw_code a, E_raw_code b -> raw_code a b
  | E_constructor a, E_constructor b -> constructor a b
  | E_matching a, E_matching b -> matching a b
  | E_record a, E_record b -> record a b
  | E_record_accessor a, E_record_accessor b -> record_accessor a b
  | E_record_update  a, E_record_update b -> record_update a b
  | (E_literal _| E_constant _| E_variable _| E_application _| E_lambda _| E_recursive _| E_let_in _| E_raw_code _| E_constructor _| E_matching _| E_record _| E_record_accessor _| E_record_update _),
    (E_literal _| E_constant _| E_variable _| E_application _| E_lambda _| E_recursive _| E_let_in _| E_raw_code _| E_constructor _| E_matching _| E_record _| E_record_accessor _| E_record_update _) ->
    Int.compare (expression_tag a) (expression_tag b)

and constant ({cons_name=ca;arguments=a}: constant) ({cons_name=cb;arguments=b}: constant) =
  cmp2 constant' ca cb (List.compare ~compare:expression) a b

and application ({lamb=la;args=a}) ({lamb=lb;args=b}) =
  cmp2 expression la lb expression a b

and lambda ({binder=ba;result=ra}) ({binder=bb;result=rb}) =
  cmp2 expression_variable ba bb expression ra rb

and recursive ({fun_name=fna;fun_type=fta;lambda=la}) {fun_name=fnb;fun_type=ftb;lambda=lb} =
  cmp3 
    expression_variable fna fnb
    type_expression     fta ftb
    lambda               la  lb

and let_in {let_binder=ba;rhs=ra;let_result=la;inline=ia} {let_binder=bb;rhs=rb;let_result=lb;inline=ib} =
  cmp4
    expression_variable ba bb
    expression ra rb
    expression la lb
    bool ia ib

and raw_code {language=la;code=ca} {language=lb;code=cb} =
  cmp2
    String.compare la lb
    expression     ca cb

and constructor {constructor=ca;element=ea} {constructor=cb;element=eb} =
  cmp2
    label ca cb
    expression ea eb

and matching {matchee=ma;cases=ca} {matchee=mb;cases=cb} =
  cmp2
    expression ma mb
    matching_expr ca cb

and record ra rb = label_map ~compare:expression ra rb

and record_accessor {record=ra;path=pa} {record=rb;path=pb} =
  cmp2
    expression ra rb
    label pa pb

and record_update {record=ra;path=pa;update=ua} {record=rb;path=pb;update=ub} =
  cmp3
    expression ra rb
    label pa pb
    expression ua ub

and matching_expr_tag = function
  Match_list    _ -> 1
| Match_option  _ -> 2
| Match_variant _ -> 3

and matching_expr a b =
  match (a,b) with
    Match_list    a, Match_list    b -> matching_content_list a b
  | Match_option  a, Match_option  b -> matching_content_option a b
  | Match_variant a, Match_variant b -> matching_content_variant a b
  | (Match_list _| Match_option _| Match_variant _),
    (Match_list _| Match_option _| Match_variant _) ->
    Int.compare (matching_expr_tag a) (matching_expr_tag b)

and matching_content_cons {hd=ha;tl=ta;body=ba;tv=va} {hd=hb;tl=tb;body=bb;tv=vb} =
  cmp4
    expression_variable ha hb
    expression_variable ta tb
    expression      ba bb
    type_expression va vb

and matching_content_list {match_nil=na;match_cons=ca} {match_nil=nb;match_cons=cb} =
  cmp2
    expression na nb
    matching_content_cons ca cb

and matching_content_some {opt=oa;body=ba;tv=ta} {opt=ob;body=bb;tv=tb} =
  cmp3
    expression_variable oa ob
    expression ba bb
    type_expression ta tb

and matching_content_option {match_none=na;match_some=sa} {match_none=nb;match_some=sb} =
  cmp2
    expression na nb
    matching_content_some sa sb

and matching_content_case {constructor=ca;pattern=pa;body=ba} {constructor=cb;pattern=pb;body=bb} =
  cmp3
    label ca cb
    expression_variable pa pb
    expression ba bb

and matching_content_variant {cases=ca;tv=ta} {cases=cb;tv=tb} =
  cmp2
    (List.compare ~compare:matching_content_case) ca cb
    type_expression ta tb


let declaration_tag = function
  | Declaration_constant _ -> 1
  | Declaration_type     _ -> 2

let declaration_constant {binder=ba;expr=ea;inline=ia} {binder=bb;expr=eb;inline=ib} =
  cmp3
    expression_variable ba bb
    expression ea eb
    bool ia ib

let declaration_type {type_binder=tba;type_expr=tea} {type_binder=tbb;type_expr=teb} =
  cmp2
    type_variable tba tbb
    type_expression tea teb

let declaration a b = 
  match (a,b) with
    Declaration_constant a, Declaration_constant b -> declaration_constant a b
  | Declaration_type     a, Declaration_type     b -> declaration_type a b
  | (Declaration_constant _| Declaration_type _),
    (Declaration_constant _| Declaration_type _) ->
    Int.compare (declaration_tag a) (declaration_tag b)

let program = List.compare ~compare:(Location.compare_wrap ~compare:declaration)

(* Environment *)
let free_variables = List.compare ~compare:expression_variable

let type_environment_binding {type_variable=va;type_=ta} {type_variable=vb;type_=tb} =
  cmp2
    type_variable va vb
    type_expression ta tb

let type_environment = List.compare ~compare:type_environment_binding 

let environment_element_definition_declaration {expression=ea;free_variables=fa} {expression=eb;free_variables=fb} =
  cmp2
    expression ea eb
    free_variables fa fb

let environment_element_definition a b = match a,b with
  | ED_binder, ED_declaration _ -> -1
  | ED_binder, ED_binder -> 0
  | ED_declaration _, ED_binder -> 1
  | ED_declaration a, ED_declaration b -> environment_element_definition_declaration a b

let rec environment_element {type_value=ta;source_environment=sa;definition=da} {type_value=tb;source_environment=sb;definition=db} =
  cmp3
    type_expression ta tb
    environment sa sb
    environment_element_definition da db

and environment_binding {expr_var=eva;env_elt=eea} {expr_var=evb;env_elt=eeb} =
  cmp2
    expression_variable eva evb
    environment_element eea eeb

and expression_environment a b = List.compare ~compare:environment_binding a b

and environment {expression_environment=eea;type_environment=tea} {expression_environment=eeb;type_environment=teb} =
  cmp2
   expression_environment eea eeb
   type_environment       tea teb

let named_type_content {type_name=tna;type_value=tva} {type_name=tnb;type_value=tvb} =
  cmp2
    type_variable tna tnb
    type_expression tva tvb

(* Solver types *)

let unionfind a b = 
  let a = UnionFind.Poly2.partitions a in
  let b = UnionFind.Poly2.partitions b in
  List.compare ~compare:(List.compare ~compare:type_variable) a b
