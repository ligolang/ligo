module Var         = Simple_utils.Var
module List        = Simple_utils.List
module Ligo_string = Simple_utils.Ligo_string
open Types
open Compare_enum

type 'a comparator = 'a -> 'a -> int
let (<?) ca cb = if ca = 0 then cb () else ca

let cmp2 f a1 b1 g a2 b2 = match f a1 b1 with 0 -> g a2 b2 | c -> c
let cmp3 f a1 b1 g a2 b2 h a3 b3 = match f a1 b1 with 0 -> (match g a2 b2 with 0 -> h a3 b3 | c -> c) | c -> c
let cmp4 f a1 b1 g a2 b2 h a3 b3 i a4 b4 = match f a1 b1 with 0 -> (match g a2 b2 with 0 -> (match h a3 b3 with 0 -> i a4 b4 | c -> c) | c -> c) | c -> c
let cmp5 f a1 b1 g a2 b2 h a3 b3 i a4 b4 j a5 b5 = match f a1 b1 with 0 -> (match g a2 b2 with 0 -> (match h a3 b3 with 0 -> (match i a4 b4 with 0 -> j a5 b5 | c -> c) | c -> c) | c -> c) | c -> c
let cmp6 f a1 b1 g a2 b2 h a3 b3 i a4 b4 j a5 b5 k a6 b6 = match f a1 b1 with 0 -> (match g a2 b2 with 0 -> (match h a3 b3 with 0 -> (match i a4 b4 with 0 -> (match j a5 b5 with 0 -> k a6 b6 | c -> c) | c -> c) | c -> c) | c -> c) | c -> c
let cmp8 f a1 b1 g a2 b2 h a3 b3 i a4 b4 j a5 b5 k a6 b6 l a7 b7 m a8 b8 = match f a1 b1 with 0 -> (match g a2 b2 with 0 -> (match h a3 b3 with 0 -> (match i a4 b4 with 0 -> (match j a5 b5 with 0 -> (match k a6 b6 with 0 -> (match l a7 b7 with 0 -> m a8 b8 | c -> c) | c -> c) | c -> c) | c -> c) | c -> c) | c -> c) | c -> c
let cmp9 f a1 b1 g a2 b2 h a3 b3 i a4 b4 j a5 b5 k a6 b6 l a7 b7 m a8 b8 n a9 b9 = match f a1 b1 with 0 -> (match g a2 b2 with 0 -> (match h a3 b3 with 0 -> (match i a4 b4 with 0 -> (match j a5 b5 with 0 -> (match k a6 b6 with 0 -> (match l a7 b7 with 0 -> (match m a8 b8 with 0 -> n a9 b9 | c -> c) | c -> c) | c -> c) | c -> c) | c -> c) | c -> c) | c -> c) | c -> c
let cmp7 f a1 b1 g a2 b2 h a3 b3 i a4 b4 j a5 b5 k a6 b6 l a7 b7 = match f a1 b1 with 0 -> (match g a2 b2 with 0 -> (match h a3 b3 with 0 -> (match i a4 b4 with 0 -> (match j a5 b5 with 0 -> (match k a6 b6 with 0 -> l a7 b7 | c -> c) | c -> c) | c -> c) | c -> c) | c -> c) | c -> c

let cmp_pair f g (a1, a2) (b1, b2) = cmp2 f a1 b1 g a2 b2

let compare_lmap_entry  compare (Label na, va) (Label nb, vb) = cmp2 String.compare na nb compare va vb
let compare_tvmap_entry compare (tva, va) (tvb, vb) = cmp2 TypeVar.compare tva tvb compare va vb

let bool a b = (Stdlib.compare : bool -> bool -> int) a b
let label (Label a) (Label b) = String.compare a b
let label_map ~compare lma lmb =
  let ra = LMap.to_kv_list_rev lma in
  let rb = LMap.to_kv_list_rev lmb in
  let aux (la,a) (lb,b) =
    cmp2 label la lb compare a b in
  List.compare aux ra rb

let typeVariableMap compare a b = List.compare (compare_tvmap_entry compare) a b

let expression_variable = ValueVar.compare
let type_variable       = TypeVar.compare
let module_variable     = ModuleVar.compare

let module_access f {module_path=mna; element=ea}
                    {module_path=mnb; element=eb} =
  cmp2
    (List.compare module_variable) mna mnb
    f ea eb

let layout_tag = function
  | L_comb -> 1
  | L_tree -> 2

let layout a b = Int.compare (layout_tag a) (layout_tag b)

let type_expression_tag ty_cont =
  match ty_cont with
    T_variable        _ -> 1
  | T_constant        _ -> 2
  | T_sum             _ -> 3
  | T_record          _ -> 4
  | T_arrow           _ -> 5
  | T_singleton       _ -> 6
  | T_for_all         _ -> 7

let rec type_expression a b =
  type_content a.type_content b.type_content

and type_content a b =
  match a, b with
    T_variable a, T_variable b -> type_variable a b
  | T_constant a, T_constant b -> injection a b
  | T_sum      a, T_sum      b -> rows a b
  | T_record   a, T_record   b -> rows a b
  | T_arrow    a, T_arrow    b -> arrow a b
  | T_singleton a , T_singleton b -> literal a b
  | T_for_all a , T_for_all b -> for_all a b
  | (T_variable _| T_constant _| T_sum _| T_record _| T_arrow _ | T_singleton _ | T_for_all _),
    (T_variable _| T_constant _| T_sum _| T_record _| T_arrow _ | T_singleton _ | T_for_all _) ->
    Int.compare (type_expression_tag a) (type_expression_tag b)

and injection {language=la ; injection=ia ; parameters=pa} {language=lb ; injection=ib ; parameters=pb} =
  cmp3
    String.compare la lb
    Stage_common.Constant.compare ia ib
    (List.compare type_expression) pa pb

and rows {content=ca; layout=la} {content=cb; layout=lb} =
  cmp2
    (label_map ~compare:row_element) ca cb
    layout la lb

and row_element {associated_type=aa;michelson_annotation=ma;decl_pos=da} {associated_type=ab;michelson_annotation=mb;decl_pos=db} =
  cmp3
    type_expression aa ab
    (Option.compare String.compare) ma mb
    Int.compare     da db

and arrow {type1=ta1;type2=tb1} {type1=ta2;type2=tb2} =
  cmp2
    type_expression ta1 ta2
    type_expression tb1 tb2

and for_all {ty_binder = ba ; kind = _ ; type_ = ta } {ty_binder = bb ; kind = _ ; type_ = tb } =
  cmp2
    type_expression ta tb
    type_variable ba bb

let option f oa ob =
  match oa,ob with
  | None, None -> 0
  | Some _, None -> 1
  | None, Some _ -> -1
  | Some a, Some b -> f a b

let binder ty_expr {var=va;ascr=aa;_} {var=vb;ascr=ab;_} =
  cmp2
    expression_variable va vb
    (option ty_expr) aa ab

let expression_tag expr =
  match expr with
    E_literal         _ -> 1
  | E_constant        _ -> 2
  | E_variable        _ -> 3
  | E_application     _ -> 4
  | E_lambda          _ -> 5
  | E_type_abstraction _ -> 6
  | E_recursive       _ -> 7
  | E_let_in          _ -> 8
  | E_raw_code        _ -> 10
  | E_type_inst       _ -> 11
  (* Variant *)
  | E_constructor     _ -> 14
  | E_matching        _ -> 15
  (* Record *)
  | E_record          _ -> 16
  | E_record_accessor _ -> 17
  | E_record_update   _ -> 18
  | E_assign          _ -> 19

and declaration_tag = function
  | Declaration_constant _ -> 1
  | Declaration_type     _ -> 2
  | Declaration_module   _ -> 3

let rec expression a b =
  expression_content a.expression_content b.expression_content

and expression_content a b =
  match a,b with
    E_literal  a, E_literal  b -> literal a b
  | E_constant a, E_constant b -> constant a b
  | E_variable a, E_variable b -> expression_variable a b
  | E_application a, E_application b -> application a b
  | E_lambda a, E_lambda b -> lambda a b
  | E_type_abstraction a, E_type_abstraction b -> type_abs a b
  | E_recursive a, E_recursive b -> recursive a b
  | E_let_in a, E_let_in b -> let_in a b
  | E_raw_code a, E_raw_code b -> raw_code a b
  | E_constructor a, E_constructor b -> constructor a b
  | E_type_inst a, E_type_inst b -> type_inst a b
  | E_matching a, E_matching b -> matching a b
  | E_record a, E_record b -> record a b
  | E_record_accessor a, E_record_accessor b -> record_accessor a b
  | E_record_update  a, E_record_update b -> record_update a b
  | E_assign a, E_assign b -> assign a b
  | (E_literal _| E_constant _| E_variable _| E_application _| E_lambda _| E_type_abstraction _| E_recursive _| E_let_in _| E_raw_code _| E_constructor _| E_matching _| E_record _| E_record_accessor _| E_record_update _ | E_type_inst _ | E_assign _),
    (E_literal _| E_constant _| E_variable _| E_application _| E_lambda _| E_type_abstraction _| E_recursive _| E_let_in _| E_raw_code _| E_constructor _| E_matching _| E_record _| E_record_accessor _| E_record_update _ | E_type_inst _ | E_assign _) ->
    Int.compare (expression_tag a) (expression_tag b)

and constant ({cons_name=ca;arguments=a}: constant) ({cons_name=cb;arguments=b}: constant) =
  cmp2 constant' ca cb (List.compare expression) a b

and constant' = Compare_enum.constant'

and type_inst ({forall=la;type_=a}) ({forall=lb;type_=b}) =
  cmp2 expression la lb type_expression a b

and application ({lamb=la;args=a}) ({lamb=lb;args=b}) =
  cmp2 expression la lb expression a b

and lambda ({binder=ba;result=ra}) ({binder=bb;result=rb}) =
  cmp2
    (binder type_expression) ba bb
    expression ra rb

and type_abs ({type_binder=ba;result=ra}) ({type_binder=bb;result=rb}) =
  cmp2
    type_variable ba bb
    expression ra rb

and recursive ({fun_name=fna;fun_type=fta;lambda=la}) {fun_name=fnb;fun_type=ftb;lambda=lb} =
  cmp3
    expression_variable fna fnb
    type_expression     fta ftb
    lambda               la  lb

and let_in {let_binder=ba;rhs=ra;let_result=la;attr = { inline=aa;no_mutation=nma;view=va;public=pua;thunk=ta;hidden=ha}} {let_binder=bb;rhs=rb;let_result=lb;attr = { inline=ab;no_mutation=nmb;view=vb;public=pub;thunk=tb;hidden=hb}} =
  cmp9
    (binder type_expression) ba bb
    expression ra rb
    expression la lb
    bool  aa ab
    bool  nma nmb
    bool  va vb
    bool  pua pub
    bool  ta tb
    bool  ha hb

and type_in {type_binder=ba;rhs=ra;let_result=la} {type_binder=bb;rhs=rb;let_result=lb} =
  cmp3
    type_variable ba bb
    type_expression ra rb
    expression la lb

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

and matching_expr_tag = function
| Match_variant _ -> 1
| Match_record _ -> 2

and matching_expr a b =
  match (a,b) with
  | Match_variant a, Match_variant b -> matching_content_variant a b
  | Match_record a, Match_record b -> matching_content_record a b
  | ( Match_variant _ | Match_record _),
    ( Match_variant _ | Match_record _) ->
    Int.compare (matching_expr_tag a) (matching_expr_tag b)

and matching_content_case {constructor=ca;pattern=pa;body=ba} {constructor=cb;pattern=pb;body=bb} =
  cmp3
    label ca cb
    expression_variable pa pb
    expression ba bb

and matching_content_variant {cases=ca;tv=ta} {cases=cb;tv=tb} =
  cmp2
    (List.compare matching_content_case) ca cb
    type_expression ta tb

and matching_content_record
    {fields = fields1; body = body1; tv = t1}
    {fields = fields2; body = body2; tv = t2} =
  cmp3
    (label_map ~compare:(binder type_expression)) fields1 fields2
    expression body1 body2
    type_expression t1 t2
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

and ascription {anno_expr=aa; type_annotation=ta} {anno_expr=ab; type_annotation=tb} =
  cmp2
    expression aa ab
    type_expression ta tb

and assign {binder=ba;expression=ea} {binder=bb;expression=eb} =
  cmp2
        (binder type_expression) ba bb
        expression ea eb

and access_path_tag = function
  | Access_tuple _ -> 1
  | Access_record _ -> 2
  | Access_map _ -> 3

and access_path pa pb = match pa,pb with
  | Access_tuple  a, Access_tuple  b -> Z.compare a b
  | Access_record a, Access_record b -> String.compare a b
  | Access_map    a, Access_map    b -> expression a b
  | (Access_tuple _ | Access_record _ | Access_map _) ,
    (Access_tuple  _ | Access_record _ | Access_map _) ->
    Int.compare (access_path_tag pa) (access_path_tag pb)
