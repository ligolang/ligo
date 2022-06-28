module Location    = Simple_utils.Location
module Var         = Simple_utils.Var
module List        = Simple_utils.List
module Ligo_string = Simple_utils.Ligo_string
open Types
open Compare_enum

type 'a comparator = 'a -> 'a -> int
let (<?) ca cb = if ca = 0 then cb () else ca

let cmp2 f a1 b1 g a2 b2 = match f a1 b1 with 0 -> g a2 b2 | c -> c
let cmp3 f a1 b1 g a2 b2 h a3 b3 = match f a1 b1 with 0 -> (match g a2 b2 with 0 -> h a3 b3 | c -> c) | c -> c

let label (Label a) (Label b) = String.compare a b
let label_map ~compare lma lmb =
  let ra = LMap.to_kv_list_rev lma in
  let rb = LMap.to_kv_list_rev lmb in
  let aux (la,a) (lb,b) =
    cmp2 label la lb compare a b in
  List.compare aux ra rb

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
  | T_singleton       _ -> 7
  | T_abstraction     _ -> 8
  | T_for_all         _ -> 9


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
  | T_abstraction a , T_abstraction b -> for_all a b
  | T_for_all a , T_for_all b -> for_all a b
  | (T_variable _| T_constant _| T_sum _| T_record _| T_arrow _ | T_singleton _ | T_abstraction _ | T_for_all _),
    (T_variable _| T_constant _| T_sum _| T_record _| T_arrow _ | T_singleton _ | T_abstraction _ | T_for_all _) ->
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

and constant' = Compare_enum.constant'
