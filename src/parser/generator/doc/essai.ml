type region
type 'a reg
type lexeme = string reg

(* Tokens *)

type integer = [`Integer of lexeme reg]
type natural = [`Natural of lexeme reg]
type ident   = [`Ident of lexeme reg]
type uident  = [`Uident of lexeme reg]
type chr     = [`Chr of lexeme reg]
type str     = [`Str of lexeme reg]

type bool_or  = [`bool_or of lexeme reg]
type bool_and = [`bool_and of lexeme reg]
type lt       = [`lt of lexeme reg]
type le       = [`le of lexeme reg]
type gt       = [`gt of lexeme reg]
type ge       = [`ge of lexeme reg]
type eq       = [`eq of lexeme reg]
type ne       = [`ne of lexeme reg]
type cat      = [`cat of lexeme reg]
type cons     = [`cons of lexeme reg]
type plus     = [`plus of lexeme reg]
type minus    = [`minus of lexeme reg]
type times    = [`times of lexeme reg]
type slash    = [`slash of lexeme reg]
type div      = [`div of lexeme reg]
type kwd_mod  = [`kwd_mod of lexeme reg]
type uminus   = [`uminus of lexeme reg]
type kwd_not  = [`kwd_not of lexeme reg]

type lpar     = [`lpar of lexeme reg]
type rpar     = [`rpar of lexeme reg]
type lbracket = [`lbracket of lexeme reg]
type rbracket = [`rbracket of lexeme reg]
type lbrace   = [`lbrace of lexeme reg]
type rbrace   = [`rbrace of lexeme reg]
type semi     = [`semi of lexeme reg]
type comma    = [`comma of lexeme reg]
type colon    = [`colon of lexeme reg]
type vbar     = [`vbar of lexeme reg]
type arrow    = [`arrow of lexeme reg]
type wild     = [`wild of lexeme reg]

type kwd_and   = [`kwd_and of lexeme reg]
type kwd_begin = [`kwd_begin of lexeme reg]
type kwd_else  = [`kwd_else of lexeme reg]
type kwd_end   = [`kwd_end of lexeme reg]
type kwd_false = [`kwd_false of lexeme reg]
type kwd_fun   = [`kwd_fun of lexeme reg]
type kwd_if    = [`kwd_if of lexeme reg]
type kwd_in    = [`kwd_in of lexeme reg]
type kwd_let   = [`kwd_let of lexeme reg]
type kwd_list  = [`kwd_list of lexeme reg]
type kwd_map   = [`kwd_map of lexeme reg]
type kwd_match = [`kwd_match of lexeme reg]
type kwd_of    = [`kwd_of of lexeme reg]
type kwd_set   = [`kwd_set of lexeme reg]
type kwd_then  = [`kwd_then of lexeme reg]
type kwd_true  = [`kwd_true of lexeme reg]
type kwd_type  = [`kwd_type of lexeme reg]
type kwd_with  = [`kwd_with of lexeme reg]

type token =
  Integer of integer
| Natural of natural
| Ident of ident
| Uident of uident
| Chr of chr
| Str of str
| Bool_or of bool_or
| Bool_and of bool_and
| Lt of lt
| Le of le
| Gt of gt
| Ge of ge
| Eq of eq
| Ne of ne
| Cat of cat
| Cons of cons
| Plus of plus
| Minus of minus
| Times of times
| Slash of slash
| Div of div
| Kwd_mod of kwd_mod
| Uminus of uminus
| Kwd_not of kwd_not
| Lpar of lpar
| Rpar of rpar
| Lbracket of lbracket
| Rbracket of rbracket
| Lbrace of lbrace
| Rbrace of rbrace
| Semi of semi
| Comma of comma
| Colon of colon
| Vbar of vbar
| Arrow of arrow
| Wild of wild
| Kwd_and of kwd_and
| Kwd_begin of kwd_begin
| Kwd_else of kwd_else
| Kwd_end of kwd_end
| Kwd_false of kwd_false
| Kwd_fun of kwd_fun
| Kwd_if of kwd_if
| Kwd_in of kwd_in
| Kwd_let of kwd_let
| Kwd_list of kwd_list
| Kwd_map of kwd_map
| Kwd_match of kwd_match
| Kwd_of of kwd_of
| Kwd_set of kwd_set
| Kwd_then of kwd_then
| Kwd_true of kwd_true
| Kwd_type of kwd_type
| Kwd_with of kwd_with

(* The following are meant to be part of a library *)

type 'item seq = 'item list
type 'item nseq = 'item * 'item seq
type ('item,'sep) nsepseq = 'item * ('sep * 'item) list
type ('item,'sep) sepseq = ('item,'sep) nsepseq option
type ('item,'sep) sep_or_term_list =
  ('item,'sep) nsepseq * 'sep option

(* The following are specific to the present grammar *)

type 'item list_of__rec_0 = {
        lbracket__1 : lbracket;
  list_of__rec_0__2 : ('item, semi) nsepseq;
        rbracket__3 : rbracket
}

type 'item list_of = [`List of 'item list_of__rec_0]

type 'item tuple__rec_0 = {
          item__1 : 'item;
         comma__2 : comma;
  tuple__rec_0__3 : ('item, comma) nsepseq
}

type 'item tuple = [`Tuple of 'item tuple__rec_0]

type 'item par__rec_0 = {
  lpar__1 : lpar;
  item__2 : 'item;
  rpar__3 : rpar
}

type 'item par = [`Par of 'item par__rec_0]

(* Non-recursive value declarations *)

type sub_irrefutable = [
  `P_Var of string
| `P_Wild
| `P_Unit
| closed_irrefutable par
]

and closed_irrefutable = [
  sub_irrefutable tuple
| `P_SubI of sub_irrefutable (* `P_SubI necessary *)
]

type irrefutable = [
  sub_irrefutable tuple
| sub_irrefutable
]

type let_binding__rec_1 = {
               variable__1 : variable;
  sub_irrefutable__nseq__2 : sub_irrefutable nseq;
                     eq__3 : eq;
                   expr__4 : expr
}

type let_binding__rec_2 = {
  irrefutable__1 : irrefutable;
           eq__2 : eq;
         expr__3 : expr
}

type let_binding = [
  `LetFun of let_binding__rec_1
| `LetNonFun of let_binding__rec_2  (* `LetNonFun necessary *)
]

type let_bindings = (let_binding, kwd_and) nsepseq

type let_declarations = {
  kwd_let : kwd_let;
  let_bindings : let_bindings
}

(*
type pattern = [
  `P_Cons of {sub_pattern: sub_pattern; cons: cons; tail: tail}
| `P_Tuple
*)

(* Type declarations *)

type type_name = ident
type field_name = ident
type constr = uident

type type_constr = [
  `T_Constr of ident
| kwd_set
| kwd_map
| kwd_list
]

type record_type = {
  lbrace : lbrace;
  record_type__2 : (field_decl, semi) sep_or_term_list;
  rbrace : rbrace
}

and field_decl = {
  field_name : field_name;
  colon : colon;
  type_expr : type_expr
}

and variant = {
  constr : constr;
  kwd_of : kwd_of;
  cartesian : cartesian
}

and sum_type = {
  vbar_opt : vbar option;
  sum_type__2 : (variant, vbar) nsepseq
}

and type_param__rec_1 = {
  core_type : core_type;
  type_constr : type_constr
}

and type_param = [
  (type_expr, comma) nsepseq par
| `T_App of type_param__rec_1
]

and core_type__rec_1 = {
  type_param : type_param;
  type_constr : type_constr
}

and core_type = [
  `T_Alias of type_name
| `T_App of core_type__rec_1
| cartesian par
]

and fun_type__rec_0 = {
  core_type : core_type;
  arrow : arrow;
  fun_type : fun_type
}

and fun_type = [
  `T_Fun of fun_type__rec_0
| `T_Core of core_type         (* `T_Core necessary *)
]

and cartesian = (fun_type, times) nsepseq

and type_expr = [
  `T_Prod of cartesian
| `T_Sum of sum_type
| `T_Record of record_type
]

type type_declaration = {
   kwd_type__1 : kwd_type;
  type_name__2 : type_name;
         eq__3 : eq;
  type_expr__4 : type_expr
}

(* Entry *)

type statement = [
  `Let of let_declarations
| `TypeDecl of type_declaration
]

type program = statement list
