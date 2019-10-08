open Simple_utils

type 'a name = {
  name : string ;
  content : 'a ;
}

(*
val make_name : string -> 'a -> 'a name
val destruct : 'a name -> ( string * 'a )
val get_name : 'a name -> string
val get_content : 'a name -> 'a
*)

module Token = Lex.Token
type token = Token.token

module O : sig

  type list_mode =
    | Trail of token
    | Trail_option of token
    | Trail_force of token
    | Trail_force_ne of token
    | Lead of token
    | Lead_ne of token
    | Separated of token
    | Separated_ne of token
    | Separated_nene of token
    | Naked
    | Naked_ne

  type 'a list_element = list_mode * 'a

  type rhs_element = [
    | `Named of string
    | `Token of token
    | `List of string list_element
    | `Option of string
  ]

  type rhs = rhs_element list name
  type rule = rhs list name

  type manual_rule_content = {
    menhir_codes : string list ;
    ast_code : string ;
  }
  type manual_rule = manual_rule_content name

  type singleton =
    | Manual of manual_rule
    | Generated of rule

  type name_element = [
    | `Named of string
    | `Current
    | `Lower
  ]

  type element = [
    | `Named of string
    | `Token of token
    | `List of name_element list_element
    | `Current
    | `Lower
  ]

  type operator = element list
  type n_operator = operator name

  type n_operators = n_operator list
  type level = n_operators name
  type level_list = level list
  type levels = level List.Ne.t

  type hierarchy = {
    prefix : string ;
    levels : levels ;
    auxiliary_rules : rule list ;
  }
  type n_hierarchy = hierarchy name
  val make_hierarchy : string -> levels -> rule list -> hierarchy

  type language = {
    entry_point : string ;
    singletons : singleton list ;
    hierarchies : n_hierarchy list ;
  }

  val get_op : n_operator -> operator
  (*
  val manual_singleton : string -> string list -> string -> singleton
  val rule_singleton : rule -> singleton
  val language : string -> singleton list -> n_hierarchy list -> language
  val name_hierarchy : string -> string -> n_operators list -> rule list -> n_hierarchy
  *)

end

module Check : sig
  open O

  val well_formed : language -> unit
  val associativity : language -> unit

end


(*
val make_constructor : Format.formatter -> (string * string) -> unit
val make_operator : Format.formatter -> (string * string) -> unit
*)

module Print_AST : sig
  (*
  open Format
  val manual_rule : formatter -> O.manual_rule -> unit
  val generated_rule : formatter -> O.rule -> unit
  val singleton : formatter -> O.singleton -> unit
  val singletons : formatter -> O.singleton list -> unit
  val n_operator : string -> string -> formatter -> O.n_operator -> unit
  val n_hierarchy : string -> formatter -> O.n_hierarchy -> unit
  val n_hierarchies : bool -> formatter -> O.n_hierarchy list -> unit
  val language : formatter -> O.language -> unit
  *)
end

module Print_Grammar : sig
  (*
  open Format
  val letters : string array
  val manual_rule : formatter -> O.manual_rule -> unit
  val generated_rule : formatter -> O.rule -> unit
  val singleton : formatter -> O.singleton -> unit
  val n_operator_rule : string -> string -> formatter -> O.n_operator -> unit
  val n_operator_code : string -> formatter -> O.n_operator -> unit
  val n_operator : string -> string -> string -> formatter -> O.n_operator -> unit 
  val level : string -> string -> formatter -> O.level -> unit
  val n_hierarchy : formatter -> O.n_hierarchy -> unit
  val language : formatter -> O.language -> unit
  *)
end

(*
val infix : string -> [`Left | `Right] -> token -> O.n_operator
(* Ocaml is bad *)
val empty_infix : string -> [`Left | `Right] -> O.n_operator
val paren : string -> string -> O.n_operator
val expression_name : string
val type_expression_name : string
val restricted_type_expression_name : string
val program_name : string
val variable_name : string
val pattern_name : string
val constructor_name : string
val int_name : string
val tz_name : string
val unit_name : string
val string_name : string
val variable : O.singleton
val int : O.singleton
val tz : O.singleton
val unit : O.singleton
val string : O.singleton
val constructor : O.singleton
*)

module Pattern : sig
  (*
  val application : O.n_operator
  val data_structure : O.n_operator
  val record_element : O.rule
  val record : O.n_operator
  val pair : O.n_operator
  val type_annotation : [> `Current | `Named of string | `Token of token ] list name
  val variable : O.n_operator
  val constructor : O.n_operator
  val module_ident : O.n_operator
  val unit : O.n_operator
  val restricted_pattern_name : string
  val restricted_pattern : O.n_hierarchy
  val main : O.n_hierarchy
  val singletons : O.singleton list
  *)
end

module Expression : sig
  (*
  val application : O.n_operator
  val type_annotation : [> `Current | `Named of string | `Token of token ] list name
  val data_structure : O.n_operator
  val fun_ : O.n_operator
  val let_in : O.n_operator
  val no_seq_name : string
  val no_match_name : string
  val record_element : O.rule
  val record : O.n_operator
  val ite : O.n_operator
  val it : O.n_operator

  (* let sequence = infix "sequence" `Left SEMICOLON *)
  val sequence : [> `List of O.list_mode * [> `Lower ] ] list name
  val match_clause : [> `Named of string | `Token of token ] list name list name
  val match_with : [> `Current 
    | `List of O.list_mode * [> `Named of string ] 
    | `Token of token ] list name
  val lt : O.n_operator
  val le : O.n_operator
  val gt : O.n_operator
  val eq : O.n_operator
  val neq : O.n_operator
  val cons : O.n_operator
  val addition : O.n_operator
  val substraction : O.n_operator
  val multiplication : O.n_operator
  val division : O.n_operator
  val arith_variable : O.n_operator
  val int : O.n_operator
  val tz : O.n_operator
  val unit : O.n_operator
  val string : O.n_operator
  val constructor : O.n_operator
  val module_ident : O.n_operator
  *)
  val access : O.n_operator
  (*
  val accessor : O.n_operator
  val assignment : O.n_operator
  val tuple : [> `List of O.list_mode * [> `Lower ] ] list name
  val name : [> `Current | `Token of token ] list name
  val main_hierarchy_name : string
  val main_hierarchy : O.n_hierarchy
  val no_sequence_expression : O.n_hierarchy
  val no_match_expression : O.n_hierarchy
  val expression : O.n_hierarchy
  val singletons : O.singleton list
  *)
end

module Type_expression : sig

  (*
  val record_element : O.rule
  val record : O.n_operator
  val application : O.n_operator
  val tuple : [> `List of O.list_mode * [> `Lower ] ] list name
  val type_variable : O.n_operator
  val restricted_type_expression : O.n_hierarchy
  val type_expression : O.n_hierarchy
  val singletons : O.singleton list
  *)

end

module Program : sig

  (*
  val statement_name : string
  val program : O.rule
  val param_name : string
  val param : O.rule
  val type_annotation_name : string
  val type_annotation : O.rule
  val let_content_name : string
  val let_content : O.rule
  val statement : O.rule
  val singletons : O.singleton list
  *)

end

(*
val language : O.language
*)
