module SMap = Ligo_helpers.X_map.String

type name = string
type type_name = string

type 'a name_map = 'a SMap.t
type 'a type_name_map = 'a SMap.t

type program = declaration list

and declaration =
  | Type_declaration of named_type_expression
  | Constant_declaration of named_expression
  (* | Macro_declaration of macro_declaration *)

and annotated_expression = {
  expression: expression ;
  type_annotation: te option ;
}

and named_expression = {
  name: name ;
  annotated_expression: ae ;
}

and named_type_expression = {
  type_name: type_name ;
  type_expression: type_expression ;
}

and te = type_expression
and ae = annotated_expression
and te_map = type_expression type_name_map
and e_map = expression name_map

and type_expression =
  | Type_tuple of te list
  | Type_sum of te_map
  | Type_record of te_map
  | Type_variable of type_name
  | Type_constant of type_name * te list

and expression =
  | Literal of literal
  | Constant of name * ae list (* For language constants, like (Cons hd tl) or (plus i j) *)
  | Variable of name
  | Tuple of ae list
  | Constructor of name * ae list (* For user defined constructors *)
  | Lambda of {
      binder: name ;
      input_type: type_expression ;
      output_type: type_expression ;
      body: block ;
    }

and literal =
  | Bool of bool
  | Number of int
  | String of string
  | Bytes of bytes

and block = instruction list
and b = block

and instruction =
  | Assignment of named_expression
  | Matching of matching
  | Loop of ae * b
  | Skip
  | Fail of ae

and matching =
  | Match_bool of {
      match_true : b ;
      match_false : b ;
    }
  | Match_list of {
      match_nil : b ;
      match_cons : name * name * b ;
    }
  | Match_option of {
      match_none : b ;
      match_some : name * b ;
    }
  | Match_tuple of (name * b) list
