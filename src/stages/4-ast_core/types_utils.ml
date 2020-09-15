
include Stage_common.Types

type location = Location.t
type 'a location_wrap = 'a Location.wrap 

type expression_
type expression_variable =  expression_ Var.t location_wrap
let expression_variable_to_yojson var = Location.wrap_to_yojson (Var.to_yojson) var
let expression_variable_of_yojson var = Location.wrap_of_yojson (Var.of_yojson) var

type type_
and type_variable = type_ Var.t
let type_variable_to_yojson var = Var.to_yojson var
let type_variable_of_yojson var = Var.of_yojson var

type sugar_type_expression_option = Ast_sugar.type_expression option
type sugar_expression_option = Ast_sugar.expression option
