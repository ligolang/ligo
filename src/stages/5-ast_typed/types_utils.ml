include Stage_common.Types

type 'a location_wrap = 'a Location.wrap

type expression_
and expression_variable = expression_ Var.t location_wrap
type type_
and type_variable = type_ Var.t
type z = Z.t

type 'a list_ne = 'a List.Ne.t
type inline = bool

type 'a extra_info__comparable = {
  compare : 'a -> 'a -> int ;
}
