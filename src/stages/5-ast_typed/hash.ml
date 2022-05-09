include Stage_common.Types

type ast_core_type_expression = [%import: Types.ast_core_type_expression]

and type_meta = [%import: Types.type_meta]

type type_expression = [%import: Types.type_expression]

and type_content = [%import: Types.type_content]

and type_injection = [%import: Types.type_injection]

and rows = [%import: Types.rows]

and ty_expr = [%import: Types.ty_expr]

and row_element = [%import: Types.row_element]
[@@deriving hash]
