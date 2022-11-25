type t

val pp
  :  no_color:bool
  -> tbl:Type.Type_var_name_tbl.t
  -> Format.formatter
  -> t
  -> unit

val diff : Type.t -> Type.t -> t
