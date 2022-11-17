type t

val pp : no_color:bool -> Format.formatter -> t -> unit
val diff : Type.t -> Type.t -> t
