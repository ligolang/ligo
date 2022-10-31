(* Data stored in strings destined to [stdout] and [stderr] *)

type buffer

val empty_buffer : init:int -> buffer


type t = {out : buffer; err : buffer}

type std = t

val empty : t

val add_line : buffer -> string -> unit
val add_nl   : buffer -> unit

val add_string : buffer -> string -> unit
val add_char   : buffer -> char -> unit

val string_of : buffer -> string

val redden : string -> string
