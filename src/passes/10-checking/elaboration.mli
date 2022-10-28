open Ligo_prim
module Location = Simple_utils.Location
open Simple_utils.Trace

type 'a t
type error = [ `Typer_cannot_decode_texists of Type.t * Location.t ]

include Monad.S with type 'a t := 'a t

val all_lmap : 'a t Record.LMap.t -> 'a Record.LMap.t t
val all_lmap_unit : unit t Record.LMap.t -> unit t

include module type of Let_syntax

val decode : Type.t -> Ast_typed.type_expression t

val run
  :  'a t
  -> raise:(([> error ] as 'err), 'wrn) raise
  -> Substitution.t
  -> 'a
