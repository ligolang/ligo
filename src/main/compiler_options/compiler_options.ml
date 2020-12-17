open Environment

type t = {
  init_env : Ast_typed.environment ;
  typer_switch : Ast_typed.typer_switch ;
  libs : string list ;
  protocol_version : Protocols.t
}
let make : ?init_env:
  Ast_typed.environment -> ?typer_switch:Ast_typed.typer_switch -> ?libs:string list -> ?protocol_version:Protocols.t -> unit -> t =
  fun ?(init_env = default Protocols.current)
      ?(typer_switch = Ast_typed.Old)
      ?(libs = ([]:string list))
      ?(protocol_version=Protocols.current) () ->
    { init_env ; typer_switch ; libs ; protocol_version }