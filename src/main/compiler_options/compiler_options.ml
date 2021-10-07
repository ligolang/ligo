open Environment

type t = {
  init_env : Ast_typed.environment ;
  infer : bool ;
  libs : string list ;
  test : bool ;
  protocol_version : Protocols.t
}
let make : 
  ?init_env:Ast_typed.environment -> 
  ?infer : bool ->
  ?libs:string list ->
  ?protocol_version:Protocols.t ->
  ?test:bool -> unit -> t =
  fun 
    ?(init_env = default Protocols.current)
    ?(infer = false)
      ?(libs = ([]:string list))
      ?(protocol_version=Protocols.current)
      ?(test = false) () ->
    { init_env; infer; libs ; protocol_version; test }
