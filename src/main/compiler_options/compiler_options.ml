open Environment

type frontend = {
  syntax : string ;
  dialect : string ; (* this does not exist *)  
  libs : string list ;
  project_root : string option ;
}

type middle_end = {
  infer : bool ;
  test : bool ;
  init_env : Environment.t ;
}

type backend = {
  protocol_version : Protocols.t ;
}

type t = {
  frontend : frontend ;
  middle_end : middle_end ;
  backend : backend ;
}

let make : 
  ?infer : bool ->
  ?libs:string list ->
  ?protocol_version:Protocols.t ->
  ?test:bool -> 
  ?project_root:string -> unit -> t =
  fun 
    ?(infer = false)
    ?(libs = ([]:string list))
    ?(protocol_version=Protocols.current)
    ?(test = false) 
    ?(project_root) () ->
      let frontend = {
        syntax = "" ;
        dialect = "" ;
        libs;
        project_root
      } in
      let middle_end = {
        infer ;
        test ;
        init_env = if test then default_with_test protocol_version else default protocol_version
      } in
      let backend = {
        protocol_version ;
      } in
      { 
        frontend ;
        middle_end ;
        backend ;
      }
