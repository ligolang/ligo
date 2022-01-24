open Environment

type t = {
  init_env : Environment.t ;
  infer : bool ;
  libs : string list ;
  test : bool ;
  protocol_version : Protocols.t ;
  project_root : string option ;
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
      { init_env = if test then default_with_test protocol_version else default protocol_version;
        infer;
        libs ;
        protocol_version;
        test ;
        project_root;
      }
