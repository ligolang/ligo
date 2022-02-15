open Environment

type raw = {
  source_file : string option ;
  entry_point : string option ; 
  views : string list ;
  syntax : string ;
  protocol_version : string option ;
  display_format : Simple_utils.Display.ex_display_format ;
  disable_typecheck : bool ;
  output_file : string option ;
  warn : bool ;
  werror : bool ;
  project_root : string option ;
  new_syntax : string option ;
  new_dialect : string option ;
  seed : int option ;
  generator : string option ;
  steps : int option ;
}

let make_raw
 ?source_file
 ?entry_point
 ?(views=[])
 ~syntax
 ?protocol_version
 ~display_format
 ?(disable_typecheck=false)
 ?output_file
 ?(warn=false)
 ?(werror=false)
 ?project_root
 ?new_syntax
 ?new_dialect
 ?seed
 ?generator
 ?steps
 ()
  = {
  source_file ;
  entry_point ;
  views ;
  syntax ;
  protocol_version ;
  display_format ;
  disable_typecheck ;
  output_file ;
  warn ;
  werror ;
  project_root ;
  new_syntax ;
  new_dialect ;
  seed ;
  generator ;
  steps ;
}

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
