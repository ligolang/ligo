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
  amount : string option ;
  balance : string option ;
  sender : string option ;
  source : string option ;
  now : string option ;
  parameter : string option ;
  storage : string option ;
  expression : string option ;
  init_file : string option ;
  with_types : bool ;
  libs : string list ;
  self_pass : bool ;
  optimize : string option ;
  without_run : bool ;
  package_name : string option ;
  cache_path : string option ;
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
 ?amount
 ?balance
 ?sender
 ?source
 ?now
 ?parameter
 ?storage
 ?expression
 ?init_file
 ?(with_types = false)
 ?(libs = [])
 ?(self_pass = false)
 ?optimize
 ?(without_run = false)
 ?package_name
 ?cache_path
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
  amount ;
  balance ;
  sender ;
  source ;
  now ;
  parameter ;
  storage ;
  expression ;
  init_file ;
  with_types ;
  libs ;
  self_pass ;
  optimize ;
  without_run ;
  package_name ;
  cache_path ;
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
