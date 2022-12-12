(* State threaded along the scanning functions of [LowAPI] *)

(* Vendor dependencies *)

module Region = Simple_utils.Region
module Pos    = Simple_utils.Pos

(* The type [mode] defines the two scanning modes of the preprocessor:
   either we copy the current characters or we skip them. *)

type mode = Copy | Skip

(* Trace of directives. We keep track of directives #if, #elif, and #else. *)

type cond  = If of mode | Elif of mode | Else
type trace = cond list

(* The type [state] groups the information that needs to be
   threaded along the scanning functions *)

type file_path = string
type module_name = string
type lexeme = string

class t ?(project_root : file_path option)
        ?(init_env : Env.t option)
        (pos' : Pos.t) =
  object (self)
    val pos = pos'
    method pos = pos
    method set_pos pos = {< pos >}

    method sync lexbuf =
      let lexeme = Lexing.lexeme lexbuf in
      let length = String.length lexeme
      and start  = pos in
      let stop   = start#shift_bytes length in
      let state  = {< pos = stop >}
      and region = Region.make ~start:pos ~stop
      in state, Region.{region; value=lexeme}

    method newline lexbuf =
      let nl     = Lexing.lexeme lexbuf in
      let ()     = Lexing.new_line lexbuf in
      let start  = pos
      and stop   = self#pos#new_line nl in
      let region = Region.make ~start ~stop
      and state  = self#set_pos stop
      in state, Region.{region; value=nl}

    val env = Option.value ~default:Env.empty init_env
    method env = env

    val mode = Copy
    method mode = mode

    val trace = []
    method trace = trace

    val out = Buffer.create 80
    method out = out

    val chans : in_channel list = []
    method chans = chans

    val incl = Filename.dirname pos'#file
    method incl = incl

    val imports : (file_path * module_name) list = []
    method imports = imports

    val ancestors : file_path list = []
    method ancestors = ancestors

    (* Directories *)

    method set_incl dir =
      if String.(dir = ".") then self else {< incl = dir >}

    method push_ancestor path = {< ancestors = path :: ancestors >}

    (* CONDITIONAL DIRECTIVES *)

    method is_copy = Caml.(mode = Copy)

    method reduce_cond =
      let rec reduce = function
                    [] -> Error Error.Dangling_endif
      | If mode::trace -> Ok {< mode; trace >}
      |       _::trace -> reduce trace
      in reduce trace

    method extend cond mode =
      match cond, trace with
        If _,   Elif _::_ -> Error Error.If_follows_elif
      | Else,     Else::_ -> Error Error.Else_follows_else
      | Else,          [] -> Error Error.Dangling_else
      | Elif _,   Else::_ -> Error Error.Elif_follows_else
      | Elif _,        [] -> Error Error.Dangling_elif
      | hd,            tl -> Ok {< trace = hd::tl; mode >}

    method set_trace trace = {< trace >}

    (* MODE *)

    method set_mode mode = {< mode >}

    method last_mode =
      let rec aux = function
                              [] -> Copy (* Should not happen *)
      | (If mode | Elif mode)::_ -> mode
      |                 _::trace -> aux trace
      in aux trace

    (* PRINTING *)

    method copy buffer =
      if self#is_copy then self#print (Lexing.lexeme buffer)

    method copy_nl buffer =
      Lexing.new_line buffer; self#print (Lexing.lexeme buffer)

    method print string = Buffer.add_string out string

    (* SYMBOL ENVIRONMENT *)

    method set_env env      = {< env >}
    method add_symbol id    = {< env = Env.add id env >}
    method remove_symbol id = {< env = Env.remove id env >}

    (* INPUT CHANNELS *)

    method set_chans chans   = {< chans >}
    method push_chan in_chan = {< chans = in_chan :: chans >}

    (* MODULES IMPORTS AND PATH RESOLUTION *)

    method set_imports imports = {< imports >}

    method push_import path imported_module =
      {< imports = (path, imported_module) :: imports >}

    val mod_res = Option.bind ~f:ModRes.make project_root
    method mod_res = mod_res
  end
