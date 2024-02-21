(* Interfacing the preprocessor with the LIGO compiler *)

(* Vendors dependencies *)

module Config = Preprocessor.Config
module LowAPI = Preprocessor.LowAPI
module CLI    = Preprocessor.CLI

(* PREPROCESSING *)

module type S =
  sig
     (* Some inputs *)

    type file_path = string
    type dirs = file_path list

    (* Preprocessor types and their results *)

    module Errors = Errors

    type nonrec result = (LowAPI.success, Errors.t) result

    type 'src preprocessor =
      ?project_root:file_path -> preprocess_define:string list -> dirs -> 'src -> result

    (* Preprocessing various sources *)

    val from_file      :            file_path preprocessor
    val from_string    :               string preprocessor
    val from_raw_input : (file_path * string) preprocessor
    val from_buffer    :             Buffer.t preprocessor
    val from_channel   :         In_channel.t preprocessor


    (* Aliases *)

    val preprocess_file      :            file_path preprocessor
    val preprocess_string    :               string preprocessor
    val preprocess_raw_input : (file_path * string) preprocessor
    val preprocess_buffer    :             Buffer.t preprocessor
    val preprocess_channel   :         In_channel.t preprocessor
  end

module Make (Config : Config.S) =
  struct
     (* Some inputs *)

    type file_path = string
    type dirs = file_path list

    (* Preprocessor types and their results *)

    module Errors = Errors

    type nonrec result = (LowAPI.success, Errors.t) result

    type 'src preprocessor =
      ?project_root:file_path -> preprocess_define:string list -> dirs -> 'src -> result

    (* Postlude (THIS FUNCTION IS THE ONLY ONE IMPURE) *)

    let finalise show_pp = function
      Error (_, msg) ->
        Error (Errors.generic msg)
    | Ok (text, deps) ->
        if show_pp then
          Printf.printf "%s\n%!" @@ Buffer.contents text else ();
        Ok (text, deps)

    (* Default parameters for the preprocessor *)

    module Default = CLI.MakeDefault (Config)

    (* Preprocessing a file *)

    let from_file ?project_root:project_root' ~preprocess_define dirs' file_path =
      let module Options =
        struct
          include Default.Options
          (* We shadow some default settings *)
          let input        = Some file_path
          let project_root = project_root'
          let dirs         = dirs'
          let define       = preprocess_define
        end in
      let open LowAPI.Make (Default.Config) (Options)
      in finalise Default.Options.show_pp @@ from_file file_path
    let preprocess_file = from_file

    (* Preprocessing a string *)

    let from_string ?project_root:project_root' ~preprocess_define dirs' string =
      let module Options =
        struct
          include Default.Options
          (* We shadow some default settings *)
          let project_root = project_root'
          let dirs         = dirs'
          let define       = preprocess_define
        end in
      let open LowAPI.Make (Config) (Options)
      in finalise Options.show_pp @@ from_string string
    let preprocess_string = from_string

    (* Preprocessing a raw input (from lsp mostly) *)

    let from_raw_input ?project_root:project_root' ~preprocess_define dirs' (file, string) =
      let module Options =
        struct
          include Default.Options
          (* We shadow some default settings *)
          let project_root = project_root'
          let dirs         = dirs'
          let define       = preprocess_define
        end in
      let open LowAPI.Make (Config) (Options)
      in finalise Options.show_pp @@ from_raw_input (file, string)
    let preprocess_raw_input = from_raw_input

    (* Preprocessing a string buffer *)

    let from_buffer ?project_root ~preprocess_define dirs buffer =
      from_string ?project_root ~preprocess_define dirs @@ Buffer.contents buffer
    let preprocess_buffer = from_buffer

    (* Preprocessing a channel *)

    let from_channel ?project_root:project_root' ~preprocess_define dirs' channel =
      let module Options =
        struct
          include Default.Options
          (* We shadow some default settings *)
          let project_root = project_root'
          let dirs         = dirs'
          let define       = preprocess_define
        end in
      let open LowAPI.Make (Config) (Options)
      in finalise Options.show_pp @@ from_channel channel
    let preprocess_channel = from_channel
  end
