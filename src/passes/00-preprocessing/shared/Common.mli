(* Interfacing the preprocessor with the LIGO compiler depending on
   the concrete syntax *)

(* Vendors dependencies *)

module Config = Preprocessor.Config
module LowAPI = Preprocessor.LowAPI

(* Functor *)

module type S =
  sig
    (* Some inputs *)

    type file_path = string
    type dirs = file_path list

    (* Preprocessor types and their results *)

    module Errors = Errors

    type nonrec result = (LowAPI.success, Errors.t) result

    type 'src preprocessor =
      ?project_root:file_path -> dirs -> 'src -> result

    (* Preprocessing various sources *)

    val from_file    :    file_path preprocessor
    val from_string  :       string preprocessor
    val from_buffer  :     Buffer.t preprocessor
    val from_channel : In_channel.t preprocessor

    (* Aliases *)

    val preprocess_file    :    file_path preprocessor
    val preprocess_string  :       string preprocessor
    val preprocess_buffer  :     Buffer.t preprocessor
    val preprocess_channel : In_channel.t preprocessor
  end

module Make (Config : Config.S) : S
