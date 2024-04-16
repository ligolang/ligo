(* This configuration interface gathers information that is not
   directly provided by the CLI (command-line options) and is specific
   to a given LIGO concrete syntax. *)

module type S =
  sig
    type block_comment_delimiters = <opening : string; closing : string>
    type line_comment_delimiter   = string (* Start of a line comment *)
    type string_delimiter         = string
    type verbatim_delimiters      = <opening : string; closing : string>

    val block     : block_comment_delimiters option
    val line      : line_comment_delimiter option
    val string    : string_delimiter option
    val verbatim  : verbatim_delimiters option
    val file_ext  : string option (* File extension *)

    type file_name   = string
    type module_name = string

    val mk_module : file_name -> module_name -> string
  end
