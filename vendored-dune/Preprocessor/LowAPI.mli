(* The low-level API of the preprocessor

  This preprocessor recognises the following directives:

  "#define", "#elif", "#else", "#endif", "#error", "#if", "#include"
  and "#undef".

   Those are derived from the C# preprocessor.

   Note that unknown directives will be treated like plain text
   instead of raising an error.

   Strings and comments are only recognised in text areas that are to
   be copied.

   Comments can be chosen from the following list:
      * "(*" and "*)" for blocks and "//" for lines;
      * "/*" and "*/" for blocks and "//" for lines;
      * "/*" and "*/" for blocks and "#" for lines;
   or any combination of the above. For other markers, you need to
   modify API.mll.

   In case of success, a buffer containing the preprocessed input is
   returned, together with the list of imported modules and their
   locations on the file system. In case of an error, we return the
   preprocessed buffer so far, if any, and an error message. *)

(* Vendor dependencies *)

module Region = Simple_utils.Region

(* The functor *)

type file_path   = string
type module_name = string
type module_deps = (file_path * module_name) list

type text    = Buffer.t
type success = text * module_deps
type message = string Region.reg
type error   = text option * message
type nonrec result = (success, error) result

type 'src preprocessor = 'src -> result

module type S =
  sig
    (* Preprocessing from various sources *)

    val from_lexbuf  : Lexing.lexbuf preprocessor
    val from_channel :    in_channel preprocessor
    val from_string  :        string preprocessor
    val from_file    :     file_path preprocessor
    val from_buffer  :      Buffer.t preprocessor
  end

module Make (Config : Config.S) (Options : Options.S) : S
