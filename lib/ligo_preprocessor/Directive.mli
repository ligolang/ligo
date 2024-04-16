(* Scanning of preprocessing directives *)

(* Vendors's dependencies *)

module Region = Simple_utils.Region
module Pos    = Simple_utils.Pos

(* DIRECTIVES *)

type file_path   = string
type module_name = string

(* Like [Wrap.comment], but also stores the delimeters
   making the [pp_comment] possible *)
type block_comment_delimiters = <opening : string; closing : string> option
type line_comment_delimiter   = string option

type block_comment = [`BlockComment of (block_comment_delimiters * string) Region.reg]
type line_comment = [`LineComment of (line_comment_delimiter * string) Region.reg]


type comment = [ block_comment | line_comment ]


type variable      = string
type error_message = string

(* Endings of preprocessing directives *)

type ending = [
  `EOL of State.t * string Region.reg
| `EOF of State.t * Region.t]

(* #include *)

(* The method [region] covers "#include". The method [file_path] is
   the string containing the file to include, with its region. *)

type include_directive = <
  region            : Region.t;
  file_path         : file_path Region.reg;
  trailing_comment  : line_comment option;
    (** Trailing block comments like [#include "x" (* y *)] are not supported *)
  previous_comments : comment list;
  add_previous_com  : comment -> include_directive;
  set_file_path     : file_path Region.reg -> include_directive
>

val mk_include :
  ?previous_comment:comment ->
  ?trailing_comment:line_comment ->
  Region.t -> file_path Region.reg -> include_directive

(* #import *)

(* The method [region] covers "#import". The method [file_path] is the
   string containing the file from which to import, with its
   region. The method [module_name] is a string with the name of the
   module to fetch, with its region. *)

type import_directive = <
  region            : Region.t;
  file_path         : file_path Region.reg;
  module_name       : module_name Region.reg;
  trailing_comment  : line_comment option;
  previous_comments : comment list;
  add_previous_com  : comment -> import_directive;
  set_file_path     : file_path Region.reg -> import_directive
>

val mk_import :
  ?previous_comment:comment ->
  ?trailing_comment:line_comment ->
  Region.t ->
  file_path Region.reg ->
  module_name Region.reg -> import_directive

(* #if and #endif *)

(* The method [region] covers "#if" or "#elif". The method
   [expression] is the argument of "#if" (a boolean expression). *)

type bool_expr = <
  region            : Region.t;
  expression        : E_AST.t;
  trailing_comment  : line_comment option;
  previous_comments : comment list;
  add_previous_com  : comment -> bool_expr
>

type if_directive   = bool_expr
type elif_directive = bool_expr

val mk_bool_expr :
  ?previous_comment:comment ->
  ?trailing_comment:line_comment ->
  Region.t ->
  E_AST.t -> if_directive

(* #define and #undef *)

(* The method [region] covers "#define" or "#undef". The method [sym]
   is the argument of "#define" (an identifier), with its region. *)

type symbol = <
  region           : Region.t;
  symbol           : variable Region.reg;
  trailing_comment : line_comment option;
  previous_comments : comment list;
  add_previous_com  : comment -> symbol
>

type define_directive = symbol
type undef_directive  = symbol

val mk_symbol :
  ?previous_comment:comment ->
  ?trailing_comment:line_comment ->
  Region.t ->
  variable Region.reg -> symbol

(* #error *)

(* In the value [Error (region, msg)], [region] covers "#error", and
   [msg] is the argument of "#error" (characters on a single line),
   with its region. *)

type error_directive = Region.t * string Region.reg

(* Linemarkers (line directives) *)

(* Line markers are anonymous directives which may carry some
   additional flags:

   https://gcc.gnu.org/onlinedocs/cpp/Preprocessor-Output.html

   of which 1 and 2 indicate, respectively, the start of a new file
   and the return from a file (after its inclusion has been
   processed). *)

type flag  = Push | Pop

type line_directive = <
  region    : Region.t;
  linenum   : int Region.reg;
  file_path : string Region.reg;
  flag      : flag Region.reg option
>

val mk_line_directive :
  Region.t ->
  int Region.reg ->
  file_path Region.reg ->
  flag Region.reg option ->
  line_directive

(* ALL DIRECTIVES *)

type t =
  PP_Include    of include_directive
| PP_Import     of import_directive
| PP_If         of if_directive
| PP_Elif       of elif_directive
| PP_Else       of Region.t
| PP_Endif      of Region.t
| PP_Define     of define_directive
| PP_Undef      of undef_directive
| PP_Error      of error_directive
| PP_Linemarker of line_directive

(* PROJECTIONS *)

val to_region : t -> Region.t
val to_lexeme : t -> string Region.reg
val project   : t -> Region.t * string
val to_string : offsets:bool -> [`Byte | `Point] -> t -> string

(* EMBEDDING PREVIOUS COMMENTS *)

val add_comment  : comment -> t -> t
val get_comments : t -> comment list

(* SCANNERS *)

type error = Region.t * Error.t

val scan_include :
  Pos.t -> State.t -> Lexing.lexbuf ->
  (State.t * include_directive * t * ending, error) result

val scan_import :
  Pos.t -> State.t -> Lexing.lexbuf ->
  (State.t * import_directive * t * ending, error) result

val scan_if :
  Pos.t -> State.t -> Lexing.lexbuf ->
  (State.t * if_directive * t * ending, error) result

val scan_elif :
  Pos.t -> State.t -> Lexing.lexbuf ->
  (State.t * elif_directive * t * ending, error) result

val scan_else :
  Pos.t -> State.t -> Lexing.lexbuf -> State.t * t * ending

val scan_endif :
  Pos.t -> State.t -> Lexing.lexbuf -> State.t * t * ending

val scan_define :
  Pos.t -> State.t -> Lexing.lexbuf ->
  (State.t * define_directive * t * ending, error) result

val scan_undef :
  Pos.t -> State.t -> Lexing.lexbuf ->
  (State.t * undef_directive * t * ending, error) result

val scan_error :
  Pos.t -> State.t -> Lexing.lexbuf ->
  (State.t * error_message Region.reg * t * ending, error) result

val scan_linemarker :
  Pos.t -> string Region.reg -> State.t -> Lexing.lexbuf ->
  (State.t * line_directive * t * ending, error) result
