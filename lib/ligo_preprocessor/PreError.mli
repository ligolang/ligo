(* Preprocessing errors *)

(* Vendors's dependencies *)

module Region = Simple_utils.Region

(* Errors *)

type file = string
type msg = string

type error =
  Missing_endif                       (* #if *)
| Dangling_endif                      (* #if *)
| If_follows_elif                     (* #if *)
| Else_follows_else                   (* #if *)
| Dangling_else                       (* #if *)
| Elif_follows_else                   (* #if *)
| Dangling_elif                       (* #if *)
| Invalid_character of char           (* #if and #elif *)
| Parse_error                         (* #if and #elif *)
| Missing_space                       (* #error, #define, #undef *)
| Error_directive of string           (* #error ONLY *)
| Invalid_symbol                      (* #define and #undef *)
| Missing_symbol                      (* #define and #undef *)
| File_not_found of file              (* #include #import *)
| Failed_opening of file * msg        (* #include #import *)
| Missing_filename                    (* #include #import *)
| Missing_module                      (* #import *)
| Cyclic_inclusion of file list * file(* #include *)
| Unexpected_argument                 (* #include and #import *)
| Newline_in_string                   (* #include and #import *)
| Unterminated_string of string       (* #include, #import & strings *)
| Invalid_character_in_string of char (* #include, #import & linemarkers *)
| Invalid_flag                        (* Linemarkers *)
| Unterminated_comment of string

type t = error

val to_string : t -> string

(* We exceptionally export an exception because it needs to be used
   inside two lexers [E_Lexer] and [API] (therefore it cannot be
   defined uniquely in only one of them). *)

type message = string Region.reg

exception Error of (Buffer.t * message)
