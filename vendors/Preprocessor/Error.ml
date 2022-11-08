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
| Cycle_in_include of file list * file(* #include *)
| Unexpected_argument                 (* #include and #import *)
| Newline_in_string                   (* #include and #import *)
| Unterminated_string of string       (* #include, #import and strings *)
| Invalid_character_in_string of char (* #include, #import and linemarkers *)
| Invalid_flag                        (* Linemarkers *)
| Unterminated_comment of string

type t = error

let sprintf = Printf.sprintf

let to_string = function
  Missing_endif ->
    sprintf "Missing #endif directive."
| Newline_in_string ->
    (* TODO: When we add quoted strings to LIGO: " or insert a backslash." *)
    sprintf
      "A string cannot be interrupted by a line break.\n\
       Hint: Remove it or close the string before."
| Unterminated_string delimiter ->
    sprintf "The string starting here is not closed.\n\
             Hint: Close it with %S." delimiter
| Dangling_endif ->
    sprintf "Dangling #endif directive.\n\
             Hint: Remove it or add an #if before."
| If_follows_elif ->
    sprintf "Directive #if found in a clause #elif."
| Else_follows_else ->
    sprintf "Directive #else found in a clause #else."
| Dangling_else ->
    sprintf "Directive #else without #if."
| Elif_follows_else ->
    sprintf "Directive #elif found in a clause #else."
| Dangling_elif ->
    sprintf "Dangling #elif directive.\n\
             Hint: Remove it or add an #if before."
| Missing_space ->
    sprintf "At least a space character is expected."
| Error_directive msg ->
    if String.(msg = "") then sprintf "Directive #error reached."
    else msg
| Parse_error ->
    "Parse error in Boolean expression."
| Invalid_symbol ->
   "Invalid symbol."
| Missing_symbol ->
   "Missing symbol."
| File_not_found file ->
    sprintf "File %S not found." file
| Failed_opening (_file, msg) ->
    sprintf "%s." msg
| Unterminated_comment ending ->
    sprintf "The comment starting here is not closed.\n\
             Hint: Close it with %S." ending
| Missing_filename ->
    sprintf "File name expected in a string literal."
| Missing_module ->
    sprintf "Module name expected in a string literal."
| Cycle_in_include (incls, file) ->
    let cycle =
      incls
      |> List.take_while ~f:(fun i -> not @@ String.equal file i)
      |> List.rev
      |> List.cons file
      |> List.map ~f:(fun incl -> sprintf "-> %S" incl)
      |> String.concat ~sep:"\n"
    in
    sprintf "Error: Dependency cycle between:\n%s" cycle
| Unexpected_argument ->
    sprintf "Unexpected argument.\n\
             Hint: Remove it."
| Invalid_character_in_string c ->
    sprintf "Invalid character %S in string.\n\
             Hint: Use non-control characters 7-bit ASCII." (Char.escaped c)
| Invalid_character c ->
    sprintf "Invalid character '%c' (%d)." c (Char.to_int c)
| Invalid_flag ->
    sprintf "Invalid flag.\n\
             Hint: 1 or 2 are allowed."

(* Exception-based errors *)

type message = string Region.reg

exception Error of (Buffer.t * message)
