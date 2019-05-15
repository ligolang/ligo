(* Positions in a file

   A position in a file denotes a single unit belonging to it, for
   example, in an ASCII text file, it is a particular character within
   that file (the unit is the byte in this instance, since in ASCII
   one character is encoded with one byte).

     Units can be either bytes (as ASCII characters) or, more
   generally, unicode points.

     The type for positions is the object type [t].

     We use here lexing positions to denote byte-oriented positions
   (field [byte]), and we manage code points by means of the fields
   [point_num] and [point_bol]. These two fields have a meaning
   similar to the fields [pos_cnum] and [pos_bol], respectively, from
   the standard module [Lexing]. That is to say, [point_num] holds the
   number of code points since the beginning of the file, and
   [point_bol] the number of code points since the beginning of the
   current line.

     The name of the file is given by the field [file], and the line
   number by the field [line].
*)

type t = <
  (* Payload *)

  byte       : Lexing.position;
  point_num  : int;
  point_bol  : int;
  file       : string;
  line       : int;

  (* Setters *)

  set_file   : string -> t;
  set_line   : int -> t;
  set_offset : int -> t;
  set        : file:string -> line:int -> offset:int -> t;

  (* The call [pos#new_line s], where the string [s] is either "\n" or
     "\c\r", updates the position [pos] with a new line. *)

  new_line : string -> t;
  add_nl   : t;

  (* The call [pos#shift_bytes n] evaluates in a position that is the
     translation of position [pos] of [n] bytes forward in the
     file. The call [pos#shift_one_uchar n] is similar, except that it
     assumes that [n] is the number of bytes making up one unicode
     point. *)

  shift_bytes     : int -> t;
  shift_one_uchar : int -> t;

  (* Getters *)

  (* The call [pos#offset `Byte] provides the horizontal offset of the
     position [pos] in bytes. (An offset is the number of units, like
     bytes, since the beginning of the current line.) The call
     [pos#offset `Point] is the offset counted in number of unicode
     points.

     The calls to the method [column] are similar to those to
     [offset], except that they give the curren column number.

     The call [pos#line_offset `Byte] is the offset of the line of
     position [pos], counted in bytes. Dually, [pos#line_offset
     `Point] counts the same offset in code points.

     The call [pos#byte_offset] is the offset of the position [pos]
     since the begininng of the file, counted in bytes.
  *)

  offset : [`Byte | `Point] -> int;
  column : [`Byte | `Point] -> int;

  line_offset : [`Byte | `Point] -> int;
  byte_offset : int;

  (* Predicates *)

  is_ghost : bool;

  (* Conversions to [string] *)

  to_string : ?offsets:bool -> [`Byte | `Point] -> string;
  compact   : ?offsets:bool -> [`Byte | `Point] -> string;
  anonymous : ?offsets:bool -> [`Byte | `Point] -> string
>

type pos = t

(* Constructors *)

val make      : byte:Lexing.position -> point_num:int -> point_bol:int -> t
val from_byte : Lexing.position -> t

(* Special positions *)

val ghost : t  (* Same as [Lexing.dummy_pos] *)
val min   : t  (* Lexing convention: line 1, offsets to 0 and file to "". *)

(* Comparisons *)

val equal : t -> t -> bool
val lt    : t -> t -> bool
