(* Regions of a file

   A _region_ is a contiguous series of bytes, for example, in a text
   file. It is here denoted by the object type [t].

     The start (included) of the region is given by the field [start],
   which is a _position_, and the end (excluded) is the position given
   by the field [stop]. The convention of including the start and
   excluding the end enables to have empty regions if, and only if,
   [start = stop]. See module [Pos] for the definition of positions.

     The first byte of a file starts at the offset zero (that is,
   column one), and [start] is always lower than or equal to [stop],
   and they must refer to the same file.
*)

type t = <
  start : Pos.t;
  stop  : Pos.t;

  (* Setters *)

  (* The call [region#shift_bytes n] evaluates in a region that is the
     translation of region [region] of [n] bytes forward in the
     file. The call [region#shift_one_uchar n] is similar, except that
     it assumes that [n] is the number of bytes making up one unicode
     point. The call [region#set_file f] sets the file name to be
     [f]. *)

  shift_bytes     : int -> t;
  shift_one_uchar : int -> t;
  set_file        : string -> t;

  (* Getters *)

  (* The method [file] returns the file name.
     The method [pos] returns the values of the fields [start] and [stop].
     The method [byte_pos] returns the start and end positions of the
     region at hand _interpreting them as lexing positions_, that is,
     the unit is the byte. *)

  file     : string;
  pos      : Pos.t * Pos.t;
  byte_pos : Lexing.position * Lexing.position;

  (* Predicates *)

  is_ghost : bool;

  (* Conversions to [string] *)

  (* The call [region#to_string ~file ~offsets mode] evaluates in a
     string denoting the region [region].

     The name of the file is present if, and only if, [file = true] or
     [file] is missing.

     The positions in the file are expressed horizontal offsets if
     [offsets = true] or [offsets] is missing (the default), otherwise
     as columns.

     If [mode = `Byte], those positions will be assumed to have bytes
     as their unit, otherwise, if [mode = `Point], they will be
     assumed to refer to code points.

     The method [compact] has the same signature and calling
     convention as [to_string], except that the resulting string is
     more compact.
   *)

  to_string : ?file:bool -> ?offsets:bool -> [`Byte | `Point] -> string;
  compact   : ?file:bool -> ?offsets:bool -> [`Byte | `Point] -> string
>

type region = t

type 'a reg = {region: t; value: 'a}

(* Constructors *)

(* The function [make] creates a region from two positions. If the
   positions are not properly ordered or refer to different files, the
   exception [Invalid] is raised. *)

exception Invalid

val make : start:Pos.t -> stop:Pos.t -> t

(* Special regions *)

(* To deal with ghost expressions, that is, pieces of abstract syntax
   that have not been built from excerpts of concrete syntax, we need
   _ghost regions_. The module [Pos] provides a [ghost] position, and
   we also provide a [ghost] region and, in type [t], the method
   [is_ghost] to check it. *)

val ghost : t (* Two [Pos.ghost] positions *)

(* Occasionnally, we may need a minimum region. It is here made of two
   minimal positions. *)

val min : t (* Two [Pos.min] positions *)

(* Comparisons *)

(* Two regions are equal if, and only if, they refer to the same file
   and their start positions are equal and their stop positions are
   equal. See [Pos.equal]. Note that [r1] and [r2] can be ghosts. *)

val equal : t -> t -> bool

(* The call [lt r1 r2] ("lower than") has the value [true] if, and
   only if, regions [r1] and [r2] refer to the same file, none is a
   ghost and the start position of [r1] is lower than that of
   [r2]. (See [Pos.lt].) *)

val lt : t -> t -> bool

(* Given two regions [r1] and [r2], we may want the region [cover r1
   r2] that covers [r1] and [r2]. We property [equal (cover r1 r2)
   (cover r2 r1)]. (In a sense, it is the maximum region, but we avoid
   that name because of the [min] function above.) If [r1] is a ghost,
   the cover is [r2], and if [r2] is a ghost, the cover is [r1]. *)

val cover : t -> t -> t
