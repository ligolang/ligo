(* Regions of a file *)

let sprintf = Printf.sprintf

type t = <
  start : Pos.t;
  stop  : Pos.t;

  (* Setters *)

  shift_bytes     : int -> t;
  shift_one_uchar : int -> t;
  set_file        : string -> t;

  (* Getters *)

  file      : string;
  pos       : Pos.t * Pos.t;
  byte_pos  : Lexing.position * Lexing.position;

  (* Predicates *)

  is_ghost : bool;

  (* Conversions to [string] *)

  to_string : ?file:bool -> ?offsets:bool -> [`Byte | `Point] -> string;
  compact   : ?file:bool -> ?offsets:bool -> [`Byte | `Point] -> string
>

type region = t

type 'a reg = {region: t; value: 'a}

(* Injections *)

exception Invalid

let make ~(start: Pos.t) ~(stop: Pos.t) =
  if start#file <> stop#file || start#byte_offset > stop#byte_offset
  then raise Invalid
  else
    object
      val    start = start
      method start = start
      val    stop  = stop
      method stop  = stop

      method shift_bytes len =
        let start = start#shift_bytes len
        and stop  = stop#shift_bytes len
        in {< start = start; stop = stop >}

      method shift_one_uchar len =
        let start = start#shift_one_uchar len
        and stop  = stop#shift_one_uchar len
        in {< start = start; stop = stop >}

      method set_file name =
        let start = start#set_file name
        and stop  = stop#set_file name
        in {< start = start; stop = stop >}

      (* Getters *)

      method file      = start#file
      method pos       = start, stop
      method byte_pos  = start#byte, stop#byte

      (* Predicates *)

      method is_ghost = start#is_ghost && stop#is_ghost

      (* Conversions to strings *)

      method to_string ?(file=true) ?(offsets=true) mode =
        let horizontal = if offsets then "character"  else "column"
        and start_offset =
          if offsets then start#offset mode else start#column mode
        and stop_offset =
          if offsets then stop#offset mode else stop#column mode in
        let info =
          if   file
          then sprintf "in file \"%s\", line %i, %s"
                 (String.escaped start#file) start#line horizontal
          else sprintf "at line %i, %s" start#line horizontal
        in if   stop#line = start#line
           then sprintf "%ss %i-%i" info start_offset stop_offset
           else sprintf "%s %i to line %i, %s %i"
                  info start_offset stop#line horizontal stop_offset

      method compact ?(file=true) ?(offsets=true) mode =
        let start_str = start#anonymous ~offsets mode
        and stop_str  = stop#anonymous ~offsets mode in
        if start#file = stop#file then
          if file then sprintf "%s:%s-%s" start#file start_str stop_str
          else sprintf "%s-%s" start_str stop_str
        else sprintf "%s:%s-%s:%s" start#file start_str stop#file stop_str
    end

(* Special regions *)

let ghost = make ~start:Pos.ghost ~stop:Pos.ghost

let min = make ~start:Pos.min ~stop:Pos.min

(* Comparisons *)

let equal r1 r2 =
   r1#file = r2#file
&& Pos.equal r1#start r2#start
&& Pos.equal r1#stop  r2#stop

let lt r1 r2 =
  r1#file = r2#file
&& not r1#is_ghost
&& not r2#is_ghost
&& Pos.lt r1#start r2#start
&& Pos.lt r1#stop  r2#stop

let cover r1 r2 =
  if r1#is_ghost
  then r2
  else if r2#is_ghost
       then r1
       else if   lt r1 r2
            then make ~start:r1#start ~stop:r2#stop
            else make ~start:r2#start ~stop:r1#stop
