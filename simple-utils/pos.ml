type t = <
  byte       : Lexing.position;
  point_num  : int;
  point_bol  : int;
  file       : string;
  line       : int;

  set_file   : string -> t;
  set_line   : int -> t;
  set_offset : int -> t;
  set        : file:string -> line:int -> offset:int -> t;
  new_line   : string -> t;
  add_nl     : t;

  shift_bytes     : int -> t;
  shift_one_uchar : int -> t;

  offset : [`Byte | `Point] -> int;
  column : [`Byte | `Point] -> int;

  line_offset : [`Byte | `Point] -> int;
  byte_offset : int;

  is_ghost : bool;

  to_string : ?offsets:bool -> [`Byte | `Point] -> string;
  compact   : ?offsets:bool -> [`Byte | `Point] -> string;
  anonymous : ?offsets:bool -> [`Byte | `Point] -> string
>

type pos = t

(* Constructors *)

let sprintf = Printf.sprintf

let make ~byte ~point_num ~point_bol =
  let () = assert (point_num >= point_bol) in
  object (self)
    val    byte      = byte
    method byte      = byte

    val    point_num = point_num
    method point_num = point_num

    val    point_bol = point_bol
    method point_bol = point_bol

    method set_file file =
      {< byte = Lexing.{byte with pos_fname = file} >}

    method set_line line =
      {< byte = Lexing.{byte with pos_lnum = line} >}

    method set_offset offset =
      {< byte = Lexing.{byte with pos_cnum = byte.pos_bol + offset} >}

    method set ~file ~line ~offset =
      let pos = self#set_file file in
      let pos = pos#set_line line in
      let pos = pos#set_offset offset
      in pos

    (* The string must not contain '\n'. See [new_line]. *)

    method shift_bytes len =
      {< byte = Lexing.{byte with pos_cnum = byte.pos_cnum + len};
         point_num = point_num + len >}

    method shift_one_uchar len =
      {< byte = Lexing.{byte with pos_cnum = byte.pos_cnum + len};
         point_num = point_num + 1 >}

    method add_nl =
      {< byte = Lexing.{byte with
                          pos_lnum = byte.pos_lnum + 1;
                          pos_bol  = byte.pos_cnum};
         point_bol = point_num >}

    method new_line string =
      let len = String.length string
      in (self#shift_bytes len)#add_nl

    method is_ghost = byte = Lexing.dummy_pos

    method file = byte.Lexing.pos_fname

    method line = byte.Lexing.pos_lnum

    method offset = function
       `Byte -> Lexing.(byte.pos_cnum - byte.pos_bol)
    | `Point -> point_num - point_bol

    method column mode = 1 + self#offset mode

    method line_offset = function
       `Byte -> byte.Lexing.pos_bol
    | `Point -> point_bol

    method byte_offset = byte.Lexing.pos_cnum

    method to_string ?(offsets=true) mode =
      let offset = self#offset mode in
      let horizontal, value =
        if offsets then "character", offset else "column", offset + 1
      in sprintf "File \"%s\", line %i, %s %i"
           self#file self#line horizontal value

    method compact ?(offsets=true) mode =
      if self#is_ghost then "ghost"
      else
        let offset = self#offset mode in
        sprintf "%s:%i:%i"
          self#file self#line (if offsets then offset else offset + 1)

    method anonymous ?(offsets=true) mode =
      if   self#is_ghost then "ghost"
      else sprintf "%i:%i" self#line
             (if offsets then self#offset mode else self#column mode)
end

let from_byte byte =
  let point_num = byte.Lexing.pos_cnum
  and point_bol = byte.Lexing.pos_bol
  in make ~byte ~point_num ~point_bol

let ghost = make ~byte:Lexing.dummy_pos ~point_num:(-1) ~point_bol:(-1)

let min =
  let byte = Lexing.{
               pos_fname = "";
               pos_lnum  = 1;
               pos_bol   = 0;
               pos_cnum  = 0}
  in make ~byte ~point_num:0 ~point_bol:0

(* Comparisons *)

let equal pos1 pos2 =
  pos1#file = pos2#file && pos1#byte_offset = pos2#byte_offset

let lt pos1 pos2 =
  pos1#file = pos2#file && pos1#byte_offset < pos2#byte_offset
