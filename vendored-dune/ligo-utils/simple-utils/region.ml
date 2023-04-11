(* Regions of a file *)

(* A shorthand *)

let sprintf = Printf.sprintf

(* The object type for regions *)

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

(* A synonym *)

type region = t

(* A convenience *)

type 'a reg = {region : t; value : 'a}

(* Injections *)

exception Different_files of string * string
exception Out_of_order_pos of string * string

let make ~(start : Pos.t) ~(stop : Pos.t) =
  if String.(<>) start#file stop#file
  then raise (Different_files (start#file, stop#file))
  else
  if start#line > stop#line
   || start#line = stop#line && start#byte_offset > stop#byte_offset
  then
    let pos1 = start#compact `Point
    and pos2 = stop#compact ` Point
    in raise (Out_of_order_pos (pos1, pos2))
  else
    object
      val    start = start
      method start = start
      val    stop  = stop
      method stop  = stop

      method shift_bytes len =
        let start = start#shift_bytes len
        and stop  = stop#shift_bytes len
        in {< start; stop >}

      method shift_one_uchar len =
        let start = start#shift_one_uchar len
        and stop  = stop#shift_one_uchar len
        in {< start; stop >}

      method set_file name =
        let start = start#set_file name
        and stop  = stop#set_file name
        in {< start; stop >}

      (* Getters *)

      method file     = start#file
      method pos      = start, stop
      method byte_pos = start#byte, stop#byte

      (* Predicates *)

      method is_ghost = start#is_ghost && stop#is_ghost

      (* Conversions to strings *)

      method to_string ?(file=true) ?(offsets=true) mode =
        let horizontal = if offsets then "character" else "column"
        and start_offset =
          if offsets then start#offset mode else start#column mode
        and stop_offset =
          if offsets then stop#offset mode else stop#column mode in
        let info =
          if file
          then sprintf "File %S, line %i, %s"
                 (String.escaped start#file) start#line horizontal
          else sprintf "Line %i, %s" start#line horizontal
        in if stop#line = start#line
           then
             if start_offset = stop_offset
             then sprintf "%s %i" info start_offset
             else sprintf "%ss %i-%i" info start_offset stop_offset
           else sprintf "%s %i to line %i, %s %i"
                        info start_offset stop#line horizontal
                        stop_offset

      method compact ?(file=true) ?(offsets=true) mode =
        if start#is_ghost || stop#is_ghost then "ghost"
        else
          let file_opt  =
            if file then Filename.basename start#file ^ ":" else ""
          and start_str = start#compact ~file:false ~offsets mode
          and stop_str  = stop#compact ~file:false ~offsets mode in
          if String.equal start#file stop#file then
            if start#line = stop#line then
              sprintf "%s%s-%i"
                      file_opt start_str
                      (if offsets then stop#offset mode else stop#column mode)
            else sprintf "%s%s-%s" file_opt start_str stop_str
          else sprintf "%s:%s-%s:%s"
                       start#file start_str stop#file stop_str
    end

let empty pos = make ~start:pos ~stop:pos

(* Making a region from the matched prefix of a lexing buffer *)

let from_lexbuf lexbuf =
  let start = Lexing.lexeme_start_p lexbuf |> Pos.from_byte
  and stop  = Lexing.lexeme_end_p lexbuf   |> Pos.from_byte
  in make ~start ~stop

(* Special regions *)

let ghost = make ~start:Pos.ghost ~stop:Pos.ghost

let wrap_ghost value = {value ; region = ghost}

let min ~file = make ~start:(Pos.min ~file) ~stop:(Pos.min ~file)

(* Comparisons *)

let equal (r1 : t) (r2 : t) =
  String.equal r1#file r2#file
&& Pos.equal r1#start r2#start
&& Pos.equal r1#stop  r2#stop

let reg_equal eq {region=r1;value=v1} {region=r2;value=v2} =
  equal r1 r2 && eq v1 v2

let lt r1 r2 =
  String.equal r1#file r2#file
&& not r1#is_ghost
&& not r2#is_ghost
&& Pos.lt r1#start r2#start
&& Pos.lt r1#stop  r2#stop

let compare r1 r2 =
  if equal r1 r2 then 0
  else if lt r1 r2 then -1
  else 1

let cover r1 r2 =
  if r1#is_ghost then r2
  else if r2#is_ghost then r1
  else 
    let pos_min p1 p2 = if Pos.lt p1 p2 then p1 else p2 
    and pos_max p1 p2 = if Pos.lt p1 p2 then p2 else p1
    in make ~start:(pos_min r1#start r2#start) ~stop:(pos_max r1#stop r2#stop)

let to_yojson f =
  `Assoc [
      ("start", Pos.to_yojson f#start) ;
      ("stop",  Pos.to_yojson f#stop) ;
    ]

let to_human_yojson f =
  `Assoc [
      ("start", Pos.to_human_yojson f#start) ;
      ("stop",  Pos.to_human_yojson f#stop) ;
    ]

let of_yojson = fun t ->
  match t with
    `Assoc [("start", start); ("stop", stop)] ->
      (match Pos.of_yojson start, Pos.of_yojson stop with
         Ok start, Ok stop -> Ok (make ~start ~stop)
       | (Error _ as e), _ | _, (Error _ as e) -> e)
  | _ ->
     Utils.error_yojson_format "{start: Pos.t, stop: Pos.t}"
