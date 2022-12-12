type buffer = {
              self : Buffer.t;
  mutable is_empty : bool
}

type t = {out : buffer; err : buffer}

type std = t

let empty_buffer ~init = {self = Buffer.create init; is_empty = true}

let empty = {
  out = empty_buffer ~init:4800; (* 80*60 = one full page *)
  err = empty_buffer ~init:160 (* 80*2 = two full lines *)
}

let add_line (b : buffer) text : unit =
  let open Buffer in
  match b.is_empty, String.(text = "") with
    true,  true  -> add_char b.self '\n'; b.is_empty <- false
  | false, true  -> add_char b.self '\n'
  | true,  false -> add_string b.self text; b.is_empty <- false
  | false, false -> add_char b.self '\n'; add_string b.self text

let add_nl (b : buffer) : unit =
  if not b.is_empty then
    begin
      Buffer.add_char b.self '\n';
      b.is_empty <- false
    end

let add_string (b : buffer) = Buffer.add_string b.self

let add_char (b : buffer) = Buffer.add_char b.self

let string_of (b : buffer) : string = Buffer.contents b.self

(* Colour *)

let redden string = Printf.sprintf "\027[31m%s\027[0m" string
