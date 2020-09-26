type json = Yojson.Safe.t

type input_type = 
| File of string
| Code of string
| None

type 'a display_format =
  | Human_readable : string display_format
  | Dev : string display_format
  | Json : json display_format

type ex_display_format = Ex_display_format : 'a display_format -> ex_display_format

let human_readable = Ex_display_format Human_readable
let dev = Ex_display_format Dev
let json = Ex_display_format Json

type 'a pp = display_format:(string display_format) -> 'a -> Location.t * string
type 'a format = {
    pp : 'a pp ;
    to_json : 'a -> json ;
}

type 'a with_format = {
    value : 'a ;
    format : 'a format ;
}

type displayable = Displayable : 'a with_format -> displayable

let print_code (l:Region.t) (input_line: unit -> string) =
  let start = l#start#line in
      let start_column = l#start#offset `Byte in
      let stop = l#stop#line in
      let stop_column = l#stop#offset `Byte in
      let len = stop - start in
      let buffer = Buffer.create (len + 100) in
      let rec loop_lines buffer curr start stop = 
        try (
          let curr = curr + 1 in
          let line = input_line () in
          let line_length = String.length line in
          (if curr >= start - 1 && curr < stop + 2 then (
            let _ = Printf.bprintf buffer "%3i" curr in
            if curr >= start && curr <= stop then (      
              if curr > start && curr < stop then        
                ignore(Printf.bprintf buffer " | \027[1m\027[31m%s\027[0m%!" (line  ^ "\n"))
              else if curr = start then ( 
                let before = String.sub line 0 start_column in
                Buffer.add_string buffer " | ";
                Buffer.add_string buffer before;
                if curr = stop then (
                  let between = String.sub line start_column (stop_column - start_column) in
                  let after = String.sub line stop_column (line_length - stop_column) in
                  ignore(Printf.bprintf buffer "\027[1m\027[31m%s\027[0m%!" between);
                  Buffer.add_string buffer after;
                  Buffer.add_string buffer "\n"
                ) else (
                  let after = String.sub line start_column (line_length - start_column) in
                  ignore(Printf.bprintf buffer "\027[1m\027[31m%s\027[0m%!" after);
                  Buffer.add_string buffer "\n"
                )
              )
              else if curr = stop then (
                let before = String.sub line 0 stop_column in
                let after = String.sub line stop_column (line_length - stop_column) in
                Buffer.add_string buffer " | ";
                ignore(Printf.bprintf buffer "\027[1m\027[31m%s\027[0m%!" before);
                Buffer.add_string buffer after;
                Buffer.add_string buffer "\n"
              )
            )
            else
              Buffer.add_string buffer (" | " ^ line ^ "\n"));
            );
            if curr < stop + 2 then
              loop_lines buffer curr start stop
            ) with 
        | _ -> ()
      in 
      loop_lines buffer 0 start stop;
      Buffer.contents buffer

let regexp = Str.regexp "\n"

let load_code_block (loc: Location.t) (m: input_type) =
  match loc, m with
  | File l, Code c -> 
    let lines = Str.split regexp c in
    let curr = ref 0 in
    print_code l (fun () ->       
      let line = List.nth lines !curr in       
      curr := !curr + 1;
      line
    )
  | File l, File f -> (
    print_endline (l#to_string `Byte);
    try
      let in_ = open_in f in
      let result = print_code l (fun () -> input_line in_) in
      close_in in_;
      result      
    with | _ -> 
      "")
  | _ ->  
    let buffer = Buffer.create 100 in
    let formatter = Format.formatter_of_buffer buffer in
    Location.pp formatter loc;
    Format.pp_print_flush formatter ();
    Buffer.contents buffer

let convert : type output . input_type -> display_format:(output display_format) -> displayable -> output =
  fun input ~display_format (Displayable { value ; format }) ->
  match display_format with
  | Json -> format.to_json value
  | Dev -> 
    let (_, msg) = format.pp ~display_format value in
    msg
  | Human_readable -> (
    let (loc, msg) = format.pp ~display_format value in
    let loc_text = load_code_block loc input in    
    if loc_text = "" then
      msg      
    else
      Format.asprintf "@[<hv>%s@,\027[1m\027[31mError\027[0m: %s@]" loc_text msg
  )

let to_json : displayable -> json = convert None ~display_format:Json

let bind_format :
  'value format -> 'error format -> ('value,'error) result format =
  fun value_format error_format ->
    let pp ~display_format a = match a with
      | Error e -> error_format.pp ~display_format e
      | Ok v -> value_format.pp ~display_format v in
    let to_json a = match a with
      | Error e -> error_format.to_json e
      | Ok v -> value_format.to_json v in
    { pp ; to_json }
