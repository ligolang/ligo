module Region = Simple_utils.Region

type thread = <
  opening     : Region.t;
  closing     : Region.t;
  length      : int;
  acc         : char list;
  to_string   : string;
  push_char   : char -> thread;
  push_string : string -> thread;
  set_opening : Region.t -> thread;
  set_closing : Region.t -> thread
>

type t = thread

let make ~opening : t =
  (* The call [explode s a] is the list made by pushing the characters
     in the string [s] on top of [a], in reverse order. For example,
     [explode "ba" ['c';'d'] = ['a'; 'b'; 'c'; 'd']]. *)

  let explode s acc =
    let rec push = function
      0 -> acc
    | i -> s.[i-1] :: push (i-1)
    in push (String.length s) in
  object
    val closing = opening
    method closing = closing

    val opening = opening
    method opening = opening

    val length = 0
    method length = length

    val acc = []
    method acc = acc

    method set_opening opening = {< opening >}

    method set_closing closing = {< closing >}

    method push_char char =
      {< opening; length=length+1; acc=char::acc >}

    method push_string str =
      {< opening;
         length = length + String.length str;
         acc = explode str acc >}

    (* The value of [thread#to_string] is a string of length
       [thread#length] containing the characters in the list
       [thread#acc], in reverse order. For instance, [thread#to_string
       = "abc"] if [thread#length = 3] and [thread#acc =
       ['c';'b';'a']]. *)

    method to_string =
      let bytes = Bytes.make length ' ' in
      let rec fill i = function
        [] -> bytes
      | char::l -> Bytes.set bytes i char; fill (i-1) l
      in fill (length-1) acc |> Bytes.to_string
  end
