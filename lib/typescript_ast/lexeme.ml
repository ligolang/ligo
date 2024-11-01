(* Reading in the source the text at a given region (expecting a
   lexeme) *)

open Core
module Region = Simple_utils.Region

let open_input, read, close_input =
  let in_channel : In_channel.t option ref = ref None in
  ( (fun ~file -> in_channel := Some (In_channel.create file))
  , (fun region ->
      let start_pos, stop_pos = region#byte_pos in
      let start_cnum = start_pos.Lexing.pos_cnum
      and stop_cnum = stop_pos.Lexing.pos_cnum in
      let len = stop_cnum - start_cnum in
      let buf = Bytes.create len in
      match !in_channel with
      | None -> ""
      | Some in_chan ->
        In_channel.seek in_chan (Int64.of_int_exn start_cnum);
        let (_ : int) = In_channel.input in_chan ~buf ~pos:0 ~len (* 0 is EOF *) in
        Bytes.to_string buf)
  , fun () ->
      match !in_channel with
      | None -> ()
      | Some channel -> In_channel.close channel )
