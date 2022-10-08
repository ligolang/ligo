(* State threaded along the scanning functions of [LowAPI] *)

(* Vendor dependencies *)

module Region    = Simple_utils.Region
module Pos       = Simple_utils.Pos
module Directive = Preprocessor.Directive

type lexeme = string

type 'token state = <
  pos            : Pos.t;
  set_pos        : Pos.t -> 'token state;
  sync           : Lexing.lexbuf -> 'token state * lexeme Region.reg;
  newline        : Lexing.lexbuf -> 'token state;

  decoder        : Uutf.decoder;
  supply         : Bytes.t -> int -> int -> unit;

  lexical_units  : 'token Unit.lex_unit list;

  push_token     :        'token -> 'token state;
  push_directive :   Directive.t -> 'token state;
  push_markup    :      Markup.t -> 'token state;

  push_line      :      Thread.t -> 'token state;
  push_block     :      Thread.t -> 'token state;
  push_space     : Lexing.lexbuf -> 'token state;
  push_tabs      : Lexing.lexbuf -> 'token state;
  push_bom       : Lexing.lexbuf -> 'token state;
  push_newline   : string Region.reg option ->
                   Lexing.lexbuf -> 'token state
>

type 'token t = 'token state

let empty ~file : 'token t =
  let decoder = Uutf.decoder ~encoding:`UTF_8 `Manual in
  object (self)
    val pos        = Pos.min ~file
    method pos     = pos

    method set_pos pos = {< pos = pos >}

    method sync lexbuf =
      let lexeme = Lexing.lexeme lexbuf in
      let length = String.length lexeme
      and start  = pos in
      let stop   = start#shift_bytes length in
      let state  = {< pos = stop >}
      and region = Region.make ~start:pos ~stop
      in state, Region.{region; value=lexeme}

    method newline lexbuf =
      let () = Lexing.new_line lexbuf in
      let nl = Lexing.lexeme lexbuf in
      self#set_pos (self#pos#new_line nl)

    method decoder = decoder
    method supply  = Uutf.Manual.src decoder

    val lexical_units = []
    method lexical_units = lexical_units

    method push_token token =
      {< lexical_units = (`Token token) :: self#lexical_units >}

    method push_directive dir =
      {< lexical_units = (`Directive dir) :: self#lexical_units >}

    method push_markup mark =
      {< lexical_units = (`Markup mark) :: self#lexical_units >}

    (* MARKUP *)

    (* Committing markup to the current logical state *)

    method push_newline ending lexbuf =
      let ()     = Lexing.new_line lexbuf in
      let value  = match ending with
                     None     -> Lexing.lexeme lexbuf
                   | Some reg -> reg.Region.value in
      let start  = (self#pos)#reset_cnum in
      let stop   = (start#new_line value)#reset_cnum in
      let region = Region.make ~start ~stop in
      let markup = Markup.Newline Region.{region; value}
      in (self#push_markup markup)#set_pos stop

    method push_line thread =
      let start  = thread#opening#start in
      let region = Region.make ~start ~stop:self#pos
      and value  = thread#to_string in
      let markup = Markup.LineCom Region.{region; value}
      in self#push_markup markup

    method push_block thread =
      let start  = thread#opening#start in
      let region = Region.make ~start ~stop:self#pos
      and value  = thread#to_string in
      let markup = Markup.BlockCom Region.{region; value}
      in self#push_markup markup

    method push_space lexbuf =
      let state, Region.{region; value} = self#sync lexbuf in
      let value  = String.length value in
      let markup = Markup.Space Region.{region; value}
      in state#push_markup markup

    method push_tabs lexbuf =
      let state, Region.{region; value} = self#sync lexbuf in
      let value  = String.length value in
      let markup = Markup.Tabs Region.{region; value}
      in state#push_markup markup

    method push_bom lexbuf =
      let state, Region.{region; value} = self#sync lexbuf in
      let markup = Markup.BOM Region.{region; value}
      in state#push_markup markup
  end
