(* Converting the textual representation of tokens produced by Menhir
   into concrete syntax *)

module LexToken = Lexer_pascaligo.LexToken

let unlex (sentence: string) : Buffer.t =
  let tokens  = Str.split (Str.regexp " ") sentence in
  let lexemes = List.map LexToken.concrete tokens in
  let buffer  = Buffer.create 31 in
  let rec trans = function
      [] -> ()
  |  [s] -> Buffer.add_string buffer s
  | s::l -> Buffer.add_string buffer (s ^ " "); trans l
  in trans lexemes; buffer

(* Reading one line from the standard input channel and unlex it. *)

let out = unlex (input_line stdin) |> Buffer.contents
let ()  = Printf.printf "%s\n" out
