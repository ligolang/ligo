(* Driver for the ReasonLIGO lexer *)

module Region = Simple_utils.Region

module IO =
  struct
    let options =
      let open EvalOpt in
      let block = mk_block ~opening:"/*" ~closing:"*/"
      in read ~block ~line:"//" ".religo"
  end

module M = LexerUnit.Make (IO) (Lexer.Make (LexToken))

let () =
  match M.trace () with
    Stdlib.Ok () -> ()
  | Error Region.{value; _} -> Printf.eprintf "\027[31m%s\027[0m%!" value
