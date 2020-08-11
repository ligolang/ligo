(* Driver for the CameLIGO parser *)

(* Dependencies *)

module Region     = Simple_utils.Region
module EvalOpt    = Lexer_shared.EvalOpt
module LexToken   = Lexer_cameligo.LexToken
module CST        = Cst.Cameligo
module SSet       = Set.Make (String)
module ParserUnit = Parser_shared.ParserUnit
module Pretty     = Parser_cameligo.Pretty

(* Input/Output *)

module IO =
  struct
    let options =
      let open EvalOpt in
      let block = mk_block ~opening:"(*" ~closing:"*)"
      in read ~block ~line:"//" ".mligo"
  end

module SubIO =
  struct
    type options = <
      libs    : string list;
      verbose : SSet.t;
      offsets : bool;
      block   : EvalOpt.block_comment option;
      line    : EvalOpt.line_comment option;
      ext     : string;
      mode    : [`Byte | `Point];
      cmd     : EvalOpt.command;
      mono    : bool;
      pretty  : bool
    >

    let options : options =
      object
        method libs    = IO.options#libs
        method verbose = IO.options#verbose
        method offsets = IO.options#offsets
        method block   = IO.options#block
        method line    = IO.options#line
        method ext     = IO.options#ext
        method mode    = IO.options#mode
        method cmd     = IO.options#cmd
        method mono    = IO.options#mono
        method pretty  = IO.options#pretty
      end

    let make =
      EvalOpt.make ~libs:options#libs
                   ~verbose:options#verbose
                   ~offsets:options#offsets
                   ?block:options#block
                   ?line:options#line
                   ~ext:options#ext
                   ~mode:options#mode
                   ~cmd:options#cmd
                   ~mono:options#mono
                   ~pretty:options#pretty
  end

module Parser =
  struct
    type ast  = CST.t
    type expr = CST.expr
    include Parser_cameligo.Parser
  end

module ParserLog =
  struct
    type ast  = CST.t
    type expr = CST.expr
    include Cst_cameligo.ParserLog
  end

module Lexer = Lexer_shared.Lexer.Make (LexToken)

module Unit =
  ParserUnit.Make (Lexer)(CST)(Parser)(Parser_msg)(ParserLog)(SubIO)

(* Main *)

let wrap = function
  Stdlib.Ok ast ->
    if IO.options#pretty then
      begin
        let doc = Pretty.print ast in
        let width =
          match Terminal_size.get_columns () with
            None -> 60
          | Some c -> c in
        PPrint.ToChannel.pretty 1.0 width stdout doc;
        print_newline ()
      end;
    flush_all ()
| Error msg ->
    begin
      flush_all ();
      Printf.eprintf "\027[31m%s\027[0m%!" msg.Region.value
    end

let () =
  match IO.options#input with
    None -> Unit.contract_in_stdin () |> wrap
  | Some file_path -> Unit.contract_in_file file_path |> wrap
