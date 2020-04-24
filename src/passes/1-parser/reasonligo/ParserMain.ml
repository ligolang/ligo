(* Driver for the ReasonLIGO parser *)

module Region = Simple_utils.Region
module SSet   = Set.Make (String)

module IO =
  struct
    let options =
      let open EvalOpt in
      let block = mk_block ~opening:"/*" ~closing:"*/"
      in read ~block ~line:"//" ".religo"
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
      mono    : bool
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
  end

module Parser =
  struct
    type ast  = AST.t
    type expr = AST.expr
    include Parser
  end

module ParserLog =
  struct
    type ast  = AST.t
    type expr = AST.expr
    include ParserLog
  end

module Lexer = Lexer.Make (LexToken)

module Unit =
  ParserUnit.Make (Lexer)(AST)(Parser)(ParErr)(ParserLog)(SubIO)

(* Main *)

let wrap = function
  Stdlib.Ok _ -> flush_all ()
| Error msg ->
    (flush_all (); Printf.eprintf "\027[31m%s\027[0m%!" msg.Region.value)

let () =
  match IO.options#input with
    None -> Unit.contract_in_stdin () |> wrap
  | Some file_path -> Unit.contract_in_file file_path |> wrap
