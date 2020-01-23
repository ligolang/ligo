
module ParserLog = Parser_pascaligo.ParserLog
module ParErr = Parser_pascaligo.ParErr
module SSet = Utils.String.Set

(* Mock options. TODO: Plug in cmdliner. *)

let pre_options =
  EvalOpt.make
    ~libs:[]
    ~verbose:SSet.empty
    ~offsets:true
    ~mode:`Point
    ~cmd:EvalOpt.Quiet
    ~mono:true (* Monolithic API of Menhir for now *)
(*  ~input:None *)
(*  ~expr:true  *)

module Parser =
  struct
    type ast  = AST.t
    type expr = AST.expr
    include Parser_pascaligo.Parser
  end

module ParserLog =
  struct
    type ast  = AST.t
    type expr = AST.expr
    include Parser_pascaligo.ParserLog
  end

module PreUnit = ParserUnit.Make (Lexer)(AST)(Parser)(ParErr)(ParserLog)
module Front   = ParserAPI.Make (Lexer)(Parser)(ParErr)

let issue_error point =
  let error = Front.format_error ~offsets:true (* TODO: CLI *)
                                 `Point (* TODO: CLI *) point
  in Stdlib.Error error
