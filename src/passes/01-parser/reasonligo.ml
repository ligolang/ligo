open Trace

module AST         = Parser_cameligo.AST
module LexToken    = Parser_reasonligo.LexToken
module Lexer       = Lexer.Make (LexToken)
module Scoping     = Parser_cameligo.Scoping
module Region      = Simple_utils.Region
module ParErr      = Parser_reasonligo.ParErr
module SyntaxError = Parser_reasonligo.SyntaxError
module SSet        = Set.Make (String)

(* Mock IOs TODO: Fill them with CLI options *)

module SubIO =
  struct
    type options = <
      libs    : string list;
      verbose : SSet.t;
      offsets : bool;
      block   : EvalOpt.block_comment option;
      line    : EvalOpt.line_comment option;
      ext     : string;   (* ".religo" *)
      mode    : [`Byte | `Point];
      cmd     : EvalOpt.command;
      mono    : bool
    >

    let options : options =
      let block = EvalOpt.mk_block ~opening:"/*" ~closing:"*/"
      in object
           method libs    = []
           method verbose = SSet.empty
           method offsets = true
           method block   = Some block
           method line    = Some "//"
           method ext     = ".religo"
           method mode    = `Point
           method cmd     = EvalOpt.Quiet
           method mono    = false
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
    include Parser_reasonligo.Parser
  end

module ParserLog =
  struct
    type ast  = AST.t
    type expr = AST.expr
    include Parser_cameligo.ParserLog
  end

module Unit =
  ParserUnit.Make (Lexer)(AST)(Parser)(ParErr)(ParserLog)(SubIO)

module Errors =
  struct
    let generic message =
      let title () = ""
      and message () = message.Region.value
      in Trace.error ~data:[] title message

    let wrong_function_arguments (expr: AST.expr) =
      let title () = "" in
      let message () =
        "It looks like you are defining a function, \
         however we do not\n\
         understand the parameters declaration.\n\
         Examples of valid functions:\n\
         let x = (a: string, b: int) : int => 3;\n\
         let tuple = ((a, b): (int, int)) => a + b; \n\
         let x = (a: string) : string => \"Hello, \" ++ a;\n" in
      let expression_loc = AST.expr_to_region expr in
      let data = [
        ("location",
         fun () -> Format.asprintf "%a" Location.pp_lift @@ expression_loc)]
      in error ~data title message

    let invalid_wild (expr: AST.expr) =
      let title () = "" in
      let message () =
        "It looks like you are using a wild pattern where it cannot be used."
      in
      let expression_loc = AST.expr_to_region expr in
      let data = [
        ("location",
         fun () -> Format.asprintf "%a" Location.pp_lift @@ expression_loc)]
      in error ~data title message

  end

let apply parser =
  let local_fail error =
    Trace.fail
    @@ Errors.generic
    @@ Unit.format_error ~offsets:SubIO.options#offsets
                        SubIO.options#mode error in
  match parser () with
    Stdlib.Ok semantic_value -> Trace.ok semantic_value

  (* Lexing and parsing errors *)

  | Stdlib.Error error -> Trace.fail @@ Errors.generic error
  (* Scoping errors *)

  | exception Scoping.Error (Scoping.Reserved_name name) ->
      let token =
        Lexer.Token.mk_ident name.Region.value name.Region.region in
      (match token with
         Stdlib.Error LexToken.Reserved_name ->
           Trace.fail @@ Errors.generic @@ Region.wrap_ghost "Reserved name."
       | Ok invalid ->
          local_fail
            ("Reserved name.\nHint: Change the name.\n", None, invalid))

  | exception Scoping.Error (Scoping.Duplicate_variant name) ->
      let token =
        Lexer.Token.mk_constr name.Region.value name.Region.region
      in local_fail
           ("Duplicate constructor in this sum type declaration.\n\
             Hint: Change the constructor.\n", None, token)

  | exception Scoping.Error (Scoping.Non_linear_pattern var) ->
      let token =
        Lexer.Token.mk_ident var.Region.value var.Region.region in
      (match token with
         Stdlib.Error LexToken.Reserved_name ->
           Trace.fail @@ Errors.generic @@ Region.wrap_ghost "Reserved name."
       | Ok invalid ->
           local_fail ("Repeated variable in this pattern.\n\
                        Hint: Change the name.\n",
                       None, invalid))

  | exception Scoping.Error (Scoping.Duplicate_field name) ->
      let token =
        Lexer.Token.mk_ident name.Region.value name.Region.region in
      (match token with
         Stdlib.Error LexToken.Reserved_name ->
           Trace.fail @@ Errors.generic @@ Region.wrap_ghost "Reserved name."
       | Ok invalid ->
           local_fail
             ("Duplicate field name in this record declaration.\n\
               Hint: Change the name.\n",
              None, invalid))

  | exception SyntaxError.Error (SyntaxError.WrongFunctionArguments expr) ->
      Trace.fail @@ Errors.wrong_function_arguments expr
  | exception SyntaxError.Error (SyntaxError.InvalidWild expr) ->
      Trace.fail @@ Errors.invalid_wild expr

(* Parsing a contract in a file *)

let parse_file source = apply (fun () -> Unit.contract_in_file source)

(* Parsing a contract in a string *)

let parse_string source = apply (fun () -> Unit.contract_in_string source)

(* Parsing an expression in a string *)

let parse_expression source = apply (fun () -> Unit.expr_in_string source)

(* Preprocessing a contract in a file *)

let preprocess source = apply (fun () -> Unit.preprocess source)
