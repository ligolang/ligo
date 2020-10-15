module CST         = Cst.Reasonligo
module LexToken    = Lexer_reasonligo.LexToken
module Lexer       = Lexer_shared.Lexer.Make (LexToken)
module Scoping     = Parser_reasonligo.Scoping
module Region      = Simple_utils.Region
module ParErr      = Parser_reasonligo.ParErr
module SyntaxError = Parser_reasonligo.SyntaxError
module SSet        = Set.Make (String)
module Pretty      = Parser_reasonligo.Pretty
module EvalOpt     = Lexer_shared.EvalOpt

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
      mono    : bool;
      pretty  : bool
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
           method pretty  = false
         end

    let make ~libs =
      EvalOpt.make ~libs
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
    include Parser_reasonligo.Parser
  end

module ParserLog =
  struct
    type ast  = CST.t
    type expr = CST.expr
    include Cst_reasonligo.ParserLog
  end

module Unit =
  ParserUnit.Make (Lexer)(CST)(Parser)(ParErr)(ParserLog)(SubIO)

let apply parser =
  let local_fail error =
    Trace.fail
    @@ Errors.generic
    @@ Unit.format_error error in
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

let parse_file libs source = apply (fun () -> Unit.contract_in_file libs source)

(* Parsing a contract in a string *)

let parse_program_string libs source = apply (fun () -> Unit.contract_in_string libs source)

(* Parsing a contract from standard input *)

let parse_program_stdin libs () = apply (fun () -> Unit.contract_in_stdin libs ())

(* Parsing an expression in a string *)

let parse_expression libs source = apply (fun () -> Unit.expr_in_string libs source)

(* Preprocessing a contract in a file *)

let preprocess libs source = apply (fun () -> Unit.preprocess libs source)

(* Pretty-print a file (after parsing it). *)
let pretty_print cst =
  let doc    = Pretty.print cst in
  let buffer = Buffer.create 131 in
  let width  =
    match Terminal_size.get_columns () with
      None -> 60
    | Some c -> c in
  let () = PPrint.ToBuffer.pretty 1.0 width buffer doc
  in Trace.ok buffer

let pretty_print_from_source libs source =
  match Trace.to_stdlib_result @@ parse_file libs source with
    Stdlib.Error e -> Trace.fail e
  | Ok cst ->
    pretty_print @@ fst cst


let pretty_print_expression cst =
  let doc    = Pretty.pp_expr cst in
  let buffer = Buffer.create 131 in
  let width  =
    match Terminal_size.get_columns () with
      None -> 60
    | Some c -> c in
  let () = PPrint.ToBuffer.pretty 1.0 width buffer doc
  in Trace.ok buffer

let pretty_print_pattern pattern =
  let doc    = Pretty.pp_pattern pattern in
  let buffer = Buffer.create 131 in
  let width  =
    match Terminal_size.get_columns () with
      None -> 60
    | Some c -> c in
  let () = PPrint.ToBuffer.pretty 1.0 width buffer doc
  in Trace.ok buffer

let pretty_print_type_expr type_expr =
  let doc    = Pretty.pp_type_expr type_expr in
  let buffer = Buffer.create 131 in
  let width  =
    match Terminal_size.get_columns () with
      None -> 60
    | Some c -> c in
  let () = PPrint.ToBuffer.pretty 1.0 width buffer doc
  in Trace.ok buffer
