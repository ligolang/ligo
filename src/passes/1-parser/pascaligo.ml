module AST      = Parser_pascaligo.AST
module LexToken = Parser_pascaligo.LexToken
module Lexer    = Lexer.Make(LexToken)
module Scoping  = Parser_pascaligo.Scoping
module Region   = Simple_utils.Region
module ParErr   = Parser_pascaligo.ParErr
module SSet     = Utils.String.Set

(* Mock IOs TODO: Fill them with CLI options *)

module type IO =
  sig
    val ext : string
    val options : EvalOpt.options
  end

module PreIO =
  struct
    let ext = ".ligo"
    let pre_options =
      EvalOpt.make ~libs:[]
                   ~verbose:SSet.empty
                   ~offsets:true
                   ~mode:`Point
                   ~cmd:EvalOpt.Quiet
                   ~mono:true
  end

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

module PreUnit =
  ParserUnit.Make (Lexer)(AST)(Parser)(ParErr)(ParserLog)

module Errors =
  struct
    (* let data =
         [("location",
           fun () -> Format.asprintf "%a" Location.pp_lift @@ loc)] *)

    let generic message =
      let title () = ""
      and message () = message.Region.value
      in Trace.error ~data:[] title message
  end

let parse (module IO : IO) parser =
  let module Unit = PreUnit (IO) in
  let local_fail error =
    Trace.fail
    @@ Errors.generic
    @@ Unit.format_error ~offsets:IO.options#offsets
                      IO.options#mode error in
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

  | exception Scoping.Error (Scoping.Duplicate_parameter name) ->
      let token =
        Lexer.Token.mk_ident name.Region.value name.Region.region in
      (match token with
         Stdlib.Error LexToken.Reserved_name ->
           Trace.fail @@ Errors.generic @@ Region.wrap_ghost "Reserved name."
       | Ok invalid ->
           local_fail
             ("Duplicate parameter.\nHint: Change the name.\n",
              None, invalid))

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

let parse_file source =
  let module IO =
    struct
      let ext = PreIO.ext
      let options =
        PreIO.pre_options ~input:(Some source) ~expr:false
    end in
  let module Unit = PreUnit (IO)
  in Wrapper.parse_file Errors.generic (module Unit : ParserUnit.S) parse

let parse_string =
  let module IO =
    struct
      let ext = PreIO.ext
      let options = PreIO.pre_options ~input:None ~expr:false
    end in
  let module Unit = PreUnit (IO)
  in Wrapper.parse_string Errors.generic (module Unit : ParserUnit.S) parse

let parse_expression =
  let module IO =
    struct
      let ext = PreIO.ext
      let options = PreIO.pre_options ~input:None ~expr:true
    end in
  let module Unit = PreUnit (IO)
  in Wrapper.parse_expression Errors.generic (module Unit : ParserUnit.S) parse
