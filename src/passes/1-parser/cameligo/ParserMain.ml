(** Driver for the CameLIGO parser *)

module IO =
  struct
    let ext = ".mligo"
    let options = EvalOpt.read "CameLIGO" ext
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
  ParserUnit.Make (Lexer)(AST)(Parser)(ParErr)(ParserLog)(IO)

(* Main *)

let issue_error point =
  let error = Unit.format_error ~offsets:IO.options#offsets
                                IO.options#mode point
  in Stdlib.Error error

let parse parser : ('a,string) Stdlib.result =
  try parser () with
    (* Scoping errors *)

  | Scoping.Error (Scoping.Reserved_name name) ->
      let token =
        Lexer.Token.mk_ident name.Region.value name.Region.region in
      (match token with
         (* Cannot fail because [name] is a not a
            reserved name for the lexer. *)
         Stdlib.Error _ -> assert false
       | Ok invalid ->
          issue_error
            ("Reserved name.\nHint: Change the name.\n", None, invalid))

  | Scoping.Error (Scoping.Duplicate_variant name) ->
      let token =
        Lexer.Token.mk_constr name.Region.value name.Region.region in
      let point = "Duplicate constructor in this sum type declaration.\n\
                   Hint: Change the constructor.\n",
                  None, token
      in issue_error point

  | Scoping.Error (Scoping.Non_linear_pattern var) ->
      let token =
        Lexer.Token.mk_ident var.Region.value var.Region.region in
      (match token with
         (* Cannot fail because [var] is a not a
            reserved name for the lexer. *)
         Stdlib.Error _ -> assert false
       | Ok invalid ->
           let point = "Repeated variable in this pattern.\n\
                        Hint: Change the name.\n",
                       None, invalid
           in issue_error point)

  | Scoping.Error (Scoping.Duplicate_field name) ->
      let token =
        Lexer.Token.mk_ident name.Region.value name.Region.region in
      (match token with
         (* Cannot fail because [name] is a not a
            reserved name for the lexer. *)
         Stdlib.Error _ -> assert false
       | Ok invalid ->
           let point = "Duplicate field name in this record declaration.\n\
                        Hint: Change the name.\n",
                       None, invalid
           in issue_error point)

let () =
  if IO.options#expr
  then match parse (fun () -> Unit.parse Unit.parse_expr) with
         Stdlib.Ok _ -> ()
       | Error msg -> Printf.eprintf "\027[31m%s\027[0m%!" msg
  else match parse (fun () -> Unit.parse Unit.parse_contract) with
         Stdlib.Ok _ -> ()
       | Error msg -> Printf.eprintf "\027[31m%s\027[0m%!" msg
