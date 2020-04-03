(* Driver for the PascaLIGO parser *)

module IO =
  struct
    let options = EvalOpt.(read ~lang:`PascaLIGO ~ext:".ligo")
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

module SSet = Set.Make (String)

(* Main *)

let issue_error error : ('a, string Region.reg) Stdlib.result =
  Stdlib.Error (Unit.format_error ~offsets:IO.options#offsets
                                  IO.options#mode error)

let parse parser : ('a, string Region.reg) Stdlib.result =
  try parser () with
    (* Scoping errors *)

  | Scoping.Error (Scoping.Duplicate_parameter name) ->
      let token =
        Lexer.Token.mk_ident name.Region.value name.Region.region in
        (match token with
           (* Cannot fail because [name] is not a reserved name for the
             lexer. *)
           Stdlib.Error _ -> assert false
         | Ok invalid ->
             issue_error ("Duplicate parameter.\nHint: Change the name.\n",
                          None, invalid))

  | Scoping.Error (Scoping.Reserved_name name) ->
      let token =
        Lexer.Token.mk_ident name.Region.value name.Region.region in
      (match token with
         (* Cannot fail because [name] is not a reserved name for the
            lexer. *)
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
         (* Cannot fail because [var] is not a reserved name for the
            lexer. *)
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
          let point =
            "Duplicate field name in this record declaration.\n\
             Hint: Change the name.\n",
            None, invalid
          in issue_error point)

(* Preprocessing the input source *)

let preproc cin : unit =
  let close () = flush_all (); close_in cin in
  let buffer = Lexing.from_channel cin in
  let open Lexing in
  let () =
    match IO.options#input with
      None | Some "-" -> ()
    | Some pos_fname ->
        buffer.lex_curr_p <- {buffer.lex_curr_p with pos_fname} in
  match Preproc.lex IO.options buffer with
    Stdlib.Error (pp_buffer, err) ->
      if SSet.mem "preproc" IO.options#verbose then
        Printf.printf "%s\n%!" (Buffer.contents pp_buffer);
      let Region.{value; _} =
        Preproc.format ~offsets:IO.options#offsets ~file:true err
      in close (); Printf.eprintf "\027[31m%s\027[0m%!" value
  | Stdlib.Ok pp_buffer ->
      (* Running the lexer and the parser on the preprocessed input *)

      let source = Lexer.String (Buffer.contents pp_buffer) in
        match Lexer.open_token_stream source with
          Stdlib.Ok instance ->
            if IO.options#expr
            then
              match parse (fun () -> Unit.apply instance Unit.parse_expr) with
                Stdlib.Ok _ -> ()
              | Error Region.{value; _} ->
                  close (); Printf.eprintf "\027[31m%s\027[0m%!" value
            else
            (match parse (fun () -> Unit.apply instance Unit.parse_contract) with
               Stdlib.Ok _ -> ()
             | Error Region.{value; _} ->
                 close (); Printf.eprintf "\027[31m%s\027[0m%!" value)
        | Stdlib.Error (Lexer.File_opening msg) ->
            flush_all (); Printf.eprintf "\027[31m%s\027[0m%!" msg

let () =
  match IO.options#input with
    Some "-" | None -> preproc stdin
    | Some file_path ->
       try open_in file_path |> preproc with
         Sys_error msg ->
           (flush_all (); Printf.eprintf "\027[31m%s\027[0m%!" msg)
