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
                   ~verbose:(SSet.singleton "cpp")   (* TODO (Debug) *)
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
    let reserved_name Region.{value; region} =
      let title () = Printf.sprintf "\nReserved name \"%s\"" value in
      let message () = "" in
      let data = [
        ("location",
         fun () -> Format.asprintf "%a" Location.pp_lift @@ region)]
      in Trace.error ~data title message

    let duplicate_parameter Region.{value; region} =
      let title () =
        Printf.sprintf "\nDuplicate parameter \"%s\"" value in
      let message () = "" in
      let data = [
        ("location",
         fun () -> Format.asprintf "%a" Location.pp_lift @@ region)]
      in Trace.error ~data title message

    let duplicate_variant Region.{value; region} =
      let title () =
        Printf.sprintf "\nDuplicate variant \"%s\" in this \
                        type declaration" value in
      let message () = "" in
      let data = [
        ("location",
         fun () -> Format.asprintf "%a" Location.pp_lift @@ region)]
      in Trace.error ~data title message

    let non_linear_pattern Region.{value; region} =
      let title () =
        Printf.sprintf "\nRepeated variable \"%s\" in this pattern" value in
      let message () = "" in
      let data = [
        ("location",
         fun () -> Format.asprintf "%a" Location.pp_lift @@ region)]
      in Trace.error ~data title message

    let duplicate_field Region.{value; region} =
      let title () =
        Printf.sprintf "\nDuplicate field name \"%s\" \
                        in this record declaration" value in
      let message () = "" in
      let data = [
        ("location",
         fun () -> Format.asprintf "%a" Location.pp_lift @@ region)]
      in Trace.error ~data title message

    let parser_error Region.{value; region} =
      let title () = ""
      and message () = value
      and loc = region in
      let data =
        [("parser_loc",
          fun () -> Format.asprintf "%a" Location.pp_lift @@ loc)]
      in Trace.error ~data title message

    let lexer_error (e: Lexer.error AST.reg) =
      let title () = "\nLexer error" in
      let message () = Lexer.error_to_string e.value in
      let data = [
          ("parser_loc",
           fun () -> Format.asprintf "%a" Location.pp_lift @@ e.region)]
      in Trace.error ~data title message
  end

let parse (module IO : IO) parser =
  let module Unit = PreUnit (IO) in
  let mk_error error =
    Unit.format_error ~offsets:IO.options#offsets
                      IO.options#mode error in
  match parser () with
    (* Scoping errors *)

    Stdlib.Ok semantic_value -> Trace.ok semantic_value
  | Stdlib.Error error -> Trace.fail @@ Errors.parser_error error
  | exception Lexer.Error e -> Trace.fail @@ Errors.lexer_error e

  | exception Scoping.Error (Scoping.Reserved_name name) ->
      let token =
        Lexer.Token.mk_ident name.Region.value name.Region.region in
      (match token with
         (* Cannot fail because [name] is a not a
            reserved name for the lexer. *)
         Stdlib.Error _ -> assert false
       | Ok invalid ->
          let point =
            "Reserved name.\nHint: Change the name.\n", None, invalid
          in Trace.fail @@ Errors.reserved_name @@ mk_error point)

  | exception Scoping.Error (Scoping.Duplicate_parameter name) ->
      let token =
        Lexer.Token.mk_ident name.Region.value name.Region.region in
      (match token with
         (* Cannot fail because [name] is a not a
            reserved name for the lexer. *)
         Stdlib.Error _ -> assert false
       | Ok invalid ->
          let point =
            "Duplicate parameter.\nHint: Change the name.\n",
            None, invalid
          in Trace.fail @@ Errors.duplicate_parameter @@ mk_error point)

  | exception Scoping.Error (Scoping.Duplicate_variant name) ->
      let token =
        Lexer.Token.mk_constr name.Region.value name.Region.region in
      let point =
        "Duplicate constructor in this sum type declaration.\n\
         Hint: Change the constructor.\n",
        None, token
      in Trace.fail @@ Errors.duplicate_variant @@ mk_error point

  | exception Scoping.Error (Scoping.Non_linear_pattern var) ->
      let token =
        Lexer.Token.mk_ident var.Region.value var.Region.region in
      (match token with
         (* Cannot fail because [var] is a not a
            reserved name for the lexer. *)
         Stdlib.Error _ -> assert false
       | Ok invalid ->
           let point =
             "Repeated variable in this pattern.\n\
              Hint: Change the name.\n",
             None, invalid
           in Trace.fail @@ Errors.non_linear_pattern @@ mk_error point)

  | exception Scoping.Error (Scoping.Duplicate_field name) ->
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
           in Trace.fail @@ Errors.duplicate_field @@ mk_error point)

let parse_file (source: string) =
  let module IO =
    struct
      let ext = PreIO.ext
      let options =
        PreIO.pre_options ~input:(Some source) ~expr:false
    end in
  let lib_path =
    match IO.options#libs with
      [] -> ""
    | libs -> let mk_I dir path = Printf.sprintf " -I %s%s" dir path
             in List.fold_right mk_I libs "" in
  let prefix =
    match IO.options#input with
      None | Some "-" -> "temp"
    | Some file -> Filename.(file |> basename |> remove_extension) in
  let suffix = ".pp" ^ IO.ext in
  let pp_input =
    if   SSet.mem "cpp" IO.options#verbose
    then prefix ^ suffix
    else let pp_input, pp_out =
           Filename.open_temp_file prefix suffix
         in close_out pp_out; pp_input in
  let cpp_cmd =
    match IO.options#input with
      None | Some "-" ->
        Printf.sprintf "cpp -traditional-cpp%s - > %s"
                       lib_path pp_input
    | Some file ->
        Printf.sprintf "cpp -traditional-cpp%s %s > %s"
                       lib_path file pp_input in
  let open Trace in
  let%bind () = sys_command cpp_cmd in
  let module Unit = PreUnit (IO) in
  let instance =
    match Lexer.open_token_stream (Lexer.File pp_input) with
      Ok instance -> instance
    | Stdlib.Error _ -> assert false (* No file opening *) in
  let thunk () = Unit.apply instance Unit.parse_contract
  in parse (module IO) thunk

let parse_string (s: string) =
  let module IO =
    struct
      let ext = PreIO.ext
      let options = PreIO.pre_options ~input:None ~expr:false
    end in
  let module Unit = PreUnit (IO) in
  let instance =
    match Lexer.open_token_stream (Lexer.String s) with
      Ok instance -> instance
    | Stdlib.Error _ -> assert false (* No file opening *) in
  let thunk () = Unit.apply instance Unit.parse_contract
  in parse (module IO) thunk

let parse_expression (s: string)  =
  let module IO =
    struct
      let ext = PreIO.ext
      let options = PreIO.pre_options ~input:None ~expr:true
    end in
  let module Unit = PreUnit (IO) in
  let instance =
    match Lexer.open_token_stream (Lexer.String s) with
      Ok instance -> instance
    | Stdlib.Error _ -> assert false (* No file opening *) in
  let thunk () = Unit.apply instance Unit.parse_expr
  in parse (module IO) thunk
