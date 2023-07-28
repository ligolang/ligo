module Michelson = Tezos_utils.Michelson
module Location = Simple_utils.Location
open Proto_alpha_utils
open Simple_utils.Trace
open Memory_proto_alpha.Protocol.Script_ir_translator
open Memory_proto_alpha.X
open Simple_utils.Runned_result
module Errors = Main_errors

let parse_constant ~raise code =
  let open Tezos_micheline in
  let open Tezos_micheline.Micheline in
  let code, errs = Micheline_parser.tokenize code in
  let code =
    match errs with
    | _ :: _ ->
      raise.error
        (Errors.unparsing_michelson_tracer
        @@ List.map ~f:(fun x -> `Tezos_alpha_error x) errs)
    | [] ->
      let code, errs = Micheline_parser.parse_expression ~check:false code in
      (match errs with
      | _ :: _ ->
        raise.error
          (Errors.unparsing_michelson_tracer
          @@ List.map ~f:(fun x -> `Tezos_alpha_error x) errs)
      | [] -> map_node (fun _ -> ()) (fun x -> x) code)
  in
  Trace.trace_alpha_tzresult ~raise Errors.unparsing_michelson_tracer
  @@ Memory_proto_alpha.node_to_canonical code


type options = Memory_proto_alpha.options

type dry_run_options =
  { parameter_ty : (Stacking.Program.meta, string) Tezos_micheline.Micheline.node option
        (* added to allow dry-running contract using `Tezos.self` *)
  ; amount : string
  ; balance : string
  ; now : string option
  ; sender : string option
  ; source : string option
  }

(* Shouldn't this be done by the cli parser ? *)
let make_dry_run_options ~raise ?tezos_context ?(constants = []) (opts : dry_run_options)
    : options
  =
  let open Proto_alpha_utils.Trace in
  let open Proto_alpha_utils.Memory_proto_alpha in
  let open Protocol.Alpha_context in
  let balance =
    match Tez.of_string opts.balance with
    | None -> raise.error @@ Errors.main_invalid_balance opts.balance
    | Some balance -> balance
  in
  let amount =
    match Tez.of_string opts.amount with
    | None -> raise.error @@ Errors.main_invalid_amount opts.amount
    | Some amount -> amount
  in
  let sender =
    match opts.sender with
    | None -> None
    | Some sender ->
      let sender =
        trace_alpha_tzresult
          ~raise
          (fun _ -> Errors.main_invalid_sender sender)
          (Contract.of_b58check sender)
      in
      Some sender
  in
  let source =
    match opts.source with
    | None -> None
    | Some source ->
      let source =
        trace_alpha_tzresult
          ~raise
          (fun _ -> Errors.main_invalid_source source)
          (Contract.of_b58check source)
      in
      Some source
  in
  let now =
    match opts.now with
    | None -> None
    | Some st ->
      (match Memory_proto_alpha.Protocol.Script_timestamp.of_string st with
      | Some t -> Some t
      | None -> raise.error @@ Errors.main_invalid_timestamp st)
  in
  let parameter_ty =
    match opts.parameter_ty with
    | Some x ->
      let x =
        Trace.trace_tzresult_lwt ~raise Errors.parsing_payload_tracer
        @@ Memory_proto_alpha.prims_of_strings x
      in
      let x = Tezos_micheline.Micheline.strip_locations x in
      Some x
    | None -> None
  in
  (* Parse constants *)
  let constants = List.map ~f:(parse_constant ~raise) constants in
  make_options
    ?tezos_context
    ~constants
    ?now
    ~amount
    ~balance
    ?sender
    ?source
    ?parameter_ty
    ()


let ex_value_ty_to_michelson ~raise (v : ex_typed_value) : _ Michelson.t * _ Michelson.t =
  let (Ex_typed_value (ty, value)) = v in
  let ty' =
    Trace.trace_tzresult_lwt ~raise Errors.unparsing_michelson_tracer
    @@ Memory_proto_alpha.unparse_michelson_ty ty
  in
  let value' =
    Trace.trace_tzresult_lwt ~raise Errors.unparsing_michelson_tracer
    @@ Memory_proto_alpha.unparse_michelson_data ty value
  in
  ty', value'


let pack_payload ~raise (payload : _ Michelson.t) ty =
  let ty =
    Trace.trace_tzresult_lwt ~raise Errors.parsing_payload_tracer
    @@ Memory_proto_alpha.prims_of_strings ty
  in
  let (Ex_ty ty) =
    Trace.trace_tzresult_lwt ~raise Errors.parsing_payload_tracer
    @@ Memory_proto_alpha.parse_michelson_ty ty
  in
  let payload =
    Trace.trace_tzresult_lwt ~raise Errors.parsing_input_tracer
    @@ Memory_proto_alpha.prims_of_strings payload
  in
  let payload =
    Trace.trace_tzresult_lwt ~raise Errors.parsing_payload_tracer
    @@ Memory_proto_alpha.parse_michelson_data payload ty
  in
  let data =
    Trace.trace_tzresult_lwt ~raise Errors.packing_payload_tracer
    @@ Memory_proto_alpha.pack ty payload
  in
  data


let fetch_lambda_types ~raise (contract_ty : _ Michelson.t) =
  match contract_ty with
  | Prim (_, "lambda", [ in_ty; out_ty ], _) -> in_ty, out_ty
  | _ -> raise.error Errors.main_unknown (*TODO*)


let run_contract
    ~raise
    ?options
    (exp : _ Michelson.t)
    (exp_type : _ Michelson.t)
    (input_michelson : _ Michelson.t)
  =
  let open! Memory_proto_alpha.Protocol in
  let input_ty, output_ty = fetch_lambda_types ~raise exp_type in
  let input_ty =
    Trace.trace_tzresult_lwt ~raise Errors.parsing_input_tracer
    @@ Memory_proto_alpha.prims_of_strings input_ty
  in
  let param_type, storage_type =
    match input_ty with
    | Prim (_, T_pair, x :: y :: ys, _) ->
      let y =
        if List.is_empty ys
        then y
        else
          Tezos_micheline.Micheline.Prim (-1, Michelson_v1_primitives.T_pair, y :: ys, [])
      in
      x, y
    | _ -> failwith ("Internal error: input_ty was not a pair " ^ __LOC__)
  in
  let (Ex_ty input_ty) =
    Trace.trace_tzresult_lwt ~raise Errors.parsing_input_tracer
    @@ Memory_proto_alpha.parse_michelson_ty input_ty
  in
  let (Ex_ty param_type) =
    Trace.trace_tzresult_lwt ~raise Errors.parsing_input_tracer
    @@ Memory_proto_alpha.parse_michelson_ty param_type
  in
  let (Ex_ty storage_type) =
    Trace.trace_tzresult_lwt ~raise Errors.parsing_input_tracer
    @@ Memory_proto_alpha.parse_michelson_ty storage_type
  in
  let output_ty =
    Trace.trace_tzresult_lwt ~raise Errors.parsing_input_tracer
    @@ Memory_proto_alpha.prims_of_strings output_ty
  in
  let (Ex_ty output_ty) =
    Trace.trace_tzresult_lwt ~raise Errors.parsing_input_tracer
    @@ Memory_proto_alpha.parse_michelson_ty output_ty
  in
  let input_michelson =
    Trace.trace_tzresult_lwt ~raise Errors.parsing_input_tracer
    @@ Memory_proto_alpha.prims_of_strings input_michelson
  in
  let input =
    Trace.trace_tzresult_lwt ~raise Errors.parsing_input_tracer
    @@ Memory_proto_alpha.parse_michelson_data input_michelson input_ty
  in
  let ty_stack_before = Script_typed_ir.Item_t (input_ty, Bot_t) in
  let ty_stack_after = Script_typed_ir.Item_t (output_ty, Bot_t) in
  let top_level =
    (* original_type_expr is probably wrong *)
    let entrypoints =
      Script_typed_ir.
        { root = Script_typed_ir.no_entrypoints; original_type_expr = Int (0, Z.zero) }
    in
    Script_tc_context.toplevel ~storage_type ~param_type ~entrypoints
  in
  let (descr : (_, _, _, _) descr) =
    Trace.trace_tzresult_lwt ~raise Errors.parsing_code_tracer
    @@ Memory_proto_alpha.parse_michelson_fail
         ~top_level
         exp
         ty_stack_before
         ty_stack_after
  in
  let open! Memory_proto_alpha.Protocol.Script_interpreter in
  let res =
    Trace.trace_tzresult_lwt ~raise Errors.error_of_execution_tracer
    @@ Memory_proto_alpha.failure_interpret ?options descr input (EmptyCell, EmptyCell)
  in
  match res with
  | Memory_proto_alpha.Succeed output ->
    let ty, value =
      ex_value_ty_to_michelson ~raise (Ex_typed_value (output_ty, output))
    in
    Success (ty, value)
  | Memory_proto_alpha.Fail expr ->
    let expr =
      Tezos_micheline.Micheline.root
      @@ Memory_proto_alpha.Protocol.Michelson_v1_primitives.strings_of_prims expr
    in
    Fail expr


let run_function
    ~raise
    ?options
    (exp : _ Michelson.t)
    (exp_type : _ Michelson.t)
    (input_michelson : _ Michelson.t)
  =
  let open! Memory_proto_alpha.Protocol in
  let input_ty, output_ty = fetch_lambda_types ~raise exp_type in
  let input_ty =
    Trace.trace_tzresult_lwt ~raise Errors.parsing_input_tracer
    @@ Memory_proto_alpha.prims_of_strings input_ty
  in
  let (Ex_ty input_ty) =
    Trace.trace_tzresult_lwt ~raise Errors.parsing_input_tracer
    @@ Memory_proto_alpha.parse_michelson_ty input_ty
  in
  let output_ty =
    Trace.trace_tzresult_lwt ~raise Errors.parsing_input_tracer
    @@ Memory_proto_alpha.prims_of_strings output_ty
  in
  let (Ex_ty output_ty) =
    Trace.trace_tzresult_lwt ~raise Errors.parsing_input_tracer
    @@ Memory_proto_alpha.parse_michelson_ty output_ty
  in
  let input_michelson =
    Trace.trace_tzresult_lwt ~raise Errors.parsing_input_tracer
    @@ Memory_proto_alpha.prims_of_strings input_michelson
  in
  let tezos_context =
    Option.map ~f:(fun ({ tezos_context; _ } : options) -> tezos_context) options
  in
  let input =
    Trace.trace_tzresult_lwt ~raise Errors.parsing_input_tracer
    @@ Memory_proto_alpha.parse_michelson_data ?tezos_context input_michelson input_ty
  in
  let ty_stack_before = Script_typed_ir.Item_t (input_ty, Bot_t) in
  let ty_stack_after = Script_typed_ir.Item_t (output_ty, Bot_t) in
  let top_level = Script_tc_context.(add_lambda (init Data)) in
  let exp' =
    match exp with
    | Seq (_, [ Prim (_, "LAMBDA", [ _; _; v ], _) ]) -> v
    | _ -> failwith "not lambda"
  in
  let (descr : (_, _, _, _) descr) =
    Trace.trace_tzresult_lwt ~raise Errors.parsing_code_tracer
    @@ Memory_proto_alpha.parse_michelson_fail
         ~top_level
         exp'
         ty_stack_before
         ty_stack_after
  in
  let open! Memory_proto_alpha.Protocol.Script_interpreter in
  let res =
    Trace.trace_tzresult_lwt ~raise Errors.error_of_execution_tracer
    @@ Memory_proto_alpha.failure_interpret ?options descr input (EmptyCell, EmptyCell)
  in
  match res with
  | Memory_proto_alpha.Succeed output ->
    let ty, value =
      ex_value_ty_to_michelson ~raise (Ex_typed_value (output_ty, output))
    in
    Success (ty, value)
  | Memory_proto_alpha.Fail expr ->
    let expr =
      Tezos_micheline.Micheline.root
      @@ Memory_proto_alpha.Protocol.Michelson_v1_primitives.strings_of_prims expr
    in
    Fail expr


let run_expression
    ~raise
    ?options
    ?legacy
    (exp : _ Michelson.t)
    (exp_type : _ Michelson.t)
  =
  let open! Memory_proto_alpha.Protocol in
  let exp_type =
    Trace.trace_tzresult_lwt ~raise Errors.parsing_input_tracer
    @@ Memory_proto_alpha.prims_of_strings exp_type
  in
  let (Ex_ty exp_type') =
    Trace.trace_tzresult_lwt ~raise Errors.parsing_input_tracer
    @@ Memory_proto_alpha.parse_michelson_ty exp_type
  in
  let top_level = Script_tc_context.(init Data)
  and ty_stack_before = Script_typed_ir.Bot_t
  and ty_stack_after = Script_typed_ir.Item_t (exp_type', Bot_t) in
  let tezos_context =
    match options with
    | None -> None
    | Some o -> Some o.Memory_proto_alpha.tezos_context
  in
  let descr =
    Trace.trace_tzresult_lwt ~raise Errors.parsing_code_tracer
    @@ Memory_proto_alpha.parse_michelson_fail
         ?legacy
         ?tezos_context
         ~top_level
         exp
         ty_stack_before
         ty_stack_after
  in
  let open! Memory_proto_alpha.Protocol.Script_interpreter in
  let res =
    Trace.trace_tzresult_lwt ~raise Errors.error_of_execution_tracer
    @@ Memory_proto_alpha.failure_interpret ?options descr EmptyCell EmptyCell
  in
  match res with
  | Memory_proto_alpha.Succeed output ->
    let ty, value =
      ex_value_ty_to_michelson ~raise (Ex_typed_value (exp_type', output))
    in
    Success (ty, value)
  | Memory_proto_alpha.Fail expr ->
    let expr =
      Tezos_micheline.Micheline.root
      @@ Memory_proto_alpha.Protocol.Michelson_v1_primitives.strings_of_prims expr
    in
    Fail expr


let run_failwith ~raise ?options (exp : _ Michelson.t) (exp_type : _ Michelson.t)
    : (int, string) Tezos_micheline.Micheline.node
  =
  let expr = run_expression ~raise ?options exp exp_type in
  match expr with
  | Success _ ->
    raise.error
      Errors.main_unknown (* TODO : simple_fail "an error of execution was expected" *)
  | Fail res -> res


let run_no_failwith ~raise ?options (exp : _ Michelson.t) (exp_type : _ Michelson.t) =
  let expr = run_expression ~raise ?options exp exp_type in
  match expr with
  | Success tval -> tval
  | Fail _ ->
    raise.error
      Errors.main_unknown (* TODO : simple_fail "unexpected error of execution" *)


let evaluate_expression ~raise ?options exp exp_type =
  let etv = run_expression ~raise ?options exp exp_type in
  match etv with
  | Success (_, value) -> value
  | Fail res -> raise.error @@ Errors.main_execution_failed res


let evaluate_constant ~raise ?options exp exp_type =
  let etv = run_expression ~raise ?options exp exp_type in
  match etv with
  | Success (_, value) ->
    let value_ =
      Trace.trace_alpha_tzresult ~raise Errors.unparsing_michelson_tracer
      @@ Memory_proto_alpha.node_to_canonical value
    in
    let _, hash, _ =
      Trace.trace_alpha_tzresult_lwt ~raise (fun _ -> Errors.main_unknown)
      @@ Memory_proto_alpha.(
           register_constant (dummy_environment ()).tezos_context value_)
    in
    hash, value
  | Fail res -> raise.error @@ Errors.main_execution_failed res


let clean_expression exp =
  let open Tezos_micheline.Micheline in
  inject_locations (fun v -> v) (strip_locations exp)


let clean_constant ~raise exp =
  let open Tezos_micheline.Micheline in
  let value = inject_locations (fun v -> v) (strip_locations exp) in
  let value_ =
    Trace.trace_alpha_tzresult ~raise Errors.unparsing_michelson_tracer
    @@ Memory_proto_alpha.node_to_canonical value
  in
  let _, hash, _ =
    Trace.trace_alpha_tzresult_lwt ~raise (fun _ -> Errors.main_unknown)
    @@ Memory_proto_alpha.(register_constant (dummy_environment ()).tezos_context value_)
  in
  hash, value


module Checks = struct
  let member_opt k obj =
    let open Yojson.Basic.Util in
    obj |> member k |> to_option (fun x -> x)


  let member_exn k obj =
    let open Yojson.Basic.Util in
    match member_opt k obj with
    | Some v -> v
    | None -> raise (Type_error ("Member not found", obj))


  let rec yojson_to_json (x : Yojson.Basic.t) : Data_encoding.Json.t =
    match x with
    | `Bool b -> `Bool b
    | `Null -> `Null
    | `Assoc kvs -> `O (List.map ~f:(fun (k, v) -> k, yojson_to_json v) kvs)
    | `List vs -> `A (List.map ~f:yojson_to_json vs)
    | `Float f -> `Float f
    | `String s -> `String s
    | `Int n -> `String (string_of_int n)


  let michelsonStorageView_check ~name ~storage_type (json : Yojson.Basic.t) =
    let open Tezos_micheline in
    let open Simple_utils.Result in
    try
      let decode_json json =
        let encoding =
          Micheline_encoding.canonical_encoding
            ~variant:"michelsonStorageView"
            Data_encoding.string
        in
        let json = json |> Data_encoding.Json.destruct ~bson_relaxation:true encoding in
        let json = Micheline.inject_locations (fun i -> i) json in
        let* json =
          match Lwt_main.run (Memory_proto_alpha.prims_of_strings json) with
          | Ok code -> Ok code
          | Error _errs ->
            let str = Format.asprintf "could not parse code in view %s" name in
            fail (`Metadata_error_JSON_object str)
        in
        Ok json
      in
      let* returnType =
        json |> member_exn "returnType" |> yojson_to_json |> decode_json
      in
      let* parameter =
        match json |> member_opt "parameter" with
        | Some v -> v |> yojson_to_json |> decode_json
        | None -> Ok (Prim (0, T_unit, [], []))
      in
      let* storage_type =
        match Lwt_main.run (Memory_proto_alpha.prims_of_strings storage_type) with
        | Ok code -> Ok code
        | Error _errs ->
          let str = Format.asprintf "could not parse storage type" in
          fail (`Metadata_error_JSON_object str)
      in
      let* code = json |> member_exn "code" |> yojson_to_json |> decode_json in
      match
        Lwt_main.run
        @@ Proto_alpha_utils.Memory_proto_alpha.typecheck_view
             parameter
             returnType
             storage_type
             code
      with
      | Ok () -> Ok ()
      | Error _errs ->
        let str =
          Format.asprintf
            "could not successfully typecheck the view \"%s\" w.r.t. to parameter, \
             returntType and storage of the contract"
            name
        in
        fail (`Metadata_error_JSON_object str)
    with
    | _ ->
      fail
        (`Metadata_error_JSON_object
          "required returnType and code in michelsonStorageView")


  let restApiQuery_check (json : Yojson.Basic.t) =
    let open Simple_utils.Result in
    try
      let _specificationUri = json |> member_exn "specificationUri" in
      let _path = json |> member_exn "path" in
      Ok ()
    with
    | _ ->
      fail
        (`Metadata_error_JSON_object "required specificationUri and path in restApiQuery")


  let view_implementation_check ~name ~storage_type (json : Yojson.Basic.t) =
    match json |> member_opt "michelsonStorageView" with
    | Some json -> michelsonStorageView_check ~name ~storage_type json
    | None ->
      (match json |> member_opt "restApiQuery" with
      | Some json -> restApiQuery_check json
      | None -> Ok ())


  let view_check ~storage_type (json : Yojson.Basic.t) =
    let open Simple_utils.Result in
    let open Yojson.Basic.Util in
    try
      let name = json |> member_exn "name" |> to_string in
      let implementations = json |> member_exn "implementations" |> to_list in
      let* _ =
        implementations
        |> List.map ~f:(view_implementation_check ~name ~storage_type)
        |> Result.all
      in
      Ok ()
    with
    | _ -> fail (`Metadata_error_JSON_object "required name and implementations in view")


  let json_check ~loc ?sha256hash ~storage_type (s : string) =
    let open Simple_utils.Result in
    let open Yojson.Basic.Util in
    try
      let computed_hash =
        Hex.(show @@ of_bytes (Tezos_crypto.Hacl.Hash.SHA256.digest Bytes.(of_string s)))
      in
      let* () =
        match sha256hash with
        | Some sha256 ->
          let given_hash = String.chop_prefix_if_exists ~prefix:"0x" sha256 in
          if String.equal given_hash computed_hash
          then Ok ()
          else fail (`Metadata_hash_fails (computed_hash, given_hash))
        | None -> Ok ()
      in
      let json = Yojson.Basic.from_string s in
      match json |> member_opt "views" with
      | Some views ->
        let views = views |> to_list in
        let* _ = Result.all @@ List.map ~f:(view_check ~storage_type) views in
        Ok ()
      | None -> Ok ()
    with
    | Yojson.Json_error e -> fail (`Metadata_invalid_JSON (loc, e))
    | _ -> Ok ()


  type protocolInfo =
    { sha256hash : string option
    ; uri : Uri.t
    ; scheme : string
    }

  let uri_check (b : bytes) =
    let valid_protocols = [ "http"; "https"; "ipfs"; "tezos-storage" ] in
    let open Simple_utils.Option in
    let s = Bytes.to_string b in
    let uri = Uri.of_string @@ s in
    let* scheme = Uri.scheme uri in
    if List.mem ~equal:String.equal valid_protocols scheme
    then return { sha256hash = None; uri; scheme }
    else if String.equal "sha256" scheme
    then (
      let uri_path = Uri.path uri in
      let uri_path = String.chop_prefix_if_exists uri_path ~prefix:"/" in
      let sha256hash = Uri.host uri in
      let uri = Uri.(of_string @@ pct_decode uri_path) in
      let* scheme = Uri.scheme uri in
      if List.mem ~equal:String.equal valid_protocols scheme
      then return { sha256hash; uri; scheme }
      else None)
    else None


  let convert_item (item : (int, string) Tezos_micheline.Micheline.node) =
    match item with
    | Prim (_, "Elt", [ String (_, k); Bytes (_, v) ], _) -> Some (k, v)
    | _ -> None


  let convert (metadata : (int, string) Tezos_micheline.Micheline.node) =
    match metadata with
    | Seq (_, bigmap) -> Option.all (List.map ~f:convert_item bigmap)
    | _ -> None


  let download uri =
    let open Cohttp_lwt_unix in
    try
      let uri = Uri.of_string uri in
      let headers = Cohttp.Header.of_list [ "Content-type", "application/json" ] in
      let _, body = Lwt_main.run @@ Client.get ~headers uri in
      Some (Lwt_main.run @@ Cohttp_lwt.Body.to_string body)
    with
    | _ -> None


  let tzip16_check
      ~loc
      ?json_download
      ~storage_type
      (metadata : (int, string) Tezos_micheline.Micheline.node)
    =
    let open Simple_utils.Result in
    let* items = of_option ~error:(`Metadata_cannot_parse loc) @@ convert metadata in
    let* _, root =
      of_option ~error:(`Metadata_no_empty_key loc)
      @@ List.find ~f:(fun (k, _) -> String.equal k "") items
    in
    let* { uri; scheme; sha256hash } =
      of_option ~error:(`Metadata_not_valid_URI (loc, Bytes.to_string root))
      @@ uri_check root
    in
    match scheme with
    | "tezos-storage" ->
      (match Uri.host uri with
      | None ->
        (* In case of empty host, the context is current contract (and thus current storage value) *)
        let location = Uri.path uri in
        let location = String.chop_prefix_if_exists location ~prefix:"/" in
        let* () =
          of_option ~error:(`Metadata_slash_not_valid_URI (loc, location))
          @@ Simple_utils.Option.some_if (not @@ String.contains location '/') ()
        in
        let location = Uri.pct_decode location in
        let* _, json =
          of_option ~error:(`Metadata_tezos_storage_not_found (loc, location))
          @@ List.find ~f:(fun (k, _) -> String.equal k location) items
        in
        json_check ~loc ?sha256hash ~storage_type Bytes.(to_string json)
      | Some _ -> return ())
    | "https" | "http" ->
      let uri = Uri.to_string uri in
      (match json_download with
      | None -> fail (`Metadata_json_download "an HTTP")
      | Some false -> return ()
      | Some true ->
        (match download uri with
        | None -> fail (`Metadata_error_download uri)
        | Some json -> json_check ~loc ~storage_type ?sha256hash json))
    | "ipfs" ->
      (match Uri.host uri with
      | None -> return ()
      | Some domain ->
        let uri = "https://ipfs.io/ipfs/" ^ domain in
        (match json_download with
        | None -> fail (`Metadata_json_download "an IPFS")
        | Some false -> return ()
        | Some true ->
          (match download uri with
          | None -> fail (`Metadata_error_download uri)
          | Some json -> json_check ~loc ~storage_type ?sha256hash json)))
    | _ -> return ()


  let is_annoted_element
      annot
      (type_ : (Mini_c.meta, string) Tezos_micheline.Micheline.node)
    =
    let open Tezos_micheline.Micheline in
    match type_ with
    | Prim (_, _, _, ss) -> List.mem ~equal:String.equal ss annot
    | _ -> false


  let rec find_annoted_element
      annot
      (type_ : (Mini_c.meta, string) Tezos_micheline.Micheline.node)
      (value : (int, string) Tezos_micheline.Micheline.node)
    =
    let open Tezos_micheline.Micheline in
    match type_, value with
    | Prim (_, "pair", types, _), Prim (_, "Pair", values, _) ->
      (match List.findi types ~f:(fun _ type_ -> is_annoted_element annot type_) with
      | Some (idx, _) -> List.nth values idx
      | _ ->
        let f idx type_ =
          match type_ with
          | Prim (_, "pair", _, _) ->
            find_annoted_element annot type_ (List.nth_exn values idx)
          | _ -> None
        in
        List.find_mapi types ~f)
    | _ -> None


  let get_bigmap_value key (value : (int, string) Tezos_micheline.Micheline.node) =
    let open Tezos_micheline.Micheline in
    match value with
    | Prim (_, "Elt", [ String (_, k); v ], _) when String.equal key k -> Some v
    | _ -> None


  let storage
      ~raise
      ~(options : Compiler_options.t)
      ~type_
      ~loc
      (exp : (int, string) Tezos_micheline.Micheline.node)
    =
    if not options.middle_end.no_metadata_check
    then (
      match find_annoted_element "%metadata" type_ exp with
      | Some metadata ->
        let open Simple_utils.Trace in
        (match
           tzip16_check
             ~loc
             ?json_download:options.tools.json_download
             ~storage_type:type_
             metadata
         with
        | Ok () -> ()
        | Error w -> raise.warning w)
      | _ -> ())
    else ()
end
