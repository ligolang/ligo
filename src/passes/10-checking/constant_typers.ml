module H=Helpers
module Ligo_proto = Environment.Protocols
module Option = Simple_utils.Option
open Simple_utils.Trace
open Errors
open Ast_typed
open H

(*
  Each constant has its own type.

  LIGO's type-system is currently too
  weak to express the constant's type. For instance:
  - "ADD" has a special kind of type of polymorphism. If "ADD" gets two `int`s,
    it will return an `int`. If it gets two `nat`s, it will return a `nat`.
    Regular polymorphism wouldn't work because "ADD" only accepts `int`s or
    `nat`s.
  - "NONE" (from Some/None) requires an annotation.

  Instead of a LIGO type, constant types are representend as functions. These
  functions take as parameters:
  - The list of types of the arguments of the constants. When typing `2 + 2`,
    the types might be `[ int ; int ]`.
  - The expected type of the whole expression. It is optional. When typing
    `[] : list(operation)`, it will be `Some ( list (operation) )`. When
    typing `2 + 2` (with no additional context), it will be `None`.
  The output is the type of the whole expression. An error is returned through
  the Trace monad if it doesn't type-check (`"toto" + 42`).

  Various helpers are defined bellow.
*)

let simple_comparator ~raise : Location.t -> string -> typer = fun loc s -> typer_2 ~raise loc s @@ fun a b ->
  let () =
    Assert.assert_true ~raise (uncomparable_types loc a b) @@
    List.exists ~f:(eq_2 (a , b)) [
      t_address () ;
      t_bool () ;
      t_bytes () ;
      t_chain_id ();
      t_int () ;
      t_key () ;
      t_key_hash () ;
      t_mutez () ;
      t_nat () ;
      t_signature ();
      t_string () ;
      t_timestamp () ;
      t_unit ();
      t_never ();
      t_michelson_code () ;
    ] in
  t_bool ()

let rec record_comparator ~raise ~test : Location.t -> string -> typer = fun loc s -> typer_2 ~raise loc s @@ fun a b ->
  let () =
    Assert.assert_true ~raise (uncomparable_types loc a b) @@ eq_1 a b
  in
  let a_r =
    trace_option ~raise (comparator_composed loc a) @@
    get_t_record a in
  let b_r = trace_option ~raise (expected_variant loc b) @@ get_t_record b in
  let aux a b : type_expression =
    comparator ~cmp:s ~raise ~test loc [a.associated_type;b.associated_type] None
  in
  let _ = List.map2_exn ~f:aux (LMap.to_list a_r.content) (LMap.to_list b_r.content) in
  t_bool ()

and sum_comparator ~raise ~test : Location.t -> string -> typer = fun loc s -> typer_2 ~raise loc s @@ fun a b ->
  let () =
    Assert.assert_true ~raise (uncomparable_types loc a b) @@ eq_1 a b
  in
  let a_r =
    trace_option ~raise (comparator_composed loc a) @@
    get_t_sum a in
  let b_r = trace_option ~raise (expected_variant loc b) @@ get_t_sum b in
  let aux a b : type_expression =
    comparator ~cmp:s ~raise ~test loc [a.associated_type;b.associated_type] None
  in
  let _ = List.map2_exn ~f:aux (LMap.to_list a_r.content) (LMap.to_list b_r.content) in
  t_bool ()

and list_comparator ~raise ~test : Location.t -> string -> typer = fun loc s -> typer_2 ~raise loc s @@ fun a_lst b_lst ->
  let () =
    Assert.assert_true ~raise (uncomparable_types loc a_lst b_lst) @@ eq_1 a_lst b_lst
  in
  let a =
    trace_option ~raise (comparator_composed loc a_lst) @@
    get_t_list a_lst in
  let b = trace_option ~raise (expected_option loc b_lst) @@ get_t_list b_lst in
  comparator ~cmp:s ~raise ~test loc [a;b] None

and set_comparator ~raise ~test : Location.t -> string -> typer = fun loc s -> typer_2 ~raise loc s @@ fun a_set b_set ->
  let () =
    Assert.assert_true ~raise (uncomparable_types loc a_set b_set) @@ eq_1 a_set b_set
  in
  let a =
    trace_option ~raise (comparator_composed loc a_set) @@
    get_t_set a_set in
  let b = trace_option ~raise (expected_option loc b_set) @@ get_t_set b_set in
  comparator ~cmp:s ~raise ~test loc [a;b] None

and map_comparator ~raise ~test : Location.t -> string -> typer = fun loc s -> typer_2 ~raise loc s @@ fun a_map b_map ->
  let () =
    Assert.assert_true ~raise (uncomparable_types loc a_map b_map) @@ eq_1 a_map b_map
  in
  let (a_key, a_value) =
    trace_option ~raise (comparator_composed loc a_map) @@
    get_t_map a_map in
  let (b_key, b_value) = trace_option ~raise (expected_option loc b_map) @@ get_t_map b_map in
  let _ = comparator ~cmp:s ~raise ~test loc [a_key;b_key] None in
  let _ = comparator ~cmp:s ~raise ~test loc [a_value;b_value] None in
  t_bool ()

and big_map_comparator ~raise ~test : Location.t -> string -> typer = fun loc s -> typer_2 ~raise loc s @@ fun a_map b_map ->
  let () =
    Assert.assert_true ~raise (uncomparable_types loc a_map b_map) @@ eq_1 a_map b_map
  in
  let (a_key, a_value) =
    trace_option ~raise (comparator_composed loc a_map) @@
    get_t_big_map a_map in
  let (b_key, b_value) = trace_option ~raise (expected_option loc b_map) @@ get_t_big_map b_map in
  let _ = comparator ~cmp:s ~raise ~test loc [a_key;b_key] None in
  let _ = comparator ~cmp:s ~raise ~test loc [a_value;b_value] None in
  t_bool ()

and option_comparator ~raise ~test : Location.t -> string -> typer = fun loc s -> typer_2 ~raise loc s @@ fun a_opt b_opt ->
  let () =
    Assert.assert_true ~raise (uncomparable_types loc a_opt b_opt) @@ eq_1 a_opt b_opt
  in
  let a =
    trace_option ~raise (comparator_composed loc a_opt) @@
    get_t_option a_opt in
  let b = trace_option ~raise (expected_option loc b_opt) @@ get_t_option b_opt in
  comparator ~cmp:s ~raise ~test loc [a;b] None

and comparator ~cmp ~raise ~test : Location.t -> typer = fun loc -> typer_2 ~raise loc cmp @@ fun a b ->
  if test
  then
    bind_exists ~raise @@ List.Ne.of_list [list_comparator ~test loc cmp [a;b] None;
                                           set_comparator ~test loc cmp [a;b] None;
                                           map_comparator ~test loc cmp [a;b] None;
                                           simple_comparator loc cmp [a;b] None;
                                           option_comparator ~test loc cmp [a;b] None;
                                           record_comparator ~test loc cmp [a;b] None;
                                           sum_comparator ~test loc cmp [a;b] None;
                                           big_map_comparator ~test loc cmp [a;b] None]
  else
    bind_exists ~raise @@ List.Ne.of_list [simple_comparator loc cmp [a;b] None;
                                           option_comparator ~test loc cmp [a;b] None;
                                           record_comparator ~test loc cmp [a;b] None;
                                           sum_comparator ~test loc cmp [a;b] None]

let sapling_verify_update ~raise loc = typer_2 ~raise loc "SAPLING_VERIFY_UPDATE" @@ fun tr state ->
  let singleton_tr = trace_option ~raise (expected_sapling_transaction loc tr) @@ get_t_sapling_transaction tr in
  let singleton_state = trace_option ~raise (expected_sapling_state loc state) @@ get_t_sapling_state state in
  let () = assert_eq_1 ~raise ~loc singleton_tr singleton_state in
  (t_option (t_pair (t_int ()) state))

let sapling_empty_state ~raise loc = typer_0 ~raise loc "SAPLING_EMPTY_STATE" @@ fun tv_opt ->
  trace_option ~raise (not_annotated loc) @@ tv_opt

let test_to_contract ~raise loc = typer_1 ~raise loc "TEST_TO_CONTRACT" @@ fun t ->
  let param_ty, _ = trace_option ~raise (expected_typed_address loc t) @@
                       get_t_typed_address t in
  let param_ty = Option.value (Ast_typed.Helpers.get_entrypoint "default" param_ty) ~default:param_ty in
  (t_contract param_ty)

let test_to_entrypoint ~raise loc = typer_2_opt ~raise loc "TEST_TO_ENTRYPOINT" @@ fun entry_tv contract_tv tv_opt ->
  let t_string = t_string () in
  let () = assert_eq_1 ~raise ~loc entry_tv t_string in
  let _ = trace_option ~raise (expected_contract loc contract_tv) @@
             get_t_typed_address contract_tv in
  let tv = trace_option ~raise (not_annotated loc) tv_opt in
  let tv' = trace_option ~raise (expected_contract loc tv) @@ get_t_contract tv in
  t_contract tv'

let test_originate_from_file ~protocol_version ~raise loc =
  match (protocol_version : Ligo_proto.t) with
  | Edo ->
    typer_4 ~raise loc "TEST_ORIGINATE_FROM_FILE" @@ fun source_file entrypoint storage balance ->
      let () = trace_option ~raise (expected_string loc source_file) @@ assert_t_string source_file in
      let () = trace_option ~raise (expected_string loc entrypoint) @@ assert_t_string entrypoint in
      let () = trace_option ~raise (expected_michelson_code loc storage) @@ assert_t_michelson_code storage in
      let () = assert_eq_1 ~raise ~loc balance (t_mutez ()) in
      (t_triplet (t_address ()) (t_michelson_code ()) (t_int ()))
  | Hangzhou ->
    typer_5 ~raise loc "TEST_ORIGINATE_FROM_FILE" @@ fun source_file entrypoint views storage balance ->
      let tlist = trace_option ~raise (expected_list loc views) @@ get_t_list views in
      let () = trace_option ~raise (expected_string loc tlist) @@ assert_t_string tlist in
      let () = trace_option ~raise (expected_string loc source_file) @@ assert_t_string source_file in
      let () = trace_option ~raise (expected_string loc entrypoint) @@ assert_t_string entrypoint in
      let () = trace_option ~raise (expected_michelson_code loc storage) @@ assert_t_michelson_code storage in
      let () = assert_eq_1 ~raise ~loc balance (t_mutez ()) in
      (t_triplet (t_address ()) (t_michelson_code ()) (t_int ()))

type typer = error:[`TC of O.type_expression list] list ref -> raise:Errors.typer_error raise -> test:bool -> protocol_version:Ligo_proto.t -> loc:Location.t -> O.type_expression list -> O.type_expression option -> O.type_expression option

let typer_of_ligo_type ?(add_tc = true) ?(fail = true) lamb_type : typer = fun ~error ~raise ~test ~protocol_version ~loc lst tv_opt ->
  ignore test; ignore protocol_version;
  let _, lamb_type = O.Helpers.destruct_for_alls lamb_type in
  Simple_utils.Trace.try_with (fun ~raise ->
      let table = infer_type_applications ~raise ~loc ~default_error:(fun loc t t' -> `Outer_error (loc, t', t)) lamb_type lst tv_opt in
      let lamb_type = H.TMap.fold (fun tv t r -> Ast_typed.Helpers.subst_type tv t r) table lamb_type in
      let _, tv = Ast_typed.Helpers.destruct_arrows_n lamb_type (List.length lst) in
      Some tv)
    (function
     | `Outer_error (loc, t', t) ->
        if fail then raise.raise (assert_equal loc t' t) else None
     | _ ->
        let arrs, _ = O.Helpers.destruct_arrows lamb_type in
        if add_tc then error := `TC arrs :: ! error else ();
        None)

let typer_of_old_typer (typer : protocol_version:_ -> raise:_ -> test:_ -> _ -> O.type_expression list -> O.type_expression option -> O.type_expression) : typer =
  fun ~error ~raise ~test ~protocol_version ~loc lst tv_opt ->
  ignore error;
  Some (typer ~protocol_version ~raise ~test loc lst tv_opt)

let typer_of_comparator (typer : raise:_ -> test:_ -> _ -> O.type_expression list -> O.type_expression option -> O.type_expression) : typer =
  fun ~error ~raise ~test ~protocol_version ~loc lst tv_opt ->
  ignore error; ignore protocol_version;
  Some (typer ~raise ~test loc lst tv_opt)

let raise_of_errors ~raise ~loc lst = function
  | [] ->
     raise.raise @@ (corner_case "Cannot find a suitable type for expression")
  | xs ->
     let tc = List.filter_map ~f:(function `TC v -> Some v) xs in
     raise.raise @@ typeclass_error loc (List.rev (List.map ~f:List.rev tc)) lst

let rec any_of : typer list -> typer = fun typers ->
  fun ~error ~raise ~test ~protocol_version ~loc lst tv_opt ->
  match typers with
  | [] -> raise_of_errors ~raise ~loc lst (! error)
  | typer :: typers ->
     match typer ~error ~raise ~test ~protocol_version ~loc lst tv_opt with
     | Some tv -> Some tv
     | None -> any_of typers ~error ~raise ~test ~protocol_version ~loc lst tv_opt

let only_supported_hangzhou c (typer : typer) : typer =
  fun ~error ~raise ~test ~protocol_version ~loc lst tv_opt ->
  match protocol_version with
  | Ligo_proto.Hangzhou -> typer ~error ~raise ~test ~protocol_version ~loc lst tv_opt
  | Ligo_proto.Edo ->
    raise.raise @@ corner_case (
      Format.asprintf "Unsupported constant %a in protocol %s"
        PP.constant' c
        (Ligo_proto.variant_to_string protocol_version)
    )

module CTMap = Simple_utils.Map.Make(struct type t = O.constant' let compare x y = O.Compare.constant' x y end)

type t = typer CTMap.t
let names : (string * O.type_expression) list ref = ref []

module Constant_types = struct

  let a_var = O.Var.of_input_var "'a"
  let b_var = O.Var.of_input_var "'b"
  let c_var = O.Var.of_input_var "'c"

  let of_ligo_type ?name t =
    let () = match name with
      | None -> ()
      | Some name -> names := (name, t) :: ! names in
    any_of [typer_of_ligo_type t]

  let mk_typer c t =
    try
      let name = Predefined.Tree_abstraction.pseudo_module_to_string c in
      (c, of_ligo_type ~name t)
    with
      (Failure _) -> (c, of_ligo_type t)

  let mk_typer_only_hangzhou c t =
    try
      let name = Predefined.Tree_abstraction.pseudo_module_to_string c in
      (c, only_supported_hangzhou c @@ of_ligo_type ~name t)
    with
      (Failure _) -> (c, only_supported_hangzhou c @@ of_ligo_type t)

  let typer_of_ligo_type_no_tc t =
    typer_of_ligo_type ~add_tc:false ~fail:false t

  let typer_of_ligo_type ?name t =
    let () = match name with
      | None -> ()
      | Some name -> names := (name, t) :: ! names in
    typer_of_ligo_type t

  let tbl : t = CTMap.of_list [
                    (* LOOPS *)
                    mk_typer C_FOLD_WHILE O.(t_for_all a_var () (t_arrow (t_arrow (t_variable a_var ()) (t_pair (t_bool ()) (t_variable a_var ())) ()) (t_arrow (t_variable a_var ()) (t_variable a_var ()) ()) ()));
                    mk_typer C_FOLD_CONTINUE O.(t_for_all a_var () (t_arrow (t_variable a_var ()) (t_pair (t_bool ()) (t_variable a_var ())) ()));
                    mk_typer C_FOLD_STOP O.(t_for_all a_var () (t_arrow (t_variable a_var ()) (t_pair (t_bool ()) (t_variable a_var ())) ()));
                    (C_FOLD, any_of [
                                 typer_of_ligo_type O.(t_for_all a_var () (t_for_all b_var () (t_arrow (t_arrow (t_pair (t_variable a_var ()) (t_variable b_var ())) (t_variable a_var ()) ()) (t_arrow (t_list (t_variable b_var ())) (t_arrow (t_variable a_var ()) (t_variable a_var ()) ()) ()) ())));
                                 typer_of_ligo_type O.(t_for_all a_var () (t_for_all b_var () (t_arrow (t_arrow (t_pair (t_variable a_var ()) (t_variable b_var ())) (t_variable a_var ()) ()) (t_arrow (t_set (t_variable b_var ())) (t_arrow (t_variable a_var ()) (t_variable a_var ()) ()) ()) ())));
                    ]);
                    (* MAP *)
                    mk_typer C_MAP_EMPTY O.(t_for_all a_var () (t_for_all b_var () (t_map (t_variable a_var ()) (t_variable b_var ()))));
                    mk_typer C_BIG_MAP_EMPTY O.(t_for_all a_var () (t_for_all b_var () (t_big_map (t_variable a_var ()) (t_variable b_var ()))));
                    (C_MAP_ADD, any_of [
                                    typer_of_ligo_type O.(t_for_all a_var () (t_for_all b_var () (t_arrow (t_variable a_var ()) (t_arrow (t_variable b_var ()) (t_arrow (t_map (t_variable a_var ()) (t_variable b_var ())) (t_map (t_variable a_var ()) (t_variable b_var ())) ()) ()) ())));
                                    typer_of_ligo_type O.(t_for_all a_var () (t_for_all b_var () (t_arrow (t_variable a_var ()) (t_arrow (t_variable b_var ()) (t_arrow (t_big_map (t_variable a_var ()) (t_variable b_var ())) (t_big_map (t_variable a_var ()) (t_variable b_var ())) ()) ()) ())));
                    ]);
                    (C_MAP_REMOVE, any_of [
                                       typer_of_ligo_type O.(t_for_all a_var () (t_for_all b_var () (t_arrow (t_variable a_var ()) (t_arrow (t_map (t_variable a_var ()) (t_variable b_var ())) (t_map (t_variable a_var ()) (t_variable b_var ())) ()) ())));
                                       typer_of_ligo_type O.(t_for_all a_var () (t_for_all b_var () (t_arrow (t_variable a_var ()) (t_arrow (t_big_map (t_variable a_var ()) (t_variable b_var ())) (t_big_map (t_variable a_var ()) (t_variable b_var ())) ()) ())));
                    ]);
                    (C_MAP_UPDATE, any_of [
                                       typer_of_ligo_type O.(t_for_all a_var () (t_for_all b_var () (t_arrow (t_variable a_var ()) (t_arrow (t_option (t_variable b_var ())) (t_arrow (t_map (t_variable a_var ()) (t_variable b_var ())) (t_map (t_variable a_var ()) (t_variable b_var ())) ()) ()) ())));
                                       typer_of_ligo_type O.(t_for_all a_var () (t_for_all b_var () (t_arrow (t_variable a_var ()) (t_arrow (t_option (t_variable b_var ())) (t_arrow (t_big_map (t_variable a_var ()) (t_variable b_var ())) (t_big_map (t_variable a_var ()) (t_variable b_var ())) ()) ()) ())));
                    ]);
                    mk_typer C_MAP_GET_AND_UPDATE O.(t_for_all a_var () (t_for_all b_var () (t_arrow (t_variable a_var ()) (t_arrow (t_option (t_variable b_var ())) (t_arrow (t_map (t_variable a_var ()) (t_variable b_var ())) (t_pair (t_option (t_variable b_var ())) (t_map (t_variable a_var ()) (t_variable b_var ()))) ()) ()) ())));
                    mk_typer C_BIG_MAP_GET_AND_UPDATE O.(t_for_all a_var () (t_for_all b_var () (t_arrow (t_variable a_var ()) (t_arrow (t_option (t_variable b_var ())) (t_arrow (t_big_map (t_variable a_var ()) (t_variable b_var ())) (t_pair (t_option (t_variable b_var ())) (t_big_map (t_variable a_var ()) (t_variable b_var ()))) ()) ()) ())));
                    (C_MAP_FIND_OPT, any_of [
                                         typer_of_ligo_type O.(t_for_all a_var () (t_for_all b_var () (t_arrow (t_variable a_var ()) (t_arrow (t_map (t_variable a_var ()) (t_variable b_var ())) (t_option (t_variable b_var ())) ()) ())));
                                         typer_of_ligo_type O.(t_for_all a_var () (t_for_all b_var () (t_arrow (t_variable a_var ()) (t_arrow (t_big_map (t_variable a_var ()) (t_variable b_var ())) (t_option (t_variable b_var ())) ()) ())));
                    ]);
                    (C_MAP_FIND, any_of [
                                     typer_of_ligo_type O.(t_for_all a_var () (t_for_all b_var () (t_arrow (t_variable a_var ()) (t_arrow (t_map (t_variable a_var ()) (t_variable b_var ())) (t_variable b_var ()) ()) ())));
                                     typer_of_ligo_type O.(t_for_all a_var () (t_for_all b_var () (t_arrow (t_variable a_var ()) (t_arrow (t_big_map (t_variable a_var ()) (t_variable b_var ())) (t_variable b_var ()) ()) ())));
                    ]);
                    (C_MAP_MEM, any_of [
                                    typer_of_ligo_type O.(t_for_all a_var () (t_for_all b_var () (t_arrow (t_variable a_var ()) (t_arrow (t_map (t_variable a_var ()) (t_variable b_var ())) (t_bool ()) ()) ())));
                                    typer_of_ligo_type O.(t_for_all a_var () (t_for_all b_var () (t_arrow (t_variable a_var ()) (t_arrow (t_big_map (t_variable a_var ()) (t_variable b_var ())) (t_bool ()) ()) ())));
                    ]);
                    (C_MAP_MAP, of_ligo_type O.(t_for_all a_var () (t_for_all b_var () (t_for_all c_var () (t_arrow (t_arrow (t_pair (t_variable a_var ()) (t_variable b_var ())) (t_variable c_var ()) ()) (t_arrow (t_map (t_variable a_var ()) (t_variable b_var ())) (t_map (t_variable a_var ()) (t_variable c_var ())) ()) ())))));
                    (C_MAP_ITER, of_ligo_type O.(t_for_all a_var () (t_for_all b_var () (t_arrow (t_arrow (t_pair (t_variable a_var ()) (t_variable b_var ())) (t_unit ()) ()) (t_arrow (t_map (t_variable a_var ()) (t_variable b_var ())) (t_unit ()) ()) ()))));
                    (C_MAP_FOLD, of_ligo_type O.(t_for_all a_var () (t_for_all b_var () (t_for_all c_var () (t_arrow (t_arrow (t_pair (t_variable c_var ()) (t_pair (t_variable a_var ()) (t_variable b_var ()))) (t_variable c_var ()) ()) (t_arrow (t_map (t_variable a_var ()) (t_variable b_var ())) (t_arrow (t_variable c_var ()) (t_variable c_var ()) ()) ()) ())))));
                    (* LIST *)
                    mk_typer C_LIST_EMPTY O.(t_for_all a_var () (t_list (t_variable a_var ())));
                    mk_typer C_CONS O.(t_for_all a_var () (t_arrow (t_variable a_var ()) (t_arrow (t_list (t_variable a_var ())) (t_list (t_variable a_var ())) ()) ()));
                    mk_typer C_LIST_MAP O.(t_for_all a_var () (t_for_all b_var () (t_arrow (t_arrow (t_variable a_var ()) (t_variable b_var ()) ()) (t_arrow (t_list (t_variable a_var ())) (t_list (t_variable b_var ())) ()) ())));
                    mk_typer C_LIST_ITER O.(t_for_all a_var () (t_arrow (t_arrow (t_variable a_var ()) (t_unit ()) ()) (t_arrow (t_list (t_variable a_var ())) (t_unit ()) ()) ()));
                    mk_typer C_LIST_FOLD O.(t_for_all a_var () (t_for_all b_var () (t_arrow (t_arrow (t_pair (t_variable b_var ()) (t_variable a_var ())) (t_variable b_var ()) ()) (t_arrow (t_list (t_variable a_var ())) (t_arrow (t_variable b_var ()) (t_variable b_var ()) ()) ()) ())));
                    mk_typer C_LIST_FOLD_LEFT O.(t_for_all a_var () (t_for_all b_var () (t_arrow (t_arrow (t_pair (t_variable b_var ()) (t_variable a_var ())) (t_variable b_var ()) ()) (t_arrow (t_variable b_var ()) (t_arrow (t_list (t_variable a_var ())) (t_variable b_var ()) ()) ()) ())));
                    mk_typer C_LIST_FOLD_RIGHT O.(t_for_all a_var () (t_for_all b_var () (t_arrow (t_arrow (t_pair (t_variable a_var ()) (t_variable b_var ())) (t_variable b_var ()) ()) (t_arrow (t_list (t_variable a_var ())) (t_arrow (t_variable b_var ()) (t_variable b_var ()) ()) ()) ())));
                    mk_typer C_LIST_HEAD_OPT O.(t_for_all a_var () (t_arrow (t_list (t_variable a_var ())) (t_option (t_variable a_var ())) ()));
                    mk_typer C_LIST_TAIL_OPT O.(t_for_all a_var () (t_arrow (t_list (t_variable a_var ())) (t_option (t_list (t_variable a_var ()))) ()));
                    (* SET *)
                    mk_typer C_SET_EMPTY O.(t_for_all a_var () (t_set (t_variable a_var ())));
                    mk_typer C_SET_LITERAL O.(t_for_all a_var () (t_arrow (t_list (t_variable a_var ())) (t_set (t_variable a_var ())) ()));
                    mk_typer C_SET_MEM O.(t_for_all a_var () (t_arrow (t_variable a_var ()) (t_arrow (t_set (t_variable a_var ())) (t_bool ()) ()) ()));
                    mk_typer C_SET_ADD O.(t_for_all a_var () (t_arrow (t_variable a_var ()) (t_arrow (t_set (t_variable a_var ())) (t_set (t_variable a_var ())) ()) ()));
                    mk_typer C_SET_REMOVE O.(t_for_all a_var () (t_arrow (t_variable a_var ()) (t_arrow (t_set (t_variable a_var ())) (t_set (t_variable a_var ())) ()) ()));
                    mk_typer C_SET_UPDATE O.(t_for_all a_var () (t_arrow (t_variable a_var ()) (t_arrow (t_bool ()) (t_arrow (t_set (t_variable a_var ())) (t_set (t_variable a_var ())) ()) ()) ()));
                    mk_typer C_SET_ITER O.(t_for_all a_var () (t_arrow (t_arrow (t_variable a_var ()) (t_unit ()) ()) (t_arrow (t_set (t_variable a_var ())) (t_unit ()) ()) ()));
                    mk_typer C_SET_FOLD O.(t_for_all a_var () (t_for_all b_var () (t_arrow (t_arrow (t_pair (t_variable b_var ()) (t_variable a_var ())) (t_variable b_var ()) ()) (t_arrow (t_set (t_variable a_var ())) (t_arrow (t_variable b_var ()) (t_variable b_var ()) ()) ()) ())));
                    mk_typer C_SET_FOLD_DESC O.(t_for_all a_var () (t_for_all b_var () (t_arrow (t_arrow (t_pair (t_variable a_var ()) (t_variable b_var ())) (t_variable b_var ()) ()) (t_arrow (t_set (t_variable a_var ())) (t_arrow (t_variable b_var ()) (t_variable b_var ()) ()) ()) ())));
                    (* ADHOC POLY *)
                    (C_SIZE, any_of [
                                 typer_of_ligo_type O.(t_for_all a_var () (t_arrow (t_list (t_variable a_var ())) (t_nat ()) ()));
                                 typer_of_ligo_type O.(t_arrow (t_bytes ()) (t_nat ()) ());
                                 typer_of_ligo_type O.(t_arrow (t_string ()) (t_nat ()) ());
                                 typer_of_ligo_type O.(t_for_all a_var () (t_arrow (t_set (t_variable a_var ())) (t_nat ()) ()));
                                 typer_of_ligo_type O.(t_for_all a_var () (t_for_all b_var () (t_arrow (t_map (t_variable a_var ()) (t_variable b_var ())) (t_nat ()) ())))
                    ]);
                    (C_CONCAT, any_of [
                                   typer_of_ligo_type ~name:"String.concat" O.(t_arrow (t_string ()) (t_arrow (t_string ()) (t_string ()) ()) ());
                                   typer_of_ligo_type ~name:"Bytes.concat"  O.(t_arrow (t_bytes ()) (t_arrow (t_bytes ()) (t_bytes ()) ()) ());
                    ]);
                    (C_SLICE, any_of [
                                  typer_of_ligo_type ~name:"String.sub" O.(t_arrow (t_nat ()) (t_arrow (t_nat ()) (t_arrow (t_string ()) (t_string ()) ()) ()) ());
                                  typer_of_ligo_type ~name:"Bytes.sub" O.(t_arrow (t_nat ()) (t_arrow (t_nat ()) (t_arrow (t_bytes ()) (t_bytes ()) ()) ()) ());
                    ]);
                    mk_typer C_BYTES_PACK O.(t_for_all a_var () (t_arrow (t_variable a_var ()) (t_bytes ()) ()));
                    mk_typer C_BYTES_UNPACK O.(t_for_all a_var () (t_arrow (t_bytes ()) (t_option (t_variable a_var ())) ()));
                    (* CRYPTO *)
                    mk_typer C_SHA256 O.(t_arrow (t_bytes ()) (t_bytes ()) ());
                    mk_typer C_SHA512 O.(t_arrow (t_bytes ()) (t_bytes ()) ());
                    mk_typer C_SHA3 O.(t_arrow (t_bytes ()) (t_bytes ()) ());
                    mk_typer C_KECCAK O.(t_arrow (t_bytes ()) (t_bytes ()) ());
                    mk_typer C_BLAKE2b O.(t_arrow (t_bytes ()) (t_bytes ()) ());
                    mk_typer C_HASH_KEY O.(t_arrow (t_key ()) (t_key_hash ()) ());
                    mk_typer C_CHECK_SIGNATURE O.(t_arrow (t_key ()) (t_arrow (t_signature ()) (t_arrow (t_bytes ()) (t_bool ()) ()) ()) ());
                    (* OPTION *)
                    mk_typer C_NONE O.(t_for_all a_var () (t_option (t_variable a_var ())));
                    mk_typer C_SOME O.(t_for_all a_var () (t_arrow (t_variable a_var ()) (t_option (t_variable a_var ())) ()));
                    mk_typer C_UNOPT O.(t_for_all a_var () (t_arrow (t_option (t_variable a_var ())) (t_variable a_var ()) ()));
                    mk_typer C_UNOPT_WITH_ERROR O.(t_for_all a_var () (t_arrow (t_option (t_variable a_var ())) (t_arrow (t_string ()) (t_variable a_var ()) ()) ()));
                    (* GLOBAL *)
                    mk_typer C_ASSERTION O.(t_arrow (t_bool ()) (t_unit ()) ());
                    mk_typer C_ASSERTION_WITH_ERROR O.(t_arrow (t_bool ()) (t_arrow (t_string ()) (t_unit ()) ()) ());
                    mk_typer C_ASSERT_SOME O.(t_for_all a_var () (t_arrow (t_option (t_variable a_var ())) (t_unit ()) ()));
                    mk_typer C_ASSERT_SOME_WITH_ERROR O.(t_for_all a_var () (t_arrow (t_option (t_variable a_var ())) (t_arrow (t_string ()) (t_unit ()) ()) ()));
                    mk_typer C_ASSERT_NONE O.(t_for_all a_var () (t_arrow (t_option (t_variable a_var ())) (t_unit ()) ()));
                    mk_typer C_ASSERT_NONE_WITH_ERROR O.(t_for_all a_var () (t_arrow (t_option (t_variable a_var ())) (t_arrow (t_string ()) (t_unit ()) ()) ()));
                    (C_FAILWITH, any_of [
                                     typer_of_ligo_type_no_tc @@ O.(t_arrow (t_string ()) (t_unit ()) ());
                                     typer_of_ligo_type_no_tc @@ O.(t_arrow (t_nat ()) (t_unit ()) ());
                                     typer_of_ligo_type_no_tc @@ O.(t_arrow (t_int ()) (t_unit ()) ());
                                     typer_of_ligo_type O.(t_for_all a_var () (t_arrow (t_string ()) (t_variable a_var ()) ()));
                                     typer_of_ligo_type O.(t_for_all a_var () (t_arrow (t_nat ()) (t_variable a_var ()) ()));
                                     typer_of_ligo_type O.(t_for_all a_var () (t_arrow (t_int ()) (t_variable a_var ()) ()));
                    ]);
                    mk_typer C_AMOUNT O.(t_mutez ());
                    mk_typer C_BALANCE O.(t_mutez ());
                    mk_typer C_LEVEL O.(t_nat ());
                    mk_typer C_SENDER O.(t_address ());
                    mk_typer C_SOURCE O.(t_address ());
                    mk_typer C_ADDRESS O.(t_for_all a_var () (t_arrow (t_contract (t_variable a_var ())) (t_address ()) ()));
                    mk_typer C_CONTRACT O.(t_for_all a_var () (t_arrow (t_address ()) (t_contract (t_variable a_var ())) ()));
                    mk_typer C_CONTRACT_OPT O.(t_for_all a_var () (t_arrow (t_address ()) (t_option (t_contract (t_variable a_var ()))) ()));
                    mk_typer C_CONTRACT_WITH_ERROR O.(t_for_all a_var () (t_arrow (t_address ()) (t_arrow (t_string ()) (t_option (t_contract (t_variable a_var ()))) ()) ()));
                    mk_typer C_CONTRACT_ENTRYPOINT_OPT O.(t_for_all a_var () (t_arrow (t_string ()) (t_arrow (t_address ()) (t_option (t_contract (t_variable a_var ()))) ()) ()));
                    mk_typer C_CONTRACT_ENTRYPOINT O.(t_for_all a_var () (t_arrow (t_string ()) (t_arrow (t_address ()) (t_contract (t_variable a_var ())) ()) ()));
                    mk_typer C_IMPLICIT_ACCOUNT O.(t_arrow (t_key_hash ()) (t_contract (t_unit ())) ());
                    mk_typer C_SET_DELEGATE O.(t_arrow (t_option (t_key_hash ())) (t_operation ()) ());
                    mk_typer C_SELF O.(t_for_all a_var () (t_arrow (t_string ()) (t_contract (t_variable a_var ())) ()));
                    mk_typer C_SELF_ADDRESS O.(t_address ());
                    mk_typer C_TOTAL_VOTING_POWER O.(t_nat ());
                    mk_typer C_VOTING_POWER O.(t_arrow (t_key_hash ()) (t_nat ()) ());
                    mk_typer C_CALL O.(t_for_all a_var () (t_arrow (t_variable a_var ()) (t_arrow (t_mutez ()) (t_arrow (t_contract (t_variable a_var ())) (t_operation ()) ()) ()) ()));
                    mk_typer C_CREATE_CONTRACT O.(t_for_all a_var () (t_for_all b_var () (t_arrow (t_arrow (t_pair (t_variable a_var ()) (t_variable b_var ())) (t_pair (t_list (t_operation ())) (t_variable b_var ())) ()) (t_arrow (t_option (t_key_hash ())) (t_arrow (t_mutez ()) (t_arrow (t_variable b_var ()) (t_pair (t_operation ()) (t_address ())) ()) ()) ()) ())));
                    mk_typer C_NOW O.(t_timestamp ());
                    mk_typer C_CHAIN_ID O.(t_chain_id ());
                    (C_INT, any_of [
                                typer_of_ligo_type O.(t_arrow (t_nat ()) (t_int ()) ());
                                typer_of_ligo_type O.(t_arrow (t_bls12_381_fr ()) (t_int ()) ());
                    ]);
                    mk_typer C_UNIT O.(t_unit ());
                    mk_typer C_NEVER O.(t_for_all a_var () (t_arrow (t_never ()) (t_variable a_var ()) ()));
                    mk_typer C_TRUE O.(t_bool ());
                    mk_typer C_FALSE O.(t_bool ());
                    mk_typer C_IS_NAT O.(t_arrow (t_int ()) (t_option (t_nat ())) ());
                    mk_typer C_PAIRING_CHECK O.(t_arrow (t_list (t_pair (t_bls12_381_g1 ()) (t_bls12_381_g2 ()))) (t_bool ()) ());
                    mk_typer C_OPEN_CHEST O.(t_arrow (t_chest_key ()) (t_arrow (t_chest ()) (t_arrow (t_nat ()) (t_chest_opening_result ()) ()) ()) ());
                    mk_typer C_VIEW O.(t_for_all a_var () (t_for_all b_var () (t_arrow (t_string ()) (t_arrow (t_variable a_var ()) (t_arrow (t_address ()) (t_option (t_variable b_var ())) ()) ()) ())));
                    (* TICKET *)
                    mk_typer C_TICKET O.(t_for_all a_var () (t_arrow (t_variable a_var ()) (t_arrow (t_nat ()) (t_ticket (t_variable a_var ())) ()) ()));
                    mk_typer C_READ_TICKET O.(t_for_all a_var () (t_arrow (t_ticket (t_variable a_var ())) (t_pair (t_pair (t_address ()) (t_pair (t_variable a_var ()) (t_nat ()))) (t_ticket (t_variable a_var ()))) ()));
                    mk_typer C_SPLIT_TICKET O.(t_for_all a_var () (t_arrow (t_ticket (t_variable a_var ())) (t_arrow (t_pair (t_nat ()) (t_nat ())) (t_option (t_pair (t_ticket (t_variable a_var ())) (t_ticket (t_variable a_var ())))) ()) ()));
                    mk_typer C_JOIN_TICKET O.(t_for_all a_var () (t_arrow (t_pair (t_ticket (t_variable a_var ())) (t_ticket (t_variable a_var ()))) (t_option (t_ticket (t_variable a_var ()))) ()));
                    (* MATH *)
                    (C_POLYMORPHIC_ADD, any_of [
                                            typer_of_ligo_type O.(t_arrow (t_string ()) (t_arrow (t_string ()) (t_string ()) ()) ());
                                            typer_of_ligo_type O.(t_arrow (t_bls12_381_g1 ()) (t_arrow (t_bls12_381_g1 ()) (t_bls12_381_g1 ()) ()) ());
                                            typer_of_ligo_type O.(t_arrow (t_bls12_381_g2 ()) (t_arrow (t_bls12_381_g2 ()) (t_bls12_381_g2 ()) ()) ());
                                            typer_of_ligo_type O.(t_arrow (t_bls12_381_fr ()) (t_arrow (t_bls12_381_fr ()) (t_bls12_381_fr ()) ()) ());
                                            typer_of_ligo_type O.(t_arrow (t_nat ()) (t_arrow (t_nat ()) (t_nat ()) ()) ());
                                            typer_of_ligo_type O.(t_arrow (t_int ()) (t_arrow (t_int ()) (t_int ()) ()) ());
                                            typer_of_ligo_type O.(t_arrow (t_mutez ()) (t_arrow (t_mutez ()) (t_mutez ()) ()) ());
                                            typer_of_ligo_type O.(t_arrow (t_nat ()) (t_arrow (t_int ()) (t_int ()) ()) ());
                                            typer_of_ligo_type O.(t_arrow (t_int ()) (t_arrow (t_nat ()) (t_int ()) ()) ());
                                            typer_of_ligo_type O.(t_arrow (t_timestamp ()) (t_arrow (t_int ()) (t_timestamp ()) ()) ());
                                            typer_of_ligo_type O.(t_arrow (t_int ()) (t_arrow (t_timestamp ()) (t_timestamp ()) ()) ());
                    ]);
                    (C_ADD, any_of [
                                typer_of_ligo_type O.(t_arrow (t_bls12_381_g1 ()) (t_arrow (t_bls12_381_g1 ()) (t_bls12_381_g1 ()) ()) ());
                                typer_of_ligo_type O.(t_arrow (t_bls12_381_g2 ()) (t_arrow (t_bls12_381_g2 ()) (t_bls12_381_g2 ()) ()) ());
                                typer_of_ligo_type O.(t_arrow (t_bls12_381_fr ()) (t_arrow (t_bls12_381_fr ()) (t_bls12_381_fr ()) ()) ());
                                typer_of_ligo_type O.(t_arrow (t_nat ()) (t_arrow (t_nat ()) (t_nat ()) ()) ());
                                typer_of_ligo_type O.(t_arrow (t_int ()) (t_arrow (t_int ()) (t_int ()) ()) ());
                                typer_of_ligo_type O.(t_arrow (t_mutez ()) (t_arrow (t_mutez ()) (t_mutez ()) ()) ());
                                typer_of_ligo_type O.(t_arrow (t_nat ()) (t_arrow (t_int ()) (t_int ()) ()) ());
                                typer_of_ligo_type O.(t_arrow (t_int ()) (t_arrow (t_nat ()) (t_int ()) ()) ());
                                typer_of_ligo_type O.(t_arrow (t_timestamp ()) (t_arrow (t_int ()) (t_timestamp ()) ()) ());
                                typer_of_ligo_type O.(t_arrow (t_int ()) (t_arrow (t_timestamp ()) (t_timestamp ()) ()) ());
                    ]);
                    (C_MUL, any_of [
                                typer_of_ligo_type O.(t_arrow (t_bls12_381_g1 ()) (t_arrow (t_bls12_381_fr ()) (t_bls12_381_g1 ()) ()) ());
                                typer_of_ligo_type O.(t_arrow (t_bls12_381_g2 ()) (t_arrow (t_bls12_381_fr ()) (t_bls12_381_g2 ()) ()) ());
                                typer_of_ligo_type O.(t_arrow (t_bls12_381_fr ()) (t_arrow (t_bls12_381_fr ()) (t_bls12_381_fr ()) ()) ());
                                typer_of_ligo_type O.(t_arrow (t_nat ()) (t_arrow (t_bls12_381_fr ()) (t_bls12_381_fr ()) ()) ());
                                typer_of_ligo_type O.(t_arrow (t_int ()) (t_arrow (t_bls12_381_fr ()) (t_bls12_381_fr ()) ()) ());
                                typer_of_ligo_type O.(t_arrow (t_bls12_381_fr ()) (t_arrow (t_nat ()) (t_bls12_381_fr ()) ()) ());
                                typer_of_ligo_type O.(t_arrow (t_bls12_381_fr ()) (t_arrow (t_int ()) (t_bls12_381_fr ()) ()) ());
                                typer_of_ligo_type O.(t_arrow (t_nat ()) (t_arrow (t_nat ()) (t_nat ()) ()) ());
                                typer_of_ligo_type O.(t_arrow (t_int ()) (t_arrow (t_int ()) (t_int ()) ()) ());
                                typer_of_ligo_type O.(t_arrow (t_nat ()) (t_arrow (t_mutez ()) (t_mutez ()) ()) ());
                                typer_of_ligo_type O.(t_arrow (t_mutez ()) (t_arrow (t_nat ()) (t_mutez ()) ()) ());
                                typer_of_ligo_type O.(t_arrow (t_int ()) (t_arrow (t_nat ()) (t_int ()) ()) ());
                                typer_of_ligo_type O.(t_arrow (t_nat ()) (t_arrow (t_int ()) (t_int ()) ()) ());
                    ]);
                    (C_SUB, any_of [
                                typer_of_ligo_type O.(t_arrow (t_bls12_381_g1 ()) (t_arrow (t_bls12_381_fr ()) (t_bls12_381_g1 ()) ()) ());
                                typer_of_ligo_type O.(t_arrow (t_bls12_381_g2 ()) (t_arrow (t_bls12_381_fr ()) (t_bls12_381_g2 ()) ()) ());
                                typer_of_ligo_type O.(t_arrow (t_bls12_381_fr ()) (t_arrow (t_bls12_381_fr ()) (t_bls12_381_fr ()) ()) ());
                                typer_of_ligo_type O.(t_arrow (t_nat ()) (t_arrow (t_nat ()) (t_int ()) ()) ());
                                typer_of_ligo_type O.(t_arrow (t_int ()) (t_arrow (t_int ()) (t_int ()) ()) ());
                                typer_of_ligo_type O.(t_arrow (t_int ()) (t_arrow (t_nat ()) (t_int ()) ()) ());
                                typer_of_ligo_type O.(t_arrow (t_nat ()) (t_arrow (t_int ()) (t_int ()) ()) ());
                                typer_of_ligo_type O.(t_arrow (t_timestamp ()) (t_arrow (t_timestamp ()) (t_int ()) ()) ());
                                typer_of_ligo_type O.(t_arrow (t_timestamp ()) (t_arrow (t_int ()) (t_timestamp ()) ()) ());
                                typer_of_ligo_type O.(t_arrow (t_mutez ()) (t_arrow (t_mutez ()) (t_mutez ()) ()) ());
                    ]);
                    (C_EDIV, any_of [
                                 typer_of_ligo_type O.(t_arrow (t_nat ()) (t_arrow (t_nat ()) (t_option (t_pair (t_nat ()) (t_nat ()))) ()) ());
                                 typer_of_ligo_type O.(t_arrow (t_int ()) (t_arrow (t_int ()) (t_option (t_pair (t_int ()) (t_nat ()))) ()) ());
                                 typer_of_ligo_type O.(t_arrow (t_nat ()) (t_arrow (t_int ()) (t_option (t_pair (t_int ()) (t_nat ()))) ()) ());
                                 typer_of_ligo_type O.(t_arrow (t_int ()) (t_arrow (t_nat ()) (t_option (t_pair (t_int ()) (t_nat ()))) ()) ());
                                 typer_of_ligo_type O.(t_arrow (t_mutez ()) (t_arrow (t_mutez ()) (t_option (t_pair (t_nat ()) (t_mutez ()))) ()) ());
                                 typer_of_ligo_type O.(t_arrow (t_mutez ()) (t_arrow (t_nat ()) (t_option (t_pair (t_mutez ()) (t_mutez ()))) ()) ());
                    ]);
                    (C_DIV, any_of [
                                typer_of_ligo_type O.(t_arrow (t_nat ()) (t_arrow (t_nat ()) (t_nat ()) ()) ());
                                typer_of_ligo_type O.(t_arrow (t_int ()) (t_arrow (t_int ()) (t_int ()) ()) ());
                                typer_of_ligo_type O.(t_arrow (t_nat ()) (t_arrow (t_int ()) (t_int ()) ()) ());
                                typer_of_ligo_type O.(t_arrow (t_int ()) (t_arrow (t_nat ()) (t_int ()) ()) ());
                                typer_of_ligo_type O.(t_arrow (t_mutez ()) (t_arrow (t_nat ()) (t_mutez ()) ()) ());
                                typer_of_ligo_type O.(t_arrow (t_mutez ()) (t_arrow (t_mutez ()) (t_nat ()) ()) ());
                    ]);
                    (C_MOD, any_of [
                                typer_of_ligo_type O.(t_arrow (t_nat ()) (t_arrow (t_nat ()) (t_nat ()) ()) ());
                                typer_of_ligo_type O.(t_arrow (t_nat ()) (t_arrow (t_int ()) (t_nat ()) ()) ());
                                typer_of_ligo_type O.(t_arrow (t_int ()) (t_arrow (t_nat ()) (t_nat ()) ()) ());
                                typer_of_ligo_type O.(t_arrow (t_int ()) (t_arrow (t_int ()) (t_nat ()) ()) ());
                                typer_of_ligo_type O.(t_arrow (t_mutez ()) (t_arrow (t_nat ()) (t_mutez ()) ()) ());
                                typer_of_ligo_type O.(t_arrow (t_mutez ()) (t_arrow (t_mutez ()) (t_mutez ()) ()) ());
                    ]);
                    mk_typer C_ABS O.(t_arrow (t_int ()) (t_nat ()) ());
                    (C_NEG, any_of [
                                typer_of_ligo_type O.(t_arrow (t_int ()) (t_int ()) ());
                                typer_of_ligo_type O.(t_arrow (t_nat ()) (t_int ()) ());
                                typer_of_ligo_type O.(t_arrow (t_bls12_381_g1 ()) (t_bls12_381_g1 ()) ());
                                typer_of_ligo_type O.(t_arrow (t_bls12_381_g2 ()) (t_bls12_381_g2 ()) ());
                                typer_of_ligo_type O.(t_arrow (t_bls12_381_fr ()) (t_bls12_381_fr ()) ());
                    ]);
                    (* LOGIC *)
                    (C_NOT, any_of [
                                typer_of_ligo_type O.(t_arrow (t_bool ()) (t_bool ()) ());
                                typer_of_ligo_type O.(t_arrow (t_int ()) (t_int ()) ());
                                typer_of_ligo_type O.(t_arrow (t_nat ()) (t_int ()) ());
                    ]);
                    (C_OR, any_of [
                               typer_of_ligo_type O.(t_arrow (t_bool ()) (t_arrow (t_bool ()) (t_bool ()) ()) ());
                               typer_of_ligo_type O.(t_arrow (t_nat ()) (t_arrow (t_nat ()) (t_nat ()) ()) ());
                    ]);
                    (C_AND, any_of [
                                typer_of_ligo_type O.(t_arrow (t_bool ()) (t_arrow (t_bool ()) (t_bool ()) ()) ());
                                typer_of_ligo_type O.(t_arrow (t_nat ()) (t_arrow (t_nat ()) (t_nat ()) ()) ());
                                typer_of_ligo_type O.(t_arrow (t_int ()) (t_arrow (t_nat ()) (t_nat ()) ()) ());
                    ]);
                    (C_XOR, any_of [
                                typer_of_ligo_type O.(t_arrow (t_bool ()) (t_arrow (t_bool ()) (t_bool ()) ()) ());
                                typer_of_ligo_type O.(t_arrow (t_nat ()) (t_arrow (t_nat ()) (t_nat ()) ()) ());
                    ]);
                    mk_typer C_LSL O.(t_arrow (t_nat ()) (t_arrow (t_nat ()) (t_nat ()) ()) ());
                    mk_typer C_LSR O.(t_arrow (t_nat ()) (t_arrow (t_nat ()) (t_nat ()) ()) ());
                    (* COMPARATOR *)
                    (C_EQ, typer_of_comparator (comparator ~cmp:"EQ"));
                    (C_NEQ, typer_of_comparator (comparator ~cmp:"NEQ"));
                    (C_LT, typer_of_comparator (comparator ~cmp:"LT"));
                    (C_GT, typer_of_comparator (comparator ~cmp:"GT"));
                    (C_LE, typer_of_comparator (comparator ~cmp:"LE"));
                    (C_GE, typer_of_comparator (comparator ~cmp:"GE"));
                    (* TEST *)
                    mk_typer C_TEST_ORIGINATE O.(t_for_all a_var () (t_for_all b_var () (t_arrow (t_arrow (t_pair (t_variable a_var ()) (t_variable b_var ())) (t_pair (t_list (t_operation ())) (t_variable b_var ())) ()) (t_arrow (t_variable b_var ()) (t_arrow (t_mutez ()) (t_triplet (t_typed_address (t_variable a_var ()) (t_variable b_var ())) (t_michelson_code ()) (t_int ())) ()) ()) ())));
                    mk_typer C_TEST_BOOTSTRAP_CONTRACT O.(t_for_all a_var () (t_for_all b_var () (t_arrow (t_arrow (t_pair (t_variable a_var ()) (t_variable b_var ())) (t_pair (t_list (t_operation ())) (t_variable b_var ())) ()) (t_arrow (t_variable b_var ()) (t_arrow (t_mutez ()) (t_unit ()) ()) ()) ())));
                    mk_typer C_TEST_LAST_ORIGINATIONS O.(t_arrow (t_unit ()) (t_map (t_address ()) (t_list (t_address ()))) ());
                    mk_typer C_TEST_NTH_BOOTSTRAP_TYPED_ADDRESS O.(t_for_all a_var () (t_for_all b_var () (t_arrow (t_nat ()) (t_typed_address (t_variable a_var ()) (t_variable b_var ())) ())));
                    mk_typer C_TEST_SET_NOW O.(t_arrow (t_timestamp ()) (t_unit ()) ());
                    mk_typer C_TEST_SET_SOURCE O.(t_arrow (t_address ()) (t_unit ()) ());
                    mk_typer C_TEST_SET_BAKER O.(t_arrow (t_address ()) (t_unit ()) ());
                    mk_typer C_TEST_NTH_BOOTSTRAP_CONTRACT O.(t_arrow (t_nat ()) (t_address ()) ());
                    mk_typer C_TEST_GET_STORAGE O.(t_for_all a_var () (t_for_all b_var () (t_arrow (t_typed_address (t_variable a_var ()) (t_variable b_var ())) (t_variable b_var ()) ())));
                    mk_typer C_TEST_GET_STORAGE_OF_ADDRESS O.(t_arrow (t_address ()) (t_michelson_code ()) ());
                    mk_typer C_TEST_GET_BALANCE O.(t_arrow (t_address ()) (t_mutez ()) ());
                    mk_typer C_TEST_MICHELSON_EQUAL O.(t_arrow (t_michelson_code ()) (t_arrow (t_michelson_code ()) (t_bool ()) ()) ());
                    mk_typer C_TEST_GET_NTH_BS O.(t_arrow (t_int ()) (t_address ()) ());
                    mk_typer C_TEST_LOG O.(t_for_all a_var () (t_arrow (t_variable a_var ()) (t_unit ()) ()));
                    mk_typer C_TEST_STATE_RESET O.(t_arrow (t_nat ()) (t_arrow (t_list (t_mutez ())) (t_unit ()) ()) ());
                    mk_typer C_TEST_GET_VOTING_POWER O.(t_arrow (t_key_hash ()) (t_nat ()) ());
                    mk_typer C_TEST_GET_TOTAL_VOTING_POWER O.(t_nat ());
                    mk_typer C_TEST_CAST_ADDRESS O.(t_for_all a_var () (t_for_all b_var () (t_arrow (t_address ()) (t_typed_address (t_variable a_var ()) (t_variable b_var ())) ())));
                    mk_typer C_TEST_RANDOM O.(t_for_all a_var () (t_arrow (t_unit ()) (t_option (t_variable a_var ())) ()));
                    mk_typer C_TEST_MUTATE_VALUE O.(t_for_all a_var () (t_arrow (t_nat ()) (t_arrow (t_variable a_var ()) (t_option (t_pair (t_variable a_var ()) (t_mutation ()))) ()) ()));
                    mk_typer C_TEST_MUTATION_TEST O.(t_for_all a_var () (t_for_all b_var () (t_arrow (t_variable a_var ()) (t_arrow (t_arrow (t_variable a_var ()) (t_variable b_var ()) ()) (t_option (t_pair (t_variable b_var ()) (t_mutation ()))) ()) ())));
                    mk_typer C_TEST_MUTATION_TEST_ALL O.(t_for_all a_var () (t_for_all b_var () (t_arrow (t_variable a_var ()) (t_arrow (t_arrow (t_variable a_var ()) (t_variable b_var ()) ()) (t_option (t_pair (t_variable b_var ()) (t_mutation ()))) ()) ())));
                    mk_typer C_TEST_SAVE_MUTATION O.(t_arrow (t_string ()) (t_arrow (t_mutation ()) (t_option (t_string ())) ()) ());
                    mk_typer C_TEST_ADD_ACCOUNT O.(t_arrow (t_string ())(t_arrow (t_key ()) (t_unit ()) ()) ());
                    mk_typer C_TEST_NEW_ACCOUNT O.(t_arrow (t_unit ()) (t_pair (t_string ()) (t_key ())) ());
                    mk_typer C_TEST_RUN O.(t_for_all a_var () (t_for_all b_var () (t_arrow (t_arrow (t_variable a_var ()) (t_variable b_var ()) ()) (t_arrow (t_variable a_var ()) (t_michelson_code ()) ()) ())));
                    mk_typer C_TEST_EVAL O.(t_for_all a_var () (t_arrow (t_variable a_var ()) (t_michelson_code ()) ()));
                    mk_typer C_TEST_COMPILE_META_VALUE O.(t_for_all a_var () (t_arrow (t_variable a_var ()) (t_michelson_code ()) ()));
                    mk_typer C_TEST_DECOMPILE O.(t_for_all a_var () (t_arrow (t_michelson_code ()) (t_variable a_var ()) ()));
                    (* mk_typer C_TEST_TO_CONTRACT O.(t_for_all a_var () (t_for_all b_var () (t_arrow (t_typed_address (t_variable a_var ()) (t_variable b_var ())) _ ()))); *)
                    mk_typer C_TEST_TO_TYPED_ADDRESS O.(t_for_all a_var () (t_for_all b_var () (t_arrow (t_contract (t_variable a_var ())) (t_typed_address (t_variable a_var ()) (t_variable b_var ())) ())));
                    mk_typer C_TEST_EXTERNAL_CALL_TO_CONTRACT O.(t_for_all a_var () (t_arrow (t_contract (t_variable a_var ())) (t_arrow (t_variable a_var ()) (t_arrow (t_mutez ()) (t_test_exec_result ()) ()) ()) ()));
                    mk_typer C_TEST_EXTERNAL_CALL_TO_CONTRACT_EXN O.(t_for_all a_var () (t_arrow (t_contract (t_variable a_var ())) (t_arrow (t_variable a_var ()) (t_arrow (t_mutez ()) (t_nat ()) ()) ()) ()));
                    mk_typer C_TEST_EXTERNAL_CALL_TO_ADDRESS O.(t_arrow (t_address ()) (t_arrow (t_michelson_code ()) (t_arrow (t_mutez ()) (t_test_exec_result ()) ()) ()) ());
                    mk_typer C_TEST_EXTERNAL_CALL_TO_ADDRESS_EXN O.(t_arrow (t_address ()) (t_arrow (t_michelson_code ()) (t_arrow (t_mutez ()) (t_int ()) ()) ()) ());
                    mk_typer C_TEST_SET_BIG_MAP O.(t_for_all a_var () (t_for_all b_var () (t_arrow (t_int ()) (t_arrow (t_big_map (t_variable a_var ()) (t_variable b_var ())) (t_unit ()) ()) ())));
                    mk_typer C_TEST_BAKER_ACCOUNT O.(t_arrow (t_pair (t_string ()) (t_key ())) (t_arrow (t_option (t_mutez ())) (t_unit ()) ()) ());
                    mk_typer C_TEST_REGISTER_DELEGATE O.(t_arrow (t_key_hash ()) (t_unit ()) ());
                    mk_typer C_TEST_BAKE_UNTIL_N_CYCLE_END O.(t_arrow (t_nat ()) (t_unit ()) ());
                    mk_typer_only_hangzhou C_TEST_CREATE_CHEST O.(t_arrow (t_bytes ()) (t_arrow (t_nat ()) (t_pair (t_chest ()) (t_chest_key ())) ()) ());
                    mk_typer_only_hangzhou C_TEST_CREATE_CHEST_KEY O.(t_arrow (t_chest ()) (t_arrow (t_nat ()) (t_chest_key ()) ()) ());
                    mk_typer_only_hangzhou C_GLOBAL_CONSTANT O.(t_for_all a_var () (t_arrow (t_string ()) (t_variable a_var ()) ()));
                    (* CUSTOM *)
                    (* BLOCKCHAIN *)
                    (C_SAPLING_VERIFY_UPDATE, typer_of_old_typer (fun ~protocol_version ~raise ~test loc -> ignore protocol_version; ignore test; sapling_verify_update ~raise loc));
                    (C_SAPLING_EMPTY_STATE, typer_of_old_typer (fun ~protocol_version ~raise ~test loc -> ignore protocol_version; ignore test; sapling_empty_state ~raise loc));
                    (* TEST*)
                    (C_TEST_TO_CONTRACT, typer_of_old_typer (fun ~protocol_version ~raise ~test loc -> ignore protocol_version; ignore test; test_to_contract ~raise loc));
                    (C_TEST_TO_ENTRYPOINT, typer_of_old_typer (fun ~protocol_version ~raise ~test loc -> ignore protocol_version; ignore test; test_to_entrypoint ~raise loc));
                    (C_TEST_ORIGINATE_FROM_FILE, typer_of_old_typer (fun ~protocol_version ~raise ~test loc -> ignore test; test_originate_from_file ~protocol_version ~raise loc));
                  ]
end

let constant_typers ~raise ~test ~protocol_version loc c =
  match CTMap.find_opt c Constant_types.tbl with
  | Some typer ->
     fun lst tv_opt ->
     let error = ref [] in
     (match typer ~error ~raise ~test ~protocol_version ~loc lst tv_opt with
      | Some tv -> tv
      | None -> raise.raise (corner_case @@ Format.asprintf "Cannot type constant %a" PP.constant' c))
  | _ ->
     raise.raise (corner_case @@ Format.asprintf "Typer not implemented for constant %a" PP.constant' c)
