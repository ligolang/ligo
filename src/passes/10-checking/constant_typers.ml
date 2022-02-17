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

let polymorphic_add ~raise loc = typer_2 ~raise loc "POLYMORPHIC_ADD" @@ fun a b ->
  if eq_2 (a , b) (t_string ())
  then t_string () else
  if eq_2 (a , b) (t_bls12_381_g1 ())
  then (t_bls12_381_g1 ()) else
  if eq_2 (a , b) (t_bls12_381_g2 ())
  then (t_bls12_381_g2 ()) else
  if eq_2 (a , b) (t_bls12_381_fr ())
  then (t_bls12_381_fr ()) else
  if eq_2 (a , b) (t_nat ())
  then t_nat () else
  if eq_2 (a , b) (t_int ())
  then t_int () else
  if eq_2 (a , b) (t_mutez ())
  then t_mutez () else
  if (eq_1 a (t_nat ()) && eq_1 b (t_int ())) || (eq_1 b (t_nat ()) && eq_1 a (t_int ()))
  then t_int () else
  if (eq_1 a (t_timestamp ()) && eq_1 b (t_int ())) || (eq_1 b (t_timestamp ()) && eq_1 a (t_int ()))
  then t_timestamp () else
    raise.raise @@ typeclass_error loc
              [ 
                [t_string();t_string()] ;
                [t_bls12_381_g1();t_bls12_381_g1()] ;
                [t_bls12_381_g2();t_bls12_381_g2()] ;
                [t_bls12_381_fr();t_bls12_381_fr()] ;
                [t_nat();t_nat()] ;
                [t_int();t_int()] ;
                [t_mutez();t_mutez()] ;
                [t_nat();t_int()] ;
                [t_int();t_nat()] ;
                [t_timestamp();t_int()] ;
                [t_int();t_timestamp()] ;
              ]
              [a; b]

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
    comparator ~raise ~test loc s [a.associated_type;b.associated_type] None
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
    comparator ~raise ~test loc s [a.associated_type;b.associated_type] None
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
  comparator ~raise ~test loc s [a;b] None

and set_comparator ~raise ~test : Location.t -> string -> typer = fun loc s -> typer_2 ~raise loc s @@ fun a_set b_set ->
  let () =
    Assert.assert_true ~raise (uncomparable_types loc a_set b_set) @@ eq_1 a_set b_set
  in
  let a =
    trace_option ~raise (comparator_composed loc a_set) @@
    get_t_set a_set in
  let b = trace_option ~raise (expected_option loc b_set) @@ get_t_set b_set in
  comparator ~raise ~test loc s [a;b] None

and map_comparator ~raise ~test : Location.t -> string -> typer = fun loc s -> typer_2 ~raise loc s @@ fun a_map b_map ->
  let () =
    Assert.assert_true ~raise (uncomparable_types loc a_map b_map) @@ eq_1 a_map b_map
  in
  let (a_key, a_value) =
    trace_option ~raise (comparator_composed loc a_map) @@
    get_t_map a_map in
  let (b_key, b_value) = trace_option ~raise (expected_option loc b_map) @@ get_t_map b_map in
  let _ = comparator ~raise ~test loc s [a_key;b_key] None in
  let _ = comparator ~raise ~test loc s [a_value;b_value] None in
  t_bool ()

and big_map_comparator ~raise ~test : Location.t -> string -> typer = fun loc s -> typer_2 ~raise loc s @@ fun a_map b_map ->
  let () =
    Assert.assert_true ~raise (uncomparable_types loc a_map b_map) @@ eq_1 a_map b_map
  in
  let (a_key, a_value) =
    trace_option ~raise (comparator_composed loc a_map) @@
    get_t_big_map a_map in
  let (b_key, b_value) = trace_option ~raise (expected_option loc b_map) @@ get_t_big_map b_map in
  let _ = comparator ~raise ~test loc s [a_key;b_key] None in
  let _ = comparator ~raise ~test loc s [a_value;b_value] None in
  t_bool ()

and option_comparator ~raise ~test : Location.t -> string -> typer = fun loc s -> typer_2 ~raise loc s @@ fun a_opt b_opt ->
  let () =
    Assert.assert_true ~raise (uncomparable_types loc a_opt b_opt) @@ eq_1 a_opt b_opt
  in
  let a =
    trace_option ~raise (comparator_composed loc a_opt) @@
    get_t_option a_opt in
  let b = trace_option ~raise (expected_option loc b_opt) @@ get_t_option b_opt in
  comparator ~raise ~test loc s [a;b] None

and comparator ~raise ~test : Location.t -> string -> typer = fun loc s -> typer_2 ~raise loc s @@ fun a b ->
  if test
  then
    bind_exists ~raise @@ List.Ne.of_list [list_comparator ~test loc s [a;b] None;
                                           set_comparator ~test loc s [a;b] None;
                                           map_comparator ~test loc s [a;b] None;
                                           simple_comparator loc s [a;b] None;
                                           option_comparator ~test loc s [a;b] None;
                                           record_comparator ~test loc s [a;b] None;
                                           sum_comparator ~test loc s [a;b] None;
                                           big_map_comparator ~test loc s [a;b] None]
  else
    bind_exists ~raise @@ List.Ne.of_list [simple_comparator loc s [a;b] None;
                                           option_comparator ~test loc s [a;b] None;
                                           record_comparator ~test loc s [a;b] None;
                                           sum_comparator ~test loc s [a;b] None]

let sapling_verify_update ~raise loc = typer_2 ~raise loc "SAPLING_VERIFY_UPDATE" @@ fun tr state ->
  let singleton_tr = trace_option ~raise (expected_sapling_transaction loc tr) @@ get_t_sapling_transaction tr in
  let singleton_state = trace_option ~raise (expected_sapling_state loc state) @@ get_t_sapling_state state in
  let () = assert_eq_1 ~raise ~loc singleton_tr singleton_state in
  (t_option (t_pair (t_int ()) state))

let sapling_empty_state ~raise loc = typer_0 ~raise loc "SAPLING_EMPTY_STATE" @@ fun tv_opt ->
  trace_option ~raise (not_annotated loc) @@ tv_opt

let test_bootstrap_contract ~raise loc = typer_3 ~raise loc "TEST_BOOTSTRAP_CONTRACT" @@ fun balance main storage ->
  let { type1 = in_ty ; type2 = _ } = trace_option ~raise (expected_function loc main) @@ get_t_arrow main in
  let _,storage_ty = trace_option ~raise (expected_pair loc in_ty) @@ get_t_pair in_ty in
  let () = assert_eq_1 ~raise ~loc balance (t_mutez ()) in
  let () = assert_eq_1 ~raise ~loc storage storage_ty in
  (t_unit ())

let test_external_call_to_contract_exn ~raise loc = typer_3 ~raise loc "TEST_EXTERNAL_CALL_TO_CONTRACT_EXN" @@ fun addr p amt  ->
  let contract_ty = trace_option ~raise (expected_contract loc addr) @@ get_t_contract addr in
  let () = assert_eq_1 ~raise ~loc amt (t_mutez ()) in
  let () = assert_eq_1 ~raise ~loc p contract_ty in
  (t_nat ())

let test_external_call_to_contract ~raise loc = typer_3 ~raise loc "TEST_EXTERNAL_CALL_TO_CONTRACT" @@ fun addr p amt  ->
  let contract_ty = trace_option ~raise (expected_contract loc addr) @@ get_t_contract addr in
  let () = assert_eq_1 ~raise ~loc amt (t_mutez ()) in
  let () = assert_eq_1 ~raise ~loc p contract_ty in
  (t_test_exec_result ())

let test_external_call_to_address_exn ~raise loc = typer_3 ~raise loc "TEST_EXTERNAL_CALL_TO_ADDRESS_EXN" @@ fun addr p amt  ->
  let () = assert_eq_1 ~raise ~loc addr (t_address ()) in
  let () = assert_eq_1 ~raise ~loc amt (t_mutez ()) in
  let () = assert_eq_1 ~raise ~loc p (t_michelson_code ()) in
  (t_nat ())

let test_external_call_to_address ~raise loc = typer_3 ~raise loc "TEST_EXTERNAL_CALL_TO_ADDRESS" @@ fun addr p amt  ->
  let () = assert_eq_1 ~raise ~loc addr (t_address ()) in
  let () = assert_eq_1 ~raise ~loc amt (t_mutez ()) in
  let () = assert_eq_1 ~raise ~loc p (t_michelson_code ()) in
  (t_test_exec_result ())

let test_last_originations ~raise loc = typer_1 ~raise loc "TEST_LAST_ORIGINATIONS" @@ fun u ->
  let () = trace_option ~raise (expected_unit loc u) @@ assert_t_unit u in
  (t_map (t_address ()) (t_list (t_address ())))

let test_mutate_value ~raise loc = typer_2 ~raise loc "TEST_MUTATE_VALUE" @@ fun n expr ->
  let () = assert_eq_1 ~raise ~loc n (t_nat ()) in
  (t_option (t_pair expr (t_mutation ())))

let test_mutation_test ~raise loc = typer_2 ~raise loc "TEST_MUTATION_TEST" @@ fun expr tester ->
  let { type1 = arg ; type2 = res } = trace_option ~raise (expected_function loc tester) @@ get_t_arrow tester in
  let () = assert_eq_1 ~raise ~loc arg expr in
  (t_option (t_pair res (t_mutation ())))

let test_mutation_test_all ~raise loc = typer_2 ~raise loc "TEST_MUTATION_TEST_ALL" @@ fun expr tester ->
  let { type1 = arg ; type2 = res } = trace_option ~raise (expected_function loc tester) @@ get_t_arrow tester in
  let () = assert_eq_1 ~raise ~loc arg expr in
  (t_list (t_pair res (t_mutation ())))

let test_save_mutation ~raise loc = typer_2 ~raise loc "TEST_SAVE_MUTATION" @@ fun dir mutation ->
  let () = assert_eq_1 ~raise ~loc mutation (t_mutation ()) in
  let () = assert_eq_1 ~raise ~loc dir (t_string ()) in
  (t_option (t_string ()))

let test_to_contract ~raise loc = typer_1 ~raise loc "TEST_TO_CONTRACT" @@ fun t ->
  let param_ty, _ = trace_option ~raise (expected_typed_address loc t) @@
                       get_t_typed_address t in
  let param_ty = Option.value (Ast_typed.Helpers.get_entrypoint "default" param_ty) ~default:param_ty in
  (t_contract param_ty)

let test_nth_bootstrap_typed_address ~raise loc = typer_1_opt ~raise loc "TEST_NTH_BOOTSTRAP_TYPED_ADDRESS" @@ fun nat tv_opt ->
  let () = trace_option ~raise (expected_nat loc nat) @@ get_t_nat nat in
  let tv = trace_option ~raise (not_annotated loc) tv_opt in
  let tv_parameter, tv_storage = trace_option ~raise (expected_option loc tv) @@ get_t_typed_address tv in
  (t_typed_address tv_parameter tv_storage)

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

let test_create_chest ~raise loc = typer_2 ~raise loc "TEST_CREATE_CHEST" @@ fun payload time ->
  let () = trace_option ~raise (expected_bytes loc payload) @@ get_t_bytes payload in
  let () = trace_option ~raise (expected_nat loc time) @@ get_t_nat time in
  t_pair (t_chest ()) (t_chest_key ())

let test_create_chest_key ~raise loc = typer_2 ~raise loc "TEST_CREATE_CHEST_KEY" @@ fun chest time ->
  let () = assert_eq_1 ~raise ~loc (t_chest ()) chest in
  let () = trace_option ~raise (expected_nat loc time) @@ get_t_nat time in
  (t_chest_key ())

let test_global_constant ~raise loc = typer_1_opt ~raise loc "TEST_GLOBAL_CONSTANT" @@ fun hash_str tv_opt ->
  let () = trace_option ~raise (expected_string loc hash_str) @@ get_t_string hash_str in
  let ret_t = trace_option ~raise (not_annotated loc) @@ tv_opt in
  ret_t

type typer = error:[`TC of O.type_expression list] list ref -> raise:Errors.typer_error raise -> test:bool -> protocol_version:Ligo_proto.t -> loc:Location.t -> O.type_expression list -> O.type_expression option -> O.type_expression option

let typer_of_ligo_type ?(add_tc = true) ?(fail = true) lamb_type : typer = fun ~error ~raise ~test ~protocol_version ~loc lst tv_opt ->
  ignore test; ignore protocol_version;
  let _, lamb_type = O.Helpers.destruct_for_alls lamb_type in
  Simple_utils.Trace.try_with (fun ~raise ->
      let table =
        let table, lamb_type = List.fold_left lst ~init:(H.TMap.empty, lamb_type)
                                 ~f:(fun ((table, lamb_type) : _ H.TMap.t * O.type_expression) matched ->
                                   match lamb_type.type_content with
                                   | T_arrow { type1 ; type2 } ->
                                      H.infer_type_application ~raise ~loc table type1 matched, type2
                                   | (T_record _ | T_sum _ | T_constant _ | T_module_accessor _ |
                                      T_singleton _ | T_abstraction _ | T_for_all _ | T_variable _) ->
                                      table, lamb_type) in
        match tv_opt with
        | Some t ->
           Simple_utils.Trace.try_with (fun ~raise ->
               H.infer_type_application ~raise ~loc ~default_error:(fun loc t t' -> `Foo (loc, t', t)) table lamb_type t)
             (function `Foo (loc, t', t) -> raise.raise (`Foo (loc, t', t))
                     | `Typer_assert_equal e -> raise.raise (`Typer_assert_equal e)
                     | `Typer_not_matching e -> raise.raise (`Typer_not_matching e))
        | None -> table in
      let lamb_type = H.TMap.fold (fun tv t r -> Ast_typed.Helpers.subst_type tv t r) table lamb_type in
      let _, tv = Ast_typed.Helpers.destruct_arrows_n lamb_type (List.length lst) in
      Some tv)
    (function
     | `Foo (loc, t', t) ->
        if fail then
          raise.raise (assert_equal loc t' t)
        else
          None
     | _ ->
        let arrs, _ = O.Helpers.destruct_arrows lamb_type in
        let () = if add_tc then
                   error := `TC arrs :: ! error
                 else
                   () in
        None)

let typer_of_old_typer (typer : O.type_expression list -> O.type_expression option -> O.type_expression) : typer =
  fun ~error ~raise ~test ~protocol_version ~loc lst tv_opt ->
  ignore error; ignore raise; ignore loc; ignore test; ignore protocol_version;
  Some (typer lst tv_opt)

let rec typer_of_typers : typer list -> typer = fun typers ->
  fun ~error ~raise ~test ~protocol_version ~loc lst tv_opt ->
  match typers with
  | [] -> (
     match ! error with
      | [] ->
         raise.raise @@ (corner_case "putt")
      | xs ->
         let tc = List.filter_map ~f:(function `TC v -> Some v) xs in
         raise.raise @@ typeclass_error loc (List.rev (List.map ~f:List.rev tc)) lst
  )
  | typer :: typers ->
     match typer ~error ~raise ~test ~protocol_version ~loc lst tv_opt with
     | Some tv -> Some tv
     | None -> typer_of_typers typers ~error ~raise ~test ~protocol_version ~loc lst tv_opt

module Constant_types = struct
  module CTMap = Simple_utils.Map.Make(struct type t = O.constant' let compare x y = O.Compare.constant' x y end)

  type t = typer CTMap.t
  let names : (string * O.type_expression) list ref = ref []

  let a_var = O.Var.of_input_var "a"
  let b_var = O.Var.of_input_var "b"
  let c_var = O.Var.of_input_var "c"

  let of_ligo_type ?name t =
    let () = match name with
      | None -> ()
      | Some name -> names := (name, t) :: ! names in
    typer_of_typers [typer_of_ligo_type t]

  let typer_of_ligo_type_no_tc t =
    typer_of_ligo_type ~add_tc:false ~fail:false t

  let typer_of_ligo_type ?name t =
    let () = match name with
      | None -> ()
      | Some name -> names := (name, t) :: ! names in
    typer_of_ligo_type t

  let tbl : t = CTMap.of_list [
                    (* LOOPS *)
                    (C_FOLD_WHILE, of_ligo_type O.(t_for_all a_var () (t_arrow (t_arrow (t_variable a_var ()) (t_pair (t_bool ()) (t_variable a_var ())) ()) (t_arrow (t_variable a_var ()) (t_variable a_var ()) ()) ())));
                    (C_FOLD_CONTINUE, of_ligo_type O.(t_for_all a_var () (t_arrow (t_variable a_var ()) (t_pair (t_bool ()) (t_variable a_var ())) ())));
                    (C_FOLD_STOP, of_ligo_type O.(t_for_all a_var () (t_arrow (t_variable a_var ()) (t_pair (t_bool ()) (t_variable a_var ())) ())));
                    (C_FOLD, typer_of_typers [
                                 typer_of_ligo_type O.(t_for_all a_var () (t_for_all b_var () (t_arrow (t_arrow (t_pair (t_variable a_var ()) (t_variable b_var ())) (t_variable a_var ()) ()) (t_arrow (t_list (t_variable b_var ())) (t_arrow (t_variable a_var ()) (t_variable a_var ()) ()) ()) ())));
                                 typer_of_ligo_type O.(t_for_all a_var () (t_for_all b_var () (t_arrow (t_arrow (t_pair (t_variable a_var ()) (t_variable b_var ())) (t_variable a_var ()) ()) (t_arrow (t_set (t_variable b_var ())) (t_arrow (t_variable a_var ()) (t_variable a_var ()) ()) ()) ())));
                    ]);
                    (* MAP *)
                    (C_MAP_EMPTY, of_ligo_type O.(t_for_all a_var () (t_for_all b_var () (t_map (t_variable a_var ()) (t_variable b_var ())))));
                    (C_BIG_MAP_EMPTY, of_ligo_type O.(t_for_all a_var () (t_for_all b_var () (t_big_map (t_variable a_var ()) (t_variable b_var ())))));
                    (C_MAP_ADD, typer_of_typers [typer_of_ligo_type O.(t_for_all a_var () (t_for_all b_var () (t_arrow (t_variable a_var ()) (t_arrow (t_variable b_var ()) (t_arrow (t_map (t_variable a_var ()) (t_variable b_var ())) (t_map (t_variable a_var ()) (t_variable b_var ())) ()) ()) ())));
                                 typer_of_ligo_type O.(t_for_all a_var () (t_for_all b_var () (t_arrow (t_variable a_var ()) (t_arrow (t_variable b_var ()) (t_arrow (t_big_map (t_variable a_var ()) (t_variable b_var ())) (t_big_map (t_variable a_var ()) (t_variable b_var ())) ()) ()) ())));
                                ]);
                    (C_MAP_REMOVE, typer_of_typers [typer_of_ligo_type O.(t_for_all a_var () (t_for_all b_var () (t_arrow (t_variable a_var ()) (t_arrow (t_map (t_variable a_var ()) (t_variable b_var ())) (t_map (t_variable a_var ()) (t_variable b_var ())) ()) ())));
                                    typer_of_ligo_type O.(t_for_all a_var () (t_for_all b_var () (t_arrow (t_variable a_var ()) (t_arrow (t_big_map (t_variable a_var ()) (t_variable b_var ())) (t_big_map (t_variable a_var ()) (t_variable b_var ())) ()) ())));
                                ]);
                    (C_MAP_UPDATE, typer_of_typers [typer_of_ligo_type O.(t_for_all a_var () (t_for_all b_var () (t_arrow (t_variable a_var ()) (t_arrow (t_option (t_variable b_var ())) (t_arrow (t_map (t_variable a_var ()) (t_variable b_var ())) (t_map (t_variable a_var ()) (t_variable b_var ())) ()) ()) ())));
                                    typer_of_ligo_type O.(t_for_all a_var () (t_for_all b_var () (t_arrow (t_variable a_var ()) (t_arrow (t_option (t_variable b_var ())) (t_arrow (t_big_map (t_variable a_var ()) (t_variable b_var ())) (t_big_map (t_variable a_var ()) (t_variable b_var ())) ()) ()) ())));
                                ]);
                    (C_MAP_GET_AND_UPDATE, of_ligo_type O.(t_for_all a_var () (t_for_all b_var () (t_arrow (t_variable a_var ()) (t_arrow (t_option (t_variable b_var ())) (t_arrow (t_map (t_variable a_var ()) (t_variable b_var ())) (t_pair (t_option (t_variable b_var ())) (t_map (t_variable a_var ()) (t_variable b_var ()))) ()) ()) ()))));
                    (C_BIG_MAP_GET_AND_UPDATE, of_ligo_type O.(t_for_all a_var () (t_for_all b_var () (t_arrow (t_variable a_var ()) (t_arrow (t_option (t_variable b_var ())) (t_arrow (t_big_map (t_variable a_var ()) (t_variable b_var ())) (t_pair (t_option (t_variable b_var ())) (t_big_map (t_variable a_var ()) (t_variable b_var ()))) ()) ()) ()))));
                    (C_MAP_FIND_OPT, typer_of_typers [typer_of_ligo_type O.(t_for_all a_var () (t_for_all b_var () (t_arrow (t_variable a_var ()) (t_arrow (t_map (t_variable a_var ()) (t_variable b_var ())) (t_option (t_variable b_var ())) ()) ())));
                                      typer_of_ligo_type O.(t_for_all a_var () (t_for_all b_var () (t_arrow (t_variable a_var ()) (t_arrow (t_big_map (t_variable a_var ()) (t_variable b_var ())) (t_option (t_variable b_var ())) ()) ())));
                                      ]);
                    (C_MAP_FIND, typer_of_typers [typer_of_ligo_type O.(t_for_all a_var () (t_for_all b_var () (t_arrow (t_variable a_var ()) (t_arrow (t_map (t_variable a_var ()) (t_variable b_var ())) (t_variable b_var ()) ()) ())));
                                  typer_of_ligo_type O.(t_for_all a_var () (t_for_all b_var () (t_arrow (t_variable a_var ()) (t_arrow (t_big_map (t_variable a_var ()) (t_variable b_var ())) (t_variable b_var ()) ()) ())));
                                 ]);
                    (C_MAP_MEM, typer_of_typers [typer_of_ligo_type O.(t_for_all a_var () (t_for_all b_var () (t_arrow (t_variable a_var ()) (t_arrow (t_map (t_variable a_var ()) (t_variable b_var ())) (t_bool ()) ()) ())));
                                 typer_of_ligo_type O.(t_for_all a_var () (t_for_all b_var () (t_arrow (t_variable a_var ()) (t_arrow (t_big_map (t_variable a_var ()) (t_variable b_var ())) (t_bool ()) ()) ())));
                                ]);
                    (C_MAP_MAP, of_ligo_type O.(t_for_all a_var () (t_for_all b_var () (t_for_all c_var () (t_arrow (t_arrow (t_pair (t_variable a_var ()) (t_variable b_var ())) (t_variable c_var ()) ()) (t_arrow (t_map (t_variable a_var ()) (t_variable b_var ())) (t_map (t_variable a_var ()) (t_variable c_var ())) ()) ())))));
                    (C_MAP_ITER, of_ligo_type O.(t_for_all a_var () (t_for_all b_var () (t_arrow (t_arrow (t_pair (t_variable a_var ()) (t_variable b_var ())) (t_unit ()) ()) (t_arrow (t_map (t_variable a_var ()) (t_variable b_var ())) (t_unit ()) ()) ()))));
                    (C_MAP_FOLD, of_ligo_type O.(t_for_all a_var () (t_for_all b_var () (t_for_all c_var () (t_arrow (t_arrow (t_pair (t_variable c_var ()) (t_pair (t_variable a_var ()) (t_variable b_var ()))) (t_variable c_var ()) ()) (t_arrow (t_map (t_variable a_var ()) (t_variable b_var ())) (t_arrow (t_variable c_var ()) (t_variable c_var ()) ()) ()) ())))));
                    (* LIST *)
                    (C_LIST_EMPTY, of_ligo_type O.(t_for_all a_var () (t_list (t_variable a_var ()))));
                    (C_CONS, of_ligo_type O.(t_for_all a_var () (t_arrow (t_variable a_var ()) (t_arrow (t_list (t_variable a_var ())) (t_list (t_variable a_var ())) ()) ())));
                    (C_LIST_MAP, of_ligo_type O.(t_for_all a_var () (t_for_all b_var () (t_arrow (t_arrow (t_variable a_var ()) (t_variable b_var ()) ()) (t_arrow (t_list (t_variable a_var ())) (t_list (t_variable b_var ())) ()) ()))));
                    (C_LIST_ITER, of_ligo_type O.(t_for_all a_var () (t_arrow (t_arrow (t_variable a_var ()) (t_unit ()) ()) (t_arrow (t_list (t_variable a_var ())) (t_unit ()) ()) ())));
                    (C_LIST_FOLD, of_ligo_type O.(t_for_all a_var () (t_for_all b_var () (t_arrow (t_arrow (t_pair (t_variable b_var ()) (t_variable a_var ())) (t_variable b_var ()) ()) (t_arrow (t_list (t_variable a_var ())) (t_arrow (t_variable b_var ()) (t_variable b_var ()) ()) ()) ()))));
                    (C_LIST_FOLD_LEFT, of_ligo_type O.(t_for_all a_var () (t_for_all b_var () (t_arrow (t_arrow (t_pair (t_variable b_var ()) (t_variable a_var ())) (t_variable b_var ()) ()) (t_arrow (t_variable b_var ()) (t_arrow (t_list (t_variable a_var ())) (t_variable b_var ()) ()) ()) ()))));
                    (C_LIST_FOLD_RIGHT, of_ligo_type O.(t_for_all a_var () (t_for_all b_var () (t_arrow (t_arrow (t_pair (t_variable a_var ()) (t_variable b_var ())) (t_variable b_var ()) ()) (t_arrow (t_list (t_variable a_var ())) (t_arrow (t_variable b_var ()) (t_variable b_var ()) ()) ()) ()))));
                    (C_LIST_HEAD_OPT, of_ligo_type O.(t_for_all a_var () (t_arrow (t_list (t_variable a_var ())) (t_option (t_variable a_var ())) ())));
                    (C_LIST_TAIL_OPT, of_ligo_type O.(t_for_all a_var () (t_arrow (t_list (t_variable a_var ())) (t_option (t_list (t_variable a_var ()))) ())));
                    (* SET *)
                    (C_SET_EMPTY, of_ligo_type O.(t_for_all a_var () (t_set (t_variable a_var ()))));
                    (C_SET_LITERAL, of_ligo_type O.(t_for_all a_var () (t_arrow (t_list (t_variable a_var ())) (t_set (t_variable a_var ())) ())));
                    (C_SET_MEM, of_ligo_type O.(t_for_all a_var () (t_arrow (t_variable a_var ()) (t_arrow (t_set (t_variable a_var ())) (t_bool ()) ()) ())));
                    (C_SET_ADD, of_ligo_type O.(t_for_all a_var () (t_arrow (t_variable a_var ()) (t_arrow (t_set (t_variable a_var ())) (t_set (t_variable a_var ())) ()) ())));
                    (C_SET_REMOVE, of_ligo_type O.(t_for_all a_var () (t_arrow (t_variable a_var ()) (t_arrow (t_set (t_variable a_var ())) (t_set (t_variable a_var ())) ()) ())));
                    (C_SET_UPDATE, of_ligo_type O.(t_for_all a_var () (t_arrow (t_variable a_var ()) (t_arrow (t_bool ()) (t_arrow (t_set (t_variable a_var ())) (t_set (t_variable a_var ())) ()) ()) ())));
                    (C_SET_ITER, of_ligo_type O.(t_for_all a_var () (t_arrow (t_arrow (t_variable a_var ()) (t_unit ()) ()) (t_arrow (t_set (t_variable a_var ())) (t_unit ()) ()) ())));
                    (C_SET_FOLD, of_ligo_type O.(t_for_all a_var () (t_for_all b_var () (t_arrow (t_arrow (t_pair (t_variable b_var ()) (t_variable a_var ())) (t_variable b_var ()) ()) (t_arrow (t_set (t_variable a_var ())) (t_arrow (t_variable b_var ()) (t_variable b_var ()) ()) ()) ()))));
                    (C_SET_FOLD_DESC, of_ligo_type O.(t_for_all a_var () (t_for_all b_var () (t_arrow (t_arrow (t_pair (t_variable a_var ()) (t_variable b_var ())) (t_variable b_var ()) ()) (t_arrow (t_set (t_variable a_var ())) (t_arrow (t_variable b_var ()) (t_variable b_var ()) ()) ()) ()))));
                    (* ADHOC POLY *)
                    (C_SIZE, typer_of_typers [typer_of_ligo_type O.(t_for_all a_var () (t_arrow (t_list (t_variable a_var ())) (t_nat ()) ()));
                              typer_of_ligo_type O.(t_arrow (t_bytes ()) (t_nat ()) ());
                              typer_of_ligo_type O.(t_arrow (t_string ()) (t_nat ()) ());
                              typer_of_ligo_type O.(t_for_all a_var () (t_arrow (t_set (t_variable a_var ())) (t_nat ()) ()));
                              typer_of_ligo_type O.(t_for_all a_var () (t_for_all b_var () (t_arrow (t_map (t_variable a_var ()) (t_variable b_var ())) (t_nat ()) ())))
                             ]);
                    (C_CONCAT, typer_of_typers [
                                   typer_of_ligo_type O.(t_arrow (t_string ()) (t_arrow (t_string ()) (t_string ()) ()) ());
                                   typer_of_ligo_type O.(t_arrow (t_bytes ()) (t_arrow (t_bytes ()) (t_bytes ()) ()) ());
                               ]);
                    (C_SLICE, typer_of_typers [
                                  typer_of_ligo_type O.(t_arrow (t_nat ()) (t_arrow (t_nat ()) (t_arrow (t_string ()) (t_string ()) ()) ()) ());
                                  typer_of_ligo_type O.(t_arrow (t_nat ()) (t_arrow (t_nat ()) (t_arrow (t_bytes ()) (t_bytes ()) ()) ()) ());
                              ]);
                    (C_BYTES_PACK, of_ligo_type ~name:"Bytes.pack" O.(t_for_all a_var () (t_arrow (t_variable a_var ()) (t_bytes ()) ())));
                    (C_BYTES_UNPACK, of_ligo_type ~name:"Bytes.unpack" O.(t_for_all a_var () (t_arrow (t_bytes ()) (t_option (t_variable a_var ())) ())));
                    (* CRYPTO *)
                    (C_SHA256, of_ligo_type O.(t_arrow (t_bytes ()) (t_bytes ()) ()));
                    (C_SHA512, of_ligo_type O.(t_arrow (t_bytes ()) (t_bytes ()) ()));
                    (C_SHA3, of_ligo_type O.(t_arrow (t_bytes ()) (t_bytes ()) ()));
                    (C_KECCAK, of_ligo_type O.(t_arrow (t_bytes ()) (t_bytes ()) ()));
                    (C_BLAKE2b, of_ligo_type O.(t_arrow (t_bytes ()) (t_bytes ()) ()));
                    (C_HASH_KEY, of_ligo_type O.(t_arrow (t_key ()) (t_key_hash ()) ()));
                    (C_CHECK_SIGNATURE, of_ligo_type O.(t_arrow (t_key ()) (t_arrow (t_signature ()) (t_arrow (t_bytes ()) (t_bool ()) ()) ()) ()));
                    (* OPTION *)
                    (C_NONE, of_ligo_type O.(t_for_all a_var () (t_option (t_variable a_var ()))));
                    (C_SOME, of_ligo_type O.(t_for_all a_var () (t_arrow (t_variable a_var ()) (t_option (t_variable a_var ())) ())));
                    (C_UNOPT, of_ligo_type O.(t_for_all a_var () (t_arrow (t_option (t_variable a_var ())) (t_variable a_var ()) ())));
                    (C_UNOPT_WITH_ERROR, of_ligo_type O.(t_for_all a_var () (t_arrow (t_option (t_variable a_var ())) (t_arrow (t_string ()) (t_variable a_var ()) ()) ())));
                    (* GLOBAL *)
                    (C_ASSERTION, of_ligo_type O.(t_arrow (t_bool ()) (t_unit ()) ()));
                    (C_ASSERTION_WITH_ERROR, of_ligo_type O.(t_arrow (t_bool ()) (t_arrow (t_string ()) (t_unit ()) ()) ()));
                    (C_ASSERT_SOME, of_ligo_type O.(t_for_all a_var () (t_arrow (t_option (t_variable a_var ())) (t_unit ()) ())));
                    (C_ASSERT_SOME_WITH_ERROR, of_ligo_type O.(t_for_all a_var () (t_arrow (t_option (t_variable a_var ())) (t_arrow (t_string ()) (t_unit ()) ()) ())));
                    (C_ASSERT_NONE, of_ligo_type O.(t_for_all a_var () (t_arrow (t_option (t_variable a_var ())) (t_unit ()) ())));
                    (C_ASSERT_NONE_WITH_ERROR, of_ligo_type O.(t_for_all a_var () (t_arrow (t_option (t_variable a_var ())) (t_arrow (t_string ()) (t_unit ()) ()) ())));
                    (C_FAILWITH, typer_of_typers [
                                  typer_of_ligo_type_no_tc @@ O.(t_arrow (t_string ()) (t_unit ()) ());
                                  typer_of_ligo_type_no_tc @@ O.(t_arrow (t_nat ()) (t_unit ()) ());
                                  typer_of_ligo_type_no_tc @@ O.(t_arrow (t_int ()) (t_unit ()) ());
                                  typer_of_ligo_type O.(t_for_all a_var () (t_arrow (t_string ()) (t_variable a_var ()) ()));
                                  typer_of_ligo_type O.(t_for_all a_var () (t_arrow (t_nat ()) (t_variable a_var ()) ()));
                                  typer_of_ligo_type O.(t_for_all a_var () (t_arrow (t_int ()) (t_variable a_var ()) ()));
                                 ]);
                    (C_AMOUNT, of_ligo_type O.(t_mutez ()));
                    (C_BALANCE, of_ligo_type O.(t_mutez ()));
                    (C_LEVEL, of_ligo_type O.(t_nat ()));
                    (C_SENDER, of_ligo_type O.(t_address ()));
                    (C_SOURCE, of_ligo_type O.(t_address ()));
                    (C_ADDRESS, of_ligo_type O.(t_for_all a_var () (t_arrow (t_contract (t_variable a_var ())) (t_address ()) ())));
                    (C_CONTRACT, of_ligo_type O.(t_for_all a_var () (t_arrow (t_address ()) (t_contract (t_variable a_var ())) ())));
                    (C_CONTRACT_OPT, of_ligo_type O.(t_for_all a_var () (t_arrow (t_address ()) (t_option (t_contract (t_variable a_var ()))) ())));
                    (C_CONTRACT_WITH_ERROR, of_ligo_type O.(t_for_all a_var () (t_arrow (t_address ()) (t_arrow (t_string ()) (t_option (t_contract (t_variable a_var ()))) ()) ())));
                    (C_CONTRACT_ENTRYPOINT_OPT, of_ligo_type O.(t_for_all a_var () (t_arrow (t_string ()) (t_arrow (t_address ()) (t_option (t_contract (t_variable a_var ()))) ()) ())));
                    (C_CONTRACT_ENTRYPOINT, of_ligo_type O.(t_for_all a_var () (t_arrow (t_string ()) (t_arrow (t_address ()) (t_contract (t_variable a_var ())) ()) ())));
                    (C_IMPLICIT_ACCOUNT, of_ligo_type O.(t_arrow (t_key_hash ()) (t_contract (t_unit ())) ()));
                    (C_SET_DELEGATE, of_ligo_type O.(t_arrow (t_option (t_key_hash ())) (t_operation ()) ()));
                    (C_SELF, of_ligo_type O.(t_for_all a_var () (t_arrow (t_string ()) (t_contract (t_variable a_var ())) ())));
                    (C_SELF_ADDRESS, of_ligo_type O.(t_address ()));
                    (C_TOTAL_VOTING_POWER, of_ligo_type O.(t_nat ()));
                    (C_VOTING_POWER, of_ligo_type O.(t_arrow (t_key_hash ()) (t_nat ()) ()));
                    (C_CALL, of_ligo_type O.(t_for_all a_var () (t_arrow (t_variable a_var ()) (t_arrow (t_mutez ()) (t_arrow (t_contract (t_variable a_var ())) (t_operation ()) ()) ()) ())));
                    (C_CREATE_CONTRACT, of_ligo_type O.(t_for_all a_var () (t_for_all b_var () (t_arrow (t_arrow (t_pair (t_variable a_var ()) (t_variable b_var ())) (t_pair (t_list (t_operation ())) (t_variable b_var ())) ()) (t_arrow (t_option (t_key_hash ())) (t_arrow (t_mutez ()) (t_arrow (t_variable b_var ()) (t_pair (t_operation ()) (t_address ())) ()) ()) ()) ()))));
                    (C_NOW, of_ligo_type O.(t_timestamp ()));
                    (C_CHAIN_ID, of_ligo_type O.(t_chain_id ()));
                    (C_INT, typer_of_typers [typer_of_ligo_type O.(t_arrow (t_nat ()) (t_int ()) ());
                             typer_of_ligo_type O.(t_arrow (t_bls12_381_fr ()) (t_int ()) ());
                            ]);
                    (C_UNIT, of_ligo_type O.(t_unit ()));
                    (C_NEVER, of_ligo_type O.(t_for_all a_var () (t_arrow (t_never ()) (t_variable a_var ()) ())));
                    (C_TRUE, of_ligo_type O.(t_bool ()));
                    (C_FALSE, of_ligo_type O.(t_bool ()));
                    (C_IS_NAT, of_ligo_type O.(t_arrow (t_int ()) (t_option (t_nat ())) ()));
                    (C_PAIRING_CHECK, of_ligo_type O.(t_arrow (t_list (t_pair (t_bls12_381_g1 ()) (t_bls12_381_g2 ()))) (t_bool ()) ()));
                    (C_OPEN_CHEST, of_ligo_type O.(t_arrow (t_chest_key ()) (t_arrow (t_chest ()) (t_arrow (t_nat ()) (t_chest_opening_result ()) ()) ()) ()));
                    (C_VIEW, of_ligo_type O.(t_for_all a_var () (t_for_all b_var () (t_arrow (t_string ()) (t_arrow (t_variable a_var ()) (t_arrow (t_address ()) (t_option (t_variable b_var ())) ()) ()) ()))));
                    (* TICKET *)
                    (C_TICKET, of_ligo_type O.(t_for_all a_var () (t_arrow (t_variable a_var ()) (t_arrow (t_nat ()) (t_ticket (t_variable a_var ())) ()) ())));
                    (C_READ_TICKET, of_ligo_type O.(t_for_all a_var () (t_arrow (t_ticket (t_variable a_var ())) (t_pair (t_pair (t_address ()) (t_pair (t_variable a_var ()) (t_nat ()))) (t_ticket (t_variable a_var ()))) ())));
                    (C_SPLIT_TICKET, of_ligo_type O.(t_for_all a_var () (t_arrow (t_ticket (t_variable a_var ())) (t_arrow (t_pair (t_nat ()) (t_nat ())) (t_option (t_pair (t_ticket (t_variable a_var ())) (t_ticket (t_variable a_var ())))) ()) ())));
                    (C_JOIN_TICKET, of_ligo_type O.(t_for_all a_var () (t_arrow (t_pair (t_ticket (t_variable a_var ())) (t_ticket (t_variable a_var ()))) (t_option (t_ticket (t_variable a_var ()))) ())));
                    (* MATH *)
                    (C_ADD, typer_of_typers [typer_of_ligo_type O.(t_arrow (t_bls12_381_g1 ()) (t_arrow (t_bls12_381_g1 ()) (t_bls12_381_g1 ()) ()) ());
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
                    (C_MUL, typer_of_typers [typer_of_ligo_type O.(t_arrow (t_bls12_381_g1 ()) (t_arrow (t_bls12_381_fr ()) (t_bls12_381_g1 ()) ()) ());
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
                    (C_SUB, typer_of_typers [typer_of_ligo_type O.(t_arrow (t_bls12_381_g1 ()) (t_arrow (t_bls12_381_fr ()) (t_bls12_381_g1 ()) ()) ());
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
                    (C_EDIV, typer_of_typers [typer_of_ligo_type O.(t_arrow (t_nat ()) (t_arrow (t_nat ()) (t_option (t_pair (t_nat ()) (t_nat ()))) ()) ());
                              typer_of_ligo_type O.(t_arrow (t_int ()) (t_arrow (t_int ()) (t_option (t_pair (t_int ()) (t_nat ()))) ()) ());
                              typer_of_ligo_type O.(t_arrow (t_nat ()) (t_arrow (t_int ()) (t_option (t_pair (t_int ()) (t_nat ()))) ()) ());
                              typer_of_ligo_type O.(t_arrow (t_int ()) (t_arrow (t_nat ()) (t_option (t_pair (t_int ()) (t_nat ()))) ()) ());
                              typer_of_ligo_type O.(t_arrow (t_mutez ()) (t_arrow (t_mutez ()) (t_option (t_pair (t_nat ()) (t_mutez ()))) ()) ());
                              typer_of_ligo_type O.(t_arrow (t_mutez ()) (t_arrow (t_nat ()) (t_option (t_pair (t_mutez ()) (t_mutez ()))) ()) ());
                            ]);
                    (C_DIV, typer_of_typers [typer_of_ligo_type O.(t_arrow (t_nat ()) (t_arrow (t_nat ()) (t_nat ()) ()) ());
                             typer_of_ligo_type O.(t_arrow (t_int ()) (t_arrow (t_int ()) (t_int ()) ()) ());
                             typer_of_ligo_type O.(t_arrow (t_nat ()) (t_arrow (t_int ()) (t_int ()) ()) ());
                             typer_of_ligo_type O.(t_arrow (t_int ()) (t_arrow (t_nat ()) (t_int ()) ()) ());
                             typer_of_ligo_type O.(t_arrow (t_mutez ()) (t_arrow (t_nat ()) (t_mutez ()) ()) ());
                             typer_of_ligo_type O.(t_arrow (t_mutez ()) (t_arrow (t_mutez ()) (t_nat ()) ()) ());
                            ]);
                    (C_MOD, typer_of_typers [typer_of_ligo_type O.(t_arrow (t_nat ()) (t_arrow (t_nat ()) (t_nat ()) ()) ());
                             typer_of_ligo_type O.(t_arrow (t_nat ()) (t_arrow (t_int ()) (t_nat ()) ()) ());
                             typer_of_ligo_type O.(t_arrow (t_int ()) (t_arrow (t_nat ()) (t_nat ()) ()) ());
                             typer_of_ligo_type O.(t_arrow (t_int ()) (t_arrow (t_int ()) (t_nat ()) ()) ());
                             typer_of_ligo_type O.(t_arrow (t_mutez ()) (t_arrow (t_nat ()) (t_mutez ()) ()) ());
                             typer_of_ligo_type O.(t_arrow (t_mutez ()) (t_arrow (t_mutez ()) (t_mutez ()) ()) ());
                            ]);
                    (C_ABS, of_ligo_type O.(t_arrow (t_int ()) (t_nat ()) ()));
                    (C_NEG, typer_of_typers [typer_of_ligo_type O.(t_arrow (t_int ()) (t_int ()) ());
                             typer_of_ligo_type O.(t_arrow (t_nat ()) (t_int ()) ());
                             typer_of_ligo_type O.(t_arrow (t_bls12_381_g1 ()) (t_bls12_381_g1 ()) ());
                             typer_of_ligo_type O.(t_arrow (t_bls12_381_g2 ()) (t_bls12_381_g2 ()) ());
                             typer_of_ligo_type O.(t_arrow (t_bls12_381_fr ()) (t_bls12_381_fr ()) ());
                             ]);
                    (* LOGIC *)
                    (C_NOT, typer_of_typers [typer_of_ligo_type O.(t_arrow (t_bool ()) (t_bool ()) ());
                             typer_of_ligo_type O.(t_arrow (t_int ()) (t_int ()) ());
                             typer_of_ligo_type O.(t_arrow (t_nat ()) (t_int ()) ());
                            ]);
                    (C_OR, typer_of_typers [typer_of_ligo_type O.(t_arrow (t_bool ()) (t_arrow (t_bool ()) (t_bool ()) ()) ());
                            typer_of_ligo_type O.(t_arrow (t_nat ()) (t_arrow (t_nat ()) (t_nat ()) ()) ());
                           ]);
                    (C_AND, typer_of_typers [typer_of_ligo_type O.(t_arrow (t_bool ()) (t_arrow (t_bool ()) (t_bool ()) ()) ());
                             typer_of_ligo_type O.(t_arrow (t_nat ()) (t_arrow (t_nat ()) (t_nat ()) ()) ());
                             typer_of_ligo_type O.(t_arrow (t_int ()) (t_arrow (t_nat ()) (t_nat ()) ()) ());
                            ]);
                    (C_XOR, typer_of_typers [typer_of_ligo_type O.(t_arrow (t_bool ()) (t_arrow (t_bool ()) (t_bool ()) ()) ());
                             typer_of_ligo_type O.(t_arrow (t_nat ()) (t_arrow (t_nat ()) (t_nat ()) ()) ());
                            ]);
                    (C_LSL, of_ligo_type O.(t_arrow (t_nat ()) (t_arrow (t_nat ()) (t_nat ()) ()) ()));
                    (C_LSR, of_ligo_type O.(t_arrow (t_nat ()) (t_arrow (t_nat ()) (t_nat ()) ()) ()));
                    (* COMPARATOR *)
                    (* (C_EQ, fun ~error ~raise ~test ~protocol_version ~loc ->
                     *        typer_of_old_typer (comparator ~raise ~test loc "EQ") ~error ~raise ~test ~protocol_version ~loc); *)
                    (* TEST *)
                    (C_TEST_ORIGINATE, of_ligo_type O.(t_for_all a_var () (t_for_all b_var () (t_arrow (t_arrow (t_pair (t_variable a_var ()) (t_variable b_var ())) (t_pair (t_list (t_operation ())) (t_variable b_var ())) ()) (t_arrow (t_variable b_var ()) (t_arrow (t_mutez ()) (t_triplet (t_typed_address (t_variable a_var ()) (t_variable b_var ())) (t_michelson_code ()) (t_int ())) ()) ()) ()))));
                    (C_TEST_SET_NOW, of_ligo_type O.(t_arrow (t_timestamp ()) (t_unit ()) ()));
                    (C_TEST_SET_SOURCE, of_ligo_type O.(t_arrow (t_address ()) (t_unit ()) ()));
                    (C_TEST_SET_BAKER, of_ligo_type O.(t_arrow (t_address ()) (t_unit ()) ()));
                    (C_TEST_NTH_BOOTSTRAP_CONTRACT, of_ligo_type O.(t_arrow (t_nat ()) (t_address ()) ()));
                    (C_TEST_GET_STORAGE, of_ligo_type O.(t_for_all a_var () (t_for_all b_var () (t_arrow (t_typed_address (t_variable a_var ()) (t_variable b_var ())) (t_variable b_var ()) ()))));
                    (C_TEST_GET_STORAGE_OF_ADDRESS, of_ligo_type O.(t_arrow (t_address ()) (t_michelson_code ()) ()));
                    (C_TEST_GET_BALANCE, of_ligo_type O.(t_arrow (t_address ()) (t_mutez ()) ()));
                    (C_TEST_MICHELSON_EQUAL, of_ligo_type O.(t_arrow (t_michelson_code ()) (t_arrow (t_michelson_code ()) (t_bool ()) ()) ()));
                    (C_TEST_GET_NTH_BS, of_ligo_type O.(t_arrow (t_int ()) (t_address ()) ()));
                    (C_TEST_LOG, of_ligo_type ~name:"Test.log" O.(t_for_all a_var () (t_arrow (t_variable a_var ()) (t_unit ()) ())));
                    (C_TEST_STATE_RESET, of_ligo_type O.(t_arrow (t_nat ()) (t_arrow (t_list (t_mutez ())) (t_unit ()) ()) ()));
                    (C_TEST_GET_VOTING_POWER, of_ligo_type O.(t_arrow (t_key_hash ()) (t_nat ()) ()));
                    (C_TEST_GET_TOTAL_VOTING_POWER, of_ligo_type O.(t_nat ()));
                    (C_TEST_CAST_ADDRESS, of_ligo_type O.(t_for_all a_var () (t_for_all b_var () (t_arrow (t_address ()) (t_typed_address (t_variable a_var ()) (t_variable b_var ())) ()))));
                    (C_TEST_RANDOM, of_ligo_type O.(t_for_all a_var () (t_arrow (t_unit ()) (t_option (t_variable a_var ())) ())));
                    (* (C_TEST_MUTATE_VALUE, of_ligo_type @@ O._) *)
                    (C_TEST_ADD_ACCOUNT, of_ligo_type O.(t_arrow (t_string ())(t_arrow (t_key ()) (t_unit ()) ()) ()));
                    (C_TEST_NEW_ACCOUNT, of_ligo_type O.(t_arrow (t_unit ()) (t_pair (t_string ()) (t_key ())) ()));
                    (C_TEST_RUN, of_ligo_type O.(t_for_all a_var () (t_for_all b_var () (t_arrow (t_arrow (t_variable a_var ()) (t_variable b_var ()) ()) (t_arrow (t_variable a_var ()) (t_michelson_code ()) ()) ()))));
                    (C_TEST_EVAL, of_ligo_type O.(t_for_all a_var () (t_arrow (t_variable a_var ()) (t_michelson_code ()) ())));
                    (C_TEST_COMPILE_META_VALUE, of_ligo_type O.(t_for_all a_var () (t_arrow (t_variable a_var ()) (t_michelson_code ()) ())));
                    (C_TEST_DECOMPILE, of_ligo_type O.(t_for_all a_var () (t_arrow (t_michelson_code ()) (t_variable a_var ()) ())));
                    (* (C_TEST_TO_CONTRACT, of_ligo_type O.(t_for_all a_var () (t_for_all b_var () (t_arrow (t_typed_address (t_variable a_var ()) (t_variable b_var ())) _ ())))); *)
                    (C_TEST_TO_TYPED_ADDRESS, of_ligo_type O.(t_for_all a_var () (t_for_all b_var () (t_arrow (t_contract (t_variable a_var ())) (t_typed_address (t_variable a_var ()) (t_variable b_var ())) ()))));
                    (C_TEST_SET_BIG_MAP, of_ligo_type O.(t_for_all a_var () (t_for_all b_var () (t_arrow (t_int ()) (t_arrow (t_big_map (t_variable a_var ()) (t_variable b_var ())) (t_unit ()) ()) ()))));
                  ]
  let find c = CTMap.find_opt c tbl
end


let rec constant_typers ~raise ~test ~protocol_version loc c =
  match Constant_types.find c with
  | Some xs ->
     fun lst tv_opt ->
     let error = ref [] in
     (match xs ~error ~raise ~test ~protocol_version ~loc lst tv_opt with
      | Some tv -> tv
      | None -> failwith "oops")
  | _ ->
  match c with
  (* COMPARATOR *)
  | C_EQ                  -> comparator ~raise ~test loc "EQ" ;
  | C_NEQ                 -> comparator ~raise ~test loc "NEQ" ;
  | C_LT                  -> comparator ~raise ~test loc "LT" ;
  | C_GT                  -> comparator ~raise ~test loc "GT" ;
  | C_LE                  -> comparator ~raise ~test loc "LE" ;
  | C_GE                  -> comparator ~raise ~test loc "GE" ;
  (* BLOCKCHAIN *)
  | C_SAPLING_VERIFY_UPDATE -> sapling_verify_update ~raise loc ;
  | C_SAPLING_EMPTY_STATE -> sapling_empty_state ~raise loc ;
  (* TEST *)
  | C_TEST_EXTERNAL_CALL_TO_CONTRACT -> test_external_call_to_contract ~raise loc ;
  | C_TEST_EXTERNAL_CALL_TO_CONTRACT_EXN -> test_external_call_to_contract_exn ~raise loc ;
  | C_TEST_EXTERNAL_CALL_TO_ADDRESS -> test_external_call_to_address ~raise loc ;
  | C_TEST_EXTERNAL_CALL_TO_ADDRESS_EXN -> test_external_call_to_address_exn ~raise loc ;
  | C_TEST_BOOTSTRAP_CONTRACT -> test_bootstrap_contract ~raise loc ;
  | C_TEST_LAST_ORIGINATIONS -> test_last_originations ~raise loc ;
  | C_TEST_MUTATE_VALUE -> test_mutate_value ~raise loc ;
  | C_TEST_MUTATION_TEST -> test_mutation_test ~raise loc ;
  | C_TEST_MUTATION_TEST_ALL -> test_mutation_test_all ~raise loc ;
  | C_TEST_TO_CONTRACT -> test_to_contract ~raise loc ;
  | C_TEST_NTH_BOOTSTRAP_TYPED_ADDRESS -> test_nth_bootstrap_typed_address ~raise loc ;
  | C_TEST_TO_ENTRYPOINT -> test_to_entrypoint ~raise loc ;
  | C_TEST_ORIGINATE_FROM_FILE -> test_originate_from_file ~protocol_version ~raise loc ;
  | C_TEST_SAVE_MUTATION -> test_save_mutation ~raise loc ;
  | C_TEST_CREATE_CHEST -> only_supported_hangzhou ~raise ~protocol_version c @@ test_create_chest ~raise loc
  | C_TEST_CREATE_CHEST_KEY -> only_supported_hangzhou ~raise ~protocol_version c @@ test_create_chest_key ~raise loc
  | C_GLOBAL_CONSTANT -> only_supported_hangzhou ~raise ~protocol_version c @@ test_global_constant ~raise loc
  (* JsLIGO *)
  | C_POLYMORPHIC_ADD  -> polymorphic_add ~raise loc ;
  | _ as cst -> raise.raise (corner_case @@ Format.asprintf "typer not implemented for constant %a" PP.constant' cst)

and only_supported_hangzhou = fun ~raise ~protocol_version c default  ->
  match protocol_version with
  | Ligo_proto.Hangzhou -> default
  | Ligo_proto.Edo ->
    raise.raise @@ corner_case (
      Format.asprintf "Unsupported constant %a in protocol %s"
        PP.constant' c
        (Ligo_proto.variant_to_string protocol_version)
    )

