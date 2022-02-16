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

let failwith_ ~raise loc = typer_1_opt ~raise loc "failwith" @@ fun t opt ->
  let _ =
    if eq_1 t (t_string ())
    then ()
    else if eq_1 t (t_nat ())
    then ()
    else if eq_1 t (t_int ())
    then ()
    else
      raise.raise @@ typeclass_error loc
        [
          [t_string()] ;
          [t_nat()] ;
          [t_int()] ;
        ]
        [t] in
  let default = t_unit () in
  Simple_utils.Option.value ~default opt

let unopt ~raise loc = typer_1 ~raise loc "UNOPT" @@ fun a ->
  let a  = trace_option ~raise (expected_option loc a) @@ get_t_option a in
  a

let unopt_with_error ~raise loc = typer_2 ~raise loc "UNOPT_WITH_ERROR" @@ fun a b ->
  let a  = trace_option ~raise (expected_option loc a) @@ get_t_option a in
  let () = trace_option ~raise (expected_option loc a) @@ assert_t_string b in
  a

let assertion ~raise loc = typer_1 ~raise loc "ASSERT" @@ fun a ->
  let () = trace_option ~raise (expected_bool loc a) @@ assert_t_bool a in
  t_unit ()

let assertion_with_error ~raise loc = typer_2 ~raise loc "ASSERT_WITH_ERROR" @@ fun a b ->
  let () = trace_option ~raise (expected_bool loc a) @@ assert_t_bool a in
  let () = trace_option ~raise (expected_string loc b) @@ assert_t_string b in
  t_unit ()

let assert_some ~raise loc = typer_1 ~raise loc "ASSERT_SOME" @@ fun a ->
  let () = trace_option ~raise (expected_option loc a) @@ assert_t_option a in
  t_unit ()

let assert_some_with_error ~raise loc = typer_2 ~raise loc "ASSERT_SOME_WITH_ERROR" @@ fun a b ->
  let () = trace_option ~raise (expected_option loc a) @@ assert_t_option a in
  let () = trace_option ~raise (expected_string loc b) @@ assert_t_string b in
  t_unit ()

let assert_none ~raise loc = typer_1 ~raise loc "ASSERT_NONE" @@ fun a ->
  let () = trace_option ~raise (expected_option loc a) @@ assert_t_option a in
  t_unit ()

let assert_none_with_error ~raise loc = typer_2 ~raise loc "ASSERT_NONE_WITH_ERROR" @@ fun a b ->
  let () = trace_option ~raise (expected_option loc a) @@ assert_t_option a in
  let () = trace_option ~raise (expected_string loc b) @@ assert_t_string b in
  t_unit ()

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

let fold ~raise loc = typer_3 ~raise loc "FOLD" @@ fun body container init ->
  let { type1 = arg ; type2 = res } = trace_option ~raise (expected_function loc body) @@ get_t_arrow body in
  let (prec , cur) = trace_option ~raise (expected_pair loc arg) @@ get_t_pair arg in
  let key = trace_option ~raise (expected_list loc container) @@ Option.map_pair_or (get_t_list,get_t_set) container in
  let () = assert_eq_1 ~raise ~loc key cur in
  let () = assert_eq_1 ~raise ~loc prec res in
  let () = assert_eq_1 ~raise ~loc res init in
  res

(** FOLD_WHILE is a fold operation that takes an initial value of a certain type
    and then iterates on it until a condition is reached. The auxillary function
    that does the fold returns either boolean true or boolean false to indicate
    whether the fold should continue or not. Necessarily then the initial value
    must match the input parameter of the auxillary function, and the auxillary
    should return type (bool * input) *)
let fold_while ~raise loc = typer_2 ~raise loc "FOLD_WHILE" @@ fun body init ->
  let { type1 = arg ; type2 = result } = trace_option ~raise (expected_function loc body) @@ get_t_arrow body in
  let () = assert_eq_1 ~raise ~loc arg init in
  let () = assert_eq_1 ~raise ~loc (t_pair (t_bool ()) init) result
  in init

(* Continue and Stop are just syntactic sugar for building a pair (bool * a') *)
let continue ~raise loc = typer_1 ~raise loc "CONTINUE" @@ fun arg ->
  t_pair (t_bool ()) arg

let stop ~raise loc = typer_1 ~raise loc "STOP" @@ fun arg ->
  (t_pair (t_bool ()) arg)

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

let test_originate ~raise loc = typer_3 ~raise loc "TEST_ORIGINATE" @@ fun main storage balance ->
  let { type1 = in_ty ; type2 = _ } = trace_option ~raise (expected_function loc main) @@ get_t_arrow main in
  let param_ty,storage_ty = trace_option ~raise (expected_pair loc in_ty) @@ get_t_pair in_ty in
  let () = assert_eq_1 ~raise ~loc balance (t_mutez ()) in
  let () = assert_eq_1 ~raise ~loc storage storage_ty in
  (t_triplet (t_typed_address param_ty storage_ty) (t_michelson_code ()) (t_int ()))

let test_state_reset ~raise loc = typer_2 ~raise loc "TEST_STATE_RESET" @@ fun n amts ->
  let amt = trace_option ~raise (expected_list loc amts) @@ get_t_list amts in
  let () = trace_option ~raise (expected_mutez loc amt) @@ get_t_mutez amt in
  let () = trace_option ~raise (expected_nat loc n) @@ get_t_nat n in
  (t_unit ())

let test_bootstrap_contract ~raise loc = typer_3 ~raise loc "TEST_BOOTSTRAP_CONTRACT" @@ fun balance main storage ->
  let { type1 = in_ty ; type2 = _ } = trace_option ~raise (expected_function loc main) @@ get_t_arrow main in
  let _,storage_ty = trace_option ~raise (expected_pair loc in_ty) @@ get_t_pair in_ty in
  let () = assert_eq_1 ~raise ~loc balance (t_mutez ()) in
  let () = assert_eq_1 ~raise ~loc storage storage_ty in
  (t_unit ())

let test_nth_bootstrap_contract ~raise loc = typer_1 ~raise loc "TEST_NTH_BOOTSTRAP_CONTRACT" @@ fun n ->
  let () = assert_eq_1 ~raise ~loc n (t_nat ()) in
  (t_address ())

let test_set_now ~raise loc = typer_1 ~raise loc "TEST_SET_NOW" @@ fun time ->
  let () = assert_eq_1 ~raise ~loc time (t_timestamp ()) in
  (t_unit ())

let test_set_source ~raise loc = typer_1 ~raise loc "TEST_SET" @@ fun s ->
  let () = assert_eq_1 ~raise ~loc s (t_address ()) in
  (t_unit ())

let test_get_nth ~raise loc = typer_1 ~raise loc "TEST_GET_NTH" @@ fun n ->
  let () = trace_option ~raise (expected_int loc n) @@ assert_t_int n in
  (t_address ())

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

let test_get_storage ~raise loc = typer_1 ~raise loc "TEST_GET_STORAGE" @@ fun c ->
  let (_, storage_ty) = trace_option ~raise (expected_typed_address loc c) @@ get_t_typed_address c in
  storage_ty

let test_get_storage_of_address ~raise loc = typer_1 ~raise loc "TEST_GET_STORAGE_OF_ADDRESS" @@ fun addr ->
  let () = assert_eq_1 ~raise ~loc addr (t_address ()) in
  (t_michelson_code ())

let test_get_balance ~raise loc = typer_1 ~raise loc "TEST_GET_BALANCE" @@ fun addr ->
  let () = assert_eq_1 ~raise ~loc addr (t_address ()) in
  (t_mutez ())

let test_michelson_equal ~raise loc = typer_2 ~raise loc "TEST_ASSERT_EQUAL" @@ fun x y ->
  let () = trace_option ~raise (expected_michelson_code loc x) @@ assert_t_michelson_code x in
  let () = trace_option ~raise (expected_michelson_code loc y) @@ assert_t_michelson_code y in
  (t_bool ())

let test_log ~raise loc = typer_1 ~raise loc "TEST_LOG" @@ fun _ -> t_unit ()

let test_last_originations ~raise loc = typer_1 ~raise loc "TEST_LAST_ORIGINATIONS" @@ fun u ->
  let () = trace_option ~raise (expected_unit loc u) @@ assert_t_unit u in
  (t_map (t_address ()) (t_list (t_address ())))

let test_compile_meta_value ~raise loc = typer_1 ~raise loc "TEST_LAST_ORIGINATIONS" @@ fun _ ->
  (t_michelson_code ())

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

let test_run ~raise loc = typer_2 ~raise loc "TEST_RUN" @@ fun lambda expr ->
  let { type1 = arg ; type2 = _ } = trace_option ~raise (expected_function loc lambda) @@ get_t_arrow lambda in
  let () = assert_eq_1 ~raise ~loc arg expr in
  (t_michelson_code ())

let test_eval ~raise loc = typer_1 ~raise loc "TEST_EVAL" @@ fun _ ->
  (t_michelson_code ())

let test_decompile ~raise loc = typer_1_opt ~raise loc "TEST_DECOMPILE" @@ fun mich tv_opt ->
  let () = trace_option ~raise (expected_michelson_code loc mich) @@ get_t_michelson_code mich in
  match tv_opt with
  | None -> raise.raise (not_annotated loc)
  | Some t -> t

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

let test_to_typed_address ~raise loc = typer_1_opt ~raise loc "TEST_TO_TYPED_ADDRESS" @@ fun contract_tv tv_opt ->
  let parameter_ty = trace_option ~raise (expected_contract loc contract_tv) @@
             get_t_contract contract_tv in
  let tv = trace_option ~raise (not_annotated loc) tv_opt in
  let (parameter_ty', storage_ty) = trace_option ~raise (expected_contract loc tv) @@ get_t_typed_address tv in
  let () = assert_eq_1 ~raise ~loc parameter_ty parameter_ty' in
  t_typed_address parameter_ty storage_ty

let test_random ~raise loc = typer_1_opt ~raise loc "TEST_RANDOM" @@ fun unit tv_opt ->
  let () = assert_eq_1 ~raise ~loc unit (t_unit ()) in
  let tv = trace_option ~raise (not_annotated loc) tv_opt in
  tv

let test_set_big_map ~raise loc = typer_2 ~raise loc "TEST_SET_BIG_MAP" @@ fun id bm ->
  let () = assert_eq_1 ~raise ~loc id (t_int ()) in
  let _ = trace_option ~raise (expected_big_map loc bm) @@ get_t_big_map bm in
  t_unit ()

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

let test_compile_contract ~raise loc = typer_1 ~raise loc "TEST_COMPILE_CONTRACT" @@ fun _ ->
  (t_michelson_code ())

let test_cast_address ~raise loc = typer_1_opt ~raise loc "TEST_CAST_ADDRESS" @@ fun addr tv_opt ->
  let cast_t = trace_option ~raise (not_annotated loc) @@ tv_opt in
  let (pty,sty) = trace_option ~raise (expected_typed_address loc cast_t) @@ get_t_typed_address cast_t in
  let () = trace_option ~raise (expected_address loc addr) @@ get_t_address addr in
  t_typed_address pty sty

let test_add_account ~raise loc = typer_2 ~raise loc "TEST_ADD_ACCOUNT" @@ fun sk pk ->
  let _ = trace_option ~raise (expected_string loc pk) @@ get_t_string sk in
  let _ = trace_option ~raise (expected_key loc pk) @@ get_t_key pk in
  (t_unit ())

let test_new_account ~raise loc = typer_1 ~raise loc "TEST_NEW_ACCOUNT" @@ fun u ->
  let _ = trace_option ~raise (expected_unit loc u) @@ get_t_unit u in
  (t_pair (t_string ()) (t_key ()))

let test_get_voting_power ~raise loc = typer_1 ~raise loc "C_TEST_GET_VOTING_POWER" @@ fun u ->
  let _ = trace_option ~raise (expected_string loc u) @@ get_t_key_hash u in
  t_nat ()

let test_get_total_voting_power ~raise loc = typer_0 ~raise loc "C_TEST_GET_TOTAL_VOTING_POWER" @@ fun _ ->
  t_nat ()

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

let rec constant_typers ~raise ~test ~protocol_version loc c : typer = match c with
  | C_UNOPT               -> unopt ~raise loc ;
  | C_UNOPT_WITH_ERROR    -> unopt_with_error ~raise loc ;
  | C_ASSERTION           -> assertion ~raise loc ;
  | C_ASSERTION_WITH_ERROR-> assertion_with_error ~raise loc ;
  | C_ASSERT_SOME         -> assert_some ~raise loc ;
  | C_ASSERT_SOME_WITH_ERROR -> assert_some_with_error ~raise loc ;
  | C_ASSERT_NONE         -> assert_none ~raise loc ;
  | C_ASSERT_NONE_WITH_ERROR -> assert_none_with_error ~raise loc ;
  | C_FAILWITH            -> failwith_ ~raise loc ;
    (* LOOPS *)
  | C_FOLD_WHILE          -> fold_while ~raise loc ;
  | C_FOLD_CONTINUE       -> continue ~raise loc ;
  | C_FOLD_STOP           -> stop ~raise loc ;
  | C_FOLD                -> fold ~raise loc ;
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
  | C_TEST_ORIGINATE -> test_originate ~raise loc ;
  | C_TEST_SET_NOW -> test_set_now ~raise loc ;
  | C_TEST_SET_SOURCE -> test_set_source ~raise loc ;
  | C_TEST_SET_BAKER -> test_set_source ~raise loc ;
  | C_TEST_EXTERNAL_CALL_TO_CONTRACT -> test_external_call_to_contract ~raise loc ;
  | C_TEST_EXTERNAL_CALL_TO_CONTRACT_EXN -> test_external_call_to_contract_exn ~raise loc ;
  | C_TEST_EXTERNAL_CALL_TO_ADDRESS -> test_external_call_to_address ~raise loc ;
  | C_TEST_EXTERNAL_CALL_TO_ADDRESS_EXN -> test_external_call_to_address_exn ~raise loc ;
  | C_TEST_GET_STORAGE -> test_get_storage ~raise loc ;
  | C_TEST_GET_STORAGE_OF_ADDRESS -> test_get_storage_of_address ~raise loc ;
  | C_TEST_GET_BALANCE -> test_get_balance ~raise loc ;
  | C_TEST_MICHELSON_EQUAL -> test_michelson_equal ~raise loc ;
  | C_TEST_GET_NTH_BS -> test_get_nth ~raise loc ;
  | C_TEST_LOG -> test_log ~raise loc ;
  | C_TEST_STATE_RESET -> test_state_reset ~raise loc ;
  | C_TEST_BOOTSTRAP_CONTRACT -> test_bootstrap_contract ~raise loc ;
  | C_TEST_NTH_BOOTSTRAP_CONTRACT -> test_nth_bootstrap_contract ~raise loc ;
  | C_TEST_LAST_ORIGINATIONS -> test_last_originations ~raise loc ;
  | C_TEST_COMPILE_META_VALUE -> test_compile_meta_value ~raise loc ;
  | C_TEST_MUTATE_VALUE -> test_mutate_value ~raise loc ;
  | C_TEST_MUTATION_TEST -> test_mutation_test ~raise loc ;
  | C_TEST_MUTATION_TEST_ALL -> test_mutation_test_all ~raise loc ;
  | C_TEST_RUN -> test_run ~raise loc ;
  | C_TEST_EVAL -> test_eval ~raise loc ;
  | C_TEST_COMPILE_CONTRACT -> test_compile_contract ~raise loc ;
  | C_TEST_DECOMPILE -> test_decompile ~raise loc ;
  | C_TEST_TO_CONTRACT -> test_to_contract ~raise loc ;
  | C_TEST_NTH_BOOTSTRAP_TYPED_ADDRESS -> test_nth_bootstrap_typed_address ~raise loc ;
  | C_TEST_TO_ENTRYPOINT -> test_to_entrypoint ~raise loc ;
  | C_TEST_TO_TYPED_ADDRESS -> test_to_typed_address ~raise loc ;
  | C_TEST_RANDOM -> test_random ~raise loc ;
  | C_TEST_SET_BIG_MAP -> test_set_big_map ~raise loc ;
  | C_TEST_ORIGINATE_FROM_FILE -> test_originate_from_file ~protocol_version ~raise loc ;
  | C_TEST_SAVE_MUTATION -> test_save_mutation ~raise loc ;
  | C_TEST_CAST_ADDRESS -> test_cast_address ~raise loc;
  | C_TEST_CREATE_CHEST -> only_supported_hangzhou ~raise ~protocol_version c @@ test_create_chest ~raise loc
  | C_TEST_CREATE_CHEST_KEY -> only_supported_hangzhou ~raise ~protocol_version c @@ test_create_chest_key ~raise loc
  | C_TEST_ADD_ACCOUNT -> test_add_account ~raise loc;
  | C_TEST_NEW_ACCOUNT -> test_new_account ~raise loc;
  | C_TEST_GET_VOTING_POWER -> test_get_voting_power ~raise loc;
  | C_TEST_GET_TOTAL_VOTING_POWER -> test_get_total_voting_power ~raise loc;
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
