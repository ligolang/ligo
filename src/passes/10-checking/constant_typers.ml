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

and comparator ~cmp ~raise ~test : Location.t -> typer = fun loc -> typer_2 ~raise loc cmp @@ fun a b ->
  if test
  then
    bind_exists ~raise @@ List.Ne.of_list [list_comparator ~test loc cmp [a;b] None;
                                           set_comparator ~test loc cmp [a;b] None;
                                           map_comparator ~test loc cmp [a;b] None;
                                           simple_comparator loc cmp [a;b] None;
                                           record_comparator ~test loc cmp [a;b] None;
                                           sum_comparator ~test loc cmp [a;b] None;
                                           big_map_comparator ~test loc cmp [a;b] None]
  else
    bind_exists ~raise @@ List.Ne.of_list [simple_comparator loc cmp [a;b] None;
                                           record_comparator ~test loc cmp [a;b] None;
                                           sum_comparator ~test loc cmp [a;b] None]

module O = Ast_typed

type typer = error:[`TC of O.type_expression list] list ref -> raise:(Errors.typer_error,Main_warnings.all) raise -> options:Compiler_options.middle_end -> loc:Location.t -> O.type_expression list -> O.type_expression option -> O.type_expression option
type typer_table = error:[`TC of O.type_expression list] list ref -> raise:(Errors.typer_error,Main_warnings.all) raise -> options:Compiler_options.middle_end -> loc:Location.t -> O.type_expression list -> O.type_expression option -> (O.type_expression * type_expression Inference.TMap.t * O.type_expression) option

(* Given a ligo type, construct the corresponding typer *)
let typer_of_ligo_type ?(add_tc = true) ?(fail = true) lamb_type : typer = fun ~error ~raise ~options ~loc lst tv_opt ->
  ignore options;
  let avs, lamb_type = O.Helpers.destruct_for_alls lamb_type in
  Simple_utils.Trace.try_with (fun ~raise ~catch:_ ->
      let table = Inference.infer_type_applications ~raise ~loc ~default_error:(fun loc t t' -> `Outer_error (loc, t', t)) avs lamb_type lst tv_opt in
      let lamb_type = Inference.TMap.fold (fun tv t r -> Ast_typed.Helpers.subst_type tv t r) table lamb_type in
      let _, tv = Ast_typed.Helpers.destruct_arrows_n lamb_type (List.length lst) in
      Some tv)
    (fun ~catch:_ -> function
     | `Outer_error (loc, t', t) ->
        if fail then raise.error (assert_equal loc t' t) else None
     | _ ->
        let arrs, _ = O.Helpers.destruct_arrows_n lamb_type (List.length lst) in
        if add_tc then error := `TC arrs :: ! error else ();
        None)

let typer_table_of_ligo_type ?(add_tc = true) ?(fail = true) lamb_type : typer_table = fun ~error ~raise ~options ~loc lst tv_opt ->
  ignore options;
  let original_type = lamb_type in
  let avs, lamb_type = O.Helpers.destruct_for_alls lamb_type in
  Simple_utils.Trace.try_with (fun ~raise ~catch:_ ->
      let table = Inference.infer_type_applications ~raise ~loc ~default_error:(fun loc t t' -> `Outer_error (loc, t', t)) avs lamb_type lst tv_opt in
      let lamb_type = Inference.TMap.fold (fun tv t r -> Ast_typed.Helpers.subst_type tv t r) table lamb_type in
      Some (lamb_type, table, original_type))
    (fun ~catch:_ -> function
     | `Outer_error (loc, t', t) ->
        if fail then raise.error (assert_equal loc t' t) else None
     | _ ->
        let arrs, _ = O.Helpers.destruct_arrows_n lamb_type (List.length lst) in
        if add_tc then error := `TC arrs :: ! error else ();
        None)


let typer_of_comparator (typer : raise:_ -> test:_ -> _ -> O.type_expression list -> O.type_expression option -> O.type_expression) : typer =
  fun ~error ~raise ~options ~loc lst tv_opt ->
  ignore error;
  Some (typer ~raise ~test:options.test loc lst tv_opt)

let raise_of_errors ~raise ~loc lst = function
  | [] ->
     raise.error @@ (corner_case "Cannot find a suitable type for expression")
  | [`TC v] ->
     raise.error @@ expected loc v lst
  | xs ->
     let tc = List.filter_map ~f:(function `TC v -> Some v) xs in
     raise.error @@ typeclass_error loc (List.rev tc) lst

(* Given a list of typers, make a new typer that tries them in order *)
let rec any_of : typer list -> typer = fun typers ->
  fun ~error ~raise ~options ~loc lst tv_opt ->
  match typers with
  | [] -> raise_of_errors ~raise ~loc lst (! error)
  | typer :: typers ->
     match typer ~error ~raise ~options ~loc lst tv_opt with
     | Some tv -> Some tv
     | None -> any_of typers ~error ~raise ~options ~loc lst tv_opt

let rec any_table_of : typer_table list -> typer_table = fun typers ->
  fun ~error ~raise ~options ~loc lst tv_opt ->
  match typers with
  | [] -> raise_of_errors ~raise ~loc lst (! error)
  | typer :: typers ->
     match typer ~error ~raise ~options ~loc lst tv_opt with
     | Some (tv, table, ot) -> Some (tv, table, ot)
     | None -> any_table_of typers ~error ~raise ~options ~loc lst tv_opt


(* This prevents wraps a typer, allowing usage only since a protocol version *)
let constant_since_protocol ~since ~constant typer : typer = fun ~error ~raise ~options ~loc ->
  if (Environment.Protocols.compare options.protocol_version since) >= 0 then
    typer ~error ~raise ~options ~loc
  else
    raise.error (constant_since_protocol loc constant since)

(* This prevents wraps a typer, allowing usage only in a particular protocol version *)
let only_on_protocol ~protocol typer : typer = fun ~error ~raise ~options ~loc ->
  if (Environment.Protocols.compare options.protocol_version protocol) = 0 then
    typer ~error ~raise ~options ~loc
  else
    fun _ _ -> None


module CTMap = Simple_utils.Map.Make(struct type t = O.constant' let compare x y = O.Compare.constant' x y end)
type t = typer CTMap.t

module Constant_types = struct

  let a_var = O.TypeVar.of_input_var "'a"
  let b_var = O.TypeVar.of_input_var "'b"
  let c_var = O.TypeVar.of_input_var "'c"

  (* Helpers *)
  let for_all binder f =
    let binder = O.TypeVar.of_input_var ("'" ^ binder) in
    t_for_all binder Type (f (t_variable binder ()))

  let (^->) arg ret = t_arrow arg ret ()

  let of_type c t =
    c, any_of [typer_of_ligo_type t]

  let of_type_since ~since ~constant c t =
    let _, t = of_type c t in
    c, constant_since_protocol ~since ~constant @@ t

  let of_types c ts =
    (c, any_of (List.map ~f:(fun v -> typer_of_ligo_type v) ts))

  let of_types_protocol c ts =
    (c, any_of (List.map ~f:(fun (protocol, v) -> only_on_protocol ~protocol @@ typer_of_ligo_type v) ts))

  let typer_of_type_no_tc t =
    typer_of_ligo_type ~add_tc:false ~fail:false t
  let () = ignore typer_of_type_no_tc
  let tbl : t = CTMap.of_list [
                    (* LOOPS *)
                    of_type C_LOOP_LEFT O.(for_all "a" @@ fun a -> for_all "b" @@ fun b -> (a ^-> t_sum_ez [("left", a) ; ("right", b)]) ^-> a ^-> b);
                    of_type C_LEFT O.(for_all "a" @@ fun a -> a ^-> t_sum_ez [("left", a) ; ("right", a)]);
                    of_type C_LOOP_CONTINUE O.(for_all "a" @@ fun a -> a ^-> t_sum_ez [("left", a) ; ("right", a)]);
                    of_type C_LOOP_STOP O.(for_all "a" @@ fun a -> a ^-> t_sum_ez [("left", a) ; ("right", a)]);
                    of_types C_ITER [
                        O.(for_all "a" @@ fun a -> (a ^-> t_unit ()) ^-> t_list a ^-> t_unit ());
                        O.(for_all "a" @@ fun a -> (a ^-> t_unit ()) ^-> t_set  a ^-> t_unit ());
                      ];
                    of_types C_FOLD [
                        O.(for_all "a" @@ fun a -> for_all "b" @@ fun b -> (t_pair a b ^-> a) ^-> t_list b ^-> a ^-> a);
                        O.(for_all "a" @@ fun a -> for_all "b" @@ fun b -> (t_pair a b ^-> a) ^-> t_set b ^-> a ^-> a);
                      ];
                    (* MAP *)
                    of_type C_MAP_EMPTY O.(for_all "a" @@ fun a -> for_all "b" @@ fun b -> t_map a b);
                    of_type C_BIG_MAP_EMPTY O.(for_all "a" @@ fun a -> for_all "b" @@ fun b -> t_big_map a b);
                    of_types C_MAP_ADD [
                        O.(for_all "a" @@ fun a -> for_all "b" @@ fun b -> a ^-> b ^-> t_map a b ^-> t_map a b);
                        O.(for_all "a" @@ fun a -> for_all "b" @@ fun b -> a ^-> b ^-> t_big_map a b ^-> t_big_map a b);
                      ];
                    of_types C_MAP_REMOVE [
                        O.(for_all "a" @@ fun a -> for_all "b" @@ fun b -> a ^-> t_map a b ^-> t_map a b);
                        O.(for_all "a" @@ fun a -> for_all "b" @@ fun b -> a ^-> t_big_map a b ^-> t_big_map a b);
                      ];
                    of_types C_MAP_UPDATE [
                        O.(for_all "a" @@ fun a -> for_all "b" @@ fun b -> a ^-> t_option b ^-> t_map a b ^-> t_map a b);
                        O.(for_all "a" @@ fun a -> for_all "b" @@ fun b -> a ^-> t_option b ^-> t_big_map a b ^-> t_big_map a b);
                      ];
                    of_type C_MAP_GET_AND_UPDATE O.(for_all "a" @@ fun a -> for_all "b" @@ fun b -> a ^-> t_option b ^-> t_map a b ^-> t_pair (t_option b) (t_map a b));
                    of_type C_BIG_MAP_GET_AND_UPDATE O.(for_all "a" @@ fun a -> for_all "b" @@ fun b -> a ^-> t_option b ^-> t_big_map a b ^-> t_pair (t_option b) (t_big_map a b));
                    of_types C_MAP_FIND_OPT [
                        O.(for_all "a" @@ fun a -> for_all "b" @@ fun b -> a ^-> t_map a b ^-> t_option b);
                        O.(for_all "a" @@ fun a -> for_all "b" @@ fun b -> a ^-> t_big_map a b ^-> t_option b);
                      ];
                    of_types C_MAP_FIND [
                        O.(for_all "a" @@ fun a -> (for_all "b" @@ fun b -> a ^-> t_map a b ^-> b));
                        O.(for_all "a" @@ fun a -> (for_all "b" @@ fun b -> a ^-> t_big_map a b ^-> b));
                      ];
                    of_type C_MAP_MAP O.(for_all "a" @@ fun a -> for_all "b" @@ fun b -> for_all "c" @@ fun c -> (t_pair a b ^-> c) ^-> t_map a b ^-> t_map a c);
                    of_type C_MAP_ITER O.(for_all "a" @@ fun a -> for_all "b" @@ fun b -> (t_pair a b ^-> t_unit ()) ^-> t_map a b ^-> t_unit ());
                    of_type C_MAP_FOLD O.(for_all "a" @@ fun a -> for_all "b" @@ fun b -> for_all "c" @@ fun c -> (t_pair c (t_pair a b) ^-> c) ^-> t_map a b ^-> c ^-> c);
                    (* LIST *)
                    of_type C_LIST_EMPTY O.(for_all "a" @@ fun a -> t_list a);
                    of_type C_CONS O.(for_all "a" @@ fun a -> a ^-> t_list a ^-> t_list a);
                    of_type C_LIST_MAP O.(for_all "a" @@ fun a -> for_all "b" @@ fun b -> (a ^-> b) ^-> t_list a ^-> t_list b);
                    of_type C_LIST_ITER O.(for_all "a" @@ fun a -> (a ^-> t_unit ()) ^-> t_list a ^-> t_unit ());
                    of_type C_LIST_FOLD O.(for_all "a" @@ fun a -> for_all "b" @@ fun b -> (t_pair b a ^-> b) ^-> t_list a ^-> b ^-> b);
                    of_type C_LIST_FOLD_LEFT O.(for_all "a" @@ fun a -> for_all "b" @@ fun b -> (t_pair b a ^-> b) ^-> b ^-> t_list a ^-> b);
                    of_type C_LIST_FOLD_RIGHT O.(for_all "a" @@ fun a -> for_all "b" @@ fun b -> (t_pair a b ^-> b) ^-> t_list a ^-> b ^-> b);
                    (* SET *)
                    of_type C_SET_EMPTY O.(for_all "a" @@ fun a -> t_set a);
                    of_type C_SET_LITERAL O.(for_all "a" @@ fun a -> t_list a ^-> t_set a);
                    of_type C_SET_MEM O.(for_all "a" @@ fun a -> a ^-> t_set a ^-> t_bool ());
                    of_type C_SET_ADD O.(for_all "a" @@ fun a -> a ^-> t_set a ^-> t_set a);
                    of_type C_SET_REMOVE O.(for_all "a" @@ fun a -> a ^-> t_set a ^-> t_set a);
                    of_type C_SET_UPDATE O.(for_all "a" @@ fun a -> a ^-> t_bool () ^-> t_set a ^-> t_set a);
                    of_type C_SET_ITER O.(for_all "a" @@ fun a -> (a ^-> t_unit ()) ^-> t_set a ^-> t_unit ());
                    of_type C_SET_FOLD O.(for_all "a" @@ fun a -> for_all "b" @@ fun b -> (t_pair b a ^-> b) ^-> t_set a ^-> b ^-> b);
                    of_type C_SET_FOLD_DESC O.(for_all "a" @@ fun a -> for_all "b" @@ fun b -> (t_pair a b ^-> b) ^-> t_set a ^-> b ^-> b);
                    of_types C_CONCAT [
                        O.(t_string () ^-> t_string () ^-> t_string ());
                        O.(t_bytes () ^-> t_bytes () ^-> t_bytes ());
                      ];
                    of_type C_BYTES_UNPACK O.(for_all "a" @@ fun a -> t_bytes () ^-> t_option a);
                    of_type C_NONE O.(for_all "a" @@ fun a -> t_option a);
                    of_type C_SOME O.(for_all "a" @@ fun a -> a ^-> t_option a);
                    of_type C_UNOPT O.(for_all "a" @@ fun a -> t_option a ^-> a);
                    of_type C_UNOPT_WITH_ERROR O.(for_all "a" @@ fun a -> t_option a ^-> t_string () ^-> a);
                    of_type C_OPTION_MAP O.(for_all "a" @@ fun a -> for_all "b" @@ fun b -> (a ^-> b) ^-> t_option a ^-> t_option b);
                    (* GLOBAL *)
                    of_type C_ADDRESS O.(for_all "a" @@ fun a -> t_contract a ^-> t_address ());
                    of_type C_CONTRACT O.(for_all "a" @@ fun a -> t_address () ^-> t_contract a);
                    of_type C_CONTRACT_OPT O.(for_all "a" @@ fun a -> t_address () ^-> t_option (t_contract a));
                    of_type C_CONTRACT_WITH_ERROR O.(for_all "a" @@ fun a -> t_address () ^-> t_string () ^-> t_contract a);
                    of_type C_CONTRACT_ENTRYPOINT_OPT O.(for_all "a" @@ fun a -> (t_string () ^-> t_address () ^-> t_option (t_contract a)));
                    of_type C_CONTRACT_ENTRYPOINT O.(for_all "a" @@ fun a -> (t_string () ^-> t_address () ^-> t_contract a));
                    of_type C_IMPLICIT_ACCOUNT O.(t_key_hash () ^-> t_contract (t_unit ()));
                    of_type C_SET_DELEGATE O.(t_option (t_key_hash ()) ^-> t_operation ());
                    of_type C_SELF O.(for_all "a" @@ fun a -> (t_string () ^-> t_contract a));
                    of_type C_SELF_ADDRESS O.(t_address ());
                    of_type C_CALL O.(for_all "a" @@ fun a -> (a ^-> t_mutez () ^-> t_contract a ^-> t_operation ()));
                    of_type C_CREATE_CONTRACT O.(for_all "a" @@ fun a -> for_all "b" @@ fun b -> (t_pair a b ^-> t_pair (t_list (t_operation ())) b) ^-> t_option (t_key_hash ()) ^-> t_mutez () ^-> b ^-> t_pair (t_operation ()) (t_address ()));
                    of_type C_UNIT O.(t_unit ());
                    of_type C_TRUE O.(t_bool ());
                    of_type C_FALSE O.(t_bool ());
                    of_type C_OPEN_CHEST O.(t_chest_key () ^-> t_chest () ^-> t_nat () ^-> t_chest_opening_result ());
                    of_type C_VIEW O.(for_all "a" @@ fun a -> for_all "b" @@ fun b -> t_string () ^-> a ^-> t_address () ^-> t_option b);
                    (* MATH *)
                    of_types C_POLYMORPHIC_ADD [
                        O.(t_string () ^-> t_string () ^-> t_string ());
                        O.(t_bls12_381_g1 () ^-> t_bls12_381_g1 () ^-> t_bls12_381_g1 ());
                        O.(t_bls12_381_g2 () ^-> t_bls12_381_g2 () ^-> t_bls12_381_g2 ());
                        O.(t_bls12_381_fr () ^-> t_bls12_381_fr () ^-> t_bls12_381_fr ());
                        O.(t_nat () ^-> t_nat () ^-> t_nat ());
                        O.(t_int () ^-> t_int () ^-> t_int ());
                        O.(t_mutez () ^-> t_mutez () ^-> t_mutez ());
                        O.(t_nat () ^-> t_int () ^-> t_int ());
                        O.(t_int () ^-> t_nat () ^-> t_int ());
                        O.(t_timestamp () ^-> t_int () ^-> t_timestamp ());
                        O.(t_int () ^-> t_timestamp () ^-> t_timestamp ());
                      ];
                    of_types C_POLYMORPHIC_SUB [
                        O.(t_bls12_381_g1 () ^-> t_bls12_381_g1 () ^-> t_bls12_381_g1 ());
                        O.(t_bls12_381_g2 () ^-> t_bls12_381_g2 () ^-> t_bls12_381_g2 ());
                        O.(t_bls12_381_fr () ^-> t_bls12_381_fr () ^-> t_bls12_381_fr ());
                        O.(t_nat () ^-> t_nat () ^-> t_int ());
                        O.(t_int () ^-> t_int () ^-> t_int ());
                        O.(t_nat () ^-> t_int () ^-> t_int ());
                        O.(t_int () ^-> t_nat () ^-> t_int ());
                        O.(t_timestamp () ^-> t_timestamp () ^-> t_int ());
                        O.(t_timestamp () ^-> t_int () ^-> t_timestamp ());
                        O.(t_mutez () ^-> t_mutez () ^-> t_option (t_mutez ())) ;
                      ];
                    of_types C_ADD [
                        O.(t_bls12_381_g1 () ^-> t_bls12_381_g1 () ^-> t_bls12_381_g1 ());
                        O.(t_bls12_381_g2 () ^-> t_bls12_381_g2 () ^-> t_bls12_381_g2 ());
                        O.(t_bls12_381_fr () ^-> t_bls12_381_fr () ^-> t_bls12_381_fr ());
                        O.(t_nat () ^-> t_nat () ^-> t_nat ());
                        O.(t_int () ^-> t_int () ^-> t_int ());
                        O.(t_mutez () ^-> t_mutez () ^-> t_mutez ());
                        O.(t_nat () ^-> t_int () ^-> t_int ());
                        O.(t_int () ^-> t_nat () ^-> t_int ());
                        O.(t_timestamp () ^-> t_int () ^-> t_timestamp ());
                        O.(t_int () ^-> t_timestamp () ^-> t_timestamp ());
                      ];
                    of_types C_MUL [
                        O.(t_bls12_381_g1 () ^-> t_bls12_381_fr () ^-> t_bls12_381_g1 ());
                        O.(t_bls12_381_g2 () ^-> t_bls12_381_fr () ^-> t_bls12_381_g2 ());
                        O.(t_bls12_381_fr () ^-> t_bls12_381_fr () ^-> t_bls12_381_fr ());
                        O.(t_nat () ^-> t_bls12_381_fr () ^-> t_bls12_381_fr ());
                        O.(t_int () ^-> t_bls12_381_fr () ^-> t_bls12_381_fr ());
                        O.(t_bls12_381_fr () ^-> t_nat () ^-> t_bls12_381_fr ());
                        O.(t_bls12_381_fr () ^-> t_int () ^-> t_bls12_381_fr ());
                        O.(t_nat () ^-> t_nat () ^-> t_nat ());
                        O.(t_int () ^-> t_int () ^-> t_int ());
                        O.(t_nat () ^-> t_mutez () ^-> t_mutez ());
                        O.(t_mutez () ^-> t_nat () ^-> t_mutez ());
                        O.(t_int () ^-> t_nat () ^-> t_int ());
                        O.(t_nat () ^-> t_int () ^-> t_int ());
                      ];
                    of_types C_SUB [
                        O.(t_bls12_381_g1 () ^-> t_bls12_381_fr () ^-> t_bls12_381_g1 ());
                        O.(t_bls12_381_g2 () ^-> t_bls12_381_fr () ^-> t_bls12_381_g2 ());
                        O.(t_bls12_381_fr () ^-> t_bls12_381_fr () ^-> t_bls12_381_fr ());
                        O.(t_nat () ^-> t_nat () ^-> t_int ());
                        O.(t_int () ^-> t_int () ^-> t_int ());
                        O.(t_int () ^-> t_nat () ^-> t_int ());
                        O.(t_nat () ^-> t_int () ^-> t_int ());
                        O.(t_timestamp () ^-> t_timestamp () ^-> t_int ());
                        O.(t_timestamp () ^-> t_int () ^-> t_timestamp ());
                        O.(t_mutez () ^-> t_mutez () ^-> t_mutez ());
                      ];
                    of_type C_SUB_MUTEZ O.(t_mutez () ^-> t_mutez () ^-> t_option (t_mutez ()));
                    of_types C_DIV [
                        O.(t_nat () ^-> t_nat () ^-> t_nat ());
                        O.(t_int () ^-> t_int () ^-> t_int ());
                        O.(t_nat () ^-> t_int () ^-> t_int ());
                        O.(t_int () ^-> t_nat () ^-> t_int ());
                        O.(t_mutez () ^-> t_nat () ^-> t_mutez ());
                        O.(t_mutez () ^-> t_mutez () ^-> t_nat ());
                      ];
                    of_types C_MOD [
                        O.(t_nat () ^-> t_nat () ^-> t_nat ());
                        O.(t_nat () ^-> t_int () ^-> t_nat ());
                        O.(t_int () ^-> t_nat () ^-> t_nat ());
                        O.(t_int () ^-> t_int () ^-> t_nat ());
                        O.(t_mutez () ^-> t_nat () ^-> t_mutez ());
                        O.(t_mutez () ^-> t_mutez () ^-> t_mutez ());
                      ];
                    of_types C_NEG [
                        O.(t_int () ^-> t_int ());
                        O.(t_nat () ^-> t_int ());
                        O.(t_bls12_381_g1 () ^-> t_bls12_381_g1 ());
                        O.(t_bls12_381_g2 () ^-> t_bls12_381_g2 ());
                        O.(t_bls12_381_fr () ^-> t_bls12_381_fr ());
                      ];
                    (* LOGIC *)
                    of_types C_NOT [
                        O.(t_bool () ^-> t_bool ());
                        O.(t_int () ^-> t_int ());
                        O.(t_nat () ^-> t_int ());
                      ];
                    of_types C_AND [
                        O.(t_bool () ^-> t_bool () ^-> t_bool ());
                        O.(t_nat () ^-> t_nat () ^-> t_nat ());
                        O.(t_int () ^-> t_nat () ^-> t_nat ());
                      ];
                    of_types C_OR [
                        O.(t_bool () ^-> t_bool () ^-> t_bool ());
                        O.(t_nat () ^-> t_nat () ^-> t_nat ());
                      ];
                    of_types C_XOR [
                        O.(t_bool () ^-> t_bool () ^-> t_bool ());
                        O.(t_nat () ^-> t_nat () ^-> t_nat ());
                      ];
                    of_type C_LSL O.(t_nat () ^-> t_nat () ^-> t_nat ());
                    of_type C_LSR O.(t_nat () ^-> t_nat () ^-> t_nat ());
                    (* TEST *)
                    of_type C_TEST_COMPILE_CONTRACT O.(for_all "a" @@ fun a -> for_all "b" @@ fun b -> (t_pair a b ^-> t_pair (t_list (t_operation ())) b) ^-> t_michelson_contract ());
                    of_type C_TEST_SIZE O.(t_michelson_contract () ^-> t_int ());
                    of_type C_TEST_ORIGINATE O.(t_michelson_contract () ^-> t_michelson_code () ^-> t_mutez () ^-> t_address ());
                    of_type C_TEST_BOOTSTRAP_CONTRACT O.(for_all "a" @@ fun a -> for_all "b" @@ fun b -> (t_pair a b ^-> t_pair (t_list (t_operation ())) b) ^-> b ^-> t_mutez () ^-> t_unit ());
                    of_type C_TEST_LAST_ORIGINATIONS O.(t_unit () ^-> t_map (t_address ()) (t_list (t_address ())));
                    of_type C_TEST_NTH_BOOTSTRAP_TYPED_ADDRESS O.(for_all "a" @@ fun a -> for_all "b" @@ fun b -> t_nat () ^-> t_typed_address a b);
                    of_type C_TEST_SET_SOURCE O.(t_address () ^-> t_unit ());
                    of_type C_TEST_SET_BAKER O.(t_test_baker_policy () ^-> t_unit ());
                    of_type C_TEST_NTH_BOOTSTRAP_CONTRACT O.(t_nat () ^-> t_address ());
                    of_type C_TEST_GET_STORAGE_OF_ADDRESS O.(t_address () ^-> t_michelson_code ());
                    of_type C_TEST_GET_BALANCE O.(t_address () ^-> t_mutez ());
                    of_type C_TEST_GET_NTH_BS O.(t_int () ^-> t_triplet (t_address ()) (t_key ()) (t_string ()));
                    of_type C_TEST_PRINT O.(t_int () ^-> t_string () ^-> t_unit ());
                    of_type C_TEST_TO_STRING O.(for_all "a" @@ fun a -> a ^-> t_string ());
                    of_type C_TEST_UNESCAPE_STRING O.(t_string () ^-> t_string ());
                    of_type C_TEST_STATE_RESET O.(t_option (t_timestamp ()) ^-> t_nat () ^-> t_list (t_mutez ()) ^-> t_unit ());
                    of_type C_TEST_GET_VOTING_POWER O.(t_key_hash () ^-> t_nat ());
                    of_type C_TEST_GET_TOTAL_VOTING_POWER O.(t_nat ());
                    of_type C_TEST_CAST_ADDRESS O.(for_all "a" @@ fun a -> for_all "b" @@ fun b -> t_address () ^-> t_typed_address a b);
                    of_type C_TEST_RANDOM O.(for_all "a" @@ fun a -> t_bool () ^-> t_gen a);
                    of_type C_TEST_GENERATOR_EVAL O.(for_all "a" @@ fun a -> t_gen a ^-> a);
                    of_type C_TEST_MUTATE_VALUE O.(for_all "a" @@ fun a -> t_nat () ^-> a ^-> t_option (t_pair a (t_mutation ())));
                    of_type C_TEST_MUTATION_TEST O.(for_all "a" @@ fun a -> for_all "b" @@ fun b -> a ^-> (a ^-> b) ^-> t_option (t_pair b (t_mutation ())));
                    of_type C_TEST_MUTATION_TEST_ALL O.(for_all "a" @@ fun a -> for_all "b" @@ fun b -> (a ^-> (a ^-> b) ^-> t_list (t_pair b (t_mutation ()))));
                    of_type C_TEST_SAVE_MUTATION O.(t_string () ^-> t_mutation () ^-> t_option (t_string ()));
                    of_type C_TEST_ADD_ACCOUNT O.(t_string () ^-> t_key () ^-> t_unit ());
                    of_type C_TEST_NEW_ACCOUNT O.(t_unit () ^-> t_pair (t_string ()) (t_key ()));
                    of_type C_TEST_RUN O.(for_all "a" @@ fun a -> for_all "b" @@ fun b -> (a ^-> b) ^-> a ^-> t_michelson_code ());
                    of_type C_TEST_DECOMPILE O.(for_all "a" @@ fun a -> t_michelson_code () ^-> a);
                    of_type C_TEST_TO_TYPED_ADDRESS O.(for_all "a" @@ fun a -> (for_all "b" @@ fun b -> t_contract a ^-> t_typed_address a b));
                    of_type C_TEST_EXTERNAL_CALL_TO_ADDRESS O.(t_address () ^-> t_option (t_string ()) ^-> t_michelson_code () ^-> t_mutez () ^-> t_test_exec_result ());
                    of_type C_TEST_EXTERNAL_CALL_TO_ADDRESS_EXN O.(t_address () ^-> t_option (t_string ()) ^-> t_michelson_code () ^-> t_mutez () ^-> t_nat ());
                    of_type C_TEST_SET_BIG_MAP O.(for_all "a" @@ fun a -> for_all "b" @@ fun b -> t_int () ^-> t_big_map a b ^-> t_unit ());
                    of_type C_TEST_BAKER_ACCOUNT O.(t_pair (t_string ()) (t_key ()) ^-> t_option (t_mutez ()) ^-> t_unit ());
                    of_type C_TEST_REGISTER_DELEGATE O.(t_key_hash () ^-> t_unit ());
                    of_type C_TEST_BAKE_UNTIL_N_CYCLE_END O.(t_nat () ^-> t_unit ());
                    of_type C_TEST_TO_CONTRACT O.(for_all "a" @@ fun a -> for_all "b" @@ fun b -> t_typed_address a b ^-> t_contract a);
                    of_type C_TEST_CREATE_CHEST O.(t_bytes () ^-> t_nat () ^-> t_pair (t_chest ()) (t_chest_key ()));
                    of_type C_TEST_CREATE_CHEST_KEY O.(t_chest () ^-> t_nat () ^-> t_chest_key ());
                    of_type C_GLOBAL_CONSTANT O.(for_all "a" @@ fun a -> t_string () ^-> a);
                    of_type C_TEST_COMPILE_CONTRACT_FROM_FILE O.(t_string () ^-> t_string () ^-> t_list (t_string ()) ^-> t_michelson_contract ());
                    of_type C_TEST_REGISTER_CONSTANT O.(t_michelson_code () ^-> t_string ());
                    of_type C_TEST_CONSTANT_TO_MICHELSON O.(t_string () ^-> t_michelson_code ());
                    of_type C_TEST_REGISTER_FILE_CONSTANTS O.(t_string () ^-> t_list (t_string ()));
                    of_type C_TEST_TO_ENTRYPOINT O.(for_all "a" @@ fun a -> for_all "b" @@ fun b -> for_all "c" @@ fun c -> (t_string () ^-> t_typed_address a b ^-> t_contract c));
                    of_type C_TEST_FAILWITH (for_all "a" @@ fun a -> for_all "b" @@ fun b -> (a ^-> b));
                    of_type C_TEST_PUSH_CONTEXT O.(t_unit () ^-> t_unit ());
                    of_type C_TEST_POP_CONTEXT O.(t_unit () ^-> t_unit ());
                    of_type C_TEST_DROP_CONTEXT O.(t_unit () ^-> t_unit ());
                    of_type C_TEST_READ_CONTRACT_FROM_FILE O.(t_string () ^-> t_michelson_contract ());
                    of_type C_TEST_SIGN O.(t_string () ^-> t_bytes () ^-> t_signature ());
                    of_type C_TEST_GET_ENTRYPOINT O.(for_all "a" @@ fun a -> t_contract a ^-> t_option (t_string ()));
                    (* SAPLING *)
                    of_type C_SAPLING_EMPTY_STATE O.(t_for_all a_var Singleton (t_sapling_state (t_variable a_var ())));
                    of_type C_SAPLING_VERIFY_UPDATE O.(t_for_all a_var Singleton (t_sapling_transaction (t_variable a_var ()) ^-> t_sapling_state (t_variable a_var ()) ^-> t_option (t_pair (t_int ()) (t_sapling_state (t_variable a_var ())))));
                    (* CUSTOM *)
                    (* COMPARATOR *)
                    (C_EQ, typer_of_comparator (comparator ~cmp:"EQ"));
                    (C_NEQ, typer_of_comparator (comparator ~cmp:"NEQ"));
                    (C_LT, typer_of_comparator (comparator ~cmp:"LT"));
                    (C_GT, typer_of_comparator (comparator ~cmp:"GT"));
                    (C_LE, typer_of_comparator (comparator ~cmp:"LE"));
                    (C_GE, typer_of_comparator (comparator ~cmp:"GE"));
                  ]

  let typer_of_type_no_tc t =
    typer_table_of_ligo_type ~add_tc:false ~fail:false t

  let failwith_typer = any_table_of [
                           typer_of_type_no_tc @@ O.(t_string () ^-> t_unit ());
                           typer_of_type_no_tc @@ O.(t_nat () ^-> t_unit ());
                           typer_of_type_no_tc @@ O.(t_int () ^-> t_unit ());
                           typer_table_of_ligo_type O.(for_all "a" @@ fun a -> t_string () ^-> a);
                           typer_table_of_ligo_type O.(for_all "a" @@ fun a -> t_nat () ^-> a);
                           typer_table_of_ligo_type O.(for_all "a" @@ fun a -> t_int () ^-> a);
                           ]

  let int_typer = any_table_of [
                      typer_table_of_ligo_type O.(t_nat () ^-> t_int ());
                      typer_table_of_ligo_type O.(t_bls12_381_fr () ^-> t_int ());
                    ]

  let ediv_typer = any_table_of [
                      typer_table_of_ligo_type O.(t_nat () ^-> t_nat () ^-> t_option (t_pair (t_nat ()) (t_nat ())));
                      typer_table_of_ligo_type O.(t_int () ^-> t_int () ^-> t_option (t_pair (t_int ()) (t_nat ())));
                      typer_table_of_ligo_type O.(t_nat () ^-> t_int () ^-> t_option (t_pair (t_int ()) (t_nat ())));
                      typer_table_of_ligo_type O.(t_int () ^-> t_nat () ^-> t_option (t_pair (t_int ()) (t_nat ())));
                      typer_table_of_ligo_type O.(t_mutez () ^-> t_mutez () ^-> t_option (t_pair (t_nat ()) (t_mutez ())));
                      typer_table_of_ligo_type O.(t_mutez () ^-> t_nat () ^-> t_option (t_pair (t_mutez ()) (t_mutez ())));
                    ]
end

let external_typers ~raise ~options loc s =
  let typer =
    match s with
    | "int" ->
       Constant_types.int_typer
    | "ediv" ->
       Constant_types.ediv_typer
    | "u_ediv" ->
       Constant_types.ediv_typer
    | _ ->
       raise.error (corner_case @@ Format.asprintf "Typer not implemented for external %s" s) in
  fun lst tv_opt ->
  let error = ref [] in
  (match typer ~error ~raise ~options ~loc lst tv_opt with
   | Some (tv, table, ot) -> (tv, table, ot)
   | None -> raise.error (corner_case @@ Format.asprintf "Cannot type external %s" s))

let constant_typers ~raise ~options loc c =
  match CTMap.find_opt c Constant_types.tbl with
  | Some typer ->
     fun lst tv_opt ->
     let error = ref [] in
     (match typer ~error ~raise ~options ~loc lst tv_opt with
      | Some tv -> tv
      | None -> raise.error (corner_case @@ Format.asprintf "Cannot type constant %a" PP.constant' c))
  | _ ->
     raise.error (corner_case @@ Format.asprintf "Typer not implemented for constant %a" PP.constant' c)
