module Ligo_proto = Environment.Protocols
module Option = Simple_utils.Option
module List = Simple_utils.List
open Errors
module I = Ast_core
module O = Ast_typed
open Ligo_prim

(*
  Each constant has its own type.

  LIGO's type-system is currently too weak to express the constant's type.
  For instance:
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

module C = Computation
module E = Elaboration

module Comparable = struct
  type ('err, 'wrn) t = Type.t -> Type.t -> (Type.t, 'err, 'wrn) C.t

  (* let trace_compare ~raise ~loc a b ~in_ =
    Trace.try_with
      (fun ~raise ~catch:_ -> in_ ~raise)
      (fun ~catch:_ _ -> raise.error (uncomparable_types loc a b)) *)

  let try_compare type1 type2 ~in_:t =
    let open C in
    try_ t ~with_:(fun _ -> raise (uncomparable_types type1 type2))


  (* [simple_comparator ~raise loc name] checks
     returns type [bool] if argument types are of the form [ t1; t2 ]
     where [t1 = t2] and [t1, t2] in [ address ; bool ; bytes ; chain_id; int; key; key_hash; mutez; nat ; ... ].
  *)
  let simple_comparator : (_, _) t =
    let open C in
    let open Let_syntax in
    let simple_types =
      Type.
        [ t_address
        ; t_bool
        ; t_bytes
        ; t_chain_id
        ; t_int
        ; t_key
        ; t_key_hash
        ; t_mutez
        ; t_nat
        ; t_signature
        ; t_string
        ; t_timestamp
        ; t_unit
        ; t_never
        ; t_michelson_code
        ; t_int64
        ]
    in
    fun type1 type2 ->
      try_compare
        type1
        type2
        ~in_:
          (let%bind () =
             try_all
             @@ List.map simple_types ~f:(fun type_ ->
                    let%bind type_ = create_type type_ in
                    let%bind () = unify type1 type_ in
                    unify type2 type_)
           in
           create_type @@ Type.t_bool)


  (* [record_comparator] is a simple extension of [comparator] to record types. *)
  let rec record_comparator : (_, _) t =
    let open C in
    let open Let_syntax in
    fun type1 type2 ->
      let%bind () = try_compare type1 type2 ~in_:(unify type1 type2) in
      let%bind row1 =
        raise_opt ~error:(comparator_composed type1) @@ Type.get_t_record type1
      in
      let%bind row2 =
        raise_opt ~error:(comparator_composed type2) @@ Type.get_t_record type2
      in
      let%bind _ =
        List.map2_exn
          (Map.data row1.fields)
          (Map.data row2.fields)
          ~f:(fun row_elem1 row_elem2 -> comparator row_elem1 row_elem2)
        |> all
      in
      create_type @@ Type.t_bool


  and sum_comparator : (_, _) t =
    let open C in
    let open Let_syntax in
    fun type1 type2 ->
      let%bind () = try_compare type1 type2 ~in_:(unify type1 type2) in
      let%bind row1 =
        raise_opt ~error:(comparator_composed type1) @@ Type.get_t_sum type1
      in
      let%bind row2 =
        raise_opt ~error:(comparator_composed type2) @@ Type.get_t_sum type2
      in
      let%bind _ =
        List.map2_exn
          (Map.data row1.fields)
          (Map.data row2.fields)
          ~f:(fun row_elem1 row_elem2 -> comparator row_elem1 row_elem2)
        |> all
      in
      create_type @@ Type.t_bool


  and list_comparator : (_, _) t =
    let open C in
    let open Let_syntax in
    fun type1 type2 ->
      let%bind () = try_compare type1 type2 ~in_:(unify type1 type2) in
      let%bind elt_type1 =
        raise_opt ~error:(comparator_composed type1) @@ Type.get_t_list type1
      in
      let%bind elt_type2 =
        raise_opt ~error:(comparator_composed type2) @@ Type.get_t_list type2
      in
      comparator elt_type1 elt_type2


  and set_comparator : (_, _) t =
    let open C in
    let open Let_syntax in
    fun type1 type2 ->
      let%bind () = try_compare type1 type2 ~in_:(unify type1 type2) in
      let%bind elt_type1 =
        raise_opt ~error:(comparator_composed type1) @@ Type.get_t_set type1
      in
      let%bind elt_type2 =
        raise_opt ~error:(comparator_composed type2) @@ Type.get_t_set type2
      in
      comparator elt_type1 elt_type2


  and map_comparator : (_, _) t =
    let open C in
    let open Let_syntax in
    fun type1 type2 ->
      let%bind () = try_compare type1 type2 ~in_:(unify type1 type2) in
      let%bind key_type1, val_type1 =
        raise_opt ~error:(comparator_composed type1) @@ Type.get_t_map type1
      in
      let%bind key_type2, val_type2 =
        raise_opt ~error:(comparator_composed type1) @@ Type.get_t_map type2
      in
      let%bind _ = comparator key_type1 key_type2 in
      comparator val_type1 val_type2


  and big_map_comparator : (_, _) t =
    let open C in
    let open Let_syntax in
    fun type1 type2 ->
      let%bind () = try_compare type1 type2 ~in_:(unify type1 type2) in
      let%bind key_type1, val_type1 =
        raise_opt ~error:(comparator_composed type1) @@ Type.get_t_big_map type1
      in
      let%bind key_type2, val_type2 =
        raise_opt ~error:(comparator_composed type1) @@ Type.get_t_big_map type2
      in
      let%bind _ = comparator key_type1 key_type2 in
      comparator val_type1 val_type2


  and comparator : (_, _) t =
    let open C in
    let open Let_syntax in
    fun type1 type2 ->
      if%bind Options.test ()
      then
        try_all
          [ list_comparator type1 type2
          ; set_comparator type1 type2
          ; map_comparator type1 type2
          ; simple_comparator type1 type2
          ; record_comparator type1 type2
          ; sum_comparator type1 type2
          ; big_map_comparator type1 type2
          ]
      else
        try_all
          [ simple_comparator type1 type2
          ; record_comparator type1 type2
          ; sum_comparator type1 type2
          ]
end

module Annot = struct
  type mode =
    | Inferred
    | Checked

  type type_ =
    { for_alls : (Type_var.t * Kind.t) list
    ; arg_types : Type.t list
    ; ret_type : Type.t
    }

  type t =
    { mode_annot : mode list
    ; types : type_ List.Ne.t
    }

  module Syntax = struct
    let loc = Location.env

    let for_all tvar ?(kind = (Type : Kind.t)) in_ =
      let tvar = Type_var.of_input_var ~loc ("'" ^ tvar) in
      let result = in_ (Type.t_variable ~loc tvar ()) in
      let types =
        List.Ne.map
          (fun type_ -> { type_ with for_alls = (tvar, kind) :: type_.for_alls })
          result.types
      in
      { result with types }


    let t_for_all tvar ?(kind = (Type : Kind.t)) in_ =
      let tvar = Type_var.of_input_var ~loc ("'" ^ tvar) in
      let result = in_ (Type.t_variable ~loc tvar ()) in
      { result with for_alls = (tvar, kind) :: result.for_alls }


    let create ~mode_annot ~types = { mode_annot; types = List.Ne.of_list types }
    let return ret_type = { for_alls = []; arg_types = []; ret_type }
    let ( ^~> ) arg_type ret_type = { for_alls = []; arg_types = [ arg_type ]; ret_type }
    let ( ^-> ) arg_type type_ = { type_ with arg_types = arg_type :: type_.arg_types }
    let ( @-> ) t1 t2 = Type.t_arrow ~loc t1 t2 ()
  end
end

type ('err, 'wrn) infer = I.expression -> (Type.t * O.expression E.t, 'err, 'wrn) C.t
type ('err, 'wrn) check = I.expression -> Type.t -> (O.expression E.t, 'err, 'wrn) C.t

type ('err, 'wrn) t =
  infer:('err, 'wrn) infer
  -> check:('err, 'wrn) check
  -> I.expression list
  -> (Type.t * O.expression list E.t, 'err, 'wrn) C.t

let of_type ({ mode_annot; types } : Annot.t) : _ t =
  let open C in
  let open Let_syntax in
  (* The function [mode] is used to determine the mode of an argument at position [i] *)
  let mode =
    let table = Hashtbl.create (module Int) in
    List.iteri mode_annot ~f:(fun i mode -> Hashtbl.set table ~key:i ~data:mode);
    fun i -> raise_opt ~error:(corner_case "bad mode annot") @@ Hashtbl.find table i
  in
  fun ~infer ~check args ->
    (* Instantiate prenex quantifier *)
    let inst { Annot.for_alls; arg_types; ret_type } : (Type.t list * Type.t, _, _) C.t =
      let%bind subst =
        for_alls
        |> List.map ~f:(fun (tvar, kind) ->
               let%bind texists = exists kind in
               return (tvar, texists))
        |> all
      in
      let apply_subst type_ =
        List.fold_right subst ~init:type_ ~f:(fun (tvar, texists) type_ ->
            Type.subst type_ ~tvar ~type_:texists)
      in
      let arg_types = List.map arg_types ~f:apply_subst in
      let ret_type = apply_subst ret_type in
      return (arg_types, ret_type)
    in
    (* Determine arguments to be inferred *)
    let%bind inferred =
      (* Problem: Monadic DSL restricts available list functions (e.g. List.filter_mapi) *)
      List.foldi args ~init:(return []) ~f:(fun i inferred arg ->
          match%bind mode i with
          | Annot.Inferred ->
            let%bind inferred = inferred in
            return ((i, arg) :: inferred)
          | Checked -> inferred)
      >>| List.rev
    in
    (* [output_args] table is used to store elaborated arguments. Later used to sort arguments by position. *)
    let output_args = Hashtbl.create (module Int) in
    (* Infer the inferred arguments *)
    let%bind () =
      inferred
      |> List.map ~f:(fun (i, arg) ->
             let%bind arg_type, arg = infer arg in
             Hashtbl.set output_args ~key:i ~data:(arg_type, arg);
             return ())
      |> all_unit
    in
    (* Select type using try-based unification on inferred types *)
    let%bind checked, ret_type =
      try_all
      @@ List.map (List.Ne.to_list types) ~f:(fun type_ ->
             (* Instantiate *)
             let%bind arg_types, ret_type = inst type_ in
             (* Split types accordingly for [arg_types] *)
             let%bind args_types =
               match List.zip arg_types args with
               | Ok result -> return result
               | Unequal_lengths ->
                 raise
                   (corner_case
                      "Unequal lengths between mode annotation and argument types")
             in
             let%bind unify_worklist, checked =
               List.foldi
                 args_types
                 ~init:(return ([], []))
                 ~f:(fun i result (arg_type, arg) ->
                   let%bind unify_worklist, checked = result in
                   match%bind mode i with
                   | Annot.Inferred ->
                     return
                       ( (i, arg_type, fst @@ Hashtbl.find_exn output_args i)
                         :: unify_worklist
                       , checked )
                   | Checked -> return (unify_worklist, (i, arg_type, arg) :: checked))
             in
             (* Unify the inferred types, using subtypes *)
             let%bind update_worklist =
               unify_worklist
               (* Reverse due to [foldi] above *)
               |> List.rev_map ~f:(fun (i, arg_type1, arg_type2) ->
                      let%bind arg_type1 = Context.tapply arg_type1 in
                      let%bind arg_type2 = Context.tapply arg_type2 in
                      let%bind f = subtype ~expected:arg_type1 ~received:arg_type2 in
                      let _inferred, expr = Hashtbl.find_exn output_args i in
                      return (i, arg_type1, f, expr))
               |> all
             in
             (* Apply the coercion on subtypes *)
             let () =
               update_worklist
               |> List.iter ~f:(fun (i, type_, f, expr) ->
                      let expr =
                        E.(
                          let%bind expr = expr in
                          f expr)
                      in
                      Hashtbl.set output_args ~key:i ~data:(type_, expr))
             in
             return (checked, ret_type))
    in
    (* Check the checked arguments *)
    let%bind () =
      checked
      (* Reverse due to [folid] above *)
      |> List.rev_map ~f:(fun (i, arg_type, arg) ->
             let%bind arg = Context.tapply arg_type >>= check arg in
             Hashtbl.set output_args ~key:i ~data:(arg_type, arg);
             return ())
      |> all_unit
    in
    (* Reconstruct arguments using [output_args] *)
    let args =
      List.mapi args ~f:(fun i _ -> snd (Hashtbl.find_exn output_args i))
      |> Elaboration.all
    in
    return (ret_type, args)


let of_comparator comparator : _ t =
  let open C in
  let open Let_syntax in
  fun ~infer ~check:_ args ->
    match args with
    | [ arg1; arg2 ] ->
      let%bind arg1_type, arg1 = infer arg1 in
      let%bind arg2_type, arg2 = infer arg2 in
      let%bind ret_type =
        let%bind arg1_type = Context.tapply arg1_type in
        let%bind arg2_type = Context.tapply arg2_type in
        comparator arg1_type arg2_type
      in
      return (ret_type, E.(all [ arg1; arg2 ]))
    | _ ->
      raise
        (corner_case
        @@ Format.asprintf
             "Unequal length between comparator arguments and applied arguments")


module Const_map = Simple_utils.Map.Make (struct
  type t = Constant.constant'

  let compare x y = Constant.compare_constant' x y
end)

let constant_typer_tbl : (Errors.typer_error, Main_warnings.all) t Const_map.t =
  let open Type in
  let open Annot.Syntax in
  Const_map.of_list
    [ (* Loops *)
      ( C_LOOP_LEFT
      , of_type
          (for_all "a"
          @@ fun a ->
          for_all "b"
          @@ fun b ->
          create
            ~mode_annot:[ Checked; Inferred ]
            ~types:[ (a @-> t_sum_ez [ "left", a; "right", b ] ~loc ()) ^-> a ^~> b ]) )
    ; ( C_LEFT
      , of_type
          (for_all "a"
          @@ fun a ->
          create
            ~mode_annot:[ Inferred ]
            ~types:[ a ^~> t_sum_ez [ "left", a; "right", a ] ~loc () ]) )
    ; ( C_LOOP_CONTINUE
      , of_type
          (for_all "a"
          @@ fun a ->
          create
            ~mode_annot:[ Inferred ]
            ~types:[ a ^~> t_sum_ez [ "left", a; "right", a ] ~loc () ]) )
    ; ( C_LOOP_STOP
      , of_type
          (for_all "a"
          @@ fun a ->
          create
            ~mode_annot:[ Inferred ]
            ~types:[ a ^~> t_sum_ez [ "left", a; "right", a ] ~loc () ]) )
    ; ( C_ITER
      , of_type
          (create
             ~mode_annot:[ Checked; Inferred ]
             ~types:
               [ (t_for_all "a"
                 @@ fun a ->
                 (a @-> t_unit ~loc ()) ^-> t_list a ~loc () ^~> t_unit ~loc ())
               ; (t_for_all "a"
                 @@ fun a -> (a @-> t_unit ~loc ()) ^-> t_set a ~loc () ^~> t_unit ~loc ()
                 )
               ; (t_for_all "a"
                 @@ fun a ->
                 t_for_all "b"
                 @@ fun b ->
                 (t_pair a b ~loc () @-> t_unit ~loc ())
                 ^-> t_map a b ~loc ()
                 ^~> t_unit ~loc ())
               ]) )
    ; ( C_FOLD
      , of_type
          (for_all "a"
          @@ fun a ->
          for_all "b"
          @@ fun b ->
          create
            ~mode_annot:[ Checked; Inferred; Inferred ]
            ~types:
              [ (t_pair a b ~loc () @-> a) ^-> t_list b ~loc () ^-> a ^~> a
              ; (t_pair a b ~loc () @-> a) ^-> t_set b ~loc () ^-> a ^~> a
              ]) )
      (* Map *)
    ; ( C_MAP_EMPTY
      , of_type
          (for_all "a"
          @@ fun a ->
          for_all "b"
          @@ fun b -> create ~mode_annot:[] ~types:[ return (t_map a b ~loc ()) ]) )
    ; ( C_BIG_MAP_EMPTY
      , of_type
          (for_all "a"
          @@ fun a ->
          for_all "b"
          @@ fun b -> create ~mode_annot:[] ~types:[ return (t_big_map a b ~loc ()) ]) )
    ; ( C_MAP_ADD
      , of_type
          (for_all "a"
          @@ fun a ->
          for_all "b"
          @@ fun b ->
          create
            ~mode_annot:[ Checked; Checked; Inferred ]
            ~types:
              [ a ^-> b ^-> t_map a b ~loc () ^~> t_map a b ~loc ()
              ; a ^-> b ^-> t_big_map a b ~loc () ^~> t_big_map a b ~loc ()
              ]) )
    ; ( C_MAP_REMOVE
      , of_type
          (for_all "a"
          @@ fun a ->
          for_all "b"
          @@ fun b ->
          create
            ~mode_annot:[ Checked; Inferred ]
            ~types:
              [ a ^-> t_map a b ~loc () ^~> t_map a b ~loc ()
              ; a ^-> t_big_map a b ~loc () ^~> t_big_map a b ~loc ()
              ]) )
    ; ( C_MAP_UPDATE
      , of_type
          (for_all "a"
          @@ fun a ->
          for_all "b"
          @@ fun b ->
          create
            ~mode_annot:[ Checked; Checked; Inferred ]
            ~types:
              [ a ^-> t_option b ~loc () ^-> t_map a b ~loc () ^~> t_map a b ~loc ()
              ; a
                ^-> t_option b ~loc ()
                ^-> t_big_map a b ~loc ()
                ^~> t_big_map a b ~loc ()
              ]) )
    ; ( C_MAP_GET_AND_UPDATE
      , of_type
          (for_all "a"
          @@ fun a ->
          for_all "b"
          @@ fun b ->
          create
            ~mode_annot:[ Checked; Checked; Inferred ]
            ~types:
              [ a
                ^-> t_option b ~loc ()
                ^-> t_map a b ~loc ()
                ^~> t_pair (t_option b ~loc ()) (t_map a b ~loc ()) ~loc ()
              ]) )
    ; ( C_BIG_MAP_GET_AND_UPDATE
      , of_type
          (for_all "a"
          @@ fun a ->
          for_all "b"
          @@ fun b ->
          create
            ~mode_annot:[ Checked; Checked; Inferred ]
            ~types:
              [ a
                ^-> t_option b ~loc ()
                ^-> t_big_map a b ~loc ()
                ^~> t_pair (t_option b ~loc ()) (t_big_map a b ~loc ()) ~loc ()
              ]) )
    ; ( C_MAP_FIND_OPT
      , of_type
          (for_all "a"
          @@ fun a ->
          for_all "b"
          @@ fun b ->
          create
            ~mode_annot:[ Checked; Inferred ]
            ~types:
              [ a ^-> t_map a b ~loc () ^~> t_option b ~loc ()
              ; a ^-> t_big_map a b ~loc () ^~> t_option b ~loc ()
              ]) )
    ; ( C_MAP_FIND
      , of_type
          (for_all "a"
          @@ fun a ->
          for_all "b"
          @@ fun b ->
          create
            ~mode_annot:[ Checked; Inferred ]
            ~types:[ a ^-> t_map a b ~loc () ^~> b; a ^-> t_big_map a b ~loc () ^~> b ]) )
    ; ( C_MAP_MAP
      , of_type
          (for_all "a"
          @@ fun a ->
          for_all "b"
          @@ fun b ->
          for_all "c"
          @@ fun c ->
          create
            ~mode_annot:[ Checked; Inferred ]
            ~types:
              [ (t_pair a b ~loc () @-> c) ^-> t_map a b ~loc () ^~> t_map a c ~loc () ])
      )
    ; ( C_MAP_ITER
      , of_type
          (for_all "a"
          @@ fun a ->
          for_all "b"
          @@ fun b ->
          create
            ~mode_annot:[ Checked; Inferred ]
            ~types:
              [ (t_pair a b ~loc () @-> t_unit ~loc ())
                ^-> t_map a b ~loc ()
                ^~> t_unit ~loc ()
              ]) )
    ; ( C_MAP_FOLD
      , of_type
          (for_all "a"
          @@ fun a ->
          for_all "b"
          @@ fun b ->
          for_all "c"
          @@ fun c ->
          create
            ~mode_annot:[ Checked; Inferred; Inferred ]
            ~types:
              [ (t_pair c (t_pair a b ~loc ()) ~loc () @-> c)
                ^-> t_map a b ~loc ()
                ^-> c
                ^~> c
              ]) )
      (* List *)
    ; ( C_LIST_EMPTY
      , of_type
          (for_all "a"
          @@ fun a -> create ~mode_annot:[] ~types:[ return @@ t_list a ~loc () ]) )
    ; ( C_CONS
      , of_type
          (for_all "a"
          @@ fun a ->
          create
            ~mode_annot:[ Inferred; Checked ]
            ~types:[ a ^-> t_list a ~loc () ^~> t_list a ~loc () ]) )
    ; ( C_LIST_MAP
      , of_type
          (for_all "a"
          @@ fun a ->
          for_all "b"
          @@ fun b ->
          create
            ~mode_annot:[ Checked; Inferred ]
            ~types:[ (a @-> b) ^-> t_list a ~loc () ^~> t_list b ~loc () ]) )
    ; ( C_LIST_ITER
      , of_type
          (for_all "a"
          @@ fun a ->
          create
            ~mode_annot:[ Checked; Inferred ]
            ~types:[ (a @-> t_unit ~loc ()) ^-> t_list a ~loc () ^~> t_unit ~loc () ]) )
    ; ( C_LIST_FOLD
      , of_type
          (for_all "a"
          @@ fun a ->
          for_all "b"
          @@ fun b ->
          create
            ~mode_annot:[ Checked; Inferred; Inferred ]
            ~types:[ (t_pair b a ~loc () @-> b) ^-> t_list a ~loc () ^-> b ^~> b ]) )
    ; ( C_LIST_FOLD_LEFT
      , of_type
          (for_all "a"
          @@ fun a ->
          for_all "b"
          @@ fun b ->
          create
            ~mode_annot:[ Checked; Inferred; Inferred ]
            ~types:[ (t_pair b a ~loc () @-> b) ^-> b ^-> t_list a ~loc () ^~> b ]) )
    ; ( C_LIST_FOLD_RIGHT
      , of_type
          (for_all "a"
          @@ fun a ->
          for_all "b"
          @@ fun b ->
          create
            ~mode_annot:[ Checked; Inferred; Inferred ]
            ~types:[ (t_pair a b ~loc () @-> b) ^-> t_list a ~loc () ^-> b ^~> b ]) )
      (* Set *)
    ; ( C_SET_EMPTY
      , of_type
          (for_all "a"
          @@ fun a -> create ~mode_annot:[] ~types:[ return @@ t_set a ~loc () ]) )
    ; ( C_SET_LITERAL
      , of_type
          (for_all "a"
          @@ fun a ->
          create ~mode_annot:[ Inferred ] ~types:[ t_list a ~loc () ^~> t_set a ~loc () ]
          ) )
    ; ( C_SET_MEM
      , of_type
          (for_all "a"
          @@ fun a ->
          create
            ~mode_annot:[ Inferred; Checked ]
            ~types:[ a ^-> t_set a ~loc () ^~> t_bool ~loc () ]) )
    ; ( C_SET_ADD
      , of_type
          (for_all "a"
          @@ fun a ->
          create
            ~mode_annot:[ Inferred; Checked ]
            ~types:[ a ^-> t_set a ~loc () ^~> t_set a ~loc () ]) )
    ; ( C_SET_REMOVE
      , of_type
          (for_all "a"
          @@ fun a ->
          create
            ~mode_annot:[ Inferred; Checked ]
            ~types:[ a ^-> t_set a ~loc () ^~> t_set a ~loc () ]) )
    ; ( C_SET_UPDATE
      , of_type
          (for_all "a"
          @@ fun a ->
          create
            ~mode_annot:[ Inferred; Checked; Checked ]
            ~types:[ a ^-> t_bool ~loc () ^-> t_set a ~loc () ^~> t_set a ~loc () ]) )
    ; ( C_SET_ITER
      , of_type
          (for_all "a"
          @@ fun a ->
          create
            ~mode_annot:[ Checked; Inferred ]
            ~types:[ (a @-> t_unit ~loc ()) ^-> t_set a ~loc () ^~> t_unit ~loc () ]) )
    ; ( C_SET_FOLD
      , of_type
          (for_all "a"
          @@ fun a ->
          for_all "b"
          @@ fun b ->
          create
            ~mode_annot:[ Checked; Inferred; Inferred ]
            ~types:[ (t_pair b a ~loc () @-> b) ^-> t_set a ~loc () ^-> b ^~> b ]) )
    ; ( C_SET_FOLD_DESC
      , of_type
          (for_all "a"
          @@ fun a ->
          for_all "b"
          @@ fun b ->
          create
            ~mode_annot:[ Checked; Inferred; Inferred ]
            ~types:[ (t_pair a b ~loc () @-> b) ^-> t_set a ~loc () ^-> b ^~> b ]) )
      (* Bytes *)
    ; ( C_CONCAT
      , of_type
          (create
             ~mode_annot:[ Inferred; Inferred ]
             ~types:
               [ t_string ~loc () ^-> t_string ~loc () ^~> t_string ~loc ()
               ; t_bytes ~loc () ^-> t_bytes ~loc () ^~> t_bytes ~loc ()
               ]) )
    ; ( C_CONCATS
      , of_type
          (create
             ~mode_annot:[ Inferred ]
             ~types:
               [ t_list ~loc (t_string ~loc ()) () ^~> t_string ~loc ()
               ; t_list ~loc (t_bytes ~loc ()) () ^~> t_bytes ~loc ()
               ]) )
      (* Option *)
    ; ( C_NONE
      , of_type
          (for_all "a"
          @@ fun a -> create ~mode_annot:[] ~types:[ return @@ t_option a ~loc () ]) )
    ; ( C_SOME
      , of_type
          (for_all "a"
          @@ fun a -> create ~mode_annot:[ Inferred ] ~types:[ a ^~> t_option a ~loc () ]
          ) )
    ; ( C_OPTION_MAP
      , of_type
          (for_all "a"
          @@ fun a ->
          for_all "b"
          @@ fun b ->
          create
            ~mode_annot:[ Checked; Inferred ]
            ~types:[ (a @-> b) ^-> t_option a ~loc () ^~> t_option b ~loc () ]) )
    ; ( C_CHECK_ENTRYPOINT
      , of_type
          (create ~mode_annot:[ Checked ] ~types:[ t_string ~loc () ^~> t_unit ~loc () ])
      )
    ; ( C_CHECK_CALL_VIEW_LITSTR
      , of_type
          (create ~mode_annot:[ Checked ] ~types:[ t_string ~loc () ^~> t_unit ~loc () ])
      )
    ; ( C_CHECK_SELF
      , of_type
          (for_all "a"
          @@ fun a ->
          create
            ~mode_annot:[ Checked ]
            ~types:[ t_string ~loc () ^~> t_option a ~loc () ]) )
    ; ( C_CREATE_CONTRACT
      , of_type
          (for_all "a"
          @@ fun a ->
          for_all "b"
          @@ fun b ->
          create
            ~mode_annot:[ Checked; Checked; Checked; Inferred ]
            ~types:
              [ (t_pair a b ~loc ()
                @-> t_pair (t_list (t_operation ~loc ()) ~loc ()) b ~loc ())
                ^-> t_option (t_key_hash ~loc ()) ~loc ()
                ^-> t_mutez ~loc ()
                ^-> b
                ^~> t_pair (t_operation ~loc ()) (t_address ~loc ()) ~loc ()
              ]) )
    ; ( C_CHECK_EMIT_EVENT
      , of_type
          (for_all "a"
          @@ fun a ->
          create
            ~mode_annot:[ Checked; Inferred ]
            ~types:[ t_string ~loc () ^-> a ^~> t_unit ~loc () ]) )
      (* Primitives *)
    ; C_UNIT, of_type (create ~mode_annot:[] ~types:[ return @@ t_unit ~loc () ])
    ; C_TRUE, of_type (create ~mode_annot:[] ~types:[ return @@ t_bool ~loc () ])
    ; C_FALSE, of_type (create ~mode_annot:[] ~types:[ return @@ t_bool ~loc () ])
    ; ( C_POLYMORPHIC_ADD
      , of_type
          (create
             ~mode_annot:[ Inferred; Inferred ]
             ~types:
               [ t_int ~loc () ^-> t_int ~loc () ^~> t_int ~loc ()
               ; t_string ~loc () ^-> t_string ~loc () ^~> t_string ~loc ()
               ; t_nat ~loc () ^-> t_nat ~loc () ^~> t_nat ~loc ()
               ; t_mutez ~loc () ^-> t_mutez ~loc () ^~> t_mutez ~loc ()
               ; t_int ~loc () ^-> t_nat ~loc () ^~> t_int ~loc ()
               ; t_nat ~loc () ^-> t_int ~loc () ^~> t_int ~loc ()
               ; t_timestamp ~loc () ^-> t_int ~loc () ^~> t_timestamp ~loc ()
               ; t_int ~loc () ^-> t_timestamp ~loc () ^~> t_timestamp ~loc ()
               ; t_int64 ~loc () ^-> t_int64 ~loc () ^~> t_int64 ~loc ()
               ; t_bls12_381_g1 ~loc ()
                 ^-> t_bls12_381_g1 ~loc ()
                 ^~> t_bls12_381_g1 ~loc ()
               ; t_bls12_381_g2 ~loc ()
                 ^-> t_bls12_381_g2 ~loc ()
                 ^~> t_bls12_381_g2 ~loc ()
               ; t_bls12_381_fr ~loc ()
                 ^-> t_bls12_381_fr ~loc ()
                 ^~> t_bls12_381_fr ~loc ()
               ]) )
    ; ( C_POLYMORPHIC_SUB
      , of_type
          (create
             ~mode_annot:[ Inferred; Inferred ]
             ~types:
               [ t_int ~loc () ^-> t_int ~loc () ^~> t_int ~loc ()
               ; t_nat ~loc () ^-> t_nat ~loc () ^~> t_int ~loc ()
               ; t_nat ~loc () ^-> t_int ~loc () ^~> t_int ~loc ()
               ; t_int ~loc () ^-> t_nat ~loc () ^~> t_int ~loc ()
               ; t_timestamp ~loc () ^-> t_timestamp ~loc () ^~> t_int ~loc ()
               ; t_timestamp ~loc () ^-> t_int ~loc () ^~> t_timestamp ~loc ()
               ; t_mutez ~loc ()
                 ^-> t_mutez ~loc ()
                 ^~> t_option (t_mutez ~loc ()) ~loc ()
               ; t_int64 ~loc () ^-> t_int64 ~loc () ^~> t_int64 ~loc ()
               ; t_bls12_381_g1 ~loc ()
                 ^-> t_bls12_381_g1 ~loc ()
                 ^~> t_bls12_381_g1 ~loc ()
               ; t_bls12_381_g2 ~loc ()
                 ^-> t_bls12_381_g2 ~loc ()
                 ^~> t_bls12_381_g2 ~loc ()
               ; t_bls12_381_fr ~loc ()
                 ^-> t_bls12_381_fr ~loc ()
                 ^~> t_bls12_381_fr ~loc ()
               ]) )
    ; ( C_ADD
      , of_type
          (create
             ~mode_annot:[ Inferred; Inferred ]
             ~types:
               [ t_int ~loc () ^-> t_int ~loc () ^~> t_int ~loc ()
               ; t_int ~loc () ^-> t_nat ~loc () ^~> t_int ~loc ()
               ; t_nat ~loc () ^-> t_nat ~loc () ^~> t_nat ~loc ()
               ; t_mutez ~loc () ^-> t_mutez ~loc () ^~> t_mutez ~loc ()
               ; t_nat ~loc () ^-> t_int ~loc () ^~> t_int ~loc ()
               ; t_timestamp ~loc () ^-> t_int ~loc () ^~> t_timestamp ~loc ()
               ; t_int ~loc () ^-> t_timestamp ~loc () ^~> t_timestamp ~loc ()
               ; t_int64 ~loc () ^-> t_int64 ~loc () ^~> t_int64 ~loc ()
               ; t_bls12_381_g1 ~loc ()
                 ^-> t_bls12_381_g1 ~loc ()
                 ^~> t_bls12_381_g1 ~loc ()
               ; t_bls12_381_g2 ~loc ()
                 ^-> t_bls12_381_g2 ~loc ()
                 ^~> t_bls12_381_g2 ~loc ()
               ; t_bls12_381_fr ~loc ()
                 ^-> t_bls12_381_fr ~loc ()
                 ^~> t_bls12_381_fr ~loc ()
               ]) )
    ; ( C_MUL
      , of_type
          (create
             ~mode_annot:[ Inferred; Inferred ]
             ~types:
               [ t_int ~loc () ^-> t_int ~loc () ^~> t_int ~loc ()
               ; t_nat ~loc () ^-> t_bls12_381_fr ~loc () ^~> t_bls12_381_fr ~loc ()
               ; t_int ~loc () ^-> t_bls12_381_fr ~loc () ^~> t_bls12_381_fr ~loc ()
               ; t_bls12_381_fr ~loc () ^-> t_nat ~loc () ^~> t_bls12_381_fr ~loc ()
               ; t_bls12_381_fr ~loc () ^-> t_int ~loc () ^~> t_bls12_381_fr ~loc ()
               ; t_nat ~loc () ^-> t_nat ~loc () ^~> t_nat ~loc ()
               ; t_nat ~loc () ^-> t_mutez ~loc () ^~> t_mutez ~loc ()
               ; t_mutez ~loc () ^-> t_nat ~loc () ^~> t_mutez ~loc ()
               ; t_int ~loc () ^-> t_nat ~loc () ^~> t_int ~loc ()
               ; t_nat ~loc () ^-> t_int ~loc () ^~> t_int ~loc ()
               ; t_int64 ~loc () ^-> t_int64 ~loc () ^~> t_int64 ~loc ()
               ; t_bls12_381_g1 ~loc ()
                 ^-> t_bls12_381_fr ~loc ()
                 ^~> t_bls12_381_g1 ~loc ()
               ; t_bls12_381_g2 ~loc ()
                 ^-> t_bls12_381_fr ~loc ()
                 ^~> t_bls12_381_g2 ~loc ()
               ; t_bls12_381_fr ~loc ()
                 ^-> t_bls12_381_fr ~loc ()
                 ^~> t_bls12_381_fr ~loc ()
               ]) )
    ; ( C_SUB
      , of_type
          (create
             ~mode_annot:[ Inferred; Inferred ]
             ~types:
               [ t_int ~loc () ^-> t_int ~loc () ^~> t_int ~loc ()
               ; t_bls12_381_g1 ~loc ()
                 ^-> t_bls12_381_fr ~loc ()
                 ^~> t_bls12_381_g1 ~loc ()
               ; t_bls12_381_g2 ~loc ()
                 ^-> t_bls12_381_fr ~loc ()
                 ^~> t_bls12_381_g2 ~loc ()
               ; t_bls12_381_fr ~loc ()
                 ^-> t_bls12_381_fr ~loc ()
                 ^~> t_bls12_381_fr ~loc ()
               ; t_nat ~loc () ^-> t_nat ~loc () ^~> t_int ~loc ()
               ; t_int ~loc () ^-> t_nat ~loc () ^~> t_int ~loc ()
               ; t_nat ~loc () ^-> t_int ~loc () ^~> t_int ~loc ()
               ; t_timestamp ~loc () ^-> t_timestamp ~loc () ^~> t_int ~loc ()
               ; t_timestamp ~loc () ^-> t_int ~loc () ^~> t_timestamp ~loc ()
               ; t_mutez ~loc () ^-> t_mutez ~loc () ^~> t_mutez ~loc ()
               ; t_int64 ~loc () ^-> t_int64 ~loc () ^~> t_int64 ~loc ()
               ]) )
    ; ( C_SUB_MUTEZ
      , of_type
          (create
             ~mode_annot:[ Checked; Checked ]
             ~types:
               [ t_mutez ~loc ()
                 ^-> t_mutez ~loc ()
                 ^~> t_option (t_mutez ~loc ()) ~loc ()
               ]) )
    ; ( C_DIV
      , of_type
          (create
             ~mode_annot:[ Inferred; Inferred ]
             ~types:
               [ t_int ~loc () ^-> t_int ~loc () ^~> t_int ~loc ()
               ; t_nat ~loc () ^-> t_nat ~loc () ^~> t_nat ~loc ()
               ; t_nat ~loc () ^-> t_int ~loc () ^~> t_int ~loc ()
               ; t_int ~loc () ^-> t_nat ~loc () ^~> t_int ~loc ()
               ; t_mutez ~loc () ^-> t_nat ~loc () ^~> t_mutez ~loc ()
               ; t_mutez ~loc () ^-> t_mutez ~loc () ^~> t_nat ~loc ()
               ; t_int64 ~loc () ^-> t_int64 ~loc () ^~> t_int64 ~loc ()
               ]) )
    ; ( C_MOD
      , of_type
          (create
             ~mode_annot:[ Inferred; Inferred ]
             ~types:
               [ t_int ~loc () ^-> t_int ~loc () ^~> t_nat ~loc ()
               ; t_nat ~loc () ^-> t_nat ~loc () ^~> t_nat ~loc ()
               ; t_nat ~loc () ^-> t_int ~loc () ^~> t_nat ~loc ()
               ; t_int ~loc () ^-> t_nat ~loc () ^~> t_nat ~loc ()
               ; t_mutez ~loc () ^-> t_nat ~loc () ^~> t_mutez ~loc ()
               ; t_mutez ~loc () ^-> t_mutez ~loc () ^~> t_mutez ~loc ()
               ; t_int64 ~loc () ^-> t_int64 ~loc () ^~> t_int64 ~loc ()
               ]) )
    ; ( C_NEG
      , of_type
          (create
             ~mode_annot:[ Inferred ]
             ~types:
               [ t_int ~loc () ^~> t_int ~loc ()
               ; t_nat ~loc () ^~> t_int ~loc ()
               ; t_bls12_381_g1 ~loc () ^~> t_bls12_381_g1 ~loc ()
               ; t_bls12_381_g2 ~loc () ^~> t_bls12_381_g2 ~loc ()
               ; t_bls12_381_fr ~loc () ^~> t_bls12_381_fr ~loc ()
               ]) )
      (* Logical operators *)
    ; ( C_NOT
      , of_type
          (create
             ~mode_annot:[ Inferred ]
             ~types:
               [ t_int ~loc () ^~> t_int ~loc ()
               ; t_nat ~loc () ^~> t_int ~loc ()
               ; t_bytes ~loc () ^~> t_bytes ~loc ()
               ; t_bool ~loc () ^~> t_bool ~loc ()
               ]) )
    ; ( C_AND
      , of_type
          (create
             ~mode_annot:[ Inferred; Inferred ]
             ~types:[ t_bool ~loc () ^-> t_bool ~loc () ^~> t_bool ~loc () ]) )
    ; ( C_OR
      , of_type
          (create
             ~mode_annot:[ Inferred; Inferred ]
             ~types:[ t_bool ~loc () ^-> t_bool ~loc () ^~> t_bool ~loc () ]) )
    ; ( C_XOR
      , of_type
          (create
             ~mode_annot:[ Inferred; Inferred ]
             ~types:[ t_bool ~loc () ^-> t_bool ~loc () ^~> t_bool ~loc () ]) )
    ; ( C_LAND
      , of_type
          (create
             ~mode_annot:[ Inferred; Inferred ]
             ~types:
               [ t_nat ~loc () ^-> t_nat ~loc () ^~> t_nat ~loc ()
               ; t_int ~loc () ^-> t_nat ~loc () ^~> t_nat ~loc ()
               ; t_int64 ~loc () ^-> t_int64 ~loc () ^~> t_int64 ~loc ()
               ; t_bytes ~loc () ^-> t_bytes ~loc () ^~> t_bytes ~loc ()
               ]) )
    ; ( C_LOR
      , of_type
          (create
             ~mode_annot:[ Inferred; Inferred ]
             ~types:
               [ t_nat ~loc () ^-> t_nat ~loc () ^~> t_nat ~loc ()
               ; t_int64 ~loc () ^-> t_int64 ~loc () ^~> t_int64 ~loc ()
               ; t_bytes ~loc () ^-> t_bytes ~loc () ^~> t_bytes ~loc ()
               ]) )
    ; ( C_LXOR
      , of_type
          (create
             ~mode_annot:[ Inferred; Inferred ]
             ~types:
               [ t_nat ~loc () ^-> t_nat ~loc () ^~> t_nat ~loc ()
               ; t_int64 ~loc () ^-> t_int64 ~loc () ^~> t_int64 ~loc ()
               ; t_bytes ~loc () ^-> t_bytes ~loc () ^~> t_bytes ~loc ()
               ]) )
    ; ( C_LSL
      , of_type
          (create
             ~mode_annot:[ Inferred; Inferred ]
             ~types:
               [ t_nat ~loc () ^-> t_nat ~loc () ^~> t_nat ~loc ()
               ; t_int64 ~loc () ^-> t_nat ~loc () ^~> t_int64 ~loc ()
               ; t_bytes ~loc () ^-> t_nat ~loc () ^~> t_bytes ~loc ()
               ]) )
    ; ( C_LSR
      , of_type
          (create
             ~mode_annot:[ Inferred; Inferred ]
             ~types:
               [ t_nat ~loc () ^-> t_nat ~loc () ^~> t_nat ~loc ()
               ; t_int64 ~loc () ^-> t_nat ~loc () ^~> t_int64 ~loc ()
               ; t_bytes ~loc () ^-> t_nat ~loc () ^~> t_bytes ~loc ()
               ]) )
      (* Tests *)
    ; ( C_TEST_ADDRESS
      , of_type
          (for_all "a"
          @@ fun a ->
          create
            ~mode_annot:[ Checked ]
            ~types:[ t_contract a ~loc () ^~> t_address ~loc () ]) )
    ; ( C_TEST_COMPILE_CONTRACT
      , of_type
          (for_all "a"
          @@ fun a ->
          for_all "b"
          @@ fun b ->
          create
            ~mode_annot:[ Inferred; Checked ]
            ~types:
              [ (t_pair a b ~loc ()
                @-> t_pair (t_list (t_operation ~loc ()) ~loc ()) b ~loc ())
                ^-> t_views ~loc b ()
                ^~> t_ast_contract ~loc ()
              ]) )
    ; ( C_TEST_COMPILE_AST_CONTRACT
      , of_type
          (create
             ~mode_annot:[ Checked ]
             ~types:[ t_ast_contract ~loc () ^~> t_michelson_contract ~loc () ]) )
    ; ( C_TEST_SIZE
      , of_type
          (create
             ~mode_annot:[ Checked ]
             ~types:[ t_michelson_contract ~loc () ^~> t_int ~loc () ]) )
    ; ( C_TEST_ORIGINATE
      , of_type
          (create
             ~mode_annot:[ Checked; Checked; Checked ]
             ~types:
               [ t_michelson_contract ~loc ()
                 ^-> t_michelson_code ~loc ()
                 ^-> t_mutez ~loc ()
                 ^~> t_address ~loc ()
               ]) )
    ; ( C_TEST_BOOTSTRAP_CONTRACT
      , of_type
          (for_all "a"
          @@ fun a ->
          for_all "b"
          @@ fun b ->
          create
            ~mode_annot:[ Checked; Inferred; Checked ]
            ~types:
              [ (t_pair a b ~loc ()
                @-> t_pair (t_list (t_operation ~loc ()) ~loc ()) b ~loc ())
                ^-> b
                ^-> t_mutez ~loc ()
                ^~> t_unit ~loc ()
              ]) )
    ; ( C_TEST_LAST_ORIGINATIONS
      , of_type
          (create
             ~mode_annot:[ Checked ]
             ~types:
               [ t_unit ~loc ()
                 ^~> t_map
                       (t_address ~loc ())
                       (t_list (t_address ~loc ()) ~loc ())
                       ~loc
                       ()
               ]) )
    ; ( C_TEST_LAST_EVENTS
      , of_type
          (for_all "a"
          @@ fun a ->
          create
            ~mode_annot:[ Checked ]
            ~types:
              [ t_string ~loc () ^~> t_list (t_pair (t_address ~loc ()) a ~loc ()) ~loc ()
              ]) )
    ; ( C_TEST_NTH_BOOTSTRAP_TYPED_ADDRESS
      , of_type
          (for_all "a"
          @@ fun a ->
          for_all "b"
          @@ fun b ->
          create
            ~mode_annot:[ Checked ]
            ~types:[ t_nat ~loc () ^~> t_typed_address a b ~loc () ]) )
    ; ( C_TEST_SET_SOURCE
      , of_type
          (create ~mode_annot:[ Checked ] ~types:[ t_address ~loc () ^~> t_unit ~loc () ])
      )
    ; ( C_TEST_SET_BAKER
      , of_type
          (create
             ~mode_annot:[ Checked ]
             ~types:[ t_test_baker_policy ~loc () ^~> t_unit ~loc () ]) )
    ; ( C_TEST_NTH_BOOTSTRAP_CONTRACT
      , of_type
          (create ~mode_annot:[ Checked ] ~types:[ t_nat ~loc () ^~> t_address ~loc () ])
      )
    ; ( C_TEST_GET_STORAGE_OF_ADDRESS
      , of_type
          (create
             ~mode_annot:[ Checked ]
             ~types:[ t_address ~loc () ^~> t_michelson_code ~loc () ]) )
    ; ( C_TEST_GET_BALANCE
      , of_type
          (create
             ~mode_annot:[ Checked ]
             ~types:[ t_address ~loc () ^~> t_mutez ~loc () ]) )
    ; ( C_TEST_GET_NTH_BS
      , of_type
          (create
             ~mode_annot:[ Checked ]
             ~types:
               [ t_int ~loc ()
                 ^~> t_triplet
                       (t_address ~loc ())
                       (t_key ~loc ())
                       (t_string ~loc ())
                       ~loc
                       ()
               ]) )
    ; ( C_TEST_PRINT
      , of_type
          (create
             ~mode_annot:[ Checked; Checked ]
             ~types:[ t_int ~loc () ^-> t_string ~loc () ^~> t_unit ~loc () ]) )
    ; ( C_TEST_TO_STRING
      , of_type
          (for_all "a"
          @@ fun a ->
          create
            ~mode_annot:[ Inferred; Checked ]
            ~types:[ a ^-> t_int ~loc () ^~> t_string ~loc () ]) )
    ; ( C_TEST_UNESCAPE_STRING
      , of_type
          (create
             ~mode_annot:[ Checked ]
             ~types:[ t_string ~loc () ^~> t_string ~loc () ]) )
    ; ( C_TEST_STATE_RESET
      , of_type
          (create
             ~mode_annot:[ Checked; Checked; Checked ]
             ~types:
               [ t_option (t_timestamp ~loc ()) ~loc ()
                 ^-> t_nat ~loc ()
                 ^-> t_list (t_mutez ~loc ()) ~loc ()
                 ^~> t_unit ~loc ()
               ]) )
    ; ( C_TEST_GET_VOTING_POWER
      , of_type
          (create ~mode_annot:[ Checked ] ~types:[ t_key_hash ~loc () ^~> t_nat ~loc () ])
      )
    ; ( C_TEST_GET_TOTAL_VOTING_POWER
      , of_type
          (create ~mode_annot:[ Checked ] ~types:[ t_unit ~loc () ^~> t_nat ~loc () ]) )
    ; ( C_TEST_CAST_ADDRESS
      , of_type
          (for_all "a"
          @@ fun a ->
          for_all "b"
          @@ fun b ->
          create
            ~mode_annot:[ Checked ]
            ~types:[ t_address ~loc () ^~> t_typed_address a b ~loc () ]) )
    ; ( C_TEST_RANDOM
      , of_type
          (for_all "a"
          @@ fun a ->
          create ~mode_annot:[ Checked ] ~types:[ t_bool ~loc () ^~> t_gen a ~loc () ]) )
    ; ( C_TEST_GENERATOR_EVAL
      , of_type
          (for_all "a"
          @@ fun a -> create ~mode_annot:[ Checked ] ~types:[ t_gen a ~loc () ^~> a ]) )
    ; ( C_TEST_MUTATE_CONTRACT
      , of_type
          (create
             ~mode_annot:[ Checked; Checked ]
             ~types:
               [ t_nat ~loc ()
                 ^-> t_ast_contract ~loc ()
                 ^~> t_option
                       (t_pair (t_ast_contract ~loc ()) (t_mutation ~loc ()) ~loc ())
                       ~loc
                       ()
               ]) )
    ; ( C_TEST_MUTATE_VALUE
      , of_type
          (for_all "a"
          @@ fun a ->
          create
            ~mode_annot:[ Checked; Inferred ]
            ~types:
              [ t_nat ~loc ()
                ^-> a
                ^~> t_option (t_pair a (t_mutation ~loc ()) ~loc ()) ~loc ()
              ]) )
    ; ( C_TEST_TRY_WITH
      , of_type
          (for_all "a"
          @@ fun a ->
          create
            ~mode_annot:[ Checked; Checked ]
            ~types:[ (t_unit ~loc () @-> a) ^-> (t_unit ~loc () @-> a) ^~> a ]) )
    ; ( C_TEST_SAVE_MUTATION
      , of_type
          (create
             ~mode_annot:[ Checked; Checked ]
             ~types:
               [ t_string ~loc ()
                 ^-> t_mutation ~loc ()
                 ^~> t_option (t_string ~loc ()) ~loc ()
               ]) )
    ; ( C_TEST_ADD_ACCOUNT
      , of_type
          (create
             ~mode_annot:[ Checked; Checked ]
             ~types:[ t_string ~loc () ^-> t_key ~loc () ^~> t_unit ~loc () ]) )
    ; ( C_TEST_NEW_ACCOUNT
      , of_type
          (create
             ~mode_annot:[ Checked ]
             ~types:
               [ t_unit ~loc () ^~> t_pair (t_string ~loc ()) (t_key ~loc ()) ~loc () ]) )
    ; ( C_TEST_RUN
      , of_type
          (for_all "a"
          @@ fun a ->
          for_all "b"
          @@ fun b ->
          create
            ~mode_annot:[ Checked; Inferred ]
            ~types:[ (a @-> b) ^-> a ^~> t_michelson_code ~loc () ]) )
    ; ( C_TEST_DECOMPILE
      , of_type
          (for_all "a"
          @@ fun a ->
          create ~mode_annot:[ Checked ] ~types:[ t_michelson_code ~loc () ^~> a ]) )
    ; ( C_TEST_TO_TYPED_ADDRESS
      , of_type
          (for_all "a"
          @@ fun a ->
          for_all "b"
          @@ fun b ->
          create
            ~mode_annot:[ Inferred ]
            ~types:[ t_contract a ~loc () ^~> t_typed_address a b ~loc () ]) )
    ; ( C_TEST_EXTERNAL_CALL_TO_ADDRESS
      , of_type
          (create
             ~mode_annot:[ Checked; Checked; Checked; Checked ]
             ~types:
               [ t_address ~loc ()
                 ^-> t_option (t_string ~loc ()) ~loc ()
                 ^-> t_michelson_code ~loc ()
                 ^-> t_mutez ~loc ()
                 ^~> t_test_exec_result ~loc ()
               ]) )
    ; ( C_TEST_EXTERNAL_CALL_TO_ADDRESS_EXN
      , of_type
          (create
             ~mode_annot:[ Checked; Checked; Checked; Checked ]
             ~types:
               [ t_address ~loc ()
                 ^-> t_option (t_string ~loc ()) ~loc ()
                 ^-> t_michelson_code ~loc ()
                 ^-> t_mutez ~loc ()
                 ^~> t_nat ~loc ()
               ]) )
    ; ( C_TEST_SET_BIG_MAP
      , of_type
          (for_all "a"
          @@ fun a ->
          for_all "b"
          @@ fun b ->
          create
            ~mode_annot:[ Checked; Inferred ]
            ~types:[ t_int ~loc () ^-> t_big_map a b ~loc () ^~> t_unit ~loc () ]) )
    ; ( C_TEST_BAKER_ACCOUNT
      , of_type
          (create
             ~mode_annot:[ Checked; Checked ]
             ~types:
               [ t_pair (t_string ~loc ()) (t_key ~loc ()) ~loc ()
                 ^-> t_option (t_mutez ~loc ()) ~loc ()
                 ^~> t_unit ~loc ()
               ]) )
    ; ( C_TEST_REGISTER_DELEGATE
      , of_type
          (create
             ~mode_annot:[ Checked ]
             ~types:[ t_key_hash ~loc () ^~> t_unit ~loc () ]) )
    ; ( C_TEST_BAKE_UNTIL_N_CYCLE_END
      , of_type
          (create ~mode_annot:[ Checked ] ~types:[ t_nat ~loc () ^~> t_unit ~loc () ]) )
    ; ( C_TEST_TO_CONTRACT
      , of_type
          (for_all "a"
          @@ fun a ->
          for_all "b"
          @@ fun b ->
          create
            ~mode_annot:[ Inferred ]
            ~types:[ t_typed_address a b ~loc () ^~> t_contract a ~loc () ]) )
    ; ( C_GLOBAL_CONSTANT
      , of_type
          (for_all "a"
          @@ fun a -> create ~mode_annot:[ Checked ] ~types:[ t_string ~loc () ^~> a ]) )
    ; ( C_TEST_COMPILE_CONTRACT_FROM_FILE
      , of_type
          (create
             ~mode_annot:[ Checked; Checked; Checked; Checked ]
             ~types:
               [ t_string ~loc ()
                 ^-> t_string ~loc ()
                 ^-> t_option (t_nat ~loc ()) ~loc ()
                 ^~> t_ast_contract ~loc ()
               ]) )
    ; ( C_TEST_REGISTER_CONSTANT
      , of_type
          (create
             ~mode_annot:[ Checked ]
             ~types:[ t_michelson_code ~loc () ^~> t_string ~loc () ]) )
    ; ( C_TEST_CONSTANT_TO_MICHELSON
      , of_type
          (create
             ~mode_annot:[ Checked ]
             ~types:[ t_string ~loc () ^~> t_michelson_code ~loc () ]) )
    ; ( C_TEST_REGISTER_FILE_CONSTANTS
      , of_type
          (create
             ~mode_annot:[ Checked ]
             ~types:[ t_string ~loc () ^~> t_list (t_string ~loc ()) ~loc () ]) )
    ; ( C_TEST_TO_ENTRYPOINT
      , of_type
          (for_all "a"
          @@ fun a ->
          for_all "b"
          @@ fun b ->
          for_all "c"
          @@ fun c ->
          create
            ~mode_annot:[ Checked; Checked ]
            ~types:
              [ t_string ~loc () ^-> t_typed_address a b ~loc () ^~> t_contract c ~loc ()
              ]) )
    ; ( C_TEST_FAILWITH
      , of_type
          (for_all "a"
          @@ fun a ->
          for_all "b" @@ fun b -> create ~mode_annot:[ Inferred ] ~types:[ a ^~> b ]) )
    ; ( C_TEST_PUSH_CONTEXT
      , of_type
          (create ~mode_annot:[ Checked ] ~types:[ t_unit ~loc () ^~> t_unit ~loc () ]) )
    ; ( C_TEST_POP_CONTEXT
      , of_type
          (create ~mode_annot:[ Checked ] ~types:[ t_unit ~loc () ^~> t_unit ~loc () ]) )
    ; ( C_TEST_DROP_CONTEXT
      , of_type
          (create ~mode_annot:[ Checked ] ~types:[ t_unit ~loc () ^~> t_unit ~loc () ]) )
    ; ( C_TEST_SET_PRINT_VALUES
      , of_type
          (create ~mode_annot:[ Checked ] ~types:[ t_bool ~loc () ^~> t_bool ~loc () ]) )
    ; ( C_TEST_READ_CONTRACT_FROM_FILE
      , of_type
          (create
             ~mode_annot:[ Checked ]
             ~types:[ t_string ~loc () ^~> t_michelson_contract ~loc () ]) )
    ; ( C_TEST_SIGN
      , of_type
          (create
             ~mode_annot:[ Checked; Checked ]
             ~types:[ t_string ~loc () ^-> t_bytes ~loc () ^~> t_signature ~loc () ]) )
    ; ( C_TEST_GET_ENTRYPOINT
      , of_type
          (for_all "a"
          @@ fun a ->
          create
            ~mode_annot:[ Checked ]
            ~types:[ t_contract a ~loc () ^~> t_option (t_string ~loc ()) ~loc () ]) )
    ; ( C_TEST_NIL_VIEWS
      , of_type
          (for_all "a"
          @@ fun a ->
          create ~mode_annot:[ Checked ] ~types:[ t_unit ~loc () ^~> t_views a ~loc () ])
      )
    ; ( C_TEST_CONS_VIEWS
      , of_type
          (for_all "a"
          @@ fun a ->
          for_all "b"
          @@ fun b ->
          for_all "c"
          @@ fun c ->
          create
            ~mode_annot:[ Inferred; Inferred; Checked ]
            ~types:
              [ a
                ^-> (t_pair ~loc b a () @-> c)
                ^-> t_views a ~loc ()
                ^~> t_views a ~loc ()
              ]) )
    ; C_EQ, of_comparator Comparable.comparator
    ; C_NEQ, of_comparator Comparable.comparator
    ; C_LT, of_comparator Comparable.comparator
    ; C_GT, of_comparator Comparable.comparator
    ; C_LE, of_comparator Comparable.comparator
    ; C_GE, of_comparator Comparable.comparator
    ; ( C_MAP_LITERAL
      , of_type
          (for_all "a"
          @@ fun a ->
          for_all "b"
          @@ fun b ->
          create
            ~mode_annot:[ Inferred ]
            ~types:[ t_list (t_pair a b ~loc ()) ~loc () ^~> t_map a b ~loc () ]) )
    ; ( C_BIG_MAP_LITERAL
      , of_type
          (for_all "a"
          @@ fun a ->
          for_all "b"
          @@ fun b ->
          create
            ~mode_annot:[ Inferred ]
            ~types:[ t_list (t_pair a b ~loc ()) ~loc () ^~> t_big_map a b ~loc () ]) )
    ; ( C_SET_LITERAL
      , of_type
          (for_all "a"
          @@ fun a ->
          create ~mode_annot:[ Inferred ] ~types:[ t_list a ~loc () ^~> t_set a ~loc () ]
          ) )
    ; ( C_TEST_INT64_OF_INT
      , of_type
          (create ~mode_annot:[ Checked ] ~types:[ t_int ~loc () ^~> t_int64 ~loc () ]) )
    ; ( C_TEST_INT64_TO_INT
      , of_type
          (create ~mode_annot:[ Checked ] ~types:[ t_int64 ~loc () ^~> t_int ~loc () ]) )
    ; ( C_INT
      , of_type
          (create ~mode_annot:[ Checked ] ~types:[ t_nat ~loc () ^~> t_int ~loc () ]) )
    ; ( C_ABS
      , of_type
          (create ~mode_annot:[ Checked ] ~types:[ t_int ~loc () ^~> t_nat ~loc () ]) )
    ; ( C_LIST_SIZE
      , of_type
          (for_all "a"
          @@ fun a ->
          create ~mode_annot:[ Inferred ] ~types:[ t_list a ~loc () ^~> t_nat ~loc () ]) )
    ; ( C_SET_SIZE
      , of_type
          (for_all "a"
          @@ fun a ->
          create ~mode_annot:[ Inferred ] ~types:[ t_set a ~loc () ^~> t_nat ~loc () ]) )
    ; ( C_MAP_SIZE
      , of_type
          (for_all "a"
          @@ fun a ->
          for_all "b"
          @@ fun b ->
          create ~mode_annot:[ Inferred ] ~types:[ t_map a b ~loc () ^~> t_nat ~loc () ])
      )
    ; ( C_SIZE
      , of_type
          (create
             ~mode_annot:[ Inferred ]
             ~types:
               [ t_string ~loc () ^~> t_nat ~loc (); t_bytes ~loc () ^~> t_nat ~loc () ])
      )
    ; ( C_SLICE
      , of_type
          (create
             ~mode_annot:[ Checked; Checked; Inferred ]
             ~types:
               [ t_nat ~loc () ^-> t_nat ~loc () ^-> t_string ~loc () ^~> t_string ~loc ()
               ; t_nat ~loc () ^-> t_nat ~loc () ^-> t_bytes ~loc () ^~> t_bytes ~loc ()
               ]) )
    ; ( C_MAP_MEM
      , of_type
          (for_all "a"
          @@ fun a ->
          for_all "b"
          @@ fun b ->
          create
            ~mode_annot:[ Checked; Inferred ]
            ~types:
              [ a ^-> t_map a b ~loc () ^~> t_bool ~loc ()
              ; a ^-> t_big_map a b ~loc () ^~> t_bool ~loc ()
              ]) )
    ]


let infer_constant ~infer ~check const args =
  let open C in
  match Const_map.find_opt const constant_typer_tbl with
  | Some typer -> typer ~infer ~check args
  | None ->
    raise
      (corner_case
      @@ Format.asprintf
           "Typer not implemented for constant %a"
           Constant.pp_constant'
           const)


module External_types = struct
  module Annot = struct
    type t = (Type.t list * Type.t) List.Ne.t

    module Syntax = struct
      let loc = Location.env
      let create xs = List.Ne.of_list xs
      let ( ^~> ) arg_type ret_type = [ arg_type ], ret_type
      let ( ^-> ) arg_type (arg_types, ret_type) = arg_type :: arg_types, ret_type
    end
  end

  type ('err, 'wrn) t = Type.t list -> (Type.t, 'err, 'wrn) C.t

  let of_type (types : Annot.t) : _ t =
    let open C in
    let open Let_syntax in
    fun received_arg_types ->
      try_all
      @@ List.map (List.Ne.to_list types) ~f:(fun (expected_arg_types, ret_type) ->
             let%bind arg_types =
               match List.zip received_arg_types expected_arg_types with
               | Ok result -> return result
               | Unequal_lengths ->
                 raise
                   (corner_case
                      "Unequal lengths between mode annotation and argument types")
             in
             (* Unify args types *)
             let%bind () =
               arg_types
               |> List.map ~f:(fun (received, expected) ->
                      let%bind received = Context.tapply received in
                      let%bind expected = Context.tapply expected in
                      unify received expected)
               |> all_unit
             in
             return ret_type)


  let map_find_opt_types : _ t =
    let open Type in
    let open C in
    let open Let_syntax in
    fun (received_arg_types : Type.t list) ->
      if List.length received_arg_types <> 2
      then
        raise (corner_case "Unequal lengths between mode annotation and argument types")
      else (
        let k = List.nth_exn received_arg_types 0 in
        let%bind k = Context.tapply k in
        let m_or_bm = List.nth_exn received_arg_types 1 in
        let%bind m_or_bm = Context.tapply m_or_bm in
        let%bind k', v =
          match get_t_big_map m_or_bm with
          | Some (k', v) -> return (k', v)
          | None ->
            (match get_t_map m_or_bm with
            | Some (k', v) -> return (k', v)
            | None ->
              raise
                (corner_case
                   "external_find_opt: second parameter is neither big_map nor map"))
        in
        let%bind k' = Context.tapply k' in
        let%bind () = unify k k' in
        let%bind v = Context.tapply v in
        let%bind loc = loc () in
        return (t_option ~loc v ()))


  let map_add_types : _ t =
    let open Type in
    let open C in
    let open Let_syntax in
    fun (received_arg_types : Type.t list) ->
      if List.length received_arg_types <> 3
      then
        raise (corner_case "Unequal lengths between mode annotation and argument types")
      else (
        let k = List.nth_exn received_arg_types 0 in
        let%bind k = Context.tapply k in
        let v = List.nth_exn received_arg_types 1 in
        let%bind v = Context.tapply v in
        let m_or_bm = List.nth_exn received_arg_types 2 in
        let%bind m_or_bm = Context.tapply m_or_bm in
        let%bind k', v' =
          match get_t_big_map m_or_bm with
          | Some (k', v') -> return (k', v')
          | None ->
            (match get_t_map m_or_bm with
            | Some (k', v') -> return (k', v')
            | None ->
              raise
                (corner_case
                   "external_find_opt: second parameter is neither big_map nor map"))
        in
        let%bind k' = Context.tapply k' in
        let%bind () = unify k k' in
        let%bind v' = Context.tapply v' in
        let%bind () = unify v v' in
        return m_or_bm)


  let map_remove_types : _ t =
    let open Type in
    let open C in
    let open Let_syntax in
    fun (received_arg_types : Type.t list) ->
      if List.length received_arg_types <> 2
      then
        raise (corner_case "Unequal lengths between mode annotation and argument types")
      else (
        let k = List.nth_exn received_arg_types 0 in
        let%bind k = Context.tapply k in
        let m_or_bm = List.nth_exn received_arg_types 1 in
        let%bind m_or_bm = Context.tapply m_or_bm in
        let%bind k', _ =
          match get_t_big_map m_or_bm with
          | Some (k', v) -> return (k', v)
          | None ->
            (match get_t_map m_or_bm with
            | Some (k', v) -> return (k', v)
            | None ->
              raise
                (corner_case
                   "external_find_opt: second parameter is neither big_map nor map"))
        in
        let%bind k' = Context.tapply k' in
        let%bind () = unify k k' in
        return m_or_bm)


  let bytes_types : (Errors.typer_error, Main_warnings.all) t =
    let open Type in
    let open Annot.Syntax in
    of_type
      (create [ t_int ~loc () ^~> t_bytes ~loc (); t_nat () ~loc ^~> t_bytes ~loc () ])


  let int_types : (Errors.typer_error, Main_warnings.all) t =
    let open Type in
    let open Annot.Syntax in
    of_type
      (create
         [ t_nat () ~loc ^~> t_int ~loc ()
         ; t_bls12_381_fr ~loc () ^~> t_int ~loc ()
         ; t_bytes ~loc () ^~> t_int ~loc ()
         ])


  let ediv_types : (Errors.typer_error, Main_warnings.all) t =
    let open Type in
    let open Annot.Syntax in
    of_type
      (create
         [ t_nat ~loc ()
           ^-> t_nat ~loc ()
           ^~> t_option (t_pair (t_nat ~loc ()) (t_nat ~loc ()) ~loc ()) ~loc ()
         ; t_int ~loc ()
           ^-> t_int ~loc ()
           ^~> t_option (t_pair (t_int ~loc ()) (t_nat ~loc ()) ~loc ()) ~loc ()
         ; t_nat ~loc ()
           ^-> t_int ~loc ()
           ^~> t_option (t_pair (t_int ~loc ()) (t_nat ~loc ()) ~loc ()) ~loc ()
         ; t_int ~loc ()
           ^-> t_nat ~loc ()
           ^~> t_option (t_pair (t_int ~loc ()) (t_nat ~loc ()) ~loc ()) ~loc ()
         ; t_mutez ~loc ()
           ^-> t_mutez ~loc ()
           ^~> t_option (t_pair (t_nat ~loc ()) (t_mutez ~loc ()) ~loc ()) ~loc ()
         ; t_mutez ~loc ()
           ^-> t_nat ~loc ()
           ^~> t_option (t_pair (t_mutez ~loc ()) (t_mutez ~loc ()) ~loc ()) ~loc ()
         ])


  let and_types : (Errors.typer_error, Main_warnings.all) t =
    let open Type in
    let open Annot.Syntax in
    of_type
      (create
         [ t_nat ~loc () ^-> t_nat ~loc () ^~> t_nat ~loc ()
         ; t_int ~loc () ^-> t_nat ~loc () ^~> t_nat ~loc ()
         ; t_bytes ~loc () ^-> t_bytes ~loc () ^~> t_bytes ~loc ()
         ])


  let or_types : (Errors.typer_error, Main_warnings.all) t =
    let open Type in
    let open Annot.Syntax in
    of_type
      (create
         [ t_nat ~loc () ^-> t_nat ~loc () ^~> t_nat ~loc ()
         ; t_bytes ~loc () ^-> t_bytes ~loc () ^~> t_bytes ~loc ()
         ])


  let xor_types : (Errors.typer_error, Main_warnings.all) t =
    let open Type in
    let open Annot.Syntax in
    of_type
      (create
         [ t_nat ~loc () ^-> t_nat ~loc () ^~> t_nat ~loc ()
         ; t_bytes ~loc () ^-> t_bytes ~loc () ^~> t_bytes ~loc ()
         ])


  let lsl_types : (Errors.typer_error, Main_warnings.all) t =
    let open Type in
    let open Annot.Syntax in
    of_type
      (create
         [ t_nat ~loc () ^-> t_nat ~loc () ^~> t_nat ~loc ()
         ; t_bytes ~loc () ^-> t_nat ~loc () ^~> t_bytes ~loc ()
         ])


  let lsr_types : (Errors.typer_error, Main_warnings.all) t =
    let open Type in
    let open Annot.Syntax in
    of_type
      (create
         [ t_nat ~loc () ^-> t_nat ~loc () ^~> t_nat ~loc ()
         ; t_bytes ~loc () ^-> t_nat ~loc () ^~> t_bytes ~loc ()
         ])
end
