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
          (Record.LMap.to_list row1.fields)
          (Record.LMap.to_list row2.fields)
          ~f:(fun (row_elem1 : Type.row_element) (row_elem2 : Type.row_element)
             -> comparator row_elem1.associated_type row_elem2.associated_type)
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
          (Record.LMap.to_list row1.fields)
          (Record.LMap.to_list row2.fields)
          ~f:(fun (row_elem1 : Type.row_element) (row_elem2 : Type.row_element)
             -> comparator row_elem1.associated_type row_elem2.associated_type)
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
    let for_all tvar ?(kind = (Type : Kind.t)) in_ =
      let tvar = Type_var.of_input_var ("'" ^ tvar) in
      let result = in_ (Type.t_variable tvar ()) in
      let types =
        List.Ne.map
          (fun type_ ->
            { type_ with for_alls = (tvar, kind) :: type_.for_alls })
          result.types
      in
      { result with types }


    let t_for_all tvar ?(kind = (Type : Kind.t)) in_ =
      let tvar = Type_var.of_input_var ("'" ^ tvar) in
      let result = in_ (Type.t_variable tvar ()) in
      { result with for_alls = (tvar, kind) :: result.for_alls }


    let create ~mode_annot ~types =
      { mode_annot; types = List.Ne.of_list types }


    let return ret_type = { for_alls = []; arg_types = []; ret_type }

    let ( ^~> ) arg_type ret_type =
      { for_alls = []; arg_types = [ arg_type ]; ret_type }


    let ( ^-> ) arg_type type_ =
      { type_ with arg_types = arg_type :: type_.arg_types }


    let ( @-> ) t1 t2 = Type.t_arrow { type1 = t1; type2 = t2 } ()
  end
end

type ('err, 'wrn) infer =
  I.expression -> (Type.t * O.expression E.t, 'err, 'wrn) C.t

type ('err, 'wrn) check =
  I.expression -> Type.t -> (O.expression E.t, 'err, 'wrn) C.t

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
    fun i ->
      raise_opt ~error:(corner_case "bad mode annot") @@ Hashtbl.find table i
  in
  fun ~infer ~check args ->
    (* Instantiate prenex quantifier *)
    let inst { Annot.for_alls; arg_types; ret_type }
        : (Type.t list * Type.t, _, _) C.t
      =
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
                      "Unequal lengths between mode annotation and argument \
                       types")
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
                       ( (arg_type, fst @@ Hashtbl.find_exn output_args i)
                         :: unify_worklist
                       , checked )
                   | Checked ->
                     return (unify_worklist, (i, arg_type, arg) :: checked))
             in
             (* Unify the inferred types *)
             let%bind () =
               unify_worklist
               (* Reverse due to [foldi] above *)
               |> List.rev_map ~f:(fun (arg_type1, arg_type2) ->
                      let%bind arg_type1 = Context.tapply arg_type1 in
                      let%bind arg_type2 = Context.tapply arg_type2 in
                      unify arg_type1 arg_type2)
               |> all_unit
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
             "Unequal length between comparator arguments and applied arguments"
        )


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
            ~types:[ (a @-> t_sum_ez [ "left", a; "right", b ] ()) ^-> a ^~> b ]
          ) )
    ; ( C_LEFT
      , of_type
          (for_all "a"
          @@ fun a ->
          create
            ~mode_annot:[ Inferred ]
            ~types:[ a ^~> t_sum_ez [ "left", a; "right", a ] () ]) )
    ; ( C_LOOP_CONTINUE
      , of_type
          (for_all "a"
          @@ fun a ->
          create
            ~mode_annot:[ Inferred ]
            ~types:[ a ^~> t_sum_ez [ "left", a; "right", a ] () ]) )
    ; ( C_LOOP_STOP
      , of_type
          (for_all "a"
          @@ fun a ->
          create
            ~mode_annot:[ Inferred ]
            ~types:[ a ^~> t_sum_ez [ "left", a; "right", a ] () ]) )
    ; ( C_ITER
      , of_type
          (create
             ~mode_annot:[ Checked; Inferred ]
             ~types:
               [ (t_for_all "a"
                 @@ fun a -> (a @-> t_unit ()) ^-> t_list a () ^~> t_unit ())
               ; (t_for_all "a"
                 @@ fun a -> (a @-> t_unit ()) ^-> t_set a () ^~> t_unit ())
               ; (t_for_all "a"
                 @@ fun a ->
                 t_for_all "b"
                 @@ fun b ->
                 (t_pair a b () @-> t_unit ()) ^-> t_map a b () ^~> t_unit ())
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
              [ (t_pair a b () @-> a) ^-> t_list b () ^-> a ^~> a
              ; (t_pair a b () @-> a) ^-> t_set b () ^-> a ^~> a
              ]) )
      (* Map *)
    ; ( C_MAP_EMPTY
      , of_type
          (for_all "a"
          @@ fun a ->
          for_all "b"
          @@ fun b -> create ~mode_annot:[] ~types:[ return (t_map a b ()) ]) )
    ; ( C_BIG_MAP_EMPTY
      , of_type
          (for_all "a"
          @@ fun a ->
          for_all "b"
          @@ fun b -> create ~mode_annot:[] ~types:[ return (t_big_map a b ()) ]
          ) )
    ; ( C_MAP_ADD
      , of_type
          (for_all "a"
          @@ fun a ->
          for_all "b"
          @@ fun b ->
          create
            ~mode_annot:[ Checked; Checked; Inferred ]
            ~types:
              [ a ^-> b ^-> t_map a b () ^~> t_map a b ()
              ; a ^-> b ^-> t_big_map a b () ^~> t_big_map a b ()
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
              [ a ^-> t_map a b () ^~> t_map a b ()
              ; a ^-> t_big_map a b () ^~> t_big_map a b ()
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
              [ a ^-> t_option b () ^-> t_map a b () ^~> t_map a b ()
              ; a ^-> t_option b () ^-> t_big_map a b () ^~> t_big_map a b ()
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
                ^-> t_option b ()
                ^-> t_map a b ()
                ^~> t_pair (t_option b ()) (t_map a b ()) ()
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
                ^-> t_option b ()
                ^-> t_big_map a b ()
                ^~> t_pair (t_option b ()) (t_big_map a b ()) ()
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
              [ a ^-> t_map a b () ^~> t_option b ()
              ; a ^-> t_big_map a b () ^~> t_option b ()
              ]) )
    ; ( C_MAP_FIND
      , of_type
          (for_all "a"
          @@ fun a ->
          for_all "b"
          @@ fun b ->
          create
            ~mode_annot:[ Checked; Inferred ]
            ~types:[ a ^-> t_map a b () ^~> b; a ^-> t_big_map a b () ^~> b ]) )
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
            ~types:[ (t_pair a b () @-> c) ^-> t_map a b () ^~> t_map a c () ])
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
              [ (t_pair a b () @-> t_unit ()) ^-> t_map a b () ^~> t_unit () ])
      )
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
              [ (t_pair c (t_pair a b ()) () @-> c) ^-> t_map a b () ^-> c ^~> c
              ]) )
      (* List *)
    ; ( C_LIST_EMPTY
      , of_type
          (for_all "a"
          @@ fun a -> create ~mode_annot:[] ~types:[ return @@ t_list a () ]) )
    ; ( C_CONS
      , of_type
          (for_all "a"
          @@ fun a ->
          create
            ~mode_annot:[ Inferred; Checked ]
            ~types:[ a ^-> t_list a () ^~> t_list a () ]) )
    ; ( C_LIST_MAP
      , of_type
          (for_all "a"
          @@ fun a ->
          for_all "b"
          @@ fun b ->
          create
            ~mode_annot:[ Checked; Inferred ]
            ~types:[ (a @-> b) ^-> t_list a () ^~> t_list b () ]) )
    ; ( C_LIST_ITER
      , of_type
          (for_all "a"
          @@ fun a ->
          create
            ~mode_annot:[ Checked; Inferred ]
            ~types:[ (a @-> t_unit ()) ^-> t_list a () ^~> t_unit () ]) )
    ; ( C_LIST_FOLD
      , of_type
          (for_all "a"
          @@ fun a ->
          for_all "b"
          @@ fun b ->
          create
            ~mode_annot:[ Checked; Inferred; Inferred ]
            ~types:[ (t_pair b a () @-> b) ^-> t_list a () ^-> b ^~> b ]) )
    ; ( C_LIST_FOLD_LEFT
      , of_type
          (for_all "a"
          @@ fun a ->
          for_all "b"
          @@ fun b ->
          create
            ~mode_annot:[ Checked; Inferred; Inferred ]
            ~types:[ (t_pair b a () @-> b) ^-> b ^-> t_list a () ^~> b ]) )
    ; ( C_LIST_FOLD_RIGHT
      , of_type
          (for_all "a"
          @@ fun a ->
          for_all "b"
          @@ fun b ->
          create
            ~mode_annot:[ Checked; Inferred; Inferred ]
            ~types:[ (t_pair a b () @-> b) ^-> t_list a () ^-> b ^~> b ]) )
      (* Set *)
    ; ( C_SET_EMPTY
      , of_type
          (for_all "a"
          @@ fun a -> create ~mode_annot:[] ~types:[ return @@ t_set a () ]) )
    ; ( C_SET_LITERAL
      , of_type
          (for_all "a"
          @@ fun a ->
          create ~mode_annot:[ Inferred ] ~types:[ t_list a () ^~> t_set a () ]
          ) )
    ; ( C_SET_MEM
      , of_type
          (for_all "a"
          @@ fun a ->
          create
            ~mode_annot:[ Inferred; Checked ]
            ~types:[ a ^-> t_set a () ^~> t_bool () ]) )
    ; ( C_SET_ADD
      , of_type
          (for_all "a"
          @@ fun a ->
          create
            ~mode_annot:[ Inferred; Checked ]
            ~types:[ a ^-> t_set a () ^~> t_set a () ]) )
    ; ( C_SET_REMOVE
      , of_type
          (for_all "a"
          @@ fun a ->
          create
            ~mode_annot:[ Inferred; Checked ]
            ~types:[ a ^-> t_set a () ^~> t_set a () ]) )
    ; ( C_SET_UPDATE
      , of_type
          (for_all "a"
          @@ fun a ->
          create
            ~mode_annot:[ Inferred; Checked; Checked ]
            ~types:[ a ^-> t_bool () ^-> t_set a () ^~> t_set a () ]) )
    ; ( C_SET_ITER
      , of_type
          (for_all "a"
          @@ fun a ->
          create
            ~mode_annot:[ Checked; Inferred ]
            ~types:[ (a @-> t_unit ()) ^-> t_set a () ^~> t_unit () ]) )
    ; ( C_SET_FOLD
      , of_type
          (for_all "a"
          @@ fun a ->
          for_all "b"
          @@ fun b ->
          create
            ~mode_annot:[ Checked; Inferred; Inferred ]
            ~types:[ (t_pair b a () @-> b) ^-> t_set a () ^-> b ^~> b ]) )
    ; ( C_SET_FOLD_DESC
      , of_type
          (for_all "a"
          @@ fun a ->
          for_all "b"
          @@ fun b ->
          create
            ~mode_annot:[ Checked; Inferred; Inferred ]
            ~types:[ (t_pair a b () @-> b) ^-> t_set a () ^-> b ^~> b ]) )
      (* Bytes *)
    ; ( C_CONCAT
      , of_type
          (create
             ~mode_annot:[ Inferred; Inferred ]
             ~types:
               [ t_string () ^-> t_string () ^~> t_string ()
               ; t_bytes () ^-> t_bytes () ^~> t_bytes ()
               ]) )
      (* Option *)
    ; ( C_NONE
      , of_type
          (for_all "a"
          @@ fun a -> create ~mode_annot:[] ~types:[ return @@ t_option a () ])
      )
    ; ( C_SOME
      , of_type
          (for_all "a"
          @@ fun a ->
          create ~mode_annot:[ Inferred ] ~types:[ a ^~> t_option a () ]) )
    ; ( C_OPTION_MAP
      , of_type
          (for_all "a"
          @@ fun a ->
          for_all "b"
          @@ fun b ->
          create
            ~mode_annot:[ Checked; Inferred ]
            ~types:[ (a @-> b) ^-> t_option a () ^~> t_option b () ]) )
    ; ( C_CHECK_ENTRYPOINT
      , of_type
          (create ~mode_annot:[ Checked ] ~types:[ t_string () ^~> t_unit () ])
      )
    ; ( C_CHECK_SELF
      , of_type
          (for_all "a"
          @@ fun a ->
          create
            ~mode_annot:[ Checked ]
            ~types:[ t_string () ^~> t_option a () ]) )
    ; ( C_CREATE_CONTRACT
      , of_type
          (for_all "a"
          @@ fun a ->
          for_all "b"
          @@ fun b ->
          create
            ~mode_annot:[ Checked; Checked; Checked; Inferred ]
            ~types:
              [ (t_pair a b () @-> t_pair (t_list (t_operation ()) ()) b ())
                ^-> t_option (t_key_hash ()) ()
                ^-> t_mutez ()
                ^-> b
                ^~> t_pair (t_operation ()) (t_address ()) ()
              ]) )
    ; ( C_CHECK_EMIT_EVENT
      , of_type
          (for_all "a"
          @@ fun a ->
          create
            ~mode_annot:[ Checked; Inferred ]
            ~types:[ t_string () ^-> a ^~> t_unit () ]) )
      (* Primitives *)
    ; C_UNIT, of_type (create ~mode_annot:[] ~types:[ return @@ t_unit () ])
    ; C_TRUE, of_type (create ~mode_annot:[] ~types:[ return @@ t_bool () ])
    ; C_FALSE, of_type (create ~mode_annot:[] ~types:[ return @@ t_bool () ])
    ; ( C_POLYMORPHIC_ADD
      , of_type
          (create
             ~mode_annot:[ Inferred; Inferred ]
             ~types:
               [ t_int () ^-> t_int () ^~> t_int ()
               ; t_string () ^-> t_string () ^~> t_string ()
               ; t_nat () ^-> t_nat () ^~> t_nat ()
               ; t_mutez () ^-> t_mutez () ^~> t_mutez ()
               ; t_int () ^-> t_nat () ^~> t_int ()
               ; t_nat () ^-> t_int () ^~> t_int ()
               ; t_timestamp () ^-> t_int () ^~> t_timestamp ()
               ; t_int () ^-> t_timestamp () ^~> t_timestamp ()
               ; t_int64 () ^-> t_int64 () ^~> t_int64 ()
               ; t_bls12_381_g1 () ^-> t_bls12_381_g1 () ^~> t_bls12_381_g1 ()
               ; t_bls12_381_g2 () ^-> t_bls12_381_g2 () ^~> t_bls12_381_g2 ()
               ; t_bls12_381_fr () ^-> t_bls12_381_fr () ^~> t_bls12_381_fr ()
               ]) )
    ; ( C_POLYMORPHIC_SUB
      , of_type
          (create
             ~mode_annot:[ Inferred; Inferred ]
             ~types:
               [ t_int () ^-> t_int () ^~> t_int ()
               ; t_nat () ^-> t_nat () ^~> t_int ()
               ; t_nat () ^-> t_int () ^~> t_int ()
               ; t_int () ^-> t_nat () ^~> t_int ()
               ; t_timestamp () ^-> t_timestamp () ^~> t_int ()
               ; t_timestamp () ^-> t_int () ^~> t_timestamp ()
               ; t_mutez () ^-> t_mutez () ^~> t_option (t_mutez ()) ()
               ; t_int64 () ^-> t_int64 () ^~> t_int64 ()
               ; t_bls12_381_g1 () ^-> t_bls12_381_g1 () ^~> t_bls12_381_g1 ()
               ; t_bls12_381_g2 () ^-> t_bls12_381_g2 () ^~> t_bls12_381_g2 ()
               ; t_bls12_381_fr () ^-> t_bls12_381_fr () ^~> t_bls12_381_fr ()
               ]) )
    ; ( C_ADD
      , of_type
          (create
             ~mode_annot:[ Inferred; Inferred ]
             ~types:
               [ t_int () ^-> t_int () ^~> t_int ()
               ; t_int () ^-> t_nat () ^~> t_int ()
               ; t_nat () ^-> t_nat () ^~> t_nat ()
               ; t_mutez () ^-> t_mutez () ^~> t_mutez ()
               ; t_nat () ^-> t_int () ^~> t_int ()
               ; t_timestamp () ^-> t_int () ^~> t_timestamp ()
               ; t_int () ^-> t_timestamp () ^~> t_timestamp ()
               ; t_int64 () ^-> t_int64 () ^~> t_int64 ()
               ; t_bls12_381_g1 () ^-> t_bls12_381_g1 () ^~> t_bls12_381_g1 ()
               ; t_bls12_381_g2 () ^-> t_bls12_381_g2 () ^~> t_bls12_381_g2 ()
               ; t_bls12_381_fr () ^-> t_bls12_381_fr () ^~> t_bls12_381_fr ()
               ]) )
    ; ( C_MUL
      , of_type
          (create
             ~mode_annot:[ Inferred; Inferred ]
             ~types:
               [ t_int () ^-> t_int () ^~> t_int ()
               ; t_nat () ^-> t_bls12_381_fr () ^~> t_bls12_381_fr ()
               ; t_int () ^-> t_bls12_381_fr () ^~> t_bls12_381_fr ()
               ; t_bls12_381_fr () ^-> t_nat () ^~> t_bls12_381_fr ()
               ; t_bls12_381_fr () ^-> t_int () ^~> t_bls12_381_fr ()
               ; t_nat () ^-> t_nat () ^~> t_nat ()
               ; t_nat () ^-> t_mutez () ^~> t_mutez ()
               ; t_mutez () ^-> t_nat () ^~> t_mutez ()
               ; t_int () ^-> t_nat () ^~> t_int ()
               ; t_nat () ^-> t_int () ^~> t_int ()
               ; t_int64 () ^-> t_int64 () ^~> t_int64 ()
               ; t_bls12_381_g1 () ^-> t_bls12_381_fr () ^~> t_bls12_381_g1 ()
               ; t_bls12_381_g2 () ^-> t_bls12_381_fr () ^~> t_bls12_381_g2 ()
               ; t_bls12_381_fr () ^-> t_bls12_381_fr () ^~> t_bls12_381_fr ()
               ]) )
    ; ( C_SUB
      , of_type
          (create
             ~mode_annot:[ Inferred; Inferred ]
             ~types:
               [ t_int () ^-> t_int () ^~> t_int ()
               ; t_bls12_381_g1 () ^-> t_bls12_381_fr () ^~> t_bls12_381_g1 ()
               ; t_bls12_381_g2 () ^-> t_bls12_381_fr () ^~> t_bls12_381_g2 ()
               ; t_bls12_381_fr () ^-> t_bls12_381_fr () ^~> t_bls12_381_fr ()
               ; t_nat () ^-> t_nat () ^~> t_int ()
               ; t_int () ^-> t_nat () ^~> t_int ()
               ; t_nat () ^-> t_int () ^~> t_int ()
               ; t_timestamp () ^-> t_timestamp () ^~> t_int ()
               ; t_timestamp () ^-> t_int () ^~> t_timestamp ()
               ; t_mutez () ^-> t_mutez () ^~> t_mutez ()
               ; t_int64 () ^-> t_int64 () ^~> t_int64 ()
               ]) )
    ; ( C_SUB_MUTEZ
      , of_type
          (create
             ~mode_annot:[ Checked; Checked ]
             ~types:[ t_mutez () ^-> t_mutez () ^~> t_option (t_mutez ()) () ])
      )
    ; ( C_DIV
      , of_type
          (create
             ~mode_annot:[ Inferred; Inferred ]
             ~types:
               [ t_int () ^-> t_int () ^~> t_int ()
               ; t_nat () ^-> t_nat () ^~> t_nat ()
               ; t_nat () ^-> t_int () ^~> t_int ()
               ; t_int () ^-> t_nat () ^~> t_int ()
               ; t_mutez () ^-> t_nat () ^~> t_mutez ()
               ; t_mutez () ^-> t_mutez () ^~> t_nat ()
               ; t_int64 () ^-> t_int64 () ^~> t_int64 ()
               ]) )
    ; ( C_MOD
      , of_type
          (create
             ~mode_annot:[ Inferred; Inferred ]
             ~types:
               [ t_int () ^-> t_int () ^~> t_nat ()
               ; t_nat () ^-> t_nat () ^~> t_nat ()
               ; t_nat () ^-> t_int () ^~> t_nat ()
               ; t_int () ^-> t_nat () ^~> t_nat ()
               ; t_mutez () ^-> t_nat () ^~> t_mutez ()
               ; t_mutez () ^-> t_mutez () ^~> t_mutez ()
               ; t_int64 () ^-> t_int64 () ^~> t_int64 ()
               ]) )
    ; ( C_NEG
      , of_type
          (create
             ~mode_annot:[ Inferred ]
             ~types:
               [ t_int () ^~> t_int ()
               ; t_nat () ^~> t_int ()
               ; t_bls12_381_g1 () ^~> t_bls12_381_g1 ()
               ; t_bls12_381_g2 () ^~> t_bls12_381_g2 ()
               ; t_bls12_381_fr () ^~> t_bls12_381_fr ()
               ]) )
      (* Logical operators *)
    ; ( C_NOT
      , of_type
          (create
             ~mode_annot:[ Inferred ]
             ~types:
               [ t_bool () ^~> t_bool ()
               ; t_int () ^~> t_int ()
               ; t_nat () ^~> t_int ()
               ]) )
    ; ( C_AND
      , of_type
          (create
             ~mode_annot:[ Inferred; Inferred ]
             ~types:
               [ t_bool () ^-> t_bool () ^~> t_bool ()
               ; t_nat () ^-> t_nat () ^~> t_nat ()
               ; t_int () ^-> t_nat () ^~> t_nat ()
               ; t_int64 () ^-> t_int64 () ^~> t_int64 ()
               ]) )
    ; ( C_OR
      , of_type
          (create
             ~mode_annot:[ Inferred; Inferred ]
             ~types:
               [ t_bool () ^-> t_bool () ^~> t_bool ()
               ; t_nat () ^-> t_nat () ^~> t_nat ()
               ; t_int64 () ^-> t_int64 () ^~> t_int64 ()
               ]) )
    ; ( C_XOR
      , of_type
          (create
             ~mode_annot:[ Inferred; Inferred ]
             ~types:
               [ t_bool () ^-> t_bool () ^~> t_bool ()
               ; t_nat () ^-> t_nat () ^~> t_nat ()
               ; t_int64 () ^-> t_int64 () ^~> t_int64 ()
               ]) )
    ; ( C_LSL
      , of_type
          (create
             ~mode_annot:[ Inferred; Inferred ]
             ~types:
               [ t_nat () ^-> t_nat () ^~> t_nat ()
               ; t_int64 () ^-> t_nat () ^~> t_int64 ()
               ]) )
    ; ( C_LSR
      , of_type
          (create
             ~mode_annot:[ Inferred; Inferred ]
             ~types:
               [ t_nat () ^-> t_nat () ^~> t_nat ()
               ; t_int64 () ^-> t_nat () ^~> t_int64 ()
               ]) )
      (* Tests *)
    ; ( C_TEST_ADDRESS
      , of_type
          (for_all "a"
          @@ fun a ->
          create
            ~mode_annot:[ Checked ]
            ~types:[ t_contract a () ^~> t_address () ]) )
    ; ( C_TEST_COMPILE_CONTRACT
      , of_type
          (for_all "a"
          @@ fun a ->
          for_all "b"
          @@ fun b ->
          create
            ~mode_annot:[ Inferred ]
            ~types:
              [ (t_pair a b () @-> t_pair (t_list (t_operation ()) ()) b ())
                ^~> t_ast_contract ()
              ]) )
    ; ( C_TEST_COMPILE_AST_CONTRACT
      , of_type
          (create
             ~mode_annot:[ Checked ]
             ~types:[ t_ast_contract () ^~> t_michelson_contract () ]) )
    ; ( C_TEST_SIZE
      , of_type
          (create
             ~mode_annot:[ Checked ]
             ~types:[ t_michelson_contract () ^~> t_int () ]) )
    ; ( C_TEST_ORIGINATE
      , of_type
          (create
             ~mode_annot:[ Checked; Checked; Checked ]
             ~types:
               [ t_michelson_contract ()
                 ^-> t_michelson_code ()
                 ^-> t_mutez ()
                 ^~> t_address ()
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
              [ (t_pair a b () @-> t_pair (t_list (t_operation ()) ()) b ())
                ^-> b
                ^-> t_mutez ()
                ^~> t_unit ()
              ]) )
    ; ( C_TEST_LAST_ORIGINATIONS
      , of_type
          (create
             ~mode_annot:[ Checked ]
             ~types:
               [ t_unit ()
                 ^~> t_map (t_address ()) (t_list (t_address ()) ()) ()
               ]) )
    ; ( C_TEST_LAST_EVENTS
      , of_type
          (for_all "a"
          @@ fun a ->
          create
            ~mode_annot:[ Checked ]
            ~types:[ t_string () ^~> t_list (t_pair (t_address ()) a ()) () ]) )
    ; ( C_TEST_NTH_BOOTSTRAP_TYPED_ADDRESS
      , of_type
          (for_all "a"
          @@ fun a ->
          for_all "b"
          @@ fun b ->
          create
            ~mode_annot:[ Checked ]
            ~types:[ t_nat () ^~> t_typed_address a b () ]) )
    ; ( C_TEST_SET_SOURCE
      , of_type
          (create ~mode_annot:[ Checked ] ~types:[ t_address () ^~> t_unit () ])
      )
    ; ( C_TEST_SET_BAKER
      , of_type
          (create
             ~mode_annot:[ Checked ]
             ~types:[ t_test_baker_policy () ^~> t_unit () ]) )
    ; ( C_TEST_NTH_BOOTSTRAP_CONTRACT
      , of_type
          (create ~mode_annot:[ Checked ] ~types:[ t_nat () ^~> t_address () ])
      )
    ; ( C_TEST_GET_STORAGE_OF_ADDRESS
      , of_type
          (create
             ~mode_annot:[ Checked ]
             ~types:[ t_address () ^~> t_michelson_code () ]) )
    ; ( C_TEST_GET_BALANCE
      , of_type
          (create
             ~mode_annot:[ Checked ]
             ~types:[ t_address () ^~> t_mutez () ]) )
    ; ( C_TEST_GET_NTH_BS
      , of_type
          (create
             ~mode_annot:[ Checked ]
             ~types:
               [ t_int ()
                 ^~> t_triplet (t_address ()) (t_key ()) (t_string ()) ()
               ]) )
    ; ( C_TEST_PRINT
      , of_type
          (create
             ~mode_annot:[ Checked; Checked ]
             ~types:[ t_int () ^-> t_string () ^~> t_unit () ]) )
    ; ( C_TEST_TO_STRING
      , of_type
          (for_all "a"
          @@ fun a ->
          create
            ~mode_annot:[ Inferred; Checked ]
            ~types:[ a ^-> t_int () ^~> t_string () ]) )
    ; ( C_TEST_UNESCAPE_STRING
      , of_type
          (create
             ~mode_annot:[ Checked ]
             ~types:[ t_string () ^~> t_string () ]) )
    ; ( C_TEST_STATE_RESET
      , of_type
          (create
             ~mode_annot:[ Checked; Checked; Checked ]
             ~types:
               [ t_option (t_timestamp ()) ()
                 ^-> t_nat ()
                 ^-> t_list (t_mutez ()) ()
                 ^~> t_unit ()
               ]) )
    ; ( C_TEST_GET_VOTING_POWER
      , of_type
          (create ~mode_annot:[ Checked ] ~types:[ t_key_hash () ^~> t_nat () ])
      )
    ; ( C_TEST_GET_TOTAL_VOTING_POWER
      , of_type
          (create ~mode_annot:[ Checked ] ~types:[ t_unit () ^~> t_nat () ]) )
    ; ( C_TEST_CAST_ADDRESS
      , of_type
          (for_all "a"
          @@ fun a ->
          for_all "b"
          @@ fun b ->
          create
            ~mode_annot:[ Checked ]
            ~types:[ t_address () ^~> t_typed_address a b () ]) )
    ; ( C_TEST_RANDOM
      , of_type
          (for_all "a"
          @@ fun a ->
          create ~mode_annot:[ Checked ] ~types:[ t_bool () ^~> t_gen a () ]) )
    ; ( C_TEST_GENERATOR_EVAL
      , of_type
          (for_all "a"
          @@ fun a -> create ~mode_annot:[ Checked ] ~types:[ t_gen a () ^~> a ]
          ) )
    ; ( C_TEST_MUTATE_CONTRACT
      , of_type
          (create
             ~mode_annot:[ Checked; Checked ]
             ~types:
               [ t_nat ()
                 ^-> t_ast_contract ()
                 ^~> t_option (t_pair (t_ast_contract ()) (t_mutation ()) ()) ()
               ]) )
    ; ( C_TEST_MUTATE_VALUE
      , of_type
          (for_all "a"
          @@ fun a ->
          create
            ~mode_annot:[ Checked; Inferred ]
            ~types:
              [ t_nat () ^-> a ^~> t_option (t_pair a (t_mutation ()) ()) () ])
      )
    ; ( C_TEST_TRY_WITH
      , of_type
          (for_all "a"
          @@ fun a ->
          create
            ~mode_annot:[ Checked; Checked ]
            ~types:[ (t_unit () @-> a) ^-> (t_unit () @-> a) ^~> a ]) )
    ; ( C_TEST_SAVE_MUTATION
      , of_type
          (create
             ~mode_annot:[ Checked; Checked ]
             ~types:
               [ t_string () ^-> t_mutation () ^~> t_option (t_string ()) () ])
      )
    ; ( C_TEST_ADD_ACCOUNT
      , of_type
          (create
             ~mode_annot:[ Checked; Checked ]
             ~types:[ t_string () ^-> t_key () ^~> t_unit () ]) )
    ; ( C_TEST_NEW_ACCOUNT
      , of_type
          (create
             ~mode_annot:[ Checked ]
             ~types:[ t_unit () ^~> t_pair (t_string ()) (t_key ()) () ]) )
    ; ( C_TEST_RUN
      , of_type
          (for_all "a"
          @@ fun a ->
          for_all "b"
          @@ fun b ->
          create
            ~mode_annot:[ Checked; Inferred ]
            ~types:[ (a @-> b) ^-> a ^~> t_michelson_code () ]) )
    ; ( C_TEST_DECOMPILE
      , of_type
          (for_all "a"
          @@ fun a ->
          create ~mode_annot:[ Checked ] ~types:[ t_michelson_code () ^~> a ]) )
    ; ( C_TEST_TO_TYPED_ADDRESS
      , of_type
          (for_all "a"
          @@ fun a ->
          for_all "b"
          @@ fun b ->
          create
            ~mode_annot:[ Inferred ]
            ~types:[ t_contract a () ^~> t_typed_address a b () ]) )
    ; ( C_TEST_EXTERNAL_CALL_TO_ADDRESS
      , of_type
          (create
             ~mode_annot:[ Checked; Checked; Checked; Checked ]
             ~types:
               [ t_address ()
                 ^-> t_option (t_string ()) ()
                 ^-> t_michelson_code ()
                 ^-> t_mutez ()
                 ^~> t_test_exec_result ()
               ]) )
    ; ( C_TEST_EXTERNAL_CALL_TO_ADDRESS_EXN
      , of_type
          (create
             ~mode_annot:[ Checked; Checked; Checked; Checked ]
             ~types:
               [ t_address ()
                 ^-> t_option (t_string ()) ()
                 ^-> t_michelson_code ()
                 ^-> t_mutez ()
                 ^~> t_nat ()
               ]) )
    ; ( C_TEST_SET_BIG_MAP
      , of_type
          (for_all "a"
          @@ fun a ->
          for_all "b"
          @@ fun b ->
          create
            ~mode_annot:[ Checked; Inferred ]
            ~types:[ t_int () ^-> t_big_map a b () ^~> t_unit () ]) )
    ; ( C_TEST_BAKER_ACCOUNT
      , of_type
          (create
             ~mode_annot:[ Checked; Checked ]
             ~types:
               [ t_pair (t_string ()) (t_key ()) ()
                 ^-> t_option (t_mutez ()) ()
                 ^~> t_unit ()
               ]) )
    ; ( C_TEST_REGISTER_DELEGATE
      , of_type
          (create
             ~mode_annot:[ Checked ]
             ~types:[ t_key_hash () ^~> t_unit () ]) )
    ; ( C_TEST_BAKE_UNTIL_N_CYCLE_END
      , of_type
          (create ~mode_annot:[ Checked ] ~types:[ t_nat () ^~> t_unit () ]) )
    ; ( C_TEST_TO_CONTRACT
      , of_type
          (for_all "a"
          @@ fun a ->
          for_all "b"
          @@ fun b ->
          create
            ~mode_annot:[ Inferred ]
            ~types:[ t_typed_address a b () ^~> t_contract a () ]) )
    ; ( C_TEST_CREATE_CHEST
      , of_type
          (create
             ~mode_annot:[ Checked; Checked ]
             ~types:
               [ t_bytes ()
                 ^-> t_nat ()
                 ^~> t_pair (t_chest ()) (t_chest_key ()) ()
               ]) )
    ; ( C_TEST_CREATE_CHEST_KEY
      , of_type
          (create
             ~mode_annot:[ Checked; Checked ]
             ~types:[ t_chest () ^-> t_nat () ^~> t_chest_key () ]) )
    ; ( C_GLOBAL_CONSTANT
      , of_type
          (for_all "a"
          @@ fun a ->
          create ~mode_annot:[ Checked ] ~types:[ t_string () ^~> a ]) )
    ; ( C_TEST_COMPILE_CONTRACT_FROM_FILE
      , of_type
          (create
             ~mode_annot:[ Checked; Checked; Checked; Checked ]
             ~types:
               [ t_string ()
                 ^-> t_string ()
                 ^-> t_list (t_string ()) ()
                 ^-> t_option (t_nat ()) ()
                 ^~> t_ast_contract ()
               ]) )
    ; ( C_TEST_REGISTER_CONSTANT
      , of_type
          (create
             ~mode_annot:[ Checked ]
             ~types:[ t_michelson_code () ^~> t_string () ]) )
    ; ( C_TEST_CONSTANT_TO_MICHELSON
      , of_type
          (create
             ~mode_annot:[ Checked ]
             ~types:[ t_string () ^~> t_michelson_code () ]) )
    ; ( C_TEST_REGISTER_FILE_CONSTANTS
      , of_type
          (create
             ~mode_annot:[ Checked ]
             ~types:[ t_string () ^~> t_list (t_string ()) () ]) )
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
              [ t_string () ^-> t_typed_address a b () ^~> t_contract c () ]) )
    ; ( C_TEST_FAILWITH
      , of_type
          (for_all "a"
          @@ fun a ->
          for_all "b"
          @@ fun b -> create ~mode_annot:[ Inferred ] ~types:[ a ^~> b ]) )
    ; ( C_TEST_PUSH_CONTEXT
      , of_type
          (create ~mode_annot:[ Checked ] ~types:[ t_unit () ^~> t_unit () ]) )
    ; ( C_TEST_POP_CONTEXT
      , of_type
          (create ~mode_annot:[ Checked ] ~types:[ t_unit () ^~> t_unit () ]) )
    ; ( C_TEST_DROP_CONTEXT
      , of_type
          (create ~mode_annot:[ Checked ] ~types:[ t_unit () ^~> t_unit () ]) )
    ; ( C_TEST_SET_PRINT_VALUES
      , of_type
          (create ~mode_annot:[ Checked ] ~types:[ t_bool () ^~> t_bool () ]) )
    ; ( C_TEST_READ_CONTRACT_FROM_FILE
      , of_type
          (create
             ~mode_annot:[ Checked ]
             ~types:[ t_string () ^~> t_michelson_contract () ]) )
    ; ( C_TEST_SIGN
      , of_type
          (create
             ~mode_annot:[ Checked; Checked ]
             ~types:[ t_string () ^-> t_bytes () ^~> t_signature () ]) )
    ; ( C_TEST_GET_ENTRYPOINT
      , of_type
          (for_all "a"
          @@ fun a ->
          create
            ~mode_annot:[ Checked ]
            ~types:[ t_contract a () ^~> t_option (t_string ()) () ]) )
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
            ~types:[ t_list (t_pair a b ()) () ^~> t_map a b () ]) )
    ; ( C_BIG_MAP_LITERAL
      , of_type
          (for_all "a"
          @@ fun a ->
          for_all "b"
          @@ fun b ->
          create
            ~mode_annot:[ Inferred ]
            ~types:[ t_list (t_pair a b ()) () ^~> t_big_map a b () ]) )
    ; ( C_SET_LITERAL
      , of_type
          (for_all "a"
          @@ fun a ->
          create ~mode_annot:[ Inferred ] ~types:[ t_list a () ^~> t_set a () ]
          ) )
    ; ( C_TEST_INT64_OF_INT
      , of_type
          (create ~mode_annot:[ Checked ] ~types:[ t_int () ^~> t_int64 () ]) )
    ; ( C_TEST_INT64_TO_INT
      , of_type
          (create ~mode_annot:[ Checked ] ~types:[ t_int64 () ^~> t_int () ]) )
    ; ( C_INT
      , of_type
          (create ~mode_annot:[ Checked ] ~types:[ t_nat () ^~> t_int () ]) )
    ; ( C_ABS
      , of_type
          (create ~mode_annot:[ Checked ] ~types:[ t_int () ^~> t_nat () ]) )
    ; ( C_LIST_SIZE
      , of_type
          (for_all "a"
          @@ fun a ->
          create ~mode_annot:[ Inferred ] ~types:[ t_list a () ^~> t_nat () ]) )
    ; ( C_SET_SIZE
      , of_type
          (for_all "a"
          @@ fun a ->
          create ~mode_annot:[ Inferred ] ~types:[ t_set a () ^~> t_nat () ]) )
    ; ( C_MAP_SIZE
      , of_type
          (for_all "a"
          @@ fun a ->
          for_all "b"
          @@ fun b ->
          create ~mode_annot:[ Inferred ] ~types:[ t_map a b () ^~> t_nat () ])
      )
    ; ( C_SIZE
      , of_type
          (create
             ~mode_annot:[ Inferred ]
             ~types:[ t_string () ^~> t_nat (); t_bytes () ^~> t_nat () ]) )
    ; ( C_SLICE
      , of_type
          (create
             ~mode_annot:[ Checked; Checked; Inferred ]
             ~types:
               [ t_nat () ^-> t_nat () ^-> t_string () ^~> t_string ()
               ; t_nat () ^-> t_nat () ^-> t_bytes () ^~> t_bytes ()
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
              [ a ^-> t_map a b () ^~> t_bool ()
              ; a ^-> t_big_map a b () ^~> t_bool ()
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
      let create xs = List.Ne.of_list xs
      let ( ^~> ) arg_type ret_type = [ arg_type ], ret_type

      let ( ^-> ) arg_type (arg_types, ret_type) =
        arg_type :: arg_types, ret_type
    end
  end

  type ('err, 'wrn) t = Type.t list -> (Type.t, 'err, 'wrn) C.t

  let of_type (types : Annot.t) : _ t =
    let open C in
    let open Let_syntax in
    fun received_arg_types ->
      try_all
      @@ List.map
           (List.Ne.to_list types)
           ~f:(fun (expected_arg_types, ret_type) ->
             let%bind arg_types =
               match List.zip received_arg_types expected_arg_types with
               | Ok result -> return result
               | Unequal_lengths ->
                 raise
                   (corner_case
                      "Unequal lengths between mode annotation and argument \
                       types")
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


  let int_types : (Errors.typer_error, Main_warnings.all) t =
    let open Type in
    let open Annot.Syntax in
    of_type (create [ t_nat () ^~> t_int (); t_bls12_381_fr () ^~> t_int () ])


  let ediv_types : (Errors.typer_error, Main_warnings.all) t =
    let open Type in
    let open Annot.Syntax in
    of_type
      (create
         [ t_nat ()
           ^-> t_nat ()
           ^~> t_option (t_pair (t_nat ()) (t_nat ()) ()) ()
         ; t_int ()
           ^-> t_int ()
           ^~> t_option (t_pair (t_int ()) (t_nat ()) ()) ()
         ; t_nat ()
           ^-> t_int ()
           ^~> t_option (t_pair (t_int ()) (t_nat ()) ()) ()
         ; t_int ()
           ^-> t_nat ()
           ^~> t_option (t_pair (t_int ()) (t_nat ()) ()) ()
         ; t_mutez ()
           ^-> t_mutez ()
           ^~> t_option (t_pair (t_nat ()) (t_mutez ()) ()) ()
         ; t_mutez ()
           ^-> t_nat ()
           ^~> t_option (t_pair (t_mutez ()) (t_mutez ()) ()) ()
         ])


  let and_types : (Errors.typer_error, Main_warnings.all) t =
    let open Type in
    let open Annot.Syntax in
    of_type
      (create
         [ t_nat () ^-> t_nat () ^~> t_nat ()
         ; t_int () ^-> t_nat () ^~> t_nat ()
         ])
end
