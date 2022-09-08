module Ligo_proto = Environment.Protocols
module Option = Simple_utils.Option
module List = Simple_utils.List
module Trace = Simple_utils.Trace
module Elaboration = Context.Elaboration
open Trace
open Errors
module I = Ast_core
module O = Ast_typed
open O.Combinators
open Subtyping
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

module Comparable = struct
  open O

  type t =
    ctx:Context.t
    -> type_expression
    -> type_expression
    -> Context.t * type_expression

  let trace_compare ~raise ~loc a b ~in_ =
    Trace.try_with
      (fun ~raise ~catch:_ -> in_ ~raise)
      (fun ~catch:_ _ -> raise.error (uncomparable_types loc a b))


  (* [simple_comparator ~raise loc name] checks
     returns type [bool] if argument types are of the form [ t1; t2 ]
     where [t1 = t2] and [t1, t2] in [ address ; bool ; bytes ; chain_id; int; key; key_hash; mutez; nat ; ... ].
  *)
  let simple_comparator ~raise : Location.t -> string -> t =
    let simple_types =
      List.Ne.of_list
        [ t_address ()
        ; t_bool ()
        ; t_bytes ()
        ; t_chain_id ()
        ; t_int ()
        ; t_key ()
        ; t_key_hash ()
        ; t_mutez ()
        ; t_nat ()
        ; t_signature ()
        ; t_string ()
        ; t_timestamp ()
        ; t_unit ()
        ; t_never ()
        ; t_michelson_code ()
        ]
    in
    fun loc _s ~ctx a b ->
      let ctx =
        trace_compare ~raise ~loc a b ~in_:(fun ~raise ->
          Trace.bind_exists ~raise
          @@ List.Ne.map
               (fun type_ ~raise ->
                 let ctx = unify ~raise ~loc ~ctx a type_ in
                 let ctx = unify ~raise ~loc ~ctx b type_ in
                 ctx)
               simple_types)
      in
      ctx, t_bool ()


  (* [record_comparator] is a simple extension of [comparator] to record types. *)
  let rec record_comparator ~raise ~test : Location.t -> string -> t =
   fun loc s ~ctx a b ->
    let ctx =
      trace_compare ~raise ~loc a b ~in_:(fun ~raise ->
        unify ~raise ~loc ~ctx a b)
    in
    let a_r =
      trace_option ~raise (comparator_composed loc a) @@ get_t_record a
    in
    let b_r =
      trace_option ~raise (comparator_composed loc b) @@ get_t_record b
    in
    let aux ctx (a : _ Rows.row_element_mini_c) (b : _ Rows.row_element_mini_c)
      : Context.t
      =
      let ctx, _ =
        comparator
          ~cmp:s
          ~raise
          ~test
          ~loc
          ~ctx
          a.associated_type
          b.associated_type
      in
      ctx
    in
    let ctx =
      List.fold2_exn
        ~init:ctx
        ~f:aux
        (Record.LMap.to_list a_r.fields)
        (Record.LMap.to_list b_r.fields)
    in
    ctx, t_bool ()


  (* [sum_comparator] is a simple extension of [comparator] to sum types. *)
  and sum_comparator ~raise ~test : Location.t -> string -> t =
   fun loc s ~ctx a b ->
    let ctx =
      trace_compare ~raise ~loc a b ~in_:(fun ~raise ->
        unify ~raise ~loc ~ctx a b)
    in
    let a_r = trace_option ~raise (comparator_composed loc a) @@ get_t_sum a in
    let b_r = trace_option ~raise (comparator_composed loc b) @@ get_t_sum b in
    let aux ctx (a : _ Rows.row_element_mini_c) (b : _ Rows.row_element_mini_c)
      : Context.t
      =
      let ctx, _ =
        comparator
          ~cmp:s
          ~raise
          ~test
          ~loc
          ~ctx
          a.associated_type
          b.associated_type
      in
      ctx
    in
    let ctx =
      List.fold2_exn
        ~init:ctx
        ~f:aux
        (Record.LMap.to_list a_r.fields)
        (Record.LMap.to_list b_r.fields)
    in
    ctx, t_bool ()


  and list_comparator ~raise ~test : Location.t -> string -> t =
   fun loc s ~ctx a_lst b_lst ->
    let ctx =
      trace_compare ~raise ~loc a_lst b_lst ~in_:(fun ~raise ->
        unify ~raise ~loc ~ctx a_lst b_lst)
    in
    let a =
      trace_option ~raise (comparator_composed loc a_lst) @@ get_t_list a_lst
    in
    let b =
      trace_option ~raise (comparator_composed loc b_lst) @@ get_t_list b_lst
    in
    comparator ~cmp:s ~raise ~test ~loc ~ctx a b


  and set_comparator ~raise ~test : Location.t -> string -> t =
   fun loc s ~ctx a_set b_set ->
    let ctx =
      trace_compare ~raise ~loc a_set b_set ~in_:(fun ~raise ->
        unify ~raise ~loc ~ctx a_set b_set)
    in
    let a =
      trace_option ~raise (comparator_composed loc a_set) @@ get_t_set a_set
    in
    let b =
      trace_option ~raise (comparator_composed loc b_set) @@ get_t_set b_set
    in
    comparator ~cmp:s ~raise ~test ~loc ~ctx a b


  and map_comparator ~raise ~test : Location.t -> string -> t =
   fun loc s ~ctx a_map b_map ->
    let ctx =
      trace_compare ~raise ~loc a_map b_map ~in_:(fun ~raise ->
        unify ~raise ~loc ~ctx a_map b_map)
    in
    let a_key, a_value =
      trace_option ~raise (comparator_composed loc a_map) @@ get_t_map a_map
    in
    let b_key, b_value =
      trace_option ~raise (comparator_composed loc b_map) @@ get_t_map b_map
    in
    let ctx, _ = comparator ~cmp:s ~raise ~test ~loc ~ctx a_key b_key in
    let ctx, _ = comparator ~cmp:s ~raise ~test ~loc ~ctx a_value b_value in
    ctx, t_bool ()


  and big_map_comparator ~raise ~test : Location.t -> string -> t =
   fun loc s ~ctx a_map b_map ->
    let ctx =
      trace_compare ~raise ~loc a_map b_map ~in_:(fun ~raise ->
        unify ~raise ~loc ~ctx a_map b_map)
    in
    let a_key, a_value =
      trace_option ~raise (comparator_composed loc a_map) @@ get_t_big_map a_map
    in
    let b_key, b_value =
      trace_option ~raise (comparator_composed loc b_map) @@ get_t_big_map b_map
    in
    let ctx, _ = comparator ~cmp:s ~raise ~test ~loc ~ctx a_key b_key in
    let ctx, _ = comparator ~cmp:s ~raise ~test ~loc ~ctx a_value b_value in
    ctx, t_bool ()


  and comparator ~cmp ~raise ~test ~loc : t =
   fun ~ctx a b ->
    if test
    then
      bind_exists ~raise
      @@ List.Ne.of_list
           [ list_comparator ~test loc cmp ~ctx a b
           ; set_comparator ~test loc cmp ~ctx a b
           ; map_comparator ~test loc cmp ~ctx a b
           ; simple_comparator loc cmp ~ctx a b
           ; record_comparator ~test loc cmp ~ctx a b
           ; sum_comparator ~test loc cmp ~ctx a b
           ; big_map_comparator ~test loc cmp ~ctx a b
           ]
    else
      bind_exists ~raise
      @@ List.Ne.of_list
           [ simple_comparator loc cmp ~ctx a b
           ; record_comparator ~test loc cmp ~ctx a b
           ; sum_comparator ~test loc cmp ~ctx a b
           ]
end

module Type = struct
  type mode =
    | Inferred
    | Checked

  type t =
    { for_alls : (O.type_variable * Kind.t) list
    ; mode_annot : mode list
    ; types : (O.type_expression list * O.type_expression) List.Ne.t
    }

  module Syntax = struct
    let for_all tvar ?(kind = (Type : Kind.t)) in_ =
      let tvar = Type_var.of_input_var ("'" ^ tvar) in
      let result = in_ (O.t_variable tvar ()) in
      { result with for_alls = (tvar, kind) :: result.for_alls }


    let create ~mode_annot ~types =
      { for_alls = []; mode_annot; types = List.Ne.of_list types }


    let return ret_type = [], ret_type
    let ( ^~> ) arg_type ret_type = [ arg_type ], ret_type
    let ( ^-> ) arg_type (arg_types, ret_type) = arg_type :: arg_types, ret_type
    let ( @-> ) t1 t2 = O.t_arrow t1 t2 ()
  end
end

type ('err, 'wrn) infer =
  raise:('err, 'wrn) raise
  -> ctx:Context.t
  -> I.expression
  -> Context.t * O.type_expression * (O.expression, 'err, 'wrn) Elaboration.t

type ('err, 'wrn) check =
  raise:('err, 'wrn) raise
  -> ctx:Context.t
  -> I.expression
  -> O.type_expression
  -> Context.t * (O.expression, 'err, 'wrn) Elaboration.t

type ('err, 'wrn) t =
  raise:('err, 'wrn) raise
  -> options:Compiler_options.middle_end
  -> infer:('err, 'wrn) infer
  -> check:('err, 'wrn) check
  -> loc:Location.t
  -> ctx:Context.t
  -> I.expression list
  -> Context.t
     * (O.expression list, 'err, 'wrn) Elaboration.t
     * O.type_expression

let t_subst t ~tvar ~type_ = O.Helpers.subst_no_capture_type tvar type_ t

let t_exists (evar : Exists_var.t) =
  t_variable ~loc:(Exists_var.loc evar) (evar :> O.type_variable) ()


let t_subst_var t ~tvar ~tvar' = t_subst t ~tvar ~type_:(t_variable tvar' ())
let t_subst_evar t ~tvar ~evar = t_subst t ~tvar ~type_:(t_exists evar)

let of_type ({ for_alls; mode_annot; types } : Type.t) : _ t =
  (* The function [mode] is used to determine the mode of an argument at position [i] *)
  let mode =
    let table = Hashtbl.create (module Int) in
    List.iteri mode_annot ~f:(fun i mode -> Hashtbl.set table ~key:i ~data:mode);
    fun ~raise i ->
      try Hashtbl.find_exn table i with
      | _ -> raise.error (corner_case "bad mode annot")
  in
  fun ~raise ~options:_ ~infer ~check ~loc ~ctx args ->
    (* Instantiate prenex quantifier *)
    let exists =
      List.map for_alls ~f:(fun (tvar, kind) ->
        tvar, kind, Exists_var.fresh ~loc ())
    in
    let subst type_ =
      List.fold exists ~init:type_ ~f:(fun type_ (tvar, _, evar) ->
        t_subst_evar type_ ~tvar ~evar)
    in
    let ctx =
      Context.(
        ctx
        |@ of_list
             (List.map exists ~f:(fun (_, kind, evar) ->
                C_exists_var (evar, kind))))
    in
    (* Determine arguments to be inferred *)
    let inferred =
      List.filter_mapi args ~f:(fun i arg ->
        match mode ~raise i with
        | Inferred -> Some (i, arg)
        | Checked -> None)
    in
    (* [output_args] table is used to store elaborated arguments. Later used to sort arguments by position. *)
    let output_args = Hashtbl.create (module Int) in
    (* Infer the inferred arguments *)
    let ctx =
      List.fold inferred ~init:ctx ~f:(fun ctx (i, arg) ->
        let ctx, arg_type, arg = infer ~raise ~ctx arg in
        Hashtbl.set output_args ~key:i ~data:(arg_type, arg);
        ctx)
    in
    (* Select type using try-based unification on inferred types *)
    let ctx, checked, ret_type =
      Trace.bind_exists ~raise
      @@ List.Ne.map
           (fun (arg_types, ret_type) ~raise ->
             let arg_types = List.map arg_types ~f:subst in
             let ret_type = subst ret_type in
             (* Split types accordingly for [arg_types] *)
             let args_types =
               match List.zip arg_types args with
               | Ok result -> result
               | Unequal_lengths ->
                 raise.error
                   (corner_case
                      "Unequal lengths between mode annotation and argument \
                       types")
             in
             let unify_worklist, checked =
               args_types
               |> List.mapi ~f:(fun i x -> i, x)
               |> List.partition_map ~f:(fun (i, (arg_type, arg)) ->
                    match (mode ~raise i : Type.mode) with
                    | Inferred ->
                      (* [Hashtbl.find_exn] cannot raise error due to inference above. *)
                      First (arg_type, fst (Hashtbl.find_exn output_args i))
                    | Checked -> Second (i, arg_type, arg))
             in
             (* Unify the inferred types *)
             let ctx =
               List.fold
                 unify_worklist
                 ~init:ctx
                 ~f:(fun ctx (arg_type1, arg_type2) ->
                 unify
                   ~raise
                   ~loc
                   ~ctx
                   (Context.apply ctx arg_type1)
                   (Context.apply ctx arg_type2))
             in
             ctx, checked, ret_type)
           types
    in
    (* Check the checked arguments *)
    let ctx =
      List.fold checked ~init:ctx ~f:(fun ctx (i, arg_type, arg) ->
        let ctx, arg = check ~raise ~ctx arg (Context.apply ctx arg_type) in
        Hashtbl.set output_args ~key:i ~data:(arg_type, arg);
        ctx)
    in
    (* Reconstruct arguments using [output_args] *)
    let args =
      List.mapi args ~f:(fun i _ -> snd (Hashtbl.find_exn output_args i))
      |> Elaboration.all
    in
    ctx, args, ret_type


let of_comparator comparator : _ t =
 fun ~raise ~options ~infer ~check:_ ~loc ~ctx args ->
  match args with
  | [ arg1; arg2 ] ->
    let ctx, arg1_type, arg1 = infer ~raise ~ctx arg1 in
    let ctx, arg2_type, arg2 = infer ~raise ~ctx arg2 in
    let ctx, ret_type =
      comparator
        ~raise
        ~test:options.test
        ~loc
        ~ctx
        (Context.apply ctx arg1_type)
        (Context.apply ctx arg2_type)
    in
    ctx, Elaboration.all [ arg1; arg2 ], ret_type
  | _ ->
    raise.error
      (corner_case
      @@ Format.asprintf
           "Unequal length between comparator arguments and applied arguments")


module Const_map = Simple_utils.Map.Make (struct
  type t = Constant.constant'

  let compare x y = Constant.compare_constant' x y
end)

let constant_typer_tbl : (Errors.typer_error, Main_warnings.all) t Const_map.t =
  let open O.Combinators in
  let open Type.Syntax in
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
            ~types:[ (a @-> t_sum_ez [ "left", a; "right", b ]) ^-> a ^~> b ]) )
    ; ( C_LEFT
      , of_type
          (for_all "a"
          @@ fun a ->
          create
            ~mode_annot:[ Inferred ]
            ~types:[ a ^~> t_sum_ez [ "left", a; "right", a ] ]) )
    ; ( C_LOOP_CONTINUE
      , of_type
          (for_all "a"
          @@ fun a ->
          create
            ~mode_annot:[ Inferred ]
            ~types:[ a ^~> t_sum_ez [ "left", a; "right", a ] ]) )
    ; ( C_LOOP_STOP
      , of_type
          (for_all "a"
          @@ fun a ->
          create
            ~mode_annot:[ Inferred ]
            ~types:[ a ^~> t_sum_ez [ "left", a; "right", a ] ]) )
    ; ( C_ITER
      , of_type
          (for_all "a"
          @@ fun a ->
          for_all "b"
          @@ fun b ->
          create
            ~mode_annot:[ Checked; Inferred ]
            ~types:
              [ (a @-> t_unit ()) ^-> t_list a ^~> t_unit ()
              ; (a @-> t_unit ()) ^-> t_set a ^~> t_unit ()
              ; (t_pair a b @-> t_unit ()) ^-> t_map a b ^~> t_unit ()
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
              [ (t_pair a b @-> a) ^-> t_list b ^-> a ^~> a
              ; (t_pair a b @-> a) ^-> t_set b ^-> a ^~> a
              ]) )
      (* Map *)
    ; ( C_MAP_EMPTY
      , of_type
          (for_all "a"
          @@ fun a ->
          for_all "b"
          @@ fun b -> create ~mode_annot:[] ~types:[ return (t_map a b) ]) )
    ; ( C_BIG_MAP_EMPTY
      , of_type
          (for_all "a"
          @@ fun a ->
          for_all "b"
          @@ fun b -> create ~mode_annot:[] ~types:[ return (t_big_map a b) ]) )
    ; ( C_MAP_ADD
      , of_type
          (for_all "a"
          @@ fun a ->
          for_all "b"
          @@ fun b ->
          create
            ~mode_annot:[ Checked; Checked; Inferred ]
            ~types:
              [ a ^-> b ^-> t_map a b ^~> t_map a b
              ; a ^-> b ^-> t_big_map a b ^~> t_big_map a b
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
              [ a ^-> t_map a b ^~> t_map a b
              ; a ^-> t_big_map a b ^~> t_big_map a b
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
              [ a ^-> t_option b ^-> t_map a b ^~> t_map a b
              ; a ^-> t_option b ^-> t_big_map a b ^~> t_big_map a b
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
                ^-> t_option b
                ^-> t_map a b
                ^~> t_pair (t_option b) (t_map a b)
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
                ^-> t_option b
                ^-> t_big_map a b
                ^~> t_pair (t_option b) (t_big_map a b)
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
              [ a ^-> t_map a b ^~> t_option b
              ; a ^-> t_big_map a b ^~> t_option b
              ]) )
    ; ( C_MAP_FIND
      , of_type
          (for_all "a"
          @@ fun a ->
          for_all "b"
          @@ fun b ->
          create
            ~mode_annot:[ Checked; Inferred ]
            ~types:[ a ^-> t_map a b ^~> b; a ^-> t_big_map a b ^~> b ]) )
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
            ~types:[ (t_pair a b @-> c) ^-> t_map a b ^~> t_map a c ]) )
    ; ( C_MAP_ITER
      , of_type
          (for_all "a"
          @@ fun a ->
          for_all "b"
          @@ fun b ->
          create
            ~mode_annot:[ Checked; Inferred ]
            ~types:[ (t_pair a b @-> t_unit ()) ^-> t_map a b ^~> t_unit () ]) )
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
            ~types:[ (t_pair c (t_pair a b) @-> c) ^-> t_map a b ^-> c ^~> c ])
      )
      (* List *)
    ; ( C_LIST_EMPTY
      , of_type
          (for_all "a"
          @@ fun a -> create ~mode_annot:[] ~types:[ return @@ t_list a ]) )
    ; ( C_CONS
      , of_type
          (for_all "a"
          @@ fun a ->
          create
            ~mode_annot:[ Inferred; Checked ]
            ~types:[ a ^-> t_list a ^~> t_list a ]) )
    ; ( C_LIST_MAP
      , of_type
          (for_all "a"
          @@ fun a ->
          for_all "b"
          @@ fun b ->
          create
            ~mode_annot:[ Checked; Inferred ]
            ~types:[ (a @-> b) ^-> t_list a ^~> t_list b ]) )
    ; ( C_LIST_ITER
      , of_type
          (for_all "a"
          @@ fun a ->
          create
            ~mode_annot:[ Checked; Inferred ]
            ~types:[ (a @-> t_unit ()) ^-> t_list a ^~> t_unit () ]) )
    ; ( C_LIST_FOLD
      , of_type
          (for_all "a"
          @@ fun a ->
          for_all "b"
          @@ fun b ->
          create
            ~mode_annot:[ Checked; Inferred; Inferred ]
            ~types:[ (t_pair b a @-> b) ^-> t_list a ^-> b ^~> b ]) )
    ; ( C_LIST_FOLD_LEFT
      , of_type
          (for_all "a"
          @@ fun a ->
          for_all "b"
          @@ fun b ->
          create
            ~mode_annot:[ Checked; Inferred; Inferred ]
            ~types:[ (t_pair b a @-> b) ^-> b ^-> t_list a ^~> b ]) )
    ; ( C_LIST_FOLD_RIGHT
      , of_type
          (for_all "a"
          @@ fun a ->
          for_all "b"
          @@ fun b ->
          create
            ~mode_annot:[ Checked; Inferred; Inferred ]
            ~types:[ (t_pair a b @-> b) ^-> t_list a ^-> b ^~> b ]) )
      (* Set *)
    ; ( C_SET_EMPTY
      , of_type
          (for_all "a"
          @@ fun a -> create ~mode_annot:[] ~types:[ return @@ t_set a ]) )
    ; ( C_SET_LITERAL
      , of_type
          (for_all "a"
          @@ fun a ->
          create ~mode_annot:[ Inferred ] ~types:[ t_list a ^~> t_set a ]) )
    ; ( C_SET_MEM
      , of_type
          (for_all "a"
          @@ fun a ->
          create
            ~mode_annot:[ Inferred; Checked ]
            ~types:[ a ^-> t_set a ^~> t_bool () ]) )
    ; ( C_SET_ADD
      , of_type
          (for_all "a"
          @@ fun a ->
          create
            ~mode_annot:[ Inferred; Checked ]
            ~types:[ a ^-> t_set a ^~> t_set a ]) )
    ; ( C_SET_REMOVE
      , of_type
          (for_all "a"
          @@ fun a ->
          create
            ~mode_annot:[ Inferred; Checked ]
            ~types:[ a ^-> t_set a ^~> t_set a ]) )
    ; ( C_SET_UPDATE
      , of_type
          (for_all "a"
          @@ fun a ->
          create
            ~mode_annot:[ Inferred; Checked; Checked ]
            ~types:[ a ^-> t_bool () ^-> t_set a ^~> t_set a ]) )
    ; ( C_SET_ITER
      , of_type
          (for_all "a"
          @@ fun a ->
          create
            ~mode_annot:[ Checked; Inferred ]
            ~types:[ (a @-> t_unit ()) ^-> t_set a ^~> t_unit () ]) )
    ; ( C_SET_FOLD
      , of_type
          (for_all "a"
          @@ fun a ->
          for_all "b"
          @@ fun b ->
          create
            ~mode_annot:[ Checked; Inferred; Inferred ]
            ~types:[ (t_pair b a @-> b) ^-> t_set a ^-> b ^~> b ]) )
    ; ( C_SET_FOLD_DESC
      , of_type
          (for_all "a"
          @@ fun a ->
          for_all "b"
          @@ fun b ->
          create
            ~mode_annot:[ Checked; Inferred; Inferred ]
            ~types:[ (t_pair a b @-> b) ^-> t_set a ^-> b ^~> b ]) )
      (* Bytes *)
    ; ( C_CONCAT
      , of_type
          (create
             ~mode_annot:[ Inferred; Inferred ]
             ~types:
               [ t_string () ^-> t_string () ^~> t_string ()
               ; t_bytes () ^-> t_bytes () ^~> t_bytes ()
               ]) )
    ; ( C_BYTES_UNPACK
      , of_type
          (for_all "a"
          @@ fun a ->
          create ~mode_annot:[ Checked ] ~types:[ t_bytes () ^~> t_option a ]) )
      (* Option *)
    ; ( C_NONE
      , of_type
          (for_all "a"
          @@ fun a -> create ~mode_annot:[] ~types:[ return @@ t_option a ]) )
    ; ( C_SOME
      , of_type
          (for_all "a"
          @@ fun a ->
          create ~mode_annot:[ Inferred ] ~types:[ a ^~> t_option a ]) )
    ; ( C_UNOPT
      , of_type
          (for_all "a"
          @@ fun a ->
          create ~mode_annot:[ Inferred ] ~types:[ t_option a ^~> a ]) )
    ; ( C_UNOPT_WITH_ERROR
      , of_type
          (for_all "a"
          @@ fun a ->
          create
            ~mode_annot:[ Inferred; Checked ]
            ~types:[ t_option a ^-> t_string () ^~> a ]) )
    ; ( C_OPTION_MAP
      , of_type
          (for_all "a"
          @@ fun a ->
          for_all "b"
          @@ fun b ->
          create
            ~mode_annot:[ Checked; Inferred ]
            ~types:[ (a @-> b) ^-> t_option a ^~> t_option b ]) )
      (* Contract *)
    ; ( C_ADDRESS
      , of_type
          (for_all "a"
          @@ fun a ->
          create
            ~mode_annot:[ Checked ]
            ~types:[ t_contract a ^~> t_address () ]) )
    ; ( C_CONTRACT
      , of_type
          (for_all "a"
          @@ fun a ->
          create
            ~mode_annot:[ Checked ]
            ~types:[ t_address () ^~> t_contract a ]) )
    ; ( C_CONTRACT_OPT
      , of_type
          (for_all "a"
          @@ fun a ->
          create
            ~mode_annot:[ Checked ]
            ~types:[ t_address () ^~> t_option (t_contract a) ]) )
    ; ( C_CONTRACT_WITH_ERROR
      , of_type
          (for_all "a"
          @@ fun a ->
          create
            ~mode_annot:[ Checked; Checked ]
            ~types:[ t_address () ^-> t_string () ^~> t_contract a ]) )
    ; ( C_CONTRACT_ENTRYPOINT_OPT
      , of_type
          (for_all "a"
          @@ fun a ->
          create
            ~mode_annot:[ Checked; Checked ]
            ~types:[ t_string () ^-> t_address () ^~> t_option (t_contract a) ]
          ) )
    ; ( C_CONTRACT_ENTRYPOINT
      , of_type
          (for_all "a"
          @@ fun a ->
          create
            ~mode_annot:[ Checked; Checked ]
            ~types:[ t_string () ^-> t_address () ^~> t_contract a ]) )
    ; ( C_IMPLICIT_ACCOUNT
      , of_type
          (create
             ~mode_annot:[ Checked ]
             ~types:[ t_key_hash () ^~> t_contract (t_unit ()) ]) )
    ; ( C_SET_DELEGATE
      , of_type
          (create
             ~mode_annot:[ Checked ]
             ~types:[ t_option (t_key_hash ()) ^~> t_operation () ]) )
    ; ( C_SELF
      , of_type
          (for_all "a"
          @@ fun a ->
          create ~mode_annot:[ Checked ] ~types:[ t_string () ^~> t_contract a ]
          ) )
    ; ( C_SELF_ADDRESS
      , of_type (create ~mode_annot:[] ~types:[ return @@ t_address () ]) )
    ; ( C_CALL
      , of_type
          (for_all "a"
          @@ fun a ->
          create
            ~mode_annot:[ Checked; Checked; Inferred ]
            ~types:[ a ^-> t_mutez () ^-> t_contract a ^~> t_operation () ]) )
    ; ( C_CREATE_CONTRACT
      , of_type
          (for_all "a"
          @@ fun a ->
          for_all "b"
          @@ fun b ->
          create
            ~mode_annot:[ Checked; Checked; Checked; Inferred ]
            ~types:
              [ (t_pair a b @-> t_pair (t_list (t_operation ())) b)
                ^-> t_option (t_key_hash ())
                ^-> t_mutez ()
                ^-> b
                ^~> t_pair (t_operation ()) (t_address ())
              ]) )
    ; ( C_EMIT_EVENT
      , of_type
          (for_all "a"
          @@ fun a ->
          create
            ~mode_annot:[ Checked; Inferred ]
            ~types:[ t_string () ^-> a ^~> t_operation () ]) )
      (* Primitives *)
    ; C_UNIT, of_type (create ~mode_annot:[] ~types:[ return @@ t_unit () ])
    ; C_TRUE, of_type (create ~mode_annot:[] ~types:[ return @@ t_bool () ])
    ; C_FALSE, of_type (create ~mode_annot:[] ~types:[ return @@ t_bool () ])
      (* Views & Chests *)
    ; ( C_OPEN_CHEST
      , of_type
          (create
             ~mode_annot:[ Checked; Checked; Checked ]
             ~types:
               [ t_chest_key ()
                 ^-> t_chest ()
                 ^-> t_nat ()
                 ^~> t_chest_opening_result ()
               ]) )
    ; ( C_VIEW
      , of_type
          (for_all "a"
          @@ fun a ->
          for_all "b"
          @@ fun b ->
          create
            ~mode_annot:[ Checked; Inferred; Checked ]
            ~types:[ t_string () ^-> a ^-> t_address () ^~> t_option b ]) )
    ; ( C_POLYMORPHIC_ADD
      , of_type
          (create
             ~mode_annot:[ Inferred; Inferred ]
             ~types:
               [ O.(t_string () ^-> t_string () ^~> t_string ())
               ; O.(
                   t_bls12_381_g1 () ^-> t_bls12_381_g1 () ^~> t_bls12_381_g1 ())
               ; O.(
                   t_bls12_381_g2 () ^-> t_bls12_381_g2 () ^~> t_bls12_381_g2 ())
               ; O.(
                   t_bls12_381_fr () ^-> t_bls12_381_fr () ^~> t_bls12_381_fr ())
               ; O.(t_nat () ^-> t_nat () ^~> t_nat ())
               ; O.(t_int () ^-> t_int () ^~> t_int ())
               ; O.(t_mutez () ^-> t_mutez () ^~> t_mutez ())
               ; O.(t_nat () ^-> t_int () ^~> t_int ())
               ; O.(t_int () ^-> t_nat () ^~> t_int ())
               ; O.(t_timestamp () ^-> t_int () ^~> t_timestamp ())
               ; O.(t_int () ^-> t_timestamp () ^~> t_timestamp ())
               ]) )
    ; ( C_POLYMORPHIC_SUB
      , of_type
          (create
             ~mode_annot:[ Inferred; Inferred ]
             ~types:
               [ O.(
                   t_bls12_381_g1 () ^-> t_bls12_381_g1 () ^~> t_bls12_381_g1 ())
               ; O.(
                   t_bls12_381_g2 () ^-> t_bls12_381_g2 () ^~> t_bls12_381_g2 ())
               ; O.(
                   t_bls12_381_fr () ^-> t_bls12_381_fr () ^~> t_bls12_381_fr ())
               ; O.(t_nat () ^-> t_nat () ^~> t_int ())
               ; O.(t_int () ^-> t_int () ^~> t_int ())
               ; O.(t_nat () ^-> t_int () ^~> t_int ())
               ; O.(t_int () ^-> t_nat () ^~> t_int ())
               ; O.(t_timestamp () ^-> t_timestamp () ^~> t_int ())
               ; O.(t_timestamp () ^-> t_int () ^~> t_timestamp ())
               ; O.(t_mutez () ^-> t_mutez () ^~> t_option (t_mutez ()))
               ]) )
    ; ( C_ADD
      , of_type
          (create
             ~mode_annot:[ Inferred; Inferred ]
             ~types:
               [ O.(
                   t_bls12_381_g1 () ^-> t_bls12_381_g1 () ^~> t_bls12_381_g1 ())
               ; O.(
                   t_bls12_381_g2 () ^-> t_bls12_381_g2 () ^~> t_bls12_381_g2 ())
               ; O.(
                   t_bls12_381_fr () ^-> t_bls12_381_fr () ^~> t_bls12_381_fr ())
               ; O.(t_nat () ^-> t_nat () ^~> t_nat ())
               ; O.(t_int () ^-> t_int () ^~> t_int ())
               ; O.(t_mutez () ^-> t_mutez () ^~> t_mutez ())
               ; O.(t_nat () ^-> t_int () ^~> t_int ())
               ; O.(t_int () ^-> t_nat () ^~> t_int ())
               ; O.(t_timestamp () ^-> t_int () ^~> t_timestamp ())
               ; O.(t_int () ^-> t_timestamp () ^~> t_timestamp ())
               ]) )
    ; ( C_MUL
      , of_type
          (create
             ~mode_annot:[ Inferred; Inferred ]
             ~types:
               [ O.(
                   t_bls12_381_g1 () ^-> t_bls12_381_fr () ^~> t_bls12_381_g1 ())
               ; O.(
                   t_bls12_381_g2 () ^-> t_bls12_381_fr () ^~> t_bls12_381_g2 ())
               ; O.(
                   t_bls12_381_fr () ^-> t_bls12_381_fr () ^~> t_bls12_381_fr ())
               ; O.(t_nat () ^-> t_bls12_381_fr () ^~> t_bls12_381_fr ())
               ; O.(t_int () ^-> t_bls12_381_fr () ^~> t_bls12_381_fr ())
               ; O.(t_bls12_381_fr () ^-> t_nat () ^~> t_bls12_381_fr ())
               ; O.(t_bls12_381_fr () ^-> t_int () ^~> t_bls12_381_fr ())
               ; O.(t_nat () ^-> t_nat () ^~> t_nat ())
               ; O.(t_int () ^-> t_int () ^~> t_int ())
               ; O.(t_nat () ^-> t_mutez () ^~> t_mutez ())
               ; O.(t_mutez () ^-> t_nat () ^~> t_mutez ())
               ; O.(t_int () ^-> t_nat () ^~> t_int ())
               ; O.(t_nat () ^-> t_int () ^~> t_int ())
               ]) )
    ; ( C_SUB
      , of_type
          (create
             ~mode_annot:[ Inferred; Inferred ]
             ~types:
               [ O.(
                   t_bls12_381_g1 () ^-> t_bls12_381_fr () ^~> t_bls12_381_g1 ())
               ; O.(
                   t_bls12_381_g2 () ^-> t_bls12_381_fr () ^~> t_bls12_381_g2 ())
               ; O.(
                   t_bls12_381_fr () ^-> t_bls12_381_fr () ^~> t_bls12_381_fr ())
               ; O.(t_nat () ^-> t_nat () ^~> t_int ())
               ; O.(t_int () ^-> t_int () ^~> t_int ())
               ; O.(t_int () ^-> t_nat () ^~> t_int ())
               ; O.(t_nat () ^-> t_int () ^~> t_int ())
               ; O.(t_timestamp () ^-> t_timestamp () ^~> t_int ())
               ; O.(t_timestamp () ^-> t_int () ^~> t_timestamp ())
               ; O.(t_mutez () ^-> t_mutez () ^~> t_mutez ())
               ]) )
    ; ( C_SUB_MUTEZ
      , of_type
          (create
             ~mode_annot:[ Checked; Checked ]
             ~types:[ t_mutez () ^-> t_mutez () ^~> t_option (t_mutez ()) ]) )
    ; ( C_DIV
      , of_type
          (create
             ~mode_annot:[ Inferred; Inferred ]
             ~types:
               [ O.(t_nat () ^-> t_nat () ^~> t_nat ())
               ; O.(t_int () ^-> t_int () ^~> t_int ())
               ; O.(t_nat () ^-> t_int () ^~> t_int ())
               ; O.(t_int () ^-> t_nat () ^~> t_int ())
               ; O.(t_mutez () ^-> t_nat () ^~> t_mutez ())
               ; O.(t_mutez () ^-> t_mutez () ^~> t_nat ())
               ]) )
    ; ( C_MOD
      , of_type
          (create
             ~mode_annot:[ Inferred; Inferred ]
             ~types:
               [ O.(t_nat () ^-> t_nat () ^~> t_nat ())
               ; O.(t_nat () ^-> t_int () ^~> t_nat ())
               ; O.(t_int () ^-> t_nat () ^~> t_nat ())
               ; O.(t_int () ^-> t_int () ^~> t_nat ())
               ; O.(t_mutez () ^-> t_nat () ^~> t_mutez ())
               ; O.(t_mutez () ^-> t_mutez () ^~> t_mutez ())
               ]) )
    ; ( C_NEG
      , of_type
          (create
             ~mode_annot:[ Inferred ]
             ~types:
               [ O.(t_int () ^~> t_int ())
               ; O.(t_nat () ^~> t_int ())
               ; O.(t_bls12_381_g1 () ^~> t_bls12_381_g1 ())
               ; O.(t_bls12_381_g2 () ^~> t_bls12_381_g2 ())
               ; O.(t_bls12_381_fr () ^~> t_bls12_381_fr ())
               ]) )
      (* Logical operators *)
    ; ( C_NOT
      , of_type
          (create
             ~mode_annot:[ Inferred ]
             ~types:
               [ O.(t_bool () ^~> t_bool ())
               ; O.(t_int () ^~> t_int ())
               ; O.(t_nat () ^~> t_int ())
               ]) )
    ; ( C_AND
      , of_type
          (create
             ~mode_annot:[ Inferred; Inferred ]
             ~types:
               [ O.(t_bool () ^-> t_bool () ^~> t_bool ())
               ; O.(t_nat () ^-> t_nat () ^~> t_nat ())
               ; O.(t_int () ^-> t_nat () ^~> t_nat ())
               ]) )
    ; ( C_OR
      , of_type
          (create
             ~mode_annot:[ Inferred; Inferred ]
             ~types:
               [ O.(t_bool () ^-> t_bool () ^~> t_bool ())
               ; O.(t_nat () ^-> t_nat () ^~> t_nat ())
               ]) )
    ; ( C_XOR
      , of_type
          (create
             ~mode_annot:[ Inferred; Inferred ]
             ~types:
               [ O.(t_bool () ^-> t_bool () ^~> t_bool ())
               ; O.(t_nat () ^-> t_nat () ^~> t_nat ())
               ]) )
    ; ( C_LSL
      , of_type
          (create
             ~mode_annot:[ Inferred; Inferred ]
             ~types:[ t_nat () ^-> t_nat () ^~> t_nat () ]) )
    ; ( C_LSR
      , of_type
          (create
             ~mode_annot:[ Inferred; Inferred ]
             ~types:[ t_nat () ^-> t_nat () ^~> t_nat () ]) )
      (* Tests *)
    ; ( C_TEST_COMPILE_CONTRACT
      , of_type
          (for_all "a"
          @@ fun a ->
          for_all "b"
          @@ fun b ->
          create
            ~mode_annot:[ Inferred ]
            ~types:
              [ (t_pair a b @-> t_pair (t_list (t_operation ())) b)
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
              [ (t_pair a b @-> t_pair (t_list (t_operation ())) b)
                ^-> b
                ^-> t_mutez ()
                ^~> t_unit ()
              ]) )
    ; ( C_TEST_LAST_ORIGINATIONS
      , of_type
          (create
             ~mode_annot:[ Checked ]
             ~types:
               [ t_unit () ^~> t_map (t_address ()) (t_list (t_address ())) ]) )
    ; ( C_TEST_LAST_EVENTS
      , of_type
          (for_all "a"
          @@ fun a ->
          create
            ~mode_annot:[ Checked ]
            ~types:[ t_string () ^~> t_list (t_pair (t_address ()) a) ]) )
    ; ( C_TEST_NTH_BOOTSTRAP_TYPED_ADDRESS
      , of_type
          (for_all "a"
          @@ fun a ->
          for_all "b"
          @@ fun b ->
          create
            ~mode_annot:[ Checked ]
            ~types:[ t_nat () ^~> t_typed_address a b ]) )
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
               [ t_int () ^~> t_triplet (t_address ()) (t_key ()) (t_string ())
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
          create ~mode_annot:[ Inferred ] ~types:[ a ^~> t_string () ]) )
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
               [ t_option (t_timestamp ())
                 ^-> t_nat ()
                 ^-> t_list (t_mutez ())
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
            ~types:[ t_address () ^~> t_typed_address a b ]) )
    ; ( C_TEST_RANDOM
      , of_type
          (for_all "a"
          @@ fun a ->
          create ~mode_annot:[ Checked ] ~types:[ t_bool () ^~> t_gen a ]) )
    ; ( C_TEST_GENERATOR_EVAL
      , of_type
          (for_all "a"
          @@ fun a -> create ~mode_annot:[ Checked ] ~types:[ t_gen a ^~> a ]) )
    ; ( C_TEST_MUTATE_CONTRACT
      , of_type
          (create
             ~mode_annot:[ Checked; Checked ]
             ~types:
               [ t_nat ()
                 ^-> t_ast_contract ()
                 ^~> t_option (t_pair (t_ast_contract ()) (t_mutation ()))
               ]) )
    ; ( C_TEST_MUTATE_VALUE
      , of_type
          (for_all "a"
          @@ fun a ->
          create
            ~mode_annot:[ Checked; Inferred ]
            ~types:[ t_nat () ^-> a ^~> t_option (t_pair a (t_mutation ())) ]) )
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
             ~types:[ t_string () ^-> t_mutation () ^~> t_option (t_string ()) ])
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
             ~types:[ t_unit () ^~> t_pair (t_string ()) (t_key ()) ]) )
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
            ~types:[ t_contract a ^~> t_typed_address a b ]) )
    ; ( C_TEST_EXTERNAL_CALL_TO_ADDRESS
      , of_type
          (create
             ~mode_annot:[ Checked; Checked; Checked; Checked ]
             ~types:
               [ t_address ()
                 ^-> t_option (t_string ())
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
                 ^-> t_option (t_string ())
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
            ~types:[ t_int () ^-> t_big_map a b ^~> t_unit () ]) )
    ; ( C_TEST_BAKER_ACCOUNT
      , of_type
          (create
             ~mode_annot:[ Checked; Checked ]
             ~types:
               [ t_pair (t_string ()) (t_key ())
                 ^-> t_option (t_mutez ())
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
            ~types:[ t_typed_address a b ^~> t_contract a ]) )
    ; ( C_TEST_CREATE_CHEST
      , of_type
          (create
             ~mode_annot:[ Checked; Checked ]
             ~types:
               [ t_bytes ()
                 ^-> t_nat ()
                 ^~> t_pair (t_chest ()) (t_chest_key ())
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
                 ^-> t_list (t_string ())
                 ^-> t_option (t_nat ())
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
             ~types:[ t_string () ^~> t_list (t_string ()) ]) )
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
            ~types:[ t_string () ^-> t_typed_address a b ^~> t_contract c ]) )
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
            ~types:[ t_contract a ^~> t_option (t_string ()) ]) )
    ; ( C_SAPLING_EMPTY_STATE
      , of_type
          (for_all ~kind:Singleton "a"
          @@ fun a ->
          create ~mode_annot:[] ~types:[ return @@ t_sapling_state a ]) )
    ; ( C_SAPLING_VERIFY_UPDATE
      , of_type
          (for_all ~kind:Singleton "a"
          @@ fun a ->
          create
            ~mode_annot:[ Inferred; Checked ]
            ~types:
              [ t_sapling_transaction a
                ^-> t_sapling_state a
                ^~> t_option
                      (t_pair
                         (t_bytes ())
                         (t_pair (t_int ()) (t_sapling_state a)))
              ]) )
    ; C_EQ, of_comparator (Comparable.comparator ~cmp:"EQ")
    ; C_NEQ, of_comparator (Comparable.comparator ~cmp:"NEQ")
    ; C_LT, of_comparator (Comparable.comparator ~cmp:"LT")
    ; C_GT, of_comparator (Comparable.comparator ~cmp:"GT")
    ; C_LE, of_comparator (Comparable.comparator ~cmp:"LE")
    ; C_GE, of_comparator (Comparable.comparator ~cmp:"GE")
    ; ( C_MAP_LITERAL
      , of_type
          (for_all "a"
          @@ fun a ->
          for_all "b"
          @@ fun b ->
          create
            ~mode_annot:[ Inferred ]
            ~types:[ t_list (t_pair a b) ^~> t_map a b ]) )
    ; ( C_BIG_MAP_LITERAL
      , of_type
          (for_all "a"
          @@ fun a ->
          for_all "b"
          @@ fun b ->
          create
            ~mode_annot:[ Inferred ]
            ~types:[ t_list (t_pair a b) ^~> t_big_map a b ]) )
    ; ( C_SET_LITERAL
      , of_type
          (for_all "a"
          @@ fun a ->
          create ~mode_annot:[ Inferred ] ~types:[ t_list a ^~> t_set a ]) )
    ]


let infer_constant ~raise ~infer ~check ~loc const =
  match Const_map.find_opt const constant_typer_tbl with
  | Some typer -> typer ~raise ~infer ~check ~loc
  | None ->
    raise.error
      (corner_case
      @@ Format.asprintf
           "Typer not implemented for constant %a"
           Constant.pp_constant'
           const)


module External_types = struct
  open O

  module Type = struct
    type t = (type_expression list * type_expression) List.Ne.t

    module Syntax = struct
      let create xs = List.Ne.of_list xs
      let ( ^~> ) arg_type ret_type = [ arg_type ], ret_type

      let ( ^-> ) arg_type (arg_types, ret_type) =
        arg_type :: arg_types, ret_type
    end
  end

  type ('err, 'wrn) t =
    raise:('err, 'wrn) raise
    -> loc:Location.t
    -> ctx:Context.t
    -> type_expression list
    -> Context.t * type_expression

  let of_type (types : Type.t) : _ t =
   fun ~raise ~loc ~ctx received_arg_types ->
    Trace.bind_exists ~raise
    @@ List.Ne.map
         (fun (expected_arg_types, ret_type) ~raise ->
           let arg_types =
             match List.zip received_arg_types expected_arg_types with
             | Ok result -> result
             | Unequal_lengths ->
               raise.error
                 (corner_case
                    "Unequal lengths between mode annotation and argument types")
           in
           (* Unify args types *)
           let ctx =
             List.fold arg_types ~init:ctx ~f:(fun ctx (received, expected) ->
               unify
                 ~raise
                 ~loc
                 ~ctx
                 (Context.apply ctx received)
                 (Context.apply ctx expected))
           in
           ctx, ret_type)
         types


  let int_types : (Errors.typer_error, Main_warnings.all) t =
    let open Type.Syntax in
    of_type (create [ t_nat () ^~> t_int (); t_bls12_381_fr () ^~> t_int () ])

  let ediv_types : (Errors.typer_error, Main_warnings.all) t =
    let open Type.Syntax in
    of_type
      (create
         [ t_nat () ^-> t_nat () ^~> t_option (t_pair (t_nat ()) (t_nat ()))
         ; t_int () ^-> t_int () ^~> t_option (t_pair (t_int ()) (t_nat ()))
         ; t_nat () ^-> t_int () ^~> t_option (t_pair (t_int ()) (t_nat ()))
         ; t_int () ^-> t_nat () ^~> t_option (t_pair (t_int ()) (t_nat ()))
         ; t_mutez ()
           ^-> t_mutez ()
           ^~> t_option (t_pair (t_nat ()) (t_mutez ()))
         ; t_mutez ()
           ^-> t_nat ()
           ^~> t_option (t_pair (t_mutez ()) (t_mutez ()))
         ])

  let and_types : (Errors.typer_error, Main_warnings.all) t =
    let open Type.Syntax in
    of_type (create [ t_nat () ^-> t_nat () ^~> t_nat (); t_int () ^-> t_nat () ^~> t_nat() ])
end

(*

let typer_of_type_no_tc t = typer_table_of_ligo_type ~add_tc:false ~fail:false t

let failwith_typer =
  any_table_of
    [ (typer_of_type_no_tc @@ O.(t_string () ^-> t_unit ()))
    ; (typer_of_type_no_tc @@ O.(t_nat () ^-> t_unit ()))
    ; (typer_of_type_no_tc @@ O.(t_int () ^-> t_unit ()))
    ; typer_table_of_ligo_type O.(for_all "a" @@ fun a -> t_string () ^-> a)
    ; typer_table_of_ligo_type O.(for_all "a" @@ fun a -> t_nat () ^-> a)
    ; typer_table_of_ligo_type O.(for_all "a" @@ fun a -> t_int () ^-> a)
    ]


let int_typer =
  any_table_of
    [ typer_table_of_ligo_type O.(t_nat () ^-> t_int ())
    ; typer_table_of_ligo_type O.(t_bls12_381_fr () ^-> t_int ())
    ]


let ediv_typer =
  any_table_of
    [ typer_table_of_ligo_type
        O.(t_nat () ^-> t_nat () ^-> t_option (t_pair (t_nat ()) (t_nat ())))
    ; typer_table_of_ligo_type
        O.(t_int () ^-> t_int () ^-> t_option (t_pair (t_int ()) (t_nat ())))
    ; typer_table_of_ligo_type
        O.(t_nat () ^-> t_int () ^-> t_option (t_pair (t_int ()) (t_nat ())))
    ; typer_table_of_ligo_type
        O.(t_int () ^-> t_nat () ^-> t_option (t_pair (t_int ()) (t_nat ())))
    ; typer_table_of_ligo_type
        O.(t_mutez () ^-> t_mutez () ^-> t_option (t_pair (t_nat ()) (t_mutez ())))
    ; typer_table_of_ligo_type
        O.(t_mutez () ^-> t_nat () ^-> t_option (t_pair (t_mutez ()) (t_mutez ())))
    ]


let external_typers ~raise ~options loc s =
  let typer =
    match s with
    | "int" -> Constant_types.int_typer
    | "ediv" -> Constant_types.ediv_typer
    | "u_ediv" -> Constant_types.ediv_typer
    | _ ->
      raise.error
        (corner_case @@ Format.asprintf "Typer not implemented for external %s" s)
  in
  fun lst tv_opt ->
    let error = ref [] in
    match typer ~error ~raise ~options ~loc lst tv_opt with
    | Some (tv, table, ot) -> tv, table, ot
    | None -> raise.error (corner_case @@ Format.asprintf "Cannot type external %s" s) *)
