open Core
open Ast_unified
open Ast_unified.Catamorphism
open Pass_type
open Errors
open Unit_test_helpers
module Trace = Simple_utils.Trace
module Location = Simple_utils.Location
include Flag.No_arg ()

(* This pass prevent shadowing in signatures (i.e. two bindings to the same variable in the same scope) *)

let name = __MODULE__

let check_for_duplicated_val ~(raise : _ Trace.raise) b =
  List.iter b ~f:(fun bound ->
      let dups = List.find_a_dup ~compare:Variable.compare (List.rev bound) in
      match dups with
      | Some v -> raise.error (duplicate_identifier v)
      | _ -> ())


let check_for_duplicated_type ~(raise : _ Trace.raise) b =
  List.iter b ~f:(fun bound ->
      let dups = List.find_a_dup ~compare:Ty_variable.compare (List.rev bound) in
      match dups with
      | Some v -> raise.error (duplicate_ty_identifier v)
      | _ -> ())


module Type_vars = struct
  type bound = Ty_variable.t list list

  let empty = [ [] ]
  let singleton x = [ x ]

  let union a b =
    match List.map2 a b ~f:List.append with
    | List.Or_unequal_lengths.Ok x -> x
    | List.Or_unequal_lengths.Unequal_lengths -> assert false


  let ftv_folder
      : ( bound, ty_expr, pattern, bound, bound, bound, bound, bound, bound, bound, bound
      , bound ) fold
    =
    let sig_expr : _ sig_expr_ -> bound = fold_sig_expr_ union union union empty in
    let sig_entry : _ sig_entry_ -> bound =
     fun si ->
      match Location.unwrap si with
      | S_type (v, _TODO, _) -> [ singleton v ]
      | S_type_var v -> [ singleton v ]
      | _ -> empty
    in
    { expr = (fun _ -> empty)
    ; ty_expr =
        (fun x -> Combinators.make_t ~loc:(Location.get_location x) (Location.unwrap x))
    ; pattern =
        (fun p -> Combinators.make_p ~loc:(Location.get_location p) (Location.unwrap p))
    ; statement = (fun _ -> empty)
    ; block = (fun _ -> empty)
    ; mod_expr = (fun _ -> empty)
    ; instruction = (fun _ -> empty)
    ; declaration = (fun _ -> empty)
    ; program_entry = (fun _ -> empty)
    ; program = (fun _ -> empty)
    ; sig_expr
    ; sig_entry
    }
end

let compile ~raise =
  let sig_expr : _ sig_expr_ -> unit =
   fun se ->
    let () = check_for_duplicated_val ~raise @@ Bound_vars.bound_sig_expr { fp = se } in
    let () =
      check_for_duplicated_type ~raise
      @@ cata_sig_expr ~f:Type_vars.ftv_folder { fp = se }
    in
    ()
  in
  Check { Iter.defaults with sig_expr }


let reduction ~raise:_ = Iter.defaults
let decompile ~raise:_ = Nothing

let%expect_test _ =
  Sig_expr.(
    {|
    (S_body
      ((S_type_var t)
       (S_type t (GEN) (TY_EXPR))))
    |}
    |->! compile;
    [%expect {|
    Err : (Small_passes_duplicate_ty_identifier t)
    |}])

let%expect_test _ =
  Sig_expr.(
    {|
    (S_body
      ((S_value x (TY_EXPR1) false)
       (S_value x (TY_EXPR2) false)))
    |}
    |->! compile;
    [%expect {|
    Err : (Small_passes_duplicate_identifier x)
    |}])

let%expect_test _ =
  Sig_expr.(
    {|
    (S_body
      ((S_value x (TY_EXPR1) false)
       (S_type x (GEN) (TY_EXPR2))))
    |}
    |-> compile;
    [%expect
      {|
    (S_body ((S_value x (TY_EXPR1) false) (S_type x (GEN) (TY_EXPR2))))
    |}])
