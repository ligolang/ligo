open Ast_unified
open Pass_type
open Simple_utils.Trace
open Errors
module Location = Simple_utils.Location
include Flag.No_arg ()

let rec wrap_multi_bindings
    : type a.
      default:(a -> a)
      -> wrap:(loc:Location.t -> declaration -> a)
      -> declaration
      -> a list
  =
 fun ~default ~wrap d ->
  let loc_of_sdecl Simple_decl.{ type_params = _; pattern; rhs_type; let_rhs } =
    List.fold
      ~f:Location.cover
      ~init:Location.generated
      [ get_p_loc pattern
      ; Option.value_map ~default:Location.generated ~f:get_t_loc rhs_type
      ; get_e_loc let_rhs
      ]
  in
  match get_d d with
  | D_attr (attr, x) ->
    let wrap ~loc d = wrap ~loc @@ d_attr ~loc (attr, d) in
    wrap_multi_bindings ~default ~wrap x
  | D_multi_const x ->
    List.map
      ~f:(fun x ->
        let loc = loc_of_sdecl x in
        wrap ~loc @@ d_const ~loc x)
      (List.Ne.to_list x)
  | D_multi_var x ->
    List.map
      ~f:(fun x ->
        let loc = loc_of_sdecl x in
        wrap ~loc @@ d_var ~loc x)
      (List.Ne.to_list x)
  | _ -> [ default (wrap ~loc:(get_d_loc d) d) ]


let block : _ block_ -> block =
 fun block ->
  let dig_attr : statement -> f:(declaration -> _ program_) -> statement list =
   fun stmt ~f ->
    let rec aux s acc : statement list =
      let loc = get_s_loc s in
      match get_s s with
      | S_attr (attr, pe) ->
        let lst = aux pe [] in
        List.map lst ~f:(fun s' -> s_attr ~loc (attr, s')) @ acc
      | S_decl d -> f d @ acc
      | x -> make_s ~loc x :: acc
    in
    aux stmt []
  in
  let f : statement -> statement list -> statement list =
   fun stmt acc ->
    let extended =
      (* turn one statement into multiple one in case it's a multi declaration *)
      let f d =
        wrap_multi_bindings
          ~default:(Fun.const stmt)
          ~wrap:(fun ~loc d -> s_decl ~loc d)
          d
      in
      dig_attr stmt ~f
    in
    extended @ acc
  in
  let stmts = List.Ne.to_list (Location.unwrap block) in
  block_of_statements (List.Ne.of_list (List.fold_right ~f ~init:[] stmts))


let program : _ program_ -> program =
 fun prg ->
  let dig_attr : program_entry -> f:(declaration -> _ list) -> program_entry list =
   fun pe ~f ->
    let rec aux pe : program_entry list =
      match get_pe pe with
      | PE_attr (attr, pe) -> List.map ~f:(pe_attr attr) (aux pe)
      | PE_export pe -> List.map ~f:pe_export (aux pe)
      | PE_declaration d -> f d
      | x -> [ make_pe x ]
    in
    aux pe
  in
  let f : program_entry -> _ program_ -> _ program_ =
   fun pe acc ->
    let extended =
      (* turn one program entry into multiple one in case it's a multi declaration *)
      let f d =
        wrap_multi_bindings ~default:Fun.id ~wrap:(fun ~loc:_ d -> pe_declaration d) d
      in
      dig_attr pe ~f
    in
    extended @ acc
  in
  make_prg @@ List.fold_right ~f ~init:[] prg


let compile ~raise:_ = Fold { idle_fold with program; block }

let reduction ~raise =
  { Iter.defaults with
    declaration =
      (function
      | { wrap_content = D_multi_const _ | D_multi_var _; _ } ->
        raise.error (wrong_reduction __MODULE__)
      | _ -> ())
  }


let name = __MODULE__
let decompile ~raise:_ = Nothing (* for now ? *)

open Unit_test_helpers.Program

let%expect_test "multi declaration in program" =
  {|
    ((PE_attr ((key inline)) (PE_declaration
      (D_multi_var (
        ((pattern (P_var x)) (let_rhs (EXPR1)))
        ((pattern (P_var y)) (let_rhs (EXPR2)))
        ((pattern (P_var z)) (let_rhs (EXPR3)))))))
      (PE_declaration
        (D_var ((pattern (P_var last)) (let_rhs (EXPR4))))))
  |}
  |-> compile;
  [%expect
    {|
        ((PE_attr ((key inline))
          (PE_declaration (D_var ((pattern (P_var x)) (let_rhs (EXPR1))))))
         (PE_attr ((key inline))
          (PE_declaration (D_var ((pattern (P_var y)) (let_rhs (EXPR2))))))
         (PE_attr ((key inline))
          (PE_declaration (D_var ((pattern (P_var z)) (let_rhs (EXPR3))))))
         (PE_declaration (D_var ((pattern (P_var last)) (let_rhs (EXPR4))))))
    |}]
