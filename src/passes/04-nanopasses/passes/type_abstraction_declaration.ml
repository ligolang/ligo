open Errors
open Ast_unified
open Pass_type
open Simple_utils.Trace

(* in expression;
```
let x =
  type 'a t = 'a * 'a in
  ((1,2) : int t)
```

this parses incorectly (the 'a binder is ignored) : See with christian
*)

let prepend_if_caml ~syntax str =
  if Syntax_types.(equal syntax CameLIGO) then "'" ^ str else str


let abstract_type params ty_expr =
  List.Ne.fold_right params ~init:ty_expr ~f:(fun ty_binder acc ->
      t_abstraction ~loc:(get_t_loc ty_expr) { ty_binder; kind = Type; type_ = acc })


let compile =
  let declaration : _ declaration_ -> declaration = function
    | { location = loc
      ; wrap_content = D_type_abstraction { name; params = None; type_expr }
      } -> d_type ~loc { name; type_expr }
    | { location = loc
      ; wrap_content = D_type_abstraction { name; params = Some params; type_expr }
      } ->
      let type_expr = abstract_type params type_expr in
      d_type ~loc { name; type_expr }
    | { location = loc; wrap_content } -> make_d ~loc wrap_content
  in
  let expr : _ expr_ -> expr = function
    | { location = loc
      ; wrap_content =
          E_type_in { type_decl = { name; params = Some params; type_expr }; body }
      } ->
      let type_expr = abstract_type params type_expr in
      e_type_in ~loc { type_decl = { name; params = None; type_expr }; body }
    | { location; wrap_content } -> make_e ~loc:location wrap_content
  in
  `Cata { idle_cata_pass with declaration; expr }


let reduction ~raise =
  { Iter.defaults with
    declaration =
      (function
      | { wrap_content = D_type_abstraction _; _ } ->
        raise.error (wrong_reduction __MODULE__)
      | _ -> ())
  ; expr =
      (function
      | { wrap_content = E_type_in { type_decl = { params = Some _; _ }; _ }; _ } ->
        raise.error (wrong_reduction __MODULE__)
      | _ -> ())
  }


let decompile =
  let pass_declaration : _ declaration_ -> declaration =
   fun decl ->
    { fp =
        Location.map
          (function
            | D_type { name; type_expr }
            | D_type_abstraction { name; params = None; type_expr } ->
              let rec aux tv e =
                match get_t_abstraction e with
                | None -> tv, e
                | Some { ty_binder; kind = _; type_ } -> aux (tv @ [ ty_binder ]) type_
              in
              let params, tail = aux [] type_expr in
              let params = params |> List.Ne.of_list_opt in
              D_type_abstraction { name; params; type_expr = tail }
            | x -> x)
          decl
    }
  in
  `Cata { idle_cata_pass with declaration = pass_declaration }


let pass ~raise =
  morph ~name:__MODULE__ ~compile ~decompile ~reduction_check:(reduction ~raise)


open Unit_test_helpers

let%expect_test "decompile" =
  {|
  ((PE_declaration
    (D_type (
      (name my_t)
      (type_expr
        (T_abstraction
          ((ty_binder a) (kind Type) (type_ 
          (T_abstraction
            ((ty_binder b) (kind Type) (type_ (T_var whatever))))))))))))
  |}
  <-| pass ~raise;
  [%expect
    {|
    ((PE_declaration
      (D_type_abstraction
       ((name my_t) (params ((a b))) (type_expr (T_var whatever))))))
    |}]

let%expect_test "compile" =
  {|
  ((PE_declaration
    (D_type_abstraction
      ((name my_t) (params ((a b))) (type_expr (T_var whatever))))))
  |}
  |-> pass ~raise;
  [%expect
    {|
    ((PE_declaration
      (D_type
       ((name my_t)
        (type_expr
         (T_abstraction
          ((ty_binder a) (kind Type)
           (type_
            (T_abstraction ((ty_binder b) (kind Type) (type_ (T_var whatever)))))))))))) |}]
