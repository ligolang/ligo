open Core

let pair_map = fun f (x , y) -> (f x , f y)

module Substitution = struct

  module Pattern = struct

    let rec declaration ~(d : Ast_typed.declaration Location.wrap) ~v ~expr : Ast_typed.declaration Location.wrap =
      let _TODO = (d, v, expr) in
      failwith "TODO: subst declaration"

    and program ~(p : Ast_typed.program) ~v ~expr : Ast_typed.program =
      List.map (fun d -> declaration ~d ~v ~expr) p

    (*
       Computes `P[v := expr]`.
    *)
    and type_value ~tv ~v ~expr =
      let self tv = type_value ~tv ~v ~expr in
      match tv with
      | P_variable v' when v' = v -> expr
      | P_variable _ -> tv
      | P_constant (x , lst) -> (
          let lst' = List.map self lst in
          P_constant (x , lst')
        )
      | P_apply ab -> (
          let ab' = pair_map self ab in
          P_apply ab'
        )
      | P_forall p -> (
          let aux c = constraint_ ~c ~v ~expr in
          let constraints = List.map aux p.constraints in
          if (p.binder = v) then (
            P_forall { p with constraints }
          ) else (
            let body = self p.body in
            P_forall { p with constraints ; body }
          )
        )

    and constraint_ ~c ~v ~expr =
      match c with
      | C_equation ab -> (
          let ab' = pair_map (fun tv -> type_value ~tv ~v ~expr) ab in
          C_equation ab'
        )
      | C_typeclass (tvs , tc) -> (
          let tvs' = List.map (fun tv -> type_value ~tv ~v ~expr) tvs in
          let tc' = typeclass ~tc ~v ~expr in
          C_typeclass (tvs' , tc')
        )
      | C_access_label (tv , l , v') -> (
          let tv' = type_value ~tv ~v ~expr in
          C_access_label (tv' , l , v')
        )

    and typeclass ~tc ~v ~expr =
      List.map (List.map (fun tv -> type_value ~tv ~v ~expr)) tc

    (* Performs beta-reduction at the root of the type *)
    let eval_beta_root ~(tv : type_value) =
      match tv with
        P_apply (P_forall { binder; constraints; body }, arg) ->
        let constraints = List.map (fun c -> constraint_ ~c ~v:binder ~expr:arg) constraints in
        (type_value ~tv:body ~v:binder ~expr:arg , constraints)
      | _ -> (tv , [])
  end

end
