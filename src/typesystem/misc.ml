open Core

let pair_map = fun f (x , y) -> (f x , f y)

module Substitution = struct

  module Pattern = struct

    (*
       Computes `P[v := expr]`.
    *)
    let rec type_value ~tv ~v ~expr =
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

  end

end
