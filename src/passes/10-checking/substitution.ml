open Ligo_prim

type t =
  { type_subst : (Kind.t * Type.t) Type_var.Map.t
  ; layout_subst : Type.layout Layout_var.Map.t
  }
[@@deriving sexp, compare]

let empty =
  { type_subst = Type_var.Map.empty; layout_subst = Layout_var.Map.empty }


let add_texists_eq t tvar kind type_ =
  { t with type_subst = Map.set t.type_subst ~key:tvar ~data:(kind, type_) }


let add_lexists_eq t lvar layout =
  { t with layout_subst = Map.set t.layout_subst ~key:lvar ~data:layout }


let find_texists_eq t tvar = Map.find t.type_subst tvar
let find_lexists_eq t lvar = Map.find t.layout_subst lvar

let merge t1 t2 =
  let merge_subst subst1 subst2 =
    Map.merge_skewed subst1 subst2 ~combine:(fun ~key:_ _eq1 eq2 -> eq2)
  in
  { type_subst = merge_subst t1.type_subst t2.type_subst
  ; layout_subst = merge_subst t1.layout_subst t2.layout_subst
  }
