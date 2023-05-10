(* open Helpers
open Cst.Pascaligo
include Fuzz_shared.Monad
open Lexing_shared
(* Helpers for swapping operators *)

let arith_bin_op_ctor =
  let add op = E_Add op in
  let sub op = E_Sub op in
  let mult op = E_Mult op in
  let div op = E_Div op in
  let mod_ op = E_Mod op in
  [ add; sub; mult; div; mod_ ]


let comp_bin_op_ctor =
  let lt op = E_Lt op in
  let leq op = E_Leq op in
  let gt op = E_Gt op in
  let geq op = E_Geq op in
  let equal op = E_Equal op in
  let neq op = E_Neq op in
  [ lt; leq; gt; geq; equal; neq ]


module Mutator (M : Monad) = struct
  open Monad_context (M)
  open Fold_helpers (M)
  open Fuzz_shared.Helpers

  let mutate_expression (expr : expr) =
    let map_return = List.map ~f:return in
    let false_return x = return (false, x) in
    let return x = return (true, x) in
    match expr with
    | E_Typed { value = { inside = _, (_, T_Var a); lpar = _; rpar = _ }; region = _ } ->
      (match a#payload with
      | "address" -> false_return expr
      | _ -> return expr)
    | E_Add op | E_Sub op | E_Mult op | E_Div op | E_Mod op ->
      let* ctor = arith_bin_op_ctor |> map_return |> oneof in
      return (ctor op)
    | E_Int i ->
      let s, z = i#payload in
      let* z = mutate_int (Z.to_int z) in
      let* f = transform_int |> map_return |> oneof in
      let z = Z.of_int (f z) in
      return (E_Int (Wrap.wrap ~attributes:i#attributes (s, z) i#region))
    | E_Nat i ->
      let s, z = i#payload in
      let* z = mutate_nat (Z.to_int z) in
      let* f = transform_nat |> map_return |> oneof in
      let z = Z.of_int (f z) in
      return (E_Nat (Wrap.wrap ~attributes:i#attributes (s, z) i#region))
    | E_Mutez i ->
      let s, z = i#payload in
      (match Int64.to_int z with
      | Some z ->
        let* z = mutate_nat z in
        let* f = transform_nat |> map_return |> oneof in
        let z = Int64.of_int (f z) in
        return (E_Mutez (Wrap.wrap ~attributes:i#attributes (s, z) i#region))
      | None -> return (E_Mutez i))
    | E_Lt op | E_Leq op | E_Gt op | E_Geq op | E_Equal op | E_Neq op ->
      let* ctor = comp_bin_op_ctor |> map_return |> oneof in
      return (ctor op)
    | E_String s ->
      let* str = mutate_string s#payload in
      let* f = oneof (map_return transform_string) in
      let str = f str in
      return (E_String (Wrap.wrap ~attributes:s#attributes str s#region))
    | _ -> return expr


  let mutate_mapper : mapper =
    { e = mutate_expression
    ; t = (fun x -> return x)
    ; s = (fun x -> return x)
    ; d = (fun x -> return x)
    }


  let mutate_module_ ?n (mod_ : Cst.Pascaligo.t) =
    let rndmod_ = map_declarations mutate_mapper mod_.decl in
    let x, ds = get_one ?n rndmod_ in
    x, { mod_ with decl = ds }
end *)
