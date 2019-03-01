open AST
open Region

module SMap = Map.Make(String)

(* open Sanity: *)
let (|>) v f = f v
let (@@) f v = f v
let map f l = List.rev (List.rev_map f l)

let type_decl_to_tenv td =
  td
  |> List.map (fun (_, name, _, type_expr) -> (name, type_expr))
  |> List.to_seq |> SMap.of_seq

let shadow name typ env =
  SMap.update name (function None -> Some [typ] | Some tl -> Some (typ :: tl)) env

let shadow_list name_typ_list env =
  List.fold_left (fun  acc (name, typ) -> shadow name typ acc) env name_typ_list

let tc ast =
  (* te is the type environment, ve is the variable environment *)
  let te = type_decl_to_tenv ast#types in
  let ve =
    SMap.empty
    |> (match ast#parameter.value with (_,name,_,ty) -> shadow name ty)
    |> shadow "storage"    (snd ast#storage.value)
    |> shadow "operations" (snd ast#operations.value)
    (* |> shadow_list @@ map (function FunDecl {value} -> value.var.value, ) lambdas *)
  in
  te, ve
