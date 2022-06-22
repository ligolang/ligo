module AST = Ast_typed
module T = Stage_common.Types
module C = AST.Combinators
module Location = Simple_utils.Location
module Trace = Simple_utils.Trace

module LMap = AST.LMap
module XList = Simple_utils.List
module LSet = Caml.Set.Make (struct
  type t = T.label
  let compare = T.compare_label
end)
(* TODO: write good comments *)
let cons_label = T.Label "#CONS"
let nil_label  = T.Label "#NIL"

let t_unit = AST.t_unit ()
let wild_binder =
  let var = AST.ValueVar.of_input_var "__" in
  let attributes = Stage_common.Helpers.empty_attribute in
  AST.{ var ; ascr = None ; attributes }

type simple_pattern =
    SP_Wildcard of AST.type_expression
  | SP_Constructor of T.label * (simple_pattern list) * AST.type_expression

let nil_constructor ty = SP_Constructor (nil_label, [SP_Wildcard t_unit], ty)
let list_constructors = LSet.of_list [cons_label ; nil_label]

let rec pp_simple_pattern ppf sp =
  match sp with
    SP_Wildcard t -> Format.fprintf ppf "_ : %a" AST.PP.type_expression t
  | SP_Constructor (Label c, ps, _) ->
    Format.fprintf ppf "%s (%s)" c
      (String.concat ~sep:", "
        (List.map ps ~f:(fun p -> Format.asprintf "%a" pp_simple_pattern p)))

let pp_simple_pattern_list ppf sps =
  List.iter sps ~f:(fun sp -> Format.fprintf ppf "%a, " pp_simple_pattern sp)

let get_variant_nested_type label (tsum : AST.t_sum) =
  let label_map = tsum.content in
  let c = LMap.find_opt label label_map in
  let c = Option.value_exn c in (* BAD *)
  c.associated_type

let rec count_type_parts (t : AST.type_expression) =
  match t.type_content with
    AST.T_record { content ; _ } ->
      LMap.fold (fun _ (row_elt : AST.row_element) ts ->
        let elt_typ = row_elt.associated_type in
        ts @ count_type_parts elt_typ)
        content []
  | _ -> [t]

let rec to_simple_pattern ty_pattern =
  let pattern', ty = ty_pattern in
  let pattern = Location.unwrap pattern' in
  match pattern with
    AST.P_unit -> [SP_Wildcard ty]
  | P_var _ when C.is_t_record ty ->
    let fields = Option.value_exn (C.get_record_fields ty) in
    let fields = List.map ~f:snd fields in
    let ps     = XList.repeat pattern' (List.length fields) in
    let ps     = List.zip_exn ps fields in
    let ps     = List.map ps ~f:to_simple_pattern in
    List.concat ps
  | P_var _    -> [SP_Wildcard ty]
  | P_list (Cons (hd, tl)) ->
    let hd_ty = Option.value_exn (C.get_t_list ty) in (* BAD *)
    let hd_tl =
      to_simple_pattern (hd, hd_ty) @
      to_simple_pattern (tl, ty) in
    [SP_Constructor (cons_label, hd_tl, ty)]
  | P_list (List ps) ->
    let hd_ty = Option.value_exn (C.get_t_list ty) in (* BAD *)
    List.fold_right ps ~init:([nil_constructor ty])
      ~f:(fun p acc ->
        let hd_tl = to_simple_pattern (p, hd_ty) @ acc in
        [SP_Constructor (cons_label, hd_tl, hd_ty)])
  | P_variant (c, p) ->
    let p_ty = get_variant_nested_type c (Option.value_exn (C.get_t_sum ty)) in
    [SP_Constructor (c, to_simple_pattern (p, p_ty), ty)]
  | P_tuple ps
  | P_record (_, ps) ->
    (* let () = Format.printf "type 3 : %a \n" AST.PP.type_expression ty in *)
    let ps_tys = Option.value_exn (C.get_record_fields ty) in (* BAD *)
    let ps_tys = List.map ~f:snd ps_tys in
    let ps = List.zip_exn ps ps_tys in
    let ps = List.map ps ~f:to_simple_pattern in
    List.concat ps

let are_keys_numeric keys =
  List.for_all keys
    ~f:(fun (T.Label l) -> Option.is_some @@ int_of_string_opt l)

let rec to_list_pattern simple_pattern =
  match simple_pattern with
    SP_Wildcard _ -> Location.wrap @@ T.P_var wild_binder
  | SP_Constructor (T.Label "#NIL", _, _) ->
    Location.wrap @@ T.P_list (T.List [])
  | SP_Constructor (T.Label "#CONS", sps, t) ->
    let rsps = List.rev sps in
    let tl = List.hd_exn rsps in
    let hd = List.rev (List.tl_exn rsps) in
    let hd = to_original_pattern hd (C.get_t_list_exn t) in
    let tl = to_list_pattern tl in
    Location.wrap @@ T.P_list (T.Cons (hd, tl))
  | SP_Constructor (T.Label c, _, _) ->
    failwith (Format.sprintf "edge case: %s in to_list_pattern" c)

and to_original_pattern simple_patterns (ty : AST.type_expression) =
  match simple_patterns with
    [] -> failwith "edge case: to_original_pattern empty patterns"
  | SP_Wildcard t::[] when AST.is_t_unit t -> Location.wrap @@ T.P_unit
  | SP_Wildcard _::[] -> Location.wrap @@ T.P_var wild_binder
  | (SP_Constructor (T.Label "#CONS", _, _) as simple_pattern)::[]
  | (SP_Constructor (T.Label "#NIL", _, _) as simple_pattern)::[] ->
    to_list_pattern simple_pattern
  | SP_Constructor (c, sps, t)::[] ->
    let t = get_variant_nested_type c (Option.value_exn (C.get_t_sum t)) in
    let ps = to_original_pattern sps t in
    Location.wrap @@ T.P_variant (c, ps)
  | _ ->
    (match ty.type_content with
    AST.T_record { content ; _ } ->
      let kvs = List.sort (LMap.to_kv_list content)
        ~compare:(fun (l1, _) (l2, _) -> T.compare_label l1 l2) in
      let labels, tys = List.unzip kvs in
      let tys = List.map tys ~f:(fun ty -> ty.associated_type) in

      let _, ps = List.fold_left tys ~init:(simple_patterns, [])
        ~f:(fun (sps, ps) t ->
            let n = List.length @@ count_type_parts t in
            let sps, rest = List.split_n sps n in
            rest, ps @ [to_original_pattern sps t]
          )
      in
      if are_keys_numeric labels then
        Location.wrap @@ T.P_tuple ps
      else
        Location.wrap @@ T.P_record (labels, ps)
    | _ -> failwith "edge case: not a record/tuple")


type matrix = simple_pattern list list

let print_matrix matrix =
  let () = Format.printf "matrix: \n" in
  let () = List.iter matrix ~f:(fun row ->
    Format.printf "[%a]\n" pp_simple_pattern_list row) in
  Format.printf "\n"

let print_vector vector =
  Format.printf "vector: \n%a\n" pp_simple_pattern_list vector

(* specialize *)
let specialize_matrix c a matrix =
  let specialize specialized row =
    match row with
      SP_Constructor (cp, r1_a, _) :: p2_n when T.equal_label c cp ->
        let row = r1_a @ p2_n in
        row :: specialized
    | SP_Constructor _  :: _ -> specialized
    | SP_Wildcard _ :: p2_n ->
      let wildcards = List.map a ~f:(fun t -> SP_Wildcard t) in
      let row = wildcards @ p2_n in
      row :: specialized
    | [] -> [] (* TODO: check is this okay? *)
  in
  List.fold_left matrix ~init:[] ~f:specialize

let specialize_vector c a q1_n =
  match q1_n with
    SP_Constructor (cp, r1_a, _) :: q2_n when T.equal_label c cp ->
      r1_a @ q2_n
  | SP_Wildcard _ :: q2_n ->
    let wildcards = List.map a ~f:(fun t -> SP_Wildcard t) in
    wildcards @ q2_n
  | _ -> failwith "edge case: specialize_vector wrong constructor"

(* default *)
let default_matrix matrix =
  let default row dp =
    match row with
      SP_Constructor _ :: _ -> dp
    | SP_Wildcard _ :: p2_n -> p2_n :: dp
    | [] -> [] (* TODO: check is this okay? *)
  in
  List.fold_right matrix ~init:[] ~f:default

let find_constuctor_arity c (t : AST.type_expression) =
  match c with
  T.Label "#CONS" ->
    (* TODO: fix this... very hacky now *)
    let t'  = C.get_t_list_exn t in
    count_type_parts t' @ [t]
  | Label "#NIL"  -> [t]
  | _       ->
  let te = get_variant_nested_type c (Option.value_exn (C.get_t_sum t)) in
  count_type_parts te

let get_all_constructors (t : AST.type_expression) =
  if C.is_t_list t then list_constructors
  else
    match C.get_t_sum t with
      Some tsum ->
        let label_map = tsum.content in
        let labels = LMap.keys label_map in
        LSet.of_list labels
    | None -> LSet.empty

let get_constructors_from_1st_col matrix =
  List.fold_left matrix ~init:(LSet.empty, Some t_unit)
    ~f:(fun (s, t) row ->
      match row with
        SP_Constructor (c, _, t) :: _ -> LSet.add c s, Some t
      | SP_Wildcard t :: _ -> s, Some t
      | [] -> s, t)

let rec algorithm_Urec matrix vector =
  (* let () = print_matrix matrix in
  let () = print_vector vector in
  let () = Format.printf "---------------\n" in *)
  match vector with
    SP_Constructor (c, _r1_n, t) :: _q2_n ->
      (* let () = Format.printf "type 1 : %a \n" AST.PP.type_expression t in *)
      let a  = find_constuctor_arity c t in
      let matrix = specialize_matrix c a matrix in
      let vector = specialize_vector c a vector in
      algorithm_Urec matrix vector
  | SP_Wildcard t :: q2_n ->
    (* let () = Format.printf "type 2 : %a \n" AST.PP.type_expression t in *)
    let complete_signature = get_all_constructors t in
    let constructors, _ = get_constructors_from_1st_col matrix in
    (*  *)
    (* let () = Format.printf "----\ncomplete signature:\n" in
    let () = LSet.iter (fun (Label l) -> Format.printf "%s, " l)
      complete_signature in
    let () = Format.printf "\nconstructors from 1st column:\n" in
    let () = LSet.iter (fun (Label l) -> Format.printf "%s, " l)
      constructors in
    let () = Format.printf "\n----\n" in *)
    (*  *)
    if not (LSet.is_empty complete_signature)
      && LSet.equal complete_signature constructors then
      LSet.fold
        (fun c b ->
          let a = find_constuctor_arity c t in
          (* let Label l = c in *)
          (* let () = Format.printf "-----\n%s arity: %d\n-----\n" l a in *)
          b || algorithm_Urec
                (specialize_matrix c a matrix)
                (specialize_vector c a vector))
        complete_signature false
    else
      algorithm_Urec (default_matrix matrix) q2_n
  | [] ->
    if List.is_empty matrix then true
    else if List.for_all matrix ~f:(List.is_empty) then false
    else failwith "edge case: algorithm Urec"

let rec algorithm_I matrix n ts =
  (* let () = Format.printf "n = %d\n" n in
  let () = print_matrix matrix in
  let () = Format.printf "---------------\n" in *)
  if n  = 0 then
    if List.is_empty matrix then Some [[]]
    else if List.for_all matrix ~f:(List.is_empty) then None
    else failwith "edge case: algorithm I"
  else
    let constructors, _ = get_constructors_from_1st_col matrix in
    let t, ts = List.split_n ts 1 in
    let t = List.hd_exn t in
    let complete_signature = get_all_constructors t in
    if (not @@ LSet.is_empty constructors) &&
      LSet.equal constructors complete_signature then
        LSet.fold (fun ck p ->
          if Option.is_some p then p
          else
            let a  = find_constuctor_arity ck t in
            let ak = List.length a in
            let matrix = specialize_matrix ck a matrix in
            let ps = algorithm_I matrix (ak + n - 1) (a @ ts) in
            match ps with
              Some ps ->
                Some
                (List.map ps ~f:(fun ps ->
                  let xs, ps = List.split_n ps ak in
                  [SP_Constructor (ck, xs, t)] @ ps))
            | None -> None
          ) complete_signature None
    else
      let dp = default_matrix matrix in
      let ps = algorithm_I dp (n - 1) ts in
      match ps with
        Some ps ->
          if LSet.is_empty constructors then
            let ps = List.map ps ~f:(fun ps -> [SP_Wildcard t] @ ps) in
            Some ps
          else
            let missing_constructors
              = LSet.diff complete_signature constructors in
            let cs = LSet.fold (fun c cs ->
              let a  = find_constuctor_arity c t in
              let a  = List.map a ~f:(fun t -> SP_Wildcard t) in
              let c  = SP_Constructor (c, a, t) in
              c :: cs
            ) missing_constructors [] in
            Some (List.fold_left cs ~init:[] ~f:(fun new_ps c ->
              let ps = List.map ps ~f:(fun p -> [c] @ p) in
              ps @ new_ps
            ))
      | None -> None

let redundant_case_analysis matrix =
  let redundant, case, _ =  List.fold_left matrix ~init:(false, 0, [])
    ~f:(fun (redundant_case_found, case, matrix) vector ->
      if redundant_case_found then (true, case, [])
      else if List.is_empty matrix then (false, case + 1, [vector])
      else
        let redundant_case_found = not @@ algorithm_Urec matrix vector in
        (redundant_case_found, case + 1, matrix @ [vector]))
  in
  (redundant, case)

let check_anomalies ~(raise : Errors.typer_error Trace.raise) ~loc eqs t =
  let ts = count_type_parts t in
  let matrix = List.map eqs ~f:(fun (p, t, _) ->
    to_simple_pattern (p, t)) in
  (* let () = List.iter eqs
    ~f:(fun (p, t, _) ->
      let sps = to_simple_pattern (p, t) in
      Format.printf "%a\n" pp_simple_pattern_list sps) in *)

  let vector = List.map ts ~f:(fun t -> SP_Wildcard t) in

  let missing_case = algorithm_Urec matrix vector in
  let () = if missing_case then
    let i = algorithm_I matrix (List.length vector) ts in
    let i = Option.value_exn i in
    let ps = List.map i ~f:(fun sp -> to_original_pattern sp t) in
    raise.raise @@ Errors.pattern_missing_cases loc ps
  else () in

  let redundant, case = redundant_case_analysis matrix in
  if redundant
  then
    let p, _, _ = List.nth_exn eqs (case - 1) in
    let loc = Location.get_location p in
    raise.raise @@ Errors.pattern_redundant_case loc
  else()