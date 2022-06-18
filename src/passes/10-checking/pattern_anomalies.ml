module AST = Ast_typed
module T = Stage_common.Types
module C = AST.Combinators

module LMap = AST.LMap
module XList = Simple_utils.List
module LSet = Caml.Set.Make (struct
  type t = T.label
  let compare = T.compare_label
end)

let cons_label = T.Label "#CONS"
let nil_label  = T.Label "#NIL"

let t_unit = AST.t_unit ()

type simple_pattern =
    SP_wildcard of AST.type_expression
  | SP_Constructor of T.label * (simple_pattern list) * AST.type_expression

let nil_constructor ty = SP_Constructor (nil_label, [SP_wildcard t_unit], ty)
let list_constructors = LSet.of_list [cons_label ; nil_label]

let rec pp_simple_pattern ppf sp =
  match sp with
    SP_wildcard t -> Format.fprintf ppf "_ : %a" AST.PP.type_expression t
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

let rec to_simple_pattern ty_pattern =
  let pattern', ty = ty_pattern in
  let pattern = T.Location.unwrap pattern' in
  match pattern with
    AST.P_unit -> [SP_wildcard ty]
  | P_var _ when C.is_t_record ty ->
    let fields = Option.value_exn (C.get_record_fields ty) in
    let fields = List.map ~f:snd fields in
    let ps     = XList.repeat pattern' (List.length fields) in
    let ps     = List.zip_exn ps fields in
    let ps     = List.map ps ~f:to_simple_pattern in
    List.concat ps
  | P_var _    -> [SP_wildcard ty]
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
  let specialize row specialized =
    match row with
      SP_Constructor (cp, r1_a, _) :: p2_n when T.equal_label c cp ->
        let row = r1_a @ p2_n in
        row :: specialized
    | SP_Constructor _  :: _ -> specialized
    | SP_wildcard _ :: p2_n ->
      let wildcards = List.map a ~f:(fun t -> SP_wildcard t) in
      let row = wildcards @ p2_n in
      row :: specialized
    | [] -> [] (* TODO: check is this okay? *)
  in
  (* Here order does not matter; change this to fold_left later *)
  List.fold_right matrix ~init:[] ~f:specialize

let specialize_vector c a q1_n =
  match q1_n with
    SP_Constructor (cp, r1_a, _) :: q2_n when T.equal_label c cp ->
      r1_a @ q2_n
  | SP_wildcard _ :: q2_n ->
    let wildcards = List.map a ~f:(fun t -> SP_wildcard t) in
    wildcards @ q2_n
  | _ -> failwith "edge case: specialize_vector wrong constructor"

(* default *)
let default_matrix matrix =
  let default row dp =
    match row with
      SP_Constructor _ :: _ -> dp
    | SP_wildcard _ :: p2_n -> p2_n :: dp
    | [] -> [] (* TODO: check is this okay? *)
  in
  List.fold_right matrix ~init:[] ~f:default

let rec count_type_parts (t : AST.type_expression) =
  match t.type_content with
    AST.T_record { content ; _ } ->
      LMap.fold (fun _ (row_elt : AST.row_element) ts -> 
        let elt_typ = row_elt.associated_type in
        ts @ count_type_parts elt_typ) 
        content []
  | _ -> [t]

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
      (* failwith "get_all_constructors: not a variant type" *)
    
let get_constructos_from_1st_col matrix = 
  List.fold_left matrix ~init:(LSet.empty, None) 
    ~f:(fun (s, t) row ->
      match row with
        SP_Constructor (c, _, t) :: _ -> LSet.add c s, Some t
      | SP_wildcard t :: _ -> s, Some t
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
  | SP_wildcard t :: q2_n ->
    (* let () = Format.printf "type 2 : %a \n" AST.PP.type_expression t in *)
    let complete_signature = get_all_constructors t in
    let constructors, _ = get_constructos_from_1st_col matrix in
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

let rec algorithm_I matrix n =
  if n  = 0 then
    if List.is_empty matrix then Some [[]]
    else if List.for_all matrix ~f:(List.is_empty) then None
    else failwith "edge case: algorithm I"
  else
    let constructors, t = get_constructos_from_1st_col matrix in
    let t = Option.value_exn t in
    let complete_signature = get_all_constructors t in
    if (not @@ LSet.is_empty constructors) && 
      LSet.equal constructors complete_signature then
        LSet.fold (fun ck p ->
          if Option.is_some p then p
          else
            let a  = find_constuctor_arity ck t in
            let ak = List.length a in
            let matrix = specialize_matrix ck a matrix in
            let ps = algorithm_I matrix (ak + n - 1) in
            match ps with
              Some ps -> Some 
                (List.map ps ~f:(fun ps -> [SP_Constructor (ck, ps, t)]))
            | None -> None
          ) complete_signature None
    else
      let dp = default_matrix matrix in
      let ps = algorithm_I dp (n - 1) in
      match ps with
        Some ps ->
          if LSet.is_empty constructors then
            Some (List.map ps ~f:(fun ps -> [SP_wildcard t] @ ps))
          else
            let missing_constructors 
              = LSet.diff complete_signature constructors in
            let cs = LSet.fold (fun c cs ->
              let a  = find_constuctor_arity c t in
              let a  = List.map a ~f:(fun t -> SP_wildcard t) in
              let c  = SP_Constructor (c, a, t) in
              c :: cs
            ) missing_constructors [] in
            Some (List.fold_left cs ~init:[] ~f:(fun new_ps c ->
              let ps = List.map ps ~f:(fun p -> [c] @ p) in
              ps @ new_ps
            ))
      | None -> None

let redundant_case_analysis matrix =
  fst @@ List.fold_left matrix ~init:(false, [])
    ~f:(fun (redundant_case_found, matrix) vector ->
      if redundant_case_found then (true, [])
      else if List.is_empty matrix then (false, [vector])
      else
        let redundant_case_found = not @@ algorithm_Urec matrix vector in
        (redundant_case_found, matrix @ [vector]))


let find_anomaly eqs =
  let matrix = List.map eqs ~f:(fun (p, t, _) ->
    to_simple_pattern (p, t)) in
  (* let () = List.iter eqs
    ~f:(fun (p, t, _) ->
      let sps = to_simple_pattern (p, t) in
      Format.printf "%a\n" pp_simple_pattern_list sps) in *)
      
  (* TODO: vector should be based on type *)
  let vector = List.map (List.hd_exn matrix)
    ~f:(fun sp -> 
      match sp with
        SP_wildcard t -> SP_wildcard t
      | SP_Constructor (_, _, t) -> SP_wildcard t) in
  let missing_case = algorithm_Urec matrix vector in
  let () = if missing_case then 
    let i = algorithm_I matrix (List.length vector) in
    let i = Option.value_exn i in
    Format.printf "FOUND MISSING CASE(S) \n";
    List.iter i ~f:(fun i -> Format.printf "- %a\n" pp_simple_pattern_list i);
  else () in
  let redundant_case = redundant_case_analysis matrix in
  if redundant_case then Format.printf "FOUND REDUNDANT CASE(S)";
  ()