module AST = Ast_typed
module T = Stage_common.Types
module C = AST.Combinators

module LMap = AST.LMap
module XList = Simple_utils.List

let unit_label = T.Label "#UNIT"
let cons_label = T.Label "#CONS"
let nil_label  = T.Label "#NIL"

type simple_pattern =
    SP_wildcard
  | SP_Constructor of T.label * (simple_pattern list) * AST.type_expression

let rec pp_simple_pattern ppf sp =
  match sp with
    SP_wildcard -> Format.fprintf ppf "_"
  | SP_Constructor (Label c, ps, _) ->
    Format.fprintf ppf "%s (%s)" c 
      (String.concat ~sep:", " 
        (List.map ps ~f:(fun p -> Format.asprintf "%a" pp_simple_pattern p)))

let rec pp_simple_pattern_list ppf sps =
  List.iter sps ~f:(fun sp -> Format.fprintf ppf "%a, " pp_simple_pattern sp)

let get_variant_nested_type label (t_sum : AST.t_sum) =
  let label_map = t_sum.content in
  let c = LMap.find_opt label label_map in
  let c = Option.value_exn c in (* BAD *)
  c.associated_type

let rec to_simple_pattern ty_pattern =
  let pattern, ty = ty_pattern in
  let pattern = T.Location.unwrap pattern in
  match pattern with
    AST.P_unit -> [SP_Constructor (unit_label, [SP_wildcard], C.t_unit ())]
  | P_var _    -> [SP_wildcard]
  | P_list (Cons (hd, tl)) ->
    let hd_ty = Option.value_exn (C.get_t_list ty) in (* BAD *)
    let hd_tl = 
      to_simple_pattern (hd, hd_ty) @
      to_simple_pattern (tl, ty) in
    [SP_Constructor (cons_label, hd_tl, ty)]
  | P_list (List ps) ->
    let hd_ty = Option.value_exn (C.get_t_list ty) in (* BAD *)
    List.fold_right ps ~init:([SP_Constructor (nil_label, [SP_wildcard], ty)])
      ~f:(fun p acc ->
        let hd_tl = to_simple_pattern (p, hd_ty) @ acc in
        [SP_Constructor (cons_label, hd_tl, hd_ty)])
  | P_variant (c, p) ->
    let p_ty = get_variant_nested_type c (Option.value_exn (C.get_t_sum ty)) in
    [SP_Constructor (c, to_simple_pattern (p, p_ty), ty)]
  | P_tuple ps
  | P_record (_, ps) ->
    let ps_tys = Option.value_exn (C.get_t_tuple ty) in (* BAD *)
    let ps = List.zip_exn ps ps_tys in
    let ps = List.map ps ~f:to_simple_pattern in
    List.concat ps

let find_anomaly eqs =
  List.iter eqs
    ~f:(fun (p, t, _) -> 
      let sps = to_simple_pattern (p, t) in
      Format.printf "%a\n" pp_simple_pattern_list sps)

type matrix = simple_pattern list list

(* specialize *)
let specialize_matrix c a matrix = 
  let specialize row specialized = 
    match row with
      SP_Constructor (cp, r1_a, _) :: p2_n when T.equal_label c cp ->
        let row = r1_a @ p2_n in
        row :: specialized
    | SP_Constructor _  :: _ -> specialized
    | SP_wildcard :: p2_n ->
      let wildcards = XList.repeat a SP_wildcard in
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
  | SP_wildcard :: q2_n ->
    let wildcards = XList.repeat a SP_wildcard in
    wildcards @ q2_n
  | _ -> failwith "edge case: specialize_vector wrong constructor"

(* default *)
let default_matrix matrix = 
  let default row dp =
    match row with
      SP_Constructor _ :: _ -> dp
    | SP_wildcard   :: p2_n -> p2_n
    | [] -> [] (* TODO: check is this okay? *)
  in
  List.fold_right matrix ~init:[] ~f:default