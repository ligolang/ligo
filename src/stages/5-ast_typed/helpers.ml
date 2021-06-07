open Types

let bind_lmap (l:_ label_map) =
  let open Trace in
  let open LMap in
  let aux k v prev =
    prev >>? fun prev' ->
    v >>? fun v' ->
    ok @@ add k v' prev' in
  fold aux l (ok empty)

let bind_fold_lmap f init (lmap:_ LMap.t) =
  let open Trace in
  let aux k v prev =
    prev >>? fun prev' ->
    f prev' k v
  in
  LMap.fold aux lmap init

let bind_map_lmap f map = bind_lmap (LMap.map f map)
let bind_map_lmap_t f map = bind_lmap (
  LMap.map 
    (fun ({associated_type;_}) -> 
      f associated_type)
    map)
let bind_map_lmapi f map = bind_lmap (LMap.mapi f map)

let range i j =
  let rec aux i j acc = if i >= j then acc else aux i (j-1) (j-1 :: acc) in
  aux i j []

let label_range i j =
  List.map ~f:(fun i -> Label (string_of_int i)) @@ range i j

let is_tuple_lmap m =
  List.for_all ~f:(fun i -> LMap.mem i m) @@ (label_range 0 (LMap.cardinal m))

let get_pair m =
  match (LMap.find_opt (Label "0") m , LMap.find_opt (Label "1") m) with
  | Some {associated_type=e1;_}, Some {associated_type=e2;_} -> Some (e1,e2)
  | _ -> None

let tuple_of_record (m: _ LMap.t) =
  let aux i = 
    let label = Label (string_of_int i) in
    let opt = LMap.find_opt (label) m in
    Option.bind ~f: (fun opt -> Some ((label,opt),i+1)) opt
  in
  Base.Sequence.to_list @@ Base.Sequence.unfold ~init:0 ~f:aux

let kv_list_of_t_sum ?(layout = L_tree) (m: row_element LMap.t) =
  let lst = LMap.to_kv_list m in
  match layout with
  | L_tree -> lst
  | L_comb -> (
      let aux (_ , { associated_type = _ ; decl_pos = a ; _ }) (_ , { associated_type = _ ; decl_pos = b ; _ }) = Int.compare a b in
      List.sort ~compare:aux lst
    )

let kv_list_of_t_record_or_tuple ?(layout = L_tree) (m: row_element LMap.t) =
  let lst =
    if (is_tuple_lmap m)
    then tuple_of_record m
    else LMap.to_kv_list m
  in
  match layout with
  | L_tree -> lst
  | L_comb -> (
      let aux (_ , { associated_type = _ ; decl_pos = a ; _ }) (_ , { associated_type = _ ; decl_pos = b ; _ }) = Int.compare a b in
      List.sort ~compare:aux lst
    )

let kv_list_of_record_or_tuple ~layout record_t_content record =
  let exps =
    if (is_tuple_lmap record)
    then tuple_of_record record
    else LMap.to_kv_list record
  in
  match layout with
  | L_tree -> List.map ~f:snd exps
  | L_comb -> (
    let types = LMap.to_kv_list record_t_content in
    let te = List.map ~f:(fun ((label_t,t),(label_e,e)) ->
      assert (label_t = label_e) ; (*TODO TEST*)
      (t,e)) (List.zip_exn types exps) in
    let s = List.sort ~compare:(fun ({ associated_type = _ ; decl_pos = a ; _ },_) ({ associated_type = _ ; decl_pos = b ; _ },_) -> Int.compare a b) te in
    List.map ~f:snd s
  )

let remove_empty_annotation (ann : string option) : string option =
  match ann with
  | Some "" -> None
  | Some ann -> Some (String.uncapitalize_ascii ann)
  | None -> None

let is_michelson_or (t: _ label_map) =
  let s = List.sort ~compare:(fun (Label k1, _) (Label k2, _) -> String.compare k1 k2) @@
    LMap.to_kv_list t in
  match s with
  | [ (Label "M_left", ta) ; (Label "M_right", tb) ] -> Some (ta,tb)
  | _ -> None

let is_michelson_pair (t: row_element label_map) : (row_element * row_element) option =
  match LMap.to_list t with
  | [ a ; b ] -> (
      if List.for_all ~f:(fun i -> LMap.mem i t) @@ (label_range 0 2)
      && Option.(
        is_some a.michelson_annotation || is_some b.michelson_annotation
      )
      then Some (a , b)
      else None
    )
  | _ -> None
