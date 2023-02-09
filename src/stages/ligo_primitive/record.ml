type 'a t = 'a Label.Map.t [@@deriving equal, compare, yojson, sexp]

let hash_fold_t f state t = Map.hash_fold_m__t (module Label) f state t
let empty = Label.Map.empty

let fold : 'a t -> init:'acc -> f:('acc -> 'a -> 'acc) -> 'acc =
 fun record ~init ~f -> Map.fold ~f:(fun ~key:_ ~data:a acc -> f acc a) record ~init


let fold_map : 'a t -> init:'acc -> f:('acc -> 'a -> 'acc * 'b) -> 'acc * 'b t =
 fun record ~init ~f ->
  Label.Map.fold_map ~f:(fun ~key:_ ~data:a acc -> f acc a) ~init record


let iter : 'a t -> f:('a -> unit) -> unit =
 fun record ~f -> Label.Map.iter ~f:(fun a -> f a) record


let map : 'a t -> f:('a -> 'b) -> 'b t = fun record ~f -> Map.map ~f record
let mapi t ~f = Map.mapi t ~f:(fun ~key ~data -> f ~label:key ~value:data)
let exists = Label.Map.exists
let of_list = Label.Map.of_alist_exn
let to_list t = Map.to_alist t
let to_list_rev t = Map.to_alist ~key_order:`Decreasing t
let labels t = Map.keys t
let values t = Map.data t
let find = Label.Map.find_exn
let find_opt = Label.Map.find
let update = Map.update
let cardinal m = Map.count m ~f:(fun _ -> true) (* ?? *)
let mem = Map.mem
let set = Map.set (* ?? *)

let update_opt t label ~f =
  match f (find_opt t label) with
  | Some value -> Map.set t ~key:label ~data:value
  | None -> t


let is_tuple m =
  List.for_all ~f:(fun i -> Label.Map.mem m i) @@ Label.range 0 (Label.Map.length m)


let tuple_of_record (m : _ t) =
  let aux i =
    let label = Label.of_int i in
    let opt = Label.Map.find m label in
    Option.bind ~f:(fun opt -> Some ((label, opt), i + 1)) opt
  in
  Base.Sequence.to_list @@ Base.Sequence.unfold ~init:0 ~f:aux


let record_of_tuple (l : _ list) =
  of_list @@ List.mapi ~f:(fun i v -> Label.of_int i, v) l


module PP = struct
  open Format
  open Simple_utils.PP_helpers

  let record_sep_expr value sep ppf (m : 'a t) =
    let lst = to_list m in
    let lst = List.dedup_and_sort ~compare:(fun (a, _) (b, _) -> Label.compare a b) lst in
    let new_pp ppf (k, v) = fprintf ppf "@[<h>%a -> %a@]" Label.pp k value v in
    fprintf ppf "%a" Simple_utils.PP_helpers.(list_sep new_pp sep) lst


  let tuple_sep_expr value sep ppf m =
    assert (is_tuple m);
    let lst = tuple_of_record m in
    let new_pp ppf (_, v) = fprintf ppf "%a" value v in
    fprintf ppf "%a" Simple_utils.PP_helpers.(list_sep new_pp sep) lst


  (* Prints records which only contain the consecutive fields
    0..(cardinal-1) as tuples *)
  let tuple_or_record_sep_expr value format_record sep_record format_tuple sep_tuple ppf m
    =
    if is_tuple m
    then fprintf ppf format_tuple (tuple_sep_expr value (tag sep_tuple)) m
    else fprintf ppf format_record (record_sep_expr value (tag sep_record)) m


  let tuple_or_record_sep_expr value =
    tuple_or_record_sep_expr value "@[<hv 7>record[%a]@]" " ,@ " "@[<hv 2>( %a )@]" " ,@ "
end

let pp f ppf r = Format.fprintf ppf "%a" (PP.tuple_or_record_sep_expr f) r

let pp_tuple f ppf t =
  Format.fprintf ppf "(@[<h>%a@])" Simple_utils.PP_helpers.(list_sep f (tag " , ")) t
