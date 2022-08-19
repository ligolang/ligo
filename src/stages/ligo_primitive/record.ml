
module LMap = struct
  include Simple_utils.Map.MakeHashable(Label)

  let to_yojson f lmap =
    let lst = List.sort ~compare:(fun (a, _) (b, _) -> Label.compare a b) (bindings lmap) in
    let lst' = List.fold_left
        ~f:(fun acc (Label k, v) -> (k , f v)::acc)
        ~init:[] lst
    in
    `Assoc lst'

  let of_yojson _f _lmap =
    failwith "TODO"

end

type 'a t = 'a LMap.t
  [@@deriving eq,yojson,hash]

let cmp2 f a1 b1 g a2 b2 = match f a1 b1 with 0 -> g a2 b2 | c -> c

let compare compare lma lmb =
  let ra = LMap.to_kv_list_rev lma in
  let rb = LMap.to_kv_list_rev lmb in
  let aux (la,a) (lb,b) =
    cmp2 Label.compare la lb compare a b in
  List.compare aux ra rb


let fold : ('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc
= fun f acc record ->
  LMap.fold (
    fun _ a acc -> f acc a
  ) record acc

let map : ('a -> 'b) -> 'a t -> 'b t
= fun f record ->
  LMap.map f record

let fold_map : ('acc -> 'a -> 'acc * 'b) -> 'acc -> 'a t -> 'acc * 'b t
= fun f acc record ->
  LMap.fold_map ~f:(
    fun _ a acc -> f acc a
  ) ~init:acc record

let of_list = LMap.of_list

let is_tuple m =
  List.for_all ~f:(fun i -> LMap.mem i m) @@ (Label.range 0 (LMap.cardinal m))

let tuple_of_record (m: _ t) =
  let aux i =
    let label = Label.of_int i in
    let opt = LMap.find_opt (label) m in
    Option.bind ~f:(fun opt -> Some ((label,opt),i+1)) opt
  in
  Base.Sequence.to_list @@ Base.Sequence.unfold ~init:0 ~f:aux

module PP = struct
  open Format
  open Simple_utils.PP_helpers

  let record_sep_expr value sep ppf (m : 'a t) =
    let lst = LMap.to_kv_list m in
    let lst = List.dedup_and_sort ~compare:(fun (a,_) (b,_) -> Label.compare a b) lst in
    let new_pp ppf (k, v) = fprintf ppf "@[<h>%a -> %a@]" Label.pp k value v in
    fprintf ppf "%a" Simple_utils.PP_helpers.(list_sep new_pp sep) lst

  let tuple_sep_expr value sep ppf m =
    assert (is_tuple m);
    let lst = tuple_of_record m in
    let new_pp ppf (_,v) = fprintf ppf "%a" value v in
    fprintf ppf "%a" Simple_utils.PP_helpers.(list_sep new_pp sep) lst

  (* Prints records which only contain the consecutive fields
    0..(cardinal-1) as tuples *)
  let tuple_or_record_sep_expr value format_record sep_record format_tuple sep_tuple ppf m =
    if is_tuple m then
      fprintf ppf format_tuple (tuple_sep_expr value (tag sep_tuple)) m
    else
      fprintf ppf format_record (record_sep_expr value (tag sep_record)) m

  let tuple_or_record_sep_expr value = tuple_or_record_sep_expr value "@[<hv 7>record[%a]@]" " ,@ " "@[<hv 2>( %a )@]" " ,@ "
end

let pp f ppf = fun r ->
  Format.fprintf ppf "%a" (PP.tuple_or_record_sep_expr f) r

let pp_tuple f ppf = fun t ->
  Format.fprintf ppf "(@[<h>%a@])"
    Simple_utils.PP_helpers.(list_sep f (tag " , ")) t
