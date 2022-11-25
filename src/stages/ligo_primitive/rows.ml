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

  let sexp_of_t _ _ = failwith "todo"

  let t_of_sexp _ _ = failwith "todo"
end

type 'a row_element_mini_c = {
  associated_type      : 'a ;
  michelson_annotation : string option [@hash.ignore] ;
  decl_pos : int [@hash.ignore] ;
  } [@@deriving eq,yojson,hash, sexp]

let cmp3 f a1 b1 g a2 b2 h a3 b3 = match f a1 b1 with 0 -> (match g a2 b2 with 0 -> h a3 b3 | c -> c) | c -> c
let compare_row_element_mini_c f {associated_type=aa;michelson_annotation=ma;decl_pos=da} {associated_type=ab;michelson_annotation=mb;decl_pos=db} =
  cmp3
    f aa ab
    (Option.compare String.compare) ma mb
    Int.compare     da db

let map_row_element_mini_c : ('a -> 'b) -> 'a row_element_mini_c -> 'b row_element_mini_c
= fun g {associated_type ; michelson_annotation ; decl_pos}  ->
  let associated_type = g associated_type in
  ({associated_type ; michelson_annotation ; decl_pos})

type 'a row_element = {
  associated_type : 'a ;
  attributes      : string list ;
  decl_pos        : int ;
  } [@@deriving eq,compare,yojson,hash,fold,map, sexp]

type 'a t = {
  fields     : 'a row_element LMap.t;
  attributes : string list ;
  } [@@deriving eq,compare,yojson,hash,fold,map, sexp]

module PP = struct
  open Simple_utils.PP_helpers
  let row_element f ppf (k, ({associated_type;_} : _ row_element)) =
    Format.fprintf ppf "@[<h>%a -> %a@]"
      Label.pp k
      f associated_type

  let record_sep f sep ppf (m : 'a LMap.t) =
    let lst = LMap.to_kv_list m in
    let lst = List.dedup_and_sort ~compare:(fun (a,_) (b,_) -> Label.compare a b) lst in
    Format.fprintf ppf "%a" (list_sep (row_element f) sep) lst
  let variant_sep_d x = record_sep x (tag " ,@ ")

  let tuple_sep f sep ppf m =
    assert (Record.is_tuple m);
    let lst = Record.tuple_of_record m in
    Format.fprintf ppf "%a" (list_sep (row_element f) sep) lst

  let tuple_or_record_sep_t value format_record sep_record format_tuple sep_tuple ppf m =
    if Record.is_tuple m then
      Format.fprintf ppf format_tuple (tuple_sep value (tag sep_tuple)) m
    else
      Format.fprintf ppf format_record (record_sep value (tag sep_record)) m

  let tuple_or_record_sep_type value = tuple_or_record_sep_t value "@[<hv 7>record[%a]@]" " ,@ " "@[<hv 2>( %a )@]" " *@ "
  let sum type_expression ppf = fun sum ->
    Format.fprintf ppf "@[<hv 4>sum[%a]@]" (variant_sep_d type_expression) sum

  let type_record type_expression ppf = fun record ->
    Format.fprintf ppf "%a" (tuple_or_record_sep_type type_expression) record

  let type_tuple type_expression ppf = fun tuple ->
    Format.fprintf ppf "(%a)" (list_sep type_expression (tag " , ")) tuple

end


let fold_map : ('acc -> 'a -> 'acc * 'b) -> 'acc -> 'a t -> 'acc * 'b t
= fun g acc {fields;attributes} ->
  let acc,fields = LMap.fold_map
  ~f:(fun _ {associated_type;attributes;decl_pos} acc ->
    let acc,associated_type = g acc associated_type in
     (acc,({associated_type;attributes;decl_pos}:'b row_element))
  ) ~init:acc fields in
  (acc,{fields;attributes})
