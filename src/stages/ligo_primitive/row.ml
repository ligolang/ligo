module Make (L : sig
  type t [@@deriving equal, compare, yojson, sexp]

  val fields : t -> Label.Set.t option
  val default : Layout.field list -> t (* ugh *)
end) =
struct
  type 'a t =
    { fields : 'a Label.Map.t
    ; layout : L.t
    }
  [@@deriving equal, compare, yojson, sexp]

  let invariant t =
    Invariant.invariant [%here] t [%sexp_of: _] (fun () ->
        Option.iter (L.fields t.layout) ~f:(fun fields ->
            assert (Set.equal fields (Map.key_set t.fields))))


  let create ~layout fields =
    let t = { fields; layout } in
    invariant t;
    t


  let create_tuple types =
    let fields =
      List.mapi
        types
        ~f:(fun i t -> (Label.of_int i, t)) in
    let layout =
      L.default
        (List.map ~f:(fun (name, _) -> Layout.{ name; annot = None }) fields) in
    let fields =
      Label.Map.of_alist_exn fields in
    { fields;
      layout }

  let of_alist_exn ~layout fields =
    let fields = Label.Map.of_alist_exn fields in
    create ~layout fields


  let hash_fold_t f state t = Map.hash_fold_m__t (module Label) f state t.fields
  let length t = Map.length t.fields

  let is_tuple t =
    List.for_all ~f:(fun i -> Label.Map.mem t.fields i) @@ Label.range 0 (length t)


  let to_tuple (t : 'a t) : 'a list =
    assert (is_tuple t);
    Map.to_alist ~key_order:`Increasing t.fields |> List.map ~f:snd


  let map : 'a 'b. ('a -> 'b) -> 'a t -> 'b t =
   fun f { fields; layout } -> { fields = Map.map fields ~f; layout }


  let iter : 'a. ('a -> unit) -> 'a t -> unit = fun f { fields; _ } -> Map.iter fields ~f
  let fold g init t = Map.fold t.fields ~init ~f:(fun ~key:_ ~data acc -> g acc data)

  let fold_map : ('acc -> 'a -> 'acc * 'b) -> 'acc -> 'a t -> 'acc * 'b t =
   fun g acc { fields; layout } ->
    let acc, fields =
      Label.Map.fold_map fields ~init:acc ~f:(fun ~key:_ ~data:field acc -> g acc field)
    in
    acc, { fields; layout }


  module PP = struct
    open Simple_utils.PP_helpers

    let row_element f ppf (k, elem) =
      Format.fprintf ppf "@[<h>%a -> %a@]" Label.pp k f elem


    let record_sep f g sep ppf (t : 'a t) =
      let lst = Map.to_alist t.fields in
      Format.fprintf ppf "%a%a" (list_sep (row_element f) sep) lst g t.layout


    let variant_sep_d x layout = record_sep x layout (tag " ,@ ")

    let tuple_or_record_type value layout ppf (t : 'a t) =
      if is_tuple t
      then Tuple.pp value ppf (to_tuple t)
      else Format.fprintf ppf "@[<hv 7>record[%a]@]" (record_sep value layout (tag " ,@ ")) t


    let sum_type type_expression layout ppf sum =
      Format.fprintf ppf "@[<hv 4>sum[%a]@]" (variant_sep_d type_expression layout) sum


    let record_type type_expression layout ppf record =
      Format.fprintf ppf "%a" (tuple_or_record_type type_expression layout) record


    let tuple_type type_expression ppf tuple =
      Format.fprintf ppf "(%a)" (list_sep type_expression (tag " , ")) tuple
  end
end

module With_optional_layout = Make (struct
  type t = Layout.t option [@@deriving equal, compare, yojson, sexp]

  let fields t = Option.map t ~f:Layout.fields
  let default fields = Some (Layout.default fields)
end)

module With_layout = struct
  module L = struct
    include Layout
    let fields t = Some (fields t)
    let default fields = Layout.default fields
  end
  include Make(L)

  (* below operations are intended for rows with layouts only *)

  let to_alist (t : 'a t) : (Label.t * 'a) list =
    List.map
      (Layout.to_list t.layout)
      ~f:(fun label -> (label, Label.Map.find_exn t.fields label))

  (* remaining operations are highly specific to layouts *)

  (* extract nice description of variant value expressed with
     left/right constructors (as in Michelson) *)
  let extract_constructor
        (row : 'a t) (value : 'v)
        (get_left : 'v -> 'v option)
        (get_right : 'v -> 'v option)
      : (Label.t * 'v * 'a) =
    let layout = Layout.to_binary row.layout in
    let rec aux (t : Layout.binary) value =
      match t with
      | Empty -> failwith (Format.asprintf "internal error: constructor not found @ %s" __LOC__)
      | Leaf {name; _} ->
         (name, value, Label.Map.find_exn row.fields name)
      | Node (l, r) ->
         (match get_left value with
          | Some value -> aux l value
          | None ->
             match get_right value with
             | Some value -> aux r value
             | None -> failwith (Format.asprintf "internal error: constructor not found @ %s" __LOC__)) in
    aux layout value

  (* extract nice description of record value expressed as pairs (as
     in Michelson) *)
  let extract_record
        (row : 'a t) (value : 'v)
        (get_pair : 'v -> ('v * 'v) option)
      : (Label.t * 'v * 'a) list =
    let get_list len value =
      let rec aux len lst value =
        if len = 1
        then Some (List.rev (value :: lst))
        else
          match get_pair value with
          | None -> None
          | Some (l, r) ->
             aux (len - 1) (l :: lst) r in
      aux len [] value in
    let rec aux (t : Layout.t) value =
      match t with
      | Field {name; _} ->
         [(name, value, Label.Map.find_exn row.fields name)]
      | Inner ts ->
         match get_list (List.length ts) value with
         | None -> failwith (Format.asprintf "internal error: field not found @ %s" __LOC__)
         | Some vs ->
            List.concat_map
              (List.zip_exn ts vs)
              ~f:(fun (t, v) -> aux t v)
    in
    aux row.layout value

end
