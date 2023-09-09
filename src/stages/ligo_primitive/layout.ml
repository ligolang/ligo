type 'field f =
  | Inner of 'field f list
  | Field of 'field
[@@deriving map, equal, compare, yojson, hash, sexp]

type t = field f

and field =
  { name : Label.t
  ; annot : string option
  }
[@@deriving equal, compare, yojson, hash, sexp]

let comb fields =
  (* this is a hack to get tuple values to work properly when their
     type is only inferred, e.g. in the "usage_orig" example in
     gitlab-pages/docs/advanced/testing.md ... TODO?

     perhaps a better fix is to allow record values to specify their
     layout, and then to specify the correct tuple layout in
     desugar_tuple_to_record in desugaring pass? *)
  let is_int field =
    Option.is_some (int_of_string_opt (Label.to_string field.name)) in
  let fields =
    if List.for_all fields ~f:is_int
    then
      List.sort
        ~compare:(fun field1 field2 -> Label.compare field1.name field2.name)
        fields
    else
      fields in
  Inner (List.map fields ~f:(fun field -> Field field))

let rec of_append_tree' : field Simple_utils.Tree.Append.t' -> t = function
  | Leaf field -> Field field
  | Node { a; b; size = _; full = _ } -> Inner [ of_append_tree' a; of_append_tree' b ]


let of_append_tree : field Simple_utils.Tree.Append.t -> t = function
  | Empty -> Inner []
  | Full node -> of_append_tree' node



let tree fields =
  let fields =
    List.sort ~compare:(fun field1 field2 -> Label.compare field1.name field2.name) fields
  in
  of_append_tree (Simple_utils.Tree.Append.of_list fields)

(* evil hack to avoid unraveling dependencies on [default] *)
let legacy_layout_flag =
  match Sys.getenv "LIGO_LEGACY_LAYOUT_TREE" with
  | Some _ -> true
  | None -> false

let default =
  if legacy_layout_flag
  then tree
  else comb

let default_of_labels : Label.t list -> field f = fun lst ->
  let layout_fields = List.map ~f:(fun name -> { name ; annot = None }) lst in
    default layout_fields

let rec fields t =
  match t with
  | Inner ts -> ts |> List.map ~f:fields |> Label.Set.union_list
  | Field { name; _ } -> Label.Set.singleton name


let rec to_list (t : t) =
  match t with
  | Inner ts -> List.concat_map ~f:to_list ts
  | Field { name; _ } -> [ name ]


let rec annot (t : t) (label : Label.t) =
  let aux { name; annot } = if Label.equal label name then annot else None in
  match t with
  | Inner ts -> List.find_map ~f:(fun t -> annot t label) ts
  | Field x -> aux x


let rec find_annot (t : t) (annot : string) =
  let aux = function
    | { name; annot = Some annot' } when String.equal annot annot' -> Some name
    | _ -> None
  in
  match t with
  | Inner ts -> List.find_map ~f:(fun t -> find_annot t annot) ts
  | Field x -> aux x


(* For Michelson [or] there is no special "comb" support; we must
   ultimately pick a binary tree layout. So it will be convenient to
   expand out to binary trees: *)

type binary =
  | Empty (* should not need this? but might as well have it? *)
  | Node of binary * binary
  | Leaf of field

let rec to_binary : t -> binary = function
  | Field f -> Leaf f
  | Inner ts ->
    let ts = List.map ~f:to_binary ts in
    List.fold_right
      ts
      ~f:(fun t acc ->
        match acc with
        | Empty -> t
        | _ -> Node (t, acc))
      ~init:Empty


let rec pp ppf (t : t) =
  match t with
  | Inner ts ->
    Format.fprintf
      ppf
      "(%a)"
      (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ") pp)
      ts
  | Field { name; annot } ->
    Format.fprintf
      ppf
      "{ name: %a%a }"
      Label.pp
      name
      (Format.pp_print_option
         ~none:(fun ppf () -> Format.fprintf ppf "")
         (fun ppf annot -> Format.fprintf ppf " annot: %s" annot))
      annot
