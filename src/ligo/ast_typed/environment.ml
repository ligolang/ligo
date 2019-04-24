open Types

type element = environment_element
let make_element : type_value -> full_environment -> element =
  fun type_value source_environment -> {type_value ; source_environment}

module Small = struct
  type t = small_environment

  let empty : t = ([] , [])


  let get_environment : t -> environment = fst
  let get_type_environment : t -> type_environment = snd
  let map_environment : _ -> t -> t = fun f (a , b) -> (f a , b)
  let map_type_environment : _ -> t -> t = fun f (a , b) -> (a , f b)

  let add : string -> element -> t -> t = fun k v -> map_environment (fun x -> (k , v) :: x)
  let add_type : string -> type_value -> t -> t = fun k v -> map_type_environment (fun x -> (k , v) :: x)
  let get_opt : string -> t -> element option = fun k x -> List.assoc_opt k (get_environment x)
  let get_type_opt : string -> t -> type_value option = fun k x -> List.assoc_opt k (get_type_environment x)
end

type t = full_environment
let empty : environment = Small.(get_environment empty)
let full_empty : t = List.Ne.singleton Small.empty
let add : string -> element -> t -> t = fun k v -> List.Ne.hd_map (Small.add k v)
let add_ez : string -> type_value -> t -> t = fun k v e -> List.Ne.hd_map (Small.add k (make_element v e)) e
let add_type : string -> type_value -> t -> t = fun k v -> List.Ne.hd_map (Small.add_type k v)
let get_opt : string -> t -> element option = fun k x -> List.Ne.find_map (Small.get_opt k) x
let get_type_opt : string -> t -> type_value option = fun k x -> List.Ne.find_map (Small.get_type_opt k) x

let get_constructor : string -> t -> (type_value * type_value) option = fun k x -> (* Left is the constructor, right is the sum type *)
  let aux = fun x ->
    let aux = fun (_type_name , x) ->
      match x.type_value' with
      | T_sum m when Map.String.mem k m -> Some (Map.String.find k m , x)
      | _ -> None
    in
    List.find_map aux (Small.get_type_environment x) in
  List.Ne.find_map aux x


module PP = struct
  open Format
  open PP_helpers

  let list_sep_scope x = list_sep x (const " | ")

  let environment_element = fun ppf (k , (ele : environment_element)) ->
    fprintf ppf "%s -> %a" k PP.type_value ele.type_value

  let type_environment_element = fun ppf (k , tv) ->
    fprintf ppf "%s -> %a" k PP.type_value tv

  let environment : _ -> environment -> unit = fun ppf lst ->
    fprintf ppf "E[%a]" (list_sep environment_element (const " , ")) lst

  let type_environment = fun ppf lst ->
    fprintf ppf "T[%a]" (list_sep type_environment_element (const " , ")) lst

  let small_environment : _ -> small_environment -> unit = fun ppf e ->
    fprintf ppf "- %a\t%a"
      environment (Small.get_environment e)
      type_environment (Small.get_type_environment e)

  let full_environment : _ -> full_environment -> unit = fun ppf e ->
    fprintf ppf "@[%a]"
      (ne_list_sep small_environment (tag "@;")) e
end

