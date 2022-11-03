(* This file represente the context which give the association of values to types *)
module Location = Simple_utils.Location
module List = Simple_utils.List
open Type
open Ligo_prim

type 'a hashable = (module Caml.Hashtbl.HashedType with type t = 'a)

let memoize (type a b) ?(size = 100) (key : a hashable) (f : a -> b) =
  let module Hashtbl = Caml.Ephemeron.K1.Make ((val key)) in
  let table : b Hashtbl.t = Hashtbl.create size in
  fun key ->
    match Hashtbl.find_opt table key with
    | Some result -> result
    | None ->
      let result = f key in
      Hashtbl.add table key result;
      result

(* 
let rec_memoize
    (type a b)
    ?(size = 100)
    (key : a hashable)
    (f : (a -> b) -> a -> b)
  =
  let module Hashtbl = Caml.Ephemeron.K1.Make ((val key)) in
  let table : b Hashtbl.t = Hashtbl.create size in
  let rec memo_f key =
    match Hashtbl.find_opt table key with
    | Some result -> result
    | None ->
      let result = f memo_f key in
      Hashtbl.add table key result;
      result
  in
  memo_f *)


let memoize2
    (type a b c)
    ?(size = 100)
    (key1 : a hashable)
    (key2 : b hashable)
    (f : a -> b -> c)
  =
  let module Hashtbl = Caml.Ephemeron.K2.Make ((val key1)) ((val key2)) in
  let table : c Hashtbl.t = Hashtbl.create size in
  fun key1 key2 ->
    match Hashtbl.find_opt table (key1, key2) with
    | Some result -> result
    | None ->
      let result = f key1 key2 in
      Hashtbl.add table (key1, key2) result;
      result


module Phys_hashable (T : T) = struct
  include T

  let equal = phys_equal
  let hash t = Caml.Hashtbl.hash t
end

module Signature = struct
  module T = struct
    type t = item list

    and item =
      | S_value of Value_var.t * Type.t
      | S_type of Type_var.t * Type.t
      | S_module of Module_var.t * t
    [@@deriving hash]
  end

  include T

  let hashable : t hashable = (module Phys_hashable (T))
  let find_map t ~f = List.find_map (List.rev t) ~f

  let get_value =
    memoize2
      hashable
      (module Value_var)
      (fun t var ->
        (find_map t ~f:(function
            | S_value (var', type_) when Value_var.equal var var' -> Some type_
            | _ -> None) [@landmark "get_value"]))


  let get_type =
    memoize2
      hashable
      (module Type_var)
      (fun t tvar ->
        (find_map t ~f:(function
            | S_type (tvar', type_) when Type_var.equal tvar tvar' -> Some type_
            | _ -> None) [@landmark "get_type"]))


  let get_module =
    memoize2
      hashable
      (module Module_var)
      (fun t mvar ->
        (find_map t ~f:(function
            | S_module (mvar', sig_) when Module_var.equal mvar mvar' ->
              Some sig_
            | _ -> None) [@landmark "get_module"]))


  let rec equal_item : item -> item -> bool =
   fun item1 item2 ->
    match item1, item2 with
    | S_value (var1, type1), S_value (var2, type2) ->
      Value_var.equal var1 var2 && Type.equal type1 type2
    | S_type (tvar1, type1), S_type (tvar2, type2) ->
      Type_var.equal tvar1 tvar2 && Type.equal type1 type2
    | S_module (mvar1, sig1), S_module (mvar2, sig2) ->
      Module_var.equal mvar1 mvar2 && equal sig1 sig2
    | _, _ -> false


  and equal t1 t2 = List.equal equal_item t1 t2

  let to_type_mapi =
    let next = ref 0 in
    memoize hashable (fun t ->
        (List.fold_right t ~init:Type_var.Map.empty ~f:(fun item map ->
             Int.incr next;
             match item with
             | S_type (tvar, type_) -> Map.set map ~key:tvar ~data:(!next, type_)
             | _ -> map) [@landmark "to_type_mapi"]))


  let to_module_mapi =
    let next = ref 0 in
    memoize hashable (fun t ->
        (List.fold_right t ~init:Module_var.Map.empty ~f:(fun item map ->
             Int.incr next;
             match item with
             | S_module (mvar, t) -> Map.set map ~key:mvar ~data:(!next, t)
             | _ -> map) [@landmark "to_module_map"]))


  let to_type_map =
    memoize hashable (fun t ->
        (List.fold_right t ~init:Type_var.Map.empty ~f:(fun item map ->
             match item with
             | S_type (tvar, type_) -> Map.set map ~key:tvar ~data:type_
             | _ -> map) [@landmark "to_type_map"]))


  let to_module_map =
    memoize hashable (fun t ->
        (List.fold_right t ~init:Module_var.Map.empty ~f:(fun item map ->
             match item with
             | S_module (mvar, t) -> Map.set map ~key:mvar ~data:t
             | _ -> map) [@landmark "to_module_map"]))


  include struct
    let list ~pp ppf xs =
      let rec loop ppf = function
        | [] -> Format.fprintf ppf ""
        | x :: xs -> Format.fprintf ppf "%a@.%a" pp x loop xs
      in
      Format.fprintf ppf "@[<v>%a@]" loop xs


    let rec pp_item ppf item =
      match item with
      | S_value (var, type_) ->
        Format.fprintf ppf "%a : %a" Value_var.pp var Type.pp type_
      | S_type (tvar, type_) ->
        Format.fprintf ppf "type %a = %a" Type_var.pp tvar Type.pp type_
      | S_module (mvar, sig_) ->
        Format.fprintf ppf "module %a = %a" Module_var.pp mvar pp sig_


    and pp ppf t = Format.fprintf ppf "@[<v>sig@,%a@,end@]" (list ~pp:pp_item) t
  end
end

type mutable_flag = Param.mutable_flag =
  | Mutable
  | Immutable
[@@deriving hash]

type pos = int [@@deriving hash]
type mut_lock = int [@@deriving hash]

module T = struct
  type t = item list

  and item =
    | C_value of Value_var.t * mutable_flag * Type.t
    | C_type of Type_var.t * Type.t
    | C_type_var of Type_var.t * Kind.t
    | C_module of Module_var.t * Signature.t
    | C_texists_var of Type_var.t * Kind.t
    | C_texists_eq of Type_var.t * Kind.t * Type.t
    | C_lexists_var of Layout_var.t
    | C_lexists_eq of Layout_var.t * Type.layout
    | C_pos of pos
    | C_mut_lock of mut_lock
  [@@deriving hash]
end

include T

let hashable : t hashable = (module Phys_hashable (T))

module PP = struct
  let list ~pp ppf xs =
    let rec loop ppf = function
      | [] -> Format.fprintf ppf ""
      | x :: xs -> Format.fprintf ppf "%a@,%a" loop xs pp x
    in
    Format.fprintf ppf "@[<hv>%a@]" loop xs


  let context ppf t =
    list ppf t ~pp:(fun ppf item ->
        match item with
        | C_value (evar, mut_flag, type_) ->
          Format.fprintf
            ppf
            "%a%a : %a"
            Param.pp_mutable_flag
            mut_flag
            Value_var.pp
            evar
            Type.pp
            type_
        | C_type (tvar, type_) ->
          Format.fprintf ppf "type %a = %a" Type_var.pp tvar Type.pp type_
        | C_type_var (tvar, kind) ->
          Format.fprintf ppf "%a :: %a" Type_var.pp tvar Kind.pp kind
        | C_texists_var (evar, kind) ->
          Format.fprintf ppf "^%a :: %a" Type_var.pp evar Kind.pp kind
        | C_texists_eq (evar, kind, type_) ->
          Format.fprintf
            ppf
            "^%a :: %a = %a"
            Type_var.pp
            evar
            Kind.pp
            kind
            Type.pp
            type_
        | C_lexists_var lvar ->
          Format.fprintf ppf "layout ^%a" Layout_var.pp lvar
        | C_lexists_eq (lvar, layout) ->
          Format.fprintf
            ppf
            "layout ^%a = %a"
            Layout_var.pp
            lvar
            Type.pp_layout
            layout
        | C_module (mvar, sig_) ->
          Format.fprintf
            ppf
            "module %a = %a"
            Module_var.pp
            mvar
            Signature.pp
            sig_
        | C_pos _ | C_mut_lock _ -> ())
end

let pp = PP.context
let empty = []
let add t item = item :: t
let join t1 t2 = t2 @ t1
let of_list items = List.rev items

(* Inifix notations for [add] and [join] *)
let ( |:: ) = add
let ( |@ ) = join
let add_value t var mut_flag type_ = t |:: C_value (var, mut_flag, type_)
let add_imm t var type_ = t |:: C_value (var, Immutable, type_)
let add_mut t var type_ = t |:: C_value (var, Mutable, type_)
let add_type t tvar type_ = t |:: C_type (tvar, type_)
let add_type_var t tvar kind = t |:: C_type_var (tvar, kind)
let add_texists_var t tvar kind = t |:: C_texists_var (tvar, kind)
let add_module t mvar mctx = t |:: C_module (mvar, mctx)
let add_lexists_var t lvar = t |:: C_lexists_var lvar

let item_of_signature_item (sig_item : Signature.item) : item =
  match sig_item with
  | S_value (var, type_) -> C_value (var, Immutable, type_)
  | S_type (tvar, type_) -> C_type (tvar, type_)
  | S_module (mvar, sig_) -> C_module (mvar, sig_)


let add_signature_item t (sig_item : Signature.item) =
  add t (item_of_signature_item sig_item)


let get_value =
  memoize2
    hashable
    (module Value_var)
    (fun t var ->
      let[@landmark "get_value"] rec loop ?(locked = false) items =
        match items with
        | C_value (var', mut_flag, type_) :: _ when Value_var.equal var var' ->
          (match mut_flag, locked with
          | Mutable, true -> Error `Mut_var_captured
          | _ -> Ok (mut_flag, type_))
        | C_mut_lock _ :: items -> loop ~locked:true items
        | _ :: items -> loop ~locked items
        | [] -> Error `Not_found
      in
      loop t)


let get_imm =
  memoize2
    hashable
    (module Value_var)
    (fun t var ->
      List.find_map t ~f:(function
          | C_value (var', Immutable, type_) when Value_var.equal var var' ->
            Some type_
          | _ -> None))


let get_mut =
  memoize2
    hashable
    (module Value_var)
    (fun t var ->
      let rec loop = function
        | C_value (var', Mutable, type_) :: _ when Value_var.equal var var' ->
          Some type_
        | C_mut_lock _ :: _ -> None
        | _ :: items -> loop items
        | [] -> None
      in
      loop t)


let get_type =
  memoize2
    hashable
    (module Type_var)
    (fun t tvar ->
      (List.find_map t ~f:(function
          | C_type (tvar', type_) when Type_var.equal tvar tvar' -> Some type_
          | _ -> None) [@landmark "get_type"]))


let get_module =
  memoize2
    hashable
    (module Module_var)
    (fun t mvar ->
      (List.find_map t ~f:(function
          | C_module (mvar', mctx) when Module_var.equal mvar mvar' -> Some mctx
          | _ -> None) [@landmark "get_module"]))


let get_type_vars =
  memoize hashable (fun t ->
      (List.filter_map t ~f:(function
          | C_type_var (tvar, _) -> Some tvar
          | _ -> None) [@landmark "get_type_vars"])
      |> Type_var.Set.of_list)


let get_texists_vars =
  memoize hashable (fun t ->
      (List.filter_map t ~f:(function
          | C_texists_var (tvar, _) | C_texists_eq (tvar, _, _) -> Some tvar
          | _ -> None) [@landmark "get_texists_vars"])
      |> Type_var.Set.of_list)


let get_lexists_vars =
  memoize hashable (fun t ->
      (List.filter_map t ~f:(function
          | C_lexists_var lvar | C_lexists_eq (lvar, _) -> Some lvar
          | _ -> None) [@landmark "get_lexists_vars"])
      |> Layout_var.Set.of_list)


let get_texists_var =
  memoize2
    hashable
    (module Type_var)
    (fun t tvar ->
      (List.find_map t ~f:(function
          | C_texists_var (tvar', kind) when Type_var.equal tvar tvar' ->
            Some kind
          | _ -> None) [@landmark "get_exists_var"]))


let get_texists_eq =
  memoize2
    hashable
    (module Type_var)
    (fun t tvar ->
      (List.find_map t ~f:(function
          | C_texists_eq (tvar', _kind, type_) when Type_var.equal tvar tvar' ->
            Some type_
          | _ -> None) [@landmark "get_exists_eq"]))


let get_type_var =
  memoize2
    hashable
    (module Type_var)
    (fun t tvar ->
      (List.find_map t ~f:(function
          | C_type_var (tvar', kind) when Type_var.equal tvar tvar' -> Some kind
          | _ -> None) [@landmark "get_type_var"]))


let get_lexists_eq =
  memoize2
    hashable
    (module Layout_var)
    (fun t lvar ->
      (List.find_map t ~f:(function
          | C_lexists_eq (lvar', layout) when Layout_var.equal lvar lvar' ->
            Some layout
          | _ -> None) [@landmark "get_layout_exists_eq"]))


module Apply = struct
  let rec type_ ctx (t : Type.t) : Type.t =
    let apply = type_ ctx in
    let return content = { t with content } in
    match t.content with
    | T_exists tvar ->
      (match get_texists_eq ctx tvar with
      | Some t -> apply t
      | None -> t)
    | T_variable _tvar -> t
    | T_construct construct ->
      let parameters = List.map ~f:apply construct.parameters in
      return @@ T_construct { construct with parameters }
    | T_sum row' ->
      let row = row ctx row' in
      return @@ T_sum row
    | T_record row' ->
      let row = row ctx row' in
      return @@ T_record row
    | T_arrow arr ->
      let arr = Arrow.map apply arr in
      return @@ T_arrow arr
    | T_singleton _ -> t
    | T_abstraction abs ->
      let abs = Abstraction.map apply abs in
      return @@ T_abstraction abs
    | T_for_all for_all ->
      let for_all = Abstraction.map apply for_all in
      return @@ T_for_all for_all


  and row ctx (t : Type.row) : Type.row =
    let fields = Record.map ~f:(row_elem ctx) t.fields in
    let layout = layout ctx t.layout in
    { fields; layout }


  and row_elem ctx (t : Type.row_element) : Type.row_element =
    Rows.map_row_element_mini_c (type_ ctx) t


  and layout ctx (t : Type.layout) : Type.layout =
    match t with
    | L_tree | L_comb -> t
    | L_exists lvar ->
      (match get_lexists_eq ctx lvar with
      | Some t -> layout ctx t
      | None -> t)


  let rec sig_item ctx (sig_item : Signature.item) : Signature.item =
    match sig_item with
    | S_type (tvar, type') -> S_type (tvar, type_ ctx type')
    | S_value (var, type') -> S_value (var, type_ ctx type')
    | S_module (mvar, sig') -> S_module (mvar, sig_ ctx sig')


  and sig_ ctx (sig_ : Signature.t) : Signature.t =
    List.map sig_ ~f:(sig_item ctx)
end

let equal_item : item -> item -> bool =
 fun item1 item2 ->
  match item1, item2 with
  | C_value (var1, mut_flag1, type1), C_value (var2, mut_flag2, type2) ->
    Value_var.equal var1 var2
    && Param.equal_mutable_flag mut_flag1 mut_flag2
    && Type.equal type1 type2
  | C_type (tvar1, type1), C_type (tvar2, type2) ->
    Type_var.equal tvar1 tvar2 && Type.equal type1 type2
  | C_type_var (tvar1, kind1), C_type_var (tvar2, kind2) ->
    Type_var.equal tvar1 tvar2 && Kind.equal kind1 kind2
  | C_texists_var (tvar1, kind1), C_texists_var (tvar2, kind2) ->
    Type_var.equal tvar1 tvar2 && Kind.equal kind1 kind2
  | C_texists_eq (evar1, kind1, type1), C_texists_eq (evar2, kind2, type2) ->
    Type_var.equal evar1 evar2
    && Kind.equal kind1 kind2
    && Type.equal type1 type2
  | C_module (mvar1, sig1), C_module (mvar2, sig2) ->
    Module_var.equal mvar1 mvar2 && Signature.equal sig1 sig2
  | C_pos pos1, C_pos pos2 -> pos1 = pos2
  | C_mut_lock lock1, C_mut_lock lock2 -> lock1 = lock2
  | _, _ -> false


let drop_until t ~f =
  let rec loop t =
    match t with
    | [] -> t, Substitution.empty
    | item :: t when f item -> t, Substitution.empty
    | item :: t ->
      let t, subst = loop t in
      let subst =
        match item with
        | C_texists_eq (tvar, kind, type_) ->
          Substitution.add_texists_eq subst tvar kind type_
        | C_lexists_eq (lvar, layout) ->
          Substitution.add_lexists_eq subst lvar layout
        | _ -> subst
      in
      t, subst
  in
  loop t


let split_at t ~at =
  let rec loop t =
    match t with
    | [] -> [], []
    | item :: t ->
      if equal_item item at
      then t, []
      else (
        let t1, t2 = loop t in
        t1, item :: t2)
  in
  loop t


let unsolved t =
  let rec loop t =
    match t with
    | [] -> [], Substitution.empty
    | item :: t ->
      let t, subst = loop t in
      let t =
        match item with
        | C_texists_var _ | C_lexists_var _ -> item :: t
        | _ -> t
      in
      let subst =
        match item with
        | C_texists_eq (tvar, kind, type_) ->
          Substitution.add_texists_eq subst tvar kind type_
        | C_lexists_eq (lvar, layout) ->
          Substitution.add_lexists_eq subst lvar layout
        | _ -> subst
      in
      t, subst
  in
  loop t


let lift t type_ ~apply ~at =
  let t1, t2 = split_at t ~at in
  let type_ = apply t2 type_ in
  let unsolved, subst = unsolved t2 in
  (t1 |@ unsolved, type_), subst


type 'a apply = t -> 'a -> 'a

type 'a exit =
  | Drop : t exit
  | Lift : 'a apply -> (t * 'a) exit

let mark =
  let next = ref 0 in
  fun t ->
    let pos =
      Int.incr next;
      !next
    in
    t |:: C_pos pos, pos


let lock =
  let next = ref 0 in
  fun t ->
    let lock =
      Int.incr next;
      !next
    in
    t |:: C_mut_lock lock, lock


let unlock (type a) (t : a) ~(on_exit : a exit) ~lock : a * Substitution.t =
  match on_exit, t with
  | Drop, t ->
    drop_until t ~f:(function
        | C_mut_lock lock' -> lock = lock'
        | _ -> false)
  | Lift apply, (t, x) -> lift t x ~apply ~at:(C_mut_lock lock)


let drop_until (type a) (t : a) ~(on_exit : a exit) ~pos : a * Substitution.t =
  match on_exit, t with
  | Drop, t ->
    drop_until t ~f:(function
        | C_pos pos' -> pos = pos'
        | _ -> false)
  | Lift apply, (t, x) -> lift t x ~apply ~at:(C_pos pos)


let insert_at t ~at ~hole =
  let t1, t2 = split_at t ~at in
  t1 |@ hole |@ t2


let add_texists_eq t evar kind type_ =
  let t1, t2 = split_at t ~at:(C_texists_var (evar, kind)) in
  t1 |@ of_list [ C_texists_eq (evar, kind, type_) ] |@ t2


let add_lexists_eq t lvar layout =
  let t1, t2 = split_at t ~at:(C_lexists_var lvar) in
  t1 |@ of_list [ C_lexists_eq (lvar, layout) ] |@ t2


let to_type_map =
  memoize hashable (fun t ->
      (List.fold_right t ~init:Type_var.Map.empty ~f:(fun item map ->
           match item with
           | C_type (tvar, type_) -> Map.set map ~key:tvar ~data:type_
           | _ -> map) [@landmark "to_type_map"]))


let to_module_map =
  memoize hashable (fun t ->
      (List.fold_right t ~init:Module_var.Map.empty ~f:(fun item map ->
           match item with
           | C_module (mvar, sig_) -> Map.set map ~key:mvar ~data:sig_
           | _ -> map) [@landmark "to_module_map"]))


let to_type_mapi =
  let next = ref 0 in
  memoize hashable (fun t ->
      (List.fold_right t ~init:Type_var.Map.empty ~f:(fun item map ->
           Int.incr next;
           match item with
           | C_type (tvar, type_) -> Map.set map ~key:tvar ~data:(!next, type_)
           | _ -> map) [@landmark "to_type_mapi"]))


let to_module_mapi =
  let next = ref 0 in
  memoize hashable (fun t ->
      (List.fold_right t ~init:Module_var.Map.empty ~f:(fun item map ->
           Int.incr next;
           match item with
           | C_module (mvar, t) -> Map.set map ~key:mvar ~data:(!next, t)
           | _ -> map) [@landmark "to_module_map"]))


let get_signature t ((local_module, path) : Module_var.t List.Ne.t) =
  let open Option.Let_syntax in
  List.fold path ~init:(get_module t local_module) ~f:(fun sig_ mvar ->
      let%bind sig_ = sig_ in
      Signature.get_module sig_ mvar)


type ('a, 'ret) contextual =
  'a
  -> to_type_map:('a -> Type.t Type_var.Map.t)
  -> to_module_map:('a -> Signature.t Module_var.Map.t)
  -> 'ret

let ctx_contextual f t = f t ~to_type_map ~to_module_map

let sig_contextual f sig_ =
  f
    sig_
    ~to_type_map:Signature.to_type_map
    ~to_module_map:Signature.to_module_map


(* Recursively fetches all types from the given module and its submodules

    For example, to get the list of all types declared in a module and its submodules,
    we perform a recusive search in the context maps and accumulate the types found.
    Then, in order to convert those maps into a id-sorted list, we can :
    1. Use [merge], and convert the merged map into a (sorted) kv_list. This will remove duplicate eponym types
    2. Use [to_kvi_list], append all the kvi_lists, and sort the resulting kvi_list by id, into a kv_list, this keeps duplicates
*)
let get_module_types : t -> (Type_var.t * Type.t) list =
  let sort_to_alist
      :  (Type_var.t, int * Type.t) List.Assoc.t
      -> (Type_var.t, Type.t) List.Assoc.t
    =
   fun list ->
    let sorted_list =
      List.sort list ~compare:(fun (_, (id1, _)) (_, (id2, _)) ->
          Int.compare id2 id1)
    in
    List.map ~f:(fun (tvar, (_, type_)) -> tvar, type_) sorted_list
  in
  memoize hashable (fun ctx ->
      let rec signature : Signature.t -> (Type_var.t, int * Type.t) List.Assoc.t
        =
       fun sig_ ->
        (* Types in the current signature *)
        let local_types = Map.to_alist @@ Signature.to_type_mapi sig_ in
        (* Recursively fetch types from submodules *)
        let modules = Map.to_alist @@ Signature.to_module_mapi sig_ in
        List.fold modules ~init:local_types ~f:(fun types (_, (_, sig_)) ->
            List.rev_append types @@ signature sig_)
      in
      let local_types = Map.to_alist @@ to_type_mapi ctx in
      let modules = Map.to_alist @@ to_module_mapi ctx in
      sort_to_alist
      @@ (List.fold modules ~init:local_types ~f:(fun types (_, (_, sig_)) ->
              List.rev_append types @@ signature sig_) [@landmark
                                                         "get_module_types"]))


(*
    Add the shadowed t_sum types nested in the fetched types.

    After using [get_modules_types], we have the ctxt types, i.e. all types declared current scope and submodules.
    There is no shadowed type in ctxt types (since ctxt is a map, shadowed types are removed when adding the shadower).
    However we want shadowed types when they are nested in another type :
      type a = Foo of int | Bar of string
      type a = a list
    Here, we want [Foo of int | Bar of string] to be found
    But we want to add nested t_sum types _only_ if they are shadowed, we don't want to add them in that case for example :
      type foo_variant = Foo of int | Bar of string
      type foo_record = { foo : foo_variant ; bar : foo_variant}
    Because [foo_variant] would appear three times in the list instead of one.

    NOTE : We could append nested types on top of the [module_types] list we have so far,
    but having a final list with all nested-types before toplevel types triggers some errors.

    NOTE : We can't just do a final id-sort of the list to have everything in declarartion order
    because the fetched nested types don't have id, only the ones retrieved from the ctxt do.

    So, if we have ctxt types :
      [t1; t2; t3]
    After adding the shadowed t_sums, we want the final list :
      [t1; tsum_shadowed_by_t1; t2; tsum_shadowed_by_t2; t3; tsum_shadowed_by_t3]

    NOTE : When [fold_type_expression] is used on t1, it will add tsum types nested in t1,
    but it might also add t1 (or not), we don't know.
    However, we want to make sure t1 is in the final list *exactly once*.
      - If it's not here, we'll lose a type and have incorrect "type [t1] not found" errors
      - If it's here more than once, we'll have a false "warning, [t1] inferred but could also be of type [t1]"
    To ensure [t1] appears once exactly, we tweak the fold function by passing a [is_top] boolean
    to ensure it will fold over all nested type in [t1] but not the toplevel one (i.e. [t1]),
    we then add [t1] manually to the list.
*)
let add_shadowed_nested_t_sum tsum_list (tvar, type_) =
  let add_if_shadowed_t_sum
      :  Type_var.t -> (Type_var.t * Type.t) list * bool -> Type.t
      -> (Type_var.t * Type.t) list * bool
    =
   fun shadower_tvar (acc, is_top_level) type_ ->
    let return x = x, false in
    match type_.content, type_.orig_var with
    | T_sum _, Some tvar ->
      if Type_var.equal tvar shadower_tvar && not is_top_level
      then return ((tvar, type_) :: acc)
      else return acc
    | T_sum _, None ->
      return acc
      (* TODO : What should we do with those sum types with no binder ? *)
    | _ -> return acc
  in
  let (nested_t_sums, _) : (Type_var.t * Type.t) list * bool =
    Type.fold type_ ~init:(tsum_list, true) ~f:(add_if_shadowed_t_sum tvar)
  in
  (tvar, type_) :: nested_t_sums


(*
  for any constructor [ctor] that belong to a sum-type `t` in the context [ctxt] return a 4-uple list:
  1. the declaration name for type `t`
  2. list of abstracted type variables in the constructor parameter (e.g. ['a ; 'b] for `Foo of ('a * int * 'b)`)
  3. type of the constructor parameter (e.g. `'a * int * 'b` for `Foo of ('a * int * 'b)`)
  4. type of the sum-type found in the context

  NOTE : Here, we return all the matching types found in the module and its submodules, even if we found matching types in current scope.
  Indeed, we want to check for other matching types in submodules anyway, to warn the user in case of conflict.
  For example :
    module Mod_a = struct
      type tx = A of int
    end
    type ty = A of int
    let a = A 42
  Here, for [a], we find a matching type [ty] in the current scope, but we still want to warn the user that type [Mod_a.tx] matches too.
*)
let get_sum
    : t -> Label.t -> (Type_var.t * Type_var.t list * Type.t * Type.t) list
  =
  let dedup =
    List.stable_dedup_staged ~compare:(fun (_, _, _, tsum1) (_, _, _, tsum2) ->
        Type.compare tsum1 tsum2)
    |> Staged.unstage
  in
  memoize2
    hashable
    (module Label)
    (fun ctx constr ->
      (let filter_tsum (var, type_) =
         let t_params, type_ = Type.destruct_type_abstraction type_ in
         match type_.content with
         | T_sum m ->
           (match Record.LMap.find_opt constr m.fields with
           | Some { associated_type; _ } ->
             Some (var, t_params, associated_type, type_)
           | None -> None)
         | _ -> None
       in
       (* Format.printf "Fetching module types...\n"; *)
       (* Format.print_flush (); *)
       (* Fetch all types declared in current module and its submodules *)
       let module_types = get_module_types ctx in
       (* Format.printf "Found all module types\n"; *)
       (* Format.print_flush (); *)
       (*  Also add the shadowed t_sum types nested in the fetched types.
        Since context is made of maps, all shadowed types are absent from the context.
        However we still want the shadowed nested t_sum, see [add_shadowed_nested_t_sum] *)
       let module_types =
         List.fold (List.rev module_types) ~init:[] ~f:add_shadowed_nested_t_sum
       in
       (* Format.printf "Module Types:\n";
  List.iter module_types ~f:(fun (tvar, type_) ->
    Format.printf
      "@[Type Variable: %a@.Type: %a@]\n"
      Type_var.pp
      tvar
      Ast_typed.PP.type_expression
      type_); *)
       (* For all types found, pick only the T_sum, and make 4-uple out of them  *)
       let matching_t_sum = List.filter_map ~f:filter_tsum @@ module_types in
       let matching_t_sum = dedup matching_t_sum in
       let general_type_opt =
         List.find
           ~f:(fun (_, tvs, _, _) -> not @@ List.is_empty tvs)
           matching_t_sum
       in
       match general_type_opt with
       | Some general_type -> [ general_type ]
       | None -> matching_t_sum) [@landmark "get_sum"])


(* 
let get_sum
    : t -> Label.t -> (Type_var.t * Type_var.t list * Type.t * Type.t) list
  =
  let dedup =
    List.stable_dedup_staged ~compare:(fun (_, _, _, tsum1) (_, _, _, tsum2) ->
        Type.compare tsum1 tsum2)
    |> Staged.unstage
  in
  memoize2
    hashable
    (module Label)
    (fun ctx constr ->
      (let filter_tsum (var, type_) =
         let t_params, type_ = Type.destruct_type_abstraction type_ in
         match type_.content with
         | T_sum m ->
           (match Record.LMap.find_opt constr m.fields with
           | Some { associated_type; _ } ->
             Some (var, t_params, associated_type, type_)
           | None -> None)
         | _ -> None
       in
       Format.printf "Fetching module types for %a\n" Label.pp constr;
       (* 1. Fetch all types declared in current scope and its submodules in [ctx] *)
       let types_and_modules = get_types_and_modules ctx in
       Format.printf "Module Types:\n";
       List.iter types_and_modules ~f:(function
           | `Type (tvar, type_) ->
             Format.printf
               "@[Type Variable: %a@.Type: %a@]@."
               Type_var.pp
               tvar
               Type.pp
               type_
           | `Module (mvar, sig_) ->
             Format.printf
               "@[Module Variable: %a@.Sig: %a@]@."
               Module_var.pp
               mvar
               Signature.pp
               sig_);
       Format.print_flush ();
       (* 2. Inline the module types, filtering modules that have been shadowed  *)
       let module_types =
         let rec loop types_and_modules ~def_mod_set =
           match types_and_modules with
           | [] -> []
           | `Type (tvar, type_) :: rest ->
             (tvar, type_) :: loop rest ~def_mod_set
           | `Module (mvar, _) :: rest when Set.mem def_mod_set mvar ->
             loop rest ~def_mod_set
           | `Module (mvar, sig_) :: rest ->
             List.rev_append
               (Signature.get_module_types sig_)
               (loop rest ~def_mod_set:(Set.add def_mod_set mvar))
         in
         loop types_and_modules ~def_mod_set:Module_var.Set.empty
       in
       Format.printf "Module Types 2:\n";
       List.iter module_types ~f:(fun (tvar, type_) ->
           Format.printf
             "@[Type Variable: %a@.Type: %a@]\n"
             Type_var.pp
             tvar
             Type.pp
             type_);
       Format.print_flush ();
       (* 3. Remove types that are shadowed *and* aren't referenced by the shaddowing type
            For example, we should remove:
            ```
              type foobar = Foo of int | Bar of int
              type foobar = int
            ```   
            but keep:
            ```
              type foobar = Foo of int | Bar of int
              type foobar = { boxed : foobar }
            ```
       *)
       let module_types =
         let rec loop types ~def_set ~ref_set =
           match types with
           | [] -> []
           | (tvar, _type_) :: types
             when Set.mem def_set tvar && not (Set.mem ref_set tvar) ->
             (* This is the case where [tvar] has been shadowed but not referenced *)
             Format.printf "Type %a is shaddowed!\n" Type_var.pp tvar;
             loop types ~def_set ~ref_set
           | (tvar, type_) :: types ->
             Format.printf
               "@[Adding %a = %a to types@.Def Set: %a@.Ref Set:%a@.@]\n"
               Type_var.pp
               tvar
               Type.pp
               type_
               Sexp.pp_hum
               (Type_var.Set.sexp_of_t def_set)
               Sexp.pp_hum
               (Type_var.Set.sexp_of_t ref_set);
             (tvar, type_)
             :: loop
                  types
                  ~def_set:(Set.add def_set tvar)
                  ~ref_set:
                    Type_var.Set.(
                      union_list
                        [ remove ref_set tvar; remove (Type.orig_vars type_) tvar; Type.free_vars type_ ])
         in
         loop
           module_types
           ~def_set:Type_var.Set.empty
           ~ref_set:Type_var.Set.empty
       in
       Format.printf "Module Types 3:\n";
       List.iter module_types ~f:(fun (tvar, type_) ->
           Format.printf
             "@[Type Variable: %a@.Type: %a@]\n"
             Type_var.pp
             tvar
             Type.pp
             type_);
       Format.print_flush ();
       (* 4. For all types found, pick only the T_sum, and make 4-tuple out of them  *)
       let matching_t_sum = List.filter_map ~f:filter_tsum @@ module_types in
       (* 5. Filter out duplicates (this prevents false warnings of "infered type is X 
             but could also be Y" where X and Y are transparently equal) 
       *)
       let matching_t_sum = dedup matching_t_sum in
       let general_type_opt =
         List.find
           ~f:(fun (_, tvs, _, _) -> not @@ List.is_empty tvs)
           matching_t_sum
       in
       match general_type_opt with
       | Some general_type -> [ general_type ]
       | None -> matching_t_sum) [@landmark "get_sum"]) *)

let get_record
    : t -> Type.row_element Record.t -> (Type_var.t option * Type.row) option
  =
  let record_hashable : Type.row_element Record.t hashable =
    (module struct
      type t = Type.t Rows.row_element_mini_c Record.t [@@deriving equal, hash]
    end)
  in
  memoize2 hashable record_hashable (fun ctx record_type ->
      (let record_type_kv : (Label.t * _ Rows.row_element_mini_c) list =
         Record.LMap.to_kv_list_rev record_type
       in
       (* [is_record_type type_] returns true if [type_] corresponds to [record_type] *)
       let is_record_type type_ =
         match type_.content with
         | T_record record_type' ->
           let record_type_kv' : (Label.t * _ Rows.row_element_mini_c) list =
             Record.LMap.to_kv_list_rev record_type'.fields
           in
           (match
              List.for_all2
                record_type_kv
                record_type_kv'
                ~f:[%equal: Label.t * Type.t Rows.row_element_mini_c]
            with
           | Ok result -> Option.some_if result (type_.orig_var, record_type')
           | Unequal_lengths -> None)
         | _ -> None
       in
       (* [find t ~to_type_map ~to_module_map] finds a record type matching [record_type] *)
       let rec find : type a. (a, (Type_var.t option * t_sum) option) contextual
         =
        fun t ~to_type_map ~to_module_map ->
         match
           to_type_map t
           |> Map.to_alist
           |> List.find_map ~f:(fun (_, type_) -> is_record_type type_)
         with
         | Some _ as result -> result
         | None ->
           let modules = to_module_map t in
           List.fold_left
             ~f:(fun res (_, sig_) ->
               match res with
               | Some _ as s -> s
               | None -> sig_contextual find @@ sig_)
             ~init:None
             (Map.to_alist modules)
       in
       ctx_contextual find @@ ctx) [@landmark "get_record"])


module Well_formed : sig
  val context : t -> bool
  val type_ : ctx:t -> Type.t -> Kind.t option
end = struct
  let rec context ctx =
    let rec loop t =
      match t with
      | [] -> true
      | item :: t ->
        loop t
        &&
        (match item with
        | C_value (var, _, type') ->
          (match type_ type' ~ctx with
          | Some Type -> true
          | _ ->
            Format.printf
              "Value %a has non-type type %a"
              Value_var.pp
              var
              Type.pp
              type';
            false)
        | C_type (tvar, type') ->
          (match type_ type' ~ctx with
          | Some _ -> true
          | None ->
            Format.printf
              "Type %a = %a is ill-kinded"
              Type_var.pp
              tvar
              Type.pp
              type';
            false)
        | C_type_var _ ->
          (* Shadowing permitted *)
          true
        | C_texists_var (evar, _) ->
          if Set.mem (get_texists_vars t) evar
          then (
            Format.printf
              "Existential variable ^%a is shadowed"
              Type_var.pp
              evar;
            false)
          else true
        | C_texists_eq (evar, kind, type') ->
          (not (Set.mem (get_texists_vars t) evar))
          &&
          (match type_ type' ~ctx with
          | Some kind' -> Kind.compare kind kind' = 0
          | _ ->
            Format.printf
              "Existential variable ^%a is ill-kinded. Expected: %a"
              Type_var.pp
              evar
              Kind.pp
              kind;
            false)
        | C_pos _ | C_mut_lock _ -> true
        | C_lexists_var lvar ->
          if Set.mem (get_lexists_vars t) lvar
          then (
            Format.printf
              "Existential layout variable ^%a is shadowed"
              Layout_var.pp
              lvar;
            false)
          else true
        | C_lexists_eq (lvar, layout_) ->
          (not (Set.mem (get_lexists_vars t) lvar)) && layout layout_ ~ctx
        | C_module (_mvar, sig_) ->
          (* Shadowing permitted *)
          signature ~ctx sig_)
    in
    loop ctx


  and layout ~ctx:_ (layout : Type.layout) : bool =
    match layout with
    | L_tree | L_comb -> true
    | L_exists _lvar -> true


  and type_ ~ctx t : Kind.t option =
    let open Option.Let_syntax in
    let open Kind in
    let rec loop (t : Type.t) ~ctx =
      let loop ?(ctx = ctx) t = loop t ~ctx in
      match t.content with
      | T_variable tvar -> get_type_var ctx tvar
      | T_exists tvar -> get_texists_var ctx tvar
      | T_construct { parameters; _ } ->
        (* Hack. No HKT parameters, so simply check if all params are
                 of kind: *. *)
        if List.for_all parameters ~f:(fun param ->
               match loop param with
               | Some (Type | Singleton) -> true
               | _ ->
                 Format.printf "Ill-kinded parameter: %a\n" Type.pp param;
                 false)
        then return Type
        else None
      | T_singleton _ -> return Singleton
      | T_arrow { type1 = arg_type; type2 = ret_type } ->
        let%bind arg_kind = loop arg_type in
        let%bind ret_kind = loop ret_type in
        (match arg_kind, ret_kind with
        | Type, Type -> Some Type
        | _ -> None)
      | T_abstraction { ty_binder = tvar; kind; type_ } ->
        let%bind kind' = loop ~ctx:(ctx |:: C_type_var (tvar, kind)) type_ in
        return @@ Arrow (kind, kind')
      | T_for_all { ty_binder = tvar; kind; type_ } ->
        (match%bind loop ~ctx:(ctx |:: C_type_var (tvar, kind)) type_ with
        | Type -> return Type
        | _ -> None)
      | T_sum rows | T_record rows ->
        if Record.LMap.for_all
             (fun _label ({ associated_type; _ } : _ Rows.row_element_mini_c) ->
               match loop associated_type with
               | Some Type -> true
               | _ -> false)
             rows.fields
        then return Type
        else None
    in
    loop t ~ctx


  and signature ~ctx sig_ =
    match sig_ with
    | [] -> true
    | item :: sig_ ->
      signature_item ~ctx item
      && signature ~ctx:(add_signature_item ctx item) sig_


  and signature_item ~ctx (sig_item : Signature.item) =
    match sig_item with
    | S_value (_var, type') ->
      (match type_ ~ctx type' with
      | Some Type -> true
      | _ -> false)
    | S_type (_tvar, type') ->
      (match type_ ~ctx type' with
      | Some _ -> true
      | _ -> false)
    | S_module (_mvar, sig_) -> signature ~ctx sig_
end

module Hashes = struct
  let table : (Type.t, Module_var.t list * Type_var.t) Hashtbl.t =
    Hashtbl.create ~size:256 (module Type)


  let context = ref (false, empty)
  let set_context (t : t) : unit = context := false, t

  let hash_types () : unit =
    let hashed, t = !context in
    if hashed
    then ()
    else (
      let rec hash_types
          : type a. (a, path:Module_var.t list -> unit) contextual
        =
       fun t ~to_type_map ~to_module_map ~path ->
        let types = Map.to_alist @@ to_type_map t in
        let modules = Map.to_alist @@ to_module_map t in
        List.iter (List.rev types) ~f:(fun (v, t) ->
            Hashtbl.set table ~key:t ~data:(path, v));
        List.iter (List.rev modules) ~f:(fun (v, t) ->
            sig_contextual hash_types t ~path:(path @ [ v ]))
      in
      Hashtbl.clear table;
      ctx_contextual hash_types t ~path:[];
      context := true, t)


  let find_type (t : Type.t) : (Module_var.t list * Type_var.t) option =
    Hashtbl.find table t
end

let unsolved t =
  List.fold_left
    t
    ~init:(Substitution.empty, [], [])
    ~f:(fun (subst, tvars, lvars) item ->
      match item with
      | C_lexists_eq (lvar, layout) ->
        Substitution.add_lexists_eq subst lvar layout, tvars, lvars
      | C_texists_eq (tvar, kind, type_) ->
        Substitution.add_texists_eq subst tvar kind type_, tvars, lvars
      | C_texists_var (tvar, kind) -> subst, (tvar, kind) :: tvars, lvars
      | C_lexists_var lvar -> subst, tvars, lvar :: lvars
      | _ -> subst, tvars, lvars)


let generalize_type ~loc ~tvars ~subst type_ =
  (* Fully apply the type w/ the substitution *)
  let type_ = Substitution.Apply.type_ subst type_ in
  (* Generalize over [tvars] *)
  List.fold_right tvars ~init:type_ ~f:(fun (_tvar, (tvar', kind)) type_ ->
      Type.t_for_all ~loc { ty_binder = tvar'; kind; type_ } ())


let generalize t type_ ~pos ~loc =
  let ctxl, ctxr = split_at t ~at:(C_pos pos) in
  (* Determine subst and generalizable vars of [ctxr] *)
  let subst, tvars, lvars = unsolved ctxr in
  (* Add equations from existential [tvars] (and [lvars])
     to universal [tvars'] (and [default_layout]) *)
  let tvars =
    (* [exn] is safe since [tvar] in tvars shouldn't be duplicated *)
    List.map tvars ~f:(fun (tvar, kind) ->
        tvar, (Type_var.fresh_like ~loc tvar, kind))
  in
  (* Add layout substs *)
  let subst =
    List.fold_left lvars ~init:subst ~f:(fun subst lvar ->
        Substitution.add_lexists_eq subst lvar Type.default_layout)
  in
  (* Add universal tvars *)
  let subst =
    List.fold_left tvars ~init:subst ~f:(fun subst (tvar, (tvar', kind)) ->
        Substitution.add_texists_eq
          subst
          tvar
          kind
          (Type.t_variable ~loc tvar' ()))
  in
  let type_ = generalize_type ~loc ~tvars ~subst type_ in
  ctxl, type_, List.map tvars ~f:snd, subst

(* 
module Codec = struct
  module I = Type
  module O = Ast_typed

  let rec encode : O.type_expression -> I.t =
   fun type_ ->
    let return content : I.t =
      { content
      ; meta = type_.type_meta
      ; orig_var = type_.orig_var
      ; location = type_.location
      }
    in
    match type_.type_content with
    | O.T_variable tvar -> return @@ I.T_variable tvar
    | O.T_singleton lit -> return @@ I.T_singleton lit
    | O.T_constant { language; injection; parameters } ->
      let parameters = List.map ~f:encode parameters in
      return @@ I.T_construct { language; constructor = injection; parameters }
    | O.T_arrow arr ->
      let arr = Arrow.map encode arr in
      return @@ I.T_arrow arr
    | O.T_abstraction abs ->
      let abs = Abstraction.map encode abs in
      return @@ I.T_abstraction abs
    | O.T_for_all abs ->
      let abs = Abstraction.map encode abs in
      return @@ I.T_for_all abs
    | O.T_sum row ->
      let row = encode_row row in
      return @@ I.T_sum row
    | O.T_record row ->
      let row = encode_row row in
      return @@ I.T_record row


  and encode_row ({ fields; layout } : O.rows) : I.row =
    let fields = Record.map ~f:encode_row_elem fields in
    let layout = encode_layout layout in
    { fields; layout }


  and encode_row_elem (row_elem : O.row_element) : I.row_element =
    Rows.map_row_element_mini_c encode row_elem


  and encode_layout (layout : Layout.t) : I.layout =
    match layout with
    | L_comb -> L_comb
    | L_tree -> L_tree


  let rec decode : raise:(_, _) raise -> ctx:t -> I.t -> O.type_expression =
   fun ~raise ~ctx type_ ->
    let decode = decode ~raise ~ctx in
    let decode_row = decode_row ~raise ~ctx in
    let return type_content : O.type_expression =
      { type_content
      ; type_meta = type_.meta
      ; orig_var = type_.orig_var
      ; location = type_.location
      }
    in
    let fail () =
      raise.error (`Typer_existential_found (type_.location, type_))
    in
    match type_.content with
    | I.T_variable tvar -> return @@ O.T_variable tvar
    | I.T_exists tvar ->
      (match Map.find ctx.solved tvar with
      | Some (_, type_) -> decode type_
      | None -> fail ())
    | I.T_singleton lit -> return @@ O.T_singleton lit
    | I.T_construct { language; constructor; parameters } ->
      let parameters = List.map ~f:decode parameters in
      return @@ O.T_constant { language; injection = constructor; parameters }
    | I.T_arrow arr ->
      let arr = Arrow.map decode arr in
      return @@ O.T_arrow arr
    | I.T_abstraction abs ->
      let abs = Abstraction.map decode abs in
      return @@ O.T_abstraction abs
    | I.T_for_all for_all ->
      let for_all = Abstraction.map decode for_all in
      return @@ O.T_for_all for_all
    | I.T_record row ->
      let row = decode_row row in
      return @@ O.T_record row
    | I.T_sum row ->
      let row = decode_row row in
      return @@ O.T_sum row


  and decode_row ~raise ~ctx ({ fields; layout } : I.row) : O.rows =
    let fields = Record.map ~f:(decode_row_elem ~raise ~ctx) fields in
    let layout = decode_layout ~ctx layout in
    O.{ fields; layout }


  and decode_row_elem ~raise ~ctx (row_elem : I.row_element) : O.row_element =
    Rows.map_row_element_mini_c (decode ~raise ~ctx) row_elem


  and decode_layout ~ctx (layout : I.layout) : Layout.t =
    match layout with
    | L_tree -> L_tree
    | L_comb -> L_comb
    | L_exists lvar ->
      (match get_layout_exists_eq ctx lvar with
      | Some layout -> decode_layout ~ctx layout
      | None -> O.default_layout)
end

let rec signature_of_module_expr : ctx:t -> Ast_typed.module_expr -> Signature.t
  =
 fun ~ctx mod_expr ->
  match mod_expr.wrap_content with
  | M_struct decls -> signature_of_module ~ctx decls
  | M_variable mvar ->
    (match get_module ctx mvar with
    | Some sig_ -> sig_
    | None -> failwith "Unbounded module")
  | M_module_path path ->
    (match get_signature ctx path with
    | Some sig_ -> sig_
    | None ->
      Format.kasprintf
        failwith
        "Unbounded signature path: %a"
        Module_expr.pp_module_path
        path)


and signature_of_module : ctx:t -> Ast_typed.module_ -> Signature.t =
 fun ~ctx module_ ->
  match module_ with
  | [] -> []
  | decl :: module_ ->
    let public, sig_item = signature_item_of_decl ~ctx decl in
    let sig_ =
      signature_of_module ~ctx:(add_signature_item ctx sig_item) module_
    in
    if public then sig_item :: sig_ else sig_


and signature_item_of_decl : ctx:t -> Ast_typed.decl -> bool * Signature.item =
 fun ~ctx decl ->
  match Location.unwrap decl with
  | D_value { binder; expr; attr = { public; _ } } ->
    public, S_value (Binder.get_var binder, Codec.encode expr.type_expression)
  | D_type { type_binder = tvar; type_expr = type_; type_attr = { public; _ } }
    -> public, S_type (tvar, Codec.encode type_)
  | D_module { module_binder = mvar; module_; module_attr = { public; _ } } ->
    let sig_' = signature_of_module_expr ~ctx module_ in
    public, S_module (mvar, sig_')


(* Load context from the outside declarations *)
let init ?env () =
  match env with
  | None -> empty
  | Some env ->
    Environment.fold env ~init:empty ~f:(fun ctx decl ->
        (* Format.printf "%d: %a\n" i (Ast_typed.PP.declaration ~use_hidden:false) decl; *)
        match Location.unwrap decl with
        | D_value { binder; expr; attr = _ } ->
          add_imm
            ctx
            (Binder.get_var binder)
            (Codec.encode expr.type_expression)
        | D_type { type_binder; type_expr; type_attr = _ } ->
          add_type ctx type_binder (Codec.encode type_expr)
        | D_module { module_binder; module_; module_attr = _ } ->
          let sig_ = signature_of_module_expr ~ctx module_ in
          add_module ctx module_binder sig_) *)

(* 
module Elaboration = struct
  type ('a, 'err, 'wrn) t = raise:('err, 'wrn) raise -> 'a

  include Monad.Make3 (struct
    type nonrec ('a, 'err, 'wrn) t = ('a, 'err, 'wrn) t

    let return x ~raise:_ = x

    let bind t ~f ~raise =
      let x = t ~raise in
      f x ~raise


    let map = `Define_using_bind
  end)

  type error = [ `Typer_existential_found of Location.t * type_expression ]

  let raise ~raise = raise

  (* "Zonking" is performed by these context application functions *)

  let rec t_apply ctx (type_ : type_expression) : type_expression =
    let self = t_apply ctx in
    let return content = { type_ with type_content = content } in
    match type_.type_content with
    | T_variable tvar ->
      (match Exists_var.of_type_var tvar with
      | Some evar ->
        (match Exists_var.Map.find_opt evar ctx.solved with
        | Some (_, type_') -> self type_'
        | None ->
          (match get_exists_eq ctx evar with
          | Some type_' -> self type_'
          | None -> type_))
      | None -> type_)
    | T_constant inj ->
      let parameters = List.map ~f:self inj.parameters in
      return @@ T_constant { inj with parameters }
    | T_sum rows ->
      let fields =
        Record.LMap.map
          (fun (row_elem : _ Rows.row_element_mini_c) ->
            let associated_type = self row_elem.associated_type in
            { row_elem with associated_type })
          rows.fields
      in
      return @@ T_sum { rows with fields }
    | T_record rows ->
      let fields =
        Record.LMap.map
          (fun (row_elem : _ Rows.row_element_mini_c) ->
            let associated_type = self row_elem.associated_type in
            { row_elem with associated_type })
          rows.fields
      in
      return @@ T_record { rows with fields }
    | T_arrow { type1; type2 } ->
      let type1 = self type1 in
      let type2 = self type2 in
      return @@ T_arrow { type1; type2 }
    | T_singleton _ -> type_
    | T_abstraction abs ->
      let type_ = self abs.type_ in
      return @@ T_abstraction { abs with type_ }
    | T_for_all for_all ->
      let type_ = self for_all.type_ in
      return @@ T_for_all { for_all with type_ }


  let rec e_apply ctx expr =
    let self = e_apply ctx in
    let return expression_content =
      let type_expression = t_apply ctx expr.type_expression in
      { expr with expression_content; type_expression }
    in
    return
    @@
    match expr.expression_content with
    | E_literal lit -> E_literal lit
    | E_constant { cons_name; arguments } ->
      E_constant { cons_name; arguments = List.map ~f:self arguments }
    | E_variable var -> E_variable var
    | E_application { lamb; args } ->
      E_application { lamb = self lamb; args = self args }
    | E_lambda lambda -> E_lambda (lambda_apply ctx lambda)
    | E_recursive { fun_name; fun_type; lambda } ->
      E_recursive
        { fun_name
        ; fun_type = t_apply ctx fun_type
        ; lambda = lambda_apply ctx lambda
        }
    | E_let_in { let_binder; rhs; let_result; attr } ->
      E_let_in
        { let_binder = binder_apply ctx let_binder
        ; rhs = self rhs
        ; let_result = self let_result
        ; attr
        }
    | E_mod_in { module_binder; rhs; let_result } ->
      E_mod_in
        { module_binder
        ; rhs = module_expr_apply ctx rhs
        ; let_result = self let_result
        }
    | E_raw_code { language; code } -> E_raw_code { language; code = self code }
    | E_type_inst { forall; type_ } ->
      E_type_inst { forall = self forall; type_ = t_apply ctx type_ }
    | E_type_abstraction type_abs ->
      E_type_abstraction { type_abs with result = self type_abs.result }
    | E_constructor { constructor; element } ->
      E_constructor { constructor; element = self element }
    | E_matching { matchee; cases } ->
      E_matching
        { matchee = self matchee; cases = matching_expr_apply ctx cases }
    | E_record expr_label_map -> E_record (Record.map ~f:self expr_label_map)
    | E_accessor { struct_; path } ->
      E_accessor { struct_ = self struct_; path }
    | E_update { struct_; path; update } ->
      E_update { struct_ = self struct_; path; update = self update }
    | E_module_accessor mod_access -> E_module_accessor mod_access
    | E_let_mut_in { let_binder; rhs; let_result; attr } ->
      E_let_mut_in
        { let_binder = binder_apply ctx let_binder
        ; rhs = self rhs
        ; let_result = self let_result
        ; attr
        }
    | E_deref var -> E_deref var
    | E_while while_loop -> E_while (While_loop.map self while_loop)
    | E_for for_loop -> E_for (For_loop.map self for_loop)
    | E_for_each for_each_loop ->
      E_for_each (For_each_loop.map self for_each_loop)
    | E_assign { binder; expression } ->
      E_assign
        { binder = binder_apply ctx binder; expression = self expression }


  and lambda_apply ctx ({ binder; result; output_type } : _ Lambda.t) =
    { binder = param_apply ctx binder
    ; result = e_apply ctx result
    ; output_type = t_apply ctx output_type
    }


  and param_apply ctx (param : 'a Param.t) = Param.map (t_apply ctx) param
  and binder_apply ctx (binder : 'a Binder.t) = Binder.map (t_apply ctx) binder

  and binder_apply_opt ctx (binder : 'a option Binder.t) =
    Binder.map (Option.map ~f:(t_apply ctx)) binder


  and matching_expr_apply ctx match_exprs =
    List.map
      match_exprs
      ~f:(Types.Match_expr.map_match_case (e_apply ctx) (t_apply ctx))


  and decl_apply ctx (decl : decl) = declaration_apply ctx decl

  and declaration_apply ctx decl : declaration =
    let loc = decl.location in
    let return content : declaration = Location.wrap ~loc content in
    match decl.wrap_content with
    | D_type decl_type -> return @@ D_type decl_type
    | D_value { binder; expr; attr } ->
      return
      @@ D_value
           { binder = binder_apply_opt ctx binder
           ; expr = e_apply ctx expr
           ; attr
           }
    | D_module { module_binder; module_; module_attr } ->
      return
      @@ D_module
           { module_binder
           ; module_ = module_expr_apply ctx module_
           ; module_attr
           }


  and module_apply ctx module_ : module_ = List.map ~f:(decl_apply ctx) module_

  and program_apply ctx program : program =
    List.map ~f:(declaration_apply ctx) program


  and module_expr_apply ctx (module_expr : module_expr) =
    let loc = module_expr.location in
    let return content : module_expr = Location.wrap ~loc content in
    match Location.unwrap module_expr with
    | M_struct module_ -> return @@ M_struct (module_apply ctx module_)
    | M_variable mvar -> return @@ M_variable mvar
    | M_module_path path -> return @@ M_module_path path


  let all_lmap lmap ~raise = Record.LMap.map (fun t -> t ~raise) lmap
  let all_list l ~raise = List.map ~f:(fun t -> t ~raise) l

  (* A pass to check all existentials are resolved *)
  let type_pass ~raise (type_ : type_expression) : unit =
    let fail () =
      raise.error (`Typer_existential_found (type_.location, type_))
    in
    let rec loop type_ =
      match type_.type_content with
      | T_variable tvar -> if Type_var.is_exists tvar then fail ()
      | T_constant inj -> List.iter ~f:loop inj.parameters
      | T_record rows | T_sum rows ->
        Record.LMap.iter
          (fun _ (row_elem : _ Rows.row_element_mini_c) ->
            loop row_elem.associated_type)
          rows.fields
      | T_arrow { type1; type2 } ->
        loop type1;
        loop type2
      | T_singleton _ -> ()
      | T_abstraction abs -> loop abs.type_
      | T_for_all for_all -> loop for_all.type_
    in
    loop type_


  let rec expression_pass ~raise expr =
    type_pass ~raise expr.type_expression;
    let self = expression_pass ~raise in
    match expr.expression_content with
    | E_literal _lit -> ()
    | E_constant { arguments; _ } -> List.iter ~f:self arguments
    | E_variable _var -> ()
    | E_application { lamb; args } ->
      self lamb;
      self args
    | E_lambda lambda -> lambda_pass ~raise lambda
    | E_recursive { fun_type; lambda; _ } ->
      type_pass ~raise fun_type;
      lambda_pass ~raise lambda
    | E_let_in { let_binder; rhs; let_result; _ } ->
      binder_pass ~raise let_binder;
      self rhs;
      self let_result
    | E_mod_in { rhs; let_result; _ } ->
      module_expr_pass ~raise rhs;
      self let_result
    | E_raw_code { code; _ } -> self code
    | E_type_inst { forall; type_ } ->
      self forall;
      type_pass ~raise type_
    | E_type_abstraction { result; _ } -> self result
    | E_constructor { element; _ } -> self element
    | E_matching { matchee; cases } ->
      self matchee;
      matching_expr_pass ~raise cases
    | E_record expr_label_map ->
      Record.LMap.iter (fun _ expr -> self expr) expr_label_map
    | E_accessor { struct_; _ } -> self struct_
    | E_update { struct_; update; _ } ->
      self struct_;
      self update
    | E_module_accessor _mod_access -> ()
    | E_let_mut_in { let_binder; rhs; let_result; _ } ->
      binder_pass ~raise let_binder;
      self rhs;
      self let_result
    | E_deref _var -> ()
    | E_assign { binder; expression } ->
      binder_pass ~raise binder;
      self expression
    | E_for { start; final; incr; f_body; _ } ->
      self start;
      self final;
      self incr;
      self f_body
    | E_for_each { collection; fe_body; _ } ->
      self collection;
      self fe_body
    | E_while { cond; body } ->
      self cond;
      self body


  and lambda_pass ~raise { binder; result; output_type } =
    param_pass ~raise binder;
    expression_pass ~raise result;
    type_pass ~raise output_type


  and param_pass ~raise (param : _ Param.t) =
    type_pass ~raise @@ Param.get_ascr param


  and binder_pass ~raise (binder : _ Binder.t) =
    type_pass ~raise @@ Binder.get_ascr binder


  and binder_pass_opt ~raise (binder : _ option Binder.t) =
    Option.iter (Binder.get_ascr binder) ~f:(type_pass ~raise)


  and matching_expr_pass ~raise match_exprs =
    List.iter
      match_exprs
      ~f:
        (Types.Match_expr.iter_match_case
           (expression_pass ~raise)
           (type_pass ~raise))


  and decl_pass ~raise (decl : decl) = declaration_pass ~raise decl

  and declaration_pass ~raise (decl : declaration) =
    match decl.wrap_content with
    | D_type decl_type -> type_pass ~raise decl_type.type_expr
    | D_value { binder; expr; _ } ->
      binder_pass_opt ~raise binder;
      expression_pass ~raise expr
    | D_module { module_; _ } -> module_expr_pass ~raise module_


  and module_pass ~raise module_ = List.iter ~f:(decl_pass ~raise) module_

  and program_pass ~raise program =
    List.iter ~f:(declaration_pass ~raise) program


  and module_expr_pass ~raise module_expr =
    match module_expr.wrap_content with
    | M_struct module_ -> module_pass ~raise module_
    | M_variable _mvar -> ()
    | M_module_path _path -> ()


  let run_expr t ~ctx ~raise =
    let expr = e_apply ctx (t ~raise) in
    expression_pass ~raise expr;
    expr


  let run_decl t ~ctx ~raise =
    let decl = decl_apply ctx (t ~raise) in
    decl_pass ~raise decl;
    decl


  let run_declaration t ~ctx ~raise =
    let decl = declaration_apply ctx (t ~raise) in
    declaration_pass ~raise decl;
    decl


  let run_module t ~ctx ~raise =
    let module_ = module_apply ctx (t ~raise) in
    module_pass ~raise module_;
    module_


  let run_program t ~ctx ~raise =
    let program = program_apply ctx (t ~raise) in
    program_pass ~raise program;
    program
end

let unsolved { items; solved } =
  let solved =
    List.fold items ~init:solved ~f:(fun solved item ->
        match item with
        | C_exists_eq (evar, kind, type_) ->
          Exists_var.Map.add evar (kind, type_) solved
        | _ -> solved)
  in
  { items =
      List.filter items ~f:(function
          | C_exists_var _ -> true
          | _ -> false)
  ; solved
  }


let enter ~ctx ~mut ~in_ =
  let ctx, pos = mark ctx ~mut in
  let ctx, ret_type, expr = in_ ctx in
  let c_pos = if mut then C_mut_pos pos else C_pos pos in
  let ctxl, ctxr = split_at ctx ~at:c_pos in
  let ret_type = apply ctxr ret_type in
  let ctxr = unsolved ctxr in
  ctxl |@ ctxr, ret_type, expr


let decl_enter ~ctx ~in_ =
  let ctx, pos = mark ctx ~mut:false in
  let ctx, sig_, ret = in_ ctx in
  let ctxl, ctxr = split_at ctx ~at:(C_pos pos) in
  let ret_sig = signature_apply ctxr sig_ in
  let ctxr = unsolved ctxr in
  ctxl |@ ctxr, ret_sig, ret


let t_subst t ~tvar ~type_ = Helpers.subst_no_capture_type tvar type_ t
(*
let t_exists (evar : Exists_var.t) =
  t_variable ~loc:(Exists_var.loc evar) (evar :> type_variable) () *)

(* let t_subst_var t ~tvar ~tvar' = t_subst t ~tvar ~type_:(t_variable tvar' ()) *)
let t_subst_evar t ~evar ~type_ = t_subst t ~tvar:evar ~type_

module Generalization = struct
  let generalize_type ~tvars type_ =
    (* Substitute existentials for rigid variables *)
    let ret_type =
      Exists_var.Map.fold
        (fun evar (_kind, tvar) ret_type ->
          t_subst_evar ret_type ~evar ~type_:(t_variable tvar ()))
        tvars
        type_
    in
    (* Quantify rigid variables *)
    Exists_var.Map.fold
      (fun _evar (kind, tvar) ret_type ->
        t_for_all (tvar : type_variable) kind ret_type)
      tvars
      ret_type


  let unsolved { items; solved } =
    let solved =
      List.fold items ~init:solved ~f:(fun solved item ->
          match item with
          | C_exists_eq (evar, kind, type_) ->
            Exists_var.Map.add evar (kind, type_) solved
          | _ -> solved)
    in
    let tvars =
      List.fold items ~init:Exists_var.Map.empty ~f:(fun tvars item ->
          match item with
          | C_exists_var (evar, kind) -> Exists_var.Map.add evar kind tvars
          | _ -> tvars)
    in
    { empty with solved }, tvars


  let enter ~ctx ~mut ~in_ =
    let open Elaboration.Let_syntax in
    let ctx, pos = mark ctx ~mut in
    let ctx, ret_type, expr = in_ ctx in
    let c_pos = if mut then C_mut_pos pos else C_pos pos in
    let ctxl, ctxr = split_at ctx ~at:c_pos in
    let ret_type = apply ctxr ret_type in
    let ctxr, tvars = unsolved ctxr in
    let tvars =
      Exists_var.Map.map (fun kind -> kind, Type_var.fresh ()) tvars
    in
    (* Add equation for later elaboration for existentials *)
    let ctxr =
      { ctxr with
        solved =
          Exists_var.Map.fold
            (fun evar (kind, tvar) solved ->
              Exists_var.Map.add evar (kind, t_variable tvar ()) solved)
            tvars
            ctxr.solved
      }
    in
    ( ctxl |@ ctxr
    , generalize_type ~tvars ret_type
    , let%bind expr = expr in
      return
      @@ Exists_var.Map.fold
           (fun _evar (kind, tvar) expr ->
             e_type_abstraction
               { type_binder = tvar; result = expr }
               (t_for_all tvar kind expr.type_expression))
           tvars
           expr )
end *)
