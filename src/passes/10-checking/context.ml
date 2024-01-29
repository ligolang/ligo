(* This file represente the context which give the association of values to types *)
module Location = Simple_utils.Location
module List = Simple_utils.List
open Ligo_prim
open Type

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

module Attrs = struct
  module Value = struct
    type t =
      { entry : bool
      ; dyn_entry : bool
      ; view : bool
      ; public : bool
      ; optional : bool
      ; leading_comments : string list
      }
    [@@deriving compare, hash, equal]

    let default =
      { entry = false
      ; dyn_entry = false
      ; view = false
      ; public = true
      ; optional = false
      ; leading_comments = []
      }


    let of_core_attr
        ({ entry; dyn_entry; view; public; leading_comments; _ } : Ast_typed.ValueAttr.t)
      =
      { entry; dyn_entry; view; public; leading_comments; optional = false }
  end

  module Type = struct
    type t =
      { public : bool
      ; leading_comments : string list
      }
    [@@deriving compare, hash, equal]

    let default = { public = true; leading_comments = [] }

    let of_core_attr ({ public; leading_comments; _ } : Ast_typed.TypeOrModuleAttr.t) =
      { public; leading_comments }
  end

  module Module = Type

  module Signature = struct
    type t =
      { public : bool
      ; leading_comments : string list
      }
    [@@deriving compare, hash, equal]

    let default = { public = true; leading_comments = [] }

    let of_core_attr ({ public; leading_comments } : Ast_typed.SignatureAttr.t) =
      { public; leading_comments }
  end
end

module Signature = struct
  module T = struct
    type t =
      { items : item Location.wrap list
      ; sort : sort
      }

    and item =
      | S_value of Value_var.t * Type.t * Attrs.Value.t
      | S_type of Type_var.t * Type.t * Attrs.Type.t
      | S_type_var of Type_var.t * Attrs.Type.t
      | S_module of Module_var.t * t * Attrs.Module.t
      | S_module_type of Module_var.t * t * Attrs.Signature.t

    and sort =
      | Ss_module
      | Ss_contract of
          { storage : Type.t
          ; parameter : Type.t
          }
    [@@deriving equal, compare, hash]
  end

  include T

  (* Extracts all `type t` (S_type_var t) ocurrences, and leaves only
     values and type declarations. Other items in the signature are
     not supported (by the parsers currently) *)
  let as_casteable (sig_ : T.t)
      : (Type_var.t list
        * [> `S_type of Type_var.t * Type.t * Attrs.Type.t
          | `S_value of Value_var.t * Type.t * Attrs.Value.t
          ]
          list)
      option
    =
    let f (tvars, items) (item : item Location.wrap) =
      match Location.unwrap item with
      | S_value (v, t, a) -> Result.Ok (tvars, `S_value (v, t, a) :: items)
      | S_type (v, t, a) -> Result.Ok (tvars, `S_type (v, t, a) :: items)
      | S_type_var (v, _) -> Result.Ok (v :: tvars, items)
      | _ -> Result.Error `Not_casteable
    in
    Result.ok @@ List.fold_result sig_.items ~init:([], []) ~f


  let hashable : t hashable = (module Phys_hashable (T))
  let find_map t ~f = List.find_map (List.rev t) ~f

  let get_contract_sort s =
    match s with
    | Ss_contract { parameter; storage } -> Some (parameter, storage)
    | Ss_module -> None


  let get_value =
    memoize2
      hashable
      (module Value_var)
      (fun t var ->
        (find_map t.items ~f:(function
            | { wrap_content = S_value (var', type_, attr); location = _ }
              when Value_var.equal var var' -> Some (type_, attr)
            | _ -> None) [@landmark "get_value"]))


  let get_type =
    memoize2
      hashable
      (module Type_var)
      (fun t tvar ->
        (find_map t.items ~f:(function
            | { wrap_content = S_type (tvar', type_, _); location = _ }
              when Type_var.equal tvar tvar' -> Some type_
            | _ -> None) [@landmark "get_type"]))


  let get_module =
    memoize2
      hashable
      (module Module_var)
      (fun t mvar ->
        (find_map t.items ~f:(function
            | { wrap_content = S_module (mvar', sig_, _); location = _ }
              when Module_var.equal mvar mvar' -> Some sig_
            | _ -> None) [@landmark "get_module"]))


  let get_module_type =
    memoize2
      hashable
      (module Module_var)
      (fun t mvar ->
        (find_map t.items ~f:(function
            | { wrap_content = S_module_type (mvar', sig_, _); location = _ }
              when Module_var.equal mvar mvar' -> Some sig_
            | _ -> None) [@landmark "get_module"]))


  let to_type_mapi =
    let next = ref 0 in
    memoize hashable (fun t ->
        (List.fold_right t.items ~init:Type_var.Map.empty ~f:(fun item map ->
             Int.incr next;
             match item with
             | { wrap_content = S_type (tvar, type_, _); location = _ } ->
               Map.set map ~key:tvar ~data:(!next, type_)
             | _ -> map) [@landmark "to_type_mapi"]))


  let to_module_mapi =
    let next = ref 0 in
    memoize hashable (fun t ->
        (List.fold_right t.items ~init:Module_var.Map.empty ~f:(fun item map ->
             Int.incr next;
             match item with
             | { wrap_content = S_module (mvar, t, _); location = _ } ->
               Map.set map ~key:mvar ~data:(!next, t)
             | _ -> map) [@landmark "to_module_map"]))


  let to_type_map =
    memoize hashable (fun t ->
        (List.fold_right t.items ~init:Type_var.Map.empty ~f:(fun item map ->
             match item with
             | { wrap_content = S_type (tvar, type_, _); location = _ } ->
               Map.set map ~key:tvar ~data:type_
             | _ -> map) [@landmark "to_type_map"]))


  let to_module_map =
    memoize hashable (fun t ->
        (List.fold_right t.items ~init:Module_var.Map.empty ~f:(fun item map ->
             match item with
             | { wrap_content = S_module (mvar, t, _); location = _ } ->
               Map.set map ~key:mvar ~data:t
             | _ -> map) [@landmark "to_module_map"]))


  include struct
    let list ~pp ppf xs =
      let rec loop ppf = function
        | [] -> Format.fprintf ppf ""
        | x :: xs -> Format.fprintf ppf "%a@.%a" pp x loop xs
      in
      Format.fprintf ppf "@[<v>%a@]" loop xs


    let rec pp_item ppf item =
      match Location.unwrap item with
      | S_value (var, type_, _attr) ->
        Format.fprintf ppf "%a : %a" Value_var.pp var Type.pp type_
      | S_type (tvar, type_, _attr) ->
        Format.fprintf ppf "type %a = %a" Type_var.pp tvar Type.pp type_
      | S_type_var (tvar, _attr) -> Format.fprintf ppf "type %a" Type_var.pp tvar
      | S_module (mvar, sig_, _attr) ->
        Format.fprintf ppf "module %a = %a" Module_var.pp mvar pp sig_
      | S_module_type (mvar, sig_, _attr) ->
        Format.fprintf ppf "module type %a = %a" Module_var.pp mvar pp sig_


    and pp_sort ppf sort =
      match sort with
      | Ss_module -> ()
      | Ss_contract { storage; parameter } ->
        Format.fprintf
          ppf
          "@[[@@contract { storage = %a; parameter = %a }]@]"
          Type.pp
          storage
          Type.pp
          parameter


    and pp ppf t =
      Format.fprintf ppf "@[<v>sig@,%a@,end%a@]" (list ~pp:pp_item) t.items pp_sort t.sort
  end
end

type mutable_flag = Param.mutable_flag =
  | Mutable
  | Immutable
[@@deriving compare, hash]

type pos = int [@@deriving compare, hash]
type mut_lock = int [@@deriving compare, hash]

module T = struct
  type t = item list

  and item =
    | C_value of Value_var.t * mutable_flag * Type.t * Attrs.Value.t
    | C_type of Type_var.t * Type.t
    | C_type_var of Type_var.t * Kind.t
    | C_module of Module_var.t * Signature.t
    | C_module_type of Module_var.t * Signature.t
    | C_texists_var of Type_var.t * Kind.t
    | C_texists_eq of Type_var.t * Kind.t * Type.t
    | C_lexists_var of Layout_var.t * fields
    | C_lexists_eq of Layout_var.t * fields * Type.layout
    | C_pos of pos
    | C_mut_lock of mut_lock
  [@@deriving compare, hash]

  and fields = Label.Set.t
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


  let pp_fields ppf t = list ~pp:Label.pp ppf (Set.to_list t)

  let item ppf item =
    match item with
    | C_value (evar, mut_flag, type_, _attr) ->
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
      Format.fprintf ppf "^%a :: %a = %a" Type_var.pp evar Kind.pp kind Type.pp type_
    | C_lexists_var (lvar, fields) ->
      Format.fprintf ppf "layout ^%a :: %a" Layout_var.pp lvar pp_fields fields
    | C_lexists_eq (lvar, fields, layout) ->
      Format.fprintf
        ppf
        "layout ^%a :: %a = %a"
        Layout_var.pp
        lvar
        pp_fields
        fields
        Type.pp_layout
        layout
    | C_module (mvar, sig_) ->
      Format.fprintf ppf "module %a = %a" Module_var.pp mvar Signature.pp sig_
    | C_module_type (mvar, sig_) ->
      Format.fprintf ppf "module %a : %a" Module_var.pp mvar Signature.pp sig_
    | C_pos _ | C_mut_lock _ -> ()


  let context ppf t = list ppf t ~pp:item
end

let pp = PP.context
let empty = []
let add t item = item :: t
let join t1 t2 = t2 @ t1
let of_list items = List.rev items

(* Inifix notations for [add] and [join] *)
let ( |:: ) = add
let ( |@ ) = join
let add_value t var mut_flag type_ attr = t |:: C_value (var, mut_flag, type_, attr)

let add_imm t var ?(attr = Attrs.Value.default) type_ =
  t |:: C_value (var, Immutable, type_, attr)


let add_mut t var type_ = t |:: C_value (var, Mutable, type_, Attrs.Value.default)
let add_type t tvar type_ = t |:: C_type (tvar, type_)
let add_type_var t tvar kind = t |:: C_type_var (tvar, kind)
let add_texists_var t tvar kind = t |:: C_texists_var (tvar, kind)
let add_module t mvar mctx = t |:: C_module (mvar, mctx)
let add_module_type t mvar mctx = t |:: C_module_type (mvar, mctx)
let add_lexists_var t lvar fields = t |:: C_lexists_var (lvar, fields)

let item_of_signature_item (sig_item : Signature.item Location.wrap) : item =
  match Location.unwrap sig_item with
  | S_value (var, type_, attr) -> C_value (var, Immutable, type_, attr)
  | S_type (tvar, type_, _attr) -> C_type (tvar, type_)
  | S_type_var (tvar, _attr) -> C_type_var (tvar, Type)
  | S_module (mvar, sig_, _attr) -> C_module (mvar, sig_)
  | S_module_type (mvar, sig_, _attr) -> C_module_type (mvar, sig_)


let add_signature_item t (sig_item : Signature.item Location.wrap) =
  add t (item_of_signature_item sig_item)


let add_signature_items t (sig_items : Signature.item Location.wrap list) =
  List.fold ~f:add_signature_item ~init:t (List.rev sig_items)


let get_value =
  memoize2
    hashable
    (module Value_var)
    (fun t var ->
      let[@landmark "get_value"] rec loop ?(locked = false) items =
        match items with
        | C_value (var', mut_flag, type_, attr) :: _ when Value_var.equal var var' ->
          (match mut_flag, locked with
          | Mutable, true -> Error `Mut_var_captured
          | _ -> Ok (mut_flag, type_, attr))
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
          | C_value (var', Immutable, type_, attr) when Value_var.equal var var' ->
            Some (type_, attr)
          | _ -> None))


let get_mut =
  memoize2
    hashable
    (module Value_var)
    (fun t var ->
      let rec loop ?(locked = false) = function
        | C_value (var', Mutable, type_, _) :: _ when Value_var.equal var var' ->
          if locked then Error `Mut_var_captured else Ok type_
        | C_mut_lock _ :: items -> loop ~locked:true items
        | _ :: items -> loop ~locked items
        | [] -> Error `Not_found
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


let get_module_type =
  memoize2
    hashable
    (module Module_var)
    (fun t mvar ->
      (List.find_map t ~f:(function
          | C_module_type (mvar', mctx) when Module_var.equal mvar mvar' -> Some mctx
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
          | C_lexists_var (lvar, _) | C_lexists_eq (lvar, _, _) -> Some lvar
          | _ -> None) [@landmark "get_lexists_vars"])
      |> Layout_var.Set.of_list)


let get_texists_var =
  memoize2
    hashable
    (module Type_var)
    (fun t tvar ->
      (List.find_map t ~f:(function
          | C_texists_var (tvar', kind) when Type_var.equal tvar tvar' -> Some kind
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


let get_lexists_var =
  memoize2
    hashable
    (module Layout_var)
    (fun t lvar ->
      (List.find_map t ~f:(function
          | C_lexists_var (lvar', fields) when Layout_var.equal lvar lvar' -> Some fields
          | _ -> None) [@landmark "get_lexists_var"]))


let get_type_var =
  memoize2
    hashable
    (module Type_var)
    (fun t tvar ->
      (List.find_map t ~f:(function
          | C_type_var (tvar', kind) when Type_var.equal tvar tvar' -> Some kind
          | _ -> None) [@landmark "get_type_var"]))


let get_type_or_type_var =
  memoize2
    hashable
    (module Type_var)
    (fun t tvar ->
      (List.find_map t ~f:(function
          | C_type_var (tvar', kind) when Type_var.equal tvar tvar' ->
            Some (`Type_var kind)
          | C_type (tvar', type_) when Type_var.equal tvar tvar' -> Some (`Type type_)
          | _ -> None) [@landmark "get_type_or_type_var"]))


let get_lexists_eq =
  memoize2
    hashable
    (module Layout_var)
    (fun t lvar ->
      (List.find_map t ~f:(function
          | C_lexists_eq (lvar', fields, layout) when Layout_var.equal lvar lvar' ->
            Some (fields, layout)
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
    let fields = Map.map ~f:(type_ ctx) t.fields in
    let layout = layout ctx t.layout in
    Row.create ~layout fields


  and layout ctx (t : Type.layout) : Type.layout =
    match t with
    | L_concrete _ -> t
    | L_exists lvar ->
      (match get_lexists_eq ctx lvar with
      | Some (_fields, t) -> layout ctx t
      | None -> t)


  let rec sig_item ctx (sig_item : Signature.item Location.wrap)
      : Signature.item Location.wrap
    =
    Location.wrap ~loc:(Location.get_location sig_item)
    @@
    match Location.unwrap sig_item with
    | S_type (tvar, type', attr) -> Signature.S_type (tvar, type_ ctx type', attr)
    | S_value (var, type', attr) -> S_value (var, type_ ctx type', attr)
    | S_type_var (var, attr) -> S_type_var (var, attr)
    | S_module (mvar, sig', attr) -> S_module (mvar, sig_ ctx sig', attr)
    | S_module_type (mvar, sig', attr) -> S_module_type (mvar, sig', attr)


  and sig_sort ctx (sig_sort : Signature.sort) : Signature.sort =
    match sig_sort with
    | Ss_module -> Ss_module
    | Ss_contract { storage; parameter } ->
      Ss_contract { storage = type_ ctx storage; parameter = type_ ctx parameter }


  and sig_ ctx (sig_ : Signature.t) : Signature.t =
    let no_exists_eq = function
      | C_value _
      | C_type _
      | C_type_var _
      | C_module _
      | C_texists_var _
      | C_lexists_var _
      | C_pos _
      | C_mut_lock _ -> true
      | _ -> false
    in
    (* Only do something if there are texists_eq or lexists_eq *)
    if List.for_all ~f:no_exists_eq ctx
    then sig_
    else { items = List.map sig_.items ~f:(sig_item ctx); sort = sig_sort ctx sig_.sort }
end

let equal_item : item -> item -> bool =
 fun item1 item2 ->
  match item1, item2 with
  | C_value (var1, mut_flag1, type1, attr1), C_value (var2, mut_flag2, type2, attr2) ->
    Value_var.equal var1 var2
    && Param.equal_mutable_flag mut_flag1 mut_flag2
    && Type.equal type1 type2
    && Attrs.Value.equal attr1 attr2
  | C_type (tvar1, type1), C_type (tvar2, type2) ->
    Type_var.equal tvar1 tvar2 && Type.equal type1 type2
  | C_type_var (tvar1, kind1), C_type_var (tvar2, kind2) ->
    Type_var.equal tvar1 tvar2 && Kind.equal kind1 kind2
  | C_texists_var (tvar1, kind1), C_texists_var (tvar2, kind2) ->
    Type_var.equal tvar1 tvar2 && Kind.equal kind1 kind2
  | C_texists_eq (evar1, kind1, type1), C_texists_eq (evar2, kind2, type2) ->
    Type_var.equal evar1 evar2 && Kind.equal kind1 kind2 && Type.equal type1 type2
  | C_module (mvar1, sig1), C_module (mvar2, sig2) ->
    Module_var.equal mvar1 mvar2 && Signature.equal sig1 sig2
  | C_pos pos1, C_pos pos2 -> pos1 = pos2
  | C_mut_lock lock1, C_mut_lock lock2 -> lock1 = lock2
  | C_lexists_var (lvar1, fields1), C_lexists_var (lvar2, fields2) ->
    Layout_var.equal lvar1 lvar2 && Set.equal fields1 fields2
  | C_lexists_eq (lvar1, fields1, layout1), C_lexists_eq (lvar2, fields2, layout2) ->
    Layout_var.equal lvar1 lvar2
    && Set.equal fields1 fields2
    && Type.equal_layout layout1 layout2
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
        | C_lexists_eq (lvar, fields, layout) ->
          Substitution.add_lexists_eq subst lvar fields layout
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
        | C_lexists_eq (lvar, fields, layout) ->
          Substitution.add_lexists_eq subst lvar fields layout
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


let add_lexists_eq t lvar fields layout =
  let t1, t2 = split_at t ~at:(C_lexists_var (lvar, fields)) in
  t1 |@ of_list [ C_lexists_eq (lvar, fields, layout) ] |@ t2


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


let get_module_of_path t ((local_module, path) : Module_var.t List.Ne.t) =
  let open Option.Let_syntax in
  List.fold path ~init:(get_module t local_module) ~f:(fun sig_ mvar ->
      let%bind sig_ = sig_ in
      Signature.get_module sig_ mvar)


let get_module_type_of_path t module_path =
  let module_path = List.Ne.rev module_path in
  let (local_signature, path) : Module_var.t List.Ne.t = module_path in
  let path = List.rev path in
  match path with
  | [] -> get_module_type t local_signature
  | local_module :: path ->
    let open Option.Let_syntax in
    let%bind t = get_module t local_module in
    let rec aux module_path (t : Signature.t) =
      match module_path with
      | [] -> Some t
      | mvar :: module_path ->
        let%bind t = Signature.get_module t mvar in
        aux module_path t
    in
    let%bind t = aux path t in
    Signature.get_module_type t local_signature


type ('a, 'ret) contextual =
  'a
  -> to_type_map:('a -> Type.t Type_var.Map.t)
  -> to_module_map:('a -> Signature.t Module_var.Map.t)
  -> 'ret

let ctx_contextual f t = f t ~to_type_map ~to_module_map

let sig_contextual f sig_ =
  f sig_ ~to_type_map:Signature.to_type_map ~to_module_map:Signature.to_module_map


(* Recursively fetches all types from the given module and its submodules

    For example, to get the list of all types declared in a module and its submodules,
    we perform a recusive search in the context maps and accumulate the types found.
    Then, in order to convert those maps into a id-sorted list, we can :
    1. Use [merge], and convert the merged map into a (sorted) kv_list. This will remove duplicate eponym types
    2. Use [to_kvi_list], append all the kvi_lists, and sort the resulting kvi_list by id, into a kv_list, this keeps duplicates
*)
let get_module_types : t -> (Type_var.t * Type.t) list =
  let sort_to_alist
      : (Type_var.t, int * Type.t) List.Assoc.t -> (Type_var.t, Type.t) List.Assoc.t
    =
   fun list ->
    let sorted_list =
      List.sort list ~compare:(fun (_, (id1, _)) (_, (id2, _)) -> Int.compare id2 id1)
    in
    List.map ~f:(fun (tvar, (_, type_)) -> tvar, type_) sorted_list
  in
  memoize hashable (fun ctx ->
      let rec signature : Signature.t -> (Type_var.t, int * Type.t) List.Assoc.t =
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
              List.rev_append types @@ signature sig_) [@landmark "get_module_types"]))


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
    | T_sum _, Some (_, tvar) ->
      if Type_var.equal tvar shadower_tvar && not is_top_level
      then return ((tvar, type_) :: acc)
      else return acc
    | T_sum _, None ->
      return acc (* TODO : What should we do with those sum types with no binder ? *)
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
let get_sum : t -> Label.t -> (Type_var.t * Type_var.t list * Type.t * Type.t) list =
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
           (match Map.find m.fields constr with
           | Some associated_type -> Some (var, t_params, associated_type, type_)
           | None -> None)
         | _ -> None
       in
       (* Fetch all types declared in current module and its submodules *)
       let module_types = get_module_types ctx in
       (*  Also add the shadowed t_sum types nested in the fetched types.
        Since context is made of maps, all shadowed types are absent from the context.
        However we still want the shadowed nested t_sum, see [add_shadowed_nested_t_sum] *)
       let module_types =
         List.fold (List.rev module_types) ~init:[] ~f:add_shadowed_nested_t_sum
       in
       (* For all types found, pick only the T_sum, and make 4-uple out of them  *)
       let matching_t_sum = List.filter_map ~f:filter_tsum @@ module_types in
       let matching_t_sum = dedup matching_t_sum in
       let general_type_opt =
         List.find ~f:(fun (_, tvs, _, _) -> not @@ List.is_empty tvs) matching_t_sum
       in
       match general_type_opt with
       | Some general_type -> [ general_type ]
       | None -> matching_t_sum) [@landmark "get_sum"])


let get_record : t -> Type.t Label.Map.t -> (Type_var.t option * Type.row) option =
  let record_hashable : Type.t Label.Map.t hashable =
    (module struct
      (* Use Record here since [hash_fold_t] is provided there *)
      type t = Type.t Record.t [@@deriving equal, hash]
    end)
  in
  memoize2 hashable record_hashable (fun ctx record_type ->
      (let record_type_kv : (Label.t * Type.t) list =
         Map.to_alist ~key_order:`Decreasing record_type
       in
       (* [is_record_type type_] returns true if [type_] corresponds to [record_type] *)
       let is_record_type type_ =
         match type_.content with
         | T_record record_type' ->
           let record_type_kv' : (Label.t * Type.t) list =
             Map.to_alist ~key_order:`Decreasing record_type'.fields
           in
           (match
              List.for_all2
                record_type_kv
                record_type_kv'
                ~f:(fun (Label (ka, _), va) (Label (kb, _), vb) ->
                  String.equal ka kb && Type.equal va vb)
            with
           | Ok result -> Option.some_if result (type_.orig_var, record_type')
           | Unequal_lengths -> None)
         | _ -> None
       in
       (* [find t ~to_type_map ~to_module_map] finds a record type matching [record_type] *)
       let rec find : type a. (a, (Type_var.t option * row) option) contextual =
        fun t ~to_type_map ~to_module_map ->
         match
           to_type_map t
           |> Map.to_alist
           |> List.find_map ~f:(fun (_, type_) -> is_record_type type_)
         with
         | Some (orig_var_opt, row) -> Some (Option.map orig_var_opt ~f:snd, row)
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
  val layout : ctx:t -> Type.layout -> bool
end = struct
  let rec context ctx =
    let rec loop t =
      match t with
      | [] -> true
      | item :: t ->
        loop t
        &&
        (match item with
        | C_value (var, _, type', _) ->
          (match type_ type' ~ctx with
          | Some Type -> true
          | _ ->
            Format.printf "Value %a has non-type type %a" Value_var.pp var Type.pp type';
            false)
        | C_type (_tvar, type') ->
          (match type_ type' ~ctx with
          | Some _ -> true
          | None ->
            (* Format.printf "Type %a = %a is ill-kinded" Type_var.pp tvar Type.pp type'; *)
            true)
        | C_type_var _ ->
          (* Shadowing permitted *)
          true
        | C_texists_var (evar, _) ->
          if Set.mem (get_texists_vars t) evar
          then (
            Format.printf "Existential variable ^%a is shadowed" Type_var.pp evar;
            false)
          else true
        | C_texists_eq (evar, kind, type') ->
          (not (Set.mem (get_texists_vars t) evar))
          &&
          (match type_ type' ~ctx with
          | Some kind' -> Kind.compare kind kind' = 0
          | _ ->
            (* Format.printf
              "Existential variable ^%a is ill-kinded. Expected: %a"
              Type_var.pp
              evar
              Kind.pp
              kind; *)
            true)
        | C_pos _ | C_mut_lock _ -> true
        | C_lexists_var (lvar, _) ->
          if Set.mem (get_lexists_vars t) lvar
          then (
            Format.printf "Existential layout variable ^%a is shadowed" Layout_var.pp lvar;
            false)
          else true
        | C_lexists_eq (lvar, _, layout_) ->
          (not (Set.mem (get_lexists_vars t) lvar)) && layout layout_ ~ctx
        | C_module (_mvar, sig_) ->
          (* Shadowing permitted *)
          signature ~ctx sig_
        | C_module_type _ -> true (* TODO *))
    in
    loop ctx


  and layout ~ctx (layout : Type.layout) : bool =
    match layout with
    | L_concrete _ -> true
    | L_exists lvar -> Set.mem (get_lexists_vars ctx) lvar


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
      | T_arrow { type1 = arg_type; type2 = ret_type; param_names = _ } ->
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
        if Map.for_all
             ~f:(fun associated_type ->
               match loop associated_type with
               | Some Type -> true
               | _ -> false)
             rows.fields
           && layout ~ctx rows.layout
        then return Type
        else None
    in
    loop t ~ctx


  and signature ~ctx sig_ =
    List.for_all ~f:(signature_item ~ctx) sig_.items && signature_sort ~ctx sig_.sort


  and signature_sort ~ctx (sig_sort : Signature.sort) =
    match sig_sort with
    | Ss_module -> true
    | Ss_contract { storage; parameter } ->
      (match type_ ~ctx storage, type_ ~ctx parameter with
      | Some Type, Some Type -> true
      | _ -> false)


  and signature_item ~ctx (sig_item : Signature.item Location.wrap) =
    match Location.unwrap sig_item with
    | S_value (_var, type', _) ->
      (match type_ ~ctx type' with
      | Some Type -> true
      | _ -> false)
    | S_type (_tvar, type', _) ->
      (match type_ ~ctx type' with
      | Some _ -> true
      | _ -> false)
    | S_type_var (_, _) -> true (* TODO *)
    | S_module (_mvar, sig_, _) -> signature ~ctx sig_
    | S_module_type (_mvar, _sig_, _) -> true (* TODO *)
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
      let rec hash_types : type a. (a, path:Module_var.t list -> unit) contextual =
       fun t ~to_type_map ~to_module_map ~path ->
        let path =
          match path with
          | [] -> []
          | mv :: _
            when Module_var.is_name mv "Curry_lib" || Module_var.is_name mv "Uncurry_lib"
            -> []
          | _ -> path
        in
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
      | C_lexists_eq (lvar, fields, layout) ->
        Substitution.add_lexists_eq subst lvar fields layout, tvars, lvars
      | C_texists_eq (tvar, kind, type_) ->
        Substitution.add_texists_eq subst tvar kind type_, tvars, lvars
      | C_texists_var (tvar, kind) -> subst, (tvar, kind) :: tvars, lvars
      | C_lexists_var (lvar, fields) -> subst, tvars, (lvar, fields) :: lvars
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
    List.map tvars ~f:(fun (tvar, kind) -> tvar, (Type_var.fresh_like ~loc tvar, kind))
  in
  (* Add layout substs *)
  let subst =
    List.fold_left lvars ~init:subst ~f:(fun subst (lvar, fields) ->
        let layout = Type.default_layout_from_field_set fields in
        Substitution.add_lexists_eq subst lvar fields layout)
  in
  (* Add universal tvars *)
  let subst =
    List.fold_left tvars ~init:subst ~f:(fun subst (tvar, (tvar', kind)) ->
        Substitution.add_texists_eq subst tvar kind (Type.t_variable ~loc tvar' ()))
  in
  let type_ = generalize_type ~loc ~tvars ~subst type_ in
  ctxl, type_, List.map tvars ~f:snd, subst


module Diff = struct
  include Simple_diff.Make (struct
    type t = int * item [@@deriving compare]
  end)

  let pp_change ppf change =
    let pp_iitem ppf (i, item) = Format.fprintf ppf "(%d, %a)" i PP.item item in
    match (change : diff) with
    | Equal _ -> ()
    | Added iitems ->
      Array.iter iitems ~f:(fun iitem -> Format.fprintf ppf "+ %a@;" pp_iitem iitem)
    | Deleted iitems ->
      Array.iter iitems ~f:(fun iitem -> Format.fprintf ppf "- %a@;" pp_iitem iitem)


  let pp ppf (ctx1, ctx2) =
    let loop ppf changes = List.iter changes ~f:(fun change -> pp_change ppf change) in
    Format.fprintf
      ppf
      "@[<v>Diff:@;%a@]@."
      loop
      (get_diff
         (Array.of_list_mapi ctx1 ~f:(fun i item -> i, item))
         (Array.of_list_mapi ctx2 ~f:(fun i item -> i, item)))
end
