(* This file represente the context which give the association of values to types *)
module Location = Simple_utils.Location
open Simple_utils.Trace
open Ast_typed
open Ligo_prim
module ValueMap = Simple_utils.Map.Make (Value_var)
module TypeMap = Ast_typed.Helpers.IdMap.Make (Type_var)
module ModuleMap = Ast_typed.Helpers.IdMap.Make (Module_var)

module Exists_var = struct
  type t = Type_var.t [@@deriving compare, hash]

  module Map = Simple_utils.Map.Make (struct
    type nonrec t = t [@@deriving compare]
  end)

  let equal t1 t2 = compare t1 t2 = 0
  let yojson_of_t t = Type_var.to_yojson t
  let loc = Type_var.get_location
  let of_type_var tvar = if Type_var.is_exists tvar then Some tvar else None
  let pp ppf t = Format.fprintf ppf "%a" Type_var.pp t
  let fresh = Type_var.fresh_exists

  let of_type_var_exn tvar =
    if not (Type_var.is_exists tvar)
    then failwith "Invalid existential variable";
    tvar
end

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


(* 
let rec_memoize2
  (type a b c)
  ?(size = 100)
  (key1 : a hashable)
  (key2 : b hashable)
  (f : (a -> b -> c) -> a -> b -> c)
  =
  let module Hashtbl = Caml.Ephemeron.K2.Make ((val key1)) ((val key2)) in
  let table : c Hashtbl.t = Hashtbl.create size in
  let rec memo_f key1 key2 =
    match Hashtbl.find_opt table (key1, key2) with
    | Some result -> result
    | None ->
      let result = f memo_f key1 key2 in
      Hashtbl.add table (key1, key2) result;
      result
  in
  memo_f *)

module Phys_hashable (T : T) = struct
  include T

  let equal = phys_equal
  let hash t = Caml.Hashtbl.hash t
end

module Signature = struct
  module T = struct
    type t = item list

    and item =
      | S_value of expression_variable * type_expression
      | S_type of type_variable * type_expression
      | S_module of module_variable * t
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
          | S_module (mvar', sig_) when Module_var.equal mvar mvar' -> Some sig_
          | _ -> None) [@landmark "get_module"]))


  let rec equal_item : item -> item -> bool =
   fun item1 item2 ->
    match item1, item2 with
    | S_value (var1, type1), S_value (var2, type2) ->
      Value_var.equal var1 var2 && type_expression_eq (type1, type2)
    | S_type (tvar1, type1), S_type (tvar2, type2) ->
      Type_var.equal tvar1 tvar2 && type_expression_eq (type1, type2)
    | S_module (mvar1, sig1), S_module (mvar2, sig2) ->
      Module_var.equal mvar1 mvar2 && equal sig1 sig2
    | _, _ -> false


  and equal t1 t2 = List.equal equal_item t1 t2

  let to_type_map =
    memoize hashable (fun t ->
      (List.fold_right t ~init:TypeMap.empty ~f:(fun item map ->
         match item with
         | S_type (tvar, type_) -> TypeMap.add map tvar type_
         | _ -> map) [@landmark "to_type_map"]))


  let to_module_map =
    memoize hashable (fun t ->
      (List.fold_right t ~init:ModuleMap.empty ~f:(fun item map ->
         match item with
         | S_module (mvar, t) -> ModuleMap.add map mvar t
         | _ -> map) [@landmark "to_module_map"]))


  include struct
    open Ast_typed.PP

    let list ~pp ppf xs =
      let rec loop ppf = function
        | [] -> Format.fprintf ppf ""
        | x :: xs -> Format.fprintf ppf "%a@.%a" pp x loop xs
      in
      Format.fprintf ppf "@[<v>%a@]" loop xs


    let rec pp_item ppf item =
      match item with
      | S_value (var, type_) ->
        Format.fprintf ppf "%a : %a" Value_var.pp var type_expression type_
      | S_type (tvar, type_) ->
        Format.fprintf ppf "type %a = %a" Type_var.pp tvar type_expression type_
      | S_module (mvar, sig_) ->
        Format.fprintf ppf "module %a = %a" Module_var.pp mvar pp sig_


    and pp ppf t = Format.fprintf ppf "@[<v>sig@,%a@,end@]" (list ~pp:pp_item) t
  end
end

type exists_variable = Exists_var.t [@@deriving hash]

type mutable_flag = Param.mutable_flag =
  | Mutable
  | Immutable
[@@deriving hash]

type pos = int [@@deriving hash]

module T = struct
  type t =
    { items : item list
    ; solved : (Kind.t * type_expression) Exists_var.Map.t [@hash.ignore]
    }

  and item =
    | C_value of expression_variable * mutable_flag * type_expression
    | C_type of type_variable * type_expression
    | C_type_var of type_variable * Kind.t
    | C_exists_var of exists_variable * Kind.t
    | C_exists_eq of exists_variable * Kind.t * type_expression
    | C_marker of exists_variable
    | C_module of module_variable * Signature.t
    | C_pos of pos
  [@@deriving hash]
end

include T

let hashable : t hashable = (module Phys_hashable (T))

module PP = struct
  open Ast_typed.PP

  let list ~pp ppf xs =
    let rec loop ppf = function
      | [] -> Format.fprintf ppf ""
      | x :: xs -> Format.fprintf ppf "%a@,%a" loop xs pp x
    in
    Format.fprintf ppf "@[<hv>%a@]" loop xs


  let context ppf t =
    list ppf t.items ~pp:(fun ppf item ->
      match item with
      | C_value (evar, mut_flag, type_) ->
        Format.fprintf
          ppf
          "%a%a : %a"
          Param.pp_mutable_flag
          mut_flag
          Value_var.pp
          evar
          type_expression
          type_
      | C_type (tvar, type_) ->
        Format.fprintf ppf "type %a = %a" Type_var.pp tvar type_expression type_
      | C_type_var (tvar, kind) ->
        Format.fprintf ppf "%a :: %a" Type_var.pp tvar Kind.pp kind
      | C_exists_var (evar, kind) ->
        Format.fprintf ppf "%a ^: %a" Exists_var.pp evar Kind.pp kind
      | C_exists_eq (evar, kind, type_) ->
        Format.fprintf
          ppf
          "%a :: %a = %a"
          Exists_var.pp
          evar
          Kind.pp
          kind
          type_expression
          type_
      | C_marker evar -> Format.fprintf ppf "|>%a" Exists_var.pp evar
      | C_module (mvar, sig_) ->
        Format.fprintf ppf "module %a = %a" Module_var.pp mvar Signature.pp sig_
      | C_pos _ -> ())


  let context_local ~pos ppf t =
    let rec loop ppf items =
      match items with
      | [] -> Format.fprintf ppf ""
      | C_value (evar, mut_flag, type_) :: items ->
        Format.fprintf
          ppf
          "%a%a : %a@,%a"
          Param.pp_mutable_flag
          mut_flag
          Value_var.pp
          evar
          type_expression
          type_
          loop
          items
      | C_type_var (tvar, kind) :: items ->
        Format.fprintf
          ppf
          "%a :: %a@,%a"
          Type_var.pp
          tvar
          Kind.pp
          kind
          loop
          items
      | C_exists_var (evar, kind) :: items ->
        Format.fprintf
          ppf
          "%a :: %a@,%a"
          Exists_var.pp
          evar
          Kind.pp
          kind
          loop
          items
      | C_exists_eq (evar, kind, type_) :: items ->
        Format.fprintf
          ppf
          "%a :: %a = %a@,%a"
          Exists_var.pp
          evar
          Kind.pp
          kind
          type_expression
          type_
          loop
          items
      | C_marker evar :: items ->
        Format.fprintf ppf "|>%a@,%a" Exists_var.pp evar loop items
      | C_type (tvar, type_) :: items ->
        Format.fprintf
          ppf
          "type %a = %a@,%a"
          Type_var.pp
          tvar
          type_expression
          type_
          loop
          items
      | C_pos pos' :: _items when pos = pos' -> Format.fprintf ppf ""
      | _ :: items -> loop ppf items
    in
    Format.fprintf ppf "@[<hv>%a@]" loop t.items
end

let pp = PP.context
let pp_ = PP.context_local
let empty = { items = []; solved = Exists_var.Map.empty }
let add t item = { t with items = item :: t.items }

let join t1 t2 =
  { items = t2.items @ t1.items
  ; solved =
      Exists_var.Map.merge
        (fun _ eq1 eq2 ->
          match eq1, eq2 with
          | eq1, None -> eq1
          | _, eq2 -> eq2)
        t1.solved
        t2.solved
  }


let of_list items = { empty with items = List.rev items }

(* Inifix notations for [add] and [join] *)
let ( |:: ) = add
let ( |@ ) = join
let add_value t evar mut_flag type_ = t |:: C_value (evar, mut_flag, type_)
let add_imm t evar type_ = t |:: C_value (evar, Immutable, type_)
let add_mut t evar type_ = t |:: C_value (evar, Mutable, type_)
let add_type t tvar type_ = t |:: C_type (tvar, type_)
let add_type_var t tvar kind = t |:: C_type_var (tvar, kind)
let add_exists_var t evar kind = t |:: C_exists_var (evar, kind)
let add_marker t evar = t |:: C_marker evar
let add_module t mvar mctx = t |:: C_module (mvar, mctx)

let add_signature_item t (sig_item : Signature.item) =
  match sig_item with
  | S_value (var, type_) -> add_imm t var type_
  | S_type (tvar, type_) -> add_type t tvar type_
  | S_module (mvar, sig_) -> add_module t mvar sig_


let get_value =
  memoize2
    hashable
    (module Value_var)
    (fun t evar ->
      (List.find_map t.items ~f:(function
        | C_value (evar', mut_flag, type_) when Value_var.equal evar evar' ->
          Some (mut_flag, type_)
        | _ -> None) [@landmark "get_value"]))


let get_imm =
  memoize2
    hashable
    (module Value_var)
    (fun t evar ->
      List.find_map t.items ~f:(function
        | C_value (evar', Immutable, type_) when Value_var.equal evar evar' ->
          Some type_
        | _ -> None))


let get_mut =
  memoize2
    hashable
    (module Value_var)
    (fun t evar ->
      List.find_map t.items ~f:(function
        | C_value (evar', Mutable, type_) when Value_var.equal evar evar' ->
          Some type_
        | _ -> None))


let get_type =
  memoize2
    hashable
    (module Type_var)
    (fun t tvar ->
      (List.find_map t.items ~f:(function
        | C_type (tvar', type_) when Type_var.equal tvar tvar' -> Some type_
        | _ -> None) [@landmark "get_type"]))


let get_module =
  memoize2
    hashable
    (module Module_var)
    (fun t mvar ->
      (List.find_map t.items ~f:(function
        | C_module (mvar', mctx) when Module_var.equal mvar mvar' -> Some mctx
        | _ -> None) [@landmark "get_module"]))


let get_type_vars =
  memoize hashable (fun t ->
    (List.filter_map t.items ~f:(function
      | C_type_var (tvar, _) -> Some tvar
      | _ -> None) [@landmark "get_type_vars"]))


let get_exists_vars =
  memoize hashable (fun t ->
    (List.filter_map t.items ~f:(function
      | C_exists_var (evar, _) -> Some evar
      | _ -> None) [@landmark "get_exists_vars"]))


let get_markers =
  memoize hashable (fun t ->
    (List.filter_map t.items ~f:(function
      | C_marker evar -> Some evar
      | _ -> None) [@landmark "get_markers"]))


let get_exists_var =
  memoize2
    hashable
    (module Exists_var)
    (fun t evar ->
      (List.find_map t.items ~f:(function
        | (C_exists_var (evar', kind) | C_exists_eq (evar', kind, _))
          when Exists_var.equal evar evar' -> Some kind
        | _ -> None) [@landmark "get_exists_var"]))


let get_type_var =
  memoize2
    hashable
    (module Type_var)
    (fun t tvar ->
      (List.find_map t.items ~f:(function
        | C_type_var (tvar', kind) when Type_var.equal tvar tvar' -> Some kind
        | _ -> None) [@landmark "get_type_var"]))


let get_exists_eq =
  memoize2
    hashable
    (module Exists_var)
    (fun t evar ->
      (List.find_map t.items ~f:(function
        | C_exists_eq (evar', _kind, type_) when Exists_var.equal evar evar' ->
          Some type_
        | _ -> None) [@landmark "get_exists_eq"]))


let equal_item : item -> item -> bool =
 fun item1 item2 ->
  match item1, item2 with
  | C_value (x1, mut_flag1, type1), C_value (x2, mut_flag2, type2) ->
    Value_var.equal x1 x2
    && Param.compare_mutable_flag mut_flag1 mut_flag2 = 0
    && compare_type_expression type1 type2 = 0
  | C_type (tvar1, type1), C_type (tvar2, type2) ->
    Type_var.equal tvar1 tvar2 && compare_type_expression type1 type2 = 0
  | C_type_var (tvar1, kind1), C_type_var (tvar2, kind2) ->
    Type_var.equal tvar1 tvar2 && Kind.compare kind1 kind2 = 0
  | C_exists_var (evar1, kind1), C_exists_var (evar2, kind2) ->
    Exists_var.equal evar1 evar2 && Kind.compare kind1 kind2 = 0
  | C_exists_eq (evar1, kind1, type1), C_exists_eq (evar2, kind2, type2) ->
    Exists_var.equal evar1 evar2
    && Kind.compare kind1 kind2 = 0
    && compare_type_expression type1 type2 = 0
  | C_marker evar1, C_marker evar2 -> Exists_var.equal evar1 evar2
  | C_module (mvar1, sig1), C_module (mvar2, sig2) ->
    Module_var.equal mvar1 mvar2 && Signature.equal sig1 sig2
  | C_pos pos1, C_pos pos2 -> pos1 = pos2
  | _, _ -> false


let drop_until t ~pos =
  let rec loop t =
    match t.items with
    | [] -> t
    | C_pos pos' :: items when pos = pos' -> { t with items }
    | item :: items ->
      loop
        { items
        ; solved =
            (match item with
             | C_exists_eq (evar, kind, type_) ->
               Exists_var.Map.add evar (kind, type_) t.solved
             | _ -> t.solved)
        }
  in
  loop t


let remove_pos t ~pos =
  { t with
    items =
      List.filter t.items ~f:(function
        | C_pos pos' when pos = pos' -> false
        | _ -> true)
  }


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
  (* Left context gets solved *)
  let solved = t.solved in
  let l, r = loop t.items in
  { items = l; solved }, { empty with items = r }


let mark =
  let next = ref 0 in
  fun t ->
    let pos =
      Int.incr next;
      !next
    in
    t |:: C_pos pos, pos


let insert_at t ~at ~hole =
  let t1, t2 = split_at t ~at in
  t1 |@ hole |@ t2


let add_exists_eq t evar kind type_ =
  let t1, t2 = split_at t ~at:(C_exists_var (evar, kind)) in
  t1 |@ of_list [ C_exists_eq (evar, kind, type_) ] |@ t2


let rec apply t (type_ : type_expression) : type_expression =
  let self = apply t in
  let return content = { type_ with type_content = content } in
  match type_.type_content with
  | T_variable tvar ->
    (match Exists_var.of_type_var tvar with
     | Some evar ->
       (match get_exists_eq t evar with
        | Some type_' -> self type_'
        | None -> type_)
     | None -> type_)
  | T_constant inj ->
    let parameters = List.map ~f:self inj.parameters in
    return @@ T_constant { inj with parameters }
  | T_sum rows ->
    let fields =
      Record.map
        (fun (row_elem : _ Rows.row_element_mini_c) ->
          let associated_type = self row_elem.associated_type in
          { row_elem with associated_type })
        rows.fields
    in
    return @@ T_sum { rows with fields }
  | T_record rows ->
    let fields =
      Record.map
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


let rec signature_item_apply t (sig_item : Signature.item) : Signature.item =
  match sig_item with
  | S_type (tvar, type_) -> S_type (tvar, apply t type_)
  | S_value (var, type_) -> S_value (var, apply t type_)
  | S_module (mvar, sig_) -> S_module (mvar, signature_apply t sig_)


and signature_apply t (sig_ : Signature.t) : Signature.t =
  List.map sig_ ~f:(signature_item_apply t)


let to_type_map =
  memoize hashable (fun t ->
    (List.fold_right t.items ~init:TypeMap.empty ~f:(fun item map ->
       match item with
       | C_type (tvar, type_) -> TypeMap.add map tvar type_
       | _ -> map) [@landmark "to_type_map"]))


let to_module_map =
  memoize hashable (fun t ->
    (List.fold_right t.items ~init:ModuleMap.empty ~f:(fun item map ->
       match item with
       | C_module (mvar, mctx) -> ModuleMap.add map mvar mctx
       | _ -> map) [@landmark "to_module_map"]))


let get_signature t ((local_module, path) : Module_var.t List.Ne.t) =
  let open Option.Let_syntax in
  List.fold path ~init:(get_module t local_module) ~f:(fun sig_ mvar ->
    let%bind sig_ = sig_ in
    Signature.get_module sig_ mvar)


type ('a, 'ret) contextual =
  'a
  -> to_type_map:('a -> type_expression TypeMap.t)
  -> to_module_map:('a -> Signature.t ModuleMap.t)
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
let get_module_types : t -> (type_variable * type_expression) list =
  memoize hashable (fun ctx ->
    let rec signature : Signature.t -> type_expression TypeMap.kvi_list =
     fun sig_ ->
      (* Types in the current signature *)
      let local_types = TypeMap.to_kvi_list @@ Signature.to_type_map sig_ in
      (* Recursively fetch types from submodules *)
      let modules = ModuleMap.to_kv_list @@ Signature.to_module_map sig_ in
      List.fold modules ~init:local_types ~f:(fun types (_, sig_) ->
        List.rev_append types @@ signature sig_)
    in
    let local_types = TypeMap.to_kvi_list @@ to_type_map ctx in
    let modules = ModuleMap.to_kv_list @@ to_module_map ctx in
    TypeMap.sort_to_kv_list
    @@ (List.fold modules ~init:local_types ~f:(fun types (_, sig_) ->
          List.rev_append types @@ signature sig_) [@landmark
                                                     "get_module_types"]))


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
  :  Label.t -> t
  -> (type_variable * type_variable list * type_expression * type_expression)
     list
  =
  memoize2
    (module Label)
    hashable
    (fun ctor ctxt ->
      (let filter_tsum (var, type_) =
         let t_params, type_ =
           Ast_typed.Helpers.destruct_type_abstraction type_
         in
         match type_.type_content with
         | T_sum m ->
           (match Record.LMap.find_opt ctor m.fields with
            | Some { associated_type; _ } ->
              Some (var, t_params, associated_type, type_)
            | None -> None)
         | _ -> None
       in
       (* Format.printf "Fetching module types...\n"; *)
       (* Format.print_flush (); *)
       (* Fetch all types declared in current module and its submodules *)
       let module_types = get_module_types ctxt in
       (* Format.printf "Found all module types\n"; *)
       (* Format.print_flush (); *)
       (*  Also add the shadowed t_sum types nested in the fetched types.
        Since context is made of maps, all shadowed types are absent from the context.
        However we still want the shadowed nested t_sum, see [add_shadowed_nested_t_sum] *)
       let module_types =
         List.fold
           (List.rev module_types)
           ~init:[]
           ~f:Ast_typed.Helpers.add_shadowed_nested_t_sum
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
       (* Filter out duplicates (this prevents false warnings of "infered type is X but could also be X"
       when a same type is present several times in the context) *)
       let remove_doubles l
         : (type_variable
           * type_variable list
           * type_expression
           * type_expression)
         list
         =
         let add_no_dup l elt
           : (type_variable
             * type_variable list
             * type_expression
             * type_expression)
           list
           =
           let (_tv, _tvs, _te, te)
                 : type_variable
                   * type_variable list
                   * type_expression
                   * type_expression
             =
             elt
           in
           match
             List.find l ~f:(fun (_tv, _tvs, _te, te') ->
               hash_type_expression te = hash_type_expression te')
           with
           | Some _ -> l
           | None -> elt :: l
         in
         List.rev @@ List.fold l ~f:add_no_dup ~init:[]
       in
       let matching_t_sum = remove_doubles matching_t_sum in
       let general_type_opt =
         List.find
           ~f:(fun (_, tvs, _, _) -> not @@ List.is_empty tvs)
           matching_t_sum
       in
       match general_type_opt with
       | Some general_type -> [ general_type ]
       | None -> matching_t_sum) [@landmark "get_sum"])


let get_record : _ Record.t -> t -> (type_variable option * rows) option =
  let record_hashable
    : type_expression Rows.row_element_mini_c Record.t hashable
    =
    (module struct
      type t = type_expression Rows.row_element_mini_c Record.t

      let equal =
        Record.equal (Rows.equal_row_element_mini_c equal_type_expression)


      let hash =
        Hash.of_fold
          (Record.hash_fold_t
             (Rows.hash_fold_row_element_mini_c hash_fold_type_expression))
    end)
  in
  memoize2 record_hashable hashable (fun record_type ctx ->
    (let record_type_kv : (Label.t * _ Rows.row_element_mini_c) list =
       Record.LMap.to_kv_list_rev record_type
     in
     (* [is_record_type type_] returns true if [type_] corresponds to [record_type] *)
     let is_record_type type_ =
       match type_.type_content with
       | T_record record_type' ->
         let record_type_kv' : (Label.t * _ Rows.row_element_mini_c) list =
           Record.LMap.to_kv_list_rev record_type'.fields
         in
         (match
            List.for_all2
              record_type_kv
              record_type_kv'
              ~f:(fun (ka, va) (kb, vb) ->
              let (Label ka) = ka in
              let (Label kb) = kb in
              String.(ka = kb)
              && type_expression_eq (va.associated_type, vb.associated_type))
          with
          | Ok result -> Option.some_if result (type_.orig_var, record_type')
          | Unequal_lengths -> None)
       | _ -> None
     in
     (* [find t ~to_type_map ~to_module_map] finds a record type matching [record_type] *)
     let rec find : type a. (a, (Type_var.t option * t_sum) option) contextual =
      fun t ~to_type_map ~to_module_map ->
       match
         to_type_map t
         |> TypeMap.to_kv_list
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
           (ModuleMap.to_kv_list modules)
     in
     ctx_contextual find @@ ctx) [@landmark "get_record"])


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
    public, S_value (Binder.get_var binder, expr.type_expression)
  | D_type { type_binder = tvar; type_expr = type_; type_attr = { public; _ } }
    -> public, S_type (tvar, type_)
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
        add_imm ctx (Binder.get_var binder) expr.type_expression
      | D_type { type_binder; type_expr; type_attr = _ } ->
        add_type ctx type_binder type_expr
      | D_module { module_binder; module_; module_attr = _ } ->
        let sig_ = signature_of_module_expr ~ctx module_ in
        add_module ctx module_binder sig_)


module Well_formed : sig
  val context : t -> bool
  val type_expr : ctx:t -> type_expression -> Kind.t option
end = struct
  let rec context ctx =
    let rec loop t =
      match t.items with
      | [] -> true
      | item :: items ->
        let t = { t with items } in
        loop t
        &&
        (match item with
         | C_value (var, _, type_) ->
           (match type_expr type_ ~ctx with
            | Some Type -> true
            | _ ->
              Format.printf
                "Value %a has non-type type %a"
                Value_var.pp
                var
                Ast_typed.PP.type_expression
                type_;
              false)
         | C_type (tvar, type_) ->
           (match type_expr type_ ~ctx with
            | Some _ -> true
            | None ->
              Format.printf
                "Type %a = %a is ill-kinded"
                Type_var.pp
                tvar
                Ast_typed.PP.type_expression
                type_;
              false)
         | C_type_var _ ->
           (* Shadowing permitted *)
           true
         | C_exists_var (evar, _) ->
           if List.mem ~equal:Exists_var.equal (get_exists_vars t) evar
           then (
             Format.printf
               "Existential variable %a is shadowed"
               Exists_var.pp
               evar;
             false)
           else true
         | C_exists_eq (evar, kind, type_) ->
           (not (List.mem ~equal:Exists_var.equal (get_exists_vars t) evar))
           &&
           (match type_expr type_ ~ctx with
            | Some kind' -> Kind.compare kind kind' = 0
            | _ ->
              Format.printf
                "Existential variable %a is ill-kinded. Expected: %a"
                Exists_var.pp
                evar
                Kind.pp
                kind;
              false)
         | C_marker evar ->
           (not (List.mem ~equal:Exists_var.equal (get_markers t) evar))
           && not (List.mem ~equal:Exists_var.equal (get_exists_vars t) evar)
         | C_pos _ -> true
         | C_module (_mvar, sig_) ->
           (* Shadowing permitted *)
           signature ~ctx sig_)
    in
    loop ctx


  and type_expr ~ctx t : Kind.t option =
    let open Option.Let_syntax in
    let open Kind in
    let rec loop (t : type_expression) ~ctx =
      let self ?(ctx = ctx) t = loop t ~ctx in
      match t.type_content with
      | T_variable tvar ->
        (match Exists_var.of_type_var tvar with
         | Some evar -> get_exists_var ctx evar
         | None -> get_type_var ctx tvar)
      | T_constant { parameters; _ } ->
        (* Hack. No HKT parameters, so simply check if all params are
           of kind: *. *)
        if List.for_all parameters ~f:(fun param ->
             match self param with
             | Some Type -> true
             | _ ->
               Format.printf
                 "Ill-kinded parameter: %a\n"
                 Ast_typed.PP.type_expression
                 param;
               false)
        then return Type
        else None
      | T_singleton _ -> return Singleton
      | T_arrow { type1 = arg_type; type2 = ret_type } ->
        let%bind arg_kind = self arg_type in
        let%bind ret_kind = self ret_type in
        (match arg_kind, ret_kind with
         | Type, Type -> Some Type
         | _ -> None)
      | T_abstraction { ty_binder = tvar; kind; type_ } ->
        let%bind kind' = self ~ctx:(ctx |:: C_type_var (tvar, kind)) type_ in
        return @@ Arrow (kind, kind')
      | T_for_all { ty_binder = tvar; kind; type_ } ->
        (match%bind self ~ctx:(ctx |:: C_type_var (tvar, kind)) type_ with
         | Type -> return Type
         | _ -> None)
      | T_sum rows | T_record rows ->
        if Record.LMap.for_all
             (fun _label ({ associated_type; _ } : _ Rows.row_element_mini_c) ->
               match self associated_type with
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
    | S_value (_var, type_) ->
      (match type_expr ~ctx type_ with
       | Some Type -> true
       | _ -> false)
    | S_type (_tvar, type_) ->
      (match type_expr ~ctx type_ with
       | Some _ -> true
       | _ -> false)
    | S_module (_mvar, sig_) -> signature ~ctx sig_
end

module Hashes = struct
  module HTBL = Caml.Hashtbl.Make (struct
    type t = type_expression

    let hash = hash_type_expression

    let equal t1 t2 =
      match assert_type_expression_eq (t1, t2) with
      | Some _ -> true
      | None -> false
  end)

  let hashtbl : (module_variable list * type_variable) HTBL.t = HTBL.create 256
  let context = ref (false, empty)
  let set_context (t : t) : unit = context := false, t

  let hash_types () : unit =
    let hashed, t = !context in
    if hashed
    then ()
    else (
      let rec hash_types
        : type a. (a, path:module_variable list -> unit) contextual
        =
       fun t ~to_type_map ~to_module_map ~path ->
        let types = TypeMap.to_kv_list @@ to_type_map t in
        let modules = ModuleMap.to_kv_list @@ to_module_map t in
        List.iter (List.rev types) ~f:(fun (v, t) ->
          HTBL.add hashtbl t (path, v));
        List.iter (List.rev modules) ~f:(fun (v, t) ->
          sig_contextual hash_types t ~path:(path @ [ v ]))
      in
      HTBL.clear hashtbl;
      ctx_contextual hash_types t ~path:[];
      context := true, t)


  let find_type (t : type_expression) : (Module_var.t list * Type_var.t) option =
    HTBL.find_opt hashtbl t
end

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
    | E_record expr_label_map -> E_record (Record.map self expr_label_map)
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


  and matching_expr_apply ctx match_expr =
    match match_expr with
    | Match_variant { cases; tv } ->
      Match_variant
        { cases =
            List.map cases ~f:(fun content ->
              { content with body = e_apply ctx content.body })
        ; tv = t_apply ctx tv
        }
    | Match_record { fields; body; tv } ->
      Match_record
        { fields =
            Record.LMap.map (fun binder -> binder_apply ctx binder) fields
        ; body = e_apply ctx body
        ; tv = t_apply ctx tv
        }


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


  and matching_expr_pass ~raise match_expr =
    match match_expr with
    | Match_variant { cases; tv } ->
      type_pass ~raise tv;
      List.iter cases ~f:(fun { body; _ } -> expression_pass ~raise body)
    | Match_record { fields; body; tv } ->
      type_pass ~raise tv;
      Record.LMap.iter (fun _ binder -> binder_pass ~raise binder) fields;
      expression_pass ~raise body


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


let enter ~ctx ~in_ =
  let ctx, pos = mark ctx in
  let ctx, ret_type, expr = in_ ctx in
  let ctxl, ctxr = split_at ctx ~at:(C_pos pos) in
  let ret_type = apply ctxr ret_type in
  let ctxr = unsolved ctxr in
  ctxl |@ ctxr, ret_type, expr


let decl_enter ~ctx ~in_ =
  let ctx, pos = mark ctx in
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


  let enter ~ctx ~in_ =
    let open Elaboration.Let_syntax in
    let ctx, pos = mark ctx in
    let ctx, ret_type, expr = in_ ctx in
    let ctxl, ctxr = split_at ctx ~at:(C_pos pos) in
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
end
