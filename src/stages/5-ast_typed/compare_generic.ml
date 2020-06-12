open Types
open Generated_fold

module M = struct
  let compare = ()              (* Hide Pervasives.compare to avoid calling it without explicit qualification. *)
  type 'a lz = unit -> 'a       (* Lazy values *)
  type t =
    | EmptyCtor
    | Record of string * (string * t lz) list
    | VariantConstructor of string * string * t lz
    | Bool of inline
    | Bytes of bytes
    | Constructor' of string
    | Expression_variable of expression_variable
    | Int of int
    | Label' of string
    | Ligo_string of ligo_string
    | Location of location
    | Operation of packed_internal_operation
    | Str of string
    | Type_expression of ast_core_type_expression
    | Unit of unit
    | Var of type_variable
    | Z of z
    | List of t lz list
    | Location_wrap of t lz Location.wrap
    | CMap of (constructor' * t lz) list
    | LMap of (label * t lz) list
    | UnionFind of t lz list list
    | Set of t lz list
    | TypeVariableMap of (type_variable * t lz) list

  type no_state = NoState

  (* TODO: make these functions return a lazy stucture *)
  let op : (no_state, t) fold_config = {
      generic = (fun NoState info ->
        match info.node_instance.instance_kind with
        | RecordInstance { fields } ->
           let aux (fld : ('xi, 'xo) Adt_info.ctor_or_field_instance) =
             ( fld.cf.name , fun () -> fld.cf_continue NoState ) in
           Record ("name_of_the_record", List.map aux fields)
        | VariantInstance { constructor ; _ } ->
           VariantConstructor ("name_of_the_variant", constructor.cf.name, fun () -> constructor.cf_continue NoState)
        | PolyInstance { poly=_; arguments=_; poly_continue } ->
           poly_continue NoState
      );
      generic_empty_ctor = (fun NoState -> EmptyCtor) ;
      int                       = (fun _visitor _state i               -> Int i );
      type_variable             = (fun _visitor _state type_variable   -> Var type_variable) ;
      bool                      = (fun _visitor _state b               -> Bool b) ;
      z                         = (fun _visitor _state i               -> Z i) ;
      string                    = (fun _visitor _state str             -> Str str) ;
      ligo_string               = (fun _visitor _state str             -> Ligo_string str) ;
      bytes                     = (fun _visitor _state bytes           -> Bytes bytes) ;
      unit                      = (fun _visitor _state ()              -> Unit ()) ;
      packed_internal_operation = (fun _visitor _state op              -> Operation op) ;
      expression_variable       = (fun _visitor _state ev              -> Expression_variable ev) ;
      constructor'              = (fun _visitor _state (Constructor c) -> Constructor' c) ;
      location                  = (fun _visitor _state loc             -> Location loc) ;
      label                     = (fun _visitor _state (Label lbl)     -> Label' lbl) ;
      ast_core_type_expression  = (fun _visitor _state te              -> Type_expression te) ;
      constructor_map           = (fun _visitor continue _state cmap   ->
        let kcmp (Constructor a, _) (Constructor b, _) = String.compare a b in
        let lst = List.sort kcmp (CMap.bindings cmap) in
        CMap (List.map (fun (k, v) -> (k, fun () -> continue NoState v)) lst));
      label_map                 = (fun _visitor continue _state lmap   ->
        let kcmp (Label a, _) (Label b, _) = String.compare a b in
        let lst = List.sort kcmp (LMap.bindings lmap) in
        LMap (List.map (fun (k, v) -> (k, fun () -> continue NoState v)) lst));
      list                      = (fun _visitor continue _state lst ->
        (List (List.map (fun x () -> continue NoState x) lst)));
      location_wrap             = (fun _visitor continue _state lwrap  ->
        let ({ wrap_content; location } : _ Location.wrap) = lwrap in
        (Location_wrap { wrap_content = (fun () -> continue NoState wrap_content) ; location}));
      option = (fun _visitor continue _state o ->
        match o with
        | None -> VariantConstructor ("built-in:option", "None", fun () -> EmptyCtor)
        | Some v -> VariantConstructor ("built-in:option", "Some", fun () -> continue NoState v));
      poly_unionfind            = (fun _visitor continue _state p   ->
        (* UnionFind.Poly2.partitions returns the partitions in a
         deterministic order, and the elements within a given
         partition also follow a deterministic order. *)
        let lst = (UnionFind.Poly2.partitions p) in
        let aux l = List.map (fun x () -> continue NoState x) l in
        UnionFind (List.map aux lst));
      poly_set                  = (fun _visitor continue _state set   ->
        Set (List.map (fun x () -> continue NoState x) (RedBlackTrees.PolySet.elements set)));
      typeVariableMap           = (fun _visitor continue _state tvmap   ->
        let kcmp (a, _) (b, _) = Var.compare a b in
        let lst = List.sort kcmp (RedBlackTrees.PolyMap.bindings tvmap) in
        TypeVariableMap (List.map (fun (k, v) -> (k, fun () -> continue NoState v)) lst));
    }

  let serialize : ((no_state, t) fold_config -> no_state -> 'a -> t) -> 'a -> t = fun fold v ->
    fold op NoState v

  (* What follows should be roughly the same for all ASTs, so it
     should be easy to share a single copy of that and of the t type
     definition above. *)

  (* Generate a unique tag for each case handled below. We can then
   compare data by their tag and contents. *)
  let tag = function
    | EmptyCtor               ->  0
    | Record              _ ->  1
    | VariantConstructor  _ ->  2
    | Bool                _ ->  3
    | Bytes               _ ->  4
    | Constructor'        _ ->  5
    | Expression_variable _ ->  6
    | Int                 _ ->  7
    | Label'              _ ->  8
    | Ligo_string         _ ->  9
    | Location            _ -> 10
    | Operation           _ -> 11
    | Str                 _ -> 12
    | Type_expression     _ -> 13
    | Unit                _ -> 14
    | Var                 _ -> 15
    | Z                   _ -> 16
    | List                _ -> 17
    | Location_wrap       _ -> 18
    | CMap                _ -> 19
    | LMap                _ -> 20
    | UnionFind           _ -> 21
    | Set                 _ -> 22
    | TypeVariableMap     _ -> 23

  let cmp2 f a1 b1 g a2 b2 = match f a1 b1 with 0 -> g a2 b2 | c -> c
  let cmp3 f a1 b1 g a2 b2 h a3 b3 = match f a1 b1 with 0 -> (match g a2 b2 with 0 -> h a3 b3 | c -> c) | c -> c
  let rec compare_field (na, va) (nb, vb) = cmp2 String.compare na nb compare_lz_t va vb
  and compare_cmap_entry (Constructor na, va) (Constructor nb, vb) = cmp2 String.compare na nb compare_lz_t va vb
  and compare_lmap_entry (Label       na, va) (Label       nb, vb) = cmp2 String.compare na nb compare_lz_t va vb
  and compare_tvmap_entry (tva, va) (tvb, vb) = cmp2 Var.compare tva tvb compare_lz_t va vb
  and compare_lz_t a b = compare_t (a ()) (b ())
  and compare_t (a : t) (b : t) =
    match (a, b) with
    | (EmptyCtor, EmptyCtor)                         -> failwith "Should not happen (unless for ctors with no args?)"
    | (Record (a, fa), Record (b, fb))               -> cmp2 String.compare a b (List.compare ~compare:compare_field) fa fb
    | (VariantConstructor (va, ca, xa), VariantConstructor (vb, cb, xb)) ->
       cmp3
         String.compare va vb
         String.compare ca cb
         compare_lz_t xa xb
    | (Bool a, Bool b)                               -> (Pervasives.compare : bool -> bool -> int) a b
    | (Bytes a, Bytes b)                             -> Bytes.compare a b
    | (Constructor' a, Constructor' b)               -> String.compare a b
    | (Expression_variable a, Expression_variable b) -> Var.compare a b
    | (Int a, Int b)                                 -> Int.compare a b
    | (Label' a, Label' b)                           -> String.compare a b
    | (Ligo_string a, Ligo_string b)                 -> Simple_utils.Ligo_string.compare a b
    | (Location a, Location b)                       -> Location.compare a b
    | (Operation a, Operation b)                     -> Pervasives.compare a b (* TODO: is there a proper comparison function defined for packed_internal_operation ? *)
    | (Str a, Str b)                                 -> String.compare a b
    | (Type_expression a, Type_expression b)         -> Pervasives.compare a b (* TODO: is there a proper comparison function defined for ast_core_type_expression ? *)
    | (Unit (), Unit ())                             -> 0
    | (Var a, Var b)                                 -> Var.compare a b
    | (Z a, Z b)                                     -> Z.compare a b
    | (List a, List b)                               -> List.compare ~compare:compare_lz_t a b
    | (Location_wrap a, Location_wrap b)             -> Location.compare_wrap ~compare:compare_lz_t a b
    | (CMap a, CMap b)                               -> List.compare ~compare:compare_cmap_entry a b
    | (LMap a, LMap b)                               -> List.compare ~compare:compare_lmap_entry a b
    | (UnionFind a, UnionFind b)                     -> List.compare ~compare:(List.compare ~compare:compare_lz_t) a b
    | (Set a, Set b)                                 -> List.compare ~compare:compare_lz_t a b
    | (TypeVariableMap a, TypeVariableMap b)         -> List.compare ~compare:compare_tvmap_entry a b

    | ((EmptyCtor | Record _ | VariantConstructor _ | Bool _ | Bytes _ | Constructor' _ | Expression_variable _ | Int _ | Label' _ | Ligo_string _ | Location _ | Operation _ | Str _ | Type_expression _ | Unit _ | Var _ | Z _ | List _ | Location_wrap _ | CMap _ | LMap _ | UnionFind _ | Set _ | TypeVariableMap _) as a),
      ((EmptyCtor | Record _ | VariantConstructor _ | Bool _ | Bytes _ | Constructor' _ | Expression_variable _ | Int _ | Label' _ | Ligo_string _ | Location _ | Operation _ | Str _ | Type_expression _ | Unit _ | Var _ | Z _ | List _ | Location_wrap _ | CMap _ | LMap _ | UnionFind _ | Set _ | TypeVariableMap _) as b) ->
       Int.compare (tag a) (tag b)


  let mk_compare : ((no_state , t) fold_config -> no_state -> 'a -> t) -> 'a -> 'a -> int = fun fold a b ->
    compare_t (serialize fold a) (serialize fold b)

  let mk_comparable : ((no_state , t) fold_config -> no_state -> 'a -> t) -> 'a extra_info__comparable = fun fold ->
    { compare = mk_compare fold }
end

(* Generate a comparison function for each type, named like the type itself. *)
include Folds(struct
  type in_state = M.no_state ;;
  type out_state = M.t ;;
  type 'a t = 'a -> 'a -> int ;;
  let f = M.mk_compare ;;
end)

module Comparable = struct
  (* Generate a comparator typeclass-like object for each type, named like the type itself. *)
  include Folds(struct
    type in_state = M.no_state ;;
    type out_state = M.t ;;
    type 'a t = 'a extra_info__comparable ;;
    let f = M.mk_comparable ;;
  end)
end
