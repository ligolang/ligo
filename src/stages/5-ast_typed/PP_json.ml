open Types
open Fold
open Format

type json = Yojson.t

module M = struct
  type no_state = NoState

  let to_json : (no_state, json) fold_config = {
      generic = (fun NoState info ->
        match info.node_instance.instance_kind with
        | RecordInstance { field_instances } ->
          let field_instances' = List.fold_left
            (fun acc (fld : ('xi, json) Adt_info.ctor_or_field_instance) -> (fld.cf.name, fld.cf_continue NoState)::acc)
            [] field_instances 
          in
          `Assoc field_instances'
        | VariantInstance { constructor ; _ } ->
          `List [ `String constructor.cf.name ; constructor.cf_continue NoState ]
        | PolyInstance { poly=_; arguments=_; poly_continue } ->
           (poly_continue NoState)
      );
      generic_empty_ctor        = (fun NoState -> `Null ) ;
      int                       = (fun _visitor NoState i               -> `Int i ) ;
      type_variable             = (fun _visitor NoState tv              -> `Assoc ["type-var", `String (asprintf "%a" Var.pp tv)] ) ;
      bool                      = (fun _visitor NoState b               -> `Bool b ) ;
      z                         = (fun _visitor NoState i               -> `String (asprintf "%a" Z.pp_print i) ) ;
      string                    = (fun _visitor NoState str             -> `String str ) ;
      ligo_string               = (fun _visitor NoState s               -> `String (asprintf "%s" (match s with Standard s -> s | Verbatim s -> s)) ) ;
      bytes                     = (fun _visitor NoState bytes           -> `String (Bytes.to_string bytes)) ;
      unit                      = (fun _visitor NoState ()              -> `String "unit" ) ;
      packed_internal_operation = (fun _visitor NoState _op             -> `String "Operation(...bytes)") ;
      expression_variable       = (fun _visitor NoState ev              -> `Assoc ["exp-var", `String (asprintf "%a" Var.pp ev.wrap_content)] ) ;
      constructor'              = (fun _visitor NoState (Constructor c) -> `Assoc ["constructor", `String c] ) ;
      location                  = (fun _visitor NoState loc             -> Location.pp_json loc) ;
      label                     = (fun _visitor NoState (Label lbl)     -> `Assoc ["label" , `String lbl] ) ;
      ast_core_type_expression  = (fun _visitor NoState te              -> `String (asprintf "%a" (Ast_core.PP.type_expression) te) ) ; (*TODO*)
      constructor_map           = (fun _visitor continue NoState cmap   ->
        let lst = List.sort (fun (Constructor a, _) (Constructor b, _) -> String.compare a b) (CMap.bindings cmap) in
        let lst' = List.fold_left
          (fun acc (Constructor k, v) -> (k , continue NoState v)::acc)
          [] lst
        in
        `Assoc lst' );
      label_map                 = (fun _visitor continue NoState lmap   ->
        let lst = List.sort (fun (Label a, _) (Label b, _) -> String.compare a b) (LMap.bindings lmap) in
        let lst' = List.fold_left
          (fun acc (Label k, v) -> (k , continue NoState v)::acc)
          [] lst
        in
        `Assoc lst' );
      list                      = (fun _visitor continue NoState lst ->
        let aux elt = continue NoState elt in
        `List (List.map aux lst) );
      location_wrap             = (fun _visitor continue NoState lwrap  ->
        let ({ wrap_content; location } : _ Location.wrap) = lwrap in
        `Assoc [("wrap_content", continue NoState wrap_content) ; ("location", `String (asprintf "%a" Location.pp location))] ); (*TODO*)
      option = (fun _visitor continue NoState o ->
        match o with
        | None -> `List [ `String "None" ; `Null ]
        | Some v -> `List [ `String "Some" ; continue NoState v ] );
      poly_unionfind            = (fun _visitor continue NoState p   ->
        let lst = (UnionFind.Poly2.partitions p) in
        let lst' = List.map (fun l -> continue NoState (UnionFind.Poly2.repr (List.hd l) p )) lst in
        `Assoc ["UnionFind", `List lst'] );
      poly_set                  = (fun _visitor continue NoState set   ->
        let lst = (RedBlackTrees.PolySet.elements set) in
        let lst' = List.map (fun el -> continue NoState el) lst in
        `Assoc ["Set", `List lst'] );
      typeVariableMap           = (fun _visitor continue NoState tvmap   ->
        let lst = List.sort (fun (a, _) (b, _) -> Var.compare a b) (RedBlackTrees.PolyMap.bindings tvmap) in
        let aux (k, v) =
          `Assoc [ asprintf "%a" Var.pp k , continue NoState v ] in
        let lst' = List.map aux lst in
        `Assoc ["typeVariableMap",  `List lst'] );
    }

  let to_json : ((no_state, json) fold_config -> no_state -> 'a -> json) -> 'a -> json = fun fold v ->
    fold to_json NoState v
  
  let print : ((no_state, json) fold_config -> no_state -> 'a -> json) -> formatter -> 'a -> unit  = fun fold ppf v ->
    fprintf ppf "%a" Yojson.pp (to_json fold v)
end

module Yojson = Fold.Folds(struct
  type in_state = M.no_state ;;
  type out_state = json ;;
  type 'a t = 'a -> json ;;
  let f = M.to_json ;;
end)

include Fold.Folds(struct
  type in_state = M.no_state ;;
  type out_state = json ;;
  type 'a t = formatter -> 'a -> unit ;;
  let f = M.print ;;
end)
