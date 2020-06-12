open Types
open Fold
open Format
open PP_helpers

module M = struct
  type no_state = NoState
  let needs_parens              = {
      generic                   = (fun NoState info ->
        match info.node_instance.instance_kind with
        | RecordInstance _ -> false
        | VariantInstance _ -> true
        | PolyInstance { poly =_; arguments=_; poly_continue } ->
           (poly_continue NoState)
      );
      generic_empty_ctor        = (fun _ -> false) ;
      type_variable             = (fun _ _ _ -> true) ;
      bool                      = (fun _ _ _ -> false) ;
      int                       = (fun _ _ _ -> false) ;
      z                         = (fun _ _ _ -> false) ;
      string                    = (fun _ _ _ -> false) ;
      ligo_string               = (fun _ _ _ -> false) ;
      bytes                     = (fun _ _ _ -> false) ;
      unit                      = (fun _ _ _ -> false) ;
      packed_internal_operation = (fun _ _ _ -> false) ;
      expression_variable       = (fun _ _ _ -> false) ;
      constructor'              = (fun _ _ _ -> false) ;
      location                  = (fun _ _ _ -> false) ;
      label                     = (fun _ _ _ -> false) ;
      ast_core_type_expression  = (fun _ _ _ -> true) ;
      constructor_map           = (fun _ _ _ _ -> false) ;
      label_map                 = (fun _ _ _ _ -> false) ;
      list                      = (fun _ _ _ _ -> false) ;
      location_wrap             = (fun _ _ _ _ -> false) ;
      option                    = (fun _visitor _continue _state o ->
        match o with None -> false | Some _ -> true) ;
      poly_unionfind            = (fun _ _ _ _ -> false) ;
      poly_set                  = (fun _ _ _ _ -> false) ;
      typeVariableMap           = (fun _ _ _ _ -> false) ;
    }

  let op ppf : (no_state, unit) fold_config = {
      generic = (fun NoState info ->
        match info.node_instance.instance_kind with
        | RecordInstance { fields } ->
           let aux ppf (fld : ('xi , 'xo) Adt_info.ctor_or_field_instance) =
             fprintf ppf "%s = %a" fld.cf.name (fun _ppf -> fld.cf_continue) NoState in
           fprintf ppf "{@,@[<hv 2> %a @]@,}" (list_sep aux (fun ppf () -> fprintf ppf " ;@ ")) fields
        | VariantInstance { constructor ; _ } ->
           if constructor.cf_new_fold needs_parens NoState
           then fprintf ppf "%s (%a)" constructor.cf.name (fun _ppf -> constructor.cf_continue) NoState
           else let spc = if String.equal constructor.cf.type_ "" then "" else " " in
                fprintf ppf "%s%s%a" constructor.cf.name spc (fun _ppf -> constructor.cf_continue) NoState
        | PolyInstance { poly=_; arguments=_; poly_continue } ->
           (poly_continue NoState)
      );
      generic_empty_ctor        = (fun NoState -> ()) ;
      int                       = (fun _visitor NoState i               -> fprintf ppf "%i" i );
      type_variable             = (fun _visitor NoState type_variable   -> fprintf ppf "Var %a" Var.pp type_variable) ;
      bool                      = (fun _visitor NoState b               -> fprintf ppf "%s" (if b then "true" else "false")) ;
      z                         = (fun _visitor NoState i               -> fprintf ppf "%a" Z.pp_print i) ;
      string                    = (fun _visitor NoState str             -> fprintf ppf "\"%s\"" str) ;
      ligo_string               = (fun _visitor NoState str             -> fprintf ppf "%a" Ligo_string.pp str) ;
      bytes                     = (fun _visitor NoState _bytes          -> fprintf ppf "bytes...") ;
      unit                      = (fun _visitor NoState ()              -> fprintf ppf "()") ;
      packed_internal_operation = (fun _visitor NoState _op             -> fprintf ppf "Operation(...bytes)") ;
      expression_variable       = (fun _visitor NoState ev              -> fprintf ppf "%a" Var.pp ev) ;
      constructor'              = (fun _visitor NoState (Constructor c) -> fprintf ppf "Constructor %s" c) ;
      location                  = (fun _visitor NoState loc             -> fprintf ppf "%a" Location.pp loc) ;
      label                     = (fun _visitor NoState (Label lbl)     -> fprintf ppf "Label %s" lbl) ;
      ast_core_type_expression  = (fun _visitor NoState te              -> fprintf ppf "%a" Ast_core.PP.type_expression te) ;
      constructor_map           = (fun _visitor continue NoState cmap   ->
        let lst = List.sort (fun (Constructor a, _) (Constructor b, _) -> String.compare a b) (CMap.bindings cmap) in
        let aux ppf (Constructor k, v) =
          fprintf ppf "(Constructor %s, %a)" k (fun _ppf -> continue NoState) v in
        fprintf ppf "CMap [@,@[<hv 2> %a @]@,]" (list_sep aux (fun ppf () -> fprintf ppf " ; ")) lst);
      label_map                 = (fun _visitor continue NoState lmap   ->
        let lst = List.sort (fun (Label a, _) (Label b, _) -> String.compare a b) (LMap.bindings lmap) in
        let aux ppf (Label k, v) =
          fprintf ppf "(Constructor %s, %a)" k (fun _ppf -> continue NoState) v in
        fprintf ppf "LMap [@,@[<hv 2> %a @]@,]" (list_sep aux (fun ppf () -> fprintf ppf " ; ")) lst);
      list                      = (fun _visitor continue NoState lst ->
        let aux ppf elt =
          fprintf ppf "%a" (fun _ppf -> continue NoState) elt in
        fprintf ppf "[@,@[<hv 2> %a @]@,]" (list_sep aux (fun ppf () -> fprintf ppf " ;@ ")) lst);
      location_wrap             = (fun _visitor continue NoState lwrap  ->
        let ({ wrap_content; location } : _ Location.wrap) = lwrap in
        fprintf ppf "{ wrap_content = %a ; location = %a }" (fun _ppf -> continue NoState) wrap_content Location.pp location);
      option = (fun _visitor continue NoState o ->
        match o with
        | None -> fprintf ppf "None"
        | Some v -> fprintf ppf "%a" (fun _ppf -> continue NoState) v) ;
      poly_unionfind            = (fun _visitor continue NoState p   ->
        let lst = (UnionFind.Poly2.partitions p) in
        let aux1 l = fprintf ppf "[@,@[<hv 2> (*%a*) %a @]@,]"
                       (fun _ppf -> continue NoState) (UnionFind.Poly2.repr (List.hd l) p)
                       (list_sep (fun _ppf -> continue NoState) (fun ppf () -> fprintf ppf " ;@ ")) l in
        let aux2 = list_sep (fun _ppf -> aux1) (fun ppf () -> fprintf ppf " ;@ ") in
        fprintf ppf "UnionFind [@,@[<hv 2> %a @]@,]" aux2 lst);
      poly_set                  = (fun _visitor continue NoState set   ->
        let lst = (RedBlackTrees.PolySet.elements set) in
        fprintf ppf "Set [@,@[<hv 2> %a @]@,]" (list_sep (fun _ppf -> continue NoState) (fun ppf () -> fprintf ppf " ;@ ")) lst);
      typeVariableMap           = (fun _visitor continue NoState tvmap   ->
        let lst = List.sort (fun (a, _) (b, _) -> Var.compare a b) (RedBlackTrees.PolyMap.bindings tvmap) in
        let aux ppf (k, v) =
          fprintf ppf "(Var %a, %a)" Var.pp k (fun _ppf -> continue NoState) v in
        fprintf ppf "typeVariableMap [@,@[<hv 2> %a @]@,]" (list_sep aux (fun ppf () -> fprintf ppf " ;@ ")) lst);
    }

  let print : ((no_state, unit) fold_config -> no_state -> 'a -> unit) -> formatter -> 'a -> unit = fun fold ppf v ->
    fold (op ppf) NoState v
end

include Fold.Folds(struct
  type in_state = M.no_state ;;
  type out_state = unit ;;
  type 'a t = formatter -> 'a -> unit ;;
  let f = M.print ;;
end)
