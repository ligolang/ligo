open Format
open Generated_fold
open Types

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
      string                    = (fun _visitor NoState str             -> `String str ) ;
      z                         = (fun _visitor NoState i               -> `String (asprintf "%a" Z.pp_print i) ) ;
      ligo_string               = (fun _visitor NoState s               -> `String (asprintf "%s" (match s with Standard s -> s | Verbatim s -> s)) ) ;
      bytes                     = (fun _visitor NoState bytes           -> `String (Bytes.to_string bytes)) ;
      expression_variable       = (fun _visitor NoState ev              -> `Assoc ["exp-var", `String (asprintf "%a" Var.pp ev.wrap_content)] ) ;
      location                  = (fun _visitor NoState loc             -> Location.pp_json loc) ;
      label                     = (fun _visitor NoState (Label lbl)     -> `Assoc ["label" , `String lbl] ) ;
      label_map                 = (fun _visitor continue NoState lmap   ->
        let lst = List.sort (fun ((Label a:Types_utils.label), _) (Label b, _) -> String.compare a b) (LMap.bindings lmap) in
        let lst' = List.fold_left
          (fun acc ((Label k:Types_utils.label), v) -> (k , continue NoState v)::acc)
          [] lst
        in
        `Assoc lst' );
      list                      = (fun _visitor continue NoState lst ->
        let aux elt = continue NoState elt in
        `List (List.map aux lst) );
      location_wrap             = (fun _visitor continue NoState lwrap  ->
        let ({ wrap_content; location } : _ Location.wrap) = lwrap in
        `Assoc [("wrap_content", continue NoState wrap_content) ; ("location", Location.pp_json location)] );
      option = (fun _visitor continue NoState o ->
        match o with
        | None -> `List [ `String "None" ; `Null ]
        | Some v -> `List [ `String "Some" ; continue NoState v ] );
      sugar_type_expression_option = (fun _visitor NoState teo ->
        match teo with
        | None -> `Null
        (*TODO: Ast_sugar has no JSON representation for now*)
        | Some t -> `String (Format.asprintf "%a" Ast_sugar.PP.type_expression t)
        ) ;
      sugar_expression_option = (fun _visitor NoState eo ->
        match eo with
        | None -> `Null
        (*TODO: Ast_sugar has no JSON representation for now*)
        | Some e -> `String (Format.asprintf "%a" Ast_sugar.PP.expression e)
        ) ;
    }

  let to_json : ((no_state, json) fold_config -> no_state -> 'a -> json) -> 'a -> json = fun fold v ->
    fold to_json NoState v
  
  let print : ((no_state, json) fold_config -> no_state -> 'a -> json) -> formatter -> 'a -> unit  = fun fold ppf v ->
    fprintf ppf "%a" Yojson.pp (to_json fold v)
end

module Yojson = Generated_fold.Folds(struct
  type in_state = M.no_state ;;
  type out_state = json ;;
  type 'a t = 'a -> json ;;
  let f = M.to_json ;;
end)

include Generated_fold.Folds(struct
  type in_state = M.no_state ;;
  type out_state = json ;;
  type 'a t = formatter -> 'a -> unit ;;
  let f = M.print ;;
end)
