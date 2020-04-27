open Fold
open Format
open PP_helpers

let needs_parens              = {
    generic                   = (fun state info ->
      match info.node_instance.instance_kind with
      | RecordInstance _ -> false
      | VariantInstance _ -> true
      | PolyInstance { poly =_; arguments=_; poly_continue } ->
         (poly_continue state)
    );
    type_variable             = (fun _ _ _ -> true) ;
    bool                      = (fun _ _ _ -> false) ;
    z                         = (fun _ _ _ -> false) ;
    string                    = (fun _ _ _ -> false) ;
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
    list_ne                   = (fun _ _ _ _ -> false) ;
    option                    = (fun _visitor _continue _state o ->
      match o with None -> false | Some _ -> true) ;
    poly_unionfind            = (fun _ _ _ _ -> false) ;
    poly_set                  = (fun _ _ _ _ -> false) ;
    typeVariableMap           = (fun _ _ _ _ -> false) ;
  }

let op ppf = {
    generic = (fun () info ->
      match info.node_instance.instance_kind with
      | RecordInstance { fields } ->
         let aux ppf (fld : 'x Adt_info.ctor_or_field_instance) =
           fprintf ppf "%s = %a" fld.cf.name (fun _ppf -> fld.cf_continue) () in
         fprintf ppf "{@,@[<hv 2> %a @]@,}" (list_sep aux (fun ppf () -> fprintf ppf " ;@ ")) fields
      | VariantInstance { constructor ; _ } ->
         if constructor.cf_new_fold needs_parens false
         then fprintf ppf "%s (%a)" constructor.cf.name (fun _ppf -> constructor.cf_continue) ()
         else let spc = if String.equal constructor.cf.type_ "" then "" else " " in
              fprintf ppf "%s%s%a" constructor.cf.name spc (fun _ppf -> constructor.cf_continue) ()
      | PolyInstance { poly=_; arguments=_; poly_continue } ->
         (poly_continue ())
    );
    type_variable             = (fun _visitor () type_variable   -> fprintf ppf "Var %a" Var.pp type_variable) ;
    bool                      = (fun _visitor () b               -> fprintf ppf "%s" (if b then "true" else "false")) ;
    z                         = (fun _visitor () i               -> fprintf ppf "%a" Z.pp_print i) ;
    string                    = (fun _visitor () str             -> fprintf ppf "\"%s\"" str) ;
    bytes                     = (fun _visitor () _bytes          -> fprintf ppf "bytes...") ;
    unit                      = (fun _visitor () ()              -> fprintf ppf "()") ;
    packed_internal_operation = (fun _visitor () _op             -> fprintf ppf "Operation(...bytes)") ;
    expression_variable       = (fun _visitor () ev              -> fprintf ppf "%a" Var.pp ev) ;
    constructor'              = (fun _visitor () (Constructor c) -> fprintf ppf "Constructor %s" c) ;
    location                  = (fun _visitor () loc             -> fprintf ppf "%a" Location.pp loc) ;
    label                     = (fun _visitor () (Label lbl)     -> fprintf ppf "Label %s" lbl) ;
    ast_core_type_expression  = (fun _visitor () te              -> fprintf ppf "%a" Ast_core.PP.type_expression te) ;
    constructor_map           = (fun _visitor continue () cmap   ->
      let lst = List.sort (fun (Constructor a, _) (Constructor b, _) -> String.compare a b) (CMap.bindings cmap) in
      let aux ppf (Constructor k, v) =
        fprintf ppf "(Constructor %s, %a)" k (fun _ppf -> continue ()) v in
      fprintf ppf "CMap [@,@[<hv 2> %a @]@,]" (list_sep aux (fun ppf () -> fprintf ppf " ; ")) lst);
    label_map                 = (fun _visitor continue () lmap   ->
      let lst = List.sort (fun (Label a, _) (Label b, _) -> String.compare a b) (LMap.bindings lmap) in
      let aux ppf (Label k, v) =
        fprintf ppf "(Constructor %s, %a)" k (fun _ppf -> continue ()) v in
      fprintf ppf "LMap [@,@[<hv 2> %a @]@,]" (list_sep aux (fun ppf () -> fprintf ppf " ; ")) lst);
    list                      = (fun _visitor continue () lst ->
      let aux ppf elt =
        fprintf ppf "%a" (fun _ppf -> continue ()) elt in
      fprintf ppf "[@,@[<hv 2> %a @]@,]" (list_sep aux (fun ppf () -> fprintf ppf " ;@ ")) lst);
    location_wrap             = (fun _visitor continue () lwrap  ->
      let ({ wrap_content; location } : _ Location.wrap) = lwrap in
      fprintf ppf "{ wrap_content = %a ; location = %a }" (fun _ppf -> continue ()) wrap_content Location.pp location);
    list_ne = (fun _visitor continue () (first, lst) ->
      let aux ppf elt =
        fprintf ppf "%a" (fun _ppf -> continue ()) elt in
      fprintf ppf "[@,@[<hv 2> %a @]@,]" (list_sep aux (fun ppf () -> fprintf ppf " ;@ ")) (first::lst));
    option = (fun _visitor continue () o ->
      match o with
      | None -> fprintf ppf "None"
      | Some v -> fprintf ppf "%a" (fun _ppf -> continue ()) v) ;
    poly_unionfind            = (fun _visitor continue () p   ->
      let lst = (UnionFind.Poly2.partitions p) in
      let aux1 l = fprintf ppf "[@,@[<hv 2> (*%a*) %a @]@,]"
                     (fun _ppf -> continue ()) (UnionFind.Poly2.repr (List.hd l) p)
                     (list_sep (fun _ppf -> continue ()) (fun ppf () -> fprintf ppf " ;@ ")) l in
      let aux2 = list_sep (fun _ppf -> aux1) (fun ppf () -> fprintf ppf " ;@ ") in
      fprintf ppf "UnionFind [@,@[<hv 2> %a @]@,]" aux2 lst);
    poly_set                  = (fun _visitor continue () set   ->
      let lst = (RedBlackTrees.PolySet.elements set) in
      fprintf ppf "Set [@,@[<hv 2> %a @]@,]" (list_sep (fun _ppf -> continue ()) (fun ppf () -> fprintf ppf " ;@ ")) lst);
    typeVariableMap           = (fun _visitor continue () tvmap   ->
      let lst = List.sort (fun (a, _) (b, _) -> Var.compare a b) (RedBlackTrees.PolyMap.bindings tvmap) in
      let aux ppf (k, v) =
        fprintf ppf "(Var %a, %a)" Var.pp k (fun _ppf -> continue ()) v in
      fprintf ppf "typeVariableMap [@,@[<hv 2> %a @]@,]" (list_sep aux (fun ppf () -> fprintf ppf " ;@ ")) lst);
  }

let print : (unit fold_config -> unit -> 'a -> unit) -> formatter -> 'a -> unit = fun fold ppf v ->
  fold (op ppf) () v

let program = print fold__program
let type_expression = print fold__type_expression
let full_environment = print fold__full_environment
