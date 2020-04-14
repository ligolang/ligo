open Types
open Fold
open Format

let print_program : formatter -> program -> unit = fun ppf p ->
  ignore ppf ;
  let assert_nostate _ = () in (* (needs_parens, state) = assert (not needs_parens && match state with None -> true | Some _ -> false) in *)
  let nostate = false, "" in
  let op = {
      generic = (fun state info ->
        assert_nostate state;
        match info.node_instance.instance_kind with
        | RecordInstance { fields } ->
           false, "{ " ^ String.concat " ; " (List.map (fun (fld : 'x Adt_info.ctor_or_field_instance) -> fld.cf.name ^ " = " ^ snd (fld.cf_continue nostate)) fields) ^ " }"
        | VariantInstance { constructor={ cf = { name; is_builtin=_; type_=_ }; cf_continue }; variant=_ } ->
           (match cf_continue nostate with
            | true,  arg -> true, name ^ " (" ^ arg ^ ")"
            | false, arg -> true, name ^ " "  ^ arg)
        | PolyInstance { poly=_; arguments=_; poly_continue } ->
           (poly_continue nostate)
      );
      type_variable =  (fun _visitor state type_meta -> assert_nostate state; false , (ignore type_meta;"TODO:TYPE_META")) ;
      type_meta = (fun _visitor state type_meta -> assert_nostate state; false , (ignore type_meta;"TODO:TYPE_META")) ;
      bool = (fun _visitor state b -> assert_nostate state; false , if b then "true" else "false") ;
      int = (fun _visitor state i -> assert_nostate state; false , string_of_int i) ;
      string = (fun _visitor state str -> assert_nostate state; false , "\"" ^ str ^ "\"") ;
      bytes = (fun _visitor state bytes -> assert_nostate state; false , (ignore bytes;"TODO:BYTES")) ;
      packed_internal_operation =  (fun _visitor state op -> assert_nostate state; false , (ignore op;"TODO:PACKED_INTERNAL_OPERATION")) ;
      expression_variable =  (fun _visitor state ev -> assert_nostate state; false , (ignore ev;"TODO:EXPRESSION_VARIABLE")) ;
      constructor' =  (fun _visitor state c -> assert_nostate state; false , (ignore c;"TODO:CONSTRUCTOR'")) ;
      location =  (fun _visitor state loc -> assert_nostate state; false , (ignore loc;"TODO:LOCATION'")) ;
      label = (fun _visitor state (Label lbl) -> assert_nostate state; true, "Label " ^ lbl) ;
      constructor_map =  (fun _visitor continue state cmap -> assert_nostate state; false , (ignore (continue,cmap);"TODO:constructor_map")) ;
      label_map =  (fun _visitor continue state lmap -> assert_nostate state; false , (ignore (continue,lmap);"TODO:label_map")) ;
      list = (fun _visitor continue state lst ->
        assert_nostate state;
        false , "[ " ^ String.concat " ; " (List.map snd @@ List.map (continue nostate) lst) ^ " ]") ;
      location_wrap =  (fun _visitor continue state lwrap -> assert_nostate state; false , (ignore (continue,lwrap);"TODO:location_wrap")) ;
      list_ne =  (fun _visitor continue state list_ne -> assert_nostate state; false , (ignore (continue,list_ne);"TODO:location_wrap")) ;
    } in
  let (_ , state) = fold__program op nostate p in
  Printf.printf "%s" state
