module BlahBluh = struct
module StringMap = Map.Make(String);;
(* generic folds for nodes *)
type 'state generic_continue_fold_node = {
  continue                 : 'state -> 'state ;
  (* generic folds for each field *)
  continue_ctors_or_fields : ('state -> 'state) StringMap.t ;
};;
(* map from node names to their generic folds *)
type 'state generic_continue_fold = ('state generic_continue_fold_node) StringMap.t;;
end

module Adt_info (M : sig type ('state , 'adt_info_node_instance_info) fold_config end) = struct
  type kind =
  | Record
  | Variant
  | Poly of string

  type 'state record_instance = {
    fields : 'state ctor_or_field_instance list;
  }

  and 'state constructor_instance = {
    constructor : 'state ctor_or_field_instance ;
    variant : ctor_or_field list
  }

  and 'state poly_instance = {
    poly : string;
    arguments : string list;
    poly_continue : 'state -> 'state
  }

  and 'state kind_instance =
  | RecordInstance of 'state record_instance
  | VariantInstance of 'state constructor_instance
  | PolyInstance of 'state poly_instance

  and 'state instance = {
    instance_declaration_name : string;
    instance_kind : 'state kind_instance;
  }

  and ctor_or_field =
    {
      name : string;
      is_builtin : bool;
      type_ : string;
    }

  and 'state ctor_or_field_instance =
    {
      cf : ctor_or_field;
      cf_continue : 'state -> 'state;
      cf_new_fold : 'state . ('state, ('state node_instance_info)) M.fold_config -> 'state -> 'state;
    }

  and node =
    {
      kind : kind;
      declaration_name : string;
      ctors_or_fields : ctor_or_field list;
    }

  (* TODO: rename things a bit in this file. *)
  and adt = node list
  and 'state node_instance_info = {
    adt           : adt ;
    node_instance : 'state instance ;
  }
  and 'state ctor_or_field_instance_info = adt * node * 'state ctor_or_field_instance
end
