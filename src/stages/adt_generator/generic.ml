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

module Adt_info (M : sig type ('in_state , 'out_state , 'adt_info_node_instance_info) fold_config end) = struct
  type kind =
  | Record
  | Variant
  | Poly of string

  type ('in_state , 'out_state) record_instance = {
    fields : ('in_state , 'out_state) ctor_or_field_instance list;
  }

  and ('in_state , 'out_state) constructor_instance = {
    constructor : ('in_state , 'out_state) ctor_or_field_instance ;
    variant : ctor_or_field list
  }

  and ('in_state , 'out_state) poly_instance = {
    poly : string;
    arguments : string list;
    poly_continue : 'in_state -> 'out_state
  }

  and ('in_state , 'out_state) kind_instance =
  | RecordInstance of ('in_state , 'out_state) record_instance
  | VariantInstance of ('in_state , 'out_state) constructor_instance
  | PolyInstance of ('in_state , 'out_state) poly_instance

  and ('in_state , 'out_state) instance = {
    instance_declaration_name : string;
    instance_kind : ('in_state , 'out_state) kind_instance;
  }

  and ctor_or_field =
    {
      name : string;
      is_builtin : bool;
      type_ : string;
    }

  and ('in_state , 'out_state) ctor_or_field_instance =
    {
      cf : ctor_or_field;
      cf_continue : 'in_state -> 'out_state;
      cf_new_fold : 'in_state 'out_state . ('in_state , 'out_state , (('in_state , 'out_state) node_instance_info)) M.fold_config -> 'in_state -> 'out_state;
    }

  and node =
    {
      kind : kind;
      declaration_name : string;
      ctors_or_fields : ctor_or_field list;
    }

  (* TODO: rename things a bit in this file. *)
  and adt = node list
  and ('in_state , 'out_state) node_instance_info = {
    adt           : adt ;
    node_instance : ('in_state , 'out_state) instance ;
  }
  and ('in_state , 'out_state) ctor_or_field_instance_info = adt * node * ('in_state , 'out_state) ctor_or_field_instance
end
