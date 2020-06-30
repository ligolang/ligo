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

module Adt_info (M : sig type ('in_state , 'out_state , 'adt_info_node_instance_info) fold_config;; type whatever;; type make_poly;; end) = struct
  type kind =
  | RecordType of record_type
  | VariantType of variant_type
  | PolyType of poly_type

  and ctor_or_field =
    {
      name : string;
      is_builtin : bool;
      type_ : string;
    }

  and record_type = {
    fields : ctor_or_field list;
    make_record : (string , M.whatever) RedBlackTrees.PolyMap.t -> M.whatever option
  }

  and ('in_state , 'out_state) record_instance = {
    field_instances : ('in_state , 'out_state) ctor_or_field_instance list;
  }

  and variant_type = {
    constructors : constructor_type list;
  }

  and constructor_type = {
    ctor : ctor_or_field;
    make_ctor : M.whatever -> M.whatever option;
  }

  and ('in_state , 'out_state) constructor_instance = {
    constructor : ('in_state , 'out_state) ctor_or_field_instance ;
    variant : ctor_or_field list
  }

  and poly_type = {
    poly_name : string;
    make_poly : M.make_poly;
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
    }

  (* TODO: rename things a bit in this file. *)
  and adt = (string, node) RedBlackTrees.PolyMap.t
  and ('in_state , 'out_state) node_instance_info = {
    adt           : adt ;
    node_instance : ('in_state , 'out_state) instance ;
  }
  and ('in_state , 'out_state) ctor_or_field_instance_info = adt * node * ('in_state , 'out_state) ctor_or_field_instance
end
