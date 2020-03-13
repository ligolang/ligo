module Adt_info = struct
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
      cf_continue : 'state -> 'state
    }

  type node =
    {
      kind : kind;
      declaration_name : string;
      ctors_or_fields : ctor_or_field list;
    }

  (* TODO: rename things a bit in this file. *)
  type adt = node list
  type 'state node_instance_info = {
    adt           : adt ;
    node_instance : 'state instance ;
  }
  type 'state ctor_or_field_instance_info = adt * node * 'state ctor_or_field_instance
end
