[@@@warning "-30"]
module Adt_info = struct
  type kind =
  | Record
  | Variant
  | Poly of string

  type 'state record_instance = {
    name : string;
    fields : 'state ctor_or_field_continue list;
  }
  and 'state constructor_instance = {
    name : string;
    constructor : 'state ctor_or_field_continue ;
    variant : ctor_or_field list
  }
  and 'state poly_instance = {
    name : string;
    type_ : string;
    arguments : string list;
    continue : 'state -> 'state
  }
  and 'state instance =
  | Record of 'state record_instance
  | Variant of 'state constructor_instance
  | Poly of 'state poly_instance

  and ctor_or_field =
    {
      name : string;
      isBuiltin : bool;
      type_ : string;
    }

  and 'state ctor_or_field_continue =
    {
      name : string;
      isBuiltin : bool;
      type_ : string;
      continue : 'state -> 'state
    }

  type node =
    {
      kind : kind;
      name : string;
      ctors_or_fields : ctor_or_field list;
    }

  (* TODO: rename things a bit in this file. *)
  type adt = node list
  type 'state node_info = unit -> adt * 'state instance
  type 'state ctor_or_field_info = unit -> adt * node * 'state ctor_or_field_continue
end
