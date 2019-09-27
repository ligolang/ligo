  type type_variable = string

  let fresh_type_variable : ?name:string -> unit -> type_variable =
    let id = ref 0 in
    let inc () = id := !id + 1 in
    fun ?name () ->
      inc () ;
      match name with
      | None -> "type_variable_" ^ (string_of_int !id)
      | Some name -> "tv_" ^ name ^ "_" ^ (string_of_int !id)


  type constant_tag =
    | C_arrow     (* * -> * -> * *)
    | C_option    (* * -> * *)
    | C_tuple     (* * … -> * *)
    | C_record    (* ( label , * ) … -> * *)
    | C_variant   (* ( label , * ) … -> * *)
    | C_map       (* * -> * -> * *)
    | C_list      (* * -> * *)
    | C_set       (* * -> * *)
    | C_unit      (* * *)
    | C_bool      (* * *)
    | C_string    (* * *)
    | C_nat       (* * *)
    | C_tez       (* * *)
    | C_timestamp (* * *)
    | C_int       (* * *)
    | C_address   (* * *)
    | C_bytes     (* * *)
    | C_key_hash  (* * *)
    | C_key       (* * *)
    | C_signature (* * *)
    | C_operation (* * *)
    | C_contract  (* * -> * *)

  type label =
    | L_int of int
    | L_string of string

  type type_value =
    | P_forall       of (type_variable * type_constraint list * type_value)
    | P_variable     of type_variable
    | P_constant     of (constant_tag * type_value list)

  and simple_c_constructor = (constant_tag * type_variable list) (* non-empty list *)
  and simple_c_constant = (constant_tag) (* for type constructors that do not take arguments *)
  and c_const = (type_variable * type_value)
  and c_equation = (type_value * type_value)
  and c_typeclass = (type_value list * typeclass)
  and c_access_label = (type_value * label * type_variable)

  and type_constraint =
    (* | C_assignment of (type_variable * type_pattern) *)
    | C_equation of c_equation (* TVA = TVB *)
    | C_typeclass of c_typeclass (* TVL ∈ TVLs, for now in extension, later add intensional (rule-based system for inclusion in the typeclass) *)
    | C_access_label of c_access_label (* poor man's type-level computation to ensure that TV.label is type_variable *)
    (* | … *)

  and typeclass = type_value list list
