include Stage_common.Types


(* generate a new type variable and gave it an id *)
let fresh_type_variable : ?name:string -> unit -> type_variable =
    Var.fresh


(* add information on the type or the kind for operator*)
type constant_tag =
  | C_arrow     (* * -> * -> * *) (* isn't this wrong*)
  | C_option    (* * -> * *)
  | C_tuple     (* * … -> * *)
  | C_record    (* ( label , * ) … -> * *)
  | C_variant   (* ( label , * ) … -> * *)
  | C_map       (* * -> * -> * *)
  | C_big_map   (* * -> * -> * *)
  | C_list      (* * -> * *)
  | C_set       (* * -> * *)
  | C_unit      (* * *)
  | C_bool      (* * *)
  | C_string    (* * *)
  | C_nat       (* * *)
  | C_mutez       (* * *)
  | C_timestamp (* * *)
  | C_int       (* * *)
  | C_address   (* * *)
  | C_bytes     (* * *)
  | C_key_hash  (* * *)
  | C_key       (* * *)
  | C_signature (* * *)
  | C_operation (* * *)
  | C_contract  (* * -> * *)

type accessor =
  | L_int of int
  | L_string of string

(* Weird stuff; please explain *)
type type_value =
  | P_forall       of p_forall
  | P_variable     of type_variable (* how a value can be a variable? *)
  | P_constant     of (constant_tag * type_value list)
  | P_apply        of (type_value * type_value)

and p_forall = {
  binder      : type_variable ;
  constraints : type_constraint list ;
  body        : type_value
}

(* Different type of constraint *) (* why isn't this a variant ? *)
and simple_c_constructor = (constant_tag * type_variable list) (* non-empty list *)
and simple_c_constant = (constant_tag) (* for type constructors that do not take arguments *)
and c_const = (type_variable * type_value)
and c_equation = (type_value * type_value)
and c_typeclass = (type_value list * typeclass)
and c_access_label = (type_value * accessor * type_variable)

(*What i was saying just before *)
and type_constraint =
  (* | C_assignment of (type_variable * type_pattern) *)
  | C_equation of c_equation (* TVA = TVB *)
  | C_typeclass of c_typeclass (* TVL ∈ TVLs, for now in extension, later add intensional (rule-based system for inclusion in the typeclass) *)
  | C_access_label of c_access_label (* poor man's type-level computation to ensure that TV.label is type_variable *)
(* | … *)

(* is the first list in case on of the type of the type class as a kind *->*->* ? *)
and typeclass = type_value list list
