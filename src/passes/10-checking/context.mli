(* This file represente the context which give the association of values to types *)
open Simple_utils
open Ligo_prim

module Signature : sig
  type t = item list

  and item =
    | S_value of Value_var.t * Type.t
    | S_type of Type_var.t * Type.t
    | S_module of Module_var.t * t

  val get_value : t -> Value_var.t -> Type.t option
  val get_type : t -> Type_var.t -> Type.t option
  val get_module : t -> Module_var.t -> t option
  val pp : Format.formatter -> t -> unit
  val pp_item : Format.formatter -> item -> unit
end

type t
and pos
and mut_lock

and mutable_flag = Param.mutable_flag =
  | Mutable
  | Immutable

and item =
  | C_value of Value_var.t * mutable_flag * Type.t
  | C_type of Type_var.t * Type.t
  | C_type_var of Type_var.t * Kind.t
  | C_module of Module_var.t * Signature.t
  | C_texists_var of Type_var.t * Kind.t
  | C_texists_eq of Type_var.t * Kind.t * Type.t
  | C_lexists_var of Layout_var.t
  | C_lexists_eq of Layout_var.t * Type.layout
  | C_pos of pos
      (** A mutable lock is a "fitch-style lock". A lock is used to 
          "lock" mutable variables in the context from being used. 
          
          Namely, this is used to prevent functions from capturing 
          mutable variables. 
      *)
  | C_mut_lock of mut_lock

val empty : t
val add : t -> item -> t
val of_list : item list -> t
val ( |:: ) : t -> item -> t
val join : t -> t -> t
val ( |@ ) : t -> t -> t
val item_of_signature_item : Signature.item -> item
val pp : Format.formatter -> t -> unit
val add_value : t -> Value_var.t -> mutable_flag -> Type.t -> t
val add_mut : t -> Value_var.t -> Type.t -> t
val add_imm : t -> Value_var.t -> Type.t -> t
val add_type : t -> Type_var.t -> Type.t -> t
val add_type_var : t -> Type_var.t -> Kind.t -> t
val add_texists_var : t -> Type_var.t -> Kind.t -> t
val add_texists_eq : t -> Type_var.t -> Kind.t -> Type.t -> t
val add_lexists_var : t -> Layout_var.t -> t
val add_lexists_eq : t -> Layout_var.t -> Type.layout -> t
val add_module : t -> Module_var.t -> Signature.t -> t

val get_value
  :  t
  -> Value_var.t
  -> (mutable_flag * Type.t, [> `Mut_var_captured | `Not_found ]) result

val get_imm : t -> Value_var.t -> Type.t option
val get_mut : t -> Value_var.t -> Type.t option
val get_type : t -> Type_var.t -> Type.t option
val get_module : t -> Module_var.t -> Signature.t option
val get_type_vars : t -> Type_var.Set.t
val get_texists_vars : t -> Type_var.Set.t
val get_lexists_vars : t -> Layout_var.Set.t
val get_type_var : t -> Type_var.t -> Kind.t option
val get_texists_var : t -> Type_var.t -> Kind.t option
val get_texists_eq : t -> Type_var.t -> Type.t option
val get_lexists_eq : t -> Layout_var.t -> Type.layout option
val get_signature : t -> Module_var.t List.Ne.t -> Signature.t option

val get_type_or_type_var
  :  t
  -> Type_var.t
  -> [ `Type of Type.t | `Type_var of Kind.t ] option

val add_signature_item : t -> Signature.item -> t
val add_signature_items : t -> Signature.item list -> t
val insert_at : t -> at:item -> hole:t -> t
val split_at : t -> at:item -> t * t
val mark : t -> t * pos
val lock : t -> t * mut_lock

val generalize
  :  t
  -> Type.t
  -> pos:pos
  -> loc:Location.t
  -> t * Type.t * (Type_var.t * Kind.t) list * Substitution.t

type 'a apply = t -> 'a -> 'a

type _ exit =
  | Drop : t exit
  | Lift : 'a apply -> (t * 'a) exit

val drop_until : 'a -> on_exit:'a exit -> pos:pos -> 'a * Substitution.t
val unlock : 'a -> on_exit:'a exit -> lock:mut_lock -> 'a * Substitution.t
val get_record : t -> Type.row_element Record.t -> (Type_var.t option * Type.row) option
val get_sum : t -> Label.t -> (Type_var.t * Type_var.t list * Type.t * Type.t) list

module Well_formed : sig
  val context : t -> bool
  val type_ : ctx:t -> Type.t -> Kind.t option
end

module Apply : sig
  val type_ : t -> Type.t -> Type.t
  val row : t -> Type.row -> Type.row
  val row_elem : t -> Type.row_element -> Type.row_element
  val sig_item : t -> Signature.item -> Signature.item
  val sig_ : t -> Signature.t -> Signature.t
end

module Hashes : sig
  val set_context : t -> unit
  val hash_types : unit -> unit
  val find_type : Type.t -> (Module_var.t list * Type_var.t) option
end
