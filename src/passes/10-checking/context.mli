(* This file represente the context which give the association of values to types *)

open Ligo_prim
module Location = Simple_utils.Location

module Refs_tbl : sig
  module type Var = sig
    type t

    val get_location : t -> Location.t
  end

  module Label_var : Var with type t = Label.t

  type 'a tagged_tbl =
    | Tagged_tbl : (Location.t, Location.Set.t) Hashtbl.t -> 'a tagged_tbl

  val pp_tbl : 'a tagged_tbl Fmt.t

  type t =
    { value_tbl : Value_var.t tagged_tbl
    ; type_tbl : Type_var.t tagged_tbl
    ; module_tbl : Module_var.t tagged_tbl
    ; label_tbl : Label_var.t tagged_tbl
    }

  val create : unit -> t
  val clear : t -> unit
  val add_ref_value : t -> key:Value_var.t -> data:Value_var.t -> unit
  val add_ref_type : t -> key:Type_var.t -> data:Type_var.t -> unit
  val add_ref_module : t -> key:Module_var.t -> data:Module_var.t -> unit
  val add_ref_label : t -> key:Label_var.t -> data:Label_var.t -> unit
  val get_value_refs : t -> key:Location.t -> Location.Set.t option
  val get_type_refs : t -> key:Location.t -> Location.Set.t option
  val get_module_refs : t -> key:Location.t -> Location.Set.t option
  val get_label_refs : t -> key:Location.t -> Location.Set.t option
  val pp : t Fmt.t
end

module Attrs : sig
  module Value : sig
    type t =
      { entry : bool
      ; dyn_entry : bool
      ; view : bool
      ; public : bool
      ; optional : bool
      ; leading_comments : string list
      }
    [@@deriving compare, sexp, hash, equal]

    val default : t
    val of_core_attr : Ast_typed.ValueAttr.t -> t
  end

  module Type : sig
    type t =
      { public : bool
      ; leading_comments : string list
      }
    [@@deriving compare, sexp, hash, equal]

    val default : t
    val of_core_attr : Ast_typed.TypeOrModuleAttr.t -> t
  end

  module Module = Type

  module Signature : sig
    type t =
      { public : bool
      ; leading_comments : string list
      }
    [@@deriving compare, sexp, hash, equal]

    val default : t
    val of_core_attr : Ast_typed.SignatureAttr.t -> t
  end
end

module Signature : sig
  type t =
    { items : item Location.wrap list
    ; sort : sort
    }
  [@@deriving compare, sexp, hash, equal]

  and sort =
    | Ss_module
    | Ss_contract of
        { storage : Type.t
        ; parameter : Type.t
        }

  and item =
    | S_value of Value_var.t * Type.t * Attrs.Value.t
    | S_type of Type_var.t * Type.t * Attrs.Type.t
    | S_type_var of Type_var.t * Attrs.Type.t
    | S_module of Module_var.t * t * Attrs.Module.t
    | S_module_type of Module_var.t * t * Attrs.Signature.t

  val as_casteable
    :  t
    -> (Type_var.t list
       * [> `S_type of Type_var.t * Type.t * Attrs.Type.t
         | `S_value of Value_var.t * Type.t * Attrs.Value.t
         ]
         list)
       option

  val get_contract_sort : sort -> (Type.t * Type.t) option

  val get_value
    :  ?should_add_reference:bool
    -> refs_tbl:Refs_tbl.t
    -> t
    -> Value_var.t
    -> (Type.t * Attrs.Value.t) option

  val get_value_with_vvar
    :  ?should_add_reference:bool
    -> refs_tbl:Refs_tbl.t
    -> t
    -> Value_var.t
    -> (Value_var.t * Type.t * Attrs.Value.t) option

  val get_type
    :  ?should_add_reference:bool
    -> refs_tbl:Refs_tbl.t
    -> t
    -> Type_var.t
    -> Type.t option

  val get_type_with_tvar
    :  ?should_add_reference:bool
    -> refs_tbl:Refs_tbl.t
    -> t
    -> Type_var.t
    -> (Type_var.t * Type.t) option

  val get_module
    :  ?should_add_reference:bool
    -> refs_tbl:Refs_tbl.t
    -> t
    -> Module_var.t
    -> t option

  val pp : Format.formatter -> t -> unit
  val pp_item : Format.formatter -> item Location.wrap -> unit
end

type t
and pos
and mut_lock

and mutable_flag = Param.mutable_flag =
  | Mutable
  | Immutable

and item =
  | C_value of Value_var.t * mutable_flag * Type.t * Attrs.Value.t
  | C_type of Type_var.t * Type.t
  | C_type_var of Type_var.t * Kind.t
  | C_module of Module_var.t * Signature.t
  | C_module_type of Module_var.t * Signature.t
  | C_texists_var of Type_var.t * Kind.t
  | C_texists_eq of Type_var.t * Kind.t * Type.t
  | C_lexists_var of Layout_var.t * fields
  | C_lexists_eq of Layout_var.t * fields * Type.layout
  | C_pos of pos
      (** A mutable lock is a "fitch-style lock". A lock is used to
          "lock" mutable variables in the context from being used.

          Namely, this is used to prevent functions from capturing
          mutable variables.
      *)
  | C_mut_lock of mut_lock

and fields = Label.Set.t

val empty : t
val add : t -> item -> t
val of_list : item list -> t
val ( |:: ) : t -> item -> t
val join : t -> t -> t
val ( |@ ) : t -> t -> t
val item_of_signature_item : Signature.item Location.wrap -> item
val pp : Format.formatter -> t -> unit
val add_value : t -> Value_var.t -> mutable_flag -> Type.t -> Attrs.Value.t -> t
val add_mut : t -> Value_var.t -> Type.t -> t
val add_imm : t -> Value_var.t -> ?attr:Attrs.Value.t -> Type.t -> t
val add_type : t -> Type_var.t -> Type.t -> t
val add_type_var : t -> Type_var.t -> Kind.t -> t
val add_texists_var : t -> Type_var.t -> Kind.t -> t
val add_texists_eq : t -> Type_var.t -> Kind.t -> Type.t -> t
val add_lexists_var : t -> Layout_var.t -> fields -> t
val add_lexists_eq : t -> Layout_var.t -> fields -> Type.layout -> t
val add_module : t -> Module_var.t -> Signature.t -> t
val add_module_type : t -> Module_var.t -> Signature.t -> t

val get_value
  :  refs_tbl:Refs_tbl.t
  -> t
  -> Value_var.t
  -> (mutable_flag * Type.t * Attrs.Value.t, [> `Mut_var_captured | `Not_found ]) result

val get_imm : refs_tbl:Refs_tbl.t -> t -> Value_var.t -> (Type.t * Attrs.Value.t) option

val get_mut
  :  refs_tbl:Refs_tbl.t
  -> t
  -> Value_var.t
  -> (Type.t, [> `Mut_var_captured | `Not_found ]) result

val get_type : refs_tbl:Refs_tbl.t -> t -> Type_var.t -> Type.t option
val get_module : refs_tbl:Refs_tbl.t -> t -> Module_var.t -> Signature.t option
val get_type_vars : t -> Type_var.Set.t
val get_texists_vars : t -> Type_var.Set.t
val get_lexists_vars : t -> Layout_var.Set.t
val get_type_var : refs_tbl:Refs_tbl.t -> t -> Type_var.t -> Kind.t option
val get_texists_var : t -> Type_var.t -> Kind.t option
val get_texists_eq : t -> Type_var.t -> Type.t option
val get_lexists_var : t -> Layout_var.t -> fields option
val get_lexists_eq : t -> Layout_var.t -> (fields * Type.layout) option

val get_module_of_path
  :  refs_tbl:Refs_tbl.t
  -> t
  -> Module_var.t Nonempty_list.t
  -> Signature.t option

val get_module_type_of_path
  :  refs_tbl:Refs_tbl.t
  -> t
  -> Module_var.t Nonempty_list.t
  -> Signature.t option

val get_type_or_type_var
  :  refs_tbl:Refs_tbl.t
  -> t
  -> Type_var.t
  -> [ `Type of Type.t | `Type_var of Kind.t ] option

val add_signature_item : t -> Signature.item Location.wrap -> t
val add_signature_items : t -> Signature.item Location.wrap list -> t
val insert_at : t -> at:item -> hole:t -> t
val split_at : t -> at:item -> t * t
val mark : t -> t * pos
val lock : t -> t * mut_lock

val generalize
  :  t
  -> Type.t
  -> pos:pos
  -> loc:Location.t
  -> poly_name_tbl:Type.Type_var_name_tbl.t
  -> t * Type.t * (Type_var.t * Kind.t) list * Substitution.t

type 'a apply = t -> 'a -> 'a

type _ exit =
  | Drop : t exit
  | Lift : 'a apply -> (t * 'a) exit

val drop_until : 'a -> on_exit:'a exit -> pos:pos -> 'a * Substitution.t
val unlock : 'a -> on_exit:'a exit -> lock:mut_lock -> 'a * Substitution.t
val get_record : t -> Type.t Label.Map.t -> (Type_var.t option * Type.row) option

val get_sum
  :  refs_tbl:Refs_tbl.t
  -> t
  -> Label.t
  -> (Type_var.t * Type_var.t list * Type.t * Type.t) list

module Well_formed : sig
  val context : refs_tbl:Refs_tbl.t -> t -> bool
  val type_ : ctx:t -> refs_tbl:Refs_tbl.t -> Type.t -> Kind.t option
  val layout : ctx:t -> Type.layout -> bool
end

module Apply : sig
  val type_ : t -> Type.t -> Type.t
  val row : t -> Type.row -> Type.row
  val layout : t -> Type.layout -> Type.layout
  val sig_item : t -> Signature.item Location.wrap -> Signature.item Location.wrap
  val sig_ : t -> Signature.t -> Signature.t
end

module Hashes : sig
  val set_context : t -> unit
  val hash_types : unit -> unit
  val find_type : Type.t -> (Module_var.t list * Type_var.t) option
end

module Diff : sig
  val pp : Format.formatter -> t * t -> unit
end
