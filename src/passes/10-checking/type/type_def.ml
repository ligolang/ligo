module Location = Simple_utils.Location
open Ligo_prim

module Layout = struct
  type t =
    | L_concrete of Layout.t
    | L_exists of Layout_var.t
  [@@deriving yojson, equal, sexp, compare, hash]

  type field = Layout.field =
    { name : Label.t
    ; annot : string option
    }

  let fields = function
    | L_concrete layout -> Some (Layout.fields layout)
    | L_exists _lvar -> None


  let default fields = L_concrete (Layout.default fields)

  let pp ppf t =
    match t with
    | L_concrete layout -> Layout.pp ppf layout
    | L_exists lvar -> Format.fprintf ppf "^%a" Layout_var.pp lvar
end

module Row = Row.Make (Layout)

type t =
  { content : content
  ; abbrev : (abbrev option[@equal.ignore] [@compare.ignore] [@hash.ignore])
  ; location : (Location.t[@equal.ignore] [@compare.ignore] [@hash.ignore] [@sexp.opaque])
  }

and abbrev =
  { orig_var : Module_var.t list * Type_var.t
  ; applied_types : t list
        (** [orig_var] doesn't remember type parameters,
            so to be able using the [orig_var] for parametric types we store the params here. *)
  }

and content =
  | T_variable of Type_var.t
  | T_exists of Type_var.t
  | T_construct of construct
  | T_sum of row * (Label.t option[@equal.ignore] [@hash.ignore] [@compare.ignore])
  (* This [Label.t] represent an original name of field in disc union type *)
  | T_record of row
  | T_arrow of t Arrow.t
  | T_singleton of Literal_value.t
  | T_abstraction of t Abstraction.t
  | T_for_all of t Abstraction.t

and row = t Row.t
and layout = Layout.t

and construct =
  { language : string
  ; constructor : Literal_types.t
  ; parameters : t list
  }
[@@deriving yojson, equal, sexp, compare, hash]
