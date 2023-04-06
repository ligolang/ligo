module type Row_lhs = sig
  type t [@@deriving eq, compare, yojson, hash, iter, sexp, compare, hash]

  val of_string : string -> t
  val to_string : t -> string
end

module Make (Row_lhs : Row_lhs) = struct
  type 'ty row_element =
    { associated_type : 'ty
    ; attributes : Attribute.t list
    ; decl_pos : int
    }
  [@@deriving yojson, map, iter, fold, sexp, eq, compare, hash]

  type 'ty row = Row_lhs.t * 'ty row_element
  [@@deriving yojson, map, fold, iter, sexp, eq, compare, hash]

  type 'ty t = 'ty row list [@@deriving yojson, map, fold, iter, sexp, eq, compare, hash]

  let make : type ty. (Row_lhs.t * ty * Attribute.t list) list -> ty t =
   fun lst ->
    let make_row : int -> Row_lhs.t * ty * Attribute.t list -> ty row =
     fun i (label, associated_type, attributes) ->
      let rows = { decl_pos = i; associated_type; attributes } in
      label, rows
    in
    List.mapi ~f:make_row lst


  let get_tys : type ty. ty t -> ty list =
   fun lst -> List.map lst ~f:(fun (_, { associated_type = ty; _ }) -> ty)


  let find_ty : type ty. ty t -> Row_lhs.t -> ty option =
   fun lst lhs ->
    List.find_map lst ~f:(fun (lhs', { associated_type = ty; _ }) ->
        if Row_lhs.equal lhs lhs' then Some ty else None)
end
