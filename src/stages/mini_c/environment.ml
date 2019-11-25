(* open Trace *)
open Types

(* module type ENVIRONMENT = sig
 *   type element = environment_element
 *   type t = environment
 *
 *   val empty : t
 *   val add : element -> t -> t
 *   val concat : t list -> t
 *   val get_opt : string -> t -> type_value option
 *   val get_i : string -> t -> (type_value * int)
 *   val of_list : element list -> t
 *   val closure_representation : t -> type_value
 * end *)

module Environment (* : ENVIRONMENT *) = struct
  type element = environment_element
  type t = environment

  let empty : t = []
  let add : element -> t -> t  = List.cons
  let concat : t list -> t  = List.concat
  let get_opt : Var.t -> t -> type_value option  = List.assoc_opt ~compare:Var.compare
  let has : Var.t -> t -> bool = fun s t ->
    match get_opt s t with
    | None -> false
    | Some _ -> true
  let get_i : Var.t -> t -> (type_value * int) = List.assoc_i ~compare:Var.compare
  let of_list : element list -> t = fun x -> x
  let to_list : t -> element list = fun x -> x
  let get_names : t -> Var.t list = List.map fst
  let remove : int -> t -> t = List.remove

  let select ?(rev = false) ?(keep = true) : Var.t list -> t -> t = fun lst env ->
    let e_lst =
      let e_lst = to_list env in
      let aux selector (s , _) =
        match List.mem ~compare:Var.compare s selector with
        | true -> List.remove_element ~compare:Var.compare s selector , keep
        | false -> selector , not keep in
      let e_lst' =
        if rev = keep
        then List.fold_map aux lst e_lst
        else List.fold_map_right aux lst e_lst
      in
      let e_lst'' = List.combine e_lst e_lst' in
      e_lst'' in
    of_list
    @@ List.map fst
    @@ List.filter snd
    @@ e_lst


  let fold : _ -> 'a -> t -> 'a = List.fold_left
  let filter : _ -> t -> t = List.filter
end

include Environment
