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
  open Ligo_prim
  module List = Simple_utils.List

  type element = environment_element
  type t = environment

  let empty : t = []
  let add : element -> t -> t = List.cons
  let concat : t list -> t = List.concat

  let get_opt : Value_var.t -> t -> type_expression option =
   fun e lst -> List.Assoc.find ~equal:Value_var.equal lst e


  let has : Value_var.t -> t -> bool =
   fun s t ->
    match get_opt s t with
    | None -> false
    | Some _ -> true


  let get_i_opt : Value_var.t -> t -> (type_expression * int) option =
   fun x lst ->
    List.find_mapi
      ~f:(fun i (e, t) -> if Value_var.equal e x then Some (t, i) else None)
      lst


  let of_list : element list -> t = fun x -> x
  let to_list : t -> element list = fun x -> x
  let get_names : t -> Value_var.t list = List.map ~f:fst
  let remove : int -> t -> t = List.remove

  let select ?(rev = false) ?(keep = true) : Value_var.t list -> t -> t =
   fun lst env ->
    let e_lst =
      let e_lst = to_list env in
      let aux selector (s, _) =
        match List.mem ~equal:Value_var.equal selector s with
        | true -> List.remove_element ~compare:Value_var.compare s selector, keep
        | false -> selector, not keep
      in
      let e_lst' =
        if Bool.equal rev keep
        then snd @@ List.fold_map ~f:aux ~init:lst e_lst
        else snd @@ List.fold_map ~f:aux ~init:lst @@ List.rev e_lst
      in
      let e_lst'' = List.zip_exn e_lst e_lst' in
      e_lst''
    in
    of_list @@ List.map ~f:fst @@ List.filter ~f:snd @@ e_lst


  let fold : _ -> 'a -> t -> 'a = fun f init -> List.fold_left ~f ~init
  let filter : _ -> t -> t = fun f -> List.filter ~f
end

include Environment
