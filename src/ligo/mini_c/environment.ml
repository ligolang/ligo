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
  let get_opt : string -> t -> type_value option  = List.assoc_opt
  let has : string -> t -> bool = fun s t ->
    match get_opt s t with
    | None -> false
    | Some _ -> true
  let get_i : string -> t -> (type_value * int) = List.assoc_i
  let of_list : element list -> t = fun x -> x
  let to_list : t -> element list = fun x -> x
  let get_names : t -> string list = List.map fst


  let fold : _ -> 'a -> t -> 'a = List.fold_left
  let filter : _ -> t -> t = List.filter

  let closure_representation : t -> type_value = fun t ->
    match t with
    | [] -> T_base Base_unit
    | [ a ] -> snd a
    | hd :: tl -> List.fold_left (fun acc cur -> T_pair (acc , snd cur)) (snd hd) tl
end

include Environment
