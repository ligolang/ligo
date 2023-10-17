open Types

module Environment = struct
  open Ligo_prim
  module List = Simple_utils.List

  type element = environment_element
  type t = environment

  let add : element -> t -> t = List.cons

  let get_i_opt : Value_var.t -> t -> (type_expression * int) option =
   fun x lst ->
    List.find_mapi
      ~f:(fun i (e, t) -> if Value_var.equal e x then Some (t, i) else None)
      lst


  let get_var_opt : Value_var.t -> t -> Value_var.t option =
   fun x lst ->
    List.find_map ~f:(fun (e, _) -> if Value_var.equal e x then Some e else None) lst
end

include Environment
