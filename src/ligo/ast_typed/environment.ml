open Types

type ele = type_value

type t = {
  environment: (string * ele) list ;
  type_environment: (string * ele) list ;
}
let empty : t = {
  (*  TODO: use maps *)
  environment = [] ;
  type_environment = [] ;
}

let get (e:t) (s:string) : ele option =
  List.assoc_opt s e.environment
let get_constructor (e:t) (s:string) : (ele * ele) option =
  let rec aux = function
    | [] -> None
    | (_, ({type_value'=(T_sum m)} as tv)) :: _ when SMap.mem s m -> Some (SMap.find s m, tv)
    | _ :: tl -> aux tl
  in
  aux e.environment

let add (e:t) (s:string) (tv:ele) : t =
  {e with environment = (s, tv) :: e.environment}
let get_type (e:t) (s:string) : ele option =
  List.assoc_opt s e.type_environment
let add_type (e:t) (s:string) (tv:ele) : t =
  {e with type_environment = (s, tv) :: e.type_environment}

module PP = struct
  open Format
  open PP_helpers

  let list_sep_d x = list_sep x (const " , ")

  let value ppf (e:t) =
    let pp ppf (s, e) = fprintf ppf "%s -> %a" s PP.type_value e in
    fprintf ppf "ValueEnv[%a]" (list_sep_d pp) e.environment

  let type_ ppf e =
    let pp ppf (s, e) = fprintf ppf "%s -> %a" s PP.type_value e in
    fprintf ppf "TypeEnv[%a]" (list_sep_d pp) e.type_environment

  let full ppf e =
    fprintf ppf "%a\n%a" value e type_ e
end

module Combinators = struct
  let env_sum_type ?(env = empty)
      ?(name = "a_sum_type")
      (lst : (string * ele) list) =
    add env name (Combinators.make_t_ez_sum lst)
end

