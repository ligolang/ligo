
type 'dcl t =
  | M_struct of 'dcl list
  | M_variable of Var.Module_var.t
  | M_module_path of module_path
  (* FUTURE: Functor ; Apply *)
  [@@deriving eq,compare,yojson,hash,fold,map]

and module_path = Var.Module_var.t Simple_utils.List.Ne.t

let fold_map : ('acc -> 'a -> 'acc * 'b) -> 'acc -> ('a) t -> 'acc * ('b) t =
  fun f acc mexp ->
    match mexp with
    | M_struct prg ->
      let (x,prg) = List.fold_map ~f ~init:acc prg in
      (x, (M_struct prg))
    | M_variable _ | M_module_path _ -> (acc,mexp)

let pp h ppf = function
  | M_struct p ->
    Format.fprintf ppf "@[<v>struct@,%a@,end@]"
      Simple_utils.PP_helpers.(list_sep (h) (tag "@,")) p
  | M_variable x -> Var.Module_var.pp ppf x
  | M_module_path path ->
    Simple_utils.PP_helpers.(ne_list_sep (Var.Module_var.pp) (tag ".")) ppf path
