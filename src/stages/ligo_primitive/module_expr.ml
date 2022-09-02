
type 'dcl t =
  | M_struct of 'dcl list
  | M_variable of Var.ModuleVar.t
  | M_module_path of module_path
  (* FUTURE: Functor ; Apply *)
  [@@deriving eq,compare,yojson,hash]

and module_path = Var.ModuleVar.t Simple_utils.List.Ne.t

let fold : ('acc -> 'dcl -> 'acc) -> 'acc -> 'dcl t -> 'acc =
  fun f acc mexp ->
    match mexp with
    | M_variable _ -> acc
    | M_struct prg ->
      List.fold ~f ~init:acc prg
    | M_module_path _ -> acc

let map : ('dcl_src -> 'dcl_dst) -> ('dcl_src) t -> ('dcl_dst) t =
  fun f -> function
    | M_struct prg ->
      let prg = List.map ~f prg in
      M_struct prg
    | M_variable x -> M_variable x
    | M_module_path path -> M_module_path path

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
  | M_variable x -> Var.ModuleVar.pp ppf x
  | M_module_path path ->
    Simple_utils.PP_helpers.(ne_list_sep (Var.ModuleVar.pp) (tag ".")) ppf path
