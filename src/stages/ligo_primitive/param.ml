type mutable_flag = 
  | Mutable
  | Immutable
[@@deriving eq, compare, yojson, hash]

type 'a t = 
  { binder : 'a Binder.t
  ; mut_flag : mutable_flag 
  }
[@@deriving eq, compare, yojson, hash, fold, map]

let pp_mutable_flag ppf mut_flag = 
  match mut_flag with
  | Mutable -> Format.fprintf ppf "mut"
  | Immutable -> ()

let make ?(mut_flag = Immutable) var ascr = 
  { binder = Binder.make var ascr; mut_flag }

let pp g ppf { binder; mut_flag } = 
  Format.fprintf ppf "%a@;%a" pp_mutable_flag mut_flag (Binder.pp g) binder

let get_var t = Binder.get_var t.binder
let set_var t var = { t with binder = Binder.set_var t.binder var }
let get_ascr t = Binder.get_ascr t.binder
let set_ascr t ascr = { t with binder = Binder.set_ascr t.binder ascr }

let get_mut_flag t = t.mut_flag

let fold_map f init { binder; mut_flag } = 
  let result, binder = Binder.fold_map f init binder in
  result, { binder; mut_flag }

let to_binder { binder; _ } = binder

