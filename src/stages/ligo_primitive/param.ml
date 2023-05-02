type mutable_flag =
  | Mutable
  | Immutable
[@@deriving eq, compare, yojson, hash, sexp]

type forced_flag =
  | Regular
  | Forced
[@@deriving eq, compare, yojson, hash, sexp]

type 'a t =
  { binder : 'a Binder.t
  ; mut_flag : mutable_flag
  ; forced_flag : forced_flag
  }
[@@deriving eq, compare, yojson, hash, fold, iter, sexp, map]

let pp_mutable_flag ppf mut_flag =
  match mut_flag with
  | Mutable -> Format.fprintf ppf "mut"
  | Immutable -> ()


let pp_forced_flag ppf forced_flag =
  match forced_flag with
  | Forced -> Format.fprintf ppf "forced"
  | Regular -> ()


let make ?(mut_flag = Immutable) ?(forced_flag = Regular) var ascr =
  { binder = Binder.make var ascr; mut_flag; forced_flag }


let pp g ppf { binder; mut_flag; forced_flag } =
  Format.fprintf
    ppf
    "%a%a@;%a"
    pp_mutable_flag
    mut_flag
    pp_forced_flag
    forced_flag
    (Binder.pp g)
    binder


let get_var t = Binder.get_var t.binder
let set_var t var = { t with binder = Binder.set_var t.binder var }
let get_ascr t = Binder.get_ascr t.binder
let set_ascr t ascr = { t with binder = Binder.set_ascr t.binder ascr }
let get_mut_flag t = t.mut_flag
let set_forced_flag t = { t with forced_flag = Forced }

let is_mut { mut_flag; _ } =
  match mut_flag with
  | Mutable -> true
  | Immutable -> false


let is_imm { mut_flag; _ } =
  match mut_flag with
  | Immutable -> true
  | Mutable -> false


let is_forced { forced_flag; _ } =
  match forced_flag with
  | Forced -> true
  | Regular -> false


let fold_map f init { binder; mut_flag; forced_flag } =
  let result, binder = Binder.fold_map f init binder in
  result, { binder; mut_flag; forced_flag }
