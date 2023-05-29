type 'a t = Label.t

let equal _ = Label.equal
let compare _ = Label.compare
let to_yojson _ = Label.to_yojson
let of_yojson _ = Label.of_yojson
let hash_fold_t _ = Label.hash_fold_t
let pp _ = Label.pp
let fold _ = Fun.const
let map _ = Fun.id
let t_of_sexp _ = Label.t_of_sexp
let sexp_of_t _ = Label.sexp_of_t
let iter _ = Label.iter
let fold_map _ a b = a, b
