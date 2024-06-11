(* Utility types and functions *)

open Core

(* Combinators *)

let (<@) = Ligo_fun.(<@)

(* Parametric rules for sequences *)

type ('a,'sep) nsepseq = 'a * ('sep * 'a) list
type ('a,'sep)  sepseq = ('a,'sep) nsepseq option

type ('a,'sep) nsep_or_term = [
  `Sep  of ('a,'sep) nsepseq
| `Term of ('a * 'sep) Nonempty_list.t
]

type ('a,'sep) sep_or_term = ('a,'sep) nsep_or_term option

type ('a,'sep) nsep_or_pref = [
  `Sep  of ('a,'sep) nsepseq
| `Pref of ('sep * 'a) Nonempty_list.t
]

(* Consing *)

let nsepseq_cons x sep (hd,tl) = x, (sep,hd)::tl

let sepseq_cons x sep = function
          None -> x, []
| Some (hd,tl) -> x, (sep,hd)::tl

let nsep_or_term_cons x sep = function
  `Sep  s -> `Sep (nsepseq_cons x sep s)
| `Term s -> `Term (Nonempty_list.cons (x,sep) s)

let sep_or_term_cons x sep = function
  None   -> `Term (Nonempty_list.singleton (x,sep))
| Some s -> nsep_or_term_cons x sep s

let nsep_or_pref_cons x sep = function
  `Sep  s -> `Sep (nsepseq_cons x sep s)
| `Pref s -> `Pref (Nonempty_list.cons (sep,x) s)

(* Rightwards iterators *)

let nsepseq_foldl f a (hd,tl) =
  List.fold_left ~f:(fun a (_,e) -> f a e) ~init:(f a hd) tl

let sepseq_foldl f i = function
    None -> i
| Some s -> nsepseq_foldl f i s

let nsep_or_term_foldl f init = function
  `Sep  s -> nsepseq_foldl f init s
| `Term s -> Nonempty_list.fold ~f:(fun a -> f a <@ fst) ~init s

let sep_or_term_foldl f i = function
  None   -> i
| Some s -> nsep_or_term_foldl f i s

let nsep_or_pref_foldl f init = function
  `Sep  s -> nsepseq_foldl f init s
| `Pref s -> Nonempty_list.fold ~f:(fun a -> f a <@ snd) ~init s

let nsepseq_iter f (hd,tl) = f hd; List.iter ~f:(f <@ snd) tl

let sepseq_iter f = function
    None -> ()
| Some s -> nsepseq_iter f s

let nsep_or_term_iter f = function
  `Sep  s -> nsepseq_iter f s
| `Term s -> Nonempty_list.iter ~f:(f <@ fst) s

let sep_or_term_iter f = function
  None   -> ()
| Some s -> nsep_or_term_iter f s

let nsep_or_pref_iter f = function
  `Sep  s -> nsepseq_iter f s
| `Pref s -> Nonempty_list.iter ~f:(f <@ snd) s

(* Reversing *)

let nsepseq_rev =
  let rec aux acc = function
    hd, (sep,snd)::tl -> aux ((sep,hd)::acc) (snd,tl)
  | hd,            [] -> hd, acc in
function
  hd, (sep,snd)::tl -> aux [sep,hd] (snd,tl)
|                 s -> s

let sepseq_rev = function
      None -> None
| Some seq -> Some (nsepseq_rev seq)

let nsep_or_term_rev = function
  `Sep  s -> `Sep (nsepseq_rev s)
| `Term s -> `Term (Nonempty_list.reverse s)

let sep_or_term_rev = function
  None   -> None
| Some s -> Some (nsep_or_term_rev s)

let nsep_or_pref_rev = function
  `Sep  s -> `Sep (nsepseq_rev s)
| `Pref s -> `Pref (Nonempty_list.reverse s)

(* Leftwards iterators *)

let nsepseq_foldr f (hd,tl) a = f hd (List.fold_right ~f:(f <@ snd) tl ~init:a)

let sepseq_foldr f = function
    None -> fun a -> a
| Some s -> nsepseq_foldr f s

let nsep_or_term_foldr f = function
  `Sep  s -> nsepseq_foldr f s
| `Term s -> fun init -> Nonempty_list.fold_right ~f:(f <@ fst) s ~init

let sep_or_term_foldr f = function
  None   -> fun i -> i
| Some s -> nsep_or_term_foldr f s

let nsep_or_pref_foldr f = function
  `Sep  s -> nsepseq_foldr f s
| `Pref s -> fun init -> Nonempty_list.fold_right ~f:(f <@ snd) s ~init

(* Maps *)

let nsepseq_map f (hd,tl) =
  f hd, List.map ~f:(fun (sep,item) -> (sep, f item)) tl

let sepseq_map f = function
      None -> None
| Some seq -> Some (nsepseq_map f seq)

let nsep_or_term_map f = function
  `Sep  s -> `Sep (nsepseq_map f s)
| `Term s -> `Term (Nonempty_list.map ~f:(fun (x, term) -> (f x, term)) s)

let sep_or_term_map f = function
  None   -> None
| Some s -> Some (nsep_or_term_map f s)

let nsep_or_pref_map f = function
  `Sep  s -> `Sep (nsepseq_map f s)
| `Pref s -> `Pref (Nonempty_list.map ~f:(fun (pref, x) -> (pref, f x)) s)

(* Conversions to non-empty lists *)

let nsepseq_to_ne_list (x, l) = Nonempty_list.(x :: List.map ~f:snd l)

let nsep_or_term_to_ne_list = function
  `Sep  s -> nsepseq_to_ne_list s
| `Term s -> Nonempty_list.map ~f:fst s

let nsep_or_pref_to_ne_list = function
  `Sep  s -> nsepseq_to_ne_list s
| `Pref s -> Nonempty_list.map ~f:snd s

(* Conversions to lists *)

let nsepseq_to_list (x,y) = x :: List.map ~f:snd y

let nsepseq_of_ne_list ~sep Nonempty_list.(hd::tl) =
  (hd, List.map ~f:(fun x -> (sep,x)) tl)

let sepseq_to_list = function
    None -> []
| Some s -> nsepseq_to_list s

let nsep_or_term_to_list = function
  `Sep  s -> nsepseq_to_list s
| `Term s -> List.map ~f:fst (Nonempty_list.to_list s)

let sep_or_term_to_list = function
  None     -> []
| Some seq -> nsep_or_term_to_list seq

let nsep_or_pref_to_list = function
  `Sep s           -> nsepseq_to_list s
| `Pref Nonempty_list.(x::l) -> List.map ~f:snd (x::l)

(* Conversions from lists *)

let list_to_sepseq (lst : 'a list) (sep : 's) : ('a, 's) sepseq =
  match lst with
    [] -> None
  | hd :: tl -> Some (hd, List.map ~f:(fun e -> sep, e) tl)

let sep_or_term_of_list ~sep ~sep_or_term lst : ('a,'sep) sep_or_term =
  match lst with
    [] -> None
  | x :: l ->
    match sep_or_term with
    | `Sep -> Some (`Sep (nsepseq_of_ne_list ~sep Nonempty_list.(x::l)))
    | `Term -> Some (`Term Nonempty_list.((x,sep) :: List.map ~f:(fun x -> (x,sep)) l))

(* Conversions to JSON *)

type json = Yojson.Safe.t

let yojson_of_nsepseq f _ (s : ('a, 'sep) nsepseq) =
  `List (nsepseq_to_list @@ nsepseq_map f s)

let yojson_of_sepseq f _ (s : ('a, 'sep) sepseq) =
  `List (sepseq_to_list @@ sepseq_map f s)

let yojson_of_sep_or_term f _ (s : ('a, 'sep) sep_or_term) =
  `List (sep_or_term_to_list @@ sep_or_term_map f s)

let yojson_of_nsep_or_term f _ (s : ('a, 'sep) nsep_or_term) =
  `List (nsep_or_term_to_list @@ nsep_or_term_map f s)

let yojson_of_nsep_or_pref f _ (s : ('a, 'sep) nsep_or_pref) =
  `List (nsep_or_pref_to_list @@ nsep_or_pref_map f s)

(* Map and concatenate lists *)

let  sepseq_concat_map s ~f = List.concat_map (sepseq_to_list  s) ~f
let nsepseq_concat_map s ~f = List.concat_map (nsepseq_to_list s) ~f
