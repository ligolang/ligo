(* Utility types and functions *)

(* Identity *)

let id x = x

(* Combinators *)

let (<@) f g x = f (g x)

let swap f x y = f y x

let lambda = fun x _ -> x

let curry f x y = f (x,y)
let uncurry f (x,y) = f x y

(* Parametric rules for sequences *)

type        'a    nseq = 'a * 'a list
type ('a,'sep) nsepseq = 'a * ('sep * 'a) list
type ('a,'sep)  sepseq = ('a,'sep) nsepseq option

type ('a,'sep) nsep_or_term = [
  `Sep  of ('a,'sep) nsepseq
| `Term of ('a * 'sep) nseq
]

type ('a,'sep) sep_or_term = ('a,'sep) nsep_or_term option

type ('a,'sep) nsep_or_pref = [
  `Sep  of ('a,'sep) nsepseq
| `Pref of ('sep * 'a) nseq
]

(* Consing *)

let nseq_cons x (hd,tl) = x, hd::tl

let nsepseq_cons x sep (hd,tl) = x, (sep,hd)::tl

let sepseq_cons x sep = function
          None -> x, []
| Some (hd,tl) -> x, (sep,hd)::tl

let nsep_or_term_cons x sep = function
  `Sep  s -> `Sep (nsepseq_cons x sep s)
| `Term s -> `Term (nseq_cons (x,sep) s)

let sep_or_term_cons x sep = function
  None   -> `Term ((x,sep), [])
| Some s -> nsep_or_term_cons x sep s

let nsep_or_pref_cons x sep = function
  `Sep  s -> `Sep (nsepseq_cons x sep s)
| `Pref s -> `Pref (nseq_cons (sep,x) s)

(* Rightwards iterators *)

let nseq_foldl f a (hd,tl) = List.fold_left ~f ~init:a (hd::tl)

let nsepseq_foldl f a (hd,tl) =
  List.fold_left ~f:(fun a (_,e) -> f a e) ~init:(f a hd) tl

let sepseq_foldl f i = function
    None -> i
| Some s -> nsepseq_foldl f i s

let nsep_or_term_foldl f i = function
  `Sep  s -> nsepseq_foldl f i s
| `Term s -> nseq_foldl (fun a (x,_) -> f a x) i s

let sep_or_term_foldl f i = function
  None -> i
| Some s -> nsep_or_term_foldl f i s

let nsep_or_pref_foldl f i = function
  `Sep  s -> nsepseq_foldl f i s
| `Pref s -> nseq_foldl (fun a (_,x) -> f a x) i s


let nseq_iter f (hd,tl) = List.iter ~f (hd::tl)

let nsepseq_iter f (hd,tl) = f hd; List.iter ~f:(f <@ snd) tl

let sepseq_iter f = function
    None -> ()
| Some s -> nsepseq_iter f s

let nsep_or_term_iter f = function
  `Sep  s -> nsepseq_iter f s
| `Term s -> nseq_iter (fun (x,_) -> f x) s

let sep_or_term_iter f = function
  None -> ()
| Some s -> nsep_or_term_iter f s

let nsep_or_pref_iter f = function
  `Sep  s -> nsepseq_iter f s
| `Pref s -> nseq_iter (fun (_,x) -> f x) s

(* Reversing *)

let nseq_rev (hd,tl) =
  let rec aux acc = function
      [] -> acc
  | x::l -> aux (nseq_cons x acc) l
in aux (hd,[]) tl

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
| `Term s -> `Term (nseq_rev s)

let sep_or_term_rev = function
  None   -> None
| Some s -> Some (nsep_or_term_rev s)

let nsep_or_pref_rev = function
  `Sep  s -> `Sep (nsepseq_rev s)
| `Pref s -> `Pref (nseq_rev s)

(* Leftwards iterators *)

let nseq_foldr f (hd,tl) init = List.fold_right ~f (hd::tl) ~init

let nsepseq_foldr f (hd,tl) a = f hd (List.fold_right ~f:(f <@ snd) tl ~init:a)

let sepseq_foldr f = function
    None -> fun a -> a
| Some s -> nsepseq_foldr f s

let nsep_or_term_foldr f = function
  `Sep  s -> nsepseq_foldr f s
| `Term s -> nseq_foldr (fun (x,_) -> f x) s

let sep_or_term_foldr f = function
  None   -> fun i -> i
| Some s -> nsep_or_term_foldr f s

let nsep_or_pref_foldr f = function
  `Sep  s -> nsepseq_foldr f s
| `Pref s -> nseq_foldr (fun (_,x) -> f x) s

(* Maps *)

let nseq_map f (hd,tl) = f hd, List.map ~f tl

let nsepseq_map f (hd,tl) =
  f hd, List.map ~f:(fun (sep,item) -> (sep, f item)) tl

let sepseq_map f = function
      None -> None
| Some seq -> Some (nsepseq_map f seq)

let nsep_or_term_map f = function
  `Sep  s -> `Sep (nsepseq_map f s)
| `Term s -> `Term (nseq_map (fun (x, term) -> (f x, term)) s)

let sep_or_term_map f = function
  None   -> None
| Some s -> Some (nsep_or_term_map f s)

let nsep_or_pref_map f = function
  `Sep  s -> `Sep (nsepseq_map f s)
| `Pref s -> `Pref (nseq_map (fun (pref, x) -> (pref, f x)) s)

(* Conversions to lists *)

let nseq_to_list (x,y) = x::y

let nsepseq_to_list (x,y) = x :: List.map ~f:snd y

let nsepseq_of_nseq ~sep (hd,tl) =
  (hd, List.map ~f:(fun x -> (sep,x)) tl)

let sepseq_to_list = function
    None -> []
| Some s -> nsepseq_to_list s

let nsepseq_to_nseq (hd, tl) = hd, (List.map ~f:snd tl)

let nsep_or_term_to_list = function
  `Sep  s -> nsepseq_to_list s
| `Term s -> List.map ~f:fst (nseq_to_list s)

let sep_or_term_to_list = function
  None -> []
| Some seq -> nsep_or_term_to_list seq

let sep_or_term_of_list ~sep ~sep_or_term lst : ('a,'sep) sep_or_term =
  match lst with
    [] -> None
  | lst ->
    Some (match sep_or_term with
    | `Sep -> `Sep (nsepseq_of_nseq ~sep (X_list.Ne.of_list lst))
    | `Term -> `Term ( X_list.Ne.of_list (List.map ~f:(fun x -> (x,sep)) lst)))

let nsep_or_pref_to_list = function
  `Sep s -> nsepseq_to_list s
| `Pref (hd,tl) -> List.map ~f:snd (hd::tl)

(* Convertions of lists *)

let list_to_sepseq (lst : 'a list) (sep : 's) : ('a, 's) sepseq =
  match lst with
    [] -> None
  | hd :: tl -> Some (hd, List.map ~f:(fun e -> sep, e) tl)

(* Map and concatenate lists *)

let    nseq_concat_map s ~f = List.concat_map (nseq_to_list    s) ~f
let  sepseq_concat_map s ~f = List.concat_map (sepseq_to_list  s) ~f
let nsepseq_concat_map s ~f = List.concat_map (nsepseq_to_list s) ~f

(* Modules based on [String], like sets and maps. *)

module String =
  struct
    include String

    module Ord =
      struct
        type nonrec t = t
        let compare = compare
      end

    module Map = Caml.Map.Make (Ord)
    module Set = Caml.Set.Make (Ord)
  end

(* Integers *)

module Int =
  struct
    type t = int

    module Ord =
      struct
        type nonrec t = t
        let compare = compare
      end

    module Map = Caml.Map.Make (Ord)
    module Set = Caml.Set.Make (Ord)
  end

(* Effectful symbol generator *)

let gen_sym =
  let counter = ref 0 in
  fun () -> incr counter; "v" ^ string_of_int !counter

(* General tracing function *)

let trace text = function
       None -> ()
| Some chan -> Core.(output_string chan text; flush chan)

(* Printing a string in red to standard error *)

let highlight msg = Printf.eprintf "\027[31m%s\027[0m%!" msg

(* When failing to parse a specifed JSON format *)

let error_yojson_format format =
  Error ("Invalid JSON value.
          An object with the following specification is expected:"
         ^ format)

(* Optional let *)

let (let*) o f =
  match o with
    None -> None
  | Some x -> f x

let return x = Some x
