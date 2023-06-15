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

(* Consing *)

let nseq_cons x (hd,tl) = x, hd::tl
let nsepseq_cons x sep (hd,tl) = x, (sep,hd)::tl

let sepseq_cons x sep = function
          None -> x, []
| Some (hd,tl) -> x, (sep,hd)::tl

(* Rightwards iterators *)

let nseq_foldl f a (hd,tl) = List.fold_left ~f ~init:a (hd::tl)

let nsepseq_foldl f a (hd,tl) =
  List.fold_left ~f:(fun a (_,e) -> f a e) ~init:(f a hd) tl

let sepseq_foldl f a = function
    None -> a
| Some s -> nsepseq_foldl f a s

let nseq_iter f (hd,tl) = List.iter ~f (hd::tl)

let nsepseq_iter f (hd,tl) = f hd; List.iter ~f:(f <@ snd) tl

let sepseq_iter f = function
    None -> ()
| Some s -> nsepseq_iter f s

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

(* Leftwards iterators *)

let nseq_foldr f (hd,tl) init = List.fold_right ~f (hd::tl) ~init

let nsepseq_foldr f (hd,tl) a = f hd (List.fold_right ~f:(f <@ snd) tl ~init:a)

let sepseq_foldr f = function
    None -> fun a -> a
| Some s -> nsepseq_foldr f s

(* Maps *)

let nseq_map f (hd,tl) = f hd, List.map ~f tl

let nsepseq_map f (hd,tl) =
  f hd, List.map ~f:(fun (sep,item) -> (sep, f item)) tl

let sepseq_map f = function
      None -> None
| Some seq -> Some (nsepseq_map f seq)

(* Conversions to lists *)

let nseq_to_list (x,y) = x::y

let nsepseq_to_list (x,y) = x :: List.map ~f:snd y

let nsepseq_of_nseq ~sep (hd,tl) =
  (hd, List.map ~f:(fun x -> (sep,x)) tl)

let sepseq_to_list = function
    None -> []
| Some s -> nsepseq_to_list s

(* Conversions to non-empty lists *)

let nsepseq_to_nseq (hd, tl) = hd, (List.map ~f:snd tl)

(* Convertions of lists *)

let list_to_nsepseq_opt (lst : 'a list) (sep : 's) : ('a, 's) nsepseq option =
  match lst with
  | [] -> None
  | hd :: tl -> Some (hd, List.map ~f:(fun e -> sep, e) tl)


let list_to_sepseq (lst : 'a list) (sep : 's) : ('a, 's) sepseq =
  match lst with
  | [] -> None
  | _ -> list_to_nsepseq_opt lst sep

(* Map and concatenate lists *)

let nseq_concat_map nseq ~f = List.concat_map (nseq_to_list nseq) ~f

let sepseq_concat_map sepseq ~f = List.concat_map (sepseq_to_list sepseq) ~f

let nsepseq_concat_map nsepseq ~f = List.concat_map (nsepseq_to_list nsepseq) ~f

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
