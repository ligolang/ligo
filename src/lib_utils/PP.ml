open Format
let string : formatter -> string -> unit = fun ppf s -> fprintf ppf "%s" s
let tag tag : formatter -> unit -> unit = fun ppf () -> fprintf ppf tag
let new_line : formatter -> unit -> unit = tag "@;"
let rec new_lines n ppf () =
  match n with
  | 0 -> new_line ppf ()
  | n -> new_line ppf () ; new_lines (n-1) ppf ()
let const const : formatter -> unit -> unit = fun ppf () -> fprintf ppf "%s" const
let comment : formatter -> string -> unit = fun ppf s -> fprintf ppf "(* %s *)" s
let list_sep value separator = pp_print_list ~pp_sep:separator value
let list value = pp_print_list ~pp_sep:(tag "") value
let ne_list_sep value separator ppf (hd, tl) =
  value ppf hd ;
  separator ppf () ;
  pp_print_list ~pp_sep:separator value ppf tl

let prepend s f ppf a =
  fprintf ppf "%s%a" s f a

let option = fun f ppf opt ->
  match opt with
  | Some x -> fprintf ppf "Some(%a)" f x
  | None -> fprintf ppf "None"

let map = fun f pp ppf x ->
  pp ppf (f x)

let pair_sep value sep ppf (a, b) = fprintf ppf "%a %s %a" value a sep value b
let smap_sep value sep ppf m =
  let module SMap = X_map.String in
  let aux k v prev = (k, v) :: prev in
  let new_pp ppf (k, v) = fprintf ppf "%s -> %a" k value v in
  let lst = List.rev @@ SMap.fold aux m [] in
  fprintf ppf "%a" (list_sep new_pp sep) lst

(* TODO: remove all uses. this is bad. *)
let printer : ('a -> unit) -> _ -> 'a -> unit = fun f ppf x ->
  let oldstdout = Unix.dup Unix.stdout in
  let name = "/tmp/wtf-" ^ (string_of_int @@ Random.bits ()) in
  let newstdout = open_out name in
  Unix.dup2 (Unix.descr_of_out_channel newstdout) Unix.stdout;
  f x;
  flush stdout;
  Unix.dup2 oldstdout Unix.stdout;
  let ic = open_in name in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  fprintf ppf "%s" (Bytes.to_string s)
