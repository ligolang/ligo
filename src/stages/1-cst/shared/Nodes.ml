(* Type definitions for all the CSTs *)

(* Vendor dependencies *)

module Utils  = Simple_utils.Utils
module Region = Simple_utils.Region

(* Utilities *)

let (<@) = Utils.(<@)

(* Lists *)

(* Conversions to the type list *)

(* Extracting regions *)

let rec last to_region = function
    [] -> Region.ghost
|  [x] -> to_region x
| _::t -> last to_region t

let nseq_to_region to_region (hd,tl) =
  Region.cover (to_region hd) (last to_region tl)

let nsepseq_to_region to_region (hd,tl) =
  let reg (_, item) = to_region item in
  Region.cover (to_region hd) (last reg tl)

let nsep_or_term_to_region to_region = function
  `Sep  s -> nsepseq_to_region to_region s
| `Term s -> nseq_to_region to_region s

let nsep_or_pref_to_region to_region = function
  `Sep  s -> nsepseq_to_region to_region s
| `Pref s -> Utils.nseq_map (fun (x,y) -> (y,x)) s
             |> nseq_to_region (to_region <@ fst)
