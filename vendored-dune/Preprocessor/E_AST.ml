(* This module defines and exports the type [t] of boolean expressions
   of preprocessing directives #if and #elif. *)

(* Vendors' dependencies *)

module Region = Simple_utils.Region

(* Boolean expressions *)

type t =
  Or     of (t * t) Region.reg
| And    of (t * t) Region.reg
| Eq     of (t * t) Region.reg
| Neq    of (t * t) Region.reg
| Not    of t Region.reg
| True   of Region.t
| False  of Region.t
| Ident  of string Region.reg
| Parens of t Region.reg

(* Projecting the regions *)

let to_region =
  let open Region in
  function
    Or     {region; _}
  | And    {region; _}
  | Eq     {region; _}
  | Neq    {region; _}
  | Not    {region; _}
  | True   region
  | False  region
  | Ident  {region; _}
  | Parens {region; _}
    -> region

(* Converting to string *)

let rec to_string =
  let open Region in
  let sprintf = Printf.sprintf in
  function
    Or {value=t1,t2; _}  -> sprintf "%s || %s" (to_string t1) (to_string t2)
  | And {value=t1,t2; _} -> sprintf "%s && %s" (to_string t1) (to_string t2)
  | Eq {value=t1,t2; _}  -> sprintf "%s == %s" (to_string t1) (to_string t2)
  | Neq {value=t1,t2; _} -> sprintf "%s != %s" (to_string t1) (to_string t2)
  | Not {value; _}       -> sprintf "!%s" (to_string value)
  | True _               -> "true"
  | False _              -> "false"
  | Ident {value; _}     -> value
  | Parens {value; _}    -> sprintf "(%s)" (to_string value)

(* Environments *)

let rec eval env =
  let open Region in
  function
    Or  {value=e1,e2; _} -> eval env e1 || eval env e2
  | And {value=e1,e2; _} -> eval env e1 && eval env e2
  | Eq  {value=e1,e2; _} -> Bool.(eval env e1 = eval env e2)
  | Neq {value=e1,e2; _} -> Bool.(eval env e1 != eval env e2)
  | Not {value=e; _}     -> not (eval env e)
  | True  _              -> true
  | False _              -> false
  | Ident {value=id; _}  -> Env.mem id env
  | Parens {value=e; _}  -> eval env e
