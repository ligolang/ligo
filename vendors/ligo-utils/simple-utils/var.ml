type 'a t = {
  name : string ;
  counter : int option ;
}

let pp ppf v =
  match v.counter with
  | None -> Format.fprintf ppf "%s" v.name
  | Some i -> Format.fprintf ppf "%s#%d" v.name i

module Int = X_int
module Option = X_option

let equal v1 v2 =
  String.equal v1.name v2.name
  && Option.equal Int.equal v1.counter v2.counter

let compare v1 v2 =
  let cname = String.compare v1.name v2.name in
  if Int.equal cname 0
  then Option.compare Int.compare v1.counter v2.counter
  else cname

let global_counter = ref 0

let reset_counter () = global_counter := 0

let of_name name =
  { name = name ;
    counter = None
  }

(* This exception indicates that some code tried to throw away the
   counter of a generated variable. It is not supposed to happen. *)
exception Tried_to_unfreshen_variable

(* TODO delete this *)
let to_name var =
  match var.counter with
  | None -> var.name
  | Some _ -> raise Tried_to_unfreshen_variable

let show v =
  match v.counter with
  | None -> Format.sprintf "%s" v.name
  | Some i -> Format.sprintf "%s#%d" v.name i

let fresh ?name () =
  let name = Option.unopt ~default:"" name in
  let counter = incr global_counter ; Some !global_counter in
  { name ; counter }

let fresh_like v =
  fresh ~name:v.name ()
