(** MODULE SIG *)
module type X = sig
  (** SIG ITEM *)

  [@view]
  val y : int -> int

  (** SIG TYPE *)
  type t

  (** SIG ITEM *)
  val p : t option
  end

(** MODULE *)
module M : X = struct
  (** TERM IN MODULE *)

  [@view]
  let y (x : int) = x + 1

  (** TYPE IN MODULE *)
  type t = {foo : nat}

  (** TERM IN MODULE *)
  let p = Some {foo = 4n}
  end

(** JUST A TYPE *)
type 'a t = 'a list

(** JUST A TERM

  with some doc
  in **several** lines

  one ~~more~~ `line`
 *)
let x : int t = [3]

let y = x

(** MODULE WITH ENTRY POINT *)
module M1 = struct
  (** BEFORE DECORATOR *)

  [@entry] // TODO pretty printer: newline above
  (** AFTER DECORATOR *)
  (** ENTRY POINT TERM *)
  let y (x : int) (t : int) = ([] : operation list), 2 * x - t

  (** NESTED MODULE *)
  module C = struct
    (** NESTED MODULE TERM *)
    let f t = t + 3
    end
  end

(** Has type with comment inside *)
let t =
  (* comment that should not break the file *)
  type x = int in
  x
