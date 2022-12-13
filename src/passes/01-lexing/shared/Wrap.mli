(* Wrapping attributes and regions with tokens *)

(* Vendor dependencies *)

module Region = Simple_utils.Region

(* Local dependencies *)

type attributes = Attr.attribute Region.reg list

(* Wrapping tokens with metadata *)

type 'payload wrap = <
  payload    : 'payload;
  attributes : attributes;
  region     : Region.t;

  set_attributes : attributes -> 'payload wrap
>

type 'a t = 'a wrap

val wrap : ?attributes:attributes -> 'a -> Region.t -> 'a wrap
val make : ?attributes:attributes -> 'a -> Region.t -> 'a wrap

val ghost : 'a -> 'a wrap
