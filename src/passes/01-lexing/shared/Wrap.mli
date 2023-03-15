(* Wrapping attributes and regions with tokens *)

(* Vendor dependencies *)

module Region    = Simple_utils.Region
module Directive = Preprocessor.Directive

(* Local dependencies *)

type attribute  = Attr.t Region.reg
type attributes = attribute list
type comment    = string Region.reg

(* Wrapping tokens with metadata *)

type 'payload wrap = <
  payload    : 'payload;
  attributes : attributes;
  region     : Region.t;
  directives : Directive.t list;
  comments   : comment list;

  set_attributes : attributes  -> 'payload wrap;
  add_attribute  : attribute   -> 'payload wrap;
  add_directive  : Directive.t -> 'payload wrap;
  add_comment    : comment     -> 'payload wrap
>

type 'a t = 'a wrap

val wrap :
  ?attributes:attributes ->
  ?directive:Directive.t ->
  ?comment:comment ->
  'a -> Region.t -> 'a wrap

val make :
  ?attributes:attributes ->
  ?directive:Directive.t ->
  ?comment:comment ->
  'a -> Region.t -> 'a wrap

val ghost : 'a -> 'a wrap
