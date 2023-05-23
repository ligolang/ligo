(* Wrapping attributes and regions with tokens *)

(* Vendor dependencies *)

module Region    = Simple_utils.Region
module Directive = Preprocessor.Directive

(* Local dependencies *)

type attribute  = Attr.t Region.reg
type attributes = attribute list

(* Comments *)

type comment =
  Block of string Region.reg
| Line  of string Region.reg

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

let wrap ?(attributes=[]) ?directive ?comment payload region =
  object
    method payload    = payload

    val    attributes = attributes
    method attributes = attributes

    method region     = region

    val    directives = Option.to_list directive
    method directives = directives

    val comments      = Option.to_list comment
    method comments   = comments

    method set_attributes attr = {< attributes = attr >}
    method add_attribute  attr = {< attributes = attr :: attributes >}
    method add_directive  dir  = {< directives = dir :: directives >}
    method add_comment    com  = {< comments = com :: comments >}
  end

let make = wrap

let ghost payload = wrap ~attributes:[] payload Region.ghost
