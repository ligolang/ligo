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
  payload      : 'payload;
  attributes   : attributes;
  region       : Region.t;
  directives   : Directive.t list;
  comments     : comment list;
  line_comment : string Region.reg option;

  set_attributes   : attributes        -> 'payload wrap;
  add_attribute    : attribute         -> 'payload wrap;
  add_directive    : Directive.t       -> 'payload wrap;
  add_comment      : comment           -> 'payload wrap;
  add_line_comment : string Region.reg -> 'payload wrap
>

type 'a t = 'a wrap

type 'a ctor =
  ?attributes:attributes ->
  ?directive:Directive.t ->
  ?comment:comment ->
  ?line_com:string Region.reg ->
  'a -> Region.t -> 'a wrap

let wrap ?(attributes=[]) ?directive ?comment ?line_com payload region =
  object
    method payload      = payload

    val    attributes   = attributes
    method attributes   = attributes

    method region       = region

    val    directives   = Option.to_list directive
    method directives   = directives

    val comments        = Option.to_list comment
    method comments     = comments

    val line_comment    = line_com
    method line_comment = line_comment

    method set_attributes attr = {< attributes = attr >}
    method add_attribute  attr = {< attributes = attr :: attributes >}
    method add_directive  dir  = {< directives = dir :: directives >}
    method add_comment    com  = {< comments = com :: comments >}
    method add_line_comment c  = {< line_comment = Some c >}
  end

let make = wrap

let ghost payload = wrap payload Region.ghost
