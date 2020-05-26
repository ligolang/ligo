#!/usr/bin/env perl6
use v6.c;
use strict;
use worries;

# TODO: find a way to do mutual recursion between the produced file and some #include-y-thingy
# TODO: make an .mli
# TODO: shorthand for `foo list` etc. in field and constructor types
# TODO: error when reserved names are used ("state", … please list them here)

my $inputADTfile         = @*ARGS[0];
my $oModuleName          = @*ARGS[1];
my $combinators_filename = @*ARGS[2];
my $folder_filename      = @*ARGS[3];
my $mapper_filename      = @*ARGS[4];

my $moduleName = $inputADTfile.subst(/\.ml$/, '').samecase("A_");
my $variant = "_ _variant";
my $record = "_ _ record";
sub poly { $^type_name }

my $l = $inputADTfile.IO.lines;
$l = $l.map(*.subst: /(^\s+|\s+$)/, "");
$l = $l.list.cache;
my $statement_re = /^((\(\*\s+)?(open|include)\s|\[\@\@\@warning\s)/;
my $statements = $l.grep($statement_re);
$l             = $l.grep(none $statement_re);
$l = $l.list.cache;
my $typeclass_re = /^\(\*\@ \s* typeclass \s+ (\w+) \s+ (\w+) \s* \*\)/;
my $typeclasses = %($l.grep($typeclass_re).map({ do given $_ { when $typeclass_re { %{ "$/[0]" => "$/[1]" } } } }).flat);
$l              = $l.grep(none $typeclass_re);
$statements    = $statements.map(*.subst(/^\(\*\s+/, '').subst(/\s+\*\)$/, ''));
$l = $l.cache.map(*.subst: /^type\s+/, "\nand ");
# TODO: find a better way to write [\*] (anything but a star), the Raku form I found <-[\*]> is very verbose.
$l = $l.join("\n").subst(/\n+/, "\n", :g); # join lines and remove consecutive newlines
$l = $l.subst(/\s*\(\* ( <-[\*]> | \*+<-[\*\)]> )* \*\)/, '', :g); # discard comments (incl. multi-line comments)
$l = $l.split(/\nand\s+/).grep(/./); # split lines again and preserve nonempty lines
$l = $l.map(*.split("\n"));
$l = $l.map: {
    my $ll = $_;
    my ($name, $kind) = do given $_[0] {
        when /^((\w|\')+)\s*\=$/   { "$/[0]", $variant }
        when /^((\w|\')+)\s*\=\s*\{$/ { "$/[0]", $record }
        when /^((\w|\')+)\s*\=\s*((\w|\')+)\s+((\w|\')+)$/ { "$/[0]", poly("$/[2]") }
        default { die "Syntax error when parsing header:" ~ $ll.perl ~ "\n$_" }
    };
    my $ctorsOrFields = do {
        when (/^((\w|\')+)\s*\=\s*((\w|\')+)\s+((\w|\')+)$/ given $_[0]) { ((0, "$/[1]"),).Seq; }
        default {
            $_[1..*].grep({ ! /^\}?$/ }).map: {
                when /^\|\s*((\w|\')+)\s*of\s+((\w|\')+)$/ { "$/[0]", "$/[1]" }
                when /^\|\s*((\w|\')+)$/ { "$/[0]", "" }
                when /^((\w|\')+)\s*\:\s*((\w|\')+)\s*\;$/ { "$/[0]", "$/[1]" }
                default { die "Syntax error when parsing body:" ~ $ll.perl ~ "\n$_" }
            }
        };
    }
    %{
      "name"          => $name ,
      "kind"          => $kind ,
      "ctorsOrFields" => $ctorsOrFields
    }
};

my $adts = (map -> (:$name , :$kind, :@ctorsOrFields) {
  {
   "name"          => $name ,
   "oNewName"      => "O.{$name}", # ($kind ne $record && $kind ne $variant) ?? "$name" !! "O.{$name}",
   "newName"       => $name ,
   "kind"          => $kind ,
   "ctorsOrFields" => @(map -> ($cf, $type) {
       my $resolvedType = $type && $l.cache.first({ $_<name> eq $type });
       my $isBuiltin = (! $type) || (! $resolvedType);
       # my $isPoly = $resolvedType && $resolvedType<kind> ne $record && $resolvedType<kind> ne $variant;
       {
           name      => $cf ,
           oNewName  => "O.{$cf}" ,
           newName   => $cf ,
           isBuiltin => $isBuiltin ,
           type      => $type ,
           oNewType  => $isBuiltin ?? "$type" !! "O.{$type}" ,
           newType   => $type ,
       }
   }, @ctorsOrFields),
  }
}, @$l.cache).list;


# Auto-generated fold functions
$*OUT = open $folder_filename, :w;
{
    say "(* This is an auto-generated file. Do not edit. *)";
    say "";
    for $statements -> $statement { say "$statement" }
    say "open $moduleName;;";

    say "";
    say "  include Adt_generator.Generic.BlahBluh";
    say "  type ('in_state, 'out_state , 'adt_info_node_instance_info) _fold_config = \{";
    say "      generic : 'in_state -> 'adt_info_node_instance_info -> 'out_state;";
    say "      generic_empty_ctor : 'in_state -> 'out_state;";
    # look for builtins, filtering out the "implicit unit-like fake argument of emtpy constructors" (represented by '')
    for $adts.map({ $_<ctorsOrFields> })[*;*].grep({$_<isBuiltin> && $_<type> ne ''}).map({$_<type>}).unique -> $builtin
    { say "      $builtin : ('in_state , 'out_state , 'adt_info_node_instance_info) _fold_config -> 'in_state -> $builtin -> 'out_state;"; }
    # look for built-in polymorphic types
    for $adts.grep({$_<kind> ne $record && $_<kind> ne $variant}).map({$_<kind>}).unique -> $poly
    { say "      $poly : 'a . ('in_state , 'out_state , 'adt_info_node_instance_info) _fold_config -> ('in_state -> 'a -> 'out_state) -> 'in_state -> 'a $poly -> 'out_state;"; }
    say '    };;';

    say "";
    say "  module Adt_info = Adt_generator.Generic.Adt_info (struct";
    say "    type nonrec ('in_state , 'out_state , 'adt_info_node_instance_info) fold_config = ('in_state , 'out_state , 'adt_info_node_instance_info) _fold_config;;";
    say "  end);;";
    say "  include Adt_info;;";
    say "  type ('in_state, 'out_state) fold_config = ('in_state , 'out_state , ('in_state , 'out_state) Adt_info.node_instance_info) _fold_config;;";

    say "";
    say '  type the_folds = {';
    for $adts.list -> $t
    { say "      fold__$t<name> : 'in_state 'out_state . the_folds -> ('in_state , 'out_state) fold_config -> 'in_state -> $t<name> -> 'out_state;";
      for $t<ctorsOrFields>.list -> $c
      { say "      fold__$t<name>__$c<name> : 'in_state 'out_state . the_folds -> ('in_state , 'out_state) fold_config -> 'in_state -> { $c<type> || 'unit' } -> 'out_state;"; } }
    say '    };;';

    # generic programming info about the nodes and fields
    say "";
    for $adts.list -> $t
    { for $t<ctorsOrFields>.list -> $c
      { say "  (* info for field or ctor $t<name>.$c<name> *)";
        say "  let info__$t<name>__$c<name> : Adt_info.ctor_or_field = \{";
        say "      name = \"$c<name>\";";
        say "      is_builtin = {$c<isBuiltin> ?? 'true' !! 'false'};";
        say "      type_ = \"$c<type>\";";
        say '    };;';
        # say "";
        say "  let continue_info__$t<name>__$c<name> : type in_qstate out_qstate . the_folds -> (in_qstate , out_qstate) fold_config -> {$c<type> || 'unit'} -> (in_qstate, out_qstate) Adt_info.ctor_or_field_instance = fun the_folds visitor x -> \{";
        say "        cf = info__$t<name>__$c<name>;";
        say "        cf_continue = (fun state -> the_folds.fold__$t<name>__$c<name> the_folds visitor state x);";
        say "        cf_new_fold = (fun visitor state -> the_folds.fold__$t<name>__$c<name> the_folds visitor state x);";
        say '      };;';
        # say "";
      }
      say "  (* info for node $t<name> *)";
      say "  let info__$t<name> : Adt_info.node = \{";
      my $kind = do given $t<kind> {
          when $record { "Record" }
          when $variant { "Variant" }
          default { "Poly \"$_\"" }
      };
      say "      kind = $kind;";
      say "      declaration_name = \"$t<name>\";";
      print "      ctors_or_fields = [ ";
      for $t<ctorsOrFields>.list -> $c { print "info__$t<name>__$c<name> ; "; }
      say "];";
      say '    };;';
      # say "";
      # TODO: factor out some of the common bits here.
      say "  let continue_info__$t<name> : type in_qstate out_qstate . the_folds -> (in_qstate , out_qstate) fold_config -> $t<name> -> (in_qstate , out_qstate) Adt_info.instance = fun the_folds visitor x ->";
      say '    {';
      say "      instance_declaration_name = \"$t<name>\";";
      do given $t<kind> {
          when $record {
              say '      instance_kind = RecordInstance {';
              print "      fields = [ ";
              for $t<ctorsOrFields>.list -> $c { print "continue_info__$t<name>__$c<name> the_folds visitor x.$c<name> ; "; }
              say "      ];";
              say '  };';
          }
          when $variant {
              say "      instance_kind =";
              say '        VariantInstance {';
              say "            constructor =";
              say "              (match x with";
              for $t<ctorsOrFields>.list -> $c { say "               | $c<name> { $c<type> ?? 'v ' !! '' }-> continue_info__$t<name>__$c<name> the_folds visitor { $c<type> ?? 'v' !! '()' }"; }
              say "              );";
              print "            variant = [ ";
              for $t<ctorsOrFields>.list -> $c { print "info__$t<name>__$c<name> ; "; }
              say "];";
              say '          };';
          }
          default {
              say "      instance_kind =";
              say '        PolyInstance {';
              say "            poly = \"$_\";";
              print "            arguments = [";
              # TODO: sort by c<name> (currently we only have one-argument
              # polymorphic types so it happens to work but should be fixed.
              for $t<ctorsOrFields>.list -> $c { print "\"$c<type>\""; }
              say "];";
              print "            poly_continue = (fun state -> visitor.$_ visitor (";
              print $t<ctorsOrFields>
                  .map(-> $c { "(fun state x -> (continue_info__$t<name>__$c<name> the_folds visitor x).cf_continue state)" })
                  .join(", ");
              say ") state x);";
              say '          };';
          }
      };
      say '    };;';
      # say "";
    }

    say "";
    say "  (* info for adt $moduleName *)";
    print "  let whole_adt_info : unit -> Adt_info.adt = fun () -> [ ";
    for $adts.list -> $t
    { print "info__$t<name> ; "; }
    say "];;";

    # fold functions
    say "";
    for $adts.list -> $t
    { say "  let fold__$t<name> : type in_qstate out_qstate . the_folds -> (in_qstate , out_qstate) fold_config -> in_qstate -> $t<name> -> out_qstate = fun the_folds visitor state x ->";
      # TODO: add a non-generic continue_fold.
      say '    let node_instance_info : (in_qstate , out_qstate) Adt_info.node_instance_info = {';
      say "        adt = whole_adt_info () ;";
      say "        node_instance = continue_info__$t<name> the_folds visitor x";
      say '      } in';
      # say "  let (state, new_x) = visitor.$t<name>.node__$t<name> x (fun () -> whole_adt_info, info__$t<name>) state continue_fold in";
      say "    visitor.generic state node_instance_info;;";
      # say "";
      for $t<ctorsOrFields>.list -> $c
      { say "  let fold__$t<name>__$c<name> : type in_qstate out_qstate . the_folds -> (in_qstate , out_qstate) fold_config -> in_qstate -> { $c<type> || 'unit' } -> out_qstate = fun the_folds visitor state { $c<type> ?? 'x' !! '()' } ->";
        # say "  let ctor_or_field_instance_info : (in_qstate , out_qstate) Adt_info.ctor_or_field_instance_info = whole_adt_info (), info__$t<name>, continue_info__$t<name>__$c<name> visitor x in";
        if ($c<type> eq '') {
            # nothing to do, this constructor has no arguments.
            say "    ignore the_folds; visitor.generic_empty_ctor state;;";
        } elsif ($c<isBuiltin>) {
            say "    ignore the_folds; visitor.$c<type> visitor state x;;"; # (*visitor.generic_ctor_or_field ctor_or_field_instance_info*)
        } else {
            say "    the_folds.fold__$c<type> the_folds visitor state x;;"; # (*visitor.generic_ctor_or_field ctor_or_field_instance_info*)
        }
        # say "  visitor.$t<name>.$t<name>__$c<name> x (fun () -> whole_adt_info, info__$t<name>, info__$t<name>__$c<name>) state continue_fold";
        # say "";
      }
    }
    # look for builtins, filtering out the "implicit unit-like fake argument of emtpy constructors" (represented by '')
    for $adts.map({ $_<ctorsOrFields> })[*;*].grep({$_<isBuiltin> && $_<type> ne ''}).map({$_<type>}).unique -> $builtin
      { say "  let fold__$builtin : type in_qstate out_qstate . the_folds -> (in_qstate , out_qstate) fold_config -> in_qstate -> $builtin -> out_qstate = fun the_folds visitor state x ->";
        say "    ignore the_folds; visitor.$builtin visitor state x;;"; } # (*visitor.generic_ctor_or_field ctor_or_field_instance_info*)

    say "";
    say '  let the_folds : the_folds = {';
    for $adts.list -> $t
    { say "    fold__$t<name>;";
      for $t<ctorsOrFields>.list -> $c
      { say "    fold__$t<name>__$c<name>;" } }
    say '  };;';

    # Tying the knot
    say "";
    for $adts.list -> $t
    { say "  let fold__$t<name> : type in_qstate out_qstate . (in_qstate , out_qstate) fold_config -> in_qstate -> $t<name> -> out_qstate = fun visitor state x -> fold__$t<name> the_folds visitor state x;;";
      for $t<ctorsOrFields>.list -> $c
      { say "  let fold__$t<name>__$c<name> : type in_qstate out_qstate . (in_qstate , out_qstate) fold_config -> in_qstate -> { $c<type> || 'unit' } -> out_qstate = fun visitor state x -> fold__$t<name>__$c<name> the_folds visitor state x;;" } }
    # look for builtins, filtering out the "implicit unit-like fake argument of emtpy constructors" (represented by '')
    for $adts.map({ $_<ctorsOrFields> })[*;*].grep({$_<isBuiltin> && $_<type> ne ''}).map({$_<type>}).unique -> $builtin
    { say "  let fold__$builtin : type in_qstate out_qstate . (in_qstate , out_qstate) fold_config -> in_qstate -> $builtin -> out_qstate = fun visitor state x -> fold__$builtin the_folds visitor state x;;"; }

    say "";
    say "  module Folds (M : sig type in_state type out_state type 'a t val f : ((in_state , out_state) fold_config -> in_state -> 'a -> out_state) -> 'a t end) = struct";
    for $adts.list -> $t
    { say "  let $t<name> = M.f fold__$t<name>;;"; }
    # look for builtins, filtering out the "implicit unit-like fake argument of emtpy constructors" (represented by '')
    for $adts.map({ $_<ctorsOrFields> })[*;*].grep({$_<isBuiltin> && $_<type> ne ''}).map({$_<type>}).unique -> $builtin
    { say "  let $builtin = M.f fold__$builtin"; }
    say "  end";
}

# auto-generated fold_map functions
$*OUT = open $mapper_filename, :w;
{
    say "(* This is an auto-generated file. Do not edit. *)";
    say "";
    for $statements -> $statement { say "$statement" }
    say "open Adt_generator.Common;;";
    say "open $moduleName;;";

    say "";
    say "module type OSig = sig";
    for $adts.list -> $t {
        say "  type $t<newName>;;";
    }

    for $adts.list -> $t {
        if ($t<kind> eq $variant) {
            for $t<ctorsOrFields>.list -> $c {
                say "  val make__$t<newName>__$c<newName> : {$c<type> ne '' ?? "$c<newType> " !! 'unit'} -> $t<newName>;;";
            }
        } elsif ($t<kind> eq $record) {
            print "  val make__$t<newName>";
            say ' :';
            for $t<ctorsOrFields>.list -> $f
            { say "    {$f<newName>}:{$f<newType>} ->"; }
            say "    $t<newName>;;";
        } else {
            print "  val make__$t<newName> : (";
            print $t<ctorsOrFields>.map({$_<newType>}).join(" , ");
            say ") $t<kind> -> $t<newName>;;";
        }
    }

    say "";
    for $adts.grep({$_<kind> ne $record && $_<kind> ne $variant && $typeclasses{$_<kind>}}).unique(:as({$_<ctorsOrFields>, $_<kind>})) -> $t
    { my $ty = $t<ctorsOrFields>[0]<type>;
      my $typeclass = $typeclasses{$t<kind>};
      say "  val extra_info__{$ty}__$typeclass : $ty extra_info__$typeclass;;"; }
    say "end";

    say "";
    say "module Mapper (* O : OSig  Functors are too slow and consume a lot of memory when compiling large files with OCaml. We're hardcoding the O module below for now. *) = struct";
    say "  module O : OSig = $oModuleName";
    say "";
    say "  (* must be provided by one of the open or include statements: *)";
    say "  module CheckInputSignature = struct";
    for $adts.grep({$_<kind> ne $record && $_<kind> ne $variant}).map({$_<kind>}).unique -> $poly
    { say "    let fold_map__$poly : type a new_a state err .{ $typeclasses{$poly} ?? " new_a extra_info__{$typeclasses{$poly}} ->" !! "" } (state -> a -> (state * new_a, err) monad) -> state -> a $poly -> (state * new_a $poly , err) monad = fold_map__$poly;;"; }
    say "  end";

    say "";
    for $adts.list -> $t {
        say "  type ('state, 'err) _continue_fold_map__$t<name> = \{";
        say "      node__$t<name> : 'state -> $t<name> -> ('state * $t<oNewName> , 'err) monad ;";
        for $t<ctorsOrFields>.list -> $c
        { say "      $t<name>__$c<name> : 'state -> {$c<type> || 'unit'} -> ('state * {$c<oNewType> || 'unit'} , 'err) monad ;" }
        say '    };;';
    }

    say "  type ('state , 'err) _continue_fold_map__$moduleName = \{";
    for $adts.list -> $t {
        say "      $t<name> : ('state , 'err) _continue_fold_map__$t<name> ;";
    }
    say '    };;';

    say "";
    for $adts.list -> $t
    { say "  type ('state, 'err) fold_map_config__$t<name> = \{";
      say "      node__$t<name> : 'state -> $t<name> -> ('state, 'err) _continue_fold_map__$moduleName -> ('state * $t<oNewName> , 'err) monad ;"; # (*Adt_info.node_instance_info ->*)
      say "      node__$t<name>__pre_state : 'state -> $t<name> -> ('state, 'err) monad ;"; # (*Adt_info.node_instance_info ->*)
      say "      node__$t<name>__post_state : 'state -> $t<name> -> $t<oNewName> -> ('state, 'err) monad ;"; # (*Adt_info.node_instance_info ->*)
      for $t<ctorsOrFields>.list -> $c
      { say "      $t<name>__$c<name> : 'state -> {$c<type> || 'unit'} -> ('state, 'err) _continue_fold_map__$moduleName -> ('state * {$c<oNewType> || 'unit'} , 'err) monad ;"; # (*Adt_info.ctor_or_field_instance_info ->*)
      }
      say '  };;' }

    say "  type ('state, 'err) fold_map_config__$moduleName = \{";
    for $adts.list -> $t
    { say "      $t<name> : ('state, 'err) fold_map_config__$t<name>;" }
    say '    };;';

    say "";
    say "  type ('state, 'err) mk_continue_fold_map = \{";
    say "      fn : ('state, 'err) mk_continue_fold_map -> ('state, 'err) fold_map_config__$moduleName -> ('state, 'err) _continue_fold_map__$moduleName";
    say '    };;';


    # fold_map functions
    say "";
    for $adts.list -> $t
    { say "  let _fold_map__$t<name> : type qstate err . (qstate,err) mk_continue_fold_map -> (qstate,err) fold_map_config__$moduleName -> qstate -> $t<name> -> (qstate * $t<oNewName>, err) monad = fun mk_continue_fold_map visitor state x ->";
      say "    let continue_fold_map : (qstate,err) _continue_fold_map__$moduleName = mk_continue_fold_map.fn mk_continue_fold_map visitor in";
      say "    visitor.$t<name>.node__$t<name>__pre_state state x >>? fun state ->"; # (*(fun () -> whole_adt_info, info__$t<name>)*)
      say "    visitor.$t<name>.node__$t<name> state x continue_fold_map >>? fun (state, new_x) ->"; # (*(fun () -> whole_adt_info, info__$t<name>)*)
      say "    visitor.$t<name>.node__$t<name>__post_state state x new_x >>? fun state ->"; # (*(fun () -> whole_adt_info, info__$t<name>)*)
      say "    return (state, new_x);;";
      # say "";
      for $t<ctorsOrFields>.list -> $c
      { say "  let _fold_map__$t<name>__$c<name> : type qstate err . (qstate,err) mk_continue_fold_map -> (qstate,err) fold_map_config__$moduleName -> qstate -> { $c<type> || 'unit' } -> (qstate * { $c<oNewType> || 'unit' }, err) monad = fun mk_continue_fold_map visitor state x ->";
        say "    let continue_fold_map : (qstate,err) _continue_fold_map__$moduleName = mk_continue_fold_map.fn mk_continue_fold_map visitor in";
        say "    visitor.$t<name>.$t<name>__$c<name> state x continue_fold_map;;"; # (*(fun () -> whole_adt_info, info__$t<name>, info__$t<name>__$c<name>)*)
        # say "";
      }
    }

    # make the "continue" object
    say "";
    say '  (* Curries the "visitor" argument to the folds (non-customizable traversal functions). *)';
    say "  let mk_continue_fold_map : 'state 'err . ('state,'err) mk_continue_fold_map = \{";
    say "      fn =";
    say "        fun self visitor ->";
    say '        {';
    for $adts.list -> $t
    { say "          $t<name> = \{";
      say "              node__$t<name> = (fun state x -> _fold_map__$t<name> self visitor state x) ;";
      for $t<ctorsOrFields>.list -> $c
      { say "              $t<name>__$c<name> = (fun state x -> _fold_map__$t<name>__$c<name> self visitor state x) ;"; }
      say '            };' }
    say '    }';
    say '  };;';
    say "";

    # fold_map functions : tying the knot
    say "";
    for $adts.list -> $t
    { say "  let fold_map__$t<name> : type qstate err . (qstate,err) fold_map_config__$moduleName -> qstate -> $t<name> -> (qstate * $t<oNewName>,err) monad =";
      say "    fun visitor state x -> _fold_map__$t<name> mk_continue_fold_map visitor state x;;";
      for $t<ctorsOrFields>.list -> $c
      { say "  let fold_map__$t<name>__$c<name> : type qstate err . (qstate,err) fold_map_config__$moduleName -> qstate -> { $c<type> || 'unit' } -> (qstate * { $c<oNewType> || 'unit' },err) monad =";
        say "    fun visitor state x -> _fold_map__$t<name>__$c<name> mk_continue_fold_map visitor state x;;"; } }

    say "";
    for $adts.list -> $t
    {
        say "  let no_op_node__$t<name> : type state . state -> $t<name> -> (state,_) _continue_fold_map__$moduleName -> (state * $t<oNewName>,_) monad =";
        say "    fun state v continue ->"; # (*_info*)
        say "      match v with";
        if ($t<kind> eq $variant) {
            for $t<ctorsOrFields>.list -> $c
            { given $c<type> {
                when '' { say "      | $c<name> -> continue.$t<name>.$t<name>__$c<name> state () >>? fun (state , ()) -> return (state , O.make__$t<newName>__$c<newName> ())"; }
                default { say "      | $c<name> v -> continue.$t<name>.$t<name>__$c<name> state v >>? fun (state , v) -> return (state , O.make__$t<newName>__$c<newName> v)"; } } }
        } elsif ($t<kind> eq $record) {
            print '        { ';
            for $t<ctorsOrFields>.list -> $f
            { print "$f<name>; "; }
            say "} ->";
            for $t<ctorsOrFields>.list -> $f
            { say "        continue.$t<name>.$t<name>__$f<name> state $f<name> >>? fun (state , $f<newName>) ->"; }
            print "        return (state , (O.make__$t<newName>";
            for $t<ctorsOrFields>.list -> $f
            { print " ~$f<newName>"; }
            say " : $t<oNewName>))";
        } else {
            print "        v -> (fold_map__$t<kind>";
            if ($t<kind> ne $record && $t<kind> ne $variant && $typeclasses{$t<kind>}) {
                for  $t<ctorsOrFields>.list -> $a
                { print " O.extra_info__$a<type>__{$typeclasses{$t<kind>}}"; }
            }
            print " ( ";
            print ( "continue.$t<name>.$t<name>__$_<name>" for $t<ctorsOrFields>.list ).join(", ");
            say " ) state v)";
            say "        >>? fun (state, x) -> return (state, O.make__$t<name> x);;";
        }
    }

    for $adts.list -> $t
    { say "  let no_op__$t<name> : type state . (state,_) fold_map_config__$t<name> = \{";
      say "    node__$t<name> = no_op_node__$t<name>;";
      say "    node__$t<name>__pre_state = (fun state v -> ignore v; return state) ;"; # (*_info*)
      say "    node__$t<name>__post_state = (fun state v new_v -> ignore (v, new_v); return state) ;"; # (*_info*)
      for $t<ctorsOrFields>.list -> $c
      { print "    $t<name>__$c<name> = (fun state v continue -> "; # (*_info*)
        if ($c<isBuiltin>) {
            print "ignore continue; return (state , v)";
        } else {
            print "continue.$c<type>.node__$c<type> state v";
        }
        say ") ;"; }
      say '    }' }

    say "  let no_op : type state . (state,_) fold_map_config__$moduleName = \{";
    for $adts.list -> $t
    { say "    $t<name> = no_op__$t<name>;" }
    say '  };;';

    say "";
    for $adts.list -> $t
    { say "  let with__$t<name> : _ -> _ fold_map_config__$moduleName -> _ fold_map_config__$moduleName = (fun node__$t<name> op -> \{ op with $t<name> = \{ op.$t<name> with node__$t<name> \} \});;";
      say "  let with__$t<name>__pre_state : _ -> _ fold_map_config__$moduleName -> _ fold_map_config__$moduleName = (fun node__$t<name>__pre_state op -> \{ op with $t<name> = \{ op.$t<name> with node__$t<name>__pre_state \} \});;";
      say "  let with__$t<name>__post_state : _ -> _ fold_map_config__$moduleName -> _ fold_map_config__$moduleName = (fun node__$t<name>__post_state op -> \{ op with $t<name> = \{ op.$t<name> with node__$t<name>__post_state \} \});;";
      for $t<ctorsOrFields>.list -> $c
      { say "  let with__$t<name>__$c<name> : _ -> _ fold_map_config__$moduleName -> _ fold_map_config__$moduleName = (fun $t<name>__$c<name> op -> \{ op with $t<name> = \{ op.$t<name> with $t<name>__$c<name> \} \});;"; } }
    say "end";
}

$*OUT = open $combinators_filename, :w;
{
    say "(* This is an auto-generated file. Do not edit. *)";
    say "";
    for $statements -> $statement { say "$statement" }
    say "open $moduleName;;";
    say "";
    for $adts.list -> $t {
        say "type nonrec $t<name> = $t<name>;;";
    }

    for $adts.list -> $t {
        if ($t<kind> eq $variant) {
            for $t<ctorsOrFields>.list -> $c {
                say "let make__$t<name>__$c<name> : {$c<type> ne '' ?? "$c<newType> " !! 'unit'} -> $t<name> = fun {$c<type> ne '' ?? 'v' !! '()'} -> $c<name> {$c<type> ne '' ?? 'v ' !! ''};;";
            }
        } elsif ($t<kind> eq $record) {
            print "let make__$t<name>";
            print ' :';
            for $t<ctorsOrFields>.list -> $f
            { print "  {$f<newName>}:{$f<newType>} ->"; }
            print "  $t<newName> = fun";
            for $t<ctorsOrFields>.list -> $f
            { print "  ~{$f<newName>}"; }
            print " -> \{";
            for $t<ctorsOrFields>.list -> $f
            { print " {$f<newName>} ;"; }
            say " \};;";
        } else {
            print "let make__$t<newName> : (";
            print $t<ctorsOrFields>.map({$_<newType>}).join(" , ");
            print ") $t<kind> -> $t<newName> = ";
            print "fun x -> x";
            say ";;";
        }
    }

    say "";
    for $adts.grep({$_<kind> ne $record && $_<kind> ne $variant && $typeclasses{$_<kind>}}).unique(:as({$_<ctorsOrFields>, $_<kind>})) -> $t
    { my $ty = $t<ctorsOrFields>[0]<type>;
      my $typeclass = $typeclasses{$t<kind>};
      say "let extra_info__{$ty}__$typeclass : $ty extra_info__$typeclass = {tc $typeclass}.$ty;;";
    }
    # Check that we won't have a cyclic module dependency when using the Folder to auto-generate the compare:
    say "(* Check that we won't have a cyclic module dependency when using the Folder to auto-generate the compare: *)";
    say "module DummyTest_ = Generated_fold;;";
}
