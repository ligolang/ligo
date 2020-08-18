#!/usr/bin/env perl6
use v6.c;
use strict;
use worries;
use lib '.';
use Parser;

my $inputADTfile    = @*ARGS[0];
my $oModuleName     = @*ARGS[1];
my $mapper_filename = @*ARGS[2];

my ($adts, $moduleName, $record, $variant, $statements, $typeclasses) = parse($inputADTfile);

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
    for $adts.grep({$_<kind> ne $record && $_<kind> ne $variant && $typeclasses{$_<kind>}}).unique(:as({$_<ctorsOrFields>, $_<kind>}), :with(&[eqv])) -> $t
    { my $ty = $t<ctorsOrFields>[0]<type>;
      my $typeclass = $typeclasses{$t<kind>};
      say "  val extra_info__{$ty}__$typeclass : $ty extra_info__$typeclass;;"; }
    say "end";

    say "";
    say "module Mapper (* O : OSig  Functors are too slow and consume a lot of memory when compiling large files with OCaml. We're hardcoding the O module below for now. *) = struct";
    say "  module O : OSig = $oModuleName";
    say "";
    say "  (* must be provided by one of the open or include statements: *)";
    say "  module CheckMapperInputSignature = struct";
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