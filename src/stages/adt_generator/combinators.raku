#!/usr/bin/env perl6
use v6.c;
use strict;
use worries;
use lib '.';
use Parser;

my $inputADTfile         = @*ARGS[0];
my $combinators_filename = @*ARGS[1];

my ($adts, $moduleName, $record, $variant, $statements, $typeclasses) = parse($inputADTfile);

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
    for $adts.grep({$_<kind> ne $record && $_<kind> ne $variant && $typeclasses{$_<kind>}}).unique(:as({$_<ctorsOrFields>, $_<kind>}), :with(&[eqv])) -> $t
    { my $ty = $t<ctorsOrFields>[0]<type>;
      my $typeclass = $typeclasses{$t<kind>};
      say "let extra_info__{$ty}__$typeclass : $ty extra_info__$typeclass = {tc $typeclass}.$ty;;";
    }
    # Check that we won't have a cyclic module dependency when using the Folder to auto-generate the compare:
    say "(* Check that we won't have a cyclic module dependency when using the Folder to auto-generate the compare: *)";
    say "module DummyTest_ = Generated_fold;;";
}
