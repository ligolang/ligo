#!/usr/bin/env perl6
use v6.c;
use strict;
use worries;

unit module Parser;
sub parse ($inputADTfile) is export {
    my $moduleName = $inputADTfile.subst(/\.ml$/, '').samecase("A_");
    my $variant = "_ _variant";
    my $record = "_ _ record";
    sub poly { $^type_name }

    my $l = $inputADTfile.IO.lines;
    # TODO: do the inlining recursively?
    $l = $l.map({
        given $_ {
            when /^(\(\*\s+)?(open|include) \s+ (<-blank -[\(]>*) \s* \(\*\@ \s* follow \s* (<-blank -[\*]>*) \s* \*\)\s*$/ {
                flat ["(* $/[2] followed from $/[3] *)"],
                     $/[3].IO.lines.list,
                     ["(* end of $/[2] followed from $/[3] *)"]
            }
            default { [$_] }
        }
    }).flat;
    $l = $l.grep(none /^\(\*\@ \s* ignore \s* \*\)/);
    $l = $l.map(*.subst: /(^\s+|\s+$)/, "", :g);
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

    ($adts, $moduleName, $record, $variant, $statements, $typeclasses)
}