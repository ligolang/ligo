#!/usr/bin/env perl6
use v6.c;
use strict;
use worries;

my $moduleName = @*ARGS[0].subst(/\.ml$/, '').samecase("A_");
my $variant = "_ _variant";
my $record = "_ _ record";
sub poly { $^type_name }

my $l = @*ARGS[0].IO.lines;
$l = $l.map(*.subst: /^\s+/, "");
$l = $l.cache.map(*.subst: /^type\s+/, "\nand ");
$l = $l.join("\n").split(/\nand\s+/).grep(/./);
$l = $l.map(*.split("\n"));
$l = $l.map: {
    my $ll = $_;
    my ($name, $kind) = do given $_[0] {
        when /^(\w+)\s*\=$/   { "$/[0]", $variant }
        when /^(\w+)\s*\=\s*\{$/ { "$/[0]", $record }
        when /^(\w+)\s*\=\s*(\w+)\s+(\w+)$/ { "$/[0]", poly("$/[2]") }
        default { die "Syntax error when parsing header:" ~ $ll.perl ~ "\n$_" }
    };
    my $ctorsOrFields = do {
        when (/^(\w+)\s*\=\s*(\w+)\s+(\w+)$/ given $_[0]) { ((0, "$/[1]"),).Seq; }
        default {
            $_[1..*].grep({ ! /^\}?$/ }).map: {
                when /^\|\s*(\w+)\s*of\s+((\'|\w)+)$/ { "$/[0]", "$/[1]" }
                when /^(\w+)\s*\:\s*((\'|\w)+)\s*\;$/ { "$/[0]", "$/[1]" }
                default { die "Syntax error when parsing body:" ~ $ll.perl ~ "\n$_" }
            }
        };
    }
    %{
      "name"          => $name ,
      "kind"          => $kind ,
      "ctorsOrFields" => $ctorsOrFields
    }
    # $_[0].subst: , '' }
};
# $l.perl.say;
# exit;

# ($cf, $isBuiltin, $type) 
       # {
       #     name      => $cf ,
       #     newName   => "$cf'" ,
       #     isBuiltin => $isBuiltin ,
       #     type      => $type ,
       #     newType   => $isBuiltin ?? $type !! "$type'"
       # }



# my @adts_raw = [
#     # typename, kind, fields_or_ctors
#     ["root", $variant, [
#          # ctor, builtin?, type
#          ["A", False, "rootA"],
#          ["B", False, "rootB"],
#          ["C", True, "string"],
#      ]],
#     ["a", $record, [
#          # field, builtin?, type
#          ["a1", False, "ta1"],
#          ["a2", False, "ta2"],
#      ]],
#     ["ta1", $variant, [
#          ["X", False, "root"],
#          ["Y", False, "ta2"],
#      ]],
#     ["ta2", $variant, [
#          ["Z", False, "ta2"],
#          ["W", True, "unit"],
#      ]],
#     # polymorphic type
#     ["rootA", poly("list"),
#      [
#       # Position (0..n-1), builtin?, type argument
#       [0, False, "a"],
#      ],
#     ],
#     ["rootB", poly("list"),
#      [
#       # Position (0..n-1), builtin?, type argument
#       [0, True, "int"],
#      ],
#     ],
#     ];

# # say $adts_raw.perl;
# my $adts = (map -> ($name , $kind, @ctorsOrFields) {
#   {
#    "name"          => $name ,
#    "newName"       => "$name'" ,
#    "kind"          => $kind ,
#    "ctorsOrFields" => @(map -> ($cf, $isBuiltin, $type) {
#        {
#            name      => $cf ,
#            newName   => "$cf'" ,
#            isBuiltin => $isBuiltin ,
#            type      => $type ,
#            newType   => $isBuiltin ?? $type !! "$type'"
#        }
#    }, @ctorsOrFields),
#   }
# }, @adts_raw).list;

my $adts = (map -> (:$name , :$kind, :@ctorsOrFields) {
  {
   "name"          => $name ,
   "newName"       => "$name'" ,
   "kind"          => $kind ,
   "ctorsOrFields" => @(map -> ($cf, $type) {
       my $isBuiltin = ! $l.cache.first({ $_<name> eq $type });
       {
           name      => $cf ,
           newName   => "$cf'" ,
           isBuiltin => $isBuiltin ,
           type      => $type ,
           newType   => $isBuiltin ?? $type !! "$type'"
       }
   }, @ctorsOrFields),
  }
}, @$l.cache).list;

# say $adts.perl;

# say $adts.perl ;

say "(* This is an auto-generated file. Do not edit. *)";

say "";
say "open $moduleName";
say "open {$moduleName}_utils";

say "";
for $adts.kv -> $index, $t {
  my $typeOrAnd = $index == 0 ?? "type" !! "and";
  say "$typeOrAnd $t<newName> =";
  if ($t<kind> eq $variant) {
      for $t<ctorsOrFields>.list -> $c
      { say "  | $c<newName> of $c<newType>" }
  } elsif ($t<kind> eq $record) {
      say '  {';
      for $t<ctorsOrFields>.list -> $f
      { say "    $f<newName> : $f<newType> ;"; }
      say '  }';
  } else {
      print "  ";
      for $t<ctorsOrFields>.list -> $a
      { print "$a<newType> "; }
      print "$t<kind>";
      say "";
  }
}

say "";
say "module Adt_info = struct";
say "  type kind =";
say "  | Record";
say "  | Variant";
say "  | Poly of string";
say "";
say "  type ctor_or_field =";
say '    {';
say "      name : string;";
say "      isBuiltin : bool;";
say "      type_ : string;";
say '    }';
say "";
say "  type node =";
say '    {';
say "      kind : kind;";
say "      name : string;";
say "      ctors_or_fields : ctor_or_field list;";
say '    }';
say "";
say "  type adt = node list";
say "  type node_info = unit -> adt * node";
say "  type ctor_or_field_info = unit -> adt * node";
say "end";


say "";
say "type 'state continue_fold =";
say '  {';
for $adts.list -> $t
{ say "    $t<name> : $t<name> -> 'state -> ($t<newName> * 'state) ;";
  for $t<ctorsOrFields>.list -> $c
  { say "    $t<name>_$c<name> : $c<type> -> 'state -> ($c<newType> * 'state) ;" } }
say '  }';

say "";
say "type 'state fold_config =";
say '  {';
for $adts.list -> $t
{ say "    $t<name> : $t<name> -> Adt_info.node_info -> 'state -> ('state continue_fold) -> ($t<newName> * 'state) ;";
  say "    $t<name>_pre_state : $t<name> -> Adt_info.node_info -> 'state -> 'state ;";
  say "    $t<name>_post_state : $t<name> -> $t<newName> -> Adt_info.node_info -> 'state -> 'state ;";
  for $t<ctorsOrFields>.list -> $c
  { say "    $t<name>_$c<name> : $c<type> -> Adt_info.ctor_or_field_info -> 'state -> ('state continue_fold) -> ($c<newType> * 'state) ;";
  } }
say '  }';

say "";
say '(* Curries the "visitor" argument to the folds (non-customizable traversal functions). *)';
say "let rec mk_continue_fold : type state . state fold_config -> state continue_fold = fun visitor ->";
say '  {';
for $adts.list -> $t
{ say "    $t<name> = fold_$t<name> visitor ;";
  for $t<ctorsOrFields>.list -> $c
  { say "    $t<name>_$c<name> = fold_$t<name>_$c<name> visitor ;"; } }
say '}';
say "";

for $adts.list -> $t
{ say "and fold_$t<name> : type state . state fold_config -> $t<name> -> state -> ($t<newName> * state) = fun visitor x state ->";
  say "  let continue_fold : state continue_fold = mk_continue_fold visitor in";
  say "  let state = visitor.$t<name>_pre_state x (fun () -> failwith \"todo\") state in";
  say "  let (new_x, state) = visitor.$t<name> x (fun () -> failwith \"todo\") state continue_fold in";
  say "  let state = visitor.$t<name>_post_state x new_x (fun () -> failwith \"todo\") state in";
  say "  (new_x, state)";
  say "";
  for $t<ctorsOrFields>.list -> $c
  { say "and fold_$t<name>_$c<name> : type state . state fold_config -> $c<type> -> state -> ($c<newType> * state) = fun visitor x state ->";
    say "  let continue_fold : state continue_fold = mk_continue_fold visitor in";
    say "  visitor.$t<name>_$c<name> x (fun () -> failwith \"todo\") state continue_fold";
    say ""; } }

say "let no_op : 'a fold_config = \{";
for $adts.list -> $t
{ say "  $t<name> = (fun v _info state continue ->";
  say "    match v with";
  if ($t<kind> eq $variant) {
    for $t<ctorsOrFields>.list -> $c
    { say "    | $c<name> v -> let (v, state) = continue.$t<name>_$c<name> v state in ($c<newName> v, state)"; }
  } elsif ($t<kind> eq $record) {
    print '      { ';
    for $t<ctorsOrFields>.list -> $f
    { print "$f<name>; "; }
    say "} ->";
    for $t<ctorsOrFields>.list -> $f
    { say "      let ($f<newName>, state) = continue.$t<name>_$f<name> $f<name> state in"; }
    print '      ({ ';
    for $t<ctorsOrFields>.list -> $f
    { print "$f<newName>; "; }
    say '}, state)';
  } else {
    print "      v -> fold_$t<kind> v state ( ";
    print ( "continue.$t<name>_$_<name>" for $t<ctorsOrFields>.list ).join(", ");
    say " )";
  }
  say "  );";
  say "  $t<name>_pre_state = (fun v _info state -> ignore v; state) ;";
  say "  $t<name>_post_state = (fun v new_v _info state -> ignore (v, new_v); state) ;";
  for $t<ctorsOrFields>.list -> $c
  { print "  $t<name>_$c<name> = (fun v _info state continue -> ";
    if ($c<isBuiltin>) {
      print "ignore continue; (v, state)";
    } else {
      print "continue.$c<type> v state";
    }
    say ") ;"; } }
say '}';
