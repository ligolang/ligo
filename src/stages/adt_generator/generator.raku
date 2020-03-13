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
say "module Adt_info = Generic.Adt_info";

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
say "type 'state continue_fold_map =";
say '  {';
for $adts.list -> $t
{ say "    $t<name> : $t<name> -> 'state -> ($t<newName> * 'state) ;";
  for $t<ctorsOrFields>.list -> $c
  { say "    $t<name>_$c<name> : $c<type> -> 'state -> ($c<newType> * 'state) ;" } }
say '  }';

say "";
say "type 'state fold_map_config =";
say '  {';
for $adts.list -> $t
{ say "    $t<name> : $t<name> -> (*Adt_info.node_instance_info ->*) 'state -> ('state continue_fold_map) -> ($t<newName> * 'state) ;";
  say "    $t<name>_pre_state : $t<name> -> (*Adt_info.node_instance_info ->*) 'state -> 'state ;";
  say "    $t<name>_post_state : $t<name> -> $t<newName> -> (*Adt_info.node_instance_info ->*) 'state -> 'state ;";
  for $t<ctorsOrFields>.list -> $c
  { say "    $t<name>_$c<name> : $c<type> -> (*Adt_info.ctor_or_field_instance_info ->*) 'state -> ('state continue_fold_map) -> ($c<newType> * 'state) ;";
  } }
say '  }';

say "";
say "module StringMap = Map.Make(String)";
say "(* generic folds for nodes *)";
say "type 'state generic_continue_fold_node = \{";
say "  continue                 : 'state -> 'state ;";
say "  (* generic folds for each field *)";
say "  continue_ctors_or_fields : ('state -> 'state) StringMap.t ;";
say '}';
say "(* map from node names to their generic folds *)";
say "type 'state generic_continue_fold = ('state generic_continue_fold_node) StringMap.t";
say "";
say "type 'state fold_config =";
say '  {';
say "    generic : 'state Adt_info.node_instance_info -> 'state -> 'state;";
for $adts.map({ $_<ctorsOrFields> })[*;*].grep({$_<isBuiltin>}).map({$_<type>}).unique -> $builtin
{ say "    $builtin : 'state fold_config -> $builtin -> 'state -> 'state;"; }
for $adts.grep({$_<kind> ne $record && $_<kind> ne $variant}).map({$_<kind>}).unique -> $builtin
{ say "    $builtin : 'a . 'state fold_config -> 'a $builtin -> ('state -> 'a -> 'state) -> 'state -> 'state;"; }
say '  }';
say "(* info for adt $moduleName *)";
print "let rec whole_adt_info : unit -> Adt_info.adt = fun () -> [ ";
for $adts.list -> $t
{ print "info_$t<name> ; "; }
say "]";

# generic programming info about the nodes and fields
say "";
for $adts.list -> $t
{ for $t<ctorsOrFields>.list -> $c
  { say "(* info for field or ctor $t<name>.$c<name> *)";
    say "and info_$t<name>_$c<name> : Adt_info.ctor_or_field = \{";
    say "  name = \"$c<name>\";";
    say "  is_builtin = {$c<isBuiltin> ?? 'true' !! 'false'};";
    say "  type_ = \"$c<type>\";";
    say '}';
    say "";
    say "and continue_info_$t<name>_$c<name> : type qstate . qstate fold_config -> $c<type> -> qstate Adt_info.ctor_or_field_instance = fun visitor x -> \{";
    say "  cf = info_$t<name>_$c<name>;";
    say "  cf_continue = fun state -> fold_$t<name>_$c<name> visitor x state;";
    say '}';
    say ""; }
  say "(* info for node $t<name> *)";
  say "and info_$t<name> : Adt_info.node = \{";
  my $kind = do given $t<kind> {
    when $record { "Record" }
    when $variant { "Variant" }
    default { "Poly \"$_\"" }
  };
  say "  kind = $kind;";
  say "  declaration_name = \"$t<name>\";";
  print "  ctors_or_fields = [ ";
  for $t<ctorsOrFields>.list -> $c { print "info_$t<name>_$c<name> ; "; }
  say "];";
  say '}';
  say "";
  # TODO: factor out some of the common bits here.
  say "and continue_info_$t<name> : type qstate . qstate fold_config -> $t<name> -> qstate Adt_info.instance = fun visitor x ->";
  say '{';
  say "  instance_declaration_name = \"$t<name>\";";
  do given $t<kind> {
    when $record {
        say '  instance_kind = RecordInstance {';
        print "    fields = [ ";
        for $t<ctorsOrFields>.list -> $c { print "continue_info_$t<name>_$c<name> visitor x.$c<name> ; "; }
        say "  ];";
        say '};';
    }
    when $variant {
        say '  instance_kind = VariantInstance {';
        say "    constructor = (match x with";
        for $t<ctorsOrFields>.list -> $c { say "    | $c<name> v -> continue_info_$t<name>_$c<name> visitor v"; }
        say "  );";
        print "    variant = [ ";
        for $t<ctorsOrFields>.list -> $c { print "info_$t<name>_$c<name> ; "; }
        say "];";
        say '};';
    }
    default {
        say '  instance_kind = PolyInstance {';
        say "    poly = \"$_\";";
        print "    arguments = [";
        # TODO: sort by c<name> (currently we only have one-argument
        # polymorphic types so it happens to work but should be fixed.
        for $t<ctorsOrFields>.list -> $c { print "\"$c<type>\""; }
        say "];";
        print "    poly_continue = (fun state -> visitor.$_ visitor x (";
        print $t<ctorsOrFields>
            .map(-> $c { "(fun state x -> (continue_info_$t<name>_$c<name> visitor x).cf_continue state)" })
            .join(", ");
        say ") state);";
        say '};';
    }
  };
  say '}';
  say ""; }

# make the "continue" object
say "";
say '(* Curries the "visitor" argument to the folds (non-customizable traversal functions). *)';
say "and mk_continue_fold_map : type qstate . qstate fold_map_config -> qstate continue_fold_map = fun visitor ->";
say '  {';
for $adts.list -> $t
{ say "    $t<name> = fold_map_$t<name> visitor ;";
  for $t<ctorsOrFields>.list -> $c
  { say "    $t<name>_$c<name> = fold_map_$t<name>_$c<name> visitor ;"; } }
say '  }';
say "";

# fold_map functions
say "";
for $adts.list -> $t
{ say "and fold_map_$t<name> : type qstate . qstate fold_map_config -> $t<name> -> qstate -> ($t<newName> * qstate) = fun visitor x state ->";
  say "  let continue_fold_map : qstate continue_fold_map = mk_continue_fold_map visitor in";
  say "  let state = visitor.$t<name>_pre_state x (*(fun () -> whole_adt_info, info_$t<name>)*) state in";
  say "  let (new_x, state) = visitor.$t<name> x (*(fun () -> whole_adt_info, info_$t<name>)*) state continue_fold_map in";
  say "  let state = visitor.$t<name>_post_state x new_x (*(fun () -> whole_adt_info, info_$t<name>)*) state in";
  say "  (new_x, state)";
  say "";
  for $t<ctorsOrFields>.list -> $c
  { say "and fold_map_$t<name>_$c<name> : type qstate . qstate fold_map_config -> $c<type> -> qstate -> ($c<newType> * qstate) = fun visitor x state ->";
    say "  let continue_fold_map : qstate continue_fold_map = mk_continue_fold_map visitor in";
    say "  visitor.$t<name>_$c<name> x (*(fun () -> whole_adt_info, info_$t<name>, info_$t<name>_$c<name>)*) state continue_fold_map";
    say ""; } }


# fold functions
say "";
for $adts.list -> $t
{ say "and fold_$t<name> : type qstate . qstate fold_config -> $t<name> -> qstate -> qstate = fun visitor x state ->";
  # TODO: add a non-generic continue_fold.
  say '  let node_instance_info : qstate Adt_info.node_instance_info = {';
  say "    adt = whole_adt_info () ;";
  say "    node_instance = continue_info_$t<name> visitor x";
  say '  } in';
  # say "  let (new_x, state) = visitor.$t<name> x (fun () -> whole_adt_info, info_$t<name>) state continue_fold in";
  say "  visitor.generic node_instance_info state";
  say "";
  for $t<ctorsOrFields>.list -> $c
  { say "and fold_$t<name>_$c<name> : type qstate . qstate fold_config -> $c<type> -> qstate -> qstate = fun visitor x state ->";
    # say "  let ctor_or_field_instance_info : qstate Adt_info.ctor_or_field_instance_info = whole_adt_info (), info_$t<name>, continue_info_$t<name>_$c<name> visitor x in";
    if ($c<isBuiltin>) {
        say "  let state = (*visitor.generic_ctor_or_field ctor_or_field_instance_info*) visitor.$c<type> visitor x state in";
    } else {
        say "  let state = (*visitor.generic_ctor_or_field ctor_or_field_instance_info*) fold_$c<type> visitor x state in";
    }
    say "  state";
    # say "  visitor.$t<name>_$c<name> x (fun () -> whole_adt_info, info_$t<name>, info_$t<name>_$c<name>) state continue_fold";
    say ""; }
}

say "let no_op : 'a fold_map_config = \{";
for $adts.list -> $t
{ say "  $t<name> = (fun v (*_info*) state continue ->";
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
    print "      v -> fold_map_$t<kind> v state ( ";
    print ( "continue.$t<name>_$_<name>" for $t<ctorsOrFields>.list ).join(", ");
    say " )";
  }
  say "  );";
  say "  $t<name>_pre_state = (fun v (*_info*) state -> ignore v; state) ;";
  say "  $t<name>_post_state = (fun v new_v (*_info*) state -> ignore (v, new_v); state) ;";
  for $t<ctorsOrFields>.list -> $c
  { print "  $t<name>_$c<name> = (fun v (*_info*) state continue -> ";
    if ($c<isBuiltin>) {
      print "ignore continue; (v, state)";
    } else {
      print "continue.$c<type> v state";
    }
    say ") ;"; } }
say '}';
