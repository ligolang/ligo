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
$l = $l.list.cache;
my $statement_re = /^((\(\*\s+)?(open|include)\s|\[\@\@\@warning\s)/;
my $statements = $l.grep($statement_re);
$l             = $l.grep(none $statement_re);
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
   "newName"       => "{$name}__'" ,
   "kind"          => $kind ,
   "ctorsOrFields" => @(map -> ($cf, $type) {
       my $isBuiltin = (! $type) || (! $l.cache.first({ $_<name> eq $type }));
       {
           name      => $cf ,
           newName   => "{$cf}__'" ,
           isBuiltin => $isBuiltin ,
           type      => $type ,
           newType   => $isBuiltin ?? "$type" !! "{$type}__'"
       }
   }, @ctorsOrFields),
  }
}, @$l.cache).list;

# say $adts.perl;

# say $adts.perl ;

say "(* This is an auto-generated file. Do not edit. *)";

say "";
for $statements -> $statement {
  say "$statement"
}
say "open $moduleName";
say "module Adt_info = Adt_generator.Generic.Adt_info";

say "";
for $adts.kv -> $index, $t {
  my $typeOrAnd = $index == 0 ?? "type" !! "and";
  say "$typeOrAnd $t<newName> =";
  if ($t<kind> eq $variant) {
      for $t<ctorsOrFields>.list -> $c {
          given $c<type> {
              when '' { say "  | $c<newName>" }
              default { say "  | $c<newName> of $c<newType>" }
          }
      }
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
for $adts.list -> $t {
    say "type 'state continue_fold_map__$t<name> = \{";
        say "    node__$t<name> : $t<name> -> 'state -> ($t<newName> * 'state) ;";
    for $t<ctorsOrFields>.list -> $c
    { say "    $t<name>__$c<name> : {$c<type> || 'unit'} -> 'state -> ({$c<newType> || 'unit'} * 'state) ;" }
    say '  }';
}

say "type 'state continue_fold_map = \{";
for $adts.list -> $t {
    say "    $t<name> : 'state continue_fold_map__$t<name> ;";
}
say '  }';

say "";
for $adts.list -> $t
{ say "type 'state fold_map_config__$t<name> = \{";
  say "    node__$t<name> : $t<name> -> 'state -> 'state continue_fold_map -> ($t<newName> * 'state) ;"; # (*Adt_info.node_instance_info ->*)
  say "    node__$t<name>__pre_state : $t<name> -> 'state -> 'state ;"; # (*Adt_info.node_instance_info ->*)
  say "    node__$t<name>__post_state : $t<name> -> $t<newName> -> 'state -> 'state ;"; # (*Adt_info.node_instance_info ->*)
  for $t<ctorsOrFields>.list -> $c
  { say "    $t<name>__$c<name> : {$c<type> || 'unit'} -> 'state -> 'state continue_fold_map -> ({$c<newType> || 'unit'} * 'state) ;"; # (*Adt_info.ctor_or_field_instance_info ->*)
  }
  say '}' }

say "type 'state fold_map_config =";
say '  {';
for $adts.list -> $t
{ say "    $t<name> : 'state fold_map_config__$t<name>;" }
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
for $adts.map({ $_<ctorsOrFields> })[*;*].grep({$_<isBuiltin> && $_<type> ne ''}).map({$_<type>}).unique -> $builtin
{ say "    $builtin : 'state fold_config -> $builtin -> 'state -> 'state;"; }
for $adts.grep({$_<kind> ne $record && $_<kind> ne $variant}).map({$_<kind>}).unique -> $builtin
{ say "    $builtin : 'a . 'state fold_config -> 'a $builtin -> ('state -> 'a -> 'state) -> 'state -> 'state;"; }
say '  }';

say "";
say 'type blahblah = {';
for $adts.list -> $t
{ say "  fold__$t<name> : 'state . blahblah -> 'state fold_config -> $t<name> -> 'state -> 'state;";
  for $t<ctorsOrFields>.list -> $c
  { say "  fold__$t<name>__$c<name> : 'state . blahblah -> 'state fold_config -> { $c<type> || 'unit' } -> 'state -> 'state;"; } }
say '}';

# generic programming info about the nodes and fields
say "";
for $adts.list -> $t
{ for $t<ctorsOrFields>.list -> $c
  { say "(* info for field or ctor $t<name>.$c<name> *)";
    say "let info__$t<name>__$c<name> : Adt_info.ctor_or_field = \{";
    say "  name = \"$c<name>\";";
    say "  is_builtin = {$c<isBuiltin> ?? 'true' !! 'false'};";
    say "  type_ = \"$c<type>\";";
    say '}';
    say "";
    say "let continue_info__$t<name>__$c<name> : type qstate . blahblah -> qstate fold_config -> {$c<type> || 'unit'} -> qstate Adt_info.ctor_or_field_instance = fun blahblah visitor x -> \{";
    say "  cf = info__$t<name>__$c<name>;";
    say "  cf_continue = fun state -> blahblah.fold__$t<name>__$c<name> blahblah visitor x state;";
    say '}';
    say ""; }
  say "(* info for node $t<name> *)";
  say "let info__$t<name> : Adt_info.node = \{";
  my $kind = do given $t<kind> {
    when $record { "Record" }
    when $variant { "Variant" }
    default { "Poly \"$_\"" }
  };
  say "  kind = $kind;";
  say "  declaration_name = \"$t<name>\";";
  print "  ctors_or_fields = [ ";
  for $t<ctorsOrFields>.list -> $c { print "info__$t<name>__$c<name> ; "; }
  say "];";
  say '}';
  say "";
  # TODO: factor out some of the common bits here.
  say "let continue_info__$t<name> : type qstate . blahblah -> qstate fold_config -> $t<name> -> qstate Adt_info.instance = fun blahblah visitor x ->";
  say '{';
  say "  instance_declaration_name = \"$t<name>\";";
  do given $t<kind> {
    when $record {
        say '  instance_kind = RecordInstance {';
        print "    fields = [ ";
        for $t<ctorsOrFields>.list -> $c { print "continue_info__$t<name>__$c<name> blahblah visitor x.$c<name> ; "; }
        say "  ];";
        say '};';
    }
    when $variant {
        say '  instance_kind = VariantInstance {';
        say "    constructor = (match x with";
        for $t<ctorsOrFields>.list -> $c { say "    | $c<name> { $c<type> ?? 'v ' !! '' }-> continue_info__$t<name>__$c<name> blahblah visitor { $c<type> ?? 'v' !! '()' }"; }
        say "  );";
        print "    variant = [ ";
        for $t<ctorsOrFields>.list -> $c { print "info__$t<name>__$c<name> ; "; }
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
            .map(-> $c { "(fun state x -> (continue_info__$t<name>__$c<name> blahblah visitor x).cf_continue state)" })
            .join(", ");
        say ") state);";
        say '};';
    }
  };
  say '}';
  say ""; }

say "";
say "(* info for adt $moduleName *)";
print "let whole_adt_info : unit -> Adt_info.adt = fun () -> [ ";
for $adts.list -> $t
{ print "info__$t<name> ; "; }
say "]";

# fold functions
say "";
for $adts.list -> $t
{ say "let fold__$t<name> : type qstate . blahblah -> qstate fold_config -> $t<name> -> qstate -> qstate = fun blahblah visitor x state ->";
  # TODO: add a non-generic continue_fold.
  say '  let node_instance_info : qstate Adt_info.node_instance_info = {';
  say "    adt = whole_adt_info () ;";
  say "    node_instance = continue_info__$t<name> blahblah visitor x";
  say '  } in';
  # say "  let (new_x, state) = visitor.$t<name>.node__$t<name> x (fun () -> whole_adt_info, info__$t<name>) state continue_fold in";
  say "  visitor.generic node_instance_info state";
  say "";
  for $t<ctorsOrFields>.list -> $c
  { say "let fold__$t<name>__$c<name> : type qstate . blahblah -> qstate fold_config -> { $c<type> || 'unit' } -> qstate -> qstate = fun blahblah { $c<type> ?? 'visitor x' !! '_visitor ()' } state ->";
    # say "  let ctor_or_field_instance_info : qstate Adt_info.ctor_or_field_instance_info = whole_adt_info (), info__$t<name>, continue_info__$t<name>__$c<name> visitor x in";
    if ($c<type> eq '') {
        # nothing to do, this constructor has no arguments.
        say "  state";
    } elsif ($c<isBuiltin>) {
        say "  ignore blahblah; visitor.$c<type> visitor x state"; # (*visitor.generic_ctor_or_field ctor_or_field_instance_info*) 
    } else {
        say "  blahblah.fold__$c<type> blahblah visitor x state"; # (*visitor.generic_ctor_or_field ctor_or_field_instance_info*)
    }
    # say "  visitor.$t<name>.$t<name>__$c<name> x (fun () -> whole_adt_info, info__$t<name>, info__$t<name>__$c<name>) state continue_fold";
    say ""; }
}

say "";
say 'let blahblah : blahblah = {';
for $adts.list -> $t
{ say "  fold__$t<name>;";
  for $t<ctorsOrFields>.list -> $c
  { say "  fold__$t<name>__$c<name>;" } }
say '}';

say "";
for $adts.list -> $t
{ say "let fold__$t<name> : type qstate . qstate fold_config -> $t<name> -> qstate -> qstate = fun visitor x state -> fold__$t<name> blahblah visitor x state";
  for $t<ctorsOrFields>.list -> $c
  { say "let fold__$t<name>__$c<name> : type qstate . qstate fold_config -> { $c<type> || 'unit' } -> qstate -> qstate = fun visitor x state -> fold__$t<name>__$c<name> blahblah visitor x state" } }


say "";
say "type 'state mk_continue_fold_map = \{";
say "  fn : 'state mk_continue_fold_map -> 'state fold_map_config -> 'state continue_fold_map";
say '}';


# fold_map functions
say "";
for $adts.list -> $t
{ say "let _fold_map__$t<name> : type qstate . qstate mk_continue_fold_map -> qstate fold_map_config -> $t<name> -> qstate -> ($t<newName> * qstate) = fun mk_continue_fold_map visitor x state ->";
  say "  let continue_fold_map : qstate continue_fold_map = mk_continue_fold_map.fn mk_continue_fold_map visitor in";
  say "  let state = visitor.$t<name>.node__$t<name>__pre_state x state in"; # (*(fun () -> whole_adt_info, info__$t<name>)*)
  say "  let (new_x, state) = visitor.$t<name>.node__$t<name> x state continue_fold_map in"; # (*(fun () -> whole_adt_info, info__$t<name>)*)
  say "  let state = visitor.$t<name>.node__$t<name>__post_state x new_x state in"; # (*(fun () -> whole_adt_info, info__$t<name>)*)
  say "  (new_x, state)";
  say "";
  for $t<ctorsOrFields>.list -> $c
  { say "let _fold_map__$t<name>__$c<name> : type qstate . qstate mk_continue_fold_map -> qstate fold_map_config -> { $c<type> || 'unit' } -> qstate -> ({ $c<newType> || 'unit' } * qstate) = fun mk_continue_fold_map visitor x state ->";
    say "  let continue_fold_map : qstate continue_fold_map = mk_continue_fold_map.fn mk_continue_fold_map visitor in";
    say "  visitor.$t<name>.$t<name>__$c<name> x state continue_fold_map"; # (*(fun () -> whole_adt_info, info__$t<name>, info__$t<name>__$c<name>)*)
    say ""; } }

# make the "continue" object
say "";
say '(* Curries the "visitor" argument to the folds (non-customizable traversal functions). *)';
say "let mk_continue_fold_map : 'stateX . 'stateX mk_continue_fold_map = \{ fn = fun self visitor ->";
say '  {';
for $adts.list -> $t
{ say "    $t<name> = \{";
  say "        node__$t<name> = (fun x state -> _fold_map__$t<name> self visitor x state) ;";
  for $t<ctorsOrFields>.list -> $c
  { say "        $t<name>__$c<name> = (fun x state -> _fold_map__$t<name>__$c<name> self visitor x state) ;"; }
  say '    };' }
say '  }';
say '}';
say "";

# fold_map functions : tying the knot
say "";
for $adts.list -> $t
{ say "let fold_map__$t<name> : type qstate . qstate fold_map_config -> $t<name> -> qstate -> ($t<newName> * qstate) = fun visitor x state -> _fold_map__$t<name> mk_continue_fold_map visitor x state";
  for $t<ctorsOrFields>.list -> $c
  { say "let fold_map__$t<name>__$c<name> : type qstate . qstate fold_map_config -> { $c<type> || 'unit' } -> qstate -> ({ $c<newType> || 'unit' } * qstate) = fun visitor x state -> _fold_map__$t<name>__$c<name> mk_continue_fold_map visitor x state"; } }


say "let no_op : 'state . 'state fold_map_config = \{";
for $adts.list -> $t
{ say "  $t<name> = \{";
  say "  node__$t<name> = (fun v state continue ->"; # (*_info*)
  say "    match v with";
  if ($t<kind> eq $variant) {
    for $t<ctorsOrFields>.list -> $c
    { given $c<type> {
        when '' { say "    | $c<name> -> let ((), state) = continue.$t<name>.$t<name>__$c<name> () state in ($c<newName>, state)"; }
        default { say "    | $c<name> v -> let (v, state) = continue.$t<name>.$t<name>__$c<name> v state in ($c<newName> v, state)"; } } }
  } elsif ($t<kind> eq $record) {
    print '      { ';
    for $t<ctorsOrFields>.list -> $f
    { print "$f<name>; "; }
    say "} ->";
    for $t<ctorsOrFields>.list -> $f
    { say "      let ($f<newName>, state) = continue.$t<name>.$t<name>__$f<name> $f<name> state in"; }
    print '      ({ ';
    for $t<ctorsOrFields>.list -> $f
    { print "$f<newName>; "; }
    say '}, state)';
  } else {
    print "      v -> fold_map__$t<kind> v state ( ";
    print ( "continue.$t<name>.$t<name>__$_<name>" for $t<ctorsOrFields>.list ).join(", ");
    say " )";
  }
  say "  );";
  say "  node__$t<name>__pre_state = (fun v state -> ignore v; state) ;"; # (*_info*)
  say "  node__$t<name>__post_state = (fun v new_v state -> ignore (v, new_v); state) ;"; # (*_info*)
  for $t<ctorsOrFields>.list -> $c
  { print "  $t<name>__$c<name> = (fun v state continue -> "; # (*_info*)
    if ($c<isBuiltin>) {
      print "ignore continue; (v, state)";
    } else {
      print "continue.$c<type>.node__$c<type> v state";
    }
    say ") ;"; }
  say '  };' }
say '}';
