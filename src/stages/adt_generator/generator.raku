#!/usr/bin/env perl6
use v6.c;
use strict;
use worries;

my $moduleName = @*ARGS[0].subst(/\.ml$/, '').samecase("A_");
my $variant = "_ _variant";
my $record = "_ _ record";
sub poly { $^type_name }

my $l = @*ARGS[0].IO.lines;
$l = $l.map(*.subst: /(^\s+|\s+$)/, "");
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
say "type ('a,'err) monad = ('a) Simple_utils.Trace.result;;";
say "let (>>?) v f = Simple_utils.Trace.bind f v;;";
say "let return v = Simple_utils.Trace.ok v;;";
say "open $moduleName;;";

say "";
say "(* must be provided by one of the open or include statements: *)";
for $adts.grep({$_<kind> ne $record && $_<kind> ne $variant}).map({$_<kind>}).unique -> $poly
{ say "let fold_map__$poly : type a new_a state . (state -> a -> (state * new_a, _) monad) -> state -> a $poly -> (state * new_a $poly , _) monad = fold_map__$poly;;"; }

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
      say "";
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
say ";;";

say "";
for $adts.list -> $t {
    say "type ('state, 'err) continue_fold_map__$t<name> = \{";
        say "    node__$t<name> : 'state -> $t<name> -> ('state * $t<newName> , 'err) monad ;";
    for $t<ctorsOrFields>.list -> $c
    { say "    $t<name>__$c<name> : 'state -> {$c<type> || 'unit'} -> ('state * {$c<newType> || 'unit'} , 'err) monad ;" }
    say '  };;';
}

say "type ('state , 'err) continue_fold_map = \{";
for $adts.list -> $t {
    say "    $t<name> : ('state , 'err) continue_fold_map__$t<name> ;";
}
say '  };;';

say "";
for $adts.list -> $t
{ say "type ('state , 'err) fold_map_config__$t<name> = \{";
  say "    node__$t<name> : 'state -> $t<name> -> ('state, 'err) continue_fold_map -> ('state * $t<newName> , 'err) monad ;"; # (*Adt_info.node_instance_info ->*)
  say "    node__$t<name>__pre_state : 'state -> $t<name> -> ('state, 'err) monad ;"; # (*Adt_info.node_instance_info ->*)
  say "    node__$t<name>__post_state : 'state -> $t<name> -> $t<newName> -> ('state, 'err) monad ;"; # (*Adt_info.node_instance_info ->*)
  for $t<ctorsOrFields>.list -> $c
  { say "    $t<name>__$c<name> : 'state -> {$c<type> || 'unit'} -> ('state, 'err) continue_fold_map -> ('state * {$c<newType> || 'unit'}  , 'err) monad ;"; # (*Adt_info.ctor_or_field_instance_info ->*)
  }
  say '};;' }

say "type ('state, 'err) fold_map_config =";
say '  {';
for $adts.list -> $t
{ say "    $t<name> : ('state, 'err) fold_map_config__$t<name>;" }
say '  };;';

say "";
say "module StringMap = Map.Make(String);;";
say "(* generic folds for nodes *)";
say "type 'state generic_continue_fold_node = \{";
say "  continue                 : 'state -> 'state ;";
say "  (* generic folds for each field *)";
say "  continue_ctors_or_fields : ('state -> 'state) StringMap.t ;";
say '};;';
say "(* map from node names to their generic folds *)";
say "type 'state generic_continue_fold = ('state generic_continue_fold_node) StringMap.t;;";
say "";
say "type ('state , 'adt_info_node_instance_info) _fold_config =";
say '  {';
say "    generic : 'state -> 'adt_info_node_instance_info -> 'state;";
# look for builtins, filtering out the "implicit unit-like fake argument of emtpy constructors" (represented by '')
for $adts.map({ $_<ctorsOrFields> })[*;*].grep({$_<isBuiltin> && $_<type> ne ''}).map({$_<type>}).unique -> $builtin
{ say "    $builtin : ('state , 'adt_info_node_instance_info) _fold_config -> 'state -> $builtin -> 'state;"; }
# look for built-in polymorphic types
for $adts.grep({$_<kind> ne $record && $_<kind> ne $variant}).map({$_<kind>}).unique -> $poly
{ say "    $poly : 'a . ('state , 'adt_info_node_instance_info) _fold_config -> ('state -> 'a -> 'state) -> 'state -> 'a $poly -> 'state;"; }
say '  };;';
say "module Arg = struct";
say "  type nonrec ('state , 'adt_info_node_instance_info) fold_config = ('state , 'adt_info_node_instance_info) _fold_config;;";
say "end;;";
say "module Adt_info = Adt_generator.Generic.Adt_info (Arg);;";
say "include Adt_info;;";
say "type 'state fold_config = ('state , 'state Adt_info.node_instance_info) _fold_config;;";

say "";
say 'type blahblah = {';
for $adts.list -> $t
{ say "  fold__$t<name> : 'state . blahblah -> 'state fold_config -> 'state -> $t<name> -> 'state;";
  for $t<ctorsOrFields>.list -> $c
  { say "  fold__$t<name>__$c<name> : 'state . blahblah -> 'state fold_config -> 'state -> { $c<type> || 'unit' } -> 'state;"; } }
say '};;';

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
    say "  cf_continue = (fun state -> blahblah.fold__$t<name>__$c<name> blahblah visitor state x);";
    say "  cf_new_fold = (fun visitor state -> blahblah.fold__$t<name>__$c<name> blahblah visitor state x);";
    say '};;';
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
  say '};;';
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
        print "    poly_continue = (fun state -> visitor.$_ visitor (";
        print $t<ctorsOrFields>
            .map(-> $c { "(fun state x -> (continue_info__$t<name>__$c<name> blahblah visitor x).cf_continue state)" })
            .join(", ");
        say ") state x);";
        say '};';
    }
  };
  say '};;';
  say ""; }

say "";
say "(* info for adt $moduleName *)";
print "let whole_adt_info : unit -> Adt_info.adt = fun () -> [ ";
for $adts.list -> $t
{ print "info__$t<name> ; "; }
say "];;";

# fold functions
say "";
for $adts.list -> $t
{ say "let fold__$t<name> : type qstate . blahblah -> qstate fold_config -> qstate -> $t<name> -> qstate = fun blahblah visitor state x ->";
  # TODO: add a non-generic continue_fold.
  say '  let node_instance_info : qstate Adt_info.node_instance_info = {';
  say "    adt = whole_adt_info () ;";
  say "    node_instance = continue_info__$t<name> blahblah visitor x";
  say '  } in';
  # say "  let (state, new_x) = visitor.$t<name>.node__$t<name> x (fun () -> whole_adt_info, info__$t<name>) state continue_fold in";
  say "  visitor.generic state node_instance_info;;";
  say "";
  for $t<ctorsOrFields>.list -> $c
  { say "let fold__$t<name>__$c<name> : type qstate . blahblah -> qstate fold_config -> qstate -> { $c<type> || 'unit' } -> qstate = fun blahblah { $c<type> ?? 'visitor' !! '_visitor' } state { $c<type> ?? 'x' !! '()' } ->";
    # say "  let ctor_or_field_instance_info : qstate Adt_info.ctor_or_field_instance_info = whole_adt_info (), info__$t<name>, continue_info__$t<name>__$c<name> visitor x in";
    if ($c<type> eq '') {
        # nothing to do, this constructor has no arguments.
        say "  ignore blahblah; state;;";
    } elsif ($c<isBuiltin>) {
        say "  ignore blahblah; visitor.$c<type> visitor state x;;"; # (*visitor.generic_ctor_or_field ctor_or_field_instance_info*) 
    } else {
        say "  blahblah.fold__$c<type> blahblah visitor state x;;"; # (*visitor.generic_ctor_or_field ctor_or_field_instance_info*)
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
say '};;';

# Tying the knot
say "";
for $adts.list -> $t
{ say "let fold__$t<name> : type qstate . qstate fold_config -> qstate -> $t<name> -> qstate = fun visitor state x -> fold__$t<name> blahblah visitor state x;;";
  for $t<ctorsOrFields>.list -> $c
  { say "let fold__$t<name>__$c<name> : type qstate . qstate fold_config -> qstate -> { $c<type> || 'unit' } -> qstate = fun visitor state x -> fold__$t<name>__$c<name> blahblah visitor state x;;" } }


say "";
say "type ('state, 'err) mk_continue_fold_map = \{";
say "  fn : ('state,'err) mk_continue_fold_map -> ('state, 'err) fold_map_config -> ('state , 'err) continue_fold_map";
say '};;';


# fold_map functions
say "";
for $adts.list -> $t
{ say "let _fold_map__$t<name> : type qstate err . (qstate,err) mk_continue_fold_map -> (qstate,err) fold_map_config -> qstate -> $t<name> -> (qstate * $t<newName>, err) monad = fun mk_continue_fold_map visitor state x ->";
  say "  let continue_fold_map : (qstate,err) continue_fold_map = mk_continue_fold_map.fn mk_continue_fold_map visitor in";
  say "  visitor.$t<name>.node__$t<name>__pre_state state x >>? fun state ->"; # (*(fun () -> whole_adt_info, info__$t<name>)*)
  say "  visitor.$t<name>.node__$t<name> state x continue_fold_map >>? fun (state, new_x) ->"; # (*(fun () -> whole_adt_info, info__$t<name>)*)
  say "  visitor.$t<name>.node__$t<name>__post_state state x new_x >>? fun state ->"; # (*(fun () -> whole_adt_info, info__$t<name>)*)
  say "  return (state, new_x);;";
  say "";
  for $t<ctorsOrFields>.list -> $c
  { say "let _fold_map__$t<name>__$c<name> : type qstate err . (qstate,err) mk_continue_fold_map -> (qstate,err) fold_map_config -> qstate -> { $c<type> || 'unit' } -> (qstate * { $c<newType> || 'unit' }, err) monad = fun mk_continue_fold_map visitor state x ->";
    say "  let continue_fold_map : (qstate,err) continue_fold_map = mk_continue_fold_map.fn mk_continue_fold_map visitor in";
    say "  visitor.$t<name>.$t<name>__$c<name> state x continue_fold_map;;"; # (*(fun () -> whole_adt_info, info__$t<name>, info__$t<name>__$c<name>)*)
    say ""; } }

# make the "continue" object
say "";
say '(* Curries the "visitor" argument to the folds (non-customizable traversal functions). *)';
say "let mk_continue_fold_map : 'state 'err . ('state,'err) mk_continue_fold_map = \{ fn = fun self visitor ->";
say '  {';
for $adts.list -> $t
{ say "    $t<name> = \{";
  say "        node__$t<name> = (fun state x -> _fold_map__$t<name> self visitor state x) ;";
  for $t<ctorsOrFields>.list -> $c
  { say "        $t<name>__$c<name> = (fun state x -> _fold_map__$t<name>__$c<name> self visitor state x) ;"; }
  say '    };' }
say '  }';
say '};;';
say "";

# fold_map functions : tying the knot
say "";
for $adts.list -> $t
{ say "let fold_map__$t<name> : type qstate err . (qstate,err) fold_map_config -> qstate -> $t<name> -> (qstate * $t<newName>,err) monad =";
  say "  fun visitor state x -> _fold_map__$t<name> mk_continue_fold_map visitor state x;;";
  for $t<ctorsOrFields>.list -> $c
  { say "let fold_map__$t<name>__$c<name> : type qstate err . (qstate,err) fold_map_config -> qstate -> { $c<type> || 'unit' } -> (qstate * { $c<newType> || 'unit' },err) monad =";
    say "  fun visitor state x -> _fold_map__$t<name>__$c<name> mk_continue_fold_map visitor state x;;"; } }


for $adts.list -> $t
{
   say "let no_op_node__$t<name> : type state . state -> $t<name> -> (state,_) continue_fold_map -> (state * $t<newName>,_) monad =";
  say "  fun state v continue ->"; # (*_info*)
  say "    match v with";
  if ($t<kind> eq $variant) {
    for $t<ctorsOrFields>.list -> $c
    { given $c<type> {
        when '' { say "    | $c<name> -> continue.$t<name>.$t<name>__$c<name> state () >>? fun (state , ()) -> return (state , $c<newName>)"; }
        default { say "    | $c<name> v -> continue.$t<name>.$t<name>__$c<name> state v >>? fun (state , v) -> return (state , $c<newName> v)"; } } }
  } elsif ($t<kind> eq $record) {
    print '      { ';
    for $t<ctorsOrFields>.list -> $f
    { print "$f<name>; "; }
    say "} ->";
    for $t<ctorsOrFields>.list -> $f
    { say "      continue.$t<name>.$t<name>__$f<name> state $f<name> >>? fun (state , $f<newName>) ->"; }
    print '      return (state , ({ ';
    for $t<ctorsOrFields>.list -> $f
    { print "$f<newName>; "; }
    say "\} : $t<newName>))";
  } else {
    print "      v -> fold_map__$t<kind> ( ";
    print ( "continue.$t<name>.$t<name>__$_<name>" for $t<ctorsOrFields>.list ).join(", ");
    say " ) state v;;";
  }
}

for $adts.list -> $t
{ say "let no_op__$t<name> : type state . (state,_) fold_map_config__$t<name> = \{";
  say "  node__$t<name> = no_op_node__$t<name>;";
  say "  node__$t<name>__pre_state = (fun state v -> ignore v; return state) ;"; # (*_info*)
  say "  node__$t<name>__post_state = (fun state v new_v -> ignore (v, new_v); return state) ;"; # (*_info*)
  for $t<ctorsOrFields>.list -> $c
  { print "  $t<name>__$c<name> = (fun state v continue -> "; # (*_info*)
    if ($c<isBuiltin>) {
      print "ignore continue; return (state , v)";
    } else {
      print "continue.$c<type>.node__$c<type> state v";
    }
    say ") ;"; }
  say '  }' }

say "let no_op : type state . (state,_) fold_map_config = \{";
for $adts.list -> $t
{ say "  $t<name> = no_op__$t<name>;" }
say '};;';

say "";
for $adts.list -> $t
{ say "let with__$t<name> : _ -> _ fold_map_config -> _ fold_map_config = (fun node__$t<name> op -> \{ op with $t<name> = \{ op.$t<name> with node__$t<name> \} \});;";
  say "let with__$t<name>__pre_state : _ -> _ fold_map_config -> _ fold_map_config = (fun node__$t<name>__pre_state op -> \{ op with $t<name> = \{ op.$t<name> with node__$t<name>__pre_state \} \});;";
  say "let with__$t<name>__post_state : _ -> _ fold_map_config -> _ fold_map_config = (fun node__$t<name>__post_state op -> \{ op with $t<name> = \{ op.$t<name> with node__$t<name>__post_state \} \});;";
  for $t<ctorsOrFields>.list -> $c
  { say "let with__$t<name>__$c<name> : _ -> _ fold_map_config -> _ fold_map_config = (fun $t<name>__$c<name> op -> \{ op with $t<name> = \{ op.$t<name> with $t<name>__$c<name> \} \});;"; } }
