#!/usr/bin/env perl6
use v6.c;
use strict;
use worries;
use lib '.';
use Parser;

# TODO: find a way to do mutual recursion between the produced file and some #include-y-thingy
# TODO: make an .mli
# TODO: shorthand for `foo list` etc. in field and constructor types
# TODO: error when reserved names are used ("state", … please list them here)

my $inputADTfile    = @*ARGS[0];
my $folder_filename = @*ARGS[1];

my ($adts, $moduleName, $record, $variant, $statements, $typeclasses) = parse($inputADTfile);

# Auto-generated fold functions
$*OUT = open $folder_filename, :w;
{
    say "(* This is an auto-generated file. Do not edit. *)";
    say "";
    for $statements -> $statement { say "$statement" }
    say "open $moduleName;;";

    say "  (* must be provided by one of the open or include statements: *)";
    say "  module CheckFolderInputSignature = struct";
    for $adts.grep({$_<kind> ne $record && $_<kind> ne $variant}).map({$_<kind>}).unique -> $poly
    { say "    let make__$poly : type a b . (a -> b option) -> a $poly -> b $poly option = make__$poly;;"; }
    say "  end";

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
    say "  type whatever =";
    say "  | NoArgument (* supplied to make constructors with no arguments *)";
    # look for builtins, filtering out the "implicit unit-like fake argument of emtpy constructors" (represented by '')
    for $adts.map({ $_<ctorsOrFields> })[*;*].grep({$_<isBuiltin> && $_<type> ne ''}).map({$_<type>}).unique -> $builtin
    { say "  | Whatever_{tc $builtin} of $builtin"; }
    for $adts.list -> $t
    { say "  | Whatever_{tc $t<name>} of $t<name>" }

    say "  type make_poly =";
    # look for built-in polymorphic types
    for $adts.grep({$_<kind> ne $record && $_<kind> ne $variant}).map({$_<kind>}).unique -> $poly
    { say "  | Make_{tc $poly} of (whatever $poly -> whatever option)"; }

    say "";
    say "  module Adt_info = Adt_generator.Generic.Adt_info (struct";
    say "    type nonrec ('in_state , 'out_state , 'adt_info_node_instance_info) fold_config = ('in_state , 'out_state , 'adt_info_node_instance_info) _fold_config;;";
    say "    type nonrec whatever = whatever;;";
    say "    type nonrec make_poly = make_poly;;";
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
        if ($t<kind> eq $variant) {
	    say "  let info__$t<name>__$c<name> : Adt_info.constructor_type = \{";
	    say "      ctor = \{";
	    say "        name = \"$c<name>\";";
	    say "        is_builtin = {$c<isBuiltin> ?? 'true' !! 'false'};";
	    say "        type_ = \"$c<type>\";";
	    say "      \};";
	    if ($c<type> eq '') {
		# this constructor has no arguments.
		say "      make_ctor = (function NoArgument -> Some (Whatever_{tc $t<name>} $c<name>) | _ -> None);";
	    } else {
		say "      make_ctor = (function Whatever_{tc $c<type>} v -> Some (Whatever_{tc $t<name>} ($c<name> v)) | _ -> None);";
	    }
	    say '    };;';
        } else {
	    say "  let info__$t<name>__$c<name> : Adt_info.ctor_or_field = \{";
	    say "        name = \"$c<name>\";";
	    say "        is_builtin = {$c<isBuiltin> ?? 'true' !! 'false'};";
	    say "        type_ = \"$c<type>\";";
	    say '    };;';
	}
        # say "";
        say "  let continue_info__$t<name>__$c<name> : type in_qstate out_qstate . the_folds -> (in_qstate , out_qstate) fold_config -> {$c<type> || 'unit'} -> (in_qstate, out_qstate) Adt_info.ctor_or_field_instance = fun the_folds visitor x -> \{";
        my $dotctor = ($t<kind> eq $variant) ?? ".ctor" !! ""; # TODO: give the full constructor info with its "make" function instead of extracting the .ctor part.
        say "        cf = info__$t<name>__$c<name>$dotctor;";
        say "        cf_continue = (fun state -> the_folds.fold__$t<name>__$c<name> the_folds visitor state x);";
        say "        cf_new_fold = (fun visitor state -> the_folds.fold__$t<name>__$c<name> the_folds visitor state x);";
        say '      };;';
        # say "";
      }
      say "  (* info for node $t<name> *)";
      say "  let info__$t<name> : Adt_info.node = \{";
      print "      kind = ";
      do given $t<kind> {
          when $record {
	      say "RecordType \{";
	      say "        fields = [";
	      for $t<ctorsOrFields>.list -> $f {
		  say "          info__$t<name>__$f<name>;";
	      }
	      say "        ];";
	      say "        make_record = (fun r -> match Adt_generator.Common.sorted_bindings r with";
	      say "        | [";
	      for $t<ctorsOrFields>.list.sort({$_<name>}) -> $f {
		  say "          (\"$f<name>\" , Whatever_{tc $f<type>} $f<name>) ;";
	      }	    
	      say "          ] -> Some (Whatever_{tc $t<name>} \{";
	      for $t<ctorsOrFields>.list -> $f { say "          $f<name> ;"; }
	      say "        \})";
	      say "        | _ -> None)";
	      say "      \};"; }
          when $variant {
	      say "VariantType \{";
	      print "        constructors = [ ";
	      for $t<ctorsOrFields>.list -> $c { print "info__$t<name>__$c<name> ; "; }
	      say "];";
	      say "      \};"; }
          default {
	      say "PolyType \{";
	      say "        poly_name = \"$_\";";
	      print "        make_poly = Make_{tc $_} (fun p -> match make__$_ ";
	      for $t<ctorsOrFields>.list -> $a { print "(function Whatever_{tc $a<type>} v -> Some v | _ -> None)"; }
	      say " p with Some p -> Some (Whatever_{tc $t<name>} p) | None -> None);";
	      say "      \};"; }
      };
      say "      declaration_name = \"$t<name>\";";
      say '    };;';
      # say "";
      # TODO: factor out some of the common bits here.
      say "  let continue_info__$t<name> : type in_qstate out_qstate . the_folds -> (in_qstate , out_qstate) fold_config -> $t<name> -> (in_qstate , out_qstate) Adt_info.instance = fun the_folds visitor x ->";
      say '    {';
      say "      instance_declaration_name = \"$t<name>\";";
      do given $t<kind> {
          when $record {
              say '      instance_kind = RecordInstance {';
              print "          field_instances = [ ";
              for $t<ctorsOrFields>.list -> $c { print "continue_info__$t<name>__$c<name> the_folds visitor x.$c<name> ; "; }
              say "];";
              say '        };';
          }
          when $variant {
              say "      instance_kind =";
              say '        VariantInstance {';
              say "            constructor =";
              say "              (match x with";
              for $t<ctorsOrFields>.list -> $c { say "               | $c<name> { $c<type> ?? 'v ' !! '' }-> continue_info__$t<name>__$c<name> the_folds visitor { $c<type> ?? 'v' !! '()' }"; }
              say "              );";
              print "            variant = [ ";
              for $t<ctorsOrFields>.list -> $c { print "info__$t<name>__$c<name>.ctor ; "; } # TODO: give the full constructor info with its "make" function.
              say "];";
              say '          };';
          }
          default {
              say "      instance_kind =";
              say '        PolyInstance {';
              say "            poly = \"$_\";";
              print "            arguments = [";
              for $t<ctorsOrFields>.list.sort({$_<name>}) -> $c { print "\"$c<type>\""; }
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
    say "  let whole_adt_info : unit -> Adt_info.adt = fun () ->";
    print "  match RedBlackTrees.PolyMap.from_list ~cmp:String.compare [ ";
    for $adts.list -> $t
    { print "\"$t<name>\" , info__$t<name> ; "; }
    say "] with Some x -> x | None -> failwith \"Internal error: duplicate nodes in ADT info\";;";

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