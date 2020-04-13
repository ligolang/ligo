#!/usr/bin/env perl
use strict;
use warnings;
use 5.010;
use Data::Dumper; $Data::Dumper::Useqq = 1; # use double quotes when dumping (we have a few "prime'" names)
sub enumerate { my $i = 0; [map { [ $i++, $_ ] } @{$_[0]}] }

my $moduleName = "A";
my $variant = "_ _variant";
my $record = "_ _ record"; my $true = 1; my $false = 0;
sub poly { $_[0] }
my $adts_raw = [
    # typename, kind, fields_or_ctors
    ["root", $variant, [
         # ctor, builtin?, type
         ["A", $false, "rootA"],
         ["B", $false, "rootB"],
         ["C", $true, "string"],
     ]],
    ["a", $record, [
         # field, builtin?, type
         ["a1", $false, "ta1"],
         ["a2", $false, "ta2"],
     ]],
    ["ta1", $variant, [
         ["X", $false, "root"],
         ["Y", $false, "ta2"],
     ]],
    ["ta2", $variant, [
         ["Z", $false, "ta2"],
         ["W", $true, "unit"],
     ]],
    # polymorphic type
    ["rootA", poly("list"),
     [
      # Position (0..n-1), builtin?, type argument
      [0, $false, "a"]
     ]],
    ["rootB", poly("list"),
     [
      # Position (0..n-1), builtin?, type argument
      [0, $true, "int"]
     ]],
    ];




my $adts = [map {
  my ($name , $kind, $ctorsOrFields) = @$_;
  {
   "name"          => $name ,
   "newName"       => "${name}'" ,
   "kind"          => $kind ,
   "ctorsOrFields" => [map {
       my ($cf, $isBuiltin, $type) = @$_;
       {
           name      => $cf ,
           newName   => "${cf}'" ,
           isBuiltin => $isBuiltin ,
           type      => $type ,
           newType   => $isBuiltin ? $type : "${type}'"
       }
   } @$ctorsOrFields],
  }
} @$adts_raw];

# print Dumper $adts ;

say "(* This is an auto-generated file. Do not edit. *)";

say "";
say "open ${moduleName}";

say "";
foreach (@{enumerate($adts)}) {
  my ($index, $t) = @$_;
  my %t = %$t;
  my $typeOrAnd = $index == 0 ? "type" : "and";
  say "${typeOrAnd} $t{newName} =";
  if ($t{kind} eq $variant) {
      foreach (@{$t{ctorsOrFields}}) {
          my %c = %$_;
          say "  | $c{newName} of $c{newType}"
      }
  }
  elsif ($t{kind} eq $record) {
      say "  {";
      foreach (@{$t{ctorsOrFields}}) {
          my %f = %$_;
          say "    $f{newName} : $f{newType} ;";
      }
      say "  }";
  } else {
      print "  ";
      foreach (@{$t{ctorsOrFields}}) {
          my %a = %$_;
          print "$a{newType} ";
      }
      print "$t{kind}";
      say "";
  }
}

say "";
say "type 'state continue_fold =";
say "  {";
foreach (@$adts) {
    my %t = %$_;
    say "    $t{name} : $t{name} -> 'state -> ($t{newName} * 'state) ;";
    foreach (@{$t{ctorsOrFields}}) {
        my %c = %$_;
        say "    $t{name}_$c{name} : $c{type} -> 'state -> ($c{newType} * 'state) ;"
    }
}
say "  }";

say "";
say "type 'state fold_config =";
say "  {";
foreach (@$adts) {
    my %t = %$_;
    say "    $t{name} : $t{name} -> 'state -> ('state continue_fold) -> ($t{newName} * 'state) ;";
    say "    $t{name}_pre_state : $t{name} -> 'state -> 'state ;";
    say "    $t{name}_post_state : $t{name} -> $t{newName} -> 'state -> 'state ;";
    foreach (@{$t{ctorsOrFields}}) {
        my %c = %$_;
        say "    $t{name}_$c{name} : $c{type} -> 'state -> ('state continue_fold) -> ($c{newType} * 'state) ;";
    }
}
say "  }";

say "";
say '(* Curries the "visitor" argument to the folds (non-customizable traversal functions). *)';
say "let rec mk_continue_fold : type state . state fold_config -> state continue_fold = fun visitor ->";
say "  {";
foreach (@$adts) {
    my %t = %$_;
    say "    $t{name} = fold_$t{name} visitor ;";
    foreach (@{$t{ctorsOrFields}}) {
        my %c = %$_;
        say "    $t{name}_$c{name} = fold_$t{name}_$c{name} visitor ;";
    }
}
say "}";
say "";

foreach (@$adts) {
    my %t = %$_;
    say "and fold_$t{name} : type state . state fold_config -> $t{name} -> state -> ($t{newName} * state) = fun visitor x state ->";
  say "  let continue_fold : state continue_fold = mk_continue_fold visitor in";
  say "  let state = visitor.$t{name}_pre_state x state in";
  say "  let (new_x, state) = visitor.$t{name} x state continue_fold in";
  say "  let state = visitor.$t{name}_post_state x new_x state in";
  say "  (new_x, state)";
  say "";
  foreach (@{$t{ctorsOrFields}}) {
      my %c = %$_;
      say "and fold_$t{name}_$c{name} : type state . state fold_config -> $c{type} -> state -> ($c{newType} * state) = fun visitor x state ->";
      say "  let continue_fold : state continue_fold = mk_continue_fold visitor in";
      say "  visitor.$t{name}_$c{name} x state continue_fold";
      say "";
  }
}

say "let no_op : 'a fold_config = {";
foreach (@$adts) {
    my %t = %$_;
    say "  $t{name} = (fun v state continue ->";
    say "    match v with";
    if ($t{kind} eq $variant) {
        foreach (@{$t{ctorsOrFields}}) {
            my %c = %$_;
            say "    | $c{name} v -> let (v, state) = continue.$t{name}_$c{name} v state in ($c{newName} v, state)";
        }
    } elsif ($t{kind} eq $record) {
        print "      { ";
        foreach (@{$t{ctorsOrFields}}) {
            my %f = %$_;
            print "$f{name}; ";
        }
        say "} ->";
        foreach (@{$t{ctorsOrFields}}) {
            my %f = %$_;
            say "      let ($f{newName}, state) = continue.$t{name}_$f{name} $f{name} state in";
        }
        print "      ({ ";
        foreach (@{$t{ctorsOrFields}}) {
            my %f = %$_;
            print "$f{newName}; "
        }
        say "}, state)";
    } else {
        print "      v -> fold_$t{kind} v state ( ";
        print join(", ", map { my %f = %$_; "continue.$t{name}_$f{name}" } @{$t{ctorsOrFields}});
        say " )";
    }
    say "  );";
    say "  $t{name}_pre_state = (fun v state -> ignore v; state) ;";
    say "  $t{name}_post_state = (fun v new_v state -> ignore (v, new_v); state) ;";
    foreach (@{$t{ctorsOrFields}}) {
        my %c = %$_;
        print "  $t{name}_$c{name} = (fun v state continue -> ";
        if ($c{isBuiltin}) {
            print "ignore continue; (v, state)";
        } else {
            print "continue.$c{type} v state";
        }
        say ") ;";
    }
}
say "}";
