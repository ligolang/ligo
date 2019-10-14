// Test map type and related built-in functions in PascaLIGO

type foobar is map(int, int)

const empty_map : foobar = map end

const map1 : foobar = map
  144 -> 23 ;
  51 -> 23 ;
  42 -> 23 ;
  120 -> 23 ;
  421 -> 23 ;
end
const map2 : foobar = map
  23 -> 0 ;
  42 -> 0 ;
end

function set_ (var n : int ; var m : foobar) : foobar is block {
  m[23] := n ;
} with m


function rm (var m : foobar) : foobar is block {
  remove 42 from map m
} with m

function patch_ (var m: foobar) : foobar is block {
  patch m with map [0 -> 5; 1 -> 6; 2 -> 7]
} with m

function patch_empty (var m : foobar) : foobar is block {
  patch m with map []
} with m

function patch_deep (var m: foobar * nat) : foobar * nat is
  begin patch m.0 with map [1 -> 9]; end with m

function size_ (const m : foobar) : nat is
  block {skip} with (size(m))

function gf (const m : foobar) : int is begin skip end with get_force(23, m)

function get (const m : foobar) : option(int) is
  begin
    skip
  end with m[42]

function get_ (const m : foobar) : option(int) is
  begin
    skip
  end with map_get(42 , m)

function iter_op (const m : foobar) : unit is
  function aggregate (const i : int ; const j : int) : unit is block
    { if (i=j) then skip else failwith("fail") } with unit ;
  block {skip}
    // map_iter(m , aggregate) ;
  with map_iter(m, aggregate) ;

function map_op (const m : foobar) : foobar is
  function increment (const i : int ; const j : int) : int is block { skip } with j + 1 ;
  block { skip } with map_map(m , increment) ;

function fold_op (const m : foobar) : int is
  function aggregate (const i : int ; const j : (int * int)) : int is block { skip } with i + j.0 + j.1 ;
  block { skip } with map_fold(m , 10 , aggregate)

function deep_op (var m : foobar) : foobar is
var coco : (int*foobar) := (0, m);
block {
  remove 42 from map coco.1 ;
  coco.1[32] := 16 ;
} with coco.1
