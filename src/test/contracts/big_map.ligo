type storage_ is big_map(int, int) * unit

function main(const p : unit; const s : storage_) : list(operation) * storage_ is
  var r : big_map(int, int) := s.0 ;
  var toto : option (int) := Some(0);
  block {
    toto := r[23];
    r[2] := 444;
    s.0 := r;
  }
  with ((nil: list(operation)), s)

function set_ (var n : int ; var m : storage_) : storage_ is block {
  var tmp : big_map(int,int) := m.0 ;
  tmp[23] := n ;
  m.0 := tmp ;
} with m

function rm (var m : storage_) : storage_ is block {
  var tmp : big_map(int,int) := m.0 ;
  remove 42 from map tmp;
  m.0 := tmp;
} with m

function gf (const m : storage_) : int is begin skip end with get_force(23, m.0)

function get (const m : storage_) : option(int) is
  begin
    skip
  end with m.0[42]

// the following is not supported (negative test cases):

// const bm : storage_ = big_map
//   144 -> 23 ;
//   51 -> 23 ;
//   42 -> 23 ;
//   120 -> 23 ;
//   421 -> 23 ;
// end

// type foobar is big_map(int, int)
// const fb : foobar = big_map
//   23 -> 0 ;
//   42 -> 0 ;
// end

// function size_ (const m : storage_) : nat is
//   block {skip} with (size(m.0))

// function iter_op (const m : storage_) : int is
//   var r : int := 0 ;
//   function aggregate (const i : int ; const j : int) : unit is block { r := r + i + j } with unit ;
//   block {
//     map_iter(m.0 , aggregate) ;
//   } with r ;

// function map_op (const m : storage_) : storage_ is
//   function increment (const i : int ; const j : int) : int is block { skip } with j + 1 ;
//   block { skip } with map_map(m.0 , increment) ;
