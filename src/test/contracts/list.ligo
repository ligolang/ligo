// Test list type and related built-in functions in PascaLIGO

type foobar is list (int)

const fb : foobar = list [23; 42]

const fb2 : foobar = 144 # fb

const fb3 : foobar = List.cons (688, fb2)

const x : foobar = list []
const y : foobar = list [3; 4; 5]
const z : foobar = 2 # y

const fb_head = List.head_opt (fb)

const fb_tail = List.tail_opt (fb)

function size_ (const m : foobar) : nat is List.length (m)

// function hdf (const m : foobar) : int is hd (m)

const bl : foobar = list [144; 51; 42; 120; 421]

function fold_op (const s : list (int)) : int is
  {
    function aggregate (const prec: int; const cur: int) : int is prec+cur
  } with List.fold (aggregate, s, 10)

function iter_op (const s : list (int)) : int is
  {
    var r : int := 0;
    function aggregate (const _i : int) : unit is
      unit;
    List.iter (aggregate, s)
  } with r

function map_op (const s : list (int)) : list (int) is
  {
    function increment (const i : int) : int is i+1
  } with List.map (increment, s)

const find_x : option (int) =
  {
    function f (const _ : int) : bool is true
  } with List.find_opt (f, x)

const find_y4 : option (int) =
  {
    function f (const y_elem : int) : bool is y_elem = 4
  } with List.find_opt (f, y)

const find_y6 : option (int) =
  {
    function f (const y_elem : int) : bool is y_elem = 6
  } with List.find_opt (f, y)

const find_z2 : option (int) =
  { 
    function f (const z_elem : int) : bool is z_elem = 2
  } with List.find_opt (f, z)


