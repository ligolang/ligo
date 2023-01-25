// Test record type in PascaLIGO

type foobar is record [foo : int; bar : int]

const fb : foobar = record [foo = 0; bar = 0]

type abc is record [a : int; b : int; c : int]

const abc : abc = record [a = 42; b = 142; c = 242]

const a : int = abc.a
const b : int = abc.b
const c : int = abc.c

function projection (const r : foobar) : int is r.foo + r.bar

function modify (var r : foobar) : foobar is
  {
    r.foo := 256
  } with r

function modify_abc (var r : abc) : abc is
  {
    const c : int = 42;
    r := r with record [b=2048; c=c]
  } with r

type big_record is record [a : int; b : int; c : int; d : int; e : int]

const br : big_record =
  record [a = 23; b = 23; c = 23; d = 23; e = 23]

type double_record is record [inner : abc]

function modify_inner (var r : double_record) : double_record is
  {
    r := r with record [inner.b = 2048]
  } with r

type color is
| Blue
| Green

type preferences is record [
  color : color;
  other : int;
]

type account is record [
  id : int;
  preferences : preferences;
]

const acc : account = record [ id = 1 ; preferences = record [ color = Blue ; other = 1]]

function change_color_preference (var account : account; const color : color ) : account is
  {
      account := account with record [preferences.color = color]
  } with account


(* testing that the left hand side of a record access can be an arbitrary expression *)
const lhs_expr_access1 = (modify_abc(abc)).b
const lhs_expr_access2  = (change_color_preference (acc, Green)).preferences.color

(* testing that the left hand side on a functional update can be an arbitrary expression *)
const lhs_expr_fupdate = (change_color_preference (acc, Green)).preferences with record [ other = 2 ]
