// Test deep access

type pii is int * int

type ppi is record [x : pii; y : pii]

type ppp is ppi * ppi

function main (const toto : unit) : int is
  block {
    var a : ppp :=
     (record [x = (0,1); y = (10,11)],
      record [x = (100,101); y = (110,111)]);
    a.1.x.1 := 2;
  } with a.1.x.1


function asymetric_tuple_access (const foo : unit) : int is
  block {
    var tuple : int * (int * (int * int)) := (0,(1,(2,3)))
  } with tuple.1 + tuple.2.1 + tuple.2.2.1 + tuple.2.2.2

type nested_record_t is
  record [nesty : record [mymap : map (int,string)]]

function nested_record (var nee : nested_record_t) : string is
  block {
    nee.nesty.mymap[1] := "one"
  } with get_force (1, nee.nesty.mymap)
