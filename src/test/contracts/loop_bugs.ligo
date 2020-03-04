function shadowing_in_body (var nee : unit) : string is block {
  var st : string := "";
  var list1 : list (string) := list ["to"; "to"];
  for x in list list1 block {
    const x : string = "ta";
    st := st ^ x;
  }
} with st
(* should be "tata" *)

function shadowing_assigned_in_body (var nee : unit) : string is block {
  var st : string := "";
  var list1 : list (string) := list ["to"; "to"];
  for x in list list1 block {
    st := st ^ x;
    var st : string := "ta";
    st := st ^ x;
  }
} with st
(* should be "toto" ??? *)
