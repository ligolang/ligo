function sum_list (var l : list (int)) : int is {
  var total : int := 0;
  for i in list l { total := total + i }
} with total

function sum_set (var s : set (int)) : int is {
  var total : int := 0;
  for i in set s { total := total + i }
} with total

function sum_map (var m : map (string, int)) : string * int is {
  var string_total : string := "";
  var int_total : int := 0;
  for key -> value in map m {
    string_total := string_total ^ key;
    int_total := int_total + value
  }
} with (string_total, int_total)
