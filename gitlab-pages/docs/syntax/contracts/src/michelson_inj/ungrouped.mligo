let michelson_add (v1 : int) (v2 : int) : int =
  [%Michelson ([%of_file "my_michelson.tz"] : int * int -> int)] (v1, v2)