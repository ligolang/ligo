type word       = string
type definition = string list
type dictionary = (word, definition) big_map

let empty_dict : dictionary = Big_map.empty

let dictionary : dictionary =
  Big_map.literal [
    ("one", ["The number 1."; "A member of a group."]);
    ("two", ["The number 2"])]