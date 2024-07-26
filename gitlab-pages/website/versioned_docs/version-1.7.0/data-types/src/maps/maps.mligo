type word       = string
type definition = string list
type dictionary = (word, definition) map

let empty_dict : dictionary = Map.empty

let dictionary : dictionary =
  Map.literal [
    ("one", ["The number 1."; "A member of a group."]);
    ("two", ["The number 2"])]