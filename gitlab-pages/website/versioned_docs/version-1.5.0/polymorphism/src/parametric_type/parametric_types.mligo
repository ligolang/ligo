type key = string
type 'value binding = key * 'value

let int_binding : int binding = "Alice", 4
let string_binding : string binding = "Bob", "cat"