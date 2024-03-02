type two_people = string * string  // Alias

let friends : two_people = ("Alice", "Johnson") // Optional parentheses
let (person_a, person_b) : two_people = friends
let first_person ((person_a, _): two_people) = person_a
let alice = first_person friends
let first_name : string = friends.0