type breed = string
let dog_breed_1 : breed = "Saluki"
let dog_breed_2 : string = "Shiba Inu"

let greet_dogs (dogs : breed list) : string =
  List.fold (fun (a, b : breed * breed) -> String.concats [a; ", "; b]) dogs "Hello"

let greeting = greet_dogs [dog_breed_1; dog_breed_2]