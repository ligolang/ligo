let friends = "Alice", "Bob" // Parentheses are optional
let alice, bob = friends
let alice, _ = friends
let alice, _bob = friends // This alice shadows the previous one
let deep = (1, (2n, "Hello"))
let _, (_, greeting) = deep // greeting = "Hello"
let film = deep.1.1 ^ ", Dolly!" // film = "Hello, Dolly!"