let a : int = 5 + 10    // int + int yields int
let b : nat = 5n + 10n  // nat + nat yields nat
let c : int = 5n + 10   // nat + int yields int
let d : int = 10 + 5n   // int + nat yields int
// let error : nat = 5n + 10