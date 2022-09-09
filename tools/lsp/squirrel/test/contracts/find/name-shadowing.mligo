type commit = int

let foo (commit : commit) : commit =
  let commit = commit + 1 in
  let commit = commit + 2 in
  commit + 42
