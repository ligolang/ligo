/* Test set operations in ReasonLIGO */

let add_op = (s: set(string)): set(string) => Set.add("foobar", s);

let remove_op = (s: set(string)): set(string) => Set.remove("foobar", s);

let remove_deep = (s: (set(string), nat)): (set(string), nat) =>
  Set.remove("foobar", s[0]);

/*
 let patch_op (s: string set) : string set =
   begin patch s with set ["foobar"]; end with s

 let patch_op_deep (s: string set * nat) : string set * nat =
   begin patch s[0] with set ["foobar"]; end with s
 */

let mem_op = (s: set(string)): bool => Set.mem("foobar", s);

let size_op = (s: set(string)): nat => Set.size(s);
