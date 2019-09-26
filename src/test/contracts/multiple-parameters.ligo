// Test functions with several parameters in PascaLIGO

function ab(const a : int; const b : int) : int is
  begin skip end with (a + b)

function abcd(const a : int; const b : int; const c : int; const d : int) : int is
  begin skip end with (a + b + c + d + 2)

function abcde(const a : int; const b : int; const c : int; const d : int; const e : int) : int is
  begin skip end with (c + e + 3)
